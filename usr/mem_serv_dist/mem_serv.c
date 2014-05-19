/**
 * \file
 * \brief Distributed (percore) memory server
 */

/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>

#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <mm/mm.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <barrelfish/morecore.h>
#include <barrelfish/monitor_client.h>
#include <barrelfish/spawn_client.h>
#include <skb/skb.h>
#include <dist/barrier.h>

#include <if/mem_defs.h>
#include <if/mem_rpcclient_defs.h>
#include <if/monitor_defs.h>
#include <if/spawn_rpcclient_defs.h>

#ifdef __scc__
#include <barrelfish_kpi/shared_mem_arch.h>
#endif

#include "skb.h"
#include "args.h"

// #include "barrier.h"

#include "mem_serv.h"
#include "steal.h"

/*
 * TODO:
 * - currently requests too much memory from initial mem_serv and other 
 *   (non_dist) mem_serv clients may suffer
 */

/// Globally track the total memory available
memsize_t mem_total = 0;
/// Globally track the actual memory available to allocate
memsize_t mem_avail = 0;
/// Globally track the local reserve memory available to allocate
memsize_t mem_local = 0;

/// MM per-core allocator instance data: B-tree to manage mem regions
struct mm mm_percore;
// static storage for MM allocator to get it started 
static char percore_nodebuf[SLAB_STATIC_SIZE(MINSPARENODES,
                                             MM_NODE_SIZE(MAXCHILDBITS))];

/// MM allocator for reserve of emergency memory used within the mem_serv only
struct mm mm_local;
// static storage for MM allocator to get it started 
static char local_nodebuf[SLAB_STATIC_SIZE(MINSPARENODES,
                                           MM_NODE_SIZE(MAXCHILDBITS))];

/// simple slot allocator used by MM
static struct slot_prealloc percore_slot_alloc;

#ifndef __scc__
static struct mm *mm_slots = &mm_percore;
#else
static struct mm *mm_slots = &mm_local;
#endif

#if 0
static void dump_ram_region(int index, struct mem_region* m)
{

    uintptr_t start, limit;

    start = (uintptr_t)m->mr_base;
    limit = start + (1UL << m->mr_bits);

    char prefix = ' ';
    size_t quantity = 1UL << m->mr_bits;

    if (m->mr_bits >= 30) {
        prefix = 'G';
        quantity >>= 30;
    }
    else if (m->mr_bits >= 20) {
        prefix = 'M';
        quantity >>= 20;
    }
    else if (m->mr_bits >= 10) {
        prefix = 'K';
        quantity >>= 10;
    }

    printf("RAM region %d: 0x%" PRIxPTR
           " - 0x%" PRIxPTR " (%lu %cB, %u bits)\n",
           index, start, limit, quantity, prefix, m->mr_bits);
}
#endif // 0

errval_t slab_refill(struct slab_alloc *slabs)
{
    errval_t err;

    // refill slab allocator if needed 
    while (slab_freecount(slabs) <= MINSPARENODES) {
        // debug_printf("running low on free slabs: slabs=%ld\n", 
        //             slab_freecount(&mm_percore.slabs));
        struct capref frame;
        err = slot_alloc(&frame);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_SLOT_ALLOC);
        }
        err = frame_create(frame, BASE_PAGE_SIZE * 8, NULL);
        if (err_is_fail(err)) {
            slot_free(frame);
            return err_push(err, LIB_ERR_FRAME_CREATE);
        }
        void *buf;
        err = vspace_map_one_frame(&buf, BASE_PAGE_SIZE * 8, frame,
                                   NULL, NULL);
        if (err_is_fail(err)) {
            cap_destroy(frame);
            return err_push(err, LIB_ERR_VSPACE_MAP);
        }
        slab_grow(slabs, buf, BASE_PAGE_SIZE * 8);
    }

    return SYS_ERR_OK;
}

static errval_t do_free(struct mm *mm, struct capref ramcap,
                        genpaddr_t base, uint8_t bits,
                        memsize_t *mem_available)
{
    errval_t ret;
    memsize_t mem_to_add;

    mem_to_add = (memsize_t)1 << bits;

    ret = mm_free(mm, ramcap, base, bits);
    if (err_is_fail(ret)) {
        if (err_no(ret) == MM_ERR_NOT_FOUND) {
            // memory wasn't there initially, add it
            ret = mm_add(mm, ramcap, bits, base);
            if (err_is_fail(ret)) {
                return err_push(ret, MM_ERR_MM_ADD);
            }
            mem_total += mem_to_add;
        } else {
            return err_push(ret, MM_ERR_MM_FREE);
        }
    }

    *mem_available += mem_to_add;

    return SYS_ERR_OK;
}

static errval_t percore_free(struct capref ramcap) 
{
    struct capability info;
    errval_t ret;

    ret = debug_cap_identify(ramcap, &info);
    if (err_is_fail(ret)) {
        return err_push(ret, MON_ERR_CAP_IDENTIFY);
    }

    if (info.type != ObjType_RAM) {
        return SYS_ERR_INVALID_SOURCE_TYPE;
    }

#if 0
    printf("%d: Cap is type %d Ram base 0x%"PRIxGENPADDR
           " (%"PRIuGENPADDR") Bits %d\n", disp_get_core_id(),
           info.type, info.u.ram.base, info.u.ram.base, 
           info.u.ram.bits);
#endif

    return do_free(&mm_percore, ramcap, info.u.ram.base,
                   info.u.ram.bits, &mem_avail);
}

#ifdef __scc__
static errval_t local_free(struct capref ramcap) 
{
    struct capability info;
    errval_t ret;

    ret = debug_cap_identify(ramcap, &info);
    if (err_is_fail(ret)) {
        return err_push(ret, MON_ERR_CAP_IDENTIFY);
    }

    if (info.type != ObjType_RAM) {
        return SYS_ERR_INVALID_SOURCE_TYPE;
    }

    return do_free(&mm_local, ramcap, info.u.ram.base,
                   info.u.ram.bits, &mem_local);
}
#endif

errval_t percore_free_handler_common(struct capref ramcap, genpaddr_t base,
                                     uint8_t bits)
{
#ifndef __scc__
    return do_free(&mm_percore, ramcap, base, bits, &mem_avail);
#else
    if (base < SHARED_MEM_MIN) {
        return do_free(&mm_local, ramcap, base, bits, &mem_local);
    } else {
        return do_free(&mm_percore, ramcap, base, bits, &mem_avail);
    }
#endif
}

memsize_t mem_available_handler_common(void) 
{
    return mem_avail;
}


static errval_t do_alloc(struct mm *mm, struct capref *ret, uint8_t bits,
                         genpaddr_t minbase, genpaddr_t maxlimit,
                         memsize_t *mem_available)
{
    errval_t err;

    assert(bits >= MINSIZEBITS);

    if (((memsize_t)1 << bits) > *mem_available) {
        return MM_ERR_NOT_FOUND;
    }

    if(maxlimit == 0) {
        err = mm_alloc(mm, bits, ret, NULL);
    } else {
        err = mm_alloc_range(mm, bits, minbase, maxlimit, ret, NULL);
    }

    if (err_is_ok(err)) {
        *mem_available -= (memsize_t)1 << bits;
    }

    return err;
}


errval_t percore_alloc(struct capref *ret, uint8_t bits,
                              genpaddr_t minbase, genpaddr_t maxlimit)
{
    return do_alloc(&mm_percore, ret, bits, minbase, maxlimit, &mem_avail);
}


static errval_t local_alloc(struct capref *ret, uint8_t bits,
                            genpaddr_t minbase, genpaddr_t maxlimit)
{
    errval_t err;

#ifdef __scc__
    // first try local memory
    err = do_alloc(&mm_local, ret, bits, minbase, maxlimit, &mem_local);
  
    // then try the general percore memory
    if (err_is_fail(err)) {
        err = percore_alloc(ret, bits, minbase, maxlimit);
    }
#else
    // first try the general percore memory
    err = percore_alloc(ret, bits, minbase, maxlimit);

    // then try the local reserve
    if (err_is_fail(err)) {
        err = do_alloc(&mm_local, ret, bits, minbase, maxlimit, &mem_local);
    }
#endif

    return err;
}

static errval_t get_more_ram(uint8_t bits, genpaddr_t minbase, 
                             genpaddr_t maxlimit)
{
    errval_t err;
    struct capref cap;
    
    // try to steal a RAM cap
    try_steal(&err, &cap, bits, minbase, maxlimit);
    if (err_is_fail(err)) {
        // try to get a local reserve RAM cap
        err = local_alloc(&cap, bits, minbase, maxlimit);
        if (err_is_fail(err)) {
            return err;
        }
    }
    // make the cap available for a subsequent alloc
#ifndef __scc__
    percore_free(cap);
#else
    local_free(cap);
#endif

    return SYS_ERR_OK;    
}

static errval_t do_slot_prealloc_refill(struct slot_prealloc *slot_alloc_inst)
{
    errval_t err;

    assert(slot_alloc_inst != NULL);

    err = slot_prealloc_refill(slot_alloc_inst);
    if (err_is_fail(err)) {
        err = get_more_ram(slot_alloc_inst->cnode_size_bits + OBJBITS_CTE, 0,0);
        if (err_is_fail(err)) {
            // debug_printf("get_more_ram failed\n");
        }
        err = slot_prealloc_refill(slot_alloc_inst);
        if (err_is_ok(err)) {
            // debug_printf("second refill succeeded\n");
        }
    }
    return err;
}

errval_t percore_allocate_handler_common(uint8_t bits,
                                         genpaddr_t minbase, 
                                         genpaddr_t maxlimit,
                                         struct capref *retcap)
{
    struct capref cap;
    errval_t err, ret;

    // debug_printf("percore alloc request: bits: %d\n", bits);

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_PERCORE_ALLOC, bits);

    // refill slot allocator if needed 
    err = do_slot_prealloc_refill(mm_slots->slot_alloc_inst);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Warning: failure in slot_prealloc_refill");
    }

    // refill slab allocators if needed 
    err = slab_refill(&mm_percore.slabs);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Warning: failure when refilling mm_percore slab");
    }

    err = slab_refill(&mm_local.slabs);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Warning: failure when refilling mm_local slab");
    }

    // do the actual allocation
#ifndef __scc__
    ret = percore_alloc(&cap, bits, minbase, maxlimit);
#else
    ret = local_alloc(&cap, bits, minbase, maxlimit);
#endif

    if (err_is_fail(ret)) {
        // debug_printf("percore_alloc(%d (%lu)) failed\n", bits, 1UL << bits);
		printf("[%d][%"PRIuDOMAINID"] percore_alloc failed, going to steal\n",
					disp_get_core_id(), disp_get_domain_id());
        try_steal(&ret, &cap, bits, minbase, maxlimit);
    }

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_PERCORE_ALLOC_COMPLETE, bits);

    *retcap = cap;
    return ret;
}


// this is a candidate for smarter calculation. possibly by the skb
static memsize_t get_percore_size(int num_cores)
{
#ifdef MEMSERV_PERCORE_DYNAMIC
    errval_t err;
    memsize_t all_mem_avail, mem_percore, tot_mem;

    // send message to mem_serv
    struct mem_rpc_client *b = get_mem_client();
    err = b->vtbl.available(b, &all_mem_avail, &tot_mem);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Warning: failure in call to mem_serv.available");
        // default to predetermined amount of memory per core
        return PERCORE_MEM;
    }

    debug_printf("available memory: %"PRIuMEMSIZE" bytes over %d cores\n", 
                  all_mem_avail, num_cores);

    mem_percore = all_mem_avail / num_cores;

    debug_printf("available memory per core: %"PRIuMEMSIZE" bytes\n", 
                  mem_percore);

    return mem_percore;
#else
    // Use predetermined amount of memory per core

    debug_printf("available memory per core: %"PRIuMEMSIZE" bytes\n", 
                 PERCORE_MEM);

    return PERCORE_MEM;
#endif
}

#ifdef MEMSERV_AFFINITY
static void set_affinity(coreid_t core)
{
    // get core affinity range and set it as the default for ram_alloc
    errval_t err;
    genpaddr_t base;
    genpaddr_t limit;
    err = get_percore_affinity(core, &base, &limit);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Warning: failure in get_percore_affinity");
        base = 0;
        limit = 0;
    }
    ram_set_affinity(base, limit);

    debug_printf("affinity range is base: %"PRIuGENPADDR", limit: %"
                 PRIuGENPADDR"\n", base, limit);
}
#endif


static memsize_t fill_mm(struct mm *mm, memsize_t mem_requested, uint8_t bits, 
                      memsize_t *mem_tot)
{
    errval_t err;

    memsize_t mem_added = 0;
    memsize_t mem_to_add = 0;
    struct capref ramcap;
    struct capability info;

    // get as much of the requested memory as we can, requesting ever 
    // smaller RAM caps until we hit the smallest RAM cap size

    while (bits >= MINALLOCBITS) {

        // debug_printf("adding memory %"PRIuMEMSIZE" (%d bits)\n", 
        //             (memsize_t)1<<bits, bits);

        err = ram_alloc(&ramcap, bits);
        if (err_is_fail(err)) {
            // skip this size and try the next size down
            bits--;
            continue;
        } 

        // XXX: Hack until we have cross-core cap management
        // Forget about remote relations of this cap. This will ensure
        // that the monitor will hand it back to us in case anyone on
        // this core deletes it.
        err = monitor_cap_set_remote(ramcap, false);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "Warning: failed to set cap non-remote. Trying next one.");
            continue;
        }

        err = debug_cap_identify(ramcap, &info);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Warning: failed to identify cap. Trying next one.");
            continue;
        }
#if 0
        debug_printf("Cap is type %d Ram base 0x%"PRIxGENPADDR
                     " (%"PRIuGENPADDR") Bits %d\n",
                     info.type, info.u.ram.base, info.u.ram.base, 
                     info.u.ram.bits);
#endif
        assert(bits == info.u.ram.bits);
        
        mem_to_add = (memsize_t)1 << bits;

        *mem_tot += mem_to_add;

        err = mm_add(mm, ramcap, bits, info.u.ram.base);
        if (err_is_ok(err)) {
            mem_added += mem_to_add;

            mem_requested -= mem_to_add;
            uint8_t new_bits = log2floor(mem_requested);
            bits = MIN(bits, new_bits);
        } else {
            DEBUG_ERR(err, "Warning: adding RAM region (%p/%d) FAILED", 
                      info.u.ram.base, info.u.ram.bits);
        }
    }

    return mem_added;
}


static errval_t init_mm(struct mm *mm, char nodebuf[], memsize_t nodebuf_size,
                        struct slot_prealloc *slot_alloc_inst,
                        memsize_t *mem_added, memsize_t *mem_tot)
{
    errval_t err;

    struct capref ramcap;
    struct capability info;

    /* XXX Base shouldn't need to be 0 ? */
    err = mm_init(mm, ObjType_RAM,
                  0, MAXSIZEBITS, MAXCHILDBITS, NULL,
                  slot_alloc_prealloc, slot_alloc_inst, true);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }

    slab_grow(&mm->slabs, nodebuf, nodebuf_size);

    // Need to bootstrap with a small cap first!
    err = ram_alloc(&ramcap, SMALLCAP_BITS);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to get small RAM from mem_serv");
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    err = debug_cap_identify(ramcap, &info);
    if (err_is_fail(err)) {
        percore_free(ramcap);
        return err_push(err, MON_ERR_CAP_IDENTIFY);
    }

#if 0
    printf("Cap is type %d Ram base 0x%"PRIxGENPADDR" Bits %d\n",
           info.type, info.u.ram.base, info.u.ram.bits);
#endif
    assert(SMALLCAP_BITS == info.u.ram.bits);

    *mem_tot += (memsize_t)1<<SMALLCAP_BITS;

    err = mm_add(mm, ramcap, SMALLCAP_BITS, info.u.ram.base);
    if (err_is_ok(err)) {
        *mem_added += (memsize_t)1<<SMALLCAP_BITS;
    } else {
        percore_free(ramcap);
        return err_push(err, MM_ERR_MM_ADD);
    }

    // try to refill slot allocator (may fail or do nothing) 
    slot_prealloc_refill(mm->slot_alloc_inst);

    return SYS_ERR_OK;
}

static errval_t init_slot_allocator(struct slot_prealloc *slot_alloc_inst, 
                                struct mm *mm)
{
    errval_t err;

    // Initialize slot allocator by passing a cnode cap for it to start with 
    struct capref cnode_cap;
    err = slot_alloc(&cnode_cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    struct capref cnode_start_cap = { .slot  = 0 };
    struct capref ram;

    err = ram_alloc(&ram, BASE_PAGE_BITS);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to allocate RAM cap for cnode");
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    err = cnode_create_from_mem(cnode_cap, ram, &cnode_start_cap.cnode,
                                DEFAULT_CNODE_BITS);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE_FROM_MEM);
    }

    // location where slot allocator will place its top-level cnode 
    struct capref top_slot_cap = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_MODULECN, // XXX: we don't have the module CNode
    };

    // init slot allocator 
    err = slot_prealloc_init(slot_alloc_inst, top_slot_cap,
                             MAXCHILDBITS,
                             CNODE_BITS, cnode_start_cap,
                             1UL << DEFAULT_CNODE_BITS, mm);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_SLOT_ALLOC_INIT);
    } 
      
    return SYS_ERR_OK;
}

errval_t initialize_percore_mem_serv(coreid_t core, coreid_t *cores, 
                                     int len_cores, memsize_t percore_mem)
{
    errval_t err;

    mem_avail = 0;
    mem_total = 0;

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_PERCORE_INIT, 0);

    err = init_slot_allocator(&percore_slot_alloc, mm_slots);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_SLOT_ALLOC_INIT);
    }

#ifdef __scc__
    ram_set_affinity(0, EXTRA_SHARED_MEM_MIN);
    err = init_mm(&mm_local, local_nodebuf, sizeof(local_nodebuf),
                  &percore_slot_alloc, &mem_local, &mem_total);
    if (err_is_fail(err)) {
        return err;
    }
    ram_set_affinity(SHARED_MEM_MIN + (PERCORE_MEM_SIZE * disp_get_core_id()),
                     SHARED_MEM_MIN + (PERCORE_MEM_SIZE * (disp_get_core_id() + 1)));
#endif
    err = init_mm(&mm_percore, percore_nodebuf, sizeof(percore_nodebuf),
                  &percore_slot_alloc, &mem_avail, &mem_total);
    if (err_is_fail(err)) {
        return err;
    }
#ifndef __scc__
    err = init_mm(&mm_local, local_nodebuf, sizeof(local_nodebuf),
                  &percore_slot_alloc, &mem_local, &mem_total);
    if (err_is_fail(err)) {
        return err;
    }
#endif

#ifdef MEMSERV_AFFINITY
    set_affinity(core);
#endif

#ifdef __scc__
    // Suck up private RAM
    ram_set_affinity(0, EXTRA_SHARED_MEM_MIN);
#endif

    // determine how much memory we need to get to fill up the percore mm
    percore_mem -= mem_total; // memory we've already taken
    percore_mem -= LOCAL_MEM; // memory we'll take for mm_local

#ifdef __scc__
    // Take all of private RAM we can get
    percore_mem = PERCORE_MEM_SIZE;
#endif

    uint8_t percore_bits = log2floor(percore_mem);
    if (percore_bits > MAXSIZEBITS) {
        percore_bits = MAXSIZEBITS;
    }
    // debug_printf("memory to use: %"PRIuMEMSIZE"\n", percore_mem);

    mem_local += fill_mm(&mm_local, LOCAL_MEM, LOCAL_MEMBITS, &mem_total);

#ifdef __scc__
    ram_set_affinity(SHARED_MEM_MIN + (PERCORE_MEM_SIZE * disp_get_core_id()),
                     SHARED_MEM_MIN + (PERCORE_MEM_SIZE * (disp_get_core_id() + 1)));
#endif

    mem_avail += fill_mm(&mm_percore, percore_mem, percore_bits, &mem_total);

    // from now on we don't care where memory comes from anymore
    ram_set_affinity(0,0);
    // also use our own memory, rather than the remote central mem_serv
    ram_alloc_set(local_alloc);

    // try to refill slot allocator (may fail or do nothing)
    // TODO: is this necessary?
    slot_prealloc_refill(mm_slots->slot_alloc_inst);

    // refill slab allocator if needed and possible 
    if (slab_freecount(&mm_percore.slabs) <= MINSPARENODES
        && mem_avail > (1UL << (CNODE_BITS + OBJBITS_CTE)) * 2
        + 10 * BASE_PAGE_SIZE) {
        slab_default_refill(&mm_percore.slabs); // may fail
    }

    if (slab_freecount(&mm_local.slabs) <= MINSPARENODES
        && mem_avail > (1UL << (CNODE_BITS + OBJBITS_CTE)) * 2
        + 10 * BASE_PAGE_SIZE) {
        slab_default_refill(&mm_local.slabs); // may fail
    }

    // try to refill slot allocator - now it shouldn't fail!
    err = slot_prealloc_refill(mm_slots->slot_alloc_inst);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Fatal internal error in RAM allocator: "
                  "failed to init slot allocator");
        return err;
    }

    // init peer data structures so we know who to contact to steal memory
    err = init_peers(core, len_cores, cores);
    if (err_is_fail(err)) {
        return err_push(err, MS_ERR_INIT_PEERS);
    }

    // done.
    debug_printf("Percore RAM allocator initialised, %"PRIuMEMSIZE
                 " MB (of %"PRIuMEMSIZE" MB) available\n",
                 mem_avail / 1024 / 1024, mem_total / 1024 / 1024);


    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_PERCORE_INIT, 9);

    return SYS_ERR_OK;
}

/**
 * \brief Request a spawnd to reconnect to a local memserv
 *
 * \param coreid The core that the spawnd is running on
 */
errval_t set_local_spawnd_memserv(coreid_t coreid)
{
    struct spawn_rpc_client *cl;
    errval_t err = spawn_rpc_client(coreid, &cl);
    if (err_is_fail(err)) {
        return err;
    }

    return cl->vtbl.use_local_memserv(cl);
}


static int run_worker(coreid_t core, struct args *args)
{
    assert(args != NULL);

    // debug_printf("Distributed mem_serv. percore server on core %d\n", core);

    // this should never return
    percore_mem_serv(core, args->cores, args->cores_len, args->ram); 
    return EXIT_FAILURE; // so we should never reach here
}


static int run_master(coreid_t core, struct args *args)
{
    assert(args != NULL);

    errval_t err;

    debug_printf("Distributed mem_serv. master on core %d\n", core);

    memsize_t percore_mem;
    if (args->ram > 0) {
        percore_mem = args->ram;
    } else {
        percore_mem = get_percore_size(args->cores_len); 
    }

    // debug_printf("spawning on %d cores\n", args->cores_len);

    // set up args for the spawn
    // -w
    // -c <core list>
    // -r <percore_mem>
    char *new_argv[7];
    new_argv[0] = args->path;
    new_argv[1] = "-w"; 
    new_argv[2] = "-c";
    new_argv[3] = list_to_string(args->cores, args->cores_len);
    assert(new_argv[3] != NULL);
    if (new_argv[3] == NULL) {
        DEBUG_ERR(LIB_ERR_MALLOC_FAIL, "out of memory");
        return EXIT_FAILURE;
    }
    new_argv[4] = "-r";
    new_argv[5] = malloc(20); // enough to fit a 64 bit number
    assert(new_argv[5] != NULL);
    if (new_argv[5] == NULL) {
        DEBUG_ERR(LIB_ERR_MALLOC_FAIL, "out of memory");
        return EXIT_FAILURE;
    }
    sprintf(new_argv[5], "%"PRIuMEMSIZE, percore_mem);
    new_argv[6] = NULL;

    for (int i = 0; i < args->cores_len; i++) {
        err = spawn_program(args->cores[i], new_argv[0], new_argv, 
                            NULL, 0, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "spawning percore mem_serv on core %d", i);
            return EXIT_FAILURE;
        }
    }
    
    // wait for all the spawned mem_servs to start up
    // err = ns_barrier_master_l(args->cores, args->cores_len, MEMSERV_DIST);
    err = nsb_master_l(args->cores, args->cores_len, MEMSERV_DIST);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "barrier_master failed");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

int common_main(int argc, char ** argv)
{
    coreid_t core = disp_get_core_id();

    struct args my_args;
    my_args = process_args(argc, argv);

    if (my_args.master) {
        return run_master(core, &my_args);
    } else {
        return run_worker(core, &my_args);
    } 

    return EXIT_SUCCESS;
}
