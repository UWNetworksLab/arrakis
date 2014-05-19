/**
 * \file
 * \brief Memory server
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <mm/mm.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <barrelfish/morecore.h>
#include <barrelfish/monitor_client.h>

#include <if/mem_defs.h>
#include <if/monitor_defs.h>

size_t mem_total = 0, mem_avail = 0;

/* parameters for size of supported RAM and thus required storage */

// XXX: Even though we could manage an arbitrary amount of RAM on any
// architecture, we use paddr_t as the type to represent region
// limits, which limits us its size.
#if defined(__x86_64__)
#       define MAXSIZEBITS     38              ///< Max size of memory in allocator
#elif defined(__i386__)
#       define MAXSIZEBITS     32
#elif defined(__arm__)
/* XXX This is better if < 32! - but there were no compile time warnings! */
#       define MAXSIZEBITS     31
#else
#       error Unknown architecture
#endif

#define MINSIZEBITS     OBJBITS_DISPATCHER ///< Min size of each allocation
#define MAXCHILDBITS    4               ///< Max branching of BTree nodes

/// Maximum depth of the BTree, assuming only branching by two at each level
#define MAXDEPTH        (MAXSIZEBITS - MINSIZEBITS + 1)
/// Maximum number of BTree nodes
#define NNODES          ((1UL << MAXDEPTH) - 1)

/* Parameters for per-core memserv */
#define PERCORE_BITS 24
#define PERCORE_MEM (1UL<<PERCORE_BITS)           ///< How much memory per-core

static struct multi_slot_allocator msa;
static struct bootinfo *bi;

/**
 * \brief Size of CNodes to be created by slot allocator.
 *
 * Must satisfy both:
 *    #CNODE_BITS >= MAXCHILDBITS           (cnode enough for max branching factor)
 *    (1UL << #CNODE_BITS) ** 2 >= #NNODES  (total number of slots is enough)
 */
#define CNODE_BITS      12
#define NCNODES         (1UL << CNODE_BITS)     ///< Maximum number of CNodes

/// Watermark at which we must refill the slab allocator used for nodes
#define MINSPARENODES   (MAXDEPTH * 8) // XXX: FIXME: experimentally determined!

/// MM allocator instance data
static struct mm mm_ram;

/// Slot allocator for MM
static struct slot_prealloc ram_slot_alloc;

static errval_t mymm_alloc(struct capref *ret, uint8_t bits, genpaddr_t minbase,
                           genpaddr_t maxlimit)
{
    errval_t err;

    assert(bits >= MINSIZEBITS);

    if(maxlimit == 0) {
        err = mm_alloc(&mm_ram, bits, ret, NULL);
    } else {
        err = mm_alloc_range(&mm_ram, bits, minbase, maxlimit, ret, NULL);
    }

    return err;
}

static errval_t mymm_free(struct capref ramcap, genpaddr_t base, uint8_t bits)
{
    errval_t ret;
    genpaddr_t mem_to_add;

    mem_to_add = (genpaddr_t)1 << bits;

    ret = mm_free(&mm_ram, ramcap, base, bits);
    if (err_is_fail(ret)) {
        if (err_no(ret) == MM_ERR_NOT_FOUND) {
            // memory wasn't there initially, add it
            ret = mm_add(&mm_ram, ramcap, bits, base);
            if (err_is_fail(ret)) {
                /* DEBUG_ERR(ret, "failed to add RAM to allocator"); */
                return ret;
            }
            mem_total += mem_to_add;
        } else {
            /* DEBUG_ERR(ret, "failed to free RAM in allocator"); */
            return ret;
        }
    }

    mem_avail += mem_to_add;

    return SYS_ERR_OK;
}


/// state for a pending reply
// because we have only one message that we send to a client, and there can only
// be one outstanding per binding (because this is an RPC interface) this is
// quite simple
struct pending_reply {
    struct mem_binding *b;
    errval_t err;
    struct capref *cap;
};


static void retry_free_reply(void *arg)
{
    struct pending_reply *r = arg;
    assert(r != NULL);
    struct mem_binding *b = r->b;
    errval_t err;

    err = b->tx_vtbl.free_monitor_response(b, NOP_CONT, r->err);
    if (err_is_ok(err)) {
        b->st = NULL;
        free(r);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = b->register_send(b, get_default_waitset(),
                               MKCONT(retry_free_reply,r));
    }

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to reply to free request");
        free(r);
    }
}

static void allocate_response_done(void *arg)
{
    struct capref *cap = arg;

    if(!capref_is_null(*cap)) {
        errval_t err = cap_delete(*cap);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "cap_delete after send. This memory will leak.");
        }
    }

    free(cap);
}

static void retry_reply(void *arg)
{
    struct pending_reply *r = arg;
    assert(r != NULL);
    struct mem_binding *b = r->b;
    errval_t err;

    err = b->tx_vtbl.allocate_response(b, MKCONT(allocate_response_done, r->cap),
                                       r->err, *r->cap);
    if (err_is_ok(err)) {
        b->st = NULL;
        free(r);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = b->register_send(b, get_default_waitset(), MKCONT(retry_reply,r));
        assert(err_is_ok(err));
    } else {
        DEBUG_ERR(err, "failed to reply to memory request");
        allocate_response_done(r->cap);
    }
}



static void mem_free_handler(struct mem_binding *b,
                             struct capref ramcap, genpaddr_t base,
                             uint8_t bits)
{
    errval_t ret;
    errval_t err;

    ret = mymm_free(ramcap, base, bits);

    err = b->tx_vtbl.free_monitor_response(b, NOP_CONT, ret);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct pending_reply *r = malloc(sizeof(struct pending_reply));
            assert(r != NULL);
            r->b = b;
            r->err = ret;
            err = b->register_send(b, get_default_waitset(),
                                   MKCONT(retry_free_reply,r));
            assert(err_is_ok(err));
        } else {
            DEBUG_ERR(err, "failed to reply to free request");
        }
    }
}


static void mem_available_handler(struct mem_binding *b)
{
    errval_t err;
    /* Reply */
    err = b->tx_vtbl.available_response(b, NOP_CONT, mem_avail, mem_total);
    if (err_is_fail(err)) {
        // FIXME: handle FLOUNDER_ERR_TX_BUSY
        DEBUG_ERR(err, "failed to reply to memory request");
    }

}

// FIXME: error handling (not asserts) needed in this function
static void mem_allocate_handler(struct mem_binding *b, uint8_t bits,
                                 genpaddr_t minbase, genpaddr_t maxlimit)
{
    struct capref *cap = malloc(sizeof(struct capref));
    errval_t err, ret;

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_ALLOC, bits);

    /* refill slot allocator if needed */
    err = slot_prealloc_refill(mm_ram.slot_alloc_inst);
    assert(err_is_ok(err));

    /* refill slab allocator if needed */
    while (slab_freecount(&mm_ram.slabs) <= MINSPARENODES) {
        struct capref frame;
        err = msa.a.alloc(&msa.a, &frame);
        assert(err_is_ok(err));
        err = frame_create(frame, BASE_PAGE_SIZE * 8, NULL);
        assert(err_is_ok(err));
        void *buf;
        err = vspace_map_one_frame(&buf, BASE_PAGE_SIZE * 8, frame, NULL, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "vspace_map_one_frame failed");
            assert(buf);
        }
        slab_grow(&mm_ram.slabs, buf, BASE_PAGE_SIZE * 8);
    }

    ret = mymm_alloc(cap, bits, minbase, maxlimit);
    if (err_is_ok(ret)) {
        mem_avail -= 1UL << bits;
    } else {
        // DEBUG_ERR(ret, "allocation of %d bits in % " PRIxGENPADDR "-%" PRIxGENPADDR " failed",
        //          bits, minbase, maxlimit);
        *cap = NULL_CAP;
    }

    /* Reply */
    err = b->tx_vtbl.allocate_response(b, MKCONT(allocate_response_done, cap),
                                       ret, *cap);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct pending_reply *r = malloc(sizeof(struct pending_reply));
            assert(r != NULL);
            r->b = b;
            r->err = ret;
            r->cap = cap;
            err = b->register_send(b, get_default_waitset(), MKCONT(retry_reply,r));
            assert(err_is_ok(err));
        } else {
            DEBUG_ERR(err, "failed to reply to memory request");
            allocate_response_done(cap);
        }
    }
}

static void dump_ram_region(int idx, struct mem_region* m)
{
#if 0
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
           " - 0x%" PRIxPTR " (%zu %cB, %u bits)\n",
           idx, start, limit, quantity, prefix, m->mr_bits);
#endif // 0
}

static genpaddr_t find_smallest_address(void)
{
    bool isFirst = true;
    genpaddr_t smallest_addr = 0;
//
    for (int i = 0; i < bi->regions_length; i++) {
        if (bi->regions[i].mr_type != RegionType_Empty) {
            continue;
        }

        if (bi->regions[i].mr_consumed) {
            continue;
        }

        if (isFirst) {
            smallest_addr = bi->regions[i].mr_base;
            isFirst = false;
            continue;
        }

        if (smallest_addr > bi->regions[i].mr_base) {
            smallest_addr = bi->regions[i].mr_base;
        }
    } // end for: for every record
    return smallest_addr;
} // end function: find_smallest_address

static genpaddr_t guess_physical_addr_start(void)
{
    genpaddr_t start_physical = find_smallest_address();
#if defined(__arm__)
    if (start_physical > 0x80000000) {
        // This is most probably a pandaboard!
        start_physical = 0x80000000;
    } else {
        // This is gem5 or some other architecture
        start_physical = 0;
    }
#else
    start_physical = 0;
#endif
    return start_physical;
} // end function: guess_physical_addr_start

// FIXME: error handling (not asserts) needed in this function
//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower
#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline)) errval_t
#else
static errval_t
#endif
initialize_ram_alloc(void)
{
    errval_t err;

    /* Initialize slot allocator by passing a cnode cap for it to start with */
    struct capref cnode_cap;
    err = slot_alloc(&cnode_cap);
    assert(err_is_ok(err));
    struct capref cnode_start_cap = { .slot  = 0 };

    struct capref ram;
    err = ram_alloc_fixed(&ram, BASE_PAGE_BITS, 0, 0);
    assert(err_is_ok(err));
    err = cnode_create_from_mem(cnode_cap, ram, &cnode_start_cap.cnode,
                              DEFAULT_CNODE_BITS);
    assert(err_is_ok(err));

    /* location where slot allocator will place its top-level cnode */
    struct capref top_slot_cap = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_MODULECN, // XXX: we don't have the module CNode
    };

    /* init slot allocator */
    err = slot_prealloc_init(&ram_slot_alloc, top_slot_cap, MAXCHILDBITS,
                           CNODE_BITS, cnode_start_cap,
                           1UL << DEFAULT_CNODE_BITS, &mm_ram);
    assert(err_is_ok(err));

    err = mm_init(&mm_ram, ObjType_RAM, guess_physical_addr_start(),
                MAXSIZEBITS, MAXCHILDBITS, NULL,
                slot_alloc_prealloc, &ram_slot_alloc, true);
    assert(err_is_ok(err));

    /* give MM allocator static storage to get it started */
    static char nodebuf[SLAB_STATIC_SIZE(MINSPARENODES, MM_NODE_SIZE(MAXCHILDBITS))];
    slab_grow(&mm_ram.slabs, nodebuf, sizeof(nodebuf));

    /* walk bootinfo and add all unused RAM caps to allocator */
    struct capref mem_cap = {
        .cnode = cnode_super,
        .slot = 0,
    };

    for (int i = 0; i < bi->regions_length; i++) {
        if (bi->regions[i].mr_type == RegionType_Empty) {
            dump_ram_region(i, bi->regions + i);

            mem_total += ((size_t)1) << bi->regions[i].mr_bits;

            if (bi->regions[i].mr_consumed) {
                // region consumed by init, skipped
                mem_cap.slot++;
                continue;
            }

            err = mm_add(&mm_ram, mem_cap, bi->regions[i].mr_bits,
                         bi->regions[i].mr_base);
            if (err_is_ok(err)) {
                mem_avail += ((size_t)1) << bi->regions[i].mr_bits;
            } else {
                DEBUG_ERR(err, "Warning: adding RAM region %d (%p/%d) FAILED",
                          i, bi->regions[i].mr_base, bi->regions[i].mr_bits);
            }

            /* try to refill slot allocator (may fail if the mem allocator is empty) */
            err = slot_prealloc_refill(mm_ram.slot_alloc_inst);
            if (err_is_fail(err) && err_no(err) != MM_ERR_SLOT_MM_ALLOC) {
                DEBUG_ERR(err, "in slot_prealloc_refill() while initialising"
                               " memory allocator");
                abort();
            }

            /* refill slab allocator if needed and possible */
            if (slab_freecount(&mm_ram.slabs) <= MINSPARENODES
                && mem_avail > (1UL << (CNODE_BITS + OBJBITS_CTE)) * 2
                                + 10 * BASE_PAGE_SIZE) {
                slab_default_refill(&mm_ram.slabs); // may fail
            }
            mem_cap.slot++;
        }
    }

    err = slot_prealloc_refill(mm_ram.slot_alloc_inst);
    if (err_is_fail(err)) {
        printf("Fatal internal error in RAM allocator: failed to initialise "
               "slot allocator\n");
        DEBUG_ERR(err, "failed to init slot allocator");
        abort();
    }

    printf("RAM allocator initialised, %zd MB (of %zd MB) available\n",
           mem_avail / 1024 / 1024, mem_total / 1024 / 1024);

    return SYS_ERR_OK;
}

static void export_callback(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    struct monitor_binding *mb = get_monitor_binding();
    err = mb->tx_vtbl. set_mem_iref_request(mb, NOP_CONT, iref);
    assert(err_is_ok(err));
}

static struct mem_rx_vtbl rx_vtbl = {
    .allocate_call = mem_allocate_handler,
    .available_call = mem_available_handler,
    .free_monitor_call = mem_free_handler,
};

static errval_t connect_callback(void *st, struct mem_binding *b)
{
    b->rx_vtbl = rx_vtbl;
    // TODO: set error handler
    return SYS_ERR_OK;
}

int main(int argc, char ** argv)
{
    errval_t err;
    struct waitset *ws = get_default_waitset();

    if(argc < 2) {
        fprintf(stderr, "Usage: %s <bootinfo_location>\n", argv[0]);
        return EXIT_FAILURE;
    }

    // First argument contains the bootinfo location
    bi = (struct bootinfo*)strtol(argv[1], NULL, 10);

    /* construct special-case LMP connection to monitor */
    static struct monitor_lmp_binding mcb;
    set_monitor_binding(&mcb.b);

    err = monitor_client_lmp_accept(&mcb, ws, DEFAULT_LMP_BUF_WORDS);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_client_lmp_accept");
    }

    idc_init();

    /* Send the cap for this endpoint to init, who will pass it to
       the monitor */
    err = lmp_ep_send0(cap_initep, 0, mcb.chan.local_cap);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "lmp_ep_send0");
    }

    // XXX: handle messages (ie. block) until the monitor binding is ready
    while (capref_is_null(mcb.chan.remote_cap)) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch while waiting for monitor");
            return EXIT_FAILURE;
        }
    }

    /* Initialize our own memory allocator */
    err = ram_alloc_set(mymm_alloc);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "ram_alloc_set");
    }

    err = initialize_ram_alloc();
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "initialize_ram_alloc");
    }

    /* Initialize self slot_allocator */
    err = multi_slot_alloc_init(&msa, DEFAULT_CNODE_SLOTS, NULL);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "multi_slot_alloc_init");
    }

    err = mem_export(NULL, export_callback, connect_callback, ws,
                     IDC_EXPORT_FLAGS_DEFAULT);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "mem_export");
    }

    /* initialise tracing */
#if defined(TRACING_EXISTS) && defined(CONFIG_TRACE)
    err = trace_my_setup();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "initialising tracing");
        // return EXIT_FAILURE;
    }
    trace_init_disp();
#endif

    // handle messages on this thread
    while (true) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in main event_dispatch loop");
            return EXIT_FAILURE;
        }
    }
}
