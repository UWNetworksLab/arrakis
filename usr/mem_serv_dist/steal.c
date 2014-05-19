/**
 * \file
 * \brief Distributed (percore) memory server: stealing related code
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
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <mm/mm.h>
#include <if/mem_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>

#include <thc/thc.h>
#include <thc/thcsync.h>

#include <if/mem_defs.h>
#include <if/mem_rpcclient_defs.h>
#include <if/mem_thc.h>

// #include "barrier.h"

#include "mem_serv.h"
#include "steal.h"

/// Keep track of our peer mem_servs
struct peer_core {
    coreid_t id;
    bool is_bound;
    struct mem_thc_client_binding_t cl; 
    thc_lock_t lock;
};

coreid_t mycore;
static struct peer_core *peer_cores;
static int num_peers;

// FIXME: possible race if handling two concurrent alloc request that both 
// try to connect to the same peer
static errval_t connect_peer(struct peer_core *peer)
{
    assert(peer != NULL);

    errval_t err;
    struct mem_binding *b;

    // debug_printf("connecting to %u\n", peer->id);

    char service_name[NAME_LEN];
    snprintf(service_name, NAME_LEN, "%s.%d", MEMSERV_DIST, peer->id);
    
    err = mem_thc_connect_by_name(service_name,
                                  get_default_waitset(),
                                  IDC_BIND_FLAGS_DEFAULT,
                                  &b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not bind (thc)");
        return err;
    }

    err = mem_thc_init_client(&peer->cl, b, b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not init client (thc)");
        return err;
    }

    thc_lock_init(&peer->lock);

    return SYS_ERR_OK;
}

static errval_t steal_from_serv(struct peer_core *peer, 
                                struct capref *ret_cap, 
                                uint8_t bits, 
                                genpaddr_t minbase, 
                                genpaddr_t maxlimit)
{
    assert(peer != NULL);

    errval_t err;

    if (!peer->is_bound) {
      printf("Connecting to new peer\n");
        err = connect_peer(peer);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to connect to peer");
            return err;
        }
        peer->is_bound = true;
    }

    // due to the single-waiter rule of thc we need to make sure we only 
    // ever have one of these rpcs outstanding at a time.
    thc_lock_acquire(&peer->lock);
    peer->cl.call_seq.steal(&peer->cl, bits, minbase, maxlimit, 
                                &err, ret_cap);
    thc_lock_release(&peer->lock);

    return err;
}

// Round-robin steal. Try to get memory from each peer in turn.
static errval_t rr_steal(struct capref *ret_cap, uint8_t bits,
                                genpaddr_t minbase, genpaddr_t maxlimit)
{
    errval_t err = MM_ERR_NOT_FOUND;
    static int next_serv = 0;

    int serv = next_serv;
    struct peer_core *peer = &peer_cores[serv];

    int i;
    for (i = 0; i < num_peers; i++) {
        if (serv >= num_peers) {
            serv = 0;
        }
        peer = &peer_cores[serv++];
        if (peer->id == mycore) {
            continue;
        }
	    printf("%d: Trying to steal from %d/%d\n", disp_get_core_id(), i, num_peers);
        err = steal_from_serv(peer, ret_cap, bits, minbase, maxlimit);
        if (err_is_ok(err)) {
            break;
        }
    }
    next_serv = serv;

    /*
    if (i >= num_peers) {
        debug_printf("rr_steal: could not steal from any peers\n");
    }
    */

    return err; 
}


static errval_t steal_and_alloc(struct capref *ret_cap, uint8_t steal_bits, 
                                uint8_t alloc_bits, 
                                genpaddr_t minbase, genpaddr_t maxlimit)
{
    errval_t err;

    struct capref ramcap;
    struct capability info;

    /*
    debug_printf("steal_and_alloc(steal_bits: %d, alloc_bits: %d, "
                 "minbase: 0x%"PRIxGENPADDR", maxlimit: 0x%"PRIxGENPADDR")\n",
                 steal_bits, alloc_bits, minbase, maxlimit);
    */

    err = rr_steal(&ramcap, steal_bits, minbase, maxlimit);
    if (err_is_fail(err)) {
        return err;
    }

    // XXX: Hack to allow monitor to allocate memory while we call RPC into it
    // These calls should just be avoided...
    struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();
    assert(mc != NULL);
    struct waitset *oldws = monitor_mem_binding->waitset;
    err = monitor_mem_binding->change_waitset(monitor_mem_binding, &mc->rpc_waitset);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "change_waitset");
    }

    // XXX: Mark as local to this core, until we have x-core cap management
    err = monitor_cap_set_remote(ramcap, false);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "Warning: failed to set cap non-remote. "
                  "This memory will leak.");
    }

    err = debug_cap_identify(ramcap, &info);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_CAP_IDENTIFY);
    }

    // XXX: Reset waitset before THC becomes active again
    err = monitor_mem_binding->change_waitset(monitor_mem_binding, oldws);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "change_waitset");
    }

#if 0
    debug_printf("STOLEN cap is type %d Ram base 0x%"PRIxGENPADDR
                 " (%"PRIuGENPADDR") Bits %d\n",
                 info.type, info.u.ram.base, info.u.ram.base, 
                 info.u.ram.bits);
#endif
    if(steal_bits != info.u.ram.bits) {
      printf("asked for %d bits, but got %d bits of type %d\n",
	     steal_bits, info.u.ram.bits, info.type);
    }
    assert(steal_bits == info.u.ram.bits);

    memsize_t mem_to_add = (memsize_t)1 << info.u.ram.bits;

    err = mm_free(&mm_percore, ramcap, info.u.ram.base, info.u.ram.bits);
    if (err_is_fail(err)) {
        if (err_no(err) == MM_ERR_NOT_FOUND) {
            // memory wasn't there initially, add it
            err = mm_add(&mm_percore, ramcap, info.u.ram.bits, info.u.ram.base);
            if (err_is_fail(err)) {
                return err_push(err, MM_ERR_MM_ADD);
            }
            mem_total += mem_to_add;
        } else {
            return err_push(err, MM_ERR_MM_FREE);
        }
    }

    mem_avail += mem_to_add;

    err = percore_alloc(ret_cap, alloc_bits, minbase, maxlimit);    

    return err;
} 


void try_steal(errval_t *ret, struct capref *cap, uint8_t bits,
               genpaddr_t minbase, genpaddr_t maxlimit)
{
    printf("[%d][%"PRIuDOMAINID"]: failed percore alloc request: bits: %d going to STEAL\n",
            disp_get_core_id(), disp_get_domain_id(), bits);
	printf("%p %p %p %p %p %p\n",	__builtin_return_address(0),
								 	__builtin_return_address(1),
									__builtin_return_address(2),
									__builtin_return_address(3),
									__builtin_return_address(4),
									__builtin_return_address(5));
    //DEBUG_ERR(*ret, "allocation of %d bits in 0x%" PRIxGENPADDR 
    //           "-0x%" PRIxGENPADDR " failed", bits, minbase, maxlimit);
    *ret = steal_and_alloc(cap, bits+1, bits, minbase, maxlimit);
    if (err_is_fail(*ret)) {
        DEBUG_ERR(*ret, "stealing of %d bits in 0x%" PRIxGENPADDR "-0x%"
                 PRIxGENPADDR " failed", bits, minbase, maxlimit);
        *cap = NULL_CAP;
    }
//	*ret = MM_ERR_NOT_FOUND;
//	*cap = NULL_CAP;
}

errval_t init_peers(coreid_t core, int len_cores, coreid_t *cores) 
{
    // initialise info about our peers 
    mycore = core;
    num_peers = len_cores;
    peer_cores = malloc(num_peers * sizeof(struct peer_core));
    if (peer_cores == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    
    for (int i = 0; i < num_peers; i++) {
        peer_cores[i].id = cores[i];
        peer_cores[i].is_bound = false;
    }

    return SYS_ERR_OK;
}

errval_t percore_steal_handler_common(uint8_t bits,
                                      genpaddr_t minbase, 
                                      genpaddr_t maxlimit,
                                      struct capref *retcap)
{
    struct capref cap;
    errval_t err, ret;

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_PERCORE_ALLOC, bits);
    /* debug_printf("%d: percore steal request: bits: %d\n", disp_get_core_id(), bits); */

    // refill slot allocator if needed 
    err = slot_prealloc_refill(mm_percore.slot_alloc_inst);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Warning: failure in slot_prealloc_refill\n");
        cap = NULL_CAP;
        return err;
    }

    // refill slab allocator if needed 
    err = slab_refill(&mm_percore.slabs);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Warning: failure when refilling mm_percore slab\n");
    }

    // get actual ram cap 
    ret = percore_alloc(&cap, bits, minbase, maxlimit);
    if (err_is_fail(ret)){
        // debug_printf("percore steal request failed\n");
        //DEBUG_ERR(ret, "allocation of stolen %d bits in 0x%" PRIxGENPADDR 
        //          "-0x%" PRIxGENPADDR " failed", bits, minbase, maxlimit);
        cap = NULL_CAP;
    }

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_PERCORE_ALLOC_COMPLETE, bits);

    *retcap = cap;
    return ret;
}


int main(int argc, char ** argv)
{
    return common_main(argc, argv);
}
