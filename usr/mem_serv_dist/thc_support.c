/**
 * \file
 * \brief Distributed (percore) memory server: code specific to THC version
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
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <barrelfish/monitor_client.h>
#include <dist/barrier.h>

#include <thc/thc.h>
#include <thc/thcsync.h>

#include <if/mem_defs.h>
#include <if/mem_rpcclient_defs.h>
#include <if/mem_thc.h>
#include <if/monitor_defs.h>

// #include "barrier.h"

#include "mem_serv.h"
#include "steal.h"

// The various request handler functions

static void percore_free_handler(struct mem_thc_service_binding_t *sv,
                                 struct capref ramcap, genpaddr_t base,
                                 uint8_t bits)
{
    errval_t ret;
    ret = percore_free_handler_common(ramcap, base, bits);
    sv->send.free_monitor(sv, ret);
}

static void mem_available_handler(struct mem_thc_service_binding_t *sv) 
{
    memsize_t mem_available;
    mem_available = mem_available_handler_common();
    sv->send.available(sv, mem_available);
}


static void percore_steal_handler(struct mem_thc_service_binding_t *sv,
                                     uint8_t bits,
                                     genpaddr_t minbase, genpaddr_t maxlimit)
{
    errval_t ret;
    struct capref cap;
    ret = percore_steal_handler_common(bits, minbase, maxlimit, &cap);
    sv->send.steal(sv, ret, cap);

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_PERCORE_ALLOC_COMPLETE, 0);

}

static void percore_allocate_handler(struct mem_thc_service_binding_t *sv,
                                     uint8_t bits,
                                     genpaddr_t minbase, genpaddr_t maxlimit)
{
    errval_t ret;
    struct capref cap;
    ret = percore_allocate_handler_common(bits, minbase, maxlimit, &cap);
    sv->send.allocate(sv, ret, cap);
    if(!capref_is_null(cap)) {
        ret = cap_delete(cap);
        if(err_is_fail(ret)) {
            DEBUG_ERR(err, "cap_delete after send. This memory will leak.");
        }
    }

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_PERCORE_ALLOC_COMPLETE, 0);
}

// Various startup procedures

static void run_server(struct mem_thc_service_binding_t *sv)
{
    mem_service_msg_t msg;
    bool loop = true;
  
    // this is the bitmap of messages we are interested in receiving
    struct mem_service_selector selector = {
        .allocate = 1,
        .available = 1,
        .free = 1,
        .steal = 1,
    };

    while (loop) {
        // receive any message
        sv->recv_any(sv, &msg, selector);

        // dispatch it
        switch(msg.msg) {
        case mem_allocate:
            percore_allocate_handler(sv, msg.args.allocate.in.bits,
                                     msg.args.allocate.in.minbase,
                                     msg.args.allocate.in.maxlimit);
            break;
        case mem_steal:
            percore_steal_handler(sv, msg.args.allocate.in.bits,
                                     msg.args.allocate.in.minbase,
                                     msg.args.allocate.in.maxlimit);
            break;
        case mem_available:
            mem_available_handler(sv);
            break;
        case mem_free_monitor:
            percore_free_handler(sv, msg.args.free.in.mem_cap); 
            break;
        default:
            debug_printf("unexpected message: %d\n", msg.msg);
            loop = false;
            break;
        }
    }
}

errval_t percore_mem_serv(coreid_t core, coreid_t *cores, 
                                 int len_cores, memsize_t ram)
{
    errval_t err;

    struct waitset *ws = get_default_waitset();

    // Init the memory allocator 
    err = initialize_percore_mem_serv(core, cores, len_cores, ram);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "initializing percore mem_serv");
        return err;
    }

    struct mem_thc_export_info e_info;
    struct mem_thc_service_binding_t *sv;
    struct mem_binding *b;
    iref_t iref;

    char service_name[NAME_LEN];
    snprintf(service_name, NAME_LEN, "%s.%d", MEMSERV_DIST, core);
    
    //    err = mem_thc_export(&e_info, service_name, ws,
    err = mem_thc_export(&e_info, NULL, ws,
                         IDC_EXPORT_FLAGS_DEFAULT,
                         &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "exporting percore mem interface");
        return err;
    }

    struct monitor_binding *mb = get_monitor_binding();
    err = mb->tx_vtbl. set_mem_iref_request(mb, NOP_CONT, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "setting monitor's percore mem_serv iref");
        return err;
    }

    // explicitly tell spawnd to use us
    err = set_local_spawnd_memserv(core);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "setting spawnd.%d's local memserv", core);
        return err;
    }

    // register only after spawnd's local memserv has been set
    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        return err;
    }
    // let the master know we are ready
    err = nsb_register_n(core, MEMSERV_DIST);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nsb_register_n failed");
    }


    do {
    while (true) {

        mem_thc_accept(&e_info, &b);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "thc accept failed");
            continue;
        }

        sv = malloc(sizeof(struct mem_thc_service_binding_t));
        if (sv == NULL) {
            DEBUG_ERR(LIB_ERR_MALLOC_FAIL, "allocating thc service binding"); 
            continue;
        }

        err = mem_thc_init_service(sv, b, b);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "thc init failed");
            continue;
        }

        async run_server(sv);

    }
    } finish;

    // should never reach here
    return SYS_ERR_OK;
}
