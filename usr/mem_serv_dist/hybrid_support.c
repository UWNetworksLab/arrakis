/**
 * \file
 * \brief Distributed (percore) memory server: code specific to hybrid version
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
#include <barrelfish/spawn_client.h>
#include <barrelfish/nameservice_client.h>
#include <thc/thc.h>

#include <dist/barrier.h>

#include <if/mem_defs.h>
#include <if/mem_rpcclient_defs.h>
#include <if/monitor_defs.h>

// #include "barrier.h"

#include "mem_serv.h"
#include "steal.h"

/// state for a pending reply
// because we have only one message that we send to a client, and there can only
// be one outstanding per binding (because this is an RPC interface) this is
// quite simple
struct pending_reply {
    struct mem_binding *b;
    struct capref *acap, cap;
    memsize_t mem_avail, mem_total;
    errval_t err;
};


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

// The various send retry functions

static void retry_allocate_reply(void *arg)
{
    struct pending_reply *r = arg;
    assert(r != NULL);
    struct mem_binding *b = r->b;
    errval_t err;

    err = b->tx_vtbl.allocate_response(b, MKCONT(allocate_response_done, r->acap),
                                       r->err, *r->acap);
    if (err_is_ok(err)) {
        b->st = NULL;
        free(r);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = b->register_send(b, get_default_waitset(), 
                               MKCONT(retry_allocate_reply,r));
    }

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to reply to memory request");
        allocate_response_done(r->acap);
        free(r);
    }
}

static void retry_steal_reply(void *arg)
{
    struct pending_reply *r = arg;
    assert(r != NULL);
    struct mem_binding *b = r->b;
    errval_t err;
    
    err = b->tx_vtbl.steal_response(b, NOP_CONT, r->err, r->cap);
    if (err_is_ok(err)) {
        b->st = NULL;
        free(r);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = b->register_send(b, get_default_waitset(), 
                               MKCONT(retry_steal_reply,r));
    }
    
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to reply to steal request");
        free(r);
    }
}

static void retry_available_reply(void *arg)
{
    struct pending_reply *r = arg;
    assert(r != NULL);
    struct mem_binding *b = r->b;
    errval_t err;
    
    err = b->tx_vtbl.available_response(b, NOP_CONT, r->mem_avail, r->mem_total);
    if (err_is_ok(err)) {
        b->st = NULL;
        free(r);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = b->register_send(b, get_default_waitset(), 
                               MKCONT(retry_available_reply,r));
    }
    
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to reply to mem_available request");
        free(r);
    }
}



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


// The various request handler functions

static void percore_free_handler(struct mem_binding *b,
                                 struct capref ramcap,
                                 genpaddr_t base, uint8_t bits)
{
    errval_t ret;

    /* printf("%d: percore_free_handler, base = %" PRIxGENPADDR ", bits = %u\n", */
    /*        disp_get_core_id(), base, bits); */

    ret = percore_free_handler_common(ramcap, base, bits);

    errval_t err;
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
    memsize_t mem_available;
    mem_available = mem_available_handler_common();

    errval_t err;
    err = b->tx_vtbl.available_response(b, NOP_CONT, mem_available, mem_total);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct pending_reply *r = malloc(sizeof(struct pending_reply));
            assert(r != NULL);
            r->b = b;
            r->mem_avail = mem_available;
            r->mem_total = mem_total;
            err = b->register_send(b, get_default_waitset(), 
                                   MKCONT(retry_available_reply,r));
            assert(err_is_ok(err));
        } else {
            DEBUG_ERR(err, "failed to reply to mem_available request");
        }
    }
}


static void percore_steal_handler(struct mem_binding *b,
                                     uint8_t bits,
                                     genpaddr_t minbase, genpaddr_t maxlimit)
{
    errval_t ret;
    struct capref cap;
    ret = percore_steal_handler_common(bits, minbase, maxlimit, &cap);

    errval_t err;
    err = b->tx_vtbl.steal_response(b, NOP_CONT, ret, cap);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct pending_reply *r = malloc(sizeof(struct pending_reply));
            assert(r != NULL);
            r->b = b;
            r->err = ret;
            r->cap = cap;
            err = b->register_send(b, get_default_waitset(), 
                                   MKCONT(retry_steal_reply,r));
            assert(err_is_ok(err));
        } else {
            DEBUG_ERR(err, "failed to reply to steal request");
        }
    }

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_PERCORE_ALLOC_COMPLETE, 0);
}

static void percore_allocate_handler(struct mem_binding *b,
                                     uint8_t bits,
                                     genpaddr_t minbase, genpaddr_t maxlimit)
{
    errval_t ret;
    struct capref *cap = malloc(sizeof(struct capref));
    ret = percore_allocate_handler_common(bits, minbase, maxlimit, cap);

    errval_t err;
    err = b->tx_vtbl.allocate_response(b, MKCONT(allocate_response_done, cap),
                                       ret, *cap);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct pending_reply *r = malloc(sizeof(struct pending_reply));
            assert(r != NULL);
            r->b = b;
            r->err = ret;
            r->acap = cap;
            err = b->register_send(b, get_default_waitset(), 
                                   MKCONT(retry_allocate_reply,r));
            assert(err_is_ok(err));
        } else {
            DEBUG_ERR(err, "failed to reply to memory request");
            allocate_response_done(cap);
        }
    }

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_MEMSERV_PERCORE_ALLOC_COMPLETE, 0);
}


// Various startup procedures

static bool memserv_exported = false;
static iref_t myiref;
struct mem_binding *monitor_mem_binding = NULL;

static void percore_export_callback(void *st, errval_t err, iref_t iref)
{

    assert(err_is_ok(err));

    assert(iref != 0);
    iref_t percore_mem_serv_iref = iref;

    struct monitor_binding *mb = get_monitor_binding();
    err = mb->tx_vtbl.set_mem_iref_request(mb, NOP_CONT, percore_mem_serv_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "registering iref with monitor");
    }

    myiref = iref;
    memserv_exported = true;
}


static struct mem_rx_vtbl percore_rx_vtbl = {
    .allocate_call = percore_allocate_handler,
    .available_call = mem_available_handler,
    .free_monitor_call = percore_free_handler,
    .steal_call = percore_steal_handler,
};

static errval_t percore_connect_callback(void *st, struct mem_binding *b)
{
    // Remember monitor's binding to this mem_serv
    if(memserv_exported && monitor_mem_binding == NULL) {
        monitor_mem_binding = b;
    }

    b->rx_vtbl = percore_rx_vtbl;
    return SYS_ERR_OK;
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

    err = mem_export(NULL, percore_export_callback, percore_connect_callback, 
                     ws, IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "exporting percore mem interface");
        return err;
    }

    while (!memserv_exported) {
        messages_wait_and_handle_next();
    }

    // the following has to be outside the export_cb since it might do
    // a bind, and invokes an RPC

    // explicitly tell spawnd to use us
    err = set_local_spawnd_memserv(core);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "setting spawnd's local memserv interface");
        return err;
    }

    // and this has to be done after spawnd's local memserv has been set,
    // so also not in the export_cb
    char service_name[NAME_LEN];
    snprintf(service_name, NAME_LEN, "%s.%d", MEMSERV_DIST, core);
    err = nameservice_register(service_name, myiref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
    // let the master know we are ready
    err = nsb_register_n(core, MEMSERV_DIST);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nsb_register_n failed");
    }

    // Enter main dispatcher loop
    THCFinish();

    assert(!"Should never return");
    abort();
}
