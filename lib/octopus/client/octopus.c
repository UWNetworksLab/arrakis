/**
 * \file
 * \brief Initialization functions for the octopus client library.
 *
 * We use two bindings: One for communication with the server using RPC calls,
 * and one for asynchronous events coming from the server.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>
#include <barrelfish/nameservice_client.h>

#include <if/monitor_defs.h>
#include <octopus/init.h>
#include <thc/thc.h>

#include "handler.h"
#include "common.h"

static struct oct_state {
    struct octopus_binding* binding;
    struct octopus_thc_client_binding_t thc_client;
    struct waitset ws;
    errval_t err;
    bool is_done;
} rpc, event;

static iref_t service_iref = 0;
static uint64_t client_identifier = 0;
static bool initialized = false;

struct octopus_binding* oct_get_event_binding(void)
{
    assert(event.binding != NULL);
    return event.binding;
}

struct octopus_thc_client_binding_t* oct_get_thc_client(void)
{
    //assert(rpc.rpc_client != NULL);
    return &rpc.thc_client;
}

static void identify_response_handler(struct octopus_binding* b)
{
    event.is_done = true;
}

static struct octopus_rx_vtbl rx_vtbl = {
        .identify_response = identify_response_handler,
        .subscription = subscription_handler,
        .trigger = trigger_handler
};

/*
static int event_handler_thread(void* st)
{
    errval_t err = SYS_ERR_OK;
    struct octopus_binding* b = oct_get_event_binding();

    b->change_waitset(b, &event.ws);

    uint64_t id = (uint64_t) st;
    err = b->tx_vtbl.identify_call(b, NOP_CONT, id, octopus_BINDING_EVENT);
    assert(err_is_ok(err));

    // TODO abort condition
    while (1) {
        err = event_dispatch(&event.ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err,
                    "error in event_dispatch for octopus event binding");
        }
    }

    return SYS_ERR_OK;
}*/

static void event_bind_cb(void *st, errval_t err, struct octopus_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "oct_event bind failed");
        goto out;
    }

    event.binding = b;
    event.binding->rx_vtbl = rx_vtbl;

out:
    assert(!event.is_done);
    event.is_done = true;
    event.err = err;
}

static void get_name_iref_reply(struct monitor_binding *mb, iref_t iref,
                                uintptr_t state)
{
    struct oct_state* ds = (struct oct_state*)state;
    service_iref = iref;
    ds->err = (iref != 0) ? SYS_ERR_OK : LIB_ERR_GET_NAME_IREF;
    ds->is_done = true;
}

static errval_t init_binding(struct oct_state* state,
        octopus_bind_continuation_fn bind_fn)
{
    errval_t err = SYS_ERR_OK;
    assert(service_iref != 0);

    state->is_done = false;
    err = octopus_bind(service_iref, bind_fn, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    //  Wait for callback to complete
    while (!state->is_done) {
        messages_wait_and_handle_next();
    }

    return state->err;
}

static errval_t get_service_iref(void)
{
    errval_t err = SYS_ERR_OK;
    if (service_iref > 0) {
        // we already have the iref
        return err;
    }

    struct monitor_binding *mb = get_monitor_binding();

    rpc.is_done = false;

    mb->rx_vtbl.get_name_iref_reply = get_name_iref_reply;
    err = mb->tx_vtbl.get_name_iref_request(mb, NOP_CONT, (uintptr_t)&rpc);
    if (err_is_fail(err)) {
        return err;
    }

    while (!rpc.is_done) {
        messages_wait_and_handle_next();
    }

    if (err_is_fail(rpc.err)) {
        return rpc.err;
    }

    return err;

}

errval_t oct_thc_init(void)
{
    errval_t err = SYS_ERR_OK;

    err = get_service_iref();
    if (err_is_fail(err)) {
        return err;
    }
    assert(service_iref != 0);

    // XXX: Can't use different waitset here?
    err = octopus_thc_connect(service_iref,
            get_default_waitset(), IDC_BIND_FLAGS_DEFAULT, &(rpc.binding));
    if (err_is_fail(err)) {
        return err;
    }

    assert(rpc.binding != NULL);
    err = octopus_thc_init_client(&rpc.thc_client, rpc.binding, rpc.binding);
    if (err_is_fail(err)) {
        return err;
    }

    // TODO: Hack. Tell the server that these bindings belong together
    octopus_thc_client_binding_t* cl = oct_get_thc_client();
    err = cl->call_seq.get_identifier(cl, &client_identifier);
    if (err_is_fail(err)) {
        return err;
    }

    // Register rpc binding using identifier
    err = cl->call_seq.identify(cl, client_identifier, octopus_BINDING_RPC);

    return err;
}

/**
 * \brief Initializes the octopus client library.
 *
 * Note the octopus rpc binding is most likely already initialized
 * by libbarrelfish (used for nameservice). This function
 * will set up the event thread to handle asynchronous events.
 */
//__attribute__((constructor))
errval_t oct_init(void)
{
    if (initialized) {
        return SYS_ERR_OK;
    }
    initialized = true;

    errval_t err = oct_thc_init();
    if (err_is_fail(err)) {
        return err;
    }

    err = init_binding(&event, event_bind_cb);
    if (err_is_fail(err)) {
        return err;
    }

    // Register event binding
    event.is_done = false;
    event.binding->tx_vtbl.identify_call(event.binding, NOP_CONT,
            client_identifier, octopus_BINDING_EVENT);
    while (!event.is_done) {
        messages_wait_and_handle_next();
    }

    return err;
}
