/**
 * \file
 * \brief Code to initialize the octopus server.
 *
 * Sets up bindings and vtables.
 */

/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/octopus_defs.h>
#include <if/monitor_defs.h>

#include <octopus_server/init.h>
#include <octopus_server/service.h>
#include <octopus_server/debug.h>

#define OCT_RPC_SERVICE_NAME "octopus_rpc"

static struct export_state {
    bool is_done;
    errval_t err;
} rpc_export;

static const struct octopus_rx_vtbl rpc_rx_vtbl = {
        .get_names_call = get_names_handler,
        .get_call = get_handler,
        .set_call = set_handler,
        .get_with_idcap_call = get_with_idcap_handler,
        .set_with_idcap_call = set_with_idcap_handler,
        .del_call = del_handler,
        .exists_call = exists_handler,
        .wait_for_call = wait_for_handler,
        .remove_trigger_call = remove_trigger_handler,

        .subscribe_call = subscribe_handler,
        .unsubscribe_call = unsubscribe_handler,
        .publish_call = publish_handler,

        .get_identifier_call = get_identifier,
        .identify_call = identify_binding,

        // Cap storage
        .get_cap_call = get_cap_handler,
        .put_cap_call = put_cap_handler,
        .remove_cap_call = remove_cap_handler,
};

static void rpc_export_cb(void *st, errval_t err, iref_t iref)
{
    rpc_export.is_done = true;
    rpc_export.err = err;

    if (err_is_ok(err)) {
        struct monitor_binding *mb = get_monitor_binding();
        OCT_DEBUG("octopus rpc iref is: %"PRIu32"\n", iref);
        err = mb->tx_vtbl.set_name_iref_request(mb, NOP_CONT, iref);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to send set_name_iref_request to monitor");
        }
    }
}

static errval_t rpc_connect_cb(void *st, struct octopus_binding *b)
{
    // Set up continuation queue
    b->st = NULL;

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rpc_rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

errval_t rpc_server_init(void)
{
    rpc_export.err = SYS_ERR_OK;
    rpc_export.is_done = false;

    errval_t err = octopus_export(&rpc_export, rpc_export_cb, rpc_connect_cb,
            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        return err;
    }

    // XXX: broken
    while (!rpc_export.is_done) {
        messages_wait_and_handle_next();
    }

    return rpc_export.err;
}

/**
 * \brief Sets up bindings for the octopus server and registers them in the
 * nameserver.
 *
 * \retval SYS_ERR_OK
 */
errval_t oct_server_init(void)
{
    errval_t err = rpc_server_init();
    if (err_is_fail(err)) {
        return err;
    }
    err = init_capstorage();

    return err;
}
