/**
 * \file
 * \brief Client for interacting with the name service
 */

/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/octopus_defs.h>
#include <if/octopus_rpcclient_defs.h>
#include <if/monitor_defs.h>
#include <octopus/getset.h> // for oct_read TODO
#include <octopus/trigger.h> // for NOP_TRIGGER

/**
 * \brief Non-blocking name service lookup
 *
 * \param iface Name of interface for which to query name server
 * \param retiref Returns pointer to IREF on success
 */
errval_t nameservice_lookup(const char *iface, iref_t *retiref)
{
    errval_t err;

    struct octopus_rpc_client *r = get_octopus_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    char* record = NULL;
    octopus_trigger_id_t tid;
    errval_t error_code;
    err = r->vtbl.get(r, iface, NOP_TRIGGER, &record, &tid, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;
    if (err_is_fail(err)) {
        if (err_no(err) == OCT_ERR_NO_RECORD) {
            err = err_push(err, LIB_ERR_NAMESERVICE_UNKNOWN_NAME);
        }
        goto out;
    }

    uint64_t iref_number = 0;
    err = oct_read(record, "_ { iref: %d }", &iref_number);
    if (err_is_fail(err) || iref_number == 0) {
        err = err_push(err, LIB_ERR_NAMESERVICE_INVALID_NAME);
        goto out;
    }
    if (retiref != NULL) {
        *retiref = iref_number;
    }

out:
    free(record);
    return err;
}

/**
 * \brief Blocking name service lookup
 *
 * \param iface Name of interface for which to query name server
 * \param retiref Returns pointer to IREF on success
 */
errval_t nameservice_blocking_lookup(const char *iface, iref_t *retiref)
{
    errval_t err;

    struct octopus_rpc_client *r = get_octopus_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    char* record = NULL;
    errval_t error_code;
    err = r->vtbl.wait_for(r, iface, &record, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;
    if (err_is_fail(err)) {
        if (err_no(err) == OCT_ERR_NO_RECORD) {
            err = err_push(err, LIB_ERR_NAMESERVICE_UNKNOWN_NAME);
        }
        goto out;
    }

    uint64_t iref_number = 0;
    err = oct_read(record, "_ { iref: %d }", &iref_number);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_NAMESERVICE_INVALID_NAME);
        goto out;
    }
    if (retiref != NULL) {
        *retiref = iref_number;
    }

out:
    free(record);
    return err;
}

/**
 * \brief Register with name service
 *
 * \param iface Name of interface to register
 * \param iref IREF to register
 */
errval_t nameservice_register(const char *iface, iref_t iref)
{
    errval_t err = SYS_ERR_OK;

    struct octopus_rpc_client *r = get_octopus_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    // Format record
    static const char* format = "%s { iref: %d }";
    size_t len = snprintf(NULL, 0, format, iface, iref);
    char* record = malloc(len+1);
    if (record == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    snprintf(record, len+1, format, iface, iref);

    char* ret = NULL;
    octopus_trigger_id_t tid;
    errval_t error_code;
    err = r->vtbl.set(r, record, 0, NOP_TRIGGER, 0, &ret, &tid, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;

out:
    free(record);
    return err;
}

/* ----------------------- BIND/INIT CODE FOLLOWS ----------------------- */


static void error_handler(struct octopus_binding *b, errval_t err)
{
    USER_PANIC_ERR(err, "asynchronous error in nameservice binding");
}

struct bind_state {
    bool done;
    errval_t err;
};

static void bind_continuation(void *st_arg, errval_t err,
                              struct octopus_binding *b)
{
    struct bind_state *st = st_arg;

    if (err_is_ok(err)) {
        b->error_handler = error_handler;

        struct octopus_rpc_client *r;
        r = malloc(sizeof(struct octopus_rpc_client));
        assert(r != NULL);
        err = octopus_rpc_client_init(r, b);
        if (err_is_fail(err)) {
            free(r);
            USER_PANIC_ERR(err, "error in nameservice_rpc_client_init");
        } else {
            set_octopus_rpc_client(r);
        }
    }

    st->err = err;
    st->done = true;
}

static void get_name_iref_reply(struct monitor_binding *mb, iref_t iref,
                                uintptr_t st_arg)
{
    struct bind_state *st = (void *)st_arg;
    errval_t err;

    if (iref == 0) {
        err = LIB_ERR_GET_NAME_IREF;
    } else {
        err = octopus_bind(iref, bind_continuation, st,
                get_default_waitset(), IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    }

    if (err_is_fail(err)) {
        st->err = err;
        st->done = true;
    }
}

/**
 * \brief Blocking bind to the name service
 *
 * Should be called once only at init time on each dispatcher.
 */
errval_t nameservice_client_blocking_bind(void)
{
    errval_t err;

    struct bind_state st = { .done = false };

    /* fire off a request for the iref for the name service */
    struct monitor_binding *mb = get_monitor_binding();
    mb->rx_vtbl.get_name_iref_reply = get_name_iref_reply;
    err = mb->tx_vtbl.get_name_iref_request(mb, NOP_CONT, (uintptr_t)&st);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_GET_NAME_IREF);
    }

    /* block on the default waitset until we're bound */
    struct waitset *ws = get_default_waitset();
    while (!st.done) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_EVENT_DISPATCH);
        }
    }

    return st.err;
}

