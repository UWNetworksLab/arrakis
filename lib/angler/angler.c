/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

#include <angler/angler.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/domain.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/waitset.h>
#include <if/monitor_defs.h>
#include <if/terminal_session_defs.h>
#include <if/terminal_session_rpcclient_defs.h>
#include <if/octopus_rpcclient_defs.h>
#include <octopus/getset.h> /* For SET_DEFAULT */
#include <octopus/trigger.h> /* For NOP_TRIGGER */

/**
 * Format of the session record stored at octopus.
 */
#define SESSION_OCT_RECORD "{session_iref: %" PRIuIREF ", in_iref: %" PRIuIREF \
                           ", out_iref: %" PRIuIREF ", conf_iref: %" PRIuIREF  \
                           "}"

/* internal state */
struct _angler_state {
    errval_t err;
    struct terminal_session_binding *binding;
};

/* internal functions */
static void bind_cb(void *st, errval_t err, struct terminal_session_binding *b);
static errval_t store_session_state(struct capref *session_id,
                                    iref_t session_iref, iref_t in_iref,
                                    iref_t out_iref, iref_t conf_iref);

/**
 * \brief Start a new session.
 *
 * \param terminal   Terminal used for the session.
 * \param session_id ID capability representing the session. Filled-in by
 *                   function.
 */
errval_t angler_new_session(char *terminal, struct capref *session_id)
{
    errval_t err;
    iref_t iref;

    assert(terminal != NULL);
    assert(session_id != NULL);

    /* Lookup terminal session iref at nameservice. */
    err = nameservice_blocking_lookup(terminal, &iref);
    if (err_is_fail(err)) {
        return err_push(err, ANGLER_ERR_LOOKUP_TERMINAL);
    }

    return angler_new_session_with_iref(iref, session_id);
}

/**
 * \brief Start a new session.
 *
 * \param session_iref Interface reference of session interface of terminal that
 *                     is used for the session.
 * \param session_id   ID capability representing the session. Filled-in by
 *                     function.
 */
errval_t angler_new_session_with_iref(iref_t session_iref,
                                      struct capref *session_id)
{
    errval_t err = SYS_ERR_OK;
    errval_t error = SYS_ERR_OK;
    struct _angler_state *state = NULL;
    struct terminal_session_rpc_client rpc_client;
    struct waitset ws;
    iref_t in_iref;
    iref_t out_iref;
    iref_t conf_iref;

    assert(session_id != NULL);

    /* Create ID capability used to represent the session. */
    err = idcap_alloc(session_id);
    if (err_is_fail(err)) {
        return err_push(err, ANGLER_ERR_CREATE_SESSIONID);
    }

    /* Initialize internal state */
    state = malloc(sizeof(struct _angler_state));
    assert(state != NULL);
    state->err = SYS_ERR_OK;
    state->binding = NULL;

    /* Bind to terminal session interface */
    waitset_init(&ws);
    err = terminal_session_bind(session_iref, bind_cb, state, &ws,
                                IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        err = err_push(err, ANGLER_ERR_BIND_TERMINAL);
        goto out;
    }

    /* Wait on the monitor until the bind completes. */
    struct monitor_binding *monitor_b = get_monitor_binding();
    struct waitset *monitor_ws = monitor_b->waitset;
    while (state->binding == NULL) {
        err = event_dispatch(monitor_ws);
        if (err_is_fail(err) || err_is_fail(state->err)) {
            err = err_push(err, ANGLER_ERR_BIND_TERMINAL);
            goto out;
        }
    }

    /* Initialize rpc client. */
    err = terminal_session_rpc_client_init(&rpc_client, state->binding);
    if (err_is_fail(err)) {
        err = err_push(err, ANGLER_ERR_INIT_RPCCLIENT);
        goto out;
    }

    /* Associate session with terminal. */
    err = rpc_client.vtbl.session_associate_with_terminal
            (&rpc_client, *session_id, &in_iref, &out_iref, &conf_iref, &error);
    if (err_is_fail(err)) {
        err = err_push(err, ANGLER_ERR_ASSOCIATE_WITH_TERMINAL);
        goto out;
    }
    if (err_is_fail(error)) {
        err = error;
        err = err_push(err, ANGLER_ERR_ASSOCIATE_WITH_TERMINAL);
        goto out;
    }

    /* Store session state at octopus using session id as access control. */
    err = store_session_state(session_id, session_iref, in_iref, out_iref,
                              conf_iref);
    if (err_is_fail(err)) {
        err = err_push(err, ANGLER_ERR_STORE_SESSION_STATE);
    }

out:
    free(state);
    return err;
}

/**
 * \privatesection
 * Internal functions and bind code follow.
 */
static void bind_cb(void *st, errval_t err, struct terminal_session_binding *b)
{
    struct _angler_state *state = st;

    if (err_is_fail(err)) {
        state->err = err;
    } else {
        state->binding = b;
    }
}

static errval_t store_session_state(struct capref *session_id,
                                    iref_t session_iref, iref_t in_iref,
                                    iref_t out_iref, iref_t conf_iref)
{
    errval_t err = SYS_ERR_OK;
    errval_t error;
    struct octopus_rpc_client *rpc_client = NULL;
    char *attributes = NULL;
    size_t attributes_len = 0;
    char *record = NULL;
    octopus_trigger_id_t tid;

    rpc_client = get_octopus_rpc_client();
    assert(rpc_client != NULL);

    /* Build attributes. */
    attributes_len = snprintf(NULL, 0, SESSION_OCT_RECORD, session_iref,
                              in_iref, out_iref, conf_iref);
    attributes = malloc(attributes_len + 1);
    assert(attributes != NULL);
    snprintf(attributes, attributes_len + 1, SESSION_OCT_RECORD, session_iref,
             in_iref, out_iref, conf_iref);


    /* Store record at octopus. */
    err = rpc_client->vtbl.set_with_idcap(rpc_client, *session_id, attributes,
                                           SET_DEFAULT, NOP_TRIGGER, false,
                                           &record, &tid, &error);
    if (err_is_fail(err)) {
        goto out;
    }
    if (err_is_fail(error)) {
        err = error;
        goto out;
    }

out:
    free(attributes);
    return err;
}
