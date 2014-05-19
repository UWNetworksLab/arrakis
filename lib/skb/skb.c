/**
 * \file
 * \brief SKB connection
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h>
#include <if/skb_rpcclient_defs.h>
#include <barrelfish/core_state_arch.h>

/* ------------------------- Connecting to skb ------------------------------ */

static void bind_cb(void *st, errval_t err, struct skb_binding *b)
{
    struct skb_state *skb_state = get_skb_state();

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }

    skb_state->skb = malloc(sizeof(struct skb_rpc_client));
    assert(skb_state->skb != NULL);
    err = skb_rpc_client_init(skb_state->skb, b);
    if (err_is_fail(err)) {
        free(skb_state->skb);
        DEBUG_ERR(err, "error in skb_rpc_client_init");
        abort();
    }

    assert(!skb_state->request_done);
    skb_state->request_done = true;
}

errval_t skb_client_connect(void)
{
    errval_t err;
    iref_t iref;
    struct skb_state *skb_state = get_skb_state();

    err = nameservice_blocking_lookup("skb", &iref);
    if (err_is_fail(err)) {
        return err;
    }

    skb_state->request_done = false;
    err = skb_bind(iref, bind_cb, NULL, get_default_waitset(),
                   IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    /* XXX: wait for connection to complete */
    while (!skb_state->request_done) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}

/* ------------------------- evaluate ------------------------------ */
errval_t skb_evaluate(char *query, char **result, char **str_error, int32_t *int_error)
{
    errval_t err;
    struct skb_state *skb_state = get_skb_state();

    err = skb_state->skb->vtbl.run(skb_state->skb, query, result, str_error,
                                   int_error);
    if (err_is_fail(err)) {
        return err_push(err, SKB_ERR_RUN);
    }
    return SYS_ERR_OK;
}
