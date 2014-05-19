/**
 * \file
 * \brief Simple capability storage
 *
 * Moved from chips in the coordination service in order
 * to get rid of chips. We don't store caps with our
 * get/set API because there is currently no good solution
 * to store caps in the SKB:
 * 1. It's easy for clients to change cap info in SKB and
 *    the server does cap_delete() on the corrupted data
 *    in case a capability is retrieved
 * 2. In case we store it as records we may need to depend
 *    on the implementation of caprefs.
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <if/octopus_defs.h>

#include <octopus_server/init.h>
#include <octopus_server/service.h>
#include <octopus_server/debug.h>

#include <hashtable/hashtable.h>

#include "queue.h"

static struct hashtable *capdb = NULL;

static void get_cap_reply(struct octopus_binding *b,
        struct oct_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.get_cap_response(b, MKCONT(free, ns), ns->cap, ns->error);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "SKB: sending %s failed!", __FUNCTION__);
    }
}

void get_cap_handler(struct octopus_binding *b, char *key)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct capref cap;

    capdb->d.get_capability(&capdb->d, key, &cap);

    if(capcmp(cap, NULL_CAP)) {
        reterr = OCT_ERR_CAP_NAME_UNKNOWN;
    }

    struct oct_reply_state* ns = NULL;
    err = new_oct_reply_state(&ns, get_cap_reply);
    assert(err_is_ok(err));
    ns->cap = cap;
    ns->error = reterr;
    ns->reply(b, ns);

    free(key);
}

static void put_cap_reply(struct octopus_binding *b,
        struct oct_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.put_cap_response(b, MKCONT(free, ns), ns->error);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "SKB: sending %s failed!", __FUNCTION__);
    }
}

void put_cap_handler(struct octopus_binding *b, char *key,
                            struct capref cap)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct capref dbcap;

    capdb->d.get_capability(&capdb->d, key, &dbcap);
    if(!capcmp(dbcap, NULL_CAP)) {
        reterr = OCT_ERR_CAP_OVERWRITE;
        err = cap_delete(cap);
        assert(err_is_ok(err));
        free(key);
    } else {
        int r = capdb->d.put_capability(&capdb->d, key, cap);
        assert(r == 0);
    }

    struct oct_reply_state* ns = NULL;
    err = new_oct_reply_state(&ns, put_cap_reply);
    assert(err_is_ok(err));
    ns->error = reterr;
    ns->reply(b, ns);
}

static void remove_cap_reply(struct octopus_binding *b,
        struct oct_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.remove_cap_response(b, MKCONT(free, ns), ns->error);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "SKB: sending %s failed!", __FUNCTION__);
    }
}

void remove_cap_handler(struct octopus_binding *b, char *key)
{
    errval_t err, reterr = SYS_ERR_OK;

    struct capref cap;
    capdb->d.get_capability(&capdb->d, key, &cap);
    if(capcmp(cap, NULL_CAP)) {
        reterr = OCT_ERR_CAP_NAME_UNKNOWN;
    }
    else {
        cap_delete(cap);
        capdb->d.remove(&capdb->d, key, strlen(key));
    }

    struct oct_reply_state* ns = NULL;
    err = new_oct_reply_state(&ns, remove_cap_reply);
    assert(err_is_ok(err));
    ns->error = reterr;
    ns->reply(b, ns);

    free(key);
}

errval_t init_capstorage(void)
{
    capdb = create_hashtable();
    assert(capdb != NULL);

    return SYS_ERR_OK;
}
