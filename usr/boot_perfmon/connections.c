/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich and Mircosoft Corporation.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <ctype.h>
#include "internal.h"
#include <barrelfish/nameservice_client.h>

struct bind_state {
    coreid_t id;
};

struct coreset *set;
struct relations *rel;
static callback retcb;

static void done_reply(struct boot_perfmon_binding *b)
{
    retcb();
}

static errval_t done_msging(void *st, coreid_t id,
                            struct boot_perfmon_binding *b)
{
    return b->tx_vtbl.done_reply(b, NOP_CONT);
}

static void done_request(struct boot_perfmon_binding *b)
{
    errval_t err;
    static coreid_t count = 0;
    count++;

    if (count == (coreset_count(set) - 1)) {
        err = relations_iterate(rel, NULL, done_msging);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "relations_iterate failed");
        }

        retcb();
    }
}

static void init(struct boot_perfmon_binding *b, coreid_t id)
{
    errval_t err;

    err = relations_add(rel, id, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "relations_add failed");
    }

    // Check if done connecting
    if (relations_count(rel) == (coreset_count(set) - 1)) {

        if (!check_leader()) { // Message leader that connect is done
            struct boot_perfmon_binding *lb;
            err = relations_get(rel, get_leader_id(), &lb);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "relations_get failed");
            }
            err = lb->tx_vtbl.done_request(lb, NOP_CONT);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "sending done failed");
            }
        }
    }
}

static struct boot_perfmon_rx_vtbl vtbl = {
    .init = init,
    .done_reply = done_reply,
    .done_request = done_request,
    .ping = ping,
    .pong = pong,
    .exit = exit_msg,
};

static void bind_cb(void *st, errval_t err, struct boot_perfmon_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failure in bind_cb");
    }
    b->rx_vtbl = vtbl;

    struct bind_state *bs = st;
    err = relations_add(rel, bs->id, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "relations_add failed");
    }
    free(bs);

    err = b->tx_vtbl.init(b, NOP_CONT, my_core_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending init failed");
    }

    // Check if done connecting
    if (relations_count(rel) == (coreset_count(set) - 1)) {

        if (!check_leader()) { // Message leader that connect is done
            struct boot_perfmon_binding *lb;
            err = relations_get(rel, get_leader_id(), &lb);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "relations_get failed");
            }
            err = lb->tx_vtbl.done_request(lb, NOP_CONT);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "sending done failed");
            }
        }
    }
}

/**
 * \brief Connect to every core with id bigger than mine
 */
static errval_t iter_connect(void *st, coreid_t id)
{
    errval_t err;

    if (my_core_id > id) {
        iref_t iref;
        char name[128];
        snprintf(name, 128, "boot_perfmon:%d", id);
        err = nameservice_blocking_lookup(name, &iref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "nameservice_blocking_lookup failed");
            abort();
        }

        struct bind_state *bs = malloc(sizeof(struct bind_state));
        if (!bs) {
            USER_PANIC("malloc failed");
        }
        bs->id = id;
        err = boot_perfmon_bind(iref, bind_cb, bs, get_default_waitset(),
                                IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "boot_perfmon_bind failed");
        }
    }

    return SYS_ERR_OK;
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    char name[128];
    snprintf(name, 128, "boot_perfmon:%d", my_core_id);
    err = nameservice_register(name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_cb(void *st, struct boot_perfmon_binding *b)
{
    b->rx_vtbl = vtbl;
    return SYS_ERR_OK;
}

/**
 * \brief Find and add all coreids to coreset
 */
static void set_skb_present(char *str)
{
    errval_t err;

    while (*str != '\0') {
        if (!isdigit((int)*str)) {
            str++;
            continue;
        }

        coreid_t id = strtol(str, &str, 10);
        err = coreset_add(set, id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "coreset_add failed");
        }
    }
}

errval_t connect(callback cb)
{
    errval_t err;
    retcb = cb;

    /* Initialize the coreset */
    err = coreset_new(&set);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CORESET_NEW);
    }

    /* Initialize the relations */
    err = relations_new(&rel);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    /* Get the list of coreids in the system and add them to the coreset */
    char *result, *str_err;
    int int_err;
    err = skb_evaluate("get_core_id_list(L),write(L).",
                       &result, &str_err, &int_err);
    if (err_is_fail(err)) {
        return err_push(err, SKB_ERR_EVALUATE);
    }
    set_skb_present(result);
    free(result);
    free(str_err);

    /* Export service */
    err = boot_perfmon_export(NULL, export_cb, connect_cb,
                              get_default_waitset(),
                              IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    /* Connect */
    err = coreset_iterate(set, NULL, iter_connect);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "coreset_add failed");
    }

    return SYS_ERR_OK;
}
