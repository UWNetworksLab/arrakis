/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/bcache_defs.h>
#include <vfs/vfs.h>
#include "bcached.h"

#include <string.h>
#include <hashtable/hashtable.h>

#define SERVICE_BASENAME        "bcache"

#define ITERATIONS      100000
#define MAXN            10

struct wait_list {
    struct bcache_binding *b;
    struct wait_list *next;
};

#if 0
// Doing the easiest thing here, just block out everyone when we're writing
static bool inwrite[NUM_BLOCKS];
static struct bcache_binding *waiting[NUM_BLOCKS];
#endif

static void get_start_handler(struct bcache_binding *b, char *key, size_t key_len)
{
    errval_t err;
    key_state_t ks;
    uintptr_t idx, length = 0;

    assert(key > (char *)BASE_PAGE_SIZE);

    ks = cache_lookup(key, key_len, &idx, &length);

    if (ks == KEY_INTRANSIT) { // key is in transit: wait for it!
        free(key);
        cache_register_wait(idx, b);
        return; // get_start_response() will be called when key arrives
    } else if (ks == KEY_MISSING) {
        idx = cache_allocate(key, key_len);
    } else if (ks == KEY_EXISTS) {
        free(key);
    } else {
        assert(0);
    }

#if 0
    // Block everyone if we have a write on this block
    if(inwrite[idx]) {
        struct wait_list *w = malloc(sizeof(struct wait_list));
        w->b = b;
        w->next = waiting[idx];
        waiting[idx] = w;
    }

    if(write) {
        inwrite[idx] = true;
    }
#endif

    bool haveit = (ks == KEY_EXISTS);
    err = b->tx_vtbl.get_start_response(b, NOP_CONT, idx, haveit,
                                        haveit ? 1 : 0, length);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_start_response");
    }
}

static void get_stop_handler(struct bcache_binding *b, uint64_t transid,
                             uint64_t idx, uint64_t length)
{
    errval_t err;

    if(transid == 0) {
        cache_update(idx, length);
    }

#if 0
    if(inwrite[idx]) {
        // Wake up all waiters
        
        inwrite[idx] = false;
    }
#endif

    /* notify issuer */
    err = b->tx_vtbl.get_stop_response(b, NOP_CONT);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_stop_response");
    }

    /* notify waiters */
    if (transid == 0) {
            struct bcache_binding *wb;
            while ((wb = cache_get_next_waiter(idx)) != NULL) {
                uint64_t l = cache_get_block_length(idx);
                err = b->tx_vtbl.get_start_response(wb, NOP_CONT, idx, true, 1, l);
                if(err_is_fail(err)) {
                    USER_PANIC_ERR(err, "get_start_response");
                }
            }
    }
}

static void new_client_handler(struct bcache_binding *b)
{
    errval_t err;
    struct bcache_state *st = b->st;

    err = bulk_init(cache_pool, cache_size, block_size, &st->bt);
    assert(err_is_ok(err));

    err = b->tx_vtbl.new_client_response(b, NOP_CONT, cache_memory);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "new_client_reply");
    }
}

static void print_stats_handler(struct bcache_binding *b)
{
    print_stats();

    errval_t err = b->tx_vtbl.print_stats_response(b, NOP_CONT);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "print_stats_response");
    }
}

static struct bcache_rx_vtbl rx_vtbl = {
    .get_start_call = get_start_handler,
    .get_stop_call = get_stop_handler,
    .new_client_call = new_client_handler,
    .print_stats_call = print_stats_handler,
};

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    // construct name
    char namebuf[32];
#ifndef WITH_SHARED_CACHE
    int name = disp_get_core_id();
#else
    int name = 0;
#endif
    size_t len = snprintf(namebuf, sizeof(namebuf), "%s.%d", SERVICE_BASENAME,
                          name);
    assert(len < sizeof(namebuf));
    namebuf[sizeof(namebuf) - 1] = '\0';

    // register this iref with the name service
    err = nameservice_register(namebuf, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_cb(void *st, struct bcache_binding *b)
{
    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;
    b->st = malloc(sizeof(struct bcache_state));
    assert(b->st != NULL);

    return SYS_ERR_OK;
}

errval_t start_service(void)
{
    return bcache_export(NULL, export_cb, connect_cb, get_default_waitset(),
                         IDC_EXPORT_FLAGS_DEFAULT);
}
