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

#include "internal.h"

struct timestamp *timestamp;
static callback retcb;
static coreset_token_t token;
static coreid_t id;
static int counter;

void ping(struct boot_perfmon_binding *b, cycles_t time)
{
    errval_t err;
    err = boot_perfmon_pong__tx(b, NOP_CONT, time);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending ping failed");
    }
}

void pong(struct boot_perfmon_binding *b, cycles_t time)
{
    errval_t err;

    timestamp[id].time[counter] = bench_tsc() - time - bench_tscoverhead();
    counter++;

    if (counter != MAX_COUNT) { /* Continue with same core */
        err = boot_perfmon_ping__tx(b, NOP_CONT, bench_tsc());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sending ping failed");
        }

    } else { /* Run tests with another coreid */

        err = coreset_get_next(set, &token, &id);
        if (err_is_fail(err)) {
            if (err_no(err) == LIB_ERR_CORESET_GET_NEXT_DONE) {
                /* Done running tests with all cores */
                retcb();
                return;
            } else {
                USER_PANIC_ERR(err, "coreset_get_next failed");
            }
        }

        struct boot_perfmon_binding *nb;
        err = relations_get(rel, id, &nb);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "relations_iterate failed");
        }

        counter = 0;
        err = boot_perfmon_ping__tx(nb, NOP_CONT, bench_tsc());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sending ping failed");
        }
    }
}

/**
 * \brief Run tests
 *
 * Only called on the leader
 */
errval_t tests(callback cb)
{
    errval_t err;
    retcb = cb;

    /* Allocate space to store data */
    timestamp = malloc(sizeof(struct timestamp) * coreset_count(set));
    if (!timestamp) {
        return LIB_ERR_MALLOC_FAIL;
    }

    /* Get the next core id to run the test with */
    token = CORESET_INIT_TOKEN;
    err = coreset_get_next(set, &token, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "coreset_get_next failed");
    }

    // Make sure it isn't my_core_id
    if (id == my_core_id) {
        err = coreset_get_next(set, &token, &id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "coreset_get_next failed");
        }
    }

    // Look up the binding
    struct boot_perfmon_binding *b;
    err = relations_get(rel, id, &b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "relations_iterate failed");
    }

    // Start the test
    counter = 0;
    err = boot_perfmon_ping__tx(b, NOP_CONT, bench_tsc());
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending ping failed");
    }

    return SYS_ERR_OK;
}
