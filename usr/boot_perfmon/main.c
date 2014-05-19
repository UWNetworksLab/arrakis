/**
 * \file
 * \brief Spawns an instance of self on every core,
 * runs boot time performance tests,
 * adds the results to skb.
 */

/*
 * Copyright (c) 2010, ETH Zurich and Mircosoft Corporation.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include "internal.h"

int global_argc;
char **global_argv;
coreid_t my_core_id;

void exit_msg(struct boot_perfmon_binding *b)
{
    exit(0);
}

static void tests_cb(void)
{
    errval_t err;

    for (int i = 0; i < coreset_count(set); i++) {
        if (i == my_core_id) {
            continue;
        }

        // Have the other nodes exit
        struct boot_perfmon_binding *b;
        err = relations_get(rel, i, &b);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "relations_get failed");
        }
        err = boot_perfmon_exit__tx(b, NOP_CONT);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sending exit failed");
        }

        // Print results
        printf("With core %d, avg %ld, variance %ld\n", i,
               bench_avg(timestamp[i].time, MAX_COUNT),
               bench_variance(timestamp[i].time, MAX_COUNT));
    }

    exit(0);
}

static void connect_cb(void)
{
    errval_t err;

    /* Perform tests */
    if (check_leader()) {
        err = tests(tests_cb);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "connect failed");
        }
    }
}

static void spawn_cb(void)
{
    errval_t err;

    /* Establish connections */
    err = connect(connect_cb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "connect failed");
    }
}

static void leader_cb(void)
{
    errval_t err;

    /* Spawn */
    err = spawn(spawn_cb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawn failed");
    }
}

int main(int argc, char *argv[])
{
    errval_t err;

    global_argc = argc;
    global_argv = argv;
    my_core_id = disp_get_core_id();
    bench_init();

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "skb_client_connect failed");
    }

    /* Set leader */
    err = set_leader(leader_cb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "set_leader failed");
    }

    messages_handler_loop();
}
