/**
 * \file
 * \brief Microbenchmark to measure the cost of shared memory clock
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <bench/bench.h>
#include <if/monitor_defs.h>
#include <if/bench_defs.h>
#include "clock.h"

#define MAX_COUNT 1000

static coreid_t my_core_id;
static coreid_t num_cores;
static struct capref clock_frame;
static struct bench_binding *array[MAX_CPUS];
static bool start_experiment_flag;

struct timestamp {
    cycles_t time0;
    cycles_t time1;
};

static struct timestamp timestamps[MAX_COUNT];

static void run_experiment(void)
{
    for (int i = 0; i < MAX_COUNT; i++) {
        timestamps[i].time0 = bench_tsc();
        clock_get_timestamp();
        timestamps[i].time1 = bench_tsc();
    }
}

static void shmc_start(struct bench_binding *b)
{
    run_experiment();
    errval_t err;
    err = b->tx_vtbl.shmc_done(b, NOP_CONT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending shmc_done failed");
    }
}

static bool experiment_flag;
static int experiment_count;
static void shmc_done(struct bench_binding *b)
{
    static int count = 0;

    count++;
    if (count == experiment_count) {
        count = 0;
        experiment_flag = true;
    }
}

static void start_experiment(void)
{
    errval_t err;

    for (int i = 1; i < num_cores; i++) {

        int count = 0;
        experiment_flag = false;
        experiment_count = i;
        for (int j = 0; j < MAX_CPUS; j++) {
            if (array[j]) {
                while(1) {
                    err = array[j]->tx_vtbl.shmc_start(array[j], NOP_CONT);
                    if (err_is_ok(err)) {
                        break;
                    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
                        messages_wait_and_handle_next();
                    } else {
                        USER_PANIC_ERR(err, "sending shmc_start failed");
                    }
                }
                count++;
                if (count == i) {
                    break;
                }
            }
        }
        run_experiment();

        printf("Running on %d cores\n", i + 1);
        for (int j = 0; j < MAX_COUNT; j++) {
            printf("page %d took %ld\n", j,
                   timestamps[j].time1 - timestamps[j].time0 - bench_tscoverhead());
        }

        while(!experiment_flag) {
            messages_wait_and_handle_next();
        }
    }
    printf("client done\n");
}

static void shmc_init_request(struct bench_binding *b, coreid_t id)
{
    array[id] = b;

    errval_t err;
    err = b->tx_vtbl.shmc_init_reply(b, NOP_CONT, clock_frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending shmc_init_reply failed");
    }

    static coreid_t count = 0;
    count++;
    if (count + 1 == num_cores) {
        start_experiment_flag = true;
    }
}

static void shmc_init_reply(struct bench_binding *b, struct capref cap)
{
    clock_frame = cap;
    errval_t err = clock_init(cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "clock_init failed");
    }
}

static struct bench_rx_vtbl rx_vtbl = {
    .shmc_init_request = shmc_init_request,
    .shmc_init_reply   = shmc_init_reply,
    .shmc_start = shmc_start,
    .shmc_done = shmc_done,
};

static void bind_cb(void *st, errval_t err, struct bench_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    b->tx_vtbl.shmc_init_request(b, NOP_CONT, my_core_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending shm_init_request failed");
    }
}

static void num_cores_reply(struct monitor_binding *st, coreid_t num)
{
    num_cores = num;
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    // register this iref with the name service
    err = nameservice_register("server", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_cb(void *st, struct bench_binding *b)
{
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    errval_t err;
    my_core_id = disp_get_core_id();
    bench_init();

    if (argc == 1) { /* server */
        struct monitor_binding *mb = get_monitor_binding();
        mb->rx_vtbl.num_cores_reply = num_cores_reply;

        // Get number of cores in the system
        err = mb->tx_vtbl.num_cores_request(mb, NOP_CONT);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error sending num_core_request");
        }

        // Spawn client on another core
        char *xargv[] = {"shared_mem_clock_bench", "dummy", NULL};
        err = spawn_program_on_all_cores(false, xargv[0], xargv, NULL,
                                         SPAWN_FLAGS_DEFAULT, NULL);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error spawning on other cores");
        }

        // Export service
        err = bench_export(NULL, export_cb, connect_cb, get_default_waitset(),
                          IDC_EXPORT_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "export failed");
        }

        // Allocate a cap for the shared memory
        err = frame_alloc(&clock_frame, BASE_PAGE_SIZE, NULL);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "frame_alloc failed");
        }
        err = clock_init(clock_frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "clock_init failed");
        }

        // Wait for all connections to be established
        start_experiment_flag = false;
        while(!start_experiment_flag) {
            messages_wait_and_handle_next();
        }

        // Start experiments
        start_experiment();

    } else { /* client */
        // Lookup service
        iref_t iref;
        err = nameservice_blocking_lookup("server", &iref);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
        }

        // Bind to service
        err = bench_bind(iref, bind_cb, NULL, get_default_waitset(),
                         IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "bind failed");
        }
    }

    messages_handler_loop();
}
