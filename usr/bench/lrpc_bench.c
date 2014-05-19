/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <bench/bench.h>
#include <if/bench_defs.h>

// for cache benchmark
#include <barrelfish/sys_debug.h>
#include <arch/x86/barrelfish/perfmon.h>
#include <arch/x86/barrelfish_kpi/perfmon_amd.h>

static bool cache_benchmark;
static bool start_benchmark_flag = false;
static struct bench_binding *binding;
static struct lmp_chan *chan;

#define ITERATIONS      1000

struct timestamps {
    uint64_t time0;
    uint64_t time1;
};
static struct timestamps timestamps[ITERATIONS];
static struct timestamps overhead[ITERATIONS];
static int currentiter;

// server side handler
static void lrpc_bench_handler(void *arg)
{
    errval_t err;
    uint64_t val;
    if (cache_benchmark) {
        val = rdpmc(0);
    } else {
        val = bench_tsc();
    }

    // try to pull a message out of the channel
    struct lmp_recv_msg msg = LMP_RECV_MSG_INIT;
    err = lmp_chan_recv(chan, &msg, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "endpoint poll failed");
        printf("endpoint_poll failed\n");
        abort();
    }

    // send back reply
    struct bench_binding *b = arg;
    err = b->tx_vtbl.lrpc_bench_reply(b, NOP_CONT, val);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error sending back reply");
        printf("lrpc_bench_reply failed\n");
        abort();
    }

    // re-register
    struct event_closure ec = {
        .handler = lrpc_bench_handler,
        .arg = b,
    };
    lmp_chan_register_recv(chan, get_default_waitset(), ec);
}

static void lrpc_init(struct bench_binding *b)
{
    binding = b;
    chan = &((struct bench_lmp_binding *)b)->chan;
}

// client side handler
static void lrpc_bench_reply_handler(struct bench_binding *b,
                                     uint64_t value)
{
    timestamps[currentiter].time1 = value;
}

static void lrpc_init_reply(struct bench_binding *b)
{
    chan = &((struct bench_lmp_binding *)b)->chan;
    start_benchmark_flag = true;
}

/// server and client rx vtbl
static struct bench_rx_vtbl rx_vtbl = {
    .lrpc_init        = lrpc_init,
    .lrpc_init_reply  = lrpc_init_reply,
    .lrpc_bench_reply = lrpc_bench_reply_handler,
};

/// Called on the client side when client connected to the server
static void bind_cb(void *st, errval_t err, struct bench_binding *b)
{
    assert(err_is_ok(err));
    b->rx_vtbl = rx_vtbl;

    err = b->tx_vtbl.lrpc_init(b, NOP_CONT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "lrpc_init_reply failed");
        printf("lrpc_init_reply failed\n");
        abort();
    }
}

static void lrpc_benchmark(uint64_t event, uint64_t umask)
{
    errval_t err;

    // check that we have a slot in the root cnode
    if (chan->remote_cap.cnode.address != CPTR_ROOTCN) {
        printf("lrpc benchmark: not in root CN\n");
        abort();
    }

    if (cache_benchmark) {
        perfmon_setup(curdispatcher(), 0, event, umask, true);

        /* Measure measurement overhead */
        for (size_t i = 0; i < ITERATIONS; i++) {
            overhead[i].time0 = rdpmc(0);
            sys_debug_flush_cache();
            overhead[i].time1 = rdpmc(0);
        }
    }

    for(currentiter = 0; currentiter < ITERATIONS; currentiter++) {
        // yield to receiver to make sure they are ready
        thread_yield_dispatcher(chan->remote_cap);

        if (cache_benchmark) {
            timestamps[currentiter].time0 = rdpmc(0);
            sys_debug_flush_cache();
        } else {
            timestamps[currentiter].time0 = bench_tsc();
        }

        err = lmp_ep_send0(chan->remote_cap, LMP_FLAG_SYNC, NULL_CAP);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "LRPC %d failed", currentiter);
            printf("lmp_ep_send0 failed\n");
            abort();
        }
        while (timestamps[currentiter].time1 == 0) {
            messages_wait_and_handle_next();
        }
    }

    /* Print results */
    for (int i = 0; i < ITERATIONS; i++) {
        printf("page %d took %ld\n", i,
                timestamps[i].time1 - bench_tscoverhead() -
                timestamps[i].time0);
    }
}

/// Called when servers setup their services
static void export_cb(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    err = nameservice_register("lrpc_server", iref);
    assert(err_is_ok(err));
}

/// Called when the client connects
static errval_t connect_cb(void *st, struct bench_binding *b)
{
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    enum {CLIENT, SERVER} mode = -1;
    errval_t err;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "client") == 0) {
            mode = CLIENT;
        } else if (strcmp(argv[i], "server") == 0) {
            mode = SERVER;
        } else if (strcmp(argv[i], "cache") == 0) {
            cache_benchmark = true;
        } else {
            fprintf(stderr, "%s: unknown argument '%s'\n", argv[0], argv[i]);
            return -1;
        }
    }

    if (mode == -1) {
        fprintf(stderr, "Usage: %s client|server [cache]\n", argv[0]);
        return -1;
    }

    if (mode == SERVER) { /* Server */
        /* Setup a server */
        err = bench_export(NULL, export_cb, connect_cb, get_default_waitset(),
                           IDC_EXPORT_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to setup a server");
            exit(EXIT_FAILURE);
        }

        while (chan == NULL || binding == NULL) {
            messages_wait_and_handle_next();
        }

        // cancel flounder-generated event loop and install our own
        lmp_chan_deregister_recv(chan);
        struct event_closure ec = {
            .handler = lrpc_bench_handler,
            .arg = binding,
        };
        lmp_chan_register_recv(chan, get_default_waitset(), ec);

        err = binding->tx_vtbl.lrpc_init_reply(binding, NOP_CONT);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "lrpc_init_reply failed");
            printf("lrpc_init_reply failed\n");
            abort();
        }

        messages_handler_loop();
    }

    /* Client */
    /* Initialize the benchmarking library */
    bench_init();

    /* Connect to the server */
    iref_t serv_iref;
    err = nameservice_blocking_lookup("lrpc_server", &serv_iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to lookup server");
        exit(EXIT_FAILURE);
    }
    assert(serv_iref != 0);

    err = bench_bind(serv_iref, bind_cb, NULL, get_default_waitset(),
                     IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to connect to server");
        exit(EXIT_FAILURE);
    }

    while (!start_benchmark_flag) {
        messages_wait_and_handle_next();
    }

    printf("LRPC call benchmark:\n");

    if (cache_benchmark) {
        fprintf(stderr, "cache benchmark NYI, sorry");
        return EXIT_FAILURE;

        lrpc_benchmark(EVENT_AMD_DATA_CACHE_MISSES, 0);
        lrpc_benchmark(EVENT_AMD_DATA_CACHE_LINES_EVICTED, UMASK_COUNT_ALL);
        lrpc_benchmark(EVENT_AMD_INSTRUCTION_CACHE_MISSES, 0);
        lrpc_benchmark(EVENT_AMD_L2_FILL_WRITEBACK, UMASK_AMD_L2_FILL_WRITEBACK_FILLS);
    } else {
        lrpc_benchmark(0, 0);
    }

    printf("End of benchmarks.\n");
    return 0;
}
