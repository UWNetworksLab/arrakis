/**
 * \file
 * \brief This micro-benchmark estimates the cost of maintaining channels.
 * It runs a simple ping-pong experiment between two dispatchers as they create
 * increasing number of channels. It outputs the RTT.
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/ping_pong_defs.h>
#include <bench/bench.h>
#include <barrelfish/resource_ctrl.h>
#include <barrelfish/waitset.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

#define POLL_CYCLES (cycles_t)0xfffffffff
#define MAX_COUNT 100
#define EXPERIMENT_COUNT 50
struct timestamp {
    cycles_t time0;
    cycles_t time1;
};
static struct timestamp timestamp[MAX_COUNT];

static bool request_done, is_server;
static const char *my_service_name = "channel_cost_bench";
static struct ping_pong_binding *my_b = NULL;
static int idx;
static void start_client(void);

static rsrcid_t my_rsrc_id;
static const char *my_manifest =
    "B 1\n"                     // Normal phase
    "G 80 160 80 480\n";        // Gang phase


/* ------------------------ COMMON MESSAGE HANDLERS ------------------------ */

static void ping(struct ping_pong_binding *b, uint64_t arg)
{
    errval_t err;
    err = trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_ROUTE_BCAST_WITH_CCAST_SEND, 1);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trace_event failed");
    }

    err = b->tx_vtbl.pong(b, NOP_CONT, 0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending ping failed");
    }
}

static void pong(struct ping_pong_binding *b, uint64_t arg)
{
    trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_ROUTE_BCAST_WITH_CCAST_SEND, 0);
    request_done = true;
    timestamp[idx].time1 = bench_tsc();
}

static void experiment(void)
{
    errval_t err;

    for (int exp_count = 0; exp_count < EXPERIMENT_COUNT; exp_count++) {
        request_done = false;
        err = my_b->tx_vtbl.init(my_b, NOP_CONT);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sending ping failed");
        }
        while (!request_done) {
            messages_wait_and_handle_next();
        }

        printf("running with %d connections\n", exp_count);


#if CONFIG_TRACE
        if (exp_count == EXPERIMENT_COUNT - 1) {
            err = trace_control(TRACE_EVENT(TRACE_SUBSYS_ROUTE,
                                            TRACE_EVENT_ROUTE_BENCH_START, 0),
                                TRACE_EVENT(TRACE_SUBSYS_ROUTE,
                                            TRACE_EVENT_ROUTE_BENCH_STOP, 0), 0);
            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "trace_control failed");
            }
        }
#endif

        trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_ROUTE_BENCH_START, 0);

        for (idx = 0; idx < MAX_COUNT; idx++) {


            timestamp[idx].time0 = bench_tsc();
            request_done = false;
            err = my_b->tx_vtbl.ping(my_b, NOP_CONT, 0);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "sending ping failed");
            }
            while (!request_done) {
                messages_wait_and_handle_next();
            }

        }

        trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_ROUTE_BENCH_STOP, 0);

        for (idx = MAX_COUNT / 10; idx < MAX_COUNT; idx++) {
            printf("%d %"PRIuCYCLES"\n", idx, timestamp[idx].time1 -
                   timestamp[idx].time0 - bench_tscoverhead());
        }

#if CONFIG_TRACE
        if (exp_count == EXPERIMENT_COUNT - 1) {
            char *buf = malloc(4096*4096);
            size_t length = trace_dump(buf, 4096*4096, NULL);
            printf("%s\n", buf);
            printf("length of buffer %lu\n", length);
        }
#endif

    }
    printf("client done\n");
}

static void init(struct ping_pong_binding *b)
{
    start_client();
}

static void rsrc_join_request(struct ping_pong_binding *b, uint32_t id)
{
    errval_t err;
    my_rsrc_id = id;

    waitset_poll_cycles = POLL_CYCLES;

    err = rsrc_join(my_rsrc_id);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "rsrc_join");
    }

    err = b->tx_vtbl.rsrc_join_reply(b, NOP_CONT);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "rsrc_join");
    }
}

static void rsrc_join_reply(struct ping_pong_binding *b)
{
    request_done = true;
}

static struct ping_pong_rx_vtbl rx_vtbl = {
    .ping = ping,
    .pong = pong,
    .init = init,
    .rsrc_join_request = rsrc_join_request,
    .rsrc_join_reply = rsrc_join_reply,
};

/* ------------------------------ CLIENT ------------------------------ */

static void bind_cb(void *st, errval_t err, struct ping_pong_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;
}

static void start_client(void)
{
    errval_t err;
    iref_t iref;
    err = nameservice_blocking_lookup(my_service_name, &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
    }

    err = ping_pong_bind(iref, bind_cb, NULL, get_default_waitset(),
                         IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }
}

/* ------------------------------ SERVER ------------------------------ */

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    // register this iref with the name service
    err = nameservice_register(my_service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_cb(void *st, struct ping_pong_binding *b)
{
    b->rx_vtbl = rx_vtbl;

    if (!my_b) {
        my_b = b;
    }

    request_done = true;
    return SYS_ERR_OK;
}

/* ------------------------------ MAIN ------------------------------ */

int main(int argc, char *argv[])
{
    errval_t err;
    bench_init();
    request_done = false;

    if (argc == 2 && strcmp(argv[1], "client") == 0) { // Client

        is_server = false;
        start_client();

    } else if (argc == 2 && strcmp(argv[1], "server") == 0) { // server

        is_server = true;
        err = ping_pong_export(NULL, export_cb, connect_cb,
                               get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "export failed");
        }

    } else {
        printf("Usage: %s client|server\n", argv[0]);
        return EXIT_FAILURE;
    }

    if (is_server) {
        // Create a manifest for gang scheduling
        err = rsrc_manifest(my_manifest, &my_rsrc_id);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "rsrc_manifest");
        }

        // Wait for client to join
        while (!request_done) {
            messages_wait_and_handle_next();
        }

        // Tell client to join the gang manifest
        request_done = false;
        err = my_b->tx_vtbl.rsrc_join_request(my_b, NOP_CONT, my_rsrc_id);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "rsrc_join");
        }
        waitset_poll_cycles = POLL_CYCLES;

        while (!request_done) {
            messages_wait_and_handle_next();
        }

        // Start experiments
        experiment();
    }

    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

    return EXIT_FAILURE;
}
