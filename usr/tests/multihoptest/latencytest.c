/**
 * \file
 * \brief Multi-hop latency test
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <string.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <bench/bench.h>
#include <if/bench_defs.h>
#include <unistd.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

static char my_name[100];

// the iref of the server
static iref_t iref;

// the binding we use for the benchmark
static struct bench_binding *binding;

// waitset used for the benchmark
struct waitset signal_waitset;

// the binding we use for signaling
static struct bench_binding *signaling_binding;

static coreid_t my_core_id;

// number of iterations
#define MAX_COUNT 1000

// buffers to send
static uint8_t buffer;
static uint8_t buffer2[100];
static uint8_t buffer3[1000];

// a time stamp
struct timestamp {
    cycles_t time0;
    cycles_t time1;
};

// array where we store all measured times
static struct timestamp timestamps[MAX_COUNT];

// variables to determine when all channels are free
static bool reply_received = false;
static bool signal_received = false;

// loop variable
static int i = 0;

// get message name
inline static char* get_message_name(int message_type)
{
    switch (message_type) {
    case 0:
        return "empty";
    case 1:
        return "payload32_1";
    case 2:
        return "payload32_2";
    case 3:
        return "payload32_4";
    case 4:
        return "payload32_8";
    case 5:
        return "payload32_16";
    case 6:
        return "payload64_1";
    case 7:
        return "payload64_2";
    case 8:
        return "payload64_4";
    case 9:
        return "payload64_8";
    case 10:
        return "payload_64_16";
    case 11:
        return "buffer (1 byte)";
    case 12:
        return "buffer (100 bytes)";
    case 13:
        return "buffer (1000 bytes)";

    default:
        printf("unknown message type\n");
        abort();
        return "";
    }
}

static void experiment_cont(void* arg);

inline static void experiment(void)
{

    if (!(reply_received && signal_received)) {
        return;
    }assert(reply_received && signal_received);

    reply_received = false;
    signal_received = false;

    // continue experiment as soon as binding can accept
    // the next message
    errval_t err;
    err = binding->register_send(binding, get_default_waitset(),
            MKCONT(experiment_cont, NULL));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in register send");
    }
}

// continue experiment
static void experiment_cont(void* arg)
{

    errval_t err;
    static bool flag = false;
    static int message_type = 0;

    // Experiment finished (with this message type)
    if (i == MAX_COUNT - 1) {

#if CONFIG_TRACE
#else
        // print measured times
        for (int j = MAX_COUNT / 10; j < MAX_COUNT; j++) {

            printf(
                    "page %d took %"PRIuCYCLES"\n",
                    j,
                    timestamps[j].time1 - bench_tscoverhead()
                            - timestamps[j].time0);
        }
#endif
        // go to next message type
        message_type++;
        flag = false;
        i = 0;
        if (message_type > 13) {

            // stop tracing
            err = trace_event(TRACE_SUBSYS_MULTIHOP,
                    TRACE_EVENT_MULTIHOP_BENCH_STOP, 0);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "trace_event failed");
            }

#if CONFIG_TRACE
            // dump trace
            char *buf = malloc(50*4096*4096);
            size_t length = trace_dump(buf, 20*4096*4096, NULL);
            printf("%s\n", buf);
            printf("length of buffer %lu\n", length);
#endif
            printf("client done!\n");
            return;
        }
    }

    if (!flag) { // Start experiment

#if CONFIG_TRACE
#else
        printf("Running latency test for message %s...\n",
                get_message_name(message_type));
#endif
        flag = true;
        timestamps[i].time0 = bench_tsc();
    } else { // Continue experiment
        i++;
        timestamps[i].time0 = bench_tsc();
    }

    // trace send event
    err = trace_event(TRACE_SUBSYS_MULTIHOP, TRACE_EVENT_MULTIHOP_MESSAGE_SEND,
            message_type);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trace_event failed");
    }

    // send next message
    switch (message_type) {
    case 0:
        err = binding->tx_vtbl.fsb_empty_request(binding, NOP_CONT);
        break;
    case 1:
        err = binding->tx_vtbl.fsb_payload32_1_request(binding, NOP_CONT, 1);
        break;

    case 2:
        err = binding->tx_vtbl.fsb_payload32_2_request(binding, NOP_CONT, 1, 2);
        break;

    case 3:
        err = binding->tx_vtbl.fsb_payload32_4_request(binding, NOP_CONT, 1, 2,
                3, 4);
        break;

    case 4:
        err = binding->tx_vtbl.fsb_payload32_8_request(binding, NOP_CONT, 1, 2,
                3, 4, 5, 6, 7, 8);
        break;

    case 5:
        err = binding->tx_vtbl.fsb_payload32_16_request(binding, NOP_CONT, 1, 2,
                3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
        break;

    case 6:
        err = binding->tx_vtbl.fsb_payload64_1_request(binding, NOP_CONT, 1);
        break;

    case 7:
        err = binding->tx_vtbl.fsb_payload64_2_request(binding, NOP_CONT, 1, 2);
        break;

    case 8:
        err = binding->tx_vtbl.fsb_payload64_4_request(binding, NOP_CONT, 1, 2,
                3, 4);
        break;

    case 9:
        err = binding->tx_vtbl.fsb_payload64_8_request(binding, NOP_CONT, 1, 2,
                3, 4, 5, 6, 7, 8);
        break;

    case 10:
        err = binding->tx_vtbl.fsb_payload64_16_request(binding, NOP_CONT, 1, 2,
                3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
        break;

    case 11:
        err = binding->tx_vtbl.fsb_buffer_request(binding, NOP_CONT, &buffer,
                1);
        break;

    case 12:
        err = binding->tx_vtbl.fsb_buffer_request(binding, NOP_CONT, buffer2,
                100);
        break;

    case 13:
        err = binding->tx_vtbl.fsb_buffer_request(binding, NOP_CONT, buffer3,
                1000);
        break;

    default:
        printf("unknown message type\n");
        abort();
        break;
    }

    // make sure send was successful
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "while running experiment\n");
    }

    // receive reply (by dispatching events from the
    // waitset we use for the benchmark)
    while (reply_received == false) {
        event_dispatch(&signal_waitset);
    }

    experiment();
}

// called when we receive a signal
static void busy_ping(struct bench_binding *b)
{
    signal_received = true;
    experiment();
}

// called when a message is received
static inline void message_received(void)
{
    errval_t err;

    // save timestamp
    timestamps[i].time1 = bench_tsc();

    // trace receive event
    err = trace_event(TRACE_SUBSYS_MULTIHOP,
            TRACE_EVENT_MULTIHOP_MESSAGE_RECEIVE, 0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trace_event failed");
    }

    reply_received = true;
}

// send continue signal to server
static void continue_signal(void* arg)
{
    errval_t err;
    err = signaling_binding->tx_vtbl.busy_ping(signaling_binding, NOP_CONT);
    assert(err_is_ok(err));
}

static void fsb_init_msg(struct bench_binding *b, coreid_t id)
{
    errval_t err;

    // change waitset of the binding
    waitset_init(&signal_waitset);
    err = b->change_waitset(b, &signal_waitset);
    assert(err_is_ok(err));

    binding = b;
    reply_received = true;

#if CONFIG_TRACE
    // configure tracing
    err = trace_control(TRACE_EVENT(TRACE_SUBSYS_MULTIHOP,
                    TRACE_EVENT_MULTIHOP_BENCH_START, 0),
            TRACE_EVENT(TRACE_SUBSYS_MULTIHOP,
                    TRACE_EVENT_MULTIHOP_BENCH_STOP, 0), 0);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "trace_control failed");
    }
#endif

    // start tracing
    err = trace_event(TRACE_SUBSYS_MULTIHOP, TRACE_EVENT_MULTIHOP_BENCH_START,
            0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trace_event failed");
    }

    experiment();
}

static void fsb_empty_reply(struct bench_binding *b)
{
    message_received();
}

static void fsb_empty_request(struct bench_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.fsb_empty_reply(b, MKCONT(continue_signal, NULL));
    assert(err_is_ok(err));
}

static void fsb_payload32_1_reply(struct bench_binding *b, int32_t p0)
{
    message_received();
}

static void fsb_payload32_2_reply(struct bench_binding *b, int32_t p0,
        int32_t p1)
{
    message_received();
}

static void fsb_payload32_4_reply(struct bench_binding *b, int32_t payload0,
        int32_t payload1, int32_t payload2, int32_t payload3)
{
    message_received();
}

static void fsb_payload32_8_reply(struct bench_binding *b, int32_t p0,
        int32_t p1, int32_t p2, int32_t p3, int32_t p4, int32_t p5, int32_t p6,
        int32_t p7)
{
    message_received();
}

static void fsb_payload32_16_reply(struct bench_binding *b, int32_t p0,
        int32_t p1, int32_t p2, int32_t p3, int32_t p4, int32_t p5, int32_t p6,
        int32_t p7, int32_t p8, int32_t p9, int32_t p10, int32_t p11,
        int32_t p12, int32_t p13, int32_t p14, int32_t p15)
{
    message_received();
}

static void fsb_payload32_1_request(struct bench_binding *b, int32_t payload0)
{
    errval_t err;
    err = b->tx_vtbl.fsb_payload32_1_reply(b, MKCONT(continue_signal, NULL), 1);
    assert(err_is_ok(err));
}

static void fsb_payload32_2_request(struct bench_binding *b, int32_t payload0,
        int32_t payload1)
{
    errval_t err;
    err = b->tx_vtbl.fsb_payload32_2_reply(b, MKCONT(continue_signal, NULL), 1,
            2);
    assert(err_is_ok(err));
}

static void fsb_payload32_4_request(struct bench_binding *b, int32_t payload0,
        int32_t payload1, int32_t payload2, int32_t payload3)
{
    errval_t err;
    err = b->tx_vtbl.fsb_payload32_4_reply(b, MKCONT(continue_signal, NULL), 1,
            2, 3, 4);
    assert(err_is_ok(err));
}

static void fsb_payload32_8_request(struct bench_binding *b, int32_t payload0,
        int32_t payload1, int32_t payload2, int32_t payload3, int32_t payload4,
        int32_t payload5, int32_t payload6, int32_t payload7)
{
    errval_t err;
    err = b->tx_vtbl.fsb_payload32_8_reply(b, MKCONT(continue_signal, NULL), 1,
            2, 3, 4, 5, 6, 7, 8);
    assert(err_is_ok(err));
}

static void fsb_payload32_16_request(struct bench_binding *b, int32_t payload0,
        int32_t payload1, int32_t payload2, int32_t payload3, int32_t payload4,
        int32_t payload5, int32_t payload6, int32_t payload7, int32_t payload8,
        int32_t payload9, int32_t payload10, int32_t payload11,
        int32_t payload12, int32_t payload13, int32_t payload14,
        int32_t payload15)
{
    errval_t err;
    err = b->tx_vtbl.fsb_payload32_16_reply(b, MKCONT(continue_signal, NULL), 1,
            2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
    assert(err_is_ok(err));
}

static void fsb_payload64_1_reply(struct bench_binding *b, int64_t p0)
{
    message_received();
}

static void fsb_payload64_2_reply(struct bench_binding *b, int64_t p0,
        int64_t p1)
{
    message_received();
}

static void fsb_payload64_4_reply(struct bench_binding *b, int64_t payload0,
        int64_t payload1, int64_t payload2, int64_t payload3)
{
    message_received();
}

static void fsb_payload64_8_reply(struct bench_binding *b, int64_t p0,
        int64_t p1, int64_t p2, int64_t p3, int64_t p4, int64_t p5, int64_t p6,
        int64_t p7)
{
    message_received();
}

static void fsb_payload64_16_reply(struct bench_binding *b, int64_t p0,
        int64_t p1, int64_t p2, int64_t p3, int64_t p4, int64_t p5, int64_t p6,
        int64_t p7, int64_t p8, int64_t p9, int64_t p10, int64_t p11,
        int64_t p12, int64_t p13, int64_t p14, int64_t p15)
{
    message_received();
}

static void fsb_payload64_1_request(struct bench_binding *b, int64_t payload0)
{
    errval_t err;
    err = b->tx_vtbl.fsb_payload64_1_reply(b, MKCONT(continue_signal, NULL), 1);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error while sending reply message in client\n");
    }
}

static void fsb_payload64_2_request(struct bench_binding *b, int64_t payload0,
        int64_t payload1)
{
    errval_t err;
    err = b->tx_vtbl.fsb_payload64_2_reply(b, MKCONT(continue_signal, NULL), 1,
            2);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error while sending reply message in client\n");
    }
}

static void fsb_payload64_4_request(struct bench_binding *b, int64_t payload0,
        int64_t payload1, int64_t payload2, int64_t payload3)
{
    errval_t err;
    err = b->tx_vtbl.fsb_payload64_4_reply(b, MKCONT(continue_signal, NULL), 1,
            2, 3, 4);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error while sending reply message in client\n");
    }
}

static void fsb_payload64_8_request(struct bench_binding *b, int64_t payload0,
        int64_t payload1, int64_t payload2, int64_t payload3, int64_t payload4,
        int64_t payload5, int64_t payload6, int64_t payload7)
{
    errval_t err;
    err = b->tx_vtbl.fsb_payload64_8_reply(b, MKCONT(continue_signal, NULL), 1,
            2, 3, 4, 5, 6, 7, 8);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error while sending reply message in client\n");
    }
}

static void fsb_payload64_16_request(struct bench_binding *b, int64_t payload0,
        int64_t payload1, int64_t payload2, int64_t payload3, int64_t payload4,
        int64_t payload5, int64_t payload6, int64_t payload7, int64_t payload8,
        int64_t payload9, int64_t payload10, int64_t payload11,
        int64_t payload12, int64_t payload13, int64_t payload14,
        int64_t payload15)
{

    errval_t err;
    err = b->tx_vtbl.fsb_payload64_16_reply(b, MKCONT(continue_signal, NULL), 1,
            2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error while sending reply message in client\n");
    }
}

static void fsb_buffer_reply(struct bench_binding *b, uint8_t *payload,
        size_t size)
{
    message_received();
    free(payload);
}

static void fsb_buffer_request(struct bench_binding *b, uint8_t *payload,
        size_t size)
{
    errval_t err;
    err = trace_event(TRACE_SUBSYS_MULTIHOP,
            TRACE_EVENT_MULTIHOP_MESSAGE_RECEIVE, 0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trace_event failed");
    }

    err = b->tx_vtbl.fsb_buffer_reply(b, MKCONT(continue_signal, NULL), payload,
            size);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error while sending reply message in client\n");
    }
    free(payload);
}

// receive virtual table
static struct bench_rx_vtbl rx_vtbl = { .fsb_init_msg = fsb_init_msg,
        .fsb_empty_request = fsb_empty_request, .fsb_empty_reply =
                fsb_empty_reply, .fsb_payload32_1_request =
                fsb_payload32_1_request, .fsb_payload32_2_request =
                fsb_payload32_2_request, .fsb_payload32_4_request =
                fsb_payload32_4_request, .fsb_payload32_8_request =
                fsb_payload32_8_request, .fsb_payload32_16_request =
                fsb_payload32_16_request, .fsb_payload32_1_reply =
                fsb_payload32_1_reply, .fsb_payload32_2_reply =
                fsb_payload32_2_reply, .fsb_payload32_4_reply =
                fsb_payload32_4_reply, .fsb_payload32_8_reply =
                fsb_payload32_8_reply, .fsb_payload32_16_reply =
                fsb_payload32_16_reply, .fsb_payload64_1_request =
                fsb_payload64_1_request, .fsb_payload64_2_request =
                fsb_payload64_2_request, .fsb_payload64_4_request =
                fsb_payload64_4_request, .fsb_payload64_8_request =
                fsb_payload64_8_request, .fsb_payload64_16_request =
                fsb_payload64_16_request, .fsb_payload64_1_reply =
                fsb_payload64_1_reply, .fsb_payload64_2_reply =
                fsb_payload64_2_reply, .fsb_payload64_4_reply =
                fsb_payload64_4_reply, .fsb_payload64_8_reply =
                fsb_payload64_8_reply, .fsb_payload64_16_reply =
                fsb_payload64_16_reply,
        .fsb_buffer_request = fsb_buffer_request, .fsb_buffer_reply =
                fsb_buffer_reply, .busy_ping = busy_ping,

};

static void bind_cb(void *st, errval_t binderr, struct bench_binding *b)
{
    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // Send an init message. This will start the benchmark.
    errval_t err;
    err = b->tx_vtbl.fsb_init_msg(b, MKCONT(continue_signal, NULL), my_core_id);
    assert(err_is_ok(err));
}

static void bind_signal_cb(void *st, errval_t binderr, struct bench_binding *b)
{

    errval_t err;
    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;
    signaling_binding = b;

    // bind a second time over the multi-hop interconnect driver
    // we will use this binding for the benchmark
    err = bench_bind(iref, bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT | IDC_BIND_FLAG_MULTIHOP);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }

}

static void export_cb(void *st, errval_t err, iref_t iref2)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service
    err = nameservice_register("multihop_server", iref2);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }
}

static errval_t connect_cb(void *st, struct bench_binding *b)
{
    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection
    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    errval_t err;

    /* Set my core id */
    my_core_id = disp_get_core_id();
    strcpy(my_name, argv[0]);

    printf("entered\n");
    bench_init();
    printf("bench_init done\n");

    if (argc == 1) { /* server */

        /*
         1. spawn domain,
         2. setup a server,
         3. wait for client to connect,
         4. run experiment
         */

        char *xargv[] = { my_name, "dummy", "dummy", "dummy", NULL };
        err = spawn_program(1, my_name, xargv, NULL, SPAWN_FLAGS_DEFAULT, NULL);
        assert(err_is_ok(err));

        /* Setup a server */
        err = bench_export(NULL, export_cb, connect_cb, get_default_waitset(),
                IDC_BIND_FLAGS_DEFAULT);
        assert(err_is_ok(err));

    } else {
        /* Connect to the server */

        printf("ns lookup\n");
        err = nameservice_blocking_lookup("multihop_server", &iref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "nameservice_blocking_lookup failed");
            abort();
        }

        printf("bench_bind\n");
        // bind a first time for signaling
        err = bench_bind(iref, bind_signal_cb, NULL, get_default_waitset(),
                IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "bind failed");
            abort();
        }
    }
    messages_handler_loop();
    return 0;
}
