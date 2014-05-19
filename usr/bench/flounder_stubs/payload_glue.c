/**
 * \file
 * \brief Flounder stubs
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
#include <string.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <bench/bench.h>
#include <if/glue_bench_types.h>

static char my_name[100];

static struct glue_bench_service_response *binding;
static coreid_t my_core_id;

#define MAX_COUNT 1000

struct timestamp {
    cycles_t time0;
    cycles_t time1;
};
static struct timestamp timestamps[MAX_COUNT];
static int benchmark_type = 0;

static void experiment(void)
{
    errval_t err;
    static bool flag = false;
    static int i = 0;

    // Experiment finished
    if (i == MAX_COUNT - 1) {
        timestamps[i].time1 = bench_tsc();

        for (int j = MAX_COUNT / 10; j < MAX_COUNT; j++) {
            printf("page %d took %"PRIuCYCLES"\n", j,
                   timestamps[j].time1 - bench_tscoverhead() -
                   timestamps[j].time0);
        }

        printf("client done\n");
        return;
    }

    if (!flag) { // Start experiment
        timestamps[i].time0 = bench_tsc();
        flag = true;
    } else { // Continue experiment
        timestamps[i].time1 = bench_tsc();
        timestamps[i+1].time0 = timestamps[i].time1;
        i++;
    }

    assert(binding);
    assert(binding->f);

    switch(benchmark_type) {
    case 0:
        assert(binding->f->fsb_payload_request);
        err = binding->f->fsb_payload_request(binding, 1, 2, 3, 4);
        break;

    case 1:
        assert(binding->f->fsb_empty_request);
        err = binding->f->fsb_empty_request(binding);
        break;

    case 2:
        assert(binding->f->fsb_payload1_request);
        err = binding->f->fsb_payload1_request(binding, 1);
        break;

    case 3:
        assert(binding->f->fsb_payload2_request);
        err = binding->f->fsb_payload2_request(binding, 1, 2);
        break;

    case 4:
        assert(binding->f->fsb_payload8_request);
        err = binding->f->fsb_payload8_request(binding, 1, 2, 3, 4, 5, 6, 7, 8);
        break;

    case 5:
        assert(binding->f->fsb_payload16_request);
        err = binding->f->fsb_payload16_request(binding, 1, 2, 3, 4, 5, 6, 7, 8,
                                                9, 10, 11, 12, 13, 14, 15, 16);
        break;

    default:
        printf("unknown benchmark type\n");
        abort();
        break;
    }

    assert(err_is_ok(err));
}

static void fsb_init_msg(struct glue_bench_service_response *b, coreid_t id)
{
    binding = b;
    printf("Running flounder_stubs_payload between core %d and core %d\n", my_core_id, 1);
    experiment();
}

static void fsb_payload_reply(struct glue_bench_service_response *b,
                              int payload0, int payload1,
                              int payload2, int payload3)
{
    experiment();
}

static void fsb_empty_reply(struct glue_bench_service_response *b)
{
    experiment();
}

static void fsb_payload1_reply(struct glue_bench_service_response *b,
                               int p0)
{
    experiment();
}

static void fsb_payload2_reply(struct glue_bench_service_response *b,
                               int p0, int p1)
{
    experiment();
}

static void fsb_payload8_reply(struct glue_bench_service_response *b,
                               int p0, int p1, int p2, int p3,
                               int p4, int p5, int p6, int p7)
{
    experiment();
}

static void fsb_payload16_reply(struct glue_bench_service_response *b,
                                int p0, int p1, int p2, int p3,
                                int p4, int p5, int p6, int p7,
                                int p8, int p9, int p10, int p11,
                                int p12, int p13, int p14, int p15)
{
    experiment();
}

static void listen_cb(struct glue_bench_service *closure, iref_t iref)
{
    /* if iref == 0, listen failed */
    assert(iref != 0); 

    errval_t err = nameservice_register("server", iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }
}

static struct glue_bench_server_call_vtbl call_vtbl = {
    .fsb_init_msg   = fsb_init_msg,
    .fsb_payload_reply = fsb_payload_reply,
    .fsb_empty_reply = fsb_empty_reply,
    .fsb_payload1_reply = fsb_payload1_reply,
    .fsb_payload2_reply = fsb_payload2_reply,
    .fsb_payload8_reply = fsb_payload8_reply,
    .fsb_payload16_reply = fsb_payload16_reply,
    ._listening = listen_cb,
    ._connected = NULL,
};

static struct glue_bench_service service = {
    .f = &call_vtbl,
};

static void fsb_payload_request(struct glue_bench_client_response *b,
                              int payload0, int payload1,
                              int payload2, int payload3)
{
    errval_t err;
    err = b->call_vtbl->fsb_payload_reply(b, 1, 2, 3, 4);
    assert(err_is_ok(err));
}

static void fsb_empty_request(struct glue_bench_client_response *b)
{
    errval_t err;
    err = b->call_vtbl->fsb_empty_reply(b);
    assert(err_is_ok(err));
}

static void fsb_payload1_request(struct glue_bench_client_response *b,
                              int payload0)
{
    errval_t err;
    err = b->call_vtbl->fsb_payload1_reply(b, 1);
    assert(err_is_ok(err));
}

static void fsb_payload2_request(struct glue_bench_client_response *b,
                                 int payload0, int payload1)
{
    errval_t err;
    err = b->call_vtbl->fsb_payload2_reply(b, 1, 2);
    assert(err_is_ok(err));
}

static void fsb_payload8_request(struct glue_bench_client_response *b,
                                 int payload0, int payload1,
                                 int payload2, int payload3,
                                 int payload4, int payload5,
                                 int payload6, int payload7)
{
    errval_t err;
    err = b->call_vtbl->fsb_payload8_reply(b, 1, 2, 3, 4, 5, 6, 7, 8);
    assert(err_is_ok(err));
}

static void fsb_payload16_request(struct glue_bench_client_response *b,
                                  int payload0, int payload1,
                                  int payload2, int payload3,
                                  int payload4, int payload5,
                                  int payload6, int payload7,
                                  int payload8, int payload9,
                                  int payload10, int payload11,
                                  int payload12, int payload13,
                                  int payload14, int payload15)
{
    errval_t err;
    err = b->call_vtbl->fsb_payload16_reply(b, 1, 2, 3, 4, 5, 6, 7, 8,
                                            9, 10, 11, 12, 13, 14, 15, 16);
    assert(err_is_ok(err));
}

static void client_connect_cb(struct glue_bench_client_response *b)
{
    errval_t err;
    err = b->call_vtbl->fsb_init_msg(b, my_core_id);
    assert(err_is_ok(err));
}

int main(int argc, char *argv[])
{
    errval_t err;

    /* Set my core id */
    my_core_id = disp_get_core_id();
    strcpy(my_name, argv[0]);

    bench_init();

    // Default first arg
    if(argc == 1) {
        argc = 2;
        argv[1] = "0";
    }

    if (argc == 2) { /* bsp core */
        benchmark_type = atoi(argv[1]);

        /*
          1. spawn domain,
          2. setup a server,
          3. wait for client to connect,
          4. run experiment
        */
        char *xargv[] = {my_name, "dummy", "dummy", NULL};
        err = spawn_program(1, my_name, xargv, NULL,
                            SPAWN_FLAGS_DEFAULT, NULL);
        assert(err_is_ok(err));

        /* Setup a server */
        errval_t r = glue_bench_listen(&service);
        assert(err_is_ok(r));
    } else {
        /* Connect to the server */
        iref_t iref;

        err = nameservice_blocking_lookup("server", &iref);
        if (err_is_fail(err)) {
            return err; // XXX
        }

        assert(iref != 0);

        static struct glue_bench_client_response_vtbl crv = {
            .fsb_payload_request = fsb_payload_request,
            .fsb_empty_request = fsb_empty_request,
            .fsb_payload1_request = fsb_payload1_request,
            .fsb_payload2_request = fsb_payload2_request,
            .fsb_payload8_request = fsb_payload8_request,
            .fsb_payload16_request = fsb_payload16_request,
            ._connected = client_connect_cb,
        };
        static struct glue_bench_client_response client;
        client.f = &crv;
        glue_bench_connect(iref, &client, 0);
    }

    messages_handler_loop();
}
