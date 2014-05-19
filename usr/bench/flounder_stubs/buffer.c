/**
 * \file
 * \brief Flounder stubs buffer
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
#include <if/bench_defs.h>

static char my_name[100];
static uint8_t buffer;

static struct bench_binding *binding;
static coreid_t my_core_id;

#define MAX_COUNT 1000

struct timestamp {
    cycles_t time0;
    cycles_t time1;
};
static struct timestamp timestamps[MAX_COUNT];

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

    err = binding->tx_vtbl.fsb_buffer_request(binding, NOP_CONT, &buffer, 1);
    assert(err_is_ok(err));
}

static void fsb_init_msg(struct bench_binding *b, coreid_t id)
{
    binding = b;
    printf("Running flounder_stubs_buffer between core %d and core %d\n", my_core_id, 1);
    experiment();
}

static void fsb_buffer_reply(struct bench_binding *b, uint8_t *payload, size_t size)
{
    experiment();
    free(payload);
}

static void fsb_buffer_request(struct bench_binding *b, uint8_t *payload, size_t size)
{
    errval_t err;
    err = b->tx_vtbl.fsb_buffer_reply(b, NOP_CONT, &buffer, 1);
    assert(err_is_ok(err));
    free(payload);
}

static struct bench_rx_vtbl rx_vtbl = {
    .fsb_init_msg   = fsb_init_msg,
    .fsb_buffer_request = fsb_buffer_request,
    .fsb_buffer_reply = fsb_buffer_reply,
};

static void bind_cb(void *st, errval_t binderr, struct bench_binding *b)
{
    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    errval_t err;
    err = b->tx_vtbl.fsb_init_msg(b, NOP_CONT, my_core_id);
    assert(err_is_ok(err));
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service
    err = nameservice_register("fsb_server", iref);
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

    bench_init();

    if (argc == 1) { /* bsp core */
        /*
          1. spawn domain,
          2. setup a server,
          3. wait for client to connect,
          4. run experiment
        */
        char *xargv[] = {my_name, "dummy", NULL};
        err = spawn_program(1, my_name, xargv, NULL,
                            SPAWN_FLAGS_DEFAULT, NULL);
        assert(err_is_ok(err));

        /* Setup a server */
        err = bench_export(NULL, export_cb, connect_cb, get_default_waitset(),
                           IDC_BIND_FLAGS_DEFAULT);
        assert(err_is_ok(err));
    } else {
        /* Connect to the server */
        iref_t iref;

        err = nameservice_blocking_lookup("fsb_server", &iref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "nameservice_blocking_lookup failed");
            abort();
        }

        err = bench_bind(iref, bind_cb, NULL,
                         get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "bind failed");
            abort();
        }
    }

    messages_handler_loop();
}
