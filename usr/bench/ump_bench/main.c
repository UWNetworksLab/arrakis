/**
 * \file
 * \brief UMP benchmarks
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "ump_bench.h"
#include <string.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>

static coreid_t num_cores;
static char my_name[100];

struct bench_binding *array[MAX_CPUS] = {NULL};
coreid_t my_core_id;

static void ump_init_msg(struct bench_binding *b, coreid_t id)
{
    static int count = 0;
    count++;
    array[id] = b;

    // All clients connected, run experiment
    if (count + 1 == num_cores) {
        for (coreid_t i = 0; i < MAX_CPUS; i++) {
            if (array[i]) {
                experiment(i);
            }
        }

        printf("client done\n");
    }
}

static struct bench_rx_vtbl rx_vtbl = {
    .ump_init_msg   = ump_init_msg,
};

static void bind_cb(void *st, errval_t binderr, struct bench_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.ump_init_msg(b, NOP_CONT, my_core_id);
    assert(err_is_ok(err));

    struct bench_ump_binding *bu = (struct bench_ump_binding*)b;
    struct flounder_ump_state *fus = &bu->ump_state;
    struct ump_chan *chan = &fus->chan;

    struct ump_chan_state *send = &chan->send_chan;
    struct ump_chan_state *recv = &chan->endpoint.chan;

    /* Wait for and reply to msgs */
    while (1) {
        volatile struct ump_message *msg;
        struct ump_control ctrl;
        while (!ump_impl_recv(recv));
        msg = ump_impl_get_next(send, &ctrl);
        msg->header.control = ctrl;
    }
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service
    err = nameservice_register("ump_server", iref);
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

static void num_cores_reply(struct monitor_binding *b, coreid_t num)
{
    // Set num cores
    num_cores = num;
}

int main(int argc, char *argv[])
{
    errval_t err;
    struct monitor_binding *mb = get_monitor_binding();

    /* Set my core id */
    my_core_id = disp_get_core_id();
    strcpy(my_name, argv[0]);

    bench_init();

    if (argc == 1) { /* bsp core */
        mb->rx_vtbl.num_cores_reply = num_cores_reply;

        /* 1. set num_cores,
           2. spawn domains,
           3. setup a server,
           4. wait for all clients to connect,
           5. run experiments
        */
        err = mb->tx_vtbl.num_cores_request(mb, NOP_CONT);
        assert(err_is_ok(err));

        while (num_cores == 0) { // XXX
            messages_wait_and_handle_next();
        }

        // Spawn domains
        char *xargv[] = {my_name, "dummy", NULL};
        err = spawn_program_on_all_cores(false, xargv[0], xargv, NULL,
                                         SPAWN_FLAGS_DEFAULT, NULL);
	DEBUG_ERR(err, "spawn program on all cores");
        assert(err_is_ok(err));

        /* Setup a server */
        err = bench_export(NULL, export_cb, connect_cb, get_default_waitset(),
                           IDC_BIND_FLAGS_DEFAULT);
        assert(err_is_ok(err));
    } else {
        /* Connect to the server */
        iref_t iref;

        err = nameservice_blocking_lookup("ump_server", &iref);
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
