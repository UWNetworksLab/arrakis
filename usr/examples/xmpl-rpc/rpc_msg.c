/** \file
 *  \brief Example rpc application using call/response stubs
 */

/*
 * Copyright (c) 2010, ETH Zurich.
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

#include <if/xmplrpc_defs.h>

const char *service_name = "xmplrpc_msg_service";

/* --------------------- Client ------------------------------ */

static void rx_myrpc_response(struct xmplrpc_binding *b, char *s)
{
    debug_printf("client: rx_myrpc_response called: %s\n", s);

    free(s);
}


static void send_myrpc_call_cb(void *a)
{
    debug_printf("client: myrpc_call sent successfully\n");
}

static void send_myrpc_call(void *a)
{
    errval_t err;

    debug_printf("client: sending mycall\n");

    struct xmplrpc_binding *b = (struct xmplrpc_binding *)a;

    struct event_closure txcont = MKCONT(send_myrpc_call_cb, b);

    err = xmplrpc_myrpc_call__tx(b, txcont, 42);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            debug_printf("client: re-sending mycall\n");
            struct waitset *ws = get_default_waitset();
	    txcont = MKCONT(send_myrpc_call, b);
            err = b->register_send(b, ws, txcont);
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
            }
        } else {
            DEBUG_ERR(err, "error sending mycall message\n");
        }
    }
}

struct xmplrpc_rx_vtbl c_rx_vtbl = {
    .myrpc_response = rx_myrpc_response,
};

static void bind_cb(void *st, errval_t err, struct xmplrpc_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }
    
    b->rx_vtbl = c_rx_vtbl;

    send_myrpc_call(b);
}


static void start_client(void)
{
    errval_t err;
    iref_t iref;

    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
    }

    err = xmplrpc_bind(iref, 
                     bind_cb, 
                     NULL /* state for bind_cb */,
                     get_default_waitset(),
                     IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }
}

/* --------------------- Server ------------------------------ */

struct server_state {
    struct xmplrpc_binding *b;
    char *s;
};

static void free_st(struct server_state* st)
{
    if (st->s != NULL) free(st->s);
    free(st);
}

static void send_myrpc_response_cb(void *a)
{
    struct server_state *st = (struct server_state*)a;

    debug_printf("server: myresponse sent succesfully\n");

    free_st(st);
}

static void send_myrpc_response(void *a)
{
    errval_t err;
    struct server_state *st = (struct server_state*)a;

    debug_printf("server: sending myresponse\n");

    struct event_closure txcont = MKCONT(send_myrpc_response_cb, st);
    err = xmplrpc_myrpc_response__tx(st->b, txcont, st->s);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            debug_printf("server: re-sending myresponse\n");
            struct waitset *ws = get_default_waitset();
            txcont = MKCONT(send_myrpc_response, st);
            err = st->b->register_send(st->b, ws, txcont);
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
                free_st(st);
            }
        } else {
            DEBUG_ERR(err, "error sending mycall message\n");
            free_st(st);
        }
    }
}

static void rx_myrpc_call(struct xmplrpc_binding *b, int i)
{
    debug_printf("server: received myrpc_call: %d\n", i);

    // prepare and send reply

    struct server_state *st = malloc(sizeof(struct server_state));
    if (st == NULL) {
        USER_PANIC("cannot reply, out of memory");
    }

    st->b = b;
    st->s = malloc(20);
    if (st->s != NULL) {
        snprintf(st->s, 20, "!%d!", i);
    }

    send_myrpc_response(st);
}

static struct xmplrpc_rx_vtbl s_rx_vtbl = {
    .myrpc_call = rx_myrpc_call,
};

static errval_t connect_cb(void *st, struct xmplrpc_binding *b) 
{    
    b->rx_vtbl = s_rx_vtbl;

    return SYS_ERR_OK;
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static void start_server(void)
{
    errval_t err;

    err = xmplrpc_export(NULL /* state pointer for connect/export callbacks */,
                        export_cb, connect_cb,
                        get_default_waitset(),
                        IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }
}



/* --------------------- Main ------------------------------ */

int main(int argc, char *argv[]) 
{
    errval_t err;

    if ((argc >= 2) && (strcmp(argv[1], "client") == 0)) {
        start_client();
    } else if ((argc >= 2) && (strcmp(argv[1], "server") == 0)) {
        start_server();
    } else {
        printf("usage: %s client|server\n", argv[0]);
        return EXIT_FAILURE;
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
