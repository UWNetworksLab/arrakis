/** \file
 *  \brief Example message application
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

#include <if/xmplmsg_defs.h>

const char *service_name = "xmpl-msg_service";

/* --------------------- Client ------------------------------ */


static void send_string_cb(void *a)
{
    // send succesful, nothing to do
}


static void send_string_ready(void *a)
{
    errval_t err;

    struct xmplmsg_binding *b = (struct xmplmsg_binding*)a;
    struct event_closure txcont = MKCONT(send_string_cb, b);

    err = xmplmsg_msg_string__tx(b, txcont, "Hello World");

    if (err_is_fail(err)) {
      DEBUG_ERR(err, "error sending msg_string message\n");
    }
}


static void send_ints_cb(void *a)
{
    errval_t err;

    struct xmplmsg_binding *b = (struct xmplmsg_binding*)a;
    struct event_closure txcont = MKCONT(send_string_cb, b);

    err = xmplmsg_msg_string__tx(b, txcont, "Hello World");

    if (err_is_fail(err)) {
      DEBUG_ERR(err, "error sending msg_string message\n");
      if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
	    txcont = MKCONT(send_string_ready, b);
            err = b->register_send(b, ws, txcont);
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
            }
        }
    }
}

static void send_ints_ready(void *a)
{
    errval_t err;

    struct xmplmsg_binding *b = (struct xmplmsg_binding*)a;

    struct event_closure txcont = MKCONT(send_ints_cb, b);

    err = xmplmsg_msg_ints__tx(b, txcont, 0x1, 0x10);

    if (err_is_fail(err)) {
      DEBUG_ERR(err, "error sending msg_ints message\n");
    }
}

static void bind_cb(void *st, errval_t err, struct xmplmsg_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed failed");
    }

    struct event_closure txcont = MKCONT(send_ints_cb, b);

    err = xmplmsg_msg_ints__tx(b, txcont, 0x1, 0x10);

    if (err_is_fail(err)) {
      DEBUG_ERR(err, "error sending msg_ints message\n");

      if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
	    txcont = MKCONT(send_ints_ready, b);
            err = b->register_send(b, ws, txcont);
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
            }
        }
    }
}

static void start_client(void)
{
    errval_t err;
    iref_t iref;

    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
    }

    err = xmplmsg_bind(iref, 
                     bind_cb, 
                     NULL /* state for bind_cb */,
                     get_default_waitset(),
                     IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }
}

/* --------------------- Server ------------------------------ */

static void rx_msg_ints(struct xmplmsg_binding *b, int ia, int ib)
{
    printf("server: received msg_ints:\n\tia:%d, ib:%d\n", ia, ib);
}


static void rx_msg_string(struct xmplmsg_binding *b, char *str)
{
    printf("server: received msg_string:\n\t%s\n", str);
    free(str); 
}

static struct xmplmsg_rx_vtbl rx_vtbl = {
    .msg_ints = rx_msg_ints,
    .msg_string = rx_msg_string,
};

static errval_t connect_cb(void *st, struct xmplmsg_binding *b) 
{    
    b->rx_vtbl = rx_vtbl;

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

    err = xmplmsg_export(NULL /* state pointer for connect/export callbacks */,
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
