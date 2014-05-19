/** \file
 *  \brief Example application using THC stubs
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

#include <thc/thc.h>

#include <if/xmplthc_defs.h>
#include <if/xmplthc_thc.h>

const char *service_name = "xmplthc_binding_service";

/* --------------------- Client ------------------------------ */

static void run_client(struct xmplthc_thc_client_binding_t *cl)
{
    int i = 42;
    char *s = NULL;

    // regular message
    cl->send.mymsg(cl, i);
    debug_printf("client: sent msg: %d\n", i);

    // call/response
    cl->send.mycall(cl, i);
    debug_printf("client: sent mycall: %d\n", i);
    cl->recv.myresponse(cl, &s);
    debug_printf("client: received myresponse: '%s'\n", s);
    free(s);

    // rpc as call/response
    cl->send.myrpc(cl, i);
    debug_printf("client: sent myrpc call msg: %d\n", i);
    cl->recv.myrpc(cl, &s);
    debug_printf("client: received myrpc response msg: '%s'\n", s);
    free(s);

    // rpc
    cl->call_seq.myrpc(cl, i, &s);
    debug_printf("client: returned from myrpc(%d, '%s')\n", i, s);
    free(s); 

    debug_printf("finished client\n");
}


static void start_client(void)
{
    errval_t err;

    struct xmplthc_binding *b;
    struct xmplthc_thc_client_binding_t *cl;

    err = xmplthc_thc_connect_by_name(service_name,
                                      get_default_waitset(),
                                      IDC_BIND_FLAGS_DEFAULT,
                                      &b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not bind (thc)");
    }

    cl = malloc(sizeof(struct xmplthc_thc_client_binding_t));
    assert(cl != NULL);

    err = xmplthc_thc_init_client(cl, b, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not init client (thc)");
    }

    run_client(cl);
}


/* --------------------- Server ------------------------------ */

static void rx_xmplthc_myrpc(struct xmplthc_thc_service_binding_t *sv, 
                             int i)
{
    debug_printf("server: received xmplthc_myrpc_call: %d\n", i);

    // send reply
    char *s = malloc(20);
    if (s != NULL) {
        snprintf(s, 20, "!%d!", i);
    }
    sv->send.myrpc(sv, s);
}



static void rx_xmplthc_mycall(struct xmplthc_thc_service_binding_t *sv, 
                              int i)
{
    debug_printf("server: received xmplthc_mycall: %d\n", i);

    // send reply
    char *s = malloc(20);
    if (s != NULL) {
        snprintf(s, 20, "!%d!", i);
    }
    sv->send.myresponse(sv, s);
}


static void rx_xmplthc_mymsg(struct xmplthc_thc_service_binding_t *sv, 
                           int i)
{
    debug_printf("server: received xmplthc_msg: %d\n", i);
}

static void run_server(struct xmplthc_thc_service_binding_t *sv)
{
    xmplthc_service_msg_t msg;
    bool loop = true;
  
    // this is the bitmap of messages we are interested in receiving
    struct xmplthc_service_selector selector = {
        .mymsg = 1,
        .mycall = 1,
        .myrpc = 1,
    };

    while (loop) {

        // receive any message
        sv->recv_any(sv, &msg, selector);

        // dispatch it
        switch(msg.msg) {
        case xmplthc_mymsg:
            rx_xmplthc_mymsg(sv, msg.args.mymsg.i);
            break;
        case xmplthc_mycall:
            rx_xmplthc_mycall(sv, msg.args.mycall.i);
            break;
        case xmplthc_myrpc:
            rx_xmplthc_myrpc(sv, msg.args.myrpc.in.i);
            break;
        default:
            debug_printf("unexpected message: %d\n", msg.msg);
            loop = false;
            break;
        }
    }
}



static void start_server(void)
{

    errval_t err;

    struct xmplthc_thc_export_info e_info;
    struct xmplthc_thc_service_binding_t *sv;
    struct xmplthc_binding *b;
    iref_t iref;

    err = xmplthc_thc_export(&e_info, service_name, 
                             get_default_waitset(),
                             IDC_EXPORT_FLAGS_DEFAULT,
                             &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "thc export failed");
    }

    err = xmplthc_thc_accept(&e_info, &b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "thc accept failed");
    }

    sv = malloc(sizeof(struct xmplthc_thc_service_binding_t));
    assert(sv != NULL);

    err = xmplthc_thc_init_service(sv, b, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "thc init failed");
    }

    run_server(sv);
}



/* --------------------- Main ------------------------------ */

int thcmain(int argc, char *argv[]) 
{
    if ((argc >= 2) && (strcmp(argv[1], "client") == 0)) {
        start_client();
    } else if ((argc >= 2) && (strcmp(argv[1], "server") == 0)) {
        start_server();
    } else {
        printf("usage: %s client|server\n", argv[0]);
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
