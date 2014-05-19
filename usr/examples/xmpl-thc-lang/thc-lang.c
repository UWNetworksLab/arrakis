/** \file
 *  \brief Example application using THC stubs and language features
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

const char *service_name = "xmplthc_lang_service";

/* --------------------- Client ------------------------------ */

static void run_client(struct xmplthc_thc_client_binding_t *cl, int id)
{
    int i = 42 + id;
    char *s = NULL;

    while (true) {
        cl->call_seq.myrpc(cl, i, &s);
        free(s); 
    }

}


static void start_client(char *arg)
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

    run_client(cl, atoi(arg));
}


/* --------------------- Server ------------------------------ */

static void rx_xmplthc_myrpc(struct xmplthc_thc_service_binding_t *sv, 
                             int i)
{
    debug_printf("received xmplthc_myrpc_call: %d\n", i);

    // send reply
    char *s = malloc(20);
    if (s != NULL) {
        snprintf(s, 20, "!%d!", i);
    }
    sv->send.myrpc(sv, s);
}




static void run_server(struct xmplthc_thc_service_binding_t *sv)
{
    xmplthc_service_msg_t msg;
    bool loop = true;
  
    // this is the bitmap of messages we are interested in receiving
    struct xmplthc_service_selector selector = {
        .mymsg = 0,
        .mycall = 0,
        .myrpc = 1,
    };

    while (loop) {
        // receive any message
        sv->recv_any(sv, &msg, selector);

        // dispatch it
        switch(msg.msg) {
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

    do {

        while (true) {

            debug_printf("server: waiting for another connection\n");

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
        
            async run_server(sv);
        }

    } finish;
}



/* --------------------- Main ------------------------------ */

int thcmain(int argc, char *argv[]) 
{
    if ((argc >= 2) && (strcmp(argv[1], "client") == 0)) {
        start_client(argv[2]);
    } else if ((argc >= 2) && (strcmp(argv[1], "server") == 0)) {
        start_server();
    } else {
        printf("usage: %s client|server\n", argv[0]);
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
