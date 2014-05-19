/** \file
 *  \brief IDC system test code
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN /* for strdup() */
#include <string.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/debug.h>
#include <if/ping_pong_defs.h>
#include <if/ping_pong_thc.h>
#include <thc/thc.h>

#define ITERATIONS    4
#define CANCEL_ITERS 32

/* ------------------------------ COMMON ------------------------------ */

static const char *my_service_name = "minitest";
static const char *kind;

/* ------------------------------ CLIENT ------------------------------ */


static void client_work(void) {
    errval_t err;
    uint64_t val;
    struct ping_pong_thc_client_binding_t cl;
    struct ping_pong_binding *b;
    err = ping_pong_thc_connect_by_name(my_service_name,
                                        get_default_waitset(),
                                        IDC_BIND_FLAGS_DEFAULT,
                                        &b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "connect failed");
        abort();
    }

    err = ping_pong_thc_init_client(&cl, b, b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "init failed");
        abort();
    }

    debug_printf("Doing non-RPC test:\n");
    val = 0;
    do {
      debug_printf("    --- %d -->\n", (int)val);
      cl.send.ping(&cl, val);
      cl.recv.pong(&cl, &val);
    } while (val < ITERATIONS);
      
    // Send shutdown request to server
    debug_printf("Finished tests: sending stop request\n");
    cl.send.stop(&cl);
}

/* ------------------------------ SERVER ------------------------------ */

static void service_client(struct ping_pong_thc_service_binding_t *sv) {
  bool stop = false;
  while (!stop) {
    ping_pong_service_msg_t m;
    sv->recv_any(sv, &m, (struct ping_pong_service_selector) {
        .ping=1, .stop=1});
    switch (m.msg) {
    case ping_pong_ping: {
      uint64_t val = m.args.ping.val;
      val++;
      printf("    <-- %d ---\n", (int)val);
      sv->send.pong(sv, val);
      break;
    }
      
    case ping_pong_stop: {
      debug_printf("Service: stopping\n");
      stop = 1;
      break;
    }
      
    default: { 
      assert(0 && "Unexpected message");
      break;
    }
    }
  }
}

static void server_work(void) {
  struct ping_pong_thc_service_binding_t *sv;
  struct ping_pong_binding *b;
  struct ping_pong_thc_export_info info;
  errval_t err;
  iref_t iref;

  debug_printf("Starting server_work\n");
  err = ping_pong_thc_export(&info,
                             my_service_name,
                             get_default_waitset(),
                             IDC_EXPORT_FLAGS_DEFAULT,
                             &iref);
  debug_printf("Done export iref=%"PRIuIREF"\n", iref);
  if (err_is_fail(err)) {
    DEBUG_ERR(err, "export failed");
    abort();
  }

  debug_printf("server waiting for connection\n");
  err = ping_pong_thc_accept(&info, &b);
  if (err_is_fail(err)) {
    DEBUG_ERR(err, "accept failed");
    abort();
  }
  
  sv = malloc(sizeof(struct ping_pong_thc_service_binding_t));
  if (sv == NULL) {
    DEBUG_ERR(err, "malloc failed");
    abort();
  }
  
  err = ping_pong_thc_init_service(sv, b, b);
  if (err_is_fail(err)) {
    DEBUG_ERR(err, "init failed");
    abort();
  }
  
  service_client(sv);
}
  
/* ------------------------------ MAIN ------------------------------ */

int main(int argc, char *argv[])
{
    // Allow arbitrary early parameters (e.g., "boot" when invoked
    // directly from menu.lst by monitor)
    if (argc >= 2 && strcmp(argv[argc-1], "client") == 0) {
      kind = "client";
      client_work();
    } else if (argc >= 2 && strcmp(argv[argc-1], "server") == 0) {
      kind = "server";
      server_work();
    } else {
      debug_printf("Usage: %s ... client|server\n", argv[0]);
      return EXIT_FAILURE;
    }

    printf("%s %s Done!\n", argv[0], kind);
    return EXIT_SUCCESS;
}

