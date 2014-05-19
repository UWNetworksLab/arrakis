/**
 * \file
 * \brief LWIP web server
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <stdio.h>
#include <lwip/netif.h>
#include <lwip/dhcp.h>
#include <netif/etharp.h>
#include <lwip/init.h>
#include <lwip/tcp.h>
#include <netif/bfeth.h>
#include <contmng/netbench.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

#include "webserver_network.h"
#include "webserver_debug.h"

static struct ip_addr serverip;
static const char *serverpath;

/* Enable tracing only when it is globally enabled */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define ENABLE_WEB_TRACING 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE


int main(int argc, char**argv)
{
    errval_t err;

    // Parse args
    if (argc != 4) {
        printf("Usage: %s CardName NFSIP NFSpath\n", argv[0]);
        return 1;
    }
//    char *card_name = argv[1];

    struct in_addr server1;
    if (inet_aton(argv[2], &server1) == 0) {
        printf("Invalid IP addr: %s\n", argv[2]);
        return 1;
    }
    serverip.addr = server1.s_addr; // XXX
    serverpath = argv[3];

    // Boot up
    DEBUGPRINT("init start\n");

    DEBUGPRINT("lwip_demo: lwip setup\n");
    printf("webserver:%u: initializing networking \n", disp_get_core_id());
    if (lwip_init_auto() == false) {
        printf("ERROR: lwip_init_auto failed!\n");
        return 1;
    }
    printf("webserver:%u: networking initialized\n", disp_get_core_id());

//    lwip_benchmark_control(1, BMS_START_REQUEST, 0, 0);
    http_server_init(serverip, serverpath);

    DEBUGPRINT("Init finished.\n");

    uint32_t eventcount = 0;
    struct waitset *ws = get_default_waitset();
    while (1) {
        // check for any event without blocking
        //err = event_dispatch_non_block(ws);
        err = event_dispatch_non_block(ws);
        if (err != LIB_ERR_NO_EVENT) {
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch");
                break;
            }
        }

//        printf("webserver:%u:  dispatching next event\n", disp_get_core_id());

        // Check if lwip has any pending work to finish
        wrapper_perform_lwip_work();
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
        eventcount++;
#if ENABLE_WEB_TRACING
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_WEBEVENTLOOP, eventcount);
#endif // ENABLE_WEB_TRACING

    } // end while: infinite

}
