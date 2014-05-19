/**
 * \file net_openport_test.c
 * \brief A test program to check the working of port management.
 * This programs open specified number of ports.
 */

/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <lwip/init.h>
#include <lwip/tcp.h>


static void event_polling_loop(void)
{
    err_t err;
    printf("Starting event polling loop\n");
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
}

int main(int argc, char *argv[])
{
    if(argc != 3) {
        printf("USAGE: %s <start_port_no> <no. of ports>\n", argv[0]);
        printf("EXAMPLE: %s 50 100\n", argv[0]);
        exit(1);
    }

    uint16_t start_port_range = atoi(argv[1]);
    int ports_to_bind = atoi(argv[2]);
    uint16_t port;
    int port_count = 0;
    err_t err;

    // Connect to the network driver driver
    assert(lwip_init_auto() == true);

    printf("openport_test: setup done\n");


    // Stopping the application here
    event_polling_loop();
    return 0;

    printf("openport_test: binding %d tcp ports starting from %u\n",
            ports_to_bind, start_port_range);

    port_count = 0;
    port = start_port_range;
    for (port_count = 0; port_count < ports_to_bind; ++port_count){

        printf("openport_test: opening port %u\n", port);
        struct tcp_pcb *pcb = tcp_new();
        if (pcb == NULL) {
            printf("openport_test: tcp_new failed in opening port %u\n", port);
            return ERR_MEM;
        }

        do {
            err = tcp_bind(pcb, IP_ADDR_ANY, port);
            if(err != ERR_OK) {
                if (err == ERR_USE){
                    printf("port %u is already in use\n", port);
                }
                else{
                    printf("some prob in opening port, exiting!!\n");
                    return(err);
                }
            }
            printf("openport_test: port %u opened\n", port);
            ++port;
        } while(err != ERR_OK);
    } /* end for: each port */

    printf("openport_test: total %d ports opened\n", port_count);

    event_polling_loop();
    return 0;

} /* end function: main */

