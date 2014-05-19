/**
 * \file
 * \brief Echo server main
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <lwip/init.h>
#include "echoserver.h"

static uint64_t minbase = -1, maxbase = -1;

void network_polling_loop(void);

static void connect_to_network(char *card_name, uint64_t queueid)
{
    if (lwip_init(card_name, queueid) == false) {
        printf("Error in lwip_init: could not start networking!!!\n");
        abort();
    }

    printf("ECHOSERVER: starting TCP server on port 7\n");
    int r = tcp_echo_server_init();
    assert(r == 0);

    printf("ECHOSERVER: starting UDP server on port 7\n");
    r = udp_echo_server_init();
    assert(r == 0);
}

int main(int argc, char**argv)
{

    static uint64_t allocated_queueid = 0;
    printf("%s running on core %u\n", argv[0], disp_get_core_id());

    /* Read commandline args */
    char *card_name = NULL;
    for (int i = 0; i < argc; i++) {
        if(strncmp(argv[i],"affinitymin=",strlen("affinitymin="))==0) {
            minbase = atol(argv[i] + strlen("affinitymin="));
            printf("minbase = %lu\n", minbase);
        }
        if(strncmp(argv[i],"affinitymax=",strlen("affinitymax=")-1)==0) {
            maxbase = atol(argv[i] + strlen("affinitymax="));
            printf("maxbase = %lu\n", maxbase);
        }
        if(strncmp(argv[i],"cardname=",strlen("cardname=")-1)==0) {
            card_name = argv[i] + strlen("cardname=");
            printf("card name = %s\n", card_name);
        }
        if(strncmp(argv[i],"queue=",strlen("queue=")-1)==0) {
            allocated_queueid = atol(argv[i] + strlen("queue="));
            printf("queue = %"PRIu64"\n", allocated_queueid);
        }
    }

    /* Set memory affinity if requested */
    if ((minbase != -1) && (maxbase != -1)) {
        ram_set_affinity(minbase, maxbase);
    }

    /* Connect to e1000 driver */
    printf("%s: trying to connect to the NIC driver...\n", argv[0]);
    connect_to_network(card_name, allocated_queueid);

    printf("echoserver: init finished.\n");

    network_polling_loop();

    return 0;
}

