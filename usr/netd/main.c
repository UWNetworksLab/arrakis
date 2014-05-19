/**
 * \file
 * \brief Echo server main
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
#include <barrelfish/net_constants.h>

// For event loops
#include <barrelfish/dispatch.h>

// standard include files
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "netd.h"
#include "netd_debug.h"

bool do_dhcp = true; // flag to control use of dhcp
// IP information for static configuration
char *ip_addr_str = NULL; // IP address for this interface
char *netmask_str = NULL; // netmask for this LAN
char *gateway_str = NULL; // default gateway address
char *dns_str = NULL; // ip address of DNS name server


static void netd_event_polling_loop(void)
{
    errval_t err;
    NETD_DEBUG("Starting event polling loop!!!\n");
    struct waitset *ws = get_default_waitset();
    uint32_t ecounter = 0;
    while (1) {
        err = event_dispatch_debug(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
//        NETD_DEBUG("event %"PRIu32" handled\n", ecounter);
        ecounter++;
    }
}


/****************************************************************************
 * netd main function
 ***************************************************************************/

int main(int argc, char **argv)
{
    char *card_name = NULL;
    uint64_t allocated_queue = 0;

    uint64_t minbase = -1, maxbase = -1;

    NETD_DEBUG("running on core %d\n", disp_get_core_id());
    NETD_DEBUG("###################################################\n");


    /* Read commandline args */
    for (int i = 0; i < argc; i++) {
        if (strncmp(argv[i], "affinitymin=", strlen("affinitymin=")) == 0) {
            minbase = atol(argv[i] + strlen("affinitymin="));
            NETD_DEBUG("minbase = %" PRIu64 "\n", minbase);
        }
        if (strncmp(argv[i], "affinitymax=", strlen("affinitymax=") - 1) == 0) {
            maxbase = atol(argv[i] + strlen("affinitymax="));
            NETD_DEBUG("maxbase = %" PRIu64 "\n", maxbase);
        }
        if (strncmp(argv[i], "cardname=", strlen("cardname=") - 1) == 0) {
            card_name = argv[i] + strlen("cardname=");
            NETD_DEBUG("card name = %s\n", card_name);
        }
        if (!strcmp(argv[i], "do_dhcp=0")) {
            do_dhcp = false;
            NETD_DEBUG("using static IP address\n");
        }

        if (strncmp(argv[i], "ip=", strlen("ip=") - 1) == 0) {
            ip_addr_str = argv[i] + strlen("ip=");
        }

        if (strncmp(argv[i], "nm=", strlen("nm=") - 1) == 0) {
            netmask_str = argv[i] + strlen("nm=");
        }

        if (strncmp(argv[i], "gw=", strlen("gw=") - 1) == 0) {
            gateway_str = argv[i] + strlen("gw=");
        }

        if (strncmp(argv[i], "dns=", strlen("dns=") - 1) == 0) {
            dns_str = argv[i] + strlen("dns=");
            printf("Ignoring the argument [%s] as it is not supported yet!\n",
                    argv[i]);
        }

    } // end for: for each argument

    if (card_name == NULL) {
        fprintf(stderr,
                "Error: netd: card name not specified, but it is required\n");
        fprintf(stderr, "Hint: try \"netd cardname=e1000\"\n");
        return 1;
    }

    if (!do_dhcp) {
        // Making sure that we have enough info for static configuration
        if ((ip_addr_str == NULL) || (netmask_str == NULL)
                || (gateway_str == NULL)) {
            USER_PANIC("Error, not enough information provided for static "
                    "IP configuration IP[%s], NM[%s], GW[%s]",
                    ip_addr_str, netmask_str, gateway_str);
            return 1;
        }
    }

    // Set memory affinity if requested
    if ((minbase != -1) && (maxbase != -1)) {
        ram_set_affinity(minbase, maxbase);
    }

    // FIXME: This has to be done for every card
    // Connect to the driver for given card
    NETD_DEBUG("trying to connect to the %s:%"PRIu64" driver...\n",
            card_name, allocated_queue);
    startlwip(card_name, allocated_queue);

    NETD_DEBUG("registering net_ARP service\n");
    // register ARP service
    init_ARP_lookup_service(card_name);

    netd_event_polling_loop();
    return 0;
}

