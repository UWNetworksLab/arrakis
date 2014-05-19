/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/waitset.h>
#include <net_device_manager/net_device_manager.h>
#include <barrelfish/nameservice_client.h>
#include "NGD_mng_debug.h"
#include <trace/trace.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
/*
#include <barrelfish/net_constants.h>
*/


#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE


// handle events in infinite loop
static void event_loop(void)
{

    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    } // end while: infinite
} // end function: event_loop


int main(int argc, char **argv)
{
    char *card_name = NULL;
    uint64_t minbase = -1, maxbase = -1;
    uint64_t total_queues = 1;
    uint8_t filter_type = 0; // 0 = software filtering

//    char *device = "loopback";
    // Read commandline args
    for (int i = 0; i < argc; i++) {
        if (strncmp(argv[i], "affinitymin=", strlen("affinitymin=")) == 0) {
            minbase = atol(argv[i] + strlen("affinitymin="));
            NGKDM_DEBUG("minbase = %" PRIu64 "\n", minbase);
        }
        if (strncmp(argv[i], "affinitymax=", strlen("affinitymax=") - 1) == 0) {
            maxbase = atol(argv[i] + strlen("affinitymax="));
            NGKDM_DEBUG("maxbase = %" PRIu64 "\n", maxbase);
        }
        if (strncmp(argv[i], "cardname=", strlen("cardname=") - 1) == 0) {
            card_name = argv[i] + strlen("cardname=");
            NGKDM_DEBUG("card name = %s\n", card_name);
        }
        if (strncmp(argv[i], "totalqueues=", strlen("totalqueues=") - 1) == 0) {
            total_queues = atol(argv[i] + strlen("totalqueues="));
            NGKDM_DEBUG("total queues= %"PRIu64"\n", total_queues);
        }
        if (strncmp(argv[i], "filtertype=", strlen("filtertype=") - 1) == 0) {
            filter_type = (uint8_t) atoi(argv[i] + strlen("filtertype="));
            NGKDM_DEBUG("filter type= %"PRIu8"\n", filter_type);
        }
    }

    if (card_name == NULL) {
        fprintf(stderr,
                "Error: net_dev_mng: card name not specified, but it is required\n");
        fprintf(stderr, "Hint: try \"%s cardname=e1000\"\n", argv[0]);
        return 1;
    }

    assert(total_queues > 0);

//    NGKDM_DEBUG
    printf("Started net_dev_manager for [%s] Q[%"PRIu64"] QT[%"PRIu8"]\n",
            card_name, total_queues, filter_type);

    errval_t err = init_device_manager(card_name, total_queues, filter_type);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "in init_device_manager");
        return 1;
    }

    NGKDM_DEBUG("done with most of things for device[%s]\n", card_name);

#if TRACE_ETHERSRV_MODE
    set_cond_termination(trace_conditional_termination);
#endif

    event_loop();
} // end function: main


