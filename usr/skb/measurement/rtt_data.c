/**
 * \file
 * \brief This file queries the local monitor for its latency data to all other cores
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/monitor_client.h>
#include <skb/skb.h>
#include "datagatherer.h"


struct measurement_data {
    uint64_t avg;
    uint64_t var;
    uint64_t min;
    uint64_t max;
    uint64_t to_core;
    uint64_t from_core;
};

static struct measurement_data measurements[MAX_CPUS];
static int nr_valid_measurements = 0;
static int nr_expected_measurements = 0;

bool rtt_done;

static void rtt_value(struct monitor_client_response *st,
                      uint64_t from_core, uint64_t to_core, uint64_t avg,
                      uint64_t var, uint64_t min, uint64_t max)
{
/*
    printf("message_rtt(%lu, %lu, %lu, %lu, %lu, %lu).\n",
                 from_core, to_core, avg, var, min, max);
    skb_add_fact("message_rtt(%lu, %lu, %lu, %lu, %lu, %lu).",
                 from_core, to_core, avg, var, min, max);
*/
    measurements[nr_valid_measurements].avg = avg;
    measurements[nr_valid_measurements].var = var;
    measurements[nr_valid_measurements].min = min;
    measurements[nr_valid_measurements].max = max;
    measurements[nr_valid_measurements].to_core = to_core;
    measurements[nr_valid_measurements].from_core = from_core;
    nr_valid_measurements++;

    if (nr_valid_measurements == nr_expected_measurements) {
        for (int i = 0; i < nr_valid_measurements; i++) {
            skb_add_fact("message_rtt(%lu, %lu, %lu, %lu, %lu, %lu).",
                 measurements[i].from_core,
                 measurements[i].to_core,
                 measurements[i].avg,
                 measurements[i].var,
                 measurements[i].min,
                 measurements[i].max);
        }
    }

    rtt_done = true;
}

static void nr_rtt_values(struct monitor_client_response *st, uint64_t nr)
{
    nr_expected_measurements = nr;
    errval_t err = st->call_vtbl->get_rtt_values(st);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "get_rtt_values\n");
    }
}

void gather_rtt_data(struct monitor_client_response *st)
{
    /* Set handlers */
    get_monitor_closure()->f->rtt_value = rtt_value;
    get_monitor_closure()->f->nr_rtt_values = nr_rtt_values;

    errval_t err = st->call_vtbl->get_nr_rtt_values(st);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "get_nr_rtt_values\n");
    }
}
