/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "ump_bench.h"

#define MAX_COUNT 1000
static struct timestamps *timestamps;

void experiment(coreid_t idx)
{
    timestamps = malloc(sizeof(struct timestamps) * MAX_COUNT);
    assert(timestamps != NULL);

    struct bench_ump_binding *bu = (struct bench_ump_binding*)array[idx];
    struct flounder_ump_state *fus = &bu->ump_state;
    struct ump_chan *chan = &fus->chan;

    struct ump_chan_state *send = &chan->send_chan;
    struct ump_chan_state *recv = &chan->endpoint.chan;

    printf("Running latency between core %d and core %d\n", my_core_id, idx);

    /* Run experiment */
    for (int i = 0; i < MAX_COUNT; i++) {
        volatile struct ump_message *msg;
        struct ump_control ctrl;
        timestamps[i].time0 = bench_tsc();
        msg = ump_impl_get_next(send, &ctrl);
        msg->header.control = ctrl;
        while (!ump_impl_recv(recv));
        timestamps[i].time1 = bench_tsc();
    }

    /* Print results */
    for (int i = MAX_COUNT / 10; i < MAX_COUNT; i++) {
        if (timestamps[i].time1 > timestamps[i].time0) {
            printf("page %d took %ld\n", i,
                   timestamps[i].time1 - bench_tscoverhead() -
                   timestamps[i].time0);
        }
    }
}
