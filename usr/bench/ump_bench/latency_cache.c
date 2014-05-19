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
#include <string.h>
#include <barrelfish/sys_debug.h>
#include <arch/x86/barrelfish/perfmon.h>
#include <arch/x86/barrelfish_kpi/perfmon_amd.h>
#include <barrelfish/dispatcher_arch.h>

#define MAX_COUNT 100
static struct timestamps *timestamps;
static struct timestamps *overhead;

void experiment(coreid_t idx)
{
    timestamps = malloc(sizeof(struct timestamps) * MAX_COUNT);
    assert(timestamps != NULL);
    overhead = malloc(sizeof(struct timestamps) * MAX_COUNT);
    assert(overhead != NULL);

    dispatcher_handle_t my_dispatcher = curdispatcher();

    struct bench_ump_binding *bu = (struct bench_ump_binding*)array[idx];
    struct flounder_ump_state *fus = &bu->ump_state;
    struct ump_chan *chan = &fus->chan;

    struct ump_chan_state *send = &chan->send_chan;
    struct ump_chan_state *recv = &chan->endpoint.chan;
    volatile struct ump_message *msg;
    struct ump_control ctrl;

    printf("Running latency_cache between core %d and core %d\n",
           my_core_id, idx);

    for (int j = 0; j < 2; j++) { /* Pick event type */
        uint64_t event;
        if (j == 0) {
            event = EVENT_AMD_DATA_CACHE_MISSES;
        } else {
            event = EVENT_AMD_INSTRUCTION_CACHE_MISSES;
        }

        /* Measure measurement overhead */
        perfmon_setup(my_dispatcher, 0, event, 0, false);
        for(size_t i = 0; i < MAX_COUNT; i++) {
            overhead[i].time0 = rdpmc(0);
            sys_debug_flush_cache();
            overhead[i].time1 = rdpmc(0);
        }
        sys_debug_print_timeslice();
        sys_debug_print_context_counter();

        /* Run experiment */
        perfmon_setup(my_dispatcher, 0, event, 0, false);
        for (int i = 0; i < MAX_COUNT; i++) {
            timestamps[i].time0 = rdpmc(0);
            sys_debug_flush_cache();
            msg = ump_impl_get_next(send, &ctrl);
            msg->header.control = ctrl;
            while (!ump_impl_recv(recv));
            timestamps[i].time1 = rdpmc(0);
        }
        sys_debug_print_timeslice();
        sys_debug_print_context_counter();

        /* Print results */
        char str[100];
        if (event == EVENT_AMD_DATA_CACHE_MISSES) {
            strcpy(str, "Data cache miss");
        } else {
            strcpy(str, "Instruction cache miss");
        }
        for (int i = MAX_COUNT / 10; i < MAX_COUNT; i++) {
            printf("%s %d %ld %ld\n", str, i,
                   overhead[i].time1 - overhead[i].time0,
                   timestamps[i].time1 - timestamps[i].time0);
        }
    }
}
