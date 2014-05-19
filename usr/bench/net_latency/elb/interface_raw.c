/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "elb.h"

#include <barrelfish/net_constants.h>
#include <if/net_queue_manager_defs.h>
#include <barrelfish/bulk_transfer_arch.h>


#include <contmng/contmng.h>

void terminate_benchmark(void)
{
    net_if_terminate();
    exit(-1);
}

static void process_cmdline(int argc, char* argv[])
{
    int i;
    for (i = 1; i < argc; i++) {
        benchmark_argument(argv[i]);
    }
}

static void eventloop(void)
{
    struct waitset *ws = get_default_waitset();

    while (1) {
        event_dispatch_non_block(ws);
        benchmark_do_pending_work();
    }
}

int main(int argc, char* argv[])
{
    printf("elb_app: Started, v3\n");
    process_cmdline(argc, argv);

    benchmark_init();
    eventloop();
}

