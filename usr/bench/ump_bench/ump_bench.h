/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef URPC_BENCH_H
#define URPC_BENCH_H

#include <stdlib.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <if/monitor_defs.h>

#include <bench/bench.h>
#include <if/bench_defs.h>

#define NUM_MSGS 16

struct timestamps {
    cycles_t time0;
    cycles_t time1;
};

extern struct bench_binding *array[MAX_CPUS];
extern coreid_t my_core_id;

void experiment(coreid_t index);

#endif // URPC_BENCH_H
