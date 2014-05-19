/**
 * \file
 * \brief Scheduler test
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/resource_ctrl.h>
#include <bench/bench.h>

#define ITERATIONS      1000000

static const char *my_manifest =
    "P 0 0 0 0 0 0\n"
    "P 0 2 0 0 0 0\n";
static rsrcid_t my_rsrc_id;

int main(int argc, char *argv[])
{
    uint64_t start, now;

    bench_init();

    // Submit manifest
    errval_t err = rsrc_manifest(my_manifest, &my_rsrc_id);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "rsrc_manifest");
        abort();
    }

    // Enter best-effort phase
    printf("Starting best-effort tests...\n");
    err = rsrc_phase(my_rsrc_id, 0);
    assert(err_is_ok(err));

    // Check that we're running
    start = bench_tsc();
    for(int i = 0; i < ITERATIONS; i++) {
        now = bench_tsc();
    }
    if(now - start < ITERATIONS) {
        printf("Something's wrong with the time\n");
        abort();
    }

    // Enter hard real-time phase
    printf("Starting hard real-time tests...\n");
    err = rsrc_phase(my_rsrc_id, 1);
    assert(err_is_ok(err));

    // Check that we don't miss a deadline
    for(int i = 0; i < ITERATIONS; i++) {
        start = bench_tsc();
        /* slice =  */
        /* while(bench_tsc() < start + DEADLINE); */
        /* if(bench_tsc() > start + DEADLINE */
        /*    start = bench_tsc(); */
    }

    return 0;
}
