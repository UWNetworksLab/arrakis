/** \file
 *  \brief Memory server benchmark application, repeatedly request memory
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Spawn benchmark on given # of cores. 
 * Request memory on each core until it runs out.
 * This benchmark program does not wait on any barriers before starting. 
 */

#include <stdio.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <trace/trace.h>

#include <barrelfish/spawn_client.h>

#include "memtest_trace.h"


#define MEM_BITS 20

static void run_benchmark_0(coreid_t core)
{
    assert(core == 0);

    errval_t err;
    struct capref ramcap;

    debug_printf("starting benchmark. allocating mem of size: %d\n", MEM_BITS);
    
    int i = -1;

    do {
        i++;
        err = ram_alloc(&ramcap, MEM_BITS);
    } while (err_is_ok(err) && (i < 10));

    debug_printf("done benchmark. allocated %d caps (%lu bytes)\n", 
                 i, i * (1UL << MEM_BITS));
}


static void run_benchmark(coreid_t core)
{
    errval_t err;
    struct capref ramcap;

    debug_printf("starting benchmark. allocating mem of size: %d\n", MEM_BITS);
    
    int i = -1;

    do {
        i++;
        err = ram_alloc(&ramcap, MEM_BITS);
    } while (err_is_ok(err));

    debug_printf("done benchmark. allocated %d caps (%lu bytes)\n", 
                 i, i * (1UL << MEM_BITS));

    ns_barrier_register_n(core, "mem_bench");
}


int main(int argc, char *argv[]) 
{
    errval_t err;

    coreid_t mycore = disp_get_core_id();

    debug_printf("This is mem_bench\n");

    if (argc >= 2) {
        assert(mycore == 0);

        int num_cores = strtol(argv[1], NULL, 10);

        debug_printf("spawning on %d cores\n", num_cores);

        err = init_tracing();
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "initialising tracing");
            return EXIT_FAILURE;
        }
        prepare_dump();

        start_tracing();

        char *path = argv[0];
        argv[1] = NULL;

        for (int i = 1; i <= num_cores; i++) {
            err = spawn_program(i, path, argv, NULL, 0, NULL);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "failed spawn %d", i);
                return EXIT_FAILURE;
            } 
            debug_printf("spawned on core %d\n", i);
        }

        //start_tracing();

        run_benchmark_0(mycore);

        ns_barrier_master(1, num_cores, "mem_bench");

        debug_printf("all benchmarks completed\n");

        stop_tracing();
        // dump_trace();
    } else {
        run_benchmark(mycore);
    }

    return EXIT_SUCCESS;
}
