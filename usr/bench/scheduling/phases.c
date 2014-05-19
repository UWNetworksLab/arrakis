/**
 * \file
 * \brief Phase-change performance benchmark
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/resource_ctrl.h>
#include <barrelfish/sys_debug.h>
#include <bench/bench.h>

#define ITERATIONS      1000
#define DELAY           100     /// Cooldown delay between iterations in ms

static const char *my_manifest =
    "B 1\n"                     // Normal phase
    "B 1\n";                    // Other normal phase

static int init_done = 1;
static rsrcid_t my_rsrc_id;
static struct thread_sem init_sem = THREAD_SEM_INITIALIZER;

static int remote_init(void *arg)
{
    errval_t err = rsrc_join(my_rsrc_id);
    assert(err_is_ok(err));
    thread_sem_post(&init_sem);
    return 0;
}

static void domain_spanned(void *arg, errval_t reterr)
{
    assert(err_is_ok(reterr));
    init_done++;
}

int main(int argc, char *argv[])
{
    int my_core_id = disp_get_core_id();
    errval_t err;
    static cycles_t timestamp[ITERATIONS];
    uint64_t tscperms;

    if(argc < 2) {
        printf("Usage: %s threads\n", argv[0]);
    }
    int nthreads = atoi(argv[1]);

    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    bench_init();

    // Submit manifest (derived from program)
    err = rsrc_manifest(my_manifest, &my_rsrc_id);

    /* Span domain to all cores */
    for (int i = my_core_id + 1; i < nthreads + my_core_id; i++) {
        err = domain_new_dispatcher(i, domain_spanned, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to span domain");
        }
        assert(err_is_ok(err));
    }

    while(init_done < nthreads) {
        thread_yield();
    }

    for (int i = my_core_id + 1; i < nthreads + my_core_id; i++) {
        err = domain_thread_create_on(i, remote_init, NULL);
        assert(err_is_ok(err));
        thread_sem_wait(&init_sem);
    }

    // Run benchmark
    bool flip = false;
    for(int i = 0; i < ITERATIONS; i++) {
        flip = !flip;
        cycles_t start = bench_tsc();
        err = rsrc_phase(my_rsrc_id, flip ? 1 : 0);
        assert(err_is_ok(err));
        cycles_t end = bench_tsc();
        timestamp[i] = end - start;

        // Let it cool down
        while(bench_tsc() < end + (DELAY * tscperms));
    }

    for(int i = 0; i < ITERATIONS; i++) {
        printf("duration %d: %" PRIuCYCLES "\n", i, timestamp[i]);
    }

    printf("number of threads: %d\n", nthreads);
    printf("client done.\n");
    return 0;
}
