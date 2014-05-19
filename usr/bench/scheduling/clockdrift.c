/**
 * \file
 * \brief APIC timer drift benchmark
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
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/sys_debug.h>
#include <bench/bench.h>

/// Number of iterations (experiment will run for 12 hours)
#define ITERATIONS      43200
//#define ITERATIONS      10

/// Delay between iterations in milliseconds
#define DELAY           1000

typedef volatile uintptr_t spinflag_t;

union padstruct {
    spinflag_t start;
    uint8_t pad[64];
};

static int init_done = 1;
static union padstruct startflag[MAX_CPUS] __attribute__ ((aligned(64)));
static uint32_t timestamp[MAX_CPUS][ITERATIONS];
static int nthreads;
static uint64_t tscperms = 0;

static void domain_spanned(void *arg, errval_t reterr)
{
  assert(err_is_ok(reterr));
  init_done++;
}

static int apic_measure_loop(void *arg)
{
    coreid_t mycore = disp_get_core_id(),
        nextcore = (mycore + 1) % nthreads;
    uint64_t lasttsc = 0, iteration = 0, tscdelay = DELAY * tscperms;

    for(;;) {
        if(mycore != 0 || lasttsc != 0) {
            // Wait for signal and reset
            while(startflag[mycore].start == 0);
            startflag[mycore].start = 0;
        }

        // Get local APIC timer value
        errval_t err = sys_debug_get_apic_timer(&timestamp[mycore][iteration]);
        assert(err_is_ok(err));

        iteration++;

        if(mycore == 0 && lasttsc != 0) {       // Master
            if(iteration == ITERATIONS) {
                // We're done
                return 0;
            }

            // Wait for rest of DELAY
            uint64_t tscnow;

            do {
                tscnow = rdtsc();
            } while(tscnow < lasttsc + tscdelay);

            lasttsc = tscnow;
        } else {
            lasttsc = rdtsc();
        }

        // Signal next core
        startflag[nextcore].start = 1;
    }

    return 0;
}

int main(int argc, char *argv[])
{
    int my_core_id = disp_get_core_id();
    errval_t err;

    if(argc < 2) {
        printf("Usage: %s threads\n", argv[0]);
    }
    nthreads = atoi(argv[1]);

    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    uint32_t tickspersec;
    err = sys_debug_get_apic_ticks_per_sec(&tickspersec);
    assert(err_is_ok(err));
    printf("APIC ticks per second: %" PRIu32 "\n", tickspersec);

    bench_init();

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

    // Start all threads
    for (int i = my_core_id + 1; i < nthreads + my_core_id; i++) {
        err = domain_thread_create_on(i, apic_measure_loop, NULL);
        assert(err_is_ok(err));
    }

    // Start locally
    apic_measure_loop(NULL);

    printf("Running on %d cores.\n", nthreads);

    // Output data
    for(uint64_t i = 0; i < ITERATIONS; i++) {
        printf("%" PRIu64 ": ", i);
        for(int n = 0; n < nthreads; n++) {
            printf("%" PRIu32 " ", timestamp[n][i]);
        }
        printf("\n");
    }

    printf("client done.\n");
    return 0;
}
