/**
 * \file
 * \brief Time coordinator
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"

/// Minimum clock-sync slack is a timeslice
// XXX: Should be set from kernel configuration, but it's not available here
#define MIN_DELAY_MS            80

/**
 * Maximum clock-sync slack is 8 timeslices.
 * Should be #MAX_DELAY_MS == #MIN_DELAY_MS * 2^x, for an arbitrary x >= 0.
 */
#define MAX_DELAY_MS            (8 * MIN_DELAY_MS)

static int received;
static errval_t error;

void timing_sync_timer_reply(errval_t err)
{
    received++;

    // Remember only the first error
    if(err_is_fail(err) && error == SYS_ERR_OK) {
        error = err;
    }
}

errval_t timing_sync_timer(void)
{
#if defined(__x86_64__) || defined(__i386__)
    uint64_t tscperms;
    errval_t err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    // Exponential backoff loop
    for(uint64_t time_offset = MIN_DELAY_MS;
        time_offset <= MAX_DELAY_MS;
        time_offset *= 2) {
        uint64_t synctime = rdtsc() + tscperms * time_offset;
        int waitfor = 0;

        received = 0;
        error = SYS_ERR_OK;

        for(int i = 0; i < MAX_CPUS; i++) {
            struct intermon_binding *b = NULL;
            err = intermon_binding_get(i, &b);
            if(err_no(err) == MON_ERR_NO_MONITOR_FOR_CORE) {
                continue;
            }
            assert(err_is_ok(err));
            err = b->tx_vtbl.rsrc_timer_sync(b, NOP_CONT, synctime);
            assert(err_is_ok(err));

            waitfor++;
        }

        err = invoke_monitor_sync_timer(synctime);
        if(err_is_fail(err)) {
            error = err;
        }

        // Collect success/failure replies
        while(received < waitfor) {
            messages_wait_and_handle_next();
        }

        if(err_is_fail(error)) {
            if(err_no(error) != SYS_ERR_SYNC_MISS) {
                return error;
            }
        } else {
            break;
        }
    }

    return error;
#else
    printf("Phase-locked local clocks not supported on this platform!\n");
    return SYS_ERR_OK;
#endif
}

#define ITERATIONS      1000

void timing_sync_bench(void)
{
    static cycles_t timestamp[ITERATIONS];

    for(int i = 0; i < ITERATIONS; i++) {
        cycles_t start = bench_tsc();
        errval_t err = timing_sync_timer();
        assert(err_is_ok(err));
        cycles_t end = bench_tsc();
        timestamp[i] = end - start;
    }

    for(int i = 0; i < ITERATIONS; i++) {
        printf("duration %d: %" PRIuCYCLES "\n", i, timestamp[i]);
    }

    int nthreads = 0;
    for(int i = 0; i <= MAX_COREID; i++) {
        struct intermon_binding *b = NULL;
        errval_t err = intermon_binding_get(i, &b);
        if(err_is_ok(err) && b != NULL) {
            nthreads++;
        }
    }

    printf("number of threads: %d\n", nthreads);
    printf("client done.\n");
}
