/** \file
 *  \brief Simple sleep call
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h>

#include <bench/bench.h>

#include "sleep.h"

#if defined(__x86_64__)
static uint64_t tscperms;
#endif

static bool initialised = false;

void sleep_init(void)
{
    if (!initialised) {
        bench_init();
#if defined(__x86_64__)
        errval_t err = sys_debug_get_tsc_per_ms(&tscperms);
        assert(err_is_ok(err));
#endif
        initialised = true;
    }
}


void cycle_sleep(uint64_t cycles)
{
    if (!initialised) {
        sleep_init();
    }

    uint64_t start = bench_tsc();
    uint64_t stop = bench_tsc();
    while ((stop - start) < cycles) {
        //        sys_yield(CPTR_NULL);
        thread_yield_dispatcher(NULL_CAP);
        stop = bench_tsc();
    }
}

// FIXME: this should be determined at runtime from the kernel...
// #define TSC_PER_MS 2513385

void milli_sleep(uint64_t ms)
{
#if defined(__x86_64__)
    uint64_t cycles = ms * tscperms;
    cycle_sleep(cycles);
#else
    USER_PANIC("milli_sleep NYI for non-x86_64");
#endif
}

