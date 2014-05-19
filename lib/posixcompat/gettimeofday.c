/*
 * Copyright (c) 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/time.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h>

// This is roughly Mon Apr 25 13:50 CEST 2011
#define TOD_OFFSET      1303732456ULL

int gettimeofday(struct timeval *tv, struct timezone *tz)
{
    uint64_t now = rdtsc();
    static uint64_t tscperms = 0;

    if(tscperms == 0) {
        errval_t err = sys_debug_get_tsc_per_ms(&tscperms);
        assert(err_is_ok(err));
        assert(tscperms >= 1000);
    }

    uint64_t tod_us = (TOD_OFFSET * 1000000) + (now / (tscperms / 1000));

    if(tv != NULL) {
        tv->tv_sec = tod_us / 1000000;
        tv->tv_usec = tod_us % 1000000;
    }

    assert(tz == NULL);
    if(tz != NULL) {
    }

    return 0;
}
