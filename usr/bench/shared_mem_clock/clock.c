/**
 * \file
 * \brief Microbenchmark to measure the cost of shared memory clock
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include "clock.h"

/*
 * \brief compare and set. If the value at address
 *        equals old, set it to new and return true,
 *        otherwise don't write to address and return false
 */
static inline bool cas(volatile uintptr_t *address, uintptr_t old,
        uintptr_t new)
{
    register bool res;
    __asm volatile("lock; cmpxchgq %2,%0     \n\t"
                   "setz %1                  \n\t"
                   : "+m" (*address), "=q" (res)
                   : "r" (new), "a" (old)
                   : "memory");
    return res;
}

static uintptr_t *pointer;

timestamp_t clock_get_timestamp(void)
{
    while(1) {
        uintptr_t time = *pointer;
        if (cas(pointer, time, time + 1)) {
            return time;
        }
    }
}

errval_t clock_init(struct capref cap)
{
    errval_t err;
    void *buf;
    err = vspace_map_one_frame(&buf, BASE_PAGE_SIZE, cap, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "clock_init failed");
    }
    pointer = (uintptr_t*)buf;

    return SYS_ERR_OK;
}
