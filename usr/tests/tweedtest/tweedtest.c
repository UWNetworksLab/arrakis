/** \file 
 *  \brief Testing code for the Cilk-like Tweed library. 
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include "tweed/tweed.h"

#include <stdio.h>
#include <barrelfish/threads.h>

#include "trace/trace.h"
#include <trace_definitions/trace_defs.h>
#include <arch/x86/barrelfish_kpi/asm_inlines_arch.h>


TASK(int, fib, 1, int, n) {
    if (n < 2) {
        return n;
    } else {
        SPAWN(fib, 1, n-1);
        int a = CALL(fib, 1, n-2);
        int b = SYNC(fib, 1);
        return a+b;
    }
}

MAIN_TASK(main, args) {
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_BENCH_PCBENCH, 1);
    trace_event(TRACE_SUBSYS_TWEED, TRACE_EVENT_TWEED_START, 0);
    uint64_t start = rdtsc();
    int fib_res = CALL(fib, 1, 35);
    uint64_t end = rdtsc();
    trace_event(TRACE_SUBSYS_TWEED, TRACE_EVENT_TWEED_END, 0);
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_BENCH_PCBENCH, 0);
    printf("cycles taken - %ld\n", (end - start));
    printf("result - %d\n", (fib_res));
    return 0;
}

int main (int argc, char* argv[]) {
    INIT_TWEED((atoi(argv[1])), main, NULL);
    return 0;
}
