/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <stdint.h>
#include <omp.h>
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <inttypes.h>

#define STACK_SIZE      (64 * 1024)

int main(int argc, char *argv[])
{
    volatile uint64_t workcnt = 0;
    int nthreads;

    bench_init();

#if CONFIG_TRACE
    errval_t err = trace_control(TRACE_EVENT(TRACE_SUBSYS_ROUTE,
                                             TRACE_EVENT_ROUTE_BENCH_START, 0),
                                 TRACE_EVENT(TRACE_SUBSYS_ROUTE,
                                             TRACE_EVENT_ROUTE_BENCH_STOP, 0), 0);
    assert(err_is_ok(err));
#endif

    if(argc == 2) {
        nthreads = atoi(argv[1]);
        backend_span_domain(nthreads, STACK_SIZE);
        bomp_custom_init();
        omp_set_num_threads(nthreads);
    } else {
        assert(!"Specify number of threads");
    }

    trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_ROUTE_BENCH_START, 0);

    uint64_t start = bench_tsc();
#pragma omp parallel
    while(rdtsc() < start + 805000000ULL) {
        workcnt++;
    }
    uint64_t end = bench_tsc();

    trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_ROUTE_BENCH_STOP, 0);

    printf("done. time taken: %" PRIu64 " cycles.\n", end - start);

#if CONFIG_TRACE
        char *buf = malloc(4096*4096);
        trace_dump(buf, 4096*4096, NULL);
        printf("%s\n", buf);
#endif

    for(;;);
    return 0;
}
