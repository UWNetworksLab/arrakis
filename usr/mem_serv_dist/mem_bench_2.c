/** \file
 *  \brief Memory server benchmark application using spanning
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
 * Span benchmark on given # of cores. The spanning is traced.
 * This benchmark program waits on barriers to ensure that the mem_serv 
 * is started before it commences.
 */

#include <stdio.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

#include "memtest_trace.h"


bool all_spanned = false;
int num_span = -1;

static void span_cb(void *arg, errval_t err)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "span failed");
        return;
    }

    static int num_spanned = 0;

    num_spanned++;
    if (num_spanned >= num_span) {
        all_spanned = true;
    }

    debug_printf("span %d succeeded\n", num_spanned);
}


int main(int argc, char *argv[]) 
{
    errval_t err;

    coreid_t mycore = disp_get_core_id();

    debug_printf("This is mem_bench_2\n");

    if (argc < 2) {
        printf("usage: %s <num_cores>\n", argv[0]);
        return EXIT_FAILURE;
    }
     
    assert(mycore == 0);

    num_span = strtol(argv[1], NULL, 10);

    debug_printf("spanning on %d cores\n", num_span);

    err = init_tracing();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "initialising tracing");
        return EXIT_FAILURE;
    }
    prepare_dump();

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_STARTED, 0);

    start_tracing();

    /*
    struct capref cap;
    err = ram_alloc(&cap, 10);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed ram_alloc");
    }
    */


    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_RUN, 0);

    for (int i = 1; i <= num_span; i++) {
        err = domain_new_dispatcher(mycore + i, span_cb, NULL);
            
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed span %d", i);
        } else {
            debug_printf("dispatcher %d on core %d spanned\n",
                         i, mycore+i);
        }   
    }

    debug_printf("waiting for all spans to complete\n");
    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_WAIT, 0);

    while (!all_spanned) {
        thread_yield();
    }

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_DONE, 0);
    debug_printf("all spans completed\n");

    stop_tracing();
    // dump_trace();
    
    return EXIT_SUCCESS;
}
