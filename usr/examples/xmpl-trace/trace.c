/** \file
 *  \brief Tracing example application
 */

/*
 * Copyright (c) 2010-2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

// Enable to benchmark the tracing framework.
#define TRACE_EXAMPLE_BENCHMARK 0

static void after_prepare(void *arg);

static volatile bool finished;

static volatile int counter = 0;

static errval_t init_tracing(void)
{
    trace_reset_all();

    debug_printf("after trace reset\n");

    // Tell the trace system when to start and stop.  We can also 
    // provide an overriding maximum duration (in cycles) as the last parameter.
    return trace_control(TRACE_EVENT(TRACE_SUBSYS_XMPL,
                                    TRACE_EVENT_XMPL_START, 0),
                        TRACE_EVENT(TRACE_SUBSYS_XMPL,
                                    TRACE_EVENT_XMPL_STOP, 0), 
                        0);
}

static void start_tracing(void)
{
    // start the trace going by providing the start event
    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_START, 0);
}

static void stop_tracing(void)
{
    // stop the trace by providing the stop event
    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_STOP, 0);
}


#if TRACE_EXAMPLE_BENCHMARK

#define NUM_EVENTS 1000

static void benchmark(void)
{
    uint64_t times[NUM_EVENTS];

    int i = 0;
    for (i=0; i < NUM_EVENTS; i++) {
        uint64_t start = rdtsc();
        trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV1, i);
        uint64_t end = rdtsc();
        times[i] = end-start;
    }

    printf("Call times:\n");
    for (i=0; i < NUM_EVENTS; i++) {
        printf("%" PRIu64 "\n", times[i]);
    }
    printf("Call time end.\n");

}

#endif

// This callback is invoked when the flushing process of the tracing framework
// is finished, so that you can continue your operations.
static void callback(void *arg)
{
    debug_printf("callback invoked\n");

    finished = true;
}

static void dump_trace(void)
{
    // dump the trace on the output.  We can copy and paste it
    // to use in Aquarium.

    debug_printf("the trace dump\n");

    // Let the trace framework decide where to flush to
    trace_flush(MKCLOSURE(callback, NULL));

    debug_printf("finished trace dump\n");

}

static void do_stuff(void)
{
    // generate our own traces

#if TRACE_EXAMPLE_BENCHMARK

    benchmark();

#else

    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

    trace_set_subsys_enabled(TRACE_SUBSYS_KERNEL, false);

    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);

    trace_set_subsys_enabled(TRACE_SUBSYS_KERNEL, true);

    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 2);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV1, 1);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV2, 2);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV1, 3);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV2, 4);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV1, 10);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV2, 11);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV1, 12);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV2, 13);

#endif
}

// Callback that is invoked after the tracing framework
// has been prepared.
static void after_prepare(void *arg)
{
    debug_printf("after_prepare starts");

    start_tracing();

    debug_printf("we are tracing now\n");

    // do stuff to generate traces
    do_stuff();

    stop_tracing();

    // flush the trace buffer
    dump_trace();

}

int main(int argc, char *argv[]) 
{
#ifndef CONFIG_TRACE
    // bail - no tracing support
    printf("%.*s: Error, no tracing support, cannot start xmpl-trace\n",
           DISP_NAME_LEN, disp_name());
    printf("%.*s: recompile with trace = TRUE in build/hake/Config.hs\n",
           DISP_NAME_LEN, disp_name());
    return -1;
#endif

    errval_t err;

    debug_printf("starting\n");

    // Enable this line if you want to flush automatically.
    // trace_set_autoflush(true);

    finished = false;

    err = init_tracing();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "initialising tracing");
        return EXIT_FAILURE;
    }

    // Make sure all subsystems get logged.
    trace_set_all_subsys_enabled(true);

    debug_printf("after init tracing\n");

    // Prepare the tracing framework. This is optional.
    trace_prepare(MKCLOSURE(after_prepare, NULL));

    while(!finished) {
        // Make sure this program is not exited before everything
        // is completed.
        event_dispatch_non_block(get_default_waitset());
        thread_yield_dispatcher(NULL_CAP);
    }

    return EXIT_SUCCESS;
}
