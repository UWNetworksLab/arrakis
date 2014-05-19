/** \file
 *  \brief Memory server benchmark tracing
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

//#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

#include "memtest_trace.h"

errval_t init_tracing(void)
{
#ifdef CONFIG_TRACE
    debug_printf("init tracing\n");

    trace_reset_all();

    // Tell the trace system when to start and stop.  We can also 
    // provide an overriding maximum duration (in cycles) as the last parameter.
    return trace_control(TRACE_EVENT(TRACE_SUBSYS_MEMTEST,
                                    TRACE_EVENT_MEMTEST_START, 0),
                        TRACE_EVENT(TRACE_SUBSYS_MEMTEST,
                                    TRACE_EVENT_MEMTEST_STOP, 0), 
                        0);
#else
    debug_printf("Warning: tracing not enabled\n");
    return SYS_ERR_OK;
#endif

}

void start_tracing(void)
{
    // start the trace going by providing the start event
    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_START, 0);
    debug_printf("tracing started\n");
}

void stop_tracing(void)
{
    // stop the trace by providing the stop event
    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_STOP, 0);
    debug_printf("tracing stopped\n");
}

#define TRACE_SIZE (4096*4096)
static char *buf = NULL;

void prepare_dump(void)
{
    buf = malloc(TRACE_SIZE);
    assert(buf != NULL);
}

void dump_trace(void)
{
    // dump the trace on the output.  We can copy and paste it
    // to use in Aquarium.
    if (buf == NULL) {
        prepare_dump();
    }
    size_t len = trace_dump(buf, TRACE_SIZE, NULL);
    printf("%s\n", buf);
    debug_printf("dump finished. len: %d\n", (int)len);
    free(buf);
}
