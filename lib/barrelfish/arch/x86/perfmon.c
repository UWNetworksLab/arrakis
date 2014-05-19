/**
 * \file
 * \brief IA32 performance monitoring
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdint.h>
#include <stdbool.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/cspace.h>
#include <barrelfish/caddr.h>
#include <barrelfish/lmp_endpoints.h>
#include <arch/x86/barrelfish/perfmon.h>
#include <arch/x86/barrelfish_kpi/perfmon_amd.h>
#include <barrelfish/dispatcher_arch.h>

/**
 * \brief Setup use of performance counter
 *
 * \param handle   The handle to the dispatcher
 * \param counter  Which counter to use.
 * \param evt      The event to use
 * \param umask    The mask on the event
 * \param kernel   Is it kernel or user readable
 *
 * Currently this only works for AMD machines and it has 4 counters
 * (0-3).
 */
errval_t perfmon_setup(dispatcher_handle_t handle, perfmon_counter_t counter,
                       perfmon_event_t evt, perfmon_mask_t umask, bool kernel)
{
    errval_t err;

    // Setup the counter
    err = invoke_perfmon_activate(cap_perfmon, evt, umask, kernel, counter, 0, 0);
    if(err_is_fail(err)) {
        err_push(err, LIB_ERR_INVOKE_PERFMON_SETUP);
    }

    // Initialize it to 0
    err = invoke_perfmon_write(cap_perfmon, counter, 0);
    if(err_is_fail(err)) {
        err_push(err, LIB_ERR_INVOKE_PERFMON_WRITE);
    }

    return SYS_ERR_OK;
}

const char *perfmon_str(uint64_t event)
{
    const char *str = NULL;

    switch(event) {
    case EVENT_AMD_L3_CACHE_MISSES:
        str = "L3 cache misses";
        break;

    case EVENT_AMD_L3_EVICTIONS:
        str = "L3 evictions";
        break;

    case EVENT_AMD_DATA_CACHE_MISSES:
        str = "Data Cache Misses";
        break;
    case EVENT_AMD_DATA_CACHE_REFILLS_L2_NB:
        str = "Data Cache Refills from L2 or Northbridge";
        break;
    case EVENT_AMD_DATA_CACHE_REFILLS_NB:
        str = "Data Cache Refills from the Northbridge";
        break;
    case EVENT_AMD_DATA_CACHE_LINES_EVICTED:
        str = "Data Cache Lines Evicted";
        break;

    case EVENT_AMD_MEM_REQ_TYPES:
        str = "Memory Requests by Type";
        break;

    case EVENT_AMD_OCTWORDS_TO_SYSTEM:
        str = "Octwords written to System";
        break;

    case EVENT_AMD_L2_CACHE_MISSES:
        str = "L2 Cache Misses";
        break;

    case EVENT_AMD_INSTRUCTION_CACHE_MISSES:
        str = "Instruction Cache Misses";
        break;

    case EVENT_AMD_L2_FILL_WRITEBACK:
        str = "L2 Cache Fill Writeback";
        break;

    case EVENT_AMD_HYPERTRANSPORT_LINK0_BANDWIDTH:
        str = "HyperTransport Link 0 Bandwidth";
        break;

    case EVENT_AMD_HYPERTRANSPORT_LINK1_BANDWIDTH:
        str = "HyperTransport Link 1 Bandwidth";
        break;

    case EVENT_AMD_HYPERTRANSPORT_LINK2_BANDWIDTH:
        str = "HyperTransport Link 2 Bandwidth";
        break;

    default:
        assert(!"Unknown performance event!");
        break;
    }

    return str;
}
