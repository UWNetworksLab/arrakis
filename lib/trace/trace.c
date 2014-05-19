/**
 * \file
 * \brief System-wide tracing
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <trace/trace.h>
#include <spawndomain/spawndomain.h>

STATIC_ASSERT_SIZEOF(struct trace_event, 16);
STATIC_ASSERT((sizeof(struct trace_buffer) <= TRACE_PERCORE_BUF_SIZE), "size mismatch");

/**
 * \brief Initialize per-core tracing buffer
 *
 * This function creates a cap for the tracing buffer in taskcn.  
 * It is called from init at startup.
 *
 * Note that it does *not* map the buffer into its own vspace.
 */
errval_t trace_init(void)
{
    errval_t err;
    size_t bytes;

    struct capref cap = {
        .cnode = cnode_task,
        .slot = TASKCN_SLOT_TRACEBUF
    };

    err = frame_create(cap, TRACE_ALLOC_SIZE, &bytes);
    if (err_is_fail(err)) {
        return err_push(err, TRACE_ERR_CREATE_CAP);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Turn off tracing for this domain
 */
errval_t trace_disable_domain(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    disp->trace_buf = NULL;
    return SYS_ERR_OK;
}


/**
 * Sets up tracing framework for current dispatcher 
 */
void trace_init_disp(void)
{ 
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    disp->trace_buf = (struct trace_buffer*)trace_buffer_va;    
}

/**
 * \brief Setup the tracing buffer for a child domain
 *
 * Assumes that the child domain is currently being spawned
 * (because we rely on libspawndomain having the right state to
 * allow us to map the trace buffer into the child's vspace).
 *
 * trace_init must already have been called
 */
errval_t trace_setup_child(struct cnoderef taskcn,
                           dispatcher_handle_t handle)
{
    errval_t err;

    // Pass tracing buffer cap to new domain
    struct capref src = {
        .cnode  = cnode_task,
        .slot   = TASKCN_SLOT_TRACEBUF
    };
    struct capref dst = {
        .cnode = taskcn,
        .slot  = TASKCN_SLOT_TRACEBUF
    };
    err = cap_copy(dst, src);
    if (err_is_fail(err)) {
        return err_push(err, TRACE_ERR_CAP_COPY);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Set up the trace buffer on the current core and notify the kernel.
 *
 * Clear the buffer. Return the cap for the buffer.
 * Should be called once on each core.
 */
errval_t trace_setup_on_core(struct capref *retcap)
{
#ifdef __i386__
    return TRACE_ERR_NO_BUFFER;
#endif

    // Clear the buffer
    trace_reset_buffer();

    // Tell the kernel that the trace buffer exists
    struct capref tracecap = {
        .cnode = cnode_task,
        .slot = TASKCN_SLOT_TRACEBUF
    };
    *retcap = tracecap;

    return SYS_ERR_OK;
}

