/**
 * \file
 * \brief Architecture specific dispatcher structure private to the user
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_32_BARRELFISH_DISPATCHER_H
#define TARGET_X86_32_BARRELFISH_DISPATCHER_H

#include <barrelfish_kpi/dispatcher_shared.h>
#include <barrelfish_kpi/dispatcher_shared_arch.h>
#include <barrelfish/dispatcher.h>

/// Dispatcher structure (including data accessed only by user code)
struct dispatcher_x86_32 {
    struct dispatcher_shared_x86_32 d;  ///< Shared (user/kernel) data. Must be first.
    struct dispatcher_generic generic;  ///< User private data

    uint16_t disp_seg_selector;         ///< Dispatcher segment selector

    /* Incoming IDC endpoints (buffers and receive cap pointers) follow */
};

static inline struct dispatcher_generic*
get_dispatcher_generic_x86_32(dispatcher_handle_t handle)
{
    struct dispatcher_x86_32 *disp = (struct dispatcher_x86_32*)handle;
    return &disp->generic;
}

static inline struct dispatcher_x86_32 *
get_dispatcher_x86_32(dispatcher_handle_t handle)
{
    return (struct dispatcher_x86_32*)handle;
}

#endif // TARGET_X86_32_BARRELFISH_DISPATCHER_H
