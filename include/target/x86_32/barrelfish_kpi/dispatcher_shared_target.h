/**
 * \file
 * \brief Architecture specific dispatcher struct shared between kernel and user
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_32_BARRELFISH_KPI_DISPATCHER_SHARED_H
#define TARGET_X86_32_BARRELFISH_KPI_DISPATCHER_SHARED_H

#include <barrelfish_kpi/dispatcher_shared.h>

///< Architecture specific kernel/user shared dispatcher struct
struct dispatcher_shared_x86_32 {
    struct dispatcher_shared_generic d; ///< Generic portion

    lvaddr_t    crit_pc_low;        ///< Critical section lower PC bound
    lvaddr_t    crit_pc_high;       ///< Critical section upper PC bound

    struct registers_x86_32 enabled_save_area;  ///< Enabled register save area
    struct registers_x86_32 disabled_save_area; ///< Disabled register save area
    struct registers_x86_32 trap_save_area;     ///< Trap register save area
    struct registers_fpu_x86_32 enabled_fpu_state;      ///< FPU register save area
    struct registers_fpu_x86_32 disabled_fpu_state;     ///< FPU register save area
};

static inline struct dispatcher_shared_x86_32*
get_dispatcher_shared_x86_32(dispatcher_handle_t handle)
{
    return (struct dispatcher_shared_x86_32*)handle;
}

static inline struct registers_x86_32*
dispatcher_x86_32_get_enabled_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_32 *disp =
        get_dispatcher_shared_x86_32(handle);
    return &disp->enabled_save_area;
}

static inline struct registers_x86_32*
dispatcher_x86_32_get_disabled_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_32 *disp =
        get_dispatcher_shared_x86_32(handle);
    return &disp->disabled_save_area;
}

static inline struct registers_x86_32*
dispatcher_x86_32_get_trap_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_32 *disp =
        get_dispatcher_shared_x86_32(handle);
    return &disp->trap_save_area;
}

static inline struct registers_fpu_x86_32*
dispatcher_x86_32_get_enabled_fpu_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_32 *disp =
        get_dispatcher_shared_x86_32(handle);
    return &disp->enabled_fpu_state;
}

static inline struct registers_fpu_x86_32*
dispatcher_x86_32_get_disabled_fpu_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_32 *disp =
        get_dispatcher_shared_x86_32(handle);
    return &disp->disabled_fpu_state;
}

#endif // TARGET_X86_32_BARRELFISH_KPI_DISPATCHER_SHARED_H
