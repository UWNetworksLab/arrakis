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

#ifndef TARGET_X86_64_BARRELFISH_KPI_DISPATCHER_SHARED_H
#define TARGET_X86_64_BARRELFISH_KPI_DISPATCHER_SHARED_H

#include <barrelfish_kpi/dispatcher_shared.h>

///< Architecture specific kernel/user shared dispatcher struct
struct dispatcher_shared_x86_64 {
    struct dispatcher_shared_generic d; ///< Generic portion

    lvaddr_t    crit_pc_low;        ///< Critical section lower PC bound
    lvaddr_t    crit_pc_high;       ///< Critical section upper PC bound

    lvaddr_t    ldt_base;           ///< Base address of local descriptor table (LDT)
    size_t      ldt_npages;         ///< Size of local descriptor table (# 4k pages)

    struct registers_x86_64 enabled_save_area;  ///< Enabled register save area
    struct registers_x86_64 disabled_save_area; ///< Disabled register save area
    struct registers_x86_64 trap_save_area;     ///< Trap register save area
    struct registers_fpu_x86_64 enabled_fpu_state;      ///< FPU register save area
    struct registers_fpu_x86_64 disabled_fpu_state;     ///< FPU register save area
};

static inline struct dispatcher_shared_x86_64*
get_dispatcher_shared_x86_64(dispatcher_handle_t handle)
{
    return (struct dispatcher_shared_x86_64*)handle;
}

static inline struct registers_x86_64*
dispatcher_x86_64_get_enabled_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_64 *disp =
        get_dispatcher_shared_x86_64(handle);
    return &disp->enabled_save_area;
}

static inline struct registers_x86_64*
dispatcher_x86_64_get_disabled_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_64 *disp =
        get_dispatcher_shared_x86_64(handle);
    return &disp->disabled_save_area;
}

static inline struct registers_x86_64*
dispatcher_x86_64_get_trap_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_64 *disp =
        get_dispatcher_shared_x86_64(handle);
    return &disp->trap_save_area;
}

static inline struct registers_fpu_x86_64*
dispatcher_x86_64_get_enabled_fpu_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_64 *disp =
        get_dispatcher_shared_x86_64(handle);
    return &disp->enabled_fpu_state;
}

static inline struct registers_fpu_x86_64*
dispatcher_x86_64_get_disabled_fpu_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_64 *disp =
        get_dispatcher_shared_x86_64(handle);
    return &disp->disabled_fpu_state;
}

#endif // TARGET_X86_64_BARRELFISH_KPI_DISPATCHER_SHARED_H
