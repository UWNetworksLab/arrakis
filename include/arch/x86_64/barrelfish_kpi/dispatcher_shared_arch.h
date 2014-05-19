/**
 * \file
 * \brief Architecture specific dispatcher struct shared between kernel and user
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_64_BARRELFISH_KPI_DISPATCHER_SHARED_ARCH_H
#define ARCH_X86_64_BARRELFISH_KPI_DISPATCHER_SHARED_ARCH_H

#include <target/x86_64/barrelfish_kpi/dispatcher_shared_target.h>

/**
 * \brief Returns whether dispatcher is currently disabled, given IP.
 *
 * \param disp  Pointer to dispatcher
 * \param ip    User-level instruction pointer.
 *
 * \return true if dispatcher disabled, false otherwise.
 */
static inline bool dispatcher_is_disabled_ip(dispatcher_handle_t handle,
                                             uintptr_t rip)
{
    /* one crit_pc pair */
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    struct dispatcher_shared_x86_64 *disp64 =
        get_dispatcher_shared_x86_64(handle);
    return disp->disabled ||
        (disp64->crit_pc_low <= rip && rip < disp64->crit_pc_high);
}

static inline arch_registers_state_t*
dispatcher_get_enabled_save_area(dispatcher_handle_t handle)
{
    return dispatcher_x86_64_get_enabled_save_area(handle);
}

static inline arch_registers_state_t*
dispatcher_get_disabled_save_area(dispatcher_handle_t handle)
{
    return dispatcher_x86_64_get_disabled_save_area(handle);
}

static inline arch_registers_state_t*
dispatcher_get_trap_save_area(dispatcher_handle_t handle)
{
    return dispatcher_x86_64_get_trap_save_area(handle);
}

static inline arch_registers_fpu_state_t*
dispatcher_get_enabled_fpu_save_area(dispatcher_handle_t handle)
{
    return dispatcher_x86_64_get_enabled_fpu_save_area(handle);
}

static inline arch_registers_fpu_state_t*
dispatcher_get_disabled_fpu_save_area(dispatcher_handle_t handle)
{
    return dispatcher_x86_64_get_disabled_fpu_save_area(handle);
}

#endif // ARCH_X86_64_BARRELFISH_KPI_DISPATCHER_SHARED_ARCH_H
