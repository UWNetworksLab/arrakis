/**
 * \file
 * \brief Architecture specific dispatcher struct shared between kernel and user
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_ARM_BARRELFISH_KPI_DISPATCHER_SHARED_ARCH_H
#define ARCH_ARM_BARRELFISH_KPI_DISPATCHER_SHARED_ARCH_H

#include <target/arm/barrelfish_kpi/dispatcher_shared_target.h>

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
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    /* one crit_pc pair */
    struct dispatcher_shared_arm *disparm =
        get_dispatcher_shared_arm(handle);
    return disp->disabled ||
        (disparm->crit_pc_low <= rip && rip < disparm->crit_pc_high);
}

static inline arch_registers_state_t*
dispatcher_get_enabled_save_area(dispatcher_handle_t handle)
{
    return &((struct dispatcher_shared_arm *)handle)->enabled_save_area;
}

static inline arch_registers_state_t*
dispatcher_get_disabled_save_area(dispatcher_handle_t handle)
{
    return &((struct dispatcher_shared_arm *)handle)->disabled_save_area;
}

static inline arch_registers_state_t*
dispatcher_get_trap_save_area(dispatcher_handle_t handle)
{
    return &((struct dispatcher_shared_arm *)handle)->trap_save_area;
}

#endif // ARCH_ARM_BARRELFISH_KPI_DISPATCHER_SHARED_ARCH_H
