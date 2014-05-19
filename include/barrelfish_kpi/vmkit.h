/**
 * \file
 * \brief Interface to shared data structures between the kernel and a VM monitor.
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KPI_VMKIT_H
#define KPI_VMKIT_H

/**
 * \brief A VMKit guest control and state structure.
 *
 * Defines some control and state values shared beween VMKit kernel and a VM
 * monitor.
 */
struct guest_control {
    /// Space to store all regs not captured in the VMCB
    struct registers_x86_64 regs;
    uint64_t        num_vm_exits_with_monitor_invocation;
    uint64_t        num_vm_exits_without_monitor_invocation;
};

#endif // KPI_VMKIT_H
