/**
 * \file
 * \brief Arch independent accessor functions for use in generic code
 * Generic include for kernel
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_64_BARRELFISH_KPI_REGISTERS_H
#define ARCH_X86_64_BARRELFISH_KPI_REGISTERS_H

#include <barrelfish_kpi/types.h> // for lvaddr_t
#include <target/x86_64/barrelfish_kpi/registers_target.h>

///< Opaque handle for the register state
typedef struct registers_x86_64 arch_registers_state_t;

///< Opaque handle for the FPU register state
typedef struct registers_fpu_x86_64 arch_registers_fpu_state_t;

static inline void
registers_set_entry(arch_registers_state_t *regs, lvaddr_t entry)
{
    registers_x86_64_set_entry(regs, entry);
}

static inline void
registers_set_param(arch_registers_state_t *regs, uint64_t param)
{
    registers_x86_64_set_param(regs, param);
}

static inline void
registers_get_param(arch_registers_state_t *regs, uint64_t *param)
{
    registers_x86_64_get_param(regs, param);
}

static inline uint64_t
registers_get_ip(arch_registers_state_t *regs)
{
    return registers_x86_64_get_ip(regs);
}

static inline uint64_t
registers_get_sp(arch_registers_state_t *regs)
{
    return registers_x86_64_get_sp(regs);
}

#endif // ARCH_X86_64_BARRELFISH_KPI_REGISTERS_H
