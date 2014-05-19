/**
 * \file
 * \brief Arch independent accessor functions for use in generic code.
 * Generic include for userland
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_32_BARRELFISH_REGISTERS_H
#define ARCH_X86_32_BARRELFISH_REGISTERS_H

#include <barrelfish_kpi/registers_arch.h>
#include <target/x86_32/registers_target.h>

static inline void
registers_set_initial(arch_registers_state_t *regs, struct thread *thread,
                      lvaddr_t entry, lvaddr_t stack, uint32_t arg1,
                      uint32_t arg2, uint32_t arg3, uint32_t arg4)
{
    registers_x86_32_set_initial(regs, thread, entry, stack, arg1, arg2, arg3, arg4);
}

#endif // ARCH_X86_32_BARRELFISH_REGISTERS_H
