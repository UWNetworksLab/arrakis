/**
 * \file
 * \brief Arch specific definition of the registers, can be included by anyone.
 * Definitions shared by kernel and user
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_32_BARRELFISH_KPI_REGISTERS_H
#define TARGET_X86_32_BARRELFISH_KPI_REGISTERS_H

#include <barrelfish_kpi/types.h> // for lvaddr_t

/** \brief Number of registers to be saved for this architecture
 *
 * This is the same as ARCH_NUMREGS, but it is used by assembler stubs, so needs
 * to be defined here as a constant.
 */
#define X86_32_NUM_REGS                        12

#ifndef __ASSEMBLER__

struct registers_x86_32 {
    uint32_t eax, ebx, ecx, edx, esi, edi, ebp, esp, eip, eflags, cs, ss;
    uint16_t fs, gs; ///< Only meaningful segment selectors in 32-bit mode
};

struct registers_fpu_x86_32 {
    // Should be aligned at 16-byte boundary, according to Intel
    // description of FXRSTOR instruction.
    // 16 bytes of padding
    uint8_t registers[512 + 16] __attribute__ ((aligned (16)));
};

static inline void
registers_x86_32_set_entry(struct registers_x86_32 *regs, lvaddr_t entry)
{
    regs->eip = entry;
    regs->cs = USER_CS;
    regs->ss = USER_SS;
    regs->eflags = USER_EFLAGS;
}

static inline void
registers_x86_32_set_param(struct registers_x86_32 *regs, uint32_t param)
{
    regs->eax = param;
}

static inline void
registers_x86_32_get_param(struct registers_x86_32 *regs, uint32_t *param)
{
    *param = regs->eax;
}

static inline uint32_t
registers_x86_32_get_ip(struct registers_x86_32 *regs)
{
    return regs->eip;
}

static inline uint32_t
registers_x86_32_get_sp(struct registers_x86_32 *regs)
{
    return regs->esp;
}

#endif // __ASSEMBLER__
#endif // TARGET_X86_32_BARRELFISH_KPI_REGISTERS_H
