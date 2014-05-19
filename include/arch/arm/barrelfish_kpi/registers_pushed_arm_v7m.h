/**
 * \file
 * \brief architecture-specific registers code
 * Armv7-M automatically pushes some registers instead of banking them.
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_ARM_BARRELFISH_KPI_REGISTERS_PUSHED_ARMV7_M_H
#define ARCH_ARM_BARRELFISH_KPI_REGISTERS_PUSHED_ARMV7_M_H

#ifndef __ASSEMBLER__
#include <barrelfish_kpi/types.h> // for lvaddr_t
#endif

/*
 * Offsets of automatically pushed registers on context switch.
 * These registers get pushed on exception entry and are restored from the stack on 
 * exception return.
 */
#define R0_REG_PUSHED      0
#define R1_REG_PUSHED      1
#define R2_REG_PUSHED      2
#define R3_REG_PUSHED      3
#define R12_REG_PUSHED     4
#define LR_REG_PUSHED      5
#define PC_REG_PUSHED      6
#define XPSR_REG_PUSHED    7     //this overlaps with the cpsr of armv7-a, but only partially

#define NUM_REGS_PUSHED    8

#ifndef __ASSEMBLER__

//analogous to registers_arm in registers_arm.h
union registers_arm_pushed {
    struct registers_arm_pushed_named {
        uint32_t r0, r1, r2, r3;
        uint32_t r12;
        uint32_t link;
        uint32_t pc;
        uint32_t xpsr;
    } named;
    uint32_t regs[sizeof(struct registers_arm_pushed_named) / sizeof(uint32_t)];
};

STATIC_ASSERT_SIZEOF(union registers_arm_pushed, NUM_REGS_PUSHED * 4);

#endif // __ASSEMBLER__

#endif // ARCH_ARM_BARRELFISH_KPI_REGISTERS_PUSHED_ARMV7_M_H
