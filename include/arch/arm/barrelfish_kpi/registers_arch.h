/**
 * \file
 * \brief architecture-specific registers code
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_ARM_BARRELFISH_KPI_REGISTERS_H
#define ARCH_ARM_BARRELFISH_KPI_REGISTERS_H

#ifndef __ASSEMBLER__
#include<stddef.h> // for offsetof
#include <barrelfish/curdispatcher_arch.h> // XXX For curdispatcher()
#include <barrelfish_kpi/types.h> // for lvaddr_t
#endif

//
// Offsets of saved registers in save area.
//
#define CPSR_REG  0
#define R0_REG    1
#define R1_REG    2
#define R2_REG    3
#define R3_REG    4
#define R4_REG    5
#define R5_REG    6
#define R6_REG    7
#define R7_REG    8
#define R8_REG    9
#define R9_REG   10
#define R10_REG  11
#define R11_REG  12
#define R12_REG  13
#define SP_REG   14
#define LR_REG   15
#define PC_REG   16

#define NUM_REGS 17            /* cpsr, r0-r15 */
#define NUM_FPU_REGS 0
#define ARCH_NUMREGS NUM_REGS

#define RIP_REG  PC_REG        /* r15 = pc == rip.x86_64 */
#define RSP_REG  SP_REG        /* r13 = sp == rsp.x86_64 */

/// Register used in system calls to encode function and arg count
#define SYSCALL_REG       0

//
// Helpers for pasting system reserved register names
//
#define REG_OFFSET_CONCAT(x)    x ## _REG
#define REG_OFFSET(name)        REG_OFFSET_CONCAT(name)

#define REG_NAME(ord)

#ifndef __ASSEMBLER__

union registers_arm {
    struct registers_arm_named {
        uint32_t cpsr;
        uint32_t r0, r1, r2, r3;
        uint32_t r4, r5, r6, r7, r8;
        uint32_t rtls;  // r9 is thread local storage
        uint32_t r10;   // r10 is for global offset table base.
        uint32_t r11, r12;
        uint32_t stack;
        uint32_t link;
        uint32_t pc;
    } named;
    struct registers_arm_syscall_args {
        uint32_t cpsr;
        uint32_t arg0, arg1, arg2, arg3;
        uint32_t arg4, arg5, arg6, arg7, arg8;
        uint32_t arg9;
        uint32_t arg10;
        uint32_t fp;
        uint32_t arg11;
        uint32_t stack;
        uint32_t link;
        uint32_t pc;
    } syscall_args;
    uint32_t regs[sizeof(struct registers_arm_named) / sizeof(uint32_t)];
};

STATIC_ASSERT_SIZEOF(union registers_arm, 17 * 4);

STATIC_ASSERT((REG_OFFSET(THREAD_REGISTER) * sizeof(uint32_t)) == offsetof(struct registers_arm_named, rtls), "Thread register conflict");


///< Opaque handle for the register state
typedef union registers_arm arch_registers_state_t;

///< Opaque handle for the FPU register state
typedef void *arch_registers_fpu_state_t;

static inline void
registers_set_entry(arch_registers_state_t *regs, lvaddr_t entry)
{
    regs->named.pc = (uint32_t)entry;
}

static inline void
registers_set_param(arch_registers_state_t *regs, uint32_t param)
{
    regs->named.r0 = param;
}

static inline void
registers_get_param(arch_registers_state_t *regs, uint32_t *param)
{
    *param = regs->named.r0;
}

static inline uint32_t
registers_get_ip(arch_registers_state_t *regs)
{
    return regs->named.pc;
}

static inline uint32_t
registers_get_sp(arch_registers_state_t *regs)
{
    return regs->named.stack;
}

#endif // __ASSEMBLER__

#endif // ARCH_ARM_BARRELFISH_KPI_REGISTERS_H
