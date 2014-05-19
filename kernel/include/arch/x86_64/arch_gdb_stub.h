/**
 * \file
 * \brief Header for x86-specific GDB stub code.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <x86.h>

/**
 * \brief X86_64 register set
 *
 * As defined by GDB.
 */
enum gdb_x86_64_register_nums {
    GDB_X86_64_RAX_REG, GDB_X86_64_RBX_REG, GDB_X86_64_RCX_REG, GDB_X86_64_RDX_REG,
    GDB_X86_64_RSI_REG, GDB_X86_64_RDI_REG, GDB_X86_64_RBP_REG, GDB_X86_64_RSP_REG,
    GDB_X86_64_R8_REG, GDB_X86_64_R9_REG, GDB_X86_64_R10_REG, GDB_X86_64_R11_REG,
    GDB_X86_64_R12_REG, GDB_X86_64_R13_REG, GDB_X86_64_R14_REG, GDB_X86_64_R15_REG,
    GDB_X86_64_RIP_REG, GDB_X86_64_EFLAGS_REG, GDB_X86_64_CS_REG, GDB_X86_64_SS_REG,

/* these are not saved/used in 64-bit mode, and currently avoided
    DS_REG, ES_REG, FS_REG, GS_REG,
*/

/* these are not used yet:
    ST0_REG, ST1_REG, ST2_REG, ST3_REG, ST4_REG, ST5_REG, ST6_REG, ST7_REG,

    FCTRL_REG, FSTAT_REG, FTAG_REG, FISEG_REG,
    FIOFF_REG, FOSEG_REG, FOOFF_REG, FOP_REG,

    XMM0_REG, XMM1_REG, XMM2_REG, XMM3_REG, XMM4_REG, XMM5_REG,
    XMM6_REG, XMM7_REG, XMM8_REG, XMM9_REG, XMM10_REG, XMM11_REG,
    XMM12_REG, XMM13_REG, XMM14_REG, XMM15_REG,
    MXCSR_REG
*/

    GDB_X86_64_NUM_REGS /* not a real register; must be last! */
};

extern uintptr_t *gdb_arch_registers;

/** Address of saved registers as void * */
#define GDB_ARCH_REGADDR    ((void*)gdb_arch_registers)

/** Number of bytes saved in GDB frame */
#define GDB_ARCH_REGBYTES   (sizeof(uintptr_t) * GDB_X86_64_NUM_REGS)
