/**
 * \file
 * \brief X86 inline asm utilities and defines
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __X86_H
#define __X86_H

#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/cpu.h>
#include <arch/x86/x86.h>
#include <barrelfish_kpi/cpu_arch.h>

/***** RFLAGS flags *****/

/* Fixed flags */
#define RFLAGS_ALWAYS1  (1 << 1)

/* Status/Control flags */
#define RFLAGS_CF       (1 << 0)
#define RFLAGS_PF       (1 << 2)
#define RFLAGS_AF       (1 << 4)
#define RFLAGS_ZF       (1 << 6)
#define RFLAGS_SF       (1 << 7)
#define RFLAGS_DF       (1 << 10)
#define RFLAGS_OF       (1 << 11)

/* System flags */
#define RFLAGS_TF       (1 << 8)
#define RFLAGS_IF       (1 << 9)
#define RFLAGS_NT       (1 << 14)
#define RFLAGS_RF       (1 << 16)
#define RFLAGS_VM       (1 << 17)
#define RFLAGS_AC       (1 << 18)
#define RFLAGS_VIF      (1 << 19)
#define RFLAGS_VIP      (1 << 20)
#define RFLAGS_ID       (1 << 21)

/* I/O privilege flags */
#define RFLAGS_IOPL0    (0 << 12)
#define RFLAGS_IOPL1    (1 << 12)
#define RFLAGS_IOPL2    (2 << 12)
#define RFLAGS_IOPL3    (3 << 12)

/**
 * State of RFLAGS when executing a user-space program: Enable interrupts
 */
#define USER_RFLAGS     (RFLAGS_ALWAYS1 | RFLAGS_IF)

/**
 * Allowed RFLAGS in user-space. Used when resuming programs.
 */
#define USER_RFLAGS_MASK \
    (RFLAGS_CF | RFLAGS_PF | RFLAGS_AF | RFLAGS_ZF | RFLAGS_SF | RFLAGS_DF | \
     RFLAGS_OF)


#ifndef __ASSEMBLER__

/**
 * Registers automatically saved on kernel stack by CPU
 */
enum x86_64_cpu_save_registers {
    X86_SAVE_RIP, X86_SAVE_CS, X86_SAVE_EFLAGS, X86_SAVE_RSP, X86_SAVE_SS,
    X86_SAVE_AREA_SIZE
};

/** \brief Enable FPU */
static inline void enable_fpu(void)
{
    uint64_t cr0, cr4;
    __asm__ __volatile__("mov %%cr0, %%rax" : "=a" (cr0) : );
    //clear EM
    cr0 &= ~(1 << 2);
    //set MP
    cr0 |= (1 << 1);
    //set NE
    cr0 |= (1 << 5);
#ifdef FPU_LAZY_CONTEXT_SWITCH
    //set TS
    cr0 |= (1 << 3);
#else
    //clear TS
    cr0 &= ~(1 << 3);
#endif
    __asm__ __volatile__("mov %%rax,%%cr0" : : "a" (cr0));
    //set OSFXSR
    __asm__ __volatile__("mov %%cr4, %%rax" : "=a" (cr4) : );
    cr4 |= (1 << 9);
    __asm__ __volatile__("mov %%rax,%%cr4" : : "a" (cr4));

#ifndef FPU_LAZY_CONTEXT_SWITCH
    __asm volatile ("finit");
#endif
}

#endif //__ASSEMBLER__

#endif //__X86_H
