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

/***** EFLAGS flags *****/

/**
 * Allowed EFLAGS in user-space. Used when resuming programs.
 */
#define USER_EFLAGS_MASK \
    (EFLAGS_CF | EFLAGS_PF | EFLAGS_AF | EFLAGS_ZF | EFLAGS_SF | EFLAGS_DF | \
     EFLAGS_OF)


#ifndef __ASSEMBLER__

/**
 * Registers automatically saved on kernel stack by CPU
 */
enum x86_32_cpu_save_registers {
    X86_SAVE_EIP, X86_SAVE_CS, X86_SAVE_EFLAGS, X86_SAVE_ESP, X86_SAVE_SS,
    X86_SAVE_AREA_SIZE
};

/** \brief Enable FPU */
static inline void enable_fpu(void)
{
    uint32_t cr0;
#ifndef __scc__
    uint32_t cr4;
#endif
    __asm__ __volatile__("mov %%cr0, %%eax" : "=a" (cr0) : );
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
    __asm__ __volatile__("mov %%eax,%%cr0" : : "a" (cr0));
#ifndef __scc__
    //set OSFXSR
    __asm__ __volatile__("mov %%cr4, %%eax" : "=a" (cr4) : );
    cr4 |= (1 << 9);
    __asm__ __volatile__("mov %%eax,%%cr4" : : "a" (cr4));
#endif

#ifndef FPU_LAZY_CONTEXT_SWITCH
    __asm volatile ("finit");
#endif
}

#endif //__ASSEMBLER__

#endif //__X86_H
