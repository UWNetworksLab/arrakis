/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_32_BARRELFISH_KPI_ASM_INLINES_H
#define ARCH_X86_32_BARRELFISH_KPI_ASM_INLINES_H

#ifndef __ASSEMBLER__

#include <target/x86_32/barrelfish_kpi/registers_target.h>

static inline void cpuid(uint32_t function, uint32_t *eax, uint32_t *ebx,
                         uint32_t *ecx, uint32_t *edx)
{
    // make it possible to omit certain return registers
    uint32_t a, b, c, d;
    if (eax == NULL) {
        eax = &a;
    }
    if (ebx == NULL) {
        ebx = &b;
    }
    if (ecx == NULL) {
        ecx = &c;
    }
    if (edx == NULL) {
        edx = &d;
    }
#ifdef __PIC__
    __asm volatile("push        %%ebx           \n\t"
                   "mov         %[ebx], %%ebx   \n\t"
                   "cpuid                       \n\t"
                   "mov         %%ebx, %[ebx]   \n\t"
                   "pop         %%ebx           \n\t"
                   : "=a" (*eax), [ebx] "=r" (*ebx), "=c" (*ecx), "=d" (*edx)
                   : "a" (function)
                   );
#else
    __asm volatile("cpuid"
                   : "=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx)
                   : "a" (function)
                   );
#endif
}

static inline void fpu_init(void)
{
    __asm volatile ("fninit");
}

static inline void fpu_save(struct registers_fpu_x86_32 *fpustate)
{
    uint8_t *regs = fpustate->registers;
    regs += 16 - ((uintptr_t)regs % 16);

    // XXX: Should really be detected at run-time (and it's not SCC-specific)
#ifdef __scc__
    __asm volatile("fnsave %0; fwait" : "=m" (*regs));
#else
    __asm volatile("fxsave %0" : "=m" (*regs));
#endif
}

static inline void fpu_restore(struct registers_fpu_x86_32 *fpustate)
{
    uint8_t *regs = fpustate->registers;
    regs += 16 - ((uintptr_t)regs % 16);

    // XXX: Should really be detected at run-time (and it's not SCC-specific)
#ifdef __scc__
    __asm volatile ("frstor %0" :: "m" (*regs));
#else
    __asm volatile ("fxrstor %0" :: "m" (*regs));
#endif
}

#endif // __ASSEMBLER__

#endif // ARCH_X86_32_BARRELFISH_KPI_ASM_INLINES_H
