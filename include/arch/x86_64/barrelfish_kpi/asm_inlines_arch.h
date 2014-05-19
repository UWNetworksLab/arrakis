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

#ifndef ARCH_X86_64_BARRELFISH_KPI_ASM_INLINES_H
#define ARCH_X86_64_BARRELFISH_KPI_ASM_INLINES_H

#ifndef __ASSEMBLER__

#include <target/x86_64/barrelfish_kpi/registers_target.h>

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
    __asm volatile("cpuid"
                   : "=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx)
                   : "a" (function)
                   );
}


/** \brief Atomic compare-and-swap on 128 bits
 *
 * If *dst == old then *dst = new, returns 0 on failure
 * 
 * Note, dest should point to a 128bit structure that is to be overwritten 
 */
static inline int cmpxchg128(volatile uint64_t dest[2], uint64_t old_top, uint64_t old_bot, uint64_t new_top, uint64_t new_bot)
{
    uint8_t ret;

    __asm volatile (
        "lock cmpxchg16b %1\n\t"
        "setz %0\n\t"
        : "=a"(ret), "=m"(*dest)//, "=d"(old_top), "=a"(old_bot)
        : "a"(old_top), "d"(old_bot), "b"(new_top), "c"(new_bot), "m"(*dest)
        : "memory");

    return ret;
}

static inline void fpu_init(void)
{
    __asm volatile ("fninit");
}

static inline void fpu_save(struct registers_fpu_x86_64 *fpustate)
{
    uint8_t *regs = fpustate->registers;
    regs += 16 - ((uintptr_t)regs % 16);

    __asm volatile("fxsaveq %0" : "=m" (*regs));
}

static inline void fpu_restore(struct registers_fpu_x86_64 *fpustate)
{
    uint8_t *regs = fpustate->registers;
    regs += 16 - ((uintptr_t)regs % 16);

    __asm volatile ("fxrstorq %0" :: "m" (*regs));
}

#endif // __ASSEMBLER__

#endif // ARCH_X86_64_BARRELFISH_KPI_ASM_INLINES_H
