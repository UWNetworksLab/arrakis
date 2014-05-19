/**
 * \file
 * \brief X86 inline asm utilities and defines
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __IA32_MSR_H
#define __IA32_MSR_H

typedef int mackerel_msr_t;

/*
 * Reading from Model-Specific Registers
 *
 * You might be tempted, gentle reader, to wonder why one should
 * bother with the apparently pointless volatile declaration here,
 * particularly if (as is the case with Barrelfish) rdmsr is an asm
 * volatile inline function anyway.   If you don't understand why,
 * you're not qualified to change this code.  If you do, you'll
 * understand why it should not be changed, as long as we are
 * compiling with GCC.
 */
static inline uint32_t ia32_msr_read_32(ia32_t *base, mackerel_msr_t index)
{
    volatile uint32_t r = rdmsr(index);
    return r;
}
static inline uint64_t ia32_msr_read_64(ia32_t *base, mackerel_msr_t index)
{
    volatile uint64_t r = rdmsr(index);
    return r;
}

/*
 * Writing to Model-Specific Registers
 */
static inline void ia32_msr_write_32(ia32_t *base, mackerel_msr_t index, uint32_t v)
{
    wrmsr(index, v);
}
static inline void ia32_msr_write_64(ia32_t *base, mackerel_msr_t index, uint64_t v)
{
    wrmsr(index, v);
}

#endif // __IA32_MSR_H
