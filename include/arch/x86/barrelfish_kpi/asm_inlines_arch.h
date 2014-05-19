/**
 * \file
 * \brief Some arch specific asm inlines
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_BARRELFISH_KPI_X86_H
#define ARCH_X86_BARRELFISH_KPI_X86_H

#ifndef __ASSEMBLER__

/** \brief This code reads the cycle counter */
static inline uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}

/** \brief This code reads the cycle counter -- flushing the
    instruction pipeline first. Throws away the processor ID information. */
static inline uint64_t rdtscp(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtscp" : "=a" (eax), "=d" (edx) :: "ecx");
    return ((uint64_t)edx << 32) | eax;
}

static inline uint64_t rdpmc(uint32_t counter)
{
    uint32_t eax, edx;

    __asm volatile("rdpmc"
                   : "=a" (eax), "=d" (edx)
                   : "c" (counter)
                   );

    return ((uint64_t)edx << 32) | eax;
}

static inline void mfence(void)
{
    __asm volatile("mfence");
}

static inline void sfence(void)
{
    __asm volatile("sfence");
}

static inline void lfence(void)
{
    __asm volatile("lfence");
}

static inline void clflush(void *line)
{
    __asm volatile("clflush %0" :: "m" (line));
}

#ifndef __scc__
#       define CACHE_LINE_SIZE 64 /* bytes */
#else
#       define CACHE_LINE_SIZE 32 /* bytes */
#endif

#ifndef __cplusplus
/* flush a range of memory from the cache */
static inline void cache_flush_range(void *base, size_t len)
{
    //mfence();

    uint8_t *line = (uint8_t *)((uintptr_t)base & ~(CACHE_LINE_SIZE-1UL));
    do {
        clflush(line);
        line += CACHE_LINE_SIZE;
    } while (line < (uint8_t *)base + len);
}
#endif

#ifdef __scc__
static inline void cl1flushmb(void)
{
    __asm volatile ( ".byte 0x0f; .byte 0x0a;\n" ); // CL1FLUSHMB
}
#endif

#endif // __ASSEMBLER__

#endif // ARCH_X86_BARRELFISH_KPI_X86_H
