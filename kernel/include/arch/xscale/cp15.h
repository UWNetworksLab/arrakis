/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __CP15_H__
#define __CP15_H__

#include <kernel.h>

void cp15_invalidate_i_and_d_caches(void);

/**
 * \brief Read domain access control register
 */
static inline uint32_t cp15_read_dacr(void)
{
    uint32_t dacr;
    __asm volatile("mrc   p15, 0, %[dacr], c3, c0, 0" : [dacr] "=r" (dacr));
    return dacr;
}

/**
 * \brief Read instruction fault status register.
 */
static inline uint32_t cp15_read_ifsr(void)
{
    uint32_t ifsr;
    __asm volatile("mrc   p15, 0, %[ifsr], c5, c0, 1" : [ifsr] "=r" (ifsr));
    return ifsr;
}

/**
 * \brief Read data fault status register.
 */
static inline uint32_t cp15_read_dfsr(void)
{
    uint32_t dfsr;
    __asm volatile("mrc   p15, 0, %[dfsr], c5, c0, 0" : [dfsr] "=r" (dfsr));
    return dfsr;
}

/**
 * \brief Read fault address register.
 */
static inline uint32_t cp15_read_far(void)
{
    uint32_t addr;
    __asm volatile(" mrc  p15, 0, %[addr], c6, c0, 0" : [addr] "=r" (addr));
    return addr;
}

static inline lpaddr_t cp15_read_ttbr(void)
{
    lpaddr_t ttbr;
    __asm volatile(" mrc  p15, 0, %[ttbr], c2, c0, 0" : [ttbr] "=r" (ttbr));
    return ttbr;
}

static inline void cp15_write_ttbr(lpaddr_t ttbr)
{
    __asm volatile(" mcr  p15, 0, %[ttbr], c2, c0, 0" :: [ttbr] "r" (ttbr));
}


static inline uint32_t cp15_read_cache_status(void){
    uint32_t cache;
    __asm volatile("mrc   p15, 0, %[cache], c1, c0, 0" : [cache] "=r" (cache));
    return cache;
}


static inline void cp15_disable_cache(void){
   
    cp15_invalidate_i_and_d_caches();
    
    __asm volatile(
                   //     "ldr r1, =0x3 \n\t"
                   "mrc p15, 0, r1, c1, c0, 0 \n\t" //read
                   "bic r1, #4 \n\t"
                   "mcr p15, 0, r1, c1, c0, 0 \n\t"
                   :::"r1");

    printf("WARNING! Caching has been disabled, configuration is: %"PRIx32"\n", cp15_read_cache_status());

}

static inline void cp15_invalidate_tlb(void)
{
    __asm volatile(" mcr  p15, 0, r0, c8, c7, 0");
}

#endif // __CP15_H__
