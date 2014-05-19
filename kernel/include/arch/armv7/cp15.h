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

static inline lpaddr_t cp15_read_ttbr0(void)
{
    lpaddr_t ttbr;
    __asm volatile(" mrc  p15, 0, %[ttbr], c2, c0, 0" : [ttbr] "=r" (ttbr));
    return ttbr;
}

static inline lpaddr_t cp15_read_ttbr1(void)
{
    lpaddr_t ttbr;
    __asm volatile(" mrc  p15, 0, %[ttbr], c2, c0, 1" : [ttbr] "=r" (ttbr));
    return ttbr;
}

static inline void cp15_write_ttbr0(lpaddr_t ttbr)
{
    __asm volatile(" mcr  p15, 0, %[ttbr], c2, c0, 0" :: [ttbr] "r" (ttbr));
}

static inline void cp15_write_ttbr1(lpaddr_t ttbr)
{
    __asm volatile(" mcr  p15, 0, %[ttbr], c2, c0, 1" :: [ttbr] "r" (ttbr));
}

static inline uint32_t cp15_read_ttbcr(void)
{
	uint32_t ttbcr;
	__asm volatile ("mrc p15, 0, %[ttbcr], c2, c0, 2" : [ttbcr] "=r" (ttbcr));
	return ttbcr;
}

static inline void cp15_write_ttbcr(uint32_t ttbcr)
{
	__asm volatile ("mcr p15, 0, %[ttbcr], c2, c0, 2" :: [ttbcr] "r" (ttbcr));
}

extern void cp15_invalidate_d_cache(void);
extern void cp15_invalidate_i_and_d_caches(void);
extern void cp15_invalidate_i_and_d_caches_fast(void);
extern void cp15_invalidate_tlb_fn(void);
extern void cp15_enable_mmu(void);
extern void cp15_enable_alignment(void);

static inline uint32_t cp15_read_cache_status(void){
    uint32_t cache;
    __asm volatile("mrc   p15, 0, %[cache], c1, c0, 0" : [cache] "=r" (cache));
    return cache;
}


static inline void cp15_disable_cache(void){

    cp15_invalidate_i_and_d_caches_fast();

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

static inline uint8_t cp15_get_cpu_id(void) {
	uint8_t cpu_id;
	__asm volatile(
			"mrc 	p15, 0, %[cpu_id], c0, c0, 5\n\t" 			// get the MPIDR register
			"and	%[cpu_id], %[cpu_id], #0xF\n\t"
			:[cpu_id] "=r" (cpu_id)
		);

	return cpu_id;
}

/*
 * Get the configuration base address
 * This is described in the Cortex A9 TRM, 4.2.32
 */
static inline uint32_t cp15_read_cbar(void)
{
  uint32_t cbar;
  __asm volatile ("mrc p15, 4, %[cbar], c15, c0, 0" : [cbar] "=r" (cbar));
  return cbar & ~0x1FFF; // Only [31:13] is valid
}

#endif // __CP15_H__
