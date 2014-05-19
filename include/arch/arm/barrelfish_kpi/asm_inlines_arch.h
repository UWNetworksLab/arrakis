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

#ifndef ARCH_ARM_BARRELFISH_KPI_ARM_H
#define ARCH_ARM_BARRELFISH_KPI_ARM_H

#ifndef __ASSEMBLER__

#if defined (__ARM_ARCH_7A__) || defined(__ARM_ARCH_7M__)

static inline void dmb(void)
{
	__asm volatile ("dmb" : : : "memory");
}

#else

static inline void dmb(void)
{
	__asm volatile ("mcr p15, 0, %0, c7, c10, 5" : : "r" (0) : "memory");
}

#endif

static inline uint8_t is_cycle_counter_overflow(void)
{
	uint32_t regval;
	__asm volatile ("mrc p15, 0, %0, c9, c12, 3\t\n" : "=r"(regval));

	return (regval & 0x80000000);
}

static inline uint32_t get_cycle_count(void)
{

	uint32_t val;
	__asm volatile ("mrc p15, 0, %0, c9, c13, 0\t\n": "=r"(val));

	return  val;
}


static inline void reset_cycle_counter(void)
{
	uint32_t val = 1; //in general enable all counters
	val |= 4;		  //reset cycle counter

	// program the performance-counter control-register:
	__asm volatile ("mcr p15, 0, %0, c9, c12, 0\t\n" :: "r"(val));

	 // enable cycle counter:
	__asm volatile ("mcr p15, 0, %0, c9, c12, 1\t\n" :: "r"(0x80000000));

	// clear overflow:
	__asm volatile ("MCR p15, 0, %0, c9, c12, 3\t\n" :: "r"(0x80000000));
}

#endif // __ASSEMBLER__

#endif //  ARCH_ARM_BARRELFISH_KPI_ARM_H
