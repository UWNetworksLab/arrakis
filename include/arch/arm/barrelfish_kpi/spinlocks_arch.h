/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_ARM_BARRELFISH_KPI_SPINLOCKS_H
#define ARCH_ARM_BARRELFISH_KPI_SPINLOCKS_H

#include <barrelfish_kpi/asm_inlines_arch.h>

typedef volatile uint32_t spinlock_t;

#ifdef __ARM_ARCH_7A__

static inline void acquire_spinlock(spinlock_t *spinlock)
{
	unsigned long tmp;

	__asm volatile (
			"1:	ldrex	%0, [%1]\n\t"
			"teq	%0, #0\n\t"
			"wfene\n\t"
			"strexeq	%0, %2, [%1]\n\t"
			"teqeq	%0, #0\n\t"
			"bne	1b"
			: "=&r" (tmp)
			: "r" (spinlock), "r" (1)
			: "cc");
	dmb();
}

static inline void release_spinlock(spinlock_t *spinlock)
{
	dmb();

	__asm volatile (
			"str	%1, [%0]\n\t"
			"sev\n\t"
			:
			: "r" (spinlock), "r" (0)
			: "cc");
}

#else

static inline void acquire_spinlock(spinlock_t *spinlock)
{
}

static inline void release_spinlock(spinlock_t *spinlock)
{
}

#endif //__ARM_ARCH_7A__

#endif // ARCH_ARM_BARRELFISH_KPI_SPINLOCKS_H
