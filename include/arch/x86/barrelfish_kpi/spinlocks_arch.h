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

#ifndef ARCH_X86_BARRELFISH_KPI_SPINLOCKS_H
#define ARCH_X86_BARRELFISH_KPI_SPINLOCKS_H

#include <stdint.h> // for uint32_t

/** \brief spinlock */
typedef volatile uint32_t spinlock_t;

static inline void acquire_spinlock(spinlock_t *lock)
{
    __asm__ __volatile__("0:\n\t"
                         "cmpl $0, %0\n\t"
                         "je 1f\n\t"
                         "pause\n\t"
                         "jmp 0b\n\t"
                         "1:\n\t"
                         "lock btsl $0, %0\n\t"
                         "jc 0b\n\t"
                         : "+m" (*lock) : : "memory", "cc");
}

static inline void release_spinlock(spinlock_t *lock)
{
       __asm__ __volatile__("movl $0, %0\n\t"
                         : "+m" (*lock) : : "memory" );
}

#endif
