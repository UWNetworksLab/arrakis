/**
 * \file
 * \brief FPU lazy context switch support
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __FPU_H
#define __FPU_H

static inline void fpu_trap_on(void)
{
    uint32_t cr0;
    __asm volatile("mov %%cr0, %%eax" : "=a" (cr0));
    cr0 |= (1 << 3);        // Set TS
    __asm volatile("mov %%eax,%%cr0" :: "a" (cr0));
}

static inline bool fpu_trap_get(void)
{
    uint32_t cr0;
    __asm volatile("mov %%cr0, %%eax" : "=a" (cr0));
    return cr0 & (1 << 3);
}

static inline void fpu_trap_off(void)
{
    clts();
}

#endif
