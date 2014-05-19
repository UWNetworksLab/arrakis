/**
 * \file
 * \brief Cache control routines for ARMv5
 */
/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <cp15.h>

// XXX: There is a function with the same name in arch/armv7/cp15.S;
//      we need to check wether can use a single function

/*
 * IXP2800_Hardware_Reference_Manual, p.94 i-cache
 * IXP2800_Hardware_Reference_Manual, p.105 D-cache
 */
void cp15_invalidate_i_and_d_caches(void)
{
    uint32_t tmp_mem;
    uint32_t *tmp = &tmp_mem; //Use variable on stack as storage space. We need a safe place in virtual memory.

    __asm volatile(
                   //Clean (write back) D-cache
                   "MOV r0, #1024 \n\t"
                   "LOOP1: \n\t"
                   "MCR p15, 0, r1, c7, c2, 5\n\t"
                   "ADD r1, r1, #32\n\t"
                   "SUBS r0, r0, #1\n\t"
                   "BNE LOOP1\n\t"
                   "MOV r0, #64\n\t"

                   //Clean (write back) mini D-cache
                   "LOOP2:\n\t"
                   "MOV r2, %[tmp]\n\t"
                   "LDR r3, [r2], #32\n\t"
                   "SUBS r0, r0, #1\n\t"
                   "BNE LOOP2\n\t"
                   "MCR p15, 0, r0, c7, c6, 0\n\t" //Invalidate D-cache
                   "mcr  p15, 0, r1, c7, c5, 0 \n\t" //Invalidate i-cache
                   ::[tmp] "r" (tmp) : "r0", "r1", "r2", "r3");
}
