/** \file
 *  \brief compare and set (cas) implementations
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CAS_H_
#define CAS_H_

#include <stdint.h>
#include <stdbool.h>

/*
 * \brief compare and set. If the value at address
 *        equals old, set it to new otherwise don't write to address.
 *        Returns the actual previous value at the address.
 */
static inline uintptr_t cas_ret_act(volatile uintptr_t *address, uintptr_t old,
        uintptr_t new)
{
    register uintptr_t res;
    __asm volatile("lock; cmpxchgq %2,%0    \n\t"
                   : "+m" (*address), "=a" (res)
                   : "r" (new), "a" (old)
                   : "memory");
    return res;
}

/*
 * \brief compare and set. If the value at address
 *        equals old, set it to new and return true,
 *        otherwise don't write to address and return false
 */
static inline bool cas(volatile uintptr_t *address, uintptr_t old,
        uintptr_t new)
{
    register bool res;
    __asm volatile("lock; cmpxchgq %2,%0     \n\t"
                   "setz %1                  \n\t"
                   : "+m" (*address), "=q" (res)
                   : "r" (new), "a" (old)
                   : "memory");
    return res;
}

#endif /* CAS_H_ */
