/* [2009-07-30 ohodson] TODO: implement! */

/**
 * \file
 * \brief Miscellaneous architecture-specific functions
 */

/*
 * Copyright (c) 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_MISC_H
#define ARCH_MISC_H

//
// Helpers for pasting #defined values into inline assembler.
//
#define STR(x) #x
#define XTR(x) STR(x)

/**
 * \brief Set thread-local-storage register.
 */
static inline void arch_set_thread_register(uintptr_t value)
{
    __asm (
        "mov "XTR(THREAD_REGISTER)", %[value]" :: [value] "r" (value)
          );
}

static inline uintptr_t arch_get_thread_register(void)
{
    uintptr_t result;
    __asm (
        "mov %[result]," XTR(THREAD_REGISTER) :  [result] "=r" (result)
          );
    return result;
}

#endif /* ARCH_MISC_H */
