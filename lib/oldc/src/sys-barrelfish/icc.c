/**
 * \file
 * \brief Support code for the Intel compiler
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>

void *_intel_fast_memcpy(void *dest, const void *src, size_t n);
void *_intel_fast_memcpy(void *dest, const void *src, size_t n)
{
    return memcpy(dest, src, n);
}

void __intel_new_proc_init_R(void);
void __intel_new_proc_init_R(void)
{
    /* I have no idea what this is supposed to do... */
}
