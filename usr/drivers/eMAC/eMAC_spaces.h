/**
 * \file
 * \brief X86 inline asm utilities and defines
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __EMAC_SPACES_H
#define __EMAC_SPACES_H

typedef int mackerel_one_byte_t;

/*
 * Reading from Model-Specific Registers
 *
 * You might be tempted, gentle reader, to wonder why one should
 * bother with the apparently pointless volatile declaration here,
 * particularly if (as is the case with Barrelfish) rdmsr is an asm
 * volatile inline function anyway.   If you don't understand why,
 * you're not qualified to change this code.  If you do, you'll
 * understand why it should not be changed, as long as we are
 * compiling with GCC.
 */
static inline uint32_t eMAC_one_byte_read_32(eMAC_t *base, mackerel_one_byte_t idx)
{
    volatile uint32_t r = 0;
//    r = rdmsr(idx);
    assert(!"NYI");
    return r;
}

static inline uint64_t eMAC_one_byte_read_64(eMAC_t *base, mackerel_one_byte_t idx)
{
    volatile uint64_t r = 0;
//    r = rdmsr(idx);
    assert(!"NYI");
    return r;
}

/*
 * Writing to Model-Specific Registers
 */
static inline void eMAC_one_byte_write_32(eMAC_t *base, mackerel_one_byte_t idx, uint32_t v)
{
//    wrmsr(idx, v);
    assert(!"NYI");
    return;
}
static inline void eMAC_one_byte_write_64(eMAC_t *base, mackerel_one_byte_t idx, uint64_t v)
{
//    wrmsr(idx, v);
    assert(!"NYI");
    return;
}

#endif // __EMAC_SPACES_H
