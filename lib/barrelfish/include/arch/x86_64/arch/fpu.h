/**
 * \file
 * \brief FPU accessor functions.
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef X86_64_FPU_H
#define X86_64_FPU_H

/// Exception number for FPU (device) not available
#define FPU_UNAVAILABLE_TRAP    7

static inline void fpu_copy(struct registers_fpu_x86_64 *dst,
                            struct registers_fpu_x86_64 *src)
{
    // XXX: Round to next 16-byte boundary
    uint8_t *dregs = dst->registers, *sregs = src->registers;
    dregs += 16 - ((uintptr_t)dregs % 16);
    sregs += 16 - ((uintptr_t)sregs % 16);

    memcpy(dregs, sregs, 512);
}

#endif
