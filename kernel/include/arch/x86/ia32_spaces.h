/**
 * \file
 * \brief Address-space support for Mackerel IA32-MSR device definitions
 */

/*
 * Copyright (c) 2007-2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IA32_SPACES_H
#define IA32_SPACES_H

static inline uint64_t ia32_msr_read_64(ia32_t *dev, size_t offset);
static inline void ia32_msr_write_64(ia32_t *dev, size_t offset,
                                     uint64_t value);

static inline uint32_t ia32_msr_read_32(ia32_t *dev, size_t offset);
static inline void ia32_msr_write_32(ia32_t *dev, size_t offset,
                                     uint32_t value);

static inline uint64_t ia32_msr_read_64(ia32_t *dev, size_t offset)
{
    return rdmsr(offset);
}

static inline void ia32_msr_write_64(ia32_t *dev, size_t offset,
                                     uint64_t value)
{
    wrmsr(offset, value);
}

static inline uint32_t ia32_msr_read_32(ia32_t *dev, size_t offset)
{
    return rdmsr(offset) & 0xffffffff;
}

static inline void ia32_msr_write_32(ia32_t *dev, size_t offset,
                                     uint32_t value)
{
    wrmsr(offset, value);
}

#endif // IA32_SPACES_H
