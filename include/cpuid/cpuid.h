/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CPUID_H_
#define CPUID_H_

#define CPUID(offset, reg)      __asm volatile("cpuid" : reg : "a" (offset))

#include <barrelfish/barrelfish.h>

static inline uint32_t cpuid_eax_read_32(cpuid_t *dev, size_t offset)
{
    uint32_t eax;
    CPUID(offset, "=a" (eax));
    return eax;
}

static inline uint32_t cpuid_ebx_read_32(cpuid_t *dev, size_t offset)
{
    uint32_t ebx;
    CPUID(offset, "=b" (ebx));
    return ebx;
}

static inline uint32_t cpuid_ecx_read_32(cpuid_t *dev, size_t offset)
{
    uint32_t ecx;
    CPUID(offset, "=c" (ecx));
    return ecx;
}

static inline uint32_t cpuid_edx_read_32(cpuid_t *dev, size_t offset)
{
    uint32_t edx;
    CPUID(offset, "=d" (edx));
    return edx;
}

static inline uint32_t cpuid_dcpa_read_32(cpuid_t *dev, size_t offset)
{
    return 0;
}

static inline uint32_t cpuid_dcpb_read_32(cpuid_t *dev, size_t offset)
{
    return 0;
}

static inline uint32_t cpuid_dcpc_read_32(cpuid_t *dev, size_t offset)
{
    return 0;
}

#endif // CPUID_H_

