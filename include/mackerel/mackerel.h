/**
 * \file
 * \brief Mackerel support
 */

/*
 * Copyright (c) 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_MACKEREL_H
#define LIBBARRELFISH_MACKEREL_H

#include <stdio.h>

#if defined(__x86_64__) || defined(__i386__)
// [2009-09-10 ohodson] TODO: Review moving this to a separate
// file like msr's in ia32_spaces.h.
#include <mackerel/io.h>
#endif

/*
 * Device address space types
 */
typedef char    *mackerel_addr_t;

/*
 * Reading from memory
 */
static inline uint8_t mackerel_read_addr_8( mackerel_addr_t base, int offset)
{
    volatile uint8_t *p = (volatile uint8_t *)(base + offset);
    return *p;
}

static inline uint16_t mackerel_read_addr_16( mackerel_addr_t base, int offset)
{
    volatile uint16_t *p = (volatile uint16_t *)(base + offset);
    return *p;
}
static inline uint32_t mackerel_read_addr_32( mackerel_addr_t base, int offset)
{
    volatile uint32_t *p = (volatile uint32_t *)(base + offset);
    return *p;
}
static inline uint64_t mackerel_read_addr_64( mackerel_addr_t base, int offset)
{
    volatile uint64_t *p = (volatile uint64_t *)(base + offset);
    return *p;
}

/*
 * Writing to memory
 */
static inline void mackerel_write_addr_8( mackerel_addr_t base,
                                             int offset, uint8_t v)
{
    volatile uint8_t *p = (volatile uint8_t *)(base + offset);
    *p = v;
}

static inline void mackerel_write_addr_16( mackerel_addr_t base,
                                               int offset, uint16_t v)
{
    volatile uint16_t *p = (volatile uint16_t *)(base + offset);
    *p = v;
}
static inline void mackerel_write_addr_32( mackerel_addr_t base,
                                               int offset, uint32_t v)
{
    volatile uint32_t *p = (volatile uint32_t *)(base + offset);
    *p = v;
}
static inline void mackerel_write_addr_64( mackerel_addr_t base,
                                               int offset, uint64_t v)
{
    volatile uint64_t *p = (volatile uint64_t *)(base + offset);
    *p = v;
}


#endif // LIBBARRELFISH_MACKEREL_H

