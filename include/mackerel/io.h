/**
 * \file
 * \brief Mackerel I/O port support
 */

/*
 * Copyright (c) 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_MACKEREL_IO_H
#define LIBBARRELFISH_MACKEREL_IO_H

typedef uint16_t mackerel_io_t;

#if defined(IN_KERNEL)

#include <x86.h>

/*
 * Reading from I/O space
 */
static inline uint8_t mackerel_read_io_8( mackerel_io_t base, int offset)
{
    volatile uint8_t result = inb(base+offset);
    return result;
}
static inline uint16_t mackerel_read_io_16( mackerel_io_t base, int offset)
{
    return inw(base+offset);
}
static inline uint32_t mackerel_read_io_32( mackerel_io_t base, int offset)
{
    return ind(base+offset);
}

/*
 * Writing to I/O space
 */
static inline void mackerel_write_io_8( mackerel_io_t base, int offset, volatile uint8_t v)
{
    outb(base+offset, v);
}
static inline void mackerel_write_io_16( mackerel_io_t base, int offset, uint16_t v)
{
    outw(base+offset, v);
}
static inline void mackerel_write_io_32( mackerel_io_t base, int offset, uint32_t v)
{
    outd(base+offset, v);
}

#else // defined(IN_KERNEL)

#include <stdio.h>
#include <stdint.h>
#include <arch/x86/barrelfish/iocap_arch.h>

/*
 * Reading from I/O space
 */
static inline uint8_t mackerel_read_io_8( mackerel_io_t base, int offset)
{
    uint8_t val;
    errval_t err = iocap_in8(cap_io, base+offset, &val);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "calling iocap_in8");
    }
    return err_is_ok(err) ? val : -1;
}
static inline uint16_t mackerel_read_io_16( mackerel_io_t base, int offset)
{
    uint16_t val;
    errval_t err = iocap_in16(cap_io, base+offset, &val);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "calling iocap_in16");
    }
    return err_is_ok(err) ? val : -1;
}
static inline uint32_t mackerel_read_io_32( mackerel_io_t base, int offset)
{
    uint32_t val;
    errval_t err = iocap_in32(cap_io, base+offset, &val);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "calling iocap_in32");
    }
    return err_is_ok(err) ? val : -1;
}

/*
 * Writing to I/O space
 */
static inline void mackerel_write_io_8( mackerel_io_t base, int offset, uint8_t v)
{
    errval_t err = iocap_out8(cap_io, base+offset, v);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "calling iocap_out8");
    }
}
static inline void mackerel_write_io_16( mackerel_io_t base, int offset, uint16_t v)
{
    errval_t err = iocap_out16(cap_io, base+offset, v);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "calling iocap_out16");
    }
}
static inline void mackerel_write_io_32( mackerel_io_t base, int offset, uint32_t v)
{
    errval_t err = iocap_out32(cap_io, base+offset, v);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "calling iocap_out32");
    }
}
#endif // IN_KERNEL

#endif // LIBBARRELFISH_MACKEREL_IO_H

