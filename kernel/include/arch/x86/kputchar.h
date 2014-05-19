/**
 * \file
 * \brief kprintf support
 */

/*
 * Copyright (c) 2008, 2010, 2011, 2012 ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARCH_X86_KPUTCHAR_H
#define KERNEL_ARCH_X86_KPUTCHAR_H

#include <arch/x86/conio.h>
#include <serial.h>
#include <arch/x86/global.h>

#define kprintf_begin()  acquire_spinlock(&global->locks.print)
#define kprintf_end()    release_spinlock(&global->locks.print)

#if 0
static void delay(uint64_t ticks)
{
    uint64_t start = rdtsc();
    while (rdtsc() < start + ticks) {}
}
#endif

/* send all output to both VGA console and serial port, for now */
static inline int
kputchar(int c)
{
#ifndef __scc__
    if (c == '\n') {
        serial_console_putchar('\r');
        //delay(200000000);
    }

    conio_putchar(c);
#endif
    serial_console_putchar(c);
    return c;
}



#endif // KERNEL_ARCH_X86_KPUTCHAR_H
