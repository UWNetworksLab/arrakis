/**
 * \file
 * \brief x86 debug registers
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <arch/x86/debugregs.h>

void debugregs_set_breakpoint(uintptr_t addr, uint8_t mode, uint8_t len)
{
    uintptr_t dr7 = 0;

    // Enable breakpoint 0 and make it global
    dr7 |= 3;

    // Require exact breakpoints (even if not supported - suggested by Intel
    // Software Developer's Manual Vol. 3A)
    dr7 |= 3 << 8;

    // Set mode and length
    dr7 |= (mode & 3) << 16;
    dr7 |= (len & 3) << 18;

    // Needs to be aligned on multi-byte breakpoint
    switch(len) {
    case 1:
        assert(addr % 2 == 0);
        break;
    case 3:
        assert(addr % 4 == 0);
        break;
    }

    // Set the breakpoint address
    __asm volatile ("mov %[addr], %%dr0" :: [addr] "r" (addr));

    // Set status (this enables the breakpoint)
    __asm volatile ("mov %[dr7], %%dr7" :: [dr7] "r" (dr7));
}
