/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/debug.h>
#include <barrelfish/dispatch.h>

void debug_dump(arch_registers_state_t *archregs)
{
#define dpr(reg) debug_printf(#reg " %08"PRIu32, archregs->named. reg)
    dpr(r0);    dpr(r1);        dpr(r2);        dpr(r3);
    dpr(r4);    dpr(r5);        dpr(r6);        dpr(r7);
    dpr(rtls);  dpr(r10);       dpr(r11);       dpr(r12);
    dpr(stack); dpr(link);      dpr(pc);        dpr(cpsr);
}

void debug_call_chain(arch_registers_state_t *archregs)
{
    // TODO: use regs argument
    void* fp = __builtin_frame_address(0);
    void* ra = __builtin_return_address(0);
    if (fp != NULL) {
        debug_printf("%8d frame %p return %p\n", 0, fp, ra);
    }
}

void debug_print_save_area(arch_registers_state_t *state)
{
    debug_dump(state);
}
