/**
 * \file
 * \brief Arch specific definition of the registers, can be included by anyone.
 * Definitions only seen in the user.
 */

/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_32_BARRELFISH_REGISTERS_H
#define TARGET_X86_32_BARRELFISH_REGISTERS_H

#include <target/x86_32/barrelfish_kpi/registers_target.h>
#include <target/x86_32/barrelfish/dispatcher_target.h>
#include "threads_priv.h"

static inline void
registers_x86_32_set_initial(struct registers_x86_32 *regs, struct thread *thread,
                             lvaddr_t entry, lvaddr_t stack, uint32_t arg1,
                             uint32_t arg2, uint32_t arg3, uint32_t arg4)
{
    assert(stack != 0);

    // Put 4 arguments and return address on the function's stack
    stack -= 5 * 4;
    uint32_t *sp = (uint32_t *)stack;
    sp[0] = 0;          // fake return address
    sp[1] = arg1;
    sp[2] = arg2;
    sp[3] = arg3;
    sp[4] = arg4;

    regs->eip = entry;
    regs->esp = stack;
    regs->fs     = 0;
    regs->gs     = get_dispatcher_x86_32(thread->disp)->disp_seg_selector;
    regs->cs = USER_CS;
    regs->ss = USER_SS;
    regs->eflags = USER_EFLAGS;
}

#endif // TARGET_X86_32_BARRELFISH_REGISTERS_H
