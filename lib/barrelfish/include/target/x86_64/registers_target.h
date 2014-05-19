/**
 * \file
 * \brief Arch specific definition of the registers, can be included by anyone.
 * Definitions only seen in the user.
 */

/*
 * Copyright (c) 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_64_BARRELFISH_REGISTERS_H
#define TARGET_X86_64_BARRELFISH_REGISTERS_H

#include <target/x86_64/barrelfish_kpi/registers_target.h>
#include <target/x86_64/barrelfish_kpi/cpu_target.h>
#include "threads_priv.h"

static inline void
registers_x86_64_set_initial(struct registers_x86_64 *regs, struct thread *thread,
                             lvaddr_t entry, lvaddr_t stack, uint64_t arg1,
                             uint64_t arg2, uint64_t arg3, uint64_t arg4)
{
    // XXX: the x86-64 ELF ABI requires that (RSP - 8) be 16-byte aligned. why?!
    assert((stack % 16) == 0);
    stack -= sizeof(uintptr_t);

    regs->rip    = entry;
    regs->rsp    = stack;
    regs->eflags = USER_EFLAGS;
    regs->fs     = thread->thread_seg_selector;
    regs->gs     = 0;

    regs->rdi = arg1;
    regs->rsi = arg2;
    regs->rdx = arg3;
    regs->rcx = arg4;
}

#endif // TARGET_X86_64_BARRELFISH_REGISTERS_H
