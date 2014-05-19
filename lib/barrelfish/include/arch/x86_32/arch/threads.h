/**
 * \file
 * \brief Threads architecture-specific code
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_ARCH_THREADS_H
#define LIBBARRELFISH_ARCH_THREADS_H

#include <barrelfish/syscall_arch.h>

#if 0
/* this is a label defined in the assembler code that implements cap_invoke() */
extern void barrelfish_post_syscall_instr(void);

/**
 * Returns true iff the thread with the given save area has successfully
 * performed a syscall. Used for the thread_invoke_cap_and_exit() hack.
 */
static inline bool thread_check_syscall_succeeded(uintptr_t *save_area)
{
    return ((save_area[EIP_REG] == (lvaddr_t)barrelfish_post_syscall_instr)
            && save_area[EAX_REG] == 0);
}
#endif

/**
 * \brief Enable FPU trap.
 */
static inline void fpu_trap_on(void)
{
    errval_t err = sys_x86_fpu_trap_on();
    assert_disabled(err_is_ok(err));
}

#endif // LIBBARRELFISH_ARCH_THREADS_H
