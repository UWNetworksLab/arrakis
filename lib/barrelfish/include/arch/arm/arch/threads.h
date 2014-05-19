/**
 * \file
 * \brief Threads architecture-specific code
 */

/*
 * Copyright (c) 2009, ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_ARCH_THREADS_H
#define LIBBARRELFISH_ARCH_THREADS_H

/* this is a label defined in the assembler code that implements cap_invoke() */
extern void barrelfish_cap_invoke_post_syscall_instr(void);
extern void barrelfish_lrpc_post_syscall_instr(void);

/**
 * Returns true iff the thread with the given save area has successfully
 * performed a syscall. Used for the thread_invoke_cap_and_exit() hack.
 */
static inline bool thread_check_syscall_succeeded(uintptr_t *save_area)
{
    assert(!"thread_check_syscall_succeeded: called");
    abort();
#if 0
    return ((save_area[RIP_REG] == (vaddr_t)barrelfish_cap_invoke_post_syscall_instr
             || save_area[RIP_REG] == (vaddr_t)barrelfish_lrpc_post_syscall_instr)
            && save_area[RAX_REG] == 0);
#endif
}

#endif // LIBBARRELFISH_ARCH_THREADS_H
