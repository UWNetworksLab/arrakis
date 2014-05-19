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
#include <barrelfish/dispatch.h>

#include <barrelfish/syscalls.h>
#include <barrelfish/syscall_arch.h>
#include <barrelfish_kpi/syscalls.h>

STATIC_ASSERT_SIZEOF(struct sysret, 2 * sizeof(uintptr_t));
STATIC_ASSERT_OFFSETOF(struct sysret, error, 0 * sizeof(uintptr_t));
STATIC_ASSERT_OFFSETOF(struct sysret, value, 1 * sizeof(uintptr_t));
STATIC_ASSERT(SYSCALL_REG == 0, "Bad register for system call argument.");

//
// System call wrappers
//

errval_t sys_print(const char* string, size_t length)
{
    return syscall3(SYSCALL_PRINT, (uintptr_t)string, (uintptr_t)length).error;
}

errval_t sys_yield(capaddr_t target)
{
    STATIC_ASSERT_SIZEOF(target, sizeof(uintptr_t));
    return syscall2(SYSCALL_YIELD, (uintptr_t) target).error;
}


#ifdef __ARM_ARCH_7M__  //cortex-m3 on pandaboard
//add syscall for restoring a context that the dispatcher can not restore by itself
errval_t sys_resume_context(arch_registers_state_t* registers){
    return syscall2(SYSCALL_RESUME_CONTEXT, (uintptr_t) registers).error;
}
#endif
