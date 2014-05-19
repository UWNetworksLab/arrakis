/**
 * \file
 * \brief User-side system call implementation
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_32_BARRELFISH_SYSCALL_H
#define ARCH_X86_32_BARRELFISH_SYSCALL_H

/* Always review the generated assembly when changing this! */
// XXX: noinline for now, until I figured out why GCC doesn't obey the asm
// constraints
static __attribute__ ((noinline))
struct sysret syscall(uintptr_t arg0, uintptr_t arg1,
                      uintptr_t arg2, uintptr_t arg3,
                      uintptr_t arg4, uintptr_t arg5,
                      uintptr_t arg6)
{
    /* XXX: This pushes the PIC register and frame pointer. Some GCC
     * versions disallow specifying EBP as a register variable. Since
     * we don't know when this is the case, we currently go via memory
     * for that variable and so the frame pointer doesn't need to be
     * pushed here. Also, we don't need to push the PIC register while
     * declaring this function noinline.
     */
    /* __asm volatile("push %ebx"); */
    /* __asm volatile("push %ebp"); */

    // This order is important!
    register uintptr_t a0 __asm("edi") = arg0;
    register uintptr_t a1 __asm("esi") = arg1;
    register uintptr_t a2_ret2 __asm("edx") = arg2;
    register uintptr_t a3_ret1 __asm("eax") = arg3;
    register uintptr_t a4 __asm("ebx") = arg4;
    register uintptr_t a6 __asm("ecx") = arg6;
    //    register uintptr_t a5 __asm("ebp") = arg5;  // Needs to be last

    __asm volatile("push %%ebp                  \n\t"
                   "mov %[arg5], %%ebp          \n\t"
                   "int $0xff                   \n\t"
                   "pop %%ebp                   \n\t"
                   /* "pop %%ebx                   \n\t" */
                   : "+a" (a3_ret1), "+d" (a2_ret2), "+r" (a6)
                   : "r" (a0), "r" (a1), "r" (a4), [arg5] "m" (arg5));

    return (struct sysret){/*error*/ a3_ret1, /*value*/ a2_ret2};
}

#define syscall7(_a, _b, _c, _d, _e, _f, _g)    \
    syscall(_a, _b, _c, _d, _e, _f, _g)
#define syscall6(_a, _b, _c, _d, _e, _f)        \
    syscall7(_a, _b, _c, _d, _e, _f, 0)
#define syscall5(_a, _b, _c, _d, _e)            \
    syscall6(_a, _b, _c, _d, _e, 0)
#define syscall4(_a, _b, _c, _d)                \
    syscall5(_a, _b, _c, _d, 0)
#define syscall3(_a, _b, _c)                    \
    syscall4(_a, _b, _c, 0)
#define syscall2(_a, _b)                        \
    syscall3(_a, _b, 0)
#define syscall1(_a)                            \
    syscall2(_a, 0)

static inline errval_t sys_x86_fpu_trap_on(void)
{
    return syscall1(SYSCALL_X86_FPU_TRAP_ON).error;
}

#endif
