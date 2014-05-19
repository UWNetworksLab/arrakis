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

#ifndef ARCH_X86_64_BARRELFISH_SYSCALL_H
#define ARCH_X86_64_BARRELFISH_SYSCALL_H

#include <barrelfish_kpi/syscalls.h>  // for struct sysret.

/*
 * Introduction to the syscall macros, or "what the hell is going on here"?
 *
 * The macros here are collectively used to expand the body of the inlined
 * function syscall() that implements all syscalls except for cap_invoke(),
 * which is a special case, due to the thread_invoke_cap_and_exit() hack.
 *
 * First, BF_SYSCALL_BODY sets up the register arguments to be passed into the
 * syscall, then BF_SYSCALL_ASM contains the asm code that includes the actual
 * syscall. This macro has a "label" argument which can be used to emit an
 * arbitrary bit of assembly code after the syscall instruction. This is used
 * by cap_invoke() to insert a label immediately following the syscall, so that
 * the threads package can tell if a thread that was about to do an invocation
 * actually reached the syscall instruction. Note that this can't be inserted
 * in the syscall code unconditionally, because we want it to be inlined, and
 * that would result in multiple definitions of the symbol.
 *
 * If you can think of a better way to do this, by all means clean it up! :)
 */

/* FIXME: the first version of this code is optimal, and works when we are
 * compiling either with optimisation on (-O) or at any optimisation level
 * without a frame pointer (-fomit-frame-pointer), however not for the
 * standard unoptimised GCC, which insists on using RBP.
 */
#if 0 /* was if defined(NDEBUG), but gcc is still screwing this up -- AB */
// XXX: Need to update this to the new syscall ABI before it will work again
# define BF_SYSCALL_ASM(arg11, label) \
    register uint64_t a11 __asm("rbp") = arg11;     \
    __asm volatile("syscall\n\t"                    \
                   label                            \
        : "+r" (a10_ret1), "+r" (a2_ret2)           \
        : "r" (a1), "r" (a3), "r" (a4), "r" (a5), "r" (a6), "r" (a7), \
          "r" (a8), "r" (a9), "r" (a11), "r" (a12), "r" (syscall_num) \
        : "r11", "rcx");
#else /* DEBUG */
# ifndef ARRAKIS
#  define BF_SYSCALL_ASM(arg11, label) \
    __asm volatile("pushq %%rbp             \n\t"   \
                   "movq %%rcx, %%rbp       \n\t"   \
                   "syscall                 \n\t"   \
                   label                            \
                   "popq %%rbp              \n\t"   \
        : "+a" (a10_ret1), "+c" (arg11), "+d" (a2_ret2), "+r" (a1), \
          "+r" (a3), "+r" (a4), "+r" (a5), "+r" (syscall_num)  \
        : "r" (a6), "r" (a7), "r" (a8), "r" (a9), "r" (a12) \
        : "r11");
#else
#  define BF_SYSCALL_ASM(arg11, label) \
    __asm volatile("pushq %%rbp             \n\t"   \
                   "movq %%rcx, %%rbp       \n\t"   \
                   "vmmcall                 \n\t"   \
                   label                            \
                   "popq %%rbp              \n\t"   \
        : "+a" (a10_ret1), "+c" (arg11), "+d" (a2_ret2), "+r" (a1), \
          "+r" (a3), "+r" (a4), "+r" (a5), "+r" (syscall_num)  \
        : "r" (a6), "r" (a7), "r" (a8), "r" (a9), "r" (a12) \
        : "r11");
#endif
#endif

/* NB: We use a10_ret (in the rax register) as both input and output
 * register, because some versions of GCC (eg. 4.2.3) fail to use rax as
 * both an input and separately as an output register
 */
#define BF_SYSCALL_BODY(num, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, \
                        arg9, arg10, arg11, arg12, label) \
    register uint64_t syscall_num __asm("rdi") = num;   \
    register uint64_t a1 __asm("rsi") = arg1;           \
    register uint64_t a2_ret2 __asm("rdx") = arg2;           \
    register uint64_t a3 __asm("r10") = arg3;           \
    register uint64_t a4 __asm("r8") = arg4;            \
    register uint64_t a5 __asm("r9") = arg5;            \
    register uint64_t a6 __asm("r12") = arg6;           \
    register uint64_t a7 __asm("r13") = arg7;           \
    register uint64_t a8 __asm("r14") = arg8;           \
    register uint64_t a9 __asm("r15") = arg9;           \
    register uint64_t a12 __asm("rbx") = arg12;         \
    register uint64_t a10_ret1 __asm("rax") = arg10;     \
    BF_SYSCALL_ASM(arg11, label)


extern bool debug_notify_syscall;

#include <stdio.h>

static inline struct sysret syscall(uint64_t num, uint64_t arg1, uint64_t arg2,
                 uint64_t arg3, uint64_t arg4, uint64_t arg5, uint64_t arg6,
                 uint64_t arg7, uint64_t arg8, uint64_t arg9, uint64_t arg10,
                 uint64_t arg11, uint64_t arg12)
{
    if(debug_notify_syscall && num == SYSCALL_INVOKE) {
        char str[256];
        snprintf(str, 256, "Syscall while forbidden! from %p, %p, %p\n",
                 __builtin_return_address(0),
                 __builtin_return_address(1),
                 __builtin_return_address(2));
        BF_SYSCALL_BODY(SYSCALL_PRINT, (uint64_t)str, 256, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, "");
    }

    BF_SYSCALL_BODY(num, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
                    arg10, arg11, arg12, "");
    return (struct sysret){/*error*/ a10_ret1, /*value*/ a2_ret2};
}

#define syscall12(_a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k, _l) \
	syscall(_a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k, _l, 0)
#define syscall11(_a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k) \
	syscall12(_a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k, 0)
#define syscall10(_a, _b, _c, _d, _e, _f, _g, _h, _i, _j) \
	syscall11(_a, _b, _c, _d, _e, _f, _g, _h, _i, _j, 0)
#define syscall9(_a, _b, _c, _d, _e, _f, _g, _h, _i) \
	syscall10(_a, _b, _c, _d, _e, _f, _g, _h, _i, 0)
#define syscall8(_a, _b, _c, _d, _e, _f, _g, _h) \
	syscall9(_a, _b, _c, _d, _e, _f, _g, _h, 0)
#define syscall7(_a, _b, _c, _d, _e, _f, _g) \
	syscall8(_a, _b, _c, _d, _e, _f, _g, 0)
#define syscall6(_a, _b, _c, _d, _e, _f) \
	syscall7(_a, _b, _c, _d, _e, _f, 0)
#define syscall5(_a, _b, _c, _d, _e) \
	syscall6(_a, _b, _c, _d, _e, 0)
#define syscall4(_a, _b, _c, _d) \
	syscall5(_a, _b, _c, _d, 0)
#define syscall3(_a, _b, _c) \
	syscall4(_a, _b, _c, 0)
#define syscall2(_a, _b) \
	syscall3(_a, _b, 0)
#define syscall1(_a) \
	syscall2(_a, 0)

static inline errval_t sys_x86_fpu_trap_on(void)
{
    return syscall1(SYSCALL_X86_FPU_TRAP_ON).error;
}

static inline errval_t sys_x86_reload_ldt(void)
{
    return syscall1(SYSCALL_X86_RELOAD_LDT).error;
}

#endif
