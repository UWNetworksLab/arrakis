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

#ifndef ARCH_ARM_BARRELFISH_SYSCALL_H
#define ARCH_ARM_BARRELFISH_SYSCALL_H

//
// This is the actual system call function. Because the return
// value is a structure, r0 is setup point to the return
// structure.  The first system call argument supplied at end of
// argument list and moved to r0 before use in syscall. This
// simplifies the amount of swizzling involved therein as r1 =
// arg1, r2 = arg2, r3 = arg3, and the remaining args including
// arg0 are on the stack.
//
extern struct sysret
syscall(uintptr_t b, uintptr_t c, uintptr_t d, uintptr_t e,
        uintptr_t f, uintptr_t g, uintptr_t h, uintptr_t i,
        uintptr_t j, uintptr_t k, uintptr_t l, uintptr_t a);

#define syscallx(a,b,c,d,e,f,g,h,i,j,k,l)                               \
    syscall(b,c,d,e,f,g,h,i,j,k,l,a)

//
// System call argument 0 is encoded thus:
//
// arg[3:0]  = syscall ordinal (e.g. SYSCALL_YIELD)
// arg[7:4]  = number of system call arguments (for sanity checking)
// arg[31:8] = SYSCALL_INVOKE arguments | do_not_care
//

//C_ASSERT(SYSCALL_COUNT <= 0xf);
#define sysord(a,n) (a) | ((n) << 4)

// The following macros add the argument count to arg0

#define syscall12(a,b,c,d,e,f,g,h,i,j,k,l)                              \
    syscallx(sysord(a,10),(b),(c),(d),(e),(f),(g),(h),(i),(j),(k),(l))

#define syscall11(a,b,c,d,e,f,g,h,i,j,k)                                \
    syscallx(sysord(a,10),(b),(c),(d),(e),(f),(g),(h),(i),(j),(k),0)

#define syscall10(a,b,c,d,e,f,g,h,i,j)                                  \
    syscallx(sysord(a,10),(b),(c),(d),(e),(f),(g),(h),(i),(j),0,0)

#define syscall9(a,b,c,d,e,f,g,h,i)                                     \
    syscallx(sysord(a,9),(b),(c),(d),(e),(f),(g),(h),(i),0,0,0)

#define syscall8(a,b,c,d,e,f,g,h)                                       \
    syscallx(sysord(a,8),(b),(c),(d),(e),(f),(g),(h),0,0,0,0)

#define syscall7(a,b,c,d,e,f,g)                                         \
    syscallx(sysord(a,7),(b),(c),(d),(e),(f),(g),0,0,0,0,0)

#define syscall6(a,b,c,d,e,f)                                           \
    syscallx(sysord(a,6),(b),(c),(d),(e),(f),0,0,0,0,0,0)

#define syscall5(a,b,c,d,e)                                             \
    syscallx(sysord(a,5),(b),(c),(d),(e),0,0,0,0,0,0,0)

#define syscall4(a,b,c,d)                                               \
    syscallx(sysord(a,4),(b),(c),(d),0,0,0,0,0,0,0,0)

#define syscall3(a,b,c)                                                 \
    syscallx(sysord(a,3),(b),(c),0,0,0,0,0,0,0,0,0)

#define syscall2(a,b)                                                   \
    syscallx(sysord(a,2),(b),0,0,0,0,0,0,0,0,0,0)

#define syscall1(a)                                                     \
    syscallx(sysord(a,1),0,0,0,0,0,0,0,0,0,0,0)


#ifdef __ARM_ARCH_7M__  //cortex-m3 on pandaboard
//add syscall for restoring a context that the dispatcher can not restore by itself
errval_t sys_resume_context(arch_registers_state_t* registers);
#endif


#endif
