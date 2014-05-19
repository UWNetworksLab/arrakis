/**
 * \file jmp.S
 * \brief setjmp and longjmp implementation for Barrelfish
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */



#include "asm.h"

BEGIN_PROC(setjmp)
//the callee-saved registers
    mov %rbx,   (%rdi)
    mov %rbp,  8(%rdi)
    mov %r12, 16(%rdi)
    mov %r13, 24(%rdi)
    mov %r14, 32(%rdi)
    mov %r15, 40(%rdi)

//we save here rsp + 8 instead of rsp, so that we can use a jmp in longjmp
//and have rsp pointing to the right location on the stack.
//usually, a ret would load a return address from the stack and implicitely
//add 8 to rsp
    lea 8(%rsp), %rax //rsp+8 (not rsp), so we can jmp in longjmp wit correct rsp
    mov %rax, 48(%rdi)

//save the return address (to the function which calls setjmp)
    mov 0(%rsp), %rax
    mov %rax, 56(%rdi)
    xor %rax,%rax
    ret
END_PROC(setjmp)

BEGIN_PROC(longjmp)
    mov %rsi,%rax
    cmp $0, %rax
    jne longjmp_ret_ok
//in case the user calls longjmp with the second argument == 0
//return 1 instead (according to the spec), because returning 0 means
//that setjmp returned from an odrinary call (rather than a return caused by
//longjmp)
    mov $1, %rax

longjmp_ret_ok:
//restore the callee-saved registers
    mov   (%rdi), %rbx
    mov  8(%rdi), %rbp
    mov 16(%rdi), %r12
    mov 24(%rdi), %r13
    mov 32(%rdi), %r14
    mov 40(%rdi), %r15

//restore the stack pointer
    mov 48(%rdi), %rsp

//restore the return address and _jump_ there (don't call, because we don't
//want to push a return address here to the stack). rsp has already the value
//as it would have after setjmp executed the ret instruction
    mov 56(%rdi), %rcx
    jmp *%rcx
END_PROC(longjmp)

