/*
 * Copyright (c) 2007, 2008, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

// XXX: for some reason, on GCC 4.4.4-14ubuntu5, this is preprocessor-defined
// to 1, and undergoes bogus expansion confusing the assembler... -AB
#ifdef __i686
#undef __i686
#endif

// Put PIC-register getter functions here, so they're in the kernel
#ifdef __PIC__
	.section .gnu.linkonce.t.__i686.get_pc_thunk.cx,"ax",@progbits
.globl __i686.get_pc_thunk.cx
	.hidden  __i686.get_pc_thunk.cx
	.type    __i686.get_pc_thunk.cx,@function
__i686.get_pc_thunk.cx:
	movl (%esp), %ecx
	ret

	.section .gnu.linkonce.t.__i686.get_pc_thunk.dx,"ax",@progbits
.globl __i686.get_pc_thunk.dx
	.hidden  __i686.get_pc_thunk.dx
	.type    __i686.get_pc_thunk.dx,@function
__i686.get_pc_thunk.dx:
	movl (%esp), %edx
	ret

	.section .gnu.linkonce.t.__i686.get_pc_thunk.di,"ax",@progbits
.globl __i686.get_pc_thunk.di
	.hidden  __i686.get_pc_thunk.di
	.type    __i686.get_pc_thunk.di,@function
__i686.get_pc_thunk.di:
	movl (%esp), %edi
	ret
#endif
