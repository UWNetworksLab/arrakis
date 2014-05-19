/**
 * \file
 * \brief System call entry point to the kernel and LRPC fast-path
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/capabilities.h>
#include <offsets.h>
#include <x86.h>
#include <asmoffsets.h>

    .text
    .globl syscall_entry

syscall_entry:
/* regular syscall path (no LRPC support) */
	/* Reserve space for return struct -- struct sysret is two uintptr_t's */
	subl    $8, %esp

	/* Setup 5-word argument buffer on stack for LMP payload */
	pushl	%ecx
	pushl	%ebp
	pushl	%ebx
	pushl	%eax
	pushl	%edx
        lea     28(%esp), %ecx
        pushl   %ecx    /* 4th function argument: CPU save frame address */
        lea     4(%esp), %ecx
        pushl   %ecx    /* 3rd function argument: buffer address */
	pushl	%esi    /* 2nd function argument: syscall arg1 */
	pushl	%edi    /* 1st function argument: syscall arg0 */

	/* Function expects save area of return struct as first "parameter" */
	lea	36(%esp), %ecx
	pushl	%ecx

        calll   sys_syscall     /* Process system call in C */

	/* Get return values */
	movl	4(%eax), %edx
	movl	(%eax), %eax

        addl    $44, %esp       /* Remove arguments from stack */
	iretl			/* Return to user-space */
