/**
 * \file
 * \brief Bootstrap the kernel.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <multiboot.h>
#include <target/x86_32/offsets_target.h>
#include <barrelfish_kpi/pic_arch.h>

/* The flags for the Multiboot header */
#define MB_FLAGS (MULTIBOOT_HEADER_FLAG_MODS_PGALIGNED | MULTIBOOT_HEADER_FLAG_NEED_MEMINFO)

	.text
	.globl	start, halt
	/* Multiboot header, 4-byte aligned */
	.align	4
	.long	MULTIBOOT_HEADER_MAGIC               /* magic */
	.long	MB_FLAGS                             /* flags */
	.long	-(MULTIBOOT_HEADER_MAGIC + MB_FLAGS) /* checksum */

start:
	// Use fake stack and hope it doesn't break
	lea	X86_32_KERNEL_STACK_SIZE, %esp

	// Initialize position-independent code
	INIT_PIC()

	/* Initialize the stack pointer */
	lea	MUNG_LOCAL(x86_32_kernel_stack + X86_32_KERNEL_STACK_SIZE), %esp

	/* Reset EFLAGS */
	pushl	$0
	popf

	/* Enter architecture-specific init -- this should never return */
	push	%ebx		/* Pointer to multiboot info struct */
	push	%eax		/* Multiboot magic value */
	call	arch_init

	/* Halt -- this should never be reached */
halt:	hlt
	jmp	halt
