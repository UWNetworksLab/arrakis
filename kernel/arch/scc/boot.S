/*
 * Copyright (c) 2007, 2008, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <target/x86_32/offsets_target.h>
#include <init.h>
#include <x86.h>
#include <barrelfish_kpi/pic_arch.h>
#include <diteinfo.h>

#define PROT_MODE_ENABLE 1
#define PAGING_ENABLE 31
#define PROT_CS 8
#define PROT_DS 16
#define PAE 0x20
#define LME 8
#define BOOT_AP_KERNEL_SIZE 4096
#define SCC_REAL_MODE_SEGMENT	0xff00
#define SCC_REAL_MODE_OFFSET	0x1000

/**
 * Get doxygen to ignore the rest of this file, as it is very confused otherwise
 * \cond
 */

	.text
	.globl start, halt
        .align 4
	.code16

//start the 16bit real-mode code here

start:
        cli
	mov $SCC_REAL_MODE_SEGMENT,%ax
	mov %ax,%ds
	mov $(gdt_ptr - start + SCC_REAL_MODE_OFFSET),%esi
	lgdt (%esi)
	mov %cr0,%eax
	or $PROT_MODE_ENABLE,%al
	mov %eax,%cr0
//	jmp PROT_CS:start_ap_pm
	.byte 0x66
	    .byte 0xea
	    .long start_pm - start + X86_32_START_KERNEL_PHYS
	    .word PROT_CS

//start the 32bit protected-mode code here

        .code32
start_pm:
        // set up stack
	mov $PROT_DS,%eax
	mov %eax,%ss
	mov %eax,%ds
	mov %eax,%es
	mov %eax,%fs
	mov %eax,%gs

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
	pushl	$0
	pushl	$DITE_BOOT_MAGIC
	call	arch_init

	/* Halt -- this should never be reached */
halt:	hlt
	jmp	halt

qemu_start:
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

        .align 16
gdt:
        .byte 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        .byte 0xff,0xff,0x00,0x00,0x00,0x9a,0xcf,0x00 // 32bit code segment for protected-mode
        .byte 0xff,0xff,0x00,0x00,0x00,0x93,0xcf,0x00 // 32bit data segment for protected-mode

gdt_ptr:
        .word gdt_ptr - gdt
	.long gdt - start + X86_32_START_KERNEL_PHYS
	.long 0

/**
 * \endcond
 */
