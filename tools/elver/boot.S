/**
 * \file
 * \brief Bootstrap the bootloader.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "../../kernel/include/multiboot.h"

#define STACK_SIZE	4096

#define MSR_IA32_EFER   0xc0000080      ///< Extended features enables
#define PAGING_ENABLE 31
#define LONG_MODE_CS 0x0008
#define PAE 	0x20
#define LME 	8

/* The flags for the Multiboot header */
#define MB_FLAGS (MULTIBOOT_HEADER_FLAG_MODS_PGALIGNED | MULTIBOOT_HEADER_FLAG_NEED_MEMINFO)

	.code32
	.text
	.globl	start, halt
	/* Multiboot header, 4-byte aligned */
	.align	4
	.long	MULTIBOOT_HEADER_MAGIC               /* magic */
	.long	MB_FLAGS                             /* flags */
	.long	-(MULTIBOOT_HEADER_MAGIC + MB_FLAGS) /* checksum */

start:
	/* Initialize the stack pointer */
 	movl	$(stack + STACK_SIZE), %esp

	/* Reset EFLAGS */
	pushl	$0
	popf

	/* Enter main -- this should never return */
	push	%ebp
	mov	%esp, %ebp
	push	%ebx		/* Pointer to multiboot info struct */
	push	%eax		/* Multiboot magic value */
	call	startup

	// Load 64-bit GDT
	mov	$gdt_ptr, %esi
	lgdt	(%esi)

        // enable page address extension
        mov %cr4,%eax
	or $PAE,%eax
	mov %eax,%cr4

        // set PML4 pointer to cr3
	mov $boot_pml4, %eax
	mov %eax,%cr3

        // enable long-mode by setting EFER.LME
	mov $MSR_IA32_EFER,%ecx
	rdmsr
	bts $LME,%eax
	wrmsr

        // enable paging
	mov %cr0,%eax
	bts $PAGING_ENABLE,%eax
	mov %eax,%cr0

	// Setup multiboot registers
	mov  	eax, %eax
	mov  	multiboot_info, %ebx
	mov	kernel_entry, %ecx

        // jmp to long-mode to the linear address corresponding the
	// real mode segment REAL_MODE_SEGMENT
//	jmp LONG_MODE_CS:start_64
        .byte 0xea
	.long start_64
	.word LONG_MODE_CS

//start the 64bit long-mode code here
        .code64
start_64:

	// initialize bootup stack for the 64bit long mode
	lea (start_64_stack)(%rip), %rsp
//        lea (kernel_stack + KERNEL_STACK_SIZE)(%rip), %rsp

	// Jump to kernel image entry
 	jmp	*%rcx

	/* Halt -- this should never be reached */
halt:	hlt
	jmp	halt

        /* Our stack area */
        .comm   stack, STACK_SIZE

//stack for 64bit mode

        .align 16
        .fill STACK_SIZE,1,0
start_64_stack:


        .align 16
gdt:
        .byte 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        .byte 0xff,0xff,0x00,0x00,0x00,0x9a,0xaf,0x00 // 64bit code segment, D _cleared_ => "16bit"
        .byte 0xff,0xff,0x00,0x00,0x00,0x92,0xcf,0x00 // data
        .byte 0xff,0xff,0x00,0x00,0x00,0x9a,0xcf,0x00 // 32bit code segment for protected-mode
	.byte 0xff,0xff,0x00,0x80,0x0b,0x92,0xff,0x00 // screen
        .byte 0xff,0xff,0x00,0x60,0x00,0x9a,0xcf,0x00 // segment at linear address 0x6000
        .byte 0xff,0xff,0x00,0x00,0x00,0x92,0xaf,0x00 // stack segment in 64bit mode

gdt_ptr:
        .word gdt_ptr - gdt
	.long gdt
	.long 0
