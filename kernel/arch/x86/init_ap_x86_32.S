/**
 * \file
 * \brief Initializes the APs to long mode.
 * This file starts an AP in real-mode, sets up the needed GDT, switches to
 * protected-mode, enables paging and the long-mode and switches to long-mode.
 * After that, it jumps to the entry point of the kernel.
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <target/x86_32/offsets_target.h>
#include <init.h>
#include <x86.h>

#define PROT_MODE_ENABLE 1
#define PAGING_ENABLE 31
#define PROT_CS 8
#define PROT_DS 16
#define PAE 0x20
#define LME 8
#define BOOT_AP_KERNEL_SIZE 4096

/**
 * Get doxygen to ignore the rest of this file, as it is very confused otherwise
 * \cond
 */

	.text
        .align 4096
	.code16
	.org X86_32_REAL_MODE_OFFSET


//start the 16bit real-mode code here

	.global x86_32_start_ap
x86_32_start_ap:
        cli
	mov $X86_32_REAL_MODE_SEGMENT,%ax
	mov %ax,%ds
	mov $(x86_32_init_ap_lock - x86_32_start_ap),%si
start_ap_spin:
	xor %ax,%ax
	lock bts %ax,(%si)
	jc start_ap_spin
	mov $(gdt_ptr - x86_32_start_ap),%esi
	lgdt (%esi)
	mov %cr0,%eax
	or $PROT_MODE_ENABLE,%al
	mov %eax,%cr0
//	jmp PROT_CS:start_ap_pm
	.byte 0x66
	    .byte 0xea
	    .long start_ap_pm - x86_32_start_ap + X86_32_REAL_MODE_LINEAR_OFFSET
	    .word PROT_CS

//start the 32bit protected-mode code here

        .code32
start_ap_pm:
        // set up stack
	mov $PROT_DS,%eax
	mov %eax,%ss
	mov %eax,%ds
	mov %eax,%es
	mov %eax,%fs
	mov %eax,%gs
	mov $(start_ap_stack - x86_32_start_ap + X86_32_REAL_MODE_LINEAR_OFFSET),%esp

        // jmp to the first C initialization function in the kernel
	// the address is copied here to this code by start_aps_startall
	// the jmp is a jmp to an absolute address. it is difficult to compute
	// it here, because it is IP-relative and the IP here is
	// REAL_MODE_LINEAR_OFFSET + some offset for _every_ AP to be started,
	// independently of the final kernel location
	mov x86_32_init_ap_global - x86_32_start_ap + X86_32_REAL_MODE_LINEAR_OFFSET,%ebx
	mov $KERNEL_BOOT_MAGIC,%eax
        mov x86_32_init_ap_absolute_entry - x86_32_start_ap + X86_32_REAL_MODE_LINEAR_OFFSET,%ecx

//we should not return here after the call, but for any case, use a call
//instead of a jmp...
	call *%ecx

//we should never reach this location...
loop_ap:
        hlt
	jmp loop_ap


//stack for 64bit mode

        .align 16
        .fill BOOT_AP_KERNEL_SIZE,1,0
start_ap_stack:


        .align 16
gdt:
        .byte 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        .byte 0xff,0xff,0x00,0x00,0x00,0x9a,0xcf,0x00 // 32bit code segment for protected-mode
        .byte 0xff,0xff,0x00,0x00,0x00,0x93,0xcf,0x00 // 32bit data segment for protected-mode

gdt_ptr:
        .word gdt_ptr - gdt
	.long gdt - x86_32_start_ap + X86_32_REAL_MODE_LINEAR_OFFSET
	.long 0

        // the absolute destination address to the first C function in the kernel.
	// The address is copied to this variable by start_aps_startall.
	.global x86_32_init_ap_absolute_entry
x86_32_init_ap_absolute_entry:
        .long 0
	.long 0

        .global x86_32_init_ap_global
x86_32_init_ap_global:
        .long 0
	.long 0


        .global x86_32_init_ap_wait
x86_32_init_ap_wait:
        .long 0
	
	.global x86_32_init_ap_apic_id
x86_32_init_ap_apic_id:
        .byte 0

        .global x86_32_init_ap_lock
x86_32_init_ap_lock:
        .byte 0


        .global x86_32_start_ap_end
x86_32_start_ap_end:

/**
 * \endcond
 */
