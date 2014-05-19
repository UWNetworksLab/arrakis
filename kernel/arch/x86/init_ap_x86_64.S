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

#include <target/x86_64/offsets_target.h>
#include <init.h>
#include <x86.h>
#include <target/x86_32/barrelfish_kpi/paging_target.h>
#include <target/x86_64/barrelfish_kpi/paging_target.h>

#define PROT_MODE_ENABLE 1
#define PAGING_ENABLE 31
#define PROT_CS 0x0018
#define PROT_DS 0x0030
#define LONG_MODE_CS 0x0008
#define PAE 0x20
#define LME 8
#define BOOT_AP_KERNEL_SIZE 4096
#define TABLE_BITS ((1 << 0) | (1 << 1) | (1 << 2))
#define PAGE_BITS ((1 << 0) | (1 << 1) | (1 << 2) | (1 << 7))

/**
 * Get doxygen to ignore the rest of this file, as it is very confused otherwise
 * \cond
 */

	.text
        .align 4096
	.code16
	.org X86_64_REAL_MODE_OFFSET


//start the 16bit real-mode code here

	.global x86_64_start_ap
x86_64_start_ap:
        cli
	mov $X86_64_REAL_MODE_SEGMENT,%ax
	mov %ax,%ds
	mov $(gdt_ptr - x86_64_start_ap),%si
	lgdt (%si)
        
        // Work around for M5's limited support for protected
        // mode.  Once in protected mode it will treat instruction
        // fetches as if ES is the segment selector.  Therefore
        // we initialize it beforehand to the expected value.
        mov $PROT_CS,%ax
        mov %ax,%es

	mov %cr0,%eax
	or $PROT_MODE_ENABLE,%al
	mov %eax,%cr0
//	jmp PROT_CS:start_ap_pm
	.byte 0x66
	    .byte 0xea
	    .long start_ap_pm - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET
	    .word PROT_CS


//start the 32bit protected-mode code here

        .code32
start_ap_pm:
        // set up data segment
	mov $PROT_DS,%eax
	mov %eax,%ds

        // acquire start-up lock
	mov $(x86_64_init_ap_lock - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET),%esi
start_ap_spin:
	xor %ax,%ax
	lock bts %ax,(%esi)
	jc start_ap_spin

        // set up stack
	mov $PROT_DS,%eax
	mov %eax,%ss
	mov $(start_ap_stack - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET),%esp

	// Fill the four page tables with identity map
	// eax = page table index, ebx = page table entry
	mov $0, %ecx
	mov $PAGE_BITS, %ebx
rep_fill:
	mov %ebx, init_ap_pdir - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET(%ecx)
	movl $0, init_ap_pdir - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET + 4(%ecx)
	add $0x200000, %ebx
	add $8, %ecx
	cmp $4096 * 4, %ecx
	jne rep_fill

        // enable page address extension
        mov %cr4,%eax
	or $PAE,%eax
	mov %eax,%cr4

        // set PML4 pointer to cr3. the right value is copied to the code
	// here by start_aps_startall
//	.global pml4_code_ptr
//pml4_code_ptr:
//	mov $0xffeeddcc,%eax
	mov $(init_ap_pml4 - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET), %eax
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

        // jmp to long-mode to the linear address corresponding the
	// real mode segment REAL_MODE_SEGMENT
//	jmp LONG_MODE_CS:start_ap_64
        .byte 0xea
	    .long start_ap_64 - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET
	    .word LONG_MODE_CS





//start the 64bit long-mode code here

        .code64
start_ap_64:
	
	// initialize bootup stack for the 64bit long mode
	lea (start_ap_stack)(%rip), %rsp
//        lea (kernel_stack + KERNEL_STACK_SIZE)(%rip), %rsp



//we should not return here after the call, but for any case, use a call
//instead of a jmp...

        // jmp to the first C initialization function in the kernel
	// the address is copied here to this code by start_aps_startall
	// the jmp is a jmp to an absolute address. it is difficult to compute
	// it here, because it is IP-relative and the IP here is
	// REAL_MODE_LINEAR_OFFSET + some offset for _every_ AP to be started,
	// independently of the final kernel location

	lea (x86_64_init_ap_global)(%rip),%rbx
	mov (%rbx),%rbx
	mov $KERNEL_BOOT_MAGIC,%rax

        lea (x86_64_init_ap_absolute_entry)(%rip),%rcx
	mov (%rcx),%rcx
	call *%rcx

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
        .byte 0xff,0xff,0x00,0x00,0x00,0x9a,0xaf,0x00 // 64bit code segment, D _cleared_ => "16bit"
        .byte 0xff,0xff,0x00,0x00,0x00,0x92,0xcf,0x00 // data
        .byte 0xff,0xff,0x00,0x00,0x00,0x9a,0xcf,0x00 // 32bit code segment for protected-mode
	.byte 0xff,0xff,0x00,0x80,0x0b,0x92,0xff,0x00 // screen
        .byte 0xff,0xff,0x00,0x60,0x00,0x9a,0xcf,0x00 // segment at linear address 0x6000
        .byte 0xff,0xff,0x00,0x00,0x00,0x92,0xaf,0x00 // stack segment in 64bit mode


gdt_ptr:
        .word gdt_ptr - gdt
	.long gdt - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET
	.long 0

	// 4GByte identity mapped page-table using 2MByte pages
	.align X86_64_BASE_PAGE_SIZE
init_ap_pml4:
	.quad (init_ap_pdpt - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET) + TABLE_BITS
	// Fill the rest with zeroes
	.fill X86_64_BASE_PAGE_SIZE - (1 * 8), 1, 0

	.align X86_64_BASE_PAGE_SIZE
init_ap_pdpt:
	.quad (init_ap_pdir  - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET) + TABLE_BITS
	.quad (init_ap_pdir2 - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET) + TABLE_BITS
	.quad (init_ap_pdir3 - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET) + TABLE_BITS
	.quad (init_ap_pdir4 - x86_64_start_ap + X86_64_REAL_MODE_LINEAR_OFFSET) + TABLE_BITS
	// Fill the rest with zeroes
	.fill X86_64_BASE_PAGE_SIZE - (4 * 8), 1, 0

	.align X86_64_BASE_PAGE_SIZE
init_ap_pdir:
	.fill X86_64_BASE_PAGE_SIZE, 1, 0

	.align X86_64_BASE_PAGE_SIZE
init_ap_pdir2:
	.fill X86_64_BASE_PAGE_SIZE, 1, 0

	.align X86_64_BASE_PAGE_SIZE
init_ap_pdir3:
 	.fill X86_64_BASE_PAGE_SIZE, 1, 0

	.align X86_64_BASE_PAGE_SIZE
init_ap_pdir4:
	.fill X86_64_BASE_PAGE_SIZE, 1, 0

        // the absolute destination address to the first C function in the kernel.
	// The address is copied to this variable by start_aps_startall.
	.global x86_64_init_ap_absolute_entry
x86_64_init_ap_absolute_entry:
        .long 0
	.long 0

        .global x86_64_init_ap_global
x86_64_init_ap_global:
        .long 0
	.long 0


        .global x86_64_init_ap_wait
x86_64_init_ap_wait:
        .long 0

        .global x86_64_init_ap_lock
x86_64_init_ap_lock:
        .byte 0


        .global x86_64_start_ap_end
x86_64_start_ap_end:

/**
 * \endcond
 */
