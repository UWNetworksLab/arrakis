/**
 * \file
 * \brief Bootstrap the kernel.
 */
/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__   1
#endif

#include <barrelfish_kpi/flags_arch.h> // ARM_MODE_MASK
#include <offsets.h> // BOOT_STACK_PHYS
        
        .text
        .arm

        .globl start, halt, got_base
        .extern kernel_stack, glbl_core_data

        // Used to track phys memory allocator limit globally.
        alloc_top .req r11

start:
        // On entry:
        //
        // MMU disabled
        // Caches in unknown state, but no lockdown
        // No TLB lockdown.
        // CPU is in a priviledged mode.
        //
        mrs     r3, cpsr                        // Ensure in SYS mode
        bic     r3, r3, #ARM_MODE_MASK
        orr     r3, r3, #ARM_MODE_SYS
        msr     cpsr_c, r3

		//init stack
        ldr     sp, =kernel_stack
        add	sp, sp, #KERNEL_STACK_SIZE

	ldr	PIC_REGISTER, =got_base

        //prepare argument
        mov		r0, r2
        b 		arch_init
        b		halt


/**
 * extern "C" void halt(void) __attribute__((noreturn))
 */
halt:
        b       .

.ltorg

got_base:
        .word                                   // Initialized by linker

        .end
