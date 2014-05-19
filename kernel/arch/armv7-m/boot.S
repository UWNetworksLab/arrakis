/**
 * \file
 * \brief Bootstrap the kernel for ARMv7-M processors, specifically the 
 *      cortex-m3 on the OMAP44XX  This code is
 *      entered from the bootloader (typically arm_molly, RedBoot,
 *      etc.).
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
//        .arm

        .globl start, halt, got_base
        .extern kernel_stack, glbl_core_data

        // Used to track phys memory allocator limit globally.
        alloc_top .req r11

start:
/*
    //idea is that the cortex-A9 has already started the MMU, and included all pagemappings
    //that we need to setup the rest  

        // On entry:
        //
        // MMU already enabled, TLB filled with necessary mappings to bootstrap
        // Cache disabled
        // CPU is in priviledged thread mode.
        //

*/
        //init stack
        ldr     r6, =kernel_stack
        mov     sp, r6//can not directly ldr to sp, so use r6 inbetween
        movw    r6, #KERNEL_STACK_SIZE//can not directly add big immediates, so move it into register first
        add     sp, r6  //sp = &kernel_stack+KERNEL_STACK_SIZE

        
        ldr     r0, =got_base
        mov     PIC_REGISTER, r0
        // equivalent to: ldr     PIC_REGISTER, =got_base

        //prepare argument
        mov     r0, r2
        //do a long call to compensate for shorter range
        ldr     r6, =arch_init//shorthand, loads the value from memory
        bx      r6
        //equivalent to: b       arch_init        
        b       halt


/**
 * extern "C" void halt(void) __attribute__((noreturn))
 */
halt:
        b       .

/**********************************************************************/
.ltorg

got_base:
        .word                                   // Initialized by linker

        .end
