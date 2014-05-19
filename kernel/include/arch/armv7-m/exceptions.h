/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __EXCEPTIONS_H__
#define __EXCEPTIONS_H__

//these are the indexes for the vector table, multiply by 4 to get the offsets
#define ARM_7M_EVECTOR_RESET        1
#define ARM_7M_EVECTOR_NMI          2
#define ARM_7M_EVECTOR_HARDFAULT    3
#define ARM_7M_EVECTOR_MEM          4
#define ARM_7M_EVECTOR_BUS          5
#define ARM_7M_EVECTOR_USAGE        6
#define ARM_7M_EVECTOR_SVCALL       11
#define ARM_7M_EVECTOR_DEBUGMON     12
#define ARM_7M_EVECTOR_PENDSV       14
#define ARM_7M_EVECTOR_SYSTICK      15
#define ARM_7M_EVECTOR_EXTERNAL     16


#define CACHE_LINE_BYTES 32

#if !defined(__ASSEMBLER__)

void *vectortable;//address of vector table. (will also be mapped to virtual address 0)
void *irq_save_pushed_area_top;//small area for threads that do not have a stack yet

/**
 * \brief Install and trigger special exception handler, that continues startup
 */
void exceptions_early_init(void);


/**
 * Install and enable exception vectors.
 *
 * This routine properly sets up the vector table, installing
 * the basic exception handlers
 */
void exceptions_init(void);

/**
 * Handle page fault in user-mode process.
 */
void handle_user_page_fault(arch_registers_state_t* saved_context)
    __attribute__((noreturn));

/**
 * Handle undefined instruction fault in user-mode process.
 */
void handle_user_undef(arch_registers_state_t* saved_context)
    __attribute__((noreturn));

/**
 * Handle faults in occuring in a priviledged mode.
 */
void fatal_kernel_fault(uint32_t   evector,
                        lvaddr_t   fault_address,
                        arch_registers_state_t* saved_context)
    __attribute__((noreturn));

/**
 * Handle any IRQ except system calls.
 *
 */
void handle_irq(uint32_t irq, arch_registers_state_t* saved_context)
    __attribute__((noreturn));

#endif // !defined(__ASSEMBLER__)

#endif // __EXCEPTIONS_H__

