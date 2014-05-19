/**
 * \file
 * \brief kernel execution and miscellany
 */

/*
 * Copyright (c) 2007, 2008, 2010 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_EXEC_H
#define KERNEL_EXEC_H

void reboot(void) __attribute__ ((noreturn));
void halt(void) __attribute__ ((noreturn));
/**
 * \brief Go to user-space at entry point 'entry'.
 *
 * This function goes to user-space and starts executing the program at
 * its entry point at virtual address 'entry'.
 *
 * \param entry Entry point address of program to execute.
 */
void __attribute__ ((noreturn)) execute(lvaddr_t entry);

/**
 * \brief Resume the given user-space snapshot.
 *
 * This function resumes user-space execution by restoring the CPU
 * registers with the ones given in the array, pointed to by 'regs'.
 */
void __attribute__ ((noreturn)) resume(arch_registers_state_t *state);

/**
 * \brief Halt processor until an interrupt arrives.
 *
 * For use in the idle loop when nothing is runnable. This function
 * puts the processor into system mode and enable interrupts on entry
 * and makes no use of the stack.
 */
void __attribute__ ((noreturn)) wait_for_interrupt(void);

#endif // KERNEL_EXEC_H
