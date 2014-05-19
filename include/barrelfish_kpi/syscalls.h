/**
 * \file
 * \brief System call numbers.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_SYSCALLS_H
#define BARRELFISH_SYSCALLS_H

#ifndef __ASSEMBLER__

/// return type from a system call: two words
struct sysret {
    errval_t  error;
    uintptr_t value;
};

/// Macro used for constructing return values from single-value syscalls
#define SYSRET(x) (struct sysret){ /*error*/ x, /*value*/ 0 }
#endif // __ASSEMBLER__

/*
 * These are the system call ordinals. Please keep the space contiguous
 * as far as possible and make sure SYSCALL_COUNT is the number of system
 * calls. Lower layers may build direct-mapped syscall tables and so
 * compactness is a virtue.
 */

/* Proper Barrelfish system calls */
#define SYSCALL_INVOKE              0       ///< Invoke a cap
#define SYSCALL_YIELD               1       ///< Yield the CPU
#define SYSCALL_LRPC                2       ///< Fast LRPC

/* Debug/Benchmarking system calls */
#define SYSCALL_DEBUG               3     ///< Benchmarking and debug syscalls
#define SYSCALL_REBOOT              4     ///< Reboot the machine
#define SYSCALL_NOP                 5     ///< No operation
#define SYSCALL_PRINT               6     ///< Write to console

/* Architecture-specific syscalls 
 * FIXME: shouldn't these be in an arch-specific header? -AB */
#ifdef __ARM_ARCH_7M__  //cortex-m3 on pandaboard
//overwrite unused syscall instead of creating yet another global one
#define SYSCALL_RESUME_CONTEXT      7     ///< Resume a context that the dispatcher can't
#else
#define SYSCALL_X86_FPU_TRAP_ON     7     ///< Turn FPU trap on (x86)
#endif  //__ARM_ARCH_7M__


#define SYSCALL_X86_RELOAD_LDT      8     ///< Reload the LDT register (x86_64)

#define SYSCALL_COUNT               9     ///< Number of syscalls [0..SYSCALL_COUNT - 1]

/*
 * To understand system calls it might be helpful to know that there
 * are four different levels of abstraction with multiplexing
 * performed at three of them (compare with Tennenhouse: Layered
 * Multiplexing Considered Harmful).
 *
 * At the bottom two levels of abstraction are the system call number
 * as defined in this file.  This is one point of multiplexing and two
 * levels of abstraction; the second one comes about because the
 * system calls defined here use non primitive C types such as
 * "struct sysret" which have to be converted and dealt with by a
 * level of abstraction to deal with the encoding used over the
 * protection boundary.  For example, on some architectures
 * structures, including output structures, are passed by reference in
 * the argument list.  Therefore below the C abstraction of the system
 * call there must be a translation to an abstraction which can cross
 * the protection boundary.
 *
 * Above this is the invoke system call.  It deals with two different
 * ways of doing additional multiplexing, based on the type of the
 * capability being invoked, and the command being invoked on the
 * capability.  This defines which kernel system call implementation
 * is to be run.  An aspect of this is that the arguments to the
 * intended kernel code have to multiplexed up in user space onto the
 * invoke system call and demultiplexed by the kernel, thus preventing
 * direct dispatch to the intended implementation.
 *
 * Above this is the endpoint invocation in which the above system
 * invocation system call is performed and demultiplexed, but the
 * target capability is a special type in which the implementation
 * code being called is not in the kernel; in this the arguments to
 * the desired functionality must be marshalled in such a way that
 * they can be delivered to the desired domain instead of the kernel.
 *
 * Knowing this will help the reader understand the various different
 * marshalling and unmarshalling code found variously in
 * monitor_invocations.h, invocations.h, syscalls.c, syscall_arch.h,
 * syscall.c and related assembler.
 */

#endif // BARRELFISH_SYSCALLS_H
