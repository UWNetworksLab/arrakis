/**
 * \file
 * \brief Exception handling.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_EXCEPT_H
#define LIBBARRELFISH_EXCEPT_H

#include <sys/cdefs.h>
#include <barrelfish_kpi/registers_arch.h>

__BEGIN_DECLS

enum exception_type {
    EXCEPT_NULL = 0,
    EXCEPT_PAGEFAULT,   ///< Page fault (or other memory access fault)
    EXCEPT_BREAKPOINT,  ///< Software breakpoint
    EXCEPT_SINGLESTEP,  ///< Single-step execution
    // TODO: illegal instruction, divide by zero, etc.
    EXCEPT_OTHER
};

/// Subtype for page fault exceptions
enum pagefault_exception_type {
    PAGEFLT_NULL = 0,
    PAGEFLT_READ,   ///< Read page fault
    PAGEFLT_WRITE,  ///< Write page fault
    PAGEFLT_EXEC,   ///< Execute (instruction fetch) page fault
};

/**
 * \brief Exception handler function
 *
 * \param type    Exception type
 * \param subtype Exception subtype
 * \param addr    Exception address
 * \param regs    Register state at time of exception
 * \param fpuregs FPU state at time of exception. NULL if unused.
 *
 * \return If this function returns, the register state specified in
 * regs/fpuregs will be resumed.
 */
typedef void (*exception_handler_fn)(enum exception_type type, int subtype,
                                     void *addr, arch_registers_state_t *regs,
                                     arch_registers_fpu_state_t *fpuregs);

errval_t thread_set_exception_handler(exception_handler_fn newhandler,
                                      exception_handler_fn *oldhandler,
                                      void *new_stack_base, void *new_stack_top,
                                      void **old_stack_base, void **old_stack_top);

__END_DECLS

#endif
