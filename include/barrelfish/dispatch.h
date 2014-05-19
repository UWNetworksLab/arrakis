/**
 * \file
 * \brief The API to lib/barrelfish/dispatch
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_DISPATCH_H
#define LIBBARRELFISH_DISPATCH_H

/// Stack size for dispatcher, in words
#define DISPATCHER_STACK_BYTES  6400     /* ~6kB */
#define DISPATCHER_STACK_WORDS  (DISPATCHER_STACK_BYTES / sizeof(uintptr_t))

#ifndef __ASSEMBLER__

#include <sys/cdefs.h>
#include <barrelfish_kpi/dispatcher_handle.h>

__BEGIN_DECLS

#ifdef  NDEBUG
#define assert_disabled(e)      ((void)sizeof(e))
#define warn_disabled(v,e)      ((void)sizeof(e))
#else

// Convert preprocessor macro to string (as recommended by CPP info manual)
#define __xstr(s)       __str(s)
#define __str(s)        #s

#define assert_disabled(e) \
    ((e) ? (void)0 : disp_assert_fail(#e, __FILE__, __func__, __xstr(__LINE__)))
#define warn_disabled(v,e)						\
    (((*(v))||(e)) ? (void)0 : (((*(v))=true),disp_warn_fail(#e, __FILE__, __func__, __xstr(__LINE__))))
#endif

void disp_init_disabled(dispatcher_handle_t handle);
int disp_init_onthread(void);

dispatcher_handle_t disp_disable(void);
void disp_enable(dispatcher_handle_t handle);


void disp_arch_init(dispatcher_handle_t handle);

/**
 * \brief Resume execution of a given register state
 *
 * This function resumes the execution of the given register state on the
 * current dispatcher. It may only be called while the dispatcher is disabled.
 *
 * \param disp Current dispatcher pointer
 * \param regs Register state snapshot
 */
void disp_resume(dispatcher_handle_t handle, arch_registers_state_t *archregs);

/**
 * \brief Switch execution between two register states, and turn off
 * disabled activations.
 *
 * This function saves as much as necessary of the current register state
 * (which, when resumed will return to the caller), and switches execution
 * by resuming the given register state.  It may only be called while the
 * dispatcher is disabled.  A side effect is that activations are reenabled.
 * Note that the thread context saved is a voluntary save so only callee
 * save registers need to be saved, but we dont currently provide any
 * way to optimise the corresponding resume.
 *
 * \param disp Current dispatcher pointer
 * \param from_regs Location to save current register state
 * \param to_regs Location from which to resume new register state
 */
void disp_switch(dispatcher_handle_t handle, arch_registers_state_t *from_state,
                 arch_registers_state_t *to_state);

/**
 * \brief Save the current register state and optionally yield the CPU
 *
 * This function saves as much as necessary of the current register state
 * (which, when resumed will return to the caller), and then either
 * re-enters the thread scheduler or yields the CPU.
 * It may only be called while the dispatcher is disabled.
 * Note that the thread context saved is a voluntary save so only callee
 * save registers need to be saved, but we dont currently provide any
 * way to optimise the corresponding resume.
 *
 * \param disp Current dispatcher pointer
 * \param regs Location to save current register state
 * \param yield If true, yield CPU to kernel; otherwise re-run thread scheduler
 * \param yield_to Endpoint capability for dispatcher to which we want to yield
 */
void disp_save(dispatcher_handle_t handle, arch_registers_state_t *state,
               bool yield, capaddr_t yield_to);


void __attribute__((noreturn)) disp_yield_disabled(dispatcher_handle_t handle);
dispatcher_handle_t disp_new(int core_id);

const char *disp_name(void);
uint64_t disp_run_counter(void);

void disp_assert_fail(const char *exp, const char *file, const char *func,
                      const char *line) __attribute((noreturn));

void disp_warn_fail(const char *exp, const char *file, const char *func,
		    const char *line);

__END_DECLS

#endif //__ASSEMBLER__

#endif
