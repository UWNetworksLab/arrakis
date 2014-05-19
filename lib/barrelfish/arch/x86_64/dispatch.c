/**
 * \file
 * \brief Dispatcher architecture-specific implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/syscalls.h>
#include "threads_priv.h"
#include <arch/ldt.h>

/* entry points defined in assembler code */
extern void run_entry(void);
extern void pagefault_entry(void);
extern void disabled_pagefault_entry(void);
extern void trap_entry(void);
extern void lrpc_entry(void);

void __attribute__ ((visibility ("hidden"))) disp_resume_end(void);

/**
 * \brief Architecture-specific dispatcher initialisation
 */
void disp_arch_init(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_64 *disp =
        get_dispatcher_shared_x86_64(handle);

    /* Set entry points */
    disp->d.dispatcher_run = (lvaddr_t)run_entry;
    disp->d.dispatcher_lrpc = (lvaddr_t)lrpc_entry;
    disp->d.dispatcher_pagefault = (lvaddr_t)pagefault_entry;
    disp->d.dispatcher_pagefault_disabled = (lvaddr_t)disabled_pagefault_entry;
    disp->d.dispatcher_trap = (lvaddr_t)trap_entry;

    disp->crit_pc_low = (lvaddr_t)disp_resume;
    disp->crit_pc_high = (lvaddr_t)disp_resume_end;

    /* Setup LDT */
    ldt_init_disabled(handle);
}

/**
 * \brief Resume execution of a given register state
 *
 * This function resumes the execution of the given register state on the
 * current dispatcher. It may only be called while the dispatcher is disabled.
 *
 * \param disp Current dispatcher pointer
 * \param regs Register state snapshot
 */
void disp_resume(dispatcher_handle_t handle, arch_registers_state_t *archregs)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);

    assert_disabled(disp->disabled);
    assert_disabled(disp->haswork);

    /* sanity check user state */
    struct registers_x86_64 *regs = archregs;
    assert_disabled(regs->rip > BASE_PAGE_SIZE);
    assert_disabled((regs->eflags & USER_EFLAGS) == USER_EFLAGS); // flags

#ifdef CONFIG_DEBUG_DEADLOCKS
    ((struct disp_priv *)disp)->yieldcount = 0;
#endif

    // Re-enable dispatcher
    disp->disabled = false; // doesn't take effect while we're in disp_resume()

    // Resume program execution
    __asm volatile ("mov        %[fs], %%ax             \n\t"
                    "mov        %%ax, %%fs              \n\t"
                    "mov        %[gs], %%ax             \n\t"
                    "mov        %%ax, %%gs              \n\t"
                    "movq        0*8(%[regs]), %%rax    \n\t"
                    "movq        2*8(%[regs]), %%rcx    \n\t"
                    "movq        3*8(%[regs]), %%rdx    \n\t"
                    "movq        4*8(%[regs]), %%rsi    \n\t"
                    "movq        5*8(%[regs]), %%rdi    \n\t"
                    "movq        6*8(%[regs]), %%rbp    \n\t"
                    "movq        8*8(%[regs]), %%r8     \n\t"
                    "movq        9*8(%[regs]), %%r9     \n\t"
                    "movq       10*8(%[regs]), %%r10    \n\t"
                    "movq       11*8(%[regs]), %%r11    \n\t"
                    "movq       12*8(%[regs]), %%r12    \n\t"
                    "movq       13*8(%[regs]), %%r13    \n\t"
                    "movq       14*8(%[regs]), %%r14    \n\t"
                    "movq       15*8(%[regs]), %%r15    \n\t"
                    "pushq      %[ss]                   \n\t"   // SS
                    "pushq       7*8(%[regs])           \n\t"   // RSP
                    "pushq      17*8(%[regs])           \n\t"   // RFLAGS
                    "pushq      %[cs]                   \n\t"   // CS
                    "pushq      16*8(%[regs])           \n\t"   // RIP
                    "movq        1*8(%[regs]), %%rbx    \n\t"   // RBX was base register
                    "iretq                              \n\t"
                    : /* No output */
                    :
                    [regs] "b" (regs),
                    [ss] "i" (USER_SS),
                    [cs] "i" (USER_CS),
                    [fs] "m" (regs->fs),
                    [gs] "m" (regs->gs)
                    );

    __asm volatile ("disp_resume_end:");
}

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
// XXX: Needs to be compiled with -O2, otherwise we use too many
// registers. Have to think about how to circumvent this without needing
// -O2.
void
#if defined(__GNUC__) && !defined(__clang__) && !defined(__ICC)
__attribute__((optimize(2)))
#endif
disp_switch(dispatcher_handle_t handle, arch_registers_state_t *from_state,
            arch_registers_state_t *to_state)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    assert_disabled(disp->disabled);
    assert_disabled(disp->haswork);

    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);

    struct registers_x86_64 *from_regs = (struct registers_x86_64*)from_state;
    struct registers_x86_64 *to_regs   = (struct registers_x86_64*)to_state;
    assert_disabled(to_regs != NULL);

    // Save resume IP, stack and control registers, ...
    // then switch stacks to dispatcher, and call resume to switch context
    // Note the embedded call to disp_resume above.
    /*
     * NB: we shouldn't have to save RBP here, rather just list it as clobbered
     * However, GCC without optimisations uses RBP as a frame pointer and won't
     * let us do that, so instead we manually save and restore it. This is
     * usually redundant (without optimisation, GCC saves and restores it; with
     * optimisation the register is used and thus GCC saves it anyway).
     */
    __asm volatile ("movq       %%rbp,  6*8(%[regs])    \n\t"
                    "movq       %%rsp,  7*8(%[regs])    \n\t"
                    "lea        switch_resume(%%rip), %%rcx\n\t"
                    "movq       %%rcx, 16*8(%[regs])    \n\t"   // RIP
                    "pushfq                             \n\t"
                    "popq       17*8(%[regs])           \n\t"   // RFLAGS
                    "mov        %%fs, %%bx              \n\t"
                    "mov        %%bx, %[fs]             \n\t"
                    "mov        %%gs, %%bx              \n\t"
                    "mov        %%bx, %[gs]             \n\t"
                    "movq       %[stack], %%rsp         \n\t"   // Switch stack
                    "callq      disp_resume             \n\t"
                    :
                    : [regs] "a" (from_regs),
                      [fs] "m" (from_regs->fs),
                      [gs] "m" (from_regs->gs),
                      [stack] "d" ((lvaddr_t)&disp_gen->stack[DISPATCHER_STACK_WORDS]),
                      [disp] "D" (disp),
                      [to_regs] "S" (to_regs)
                    : "rbx", "rcx", "rsp",
                      "r8", "r9", "r10", "r12", "r13", "r14", "r15"
                    );

    __asm volatile ("switch_resume:");
}

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
// XXX: Needs to be compiled with -O2, otherwise we use too many
// registers. Have to think about how to circumvent this without needing
// -O2.
void
#if defined(__GNUC__) && !defined(__clang__) && !defined(__ICC)
__attribute__((optimize(2)))
#endif
disp_save(dispatcher_handle_t handle, arch_registers_state_t *state,
          bool yield, capaddr_t yield_to)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    assert_disabled(disp->disabled);

    struct registers_x86_64 *regs = state;

    // Save resume IP, stack and control registers
    // See disp_switch above for details
    // XXX: Using the clobber list here to make the compiler save only
    // used registers. Be very careful when changing the code below
    // this asm block! If registers in the clobber list are
    // subsequently used, they won't be restored at save_resume.
    __asm volatile ("movq       %%rbp,  6*8(%[regs])    \n\t"
                    "movq       %%rsp,  7*8(%[regs])    \n\t"
                    "lea        save_resume(%%rip), %%rcx\n\t"
                    "movq       %%rcx, 16*8(%[regs])    \n\t"   // RIP
                    "pushfq                             \n\t"
                    "popq       17*8(%[regs])           \n\t"   // RFLAGS
                    "mov        %%fs, %%bx              \n\t"
                    "mov        %%bx, %[fs]             \n\t"
                    "mov        %%gs, %%bx              \n\t"
                    "mov        %%bx, %[gs]             \n\t"
                    :
                    : [regs] "a" (regs),
                      [fs] "m" (regs->fs),
                      [gs] "m" (regs->gs)
                    : "rbx", "rcx", "rdx", "rsi", "rdi",
                      "r8", "r9", "r10", "r12", "r13", "r14", "r15"
                    );

    if (yield) {
//        trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_SYS_YIELD, 1);

        sys_yield(yield_to);
        // may fail if target doesn't exist; if so, just fall through
    }
    // this code won't run if the yield succeeded

    // enter thread scheduler again
    // this doesn't return, and will call disp_yield if there's nothing to do
    thread_run_disabled(handle);

    __asm volatile ("save_resume:");
}
