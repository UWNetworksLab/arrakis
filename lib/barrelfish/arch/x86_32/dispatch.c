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

/* entry points defined in assembler code */
extern void run_entry(void);
extern void pagefault_entry(void);
extern void disabled_pagefault_entry(void);
extern void trap_entry(void);

void __attribute__ ((visibility ("hidden"))) disp_resume_end(void);

/**
 * \brief Architecture-specific dispatcher initialisation
 */
void disp_arch_init(dispatcher_handle_t handle)
{
    struct dispatcher_shared_x86_32 *disp =
        get_dispatcher_shared_x86_32(handle);
    struct dispatcher_x86_32 *disp_priv = get_dispatcher_x86_32(handle);

    disp->d.dispatcher_run = (lvaddr_t)run_entry;
    disp->d.dispatcher_pagefault = (lvaddr_t)pagefault_entry;
    disp->d.dispatcher_pagefault_disabled = (lvaddr_t)disabled_pagefault_entry;
    disp->d.dispatcher_trap = (lvaddr_t)trap_entry;

    disp->crit_pc_low = (lvaddr_t)disp_resume;
    disp->crit_pc_high = (lvaddr_t)disp_resume_end;

    // Setup GS register
    // XXX: Set this to the kernel-provided GDT dispatcher entry, until we have
    // LDT setup code for x86-32
    disp_priv->disp_seg_selector = 0x33;

    /* load this segment to GS */
    __asm volatile("mov %%ax, %%gs"
                   : /* No outputs */
                   : "a" (disp_priv->disp_seg_selector));
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
    struct registers_x86_32* regs = archregs;
    assert_disabled(regs->eip > BASE_PAGE_SIZE);
    assert_disabled((regs->eflags & USER_EFLAGS) == USER_EFLAGS); // flags
    assert_disabled(regs->cs == USER_CS);
    assert_disabled(regs->ss == USER_SS);

#ifdef CONFIG_DEBUG_DEADLOCKS
    ((struct disp_priv *)disp)->yieldcount = 0;
#endif

    // Re-enable dispatcher
    disp->disabled = 0; // doesn't take effect while we're in disp_resume()

    // Resume program execution
    __asm volatile ("mov        %[fs], %%fs             \n\t"
                    "mov        %[gs], %%gs             \n\t"
                    "movl        1*4(%[regs]), %%ebx    \n\t"
                    "movl        2*4(%[regs]), %%ecx    \n\t"
                    "movl        3*4(%[regs]), %%edx    \n\t"
                    "movl        4*4(%[regs]), %%esi    \n\t"
                    "movl        5*4(%[regs]), %%edi    \n\t"
                    "movl        6*4(%[regs]), %%ebp    \n\t"
                    // XXX: This is now done on the thread's stack
                    // Dunno if there's a better way in protected mode
                    // XXX: Assuming SS is always the same
                    "mov        11*4(%[regs]), %%ss     \n\t"
                    "movl        7*4(%[regs]), %%esp    \n\t"
                    "pushl       9*4(%[regs])           \n\t"   // EFLAGS
                    "pushl      10*4(%[regs])           \n\t"   // CS
                    "pushl       8*4(%[regs])           \n\t"   // EIP
                    "movl        0*4(%[regs]), %%eax    \n\t"   // EAX was base register
                    "iretl                              \n\t"
                    : /* No output */
                    :
                    [regs] "a" (regs),
                    [fs] "m" (regs->fs),
                    [gs] "m" (regs->gs)
                    );

    __asm volatile ("disp_resume_end:");
}

/**
 * \brief Switch execution between two register states
 *
 * This function saves as much as necessary of the current register state
 * (which, when resumed will return to the caller), and switches execution
 * by resuming the given register state.  It may only be called while the
 * dispatcher is disabled.
 *
 * \param disp Current dispatcher pointer
 * \param from_regs Location to save current register state
 * \param to_regs Location from which to resume new register state
 */
void disp_switch(dispatcher_handle_t handle, arch_registers_state_t *from_state,
                 arch_registers_state_t *to_state)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    assert_disabled(disp->disabled);
    assert_disabled(disp->haswork);

    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);

    struct registers_x86_32 *from_regs = (struct registers_x86_32*)from_state;
    struct registers_x86_32 *to_regs   = (struct registers_x86_32*)to_state;
    assert_disabled(to_regs != NULL);

    // Save resume IP, stack and control registers, ...
    // then switch stacks to dispatcher, and call resume to switch context
    /*
     * NB: we shouldn't have to save RBP here, rather just list it as clobbered
     * However, GCC without optimisations uses RBP as a frame pointer and won't
     * let us do that, so instead we manually save and restore it. This is
     * usually redundant (without optimisation, GCC saves and restores it; with
     * optimisation the register is used and thus GCC saves it anyway).
     */
    __asm volatile ("movl       %%ebp,  6*4(%[regs])    \n\t"
                    "movl       %%ebx,  1*4(%[regs])    \n\t"
                    "movl       %%esp,  7*4(%[regs])    \n\t"
                    // XXX: This is not PIC! - Need to fix
                    "lea        switch_resume, %%ecx    \n\t"
                    "movl       %%ecx,  8*4(%[regs])    \n\t"   // EIP
                    "pushfl                             \n\t"
                    "popl       9*4(%[regs])            \n\t"   // EFLAGS
                    "movl       %%cs, %%ecx             \n\t"
                    "movl       %%ecx, 10*4(%[regs])    \n\t"   // CS
                    "movl       %%ss, %%ecx             \n\t"
                    "movl       %%ecx, 11*4(%[regs])    \n\t"   // SS
                    "mov        %%fs, %[fs]             \n\t"
                    "mov        %%gs, %[gs]             \n\t"
                    "movl       %[stack], %%esp         \n\t"   // Switch stack
                    // Call disp_resume on new stack
                    "movl       %[to_regs], %%eax       \n\t"
                    "pushl      %%eax                   \n\t"
                    "movl       %[disp], %%eax          \n\t"
                    "pushl      %%eax                   \n\t"
                    "calll      disp_resume             \n\t"
                    :
                    : [regs] "a" (from_regs),
                      [fs] "m" (from_regs->fs),
                      [gs] "m" (from_regs->gs),
                      [stack] "d" ((lvaddr_t)&disp_gen->stack[DISPATCHER_STACK_WORDS]),
                      [disp] "m" (disp),
                      [to_regs] "m" (to_regs)
                    : "ecx", "esp", "esi", "edi"
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
 *
 * \param disp Current dispatcher pointer
 * \param regs Location to save current register state
 * \param yield If true, yield CPU to kernel; otherwise re-run thread scheduler
 * \param yield_to Endpoint capability for dispatcher to which we want to yield
 */
void disp_save(dispatcher_handle_t handle, arch_registers_state_t *state,
               bool yield, capaddr_t yield_to)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    assert_disabled(disp->disabled);

    struct registers_x86_32 *regs = state;

    // Save resume IP, stack and control registers
    // See disp_switch above for details
    // XXX: Using the clobber list here to make the compiler save only
    // used registers. Be very careful when changing the code below
    // this asm block! If registers in the clobber list are
    // subsequently used, they won't be restored at save_resume.
    __asm volatile ("movl       %%ebp,  6*4(%[regs])    \n\t"
                    "movl       %%esp,  7*4(%[regs])    \n\t"
                    // XXX: This is not PIC! - Need to fix
                    "lea        save_resume, %%ecx      \n\t"
                    "movl       %%ecx,  8*4(%[regs])    \n\t"   // EIP
                    "pushfl                             \n\t"
                    "popl       9*4(%[regs])            \n\t"   // EFLAGS
                    "movl       %%cs, %%ecx             \n\t"
                    "movl       %%ecx, 10*4(%[regs])    \n\t"   // CS
                    "movl       %%ss, %%ecx             \n\t"
                    "movl       %%ecx, 11*4(%[regs])    \n\t"   // SS
                    "mov        %%fs, %[fs]             \n\t"
                    "mov        %%gs, %[gs]             \n\t"
                    :
                    : [regs] "a" (regs),
                      [fs] "m" (regs->fs),
                      [gs] "m" (regs->gs)
                    : "ecx", "edx", "esi", "edi"
                    );

    if (yield) {
        sys_yield(yield_to);
        // may fail if target doesn't exist; if so, just fall through
    }
    // this code won't run if the yield succeeded

    // enter thread scheduler again
    // this doesn't return, and will call disp_yield if there's nothing to do
    thread_run_disabled(handle);

    __asm volatile ("save_resume:");
}
