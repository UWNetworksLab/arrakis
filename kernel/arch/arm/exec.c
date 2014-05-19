/**
 * \file
 * \brief ARM execution and miscellany
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <init.h>
#include <arm.h>
#include <arm_hal.h>
#include <exec.h>
#include <exceptions.h>
#include <misc.h>
#include <cp15.h>   // for invalidating tlb and cache

static arch_registers_state_t upcall_state;

extern uint32_t ctr;
static inline __attribute__((noreturn))
void do_resume(uint32_t *regs)
{
    STATIC_ASSERT(CPSR_REG ==  0, "");
    STATIC_ASSERT(R0_REG   ==  1, "");
    STATIC_ASSERT(PC_REG   == 16, "");

    // Flush cashes and tlb
    cp15_invalidate_tlb();
    cp15_invalidate_i_and_d_caches();

    __asm volatile(
        // lr = r14, used as tmp register.
        // Load cpsr into lr and move regs to next entry (postindex op)
        // LDR = read word from memory
        //        target register
        //        /   use register containing "regs" as base register
        //       /   /           post index: only base register is used for
        //      /   /           /     addressing and the offset added afterwards
        "ldr    lr, [%[regs]], #4                       \n\t"
        // set spsr_fc to value of lr == regs.cpsr
        // restore cpsr
        //        bits indicating spsr
        //       /         read from register lr
        //      /         /
        "msr    spsr_fc, lr                             \n\t"
        // Restore register r0 to r15,"^" means: cpsr := spsr
        // This is deprecated as LR and PC are both included in this command
        // see ARMv7 TRM A8.6.53
        //               will increment the base pointer
        //              /
        /* "mov lr, %[regs]                                \n\t" */
        /* "ldmia  lr!, {r0-r12}^                          \n\t" */
        /* // Restore stack pointer */
        /* "ldmia  lr!, {r13}                              \n\t" */
        /* // Restore LR and PC */
        /* "ldmia  lr!, {r14-r15}                          \n\t" */
        "ldmia  %[regs], {r0-r15}^                          \n\t"
        // Make sure pipeline is clear
        "nop                          \n\t"
        "nop                          \n\t"
        "nop                          \n\t"
        "nop                          \n\t"
        "nop                          \n\t"
        "nop                          \n\t"
        :: [regs] "r" (regs) : "lr");

    panic("do_resume returned.");
}

/// Ensure context is for user-mode with interrupts enabled.
static inline void
ensure_user_mode_policy(arch_registers_state_t *state)
{
    uintptr_t cpsr_if_mode = CPSR_F_MASK | ARM_MODE_USR;

    if ((state->named.cpsr & (CPSR_IF_MASK | ARM_MODE_MASK)) != cpsr_if_mode) {
        assert(0 == (state->named.cpsr & ARM_MODE_PRIV));
        state->named.cpsr &= CPSR_IF_MASK | ARM_MODE_MASK;
        state->named.cpsr |= cpsr_if_mode;
    }
}

/**
 * \brief Go to user-space at entry point 'entry'.
 *
 * This function goes to user-space and starts executing the program at
 * its entry point at virtual address 'entry'.
 *
 * \param entry Entry point address of program to execute.
 */
void __attribute__ ((noreturn))
execute(lvaddr_t entry)
{
    dispatcher_handle_t handle = dcb_current->disp;
    struct dispatcher_shared_arm *disp_arm = get_dispatcher_shared_arm(handle);

    arch_registers_state_t *state = &upcall_state;
    assert(0 != disp_arm->got_base);

    state->named.r10 = disp_arm->got_base;

    struct dispatcher_shared_generic *disp_gen
        = get_dispatcher_shared_generic(handle);

    state->named.rtls = disp_gen->udisp;

    state->named.pc = entry;
    ensure_user_mode_policy(state);
    do_resume(state->regs);
}

/**
 * \brief Resume the given user-space snapshot.
 *
 * This function resumes user-space execution by restoring the CPU
 * registers with the ones given in the array, pointed to by 'state'.
 */
uint32_t ctr=0;
void __attribute__ ((noreturn)) resume(arch_registers_state_t *state)
{
    ctr++;
    state->named.rtls = arch_get_thread_register();
    ensure_user_mode_policy(state);

    /*
      This function succeeds the first time executed, i.e.
      when init is started for the first time.
      If we hold the execution here after the first execption, we are still good
    */
    //    while(ctr>1);
    do_resume(state->regs);
}

void wait_for_interrupt(void)
{
    // REVIEW: Timer interrupt could be masked here.

    // Switch to system mode with interrupts enabled. -- OLD
    // Switch to priviledged mode with interrupts enabled.
    __asm volatile(
#if defined(__ARM_ARCH_5__)
            //XXX: qemu 0.14 chokes on ARM_MODE_PRIV?! -SG
        "mov    r0, #" XTR(ARM_MODE_SYS) "              \n\t"
#else
        "mov    r0, #" XTR(ARM_MODE_PRIV) "              \n\t"
#endif
        "msr    cpsr_c, r0                              \n\t"
        "0:                                             \n\t"
#if defined(__ARM_ARCH_6K__)
        "wfe                                            \n\t"
#elif defined(__ARM_ARCH_5TEJ__)
        "mcr    p15, 0, r0, c7, c10, 4                  \n\t"
#elif defined(__ARM_ARCH_5TE__)
	// XXX: Need to change for Netronome?
        "mcr    p15, 0, r0, c7, c10, 4                  \n\t"
#elif defined(__ARM_ARCH_7A__)
        "wfi                  \n\t"
#else
          // If no WFI functionality exists on system, just
          // spinning here is okay.
#error "Unknown platform for wait_for_interrupt"
#endif //
        "b      0b                                      \n\t" ::: "r0");

    panic("wfi returned");
}
