/**
 * \file
 * \brief ARMv7-M upcalls
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
#include <barrelfish_kpi/registers_pushed_arm_v7m.h>//for autopushed register access

static arch_registers_state_t upcall_state;//XXX: is this even used??

extern uint32_t ctr;

/*
 * On armv7-m, the registers r0-r3,r12,lr,pc,xpsr are popped from the stack
 * whenever an exception returns -> we copy those values onto the thread mode stack after
 * restoring the others
 */
static inline __attribute__((noreturn))
void do_resume(uint32_t *regs)
{
    STATIC_ASSERT(CPSR_REG ==  0, "wrong register layout");
    STATIC_ASSERT(R0_REG   ==  1, "wrong register layout");
    STATIC_ASSERT(PC_REG   ==  16, "wrong register layout");
    STATIC_ASSERT(XPSR_REG_PUSHED ==  7, "wrong register layout");
    STATIC_ASSERT(R0_REG_PUSHED   ==  0, "wrong register layout");
    STATIC_ASSERT(PC_REG_PUSHED   ==  6, "wrong register layout");
  

    
    if (((arch_registers_state_t*) regs)->named.stack == 0){
        printf("uninitialized stack pointer -> point to irq_save_pushed_area_top\n");
        ((arch_registers_state_t*) regs)->named.stack = (uint32_t) irq_save_pushed_area_top;
        printf("new stack pointer: 0x%x\n", ((arch_registers_state_t*) regs)->named.stack);
    }
    
    //this is probably the ONLY time we do not have to adjust a future pc - we have to 
    //adjust the XPSR instead...
    //the EPSR part of the XPSR is normally ignored (mrs reads it as 0, msr is ignored),
    //but when restoring the XPSR on modeswitch, it is actually read.
    //This means we have to always ensure the Thumb-bit is set
    if ((((arch_registers_state_t*) regs)->named.cpsr & 0x01000000) == 0){
        printf("EPSR thumb bit not set -> fixed\n");
        ((arch_registers_state_t*) regs)->named.cpsr |= 0x01000000;
        printf("new XPSR register: 0x%x\n", ((arch_registers_state_t*) regs)->named.cpsr);
        //should only actually happen when we first execute a new process
    }
    

    __asm volatile(
        "mov    r0, %[regs]                         \n\t"  //address where the regs are
        "ldr    r1, [r0, #56]                       \n\t"  //stored stack pointer
        "sub    r1, #32                             \n\t"  //allocate stack space for 8 registers
        //copy the 8 expected registers
        //XXX: could probably be shortened by using ldm, but only for the first 4 registers
        "ldr    r2, [r0,#(" XTR(R0_REG) "*4)]       \n\t"  //copy r0 entry, using r2 as temp
        "str    r2, [r1,#(" XTR(R0_REG_PUSHED) "*4)]\n\t"
        "ldr    r2, [r0,#(" XTR(R1_REG) "*4)]       \n\t"  //copy r1 entry, using r2 as temp
        "str    r2, [r1,#(" XTR(R1_REG_PUSHED) "*4)]\n\t"
        "ldr    r2, [r0,#(" XTR(R2_REG) "*4)]       \n\t"  //copy r2 entry, using r2 as temp
        "str    r2, [r1,#(" XTR(R2_REG_PUSHED) "*4)]\n\t"
        "ldr    r2, [r0,#(" XTR(R3_REG) "*4)]       \n\t"  //copy r3 entry, using r2 as temp
        "str    r2, [r1,#(" XTR(R3_REG_PUSHED) "*4)]\n\t"
        "ldr    r2, [r0,#(" XTR(R12_REG) "*4)]      \n\t"  //copy r12 entry, using r2 as temp
        "str    r2, [r1,#(" XTR(R12_REG_PUSHED)"*4)]\n\t"
        "ldr    r2, [r0,#(" XTR(LR_REG) "*4)]       \n\t"  //copy lr entry, using r2 as temp
        "str    r2, [r1,#(" XTR(LR_REG_PUSHED) "*4)]\n\t"
        "ldr    r2, [r0,#(" XTR(PC_REG) "*4)]       \n\t"  //copy pc entry, using r2 as temp
        "str    r2, [r1,#(" XTR(PC_REG_PUSHED) "*4)]\n\t"
        "ldr    r2, [r0]                            \n\t"  //copy xpsr entry, using r2 as temp
        "str    r2, [r1,#28]                        \n\t"
        //set thread stack pointer
        "msr    PSP, r1                             \n\t"  //set thread stack pointer to saved context
        //restore unpushed registers: r4-r11
        "add    r0, #(" XTR(R4_REG) "*4)            \n\t"  //point to r4 entry
        "ldmia  r0, {r4-r11}                        \n\t"  //restore r4 - r11
        "ldr    lr, =#0xFFFFFFFD                    \n\t"  //special return address to change modes
        "bx     lr                                  \n\t"  //actual context switch
    :: [regs] "r" (regs) : "lr");

    panic("do_resume returned.");
}

/// Ensure context is for user-mode with interrupts enabled.
static inline void
ensure_user_mode_policy(arch_registers_state_t *state)
{
    //this should be a no-op on armv7-m:
    //user mode is ensured by the return address used in do_resume
    //exeptions can be tail-chained instead of preemting each other, meaning we never
    //have to completely disable them
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
/*XXX: WARNING: the way this is currently implemented, we will probably never wake up
 *  we are already in handler mode, and the interrupt we are waiting for does not have
 *  a higher priority, so it will only be taken when we exit -- which is never (I think)
 *  (was not able to test it with IPIs, but we do stop servicing timer interrupts)
 *
 *  Solution: in this function, first increase the priority of IPIs to the maximum
 *  write a separate handler for IPIs, that first checks where it came from.
 *      if it came from handler mode, it must have interrupted this wait 
 *          -> set priority back down again and then handle the interrupt normally 
 *              (no context save necessary)
 *      if it came from thread mode, treat it as any other interrupt (context save etc.)
 *
 *  always setting the the priority back down will ensure we never interrupt the kernel
 *  while it is doing actual work
 */
    __asm volatile(
        "0:                                             \n\t"
        "wfi                                            \n\t"
        "b      0b                                      \n\t" :::);

    panic("wfi returned");
}
