/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__
#endif // __ASSEMBLER__

#include <asmoffsets.h> // OFFSETOF etc.
#include <barrelfish_kpi/registers_arch.h> // CPSR_REG etc.
#include <barrelfish_kpi/flags_arch.h> // CPSR_IF_MASK etc.
#include <exceptions.h>

#define EXCEPTION_MODE_STACK_BYTES       256
#define SYSTEM_MODE_STACK_BYTES         8192

        .arm
        .text

        .globl exceptions_init
        .globl pabt_handler
        //
        // Exception mode stacks
        //
        // These are small stacks used to figure out where to
	// spill registers. As these are banked functions are
        // expected to leave them as found (ie. so they do not
        // need to be reset next time around).
        //
        .align 4

abt_stack:
        .space EXCEPTION_MODE_STACK_BYTES, 0
abt_stack_top:
        .space 4, 0

irq_stack:
        .space EXCEPTION_MODE_STACK_BYTES, 0
irq_stack_top:
         .space 4, 0

undef_stack:
        .space EXCEPTION_MODE_STACK_BYTES, 0
undef_stack_top:
        .space 4, 0

svc_stack:
        .space EXCEPTION_MODE_STACK_BYTES, 0
svc_stack_top:
        .space 4, 0

        //
        // System mode stack
        //
        // This is used for calls to C routines.
        //
        // Users of this stack are expected to set sp_sys to this value
        // before calling C routines.
        //
sys_stack:
        .space SYSTEM_MODE_STACK_BYTES, 0
sys_stack_top:

irq_save_area:
        .space (NUM_REGS * 4), 0

got_for_sys_mode:
        .word   0

        //
        // void exceptions_init(void)
        //
        // Assumes running in System Mode.
        //
exceptions_init:
        ldr     r0, null_handler
        ldr     r1, =ETABLE_ADDR
        mov     r2, #7
$exceptions_init_do_one:
        str     r0, [r1], #4
        subs    r2, r2, #1
        bpl     $exceptions_init_do_one
$exceptions_init_en_high:
        mrc     p15, 0, r0, c1, c0, 0
        orr     r0, r0, #0x2000
        mcr     p15, 0, r0, c1, c0, 0
$exceptions_load_stacks:
        stmfd   sp!, {lr}
        mov     r0, #ARM_MODE_ABT
        ldr     r1, = abt_stack_top
        bl      set_stack_for_mode
        mov     r0, #ARM_MODE_IRQ
        ldr     r1, = irq_stack_top
        bl      set_stack_for_mode
        mov     r0, # ARM_MODE_UND
        ldr     r1, = undef_stack_top
        bl      set_stack_for_mode
        mov     r0, # ARM_MODE_SVC
        ldr     r1, = svc_stack_top
        bl      set_stack_for_mode
$expections_install_handlers:
        mov     r0, # ARM_EVECTOR_PABT
        adr     r1, pabt_handler
        bl      exceptions_install_handler
        mov     r0, # ARM_EVECTOR_DABT
        adr     r1, dabt_handler
        bl      exceptions_install_handler
        mov     r0, # ARM_EVECTOR_IRQ
        adr     r1, irq_handler
        bl      exceptions_install_handler
        mov     r0, # ARM_EVECTOR_UNDEF
        adr     r1, undef_handler
        bl      exceptions_install_handler
        mov     r0, # ARM_EVECTOR_SWI
        adr     r1, swi_handler
        bl      exceptions_install_handler
$exceptions_save_got:
        ldr     r0, = got_for_sys_mode
        str     PIC_REGISTER, [r0]
$exceptions_init_done:
        ldmfd   sp!, {pc}

        //
        // void set_stack_for_mode(uint8_t cpu_mode, uintptr_t stack)
        //
set_stack_for_mode:
        mrs     r3, cpsr                // Save current mode
        and     r0, r0, # ARM_MODE_MASK
        bic     r2, r3, # ARM_MODE_MASK
        orr     r2, r2, r0
        msr     cpsr_c, r2              // Switch to cpu_mode
        mov     sp, r1
        msr     cpsr_c, r3              // Switch back
        bx      lr

        //
        // void exceptions_install_handler(int vector_offset,
        //                                 void (*handler)(void)
        //
exceptions_install_handler:
        ldr     r2, = ETABLE_ADDR
        add     r2, r2, r0
        sub     r1, r1, r2
        subs    r1, r1, #8
        lsr     r1, r1, #2
        ldr     r0, [r2]
        mov     r3, #0xff000000
        and     r0, r0, r3
        bic     r1, r1, r3
        orr     r0, r0, r1
        str     r0, [r2]
        bx      lr

        //
        // void null_handler(void)
        //
null_handler:
        b       .

        //
        // Macro definition to get pointer to arch specific dispatcher
        //
        // Logical equivalent of C function with same name
        //
.macro get_dispatcher_shared_arm out
        ldr \out, = dcb_current // out = address of pointer to dcb_current
        ldr \out, [\out]        // out = dcb_current
        ldr \out, [\out, #OFFSETOF_DCB_DISP] //now ptr to dispatcher_shared_arm
.endm

        //
        // Macro to determine if dispatcher is disabled.
        //
        // pc and disp arguments are unmodified.
        // out contains result
        //
.macro disp_is_disabled disp, pc, out
        // disp->disabled || (disp->crit_pc_lo <= pc && pc < disp->crit_pc_hi)
        ldrb    \out, [\disp, #OFFSETOF_DISP_DISABLED]
        cmp     \out, #1
        bhs     0f                      // disp->disabled >= 0      | disabled
                                        // disp->disabled = false
        ldr     \out, [\disp, #OFFSETOF_DISP_CRIT_PC_LOW]
        cmp     \out, \pc
        movhi   \out, #0
        bhi     0f                      // disp->crit_pc_low > pc   | enabled
        ldr     \out, [\disp, #OFFSETOF_DISP_CRIT_PC_HIGH]
        cmp     \pc, \out
        movhs   \out, #0                // pc >= disp->crit_pc_high | enabled
        movlo   \out, #1                // pc <  disp->crit_pc_high | disable
0:
.endm

        //
        // Macro to spill registers
        //
        // Assumptions:
        //      - context is in scratch registers set {r0-r3}.
        //      - spsr is also in scratch register set.
        //      - stack holds spilled scratch registers.
        //      - lr contains pc for context
        // Side-effects:
        //      - spills context
        //      - pops scratch registers off stack (sp -> sp + 16).
        //      - trashes spsr_reg
        //
.macro save_context context, spsr_reg
        .if     CPSR_REG <> 0
        .err    "Invariant failure: CPSR offset != 0"
        .endif
        .if     PC_REG <> 16
        .err    "Invariant failure: PC offset != 16"
        .endif
        str     \spsr_reg, [\context, #(CPSR_REG * 4)]
        str     lr, [\context, #(PC_REG * 4)]
        add     \spsr_reg, \context, #(LR_REG * 4)
        stmda   \spsr_reg, {r4-r14}^
        add     \spsr_reg, \context, #(R3_REG * 4)
        pop     {r4-r7}                         // Pop spilled scratch registers
        stmda   \spsr_reg!, {r4-r7}             // And Save them
.endm

        //
        // Macro to initialize system mode stack
        //
.macro init_sys_stack
        ldr sp, =sys_stack_top
.endm

        //
        // Macro to initialize SVC pic register
        //
.macro init_sys_pic_register
        ldr PIC_REGISTER, = got_for_sys_mode
        ldr PIC_REGISTER, [PIC_REGISTER]
.endm

        //
        // Macro to enter SYS mode with interrupts disabled.
        // Set up stack and GOT pointer.
        //
.macro enter_sys scratch
        mov \scratch, #(CPSR_IF_MASK | ARM_MODE_SYS)
        msr cpsr_c, \scratch
        init_sys_pic_register
        init_sys_stack
.endm

        //
        // void pabt_handler(void)
        //
        // Entered in ABT mode, IRQ disabled, ARM state.
        //
pabt_handler:
        stmfd   sp!, {r0-r3}                    // Save for scratch use
        sub     lr, lr, #4                      // lr = faulting pc
        mrs     r3, spsr                        // r3 = spsr until save_context
        ands    r1, r3, #ARM_MODE_PRIV
        bne     $pabt_kernel
$pabt_user:
        get_dispatcher_shared_arm r2
        mov     r0, lr                          // r0 = faulting pc
        disp_is_disabled r2, r0, r1             // r1 = 1 if disabled, else 0
        cmp     r1, #0
        addeq   r1, r2, #OFFSETOF_DISP_ENABLED_AREA
        addne   r1, r2, #OFFSETOF_DISP_TRAP_AREA
        save_context r1, r3                     // r1 = save area
        enter_sys r3
        b       handle_user_page_fault          // f(fault_addr, save_area)
$pabt_kernel:
        // {r0-r3} spilled to stack
        sub     r2, sp, #(NUM_REGS * 4)         // Reserve stack space for save
        save_context r2, r3                     // r2 = save_area
        mov     r1, lr                          // r1 = faulting pc        
        mov     r0, #ARM_EVECTOR_PABT
        enter_sys r3
        b       fatal_kernel_fault              // f(evector, addr, save_area)

        //
        // void dabt_handler(void)
        //
        // Entered in ABT mode, IRQ disabled, ARM state.
        //
dabt_handler:
        stmfd   sp!, {r0-r3}                    // Save for scratch use
        sub     lr, lr, #8                      // lr = faulting instruction
        mrs     r3, spsr                        // r3 = spsr until save_context
        ands    r1, r3, #ARM_MODE_PRIV
        bne     $dabt_kernel
$dabt_user:
        get_dispatcher_shared_arm r2
        mov     r0, lr                          // r0 = faulting pc
        disp_is_disabled r2, r0, r1             // r1 = disp_is_disabled
        cmp     r1, #0
        addeq   r1, r2, #OFFSETOF_DISP_ENABLED_AREA
        addne   r1, r2, #OFFSETOF_DISP_TRAP_AREA
        save_context    r1, r3                  // r1 = save_area
        mrc     p15, 0, r0, c6, c0, 0           // r0 = fault address
        enter_sys r3
        b       handle_user_page_fault          // f(fault_addr, save_area)
$dabt_kernel:
        // {r0-r3} spilled to stack
        sub     r2, sp, #(NUM_REGS * 4)         // Reserve stack space for save
        save_context r2, r3                     // r2 = save_area
        mrc     p15, 0, r1, c6, c0, 0           // r1 = fault address
        mov     r0, #ARM_EVECTOR_DABT
        enter_sys r3
        b       fatal_kernel_fault              // f(evector, addr, save_area)

        //
        // void undef_handler(void)
        //
        // Entered in UNDEF mode, IRQ disabled, ARM state.
        //
        // NB Identical to PABT except for final jump in undef_user and
        // code doesn't adjust lr to point to faulting instruction since
        // it was undefined and there's no point re-executing it.
        //
undef_handler:
        stmfd   sp!, {r0-r3}                    // Save for scratch use
        mrs     r3, spsr                        // r3 = spsr until save_context
        ands    r1, r3, #ARM_MODE_PRIV
        bne     $undef_kernel
$undef_user:
        get_dispatcher_shared_arm r2
        sub     r0, lr, #4                      // r0 = faulting pc
        disp_is_disabled r2, r0, r1             // r1 = 1 if disabled, else 0
        cmp     r1, #0
        addeq   r1, r2, #OFFSETOF_DISP_ENABLED_AREA
        addne   r1, r2, #OFFSETOF_DISP_TRAP_AREA
        save_context r1, r3                     // r1 = save area
        enter_sys r3
        b       handle_user_undef               // f(fault_addr, save_area)
$undef_kernel:
        sub     r2, sp, #(NUM_REGS * 4)         // Save to stack
        save_context r2, r3                     // r2 = saved context
        sub     r1, lr, #4                      // r1 = fault address
        mov     r0, #ARM_EVECTOR_UNDEF
        enter_sys r3
        bl      fatal_kernel_fault              // f(evector, addr, save_area)

        //
        // void irq_handler(void)
        //
        // Entered in IRQ mode, IRQ disabled, ARM state
        //
irq_handler:
        stmfd   sp!, {r0-r3}                    // Save for scratch use
        sub     lr, lr, #4                      // lr = return address
        mrs     r3, spsr                        // r0 = spsr until save_context
        ands    r1, r3, #ARM_MODE_PRIV
        bne     $irq_kernel
$irq_user:
        get_dispatcher_shared_arm r2
        mov     r1, lr
        disp_is_disabled r2, r1, r0             // r0 = 1 if disabled, else 0
        cmp     r0, #0
        addeq   r0, r2, #OFFSETOF_DISP_ENABLED_AREA
        addne   r0, r2, #OFFSETOF_DISP_DISABLED_AREA
        save_context    r0, r3                  // r0 = save area
        enter_sys       r3
        mov     lr, #0
        mov     r11, #0
        b       handle_irq                      // f(save_area, fault_pc)
$irq_kernel:
        // CPU was in System mode.
        ldr     r0, =irq_save_area
        save_context    r0, r3                  // r0 = saved context
        enter_sys       r3
        b       handle_irq                      // f(save_area)

        //
        // void swi_handler(void)
        //
        // Entered in SVC mode, IRQ disabled, ARM state.
        //
        // r0 = encoded syscall ordinal
        // r1 = syscall arg0
        // r2 = syscall arg1
        // ...
        // r7 = syscall arg6
        //
        // For now the system saves the caller's context here, because
        // some fraction of system calls do not return directly.
        //
swi_handler:
        .if SYSCALL_REG <> 0
        .error "Syscall entry broken. Expected ordinal reg to be r0."
        .endif

        stmfd   sp!, {r0-r3}                    // Save for scratch use
        mrs     r3, spsr                        // r3 = spsr until save_context
        ands    r1, r3, #ARM_MODE_PRIV
        bne     $swi_kernel
$swi_user:
        get_dispatcher_shared_arm r2
        disp_is_disabled r2, lr, r1             // r1 = 1 if disabled, else 0
        cmp     r1, #0
        addeq   r0, r2, #OFFSETOF_DISP_ENABLED_AREA
        addne   r0, r2, #OFFSETOF_DISP_DISABLED_AREA
        save_context r0, r3                     // r0 = save area, r3 = scratch
        enter_sys r3
        ldr     r11, [r0, #48]                  // context->fp
        ldr     lr, [r0, #60]                   // context->lr
        b       sys_syscall

$swi_kernel:
        b       sys_syscall_kernel
