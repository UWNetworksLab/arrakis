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
#include <barrelfish_kpi/registers_pushed_arm_v7m.h> // PC_REG_PUSHED etc.
#include <barrelfish_kpi/flags_arch.h> // CPSR_IF_MASK etc.
#include <exceptions.h>

//#define EXCEPTION_MODE_STACK_BYTES       256
#define HANDLER_MODE_STACK_BYTES         8192

#define VECTORTABLE_BYTES       320 //16 system interrupts + 64 external interrupts, 4 bytes each


        .syntax unified
        .text

        .globl exceptions_init
        .globl exceptions_early_init
        .globl vectortable
        .globl irq_save_pushed_area_top
        //
        .balign 4096 //so we can treat this as just a page when setting up the MMU  
        //XXX: "balign" seems to be GNU-specific. 
        //other arm assemblers use the corresponding power of two as their argument instead
        
        
vectortable:    //where the interrupt handlers will be looked up
        .space VECTORTABLE_BYTES, 0
        //XXX: maybe we rather want the table in .data instead of .text, but then 
        //we will have to make sure both the page with the table AND the page with the 
        //handlers will be accessible (mapped in cache MMU) 

        //
        // Handler mode stack
        //
        // used whenever we transfer to handler mode (priviledged) from an unpriviledged mode,
        // in which case we can assume that it is safe to overwrite all of it
        // DO NOT USE THIS WHEN CAME FROM HANDLER MODE IN THE FIRST PLACE
        //  all exception handlers that are allowed to interrupt others need to check first
        //  if they interrupted another handler, in which case they are not allowed to set their
        //  own stack!
handler_stack:
        .space HANDLER_MODE_STACK_BYTES, 0
handler_stack_top:

irq_save_area:
        .space (NUM_REGS * 4), 0
        
irq_save_pushed_area:       //used when we switch to a process with uninitialized stack
        .space (NUM_REGS_PUSHED * 4), 0
irq_save_pushed_area_top:

got_for_handler_mode:
        .word   0




        // void exceptions_early_init(void)
        //
        // Assumes running in priviledged thread Mode.
        // sets up the special handler used to cleanly transition into handler mode
        // DOES NOT RETURN, instead triggers the special handler
        //
exceptions_early_init:
        //use SVcall, because this is always enabled and can be triggered by a single instruction
        //XXX: using the fact that whatever vector table we are currently using, 
        //it will be mapped to address 0
        mov		r2, #0			            // base of vectortable, where the handler will be stored
        mov     r0, # ARM_7M_EVECTOR_SVCALL
        adr     r1, initialization_handler
		add	    r2, r2, r0 , LSL #2         // vectortable + 4*vector_index
		orr     r1, #1                      //lsb of handler must be 1 to indicate thumb mode
		str		r1, [r2]                    //write handler address into table
        //change thread mode stack pointer (the only banked register)
        mrs     r1, MSP
        msr     PSP, r1     //copy main stack pointer to thread stack pointer
        mov     r1, #2
        msr     control, r1 //set threadmode stackpointer to PSP
        //trigger the special exception -> will never return
        svc     #0
        
        
        //special handler that may only be triggered once -> used so we are in handler mode
        //gives control over to initialization code in armv7-m/init.c        
initialization_handler:
        mov     r1, #1
        msr     control, r1 //set threadmode unpriviledged
        ldr r4, =text_init_continued
        bx  r4
        
        
        //
        // void exceptions_init(void)
        //
        // Assumes running in handler Mode.
        //
exceptions_init:
		push	{lr}
		push	{r0-r4}
        ldr     r0, null_handler
        ldr     r1, =vectortable
$exceptions_install_handlers:
        mov     r0, # ARM_7M_EVECTOR_MEM
        ldr     r1, =pagefault_handler
        bl      exceptions_install_handler
        mov     r0, # ARM_7M_EVECTOR_BUS
        ldr     r1, =pagefault_handler
        bl      exceptions_install_handler
        mov     r0, # ARM_7M_EVECTOR_SYSTICK
        ldr     r1, =generic_irq_handler
        bl      exceptions_install_handler
        mov     r0, # ARM_7M_EVECTOR_USAGE
        ldr     r1, =usagefault_handler              
        bl      exceptions_install_handler
        mov     r0, # ARM_7M_EVECTOR_SVCALL
        ldr     r1, =swi_handler
        bl      exceptions_install_handler
        mov     r0, # ARM_7M_EVECTOR_HARDFAULT
        ldr     r1, =generic_irq_handler
        bl      exceptions_install_handler
        mov     r0, # ARM_7M_EVECTOR_NMI
        ldr     r1, =generic_irq_handler
        bl      exceptions_install_handler
$exceptions_save_got:
        ldr     r0, = got_for_handler_mode
        mov     r1, PIC_REGISTER
        str     r1, [r0]
$exceptions_init_done:
        //ldmfd   sp!, {pc}
        pop		{r0-r4}
        pop		{pc}


		//
		//void exceptions_install_handler(int vector_index, void (*handler)(void)
		// writes entry into vectortable (and nothing else)
		//
exceptions_install_handler:
		ldr		r2, =vectortable			// base of vectortable, where the handler will be stored
		add	    r2, r2, r0 , LSL #2         // vectortable + 4*vector_index
		orr     r1, #1                      //lsb of handler must be 1 to indicate thumb mode
		str		r1, [r2]                    //write handler address into table
		bx		lr
        //
        // void null_handler(void)
        //
null_handler:
        b       .


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
        IT      hs
        bhs     0f                      // disp->disabled >= 0      | disabled
                                        // disp->disabled = false
        ldr     \out, [\disp, #OFFSETOF_DISP_CRIT_PC_LOW]
        cmp     \out, \pc
        ITt     hi
        movhi   \out, #0
        bhi     0f                      // disp->crit_pc_low > pc   | enabled
        ldr     \out, [\disp, #OFFSETOF_DISP_CRIT_PC_HIGH]
        
        cmp     \pc, \out
        ITe     hs
        movhs   \out, #0                // pc >= disp->crit_pc_high | enabled
        movlo   \out, #1                // pc <  disp->crit_pc_high | disable
0:
.endm


        //
        // Macro to initialize handler mode stack
        //
.macro init_handler_stack
        ldr sp, =handler_stack_top
.endm

        //
        // Macro to initialize handler pic register
        //
.macro init_handler_pic_register
        ldr PIC_REGISTER, = got_for_handler_mode
        ldr PIC_REGISTER, [PIC_REGISTER]
.endm


        //
        // Macro to save all registers (as they were at the moment of interrupt)
        //  into context
        // pops from threadmode stack (PSP -> PSP - 8*4)
        // only temp_regs will be clobbered
        //
.macro save_context context, temp_reg1, temp_reg2
        .if     CPSR_REG <> 0
        .err    "Invariant failure: CPSR offset != 0"
        .endif
        .if     PC_REG <> 16
        .err    "Invariant failure: PC offset != 16"
        .endif
        mrs     \temp_reg1, PSP     //thread mode stack
        ldr     \temp_reg2, [\temp_reg1, #(R0_REG_PUSHED *4)]            //read pushed r0
        str     \temp_reg2, [\context, #(R0_REG*4)]               //copy to context.r0    
        ldr     \temp_reg2, [\temp_reg1, #(R1_REG_PUSHED *4)]            //read pushed r1
        str     \temp_reg2, [\context, #(R1_REG *4)]             //copy to context.r1    
        ldr     \temp_reg2, [\temp_reg1, #(R2_REG_PUSHED *4)]            //read pushed r2
        str     \temp_reg2, [\context, #(R2_REG *4)]             //copy to context.r2
        ldr     \temp_reg2, [\temp_reg1, #(R3_REG_PUSHED *4)]            //read pushed r3
        str     \temp_reg2, [\context, #(R3_REG *4)]             //copy to context.r3
        ldr     \temp_reg2, [\temp_reg1, #(R12_REG_PUSHED *4)]           //read pushed r12
        str     \temp_reg2, [\context, #(R12_REG * 4)]           //copy to context.r12
        ldr     \temp_reg2, [\temp_reg1, #(LR_REG_PUSHED *4)]            //read pushed lr
        str     \temp_reg2, [\context, #(LR_REG * 4)]            //copy to context.lr
        ldr     \temp_reg2, [\temp_reg1, #(PC_REG_PUSHED *4)]            //read pushed pc
        str     \temp_reg2, [\context, #(PC_REG * 4)]            //copy to context.pc
        ldr     \temp_reg2, [\temp_reg1, #(XPSR_REG_PUSHED *4)]          //read pushed xpsr
        str     \temp_reg2, [\context, #(CPSR_REG * 4)]          //copy to context.xpsr
        add     \temp_reg1, #(NUM_REGS_PUSHED*4)                 //we read the pushed registers -> restore stack
        str     \temp_reg1, [\context, #(SP_REG * 4)]           //copy to context.sp
        //store rest of registers in context
        add     \temp_reg1, \context, #(R4_REG * 4)     //point to first unpushed register
        stmia   \temp_reg1, {r4-r11}                   //store rest
.endm



/*
 * void generic_irq_handler(void)
 *
 * handler for pretty much any IRQ we get, except system calls (as those take arguments),
 * and traps (or rather, what the dispatcher would see as traps -- pagefaults, usage faults)
 * checks if we interrupted userspace or kernelspace,
 * reads out IRQ number,
 * calls a handler that does the case distinction in C
 */
generic_irq_handler:
    and     r1, lr, #8          // r1 is now 0 if we come from handler mode
    cbnz    r1, generic_irq_handler_user //compares without changing condition flags
    
generic_irq_handler_kernel: //no "$" because that bloated the layout, reducing the branch range
    mov     r2, sp          //place where pushed registers start
    ldr     r1, [sp, #(PC_REG_PUSHED * 4)]   //this should be where the pushed pc ends up
    ldr     r3, =fatal_kernel_fault
    bx      r3
generic_irq_handler_user:
    init_handler_stack
    get_dispatcher_shared_arm r2
    mrs     r0, PSP
    ldr     r0, [r0, #(PC_REG_PUSHED*4)]    // r0 = faulting pc
    disp_is_disabled r2, r0, r1             // r1 = 1 if disabled, else 0
    cmp     r1, #0
    ITe     eq    
    addeq   r1, r2, #OFFSETOF_DISP_ENABLED_AREA
    addne   r1, r2, #OFFSETOF_DISP_DISABLED_AREA
    save_context r1, r3, r2                     // r1 = save area
    init_handler_pic_register     //only set after saving context
    mrs     r0, IPSR            //what IRQ are we currently handling
    ldr     r3, =handle_irq
    bx      r3

/*
 * void pagefault_handler(void)
 *
 * handler just for pagefaults (which in this case are bus faults or mem faults)
 */
pagefault_handler:
    and     r1, lr, #8          // r1 is now 0 if we come from handler mode
    cbnz    r1, pagefault_handler_user //compares without changing condition flags
    
pagefault_handler_kernel:
    mov     r2, sp          //place where pushed registers start
    ldr     r1, [sp, #(PC_REG_PUSHED * 4)]   //this should be where the pushed pc ends up
    ldr     r3, =fatal_kernel_fault
    bx      r3
pagefault_handler_user:
    init_handler_stack
    get_dispatcher_shared_arm r2
    mrs     r0, PSP
    ldr     r0, [r0, #(PC_REG_PUSHED*4)]    // r0 = faulting pc
    disp_is_disabled r2, r0, r1             // r1 = 1 if disabled, else 0
    cmp     r1, #0
    ITe     eq    
    addeq   r1, r2, #OFFSETOF_DISP_ENABLED_AREA
    addne   r1, r2, #OFFSETOF_DISP_TRAP_AREA
    save_context r1, r3, r2                     // r1 = save area
    mov     r0, r1                 //save area must be first argument
    init_handler_pic_register     //only set after saving context
    ldr     r3, =handle_user_page_fault
    bx      r3

/*
 * void usagefault_handler(void)
 *
 * handler just for usagefaults, which can be:
 *      undefined instruction
 *      unaligned access
 *      division by 0
 */
usagefault_handler:
    and     r1, lr, #8          // r1 is now 0 if we come from handler mode
    cbnz    r1, usagefault_handler_user //compares without changing condition flags
    
usagefault_handler_kernel:
    mov     r2, sp          //place where pushed registers start
    ldr     r1, [sp, #(PC_REG_PUSHED * 4)]   //this should be where the pushed pc ends up
    ldr     r3, =fatal_kernel_fault
    bx      r3
usagefault_handler_user:
    init_handler_stack
    get_dispatcher_shared_arm r2
    mrs     r0, PSP
    ldr     r0, [r0, #(PC_REG_PUSHED*4)]    // r0 = faulting pc
    disp_is_disabled r2, r0, r1             // r1 = 1 if disabled, else 0
    cmp     r1, #0
    ITe     eq    
    addeq   r1, r2, #OFFSETOF_DISP_ENABLED_AREA
    addne   r1, r2, #OFFSETOF_DISP_TRAP_AREA
    save_context r1, r3, r2                     // r1 = save area
    mov     r0, r1                 //save area must be first argument
    init_handler_pic_register     //only set after saving context
    ldr     r3, =handle_user_undef
    bx      r3 
    
        //
        // void swi_handler(void)
        //
        // handles all software interrupts (SVCALL)
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
    
//registers r0-r3,r12 are available for scratch use, since they are already on PSP stack
    and     r1, lr, #8          // r1 is now 0 if we come from handler mode
    cbnz    r1, swi_user //compares without changing condition flags
swi_kernel:
    ldr     r3, =sys_syscall_kernel
    bx      r3
swi_user:   //much like generic_irq_handler, but the save area is in r0
    init_handler_stack
    get_dispatcher_shared_arm r2
    mrs     r0, PSP
    ldr     r0, [r0, #(PC_REG_PUSHED*4)]    // r0 = faulting pc
    disp_is_disabled r2, r0, r1             // r1 = 1 if disabled, else 0
    cmp     r1, #0
    ITe     eq    
    addeq   r1, r2, #OFFSETOF_DISP_ENABLED_AREA
    addne   r1, r2, #OFFSETOF_DISP_DISABLED_AREA
    save_context r1, r3, r2                     // r1 = save area
    mov     r0, r1                              //sys_syscall expects the context to be in r0
    init_handler_pic_register     //only set this after saving context!
    ldr     r3, =sys_syscall
    bx      r3

