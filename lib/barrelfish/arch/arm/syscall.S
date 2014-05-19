/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __thumb2__
//normal arm instructions
        .arm
        .text
        .globl syscall

syscall:
        // Save pointer to return structure (r0), callee-save
	// registers (r4-r10,r12) that are cloberred.
        mov     r12, sp
        push    {r0, r4-r10, r11, r12, lr, pc}
        ldmia   r12, {r4-r10, r12}
        ldr     r0, [sp, #80]
        ldr     lr, =swi_done
        swi     #0
swi_done:
	// This is where we end up executing after the system call?
	// This accesses the stack, which was restored in do_resume
        pop     {r3,r4-r10} // pop 7 registers
        // r0,r1 contain return values, r3 points to return structure
        str     r0, [r3, #0]
        str     r1, [r3, #4]
        pop {r11}

        pop {r3} // Using r3 to tempararily hold the value of sp register
                 // Warning: Assuming that clobbering r3 is OK!!
        pop {lr} // Return address where we have to return
        mov sp, r3  // Restoring sp from r3 now.
        mov pc, lr // return by loading pc
	nop
	nop
	nop
	nop
	
	
#else   //use only the thumb2 instruction set
        .syntax unified
        .text
        .globl syscall
syscall:
    // Save pointer to return structure (r0), callee-save
	// registers (r4-r10,r12) that are cloberred.
        mov     r12, sp
        //pc can not be in the list, but it is also not restored -> ignore
        push    {r0, r4-r10, r11, r12, lr}
        ldr     r0,  [sp, #76]   
        ldmia   r12, {r4-r10, r12}
        ldr     lr, =swi_done
        svc     #0         
swi_done:
	// This is where we end up executing after the system call?
	// This accesses the stack, which was restored in do_resume
        pop     {r3,r4-r10} // pop 7 registers
        // r0,r1 contain return values, r3 points to return structure
        str     r0, [r3, #0]
        str     r1, [r3, #4]
        pop {r11}

        pop {r3} // Using r3 to temporarily hold the value of sp register
                 // Warning: Assuming that clobbering r3 is OK!
        pop {lr} // Return address where we have to return
        mov sp, r3  // Restoring sp from r3 now.
        bx  lr      // return by loading pc
#endif
