/**
 * \file
 * \brief Dispatcher entry points.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/dispatch.h>
#include <asmoffsets.h>

	.text
	.globl run_entry, pagefault_entry, disabled_pagefault_entry, trap_entry, lrpc_entry

run_entry:
        // Get dispatcher stack to rsp
        movq    %rdi, %rsp
        addq    $(OFFSETOF_DISP_PRIV_STACK_LIMIT), %rsp
	callq	disp_run	// Handle activation in C

//
// void pagefault_entry(vaddr_t fault_addr, uintptr_t error, vaddr_t ip)
//
pagefault_entry:
	// Get dispatcher stack
        movq    %rdi, %rsp
        addq    $(OFFSETOF_DISP_PRIV_STACK_LIMIT), %rsp
	callq	disp_pagefault	// handle activation in C

//
// void disabled_pagefault_entry(vaddr_t fault_addr, uintptr_t error, vaddr_t ip)
//
disabled_pagefault_entry:
	// Get trap stack
        movq    %rdi, %rsp
        addq    $(OFFSETOF_DISP_PRIV_TRAP_STACK_LIMIT), %rsp
	callq	disp_pagefault_disabled	// handle activation in C

//
// void trap_entry(uintptr_t irq, uintptr_t error, vaddr_t ip)
//
trap_entry:
	// Get trap stack
        movq    %rdi, %rsp
        addq    $(OFFSETOF_DISP_PRIV_TRAP_STACK_LIMIT), %rsp
	callq	disp_trap	// handle activation in C

lrpc_entry:
        /* register state:
         * rdi = epoffset
         * esi = bufpos of reserved space in endpoint
         * rdx, r10, r8, r9 = message body
         * rax = dispatcher
         */

        /* shuffle registers to match calling parameters of disp_lrpc */
        mov     %r10, %rcx

        // Load dispatcher to rsp
        movq    %rax, %rsp

        // Compute address of endpoint
        addq    %rsp, %rdi
        subq    $OFFSETOF_LMP_ENDPOINT_KERNPART, %rdi

        // Load dispatcher stack to rsp
        addq    $(OFFSETOF_DISP_PRIV_STACK_LIMIT), %rsp

        // Push dispatcher pointer to top of stack (7th function argument)
        pushq   %rax

        callq   disp_lrpc /* disp_lrpc(ep, bufpos, w1, w2, w3, w4, handle) */
