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
	.globl run_entry, pagefault_entry, disabled_pagefault_entry, trap_entry

run_entry:
        // Initialize stack for setup code
        movl 	%edi, %esp
        addl    $(OFFSETOF_DISP_PRIV_STACK_LIMIT), %esp
	push	%edi // disp ptr
	call 	disp_run	// Handle activation in C

//
// void pagefault_entry(disp ptr, vaddr_t fault_addr, uintptr_t error, vaddr_t ip)
//
pagefault_entry:
	// Get dispatcher stack
        movl 	%edi, %esp
        addl    $(OFFSETOF_DISP_PRIV_STACK_LIMIT), %esp
	push	%edx
	push	%ecx
	push	%eax
        push    %edi
	call	disp_pagefault	// handle activation in C

//
// void disabled_pagefault_entry(disp ptr, vaddr_t fault_addr, uintptr_t error, vaddr_t ip)
//
disabled_pagefault_entry:
	// Get trap stack
        movl 	%edi, %esp
        addl    $(OFFSETOF_DISP_PRIV_TRAP_STACK_LIMIT), %esp
	push	%edx
	push	%ecx
	push	%eax
        push    %edi
	call	disp_pagefault_disabled	// handle activation in C

//
// void trap_entry(disp ptr, uintptr_t irq, uintptr_t error, vaddr_t ip)
//
trap_entry:
	// Get trap stack
        movl    %edi, %esp
        addl    $(OFFSETOF_DISP_PRIV_TRAP_STACK_LIMIT), %esp
	push	%edx
	push	%ecx
	push	%eax
        push    %edi
	call	disp_trap	// handle activation in C
