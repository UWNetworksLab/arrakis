/**
 * \file
 * \brief libc startup code.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/dispatch.h>
#include <asmoffsets.h>

	.text
	.globl  _start, _start_init

_start:
        mov 	$0, %esi

_start_generic:
        // Initialize stack for setup code
        movl 	%edi, %esp
        addl    $(OFFSETOF_DISP_PRIV_STACK_LIMIT), %esp

	// Call barrelfish_init_disabled (doesn't return)
	push	%esi
	push	%edi
	call	barrelfish_init_disabled

/* special entry point for init domain */
_start_init:
        movl	$1, %esi
        jmp 	_start_generic
