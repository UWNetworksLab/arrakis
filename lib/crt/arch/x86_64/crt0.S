/**
 * \file
 * \brief libc startup code.
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
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
        mov     $0, %rsi

_start_generic: // Assumption: rdi = dispatcher
        // Initialize stack for setup code
        movq    %rdi, %rsp
        addq    $(OFFSETOF_DISP_PRIV_STACK_LIMIT), %rsp

	// Call barrelfish_init_disabled (doesn't return)
	jmp	barrelfish_init_disabled

/* special entry point for init domain */
_start_init:
        mov     $1, %rsi
        jmp     _start_generic
