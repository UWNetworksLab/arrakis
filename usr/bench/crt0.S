/**
 * \file
 * \brief bench startup code.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/// User-space default stack size is 32K
#define STACK_SIZE	(32 * 1024)

	.text
	.globl  _start

_start:
	// Initialize stack for program
	lea	(stack + STACK_SIZE)(%rip), %rsp

	// Call main() function
	movq    $0, %rsi
	movq    $0, %rdi
	movq    $0, %rax
	callq   main

	// In case main() returns we loop endlessly
halt:
	jmp     halt

	.bss
	.comm   stack, STACK_SIZE
