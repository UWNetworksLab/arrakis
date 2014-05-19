/*
 * Copyright (c) 2009, ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ARCH__ARM_ASM_H__
#define __ARCH__ARM_ASM_H__

#define BEGIN_PROC(name) \
	.text			; \
	.align 2		; \
	.globl name		; \
	.type name, %function	; \
name:	.fnstart

#define END_PROC(name) \
	.fnend			; \
	.size name, .-name	;

#endif /* __ARCH__ARM_ASM_H__ */

