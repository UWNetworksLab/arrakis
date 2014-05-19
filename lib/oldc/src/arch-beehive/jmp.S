/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <regconvention.h>

	.globl	_setjmp
	.globl	_longjmp

/* These are currently organised so that a stop cleanly cannot
 * stop in the middle of save/restore, by keeping the queues
 * nonempty.
 */

_setjmp:
	// 17 addresses
	aqw_add	a1, a1, 0
	
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	aqw_add a1, a1, 4
	// 17 stores
	ld	wq, s1
	ld	wq, s2
	ld	wq, s3
	ld	wq, s4
	
	ld	wq, s5
	ld	wq, s6
	ld	wq, s7
	ld	wq, s8
	
	ld	wq, s9
	ld	wq, s10
	ld	wq, s11
	ld	wq, s12
	
	ld	wq, s13
	ld	wq, s14
	ld	wq, fp
	ld	wq, sp
	
	ld	wq, link
	ld	r1, 0
	j	link

lj0:	ld	a2, 1
_longjmp:
	// return value
	ld	r1, a2
	jz	lj0

	// 17 addresses
	aqr_add	a1, a1, 0
	
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4

	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	aqr_add a1, a1, 4
	// 17 loads
	ld	s1, rq
	ld	s2, rq
	ld	s3, rq
	ld	s4, rq
	
	ld	s5, rq
	ld	s6, rq
	ld	s7, rq
	ld	s8, rq
	
	ld	s9, rq
	ld	s10, rq
	ld	s11, rq
	ld	s12, rq
	
	ld	s13, rq
	ld	s14, rq
	ld	fp, rq
	ld	sp, rq

	j	rq
