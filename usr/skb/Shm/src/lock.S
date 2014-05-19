/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1993-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): ___________________________________. 
 * 
 * END LICENSE BLOCK */
/**************************************************************************
 *
 *  IDENTIFICATION	lock.S
 *
 *  VERSION		$Id: lock.S,v 1.1.1.1 2006/09/23 01:56:26 snovello Exp $
 *
 *  DESCRIPTION		Processor dependent code for spin locks
 *
 **************************************************************************/

/*
 *	int mutex_lock_failed(int *plock)
 *
 *	Set the lock to non-zero and return zero or non-zero
 *	corresponding to the old value.
 */

#include "config.h"

#if !defined(SVR4) && !defined(__ELF__)
#define mutex_lock_failed _mutex_lock_failed
#endif

/*******************/
#ifdef sparc
/*******************/

/* 
 * Some SPARC implementations don't have the swap instruction
 * (it is emulated by the kernel). Therefore we use ldstub,
 * which reads a byte into a register and overwrites it with ones.
 *
 *	.global	mutex_lock_failed
 * mutex_lock_failed:
 *	add     %g0,1,%o1
 *	swap    [%o0],%o1
 *	retl
 *	xor     %o1,1,%o0
 */

	.global	mutex_lock_failed
mutex_lock_failed:
	ldstub  [%o0+3],%o1
	retl
	add   %o1,%g0,%o0

#endif /* sparc */


/*******************/
#ifdef mc68000
/*******************/

	.text
	.globl	mutex_lock_failed
mutex_lock_failed:
	tas	sp@(4)@(3)
	bnes	1f
	clrl	d0
	rts
1:	moveq	#1,d0
	rts

#endif /* mc68000 */


/*******************/
#ifdef m88k
/*******************/

        version  "03.00"
        file     "lock.S"
        text
        align   16
        global  mutex_lock_failed
        type    mutex_lock_failed,#function

mutex_lock_failed:
        or      #r8,#r0,1
        xmem    #r8,#r2,#r0
        jmp.n   #r1
        or      #r2,#r0,#r8

#endif /* m88k */


/*******************/
#ifdef i386
/*******************/

	.text
	.globl	mutex_lock_failed
mutex_lock_failed:
	movl  4(%esp),%edx
	movl  $1,%eax
	xchg  %eax,(%edx)
	ret

#endif /* i386 */


/*******************/
#ifdef mips
/*******************/

        .text
        .globl  mutex_lock_failed
mutex_lock_failed:
        ll      $2, 0($4)
        bne     $2, 0, $32
        li      $15, 1
        sc      $15, 0($4)
        beq     $15, 0, $33
        move    $2, $0
        j       $31
$33:
        li      $2, 1
$32:
        j       $31

#endif /* mips */


/*******************/
#if (defined(_PA_RISC1_0) || defined(_PA_RISC1_1))
/*******************/

; LDCWS instruction requires 16-byte alignment 

        .SPACE  $TEXT$,SORT=8
        .SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY,SORT=24
mutex_lock_failed
        .PROC
        .CALLINFO CALLER,FRAME=0,ENTRY_SR=3
        .ENTRY
        LDCWS   0(0,%r26),%r31  ;offset 0x0
        COMICLR,<>      0,%r31,%r28     ;offset 0x4
        LDI     1,%r28  ;offset 0x8
        .EXIT
        BV,N    %r0(%r2)        ;offset 0xc
        .PROCEND ;in=26;out=28;

        .SPACE  $TEXT$
        .SUBSPA $CODE$
        .SPACE  $PRIVATE$,SORT=16
        .SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31,SORT=16
        .SPACE  $TEXT$
        .SUBSPA $CODE$
        .EXPORT mutex_lock_failed,ENTRY,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
        .END

#endif /* _PA_RISC1_0 || _PA_RISC1_1 */


/*******************/
#ifdef __alpha__
/*******************/

.text
        .align 3
        .globl mutex_lock_failed
        .ent mutex_lock_failed
mutex_lock_failed:
        .frame $30,0,$26,0
        .prologue 0
	ldl_l	$0, 0($16)
	bne	$0, l1
	bis	$31, 0x1, $1
	stl_c	$1, 0($16)
	beq	$1, l1
	bis	$31, $31, $0
        mb
    	ret	$31, ($26), 1
l1:	bis	$31, 0x1, $0
    	ret	$31, ($26), 1
        .end mutex_lock_failed

#endif

/*******************/
#if 0 /* #ifdef PowerPC */
/*******************/
atomic register swap
mutex_lock_failed:
	lwarx	r5, 0, r3
	stwcx	r4, 0, r3
	bne-	mutex_lock_failed:
#endif
