/*
 * Australian Public Licence B (OZPLB)
 * 
 * Version 1-0
 * 
 * Copyright (c) 2004 National ICT Australia
 * 
 * All rights reserved. 
 * 
 * Developed by: Embedded, Real-time and Operating Systems Program (ERTOS)
 *               National ICT Australia
 *               http://www.ertos.nicta.com.au
 * 
 * Permission is granted by National ICT Australia, free of charge, to
 * any person obtaining a copy of this software and any associated
 * documentation files (the "Software") to deal with the Software without
 * restriction, including (without limitation) the rights to use, copy,
 * modify, adapt, merge, publish, distribute, communicate to the public,
 * sublicense, and/or sell, lend or rent out copies of the Software, and
 * to permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimers.
 * 
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimers in the documentation and/or other materials provided
 *       with the distribution.
 * 
 *     * Neither the name of National ICT Australia, nor the names of its
 *       contributors, may be used to endorse or promote products derived
 *       from this Software without specific prior written permission.
 * 
 * EXCEPT AS EXPRESSLY STATED IN THIS LICENCE AND TO THE FULL EXTENT
 * PERMITTED BY APPLICABLE LAW, THE SOFTWARE IS PROVIDED "AS-IS", AND
 * NATIONAL ICT AUSTRALIA AND ITS CONTRIBUTORS MAKE NO REPRESENTATIONS,
 * WARRANTIES OR CONDITIONS OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO ANY REPRESENTATIONS, WARRANTIES OR CONDITIONS
 * REGARDING THE CONTENTS OR ACCURACY OF THE SOFTWARE, OR OF TITLE,
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NONINFRINGEMENT,
 * THE ABSENCE OF LATENT OR OTHER DEFECTS, OR THE PRESENCE OR ABSENCE OF
 * ERRORS, WHETHER OR NOT DISCOVERABLE.
 * 
 * TO THE FULL EXTENT PERMITTED BY APPLICABLE LAW, IN NO EVENT SHALL
 * NATIONAL ICT AUSTRALIA OR ITS CONTRIBUTORS BE LIABLE ON ANY LEGAL
 * THEORY (INCLUDING, WITHOUT LIMITATION, IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHERWISE) FOR ANY CLAIM, LOSS, DAMAGES OR OTHER
 * LIABILITY, INCLUDING (WITHOUT LIMITATION) LOSS OF PRODUCTION OR
 * OPERATION TIME, LOSS, DAMAGE OR CORRUPTION OF DATA OR RECORDS; OR LOSS
 * OF ANTICIPATED SAVINGS, OPPORTUNITY, REVENUE, PROFIT OR GOODWILL, OR
 * OTHER ECONOMIC LOSS; OR ANY SPECIAL, INCIDENTAL, INDIRECT,
 * CONSEQUENTIAL, PUNITIVE OR EXEMPLARY DAMAGES, ARISING OUT OF OR IN
 * CONNECTION WITH THIS LICENCE, THE SOFTWARE OR THE USE OF OR OTHER
 * DEALINGS WITH THE SOFTWARE, EVEN IF NATIONAL ICT AUSTRALIA OR ITS
 * CONTRIBUTORS HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH CLAIM, LOSS,
 * DAMAGES OR OTHER LIABILITY.
 * 
 * If applicable legislation implies representations, warranties, or
 * conditions, or imposes obligations or liability on National ICT
 * Australia or one of its contributors in respect of the Software that
 * cannot be wholly or partly excluded, restricted or modified, the
 * liability of National ICT Australia or the contributor is limited, to
 * the full extent permitted by the applicable legislation, at its
 * option, to:
 * a.  in the case of goods, any one or more of the following:
 * i.  the replacement of the goods or the supply of equivalent goods;
 * ii.  the repair of the goods;
 * iii. the payment of the cost of replacing the goods or of acquiring
 *  equivalent goods;
 * iv.  the payment of the cost of having the goods repaired; or
 * b.  in the case of services:
 * i.  the supplying of the services again; or
 * ii.  the payment of the cost of having the services supplied again.
 * 
 * The construction, validity and performance of this licence is governed
 * by the laws in force in New South Wales, Australia.
 */

#include <arch/asm.h>

#ifdef __GNUC__
 #ifndef _SOFT_FLOAT
  #error SAVE FLOAT!!
 #endif
#else
#error FIXME
#endif

/* Argument long* buf in r3 */
BEGIN_PROC(.setjmp)
	mflr	%r10
	mfcr	%r11
	std	%r10,	 0(%r3)
	std	sp,	 8(%r3)
	std	toc,	16(%r3)
	std	%r11,	24(%r3)
	std	%r13,	32(%r3)
	std	%r14,	40(%r3)
	std	%r15,	48(%r3)
	std	%r16,	56(%r3)
	std	%r17,	64(%r3)
	std	%r18,	72(%r3)
	std	%r19,	80(%r3)
	std	%r20,	88(%r3)
	std	%r21,	96(%r3)
	std	%r22,	104(%r3)
	std	%r23,	112(%r3)
	std	%r24,	120(%r3)
	std	%r25,	128(%r3)
	std	%r26,	136(%r3)
	std	%r27,	144(%r3)
	std	%r28,	152(%r3)
	std	%r29,	160(%r3)
	std	%r30,	168(%r3)
	std	%r31,	176(%r3)
	li	%r3,	0
	blr
END_PROC(.setjmp)

/* Argument long* buf in r3, int val in r4 */
BEGIN_PROC(.longjmp)
	ld	%r10,	 0(%r3)
	ld	sp,	 8(%r3)
	ld	toc,	16(%r3)
	ld	%r11,	24(%r3)
	ld	%r13,	32(%r3)
	ld	%r14,	40(%r3)
	ld	%r15,	48(%r3)
	ld	%r16,	56(%r3)
	ld	%r17,	64(%r3)
	ld	%r18,	72(%r3)
	ld	%r19,	80(%r3)
	ld	%r20,	88(%r3)
	ld	%r21,	96(%r3)
	mtlr	%r10
	ld	%r22,	104(%r3)
	ld	%r23,	112(%r3)
	ld	%r24,	120(%r3)
	ld	%r25,	128(%r3)
	ld	%r26,	136(%r3)
	mtcrf	0x38,	%r11
	ld	%r27,	144(%r3)
	ld	%r28,	152(%r3)
	ld	%r29,	160(%r3)
	ld	%r30,	168(%r3)
	ld	%r31,	176(%r3)
	mr	%r3,	%r4
	blr
END_PROC(.longjmp)
