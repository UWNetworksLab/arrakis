/**
 * \file
 * \brief Cache control routines for ARMv7.
 */
/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__   1
#endif

        .text
        .arm

        .globl cp15_invalidate_d_cache,\
               cp15_invalidate_i_and_d_caches,\
               cp15_invalidate_i_and_d_caches_fast, \
	       cp15_invalidate_tlb_fn, \
	       cp15_enable_mmu, \
           cp15_enable_alignment

/* Based on algorithm from ARM Architecture Reference Manual */
cp15_invalidate_d_cache:
	stmdb	sp!, {r4, r5, r6, r7, r8, r9, r10, r11}

	mrc	p15, 1, r0, c0, c0, 1 // Read CLIDR
	ands	r3, r0, #0x7000000
	mov 	r3, r3, lsr #23		// Cache level value (naturally aligned)
	beq	Finished
	mov 	r10, #0

Loop1:	add 	r2, r10, r10, lsr #1    // Work out 3xcachelevel
        mov	r1, r0, lsr r2          // bottom 3 bits: cache tp for this lvl
        and	r1, r1, #7              // get those 3 bits alone
        cmp 	r1, #2
        blt	Skip                    // no cache or only i-cache at this lvl
        mcr 	p15, 2, r10, c0, c0, 0  // write Cache Size selection register
        isb                          	// ISB to sync change to CacheSizeID reg
        mrc 	p15, 1, r1, c0, c0, 0 	// reads current Cache Size ID register
        and	r2, r1, #7              // extract line length field
        add	r2, r2, #4              // +4 for line len off. (log2 16 bytes)
        ldr	r4, =0x3ff
        ands	r4, r4, r1, lsr #3      // R4 is max num. on way size (rt algnd)
        clz	r5, r4                  // R5 is bit pos. of way size increment
        ldr	r7, =0x00007fff
        ands	r7, r7, r1, lsr #13     // R7 is max num. of indx sz (rt algnd)
Loop2:	mov	r9, r4                  // R9 working cpy of max way sz (rt al.)
Loop3: 	orr 	r11, r10, r9, lsl r5    // factor way num., cache num. into R11
        orr 	r11, r11, r7, lsl r2    // factor in index number
        mcr 	p15, 0, r11, c7, c10, 2 // clean by set/way
        subs 	r9, r9, #1             	// decrement way number
        bge	Loop3
        subs 	r7, r7, #1             	// decrement index
        bge 	Loop2
Skip:	add 	r10, r10, #2            // increment cache number
        cmp 	r3, r10
        bgt 	Loop1
Finished:
	dsb
	ldmia	sp!, {r4, r5, r6, r7, r8, r9, r10, r11}
	bx	lr

cp15_invalidate_i_and_d_caches:
	stmdb	sp!, {lr}
	bl 	cp15_invalidate_d_cache
	mcr	p15, 0, r0, c7, c5, 0    // Inval. all I caches to PoU (ICIALLU)
	dsb
	isb
	ldmia	sp!, {lr}
	bx	lr




/**********************************************************************/
/* Based on algorithm from ARM Architecture Reference Manual errata comments

Reference: ARM Architecture Refrence Manual ARMv7-A
Section B2.2.4 Cache maintenance functionality
        (ARMv7 cache maintenance operation)
        page 1251:
    This version is supposed to be faster than cp15_invalidate_d_cache version
    (as per ARM architecture ref manual)
 */

cp15_invalidate_d_cache_fast:
	stmdb   sp!, {r4, r5, r6, r7, r8, r9, r10, r11}

	mrc p15, 1, r0, c0, c0, 1   // Read CLIDR into r0
	ands R3, r0, #0x7000000
	mov r3, r3, lsr #23         // Cache level value (naturally aligned)
	beq Finished11
	mov r10, #0

Loop11:
	add r2, r10, r10, lsr #1    // Work out 3 x cachelevel
	mov r1, r0, lsr r2          // bottom 3 bits are cache tp for this level
	and r1, r1, #7              // get those 3 bits alone
	cmp r1, #2
	blt Skip11                  // no cache or only i- cache at this level
	mcr p15, 2, r10, c0, c0, 0  // write CSSELR from R10
	isb                         // ISB to sync change to CCSIDR
	mrc p15, 1, r1, c0, c0, 0   // read current CCSIDR to R1
	and r2, r1, #7              // extract line length field
	add r2, r2, #4              // add 4 for line len offset (log2 16 bytes)
	ldr r4, =0x3ff
	ands r4, r4, r1, lsr #3     // R4 is max num. on way sz (right aligned)
	clz r5, r4                  // R5 is bit position of way size increment
	mov r9, r4                  // R9 working cpy of max way size (rt algn.)

Loop12:
	ldr r7, =0x00007fff
	ands r7, r7, r1, lsr #13    // R7 is max num. of index sz (rt aligned) 

Loop13:
	orr r11, r10, r9, lsl r5    // factor way num. and cache number into R11
	orr r11, r11, r7, lsl r2    // factor in index number
	mcr p15, 0, r11, c7, c10, 2 // DCCSW, clean by set/way
	subs r7, r7, #1             // decrement index
	bge Loop13
	subs r9, r9, #1             // decrement way number
	bge Loop12

Skip11:
	add r10, r10, #2            // increment cache number
	cmp r3, r10
	bgt Loop11
	dsb
	
Finished11:
	dsb
	ldmia   sp!, {r4, r5, r6, r7, r8, r9, r10, r11}
	bx	lr

cp15_invalidate_i_and_d_caches_fast:
	stmdb   sp!, {lr}
	bl cp15_invalidate_d_cache_fast
	mcr	p15, 0, r0, c7, c5, 0 // Inval. all i-caches to PoU (ICIALLU)
	dsb
	isb
	ldmia   sp!, {lr}
	bx	lr

/**********************************************************************/
/* TLBFlush Based on code from ARM Architecture Reference Manual

Reference: ARM Architecture Refrence Manual ARMv7-A
Section B3.12.34 CP15 c8, TLB maintenance operations
        page 1415:
 */
cp15_invalidate_tlb:
	stmdb	sp!, {r4, r5, r6, r7, r8, r9, r10, r11} // not needed!
        // invalidate all unlocked entries in instruction TLB
        mcr p15, 0, r0, c8, c5, 0 // ITLBIALL, invalidate instruction TLB
        mcr p15, 0, r0, c8, c6, 0 // DTLBIALL, invalidate data TLB
        mcr p15, 0, r0, c8, c7, 0 // TLBIALL, invalidate unified TLB
        nop         // Note: to avoid issues with pipelined instructions
        nop
        nop
        nop
        nop
        nop
        nop
        nop

	dsb
	ldmia	sp!, {r4, r5, r6, r7, r8, r9, r10, r11} // not needed!
	bx	lr

cp15_invalidate_tlb_fn:
	stmdb	sp!, {lr}
	bl cp15_invalidate_tlb
	dsb
	isb
	ldmia	sp!, {lr}
	bx	lr

cp15_enable_mmu:
	// Initial domain permissions
	ldr r0, =0x55555555
	mcr p15, 0, r0, c3, c0, 0
	// Set ASID to 0
	mov r0, #0
	mcr p15, 0, r0, c13, c0, 1
	// Set the Domain Access register
	mov r0, #1
	mcr p15, 0, r0, c3, c0, 0
	// Reference: ARM Architecture Refrence Manual ARMv7-A
	// Section: B2.12.17 c1, System Control Register (SCTLR)
	// Enable: D-Cache, I-Cache, Alignment, MMU
	// (0x007) --> works
	// Everything without D-Cache (0x003) --> works
	ldr r1, =0x1007
	mrc p15, 0, r0, c1, c0, 0  // read sys. conf. reg.
	orr r0, r0, r1
	mcr p15, 0, r0, c1, c0, 0	// enable MMU
	// Clear pipeline
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	// Wait on some CP15 register
	mrc	p15, 0, r0, c2, c0, 0
	mov	r0, r0
	sub	pc, pc, #4

	bx	lr

cp15_enable_alignment:
    mrc  p15, 0, r0, c1, c0, 0
    bic  r0, r0, #2
    mcr  p15, 0, r0, c1, c0, 0 

    bx lr
	
