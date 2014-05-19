/**
 * \file
 * \brief Arch specific CPU declarations
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*-
 * Copyright (c) 1989, 1990 William F. Jolitz
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      from: @(#)segments.h    7.1 (Berkeley) 5/9/91
 * $FreeBSD$
 */

#ifndef TARGET_X86_64_BARRELFISH_KPI_CPU_H
#define TARGET_X86_64_BARRELFISH_KPI_CPU_H

/*
 * AMD64 Segmentation Data Structures and definitions
 */

/**
 * \brief Segment descriptor
 */
union segment_descriptor { // FIXME: rename to x86_64_segment_descriptor
    uint64_t raw;
    struct {
        uint64_t lo_limit:16;
        uint64_t lo_base:24;
        uint64_t type:4;
        uint64_t system_desc:1;         // S
        uint64_t privilege_level:2;     // DPL
        uint64_t present:1;             // P
        uint64_t hi_limit:4;
        uint64_t available:1;           // AVL
        uint64_t long_mode:1;           // L
        uint64_t operation_size:1;      // D/B
        uint64_t granularity:1;         // G
        uint64_t hi_base:8;
    } d;
    struct {
        uint64_t lo_limit:16;
        uint64_t lo_base:24;
        uint64_t type:4;
        uint64_t always0:1;
        uint64_t privilege_level:2;
        uint64_t present:1;
        uint64_t hi_limit:4;
        uint64_t available:1;
        uint64_t always0_1:2;
        uint64_t granularity:1;
        uint64_t hi_base:8;
    } sys_lo; ///< low part of system descriptor (TSS, LDT, etc.)
    struct {
        uint64_t base:32;
        uint64_t reserved:8;
        uint64_t always0:5;
        uint64_t reserved2:19;
    } sys_hi; ///< high part of system descriptor (TSS, LDT, etc.)
};

///< Constructs a segment selector in the LDT with user privilege
#define X86_64_LDT_SELECTOR(s) (((s)<<3) | 7)

///< Return the index of a segment selector
#define X86_64_SELECTOR_IDX(s) ((s)>>3)

#endif
