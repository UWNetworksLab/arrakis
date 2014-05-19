/**
 * \file
 * \brief x86-64 interrupt/exception handling
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
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

#ifndef KERNEL_IRQ_H
#define KERNEL_IRQ_H

/*
 * AMD64 Segmentation Data Structures and definitions
 */

/*
 * Selectors
 */

#define SEL_RPL_MASK    3       /* requester priv level */
#define ISPL(s) ((s)&3)         /* what is the priority level of a selector */
#define SEL_KPL 0               /* kernel priority level */
#define SEL_UPL 3               /* user priority level */
#define ISLDT(s)        ((s)&SEL_LDT)   /* is it local or global */
#define SEL_LDT 4               /* local descriptor table */
#define IDXSEL(s)       (((s)>>3) & 0x1fff)             /* index of selector */
#define LSEL(s,r)       (((s)<<3) | SEL_LDT | r)        /* a local selector */
#define GSEL(s,r)       (((s)<<3) | r)                  /* a global selector */

/**
 * Gate descriptors (e.g. indirect descriptors, trap, interrupt etc. 128 bit)
 * Only interrupt and trap gates have gd_ist.
 */
struct  gate_descriptor {
    uint64_t gd_looffset:16;       /* gate offset (lsb) */
    uint64_t gd_selector:16;       /* gate segment selector */
    uint64_t gd_ist:3;             /* IST table index */
    uint64_t gd_xx:5;              /* unused */
    uint64_t gd_type:5;            /* segment type */
    uint64_t gd_dpl:2;             /* segment descriptor priority level */
    uint64_t gd_p:1;               /* segment descriptor present */
    uint64_t gd_hioffset:48;       /* gate offset (msb) */
    uint64_t sd_xx1:32;
} __attribute__((packed));

/* system segments and gate types */
#define SDT_SYSNULL      0      /* system null */
#define SDT_SYSLDT       2      /* system 64 bit local descriptor table */
#define SDT_SYSTSS       9      /* system available 64 bit TSS */
#define SDT_SYSBSY      11      /* system busy 64 bit TSS */
#define SDT_SYSCGT      12      /* system 64 bit call gate */
#define SDT_SYSIGT      14      /* system 64 bit interrupt gate */
#define SDT_SYSTGT      15      /* system 64 bit trap gate */

/* memory segment types */
#define SDT_MEMRO       16      /* memory read only */
#define SDT_MEMROA      17      /* memory read only accessed */
#define SDT_MEMRW       18      /* memory read write */
#define SDT_MEMRWA      19      /* memory read write accessed */
#define SDT_MEMROD      20      /* memory read only expand dwn limit */
#define SDT_MEMRODA     21      /* memory read only expand dwn limit accessed */
#define SDT_MEMRWD      22      /* memory read write expand dwn limit */
#define SDT_MEMRWDA     23      /* memory read write expand dwn limit accessed */
#define SDT_MEME        24      /* memory execute only */
#define SDT_MEMEA       25      /* memory execute only accessed */
#define SDT_MEMER       26      /* memory execute read */
#define SDT_MEMERA      27      /* memory execute read accessed */
#define SDT_MEMEC       28      /* memory execute only conforming */
#define SDT_MEMEAC      29      /* memory execute only accessed conforming */
#define SDT_MEMERC      30      /* memory execute read conforming */
#define SDT_MEMERAC     31      /* memory execute read accessed conforming */

/*
 * Size of IDT table
 */
#define NIDT    256             /* 32 reserved, 16 h/w, 0 s/w, linux's 0x80 */

/*
 * Entries in the Global Descriptor Table (GDT)
 */
#define NULL_SEL        0       /**< Null descriptor */
#define KCODE_SEL       1       /**< Kernel code descriptor */
#define KSTACK_SEL      2       /**< Shared user/kernel stack descriptor */
#define USTACK_SEL      3       /**< User stack descriptor */
#define UCODE_SEL       4       /**< User code descriptor */
#define TSS_LO_SEL      5       /**< Task State Segment (TSS) -- low 64bit */
#define TSS_HI_SEL      6       /**< Task State Segment (TSS) -- high 64bit */
#define LDT_LO_SEL      7       /**< Local descriptor table (LDT) -- low */
#define LDT_HI_SEL      8       /**< Local descriptor table (LDT) -- high */
#define NGDT_MEM        9       /**< Number of descriptors */

/**
 * region descriptors, used to load gdt/idt tables before segments yet exist.
 */
struct region_descriptor {
    uint16_t rd_limit;          /**< segment extent */
    uint64_t rd_base;           /**< base address  */
} __attribute__((packed));

struct task_state_segment {
    uint32_t    reserved;
    uint64_t    rsp[3];
    uint64_t    reserved2;
    uint64_t    ist[7];
    uint64_t    reserved3;
    uint16_t    reserved4;
    uint16_t    iomap_base;
} __attribute__ ((packed));

void setup_default_idt(void);

errval_t irq_table_set(unsigned int nidt, capaddr_t endpoint);
errval_t irq_table_delete(unsigned int nidt);

#endif
