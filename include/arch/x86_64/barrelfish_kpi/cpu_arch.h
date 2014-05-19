/**
 * \file
 * \brief Arch specific CPU declarations
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_64_BARRELFISH_KPI_CPU_H
#define ARCH_X86_64_BARRELFISH_KPI_CPU_H

/// This CPU supports lazy FPU context switching
#define FPU_LAZY_CONTEXT_SWITCH

/*
 * Entries in the Interrupt Descriptor Table (IDT)
 */
#define IDT_DE          0       /* #DE: Divide Error */
#define IDT_DB          1       /* #DB: Debug */
#define IDT_NMI         2       /* Nonmaskable External Interrupt */
#define IDT_BP          3       /* #BP: Breakpoint */
#define IDT_OF          4       /* #OF: Overflow */
#define IDT_BR          5       /* #BR: Bound Range Exceeded */
#define IDT_UD          6       /* #UD: Undefined/Invalid Opcode */
#define IDT_NM          7       /* #NM: No Math Coprocessor */
#define IDT_DF          8       /* #DF: Double Fault */
#define IDT_FPUGP       9       /* Coprocessor Segment Overrun */
#define IDT_TS          10      /* #TS: Invalid TSS */
#define IDT_NP          11      /* #NP: Segment Not Present */
#define IDT_SS          12      /* #SS: Stack Segment Fault */
#define IDT_GP          13      /* #GP: General Protection Fault */
#define IDT_PF          14      /* #PF: Page Fault */
#define IDT_MF          16      /* #MF: FPU Floating-Point Error */
#define IDT_AC          17      /* #AC: Alignment Check */
#define IDT_MC          18      /* #MC: Machine Check */
#define IDT_XF          19      /* #XF: SIMD Floating-Point Exception */

#endif
