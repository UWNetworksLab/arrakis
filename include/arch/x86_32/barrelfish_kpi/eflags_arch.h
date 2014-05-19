/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_32_BARRELFISH_KPI_EFLAGS_H
#define ARCH_X86_32_BARRELFISH_KPI_EFLAGS_H

#define USER_CS                         0x23
#define USER_SS                         0x1b

/***** EFLAGS flags *****/

/* Fixed flags */
#define EFLAGS_ALWAYS1  (1 << 1)

/* Status/Control flags */
#define EFLAGS_CF       (1 << 0)
#define EFLAGS_PF       (1 << 2)
#define EFLAGS_AF       (1 << 4)
#define EFLAGS_ZF       (1 << 6)
#define EFLAGS_SF       (1 << 7)
#define EFLAGS_DF       (1 << 10)
#define EFLAGS_OF       (1 << 11)

/* System flags */
#define EFLAGS_TF       (1 << 8)
#define EFLAGS_IF       (1 << 9)
#define EFLAGS_NT       (1 << 14)
#define EFLAGS_RF       (1 << 16)
#define EFLAGS_VM       (1 << 17)
#define EFLAGS_AC       (1 << 18)
#define EFLAGS_VIF      (1 << 19)
#define EFLAGS_VIP      (1 << 20)
#define EFLAGS_ID       (1 << 21)

/* I/O privilege flags */
#define EFLAGS_IOPL0    (0 << 12)
#define EFLAGS_IOPL1    (1 << 12)
#define EFLAGS_IOPL2    (2 << 12)
#define EFLAGS_IOPL3    (3 << 12)

/**
 * State of EFLAGS when executing a user-space program: Enable interrupts
 */
#define USER_EFLAGS     (EFLAGS_ALWAYS1 | EFLAGS_IF)

#endif // ARCH_X86_32_BARRELFISH_KPI_EFLAGS_H
