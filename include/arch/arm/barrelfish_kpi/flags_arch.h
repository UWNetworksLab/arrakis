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

#ifndef ARCH_ARM_BARRELFISH_KPI_FLAGS_H
#define ARCH_ARM_BARRELFISH_KPI_FLAGS_H

#define ARM_MODE_USR    0x10
#define ARM_MODE_FIQ    0x11
#define ARM_MODE_IRQ    0x12
#define ARM_MODE_SVC    0x13
#define ARM_MODE_ABT    0x17
#define ARM_MODE_UND    0x1b
#define ARM_MODE_SYS    0x1f
#define ARM_MODE_MASK   0x1f
#define ARM_MODE_PRIV   0x0f

#define CPSR_IF_MASK    0xc0
#define CPSR_I_MASK     0x80
#define CPSR_F_MASK     0x40

#endif // ARCH_ARM_BARRELFISH_KPI_FLAGS_H
