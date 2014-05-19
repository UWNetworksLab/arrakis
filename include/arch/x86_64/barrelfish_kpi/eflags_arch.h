/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_64_BARRELFISH_KPI_EFLAGS_H
#define ARCH_X86_64_BARRELFISH_KPI_EFLAGS_H

/**
 * State of EFLAGS when executing a user-space program: Enable interrupts
 */
#ifndef ARRAKIS
// Unprivileged mode
#       define USER_CS                         0x23
#       define USER_SS                         0x1b
#else
// Privileged mode
#       define USER_CS                         0x8
#       define USER_SS                         0x10
#endif

#define USER_EFLAGS                     0x202

#endif // ARCH_X86_64_BARRELFISH_KPI_EFLAGS_H
