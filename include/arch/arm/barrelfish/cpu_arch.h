/**
 * \file
 * \brief Architecture specific CPU bits.
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARM_BARRELFISH_CPU_H
#define ARM_BARRELFISH_CPU_H

// XXX: placeholder; there's definitely more than one arm in existence!
#ifdef __ARM_ARCH_7A__
#define CURRENT_CPU_TYPE CPU_ARM7
#elif __ARM_ARCH_5__
#define CURRENT_CPU_TYPE CPU_ARM5
#else
#error "must define CURRENT_CPU_TYPE"
#endif

#endif
