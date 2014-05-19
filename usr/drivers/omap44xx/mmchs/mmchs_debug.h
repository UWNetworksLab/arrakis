/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MMCHS2_DEBUG_H
#define MMCHS2_DEBUG_H

//#define MMCHS_SERVICE_DEBUG 1

#if defined(MMCHS_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define MMCHS_DEBUG(x...) debug_printf(x)
#else
#define MMCHS_DEBUG(x...) ((void)0)
#endif

#endif // MMCHS2_DEBUG_H