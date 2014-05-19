/*
 * Copyright (c) 2008, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __E1000_DEBUG_H__
#define __E1000_DEBUG_H__

#include "e1000n.h"

/*****************************************************************
 * Debug printer:
 *****************************************************************/
//#define E1000_SERVICE_DEBUG 1
#if defined(E1000_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define E1000_DEBUG(fmt, ...) printf(DRIVER_STRING fmt, ##__VA_ARGS__)
#else
#define E1000_DEBUG(fmt, ...) ((void)0)
#endif

#endif // __E1000_DEBUG_H__
