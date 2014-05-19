/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LOOPBACK_DEBUG_H_
#define LOOPBACK_DEBUG_H_

// *****************************************************************
// * Debug printer:
// *****************************************************************
//#define LOOPBACK_SERVICE_DEBUG 1
#if defined(LOOPBACK_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define LO_DEBUG(x...) printf(" LO: " x)
#else
#define LO_DEBUG(x...) ((void)0)
#endif

#endif // LOOPBACK_DEBUG_H_

