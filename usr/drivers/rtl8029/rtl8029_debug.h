/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef RTL8029_DEBUG_H_
#define RTL8029_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/
//#define RTL8029_SERVICE_DEBUG 1
#if defined(RTL8029_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define RTL8029_DEBUG(x...) printf("RTL8029: " x)
#else
#define RTL8029_DEBUG(x...) ((void)0)
#endif

#endif // RTL8029_DEBUG_H_
