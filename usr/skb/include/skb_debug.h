/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SKB_DEBUG_H_
#define SKB_DEBUG_H_


/*****************************************************************
 * Debug printer and its power-switch:
 *****************************************************************/
//#define SKB_SERVICE_DEBUG 1
#if defined(SKB_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define SKB_DEBUG(x...) printf("skb_service: " x)
#else
#define SKB_DEBUG(x...) ((void)0)
#endif

#endif // SKB_DEBUG_H_
