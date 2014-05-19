/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef RPC_DEBUG_H_
#define RPC_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/
//#define RPC_DEBUG 1
#if defined(RPC_DEBUG) || defined(GLOBAL_DEBUG)
#define RPC_DEBUGP(x...) printf("RPC: " x)
#else
#define RPC_DEBUGP(x...) ((void)0)
#endif

#endif // RPC_DEBUG_H_
