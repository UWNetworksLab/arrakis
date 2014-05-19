/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LPC_TIMER_DEBUG_H
#define LPC_TIMER_DEBUG_H


/*****************************************************************
 * Debug printer:
 *****************************************************************/
#if defined(LPC_TIMER_DEBUG) /* || defined(GLOBAL_DEBUG)  -- too noisy! */
#define LPC_DEBUG(x...) printf("lpc_timer: " x)
#else
#define LPC_DEBUG(x...) ((void)0)
#endif
#endif // LPC_TIMER_DEBUG_H
