/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef SERIAL_DEBUG_H_
#define SERIAL_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

#if defined(SERIAL_DRIVER_DEBUG) || defined(GLOBAL_DEBUG)
#define SERIAL_DEBUG(arg...) debug_printf(arg)
#else
#define SERIAL_DEBUG(arg...) ((void)0)
#endif

#endif // SERIAL_DEBUG_H_
