/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef VTD_DEBUG_H_
#define VTD_DEBUG_H_

/*****************************************************************
 * Debug printer and its power-switch:
 *****************************************************************/

//#define VTD_DEBUG_ 1

#if defined(VTD_DEBUG_) || defined(GLOBAL_DEBUG)
#define VTD_DEBUG(x...) printf("vtd: " x)
#else
#define VTD_DEBUG(x...) ((void)0)
#endif

#endif // VTD_DEBUG_H_
