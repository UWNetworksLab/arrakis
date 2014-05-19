/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DEVICE_MANAGER_DEBUG_H_
#define DEVICE_MANAGER_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

//#define DEVICE_MNG_SERVICE_DEBUG 1

#if defined(DEVICE_MNG_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define NDM_DEBUG(x...) printf("NDM: " x)
#else
#define NDM_DEBUG(x...) ((void)0)
#endif // defined(DEVICE_MNG_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)

#endif // DEVICE_MANAGER_DEBUG_H_

