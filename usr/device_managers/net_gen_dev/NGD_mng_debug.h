/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _NET_GEN_DEV_MNG_DEBUG_H_
#define _NET_GEN_DEV_MNG_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

#define NET_GEN_DEV_MNG_SERVICE_DEBUG 1

#if defined(NET_GEN_DEV_MNG_SERVICE_DEBUG ) || defined(GLOBAL_DEBUG)
#define NGKDM_DEBUG(x...) printf("NGKDM: " x)
#else
#define NGKDM_DEBUG(x...) ((void)0)
#endif // defined(E1K_DEV_MNG_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)

#endif // _NET_GEN_DEV_MNG_DEBUG_H_


