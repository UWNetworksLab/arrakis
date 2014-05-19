/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _EMAC_DEBUG_H_
#define _EMAC_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

//#define EMAC_DEBUG(x...) printf("EMAC: " x)
//#define EMAC_DEBUG(x...) ((void)0)


#if defined(EMAC_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define EMAC_DEBUG(x...) printf("EMAC: " x)
#else
#define EMAC_DEBUG(x...) ((void)0)
#endif


#endif // _EMAC_DEBUG_H_
