/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef AHCID_DEBUG_H_
#define AHCID_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

//#define AHCI_SERVICE_DEBUG 1

#if defined(AHCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define AHCID_DEBUG(x...) printf("ahcid: " x)
#else
#define AHCID_DEBUG(x...) ((void)0)
#endif

#endif // AHCI_DEBUG_H_
