/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef AHCI_DEBUG_H_
#define AHCI_DEBUG_H_

#include <stdio.h>

/*****************************************************************
 * Debug printer:
 *****************************************************************/

#if defined(AHCI_LIB_DEBUG) || defined(GLOBAL_DEBUG)
#define AHCI_DEBUG(x...) printf("ahci: " x)
#else
#define AHCI_DEBUG(x...) ((void)0)
#endif
#if defined(AHCI_LIB_DEBUG) || defined(GLOBAL_DEBUG)
#define AHCI_TRACE_ENTER0() AHCI_TRACE_ENTER(" ")
#define AHCI_TRACE_ENTER(x...) do { \
    printf("ahci: entering %s. ", __FUNCTION__); \
    printf(x); \
    printf("\n"); \
} while(0)
#else
#define AHCI_TRACE_ENTER0() ((void)0)
#define AHCI_TRACE_ENTER(x...) ((void)0)
#endif

#endif // AHCI_DEBUG_H_
