/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_DEBUG_H_
#define PCI_DEBUG_H_


/*****************************************************************
 * Debug printer and its power-switch:
 *****************************************************************/

//#define PCI_SERVICE_DEBUG 1

#if defined(PCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define PCI_DEBUG(x...) printf("pci_service: " x)
#else
#define PCI_DEBUG(x...) ((void)0)
#endif


#endif // PCI_DEBUG_H_
