/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitätstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef UHCI_DEBUG_H_
#define UHCI_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

#if defined(UHCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define UHCI_DEBUG(x...) printf("uhci: " x)
#else
#define UHCI_DEBUG(x...) ((void)0)
#endif

#endif // UHCI_DEBUG_H_
