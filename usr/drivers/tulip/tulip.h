/** \file
 * \brief DEC Tulip ethernet driver
 *
 * This file is a driver for the DEC21140 Tulip ethernet card
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _TULIP_H
#define _TULIP_H

#include <stdint.h>
#include <pci/pci.h>

//#define TULIP_DEBUG
#ifdef TULIP_DEBUG
#define TU_DEBUG(x...) printf("TULIP: " x)
#else
#define TU_DEBUG(x...) ((void)0)
#endif

#define PCI_VENDOR_DEC          0x1011
#define PCI_DEVICE_DEC_TULIP    0x9
#define TULIP_PORTBASE          0xec00   
#define TULIP_PORTEND           0xec1f
#define TULIP_IRQ               9

#endif // _TULIP_H
