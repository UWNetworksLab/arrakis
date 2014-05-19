/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef E10K_H_
#define E10K_H_

#include "e10k_dev.h"

//#define E10K_PCI_DEVID 0x10FB
#define E10K_PCI_DEVID 0x154d

void e10k_phy_init(e10k_t* d);

extern int qi;

#endif // ndef E10K_H_
