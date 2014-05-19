/**
 * \file PCI VM-to-host interface
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_HOST_H
#define PCI_HOST_H

#include "pci.h"

void init_host_devices(struct pci *pci);

#endif
