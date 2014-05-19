/**
 * \file PCI device functions
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_DEVICES_H
#define PCI_DEVICES_H

#include "lpc.h"
#include "guest.h"

struct pci_device *pci_hostbridge_new(void);
struct pci_device *pci_ethernet_new(struct lpc *lpc, struct guest *g);
struct pci_device *pci_vmkitmon_eth_new(struct lpc *lpc, struct guest *g);

#endif
