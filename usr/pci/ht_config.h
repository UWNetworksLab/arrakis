/**
 * \file
 * \brief HyperTransport Configuration driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef HT_CONFIG_H
#define HT_CONFIG_H

#include "pci.h"
#include <pci/mem.h>

void ht_config_init(struct pci_address pciaddr, struct device_mem *bar_info,
                    uint64_t nr_mapped_regions, uint32_t irq);

#endif
