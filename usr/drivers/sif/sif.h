/**
 * \file
 * \brief SIF driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SIF_H
#define SIF_H

#include <stdint.h>
#include <barrelfish/barrelfish.h>
#include <pci/pci.h>

void sif_init(struct device_mem *bar_info, int nr_mapped_regions);
void sif_interrupt_handler(void *arg);

#endif
