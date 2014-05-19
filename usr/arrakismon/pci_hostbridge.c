/**
 * \file Fake PCI host bridge
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>

#include "vmkitmon.h"
#include "pci.h"
#include "pci_devices.h"
#include "pci_hdr0_mem_dev.h"

#define INVALID         0xffffffff

struct pci_hostbridge {
    pci_hdr0_mem_t      ph;
    uint32_t            pci_header[0x40];
};

static void confspace_write(struct pci_device *dev,
                            union pci_config_address_word addr,
                            enum opsize size, uint32_t val)
{
}

static void confspace_read(struct pci_device *dev,
                           union pci_config_address_word addr,
                           enum opsize size, uint32_t *val)
{
    struct pci_hostbridge *h = dev->state;

    if(addr.d.fnct_nr != 0) {
        *val = INVALID;
        return;
    }

    if(addr.d.doubleword < 0x40) {
        *val = h->pci_header[addr.d.doubleword];
    } else {
        *val = INVALID;
    }
}

struct pci_device *pci_hostbridge_new(void)
{
    struct pci_device *dev = calloc(1, sizeof(struct pci_device));
    struct pci_hostbridge *host = calloc(1, sizeof(struct pci_hostbridge));
    pci_hdr0_mem_t *ph = &host->ph;

    dev->confspace_write = confspace_write;
    dev->confspace_read = confspace_read;
    dev->state = host;

    pci_hdr0_mem_initialize(ph, (mackerel_addr_t)host->pci_header);

    // Fake a host bridge
    pci_hdr0_mem_vendor_id_wr(ph, 0x8086);
    pci_hdr0_mem_device_id_wr(ph, 1);
    pci_hdr0_mem_class_code_clss_wrf(ph, pci_hdr0_mem_bridge);

    return dev;
}
