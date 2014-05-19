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

#include <stdlib.h>

#include "vmkitmon.h"
#include "pci.h"
#include "pci_host.h"
#include "pci_hdr0_mem_dev.h"

#define INVALID         0xffffffff

#define VMKIT_PCI_HOST_DEBUG_SWITCH

#if defined(VMKIT_PCI_HOST_DEBUG_SWITCH)
#define VMKIT_PCI_HOST_DEBUG(x...) printf("VMKit PCI host: " x)
#else
#define VMKIT_PCI_HOST_DEBUG(x...) ((void)0)
#endif

struct pci_host {
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
    struct pci_host *h = dev->state;

    if(addr.d.fnct_nr != 0) {
        *val = INVALID;
        return;
    }

    if(addr.d.doubleword < 0x40) {
        *val = h->pci_header[addr.d.doubleword];
        VMKIT_PCI_HOST_DEBUG("reading register %d, opsize %d: %x\n",
                             addr.d.doubleword, size, *val);

        if(size == 1) {
            *val = (*val) & 0xffff;
        }
    } else {
        *val = INVALID;
    }
}

void init_host_devices(struct pci *pci)
{
    struct pci_device *dev = calloc(1, sizeof(struct pci_device));
    struct pci_host *host = calloc(1, sizeof(struct pci_host));
    pci_hdr0_mem_t *ph = &host->ph;

    dev->confspace_write = confspace_write;
    dev->confspace_read = confspace_read;
    dev->state = host;

    pci_hdr0_mem_initialize(ph, (mackerel_addr_t)host->pci_header);

    int r = pci_attach_device(pci, 0, 1, dev);
    assert(r == 0);
}
