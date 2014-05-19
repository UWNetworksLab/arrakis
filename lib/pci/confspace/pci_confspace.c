/**
 * \file
 * \brief PCI configuration library
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <arch/x86/barrelfish/iocap_arch.h>
#include <pci/confspace/pci_confspace.h>

#define PCI_CONFIG_ADDRESS_PORT 0x0cf8
#define PCI_CONFIG_DATA_PORT    0x0cfc

union pci_config_address_word {
     uint32_t raw;
    struct {
         uint32_t mbz : 2;
         uint32_t  doubleword : 6;
         uint32_t fnct_nr : 3;
         uint32_t dev_nr : 5;
         uint32_t bus_nr : 8;
         uint32_t res : 7;
         uint32_t enable_conf_space_mapping : 1;
    } __attribute__ ((packed)) d;
} __attribute__ ((packed)) ;

uint32_t pci_read_conf_header(struct pci_address *address, uint64_t dword)
{
    union pci_config_address_word cfg;
    uint32_t val = 0;
    int r;

    cfg.raw = 0;
    cfg.d.bus_nr = address->bus;
    cfg.d.dev_nr = address->device;
    cfg.d.fnct_nr = address->function;
    cfg.d.enable_conf_space_mapping = 1;
    cfg.d.doubleword = dword;
    r = iocap_out32(cap_io, PCI_CONFIG_ADDRESS_PORT, cfg.raw);
    assert(r == 0);
    if (r != 0) {
        return -1; // XXX
    }
    r = iocap_in32(cap_io, PCI_CONFIG_DATA_PORT, &val);
    assert(r == 0);
    if (r != 0) {
        return -1; // XXX
    }
    return val;
}

void pci_write_conf_header(struct pci_address *address, uint64_t dword,
                           uint32_t data)
{
    union pci_config_address_word cfg;
    int r;

    cfg.raw = 0;
    cfg.d.bus_nr = address->bus;
    cfg.d.dev_nr = address->device;
    cfg.d.fnct_nr = address->function;
    cfg.d.enable_conf_space_mapping = 1;
    cfg.d.doubleword = dword;
    r = iocap_out32(cap_io, PCI_CONFIG_ADDRESS_PORT, cfg.raw);
    assert(r == 0); // XXX
    r = iocap_out32(cap_io, PCI_CONFIG_DATA_PORT, data);
    assert(r == 0); // XXX
}
