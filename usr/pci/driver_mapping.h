/**
 * \file
 * \brief Database file to identify drivers to initialize
 *
 * This is a simple database which is used to identify which driver to load
 * for which PCI class
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef DRIVER_MAPPING_H_
#define DRIVER_MAPPING_H_

#include <pci/mem.h>
#include "pci.h"

//#include "rtl8029.h"
#include "ht_config.h"
//#include "vbe.h"

struct driver_mapping_entry {
    uint8_t class_code;
    uint8_t subclass_code;
    uint8_t prog_interface;
    uint16_t vendor_id;
    uint16_t device_id;
    void (*init_device)(struct pci_address pciaddr, struct device_mem *bar_info,
                        uint64_t nr_mapped_regions, uint32_t irq);
    struct {
        uint64_t initialized : 1;
        uint64_t res : 63;
    } flags;
};

static struct driver_mapping_entry entries[] = {
    // Intel e1000 (4.7.4 p99)
    /*
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x105e, e1000_init, {0, 0}},
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x107d, e1000_init, {0, 0}},
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x1081, e1000_init, {0, 0}},
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x1082, e1000_init, {0, 0}},
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x1083, e1000_init, {0, 0}},
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x108b, e1000_init, {0, 0}},
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x108c, e1000_init, {0, 0}},
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x1096, e1000_init, {0, 0}},
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x1097, e1000_init, {0, 0}},
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x1098, e1000_init, {0, 0}},
    {PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_INTEL, 0x109a, e1000_init, {0, 0}},
    */

    // Realtek RTL8029(AS)
    //{PCI_CLASS_ETHERNET, 0, 0, PCI_VENDOR_REALTEK, PCI_DEVICE_RTL8029, rtl8029_initialize_card, {0, 0}}

    // AMD HyperTransport Configuration
    //{PCI_CLASS_HOST_BRIDGE, 0, 0, PCI_VENDOR_AMD, 0x1200, ht_config_init, {0, 0}},

    // VBE 2.0 compatible graphics card
/*     {PCI_CLASS_DISPLAY, 0, 0, 0x1013, 0x00b8, vbe_init, {0, 0}}, */
/*     {PCI_CLASS_DISPLAY, 0, 0, 0x1234, 0x1111, vbe_init, {0, 0}}, */
/*     {PCI_CLASS_DISPLAY, 0, 0, 0x8086, 0x2a42, vbe_init, {0, 0}} */
};

static uint64_t driver_mapping_index;

static inline struct driver_mapping_entry
driver_mapping_search_entry(uint8_t class, uint16_t vendor, uint16_t device)
{
    for(int i = 0; i < sizeof(entries) / sizeof(entries[0]); i++) {
        struct driver_mapping_entry *entry = &entries[i];

        if(entry->class_code == class && entry->vendor_id == vendor &&
           entry->device_id == device) {
            return *entry;
        }
    }

    return (struct driver_mapping_entry) { .class_code = 0xff };
}

static inline struct driver_mapping_entry driver_mapping_get_next_entry(void)
{
    if(driver_mapping_index >= (sizeof(entries) / sizeof(entries[0]))) {
        driver_mapping_index = 0;
    }
    return(entries[driver_mapping_index++]);
}


static inline struct driver_mapping_entry driver_mapping_get_entry(void)
{
    driver_mapping_index = 0;
    return driver_mapping_get_next_entry();
}

static inline size_t driver_mapping_get_nr_entries(void)
{
    return(sizeof(entries) / sizeof(entries[0]));
}

#endif // DRIVER_MAPPING_H_
