/**
 * \file
 * \brief Header file for the PCI driver
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_H_
#define PCI_H_

#include <string.h>

#include <pci/confspace/mackerelpci.h>
#include <pci/confspace/pci_confspace.h>
#include "pci_hdr0_dev.h"
#include "pci_hdr1_dev.h"

/// BIOS area is 1MB in size
#define BIOS_BITS       20

#if 0

struct pci_device_info {
    struct pci_address addr;
    struct bus *bus;
    uint16_t vendor_id;
    uint16_t device_id;
    pci_hdr0_class_code_t classcode;
    bool pcie;
    uint32_t irq;
    struct device_mem *bar_info;
    int nr_allocated_bars;
    bool driver_loaded;
#if 0
    void *lowlevel_representation; /**< representation of the hardware
                                       access to the upper part
                                      of the per core instance of the driver.
                                      not used by applications (or libraries) */
    void *logical_representation; /**< representation of the device to
                                       libraries/applications (such as
                                       representing "eth0") */
#endif
};
#endif

errval_t pci_setup_root_complex(void);
void pci_add_root(struct pci_address addr, uint8_t maxchild, char* handle);
void pci_program_bridges(void);
void pci_init(void);
void pci_init_datastructures(void);
errval_t device_init(bool enable_irq, uint8_t coreid, int vector,
                     uint32_t class_code, uint32_t sub_class, uint32_t prog_if,
                     uint32_t vendor_id, uint32_t device_id,
                     uint32_t *bus, uint32_t *dev,uint32_t *fun,
                     int *nr_allocated_bars);
int pci_bar_to_caps_index(uint8_t bus, uint8_t dev, uint8_t fun, uint8_t BAR);
int pci_get_nr_caps_for_bar(uint8_t bus, uint8_t dev, uint8_t fun, uint8_t index);
struct capref pci_get_cap_for_device(uint8_t bus, uint8_t dev, uint8_t fun,
                                     uint8_t index, int cap_nr);
uint8_t pci_get_cap_type_for_device(uint8_t bus, uint8_t dev, uint8_t fun,
                                    uint8_t index);
void pci_enable_interrupt_for_device(uint32_t bus, uint32_t dev, uint32_t fun,
                                    bool pcie);
errval_t pcie_setup_confspace(void);

errval_t pci_msix_enable(struct pci_address *addr, uint16_t *count);
errval_t pci_msix_vector_init(struct pci_address *addr, uint16_t idx,
                              uint8_t destination, uint8_t vector);

extern const char *skb_bridge_program;
extern uint16_t max_numvfs;

#endif // PCI_H_
