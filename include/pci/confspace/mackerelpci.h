/**
 * \file
 * \brief Mackerel support for PCI config space
 */

/*
 * Copyright (c) 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_MACKERELPCI_H
#define LIBBARRELFISH_MACKERELPCI_H

#include "pci_confspace.h"
#include <mackerel/mackerel.h>

typedef struct pci_address mackerel_pci_t;

/*
 * Reading from PCI config space
 */
static inline uint8_t mackerel_read_pci_8(mackerel_pci_t base, int offset)
{
    lvaddr_t va = pcie_confspace_access(base);
    if (va != 0) {
        return mackerel_read_addr_8((void *)va, offset);
    }

    uint32_t word = pci_read_conf_header(&base, offset / 4);
    return (word >> (offset % 4) * NBBY) & 0xff;
}

static inline uint16_t mackerel_read_pci_16(mackerel_pci_t base, int offset)
{
    lvaddr_t va = pcie_confspace_access(base);
    if (va != 0) {
        return mackerel_read_addr_16((void *)va, offset);
    }

    assert(offset % 2 == 0);
    uint32_t word = pci_read_conf_header(&base, offset / 4);
    if (offset % 4 == 0) {
        return word & 0xffff;
    } else {
        return word >> 16;
    }
}

static inline uint32_t mackerel_read_pci_32(mackerel_pci_t base, int offset)
{
    lvaddr_t va = pcie_confspace_access(base);
    if (va != 0) {
        return mackerel_read_addr_32((void *)va, offset);
    }

    assert(offset % 4 == 0);
    return pci_read_conf_header(&base, offset / 4);
}

//static inline uint64_t mackerel_read_pci_64(mackerel_pci_t base, int offset);

static inline void mackerel_write_pci_8(mackerel_pci_t base, int offset, uint8_t v)
{
    lvaddr_t va = pcie_confspace_access(base);
    if (va != 0) {
        return mackerel_write_addr_8((void *)va, offset, v);
    }

    // FIXME: this is truly awful
    uint32_t old = pci_read_conf_header(&base, offset / 4);
    uint32_t new = (old & (0xff << (offset % 4) * NBBY)) | (v << (offset % 4) * NBBY);
    pci_write_conf_header(&base, offset / 4, new);
}

static inline void mackerel_write_pci_16(mackerel_pci_t base, int offset, uint16_t v)
{
    lvaddr_t va = pcie_confspace_access(base);
    if (va != 0) {
        return mackerel_write_addr_16((void *)va, offset, v);
    }

    // FIXME: this is truly awful
    assert(offset % 2 == 0);
    uint32_t old = pci_read_conf_header(&base, offset / 4);
    uint32_t new;
    if (offset % 4 == 0) {
        new = (old & 0xffff0000) | v;
    } else {
        new = (old & 0xffff) | (v << 16);
    }
    pci_write_conf_header(&base, offset / 4, new);
}

static inline void mackerel_write_pci_32(mackerel_pci_t base, int offset, uint32_t v)
{
    lvaddr_t va = pcie_confspace_access(base);
    if (va != 0) {
        return mackerel_write_addr_32((void *)va, offset, v);
    }

    assert(offset % 4 == 0);
    pci_write_conf_header(&base, offset / 4, v);
}

//static inline void mackerel_write_pci_64(mackerel_pci_t base, int offset, uint64_t v);

#endif
