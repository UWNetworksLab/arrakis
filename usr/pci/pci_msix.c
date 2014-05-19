/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include <barrelfish/barrelfish.h>

#include "pci.h"

#define PCI_CAP_MSIX 0x11

/*****************************************************************************/
/* Context management */

struct pci_msix_context {
    bool enabled;
    uint8_t vectors;

    volatile uint32_t *table;

    struct pci_address addr;
    struct pci_msix_context *next;
};

static struct pci_msix_context *contexts = NULL;

static struct pci_msix_context *get_context(struct pci_address *addr,
                                            bool alloc)
{
    struct pci_msix_context *ctx = contexts;
    while (ctx != NULL) {
        if (!memcmp(addr, &ctx->addr, sizeof(*addr)))
            return ctx;
    }

    if (alloc) {
        ctx = calloc(1, sizeof(*ctx));
        memcpy(&ctx->addr, addr, sizeof(*addr));
        ctx->next = contexts;
        contexts = ctx;
    }
    return ctx;
}

/*****************************************************************************/
/* PCI helpers */

/**
 * Find PCI capability by type.
 * @return Offset of first capability with specified type
 */
static uint8_t pci_cap_find(pci_hdr0_t *hdr, struct pci_address *addr,
                            uint8_t type)
{
    pci_hdr0_status_t status;
    uint8_t offset;
    uint32_t header;
    uint8_t max;

    // No caplist -> abort
    status = pci_hdr0_status_rd(hdr);
    if (!status.caplist) {
        return 0;
    }

    max = 48;
    // Spec says bottom 2 bits must be masked
    offset = pci_hdr0_cap_ptr_rd(hdr) & ~0x3U;
    do {
        assert(offset >= 0x40);
        header = pci_read_conf_header(addr, offset / 4);
        if ((header & 0xff) == type) {
            return offset;
        }
        offset = (header >> 8) & 0xfc;
        max--;
    } while (max > 0 && offset != 0);

    return 0;
}


/*****************************************************************************/
/* MSI-X implementation */

errval_t pci_msix_enable(struct pci_address *addr, uint16_t *count)
{
    uint8_t off;
    uint32_t cap[3];
    uint8_t bir;
    struct capref tablecap;
    struct frame_identity frameid = { .base = 0, .bits = 0 };
    errval_t err;
    void *virt;
    struct pci_msix_context *ctx;
    uint16_t i;
    int bar_index;
    volatile uint32_t *table;
    pci_hdr0_t hdr;

    pci_hdr0_initialize(&hdr, *addr);

    if (!(off = pci_cap_find(&hdr, addr, PCI_CAP_MSIX))) {
        return PCI_ERR_MSIX_NOTSUP;
    }

    ctx = get_context(addr, true);

    off /= 4;
    cap[0] = pci_read_conf_header(addr, off);
    cap[1] = pci_read_conf_header(addr, off + 1);
    cap[2] = pci_read_conf_header(addr, off + 2);

    // TODO: How do we do this using mackerel?
    *count = ((cap[0] >> 16) & ((1 << 11) - 1)) + 1;

    if (!ctx->enabled) {
        // Make sure MSI-X is disabled during initialization
        cap[0] &= ~(1 << 31);
        pci_write_conf_header(addr, off, cap[0]);


        // Find BAR for MSI-X table and map memory
        bir = cap[1] & 0x7;
        // TODO map all caps
        bar_index = pci_bar_to_caps_index(addr->bus, addr->device,
                                          addr->function, bir);
        assert(bar_index >= 0);
        assert(pci_get_nr_caps_for_bar(addr->bus, addr->device, addr->function,
                                        bar_index) == 1);
        tablecap = pci_get_cap_for_device(addr->bus, addr->device, addr->function,
                                          bar_index, 0);
        invoke_frame_identify(tablecap, &frameid);
        err = vspace_map_one_frame_attr(&virt, 1 << frameid.bits, tablecap,
                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
        assert(err_is_ok(err));

        // Enable MSI-X and function mask
        cap[0] |= (1 << 31) | (1 << 30);
        pci_write_conf_header(addr, off, cap[0]);

        // Calculate address for table
        cap[1] &= ~0x7;
        assert(cap[1] + (*count*16) <= (1ULL << frameid.bits));
        table = (uint32_t *) ((uintptr_t) virt + cap[1]);
    } else {
        table = ctx->table;
    }


    // Make sure all interrupts are masked
    for (i = 0; i < *count; i++) {
        table[4*i + 3] |= 1;
    }

    // Disable INTX
    pci_hdr0_command_int_dis_wrf(&hdr, 1);

    // Disable function mask
    cap[0] &= ~(1 << 30);
    pci_write_conf_header(addr, off, cap[0]);

    // Save context
    ctx->enabled = true;
    ctx->vectors = *count;
    ctx->table = table;

    return SYS_ERR_OK;
}

errval_t pci_msix_vector_init(struct pci_address *addr, uint16_t idx,
                              uint8_t destination, uint8_t vector)
{
    struct pci_msix_context *ctx;
    volatile uint32_t *entry;

    ctx = get_context(addr, false);
    if (!ctx || !ctx->enabled) {
        return PCI_ERR_MSIX_DISABLED;
    }

    if (idx >= ctx->vectors) {
        return PCI_ERR_MSIX_BADVECTOR;
    }

    entry = ctx->table + 4 * idx;
    // Message address
    entry[0] = 0xFEE00000 | ((uint32_t) destination << 12);
    // Message address high
    entry[1] = 0x0;
    // Message data
    entry[2] = vector;
    // Message control (unmask)
    entry[3] &= ~0x1;

    return SYS_ERR_OK;

}

