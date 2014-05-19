/**
 * \file
 * \brief PCI configuration space access
 */

/*
 * Copyright (c) 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <stdlib.h>
#include <acpi_client/acpi_client.h>
#include <pci/confspace/pci_confspace.h>

static uint8_t startbus, endbus;
static struct memobj_one_frame_lazy *memobj = NULL;
static struct vregion *vregion = NULL;
static struct capref region_cap;

/// Enable PCIe. Default is yes.
static bool pcie_enabled = true;

/* FIXME: XXX: super-hacky bitfield to track if we already mapped something */
static uint8_t *mapped_bitfield;

int pcie_confspace_init(struct capref ram_cap, lpaddr_t base, uint16_t segment, uint8_t startbusarg,
                        uint8_t endbusarg)
{
    errval_t err;

    assert(segment == 0); // unhandled!

    startbus = startbusarg;
    endbus = endbusarg;

    // compute size of region
    size_t region_pages = (endbus + 1 - startbus) << 8;
    size_t region_bytes = region_pages * BASE_PAGE_SIZE;

    region_cap = ram_cap;

    memobj = malloc(sizeof(struct memobj_one_frame_lazy));
    assert(memobj);
    vregion = malloc(sizeof(struct vregion));
    assert(vregion);
    err = memobj_create_one_frame_lazy(memobj, region_bytes, 0, region_cap,
                                       BASE_PAGE_SIZE);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "memobj_create_one_frame_lazy failed");
        return -1;
    }
    err = vregion_map(vregion, get_current_vspace(), &memobj->m, 0, region_bytes,
                      VREGION_FLAGS_READ_WRITE_NOCACHE);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vregion_map failed");
        return -1;
    }

    /* allocate space for bitfield */
    mapped_bitfield = calloc(region_pages / NBBY, 1);
    assert(mapped_bitfield != NULL);

    return 0;
}

static bool is_mapped(uint64_t page)
{
    return mapped_bitfield[page / NBBY] & (1 << page % NBBY);
}

static void set_mapped(uint64_t page)
{
    mapped_bitfield[page / NBBY] |= (1 << page % NBBY);
}

static lvaddr_t map_page(uint64_t page)
{
    errval_t err;

    if (!is_mapped(page)) {
        err = memobj->m.f.pagefault(&memobj->m, vregion, page * BASE_PAGE_SIZE, 0);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "memobj->f.pagefault failed");
            return 0;
        }
        set_mapped(page);
    }

    genvaddr_t genvaddr = vregion_get_base_addr(vregion);
    return genvaddr + page * BASE_PAGE_SIZE;
}

lvaddr_t pcie_confspace_access(struct pci_address addr)
{
    if (vregion == NULL || !pcie_enabled || addr.bus < startbus
        || addr.bus > endbus) {
        return 0;
    }

    uint64_t page = (addr.bus << 8) | (addr.device << 3) | addr.function;
    return map_page(page);
}

void pcie_enable(void)
{
    pcie_enabled = true;
}

void pcie_disable(void)
{
    pcie_enabled = false;
}
