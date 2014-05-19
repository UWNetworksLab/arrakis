/**
 * \file
 * \brief PCIe Initialization
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
#include <mm/mm.h>
#include <pci/confspace/pci_confspace.h>
#include <acpi_client/acpi_client.h>
#include <skb/skb.h>

#include "pci.h"
#include "pci_debug.h"

errval_t pcie_setup_confspace(void) {

    errval_t err;
    uint64_t address;
    uint16_t segment;
    uint8_t sbus;
    uint8_t ebus;

    struct acpi_rpc_client* cl = get_acpi_rpc_client();
    cl->vtbl.get_pcie_confspace(cl, &err, &address, &segment, &sbus, &ebus);
    if (err_is_ok(err)) {

        size_t region_pages = (ebus + 1 - sbus) << 8;
        size_t region_bytes = region_pages * BASE_PAGE_SIZE;
        uint8_t region_bits = log2ceil(region_bytes);

        struct capref pcie_cap;
        struct acpi_rpc_client* acl = get_acpi_rpc_client();
        errval_t error_code;
        err = acl->vtbl.mm_alloc_range_proxy(acl, region_bits, address,
                address + (1UL << region_bits), &pcie_cap, &error_code);
        if (err_is_fail(err)) {
            return err;
        }
        if (err_is_fail(error_code)) {
            return error_code;
        }

        PCI_DEBUG("calling confspace init with: %"PRIu64", %"PRIu16", %"PRIu8", %"PRIu8"\n",
                address, segment, sbus, ebus);
        int r = pcie_confspace_init(pcie_cap, address, segment, sbus, ebus);
        assert(r == 0);
    }

    return err;
}
