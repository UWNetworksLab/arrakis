/**
 * \file
 * \brief HyperTransport Configuration driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>

#include "ht_config.h"
#include "ht_config_dev.h"

#include "pci_debug.h"

static void route(unsigned int rt)
{
    if(rt & (1 << 0)) {
        printf("Self ");
    }
    if(rt & (1 << 1)) {
        printf("Link 0[0] ");
    }
    if(rt & (1 << 2)) {
        printf("Link 1[0] ");
    }
    if(rt & (1 << 3)) {
        printf("Link 2[0] ");
    }
    if(rt & (1 << 4)) {
        printf("Link 3[0] ");
    }
    if(rt & (1 << 5)) {
        printf("Link 0[1] ");
    }
    if(rt & (1 << 6)) {
        printf("Link 1[1] ");
    }
    if(rt & (1 << 7)) {
        printf("Link 2[1] ");
    }
    if(rt & (1 << 8)) {
        printf("Link 3[1] ");
    }
}

void ht_config_init(struct pci_address pciaddr, struct device_mem *bar_info,
                    uint64_t nr_mapped_regions, uint32_t irq)
{
    struct ht_config_t ht;
    ht_config_nodeid_t nodeid;

    ht_config_initialize(&ht, pciaddr);

    pcie_disable();

    nodeid = ht_config_nodeid_rd(&ht);
    printf("From Node %d:\n", nodeid.nodeid);

    for(int i = 0; i < 8; i++) {
        ht_config_rtnode_t node = ht_config_rtnodes_rd(&ht, i);

        printf("To Node %d: Request ", i); route(node.rqrte);
        printf(", Response "); route(node.rprte);
        printf(", Broadcast "); route(node.bcrte);
        printf("\n");
    }

    pcie_enable();
}
