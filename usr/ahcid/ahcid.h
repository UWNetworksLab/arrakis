/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef AHCID_H_
#define AHCID_H_
#include <barrelfish/barrelfish.h>
#include "ahci_port_dev.h"
#include <ahci/ahci_dma_pool.h>

enum ahcid_port_status {
    AHCID_PORT_STATUS_UNINITIALIZED = 0,
    AHCID_PORT_STATUS_IDLE = 1,
    AHCID_PORT_STATUS_IDENTIFY_PENDING,
};

struct ahcid_port_info {
    ahci_port_t port;
    struct ahci_dma_region *command_list;
    struct ahci_dma_region *receive_fis;
    enum ahcid_port_status status;
    // XXX: use data in ahci_port_t?
    struct ahci_dma_region **prdts;
    size_t prdt_count;
    uint16_t identify_data[256];
    struct ahci_mgmt_binding *binding;
};

uint32_t ahcid_port_offset(uint32_t port);
errval_t ahcid_ports_init(struct ahcid_port_info **ports, size_t num_ports,
		uint32_t active_ports_bf, void* base_address);

#endif //AHCID_H_
