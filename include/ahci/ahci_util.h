/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _AHCI_UTIL_H
#define _AHCI_UTIL_H

#include <barrelfish/barrelfish.h>
#include <ahci/ahci_dma_pool.h>
#include <dev/ahci_port_dev.h>

#define PORT_SIZE 0x80

#define BLOCK_SIZE 512

// The QEMU AHCI emulation only supports PRDs of size 512 bytes
// PR_SIZE can be used together with AHCI_FIXED_PR_SIZE to enforce 
// PRs of fixed size specified in PR_SIZE
// If not forced, PRs will be of arbitrary size with max length 4MB as 
// specified in the AHCI spec
#define PR_SIZE 512
#define MAX_PR_SIZE (128 * 1024)

#define PRDT_OFFSET 0x80

#ifndef CEIL_DIV
#define CEIL_DIV(x, d) (((x) + ((d)-1)) / (d))
#endif

struct ahci_command_slot {
    struct ahci_dma_region *command_table;
    bool in_use;
    void *tag;
};

struct ahci_port_info {
    ahci_port_t port;
    void *mapped_vaddr;
    void *port_base;
    struct ahci_dma_region *command_list;
    struct ahci_dma_region *receive_fis;
    struct ahci_command_slot command_slots[32];
    uint32_t hba_capabilities;
    struct capref hba_cap;
};

errval_t ahci_port_alloc_dma_structs(ahci_port_t *port,
        struct ahci_dma_region **command_list,
        struct ahci_dma_region **receive_fis);

#endif // _AHCI_UTIL_H
