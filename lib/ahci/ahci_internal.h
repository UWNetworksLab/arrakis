/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _AHCI_INTERNAL_H
#define _AHCI_INTERNAL_H

void ahci_dump_command(int command, struct ahci_port_info *port);

void ahci_dump_rfis(struct ahci_port_info *port);

int ahci_find_free_command_slot(struct ahci_port_info *port);

void ahci_port_free_dma_structs(struct ahci_port_info *port);

errval_t ahci_setup_command(int *command, struct ahci_port_info *port,
        uint8_t *fis, size_t fis_length, size_t num_prds, bool is_write);

errval_t ahci_add_physical_regions(struct ahci_port_info *port, int command,
        struct ahci_dma_region *buf, size_t buflen);


#endif
