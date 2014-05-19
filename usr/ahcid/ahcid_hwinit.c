/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "ahcid.h"
#include <ahci/ahci_dma_pool.h>
#include <ahci/ahci_util.h>

uint32_t ahcid_port_offset(uint32_t port)
{
    return 0x100 + port * 0x80;
}

errval_t ahcid_ports_init(struct ahcid_port_info **ports, size_t num_ports,
        uint32_t active_ports_bf, void* base_address)
{
    errval_t r;
    int i;
    for (i = 0; i < num_ports; i++) {
        if ((active_ports_bf >> i)&0x1) {
            // only initialize implemented ports
            ports[i] = calloc(1, sizeof(struct ahcid_port_info));
            if (ports[i] == NULL) {
                return LIB_ERR_MALLOC_FAIL;
            }

            ahci_port_initialize(&ports[i]->port, base_address + ahcid_port_offset(i));

            // setup dma regions for command list and receive FIS area
            r = ahci_port_alloc_dma_structs(&ports[i]->port,
			    &ports[i]->command_list, &ports[i]->receive_fis);
            assert(err_is_ok(r));

            // enable rFIS area
            ahci_port_cmd_t cmd = ahci_port_cmd_rd(&ports[i]->port);
            cmd = ahci_port_cmd_fre_insert(cmd, 1);
            ahci_port_cmd_wr(&ports[i]->port, cmd);
        } else {
            ports[i] = NULL;
        }
    }
    return 0;
}

