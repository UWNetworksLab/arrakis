/**
 * \file
 * \brief Intel e1000 driver: Prototypes
 *
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _EMAC_DRIVER_H_
#define _EMAC_DRIVER_H_
#include <barrelfish/barrelfish.h>
#include <net_queue_manager/net_queue_manager.h>
#include "eMAC_dev.h"
#include "eMAC_debug.h"


errval_t transmit_pbuf_list(struct client_closure *cl);
void eMAC_hwinit(uint8_t phy_id);
uint64_t get_tx_free_slots_count(void);
uint32_t RX_max_slots; // = SLOTS - 1;


#endif // _EMAC_DRIVER_H_
