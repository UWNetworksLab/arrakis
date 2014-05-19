/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BFETH_H
#define BFETH_H

#include <lwip/netif.h>

void bfeth_input(struct netif *netif, uint64_t pbuf_id, uint64_t paddr,
        uint64_t len, uint64_t packet_len, struct pbuf *pp);
err_t bfeth_init(struct netif *netif);

#endif
