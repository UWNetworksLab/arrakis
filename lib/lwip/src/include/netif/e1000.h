/*
 * Copyright (c) 2007, 2008, 2009, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef E1000_H
#define E1000_H

#include <lwip/netif.h>
#include <net_queue_manager/net_queue_manager.h>

err_t ethernetif_init(struct netif *netif);
void e1000n_polling_loop(struct waitset *ws);
bool e1000n_queue_empty(void);
int e1000n_driver_init(int argc, char **argv);

void ethernetif_backend_init(char *service_name, uint64_t queueid,
                             ether_get_mac_address_t get_mac_ptr,
                             ether_terminate_queue terminate_queue_ptr,
                             ether_transmit_pbuf_list_t transmit_ptr,
                             ether_get_tx_free_slots tx_free_slots_ptr,
                             ether_handle_free_TX_slot handle_free_tx_slot_ptr,
                             size_t rx_bufsz,
                             ether_rx_register_buffer rx_register_buffer_ptr,
                             ether_rx_get_free_slots rx_get_free_slots_ptr);

void lwip_arrakis_start(int *argc, char ***argv);

void arranet_polling_loop(void);

#endif
