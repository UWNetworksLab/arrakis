/**
 * \file
 * \brief LWIP test/demo code
 */

/*
 * Copyright (c) 2013, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <netif/e1000.h>

static ether_terminate_queue ether_terminate_queue_ptr = NULL;
static ether_get_mac_address_t ether_get_mac_address_ptr = NULL;
static ether_transmit_pbuf_list_t ether_transmit_pbuf_list_ptr = NULL;
static ether_get_tx_free_slots tx_free_slots_fn_ptr = NULL;
static ether_handle_free_TX_slot handle_free_tx_slot_fn_ptr = NULL;
static ether_rx_register_buffer rx_register_buffer_fn_ptr = NULL;
static ether_rx_get_free_slots rx_get_free_slots_fn_ptr = NULL;

void ethernetif_backend_init(char *service_name, uint64_t queueid,
                             ether_get_mac_address_t get_mac_ptr,
                             ether_terminate_queue terminate_queue_ptr,
                             ether_transmit_pbuf_list_t transmit_ptr,
                             ether_get_tx_free_slots tx_free_slots_ptr,
                             ether_handle_free_TX_slot handle_free_tx_slot_ptr,
                             size_t rx_bufsz,
                             ether_rx_register_buffer rx_register_buffer_ptr,
                             ether_rx_get_free_slots rx_get_free_slots_ptr)
{
    ether_terminate_queue_ptr = terminate_queue_ptr;
    ether_get_mac_address_ptr = get_mac_ptr;
    ether_transmit_pbuf_list_ptr = transmit_ptr;
    tx_free_slots_fn_ptr = tx_free_slots_ptr;
    handle_free_tx_slot_fn_ptr = handle_free_tx_slot_ptr;
    rx_register_buffer_fn_ptr = rx_register_buffer_ptr;
    rx_get_free_slots_fn_ptr = rx_get_free_slots_ptr;
    /* printf("PBUF_POOL_BUFSIZE = %u, rx buffer size = %zu\n", PBUF_POOL_BUFSIZE, */
    /*        rx_bufsz); */
}

void process_received_packet(struct driver_rx_buffer *buffer, size_t count,
                             uint64_t flags)
{
#if 0
    // Drop packets with invalid checksums
    if(flags & NETIF_RXFLAG_IPCHECKSUM) {
        if(!(flags & NETIF_RXFLAG_IPCHECKSUM_GOOD)) {
            goto out;
        }
    }

    if(flags & NETIF_RXFLAG_L4CHECKSUM) {
        if(!(flags & NETIF_RXFLAG_L4CHECKSUM_GOOD)) {
            goto out;
        }
    }

    // TODO: Do something with the packet

 out:
    //now we have consumed the preregistered pbuf containing a received packet
    //which was processed in this function. Therefore we have to register a new
    //free buffer for receiving packets.
    errval_t err;
    do {
        err = rx_register_buffer_fn_ptr(p->pa, p->payload, p);
    } while(err_is_ok(err));
#endif
}

bool handle_tx_done(void *opaque)
{
    return true;
}

int main(int argc, char *argv[])
{
    uint8_t mac[6];

    printf("Starting e10k control plane...\n");

    e1000n_driver_init(argc, argv);

    ether_get_mac_address_ptr(mac);
    printf("Control plane MAC address %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx\n",
           mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);

    // TODO: Add buffers to RX ring for packet reception

    for(;;) {
        e1000n_polling_loop(get_default_waitset());
    }

    return 0;
}
