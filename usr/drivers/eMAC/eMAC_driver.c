/**
 * \file
 * \brief SCC eMAC driver.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <net_queue_manager/net_queue_manager.h>
#include "eMAC_driver.h"


/// The only instance of the RTL8029AS we're handling
//static eMAC_t emac;

static uint64_t assumed_queue_id = 0; // queue_id that will be initialized

uint64_t eMAC_mac;
/// This buffers the card's MAC address upon card reset

/* driver will initially copy the packet here. */
//static uint8_t packetbuf[PACKET_SIZE];

/* the length of packet copied into packetbuf */
//static uint16_t packet_length;


/**
 * \brief Send Ethernet packet.
 *
 * The packet should be a complete Ethernet frame. Nothing is added
 * by the card or the driver.
 *
 */
/* FIXME: Modify the transmit_pbuf_list function to work with driver_buffer
 * datastructure instead of working with procon library.  */
//static errval_t EMAC_send_ethernet_packet_fn(struct client_closure *cl)
static errval_t transmit_pbuf_list_fn(struct driver_buffer *buffers,
                                      size_t                count)
{
    EMAC_DEBUG("send pkt function called\n");
    assert(!"Older implementation, will not work without modification");
    return (ETHERSRV_ERR_CANT_TRANSMIT);
//    return (transmit_pbuf_list(cl));
}


static void get_mac_address_fn(uint8_t *mac)
{
    uint64_t mac_value = eMAC_mac;

    for (int i = 5; i != 0; i--) {
        mac[i] = mac_value & 0xFF;
        mac_value >>= 8;
    }
}

static bool handle_free_TX_slot_fn(void)
{
    // FIXME: Need better implementation of this function
    // FIXME: where is notify_client_next_free_tx for this driver
    return false;
}

static uint64_t rx_find_free_slot_count_fn(void)
{
    // FIXME: temparary hardcoded value for free slot count
    return RX_max_slots - 100;
}




static void EMAC_init(uint8_t phy_id, char *service_name)
{

    EMAC_DEBUG("starting hardware init\n");
    eMAC_hwinit(phy_id);
    /* FIXME: do hardware init*/
    EMAC_DEBUG("Done with hardware init\n");

    ethersrv_init(service_name, assumed_queue_id, get_mac_address_fn,
            NULL,
            transmit_pbuf_list_fn,
            get_tx_free_slots_count,
            handle_free_TX_slot_fn,
            RX_max_slots,   // from eMAC_hwinit.c file
            NULL, // only needed when application owns the queue
            rx_find_free_slot_count_fn
            );
}



//this functions polls all the client's channels as well as the transmit and
//receive descriptor rings
static void polling_loop(void)
{
    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
//        EMAC_DEBUG("inside event dispatch\n");
/*        notify_client_next_free_tx();
*/
    }
    printf("ERROR: End of polling\n");
}


int main(int argc, char *argv[])
{
    char *service_name = 0;
    uint8_t device = 2;

    EMAC_DEBUG("Starting EMAC standalone driver.....\n");
    for (int i = 0; i < argc; i++) {
        EMAC_DEBUG("arg %d = %s\n", i, argv[i]);
        if(strncmp(argv[i],"servicename=",strlen("servicename=")-1)==0) {
            service_name = argv[i] + strlen("servicename=");
            EMAC_DEBUG("service name = %s\n", service_name);
        }
        if(strncmp(argv[i],"device=",strlen("device=")-1)==0) {
            device = atoi(argv[i] + strlen("device="));
            EMAC_DEBUG("device = %u\n", device);
        }
    }

    if (service_name == 0) {
        uint8_t size = 16;
        service_name = (char *)malloc(size);
        snprintf(service_name, size, "eMAC%d_%d", device, disp_get_core_id());
        /* FIXME: make sure that there are no white-spaces in name */
    }
    EMAC_DEBUG("service name = [%s]\n", service_name);

    printf("Starting EMAC for hardware\n");

    // Initialize driver
    EMAC_init(device, service_name);
    EMAC_DEBUG("registered the driver.....\n");

    EMAC_DEBUG("starting to poll\n");
    polling_loop(); //loop myself
}

