/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <net_queue_manager/net_queue_manager.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish/debug.h>
#include <trace/trace.h>

//#include "helper.h"
#include "lo_debug.h"

/// MTU is 1500 bytes, plus Ethernet header and CRC is max packet size
//#define PACKET_SIZE     (1500 + 14 + 4)
#define PACKET_SIZE     (2048)

/// Maximum packet size is write buffer size
#define WRITE_BUF_SIZE  PACKET_SIZE

#define RECEIVE_BUFFER_SIZE (2048) // MAX size of ethernet packet

#if 0
#define MAX_ALLOWED_PKT_PER_ITERATION    (0xff)  // working value
#define DRIVER_RECEIVE_BUFFERS   (1024 * 8) // Number of buffers with driver

#define DRIVER_TRANSMIT_BUFFER   (1024 * 8)

// Data-structure to map sent buffer slots back to application slots
struct pbuf_desc {
    void *opaque;
};
static struct pbuf_desc pbuf_list_tx[DRIVER_TRANSMIT_BUFFER];
//remember the tx pbufs in use

static uint32_t ether_transmit_index = 0, ether_transmit_bufptr = 0;

/* TODO: check if these variables are used */
static uint32_t receive_index = 0, receive_bufptr = 0;
static uint32_t receive_free = 0;
static void **receive_opaque = NULL;

static bool handle_free_TX_slot_fn(void);
#endif // 0

// Buffer registered by net_queue_mgr library
static uint8_t *packetbuf = NULL;
static void *rx_packet_opaque = NULL;

// variable identifying the loopback device
extern bool is_loopback_device;


// FIXME: This is just a placeholder function.  Should be removed
static uint64_t rtl_tx_slots_count_fn(void)
{
    return 1000; // RTL_TX_RING_SIZE;
}

/**
 * \brief Send Ethernet packet.
 *
 * The packet should be a complete Ethernet frame. Nothing is added
 * by the card or the driver.
 *
 */
static errval_t lo_send_ethernet_packet_fn(struct driver_buffer *buffers,
                                           size_t  count)
{
    // Find the length of entire packet
    uint64_t pkt_len = 0;
    for (int idx = 0; idx < count; idx++) {
        pkt_len += buffers[idx].len;
    }

    // copy packet to internal buffer
    LO_DEBUG("sending ethernet packet with opaque %p\n", tx_opaque);
    assert(pkt_len <= WRITE_BUF_SIZE);
    assert(packetbuf != NULL);

    pkt_len = 0;
    for (int idx = 0; idx < count; idx++) {
        memcpy_fast((packetbuf + pkt_len), buffers[idx].va,
                buffers[idx].len);
        pkt_len += buffers[idx].len;
    }

    // we are done with packet copying
    // marking the packetbuf as NULL again so that next slot can be registerd
    packetbuf = NULL;

    // treat it as incoming packet and handle it!
    //process_received_packet(rx_packet_opaque, pkt_len, true);
    /* TODO ak: broken lo
     * sf_process_received_packet_lo(rx_packet_opaque, tx_opaque, pkt_len, true,
            0);*/

    // Tell the client we sent them!!!
    for (int idx = 0; idx < count; idx++) {
        handle_tx_done(buffers[idx].opaque);
    }

    return SYS_ERR_OK;
} // end function: lo_send_ethernet_packet_fn

#if 0
// commented as we receive packet directly in send path
static void lo_receive_packet(void)
{
    assert(!"NYI");
} // end function: lo_receive_packet
#endif // 0

static bool handle_free_TX_slot_fn(void)
{
    return false;
}

/**
 * Callback for net_queue_mgr library. Since we do PIO anyways, we only ever use
 * one buffer.
 */
static uint64_t find_rx_free_slot_count_fn(void)
{
    if (packetbuf == NULL) {
        return 1;
    } else {
        return 0;
    }
}

/** Callback for net_queue_mgr library. */
static errval_t register_rx_buffer_fn(uint64_t paddr, void *vaddr,
        void *rx_opaque)
{
    if (packetbuf != NULL) {
        return ETHERSRV_ERR_TOO_MANY_BUFFERS;
    }

    packetbuf = vaddr;
    rx_packet_opaque = rx_opaque;
    return SYS_ERR_OK;
}


// *********************************************************************
// Global state

// Service name
static char* service_name = "lo";

static uint64_t assumed_queue_id = 0; // queue_id that will be initialized

// Indicates whether we should rely on cache coherence for descriptor rings
static bool cache_coherence = true;

// Indicates whether TX head index write back should be used
static bool use_txhwb = true;

// *****************************************************************
//  MAC address
//  ****************************************************************
static uint8_t macaddr[6] = {10,10,10,10,10,10};

static void get_mac_address_fn(uint8_t *mac)
{
    memcpy(mac, macaddr, sizeof(macaddr));
}


static void parse_cmdline(int argc, char **argv)
{
    int i;
    bool has_queue = false;

    for (i = 1; i < argc; i++) {
        if (strncmp(argv[i], "cardname=", strlen("cardname=") - 1) == 0) {
            service_name = argv[i] + strlen("cardname=");
        } else if (strncmp(argv[i], "queue=", strlen("queue=") - 1) == 0) {
            assumed_queue_id = atol(argv[i] + strlen("queue="));
            has_queue = true;
        } else if (strncmp(argv[i], "cache_coherence=",
                           strlen("cache_coherence=") - 1) == 0) {
            cache_coherence = !!atol(argv[i] + strlen("cache_coherence="));
        } else if (strncmp(argv[i], "head_idx_wb=",
                           strlen("head_idx_wb=") - 1) == 0) {
            use_txhwb = !!atol(argv[i] + strlen("head_idx_wb="));
        } else {
            ethersrv_argument(argv[i]);
        }
    }

    if (!has_queue) {
        USER_PANIC("For queue driver the queue= parameter has to be specified "
                   "on the command line!");
    }
}


static void eventloop(void)
{
    struct waitset *ws;
    errval_t err;

    printf("eventloop()\n");

    ws = get_default_waitset();
    while (1) {
        err = event_dispatch_non_block(ws);
        do_pending_work_for_all();
//        check_for_new_packets();
//        check_for_free_txbufs();
    }
}


// *****************************************************************
//  * Init
// ****************************************************************

static void lo_init(void)
{
    LO_DEBUG("starting loopback device init\n");
    is_loopback_device = true;
    ethersrv_init(service_name, assumed_queue_id, get_mac_address_fn, NULL,
            lo_send_ethernet_packet_fn,
            rtl_tx_slots_count_fn, handle_free_TX_slot_fn,
            PACKET_SIZE, register_rx_buffer_fn, find_rx_free_slot_count_fn);
}



int main(int argc, char **argv)
{
    printf("Started lo_queuemanager\n");
    parse_cmdline(argc, argv);
    lo_init();
    eventloop();
}



#if 0
/*****************************************************************
 * Transmit logic
 ****************************************************************/
/* check if there are enough free buffers with driver,
 * so that packet can be sent
 * */
static bool can_transmit(int numbufs)
{
    uint64_t nr_free;
    assert(numbufs < DRIVER_TRANSMIT_BUFFER);
    if (ether_transmit_index >= ether_transmit_bufptr) {
        nr_free = DRIVER_TRANSMIT_BUFFER -
            ((ether_transmit_index - ether_transmit_bufptr) %
                DRIVER_TRANSMIT_BUFFER);
    } else {
        nr_free = (ether_transmit_bufptr - ether_transmit_index) %
            DRIVER_TRANSMIT_BUFFER;
    }
    return (nr_free > numbufs);
}

static uint64_t transmit_pbuf(uint64_t buffer_address,
                              size_t packet_len, bool last, void *opaque)
{
    assert(!"NYI");
    transmit_ring[ether_transmit_index] = tdesc;
    pbuf_list_tx[ether_transmit_index].opaque = opaque;

    ether_transmit_index = (ether_transmit_index + 1) % DRIVER_TRANSMIT_BUFFER;

    // FIXME: copy the packet to destination memory slot

    LO_DEBUG("ether_transmit_index %"PRIu32"\n", ether_transmit_index);
    /* Actual place where packet is sent.  Adding trace_event here */
#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NO_S,
    		(uint32_t)client_data);
#endif // TRACE_ETHERSRV_MODE

    return 0;
}


/* Send the buffer to device driver TX ring.
 * NOTE: This function will get called from ethersrv.c */
static errval_t transmit_pbuf_list_fn(struct driver_buffer *buffers,
                                      size_t                count,
                                      void                 *opaque)
{
    errval_t r;
    LO_DEBUG("transmit_pbuf_list_fn(count=%"PRIu64")\n", count);
    if (!can_transmit(count)){
        while(handle_free_TX_slot_fn());
        if (!can_transmit(count)){
            return ETHERSRV_ERR_CANT_TRANSMIT;
        }
    }

    for (int i = 0; i < count; i++) {
        r = transmit_pbuf(buffers[i].pa, buffers[i].len,
                    i == (count - 1), //last?
                    opaque);
        if(err_is_fail(r)) {
            //LO_DEBUG("ERROR:transmit_pbuf failed\n");
            printf("ERROR:transmit_pbuf failed\n");
            return r;
        }
        LO_DEBUG("transmit_pbuf done for pbuf 0x%p, index %"PRIu64"\n",
            opaque, i);
    } // end for: for each pbuf
#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET,  TRACE_EVENT_NNET_TXDRVADD,
        (uint32_t)0);
#endif // TRACE_ONLY_SUB_NNET

    return SYS_ERR_OK;
} // end function: transmit_pbuf_list_fn


static uint64_t find_tx_free_slot_count_fn(void)
{

    uint64_t nr_free;
    if (ether_transmit_index >= ether_transmit_bufptr) {
        nr_free = DRIVER_TRANSMIT_BUFFER -
            ((ether_transmit_index - ether_transmit_bufptr) %
                DRIVER_TRANSMIT_BUFFER);
    } else {
        nr_free = (ether_transmit_bufptr - ether_transmit_index) %
            DRIVER_TRANSMIT_BUFFER;
    }

    return nr_free;
} // end function: find_tx_queue_len

static bool handle_free_TX_slot_fn(void)
{
    uint64_t ts = rdtsc();
    bool sent = false;
    volatile struct tx_desc *txd;
    if (ether_transmit_bufptr == ether_transmit_index) {
        return false;
    }

    txd = &transmit_ring[ether_transmit_bufptr];
    if (txd->ctrl.legacy.sta_rsv.d.dd != 1) {
        return false;
    }

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXDRVSEE,
                0);
#endif // TRACE_ONLY_SUB_NNET


    sent = handle_tx_done(pbuf_list_tx[ether_transmit_bufptr].opaque);

    ether_transmit_bufptr = (ether_transmit_bufptr + 1)%DRIVER_TRANSMIT_BUFFER;
    netbench_record_event_simple(bm, RE_TX_DONE, ts);
    return true;
}

static errval_t rx_register_buffer_fn(uint64_t paddr, void *vaddr,
        void *opaque)
{
    return add_desc(paddr, opaque);
}

static uint64_t rx_find_free_slot_count_fn(void)
{
    return DRIVER_RECEIVE_BUFFERS - receive_free;
}
#endif // 0


