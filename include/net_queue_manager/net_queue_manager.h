/**
 * \file
 * \brief Header file for net_queue_manager.h
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef net_queue_manager_H
#define net_queue_manager_H

#include <barrelfish/barrelfish.h>
#include <barrelfish/bulk_transfer.h>
#include <contmng/contmng.h>
#include <contmng/netbench.h>
#include <procon/procon.h>
#include <if/net_queue_manager_defs.h>
#include <barrelfish/net_constants.h>
#include <net_interfaces/flags.h>

/*****************************************************************
 * Constants:
 *****************************************************************/

#define MAX_NR_TRANSMIT_PBUFS 80

#define RECEIVE_CONNECTION 0
#define TRANSMIT_CONNECTION 1

/*****************************************************************
 * common data-structures
 *****************************************************************/

#define MAX_PAUSE_BUFFER        10

struct bufdesc {
    char pkt_data[1600];
    size_t pkt_len;
    uint64_t flags;
};

struct filter {
    uint64_t filter_id;
    uint64_t filter_type;
    uint8_t *data;
    int32_t len;
    bool paused;
    struct bufdesc pause_buffer[MAX_PAUSE_BUFFER];
    int pause_bufpos;
    struct buffer_descriptor *buffer;
    struct filter *next;
};

// State required in TX path to remember information about buffers
struct buffer_state_metadata {
    struct net_queue_manager_binding *binding;
    uint64_t offset;
//    uint64_t spp_index;
//    uint64_t tx_pending;
};

struct bsm_queue {
    size_t buffer_state_size; // size of slot queue
    size_t buffer_state_head; // the index pointer
    size_t buffer_state_used; // how many of them are used?
    struct buffer_state_metadata *buffer_state;  // array of slots
};


struct buffer_descriptor {
    uint64_t buffer_id;  // buffer identifier
    struct net_queue_manager_binding *con; // binding to which buffer belongs
    struct capref cap; // cap backing the buffer memory
//    struct shared_pool_private *spp_prv; // shared producer consumer pool

    struct bsm_queue rxq; // queue for receive path
    struct bsm_queue txq; // queue for send path

    uint8_t role;  // Role of buffer (RX/TX)
    lpaddr_t pa;    // Physical address of buffer
    uint64_t bits;  // Size of buffer (encoded in bits)
    void *va;       // Virtual address of buffer
    uint64_t queueid; // The queueid to which this buffer belongs

    struct buffer_descriptor *next; // The next buffer (in singly linked list)
    struct filter* tx_filters;  // List of filters associated with this buf
};

enum pbuf_lifecycle {
    PBUF_REGISTERED,
    PKT_RECEIVED,
    RECV_NOTIFICATION_GENERATED,
    RECV_NOTIFICATION_SENT,
    PKT_ARRIVED_APP, // From here bellow, everything will be in APP
    PKT_REACHED_APP,
    PBUF_REGISTER_STARTED,
    PBUF_REGISTER_GENERATED,
    PBUF_REGISTER_SENT
};


struct driver_buffer {
    uint64_t pa;
    void    *va;
    size_t   len;
    void    *opaque;
    uint64_t flags;
};

struct driver_rx_buffer {
    size_t len;
    void  *opaque;
};


#define MAX_STAT_EVENTS   5   // This is the count of pbuf_lifecycle events
#define MAX_CHUNKS   5 // Max no. of allowed chunks for single packet


/* This is client_closure for network service */
struct client_closure {
    int cl_no;  // Client indentifier
    struct buffer_descriptor *buffer_ptr; // buffer associated with client
    struct shared_pool_private *spp_ptr; // shared pool associted with client

    /* Following two are used by packet transmit logic */
    uint64_t tx_index;  // index of which is next slot to be sent
    uint64_t rx_index;  // index of which is next slot to be received

    uint64_t queueid; // The queueid to which this buffer belongs
    struct net_queue_manager_binding *app_connection; // Binding pointer to talk back
    struct cont_queue *q; // Cont management queue to report events


    // Place to store data when there are multiple parts to the packet
    struct driver_buffer driver_buff_list[MAX_CHUNKS]; // list of already seen chunks
    uint8_t chunk_counter; // How many chunks are already seen?

    // For debugging and benchmarking help
    uint8_t debug_print; // To control connection level debug prints
    uint8_t debug_state;  // debug state for rx benchmark
    uint8_t debug_state_tx; // debug state of tx benchmark
    uint64_t start_ts;  // timestap of start of rx benchmark
    uint64_t start_ts_tx; // timestamp of start of tx benchmark
    uint64_t out_trigger_counter;  // index marking when to stop tx benchmark


    // Some statistics.  Not needed for functionality
    uint64_t pkt_count;  // # packets
    uint64_t in_filter_matched;  // # total filters matched
    uint64_t in_filter_matched_f; // # failed processing of matched filter
    uint64_t in_filter_matched_p; // # successful processing of matched filter

    uint64_t dropped_pkt_count;  // # packets dropped (for no space in hw-queue)
    uint64_t pbuf_count;  // # pbufs sent
    uint64_t in_dropped_app_buf_full; // # packets dropped for lack of buffers
}; /* holds info about how much data is transferred to NIC. */

/*****************************************************************
 * Driver states
 * ***************************************************************/


/*******************************************************************
 *  Following functions must be implemented by the driver which is
 *  using the library.
 ******************************************************************/
typedef void (*ether_get_mac_address_t)(uint8_t *mac);
typedef void (*ether_terminate_queue)(void);
typedef errval_t (*ether_transmit_pbuf_list_t)(
    struct driver_buffer *buffers,
    size_t                count);
typedef uint64_t (*ether_get_tx_free_slots)(void);
typedef bool (*ether_handle_free_TX_slot)(void);
typedef errval_t (*ether_rx_register_buffer)(uintptr_t paddr, void *vaddr,
                                               void *opaque);
typedef uint64_t (*ether_rx_get_free_slots)(void);



/*****************************************************************/

/**
 * Initialize pair of ethernet RX/TX queues.
 *
 * @param service_name             Service name for the card to which this queue
 *                                   belongs
 * @param queueid                  Queue index
 * @param get_mac_ptr              Callback that returns MAC address of the card
 * @param terminate_queue_ptr      Callback that terminates the queue driver, or
 *                                   NULL if this is not supported.
 * @param transmit_ptr             Callback to put a buffer chain in TX queue
 * @param tx_free_slots_ptr        Callback that returns number of free slots in
 *                                   TX queue
 * @param handle_free_tx_slots_ptr Callback to remove transmitted buffers from
 *                                   queue
 * @param rx_buffer_size           Expected RX buffer size
 * @param rx_register_buffer_ptr   Callback to register buffer for RX queue
 * @param rx_get_free_slots_ptr    Callback that returns number of free slots in
 *                                   RX queue
 */
void ethersrv_init(
    char *service_name,
    uint64_t queueid,
    ether_get_mac_address_t     get_mac_ptr,
    ether_terminate_queue       terminate_queue_ptr,
    ether_transmit_pbuf_list_t  transmit_ptr,
    ether_get_tx_free_slots     tx_free_slots_ptr,
    ether_handle_free_TX_slot   handle_free_tx_slots_ptr,
    size_t                      rx_buffer_size,
    ether_rx_register_buffer    rx_register_buffer_ptr,
    ether_rx_get_free_slots     rx_get_free_slots_ptr);


bool waiting_for_netd(void);

/**
 * Pass command line argument not used by driver to library. Can be called
 * multiple times, once for each parameter.
 */
void ethersrv_argument(const char* arg);

bool handle_tx_done(void *opaque);

struct buffer_descriptor *find_buffer(uint64_t buffer_id);

void process_received_packet(struct driver_rx_buffer *buffer, size_t count,
    uint64_t flags);

// For local loopback device
void sf_process_received_packet_lo(void *opaque_rx, void *opaque_tx,
        size_t pkt_len, bool is_last, uint64_t flags);

/* for frag.c */
bool handle_fragmented_packet(void* packet, size_t len, uint64_t flags);


struct filter *execute_filters(void *data, size_t len);

/* FIXME: put this into the local include file.  */
bool copy_packet_to_user(struct buffer_descriptor* buffer,
				void *data, uint64_t len, uint64_t flags);

void do_pending_work_for_all(void);

//debug
void print_statistics(void);


// For recording statistics

enum Recorded_Events {
    RE_ALL = 0,
    RE_FILTER,
    RE_COPY,
    RE_DROPPED,
    RE_USEFUL, // used
    RE_PBUF_REG, // used
    RE_PROCESSING_ALL, // used
    RE_RX_ALL, // used
    RE_PENDING_WORK,
    RE_PENDING_1,
    RE_PENDING_2,
    RE_PENDING_3,
    RE_PENDING_4,

    RE_TX_NOTI_CS,
    RE_TX_T,
    RE_TX_SP_S,
    RE_TX_SP_F,
    RE_TX_DONE,
    RE_TX_W_ALL,
    RE_TX_DONE_NN,
    RE_TX_DONE_N,
    RE_TX_SP_MSG,
    RE_TX_SP_MSG_Q,
    EVENT_LIST_SIZE
};

extern struct netbench_details *bm;

// **************************************
// Use of optimised memcpy for SCC

void *memcpy_fast(void *dst0, const void *src0, size_t length);

#endif // net_queue_manager_H
