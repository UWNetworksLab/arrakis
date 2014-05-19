/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef Queue_Manager_local_H_
#define Queue_Manager_local_H_
#include <net_queue_manager/net_queue_manager.h>

// registered buffers:
extern struct buffer_descriptor *buffers_list;

/* NETD connections */
#define NETD_BUF_NR 2
struct net_queue_manager_binding *netd[NETD_BUF_NR];

// Measurement purpose, counting interrupt numbers
extern uint64_t interrupt_counter;
extern uint64_t total_rx_p_count;
extern uint64_t total_interrupt_time;
extern struct client_closure *g_cl;
extern uint64_t total_rx_datasize;

// Function prototypes for ether services
//struct buffer_descriptor *find_buffer(uint64_t buffer_id);

// Function prototypes for ether_control service
void init_soft_filters_service(char *service_name, uint64_t qid,
                               size_t rx_bufsz);
void sf_process_received_packet(struct driver_rx_buffer *buf, size_t count,
                                uint64_t flags);


// To get the mac address from device
uint64_t get_mac_addr_from_device(void);

// Fn ptrs to manage receive buffers in queue
extern ether_rx_register_buffer rx_register_buffer_fn_ptr;
extern ether_rx_get_free_slots rx_get_free_slots_fn_ptr;

// support for loopback device
extern bool is_loopback_device;
struct buffer_descriptor *get_lo_receiver(void *opaque);

void* sf_rx_ring_buffer(void *opaque);

#endif // Queue_Manager_local_H_

