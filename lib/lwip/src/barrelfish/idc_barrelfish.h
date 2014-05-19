/**
 * \file
 * \brief Header file for the interfaceing part to the network driver
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

#ifndef IDC_BARRELFISH_H_
#define IDC_BARRELFISH_H_

#include <barrelfish/barrelfish.h>
#include "lwip/pbuf.h"
#include <lwip/ip_addr.h>
#include <if/net_queue_manager_defs.h>
#include <procon/procon.h>
#include <contmng/contmng.h>

/**
 * \brief Receive and transmit sides
 *
 * The link to the network driver is composed by two distinct
 * channel. We identify these channels thanks to the following
 * constants.
 *
 */
#define RECEIVE_CONNECTION 0
#define TRANSMIT_CONNECTION 1

struct buffer_desc {
    struct capref cap;
    struct net_queue_manager_binding *con;
    lpaddr_t pa;
    void *va;
    size_t size;
    uint64_t buffer_id;  // Assigned by the network driver after registering
    uint8_t  role;  // RX or TX buffer
    struct shared_pool_private *spp_prv;
    struct buffer_desc *next;
};

/* closure for network card client */
struct client_closure_NC {
    struct cont_queue *q;       /* queue to continuation */
    struct buffer_desc *buff_ptr;
    struct shared_pool_private *spp_ptr;
    uint8_t benchmark_status;
    uint64_t benchmark_delta;
    uint64_t benchmark_cl;
    uint64_t queueid; // allocated queue id
    uint8_t  role;  // RX or TX buffer
};



void idc_connect_port_manager_service(char *service_name);
void idc_connect_to_driver(char *card_name, uint64_t queueid);

uint64_t idc_send_packet_to_network_driver(struct pbuf *p);
void idc_register_buffer(struct buffer_desc *buff_ptr,
        struct shared_pool_private *spp_ptr, uint8_t binding_index);
void idc_get_mac_address(uint8_t * mac);
int lwip_check_sp_capacity(int direction);
int idc_check_capacity(int direction);
uint64_t idc_check_driver_load(void);
uint64_t idc_get_packet_drop_count(void);


void idc_register_receive_callback(void (*f)
                                    (void *, uint64_t, uint64_t, uint64_t,
                                     uint64_t, struct pbuf *), void *data);
uint64_t idc_get_next_packet(uint8_t * packet);
void idc_register_pbuf(uint64_t pbuf_id, uint64_t paddr, uint64_t len);
void idc_register_freeing_callback(void (*f) (struct pbuf *));
void idc_print_statistics(void);
void idc_print_cardinfo(void);
void network_polling_loop(void);
void idc_benchmark_control(int connection, uint8_t state, uint64_t trigger,
        uint64_t cl);
uint64_t perform_lwip_work(void);

uint8_t get_driver_benchmark_state(int direction, uint64_t *delta,
        uint64_t *cl);
void debug_show_spp_status(int connection);

// FIXME: following code needs cleaning
/* netd services */
void idc_connect_netd(void);

void idc_get_ip(void);
err_t idc_tcp_new_port(uint16_t * port_no);
err_t idc_udp_new_port(uint16_t * port_no);
err_t idc_redirect_tcp(struct ip_addr *local_ip,
                       uint16_t local_port, struct ip_addr *remote_ip,
                       uint16_t remote_port);
err_t idc_close_udp_port(uint16_t port);
err_t idc_close_tcp_port(uint16_t port);
err_t idc_bind_udp_port(uint16_t port);
err_t idc_bind_tcp_port(uint16_t port);
err_t idc_pause_tcp(struct ip_addr *local_ip, u16_t local_port,
                    struct ip_addr *remote_ip, u16_t remote_port);



// ************************************************************************
//                 ARP lookup interface function
// ************************************************************************

// connect to ARP lookup service
void idc_connect_ARP_lookup_service(char *service_name);

// get ip address from netd
void idc_get_ip_from_ARP_lookup(void);

// Request ARP lookup from netd
uint64_t idc_ARP_lookup(uint32_t ip);

#endif // IDC_BARRELFISH_H_
