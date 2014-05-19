/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <assert.h>
#include <lwip/netif.h>
#include <lwip/udp.h>
#include <lwip/ip_addr.h>
#include <netif/bfeth.h>
#include <trace/trace.h>
#include "tcp_server_bm.h"

//called whenever a new datagram for that pcb arrived
static void udp_recv_handler_bm(void *arg, struct udp_pcb *pcb,
        struct pbuf *p, struct ip_addr *addr, u16_t port)
{
    // Send the data to benchmarking code for furthur analysis
    handle_data_arrived(p->payload, p->len);

    //send the echo
    struct ip_addr destaddr = *addr;
    udp_sendto(pcb, p, &destaddr, port);
    pbuf_free(p);
}

int udp_server_bm_init(uint16_t bind_port)
{
    err_t r;

    //create a new UDP PCB
    struct udp_pcb *pcb = udp_new(); //UDP connection data
    if (pcb == NULL) {
        return ERR_MEM;
    }

    //bind it to every IP of every interface and define a specific port to
    //bind to
    r = udp_bind(pcb, IP_ADDR_ANY, bind_port);
    if(r != ERR_OK) {
        udp_remove(pcb);
        return(r);
    }

    //install a callback for received datagrams
    udp_recv(pcb, udp_recv_handler_bm, 0);
    printf("installed receive callback for server.\n");
    printf("udp_server_bm_init (): bound.\n");
    return (0);
} // end function: udp_server_bm_init

// *********************************************************************

// ***************************************************************
// udp client code
// ***************************************************************

// A client pcb
static struct udp_pcb *client_pcb = NULL;

// single pbuf that will be used by client to send over and over again.
static struct pbuf *udp_pbuf_cl = NULL;

static void *payload_ptr = NULL;
static size_t udp_msg_size = 0;

//called whenever a new datagram for that pcb arrived
static void udp_recv_handler_bm_client(void *arg, struct udp_pcb *pcb,
        struct pbuf *p, struct ip_addr *addr, u16_t port)
{
    // Send the data to benchmarking code for furthur analysis
    handle_data_arrived(p->payload, p->len);
    pbuf_free(p);
}

// initialize udp connection for the client
int udp_client_bm_init(char *ip_addr_str,  uint16_t server_port)
{
    err_t r;

    // Preparing IP address for use
    assert(ip_addr_str != NULL);
    struct in_addr addr;
    if (inet_aton(ip_addr_str, &addr) == 0) {
        printf("Invalid IP addr: %s\n", ip_addr_str);
        USER_PANIC("Invalid IP address %s", ip_addr_str);
        return -1;
    }
    struct ip_addr server_addr;
    server_addr.addr = addr.s_addr;

    // Prepare udp_pcb
    client_pcb = udp_new();
    if (client_pcb == NULL) {
        USER_PANIC("udp_new failed");
        return -1;
    }

    // Connecting to given IP address, port no.
    //don't use htons() on port no. (don't know why...)
    r = udp_connect(client_pcb, &server_addr, server_port);
    if(r != ERR_OK) {
        USER_PANIC("udp_connect failed");
        return(r);
    }

    udp_recv(client_pcb, udp_recv_handler_bm_client, 0);

    // Connection established!
    printf("udp benchmark client started\n");
    handle_connection_opened();
    return (0);
} // end function: udp_client_bm_init


void *prepare_udp_buffer(size_t payload_size)
{

//    udp_pbuf_cl = pbuf_alloc(PBUF_TRANSPORT, payload_size, PBUF_POOL);
    udp_pbuf_cl = pbuf_alloc(PBUF_TRANSPORT, payload_size, PBUF_RAM);
    if (udp_pbuf_cl == NULL){
        USER_PANIC("pbuf_alloc failed in prepare_udb_buffer");
        return NULL;
    }
    assert(udp_pbuf_cl != NULL);
    assert(udp_pbuf_cl->payload != NULL);
    assert(udp_pbuf_cl->len == payload_size);
    payload_ptr = udp_pbuf_cl->payload;
    udp_msg_size = payload_size;
    return udp_pbuf_cl->payload;
}

// send single message over udp connection
int send_udp_message_client(void)
{
    err_t err;

    // resetting the values as they will be changed by
    // pbuf_header function
    udp_pbuf_cl->len = udp_msg_size;
    udp_pbuf_cl->tot_len = udp_msg_size;
    udp_pbuf_cl->payload = payload_ptr;

    err = udp_send(client_pcb, udp_pbuf_cl);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "udp_send failed");
        return -1;
    }
    return 0;
} // end function: send_message_client

