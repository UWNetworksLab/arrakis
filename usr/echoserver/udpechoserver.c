/**
 * \file
 * \brief Echo server
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
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
#include <lwip/dhcp.h>
#include <netif/etharp.h>
#include <lwip/init.h>
#include <lwip/udp.h>
#include <netif/bfeth.h>
#include "echoserver.h"

extern void idc_print_statistics(void);
extern void idc_print_cardinfo(void);

extern uint64_t minbase, maxbase;

//called whenever a new datagram for that pcb arrived
static void echo_recv_handler(void *arg, struct udp_pcb *pcb, struct pbuf *pbuf,
                             struct ip_addr *addr, u16_t port)
{
    if ((pbuf->tot_len > 2) && (pbuf->tot_len < 200)) {
        if (strncmp(pbuf->payload, "stat", 4) == 0) {
            idc_print_statistics();
        }
        if (strncmp(pbuf->payload, "cardinfo", 8) == 0) {
            idc_print_cardinfo();
        }
    }
    //send the echo
    struct ip_addr destaddr = *addr;
    udp_sendto(pcb, pbuf, &destaddr, port);
    pbuf_free(pbuf);
}

int udp_echo_server_init(void)
{
    int r;

    uint16_t bind_port = 7; //don't use htons() (don't know why...)


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

    printf("udp_echo_server_init(): bound.\n");
    //install a callback for received datagrams
    udp_recv(pcb, echo_recv_handler, 0 /*client data, arg in callback*/);
    printf("installed receive callback.\n");

    struct pbuf * test_pbuf = pbuf_alloc(PBUF_RAW, 256, PBUF_RAM);
    memcpy(test_pbuf->payload, "DDDDDDDDDUUUUUUUUU", 16);


    netif_default->linkoutput(netif_default, test_pbuf);
    return (0);
}
