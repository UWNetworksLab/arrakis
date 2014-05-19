/**
 * \file
 * \brief Echo server
 */

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
#include <lwip/dhcp.h>
#include <netif/etharp.h>
#include <lwip/init.h>
#include <lwip/tcp.h>
#include <netif/bfeth.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include "echoserver.h"


extern void idc_print_statistics(void);
extern void idc_print_cardinfo(void);

extern uint64_t minbase, maxbase;

#if TRACE_ONLY_SUB_NNET
static size_t n64b = 0;
#endif

static void echo_server_close(struct tcp_pcb *tpcb)
{
    tcp_arg(tpcb, NULL);
    tcp_close(tpcb);
}


static void echo_server_err(void *arg, err_t err)
{
    printf("echo_server_err! %p %d\n", arg, err);
}


static err_t echo_server_recv(void *arg, struct tcp_pcb *tpcb, struct pbuf *p,
                              err_t err)
{
    int r;
    if (p == NULL) {
        // close the connection
        echo_server_close(tpcb);
        return ERR_OK;
    }

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_RXAPPRCV,
                0);
#endif // TRACE_ONLY_SUB_NNET

    /* don't send an immediate ack here, do it later with the data */
    tpcb->flags |= TF_ACK_DELAY;
    assert(p->next == 0);

    /*if ((p->tot_len > 2) && (p->tot_len < 200)) {
        if (strncmp(p->payload, "stat", 4) == 0) {
            idc_print_statistics();
        }
        if (strncmp(p->payload, "cardinfo", 8) == 0) {
            idc_print_cardinfo();
        }
        if (strncmp(p->payload, "lwipinfo", 8) == 0) {
            printf("echoserver's memory affinity: [0x%lx, 0x%lx]\n",
                minbase, maxbase);
        }
    }*/

#if TRACE_ONLY_SUB_NNET
    if (p->tot_len == 64) {
        n64b++;
        if (n64b == 5) {
            trace_control(TRACE_EVENT(TRACE_SUBSYS_NNET,
                                      TRACE_EVENT_NNET_START, 0),
                          TRACE_EVENT(TRACE_SUBSYS_NNET,
                                      TRACE_EVENT_NNET_STOP, 0), 0);
            trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_START, 0);
        } else if (n64b == 8) {
            trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_STOP, 0);
            char* trbuf = malloc(4096*4096);
            size_t length = trace_dump(trbuf, 4096*4096, NULL);
            printf("%s\n", trbuf);
            printf("length of buffer %zu\n", length);
            free(trbuf);
        }
    }


    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXAPPSNT, 0);
#endif


    //XXX: can we do that without needing to copy it??
    r = tcp_write(tpcb, p->payload, p->len, TCP_WRITE_FLAG_COPY);
    assert(r == ERR_OK);

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TX_TCP_WRITE, 0);
#endif

    // make sure data gets sent immediately
    r = tcp_output(tpcb);
    assert(r == ERR_OK);

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TX_TCP_OUTPUT, 0);
#endif

    tcp_recved(tpcb, p->len);
#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TX_TCP_RECV, 0);
#endif


    //now we can advertise a bigger window
    pbuf_free(p);
#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TX_TCP_FREE, 0);
#endif


    return ERR_OK;
}

static err_t echo_server_sent(void *arg, struct tcp_pcb *tpcb, u16_t length)
{
    return ERR_OK;
}

static err_t echo_server_accept(void *arg, struct tcp_pcb *tpcb, err_t err)
{
    assert(err == ERR_OK);
    tcp_recv(tpcb, echo_server_recv);
    tcp_sent(tpcb, echo_server_sent);
    tcp_err(tpcb, echo_server_err);

    tcp_arg(tpcb, 0);

    return ERR_OK;
}

int tcp_echo_server_init(void)
{
    err_t r;

    uint16_t bind_port = 7; //don't use htons() (don't know why...)


    struct tcp_pcb *pcb = tcp_new();
    if (pcb == NULL) {
        return ERR_MEM;
    }

    r = tcp_bind(pcb, IP_ADDR_ANY, bind_port);
    if(r != ERR_OK) {
        return(r);
    }

    struct tcp_pcb *pcb2 = tcp_listen(pcb);
    assert(pcb2 != NULL);
    tcp_accept(pcb2, echo_server_accept);


    printf("TCP echo_server_init(): bound.\n");
    printf("TCP installed receive callback.\n");
    return (0);
}
