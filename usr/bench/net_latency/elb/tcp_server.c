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
#include <lwip/tcp.h>
#include <lwip/ip_addr.h>
#include <netif/bfeth.h>
#include <trace/trace.h>
#include "tcp_server_bm.h"


static void tcp_server_bm_close(struct tcp_pcb *tpcb)
{
    tcp_arg(tpcb, NULL);
    tcp_close(tpcb);
}


static void tcp_server_bm_err(void *arg, err_t err)
{
    printf("tcp_server_bm_err! %p %d\n", arg, err);
}


static err_t tcp_server_bm_recv(void *arg, struct tcp_pcb *tpcb, struct pbuf *p,
                              err_t err)
{
    int r;
    if (p == NULL) {
        // close the connection
        tcp_server_bm_close(tpcb);
        printf("Error in tcp_server_bm_recv");
        return ERR_OK;
    }

    /* don't send an immediate ack here, do it later with the data */
    // FIXME: send ack immediately!
    tpcb->flags |= TF_ACK_DELAY;
    assert(p->next == 0);

    // Send the data to benchmarking code for furthur analysis
    handle_data_arrived(p->payload, p->len);

    //XXX: can we do that without needing to copy it??
    r = tcp_write(tpcb, p->payload, p->len, TCP_WRITE_FLAG_COPY);
    assert(r == ERR_OK);

    // make sure data gets sent immediately
    r = tcp_output(tpcb);
    assert(r == ERR_OK);

    tcp_recved(tpcb, p->len);

    //now we can advertise a bigger window
    pbuf_free(p);

    return ERR_OK;
}

static err_t tcp_server_bm_sent(void *arg, struct tcp_pcb *tpcb, u16_t length)
{
    return ERR_OK;
}

static err_t tcp_server_bm_accept(void *arg, struct tcp_pcb *tpcb, err_t err)
{
    assert(err == ERR_OK);
    tcp_recv(tpcb, tcp_server_bm_recv);
    tcp_sent(tpcb, tcp_server_bm_sent);
    tcp_err(tpcb, tcp_server_bm_err);

    tcp_arg(tpcb, 0);

    return ERR_OK;
}

int tcp_server_bm_init(uint16_t bind_port)
{
    err_t r;

    //don't use htons() (don't know why...)

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
    tcp_accept(pcb2, tcp_server_bm_accept);

    printf("TCP tcp_server_bm_init(): bound.\n");
    printf("TCP installed receive callback.\n");
    printf("TCP benchmark server started\n");
    return (0);
}

// ***************************************************************
// tcp client code
// ***************************************************************


static void close_connection(struct tcp_pcb *pcb)
{
    printf("closing(pcb: %p)\n", pcb);
    tcp_close(pcb);
    tcp_arg(pcb, NULL);
    tcp_sent(pcb, NULL);
    tcp_recv(pcb, NULL);
    printf("connection closed:\n");
}


static err_t tcp_is_sent_client(void *arg, struct tcp_pcb *pcb, u16_t len)
{
//  assert(pcb != NULL);
//  printf("sent %u bytes.\n", len);
    return ERR_OK;
}

static void tcp_is_err_client(void *arg, err_t err)
{
    printf("tcp is err: %d\n", (int)err);
}

static err_t tcp_is_poll_client(void *arg, struct tcp_pcb *pcb)
{
    printf("tcp is poll\n");
    return ERR_OK;
}


static err_t tcp_is_recv_client(void *arg, struct tcp_pcb *pcb,
        struct pbuf *pb, err_t err)
{
    assert(err == ERR_OK);
    assert(pcb != NULL);

    if (pb == NULL) {
        printf("finished receiving data\n");
        close_connection(pcb);
    }

    // pointer to the payload
    char *payload = (char *)pb->payload;
    handle_data_arrived(payload, pb->tot_len);

    // Inform TCP that we have taken the data.
    tcp_recved(pcb, pb->tot_len);

    // Free the packet buffer
    pbuf_free(pb);

    return ERR_OK;
} // end function: tcp_is_recv_client


static err_t tcp_is_connected_client(void *arg, struct tcp_pcb *pcb, err_t err)
{

    if (err != ERR_OK) {
        fprintf(stderr, "tcp connection failed\n");
        close_connection(pcb);
        return err;
    }

    tcp_sent(pcb, tcp_is_sent_client);
    tcp_recv(pcb, tcp_is_recv_client);
    tcp_err( pcb, tcp_is_err_client);
    tcp_poll(pcb, tcp_is_poll_client, 10);

    printf("tcp client connected\n");
    handle_connection_opened();

    return ERR_OK;
}

static struct tcp_pcb *client_pcb = NULL;

// initialize tcp connection for the client
int tcp_client_bm_init(char *ip_addr_str,  uint16_t server_port)
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
    struct ip_addr ip;
    ip.addr = addr.s_addr;

    // Prepare tcp_pcb
    client_pcb = tcp_new();
    if (client_pcb == NULL) {
        USER_PANIC("tcp_new failed");
        return -1;
    }

    //don't use htons() on port no. (don't know why...)
    r = tcp_connect(client_pcb, &ip, server_port, tcp_is_connected_client);
    if(r != ERR_OK) {
        USER_PANIC("tcp_connect failed");
        return(r);
    }

    // Connection established!
    printf("TCP benchmark client started\n");
    return (0);
} // end function: tcp_client_bm_init


// send single message over TCP connection
int send_message_client(void *msg, size_t len)
{
    err_t err;

//    printf("send_message(pcb: %p, msg: %p, len: %d)\n",
//           pcb, msg, (int)len);

    if (len > 0) {
        assert(tcp_sndbuf(client_pcb) >= len);

        err = tcp_write(client_pcb, msg, len, TCP_WRITE_FLAG_COPY);
        if (err != ERR_OK) {
            USER_PANIC_ERR(err, "tcp_write failed in send_message");
            return -1;
        }
    }

    // FIXME: Do I need this?
    err = tcp_output(client_pcb);
    if (err != ERR_OK) {
        USER_PANIC_ERR(err, "tcp_write failed in send_message");
        return -1;
    }

    // printf("done send_message()\n");

    return 0;
} // end function: send_message_client

