/** \file
 *  \brief Simple program to send or receive files through a TCP/IP connection
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>

#include <errno.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>

#include <lwip/netif.h>
#include <lwip/dhcp.h>
#include <netif/etharp.h>
#include <lwip/init.h>
#include <lwip/tcp.h>
#include <netif/bfeth.h>
#include <lwip/ip_addr.h>


/* -------------- Coordination ----------------------*/

// condition used to singal controlling code to wait for a condition
static bool wait_cond;

static inline void
wait_for_condition (void) {
    while (wait_cond) {
        messages_wait_and_handle_next();
    }
}


/* -------------- LWIP/network initialisation ----------------------*/

#define MIN(a,b) ((a) < (b) ? (a) : (b))

#define SEND_TIMER_MSECS 4


/* -------------- Networking ----------------------*/

static err_t tcp_is_sent(void *arg, struct tcp_pcb *pcb, u16_t len);
static err_t tcp_is_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *pb, 
                         err_t err);

static void tcp_is_err(void *arg, err_t err)
{
    debug_printf("tcp is err: %d\n", (int)err);

}

static err_t tcp_is_poll(void *arg, struct tcp_pcb *pcb)
{
    debug_printf("tcp is poll\n");

    return ERR_OK;
}



static err_t tcp_is_connected(void *arg, struct tcp_pcb *pcb, err_t err)
{
    //    debug_printf("tcp connected\n");

    if (err != ERR_OK) {
        fprintf(stderr, "tcp connection failed\n");
        wait_cond = false;
        return err;
    }

    tcp_sent(pcb, tcp_is_sent);
    tcp_recv(pcb, tcp_is_recv);
    tcp_err( pcb, tcp_is_err);
    tcp_poll(pcb, tcp_is_poll, 10);

    wait_cond = false;

    return ERR_OK;
}


static struct tcp_pcb *connect(struct ip_addr *ip, int port)
{
    //    debug_printf("connect()\n");

    err_t err;
    struct tcp_pcb *pcb;

    pcb = tcp_new();
    if (pcb == NULL) {
        fprintf(stderr, "failed to create new pcb\n");
        assert(pcb != NULL);
        return NULL;
    }

    //    debug_printf("pcb created\n");

    //    debug_printf("connecting to server\n");
    wait_cond = true;
    err = tcp_connect(pcb, ip, port, tcp_is_connected);
    wait_for_condition();

    // TODO: proper error handling
    if (err != ERR_OK) {
        fprintf(stderr, "error connecting %d\n", err);
        return NULL;
    }

    return pcb;
}


static err_t tcp_server_accept(void *arg, struct tcp_pcb *tpcb, err_t err)
{

    debug_printf("accepted new connection\n");

    tcp_setprio(tpcb, TCP_PRIO_MIN);
    tcp_arg(tpcb, tpcb);
    tcp_recv(tpcb, tcp_is_recv);
    tcp_err(tpcb, tcp_is_err);
    tcp_poll(tpcb, NULL, 4);

    return ERR_OK;
}


static struct tcp_pcb *bind(int port)
{
    // debug_printf("bind\n");
    struct tcp_pcb *pcb = tcp_new();
    if (pcb == NULL) {
        return NULL;
    }

    //    debug_printf("got new pcb\n");

    //    debug_printf("calling tcp_bind\n");

    err_t err = tcp_bind(pcb, IP_ADDR_ANY, port);
    if(err != ERR_OK) {
        if (err == ERR_USE) {
            fprintf(stderr, "Another connection is bound to the same port.\n");
        }
        return NULL;
    }

    //    debug_printf("tcp_bind completed\n");

    //    debug_printf("calling tcp_listen\n");
    
    pcb = tcp_listen(pcb);
    if (pcb == NULL) {
        return NULL;
    }

    //    debug_printf("finished tcp_listen\n");

    tcp_arg(pcb, pcb); //callback argument
    tcp_accept(pcb, tcp_server_accept);

    //    debug_printf("bind finished\n");

    return pcb;
}

static void close_connection(struct tcp_pcb *pcb)
{
    //wait_cond = true;
    //    debug_printf("closing(pcb: %p)\n", pcb);
    tcp_close(pcb);
    tcp_arg(pcb, NULL);
    tcp_sent(pcb, NULL);
    tcp_recv(pcb, NULL);
    debug_printf("connection closed\n");
    //    wait_for_condition();    
}


/* -------------- File receiving ----------------------*/

static FILE* recv_f;

static err_t tcp_is_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *pb, 
                         err_t err)
{
    //    debug_printf("tcp is recv\n");

    static int tot = 0;

    int len;
    char *payload;

    if (pb == NULL) {
        // connection closed. clean up and then EXIT the program.
        debug_printf("finished receiving file. %d bytes\n", tot);
        close_connection(pcb);
        fflush(recv_f);
        fclose(recv_f);
        exit(EXIT_SUCCESS);
    } else if ((err == ERR_OK) && (pb != NULL)) {

        // pointer to the payload
        payload = (char *)pb->payload; 

        // size of the payload
        len = pb->tot_len; 

        //        debug_printf("Got data [%d bytes]\n", len);
        //        printf("data: %s\n", payload);

        // write out to file
        int n = fwrite(payload, 1, len, recv_f);
        fflush(recv_f);
        //        debug_printf("wrote %d bytes\n", n);

        tot += n;

        //        debug_printf("for a total of: %d bytes\n", tot);

        // Inform TCP that we have taken the data.
        tcp_recved(pcb, pb->tot_len);  

        // Free the packet buffer
        pbuf_free(pb);
    }

    //    debug_printf("done tcp is recv\n");

    return ERR_OK;
}


static errval_t do_receive_file(int port, char *path)
{

    assert(path != NULL);

    errval_t err;

    debug_printf("receive file %s on port %d\n", path, port);

    //    debug_printf("opening %s for writing\n", path);

    // open file to receive
    recv_f = fopen(path, "w");  
    if (recv_f == NULL) {
        err = errno;
        fprintf(stderr, "failed to fopen %s for writing\n", path);
        return err;
    }

    //    debug_printf("%s successfully opened\n", path);

    //    debug_printf("binding\n");

    struct tcp_pcb *pcb;
    pcb = bind(port);
    if (pcb == NULL) {
        assert(pcb != NULL);
        // return SOME_ERR;
    }
    
    //    debug_printf("bound\n");

    return SYS_ERR_OK;
}


/* -------------- File sending ----------------------*/

static errval_t send_message(struct tcp_pcb *pcb, void *msg, size_t len);


static err_t tcp_is_sent(void *arg, struct tcp_pcb *pcb, u16_t len)
{
    assert(pcb != NULL);

    //    debug_printf("sent %u bytes.\n", len);

    wait_cond = false;

    return ERR_OK;
}


// FIX: make this non-recursive
static errval_t send_message(struct tcp_pcb *pcb, void *msg, size_t len)
{
    err_t err;

    //    debug_printf("send_message(pcb: %p, msg: %p, len: %d)\n",
    //           pcb, msg, (int)len);

    if (len > 0) {
        wait_cond = true;
        uint16_t send_size = MIN(tcp_sndbuf(pcb), len);
        // debug_printf("\tsend_size: %d.\n", send_size);
        if (send_size <= 0) {
            // debug_printf("\tnot enough space in sndbuf.  will retry later.\n");
            // ran out of memory, wait for a send to happen
            //            debug_printf("\twaiting for condition.\n");
            wait_for_condition();
            // and try again
            // debug_printf("\tand trying again.\n");
            send_message(pcb, msg, len);
        } else {
            // debug_printf("\tsent %d bytes of %lu.\n", send_size, len);
            err = tcp_write(pcb, msg, send_size, TCP_WRITE_FLAG_COPY);
            // TODO: proper error handling
            if (err != ERR_OK) {
                fprintf(stderr, "error writing %d\n", err);
                return LWIP_ERR_MEM;  //TODO: what errno to use?
            }
            if (send_size < len) {
                // debug_printf("\tdidn't send whole message, so going to send rest now\n");
                send_message(pcb, msg + send_size, len - send_size);
            }
        }
    }

    err = tcp_output(pcb);

    // TODO: proper error handling
    if (err != ERR_OK) {
        fprintf(stderr, "error in tcp_output %d\n", err);
        return LWIP_ERR_MEM;  //TODO: what errno to use?
    }

    // debug_printf("done send_message()\n");

    return SYS_ERR_OK;
}

#define BUFLEN 2048
static const int buflen = BUFLEN;
//static char buf[buflen];
static char buf[BUFLEN];



static errval_t send_file(struct tcp_pcb *pcb, char *path)
{

    assert(pcb != NULL);
    assert(path != NULL);

    // debug_printf("send_file(pcb: %p, path: %s)\n", pcb, path);

    FILE *f;
    errval_t err;

    //    debug_printf("opening file %s...", path);

    // open the file
    f = fopen(path, "r");  
    if (f == NULL) {
        err = errno;
        fprintf(stderr, "failed to fopen %s\n", path);
        return err;
    }

    //    debug_printf("done.\n");

    //    debug_printf("going to send file\n");

    //    int i = 0;
    int n;
    while ((n = fread(buf, 1, buflen, f)) > 0) {
        // debug_printf("sending part %d\n", i); i++;
        err = send_message(pcb, buf, n);
        if (err_is_fail(err)) {
            fprintf(stderr, "failed while sending message: %d\n", (int)err);
            // debug_printf("going to close f: %p\n", f);
            fclose(f);
            wait_cond = false;
            return err;
        }
    }

    // debug_printf("going to close f: %p\n", f);
    fclose(f);

    // debug_printf("done send_file\n");

    wait_cond = false;

    return SYS_ERR_OK;
}


static errval_t do_send_file(struct in_addr *addr, int port, char *path)
{
    assert(addr != NULL);
    assert(path != NULL);

    errval_t err;

    debug_printf("send file %s to %s:%d\n", path, inet_ntoa(*addr), port);

    static struct ip_addr ip;
    ip.addr = addr->s_addr; // XXX

    //    debug_printf("ready to connect\n");

    struct tcp_pcb *pcb;
    pcb = connect(&ip, port);
    if (pcb == NULL) {
        assert(pcb != NULL);
        // return SOME_ERR;
    }
    
    //    debug_printf("connected\n");

    //    debug_printf("start sending.\n");

    wait_cond = true;
    err = send_file(pcb, path);
    if (err_is_fail(err)) {
        return err;
    }

    wait_for_condition();

    debug_printf("send finished.\n");

    // debug_printf("closing connection.\n");

    //    close_connection(pcb);

    debug_printf("connection closed.\n");

    return SYS_ERR_OK;
}



/* ------------------- Main -------------------------*/

struct args {
    struct in_addr addr;
    bool addr_set;
    int port;
    bool send;
    char *path;    
};

static struct args process_args(int argc, char *argv[])
{
    struct args res = (struct args) {
        .addr_set = false,
        .port = 0,
        .send = false,
        .path = NULL,
    };
    
    int opt;
    size_t flen;

    while ((opt = getopt(argc, argv, "sra:p:f:")) != -1) {
 
        switch (opt) {
        case 's':
            // send
            res.send = true;
            break;
        case 'r':
            // receive
            res.send = false;
            break;
        case 'a':
            // IP address
            if (inet_aton(optarg, &res.addr) == 0) {
                fprintf(stderr, "Invalid IP addr: %s\n", optarg);
                goto fail;
            }            
            res.addr_set = true;
            break;
        case 'p':
            // port
            res.port = atoi(optarg);
            break;
        case 'f':
            // file name
            flen = strlen(optarg);
            res.path = malloc(flen + 1);
            if (res.path == NULL) {
                fprintf(stderr, "failed to allocate memory\n");
                goto fail;
            }
            strncpy(res.path, optarg, flen+1);
            break;
        default:
            goto fail;
        }
    }

    // check conditions for correct arguments
    if ((res.path == NULL) || (res.send && !res.addr_set)) {
        goto fail;
    }

    return res;

 fail:
    fprintf(stderr, "Usage: %s [-sr] [-a address] [-p port] [-f file]\n", 
           argv[0]);
    exit(EXIT_FAILURE);
    return res;
}

// this is a hack to force vfs library code to be included
extern void vfs_dummy(void);

int main(int argc, char *argv[])
{

    struct args args = process_args(argc, argv);

    // debug_printf("IP addr: %s port: %d\n", inet_ntoa(args.addr), args.port);
    // debug_printf("%s file %s\n", args.send? "send": "receive", args.path);

    // Boot up
    //    debug_printf("calling stack_init()\n");

    lwip_init_auto();

    //    debug_printf("back from stack_init()\n");

    vfs_dummy();

    if (args.send) {
        do_send_file(&args.addr, args.port, args.path);
        debug_printf("do_send_file finished\n");
    } else {
        do_receive_file(args.port, args.path);
    } 

#if 0
    // start event loop
    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (true) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
#endif

    return EXIT_SUCCESS;
}



