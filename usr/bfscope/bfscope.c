/**
 * \file
 * \brief Barrelfish trace server, Version 2
 */

/*
 * Copyright (c) 2007-2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/lmp_endpoints.h>
#include <barrelfish/event_queue.h>
#include <barrelfish/nameservice_client.h>
#include <trace/trace.h>

#include <flounder/flounder.h>
#include <if/monitor_defs.h>

#include <if/empty_defs.h>

/* LWIP Network stack includes */
#include <lwip/init.h>
#include <lwip/netif.h>
#include <lwip/dhcp.h>
#include <lwip/tcp.h>
#include <netif/bfeth.h>
#include <netif/etharp.h>

#define BFSCOPE_TCP_PORT 6666

#define BFSCOPE_BUFLEN (2<<20)

extern struct waitset *lwip_waitset;

static char *trace_buf = NULL;
static size_t trace_length = 0;
static size_t trace_sent = 0;
static bool dump_in_progress = false;

/// Timestamp when the sending of a trace dump over the network started
static uint64_t timestamp_start = 0;

/// The client that connected to this bfscope instance.
static struct tcp_pcb *bfscope_client = NULL;

/// If we are in autoflush is enabled, bfscope can itself determine to flush. In
/// that case, we don't want to notify anyone after doing a locally initiated flush.
static bool local_flush = false;


#define DEBUG if (0) printf

static void bfscope_send_flush_ack_to_monitor(void);

/*
 * \brief Close the specified TCP connection
 */
static void bfscope_connection_close(struct tcp_pcb *tpcb)
{
    DEBUG("bfscope: close\n");
    trace_length = 0;
    tcp_arg(tpcb, NULL);
    tcp_close(tpcb);
    bfscope_client = NULL;
}

/*
 * \brief Error callback from lwip
 */
static void error_cb(void *arg, err_t err)
{
    struct tcp_pcb *tpcb = (struct tcp_pcb *)arg;

    DEBUG("bfscope: TCP(%p) error %d\n", arg, err);

    if (tpcb) {
        bfscope_connection_close(tpcb);
    }
}

/*
 * Call this method when you finished dumping the current trace buffer.
 */
static void bfscope_trace_dump_finished(void)
{
    trace_length = 0;
    trace_sent = 0;
    dump_in_progress = false;

    if (!local_flush) {
        bfscope_send_flush_ack_to_monitor();
    } else {
        // Locally initiated flush is finished.
        local_flush = false;
    }
}

/*
 * \brief Send the next chunk of trace data down given TCP connection
 */
static void bfscope_trace_send(struct tcp_pcb *tpcb)
{
    char *bufptr;
    int len;

    //DEBUG("tcp_sndbuf=%d\n", tcp_sndbuf(tpcb));

    bufptr = trace_buf + trace_sent;
    len = trace_length - trace_sent;

    int more = 0;
    if (len > tcp_sndbuf(tpcb)) {
        len = tcp_sndbuf(tpcb);
        more = 1;
    }

    /* Give the data to LWIP until it runs out of buffer space */
    err_t lwip_err = tcp_write(tpcb, bufptr, len,
                      TCP_WRITE_FLAG_COPY | (more ? TCP_WRITE_FLAG_MORE : 0));

    //DEBUG("%d %ld+%d\n", r, trace_sent, len);

    if (lwip_err == ERR_MEM) {
        printf("bfscope: lwip: out of memory\n");
        return;
    }

    trace_sent += len;

    if (trace_sent >= trace_length) {
        /* No more events */
        uint64_t timestamp_stop = rdtsc();
        DEBUG("bfscope: done (%lu bytes) in %ld cycles\n",
               trace_sent, timestamp_stop - timestamp_start);

        bfscope_trace_dump_finished();
    }
}

/*
 * \brief Callback from LWIP when each chunk of data has been sent
 */
static err_t send_cb(void *arg, struct tcp_pcb *tpcb, u16_t length)
{
    //printf("send_cb %d\n", length);

    /* If we haven't finished sending the trace, then send more data */
    if (trace_length) {
        bfscope_trace_send(tpcb);
    }

    return ERR_OK;
}

/*
 * \brief This method should be called when a trace should be dumped on the network.
 */
static void bfscope_trace_dump_network(void)
{
    assert(bfscope_client != NULL);
    assert(trace_length > 0);

    printf("bfscope: sending %lu bytes to network...\n", trace_length);

    /* Send length field */
    char tmpbuf[10];
    int len;
    len = snprintf(tmpbuf, 9, "%08ld", trace_length);
    tcp_write(bfscope_client, tmpbuf, 8, TCP_WRITE_FLAG_COPY);

    /* Start to send the trace */
    timestamp_start = rdtsc();
    trace_sent = 0;

    bfscope_trace_send(bfscope_client);

    tcp_output(bfscope_client);
}

/*
 * \brief This method should be called when a trace should be dumped on the console.
 */
static void bfscope_trace_dump_console(void)
{
     printf("%s\n", trace_buf);

     bfscope_trace_dump_finished();
}

/*
 * \brief This method should be called when a trace should be dumped.
 *
 * (Based upon a different application calling trace_flush() or so.)
 */
static void bfscope_trace_dump(void)
{
    if(dump_in_progress) {
        // Currently there is already a dump in progress, do nothing.
        return;
    }

    int number_of_events = 0;
    // Acquire the trace buffer
    trace_length = trace_dump(trace_buf, BFSCOPE_BUFLEN, &number_of_events);

    DEBUG("bfscope: trace length %lu, nr. of events %d\n", trace_length, number_of_events);

    if (trace_length <= 0 || number_of_events <= 0) {
        DEBUG("bfscope: trace length too small, not dumping.\n");
        return;
    }

    dump_in_progress = true;


    if (bfscope_client != NULL) {
        // We have a connected client, dump to network
        bfscope_trace_dump_network();
    } else {
        // There is no client, just dump to console
        bfscope_trace_dump_console();
    }
}

/*
 * \brief Callback from LWIP when we receive TCP data
 */
static err_t recv_cb(void *arg, struct tcp_pcb *tpcb, struct pbuf *p,
                     err_t err)
{
    if (p == NULL) {
        // close the connection
        bfscope_connection_close(tpcb);
        return ERR_OK;
    }

    /* don't send an immediate ack here, do it later with the data */
    tpcb->flags |= TF_ACK_DELAY;

    assert(p->next == 0);


    if ((p->tot_len > 2) && (p->tot_len < 200)) {
        if (strncmp(p->payload, "trace", strlen("trace")) == 0) {

            DEBUG("bfscope: trace request\n");

            // NOOP

        } else {
            DEBUG("bfscope: could not understand request\n");
        }
    }


    /* Done with the incoming data */
    tcp_recved(tpcb, p->len);
    pbuf_free(p);

    return ERR_OK;
}

/*
 * \brief Callback from LWIP when a client connects to our TCP listen sock
 */
static err_t accept_cb(void *arg, struct tcp_pcb *tpcb, err_t err)
{
    printf("bfscope: connected\n");

    assert(err == ERR_OK);

    tcp_recv(tpcb, recv_cb);
    tcp_sent(tpcb, send_cb);
    tcp_err(tpcb, error_cb);
    tcp_arg(tpcb, (void*)tpcb);

    tcp_accepted(tpcb);

    bfscope_client = tpcb;

    return ERR_OK;
}

/*
 * \brief Start listening on the bfscope server port
 */
static err_t bfscope_server_init(void)
{
    err_t err;

    uint16_t bind_port = BFSCOPE_TCP_PORT;

    struct tcp_pcb *pcb = tcp_new();
    if (pcb == NULL) {
        return ERR_MEM;
    }

    err = tcp_bind(pcb, IP_ADDR_ANY, bind_port);
    if(err != ERR_OK) {
        return(err);
    }

    struct tcp_pcb *pcb2 = tcp_listen(pcb);
    assert(pcb2 != NULL);
    tcp_accept(pcb2, accept_cb);

    printf("bfscope: listening on port %d\n", BFSCOPE_TCP_PORT);

    return ERR_OK;
}

//------------------------------------------------------------------------------
// Monitor Messaging Interface
//------------------------------------------------------------------------------

/*
 * This function is called when we receive a flush message from our monitor.
 */
static void bfscope_handle_flush_msg(struct monitor_binding *mb, iref_t iref)
{
    printf("bfscope flush request message received!\n");

    bfscope_trace_dump();
}

struct bfscope_ack_send_state {
    struct event_queue_node qnode;
    struct monitor_binding *monitor_binding;
};

static void bfscope_send_flush_ack_cont(void* arg)
{
    errval_t err;

    struct bfscope_ack_send_state *state = (struct bfscope_ack_send_state*) arg;
    struct monitor_binding *monitor_binding = state->monitor_binding;

    err = monitor_binding->tx_vtbl.bfscope_flush_ack(monitor_binding, MKCONT(free, state));

    if (err_is_ok(err)) {
        event_mutex_unlock(&monitor_binding->mutex);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = monitor_binding->register_send(monitor_binding, monitor_binding->waitset, MKCONT(&bfscope_send_flush_ack_cont, state));
        assert(err_is_ok(err));
    } else {
        event_mutex_unlock(&monitor_binding->mutex);
        //TODO: Error handling
        USER_PANIC_ERR(err, "Could not send flush ack message to monitor of bfscope");
    }
}

/*
 * Call this method when bfscope is done with flushing and wants to notify
 * the initiator of the flush request.
 */
static void bfscope_send_flush_ack_to_monitor(void) {


    struct bfscope_ack_send_state *state = malloc(sizeof(struct bfscope_ack_send_state));
    //memset(state, 0, sizeof(struct trace_broadcast_start_state));

    state->monitor_binding = get_monitor_binding();

    event_mutex_enqueue_lock(&state->monitor_binding->mutex, &state->qnode, MKCLOSURE(&bfscope_send_flush_ack_cont, state));
}

//------------------------------------------------------------------------------
// Interface Exporting
//------------------------------------------------------------------------------

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    printf("bfscope: exported at iref %"PRIuIREF"\n", iref);

    // register this iref with the name service
    err = nameservice_register("bfscope", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_cb(void *st, struct empty_binding *b)
{
    USER_PANIC("bfscope: connect_cb got called");
}

//------------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------------

int main(int argc, char**argv)
{

#ifndef CONFIG_TRACE
    // bail - no tracing support
    printf("%.*s: Error, no tracing support, cannot start bfscope\n",
           DISP_NAME_LEN, disp_name());
    printf("%.*s: recompile with trace = TRUE in build/hake/Config.hs\n",
           DISP_NAME_LEN, disp_name());
    return -1;
#endif

    // Allocate the outgoing buffer
    if (trace_buf == NULL) {
        trace_buf = malloc(BFSCOPE_BUFLEN);
    }
    assert(trace_buf);

    /* Disable tracing for bfscope */
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    disp->trace_buf = NULL;

    printf("%.*s running on core %d\n", DISP_NAME_LEN, disp_name(),
           disp_get_core_id());

    /* Connect to e1000 driver */
    printf("%.*s: trying to connect to the e1000 driver...\n",
           DISP_NAME_LEN, disp_name());

    lwip_init_auto();

    err_t lwip_err = bfscope_server_init();

    assert(lwip_err == ERR_OK);


    // Export our empty interface
    errval_t err;
    err = empty_export(NULL /* state pointer for connect/export callbacks */,
            export_cb, connect_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    // Register our message handlers with the monitor
    struct monitor_binding *monitor_binding;
    monitor_binding = get_monitor_binding();
    monitor_binding->rx_vtbl.bfscope_flush_send = &bfscope_handle_flush_msg;


    while (1) {
        //err = event_dispatch(lwip_waitset);
        err = event_dispatch_non_block(lwip_waitset);

        if (err == LIB_ERR_NO_EVENT) {
            // It is ok that no event is dispatched.
            err = ERR_OK;
        }

        DEBUG("bfscope: dispatched event, autoflush: %d\n",((struct trace_buffer*) trace_buffer_master)->autoflush);

        // Check if we are in autoflush mode
        if(((struct trace_buffer*) trace_buffer_master)->autoflush) {
            local_flush = true;
            bfscope_trace_dump();
        }

        thread_yield_dispatcher(NULL_CAP);


        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

    return 0;
}

