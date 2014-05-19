/**
 * \file
 * \brief Datapath Communication between LWIP and network driver
 *
 * This file manages and performs the datapath communication between LWIP
 * and the network driver
 */

/*
 * Copyright (c) 2007-11 ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <stdio.h>
#include <assert.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <contmng/contmng.h>
#include <contmng/netbench.h>
#include <procon/procon.h>
#include "lwip/pbuf.h"
#include "lwip/init.h"
#include "lwip/sys.h"
#include "mem_barrelfish.h"
#include "idc_barrelfish.h"
#include <if/net_queue_manager_defs.h>
//#include <if/net_ports_defs.h>
//#include <if/net_ports_rpcclient_defs.h>
#include <barrelfish/bulk_transfer_arch.h>
#include <net_interfaces/net_interfaces.h>

#include "lwip_barrelfish_debug.h"

/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define LWIP_TRACE_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE

struct waitset *lwip_waitset;
bool lwip_init_done = false;


static int inflight_tx_requests = 0;
static int inflight_tx_limit = 127;
static int MAX_TRIES_TX = 200;

uint64_t incoming_packet_count = 0;
uint64_t incoming_tx_done_count = 0;
uint64_t outgoing_packet_count = 0;
uint64_t chained_pbuf_count = 0;


/*************************************************************
 * \defGroup LocalStates Local states
 *
 * @{
 *
 ****************************************************************/


uint64_t lwip_queue_id = 0; // queue_id allocated to this application

/**
 * \brief
 *
 *
 *
 */
static void (*lwip_rec_handler) (void *, uint64_t, uint64_t, uint64_t,
                                 uint64_t, struct pbuf *) = NULL;


/**
 * \brief
 *
 *
 *
 */
static void *lwip_rec_data;


/**
 * \brief
 *
 *
 *
 */
static void (*lwip_free_handler) (struct pbuf *) = NULL;


// Statistics about driver state
static uint64_t driver_tx_slots_left = 0;

uint64_t idc_check_driver_load(void)
{
    return driver_tx_slots_left;
}

uint64_t idc_get_packet_drop_count(void)
{
    return driver_tx_slots_left;
}

// checks if LWIP has any work to do, and does it without blocking
// or waiting for any events.
uint64_t perform_lwip_work(void)
{
    uint64_t ec = 0;
    struct waitset *ws = get_default_waitset();
    while (1) {
        // check for any event without blocking
        errval_t err = event_dispatch_non_block(ws);
        if (err == LIB_ERR_NO_EVENT) {
            break;
        }
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch_nonblock");
            break;
        }
        ++ec;
    }
    return ec;
}

uint64_t idc_send_packet_to_network_driver(struct pbuf *p)
{
    size_t idx;
    ptrdiff_t offset;
    perform_lwip_work();


#if LWIP_TRACE_MODE
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_LWIPTX, 0);
#endif // LWIP_TRACE_MODE

    LWIPBF_DEBUG("%s: idc_send_packet_to_network_driver: called\n", disp_name());

    size_t pbuf_chain_len = pbuf_clen(p);
    struct waitset *ws = get_default_waitset();
    int counter = 0;
    while (pbuf_chain_len >= (inflight_tx_limit - inflight_tx_requests)) {

        errval_t err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            abort();
            // I should push error LWIP_ERR_TXFULL on this before returning
            return 0;
        }

        ++counter;
        if (counter >= MAX_TRIES_TX) {
            printf("send_packet waited for %d events, and now giving up\n",
                    counter);
            return 0;
        }
    } // end while


    size_t more_chunks = false;
    uint64_t pkt_count = 0;
    while(p != NULL) {
        // Note: since we are freeing each pbuf in the chain separately, we
        // need to increment the reference count seperately, since lwip only
        // incremented the first pbuf's reference counter
        if (pkt_count != 0) {
//            pbuf_ref(p);
            ++chained_pbuf_count;
        }

        more_chunks = (p->next != NULL);
        idx = mem_barrelfish_put_pbuf(p);

        offset = p->payload - buffer_base;

        ++inflight_tx_requests;
        errval_t err = buffer_tx_add(idx, offset % buffer_size, p->len,
                more_chunks, p->nicflags);
        if (err != SYS_ERR_OK) {
            printf("idc_send_packet_to_network_driver: failed\n");
            USER_PANIC("idc_send_packet_to_network_driver: failed\n");
            LWIPBF_DEBUG("idc_send_packet_to_network_driver: failed\n");
            return 0;
        }
        if (pbuf_chain_len > 1) {
            LWIPBF_DEBUG
            //printf
                ("%s:chained pbuf %"PRIu64" (idx %"PRIu64"): "
                    "chain elem %"PRIu64" pbuf_ref = %"PRIu16" \n",
                   disp_name(), outgoing_packet_count, idx, pkt_count, p->ref);
       } else {
            LWIPBF_DEBUG
            //printf
                ("%s:Packet pbuf %"PRIu64" (idx %"PRIu64"): "
                    "chain elem %"PRIu64" pbuf_ref = %"PRIu16" \n",
                   disp_name(), outgoing_packet_count, idx, pkt_count, p->ref);
       }


        LWIPBF_DEBUG("idc_send_packet_to_network_driver: terminated\n");
        ++pkt_count;
        p = p->next;
        outgoing_packet_count++;
    }
    return pkt_count;
} // end function: idc_send_packet_to_network_driver


void debug_show_spp_status(int connection)
{
    assert(!"NYI");
}


int lwip_check_sp_capacity(int direction)
{
    assert(!"NYI");
    return -1;
}


int idc_check_capacity(int direction)
{
    assert(!"NYI");
    return -1;
}


void idc_get_mac_address(uint8_t * mac_client)
{
    benchmark_get_mac_address(mac_client);
}


void idc_print_statistics(void)
{
    LWIPBF_DEBUG("idc_print_statistics: called\n");
    assert(!"NYI");
    LWIPBF_DEBUG("idc_print_statistics: terminated\n");
}


void idc_print_cardinfo(void)
{
    printf("idc_print_cardinfo: Not yet Implemented\n");
    // FIXME: It should send msg to device driver and not queue manager
}


void idc_benchmark_control(int connection, uint8_t state, uint64_t trigger,
        uint64_t cl)
{
     LWIPBF_DEBUG("idc_debug_status:  called with status %x [%"PRIu64"]\n",
     state, trigger);
     assert(!"NYI");
}


/**
 * \brief
 *
 *
 *
 */
void idc_register_receive_callback(void (*f)
                                    (void *, uint64_t, uint64_t, uint64_t,
                                     uint64_t, struct pbuf *), void *data)
{

    LWIPBF_DEBUG("idc_register_receive_callback: called\n");

    assert(f != 0);
    lwip_rec_handler = f;
    lwip_rec_data = data;

    LWIPBF_DEBUG("idc_register_receive_callback: terminated\n");

}


void idc_register_freeing_callback(void (*f) (struct pbuf *))
{

    LWIPBF_DEBUG("idc_register_freeing_callback: called\n");

    lwip_free_handler = f;

    LWIPBF_DEBUG("idc_register_freeing_callback: terminated\n");

}


/*************************************************************
 * \defGroup MessageHandlers Message Handlers
 *
 * (...)
 *
 * @{
 *
 ****************************************************************/

uint8_t get_driver_benchmark_state(int direction,
        uint64_t *delta, uint64_t *cl)
{
    assert(!"NYI");
    return 0;
}

// antoinek: Might need to reenable this when we enable multi threaded lwip
// again
//bool lwip_in_packet_received = false;
static void handle_incoming(size_t idx, size_t len, uint64_t more,
                            uint64_t flags)
{
    struct pbuf *p;

    assert(!more);
#if LWIP_TRACE_MODE
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_LWIPRX, 0);
#endif // LWIP_TRACE_MODE

    ++incoming_packet_count;
    LWIPBF_DEBUG
    //printf
        ("%s:handle_incoming: incoming packet no %"PRIu64": len %"PRIu64"\n",
            disp_name(), incoming_packet_count, len);

    // Get the pbuf for this index
    p = mem_barrelfish_get_pbuf(idx);
    assert(p != NULL);

    LWIPBF_DEBUG("handle_incoming: incoming packet: len %"PRIu64"\n", len);
    p->nicflags = flags;
    lwip_rec_handler(lwip_rec_data, idx, -1ULL, len, len, p);

}

static void handle_tx_done(size_t idx)
{

    // this TX request is finished, so reduce the number of inflight TX requests
    --inflight_tx_requests;
    ++incoming_tx_done_count;
    struct pbuf *p = mem_barrelfish_get_pbuf(idx);
    assert(p != NULL);

    LWIPBF_DEBUG
    //printf
        ("%s:%s:TX_done %"PRIu64": for outgoing packet no %"PRIu64" "
            "(idx=%"PRIu64"): pbuf_ref = %"PRIu16" \n",
            disp_name(), __func__,  incoming_tx_done_count,
            outgoing_packet_count, idx, p->ref);
    lwip_free_handler(p);
}




// antoinek: We don't need to connect here, as the interface already did that
// for us. Maybe some internal initialization?
void idc_connect_to_driver(char *card_name, uint64_t queueid)
{
    lwip_queue_id = queueid;
    net_if_init(card_name, queueid);
}




/*
 * antoinek: FIXME: These should be renamed in some resonable manner
 */

void benchmark_rx_done(size_t idx, size_t len, uint64_t more, uint64_t flags)
{
    LWIPBF_DEBUG("benchmark_rx_done(%"PRIu64", %"PRIu64")\n", idx, len);
    if (lwip_init_done) {
        handle_incoming(idx, len, more, flags);
    }
}

void benchmark_tx_done(size_t idx)
{
    LWIPBF_DEBUG("benchmark_tx_done(%"PRIu64")\n", idx);
    handle_tx_done(idx);
}

void benchmark_do_pending_work(void)
{

}

