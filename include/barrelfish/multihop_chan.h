/**
 * \file
 * \brief Bidirectional Multi-hop channel
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_MULTIHOP_CHAN_H
#define BARRELFISH_MULTIHOP_CHAN_H

#include <sys/cdefs.h>

#include <barrelfish/monitor_client.h>
#include <barrelfish/waitset.h>
#include <flounder/flounder_support_caps.h>

__BEGIN_DECLS

// turn on / off debugging of the multi-hop interconnect driver
#define MULTIHOP_DEBUG_ENABLED 0

// set this to true in order to use flow control in the multi-hop channel
#define MULTIHOP_FLOW_CONTROL 1

// the window size (only effective if flow control is turned on)
#define MULTIHOP_WINDOW_SIZE 100

// the ration of unacknowledged received messages until we trigger
// sending a dummy message
#define MULTIHOP_WINDOW_RATIO_DUMMY_MESSAGE 0.5

// number of buckets in the forwarding (hash) table at monitors
#define MULTIHOP_FORWARDING_TABLE_BUCKETS 10

// number of buckets in the mapping table (at dispatchers)
#define MULTIHOP_MAPPING_TABLE_BACKETS 10

///////////////////////////////////////////////////////

// DEBUG PRINTER

///////////////////////////////////////////////////////

#if MULTIHOP_DEBUG_ENABLED
#define MULTIHOP_DEBUG(x...) printf("MULTI-HOP: " x)
#else
#define MULTIHOP_DEBUG(x...) ((void)0)
#endif

///////////////////////////////////////////////////////

// CHANNEL STATE & MESSAGE HANDLER & BIND CONTINUATION

///////////////////////////////////////////////////////

struct multihop_chan;

typedef uint64_t multihop_vci_t; ///< Virtual circuit identifier

// bind continuation handler
struct multihop_bind_continuation {
    /**
     * \brief Handler which runs when a binding succeeds or fails
     * \param st State pointer set in closure
     * \param err error code indicating success/failure of binding
     * \param mc On success, contains pointer to channel
     */
    void (*handler)(void *st, errval_t err, struct multihop_chan *mc);
    void *st;
};

// message receive handler
struct multihop_receive_handler {
    void (*handler)(void *arg, uint8_t *message, size_t length);
    void *arg;
};

// possible message types:
// 1: a message containing payload
// 2: a "dummy" message (used only for acknowledgments)
enum {
    MULTIHOP_MESSAGE_FLAG_DUMMY, MULTIHOP_MESSAGE_FLAG_PAYLOAD
};

// A bidirectional MULTIHOP channel
struct multihop_chan {

    // connection state
    enum {
        // Disconnected
        MULTIHOP_DISCONNECTED,

        // Waiting for a bind reply
        MULTIHOP_BIND_WAIT,

        // Connection established
        MULTIHOP_CONNECTED,
    } connstate;

    iref_t iref; // IREF to which we bind
    multihop_vci_t my_vci; // my vci for this channel
    multihop_vci_t vci; // vci to use on outgoing messages
    uint8_t direction; // direction information

    struct monitor_binding *monitor_binding; // the monitor binding
    struct multihop_bind_continuation bind_continuation; // Continuation for bind

    //receive handler
    struct multihop_receive_handler rx_handler;

    // caps receive handler & caps reply message receive handler
    struct monitor_cap_handlers cap_handlers;

    // number of unacknowledged messages sent and received
    uint32_t unacked_received;
    uint32_t unacked_send;
};

///////////////////////////////////////////////////////

// BINDING & CONNECTION SET-UP

///////////////////////////////////////////////////////

// bind to service
errval_t multihop_chan_bind(struct multihop_chan *mc,
        struct multihop_bind_continuation cont, iref_t iref,
        struct waitset *waitset);

// send bind reply back to the monitor
void multihop_chan_send_bind_reply(struct multihop_chan *mc, errval_t err,
        multihop_vci_t vci, struct waitset *waitset);

///////////////////////////////////////////////////////

// CONTROL FUNCTIONS

///////////////////////////////////////////////////////

//brief Initialize the multi-hop driver
void multihop_init(void);

// set the message receive handler
inline static void multihop_chan_set_receive_handler(struct multihop_chan *mc,
        struct multihop_receive_handler rx_handler)
{
    mc->rx_handler = rx_handler;
}

// set the caps receive handler & the caps reply receive handler
inline static void multihop_chan_set_caps_receive_handlers(
        struct multihop_chan *mc, struct monitor_cap_handlers cap_handlers)
{
    mc->cap_handlers = cap_handlers;
}

// change the waitset of the multi-hop channel
errval_t multihop_chan_change_waitset(struct multihop_chan *mc,
        struct waitset *ws);

// register a continuation closure to be invoked on the given waitset when the
// multi-hop channel may be able to accept the next message
errval_t multihop_chan_register_send(struct multihop_chan *mc,
        struct waitset *ws, struct event_closure cont);

// is the send window full?
bool multihop_chan_is_window_full(struct multihop_chan *mc);

///////////////////////////////////////////////////////

// MESSAGE & CAPABILITY FORWARDING

///////////////////////////////////////////////////////

// send a message over the multi-hop channel
errval_t multihop_send_message(struct multihop_chan *mc,
        struct event_closure _continuation, void *msg, size_t msglen);

// send a capability over the multi-hop channel
errval_t multihop_send_capability(struct multihop_chan *mc,
        struct event_closure _continuation,
        struct flounder_cap_state *cap_state, struct capref cap);

__END_DECLS

#endif // BARRELFISH_MULTIHOP_CHAN_H
