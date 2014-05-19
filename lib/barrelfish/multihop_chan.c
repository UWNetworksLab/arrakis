/**
 * \file
 * \brief Bidirectional Multi-hop channel implementation
 */

/*
 * Copyright (c) 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/multihop_chan.h>
#include <barrelfish/idc_export.h>
#include <flounder/flounder_support.h>
#include <if/monitor_defs.h>
#include <collections/hash_table.h>
#include <bench/bench.h>

// TODO remove this
#include <stdio.h>

#ifndef CONFIG_INTERCONNECT_DRIVER_MULTIHOP
#error "This file shouldn't be compiled without CONFIG_INTERCONNECT_DRIVER_MULTIHOP"
#endif

///////////////////////////////////////////////////////

// HASH TABLE

///////////////////////////////////////////////////////

/**
 * We use a hash table to map VCIs to bindings.
 */
static collections_hash_table *mappings;

// is the mapping table initialized?
static bool is_mapping_table_initialized = false;

// initialize the mapping table
static inline void multihop_chan_init_mapping_table(void)
{

    if (!is_mapping_table_initialized) {
        is_mapping_table_initialized = true;
        collections_hash_create_with_buckets(&mappings, MULTIHOP_MAPPING_TABLE_BACKETS,
                free);
        /*
         *  We use a constant as seed for the random function.
         *  We could use something like bench_tsc() as seed, but
         *  this would require a dependency on lib/bench.
         *
         *  This makes assigned VCIs predictable. But because we trust
         *  the monitor not to send manipulated VCIs, this is not a
         *  security issue.
         */
        srand(11);
    }
}

// insert entry in the mapping table and return VCI
static inline multihop_vci_t multihop_chan_mapping_insert(
        struct multihop_chan *chan_state)
{

    assert(chan_state != NULL);
    multihop_vci_t vci;

    multihop_chan_init_mapping_table();

    do {
        // we assign VCIs randomly, but need
        // to make sure that it is not yet taken
        vci = (multihop_vci_t) rand();
    } while (collections_hash_find(mappings, vci) != NULL);

    // insert into forwarding table
    collections_hash_insert(mappings, vci, chan_state);
    return vci;
}

// delete entry from forwarding table
static inline void multihop_chan_mapping_delete(multihop_vci_t vci)
{
    assert(is_mapping_table_initialized);
    collections_hash_delete(mappings, vci);
}

// get entry from the mapping table
static inline struct multihop_chan* multihop_chan_mappings_lookup(multihop_vci_t vci)
{

    assert(is_mapping_table_initialized);
    struct multihop_chan *chan_state = collections_hash_find(mappings, vci);

    if (chan_state == NULL) {
        USER_PANIC("invalid virtual circuit identifier in multi-hop channel");
    }
    return chan_state;
}

///////////////////////////////////////////////////////

// BIND & CREATE A NEW MULTIHOP CHANNEL

///////////////////////////////////////////////////////

static void multihop_new_monitor_binding_continuation(void *st, errval_t err,
        struct monitor_binding *monitor_binding);

static void multihop_chan_bind_cont(void *st);

/**
 * \brief Initialize a new multihop channel
 *
 * \param mc  Storrage for the multihop channel state
 * \param cont  Continuation for bind completion/failure
 * \param iref  IREF of the service to which we want to bind
 * \param waitset to use
 */
errval_t multihop_chan_bind(struct multihop_chan *mc,
        struct multihop_bind_continuation cont, iref_t iref,
        struct waitset *waitset)
{
    errval_t err;

    // store bind arguments
    mc->bind_continuation = cont;
    mc->iref = iref;
    mc->connstate = MULTIHOP_BIND_WAIT;

    // create new monitor binding
    err = monitor_client_new_binding(multihop_new_monitor_binding_continuation,
            mc, waitset, DEFAULT_LMP_BUF_WORDS);
    return err;
}

/**
 * \brief Internal function called as soon as the new monitor binding is created
 * \param st pointer to the multi-hop channel
 * \param err error variable indicating success / failure
 * \param monitor_binding the new monitor binding
 */
static void multihop_new_monitor_binding_continuation(void *st, errval_t err,
        struct monitor_binding *monitor_binding)
{
    struct multihop_chan *mc = st;

    if (err_is_fail(err)) {
        // report error to user
        err = err_push(err, LIB_ERR_MONITOR_CLIENT_BIND);
        mc->bind_continuation.handler(mc->bind_continuation.st, err, NULL);
    } else {
        mc->monitor_binding = monitor_binding;

        // get a virtual circuit identifier (VCI) for this binding
        mc->my_vci = multihop_chan_mapping_insert(mc);

        // send request to the monitor
        multihop_chan_bind_cont(mc);
    }
}

/**
 * \brief Continuation function for binding. This function
 *        send the bind request to the monitor.
 * \param pointer to the multihop_chan
 */
static void multihop_chan_bind_cont(void *st)
{

    errval_t err;
    struct multihop_chan *mc = st;
    struct monitor_binding *monitor_binding = mc->monitor_binding;

    // send bind request to the monitor
    // we do not get a lock on the monitor binding, as we did not expose it to the application
    MULTIHOP_DEBUG("sending bind request to monitor...\n");
    err = monitor_binding->tx_vtbl.multihop_bind_client_request(monitor_binding,
            NOP_CONT, mc->iref, mc->my_vci);

    if (err_is_ok(err)) {
        // request was successfully sent
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        // register to retry
        err = monitor_binding->register_send(monitor_binding,
                monitor_binding->waitset, MKCONT(multihop_chan_bind_cont, st));
        assert(err_is_ok(err));
    } else { // permanent failure sending message
        mc->bind_continuation.handler(mc->bind_continuation.st,
                err_push(err, LIB_ERR_BIND_MULTIHOP_REQ), NULL);
        //TODO destroy channel state?
    }
}

/**
 * \brief Handles the bind reply message from the monitor
 * \param monitor_binding
 * \param ingoing_vci my (ingoing) virtual circuit identifier
 * \param outgoing_vci virtual circuit identifier to use for outgoing messages
 */
static void multihop_bind_reply_handler(struct monitor_binding *monitor_binding,
        multihop_vci_t ingoing_vci, multihop_vci_t outgoing_vci, errval_t msgerr)
{
    MULTIHOP_DEBUG("dispatcher has received bind reply!\n");

    struct multihop_chan *mc = multihop_chan_mappings_lookup(ingoing_vci);

    assert(mc->connstate == MULTIHOP_BIND_WAIT);
    assert(mc->bind_continuation.handler != NULL);

    if (err_is_ok(msgerr)) { /* bind succeeded */
        mc->direction = 1;
        mc->unacked_received = 0;
        mc->unacked_send = 0;
        mc->vci = outgoing_vci;
        mc->connstate = MULTIHOP_CONNECTED;
        mc->bind_continuation.handler(mc->bind_continuation.st, msgerr, mc);
    } else { /* bind failed */
        mc->connstate = MULTIHOP_DISCONNECTED;
        multihop_chan_mapping_delete(mc->my_vci);
        mc->bind_continuation.handler(mc->bind_continuation.st, msgerr, mc);
        free(mc);
    }
}

///////////////////////////////////////////////////////

// HANDLE BIND REQUESTS

///////////////////////////////////////////////////////

static void send_bind_reply(void *arg);

static void multihop_new_monitor_binding_continuation2(void *st, errval_t err,
        struct monitor_binding *monitor_binding);

// struct for the reply state
struct bind_multihop_reply_state {
    struct multihop_chan *mc;
    struct monitor_binding *monitor_binding;
    struct monitor_multihop_bind_service_reply__args args;
    struct event_queue_node qnode;
};

/**
 * \brief This method handles incoming bind requests from the monitor
 * \param monitor_binding
 * \param service_id the ID of the servict to bind to
 * \param vci the virtual circuit identifier to use on outgoing messages
 */

static void multihop_bind_service_request_handler(
        struct monitor_binding *monitor_binding, uintptr_t service_id,
        multihop_vci_t vci)
{
    errval_t err;
    struct idc_export *e = (void *) service_id;

    // call the binding's connect handler
    if (e->multihop_connect_callback != NULL) {
        err = e->multihop_connect_callback(e->connect_cb_st, vci);
    } else {
        err = LIB_ERR_NO_MULTIHOP_BIND_HANDLER;
    }

    if (err_is_fail(err)) {
        multihop_chan_send_bind_reply(NULL, err, vci, NULL);
    } else {
        // do nothing, as the binding is responsible for sending a reply
    }
}

/**
 * \brief Send a reply back to the monitor. If the error code indicates success, this function
 *        creates a new monitor binding and registers to receive messages.
 * \param multihop_chan
 * \param err error code to send back
 * \param vci my vci for ingoing messages
 * \param waitset waitset to use for the channel
 */
void multihop_chan_send_bind_reply(struct multihop_chan *mc, errval_t msgerr,
        multihop_vci_t vci, struct waitset *waitset)
{

    errval_t err;
    struct bind_multihop_reply_state *reply_state = malloc(
            sizeof(struct bind_multihop_reply_state));
    assert(reply_state != NULL);

    if (err_is_ok(msgerr)) {
        // make sure channel exists
        assert(mc != NULL);
    } else {
        // make sure channel is not created
        assert(mc == NULL);
    }

    reply_state->mc = mc;
    reply_state->args.err = msgerr;
    reply_state->args.receiver_vci = vci;

    if (err_is_ok(msgerr)) {
        // get a vci for this binding
        reply_state->mc->my_vci = multihop_chan_mapping_insert(mc);
        reply_state->args.sender_vci = reply_state->mc->my_vci;
    } else {
        reply_state->args.sender_vci = 0;
    }

    if (err_is_ok(msgerr)) {

        // create a new monitor binding
        err = monitor_client_new_binding(
                multihop_new_monitor_binding_continuation2, reply_state,
                waitset, DEFAULT_LMP_BUF_WORDS);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(
                    err,
                    "Could not create a new monitor binding in the multi-hop interconnect driver");
        }
    } else {
        reply_state->monitor_binding = get_monitor_binding();
        // wait for the ability to use the monitor binding
        event_mutex_enqueue_lock(&reply_state->monitor_binding->mutex,
                &reply_state->qnode, MKCLOSURE(send_bind_reply, reply_state));
    }

}

/**
 * \brief Internal function that is called as soon as a new monitor binding is created
 */
static void multihop_new_monitor_binding_continuation2(void *st, errval_t err,
        struct monitor_binding *monitor_binding)
{

    struct bind_multihop_reply_state *reply_state = st;
    if (err_is_fail(err)) {
        reply_state->args.err = err;
    } else {
        reply_state->monitor_binding = monitor_binding;
        reply_state->mc->monitor_binding = monitor_binding;
        reply_state->mc->direction = 2;
        reply_state->mc->unacked_received = 0;
        reply_state->mc->unacked_send = 0;
        reply_state->mc->connstate = MULTIHOP_CONNECTED;
    }

    // wait for the ability to use the monitor binding
    event_mutex_enqueue_lock(&reply_state->monitor_binding->mutex,
            &reply_state->qnode, MKCLOSURE(send_bind_reply, reply_state));
}

/**
 * \ brief Internal function to send a reply back to the monitor
 *
 */
static void send_bind_reply(void *st)
{

    errval_t err;
    struct bind_multihop_reply_state *reply_state = st;
    struct monitor_binding *monitor_binding = reply_state->monitor_binding;

    // send back a bind success / failure message to the monitor
    MULTIHOP_DEBUG("sending reply back to monitor...\n");
    err = monitor_binding->tx_vtbl.multihop_bind_service_reply(monitor_binding,
            NOP_CONT, reply_state->args.receiver_vci,
            reply_state->args.sender_vci, reply_state->args.err);

    if (err_is_ok(err)) {
        event_mutex_unlock(&monitor_binding->mutex);
        free(reply_state);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = monitor_binding->register_send(monitor_binding,
                monitor_binding->waitset, MKCONT(send_bind_reply, reply_state));
        assert(err_is_ok(err));
        // this shouldn't fail, as we have the mutex
    } else {
        event_mutex_unlock(&monitor_binding->mutex);
        USER_PANIC_ERR(
                err,
                "failed sending back reply to multi-hop bind request to monitor");
        free(st);
    }
}

///////////////////////////////////////////////////////

// SEND AND RECEIVE MESSAGES

///////////////////////////////////////////////////////

/**
 * \brief Send a multi-hop message that contains no payload.
 * 		  It is used to acknowledge received messages.
 *
 * \param mc pointer to the multi-hop channel
 */
static void multihop_send_dummy_message(struct multihop_chan *mc)
{
    assert(mc->connstate == MULTIHOP_CONNECTED);

#if MULTIHOP_FLOW_CONTROL

    MULTIHOP_DEBUG("sending dummy message, ack %d...\n", mc->unacked_received);

    errval_t err;
    struct monitor_binding *monitor_binding = mc->monitor_binding;

    // send message
    err = monitor_binding->tx_vtbl.multihop_message(monitor_binding, NOP_CONT,
            mc->vci, mc->direction, MULTIHOP_MESSAGE_FLAG_DUMMY,
            mc->unacked_received, (uint8_t *) mc, 1);

    if (err_is_ok(err)) {
        // we have just acknowledged all received messages
        mc->unacked_received = 0;
    } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
        USER_PANIC_ERR(err,
                "Could not send dummy message over multi-hop channel\n");

    }

#endif // MULTIHOP_FLOW_CONTROL
}

/**
 * \brief Send a multi-hop message
 *
 * \param mc pointer to the multi-hop channel
 * \param _continuation callback to be executed after the message is sent
 * \param msg pointer to the message payload
 * \param msglen length of the message payload (in bytes)
 *
 */
errval_t multihop_send_message(struct multihop_chan *mc,
        struct event_closure _continuation, void *msg, size_t msglen)
{

    errval_t err;
    struct monitor_binding *monitor_binding = mc->monitor_binding;
    assert(mc->connstate == MULTIHOP_CONNECTED);

#if MULTIHOP_FLOW_CONTROL
    // make sure that we can send another message
    if (mc->unacked_send == MULTIHOP_WINDOW_SIZE) {
        return FLOUNDER_ERR_TX_BUSY;
    }
#endif // MULTIHOP_FLOW_CONTROL
    // send message
    err = monitor_binding->tx_vtbl.multihop_message(monitor_binding,
            _continuation, mc->vci, mc->direction,
            MULTIHOP_MESSAGE_FLAG_PAYLOAD, mc->unacked_received,
            (uint8_t *) msg, msglen);

#if MULTIHOP_FLOW_CONTROL
    if (err_is_ok(err)) {
        // update flow control information
        mc->unacked_received = 0;
        mc->unacked_send = mc->unacked_send + 1;
    }
#endif  // MULTIHOP_FLOW_CONTROL
    return err;
}

/**
 * \brief Send a capability over the multi-hop channel
 *
 * \param mc pointer to the multi-hop channel
 * \param _continuation callback to be executed after the message is sent
 * \param cap_state pointer to the cap state of the channel
 * \param cap the capability to send
 */
errval_t multihop_send_capability(struct multihop_chan *mc,
        struct event_closure _continuation,
        struct flounder_cap_state *cap_state, struct capref cap)
{

    errval_t err;
    assert(mc->connstate == MULTIHOP_CONNECTED);
    struct monitor_binding *mon_binding = mc->monitor_binding;

    // send the message
    err = mon_binding->tx_vtbl.multihop_cap_send(mon_binding, _continuation,
                                                 mc->vci, mc->direction,
                                                 SYS_ERR_OK, cap,
                                                 cap_state->tx_capnum);

    if (err_is_ok(err)) {
        // increase capability number
        cap_state->tx_capnum++;
        return err;
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        return err;
    } else {
        return err_push(err, LIB_ERR_MONITOR_CAP_SEND);
    }
}

/**
 * \brief Handle a incoming multi-hop message
 *
 * \param mon_closure the monitor binding
 * \param vci the virtual circuit identifier of the channel
 * \param direction direction of the message
 * \param flags message flags
 * \param ack number of messages that the sender acknowledges
 * \param buf pointer to the message payload
 * \param buflen size of the received message
 *
 */
static void handle_multihop_message(struct monitor_binding *mon_closure,
        multihop_vci_t vci, uint8_t direction, uint8_t flags, uint32_t ack,
        uint8_t *buf, size_t buflen)
{

    struct multihop_chan *mc = multihop_chan_mappings_lookup(vci);
    assert(mc->connstate == MULTIHOP_CONNECTED);

    if (flags == MULTIHOP_MESSAGE_FLAG_DUMMY) {
        // this is a dummy message
        MULTIHOP_DEBUG("received dummy message, acked: %d\n", ack);
        assert(ack <= mc->unacked_send);
        mc->unacked_send = mc->unacked_send - ack;

        // we need to execute the message receive handler,
        // because the flounder-stubs might be waiting to
        // receive a dummy message
        mc->rx_handler.handler(mc->rx_handler.arg, NULL, 0);

    } else { // this message contains payload

#if MULTIHOP_FLOW_CONTROL

        // update flow control information
        mc->unacked_received = mc->unacked_received + 1;
        assert(ack <= mc->unacked_send);
        mc->unacked_send = mc->unacked_send - ack;

#endif // MULTIHOP_FLOW_CONTROL
        // deliver message to the message handler
        mc->rx_handler.handler(mc->rx_handler.arg, buf, buflen);

        // send a dummy message back if necessary
        if (mc->unacked_received
                > MULTIHOP_WINDOW_SIZE * MULTIHOP_WINDOW_RATIO_DUMMY_MESSAGE) {
            multihop_send_dummy_message(mc);
        }
    }
}

/**
 * \brief Handle a incoming capability
 *
 * \param mon_closure the monitor binding
 * \param vci the virtual circuit identifier of the channel
 * \param direction direction of the message
 * \param cap reference to the capability
 * \param capid id of the capability
 *
 */
static void multihop_handle_capability(struct monitor_binding *mon_closure,
        multihop_vci_t vci, uint8_t direction, errval_t msgerr,
        struct capref cap, uint32_t capid)
{

    struct multihop_chan *mc = multihop_chan_mappings_lookup(vci);
    assert(mc->connstate == MULTIHOP_CONNECTED);
    assert(mc->cap_handlers.cap_receive_handler != NULL);

    // deliver capability to the handler
    mc->cap_handlers.cap_receive_handler(mc->cap_handlers.st, msgerr, cap, capid);
}

///////////////////////////////////////////////////////

// CONTROL FUNCTIONS & INIT

///////////////////////////////////////////////////////

/**
 * \ brief Change the waitset of the multi-hop channel
 *
 * \param mc pointer to the multi-hop channel
 * \param ws the new waitset
 */
errval_t multihop_chan_change_waitset(struct multihop_chan *mc,
        struct waitset *ws)
{

    errval_t err;
    err = flounder_support_change_monitor_waitset(mc->monitor_binding, ws);
    if (err_is_fail(err)) {
        return (err_push(err, FLOUNDER_ERR_CHANGE_MONITOR_WAITSET));
    } else {
        return err;
    }
}

/**
 * \brief register a continuation closure to be invoked on the given waitset when the
 * 		  multi-hop channel may be able to accept the next message
 */
errval_t multihop_chan_register_send(struct multihop_chan *mc,
        struct waitset *ws, struct event_closure cont)
{
    return mc->monitor_binding->register_send(mc->monitor_binding, ws, cont);
}

/**
 * \brief Is the send window full?
 *
 */bool multihop_chan_is_window_full(struct multihop_chan *mc)
{
#if MULTIHOP_FLOW_CONTROL
    return mc->unacked_send == MULTIHOP_WINDOW_SIZE;
#else
    return false;
#endif
}

/**
 * \brief Initialize the multi-hop interconnect driver
 *
 */
void multihop_init(void)
{
    struct monitor_binding *mb = get_monitor_binding();
    mb->rx_vtbl.multihop_bind_service_request =
            &multihop_bind_service_request_handler; // handler for incoming bind request messages from the monitor
    mb->rx_vtbl.multihop_bind_client_reply = &multihop_bind_reply_handler; // handler for incoming reply messages from the monitor
    mb->rx_vtbl.multihop_message = &handle_multihop_message; // handler for incoming messages from the monitor
    mb->rx_vtbl.multihop_cap_send = &multihop_handle_capability; // handler for incoming capabilities from the monitor
}
