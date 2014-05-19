/**
 * \file
 * \brief Multi-hop channel support at the monitor
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <collections/hash_table.h>
#include <bench/bench.h>
#include <barrelfish/multihop_chan.h>

///////////////////////////////////////////////////////

// ROUTING TABLE

///////////////////////////////////////////////////////

/* The routing table is used to determine where to
 * forward a connection set-up request.
 *
 * The routing table is constructed by the RTS
 * (Routing table set-up dispatcher), using
 * information from the System Knowledge Base (SKB).
 * The RTS will send the routing table to the monitor
 * that is first booted once it has constructed the
 * routing table.
 *
 * In cases where there is no SKB, the RTS will also
 * never send us an routing table. We use direct routing
 * is this case.
 */

// the routing table (as two dimensional array indexed by source and dest core)
static coreid_t **routing_table;

// the maximum source core ID in the routing table
static coreid_t routing_table_max_coreid;

// the number of outstanding entries to expect in the routing table
// (this is a kludge used while receiving entries from the rts program)
static coreid_t routing_table_nentries;

// hack for synchronisation when requesting routing table from another monitor
static bool saw_routing_table_response;

/*
 *  Print the routing table of a monitor, if present.
 */
static void multihop_print_routing_table(void)
{
#if MULTIHOP_DEBUG_ENABLED
    if (routing_table == NULL) {
        MULTIHOP_DEBUG("routing table not present on core %u\n", my_core_id);
        return;
    }

    size_t buffer_size = (((size_t)routing_table_max_coreid) + 1) * 5;
    char buffer[buffer_size];

    // Print the header
    MULTIHOP_DEBUG("routing table of monitor %u:\n", my_core_id);
    {
        char *p = buffer;
        for (unsigned i = 0; i <= routing_table_max_coreid; i++) {
            p += sprintf(p, " %3u", i);
        }
        MULTIHOP_DEBUG("      To:%s\n", buffer);
    }

    // Print each line
    for (unsigned src = 0; src <= routing_table_max_coreid; src++) {
        if (routing_table[src] == NULL) {
            continue;
        }

        // convert (my part of) the routing table into a single string
        char *p = buffer;
        int total_char = 0, w_char = 0;
        for (unsigned i = 0; i <= routing_table_max_coreid; i++) {
            w_char = snprintf(p, buffer_size - total_char, " %3u",
                              routing_table[src][i]);
            assert(w_char > 0);
            total_char += w_char;
            p += w_char;
        }
        MULTIHOP_DEBUG("From %3u:%s\n", src, buffer);
    }
#endif // MULTIHOP_DEBUG_ENABLED
}

// start to receive a new routing table from the RTS
static void multihop_routing_table_new(struct monitor_binding *b,
                                       coreid_t max_coreid, coreid_t nentries)
{
    // sanity-check input (FIXME: report errors!)
    assert(max_coreid >= my_core_id);
    assert(nentries > 0 && nentries <= (max_coreid + 1));

    // FIXME: we don't yet support changes to the existing routing table
    assert(routing_table == NULL);

    routing_table_max_coreid = max_coreid;
    routing_table_nentries = nentries;

    // allocate space for the max core ID
    routing_table = calloc(((uintptr_t)max_coreid) + 1, sizeof(coreid_t *));
    assert(routing_table != NULL);
}

// receive a part of the routing table from RTS (routing table set-up dispatcher)
static void multihop_routing_table_set(struct monitor_binding *b,
                                       coreid_t from, coreid_t *to, size_t len)
{
    // sanity-check input (FIXME: report errors!)
    // FIXME: we don't yet support changes to the existing routing table
    assert(routing_table != NULL);
    assert(from <= routing_table_max_coreid);
    assert(routing_table[from] == NULL);
    assert(len == routing_table_max_coreid + 1);
    routing_table[from] = to;

    if (--routing_table_nentries == 0) {
        // we have received the complete table!
        MULTIHOP_DEBUG("monitor on core %d has received the complete"
                       " routing table (from RTS)\n", my_core_id);
        multihop_print_routing_table();
    }
}

/*
 * Request (my part of) the routing table from another monitor.
 * This method blocks until a reply is received.
 */
errval_t multihop_request_routing_table(struct intermon_binding *b)
{
    errval_t err;

    // request the routing table
    err = b->tx_vtbl.multihop_routing_table_request(b, NOP_CONT, my_core_id);
    if (err_is_fail(err)) {
        return err;
    }

    // wait until we have received a reply
    while (!saw_routing_table_response) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}

// handle request for a portion of the routing table from another monitor
static void multihop_handle_routing_table_request(struct intermon_binding *b,
                                                  coreid_t core_id)
{
    errval_t err;

    if (routing_table != NULL && core_id <= routing_table_max_coreid
        && routing_table[core_id] != NULL) {
        // if we have a routing table, send routing table to other core
        err = b->tx_vtbl.multihop_routing_table_response(b, NOP_CONT,
                SYS_ERR_OK, core_id, routing_table_max_coreid,
                routing_table[core_id], routing_table_max_coreid + 1);
    } else {
        // if we don't have a routing table, send an error reply
        err = b->tx_vtbl.multihop_routing_table_response(b, NOP_CONT,
                MON_ERR_INCOMPLETE_ROUTE, core_id, routing_table_max_coreid,
                NULL, 0);
    }

    assert(err_is_ok(err)); // FIXME
}

// handle the response to a routing table request from the other monitor
static void multihop_handle_routing_table_response(struct intermon_binding *b,
                                                   errval_t err,
                                                   coreid_t source_coreid,
                                                   coreid_t max_coreid,
                                                   coreid_t *to, size_t len)
{
    assert(routing_table == NULL);
    assert(source_coreid == my_core_id);

    if (err_is_ok(err)) {
        assert(to != NULL);
        routing_table = calloc(((uintptr_t)max_coreid) + 1, sizeof(coreid_t *));
        assert(routing_table != NULL);
        routing_table_max_coreid = max_coreid;

        assert(len == max_coreid + 1);
        assert(source_coreid <= max_coreid);
        routing_table[source_coreid] = to;
    } else {
        assert(to == NULL);

        if (err_no(err) != MON_ERR_INCOMPLETE_ROUTE) {
            DEBUG_ERR(err, "unexpected error retrieving routing table");
        }
    }

    saw_routing_table_response = true;
}

// grow the routing table to a set of desination cores, via a given forwarder
static void multihop_routing_table_grow(struct intermon_binding *b,
                                        coreid_t forwarder,
                                        coreid_t *destinations, size_t ndests)
{
    assert(ndests > 0);

    // check the max core ID in the destinations
    coreid_t max_coreid = my_core_id;
    for (unsigned i = 0; i < ndests; i++) {
        if (destinations[i] > max_coreid) {
            max_coreid = destinations[i];
        }
    }

    // ensure we have an allocated routing table; if necessary, grow it
    if (routing_table == NULL) {
        routing_table = calloc(((uintptr_t)max_coreid) + 1, sizeof(coreid_t *));
        assert(routing_table != NULL);
        routing_table_max_coreid = max_coreid;
    } else if (max_coreid > routing_table_max_coreid) {
        for (unsigned i = 0; i <= routing_table_max_coreid; i++) {
            if (routing_table[i] != NULL) {
                routing_table[i] = realloc(routing_table[i],
                                           (((uintptr_t)max_coreid) + 1)
                                           * sizeof(coreid_t));
                assert(routing_table[i] != NULL);
                // XXX: the default for the unconfigured part of the routing
                // table is direct routing
                for (unsigned j = routing_table_max_coreid + 1; j <= max_coreid; j++) {
                    routing_table[i][j] = j;
                }
            }
        }

        routing_table = realloc(routing_table, (((uintptr_t)max_coreid) + 1)
                                               * sizeof(coreid_t *));
        assert(routing_table != NULL);
        memset(&routing_table[routing_table_max_coreid + 1], 0,
               (max_coreid - routing_table_max_coreid) * sizeof(coreid_t *));
        routing_table_max_coreid = max_coreid;
    }

    // ensure I have my own routes (the default is direct routing)
    if (routing_table[my_core_id] == NULL) {
        routing_table[my_core_id] = malloc((((uintptr_t)routing_table_max_coreid) + 1)
                                           * sizeof(coreid_t));
        assert(routing_table[my_core_id] != NULL);
        for (unsigned i = 0; i <= routing_table_max_coreid; i++) {
            routing_table[my_core_id][i] = i;
        }
    }

    // update routes to destinations for all origins in my routing table and myself
    for (unsigned src = 0; src <= routing_table_max_coreid; src++) {
        if (routing_table[src] != NULL) {
            for (unsigned i = 0; i < ndests; i++) {
                routing_table[src][destinations[i]] = routing_table[src][forwarder];
            }
        }
    }

    free(destinations);
}

// return the next hop (based on the routing table)
static inline coreid_t get_next_hop(coreid_t dest)
{

    assert(dest != my_core_id);

    if (routing_table != NULL
        && my_core_id <= routing_table_max_coreid
        && dest <= routing_table_max_coreid
        && routing_table[my_core_id] != NULL) {
        // if we have a routing table, look up next hop
        return routing_table[my_core_id][dest];
    } else {
        // if we don't have a routing table, route directly
        return dest;
    }
}

///////////////////////////////////////////////////////

// FORWARDING (HASH) TABLE

///////////////////////////////////////////////////////

/**
 * Messages are forwarded based on the forwarding table.
 * We use a hash table to map virtual circuit identifiers (VCIs)
 * to a pointer to the channel state.
 */
static collections_hash_table *forwarding_table;

// is forwarding table initialized?
static bool is_forwarding_table_initialized = false;

struct monitor_multihop_chan_state;

// initialize the forwarding table
static inline void init_forwarding_table(void)
{

    if (!is_forwarding_table_initialized) {
        is_forwarding_table_initialized = true;
        collections_hash_create_with_buckets(&forwarding_table,
                MULTIHOP_FORWARDING_TABLE_BUCKETS, free);

        /**
         * We initialize the random function with the current time stamp
         * in order to make assigned VCIs unpredictable. This makes it hard
         * for an attacker that sends message with manipulated VCIs to actually
         * find a valid VCI.
         */
        srand(bench_tsc());
    }
}

// insert entry in forwarding table and return VCI
static inline multihop_vci_t forwarding_table_insert(
        struct monitor_multihop_chan_state *chan_state)
{

    assert(chan_state != NULL);
    multihop_vci_t vci;

    // we call initialize before we insert an entry
    init_forwarding_table();

    do {
        // we assign VCIs randomly, but need to
        // make sure, that it is not yet taken
        vci = (multihop_vci_t) rand();
    } while (collections_hash_find(forwarding_table, vci) != NULL);

    // insert into forwarding table
    collections_hash_insert(forwarding_table, vci, chan_state);
    return vci;
}

// delete entry from forwarding table
static inline void forwarding_table_delete(multihop_vci_t vci)
{
    assert(is_forwarding_table_initialized);
    collections_hash_delete(forwarding_table, vci);
}

// get entry from the forwarding table
static inline struct monitor_multihop_chan_state* forwarding_table_lookup(
        multihop_vci_t vci)
{

    assert(is_forwarding_table_initialized);
    struct monitor_multihop_chan_state *chan_state = collections_hash_find(forwarding_table,
            vci);

    if (chan_state == NULL) {
        USER_PANIC("invalid virtual circuit identifier in multi-hop channel");
    }
    return chan_state;
}

///////////////////////////////////////////////////////

// STRUCT FOR THE PER - CHANNEL STATE

///////////////////////////////////////////////////////

struct monitor_multihop_chan_state {
    struct direction {
        enum {
            MULTIHOP_ENDPOINT, // if this is an endpoint, the communication partner is on the same core
            MULTIHOP_NODE
        // communication partner is a monitor on another core
        } type;

        multihop_vci_t vci; // the virtual circuit identifier to use on outgoing messages

        // bindings to the "next hop"
        union {
            struct monitor_binding *monitor_binding; // used at endpoints to identify the dispatcher
            struct intermon_binding *intermon_binding; // monitor binding of next hop
        } binding;
    } dir1, dir2;

    // temporary storage for a virtual circuit identifier
    multihop_vci_t tmp_vci;

    // connection state
    enum {
        MONTIOR_MULTIHOP_DISCONNECTED, // Disconnected
        MONITOR_MULTIHOP_BIND_WAIT, // Waiting for a bind reply message
        MONITOR_MULTIHOP_CONNECTED,
    // Connection established
    } connstate;
};

// get the direction
static inline struct direction* multihop_get_direction(
        struct monitor_multihop_chan_state *chan_state, uint8_t direction)
{
    if (direction == 1) {
        return &chan_state->dir1;
    } else if (direction == 2) {
        return &chan_state->dir2;
    } else {
        USER_PANIC("unknown direction in multihop channel: %d", direction);
        return NULL;
    }
}

// get the opposite direction
static inline uint8_t multihop_get_opposite_direction(
        struct monitor_multihop_chan_state *chan_state, uint8_t direction,
        struct direction **dir)
{
    if (direction == 2) {
        *dir = &chan_state->dir1;
        return 1;
    } else if (direction == 1) {
        *dir = &chan_state->dir2;
        return 2;
    } else {
        USER_PANIC("unknown direction in multihop channel: %d", direction);
        return 0;
    }
}

////////////////////////////////////////////////////////////

// MULTI-HOP CHANNEL SETUP

////////////////////////////////////////////////////////////

static void
multihop_monitor_bind_request_busy_cont(struct intermon_binding *b,
        struct intermon_msg_queue_elem *e);

static void
multihop_monitor_bind_request_cont(
        struct monitor_multihop_chan_state *chan_state, iref_t iref,
        coreid_t core);

static void
multihop_bind_service_request(uintptr_t service_id,
        struct monitor_multihop_chan_state *chan_state);

static void
multihop_intermon_bind_reply_cont(struct intermon_binding *intermon_binding,
        multihop_vci_t receiver_vci, multihop_vci_t sender_vci, errval_t msgerr);

static void
multihop_monitor_bind_reply_client(struct monitor_binding *domain_binding,
        multihop_vci_t receiver_vci, multihop_vci_t sender_vci, errval_t msgerr);

static inline void
multihop_monitor_request_error(struct monitor_multihop_chan_state *chan_state,
        errval_t msgerr);

/**
 * \brief This method handles a bind request message from a local dispatcher
 *
 * \param b The monitor binding
 * \param iref The iref of the service
 * \param vci The vci of the local dispatcher (this vci should be used for messages sent to the dispatcher)
 */
static void multihop_monitor_bind_request_handler(struct monitor_binding *b,
        iref_t iref, multihop_vci_t vci)
{

    errval_t err;
    coreid_t core_id;
    struct monitor_multihop_chan_state *chan_state = NULL;

    MULTIHOP_DEBUG(
            "monitor on core %d received a bind multi-hop message from a local dispatcher, iref: %d\n", my_core_id, (int) iref);

    // Look up core_id from the iref
    err = iref_get_core_id(iref, &core_id);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "iref_get_core_id failed");
        multihop_monitor_bind_reply_client(b, vci, 0, err); // send back error message
        return;
    }

    // allocate local state for the connection
    chan_state = malloc(sizeof(struct monitor_multihop_chan_state));
    assert(chan_state != NULL);
    chan_state->connstate = MONITOR_MULTIHOP_BIND_WAIT;
    chan_state->dir2.type = MULTIHOP_ENDPOINT;
    chan_state->dir2.vci = vci;
    chan_state->dir2.binding.monitor_binding = b;

    // get a virtual circuit identifier (VCI) for this channel
    // and insert mapping into forwarding table
    chan_state->tmp_vci = forwarding_table_insert(chan_state);

    // make sure that service is not on same core as the client
    if (core_id == my_core_id) {
        multihop_monitor_request_error(chan_state,
                LIB_ERR_BIND_MULTIHOP_SAME_CORE);
        forwarding_table_delete(chan_state->tmp_vci);
        return;
    }

    // determine where to forward the message
    coreid_t next_hop = get_next_hop(core_id);

    // Get connection to the monitor to forward request to
    err = intermon_binding_get(next_hop,
            &chan_state->dir1.binding.intermon_binding);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err,
                "intermon_binding_get failed");
        multihop_monitor_request_error(chan_state, err);
        forwarding_table_delete(chan_state->tmp_vci);
        return;
    }

    // call continuation function
    multihop_monitor_bind_request_cont(chan_state, iref, core_id);
}

struct multihop_monitor_bind_request_state {
    struct intermon_msg_queue_elem elem;
    struct monitor_multihop_chan_state *chan_state;
    iref_t iref;
    coreid_t core;
};

// called when channel is no longer busy
static void multihop_monitor_bind_request_busy_cont(struct intermon_binding *b,
        struct intermon_msg_queue_elem *e)
{
    struct multihop_monitor_bind_request_state *st =
            (struct multihop_monitor_bind_request_state *) e;
    multihop_monitor_bind_request_cont(st->chan_state, st->iref, st->core);
    free(e);
}

/**
 * \brief Sends a bind request to the "next hop"
 *
 * \param chan_state pointer to the channel state
 * \param iref the iref of the service
 * \param core core ID of the service
 */
static void multihop_monitor_bind_request_cont(
        struct monitor_multihop_chan_state *chan_state, iref_t iref,
        coreid_t core)
{

    errval_t err;
    struct intermon_binding *mon_binding =
            chan_state->dir1.binding.intermon_binding;

    // send request to next hop
    err = mon_binding->tx_vtbl.bind_multihop_intermon_request(mon_binding,
            NOP_CONT, iref, chan_state->tmp_vci, core);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct multihop_monitor_bind_request_state *me = malloc(
                    sizeof(struct multihop_monitor_bind_request_state));
            struct intermon_state *ist = mon_binding->st;
            me->chan_state = chan_state;
            me->iref = iref;
            me->elem.cont = multihop_monitor_bind_request_busy_cont;

            err = intermon_enqueue_send(mon_binding, &ist->queue,
                    get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }
        // return error code to client
        multihop_monitor_request_error(chan_state, err);
        forwarding_table_delete(chan_state->tmp_vci);
    }
}

/**
 * \brief Handles a bind request from another monitor
 *
 * \param b intermonitor binding
 * \param iref the iref of the service
 * \param vci The vci to use
 * \param the core ID of the service
 */
static void multihop_intermon_bind_request_handler(struct intermon_binding *b,
        iref_t iref, multihop_vci_t vci, coreid_t core)
{
    errval_t err;

    MULTIHOP_DEBUG(
            "monitor on core %d received multi-hop bind request with vci %d\n", my_core_id, (int) vci);

    // allocate channel state & fill in needed information
    struct monitor_multihop_chan_state *chan_state = malloc(
            sizeof(struct monitor_multihop_chan_state));
    chan_state->connstate = MONITOR_MULTIHOP_BIND_WAIT;
    chan_state->dir2.vci = vci;
    chan_state->dir2.binding.intermon_binding = b;
    chan_state->dir2.type = MULTIHOP_NODE;
    chan_state->tmp_vci = forwarding_table_insert(chan_state);

    if (core == my_core_id) {
        // service is on same core than this monitor, therefore we forward to local dispatcher

        // get the service's connection
        err = iref_get_binding(iref, &chan_state->dir1.binding.monitor_binding);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err,
                    "Multihop set-up: could not get domain-binding for iref");
        }

        // get the service id
        uintptr_t service_id = 0;
        err = iref_get_service_id(iref, &service_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err,
                    "Multihop set-up: could not get service id for iref");
        }

        // forward request to service
        multihop_bind_service_request(service_id, chan_state);

    } else {
        // we have to forward the request to another monitor
        // we get the core id of the next hop from the routing table
        coreid_t next_hop = get_next_hop(core);

        // get connection to the "next-hop" monitor
        err = intermon_binding_get(next_hop,
                &chan_state->dir1.binding.intermon_binding);
        if (err_is_fail(err)) {
            debug_err(__FILE__, __func__, __LINE__, err,
                    "intermon_binding_get failed");
            multihop_monitor_request_error(chan_state, err);
            forwarding_table_delete(chan_state->tmp_vci);
            return;
        }

        // send request to next hop
        multihop_monitor_bind_request_cont(chan_state, iref, core);
    }
}

// used if channel is busy while sending request to service
struct multihop_bind_service_request_state {
    struct monitor_msg_queue_elem elem;
    uintptr_t service_id;
    struct monitor_multihop_chan_state *chan_state;
};

// used when channel is no longer busy
static void multihop_bind_service_busy_cont(struct monitor_binding *b,
        struct monitor_msg_queue_elem *e)
{
    struct multihop_bind_service_request_state *st =
            (struct multihop_bind_service_request_state *) e;
    multihop_bind_service_request(st->service_id, st->chan_state);
    free(e);
}

/**
 * \brief Forward bind request to service's dispatcher
 *
 * \param domain_binding binding to service
 * \param service_id Id of the service
 * \param vci my vci
 */
static void multihop_bind_service_request(uintptr_t service_id,
        struct monitor_multihop_chan_state *chan_state)
{
    errval_t err;
    MULTIHOP_DEBUG(
            "monitor on core %d is forwarding bind request to local dispatcher...\n", my_core_id);
    err =
            chan_state->dir1.binding.monitor_binding->tx_vtbl.multihop_bind_service_request(
                    chan_state->dir1.binding.monitor_binding, NOP_CONT,
                    service_id, chan_state->tmp_vci);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct monitor_binding *monitor_binding =
                    chan_state->dir1.binding.monitor_binding;
            struct multihop_bind_service_request_state *me = malloc(
                    sizeof(struct multihop_bind_service_request_state));
            struct monitor_state *ist = monitor_binding->st;
            me->service_id = service_id;
            me->chan_state = chan_state;
            me->elem.cont = &multihop_bind_service_busy_cont;

            err = monitor_enqueue_send(monitor_binding, &ist->queue,
                    get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        // return error code to client
        multihop_monitor_request_error(chan_state, err);
        forwarding_table_delete(chan_state->tmp_vci);
    }
}

/**
 * \brief Handle a reply message coming from service's dispatcher
 *
 * \param mon_binding Binding to service's dispatcher
 * \param my_vci my virtual circuit identifier
 * \param sender_vci virtual circuit identifier of the sender
 * \param msgerr error code
 */
static void multihop_monitor_service_bind_reply_handler(
        struct monitor_binding *mon_binding, multihop_vci_t receiver_vci,
        multihop_vci_t sender_vci, errval_t msgerr)
{
    MULTIHOP_DEBUG(
            "monitor on core %d received bind reply message. Status: %s. my_vci: %d\n", my_core_id, err_is_ok(msgerr) ? "success" : "failed", (int) receiver_vci);

    struct monitor_multihop_chan_state *chan_state = forwarding_table_lookup(
            receiver_vci);

    assert(chan_state->connstate == MONITOR_MULTIHOP_BIND_WAIT);

    multihop_vci_t next_receiver_vci = chan_state->dir2.vci;
    struct intermon_binding *next_hop_binding =
            chan_state->dir2.binding.intermon_binding;
    if (err_is_ok(msgerr)) { /* bind succeeded */
        chan_state->dir1.type = MULTIHOP_ENDPOINT;
        chan_state->dir1.vci = sender_vci;
        chan_state->dir1.binding.monitor_binding = mon_binding;
        chan_state->connstate = MONITOR_MULTIHOP_CONNECTED;
    } else {
        // delete entry from forwarding table
        forwarding_table_delete(receiver_vci);
    }

    // (stack-ripped) forward reply to next monitor
    multihop_intermon_bind_reply_cont(next_hop_binding, next_receiver_vci,
            receiver_vci, msgerr);
}

struct multihop_intermon_bind_reply_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_bind_multihop_intermon_reply__args args;
};

// called when channel is no longer busy
static void multihop_intermon_bind_reply_busy_cont(struct intermon_binding *b,
        struct intermon_msg_queue_elem *e)
{
    struct multihop_intermon_bind_reply_state *st =
            (struct multihop_intermon_bind_reply_state *) e;
    multihop_intermon_bind_reply_cont(b, st->args.receiver_vci,
            st->args.sender_vci, st->args.err);
    free(e);
}

/**
 * \brief Forward a bind reply message to the next monitor
 *
 * \param intermon_binding binding to the next monitor
 */
static void multihop_intermon_bind_reply_cont(
        struct intermon_binding *intermon_binding, multihop_vci_t receiver_vci,
        multihop_vci_t sender_vci, errval_t msgerr)
{
    errval_t err;
    MULTIHOP_DEBUG("monitor on core %d is forwarding reply\n", my_core_id);
    err = intermon_binding->tx_vtbl.bind_multihop_intermon_reply(
            intermon_binding, NOP_CONT, receiver_vci, sender_vci, msgerr);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct multihop_intermon_bind_reply_state *me = malloc(
                    sizeof(struct multihop_intermon_bind_reply_state));
            struct intermon_state *ist = intermon_binding->st;
            me->args.sender_vci = sender_vci;
            me->args.receiver_vci = receiver_vci;
            me->args.err = msgerr;
            me->elem.cont = multihop_intermon_bind_reply_busy_cont;

            err = intermon_enqueue_send(intermon_binding, &ist->queue,
                    get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }USER_PANIC_ERR(err,
                "Could not forward bind reply message in multi-hop channel");
    }
}

/**
 * \brief Handles a reply message from another monitor
 *
 * \param binding Binding to the other monitor
 * \param my_vci My virtual circuit identifier
 * \param sender_vci virtual circuit identifier of the sender
 * \param msgerr error code
 */
static void multihop_intermon_bind_reply_handler(
        struct intermon_binding *binding, multihop_vci_t receiver_vci,
        multihop_vci_t sender_vci, errval_t msgerr)
{
    MULTIHOP_DEBUG(
            "monitor on core %d has received a bind reply\n", my_core_id);
    struct monitor_multihop_chan_state *chan_state = forwarding_table_lookup(
            receiver_vci);

    assert(chan_state->connstate == MONITOR_MULTIHOP_BIND_WAIT);

    if (err_is_ok(msgerr)) {
        chan_state->dir1.type = MULTIHOP_NODE;
        chan_state->dir1.binding.intermon_binding = binding;
        chan_state->dir1.vci = sender_vci;
        chan_state->connstate = MONITOR_MULTIHOP_CONNECTED;

        if (chan_state->dir2.type == MULTIHOP_NODE) {
            multihop_intermon_bind_reply_cont(
                    chan_state->dir2.binding.intermon_binding,
                    chan_state->dir2.vci, receiver_vci, msgerr);
        } else {
            multihop_monitor_bind_reply_client(
                    chan_state->dir2.binding.monitor_binding,
                    chan_state->dir2.vci, receiver_vci, msgerr);
        }
    } else {

        // connection was refused

        if (chan_state->dir2.type == MULTIHOP_NODE) {
            multihop_intermon_bind_reply_cont(
                    chan_state->dir2.binding.intermon_binding,
                    chan_state->dir2.vci, 0, msgerr);
        } else {
            multihop_monitor_bind_reply_client(
                    chan_state->dir2.binding.monitor_binding,
                    chan_state->dir2.vci, 0, msgerr);
        }

        // delete entry from forwarding table
        forwarding_table_delete(receiver_vci);
    }
}

struct multihop_monitor_bind_reply_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_multihop_bind_client_reply__args args;
};

// continue function to forward a message to a dispatcher
static void multihop_monitor_bind_reply_busy_cont(struct monitor_binding *b,
        struct monitor_msg_queue_elem *e)
{
    struct multihop_monitor_bind_reply_state *st =
            (struct multihop_monitor_bind_reply_state *) e;
    multihop_monitor_bind_reply_client(b, st->args.receiver_vci,
            st->args.sender_vci, st->args.err);
    free(e);
}

/**
 * \brief Send a reply to the dispatcher who originally sent the request
 *
 * \param domain_binding The monitor_binding to use
 * \param receiver_vci The VCI of the receiver
 * \param sender_vci The VCI of the sender
 * \param msgerr The error code
 */
static void multihop_monitor_bind_reply_client(
        struct monitor_binding *domain_binding, multihop_vci_t receiver_vci,
        multihop_vci_t sender_vci, errval_t msgerr)
{
    errval_t err;
    MULTIHOP_DEBUG(
            "monitor on core %d is sending reply to dispatcher\n", my_core_id);
    err = domain_binding->tx_vtbl.multihop_bind_client_reply(domain_binding,
            NOP_CONT, receiver_vci, sender_vci, msgerr);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct multihop_monitor_bind_reply_state *me = malloc(
                    sizeof(struct multihop_monitor_bind_reply_state));
            assert(me != NULL);
            struct monitor_state *ist = domain_binding->st;
            me->args.receiver_vci = receiver_vci;
            me->args.sender_vci = sender_vci;
            me->args.err = msgerr;
            me->elem.cont = multihop_monitor_bind_reply_busy_cont;

            err = monitor_enqueue_send(domain_binding, &ist->queue,
                    get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;

        }

        USER_PANIC_ERR(
                err,
                "Could not forward bind reply to client's dispatcher in multi-hop channel");
    }
}

/**
 * \brief send an error code back
 *
 */
static inline void multihop_monitor_request_error(
        struct monitor_multihop_chan_state *chan_state, errval_t msgerr)
{
    assert(chan_state != NULL);
    if (chan_state->dir2.type == MULTIHOP_NODE) {
        multihop_intermon_bind_reply_cont(
                chan_state->dir2.binding.intermon_binding, chan_state->dir2.vci,
                0, msgerr);
    } else {
        multihop_monitor_bind_reply_client(
                chan_state->dir2.binding.monitor_binding, chan_state->dir2.vci,
                0, msgerr);
    }
}

///////////////////////////////////////////////////////

// MESSAGE FORWARDING

///////////////////////////////////////////////////////

static inline void multihop_message_monitor_forward(struct monitor_binding *b,
        multihop_vci_t vci, uint8_t direction, uint8_t flags, uint32_t ack,
        uint8_t *payload, size_t size, bool first_try);

static void multihop_message_forward_continue(struct monitor_binding *b,
        struct monitor_msg_queue_elem *e);

static inline void multihop_message_intermon_forward(struct intermon_binding *b,
        multihop_vci_t vci, uint8_t direction, uint8_t flags, uint32_t ack,
        uint8_t *payload, size_t size, bool first_try);

static void multihop_message_intermon_forward_cont(struct intermon_binding *b,
        struct intermon_msg_queue_elem *e);

// monitor message forwarding state
struct monitor_multihop_message_forwarding_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_multihop_message__args args;
};

// inter-monitor forwarding state
struct intermon_message_forwarding_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_multihop_message__args args;
};

/**
 * \brief Handle a multi-hop message coming from a local dispatcher.
 *        The message must be forwarded to the next hop.
 *
 * \param mon_binding the monitor binding
 * \param vci the virtual circuit identifier of the message
 * \param direction direction of the message
 * \param flags message flags
 * \param ack number of messages acknowledged with this message
 * \param payload pointer to the message payload
 * \size size of the message payload
 *
 */
static void multihop_message_handler(struct monitor_binding *mon_binding,
        multihop_vci_t vci, uint8_t direction, uint8_t flags, uint32_t ack,
        uint8_t *payload, size_t size)
{

    MULTIHOP_DEBUG(
            "monitor on core %d received multi-hop message (from local dispatcher). VCI %llu, direction %d, flags %d, ack %d\n", my_core_id, (unsigned long long) vci, direction, flags, ack);

    // get forwarding information
    errval_t err;
    struct monitor_multihop_chan_state *chan_state = forwarding_table_lookup(
            vci);
    struct direction *dir = multihop_get_direction(chan_state, direction);
    struct intermon_binding *b = dir->binding.intermon_binding;

    struct intermon_state *ist = b->st;
    if (msg_queue_is_empty(&ist->queue)) {

        // if the message queue is empty, we can directly forward
        // the message
        multihop_message_intermon_forward(b, dir->vci, direction, flags, ack,
                payload, size, true);
    } else {
        // if the message queue is not empty, we have to
        // enqueue the message (to make sure we do not bypass
        // other messages)
        struct intermon_message_forwarding_state *me = malloc(
                sizeof(struct intermon_message_forwarding_state));
        me->args.vci = dir->vci;
        me->args.direction = direction;
        me->args.flags = flags;
        me->args.ack = ack;
        me->args.payload = payload;
        me->args.size = size;
        me->elem.cont = multihop_message_intermon_forward_cont;

        err = intermon_enqueue_send(b, &ist->queue, get_default_waitset(),
                &me->elem.queue);
        assert(err_is_ok(err));
    }
}

// continue function for intermonitor message forwarding
static void multihop_message_intermon_forward_cont(struct intermon_binding *b,
        struct intermon_msg_queue_elem *e)
{

    struct intermon_message_forwarding_state *st =
            (struct intermon_message_forwarding_state *) e;

    multihop_message_intermon_forward(b, st->args.vci, st->args.direction,
            st->args.flags, st->args.ack, st->args.payload, st->args.size,
            false);
    free(e);
}

/**
 * \brief Forward a message to another monitor.
 *
 */
static inline void multihop_message_intermon_forward(struct intermon_binding *b,
        multihop_vci_t vci, uint8_t direction, uint8_t flags, uint32_t ack,
        uint8_t *payload, size_t size, bool first_try)
{

    errval_t err;

    // try to forward message
    err = b->tx_vtbl.multihop_message(b, MKCONT(free, payload), vci, direction,
            flags, ack, payload, size);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct intermon_message_forwarding_state *me = malloc(
                    sizeof(struct intermon_message_forwarding_state));
            struct intermon_state *ist = b->st;
            me->args.vci = vci;
            me->args.direction = direction;
            me->args.flags = flags;
            me->args.ack = ack;
            me->args.payload = payload;
            me->args.size = size;
            me->elem.cont = multihop_message_intermon_forward_cont;

            if (first_try) {
                // if this is the first time that we try to send this message
                // we can enqueue it at the back of the message queue
                err = intermon_enqueue_send(b, &ist->queue,
                        get_default_waitset(), &me->elem.queue);
            } else {
                // if this is NOT the first time that we try to send this message
                // we have to enqueue it at the FRONT to make sure that the
                // original message order is preserved
                err = intermon_enqueue_send_at_front(b, &ist->queue,
                        get_default_waitset(), &me->elem.queue);
            }

            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "Could not forward multi-hop message\n");
    }
}

/**
 * \brief Handle a message coming from another monitor. We have
 *        to forward the message either to another monitor or
 *        to a dispatcher
 *
 * \param mon_binding the monitor binding
 * \param vci the virtual circuit identifier of the message
 * \param direction direction of the message
 * \param flags message flags
 * \param ack number of messages acknowledged with this message
 * \param payload pointer to the message payload
 * \size size of the message payload
 */
static void intermon_multihop_message_handler(struct intermon_binding *binding,
        multihop_vci_t vci, uint8_t direction, uint8_t flags, uint32_t ack,
        uint8_t *payload, size_t size)
{

    MULTIHOP_DEBUG(
            "monitor on core %d received multi-hop message (from other monitor). VCI %llu, direction %d, flags %d, ack %d\n", my_core_id, (unsigned long long) vci, direction, flags, ack);

    errval_t err;
    struct monitor_multihop_chan_state *chan_state = forwarding_table_lookup(
            vci);
    struct direction *dir = multihop_get_direction(chan_state, direction);

    if (dir->type == MULTIHOP_ENDPOINT) {
        // we have to forward the message to a local dispatcher
        struct monitor_binding *b = dir->binding.monitor_binding;
        struct monitor_state *ist = b->st;

        if (msg_queue_is_empty(&ist->queue)) {
            // if the message queue is empty, we can directly forward
            // the message
            multihop_message_monitor_forward(b, dir->vci, direction, flags, ack,
                    payload, size, true);
        } else {
            // if the message queue is not empty, we have to
            // enqueue the message (to make sure we do not bypass
            // other messages)
            struct monitor_multihop_message_forwarding_state *me = malloc(
                    sizeof(struct monitor_multihop_message_forwarding_state));
            assert(me != NULL);
            me->args.vci = dir->vci;
            me->args.direction = direction;
            me->args.flags = flags;
            me->args.ack = ack;
            me->args.payload = payload;
            me->args.size = size;
            me->elem.cont = multihop_message_forward_continue;

            err = monitor_enqueue_send(b, &ist->queue, get_default_waitset(),
                    &me->elem.queue);
            assert(err_is_ok(err));
        }
        return;
    } else {
        // we have to forward the message to the next hop (--> another monitor)
        struct intermon_binding *b = dir->binding.intermon_binding;
        struct intermon_state *ist = b->st;

        if (msg_queue_is_empty(&ist->queue)) {
            // message queue is empty --> send directly
            multihop_message_intermon_forward(b, dir->vci, direction, flags,
                    ack, payload, size, true);
        } else {
            // enqueue message
            struct intermon_message_forwarding_state *me = malloc(
                    sizeof(struct intermon_message_forwarding_state));
            me->args.vci = dir->vci;
            me->args.direction = direction;
            me->args.flags = flags;
            me->args.ack = ack;
            me->args.payload = payload;
            me->args.size = size;
            me->elem.cont = multihop_message_intermon_forward_cont;

            err = intermon_enqueue_send(b, &ist->queue, get_default_waitset(),
                    &me->elem.queue);
            assert(err_is_ok(err));
        }
        return;
    }
}

// continue function to forward a message to a dispatcher
static void multihop_message_forward_continue(struct monitor_binding *b,
        struct monitor_msg_queue_elem *e)
{

    struct monitor_multihop_message_forwarding_state *st =
            (struct monitor_multihop_message_forwarding_state *) e;

    multihop_message_monitor_forward(b, st->args.vci, st->args.direction,
            st->args.flags, st->args.ack, st->args.payload, st->args.size,
            false);
    free(e);
}

/**
 * \brief Forward a message to a dispatcher
 */
static inline void multihop_message_monitor_forward(struct monitor_binding *b,
        multihop_vci_t vci, uint8_t direction, uint8_t flags, uint32_t ack,
        uint8_t *payload, size_t size, bool first_try)
{

    errval_t err;

    // try to forward message
    err = b->tx_vtbl.multihop_message(b, MKCONT(free, payload), vci, direction,
            flags, ack, payload, size);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct monitor_multihop_message_forwarding_state *me = malloc(
                    sizeof(struct monitor_multihop_message_forwarding_state));
            assert(me != NULL);
            struct monitor_state *ist = b->st;
            me->args.vci = vci;
            me->args.direction = direction;
            me->args.flags = flags;
            me->args.ack = ack;
            me->args.payload = payload;
            me->args.size = size;
            me->elem.cont = multihop_message_forward_continue;

            if (first_try) {
                err = monitor_enqueue_send(b, &ist->queue,
                        get_default_waitset(), &me->elem.queue);
            } else {
                err = monitor_enqueue_send_at_front(b, &ist->queue,
                        get_default_waitset(), &me->elem.queue);
            }

            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "failed forwarding multihop message\n");
    }
}

///////////////////////////////////////////////////////

// CAPABILITY FORWARDING

///////////////////////////////////////////////////////

static void multihop_cap_send_intermon_forward_cont(struct intermon_binding *b,
        struct intermon_msg_queue_elem *e);

static inline void multihop_cap_send_intermon_forward(
        struct intermon_binding *b, multihop_vci_t vci, uint8_t direction,
        uint32_t capid, errval_t msgerr, intermon_caprep_t caprep, bool null_cap);

static void multihop_cap_send_forward_cont(struct monitor_binding *b,
        struct monitor_msg_queue_elem *e);

inline static void multihop_cap_send_forward(struct monitor_binding *b,
        multihop_vci_t vci, uint8_t direction, uint32_t capid, errval_t msgerr,
        struct capref cap);

// intermonitor capability forwarding state
struct multihop_intermon_capability_forwarding_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_multihop_cap_send__args args;
};

// monitor capability forwarding state
struct multihop_capability_forwarding_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_multihop_cap_send__args args;
};

/**
 * \brief Handle capability send request from local monitor.
 *        The capability must be forwarded to the next hop.
 *
 * \param monitor_binding
 * \param vci the virtual circuit identifier (VCI)
 * \param direction the direction
 * \param cap reference to the capability
 * \capid ID of the capability
 *
 */
static void multihop_cap_send_request_handler(
        struct monitor_binding *monitor_binding, multihop_vci_t vci,
        uint8_t direction, errval_t msgerr, struct capref cap, uint32_t capid)
{

    MULTIHOP_DEBUG(
            "monitor on core %d received a capability (from local dispatcher). VCI %llu, direction %d, cap ID %d\n", my_core_id, (unsigned long long) vci, direction, capid);

    errval_t err;
    struct capability capability;
    intermon_caprep_t caprep;
    memset(&caprep, 0, sizeof(caprep));
    bool null_cap = capref_is_null(cap);
    bool has_descendants;

    // XXX: this field is ignored when the local dispatcher originates the cap
    msgerr = SYS_ERR_OK;

    // get forwarding information
    struct monitor_multihop_chan_state *chan_state = forwarding_table_lookup(
            vci);
    struct direction *dir = multihop_get_direction(chan_state, direction);
    struct intermon_binding *b = dir->binding.intermon_binding;

    if (!null_cap) {
        // get binary representation of capability
        err = monitor_cap_identify(cap, &capability);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "monitor_cap_identify failed, ignored");
            return;
        }

        // if we can't transfer the cap, it is delivered as NULL
        if (!monitor_can_send_cap(&capability)) {
            cap = NULL_CAP;
            null_cap = true;
            msgerr = MON_ERR_CAP_SEND;
        }
    }

    if (!null_cap) {
        // FIXME: this seems to be totally bogus. it assumes a give_away cap -AB

        // mark capability as remote
        err = monitor_cap_remote(cap, true, &has_descendants);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "monitor_cap_remote failed");
            return;
        }

        // XXX: This is a typedef of struct that flounder is generating.
        // Flounder should not be generating this and we shouldn't be using it.
        capability_to_caprep(&capability, &caprep);

        // destroy capability on this core
        err = cap_destroy(cap);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap destroy failed");
        }
    }

    // enqueue capability in order to be forwarded
    struct multihop_intermon_capability_forwarding_state *me = malloc(
            sizeof(struct multihop_intermon_capability_forwarding_state));
    struct intermon_state *ist = b->st;
    me->args.vci = dir->vci;
    me->args.direction = direction;
    me->args.capid = capid;
    me->args.err = msgerr;
    me->args.cap = caprep;
    me->args.null_cap = null_cap;
    me->elem.cont = multihop_cap_send_intermon_forward_cont;

    err = intermon_enqueue_send(b, &ist->queue, get_default_waitset(),
            &me->elem.queue);
    assert(err_is_ok(err));
}

// continue function for intermonitor capability forwarding
static void multihop_cap_send_intermon_forward_cont(struct intermon_binding *b,
        struct intermon_msg_queue_elem *e)
{
    struct multihop_intermon_capability_forwarding_state *st =
            (struct multihop_intermon_capability_forwarding_state *) e;
    multihop_cap_send_intermon_forward(b, st->args.vci, st->args.direction,
        st->args.capid, st->args.err, st->args.cap, st->args.null_cap);
    free(e);
}

/**
 * \brief Forward capability to the next hop
 *
 */
static inline void multihop_cap_send_intermon_forward(
        struct intermon_binding *b, multihop_vci_t vci, uint8_t direction,
        uint32_t capid, errval_t msgerr, intermon_caprep_t caprep, bool null_cap)
{

    errval_t err;

    // try to forward
    err = b->tx_vtbl.multihop_cap_send(b, NOP_CONT, vci, direction, capid, msgerr,
            caprep, null_cap);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct multihop_intermon_capability_forwarding_state *me =
                    malloc(
                            sizeof(struct multihop_intermon_capability_forwarding_state));
            struct intermon_state *ist = b->st;
            me->args.vci = vci;
            me->args.direction = direction;
            me->args.capid = capid;
            me->args.err = msgerr;
            me->args.cap = caprep;
            me->args.null_cap = null_cap;
            me->elem.cont = multihop_cap_send_intermon_forward_cont;

            err = intermon_enqueue_send_at_front(b, &ist->queue,
                    get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err,
                "Could not forward capability over multi-hop channel\n");
    }
}

/**
 * \brief Handle a capability coming from another monitor.
 *        The capability must be either forwarded to another
 *        monitor or to a local dispatcher.
 *
 */
static void multihop_intermon_cap_send_handler(
        struct intermon_binding *intermon_binding, multihop_vci_t vci,
        uint8_t direction, uint32_t capid, errval_t msgerr,
        intermon_caprep_t caprep, bool null_cap)
{

    MULTIHOP_DEBUG(
            "monitor on core %d received a capability (from other monitor). VCI %llu, direction %d, cap ID %d\n", my_core_id, (unsigned long long) vci, direction, capid);

    errval_t err;
    struct monitor_multihop_chan_state *chan_state = forwarding_table_lookup(
            vci);
    struct direction *dir = multihop_get_direction(chan_state, direction);

    if (dir->type == MULTIHOP_ENDPOINT) {
        // we have to forward the message to a local dispatcher

        // Construct the capability
        struct capability *capability = (struct capability *) &caprep;
        struct capref cap;

        if (null_cap) {
            cap = NULL_CAP;
        } else {
            err = slot_alloc(&cap);
            if (err_is_fail(err)) {

                // send a msg indicating that we failed
                // to allocate a slot for the capability
                cap = NULL_CAP;
                msgerr = err;
                goto do_send;
            }

            // create capability
            // note that we just pass anything as core_id, because
            // it is not being used
            err = monitor_cap_create(cap, capability, my_core_id);
            if (err_is_fail(err)) {
                slot_free(cap);

                // send a msg indicating that we failed
                // to create the capability
                cap = NULL_CAP;
                msgerr = err_push(err, MON_ERR_CAP_CREATE);
                goto do_send;
            }

            // mark capability as remote
            bool has_descendants;
            err = monitor_cap_remote(cap, true, &has_descendants);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_cap_remote failed");
                return;
            }
        }

do_send: ;
        // enqueue the capability in order to be forwarded to
        // the local dispatcher
        struct monitor_binding *b = dir->binding.monitor_binding;
        struct multihop_capability_forwarding_state *me = malloc(
                sizeof(struct multihop_capability_forwarding_state));
        assert(me != NULL);
        struct monitor_state *ist = b->st;
        me->args.vci = dir->vci;
        me->args.direction = direction;
        me->args.cap = cap;
        me->args.capid = capid;
        me->args.err = msgerr;
        me->elem.cont = multihop_cap_send_forward_cont;

        err = monitor_enqueue_send(b, &ist->queue, get_default_waitset(),
                &me->elem.queue);
        assert(err_is_ok(err));
        return;

    } else {
        // we have to forward the capability to the next hop
        // we therefore enqueue the capability
        struct intermon_binding *b = dir->binding.intermon_binding;
        struct multihop_intermon_capability_forwarding_state *me = malloc(
                sizeof(struct multihop_intermon_capability_forwarding_state));
        struct intermon_state *ist = b->st;
        me->args.vci = dir->vci;
        me->args.direction = direction;
        me->args.capid = capid;
        me->args.err = msgerr;
        me->args.cap = caprep;
        me->args.null_cap = null_cap;
        me->elem.cont = multihop_cap_send_intermon_forward_cont;

        err = intermon_enqueue_send(b, &ist->queue, get_default_waitset(),
                &me->elem.queue);
        assert(err_is_ok(err));
        return;
    }
}

// continue function for monitor capability forwarding
static void multihop_cap_send_forward_cont(struct monitor_binding *b,
        struct monitor_msg_queue_elem *e)
{
    struct multihop_capability_forwarding_state *st =
            (struct multihop_capability_forwarding_state *) e;
    multihop_cap_send_forward(b, st->args.vci, st->args.direction,
                              st->args.capid, st->args.err, st->args.cap);
    free(e);
}

/**
 * \brief Forward capability to a local dispatcher
 *
 */
inline static void multihop_cap_send_forward(struct monitor_binding *b,
        multihop_vci_t vci, uint8_t direction, uint32_t capid, errval_t msgerr,
        struct capref cap)
{
    errval_t err;

// try to send
    err = b->tx_vtbl.multihop_cap_send(b, NOP_CONT, vci, direction, msgerr,
                                       cap, capid);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct multihop_capability_forwarding_state *me = malloc(
                    sizeof(struct multihop_capability_forwarding_state));
            assert(me != NULL);
            struct monitor_state *ist = b->st;
            me->args.vci = vci;
            me->args.direction = direction;
            me->args.cap = cap;
            me->args.capid = capid;
            me->args.err = msgerr;
            me->elem.cont = multihop_cap_send_forward_cont;

            err = monitor_enqueue_send_at_front(b, &ist->queue,
                    get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err,
                "failed to forward capability over multi-hop channel\n");
    }
}

///////////////////////////////////////////////////////

// INITIALIZATION

///////////////////////////////////////////////////////

// set up receive vtable in the intermonitor interface
errval_t multihop_intermon_init(struct intermon_binding *ib)
{
    ib->rx_vtbl.bind_multihop_intermon_request =
            &multihop_intermon_bind_request_handler;
    ib->rx_vtbl.bind_multihop_intermon_reply =
            &multihop_intermon_bind_reply_handler;
    ib->rx_vtbl.multihop_message = &intermon_multihop_message_handler;
    ib->rx_vtbl.multihop_cap_send = &multihop_intermon_cap_send_handler;
    ib->rx_vtbl.multihop_routing_table_request =
            &multihop_handle_routing_table_request;
    ib->rx_vtbl.multihop_routing_table_response =
            &multihop_handle_routing_table_response;
    ib->rx_vtbl.multihop_routing_table_grow =
            &multihop_routing_table_grow;

    return SYS_ERR_OK;
}

// set up receive vtable in the monitor interface
errval_t multihop_monitor_init(struct monitor_binding *mb)
{
    mb->rx_vtbl.multihop_bind_client_request =
            &multihop_monitor_bind_request_handler;
    mb->rx_vtbl.multihop_bind_service_reply =
            &multihop_monitor_service_bind_reply_handler;
    mb->rx_vtbl.multihop_message = &multihop_message_handler;
    mb->rx_vtbl.multihop_cap_send = &multihop_cap_send_request_handler;
    mb->rx_vtbl.multihop_routing_table_new =
            &multihop_routing_table_new;
    mb->rx_vtbl.multihop_routing_table_set =
            &multihop_routing_table_set;

    return SYS_ERR_OK;
}
