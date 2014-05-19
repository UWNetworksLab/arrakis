/**
 * \file
 * \brief Bidirectional UMP channel
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_UMP_CHAN_H
#define BARRELFISH_UMP_CHAN_H

#include <sys/cdefs.h>

#include <barrelfish/ump_endpoint.h>
#include <barrelfish/monitor_client.h>

__BEGIN_DECLS

struct ump_chan;
struct monitor_binding;

struct ump_bind_continuation {
    /**
     * \brief Handler which runs when a binding succeeds or fails
     * \param st State pointer set in closure
     * \param err Success/failure of binding
     * \param uc On success, contains pointer to channel
     * \param notify On success, contains (optional) notification capability
     */
    void (*handler)(void *st, errval_t err, struct ump_chan *uc,
                    struct capref notify);
    void *st;
};

/// A bidirectional UMP channel
struct ump_chan {
    struct monitor_cap_handlers cap_handlers;   /* XXX: must be first */

    struct ump_chan_state send_chan;       ///< Outgoing UMP channel state
    struct ump_endpoint endpoint;          ///< Incoming UMP endpoint

    /// connection state
    enum {UMP_DISCONNECTED,     ///< Disconnected
          UMP_BIND_WAIT,        ///< Waiting for bind reply
          UMP_CONNECTED,        ///< Connection established
    } connstate;

    struct capref frame;        ///< Cap to shared frame
    struct vregion *vregion;    ///< VRegion for shared frame
    ump_index_t max_send_msgs;  ///< Number of messages that fit in the send channel
    ump_index_t max_recv_msgs;  ///< Number of messages that fit in the recv channel
    uintptr_t monitor_id;       ///< Local monitor's connection ID for this channel
    struct monitor_binding *monitor_binding; ///< Monitor binding used for cap xfer

    uintptr_t sendid;  ///< id for tracing
    uintptr_t recvid;  ///< id for tracing

    /* Arguments for an ongoing bind attempt */
    iref_t iref;                ///< IREF to which we bound
    size_t inchanlen, outchanlen;
    struct capref notify_cap;
    struct ump_bind_continuation bind_continuation; ///< Continuation for bind
};

struct event_queue_node;

errval_t ump_chan_init(struct ump_chan *uc,
                       volatile void *inbuf, size_t inbufsize,
                       volatile void *outbuf, size_t outbufsize);
errval_t ump_chan_bind(struct ump_chan *uc, struct ump_bind_continuation cont,
                       struct event_queue_node *qnode,  iref_t iref,
                       struct monitor_binding *monitor_binding,
                       size_t inchanlen, size_t outchanlen,
                       struct capref notify_cap);
errval_t ump_chan_accept(struct ump_chan *uc, uintptr_t mon_id,
                         struct capref frame, size_t inchanlen, size_t outchanlen);
void ump_chan_send_bind_reply(struct monitor_binding *mb,
                              struct ump_chan *uc, errval_t err,
                              uintptr_t monitor_id, struct capref notify_cap);
void ump_chan_destroy(struct ump_chan *uc);
void ump_init(void);

/**
 * \brief Register an event handler to be notified when messages can be received
 *
 * In the future, call the closure on the given waitset when it is likely that
 * a message can be received on the channel. A channel may only be registered
 * with a single receive event handler on a single waitset at any one time.
 *
 * \param uc UMP channel
 * \param ws Waitset
 * \param closure Event handler
 */
static inline errval_t ump_chan_register_recv(struct ump_chan *uc,
                                               struct waitset *ws,
                                               struct event_closure closure)
{
    return ump_endpoint_register(&uc->endpoint, ws, closure);
}

/**
 * \brief Cancel an event registration made with ump_chan_register_recv()
 *
 * \param uc UMP channel
 */
static inline errval_t ump_chan_deregister_recv(struct ump_chan *uc)
{
    return ump_endpoint_deregister(&uc->endpoint);
}

static inline errval_t ump_chan_recv(struct ump_chan *uc,
                                     volatile struct ump_message **msg)
{
    assert(msg != NULL);
    return ump_endpoint_recv(&uc->endpoint, msg);
}

static inline volatile struct ump_message *ump_chan_get_next(
                                struct ump_chan *uc, struct ump_control *ctrl)
{
    return ump_impl_get_next(&uc->send_chan, ctrl);
}

/**
 * \brief Migrate an event registration made with
 * ump_chan_register_recv() to a new waitset
 *
 * \param lc LMP channel
 * \param ws New waitset
 */
static inline void ump_chan_migrate_recv(struct ump_chan *lc,
                                         struct waitset *ws)
{
    ump_endpoint_migrate(&lc->endpoint, ws);
}

__END_DECLS

#endif // BARRELFISH_UMP_CHAN_H
