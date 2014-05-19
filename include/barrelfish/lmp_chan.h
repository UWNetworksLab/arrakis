/**
 * \file
 * \brief Bidirectional LMP channel
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_LMP_CHAN_H
#define BARRELFISH_LMP_CHAN_H

#include <sys/cdefs.h>

#include <barrelfish/waitset.h>
#include <barrelfish/lmp_endpoints.h>
#include <barrelfish/idc.h>

__BEGIN_DECLS

struct lmp_chan;
struct event_queue_node;

struct lmp_bind_continuation {
    /**
     * \brief Handler which runs when a binding succeeds or fails
     * \param st State pointer set in closure
     * \param err Success/failure of binding
     * \param lc On success, contains pointer to channel
     */
    void (*handler)(void *st, errval_t err, struct lmp_chan *lc);
    void *st;
};

/// A bidirectional LMP channel
struct lmp_chan {
    struct waitset_chanstate send_waitset; ///< State belonging to waitset (for send)
    struct lmp_chan *next, *prev; ///< Next/prev in list of channels with send events
    struct capref local_cap, remote_cap;   ///< Capabilities to local/remote endpoints
    struct lmp_endpoint *endpoint;         ///< Incoming LMP endpoint (in dispatcher)

    /// connection state
    enum {LMP_DISCONNECTED,     ///< Disconnected
          LMP_BIND_WAIT,        ///< Waiting for bind reply
          LMP_MONITOR_ACCEPT,   ///< Special case for monitor binding: waiting for cap
          LMP_CONNECTED,        ///< Connection established
    } connstate;

    /* Arguments for an ongoing bind attempt */
    struct monitor_binding *monitor_binding;
    struct lmp_bind_continuation bind_continuation; ///< Continuation for bind
    iref_t iref;            ///< IREF
    size_t buflen_words;    ///< requested LMP buffer length, in words
};

void lmp_chan_init(struct lmp_chan *lc);
void lmp_chan_destroy(struct lmp_chan *lc);
errval_t lmp_chan_bind(struct lmp_chan *lc, struct lmp_bind_continuation cont,
                       struct event_queue_node *qnode, iref_t iref,
                       size_t buflen_words);
errval_t lmp_chan_accept(struct lmp_chan *lc, size_t buflen_words,
                         struct capref endpoint);
errval_t lmp_chan_register_send(struct lmp_chan *lc, struct waitset *ws,
                                struct event_closure closure);
errval_t lmp_chan_deregister_send(struct lmp_chan *lc);
void lmp_chan_migrate_send(struct lmp_chan *lc, struct waitset *ws);
errval_t lmp_chan_alloc_recv_slot(struct lmp_chan *lc);
void lmp_channels_retry_send_disabled(dispatcher_handle_t handle);
void lmp_init(void);

/**
 * \brief Register an event handler to be notified when messages can be received
 *
 * In the future, call the closure on the given waitset when it is likely that
 * a message can be received on the channel. A channel may only be registered
 * with a single receive event handler on a single waitset at any one time.
 *
 * \param lc LMP channel
 * \param ws Waitset
 * \param closure Event handler
 */
static inline errval_t lmp_chan_register_recv(struct lmp_chan *lc,
                                              struct waitset *ws,
                                              struct event_closure closure)
{
    return lmp_endpoint_register(lc->endpoint, ws, closure);
}

/**
 * \brief Cancel an event registration made with lmp_chan_register_recv()
 *
 * \param lc LMP channel
 */
static inline errval_t lmp_chan_deregister_recv(struct lmp_chan *lc)
{
    return lmp_endpoint_deregister(lc->endpoint);
}

/**
 * \brief Migrate an event registration made with
 * lmp_chan_register_recv() to a new waitset
 *
 * \param lc LMP channel
 * \param ws New waitset
 */
static inline void lmp_chan_migrate_recv(struct lmp_chan *lc,
                                         struct waitset *ws)
{
    lmp_endpoint_migrate(lc->endpoint, ws);
}

/**
 * \brief Receive a message from an LMP channel, if possible
 *
 * Non-blocking. May fail if no message is available.
 *
 * \param lc  LMP channel
 * \param msg LMP message buffer, to be filled-in
 * \param cap If non-NULL, filled-in with location of received capability, if any
 */
static inline errval_t lmp_chan_recv(struct lmp_chan *lc,
                                     struct lmp_recv_msg *msg,
                                     struct capref *cap)
{
    assert(msg != NULL);
    assert(msg->buf.buflen == LMP_MSG_LENGTH);
    return lmp_endpoint_recv(lc->endpoint, &msg->buf, cap);
}

/**
 * \brief Set the receive capability slot for an LMP channel
 *
 * \param lc LMP channel
 * \param slot Receive slot
 */
static inline void lmp_chan_set_recv_slot(struct lmp_chan *lc,
                                              struct capref slot)
{
    lmp_endpoint_set_recv_slot(lc->endpoint, slot);
}

/**
 * \brief Is the given error value a transient LMP error
 *
 * Returns true iff the given error code indicates a transient
 * LMP send error condition that may succeed on a subsequent retry.
 */
static inline bool lmp_err_is_transient(errval_t err)
{
    enum err_code ec = err_no(err);
    return ec == SYS_ERR_LMP_BUF_OVERFLOW
           || ec == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_LOOKUP
           || ec == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID
           || ec == SYS_ERR_LMP_CAPTRANSFER_DST_SLOT_OCCUPIED
           || ec == SYS_ERR_LMP_TARGET_DISABLED;
}

/**
 * \brief Apply generic IDC control operation to LMP send flags
 */
static inline lmp_send_flags_t idc_control_to_lmp_flags(idc_control_t control,
                                                        lmp_send_flags_t flags)
{
    switch (control) {
    case IDC_CONTROL_SET_SYNC:
        return (lmp_send_flags_t) ((unsigned)flags | (unsigned)LMP_FLAG_SYNC);

    case IDC_CONTROL_CLEAR_SYNC:
        return (lmp_send_flags_t) ((unsigned)flags & ~(unsigned)LMP_FLAG_SYNC);

    default: // no-op for other control ops
        return flags;
    }
}

#include <barrelfish/lmp_chan_arch.h>

__END_DECLS

#endif // BARRELFISH_LMP_CHAN_H
