/**
 * \file
 * \brief Bidirectional LMP channel implementation
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/lmp_chan.h>
#include <barrelfish/dispatcher_arch.h>
#include <if/monitor_defs.h>
#include <barrelfish/caddr.h>
#include <barrelfish/idc_export.h>
#include <barrelfish/waitset_chan.h>
#include "waitset_chan_priv.h"

/**
 * \brief Initialise a new LMP channel
 *
 * \param lc  Storage for channel state
 */
void lmp_chan_init(struct lmp_chan *lc)
{
    assert(lc != NULL);
    lc->connstate = LMP_DISCONNECTED;
    waitset_chanstate_init(&lc->send_waitset, CHANTYPE_LMP_OUT);
    lc->endpoint = NULL;
#ifndef NDEBUG
    lc->prev = lc->next = NULL;
#endif
}

/**
 * \brief Copy cap to root CNode, enabling its use with LRPC
 *
 * \param src   Cap to move to root CNode
 * \param dest  Return pointer for the new slot
 *
 * The source capability is copied into the root CNode, deleted and freed
 */
// workaround inlining bug with gcc 4.4.1 shipped with ubuntu 9.10 and 4.4.3 in Debian
#if defined(__i386__) && defined(__GNUC__) \
    && __GNUC__ == 4 && __GNUC_MINOR__ == 4 && __GNUC_PATCHLEVEL__ <= 3
static __attribute__((noinline)) errval_t move_to_root(struct capref src,
                                                       struct capref *dest)
#else
static errval_t move_to_root(struct capref src, struct capref *dest)
#endif
{
    errval_t err;

    err = slot_alloc_root(dest);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    err = cap_copy(*dest, src);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_COPY);
    }

    err = cap_delete(src);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_WHILE_DELETING);
    }

    err = slot_free(src);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_WHILE_FREEING_SLOT);
    }

    return SYS_ERR_OK;
}

/// Handler for LMP bind reply messages from the Monitor
static void bind_lmp_reply_handler(struct monitor_binding *b,
                                   errval_t success, uintptr_t mon_id,
                                   uintptr_t conn_id,
                                   struct capref endpoint)
{
    struct lmp_chan *lc = (void *)conn_id;
    errval_t err;

    assert(lc->connstate == LMP_BIND_WAIT);

    if (err_is_ok(success)) { /* bind succeeded */
        lc->connstate = LMP_CONNECTED;

        /* Place the cap in the rootcn, to allow LRPC */
        err = move_to_root(endpoint, &lc->remote_cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error moving endpoint cap to root in LMP bind reply");
            // leave it where it is, and continue
            lc->remote_cap = endpoint;
        }
    }

    /* either way, tell the user what happened */
    assert(lc->bind_continuation.handler != NULL);
    lc->bind_continuation.handler(lc->bind_continuation.st, success, lc);
}

static void send_bind_cont(void *arg)
{
    struct lmp_chan *lc = arg;
    struct monitor_binding *b = lc->monitor_binding;
    errval_t err;

    /* Send bind request to the monitor */
    err = b->tx_vtbl.bind_lmp_client_request(b, NOP_CONT, lc->iref,
                                            (uintptr_t)lc, lc->buflen_words,
                                            lc->local_cap);
    if (err_is_ok(err)) { // request sent ok
        event_mutex_unlock(&b->mutex);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        // register to retry
        err = b->register_send(b, b->waitset, MKCONT(send_bind_cont,lc));
        assert(err_is_ok(err)); // we hold the monitor binding mutex
    } else { // permanent failure sending message
        event_mutex_unlock(&b->mutex);
        lc->bind_continuation.handler(lc->bind_continuation.st,
                                      err_push(err, LIB_ERR_BIND_LMP_REQ), NULL);
    }
}

/**
 * \brief Initialise a new LMP channel and initiate a binding
 *
 * \param lc  Storage for channel state
 * \param cont Continuation for bind completion/failure
 * \param qnode Storage for an event queue node (used for queuing bind request)
 * \param iref IREF to which to bind
 * \param buflen_words Size of incoming buffer, in number of words
 */
errval_t lmp_chan_bind(struct lmp_chan *lc, struct lmp_bind_continuation cont,
                       struct event_queue_node *qnode, iref_t iref,
                       size_t buflen_words)
{
    errval_t err;

    lmp_chan_init(lc);

    /* store bind arguments */
    lc->iref = iref;
    lc->buflen_words = buflen_words;
    lc->bind_continuation = cont;

    /* allocate a cap slot for the new endpoint cap */
    err = slot_alloc(&lc->local_cap);
    if (err_is_fail(err)) {
        waitset_chanstate_destroy(&lc->send_waitset);
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    /* allocate a local endpoint */
    err = lmp_endpoint_create_in_slot(buflen_words, lc->local_cap,
                                      &lc->endpoint);
    if (err_is_fail(err)) {
        slot_free(lc->local_cap);
        waitset_chanstate_destroy(&lc->send_waitset);
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    // wait for the ability to use the monitor binding
    lc->connstate = LMP_BIND_WAIT;
    struct monitor_binding *mb = lc->monitor_binding = get_monitor_binding();
    event_mutex_enqueue_lock(&mb->mutex, qnode,
                             MKCLOSURE(send_bind_cont, lc));

    return SYS_ERR_OK;
}

/// Destroy the local state associated with a given channel
void lmp_chan_destroy(struct lmp_chan *lc)
{
    lc->connstate = LMP_DISCONNECTED;
    cap_destroy(lc->local_cap);

    if (lc->endpoint != NULL) {
        lmp_endpoint_free(lc->endpoint);
    }

    // remove from send retry queue on dispatcher
    if (waitset_chan_is_registered(&lc->send_waitset)) {
        assert(lc->prev != NULL && lc->next != NULL);
        dispatcher_handle_t handle = disp_disable();
        struct dispatcher_generic *disp = get_dispatcher_generic(handle);
        if (lc->next == lc->prev) {
            assert_disabled(lc->next == lc);
            assert_disabled(disp->lmp_send_events_list == lc);
            disp->lmp_send_events_list = NULL;
        } else {
            lc->prev->next = lc->next;
            lc->next->prev = lc->prev;
        }
        disp_enable(handle);

#ifndef NDEBUG
        lc->next = lc->prev = NULL;
#endif
    }

    waitset_chanstate_destroy(&lc->send_waitset);
}

struct bind_lmp_reply_state {
    struct monitor_binding *b;
    struct lmp_chan *lc;
    struct monitor_bind_lmp_reply_monitor__args args;
    struct event_queue_node qnode;
};

static void send_bind_reply(void *arg)
{
    struct bind_lmp_reply_state *st = arg;
    struct monitor_binding *b = st->b;
    errval_t err;

    err = st->b->tx_vtbl.bind_lmp_reply_monitor(st->b, NOP_CONT, st->args.err,
                                                st->args.mon_id, st->args.conn_id, 
                                                st->args.ep);
    if (err_is_ok(err)) {
        event_mutex_unlock(&b->mutex);
        free(st);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = st->b->register_send(st->b, st->b->waitset,
                                   MKCONT(send_bind_reply,st));
        assert(err_is_ok(err)); // shouldn't fail, as we have the mutex
    } else {
        event_mutex_unlock(&b->mutex);
        USER_PANIC_ERR(err, "failed sending back reply to LMP bind request;"
                       " request dropped!");
        if (st->lc != NULL) {
            lmp_chan_destroy(st->lc);
            // FIXME: how do we tell the binding about this!?
        }
        free(st);
    }
}

/// Handler for LMP bind request messages from the Monitor
static void bind_lmp_service_request_handler(struct monitor_binding *b,
                                             uintptr_t service_id,
                                             uintptr_t mon_id,
                                             size_t buflen_words,
                                             struct capref endpoint)
{
    struct idc_export *e = (void *)service_id;
    struct lmp_chan *lc = NULL;
    errval_t err;

    // call the binding's connect handler
    if (e->lmp_connect_callback != NULL) {
        err = e->lmp_connect_callback(e->connect_cb_st, buflen_words, endpoint, &lc);
    } else {
        err = LIB_ERR_NO_LMP_BIND_HANDLER;
    }

    if (err_is_ok(err)) {
        assert(lc != NULL);
    }

    // wait for the ability to use the monitor binding
    struct bind_lmp_reply_state *st = malloc(sizeof(struct bind_lmp_reply_state));
    assert(st != NULL);

    st->b = b;
    st->lc = lc;
    st->args.err = err;
    st->args.mon_id = mon_id;
    if (err_is_ok(err)) {
        st->args.conn_id = (uintptr_t)lc;
        st->args.ep = lc->local_cap;
    } else {
        st->args.conn_id = 0;
        st->args.ep = NULL_CAP;
    }

    event_mutex_enqueue_lock(&b->mutex, &st->qnode,
                             MKCLOSURE(send_bind_reply, st));
}

/**
 * \brief Initialise a new LMP channel to accept an incoming binding request
 *
 * \param lc  Storage for channel state
 * \param buflen_words Size of incoming buffer, in words
 * \param endpoint Capability to remote LMP endpoint
 */
errval_t lmp_chan_accept(struct lmp_chan *lc,
                         size_t buflen_words, struct capref endpoint)
{
    errval_t err;

    lmp_chan_init(lc);
    lc->remote_cap = endpoint;

    /* allocate a cap slot for the new endpoint cap */
    err = slot_alloc(&lc->local_cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    /* allocate a local endpoint */
    err = lmp_endpoint_create_in_slot(buflen_words, lc->local_cap,
                                      &lc->endpoint);
    if (err_is_fail(err)) {
        slot_free(lc->local_cap);
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    /* mark connected */
    lc->connstate = LMP_CONNECTED;
    return SYS_ERR_OK;
}

/**
 * \brief Register an event handler to be notified when messages can be sent
 *
 * In the future, call the closure on the given waitset when it is likely that
 * a message can be sent on the channel. A channel may only be registered
 * with a single send event handler on a single waitset at any one time.
 *
 * \param lc LMP channel
 * \param ws Waitset
 * \param closure Event handler
 */
errval_t lmp_chan_register_send(struct lmp_chan *lc, struct waitset *ws,
                                 struct event_closure closure)
{
    assert(lc != NULL);
    assert(ws != NULL);

    errval_t err = waitset_chan_register(ws, &lc->send_waitset, closure);
    if (err_is_fail(err)) {
        return err;
    }

    // enqueue in list of channels with a registered event to retry sending
    assert(lc->next == NULL && lc->prev == NULL);
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);
    if (dp->lmp_send_events_list == NULL) {
        dp->lmp_send_events_list = lc;
        lc->next = lc->prev = lc;
    } else {
        lc->prev = dp->lmp_send_events_list->prev;
        lc->next = dp->lmp_send_events_list;
        lc->prev->next = lc;
        lc->next->prev = lc;
    }
    disp_enable(handle);

    return err;
}

/**
 * \brief Cancel an event registration made with lmp_chan_register_send()
 *
 * \param lc LMP channel
 */
errval_t lmp_chan_deregister_send(struct lmp_chan *lc)
{
    assert(lc != NULL);
    errval_t err = waitset_chan_deregister(&lc->send_waitset);
    if (err_is_fail(err)) {
        return err;
    }

    // dequeue from list of channels with send events
    assert(lc->next != NULL && lc->prev != NULL);
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);
    if (lc->next == lc->prev) {
        assert_disabled(dp->lmp_send_events_list == lc);
        dp->lmp_send_events_list = NULL;
    } else {
        lc->prev->next = lc->next;
        lc->next->prev = lc->prev;
        if (dp->lmp_send_events_list == lc) {
            dp->lmp_send_events_list = lc->next;
        }
    }
#ifndef NDEBUG
    lc->prev = lc->next = NULL;
#endif

    disp_enable(handle);
    return err;
}

/**
 * \brief Migrate an event registration to a new waitset.
 *
 * \param lc LMP channel
 * \param ws New waitset to migrate to
 */
void lmp_chan_migrate_send(struct lmp_chan *lc, struct waitset *ws)
{
    assert(lc != NULL);
    waitset_chan_migrate(&lc->send_waitset, ws);
}

/**
 * \brief Allocate a new receive capability slot for an LMP channel
 *
 * This utility function allocates a new receive slot (using #slot_alloc)
 * and sets it on the channel (using #lmp_chan_set_recv_slot).
 *
 * \param lc LMP channel
 */
errval_t lmp_chan_alloc_recv_slot(struct lmp_chan *lc)
{
    struct capref slot;

    errval_t err = slot_alloc(&slot);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    lmp_chan_set_recv_slot(lc, slot);
    return SYS_ERR_OK;
}

/**
 * \brief Trigger send events for all LMP channels that are registered
 *
 * We don't have a good way to determine when we are likely to be able
 * to send on an LMP channel, so this function just trigger all such
 * pending events every time the dispatcher is rescheduled.
 *
 * Must be called while disabled and from dispatcher logic.
 */
void lmp_channels_retry_send_disabled(dispatcher_handle_t handle)
{
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);
    struct lmp_chan *lc, *first = dp->lmp_send_events_list, *next;
    errval_t err;

    for (lc = first; lc != NULL; lc = next) {
        next = lc->next;
        assert(next != NULL);
        err = waitset_chan_trigger_disabled(&lc->send_waitset, handle);
        assert_disabled(err_is_ok(err)); // shouldn't fail
#ifndef NDEBUG
        lc->next = lc->prev = NULL;
#endif
        if (next == first) {
            break; // wrapped
        }
    }

    dp->lmp_send_events_list = NULL;
}

/// Initialise the LMP channel driver
void lmp_init(void)
{
    struct monitor_binding *mcb = get_monitor_binding();
    mcb->rx_vtbl.bind_lmp_reply_client = bind_lmp_reply_handler;
    mcb->rx_vtbl.bind_lmp_service_request = bind_lmp_service_request_handler;
}
