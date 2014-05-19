/**
 * \file
 * \brief Bidirectional UMP channel implementation
 */

/*
 * Copyright (c) 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/ump_chan.h>
#include <barrelfish/idc_export.h>
#include <if/monitor_defs.h>

/* UMP channels need to be mapped non-cacheable on SCC */
#ifdef __scc__
#  include <barrelfish_kpi/shared_mem_arch.h>
/* #  define UMP_MAP_ATTR VREGION_FLAGS_READ_WRITE_NOCACHE */
#  define UMP_MAP_ATTR VREGION_FLAGS_READ_WRITE_MPB
#else
#  define UMP_MAP_ATTR VREGION_FLAGS_READ_WRITE
#endif

#ifndef CONFIG_INTERCONNECT_DRIVER_UMP
#error "This file shouldn't be compiled without CONFIG_INTERCONNECT_DRIVER_UMP"
#endif

/**
 * \brief Initialise a new UMP channel
 *
 * Most code should be using one of ump_chan_bind() or ump_chan_accept().
 *
 * \param uc Storage for channel state
 * \param inbuf Pointer to incoming message buffer
 * \param inbufsize Size of inbuf in bytes (must be multiple of UMP message size)
 * \param outbuf Pointer to outgoing message buffer
 * \param outbufsize Size of outbuf in bytes (must be multiple of UMP message size)
 */
errval_t ump_chan_init(struct ump_chan *uc,
                       volatile void *inbuf, size_t inbufsize,
                       volatile void *outbuf, size_t outbufsize)
{
    assert(uc != NULL);
    errval_t err;

    err = ump_endpoint_init(&uc->endpoint, inbuf, inbufsize);
    if (err_is_fail(err)) {
        return err;
    }

    err = ump_chan_state_init(&uc->send_chan, outbuf, outbufsize, UMP_OUTGOING);
    if (err_is_fail(err)) {
        return err;
    }

    uc->max_send_msgs = outbufsize / UMP_MSG_BYTES;
    uc->max_recv_msgs = inbufsize / UMP_MSG_BYTES;

    memset(&uc->cap_handlers, 0, sizeof(uc->cap_handlers));
    uc->iref = 0;
    uc->monitor_binding = get_monitor_binding(); // TODO: expose non-default to caller

    return SYS_ERR_OK;
}

/// Destroy the local state associated with a given channel
void ump_chan_destroy(struct ump_chan *uc)
{
    ump_endpoint_destroy(&uc->endpoint);
}

/// Handler for UMP bind reply messages from the Monitor
static void bind_ump_reply_handler(struct monitor_binding *b, uintptr_t mon_id,
                                   uintptr_t conn_id, errval_t success,
                                   struct capref notify)
{
    struct ump_chan *uc = (void *)conn_id;

    assert(uc->connstate == UMP_BIND_WAIT);

    if (err_is_ok(success)) { /* bind succeeded */
        uc->connstate = UMP_CONNECTED;
        uc->monitor_id = mon_id;
    } else { /* bind failed */
        uc->connstate = UMP_DISCONNECTED;
        /* TODO: delete endpoint, destroy local_cap */
    }

    /* either way, tell the user what happened */
    assert(uc->bind_continuation.handler != NULL);
    uc->bind_continuation.handler(uc->bind_continuation.st, success, uc, notify);
}

struct bind_ump_reply_state {
    struct monitor_binding *b;
    struct ump_chan *uc;
    struct monitor_bind_ump_reply_monitor__args args;
    struct event_queue_node qnode;
};

static void send_bind_reply(void *arg)
{
    struct bind_ump_reply_state *st = arg;
    struct monitor_binding *b = st->b;
    errval_t err;

    // send back a bind success/failure message to the monitor
    err =
        st->b->tx_vtbl.bind_ump_reply_monitor(st->b, NOP_CONT, st->args.mon_id,
                                              st->args.conn_id, st->args.err,
                                              st->args.notify);
    if (err_is_ok(err)) {
        event_mutex_unlock(&b->mutex);
        free(st);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = st->b->register_send(st->b, st->b->waitset,
                                   MKCONT(send_bind_reply, st));
        assert(err_is_ok(err)); // shouldn't fail, as we have the mutex
    } else {
        event_mutex_unlock(&b->mutex);
        USER_PANIC_ERR(err, "failed sending back reply to UMP bind request;"
                       " request dropped!");
        if (st->uc != NULL) {
            ump_chan_destroy(st->uc);
            // FIXME: how do we tell the binding about this!?
        }
        free(st);
    }
}

/// Handler for UMP bind request messages from the Monitor
static void bind_ump_service_request_handler(struct monitor_binding *b,
                                             uintptr_t service_id,
                                             uintptr_t mon_id,
                                             struct capref frame,
                                             size_t channel_length_in,
                                             size_t channel_length_out,
                                             struct capref notify_cap)
{
    struct idc_export *e = (void *)service_id;
    errval_t err;

    // call the binding's connect handler
    if (e->ump_connect_callback != NULL) {
        err = e->ump_connect_callback(e->connect_cb_st, b, mon_id, frame,
                                      channel_length_in, channel_length_out,
                                      notify_cap);
    } else {
        err = LIB_ERR_NO_UMP_BIND_HANDLER;
    }

    if (err_is_fail(err)) {
        ump_chan_send_bind_reply(b, NULL, err, mon_id, NULL_CAP);
    } else {
        // binding is responsible for sending reply
    }
}

void ump_chan_send_bind_reply(struct monitor_binding *mb,
                              struct ump_chan *uc, errval_t err,
                              uintptr_t monitor_id, struct capref notify_cap)
{
    struct bind_ump_reply_state *st = malloc(sizeof(struct bind_ump_reply_state));
    assert(st != NULL);

    if (err_is_ok(err)) {
        assert(uc != NULL);
    } else {
        assert(uc == NULL);
    }

    st->b = mb;
    st->uc = uc;
    st->args.err = err;
    st->args.mon_id = monitor_id;
    st->args.conn_id = err_is_ok(err) ? (uintptr_t)uc : 0;
    st->args.notify = notify_cap;

    // wait for the ability to use the monitor binding
    event_mutex_enqueue_lock(&mb->mutex, &st->qnode,
                             MKCLOSURE(send_bind_reply, st));
}

static void send_bind_cont(void *arg)
{
    struct ump_chan *uc = arg;
    struct monitor_binding *b = uc->monitor_binding;
    errval_t err;

    /* Send bind request to the monitor */
    assert(uc->monitor_binding == b);
    assert(b->tx_vtbl.bind_ump_client_request);
    err = b->tx_vtbl.bind_ump_client_request(b, NOP_CONT, uc->iref,
                                             (uintptr_t)uc, uc->frame,
                                             uc->inchanlen, uc->outchanlen,
                                             uc->notify_cap);
    if (err_is_ok(err)) { // request sent ok
        event_mutex_unlock(&b->mutex);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        // register to retry
        err = b->register_send(b, b->waitset, MKCONT(send_bind_cont,uc));
        assert(err_is_ok(err)); // we hold the monitor binding mutex
    } else { // permanent failure sending message
        event_mutex_unlock(&b->mutex);
        uc->bind_continuation.handler(uc->bind_continuation.st,
                                      err_push(err, LIB_ERR_BIND_UMP_REQ),
                                      NULL, NULL_CAP);
    }
}

/**
 * \brief Initialise a new UMP channel and initiate a binding
 *
 * \param uc  Storage for channel state
 * \param cont Continuation for bind completion/failure
 * \param qnode Storage for an event queue node (used for queuing bind request)
 * \param iref IREF to which to bind
 * \param monitor_binding Monitor binding to use
 * \param inchanlen Size of incoming channel, in bytes (rounded to #UMP_MSG_BYTES)
 * \param outchanlen Size of outgoing channel, in bytes (rounded to #UMP_MSG_BYTES)
 * \param notify_cap Capability to use for notifications, or #NULL_CAP
 */
errval_t ump_chan_bind(struct ump_chan *uc, struct ump_bind_continuation cont,
                       struct event_queue_node *qnode,  iref_t iref,
                       struct monitor_binding *monitor_binding,
                       size_t inchanlen, size_t outchanlen,
                       struct capref notify_cap)
{
    errval_t err;

    // round up channel sizes to message size
    inchanlen = ROUND_UP(inchanlen, UMP_MSG_BYTES);
    outchanlen = ROUND_UP(outchanlen, UMP_MSG_BYTES);

    // compute size of frame needed and allocate it
    size_t framesize = inchanlen + outchanlen;
#ifdef __scc__
    ram_set_affinity(SHARED_MEM_MIN + (PERCORE_MEM_SIZE * disp_get_core_id()),
                     SHARED_MEM_MIN + (PERCORE_MEM_SIZE * (disp_get_core_id() + 1)));
#endif
    err = frame_alloc(&uc->frame, framesize, &framesize);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }
#ifdef __scc__
    ram_set_affinity(0, 0);
#endif

    // map it in
    void *buf;
    err = vspace_map_one_frame_attr(&buf, framesize, uc->frame, UMP_MAP_ATTR,
                                    NULL, &uc->vregion);
    if (err_is_fail(err)) { 
        cap_destroy(uc->frame);
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    // initialise channel state
    err = ump_chan_init(uc, buf, inchanlen, (char *)buf + inchanlen, outchanlen);
    if (err_is_fail(err)) {
        vregion_destroy(uc->vregion);
        cap_destroy(uc->frame);
        return err;
    }

    // Ids for tracing
    struct frame_identity id;
    err = invoke_frame_identify(uc->frame, &id);
    if (err_is_fail(err)) {
        vregion_destroy(uc->vregion);
        cap_destroy(uc->frame);
        return err_push(err, LIB_ERR_FRAME_IDENTIFY);
    }
    uc->recvid = (uintptr_t)id.base;
    uc->sendid = (uintptr_t)(id.base + inchanlen);

    // store bind args
    uc->bind_continuation = cont;
    uc->monitor_binding = monitor_binding;
    uc->iref = iref;
    uc->inchanlen = inchanlen;
    uc->outchanlen = outchanlen;
    uc->notify_cap = notify_cap;

    // wait for the ability to use the monitor binding
    uc->connstate = UMP_BIND_WAIT;
    event_mutex_enqueue_lock(&monitor_binding->mutex, qnode,
                             MKCLOSURE(send_bind_cont, uc));

    return SYS_ERR_OK;
}

/**
 * \brief Initialise a new UMP channel to accept an incoming binding request
 *
 * \param uc  Storage for channel state
 * \param mon_id Monitor's connection ID for this channel
 * \param frame Frame capability containing channel
 * \param inchanlen Size of incoming channel, in bytes (multiple of #UMP_MSG_BYTES)
 * \param outchanlen Size of outgoing channel, in bytes (multiple of #UMP_MSG_BYTES)
 */
errval_t ump_chan_accept(struct ump_chan *uc, uintptr_t mon_id,
                         struct capref frame, size_t inchanlen,
                         size_t outchanlen)
{
    errval_t err;

    uc->monitor_id = mon_id;
    uc->frame = frame;

    // check that the frame is big enough
    struct frame_identity frameid;
    err = invoke_frame_identify(frame, &frameid);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_IDENTIFY);
    }

    // Ids for tracing
    uc->recvid = (uintptr_t)(frameid.base + outchanlen);
    uc->sendid = (uintptr_t)frameid.base;

    size_t framesize = ((uintptr_t)1) << frameid.bits;
    if (framesize < inchanlen + outchanlen) {
        return LIB_ERR_UMP_FRAME_OVERFLOW;
    }

    // map it in
    void *buf;
    err = vspace_map_one_frame_attr(&buf, framesize, frame, UMP_MAP_ATTR,
                                    NULL, &uc->vregion);
    if (err_is_fail(err)) {
        cap_destroy(uc->frame);
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    // initialise channel state
    err = ump_chan_init(uc, (char *)buf + outchanlen, inchanlen, buf, outchanlen);
    if (err_is_fail(err)) {
        vregion_destroy(uc->vregion);
        cap_destroy(uc->frame);
        return err;
    }

    /* mark connected */
    uc->connstate = UMP_CONNECTED;
    return SYS_ERR_OK;
}

/// Initialise the UMP channel driver
void ump_init(void)
{
    struct monitor_binding *mcb = get_monitor_binding();
    mcb->rx_vtbl.bind_ump_reply_client = bind_ump_reply_handler;
    mcb->rx_vtbl.bind_ump_service_request = bind_ump_service_request_handler;
}
