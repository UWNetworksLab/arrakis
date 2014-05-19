/**
 * \file
 * \brief Arch-specific inter-monitor communication
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include "monitor.h"
#include <trace/trace.h>
#include <barrelfish_kpi/shared_mem_arch.h>
#include <notify_ipi.h>

/******* stack-ripped bind_monitor_request_scc *******/

static void bind_monitor_reply_scc_handler(struct intermon_binding *b,
                                           struct intermon_msg_queue_elem *e);

struct bind_monitor_reply_scc_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_bind_monitor_reply_scc__args args;
};

static void bind_monitor_reply_scc_cont(struct intermon_binding *b,
                                        errval_t err, chanid_t chanid)
{
    errval_t err2;

    err2 = b->tx_vtbl.bind_monitor_reply_scc(b, NOP_CONT, err,
                                             chanid, my_core_id);
    if (err_is_fail(err2)) {
        if(err_no(err2) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_monitor_reply_scc_state *me =
                malloc(sizeof(struct bind_monitor_reply_scc_state));
            assert(me != NULL);
            struct intermon_state *ist = b->st;
            assert(ist != NULL);
            me->args.err = err;
            me->args.chan_id = chanid;
            me->elem.cont = bind_monitor_reply_scc_handler;

            err = intermon_enqueue_send(b, &ist->queue,
                                        get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        DEBUG_ERR(err2, "reply failed");
    }
}

static void bind_monitor_reply_scc_handler(struct intermon_binding *b,
                                           struct intermon_msg_queue_elem *e)
{
    struct bind_monitor_reply_scc_state *st = (struct bind_monitor_reply_scc_state *)e;
    bind_monitor_reply_scc_cont(b, st->args.err, st->args.chan_id);
    free(e);
}

/**
 * \brief A monitor receives request to setup a connection
 * with another newly booted monitor from a third monitor
 */
static void bind_monitor_request_scc(struct intermon_binding *b,
                                     coreid_t core_id, 
                                     intermon_caprep_t caprep,
                                     chanid_t chan_id, 
                                     coreid_t from_core_id)
{
    struct intermon_ump_ipi_binding *umpb = NULL;
    errval_t err;

    /* Create the cap */
    struct capability cap_raw;
    caprep_to_capability(&caprep, &cap_raw);
    if (cap_raw.type != ObjType_Frame) {
        err = MON_ERR_WRONG_CAP_TYPE;
        goto error;
    }

    struct capref frame;
    err = slot_alloc(&frame);
    if (err_is_fail(err)) {
        goto error;
    }

    ram_set_affinity(cap_raw.u.frame.base, cap_raw.u.frame.base + ((genpaddr_t)1 << cap_raw.u.frame.bits));
    err = frame_alloc(&frame, ((genpaddr_t)1 << cap_raw.u.frame.bits), NULL);
    ram_set_affinity(0,0);
    
    /* err = monitor_cap_create(frame, &cap_raw, core_id); */
    if (err_is_fail(err)) {
        goto error;
    }

    struct frame_identity frameid = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(frame, &frameid);
    assert(err == SYS_ERR_OK);

    printf("bind_monitor_request: URPC physical frame at 0x%llx\n", frameid.base);

    /* Setup the connection */
    void *buf;
    err = vspace_map_one_frame_attr(&buf, MON_URPC_SIZE, frame,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL,
                                    NULL);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_VSPACE_MAP);
        goto error;
    }

    // Create remote notify cap
    struct capref notify_cap;
    err = notification_create_cap(chan_id, core_id, &notify_cap);
    assert(err == SYS_ERR_OK);

    // Allocate my own notification caps
    struct capref ep, my_notify_cap;
    struct lmp_endpoint *iep;
    int chanid;
    err = endpoint_create(LMP_RECV_LENGTH, &ep, &iep);
    assert(err_is_ok(err));
    err = notification_allocate(ep, &chanid);
    assert(err == SYS_ERR_OK);
    err = notification_create_cap(chanid, my_core_id, &my_notify_cap);
    assert(err == SYS_ERR_OK);

    // setup our side of the binding
    umpb = malloc(sizeof(struct intermon_ump_ipi_binding));
    assert(umpb != NULL);

    err = intermon_ump_ipi_init(umpb, get_default_waitset(),
                                buf + MON_URPC_CHANNEL_LEN,
                                MON_URPC_CHANNEL_LEN,
                                buf, MON_URPC_CHANNEL_LEN, notify_cap,
                                my_notify_cap, ep, iep);
    assert(err_is_ok(err));

    // Identify UMP frame for tracing
    struct frame_identity umpid;
    err = invoke_frame_identify(frame, &umpid);
    assert(err_is_ok(err));
    umpb->ump_state.chan.recvid = (uintptr_t)umpid.base;
    umpb->ump_state.chan.sendid =
        (uintptr_t)(umpid.base + MON_URPC_CHANNEL_LEN);

    // connect it to our request handlers
    err = intermon_init(&umpb->b, core_id);
    assert(err_is_ok(err));

    /* Send reply */
reply:
    assert(umpb != NULL);
    bind_monitor_reply_scc_cont(&umpb->b, err, chanid);
    return;

error:
    assert(!"Argh");
    // FIXME: cleanup!
    goto reply;
}

/**
 * \brief The monitor that proxied the request for one monitor to
 * setup a connection with another monitor gets the reply
 */
static void bind_monitor_reply_scc(struct intermon_binding *binding,
                                   errval_t err, chanid_t chan_id,
                                   coreid_t core_id)
{
    struct intermon_ump_ipi_binding *b = (struct intermon_ump_ipi_binding *)binding;

    // Create notify cap to that core
    struct capref notify_cap;
    err = notification_create_cap(chan_id, core_id, &notify_cap);
    assert(err == SYS_ERR_OK);

    // And assign it to the binding
    err = ipi_notify_set(&b->ipi_notify, notify_cap);
    assert(err_is_ok(err));

    if (err_is_fail(err)) { // XXX
        DEBUG_ERR(err, "Got error in bind monitor reply");
    }
}

/******* stack-ripped bind_monitor_proxy_scc *******/

static void bind_monitor_request_scc_handler(struct intermon_binding *b,
                                           struct intermon_msg_queue_elem *e);

struct bind_monitor_request_scc_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_bind_monitor_request_scc__args args;
};

static void bind_monitor_request_scc_cont(struct intermon_binding *dst_binding,
                                          coreid_t src_core_id, 
                                          intermon_caprep_t caprep,
                                          chanid_t chan_id,
                                          coreid_t core_id)
{
    errval_t err;

    err = dst_binding->tx_vtbl.
        bind_monitor_request_scc(dst_binding, NOP_CONT, src_core_id, 
                                 caprep, chan_id, core_id);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_monitor_request_scc_state *me =
                malloc(sizeof(struct bind_monitor_request_scc_state));
            assert(me != NULL);
            struct intermon_state *ist = dst_binding->st;
            assert(ist != NULL);
            me->args.core_id = src_core_id;
            me->args.cap = caprep;
            me->args.chan_id = chan_id;
            me->args.from_core_id = core_id;
            me->elem.cont = bind_monitor_request_scc_handler;

            err = intermon_enqueue_send(dst_binding, &ist->queue,
                                        get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        DEBUG_ERR(err, "forwarding bind request failed");
    }
}

static void bind_monitor_request_scc_handler(struct intermon_binding *b,
                                           struct intermon_msg_queue_elem *e)
{
    struct bind_monitor_request_scc_state *st = (struct bind_monitor_request_scc_state *)e;
    bind_monitor_request_scc_cont(b, st->args.core_id, st->args.cap,
                                  st->args.chan_id, st->args.from_core_id);
    free(e);
}

/**
 * \brief A monitor asks this monitor to proxy
 * its request to bind to another monitor
 */
static void bind_monitor_proxy_scc(struct intermon_binding *b,
                                   coreid_t dst_core_id,
                                   intermon_caprep_t caprep,
                                   chanid_t chan_id,
                                   coreid_t core_id)
{
    seen_connections++;
    errval_t err;

    /* Get source monitor's core id */
    coreid_t src_core_id = ((struct intermon_state *)b->st)->core_id;

    /* Get destination monitor */
    struct intermon_binding *dst_binding = NULL;
    err = intermon_binding_get(dst_core_id, &dst_binding);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "intermon_binding_get failed");
    }

    // Proxy the request
    bind_monitor_request_scc_cont(dst_binding, src_core_id, 
                                  caprep, chan_id, core_id);
}

/**
 * \brief Notification of a newly booted monitor.
 *  Setup our connection and request the sender to proxy
 *  the bind request to the monitor
 */
static void new_monitor_notify(struct intermon_binding *b,
                               coreid_t core_id)
{
    errval_t err;

    /* Setup the connection */
    ram_set_affinity(SHARED_MEM_MIN + (PERCORE_MEM_SIZE * my_core_id),
                     SHARED_MEM_MIN + (PERCORE_MEM_SIZE * (my_core_id + 1)));
    struct capref frame;
    err = frame_alloc(&frame, MON_URPC_SIZE, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame_alloc failed");
        return; // FIXME: cleanup
    }
    ram_set_affinity(0, 0);     // Reset affinity

    void *buf;
    err = vspace_map_one_frame_attr(&buf, MON_URPC_SIZE, frame, VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        assert(buf); // XXX
    }
    // XXX: Clear the frame (kernel can't do it for us)
    memset(buf, 0, MON_URPC_SIZE);

    // Allocate my own notification caps
    struct capref ep, my_notify_cap;
    struct lmp_endpoint *iep;
    int chanid;
    err = endpoint_create(LMP_RECV_LENGTH, &ep, &iep);
    assert(err_is_ok(err));
    err = notification_allocate(ep, &chanid);
    assert(err == SYS_ERR_OK);
    err = notification_create_cap(chanid, my_core_id, &my_notify_cap);
    assert(err == SYS_ERR_OK);

    // init our end of the binding and channel
    struct intermon_ump_ipi_binding *ump_binding =
        malloc(sizeof(struct intermon_ump_ipi_binding));
    assert(ump_binding != NULL);
    err = intermon_ump_ipi_init(ump_binding, get_default_waitset(),
                                buf, MON_URPC_CHANNEL_LEN,
                                buf + MON_URPC_CHANNEL_LEN,
                                MON_URPC_CHANNEL_LEN, NULL_CAP, my_notify_cap,
                                ep, iep);
    assert(err_is_ok(err));
    /* if (err_is_fail(err)) { */
    /*     cap_destroy(frame); */
    /*     return err_push(err, LIB_ERR_UMP_CHAN_BIND); */
    /* } */

    // Identify UMP frame for tracing
    struct frame_identity umpid = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(frame, &umpid);
    assert(err_is_ok(err));
    ump_binding->ump_state.chan.recvid = (uintptr_t)umpid.base;
    ump_binding->ump_state.chan.sendid =
        (uintptr_t)(umpid.base + MON_URPC_CHANNEL_LEN);

    err = intermon_init(&ump_binding->b, core_id);
    assert(err_is_ok(err));

    /* Identify the frame cap */
    struct capability frame_cap;
    err = monitor_cap_identify(frame, &frame_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "monitor_cap_identify failed");
        return; // FIXME: cleanup
    }

    intermon_caprep_t caprep;
    capability_to_caprep(&frame_cap, &caprep);

    /* reply to the sending monitor to proxy request */
    err = b->tx_vtbl.bind_monitor_proxy_scc(b, NOP_CONT, core_id,
                                            caprep, chanid, my_core_id);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind proxy request failed");
    }
}

errval_t arch_intermon_init(struct intermon_binding *b)
{
    b->rx_vtbl.bind_monitor_request_scc = bind_monitor_request_scc;
    b->rx_vtbl.bind_monitor_reply_scc = bind_monitor_reply_scc;
    b->rx_vtbl.bind_monitor_proxy_scc = bind_monitor_proxy_scc;
    b->rx_vtbl.new_monitor_notify = new_monitor_notify;

    return SYS_ERR_OK;
}
