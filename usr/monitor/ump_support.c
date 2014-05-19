/**
 * \file
 * \brief UMP channel support
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include "monitor.h"

/******* stack-ripped monitor_bind_ump_client_request *******/

static void monitor_bind_ump_client_request_error(struct monitor_binding *b,
                                                  struct capref frame,
                                                  uintptr_t conn_id,
                                                  uintptr_t domain_id,
                                                  errval_t err)
{
    errval_t err2;

    err2 = cap_destroy(frame);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    if (conn_id != 0) {
        err2 = remote_conn_free(conn_id);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err2, "remote_conn_free failed");
        }
    }

    err2 = b->tx_vtbl.bind_ump_reply_client(b, NOP_CONT, 0, domain_id, err,
                                            NULL_CAP);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "error reply failed");
    }
}

static void bind_ump_request_handler(struct intermon_binding *b,
                                     struct intermon_msg_queue_elem *e);

struct bind_ump_request_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_bind_ump_request__args args;
    struct frame_identity frameid;
    struct capability capability;
    struct monitor_binding *mb;
    struct capref frame;
    uintptr_t domain_id;
};

static void bind_ump_request_cont(struct intermon_binding *intermon_binding,
                                  iref_t iref, uintptr_t conn_id,
                                  uint32_t channel_length_in,
                                  uint32_t channel_length_out,
                                  struct frame_identity frameid,
                                  struct capability capability,
                                  struct monitor_binding *mb,
                                  struct capref frame,
                                  uintptr_t domain_id)
{
    errval_t err;

    intermon_caprep_t caprep;
    capability_to_caprep(&capability, &caprep);

    /* Send the request to the monitor on the server's core */
    err = intermon_binding->tx_vtbl.
        bind_ump_request(intermon_binding, NOP_CONT, iref, conn_id, channel_length_in,
                         channel_length_out, frameid.base, frameid.bits,
                         caprep);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_ump_request_state *me =
                malloc(sizeof(struct bind_ump_request_state));
            struct intermon_state *ist = intermon_binding->st;
            me->args.iref = iref;
            me->args.mon_id = conn_id;
            me->args.channel_length_in = channel_length_in;
            me->args.channel_length_out = channel_length_out;
            me->frameid = frameid;
            me->capability = capability;
            me->mb = mb;
            me->frame = frame;
            me->domain_id = domain_id;
            me->elem.cont = bind_ump_request_handler;

            err = intermon_enqueue_send(intermon_binding, &ist->queue,
                                        get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "failed forwarding UMP bind request");
        monitor_bind_ump_client_request_error(mb, frame, conn_id, domain_id, err);
    }
}

static void bind_ump_request_handler(struct intermon_binding *b,
                                     struct intermon_msg_queue_elem *e)
{
    struct bind_ump_request_state *st = (struct bind_ump_request_state *)e;
    bind_ump_request_cont(b, st->args.iref, st->args.mon_id,
                          st->args.channel_length_in,
                          st->args.channel_length_out, st->frameid,
                          st->capability, st->mb, st->frame, st->domain_id);
    free(e);
}

static void monitor_bind_ump_client_request(struct monitor_binding *mb,
                                            iref_t iref, uintptr_t domain_id,
                                            struct capref frame,
                                            size_t channel_length_in,
                                            size_t channel_length_out,
                                            struct capref notify)
{
    uint8_t core_id;
    uintptr_t conn_id = 0;
    errval_t err;
    struct remote_conn_state *conn = NULL;

    // Get the core id
    err = iref_get_core_id(iref, &core_id);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "iref_get_core_id failed");
        monitor_bind_ump_client_request_error(mb, frame, conn_id, domain_id, err);
        return;
    }

    if (core_id == my_core_id) {
        USER_PANIC("Same-core UMP binding NYI");
    }

    /* Identify frame */
    struct frame_identity frameid;
    err = invoke_frame_identify(frame, &frameid);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "frame_identify failed");
        monitor_bind_ump_client_request_error(mb, frame, conn_id, domain_id, err);
        return;
    }

    // Identify notify cap
    struct capability capability;
    err = monitor_cap_identify(notify, &capability);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_identify failed, ignored");
        return;
    }
    assert(capability.type == ObjType_Notify_RCK
           || capability.type == ObjType_Notify_IPI
           || capability.type == ObjType_Null);
    /* assert(capability.u.notify.coreid == my_core_id); */

    /* Forward request to the corresponding monitor */
    // Create local state
    err = remote_conn_alloc(&conn, &conn_id, REMOTE_CONN_UMP);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "remote_conn_alloc failed");
        monitor_bind_ump_client_request_error(mb, frame, conn_id, domain_id, err);
        return;
    }

    // Track data
    conn->domain_id = domain_id;
    conn->domain_binding = mb;
    conn->x.ump.frame = frame;
    conn->core_id = core_id;

    // Get connection to the monitor to forward request to
    struct intermon_binding *intermon_binding;
    err = intermon_binding_get(core_id, &intermon_binding);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "intermon_binding_get failed");
        monitor_bind_ump_client_request_error(mb, frame, conn_id, domain_id, err);
        return;
    }

    bind_ump_request_cont(intermon_binding, iref, conn_id, channel_length_in,
                          channel_length_out, frameid, capability, mb, frame,
                          domain_id);
}

/******* stack-ripped monitor_bind_ump_reply *******/

static void bind_ump_reply_handler(struct intermon_binding *b,
                                   struct intermon_msg_queue_elem *e);

struct bind_ump_reply_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_bind_ump_reply__args args;
    struct capability capability;
};

static void bind_ump_reply_cont(struct intermon_binding *mon_binding,
                                uintptr_t your_mon_id, uintptr_t my_mon_id,
                                uintptr_t msgerr, struct capability capability)
{
    errval_t err;

    intermon_caprep_t caprep;
    capability_to_caprep(&capability, &caprep);

    err = mon_binding->tx_vtbl.
        bind_ump_reply(mon_binding, NOP_CONT, your_mon_id, my_mon_id, msgerr,
                       caprep);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_ump_reply_state *me =
                malloc(sizeof(struct bind_ump_reply_state));
            struct intermon_state *ist = mon_binding->st;
            me->args.con_id = your_mon_id;
            me->args.mon_id = my_mon_id;
            me->args.err = msgerr;
            me->capability = capability;
            me->elem.cont = bind_ump_reply_handler;

            err = intermon_enqueue_send(mon_binding, &ist->queue,
                                        get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "failed forwarding UMP bind reply");
        // cleanup
        if (err_is_ok(msgerr)) {
            err = remote_conn_free(my_mon_id);
            assert(err_is_ok(err));
        }
    }
}

static void bind_ump_reply_handler(struct intermon_binding *b,
                                   struct intermon_msg_queue_elem *e)
{
    struct bind_ump_reply_state *st = (struct bind_ump_reply_state *)e;
    bind_ump_reply_cont(b, st->args.con_id, st->args.mon_id, st->args.err,
                        st->capability);
    free(e);
}

static void monitor_bind_ump_reply(struct monitor_binding *dom_binding,
                                   uintptr_t my_mon_id, uintptr_t domain_id,
                                   errval_t msgerr, struct capref notify)
{
    errval_t err;

    struct remote_conn_state *conn = remote_conn_lookup(my_mon_id);
    if (conn == NULL) {
        USER_PANIC("invalid mon_id in UMP bind reply");
        return;
    }

    uintptr_t your_mon_id = conn->mon_id;
    struct intermon_binding *mon_binding = conn->mon_binding;

    if (err_is_ok(msgerr)) {
        /* Connection accepted */
        conn->domain_id = domain_id;
        conn->domain_binding = dom_binding;
    } else {
//error:
        /* Free the cap */
        err = cap_destroy(conn->x.ump.frame);
        assert(err_is_ok(err));

        err = remote_conn_free(my_mon_id);
        assert(err_is_ok(err));
    }

    // Identify notify cap
    struct capability capability;
    err = monitor_cap_identify(notify, &capability);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_identify failed, ignored");
        return;
    }
    assert(capability.type == ObjType_Notify_RCK
           || capability.type == ObjType_Notify_IPI
           || capability.type == ObjType_Null);
    /* assert(capability.u.notify.coreid == my_core_id); */

    bind_ump_reply_cont(mon_binding, your_mon_id, my_mon_id, msgerr, capability);
}

/******* stack-ripped intermon_bind_ump_request *******/

static void bind_ump_service_request_handler(struct monitor_binding *b,
                                             struct monitor_msg_queue_elem *e);

struct bind_ump_service_request_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_bind_ump_service_request__args args;
    struct intermon_binding *binding;
    uintptr_t your_mon_id;
};

static void bind_ump_service_request_cont(struct monitor_binding *domain_binding,
                                          uintptr_t service_id,
                                          con_id_t my_mon_id,
                                          struct capref frame,
                                          uint32_t channel_length_in,
                                          uint32_t channel_length_out,
                                          struct capref notify_cap,
                                          struct intermon_binding *binding,
                                          con_id_t your_mon_id)
{
    errval_t err, err2;

    /* Proxy the request */
    err = domain_binding->tx_vtbl.
        bind_ump_service_request(domain_binding, NOP_CONT, service_id,
                                 my_mon_id, frame,
                                 channel_length_in, channel_length_out,
                                 notify_cap);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_ump_service_request_state *me =
                malloc(sizeof(struct bind_ump_service_request_state));
            struct monitor_state *ist = domain_binding->st;
            me->args.service_id = service_id;
            me->args.mon_id = my_mon_id;
            me->args.frame = frame;
            me->args.channel_length_in = channel_length_in;
            me->args.channel_length_out = channel_length_out;
            me->args.notify = notify_cap;
            me->binding = binding;
            me->your_mon_id = your_mon_id;
            me->elem.cont = bind_ump_service_request_handler;

            err = monitor_enqueue_send(domain_binding, &ist->queue,
                                       get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        err2 = cap_delete(frame);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err2, "Cap delete failed");
        }
        err2 = slot_free(frame);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err2, "Cap destroy default failed");
        }
        err2 = remote_conn_free(my_mon_id);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err2, "remote_conn_free failed");
        }
        intermon_caprep_t nullcap = {0,0,0,0};
        err2 = binding->tx_vtbl.bind_ump_reply(binding, NOP_CONT, your_mon_id, 0, err,
                                               nullcap);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err2, "Sending bind_ump_reply1 failed");
        }
    }
}

static void bind_ump_service_request_handler(struct monitor_binding *b,
                                             struct monitor_msg_queue_elem *e)
{
    struct bind_ump_service_request_state *st = (struct bind_ump_service_request_state *)e;
    bind_ump_service_request_cont(b, st->args.service_id, st->args.mon_id,
                                  st->args.frame, st->args.channel_length_in,
                                  st->args.channel_length_out, st->args.notify,
                                  st->binding, st->your_mon_id);
    free(e);
}

static void intermon_bind_ump_request(struct intermon_binding *ib,
                                      iref_t iref, con_id_t your_mon_id,
                                      uint32_t channel_length_in,
                                      uint32_t channel_length_out,
                                      genpaddr_t framebase, uint8_t framebits,
                                      intermon_caprep_t caprep)
{
    errval_t err;

    /* Get client's core_id */
    struct intermon_state *ist = ib->st;
    assert(ist != NULL);
    coreid_t core_id = ist->core_id;

    /* Construct the frame capability */
    struct capability frame_cap = {
        .type = ObjType_Frame,
        .rights = CAPRIGHTS_READ_WRITE, // XXX
        .u.frame = {
            .base = framebase,
            .bits = framebits
        }
    };

    // Construct the notify cap
    struct capref notify_cap = NULL_CAP;
    struct capability capability;
    caprep_to_capability(&caprep, &capability);
    if(capability.type != ObjType_Null) {
        err = slot_alloc(&notify_cap);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Failed to allocate slot from channel_alloc");
        }
        err = monitor_cap_create(notify_cap, &capability, core_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "monitor_cap_create failed");
        }
    }

    // XXX: Put frame cap on a separate allocator as it is not deleted anymore
    struct capref frame;
    err = slot_alloc(&frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to allocate slot from channel_alloc");
    }
    err = monitor_cap_create(frame, &frame_cap, core_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_create failed");
    }

    /* Get the server's connection */
    struct monitor_binding *domain_binding = NULL;
    err = iref_get_binding(iref, &domain_binding);
    assert(err_is_ok(err));

    /* Get the service id */
    uintptr_t service_id = 0;
    err = iref_get_service_id(iref, &service_id);
    assert(err_is_ok(err));

    /* Create a new connection state */
    uintptr_t my_mon_id;
    struct remote_conn_state *con;
    err = remote_conn_alloc(&con, &my_mon_id, REMOTE_CONN_UMP);
    assert(err_is_ok(err));

    // Set the monitor portion of it
    con->mon_id = your_mon_id;
    con->mon_binding = ib;
    con->x.ump.frame = frame;
    con->core_id = core_id;

    bind_ump_service_request_cont(domain_binding, service_id, my_mon_id,
                                  frame, channel_length_in, channel_length_out,
                                  notify_cap, ib, your_mon_id);
}

/******* stack-ripped intermon_bind_ump_reply *******/

static void bind_ump_reply_client_handler(struct monitor_binding *b,
                                          struct monitor_msg_queue_elem *e);

struct bind_ump_reply_client_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_bind_ump_reply_client__args args;
};

static void bind_ump_reply_client_cont(struct monitor_binding *domain_binding,
                                       uintptr_t my_mon_id,
                                       uintptr_t domain_id,
                                       errval_t msgerr,
                                       struct capref notify_cap)
{
    errval_t err;

    err = domain_binding->tx_vtbl.
        bind_ump_reply_client(domain_binding, NOP_CONT, my_mon_id, domain_id,
                              msgerr, notify_cap);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_ump_reply_client_state *me =
                malloc(sizeof(struct bind_ump_reply_client_state));
            struct monitor_state *ist = domain_binding->st;
            me->args.mon_id = my_mon_id;
            me->args.conn_id = domain_id;
            me->args.err = msgerr;
            me->args.notify = notify_cap;
            me->elem.cont = bind_ump_reply_client_handler;

            err = monitor_enqueue_send(domain_binding, &ist->queue,
                                       get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "UMP bind reply failed");
        // cleanup
        err = remote_conn_free(my_mon_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "remote_conn_free failed");
        }
    }
}

static void bind_ump_reply_client_handler(struct monitor_binding *b,
                                             struct monitor_msg_queue_elem *e)
{
    struct bind_ump_reply_client_state *st = (struct bind_ump_reply_client_state *)e;
    bind_ump_reply_client_cont(b, st->args.mon_id, st->args.conn_id,
                               st->args.err, st->args.notify);
    free(e);
}

static void intermon_bind_ump_reply(struct intermon_binding *ib, 
                                    uint64_t my_mon_id, uint64_t your_mon_id,
                                    errval_t msgerr, 
                                    intermon_caprep_t caprep)
{
    errval_t err;
    struct remote_conn_state *con = remote_conn_lookup(my_mon_id);
    if (con == NULL) {
        USER_PANIC_ERR(0, "unknown mon_id in UMP bind reply");
        return;
    }

    uintptr_t domain_id = con->domain_id;
    struct monitor_binding *domain_binding = con->domain_binding;
    struct capref notify_cap = NULL_CAP;

    if (err_is_ok(msgerr)) { /* bind succeeded */
        con->mon_id = your_mon_id;
        con->mon_binding = ib;

#if 0
        /* map in UMP channel state */
        void *buf;
        err = vspace_map_one_frame_attr(&buf,
              2 * (UMP_CHANNEL_SIZE + con->localchan.size * sizeof(uintptr_t)),
                                        con->frame, VREGION_FLAGS_READ,
                                        NULL, NULL);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "vspace_map_one_frame failed");
            // XXX: should not be an assert, but we don't have any way to do
            // connection teardown here!
            assert(buf != NULL);
        }
        con->sharedchan = buf;
        con->localchan.buf = buf + 2 * UMP_CHANNEL_SIZE;

        // XXX: Put frame cap on a separate allocator as it is not deleted anymore
        struct capref frame_copy;
        err = slot_alloc(&frame_copy);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Failed to allocator slot from channel_alloc");
        }
        err = cap_copy(frame_copy, con->frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Failed create copy of frame cap");
        }
        err = cap_destroy(con->frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_destroy_default failed");
        }
        con->frame = frame_copy;
#endif

        struct capability capability;
        caprep_to_capability(&caprep, &capability);

        if(capability.type != ObjType_Null) {
            // Get core id of sender
            coreid_t core_id = ((struct intermon_state *)ib->st)->core_id;

            // Construct the notify cap
            err = slot_alloc(&notify_cap);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "Failed to allocate slot from channel_alloc");
            }

            err = monitor_cap_create(notify_cap, &capability, core_id);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_cap_create failed");
            }
        }
    } else { /* bind refused */
        err = cap_destroy(con->x.ump.frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_destroy_default failed");
        }
        err = remote_conn_free(my_mon_id);
        assert(err_is_ok(err));
    }

    bind_ump_reply_client_cont(domain_binding, my_mon_id, domain_id, msgerr,
                               notify_cap);
}

errval_t ump_intermon_init(struct intermon_binding *ib)
{
    ib->rx_vtbl.bind_ump_request = intermon_bind_ump_request;
    ib->rx_vtbl.bind_ump_reply = intermon_bind_ump_reply;
    return SYS_ERR_OK;
}

errval_t ump_monitor_init(struct monitor_binding *mb)
{
    mb->rx_vtbl.bind_ump_client_request = monitor_bind_ump_client_request;
    mb->rx_vtbl.bind_ump_reply_monitor = monitor_bind_ump_reply;
    return SYS_ERR_OK;
}
