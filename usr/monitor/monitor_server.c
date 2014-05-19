/** \file
 * \brief Monitor's connection with the dispatchers on the same core
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <barrelfish/debug.h> // XXX: To set the cap_identify_reply handler
#include <barrelfish/sys_debug.h> // XXX: for sys_debug_send_ipi
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <if/mem_defs.h>
#include <barrelfish/monitor_client.h>
#include <if/monitor_loopback_defs.h>

// the monitor's loopback binding to itself
static struct monitor_binding monitor_self_binding;

/* ---------------------- MULTIBOOT REQUEST CODE START ---------------------- */

struct multiboot_cap_state {
    struct monitor_msg_queue_elem elem;
    cslot_t slot;
};

static void ms_multiboot_cap_request(struct monitor_binding *b, cslot_t slot);

static void ms_multiboot_cap_request_handler(struct monitor_binding *b,
                                             struct monitor_msg_queue_elem *e)
{
    struct multiboot_cap_state *ms = (struct multiboot_cap_state*)e;
    ms_multiboot_cap_request(b, ms->slot);
    free(ms);
}

static void ms_multiboot_cap_request(struct monitor_binding *b, cslot_t slot)
{
    errval_t err1, err2;

    struct capref cap = {
        .cnode = cnode_module,
        .slot  = slot,
    };

    // Call frame_identify to check if cap exists
    struct frame_identity id;
    err1 = invoke_frame_identify(cap, &id);
    if (err_is_fail(err1)) {
        err2 = b->tx_vtbl.multiboot_cap_reply(b, NOP_CONT, NULL_CAP, err1);
    } else {
        err2 = b->tx_vtbl.multiboot_cap_reply(b, NOP_CONT, cap, err1);
    }
    if (err_is_fail(err2)) {
        if (err_no(err2) == FLOUNDER_ERR_TX_BUSY) {
            struct monitor_state *mon_state = b->st;
            struct multiboot_cap_state *ms =
                malloc(sizeof(struct multiboot_cap_state));
            assert(ms);
            ms->slot = slot;
            ms->elem.cont = ms_multiboot_cap_request_handler;
            err1 = monitor_enqueue_send(b, &mon_state->queue,
                                       get_default_waitset(), &ms->elem.queue);
            if (err_is_fail(err1)) {
                USER_PANIC_ERR(err1, "monitor_enqueue_send failed");
            }
        } else {
            USER_PANIC_ERR(err2, "sending multiboot_cap_reply failed");
        }
    }
}

/* ----------------------- MULTIBOOT REQUEST CODE END ----------------------- */

static void alloc_iref_reply_handler(struct monitor_binding *b,
                                       struct monitor_msg_queue_elem *e);

struct alloc_iref_reply_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_alloc_iref_reply__args args;
    struct monitor_binding *b;
};

static void alloc_iref_reply_cont(struct monitor_binding *b,
                                    uintptr_t service_id,
                                    iref_t iref, errval_t reterr)
{
    errval_t err;

    err = b->tx_vtbl.alloc_iref_reply(b, NOP_CONT, service_id, iref, reterr);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct alloc_iref_reply_state *me =
                malloc(sizeof(struct alloc_iref_reply_state));
            assert(me != NULL);
            struct monitor_state *ist = b->st;
            assert(ist != NULL);
            me->args.service_id = service_id;
            me->args.iref = iref;
            me->args.err = reterr;
            me->b = b;
            me->elem.cont = alloc_iref_reply_handler;

            err = monitor_enqueue_send(b, &ist->queue,
                                       get_default_waitset(), &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_enqueue_send failed");
            }
            return;
        }

        USER_PANIC_ERR(err, "reply failed");
    }
}

static void alloc_iref_reply_handler(struct monitor_binding *b,
                                       struct monitor_msg_queue_elem *e)
{
    struct alloc_iref_reply_state *st = (struct alloc_iref_reply_state *)e;
    alloc_iref_reply_cont(b, st->args.service_id, st->args.iref,
                          st->args.err);
    free(e);
}

static void alloc_iref_request(struct monitor_binding *b,
                               uintptr_t service_id)
{
    errval_t reterr;

    iref_t iref = 0;
    reterr = iref_alloc(b, service_id, &iref);
    alloc_iref_reply_cont(b, service_id, iref, reterr);
}

/******* stack-ripped bind_lmp_service_request *******/

static void bind_lmp_client_request_error_handler(struct monitor_binding *b,
                                                  struct monitor_msg_queue_elem *e);

struct bind_lmp_client_request_error_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_bind_lmp_reply_client__args args;
    struct monitor_binding *serv_binding;
    struct capref ep;
};

static void bind_lmp_client_request_error(struct monitor_binding *b,
                                          errval_t err, uintptr_t domain_id,
                                          struct monitor_binding *serv_binding,
                                          struct capref ep)
{
    errval_t err2;

    err2 = b->tx_vtbl.bind_lmp_reply_client(b, NOP_CONT, err, 0, domain_id,
                                            NULL_CAP);
    if (err_is_fail(err2)) {
        if(err_no(err2) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_lmp_client_request_error_state *me =
                malloc(sizeof(struct bind_lmp_client_request_error_state));
            assert(me != NULL);
            struct monitor_state *ist = b->st;
            assert(ist != NULL);
            me->args.err = err;
            me->args.conn_id = domain_id;
            me->serv_binding = serv_binding;
            me->ep = ep;
            me->elem.cont = bind_lmp_client_request_error_handler;

            err = monitor_enqueue_send(b, &ist->queue,
                                       get_default_waitset(), &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_enqueue_send failed");
            }
            return;
        }

        USER_PANIC_ERR(err2, "error reply failed");
        USER_PANIC_ERR(err, "The reason for lmp failure");
    }

    /* Delete the EP cap */
    // Do not delete the cap if client or service is monitor itself
    if (b != &monitor_self_binding && serv_binding != &monitor_self_binding) {
        err = cap_destroy(ep);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_destroy failed");
        }
    }
}

static void bind_lmp_client_request_error_handler(struct monitor_binding *b,
                                                  struct monitor_msg_queue_elem *e)
{
    struct bind_lmp_client_request_error_state *st = (struct bind_lmp_client_request_error_state *)e;
    bind_lmp_client_request_error(b, st->args.err, st->args.conn_id,
                                  st->serv_binding, st->ep);
    free(e);
}

static void bind_lmp_service_request_handler(struct monitor_binding *b,
                                             struct monitor_msg_queue_elem *e);

struct bind_lmp_service_request_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_bind_lmp_service_request__args args;
    struct monitor_binding *b;
    uintptr_t domain_id;
};

static void bind_lmp_service_request_cont(struct monitor_binding *serv_binding,
                                          uintptr_t service_id, uintptr_t con_id,
                                          size_t buflen, struct capref ep,
                                          struct monitor_binding *b,
                                          uintptr_t domain_id)
{
    errval_t err, err2;

    struct monitor_state *ist = serv_binding->st;
    struct event_closure send_cont = NOP_CONT;
    struct capref *capp = NULL;

    if (serv_binding != &monitor_self_binding && b != &monitor_self_binding) {
        // save EP cap to be destroyed after the send is done
        capp = caprefdup(ep);
        send_cont = MKCONT(destroy_outgoing_cap, capp);
    }

    err = serv_binding->tx_vtbl.
        bind_lmp_service_request(serv_binding, send_cont, service_id,
                                 con_id, buflen, ep);
    if (err_is_fail(err)) {
        free(capp);

        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_lmp_service_request_state *me =
                malloc(sizeof(struct bind_lmp_service_request_state));
            assert(me != NULL);
            me->args.service_id = service_id;
            me->args.mon_id = con_id;
            me->args.buflen = buflen;
            me->args.ep = ep;
            me->b = b;
            me->domain_id = domain_id;
            me->elem.cont = bind_lmp_service_request_handler;

            err = monitor_enqueue_send(serv_binding, &ist->queue,
                                       get_default_waitset(), &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_enqueue_send failed");
            }
            return;
        }

        err2 = lmp_conn_free(con_id);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err2, "lmp_conn_free failed");
        }
        bind_lmp_client_request_error(b, err, domain_id, serv_binding, ep);
        return;
    }
}

static void bind_lmp_service_request_handler(struct monitor_binding *b,
                                             struct monitor_msg_queue_elem *e)
{
    struct bind_lmp_service_request_state *st = (struct bind_lmp_service_request_state *)e;
    bind_lmp_service_request_cont(b, st->args.service_id, st->args.mon_id,
                                  st->args.buflen, st->args.ep, st->b,
                                  st->domain_id);
    free(e);
}

static void bind_lmp_client_request(struct monitor_binding *b,
                                    iref_t iref, uintptr_t domain_id,
                                    size_t buflen, struct capref ep)
{
    errval_t err;
    struct monitor_binding *serv_binding = NULL;

    /* Look up core_id from the iref */
    uint8_t core_id;
    err = iref_get_core_id(iref, &core_id);
    if (err_is_fail(err)) {
        bind_lmp_client_request_error(b, err, domain_id, serv_binding, ep);
        return;
    }

    // Return error if service on different core
    if (core_id != my_core_id) {
        err = MON_ERR_IDC_BIND_NOT_SAME_CORE;
        bind_lmp_client_request_error(b, err, domain_id, serv_binding, ep);
        return;
    }

    /* Lookup the server's connection to monitor */
    err = iref_get_binding(iref, &serv_binding);
    if (err_is_fail(err)) {
        bind_lmp_client_request_error(b, err, domain_id, serv_binding, ep);
        return;
    }

    /* Lookup the server's service_id */
    uintptr_t service_id;
    err = iref_get_service_id(iref, &service_id);
    if (err_is_fail(err)) {
        bind_lmp_client_request_error(b, err, domain_id, serv_binding, ep);
        return;
    }

    /* Allocate a new monitor connection */
    uintptr_t con_id;
    struct lmp_conn_state *conn;
    err = lmp_conn_alloc(&conn, &con_id);
    if (err_is_fail(err)) {
        bind_lmp_client_request_error(b, err, domain_id, serv_binding, ep);
        return;
    }

    conn->domain_id = domain_id;
    conn->domain_binding = b;

    /* Send request to the server */
    bind_lmp_service_request_cont(serv_binding, service_id, con_id, buflen, ep,
                                  b, domain_id);
}

/******* stack-ripped bind_lmp_reply *******/

static void bind_lmp_reply_client_handler(struct monitor_binding *b,
                                          struct monitor_msg_queue_elem *e);

struct bind_lmp_reply_client_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_bind_lmp_reply_client__args args;
    struct monitor_binding *b;
};

static void bind_lmp_reply_client_cont(struct monitor_binding *client_binding,
                                       errval_t msgerr, uintptr_t mon_conn_id,
                                       uintptr_t client_conn_id,
                                       struct capref ep,
                                       struct monitor_binding *b)
{
    errval_t err;

    struct monitor_state *ist = client_binding->st;
    struct event_closure send_cont = NOP_CONT;
    struct capref *capp = NULL;

    if (client_binding != &monitor_self_binding && b != &monitor_self_binding) {
        // save EP cap to be destroyed after the send is done
        capp = caprefdup(ep);
        send_cont = MKCONT(destroy_outgoing_cap, capp);
    }

    err = client_binding->tx_vtbl.
        bind_lmp_reply_client(client_binding, send_cont,
                              SYS_ERR_OK, mon_conn_id, client_conn_id, ep);
    if (err_is_fail(err)) {
        free(capp);

        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_lmp_reply_client_state *me =
                malloc(sizeof(struct bind_lmp_reply_client_state));
            assert(me != NULL);
            me->args.err = msgerr;
            me->args.mon_id = mon_conn_id;
            me->args.conn_id = client_conn_id;
            me->args.ep = ep;
            me->b = b;
            me->elem.cont = bind_lmp_reply_client_handler;

            err = monitor_enqueue_send(client_binding, &ist->queue,
                                       get_default_waitset(), &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_enqueue_send failed");
            }
            return;
        }

        USER_PANIC_ERR(err, "failed sending IDC bind reply");
    }

    if(err_is_fail(msgerr)) {
        return;
    }
}

static void bind_lmp_reply_client_handler(struct monitor_binding *b,
                                          struct monitor_msg_queue_elem *e)
{
    struct bind_lmp_reply_client_state *st = (struct bind_lmp_reply_client_state *)e;
    bind_lmp_reply_client_cont(b, st->args.err, st->args.mon_id, st->args.conn_id,
                               st->args.ep, st->b);
    free(e);
}

static void bind_lmp_reply(struct monitor_binding *b,
                           errval_t msgerr, uintptr_t mon_conn_id,
                           uintptr_t user_conn_id, struct capref ep)
{
    errval_t err;
    struct monitor_binding *client_binding = NULL;

    struct lmp_conn_state *conn = lmp_conn_lookup(mon_conn_id);
    if (conn == NULL) {
        DEBUG_ERR(0, "invalid connection ID");
        goto cleanup;
    }

    client_binding = conn->domain_binding;
    uintptr_t client_conn_id = conn->domain_id;

    err = lmp_conn_free(mon_conn_id);
    assert(err_is_ok(err));

    if (err_is_fail(msgerr)) {
        bind_lmp_reply_client_cont(client_binding, msgerr, 0, client_conn_id,
                                   ep, b);
    } else {
        bind_lmp_reply_client_cont(client_binding, SYS_ERR_OK, mon_conn_id,
                                   client_conn_id, ep, b);
    }
    return;

cleanup:
    /* Delete the ep cap */
    // XXX: Do not delete the cap if client or service is monitor
    if (client_binding != &monitor_self_binding && b != &monitor_self_binding) {
        err = cap_destroy(ep);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_destroy failed");
        }
    }
}

/* ---------------------- NEW MONITOR BINDING CODE START -------------------- */

struct new_monitor_binding_reply_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_new_monitor_binding_reply__args args;
};

static void
new_monitor_binding_reply_cont(struct monitor_binding *b,
                               errval_t reterr, struct capref retcap,
                               uintptr_t st);

static void new_monitor_binding_reply_handler(struct monitor_binding *b,
                                              struct monitor_msg_queue_elem *e)
{
    struct new_monitor_binding_reply_state *st =
        (struct new_monitor_binding_reply_state *)e;
    new_monitor_binding_reply_cont(b, st->args.err, st->args.ep, st->args.st);
    free(st);
}

static void
new_monitor_binding_reply_cont(struct monitor_binding *b,
                               errval_t reterr, struct capref retcap,
                               uintptr_t st)
{
    errval_t err =
        b->tx_vtbl.new_monitor_binding_reply(b, NOP_CONT, reterr, retcap, st);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct monitor_state *ms = b->st;
            struct new_monitor_binding_reply_state *me =
                malloc(sizeof(struct new_monitor_binding_reply_state));
            assert(me != NULL);
            me->args.err = reterr;
            me->args.ep = retcap;
            me->args.st = st;
            me->elem.cont = new_monitor_binding_reply_handler;
            err = monitor_enqueue_send(b, &ms->queue,
                                       get_default_waitset(), &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_enqueue_send failed");
            }
            return;
        }

        USER_PANIC_ERR(err, "failed to send new_monitor_binding_reply");
    }
}

/**
 * \brief Setup a new idc channel between monitor and domain
 *
 * \bug on error send message back to domain
 */
static void new_monitor_binding_request(struct monitor_binding *b, uintptr_t st)
{
    struct capref retcap = NULL_CAP;
    errval_t err, reterr = SYS_ERR_OK;

    struct monitor_lmp_binding *lmpb =
        malloc(sizeof(struct monitor_lmp_binding));
    assert(lmpb != NULL);

    // setup our end of the binding
    err = monitor_client_lmp_accept(lmpb, get_default_waitset(),
                                    DEFAULT_LMP_BUF_WORDS);
    if (err_is_fail(err)) {
        free(lmpb);
        reterr = err_push(err, LIB_ERR_MONITOR_CLIENT_ACCEPT);
        goto out;
    }

    retcap = lmpb->chan.local_cap;
    monitor_server_init(&lmpb->b);

out:
    new_monitor_binding_reply_cont(b, reterr, retcap, st);
}

/* ---------------------- NEW MONITOR BINDING CODE END ---------------------- */

static void get_mem_iref_request(struct monitor_binding *b)
{
    errval_t err;

    // Mem serv not registered yet
    assert(mem_serv_iref != 0);

    err = b->tx_vtbl.get_mem_iref_reply(b, NOP_CONT, mem_serv_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "reply failed");
    }
}

static void get_name_iref_request(struct monitor_binding *b, uintptr_t st)
{
    errval_t err;
    err = b->tx_vtbl.get_name_iref_reply(b, NOP_CONT, name_serv_iref, st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "reply failed");
    }
}

static void get_ramfs_iref_request(struct monitor_binding *b, uintptr_t st)
{
    errval_t err;
    err = b->tx_vtbl.get_ramfs_iref_reply(b, NOP_CONT, ramfs_serv_iref, st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "reply failed");
    }
}

static void set_mem_iref_request(struct monitor_binding *b, 
                                 iref_t iref)
{
    mem_serv_iref = iref;
    update_ram_alloc_binding = true;
}

static void get_monitor_rpc_iref_request(struct monitor_binding *b, 
                                         uintptr_t st_arg)
{
    errval_t err;

    // monitor rpc not registered yet
    assert(monitor_rpc_iref != 0);

    err = b->tx_vtbl.get_monitor_rpc_iref_reply(b, NOP_CONT,
                                                monitor_rpc_iref, st_arg);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "reply failed");
    }
}


void set_monitor_rpc_iref(iref_t iref)
{
    if (monitor_rpc_iref != 0) {
        // Called multiple times, return error
        DEBUG_ERR(0, "Attempt to reset monitor rpc IREF ignored");
        return;
    }

    monitor_rpc_iref = iref;
}


static void set_name_iref_request(struct monitor_binding *b, 
                                  iref_t iref)
{
    if (name_serv_iref != 0) {
        // Called multiple times, return error
        DEBUG_ERR(0, "Attempt to reset name serv IREF ignored");
        return;
    }

    name_serv_iref = iref;
}

static void set_ramfs_iref_request(struct monitor_binding *b,
                                  iref_t iref)
{
    if (ramfs_serv_iref != 0) {
        // Called multiple times, return error
        DEBUG_ERR(0, "Attempt to reset name serv IREF ignored");
        return;
    }

    ramfs_serv_iref = iref;
}

struct send_cap_st {
    struct rcap_st rcap_st;        // This must be first
    uintptr_t my_mon_id;
    struct capref cap;
    uint32_t capid;
    uint8_t give_away;
    struct capability capability;
    errval_t msgerr;
    bool has_descendants;
    coremask_t on_cores;
};

static void cap_send_request_2(uintptr_t my_mon_id, struct capref cap,
                               uint32_t capid, struct capability capability,
                               errval_t msgerr,
                               uint8_t give_away, bool has_descendants,
                               coremask_t on_cores);

static void cap_send_request_cb(void * st_arg) {
    errval_t err;
    struct send_cap_st * st = (struct send_cap_st *) st_arg;
    if (err_is_fail(st->rcap_st.err)) {
        // lock failed, unlock any cores we locked
        err = rcap_db_release_lock(&(st->capability), st->rcap_st.cores_locked);
        assert (err_is_ok(err));

        // try again - TODO, introduce some backoff here
        err = rcap_db_acquire_lock(&(st->capability), (struct rcap_st *)st);
        assert (err_is_ok(err));
    } else {
        cap_send_request_2(st->my_mon_id, st->cap, st->capid, st->capability,
                           st->msgerr, st->give_away, st->has_descendants,
                           st->on_cores);
    }
}

/// FIXME: If on the same core, fail. (Why? -AB)
/// XXX: size of capability is arch specific
static void cap_send_request(struct monitor_binding *b,
                             uintptr_t my_mon_id, struct capref cap,
                             uint32_t capid, uint8_t give_away)
{
    errval_t err, msgerr = SYS_ERR_OK;
    struct capability capability;
    bool has_descendants;
    coremask_t on_cores;

    if (!capref_is_null(cap)) {
        err = monitor_cap_identify(cap, &capability);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "monitor_cap_identify failed, ignored");
            return;
        }

        // if we can't transfer the cap, it is delivered as NULL
        if (!monitor_can_send_cap(&capability)) {
            cap = NULL_CAP;
            msgerr = MON_ERR_CAP_SEND;
        }
    }

    if (capref_is_null(cap)) {
        // we don't care about capabilities, has_descendants, or on_cores here,
        // make the compiler happy though
        static struct capability null_capability;
        static coremask_t null_mask;
        cap_send_request_2(my_mon_id, cap, capid, null_capability,
                           msgerr, give_away, false, null_mask);
    } else if (!give_away) {
        if (!rcap_db_exists(&capability)) {
            err = monitor_cap_remote(cap, true, &has_descendants);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_cap_remote failed");
                return;
            }
            err = rcap_db_add(&capability, has_descendants);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "rcap_db_add failed");
                return;
            }
        }

        err = rcap_db_get_info(&capability, &has_descendants, &on_cores);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "rcap_db_get_info failed");
            return;
        }

        // allocate state for callback
        struct send_cap_st * send_cap_st = malloc (sizeof(struct send_cap_st));
        send_cap_st->rcap_st.free_at_ccast = false;
        send_cap_st->rcap_st.cb            = cap_send_request_cb;
        send_cap_st->my_mon_id  = my_mon_id;
        send_cap_st->cap        = cap;
        send_cap_st->capability = capability;
        send_cap_st->capid      = capid;
        send_cap_st->msgerr     = msgerr;
        send_cap_st->give_away  = give_away;
        send_cap_st->has_descendants = has_descendants;
        send_cap_st->on_cores   = on_cores;

        err = rcap_db_acquire_lock(&capability, (struct rcap_st *)send_cap_st);
        assert (err_is_ok(err));
        // continues in cap_send_request_2 (after cap_send_request_cb)

    } else { // give_away cap
        err = monitor_cap_remote(cap, true, &has_descendants);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "monitor_cap_remote failed");
            return;
        }

        // TODO ensure that no more copies of this cap are on this core
        static coremask_t null_mask;
        // call continuation directly
        cap_send_request_2(my_mon_id, cap, capid, capability, msgerr, give_away,
                           has_descendants, null_mask);
    }
}

struct cap_send_request_state {
    struct intermon_msg_queue_elem elem;
    uintptr_t your_mon_id;
    uint32_t capid;
    errval_t msgerr;
    intermon_caprep_t caprep;
    uint8_t give_away;
    bool has_descendants;
    coremask_t on_cores;
    bool null_cap; 
};

static void cap_send_request_2_handler(struct intermon_binding *b,
                                       struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct cap_send_request_state *st = (struct cap_send_request_state*)e;

    err = b->tx_vtbl.cap_send_request(b, NOP_CONT, st->your_mon_id, st->capid,
                                      st->caprep, st->msgerr, st->give_away,
                                      st->has_descendants, st->on_cores.bits,
                                      st->null_cap); 
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct intermon_state *intermon_state = b->st;
            struct cap_send_request_state *ms =
                malloc(sizeof(struct cap_send_request_state));
            assert(ms);

            ms->your_mon_id = st->your_mon_id;
            ms->capid = st->capid;
            ms->caprep = st->caprep;
            ms->msgerr = st->msgerr;
            ms->give_away = st->give_away;
            ms->has_descendants = st->has_descendants;
            ms->on_cores = st->on_cores;
            ms->null_cap = st->null_cap; 
            ms->elem.cont = cap_send_request_2_handler;

            errval_t err1 = intermon_enqueue_send(b, &intermon_state->queue,
                                                  get_default_waitset(),
                                                  &ms->elem.queue);
            if (err_is_fail(err1)) {
                USER_PANIC_ERR(err1, "monitor_enqueue_send failed");
            }

        } else {
            USER_PANIC_ERR(err, "forwarding cap failed");
        }
    }

}

static void cap_send_request_2(uintptr_t my_mon_id, struct capref cap,
                               uint32_t capid, struct capability capability,
                               errval_t msgerr,
                               uint8_t give_away, bool has_descendants,
                               coremask_t on_cores)
{
    errval_t err;
    struct remote_conn_state *conn = remote_conn_lookup(my_mon_id);
    if (conn == NULL) {
        USER_PANIC_ERR(0, "invalid mon_id, ignored");
        return;
    }

    struct intermon_binding *binding = conn->mon_binding;
    uintptr_t your_mon_id = conn->mon_id;


    // XXX: This is a typedef of struct that flounder is generating.
    // Flounder should not be generating this and we shouldn't be using it.
    intermon_caprep_t caprep;
    capability_to_caprep(&capability, &caprep);


    bool null_cap = capref_is_null(cap);
    if (!null_cap) {
        err = cap_destroy(cap);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_destroy failed");
        }
    }

    err = binding->tx_vtbl.
        cap_send_request(binding, NOP_CONT, your_mon_id, capid,
                         caprep, msgerr, give_away, has_descendants, on_cores.bits,
                         null_cap);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct intermon_state *intermon_state = binding->st;
            struct cap_send_request_state *ms =
                malloc(sizeof(struct cap_send_request_state));
            assert(ms);

            ms->your_mon_id = your_mon_id;
            ms->capid = capid;
            ms->caprep = caprep;
            ms->msgerr = msgerr;
            ms->give_away = give_away;
            ms->has_descendants = has_descendants;
            ms->on_cores = on_cores;
            ms->null_cap = null_cap;
            ms->elem.cont = cap_send_request_2_handler;

            errval_t err1 = intermon_enqueue_send(binding, &intermon_state->queue,
                                                  get_default_waitset(),
                                                  &ms->elem.queue);
            if (err_is_fail(err1)) {
                USER_PANIC_ERR(err1, "monitor_enqueue_send failed");
            }

        } else {
            USER_PANIC_ERR(err, "forwarding cap failed");
        }
    }
}

#if 0
struct capref domains[MAX_DOMAINS];

static void assign_domain_id_request(struct monitor_binding *b, uintptr_t ust,
                                     struct capref disp, struct capref ep)
{
    for(domainid_t id = 1; id < MAX_DOMAINS; id++) {
        if(domains[id].cnode.address_bits == 0) {
            domains[id] = ep;
            errval_t err = invoke_domain_id(disp, id);
            assert(err_is_ok(err));

            err = b->tx_vtbl.assign_domain_id_reply(b, NOP_CONT, ust, id);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "assign domain ID failed\n");
            }
            return;
        }
    }

    // Return error
    errval_t err = b->tx_vtbl.assign_domain_id_reply(b, NOP_CONT, ust, 0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "assign domain ID failed\n");
    }
}
#endif

static void span_domain_request(struct monitor_binding *mb,
                                uintptr_t domain_id, uint8_t core_id,
                                struct capref vroot, struct capref disp)
{
    errval_t err, err2;

    trace_event(TRACE_SUBSYS_MONITOR, TRACE_EVENT_MONITOR_SPAN0, core_id);
    
    struct span_state *state;
    uintptr_t state_id;

    err = span_state_alloc(&state, &state_id);
    if (err_is_fail(err)) {
        err_push(err, MON_ERR_SPAN_STATE_ALLOC);
        goto reply;
    }

    state->core_id   = core_id;
    state->vroot     = vroot;
    state->mb        = mb;
    state->domain_id = domain_id;

    trace_event(TRACE_SUBSYS_MONITOR, TRACE_EVENT_MONITOR_SPAN1, core_id);

    /* Look up the destination monitor */
    struct intermon_binding *ib;
    err = intermon_binding_get(core_id, &ib);
    if (err_is_fail(err)) {
        goto reply;
    }

    /* Idenfity vroot */
    struct capability vroot_cap;
    err = monitor_cap_identify(vroot, &vroot_cap);
    if (err_is_fail(err)) {
        err_push(err, MON_ERR_CAP_IDENTIFY);
        goto reply;
    }
    if (vroot_cap.type != ObjType_VNode_x86_64_pml4) { /* Check type */
        err = MON_ERR_WRONG_CAP_TYPE;
        goto reply;
    }

    /* Identify the dispatcher frame */
    struct frame_identity frameid;
    err = invoke_frame_identify(disp, &frameid);
    if (err_is_fail(err)) {
        err_push(err, LIB_ERR_FRAME_IDENTIFY);
        goto reply;
    }

    bool has_descendants;
    err = monitor_cap_remote(disp, true, &has_descendants);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_remote failed");
        return;
    }
    err = monitor_cap_remote(vroot, true, &has_descendants);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_remote failed");
        return;
    }

    /* Send msg to destination monitor */
    err = ib->tx_vtbl.span_domain_request(ib, NOP_CONT, state_id,
                                          vroot_cap.u.vnode_x86_64_pml4.base,
                                          frameid.base, frameid.bits);

    if (err_is_fail(err)) {
        err_push(err, MON_ERR_SEND_REMOTE_MSG);
        goto reply;
    }
    goto cleanup;

 reply:
    err2 = mb->tx_vtbl.span_domain_reply(mb, NOP_CONT, err, domain_id);
    if (err_is_fail(err2)) {
        // XXX: Cleanup?
        USER_PANIC_ERR(err2, "Failed to reply to the user domain");
    }
    if(state_id != 0) {
        err2 = span_state_free(state_id);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err2, "Failed to free span state");
        }
    }

 cleanup:
    err2 = cap_destroy(vroot);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "Failed to destroy span_vroot cap");
    }
    err2 = cap_destroy(disp);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "Failed to destroy disp cap");
    }
}

static void num_cores_request(struct monitor_binding *b)
{
    /* XXX: This is deprecated and shouldn't be used: there's nothing useful you
     * can do with the result, unless you assume that core IDs are contiguous
     * and start from zero, which is a false assumption! Go ask the SKB...
     */

    debug_printf("Application invoked deprecated num_cores_request() API."
                 " Please fix it!\n");

    /* Send reply */
    errval_t err = b->tx_vtbl.num_cores_reply(b, NOP_CONT, num_monitors);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending num_cores_reply failed");
    }
}

struct monitor_rx_vtbl the_table = {
    .alloc_iref_request = alloc_iref_request,

    .bind_lmp_client_request= bind_lmp_client_request,
    .bind_lmp_reply_monitor = bind_lmp_reply,

    .boot_core_request = boot_core_request,
    .boot_initialize_request = boot_initialize_request,
    .multiboot_cap_request = ms_multiboot_cap_request,

    .new_monitor_binding_request = new_monitor_binding_request,

    .get_mem_iref_request  = get_mem_iref_request,
    .get_name_iref_request = get_name_iref_request,
    .get_ramfs_iref_request = get_ramfs_iref_request,
    .set_mem_iref_request  = set_mem_iref_request,
    .set_name_iref_request = set_name_iref_request,
    .set_ramfs_iref_request = set_ramfs_iref_request,
    .get_monitor_rpc_iref_request  = get_monitor_rpc_iref_request,

    .cap_send_request = cap_send_request,

    .span_domain_request    = span_domain_request,

    .num_cores_request  = num_cores_request,

    //.assign_domain_id_request = assign_domain_id_request,
};

errval_t monitor_client_setup(struct spawninfo *si)
{
    errval_t err;

    struct monitor_lmp_binding *b =
        malloc(sizeof(struct monitor_lmp_binding));
    assert(b != NULL);

    // setup our end of the binding
    err = monitor_client_lmp_accept(b, get_default_waitset(),
                                    DEFAULT_LMP_BUF_WORDS);
    if (err_is_fail(err)) {
        free(b);
        return err_push(err, LIB_ERR_MONITOR_CLIENT_ACCEPT);
    }

    // copy the endpoint cap to the recipient
    struct capref dest = {
        .cnode = si->rootcn,
        .slot  = ROOTCN_SLOT_MONITOREP,
    };

    err = cap_copy(dest, b->chan.local_cap);
    if (err_is_fail(err)) {
        // TODO: destroy binding
        return err_push(err, LIB_ERR_CAP_COPY);
    }

    // Copy the performance monitoring cap to all spawned processes.
    struct capref src;
    dest.cnode = si->taskcn;
    dest.slot = TASKCN_SLOT_PERF_MON;
    src.cnode = cnode_task;
    src.slot = TASKCN_SLOT_PERF_MON;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_PERF_MON);
    }    

    // copy our receive vtable to the binding
    monitor_server_init(&b->b);

    return SYS_ERR_OK;
}

errval_t monitor_client_setup_mem_serv(void)
{
    /* construct special-case LMP connection to mem_serv */
    static struct monitor_lmp_binding mcb;
    struct waitset *ws = get_default_waitset();
    errval_t err;

    err = monitor_client_lmp_accept(&mcb, ws, DEFAULT_LMP_BUF_WORDS);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_client_setup_mem_serv");
    }
    assert(err_is_ok(err));

    /* Send the cap for this endpoint to init, who will pass it to the monitor */
    err = lmp_ep_send0(cap_initep, 0, mcb.chan.local_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "lmp_ep_send0 failed");
    }

    // copy our receive vtable to the binding
    monitor_server_init(&mcb.b);

    // XXX: handle messages (ie. block) until the monitor binding is ready
    while (capref_is_null(mcb.chan.remote_cap)) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch waiting for mem_serv binding");
            return err_push(err, LIB_ERR_EVENT_DISPATCH);
        }
    }

    return SYS_ERR_OK;
}

/// Setup a dummy monitor binding that "sends" all requests to the local handlers
errval_t monitor_client_setup_monitor(void)
{
    monitor_loopback_init(&monitor_self_binding);
    monitor_server_init(&monitor_self_binding);
    set_monitor_binding(&monitor_self_binding);
    idc_init();
    // XXX: Need a waitset here or loopback won't work as expected
    // when binding to the ram_alloc service
    monitor_self_binding.mutex.equeue.waitset = get_default_waitset();

    return SYS_ERR_OK;
}

errval_t monitor_server_init(struct monitor_binding *b)
{
    struct monitor_state *lst = malloc(sizeof(struct monitor_state));
    assert(lst != NULL);
    lst->queue.head = lst->queue.tail = NULL;

    // copy our receive vtable to the new binding
    b->rx_vtbl = the_table;
    b->st = lst;
    // TODO: set error_handler

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
    errval_t err;
    err = ump_monitor_init(b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ump_monitor_init failed");
    }
#endif

#ifdef CONFIG_INTERCONNECT_DRIVER_MULTIHOP
    errval_t err2;
    err2 = multihop_monitor_init(b);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "multihop_monitor_init failed");
    }
#endif // CONFIG_INTERCONNECT_DRIVER_MULTIHOP

#ifdef CONFIG_TRACE
    errval_t err3;
    err3 = bfscope_monitor_init(b);
    if (err_is_fail(err3)) {
        USER_PANIC_ERR(err3, "bfscope_monitor_init failed");
    }

    err3 = trace_monitor_init(b);
    if (err_is_fail(err3)) {
        USER_PANIC_ERR(err3, "trace_monitor_init failed");
    }
#endif // CONFIG_TRACE

    return monitor_server_arch_init(b);
}
