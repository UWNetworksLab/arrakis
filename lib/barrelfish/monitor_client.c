/**
 * \file
 * \brief Client for interacting with the monitor
 */

/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatcher_arch.h>
#include <if/monitor_defs.h>
#include <barrelfish/monitor_client.h>
#include <if/monitor_blocking_rpcclient_defs.h>
#include <string.h>
#include <inttypes.h>

static void error_handler(struct monitor_binding *b, errval_t err)
{
    debug_err(__FILE__, __func__, __LINE__, err,
              "asynchronous error in monitor binding");
    abort();
}

/// Handler for incoming LMP messages from the monitor binding with an
/// indirected cap for a UMP/BMP/... channel
static void cap_receive_request_handler(struct monitor_binding *b,
                                        uintptr_t conn_id, errval_t success,
                                        struct capref cap, uint32_t capid)
{
    /* XXX: this relies on the monitor_cap_handlers table being the first thing
     * in every channel state struct */
    struct monitor_cap_handlers *h = (void *)conn_id;
    assert(h->cap_receive_handler != NULL);
    h->cap_receive_handler(h->st, success, cap, capid);
}

/// vtable for handlers declared in this file
// other handlers are set directly on the binding by various init()
// functions after we bind to the monitor
static struct monitor_rx_vtbl monitor_rx_vtbl = {
    .cap_receive_request = cap_receive_request_handler,
};

static void monitor_accept_recv_handler(void *arg)
{
    struct monitor_lmp_binding *b = arg;
    struct lmp_recv_msg msg = LMP_RECV_MSG_INIT;
    struct capref cap;
    errval_t err;

    // try to retrieve a message from the channel
    err = lmp_chan_recv(&b->chan, &msg, &cap);
    if (err_is_fail(err)) {
        if (err_no(err) == LIB_ERR_NO_LMP_MSG) {
            // nothing there, re-register
            struct event_closure recv_handler = {
                .handler = monitor_accept_recv_handler,
                .arg = b,
            };
            err = lmp_chan_register_recv(&b->chan, b->b.waitset, recv_handler);
            b->b.error_handler(&b->b, err_push(err, LIB_ERR_CHAN_REGISTER_RECV));
        } else {
            // real error, report to user
            b->b.error_handler(&b->b, err_push(err, LIB_ERR_LMP_CHAN_RECV));
        }
        return;
    }

    // if we're the monitor, we might be waiting for the other side's cap
    assert(b->chan.connstate == LMP_MONITOR_ACCEPT);
    assert(!capref_is_null(cap));
    b->chan.remote_cap = cap;
    b->chan.connstate = LMP_CONNECTED;

    /* allocate a new receive slot */
    err = lmp_chan_alloc_recv_slot(&b->chan);
    if (err_is_fail(err)) {
        // XXX: report the error, but continue
        b->b.error_handler(&b->b, err_push(err, LIB_ERR_LMP_ALLOC_RECV_SLOT));
    }

    /* Run the RX handler; has a side-effect of registering for receive events */
    monitor_lmp_rx_handler(b);
}

static errval_t init_lmp_binding(struct monitor_lmp_binding *mcb,
                                 struct waitset *ws, size_t buflen_words)
{
    errval_t err;

    monitor_lmp_init(mcb, ws);

    /* allocate a cap slot for the new endpoint cap */
    err = slot_alloc(&mcb->chan.local_cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    /* allocate a local endpoint */
    err = lmp_endpoint_create_in_slot(buflen_words, mcb->chan.local_cap,
                                      &mcb->chan.endpoint);
    if (err_is_fail(err)) {
        /* TODO: free cap slot */
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    /* allocate an initial receive slot */
    err = lmp_chan_alloc_recv_slot(&mcb->chan);
    if (err_is_fail(err)) {
        return err;
    }

    /* setup error handler */
    mcb->b.error_handler = error_handler;

    /* setup initial receive handlers */
    mcb->b.rx_vtbl = monitor_rx_vtbl;

    return SYS_ERR_OK;
}

/**
 * \brief Initiate a new LMP binding to the monitor
 *
 * Must only be called once at startup time on any dispatcher.
 *
 * \param mcb         Storage for binding state
 * \param ws          Waitset for handling incoming messages
 * \param cont        Continuation for when binding completes or fails
 * \param st          State passed to continuation function
 * \param buflen_words Size of incoming buffer, in number of words
 */
errval_t monitor_client_lmp_bind(struct monitor_lmp_binding *mcb,
                                 monitor_bind_continuation_fn *cont, void *st,
                                 struct waitset *ws, size_t lmp_buflen_words)
{
    errval_t err = init_lmp_binding(mcb, ws, lmp_buflen_words);
    if (err_is_fail(err)) {
        return err;
    }

    mcb->chan.remote_cap = cap_monitorep;

    /* Send the local endpoint cap to the monitor */
    mcb->chan.connstate = LMP_CONNECTED; /* pre-established */
    err = lmp_chan_send0(&mcb->chan, 0, mcb->chan.local_cap);
    if (err_is_fail(err)) {
        /* XXX: I'm lazily assuming this can never fail with a transient error,
         * since we only do it once at dispatcher startup. If not, we need to
         * register and retry here */
        assert(!lmp_err_is_transient(err));
        return err;
    }

    /* Run the RX handler; has a side-effect of registering for receive events */
    monitor_lmp_rx_handler(mcb);

    /* Run the continuation */
    cont(st, SYS_ERR_OK, &mcb->b);

    return SYS_ERR_OK;
}

/**
 * \brief Accept a new LMP binding in a client from the monitor
 *
 * Should only be used in the monitor.
 *
 * \param mcb         Storage for binding state
 * \param ws          Waitset for handling incoming messages
 * \param buflen_words Size of incoming buffer, in number of words
 */
errval_t monitor_client_lmp_accept(struct monitor_lmp_binding *mcb,
                                   struct waitset *ws, size_t lmp_buflen_words)
{
    errval_t err = init_lmp_binding(mcb, ws, lmp_buflen_words);
    if (err_is_fail(err)) {
        return err;
    }

    mcb->chan.connstate = LMP_MONITOR_ACCEPT;
    mcb->chan.remote_cap = NULL_CAP; // will be sent to us by the client

    /* Register for receive notification on our special handler */
    struct event_closure receive_handler = {
        .handler = monitor_accept_recv_handler,
        .arg = mcb,
    };
    err = lmp_chan_register_recv(&mcb->chan, ws, receive_handler);
    if (err_is_fail(err)) {
        return err; // TODO: cleanup?
    }

    return SYS_ERR_OK;
}

static void new_monitor_binding_reply_handler(struct monitor_binding *b,
                                              errval_t err, struct capref ep,
                                              uintptr_t st)
{
    struct monitor_lmp_binding *lmpb = (void *)st;

    if (err_is_fail(err)) {
        goto out;
    }

    // success! store the cap
    lmpb->chan.remote_cap = ep;

    /* Send the local endpoint cap to the monitor */
    lmpb->chan.connstate = LMP_CONNECTED; /* pre-established */
    err = lmp_chan_send0(&lmpb->chan, 0, lmpb->chan.local_cap);
    if (err_is_fail(err) && lmp_err_is_transient(err)) {
        // XXX: TODO: stack-rip, register send event and retry!
        USER_PANIC_ERR(err, "AB was lazy and didn't bother to retry a transient send error");
    }

out:
    /* Run the continuation */
    assert(lmpb->b.bind_cont != NULL);
    lmpb->b.bind_cont(lmpb->b.st, err, &lmpb->b);

    if (err_is_fail(err)) {
        // destroy the binding
        monitor_lmp_destroy(lmpb);
        free(lmpb);
    }
}

static void new_monitor_binding_request_sender(void *arg)
{
    struct monitor_lmp_binding *lmpb = arg; // new binding
    struct monitor_binding *mb = get_monitor_binding();
    errval_t err;

    /* Send request to the monitor on our existing binding */
    err = mb->tx_vtbl.new_monitor_binding_request(mb, NOP_CONT, (uintptr_t)lmpb);
    if (err_is_ok(err)) {
        event_mutex_unlock(&mb->mutex);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = mb->register_send(mb, mb->waitset,
                                MKCONT(new_monitor_binding_request_sender,lmpb));
        assert(err_is_ok(err)); // shouldn't fail, as we have the mutex
    } else { // permanent error
        event_mutex_unlock(&mb->mutex);
        lmpb->b.bind_cont(lmpb->b.st, err, &lmpb->b);
        monitor_lmp_destroy(lmpb);
        free(lmpb);
    }
}

/**
 * \brief Initiate a new (subsequent to boot) LMP binding to the monitor
 *
 * May be called after initialisation to create another binding to the monitor.
 * Must not be called in the monitor.
 *
 * \param cont        Continuation for when binding completes or fails
 * \param st          State passed to continuation function
 * \param buflen_words Size of incoming buffer, in number of words
 * \param ws          Waitset for handling incoming messages
 */
errval_t monitor_client_new_binding(monitor_bind_continuation_fn *cont, void *st,
                                    struct waitset *ws, size_t lmp_buflen_words)
{
    errval_t err;

    // allocate space for the binding state
    struct monitor_lmp_binding *lmpb = malloc(sizeof(struct monitor_lmp_binding));
    if (lmpb == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    err = init_lmp_binding(lmpb, ws, lmp_buflen_words);
    if (err_is_fail(err)) {
        free(lmpb);
        return err;
    }

    // copy the rx_vtbl from the main monitor binding
    struct monitor_binding *mcb = get_monitor_binding();
    lmpb->b.rx_vtbl = mcb->rx_vtbl;

    // store continuation
    lmpb->b.bind_cont = cont;
    lmpb->b.st = st;

    /* Run the RX handler; has a side-effect of registering for receive events */
    monitor_lmp_rx_handler(lmpb);

    /* Set reply handler (should only need to do this once!) */
    mcb->rx_vtbl.new_monitor_binding_reply = new_monitor_binding_reply_handler;

    /* Wait to use the monitor binding */
    event_mutex_enqueue_lock(&mcb->mutex, &lmpb->b.event_qnode,
                             MKCLOSURE(new_monitor_binding_request_sender,lmpb));

    return SYS_ERR_OK;
}

errval_t monitor_cap_set_remote(struct capref cap, bool remote)
{
    struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();
    assert(mc != NULL);
    errval_t err, reterr;

    err = mc->vtbl.cap_set_remote(mc, cap, remote, &reterr);
    if(err_is_fail(err)) {
        return err;
    } else {
        return reterr;
    }
}

struct bind_state {
    bool done;
    errval_t err;
};

static void monitor_rpc_bind_continuation(void *st_arg, errval_t err,
                                          struct monitor_blocking_binding *b)
{
    struct bind_state *st = st_arg;

    if (err_is_ok(err)) {
        struct monitor_blocking_rpc_client *r = 
            malloc(sizeof(struct monitor_blocking_rpc_client));
        assert(r != NULL);
        err = monitor_blocking_rpc_client_init(r, b);
        if (err_is_fail(err)) {
            free(r);
            USER_PANIC_ERR(err, "error in mem_rpc_client_init");
        } else {
            set_monitor_blocking_rpc_client(r);
        }
    }

    st->err  = err;
    st->done = true;
}

static void get_monitor_rpc_iref_reply(struct monitor_binding *mb, iref_t iref,
                                       uintptr_t st_arg)
{
    errval_t err;

    struct bind_state *st = (void *)st_arg;
    assert(iref != 0);

    struct monitor_blocking_lmp_binding *mbb = malloc(sizeof(*mbb));
    assert(mbb != NULL);

    err = monitor_blocking_lmp_bind(mbb, iref, monitor_rpc_bind_continuation,
                                    st, get_default_waitset(),
                                    IDC_BIND_FLAG_RPC_CAP_TRANSFER,
                                    LMP_RECV_LENGTH);
    if (err_is_fail(err)) {
        st->err  = err;
        st->done = true;
    }
}

/**
 * \brief Sets up binding to monitor's blocking rpc channel
 */
errval_t monitor_client_blocking_rpc_init(void)
{
    errval_t err;

    struct bind_state st = { .done = false };

    /* fire off a request for the iref for monitor rpc channel */
    struct monitor_binding *mb = get_monitor_binding();
    mb->rx_vtbl.get_monitor_rpc_iref_reply = get_monitor_rpc_iref_reply;
    err = mb->tx_vtbl.get_monitor_rpc_iref_request(mb, NOP_CONT, 
                                                   (uintptr_t) &st);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_GET_MON_BLOCKING_IREF);
    }

    
    /* block on the default waitset until we're bound */
    struct waitset *ws = get_default_waitset();
    while (!st.done) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_EVENT_DISPATCH);
        }
    }
    
    return st.err;
}
