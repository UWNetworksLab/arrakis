/**
 * \file
 * \brief Monitor's connection with the dispatchers on the same core
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
#include <barrelfish/debug.h> // XXX: To set the cap_identify_reply handler
#include <barrelfish/sys_debug.h> // XXX: for sys_debug_send_ipi
#include <trace/trace.h>
#include <if/mem_defs.h>
#include <barrelfish/monitor_client.h>
#include <if/monitor_loopback_defs.h>
#include <notify_ipi.h>

struct ipi_alloc_notify_reply_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_ipi_alloc_notify_reply__args args;
};

static void
ipi_alloc_notify_reply_cont(struct monitor_binding *b,
                            uintptr_t state, struct capref notify_cap,
                            errval_t err);

static void ipi_alloc_notify_reply_handler(struct monitor_binding *b,
                                           struct monitor_msg_queue_elem *e)
{
    struct ipi_alloc_notify_reply_state *st =
        (struct ipi_alloc_notify_reply_state *)e;
    ipi_alloc_notify_reply_cont(b, st->args.state, st->args.notify,
                                st->args.err);
    free(st);
}

static void ipi_alloc_notify_reply_cont(struct monitor_binding *b,
                                        uintptr_t state,
                                        struct capref notify_cap,
                                        errval_t reterr)
{
    errval_t err =
        b->tx_vtbl.ipi_alloc_notify_reply(b, NOP_CONT, state,
                                          notify_cap, reterr);

    if(err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct monitor_state *st = b->st;
            struct ipi_alloc_notify_reply_state *me =
                malloc(sizeof(struct ipi_alloc_notify_reply_state));
            assert(me != NULL);
            me->args.state = state;
            me->args.notify = notify_cap;
            me->args.err = reterr;
            me->elem.cont = ipi_alloc_notify_reply_handler;
            err = monitor_enqueue_send(b, &st->queue,
                                       get_default_waitset(), &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_enqueue_send failed");
            }
            return;
        }
        USER_PANIC_ERR(err, "sending reply");
    }
    assert(err_is_ok(err));
}

static void ipi_alloc_notify_request(struct monitor_binding *st,
                                     struct capref ep, uintptr_t state)
{
    errval_t err;
    struct capref notify_cap = NULL_CAP;

    // Allocate a local notify channel ID
    int chanid;
    err = notification_allocate(ep, &chanid);
    if(err_is_fail(err)) {
        goto out;
    }

    // Get my arch ID
    uintptr_t my_arch_id = 0;
    err = invoke_monitor_get_arch_id(&my_arch_id);
    assert(err == SYS_ERR_OK);

    // Create notify cap
    err = notification_create_cap(chanid, my_arch_id, &notify_cap);

 out:
    // Return the notify cap or error
    ipi_alloc_notify_reply_cont(st, state, notify_cap, err);
}

errval_t monitor_server_arch_init(struct monitor_binding *b)
{
    b->rx_vtbl.ipi_alloc_notify_request = ipi_alloc_notify_request;
    return SYS_ERR_OK;
}
