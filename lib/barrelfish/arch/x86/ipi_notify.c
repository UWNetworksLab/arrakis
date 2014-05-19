/**
 * \file
 * \brief Bidirectional IPI (inter-processor interrupt) signaling implementation
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <arch/x86/barrelfish/ipi_notify.h>
#include <if/monitor_defs.h>

static void ipi_alloc_notify_reply(struct monitor_binding *b, uintptr_t st,
                                   struct capref notify_cap, errval_t err)
{
    struct ipi_notify *uc = (void *)st;
    uc->my_notify_cap = notify_cap;
    assert(uc->cont.handler != NULL);
    uc->cont.handler(uc->cont.st, err, uc);
}

errval_t ipi_notify_init(struct ipi_notify *rn, struct capref rmt_notify_cap,
                         struct capref my_notify_cap, struct capref ep,
                         struct lmp_endpoint *iep)
{
    rn->rmt_notify_cap = rmt_notify_cap;
    rn->my_notify_cap = my_notify_cap;
    rn->ep = ep;
    rn->iep = iep;
    return SYS_ERR_OK;
}

/**
 * \brief Set remote notify cap.
 *
 * \param notify        Cap to notify the remote side.
 */
errval_t ipi_notify_set(struct ipi_notify *rn, struct capref notify)
{
    rn->rmt_notify_cap = notify;
    return SYS_ERR_OK;
}

static void ipi_alloc_notify_try_request(void* arg) {
    struct ipi_notify* uc = arg;
    struct monitor_binding* b = get_monitor_binding();

    errval_t err = b->tx_vtbl.ipi_alloc_notify_request(b, NOP_CONT, uc->ep, (uintptr_t)uc);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            b->register_send(b, b->waitset, MKCONT(ipi_alloc_notify_try_request, uc));
        } else {
            USER_PANIC_ERR(err, "ipi_notify_alloc fail");
        }
    }
}

/**
 * \brief Initialise a new IPI notify channel
 *
 * \param uc Storage for channel state
 */
errval_t ipi_notify_alloc(struct ipi_notify *uc,
                          struct ipi_alloc_continuation cont)
{
    // Allocate receive endpoint
    // use minimum-sized endpoint buffer, as we don't care about its contents
    errval_t err = endpoint_create(LMP_RECV_LENGTH, &uc->ep, &uc->iep);
    assert(err_is_ok(err));

    // Initialize the rest
    uc->cont = cont;

    ipi_alloc_notify_try_request(uc);

    return SYS_ERR_OK;
}

static void ipi_notify_handler(void *arg)
{
    struct ipi_notify *uc = arg;
    struct lmp_recv_buf dummy = { .buflen = 0 };

    // Consume the endpoint message
    errval_t err = lmp_endpoint_recv(uc->iep, &dummy, NULL);
    assert(err_is_ok(err));

    // Call user's closure
    uc->closure.handler(uc->closure.arg);
}

/**
 * \brief Register an event handler to be notified when messages can be received
 *
 * In the future, call the closure on the given waitset when it is likely that
 * a message can be received on the channel. A channel may only be registered
 * with a single receive event handler on a single waitset at any one time.
 *
 * \param uc IPI channel
 * \param ws Waitset
 * \param closure Event handler
 */
errval_t ipi_notify_register(struct ipi_notify *uc,
                             struct waitset *ws,
                             struct event_closure closure)
{
    struct event_closure cl = {
        .handler = ipi_notify_handler,
        .arg = uc
    };

    uc->closure = closure;
    assert(uc->iep != NULL);
    return lmp_endpoint_register(uc->iep, ws, cl);
}

/// Destroy the local state associated with a given channel
void ipi_notify_destroy(struct ipi_notify *uc)
{
    lmp_endpoint_free(uc->iep);
    cap_destroy(uc->ep);
    cap_destroy(uc->my_notify_cap);
    cap_destroy(uc->rmt_notify_cap);
}

/// Initialise the IPI channel driver
void ipi_init(void)
{
    struct monitor_binding *mcb = get_monitor_binding();

    mcb->rx_vtbl.ipi_alloc_notify_reply = ipi_alloc_notify_reply;
}
