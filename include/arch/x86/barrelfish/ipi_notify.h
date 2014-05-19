/**
 * \file
 * \brief Bidirectional IPI signaling channel
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_IPI_NOTIFY_H
#define BARRELFISH_IPI_NOTIFY_H

#include <barrelfish/waitset.h>
#include <barrelfish/capabilities.h>
#include <barrelfish/lmp_endpoints.h>

struct ipi_notify;

struct ipi_alloc_continuation {
    /**
     * \brief Handler which runs when a binding succeeds or fails
     * \param st State pointer set in closure
     * \param err Success/failure of binding
     * \param uc On success, contains pointer to channel
     */
    void (*handler)(void *st, errval_t err, struct ipi_notify *rc);
    void *st;
};

/// A bidirectional IPI channel
struct ipi_notify {
    struct capref my_notify_cap, rmt_notify_cap, ep;
    struct lmp_endpoint *iep;
    struct ipi_alloc_continuation cont;
    struct event_closure closure;
};

errval_t ipi_notify_init(struct ipi_notify *rn, struct capref rmt_notify_cap,
                         struct capref my_notify_cap, struct capref ep,
                         struct lmp_endpoint *iep);
errval_t ipi_notify_alloc(struct ipi_notify *uc,
                          struct ipi_alloc_continuation cont);
errval_t ipi_notify_set(struct ipi_notify *uc, struct capref notify);
void ipi_notify_destroy(struct ipi_notify *uc);
void ipi_init(void);
errval_t ipi_notify_register(struct ipi_notify *uc,
                             struct waitset *ws,
                             struct event_closure closure);

/**
 * \brief Cancel an event registration made with ipi_chan_register_recv()
 *
 * \param uc IPI channel
 */
static inline errval_t ipi_notify_deregister(struct ipi_notify *uc)
{
    return lmp_endpoint_deregister(uc->iep);
}

static inline errval_t ipi_notify_raise(struct ipi_notify *rc)
{
    // Send notification
    return invoke_ipi_notify_send(rc->rmt_notify_cap);
}

#endif // BARRELFISH_IPI_NOTIFY_H
