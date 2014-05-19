/**
 * \file
 * \brief UMP endpoint declarations
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_UMP_ENDPOINT_H
#define LIBBARRELFISH_UMP_ENDPOINT_H

#include <sys/cdefs.h>

#include <barrelfish/waitset.h>
#include <barrelfish/ump_impl.h>

__BEGIN_DECLS

/// Incoming UMP endpoint
struct ump_endpoint {
    struct waitset_chanstate waitset_state; ///< Waitset per-channel state
    struct ump_chan_state    chan;          ///< Incoming UMP channel state to poll
};

errval_t ump_endpoint_init(struct ump_endpoint *ep, volatile void *buf,
                           size_t bufsize);
void ump_endpoint_destroy(struct ump_endpoint *ep);
errval_t ump_endpoint_register(struct ump_endpoint *ep, struct waitset *ws,
                                struct event_closure closure);
errval_t ump_endpoint_deregister(struct ump_endpoint *ep);
void ump_endpoint_migrate(struct ump_endpoint *ep, struct waitset *ws);

/**
 * \brief Returns true iff there is a message pending on the given UMP endpoint
 */
static inline bool ump_endpoint_can_recv(struct ump_endpoint *ep)
{
    return ump_impl_poll(&ep->chan) != NULL;
}

/**
 * \brief Retrieve a message from the given UMP endpoint, if possible
 *
 * Non-blocking, may fail if there are no messages available.
 *
 * \param ep UMP endpoint
 * \param retmsg Storage for incoming message
 */
static inline errval_t ump_endpoint_recv(struct ump_endpoint *ep,
                                          volatile struct ump_message **msg)
{
    *msg = ump_impl_recv(&ep->chan);

    if(*msg != NULL) {
        return SYS_ERR_OK;
    } else {
        return LIB_ERR_NO_UMP_MSG;
    }
}

__END_DECLS

#endif // LIBBARRELFISH_UMP_ENDPOINT_H
