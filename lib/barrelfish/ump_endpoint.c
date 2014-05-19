/**
 * \file
 * \brief Incoming UMP endpoints
 */

/*
 * Copyright (c) 2009, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/ump_endpoint.h>
#include <barrelfish/ump_impl.h>
#include <barrelfish/waitset.h>
#include <barrelfish/waitset_chan.h>
#include "waitset_chan_priv.h"

/**
 * \brief Initialise a new UMP endpoint
 *
 * \param ep Storage for endpoint state
 * \param buf Pointer to incoming message buffer
 * \param bufsize Size of buf in bytes (must be multiple of UMP message size)
 */
errval_t ump_endpoint_init(struct ump_endpoint *ep, volatile void *buf,
                           size_t bufsize)
{
    errval_t err = ump_chan_state_init(&ep->chan, buf, bufsize, UMP_INCOMING);
    if (err_is_fail(err)) {
        return err;
    }

    waitset_chanstate_init(&ep->waitset_state, CHANTYPE_UMP_IN);
    return SYS_ERR_OK;
}

/**
 * \brief Destroy the local state associated with a given UMP endpoint
 */
void ump_endpoint_destroy(struct ump_endpoint *ep)
{
    waitset_chanstate_destroy(&ep->waitset_state);
}

/**
 * \brief Register an event handler to be notified when messages can be received
 *
 * In the future, call the closure on the given waitset when it is likely that
 * a message can be received on the endpoint. An endpoint may only be registered
 * with a single event handler on a single waitset at any one time.
 *
 * \param ep UMP endpoint
 * \param ws Waitset
 * \param closure Event handler
 */
errval_t ump_endpoint_register(struct ump_endpoint *ep, struct waitset *ws,
                                struct event_closure closure)
{
    assert(ep != NULL);
    assert(ws != NULL);

    if (ump_endpoint_can_recv(ep)) { // trigger event immediately
        return waitset_chan_trigger_closure(ws, &ep->waitset_state, closure);
    } else {
        return waitset_chan_register_polled(ws, &ep->waitset_state, closure);
    }
}

/**
 * \brief Cancel an event registration made with ump_endpoint_register()
 *
 * \param ep UMP Endpoint
 */
errval_t ump_endpoint_deregister(struct ump_endpoint *ep)
{
    assert(ep);
    return waitset_chan_deregister(&ep->waitset_state);
}

#include <stdio.h>
/**
 * \brief Migrate an event registration made with ump_endpoint_register() to a new waitset.
 *
 * \param ep LMP Endpoint
 * \param ws New waitset
 */
void ump_endpoint_migrate(struct ump_endpoint *ep, struct waitset *ws)
{
    printf("ump_endpoint_migrate\n");
    waitset_chan_migrate(&ep->waitset_state, ws);
}
