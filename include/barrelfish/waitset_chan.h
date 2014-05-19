/**
 * \file
 * \brief Waitset interface to channel implementations
 */

/*
 * Copyright (c) 2009, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_WAITSET_CHAN_H
#define BARRELFISH_WAITSET_CHAN_H

#include <sys/cdefs.h>
#include <barrelfish/waitset.h>

__BEGIN_DECLS

static inline bool waitset_chan_is_registered(struct waitset_chanstate *chan)
{
    return chan->waitset != NULL &&
            (chan->state == CHAN_IDLE || chan->state == CHAN_POLLED);
}

void waitset_chanstate_init(struct waitset_chanstate *chan,
                            enum ws_chantype chantype);
void waitset_chanstate_destroy(struct waitset_chanstate *chan);
errval_t waitset_chan_trigger(struct waitset_chanstate *chan);
errval_t waitset_chan_trigger_closure(struct waitset *ws,
                                      struct waitset_chanstate *chan,
                                      struct event_closure closure);
errval_t waitset_chan_deregister(struct waitset_chanstate *chan);
errval_t waitset_chan_register(struct waitset *ws, struct waitset_chanstate *chan,
                               struct event_closure closure);
void waitset_chan_migrate(struct waitset_chanstate *chan,
                          struct waitset *new_ws);
errval_t waitset_chan_register_polled(struct waitset *ws,
                                      struct waitset_chanstate *chan,
                                      struct event_closure closure);

__END_DECLS

#endif // BARRELFISH_WAITSET_CHAN_H
