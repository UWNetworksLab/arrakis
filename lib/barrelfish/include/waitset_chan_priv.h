/**
 * \file
 * \brief Libbarrelfish-private waitset interface to channel implementations
 */

/*
 * Copyright (c) 2009, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_WAITSET_CHAN_PRIV_H
#define BARRELFISH_WAITSET_CHAN_PRIV_H

#include <barrelfish/waitset.h>

errval_t waitset_chan_trigger_disabled(struct waitset_chanstate *chan,
                                       dispatcher_handle_t handle);
errval_t waitset_chan_trigger_closure_disabled(struct waitset *ws,
                                               struct waitset_chanstate *chan,
                                               struct event_closure closure,
                                               dispatcher_handle_t handle);
errval_t waitset_chan_deregister_disabled(struct waitset_chanstate *chan);
errval_t waitset_chan_register_disabled(struct waitset *ws,
                                        struct waitset_chanstate *chan,
                                        struct event_closure closure);
errval_t waitset_chan_register_polled_disabled(struct waitset *ws,
                                               struct waitset_chanstate *chan,
                                               struct event_closure closure,
                                               dispatcher_handle_t handle);
errval_t waitset_chan_start_polling(struct waitset_chanstate *chan);
errval_t waitset_chan_stop_polling(struct waitset_chanstate *chan);

#endif // BARRELFISH_WAITSET_CHAN_PRIV_H
