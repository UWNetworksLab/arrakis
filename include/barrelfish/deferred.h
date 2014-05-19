/**
 * \file
 * \brief Deferred events (ie. timers)
 */

/*
 * Copyright (c) 2009, 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_DEFERRED_H
#define BARRELFISH_DEFERRED_H

#include <sys/cdefs.h>
#include <barrelfish/waitset.h>

__BEGIN_DECLS

struct deferred_event {
    struct waitset_chanstate waitset_state; ///< Waitset state
    struct deferred_event *next, *prev; ///< Next/prev in dispatcher queue
    systime_t time;                     ///< System time for event
};

systime_t get_system_time(void);

void deferred_event_init(struct deferred_event *event);
errval_t deferred_event_register(struct deferred_event *event,
                                 struct waitset *ws, delayus_t delay,
                                 struct event_closure closure);
errval_t deferred_event_cancel(struct deferred_event *event);
errval_t barrelfish_usleep(delayus_t delay);

struct periodic_event {
    struct deferred_event de;
    struct event_closure cl;
    struct waitset *waitset;
    delayus_t period;
};

errval_t periodic_event_create(struct periodic_event *event, struct waitset *ws,
                               delayus_t period, struct event_closure closure);
errval_t periodic_event_cancel(struct periodic_event *event);

// XXX: internal to libbarrelfish; should be in another header file
void trigger_deferred_events_disabled(dispatcher_handle_t dh, systime_t now);

__END_DECLS

#endif // BARRELFISH_DEFERRED_H
