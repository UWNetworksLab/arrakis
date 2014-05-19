/**
 * \file
 * \brief Event queue
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_EVENT_QUEUE_H
#define BARRELFISH_EVENT_QUEUE_H

#include <sys/cdefs.h>

#include <barrelfish/waitset.h>
#include <barrelfish/threads.h>

__BEGIN_DECLS

/// What mode does an event queue operate in?
enum event_queue_mode {
    /// Run events continuously, as the waitset allows
    EVENT_QUEUE_CONTINUOUS,

    /// Trigger one event at a time, when event_queue_trigger() is called
    EVENT_QUEUE_ONESHOT,
};

struct event_queue_node {
    struct event_closure event;
    struct event_queue_node *next, *prev;
    bool run;
};

struct event_queue {
    struct thread_mutex mutex;
    struct waitset *waitset;
    struct waitset_chanstate waitset_state;
    struct event_queue_node *head, *tail;
    enum event_queue_mode mode;
};

void event_queue_init(struct event_queue *evq, struct waitset *waitset,
                      enum event_queue_mode mode);
void event_queue_add(struct event_queue *q, struct event_queue_node *qn,
                     struct event_closure event);
errval_t event_queue_cancel(struct event_queue *q, struct event_queue_node *qn);
errval_t event_queue_trigger(struct event_queue *q);

__END_DECLS

#endif // BARRELFISH_EVENT_QUEUE_H
