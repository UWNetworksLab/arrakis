/**
 * \file
 * \brief Event mutex
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_EVENT_MUTEX_H
#define BARRELFISH_EVENT_MUTEX_H

#include <sys/cdefs.h>

#include <barrelfish/event_queue.h>
#include <barrelfish/threads.h>

__BEGIN_DECLS

struct event_mutex {
    struct thread_mutex tmutex;
    struct event_queue equeue;
    struct thread *tqueue;
    bool locked;
};

void event_mutex_init(struct event_mutex *em, struct waitset *waitset);
bool event_mutex_enqueue_lock(struct event_mutex *em,
                              struct event_queue_node *qn,
                              struct event_closure lockcont);
void event_mutex_threaded_lock(struct event_mutex *em);
void event_mutex_unlock(struct event_mutex *em);

__END_DECLS

#endif // BARRELFISH_EVENT_MUTEX_H
