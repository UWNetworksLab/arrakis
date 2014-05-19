/**
 * \file
 * \brief Event mutex implementation
 *
 * This code implements an event-driven mutex which supports lock acquisition
 * events.
 */

/*
 * Copyright (c) 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/event_mutex.h>
#include "threads_priv.h"

/// Initialise a new event mutex
void event_mutex_init(struct event_mutex *em, struct waitset *waitset)
{
    thread_mutex_init(&em->tmutex);
    event_queue_init(&em->equeue, waitset, EVENT_QUEUE_ONESHOT);
    em->tqueue = NULL;
    em->locked = false;
}

/**
 * \brief Enqueue a lock acquisition request for the mutex
 *
 * \param em Event mutex
 * \param qn Storage for queue node. allocated by caller, live until continuation runs
 * \param lockcont Continuation for lock acquisition
 *
 * \returns true iff the lock was acquired and the continuation run, false if enqueued
 */
bool event_mutex_enqueue_lock(struct event_mutex *em,
                              struct event_queue_node *qn,
                              struct event_closure lockcont)
{
    thread_mutex_lock(&em->tmutex);

    if (!em->locked) {
        // got the lock! we're the first in, so we can just run our event
        em->locked = true;
        thread_mutex_unlock(&em->tmutex);
        lockcont.handler(lockcont.arg);
        return true;
    } else {
        // failed, enqueue our event
        event_queue_add(&em->equeue, qn, lockcont);
        thread_mutex_unlock(&em->tmutex);
        return false;
    }
}

/**
 * \brief Acquire the mutex, blocking the calling thread until done
 *
 * \param em Event mutex
 */
void event_mutex_threaded_lock(struct event_mutex *em)
{
    thread_mutex_lock(&em->tmutex);

    if (!em->locked) {
        // got the lock! we're done...
        em->locked = true;
        thread_mutex_unlock(&em->tmutex);
    } else {
        // add ourselves to the thread queue and block
        // XXX: TODO: the mutex unlock and block on the queue must be atomic
        assert(!"this is broken without thread_block_and_release_mutex()");
        thread_mutex_unlock(&em->tmutex);
        void *wakeup_reason = thread_block(&em->tqueue);

        assert(wakeup_reason == em);
        assert(em->locked);
    }
}

/**
 * \brief Unlock the mutex, which must be held by the caller
 *
 * \note This always prefers releasing a thread rather than triggering the next
 * event, so threaded code can starve event-driven code in a program that mixes
 * the two models.
 */
void event_mutex_unlock(struct event_mutex *em)
{
    errval_t err;

    thread_mutex_lock(&em->tmutex);
    assert(em->locked);

    // is there a thread waiting? if so unblock it
    if (em->tqueue != NULL) {
        thread_unblock_one(&em->tqueue, em);
        thread_mutex_unlock(&em->tmutex);
        return;
    }

    // fire the next event if there is one, handing over the lock
    err = event_queue_trigger(&em->equeue);
    if (err_is_ok(err)) {
        // new event is now triggered: keep lock held
    } else {
        assert(err_no(err) == LIB_ERR_EVENT_QUEUE_EMPTY);
        // nothing else in the queue: release lock
        em->locked = false;
    }

    thread_mutex_unlock(&em->tmutex);
}

