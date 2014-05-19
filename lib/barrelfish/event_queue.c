/**
 * \file
 * \brief Event queue implementatino
 *
 * This code implements a thread-safe queue of pending events which are
 * serviced by a single waitset.
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
#include <barrelfish/event_queue.h>
#include <barrelfish/waitset_chan.h>

/**
 * \brief Initialise a new event queue
 *
 * \param q Storage for event queue
 * \param waitset Waitset that will service the queue
 * \param mode Operating mode for the queue
 */
void event_queue_init(struct event_queue *q, struct waitset *waitset,
                      enum event_queue_mode mode)
{
    waitset_chanstate_init(&q->waitset_state, CHANTYPE_EVENT_QUEUE);
    thread_mutex_init(&q->mutex);
    q->head = q->tail = NULL;
    q->waitset = waitset;
    q->mode = mode;
}

static struct event_queue_node *next_event(struct event_queue *q)
{
    // dequeue the next node from the head
    struct event_queue_node *qn = q->head;

    if (qn == NULL) {
        return NULL;
    }

    assert(qn->prev == NULL);

    if (qn->next == NULL) {
        assert(q->tail == qn);
        q->head = q->tail = NULL;
    } else {
        qn->next->prev = NULL;
        q->head = qn->next;
    }

    return qn;
}

static void event_queue_runner(void *arg)
{
    struct event_queue *q = arg;
    errval_t err;

    assert(q->mode == EVENT_QUEUE_CONTINUOUS);

    thread_mutex_lock(&q->mutex);

    // dequeue the next node from the head
    struct event_queue_node *qn = next_event(q);
    if (qn == NULL) {
        // an event was cancelled while we were pending
        thread_mutex_unlock(&q->mutex);
        return;
    }

    if (q->head == NULL) {
        // queue is non-empty: trigger ourselves again
        struct event_closure self = {
            .handler = event_queue_runner,
            .arg = arg
        };
        err = waitset_chan_trigger_closure(q->waitset, &q->waitset_state, self);
        assert(err_is_ok(err)); // shouldn't fail
    }

    qn->run = true;
    thread_mutex_unlock(&q->mutex);

    // run closure
    qn->event.handler(qn->event.arg);
}

/**
 * \brief Add a new event to an event queue
 *
 * \param q Event queue
 * \param qn Storage for queue node (uninitialised)
 * \param event Event closure
 */
void event_queue_add(struct event_queue *q, struct event_queue_node *qn,
                     struct event_closure event)
{
    errval_t err;

    qn->event = event;
    qn->run = false;

    thread_mutex_lock(&q->mutex);

    // enqueue at tail
    if (q->tail == NULL) {
        assert(q->head == NULL);
        qn->next = qn->prev = NULL;
        q->head = q->tail = qn;

        // was empty: need to trigger queue runner if in continuous mode
        if (q->mode == EVENT_QUEUE_CONTINUOUS) {
            struct event_closure runner = {
                .handler = event_queue_runner,
                .arg = q
            };
            err = waitset_chan_trigger_closure(q->waitset, &q->waitset_state,
                                               runner);
            assert(err_is_ok(err)); // shouldn't fail
        }
    } else {
        assert(q->tail != qn); // don't re-enqueue the same node!
        assert(q->tail->next == NULL);
        q->tail->next = qn;
        qn->prev = q->tail;
        qn->next = NULL;
        q->tail = qn;

        // runner is already active if it needs to be, don't need to do anything
    }

    thread_mutex_unlock(&q->mutex);
}

/**
 * \brief Cancel an event previously added to an event queue
 *
 * \param q Event queue
 * \param qn Queue node which was previously added to #q by event_queue_add()
 */
errval_t event_queue_cancel(struct event_queue *q, struct event_queue_node *qn)
{
    errval_t err;

    if (qn->run) {
        return LIB_ERR_EVENT_ALREADY_RUN;
    }

    thread_mutex_lock(&q->mutex);

    if (qn->run) {
        thread_mutex_unlock(&q->mutex);
        return LIB_ERR_EVENT_ALREADY_RUN;
    }

    // dequeue
    if (qn->next == NULL) {
        assert(q->tail == qn);
        q->tail = qn->prev;
    } else {
        qn->next->prev = qn->prev;
    }

    if (qn->prev == NULL) {
        assert(q->head == qn);
        q->head = qn->next;
    } else {
        qn->prev->next = qn->next;
    }

    // if the queue is now empty, we should cancel the runner
    if (q->head == NULL && q->mode == EVENT_QUEUE_CONTINUOUS) {
        assert(q->tail == NULL);
        err = waitset_chan_deregister(&q->waitset_state);
        if (err_is_fail(err)) {
            // can fail if the event already fired, but this is ok
            assert(err_no(err) == LIB_ERR_CHAN_NOT_REGISTERED);
        }
    }

    thread_mutex_unlock(&q->mutex);
    return SYS_ERR_OK;
}

/**
 * \brief Trigger the next event on a queue which is operating in one-shot mode
 *
 * Must not be called before the previously-triggered event has run.
 *
 * \param q Event queue
 */
errval_t event_queue_trigger(struct event_queue *q)
{
    assert(q->mode == EVENT_QUEUE_ONESHOT);

    thread_mutex_lock(&q->mutex);

    struct event_queue_node *qn = next_event(q);

    thread_mutex_unlock(&q->mutex);

    if (qn == NULL) {
        return LIB_ERR_EVENT_QUEUE_EMPTY;
    }

    // trigger closure on waitset
    qn->run = true;
    return waitset_chan_trigger_closure(q->waitset, &q->waitset_state, qn->event);
}
