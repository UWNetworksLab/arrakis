/**
 * \file
 * \brief Queue for stack-ripped inter-monitor code
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"

/**
 * \brief Enqueue on a waitset queue.
 *
 * \param qs    Pointer to queue to enqueue on
 * \param ms    Pointer to element to enqueue
 *
 * \return true if queue was empty, false if not.
 */
static bool enqueue_send(struct msg_queue *q, struct msg_queue_elem *m)
{
    assert(m->next == NULL);

    // Enqueue on the queue
    if(q->tail != NULL) {
        q->tail->next = m;
    } else {
        assert(q->head == NULL);
        q->head = m;
    }
    q->tail = m;

    return q->head == q->tail ? true : false;
}

/**
 * \brief Enqueue an element on a waitset queue IN FRONT.
 *
 * \param qs    Pointer to queue to enqueue on
 * \param ms    Pointer to element to enqueue
 *
 * \return true if queue was empty, false if not.
 */
static bool enqueue_send_at_front(struct msg_queue *q, struct msg_queue_elem *m)
{
    assert(m->next == NULL);
    if(q->tail == NULL) {
        assert(q->head == NULL);
    	q->head = m;
    	q->tail = m;
    } else {
    	m->next = q->head;
    	q->head = m;
    }
    return q->head == q->tail ? true : false;
}

static struct msg_queue_elem *dequeue_send(struct msg_queue *q)
{
    // Queue should have at least one element
    assert(q->head != NULL && q->tail != NULL);

    struct msg_queue_elem *e = q->head;
    q->head = e->next;
    if(q->head == NULL) {
        q->tail = NULL;
    }

    return e;
}

static void intermon_send_handler(void *arg)
{
    struct intermon_binding *b = arg;
    struct intermon_state *is = b->st;
    struct msg_queue *mq = &is->queue;

    // Dequeue next element from the queue
    struct intermon_msg_queue_elem *e =
        (struct intermon_msg_queue_elem *)dequeue_send(mq);

    // If the queue is non-empty, re-register
    if (!msg_queue_is_empty(mq)) {
        struct waitset *ws = get_default_waitset(); // XXX: store this on the q?
        errval_t err = b->register_send(b, ws, MKCONT(intermon_send_handler,b));
        assert(err_is_ok(err));
    }

    assert(e->cont != NULL);
    e->cont(b, e);
}

errval_t intermon_enqueue_send(struct intermon_binding *b, struct msg_queue *q,
                               struct waitset *ws, struct msg_queue_elem *ms)
{
    ms->next = NULL;

    // If queue was empty, enqueue on waitset
    if(enqueue_send(q, ms)) {
        return b->register_send(b, ws, MKCONT(intermon_send_handler,b));
    } else {
        return SYS_ERR_OK;
    }
}

errval_t intermon_enqueue_send_at_front(struct intermon_binding *b, struct msg_queue *q,
                               struct waitset *ws, struct msg_queue_elem *ms)
{
    ms->next = NULL;

    // If queue was empty, enqueue on waitset
    if(enqueue_send_at_front(q, ms)) {
        return b->register_send(b, ws, MKCONT(intermon_send_handler,b));
    } else {
        return SYS_ERR_OK;
    }
}

static void monitor_send_handler(void *arg)
{
    struct monitor_binding *b = arg;
    struct monitor_state *is = b->st;
    struct msg_queue *mq = &is->queue;

    // Dequeue next element from the queue
    struct monitor_msg_queue_elem *e =
        (struct monitor_msg_queue_elem *)dequeue_send(mq);

    // If the queue is non-empty, re-register
    if (!msg_queue_is_empty(mq)) {
        struct waitset *ws = get_default_waitset(); // XXX: store this on the q?
        errval_t err = b->register_send(b, ws, MKCONT(monitor_send_handler,b));
        assert(err_is_ok(err));
    }

    assert(e->cont != NULL);
    e->cont(b, e);
}

errval_t monitor_enqueue_send(struct monitor_binding *b, struct msg_queue *q,
                              struct waitset *ws, struct msg_queue_elem *ms)
{
    ms->next = NULL;

    // If queue was empty, enqueue on waitset
    if(enqueue_send(q, ms)) {
        return b->register_send(b, ws, MKCONT(monitor_send_handler,b));
    } else {
        return SYS_ERR_OK;
    }
}

errval_t monitor_enqueue_send_at_front(struct monitor_binding *b, struct msg_queue *q,
                              struct waitset *ws, struct msg_queue_elem *ms)
{
    ms->next = NULL;

    // If queue was empty, enqueue on waitset
    if(enqueue_send_at_front(q, ms)) {
        return b->register_send(b, ws, MKCONT(monitor_send_handler,b));
    } else {
        return SYS_ERR_OK;
    }
}

/// Common send continuation function to destroy a cap that has been sent
/// NB: Not part of the queue functionality
void destroy_outgoing_cap(void *arg)
{
    struct capref *cap = arg;
    assert(cap != NULL);
    assert(!capref_is_null(*cap));

    errval_t err = cap_destroy(*cap);
    if (err_is_fail(err)) {
        if(err_no(err) != SYS_ERR_CAP_NOT_FOUND) {
            DEBUG_ERR(err, "cap_destroy failed");
        }
    }

    free(cap);
}

/// Returns a copy of the given capref in a malloc'ed buffer
struct capref *caprefdup(struct capref cap)
{
    struct capref *p = malloc(sizeof(struct capref));
    assert(p != NULL);
    *p = cap;
    return p;
}
