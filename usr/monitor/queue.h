/**
 * \file
 * \brief Queue for stack-ripped inter-monitor code
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef QUEUE_H
#define QUEUE_H

struct msg_queue {
    struct msg_queue_elem *head, *tail;
};

struct msg_queue_elem {
    struct msg_queue_elem *next;
};

static inline bool msg_queue_is_empty(struct msg_queue *q)
{
    return (q->head == NULL);
}

struct intermon_msg_queue_elem;
typedef void (*intermon_msg_cont_handler_fn)(struct intermon_binding *b,
                                             struct intermon_msg_queue_elem *);

struct intermon_msg_queue_elem {
    struct msg_queue_elem queue;
    intermon_msg_cont_handler_fn cont;
};

errval_t intermon_enqueue_send(struct intermon_binding *b, struct msg_queue *q,
                               struct waitset *ws, struct msg_queue_elem *ms);

errval_t intermon_enqueue_send_at_front(struct intermon_binding *b, struct msg_queue *q,
                               struct waitset *ws, struct msg_queue_elem *ms);

struct monitor_msg_queue_elem;
typedef void (*monitor_msg_cont_handler_fn)(struct monitor_binding *b,
                                            struct monitor_msg_queue_elem *);

struct monitor_msg_queue_elem {
    struct msg_queue_elem queue;
    monitor_msg_cont_handler_fn cont;
};

errval_t monitor_enqueue_send(struct monitor_binding *b, struct msg_queue *q,
                              struct waitset *ws, struct msg_queue_elem *ms);

errval_t monitor_enqueue_send_at_front(struct monitor_binding *b, struct msg_queue *q,
                              struct waitset *ws, struct msg_queue_elem *ms);


void destroy_outgoing_cap(void *arg);
struct capref *caprefdup(struct capref cap);

#endif
