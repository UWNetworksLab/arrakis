/**
 * \file
 * \brief Queue to deal with flounder continuations.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include "queue.h"

static void oct_rpc_send_next(void *arg)
{
    struct octopus_binding *b = arg;

    struct oct_reply_state* current = oct_rpc_dequeue_reply(b);

    // If more state in the queue, send them
    if (current) {
        current->reply(b, current);
    }
}

void oct_rpc_enqueue_reply(struct octopus_binding *b,
        struct oct_reply_state* st)
{
    if (b->st == NULL) {
        struct waitset *ws = get_default_waitset();
        b->register_send(b, ws, MKCONT(oct_rpc_send_next, b));
    }

    struct oct_reply_state** walk = (struct oct_reply_state**) &(b->st);
    for (; *walk != NULL; walk = &(*walk)->next) {
        // continue
    }
    *walk = st;
    st->next = NULL;
}

struct oct_reply_state* oct_rpc_dequeue_reply(struct octopus_binding *b)
{
    struct oct_reply_state* head = b->st;
    b->st = head->next;

    // Reregister for sending, if we need to send more
    if (b->st != NULL) {
        struct waitset *ws = get_default_waitset();
        errval_t err = b->register_send(b, ws, MKCONT(oct_rpc_send_next, b));
        assert(err_is_ok(err));
    }

    return head;
}

