/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CHIPS_QUEUE_H
#define CHIPS_QUEUE_H

#include <barrelfish/barrelfish.h>
#include <if/nameservice_defs.h>

struct ns_reply_state;
typedef void(*reply_handler_fn)(struct nameservice_binding*, struct ns_reply_state*);

struct ns_reply_state {
    errval_t err;
    iref_t iref;
    struct capref cap;
    nameservice_reghandle_t reg_handle;
    nameservice_srvref_t ref_handle;
    uint32_t semval;

    reply_handler_fn rpc_reply;
    struct ns_reply_state* next;
};

void enqueue_msg_state(struct nameservice_binding *b, struct ns_reply_state* st);
struct ns_reply_state* dequeue_msg_state(struct nameservice_binding *b);
errval_t new_ns_reply(struct ns_reply_state**, reply_handler_fn);

#endif // CHIPS_QUEUE_H
