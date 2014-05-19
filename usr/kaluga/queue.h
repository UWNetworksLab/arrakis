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

#ifndef MON_QUEUE_H
#define MON_QUEUE_H

#include <barrelfish/barrelfish.h>
#include <if/monitor_defs.h>

struct mon_msg_state;
typedef void(*send_handler_fn)(struct monitor_binding*, struct mon_msg_state*);

struct mon_msg_state {
    coreid_t core_id;
    uint64_t arch_id; // TODO: uint64?

    send_handler_fn send;
    struct mon_msg_state *next;
};

void enqueue_msg_state(struct monitor_binding *b, struct mon_msg_state* st);
struct mon_msg_state* dequeue_msg_state(struct monitor_binding *b);

#endif // MON_QUEUE_H
