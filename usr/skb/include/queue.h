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

#include <barrelfish/barrelfish.h>
#include <if/skb_defs.h>
#include <include/skb_server.h>

void enqueue_reply_state(struct skb_binding *b, struct skb_reply_state* st);
struct skb_reply_state* dequeue_reply_state(struct skb_binding *b);

#endif
