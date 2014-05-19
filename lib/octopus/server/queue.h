/**
 * \file
 * \brief Queue for stack-ripped octopus server-side handler code
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_QUEUE_H
#define OCTOPUS_QUEUE_H

#include <barrelfish/barrelfish.h>
#include <if/octopus_defs.h>

#include <octopus_server/service.h>

void oct_rpc_enqueue_reply(struct octopus_binding *b,
        struct oct_reply_state* st);
struct oct_reply_state* oct_rpc_dequeue_reply(struct octopus_binding *b);

#endif // OCTOPUS_QUEUE_H
