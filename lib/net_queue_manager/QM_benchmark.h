/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef Queue_Manager_benchmark_H_
#define Queue_Manager_benchmark_H_
#include <barrelfish/barrelfish.h>
#include <net_queue_manager/net_queue_manager.h>
#include <contmng/netbench.h>
#include <stdio.h>
#include <string.h>
#include "queue_manager_debug.h"
#include "queue_manager_local.h"

void benchmark_control_request(struct net_queue_manager_binding *cc,
        uint64_t queueid, uint8_t state, uint64_t trigger, uint64_t cl_data);

void reset_client_closure_stat(struct client_closure *cc);

#endif // Queue_Manager_benchmark_H_
