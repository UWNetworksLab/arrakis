/**
 * \file
 * \brief Handler functions for incoming messages.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef OCT_HANDLER_H_
#define OCT_HANDLER_H_

#include <if/octopus_defs.h>
#include <octopus/trigger.h>
#include <octopus/pubsub.h>

void trigger_handler(struct octopus_binding*, octopus_trigger_id_t,
        uint64_t, octopus_mode_t, char*, uint64_t);
void subscription_handler(struct octopus_binding*, subscription_t,
        uint64_t, octopus_mode_t, char*, uint64_t);

#endif /* OCT_HANDLER_H_ */
