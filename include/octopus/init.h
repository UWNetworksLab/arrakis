/**
 * \file
 * \brief Header file for the octopus initialization/general functions.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_INIT_H_
#define OCTOPUS_INIT_H_

#include <barrelfish/barrelfish.h>
#include <if/octopus_defs.h>
#include <if/octopus_thc.h>

STATIC_ASSERT(sizeof(uintptr_t) <= sizeof(uint64_t),
        "Sending pointers might fail :-(.");

errval_t oct_init(void);
errval_t oct_thc_init(void);

struct octopus_thc_client_binding_t* oct_get_thc_client(void);
struct octopus_binding* oct_get_event_binding(void);

#endif /* OCTOPUS_INIT_H_ */
