/**
 * \file
 * \brief Architecture specific dispatcher structure private to the user
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_ARM_BARRELFISH_DISPATCHER_H
#define TARGET_ARM_BARRELFISH_DISPATCHER_H

#include <barrelfish_kpi/dispatcher_shared.h>
#include <barrelfish_kpi/dispatcher_shared_arch.h>
#include <barrelfish/dispatcher.h>

/// Dispatcher structure (including data accessed only by user code)
struct dispatcher_arm {
    struct dispatcher_shared_arm d;  ///< Shared (user/kernel) data. Must be first.
    struct dispatcher_generic generic;   ///< User private data
    /* Incoming LMP endpoints (buffers and receive cap pointers) follow */
};

#endif // TARGET_ARM_BARRELFISH_DISPATCHER_H
