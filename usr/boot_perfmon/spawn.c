/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich and Mircosoft Corporation.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "internal.h"
#include <barrelfish/spawn_client.h>

errval_t spawn(callback cb)
{
    errval_t err;

    if (check_leader()) { // Only leader spawns other nodes
        char id[128];
        snprintf(id, 128, "%d", my_core_id);
        char *xargv[] = {global_argv[0], id, NULL};
        err = spawn_program_on_all_cores(false, xargv[0], xargv, NULL,
                                         SPAWN_FLAGS_DEFAULT, NULL);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "spawn_program_on_all_cores failed");
        }
    }

    cb();
    return SYS_ERR_OK;
}
