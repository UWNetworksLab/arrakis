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

static bool leader;
static coreid_t leader_id;

bool check_leader(void)
{
    return leader;
}

coreid_t get_leader_id(void)
{
    return leader_id;
}

errval_t set_leader(callback cb)
{
    if (global_argc == 1) {
        leader = true;
        leader_id = my_core_id;
    } else {
        leader = false;
        leader_id = strtol(global_argv[1], NULL, 10);
    }

    cb();
    return SYS_ERR_OK;
}
