/**
 * \file
 * \brief test of barrier functionality from libdist
 */

/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>

#include <dist/barrier.h>

#include "disttest.h"

// TODO: more extensive tests

void barrier_test(void)
{
    errval_t err;

    err = nsb_register("barrier_test");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nsb_register failed");
    }

    err = nsb_wait("barrier_test");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nsb_wait failed");
    }

}
