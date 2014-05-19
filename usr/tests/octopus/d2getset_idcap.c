/**
 * \file
 * \brief Tests for octopus get_with_idcap/set_with_idcap API
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <octopus/octopus.h>

#include "common.h"

struct capref idcap1, idcap2;

static void set_records(void)
{
    errval_t err;

    err = idcap_alloc(&idcap1);
    ASSERT_ERR_OK(err);

    err = idcap_alloc(&idcap2);
    ASSERT_ERR_OK(err);

    err = oct_set_with_idcap(idcap1, "{session_iref: %d, io_iref: %d}", 1, 2);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error installing first record.");
    }
    ASSERT_ERR_OK(err);

    err = oct_set_with_idcap(idcap2, "{session_iref: %d, io_iref: %d}", 3, 4);
    ASSERT_ERR_OK(err);
}

static void get_records(void)
{
    errval_t err;

    char *data = NULL;

    err = oct_get_with_idcap(&data, idcap1);
    printf("idcap1-record: %s\n", data);

    err = oct_get_with_idcap(&data, idcap2);
    printf("idcap2-record: %s\n", data);
}

int main(int argc, char *argv[])
{
    oct_init();

    set_records();
    get_records();

    printf("d2getset SUCESS!\n");
    return EXIT_SUCCESS;
}
