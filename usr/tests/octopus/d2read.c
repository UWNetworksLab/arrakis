/**
 * \file
 * \brief Tests oct_read call
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>

#include <skb/skb.h>
#include <octopus/octopus.h>

#include "common.h"

int main(int argc, char *argv[])
{
    errval_t err = SYS_ERR_OK;
    oct_init();

    char* name = NULL;
    char* attr1 = NULL;
    double d;
    uint64_t i;

    err = oct_read("record", "%s", &name);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(name, "record");
    free(name);

    err = oct_read("record {}", "%s {}", &name);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(name, "record");
    free(name);

    err = oct_read("record { attr1: 'Test String 123' }", "%s { attr1: %s }", &name, &attr1);
    ASSERT_STRING(name, "record");
    ASSERT_STRING(attr1, "Test String 123");
    ASSERT_ERR_OK(err);
    free(name);
    free(attr1);

    err = oct_read("record2 { str: 'string', float: 12.0, integer: 1212}",
    		"_ { float: %f, str: %s, integer: %d }", &d, &attr1, &i);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(attr1, "string");
    assert(d == 12.0);
    assert(i == 1212);

    return EXIT_SUCCESS;
}
