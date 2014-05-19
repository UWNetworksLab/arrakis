/**
 * \file
 * \brief start for libdist tests
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
#include <stdlib.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>

#include "disttest.h"


int main(int argc, char *argv[])
{
    barrier_test();

    printf("disttest passed successfully! on core %d\n", disp_get_core_id());
    return EXIT_SUCCESS;
}
