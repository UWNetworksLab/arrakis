/**
 * \file
 * \brief memory integrity test.
 */

/*
 * Copyright (c) 2007, 2008, 2011, ETH Zurich.
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

#define TRIES           1000
#define CHUNKSIZE       100

static void *ptrs[TRIES];

static void write_pattern(void *addr)
{
    char *p = (char *)addr;

    for(size_t i = 0; i < CHUNKSIZE; i++) {
        p[i] = i;
    }
}

static void check_pattern(void *addr)
{
    char *p = (char *)addr;

    for(size_t i = 0; i < CHUNKSIZE; i++) {
        assert(p[i] == i);
    }
}

int main(int argc, char *argv[])
{
    size_t i;

    for(i = 0; i < TRIES; i++) {
        ptrs[i] = malloc(CHUNKSIZE);
        write_pattern(ptrs[i]);
    }


    for(i = 0; i < TRIES; i++) {
        check_pattern(ptrs[i]);
        free(ptrs[i]);
    }

    for(i = 0; i < TRIES; i++) {
        free(malloc(CHUNKSIZE));
    }

    printf("memtest passed successfully! on core %d\n", disp_get_core_id());
    return EXIT_SUCCESS;
}
