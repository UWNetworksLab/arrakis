/**
 * \file
 * \brief Simple test that locks/unlocks a couple of times
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

#define LOCKIT_CALLS 100000

//static struct periodic_event lock_timer;
static bool locked = false;
char* lock = NULL;

static void lockit(void* arg)
{
    errval_t err = SYS_ERR_OK;

    if (!locked) {
        err = oct_lock("lock_test", &lock);
        DEBUG_ERR(err, "lock");
        assert(err_is_ok(err));
        locked = true;
    } else {
        err = oct_unlock(lock);
        DEBUG_ERR(err, "unlock");
        assert(err_is_ok(err));
        locked = false;
        free(lock);
    }
}

int main(int argc, char *argv[])
{
    oct_init();

    /*
     debug_printf("create periodic event...\n");
     err = periodic_event_create(&lock_timer, get_default_waitset(),
     (300 * 1000),
     MKCLOSURE(lockit, &lock_timer));
     assert(err_is_ok(err));
     */

    for (size_t i = 0; i < LOCKIT_CALLS; i++) {
        lockit(NULL);
    }
    printf("done\n");
    return EXIT_SUCCESS;
}
