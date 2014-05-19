/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>

static int yield_thread(void *dummy)
{
    for(;;) {
        thread_yield();
    }
    return -1;
}

static int worker_thread(void *dummy)
{
    for(;;);
    return -1;
}

int main(int argc, char *argv[])
{
  //    assert(argc == 3);

  int yielders = 1; //atoi(argv[1]);
  int workers = 0; //atoi(argv[2]);

    printf("Yield test. Yielders: %d, workers: %d\n", yielders, workers);

    for(int i = 0; i < yielders; i++) {
        struct thread *t = thread_create(yield_thread, NULL);
        errval_t err = thread_detach(t);
        assert(err_is_ok(err));
    }

    for(int i = 0; i < workers; i++) {
        struct thread *t = thread_create(worker_thread, NULL);
        errval_t err = thread_detach(t);
        assert(err_is_ok(err));
    }

    thread_exit();
}
