/** \file
 *  \brief Example application using threads
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include <barrelfish/barrelfish.h>

#include <barrelfish/threads.h>

#define USEJOIN

// the code that each thread runs
static int my_thread(void *data)
{
    printf("this is my_thread saying hello\n");
    return 1;
}

int main(int argc, char *argv[])
{
#ifdef USEJOIN
    errval_t err;
    int ret;
#endif

    debug_printf("starting thread to run %p\n", my_thread);
    struct thread *t_id = thread_create(my_thread, NULL); 
    if (t_id == NULL) {
        debug_printf("ERROR: starting thread\n");
    }

#ifdef USEJOIN
    err = thread_join(t_id, &ret);
    if (err_is_ok(err)) {
        debug_printf("joined thread, return value: %d\n", ret);
    } else {
        DEBUG_ERR(err, "in thread_join");
    }
#else
    while (1) {
        thread_yield();
    }
#endif

    return EXIT_SUCCESS;
}

