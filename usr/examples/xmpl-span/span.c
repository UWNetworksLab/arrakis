/** \file
 *  \brief Example spaning application
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
#include <string.h>

#include <barrelfish/barrelfish.h>

int global;

static int do_hello(void *arg)
{
    debug_printf("Hello! the global is: %x\n", global);
    return EXIT_SUCCESS;
}

bool all_spanned = false;
int num_span = -1;

static void span_cb(void *arg, errval_t err)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "span failed");
        return;
    }

    static int num_spanned = 0;

    num_spanned++;
    if (num_spanned >= num_span) {
        all_spanned = true;
    }

}

int main(int argc, char *argv[])
{
    errval_t err;
    coreid_t mycore = disp_get_core_id();

    if (argc == 2) {
        num_span = atoi(argv[1]);

        debug_printf("spanning on %d cores\n", num_span);

        for (int i = 1; i <= num_span; i++) {

            err = domain_new_dispatcher(mycore + i, span_cb, NULL);
            
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "failed span %d", i);
            } 
        }
    } else {
        debug_printf("usage %s num_span\n", argv[0]);
        return EXIT_FAILURE;
    }

    while (!all_spanned) {
        thread_yield();
    }

    global = 0xC007;

    for(int i = 1; i <= num_span; i++) {
        debug_printf("starting thread on %d\n", mycore + i); 

        err = domain_thread_create_on(mycore + i, do_hello, NULL);

        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed thread create %d", i);
        }
    }

    debug_printf("Finished\n");
}
