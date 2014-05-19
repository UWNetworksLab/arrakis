/**
 * \file
 * \brief FPU context switch test program.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>

static const char *progname = NULL;

static int fpu_thread(void *arg)
{
    double save = 0.0, lastsave = 0.0;
    int n = (uintptr_t)arg;
    int j = 0;

    for(uint64_t i = 0;; i++) {
        save = sin(save + 0.1) + 0.1;
        // printf("%s(%d): %g\n", progname, n, save);
        double test = sin(lastsave + 0.1) + 0.1;
        if(save != test) { // if (fabs(save - test) > 0.00000001)
            printf("ERROR %s(%d): %.15g != %.15g at iteration %" PRIu64 "\n",
                   progname, n, save, test, i);
            abort();
        }
        lastsave = save;

        if(i % 50000000 == 0) {
            printf("%s(%d): iteration %" PRIu64 "\n", progname, n, i);
            j++;
            if(j == 3) {
                printf("fputest passed successfully!\n");
                thread_exit();
            }
        }
    }
}

int main(int argc, char *argv[])
{
    if(argc != 3) {
        printf("Usage: %s <identifier> <number of threads>\n", argv[0]);
        exit(1);
    }

    int nthreads = atoi(argv[2]);
    progname = argv[1];

    for(int i = 0; i < nthreads; i++) {
        thread_create(fpu_thread, (void *)(uintptr_t)i);
    }

    thread_exit();
    return 0;
}
