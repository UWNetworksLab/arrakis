/**
 * \file
 * \brief Generic/base microbenchmark code.
 *
 * This file implements some (currently very primitive) services for
 * running and printing the results of a set of microbenchmarks.
 * Most of the benchmarks themselves are defined in the architecture-specific
 * part, in arch_microbenchmarks.c.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <string.h>
#include <microbenchmarks.h>
#include <misc.h>

static uint64_t divide_round(uint64_t quotient, uint64_t divisor)
{
    if ((quotient % divisor) * 2 >= divisor) {
        // round up
        return (quotient / divisor) + 1;
    } else {
        return (quotient / divisor);
    }
}

static int microbench_print(struct microbench *mb, char *buf, size_t len)
{
    return snprintf(buf, len, "%" PRIu64 " ticks",
                    divide_round(mb->result, MICROBENCH_ITERATIONS));
}

static int microbenchmarks_run(struct microbench *benchs, size_t nbenchs)
{
    for (size_t i = 0; i < nbenchs; i++) {
        int                     r;
        struct microbench       *mb;

        mb = &benchs[i];
        printk(LOG_NOTE, "Running benchmark %zu/%zu: %s\n", i + 1, nbenchs,
               mb->name);
        r = mb->run_func(mb);

        if (r != 0) {
            printk(LOG_ERR, "%s: Error %d running %s\n", __func__, r, mb->name);
            return r;
        }
    }

    return 0;
}

static int microbenchmarks_print_all(struct microbench *benchs, size_t nbenchs)
{
    int i, r;
    struct microbench *mb;
    char buf[64];

    for (i = 0; i < nbenchs; i++) {
        mb = &benchs[i];
        r = microbench_print(mb, buf, sizeof(buf) - 1);
        if (r <= 0 || r >= sizeof(buf)) {
            /* FIXME: error handling */
            return -1;
        }
        printf("%40s: %s\n", mb->name, buf);
    }

    return 0;
}

void microbenchmarks_run_all(void)
{
    microbenchmarks_run(arch_benchmarks, arch_benchmarks_size);

    printf("\n------------------------ Statistics ------------------------\n");
    microbenchmarks_print_all(arch_benchmarks, arch_benchmarks_size);
    printf("------------------------------------------------------------\n\n");
}
