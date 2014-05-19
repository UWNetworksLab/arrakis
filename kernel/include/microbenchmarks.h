/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __MICROBENCHMARKS_H
#define __MICROBENCHMARKS_H

// The number of times the benchmark should run each instruction
#define MICROBENCH_ITERATIONS 64

struct microbench; // forward declaration

/* function that executes a particular microbenchmark, storing its result
 * return value should be zero on success
 */
typedef int (* microbench_run_func)(struct microbench *);

struct microbench {
    const char * NTS name;
    microbench_run_func run_func;
    uint64_t result;
};

void microbenchmarks_run_all(void);

extern struct microbench arch_benchmarks[];
extern size_t arch_benchmarks_size;

#endif //__MICROBENCHMARKS_H
