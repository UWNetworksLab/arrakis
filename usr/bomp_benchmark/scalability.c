/**
 * \file
 * \brief libbomp test.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <omp.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#ifdef POSIX
static inline uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}
#endif

#define N 10000000

int main(int argc, char *argv[])
{
        uint64_t begin, end;
        int i;
        static int a[N];

#ifndef POSIX
	bomp_custom_init();
#endif
        assert(argc == 2);
        omp_set_num_threads(atoi(argv[1]));

        for (i=0;i<N;i++) a[i]= 2*i;

        begin = rdtsc();

#pragma omp parallel for
        for (i=0;i<N;i++) a[i]= 2*i;

        end = rdtsc();

	printf("Value of sum is %d, time taken %lu\n", 0, end - begin);
}
