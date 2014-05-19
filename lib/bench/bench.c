/**
 * \file
 * \brief Bench library initialization.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <bench/bench_arch.h>

static cycles_t tsc_overhead;

/**
 * \brief Measure overhead of taking timestamp
 */
static void measure_tsc(void)
{
    uint64_t begin;
    uint64_t end;

    begin = bench_tsc();
    for(int i = 0; i < 1000; i++) {
        end = bench_tsc();
    }

    tsc_overhead = (end - begin) / 1000;
}

/**
 * \brief Initialize benchmarking library
 *
 * \bug x86 specific
 */
void bench_init(void)
{
    bench_arch_init();

    /* Measure overhead of taking timestamps */
    measure_tsc();
}

/**
 * Return the measured tsc overhead
 */
cycles_t bench_tscoverhead(void)
{
    return tsc_overhead;
}

/**
 * \brief Compute averages
 *
 * If certain datapoints should be ignored, they should be marked with
 * #BENCH_IGNORE_WATERMARK
 */
cycles_t bench_avg(cycles_t *array, size_t len)
{
    cycles_t sum = 0;
    size_t count = 0;

    // Discarding some initial observations
    for (size_t i = len >> 3; i < len; i++) {
        if (array[i] != BENCH_IGNORE_WATERMARK) {
            sum += array[i];
            count++;
        }
    }

    return sum / count;
}

/**
 * \brief Compute variance
 *
 * If certain datapoints should be ignored, they should be marked with
 * #BENCH_IGNORE_WATERMARK
 */
cycles_t bench_variance(cycles_t *array, size_t len)
{
    cycles_t avg = bench_avg(array, len);

    cycles_t sum = 0;
    size_t count = 0;

    // Discard some initial observations
    for (size_t i = len >> 3; i < len; i++) {
        if (array[i] != BENCH_IGNORE_WATERMARK) {
            sum += array[i] * array[i];
            count++;
        }
    }

    return (sum / count) - (avg * avg);
}

/**
 * \brief Compute minimum
 *
 * If certain datapoints should be ignored, they should be marked with
 * #BENCH_IGNORE_WATERMARK
 */
cycles_t bench_min(cycles_t *array, size_t len)
{
    size_t i = len >> 3;
    cycles_t min = (cycles_t) -1ULL;

    for (; i < len; i++) {
        if (array[i] != BENCH_IGNORE_WATERMARK && array[i] < min) {
            min = array[i];
        }
    }

    return min;
}

/**
 * \brief Compute maximum
 *
 * If certain datapoints should be ignored, they should be marked with
 * #BENCH_IGNORE_WATERMARK
 */
cycles_t bench_max(cycles_t *array, size_t len)
{
    size_t i = len >> 3;
    cycles_t max = 0;

    for (; i < len; i++) {
        if (array[i] != BENCH_IGNORE_WATERMARK && array[i] > max) {
            max = array[i];
        }
    }

    return max;
}
