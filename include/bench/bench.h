/**
 * \file
 * \brief Bench library.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BENCH_H
#define BENCH_H

#include <barrelfish/types.h> // for cycles_t
#include <bench/bench_arch.h>
#include <sys/cdefs.h>

#define BENCH_IGNORE_WATERMARK 0XDEADBEEF

__BEGIN_DECLS
void bench_init(void);
cycles_t bench_avg(cycles_t *array, size_t len);
cycles_t bench_variance(cycles_t *array, size_t len);
cycles_t bench_min(cycles_t *array, size_t len);
cycles_t bench_max(cycles_t *array, size_t len);
cycles_t bench_tscoverhead(void);
__END_DECLS


/*
 * Control functions for benchmarks
 */

enum bench_ctl_mode {
    // Fixed number of runs (exactly min_runs)
    BENCH_MODE_FIXEDRUNS,
};

struct bench_ctl {
    enum bench_ctl_mode mode;
    size_t              result_dimensions;

    size_t              min_runs;
    size_t              dry_runs;

    size_t              result_count;
    cycles_t           *data;
};

typedef struct bench_ctl bench_ctl_t;


/**
 * Initialize a benchmark control instance.
 *
 * @param mode       Mode of the benchmark (enum bench_ctl_mode)
 * @param dimensions Number of values each run produces
 * @param min_runs   Minimal number of runs to be executed
 *
 * @return Control handle, to be passed on subsequent calls to bench_ctl_
 *         functions.
 */
bench_ctl_t *bench_ctl_init(enum bench_ctl_mode mode,
                            size_t              dimensions,
                            size_t              min_runs);

/**
 * Frees all resources associated with this benchmark control instance. Should
 * be called after the benchmark is done and the results are dumped.
 *
 * @param ctl Control handle
 */
void bench_ctl_destroy(bench_ctl_t *ctl);

/**
 * Add a fixed number of dry runs whose results should not be recorded. Should
 * be called before any calls to bench_ctl_add_run().
 *
 * @param ctl      Control handle
 * @param dry_runs Number of dry runs
 */
void bench_ctl_dry_runs(bench_ctl_t *ctl,
                        size_t       dry_runs);

/**
 * Add results from one run of the benchmark.
 *
 * @param ctl    Control handle
 * @param result Pointer to the 'dimensions' values of this run
 *
 * @return true if this was the last run necessary, false if more runs are
 *         needed.
 */
bool bench_ctl_add_run(bench_ctl_t *ctl,
                       cycles_t* result);

/**
 * Dump results of the benchmark to the standard output. One line per run, the
 * lines will be prefixed with prefix, the values separeted using commas.
 *
 * @param ctl    Control handle
 * @param prefix String to be printed before each line
 */
void bench_ctl_dump_csv(bench_ctl_t *ctl,
                        const char  *prefix, uint64_t tscperus);
/**
 * Use bincounting to reduce the amount of data that is printed.
 * One line per bin (bin_count + 2 for those below and above the specified
 * range) with two columns each is emited at most. Lines for empty bins are
 * omitted except for above and below. The first column contains the start
 * of the range for this bin (or below/above for the border cases), while the
 * second column contains the number of values in that bin.
 *
 * @param ctl       Control handle
 * @param dimenison Index of the value in the run
 * @param bin_count Number of bins to create
 * @param min       Minimum of range to be represented by bins
 * @param max       Maximum of range to be represented by bins
 * @param prefix    String to be printed before each line
 * @param tscperus  cpu ticks per micro-second
 */
void bench_ctl_dump_csv_bincounting(bench_ctl_t *ctl,
                                    size_t dimension,
                                    size_t bin_count,
                                    cycles_t min,
                                    cycles_t max,
                                    const char *prefix,
                                    cycles_t tscperus);


void bench_ctl_dump_analysis(bench_ctl_t *ctl,
                                    size_t dimension,
                                    const char *prefix,
                                    cycles_t tscperus);

#endif // BENCH_H
