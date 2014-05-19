/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>

bench_ctl_t *bench_ctl_init(enum bench_ctl_mode mode,
                            size_t              dimensions,
                            size_t              min_runs)
{
    bench_ctl_t *ctl;

    ctl = calloc(1, sizeof(*ctl));
    ctl->mode = mode;
    ctl->result_dimensions = dimensions;
    ctl->min_runs = min_runs;

    if (mode == BENCH_MODE_FIXEDRUNS) {
        ctl->data = calloc(min_runs * dimensions, sizeof(*ctl->data));
    } else {
        assert(!"NYI");
    }

    return ctl;
}

void bench_ctl_destroy(bench_ctl_t *ctl)
{
    free(ctl->data);
    free(ctl);
}

void bench_ctl_dry_runs(bench_ctl_t *ctl,
                        size_t       dry_runs)
{
    ctl->dry_runs = dry_runs;
}

bool bench_ctl_add_run(bench_ctl_t *ctl,
                       cycles_t* result)
{
    cycles_t *dst;

    if (ctl->result_count == ctl->min_runs) {
        return true;
    }

    dst = ctl->data + ctl->result_count * ctl->result_dimensions;
    memcpy(dst, result, sizeof(dst) * ctl->result_dimensions);

    ctl->result_count++;

    return ctl->result_count == ctl->min_runs;
}

void bench_ctl_dump_csv(bench_ctl_t *ctl,
                        const char  *prefix,
                        uint64_t tscperus)
{
    size_t i, j;
    cycles_t *v;
    size_t dim = ctl->result_dimensions;

    for (i = 0; i < ctl->result_count; i++) {
        printf("%s", prefix);

        v = ctl->data + i * dim;
        for (j = 0; j < dim; j++) {
            printf("%"PRIuCYCLES", %f", v[j], v[j]/(float)tscperus);
            if (j != dim - 1) {
                printf(",");
            }
        }
        printf("\n");
    }
    fflush(stdout);
}


/**
 * Return bin index for this value. We keep two more bins than bin count, one
 * for the values below min (bin_count), and one for those above (bin_count + 1)
 */
static inline size_t val2bin(size_t bin_count, cycles_t min, cycles_t max,
                             cycles_t value)
{
    cycles_t bin_width = (max - min) / bin_count;

    if (value < min) {
        return bin_count;
    } else if (value >= max) {
        return bin_count + 1;
    }

    return (value - min) / bin_width;
}

/** Return the lower value for a bin */
static inline cycles_t bin2val(size_t bin_count, cycles_t min, cycles_t max,
                               size_t idx)
{
    cycles_t bin_width = (max - min) / bin_count;
    return min + idx * bin_width;
}

/**
 * Returns a newly allocated array of bins, filled with the desired values.
 * The array has bin_count + 2 elements. result[bin_count] contains the number
 * of values below the minium, result[bin_count + 1] those above the maximum.
 * The caller is responsible for freeing the array.
 */
static cycles_t *do_bincounting(bench_ctl_t *ctl,
                                size_t dimension,
                                size_t bin_count,
                                cycles_t min,
                                cycles_t max)
{
    cycles_t *bins;
    size_t i;
    cycles_t *v;

    bins = calloc(bin_count + 2, sizeof(size_t));

    for (i = 0; i < ctl->result_count; i++) {
        v = ctl->data + (ctl->result_dimensions * i + dimension);
        bins[val2bin(bin_count, min, max, *v)]++;
    }

    return bins;
}

static uint64_t *do_sorting(bench_ctl_t *ctl,
                                size_t dimension)
{
    size_t i, j;
    size_t len = ctl->result_count;
    uint64_t *sorted_array;
    cycles_t temp_holder;

    // create a sorted array
    sorted_array = calloc(ctl->result_count, sizeof(uint64_t));
    assert(sorted_array != NULL);
    // Copy data into sorted array
    for (i = 0; i < len; i++) {
        sorted_array[i] = *(ctl->data + (ctl->result_dimensions * i
                    + dimension));
    }

    // sort the array
    for (i = 0; i < len; ++i) {
        for (j = i; j < len; ++j) {
            if (sorted_array[i] > sorted_array[j]) {
                temp_holder = sorted_array[i];
                sorted_array[i] = sorted_array[j];
                sorted_array[j] = temp_holder;
            }
        } // end for: j
    } // end for: i
    return sorted_array;
} // end function: do_sorting

void bench_ctl_dump_analysis(bench_ctl_t *ctl,
                                    size_t dimension,
                                    const char *prefix,
                                    cycles_t tscperus)
{

    uint64_t *final_array = do_sorting(ctl, dimension);
    size_t len = ctl->result_count;
    size_t max99 = (size_t)((0.99 * len) + 0.5);
    printf("run [%"PRIu64"], med_pos[%"PRIu64"], min_pos[%"PRIu64"], "
           "P99[%"PRIu64"], max[%"PRIu64"]\n",
        (uint64_t)len,
        (uint64_t)(len/2),
        (uint64_t)0,
        (uint64_t)(max99-1),
        (uint64_t)(len-1));

    printf("run [%"PRIu64"], med[%"PRIu64"], min[%"PRIu64"], "
           "P99[%"PRIu64"], max[%"PRIu64"]\n",
        (uint64_t)len,
        (uint64_t)final_array[len/2],
        (uint64_t)final_array[0],
        (uint64_t)final_array[max99-1],
        (uint64_t)final_array[len-1]);

    printf("run [%"PRIu64"], med[%f], min[%f], "
           "P99[%f], max[%f]\n",
        (uint64_t)len,
        (final_array[len/2]/(float)tscperus),
        (final_array[0]/(float)tscperus),
        (final_array[max99-1]/(float)tscperus),
        (final_array[len-1]/(float)tscperus));

    printf("%s, %"PRIu64" %f %f %f %f\n",
            prefix,
            (uint64_t)len,
            (final_array[len/2]/(float)tscperus),
            (final_array[0]/(float)tscperus),
            (final_array[max99-1]/(float)tscperus),
            (final_array[len-1]/(float)tscperus));

} // end function: bench_ctl_dump_analysis


void bench_ctl_dump_csv_bincounting(bench_ctl_t *ctl,
                                    size_t dimension,
                                    size_t bin_count,
                                    cycles_t min,
                                    cycles_t max,
                                    const char *prefix,
                                    cycles_t tscperus)
{
    cycles_t *bins;
    size_t i;
    cycles_t val;

    bins = do_bincounting(ctl, dimension, bin_count, min, max);

    printf("%sbellow,%"PRIuCYCLES"\n", prefix, bins[bin_count]);
    printf("%sabove,%"PRIuCYCLES"\n", prefix, bins[bin_count+1]);
    for (i = 0; i < bin_count; i++) {
        if (bins[i] > 0) {
            val = bin2val(bin_count, min, max, i);
            printf("%s%"PRIuCYCLES",%"PRIuCYCLES", %f\n", prefix, val, bins[i],
                   val/ (float)tscperus);
        }
    }

    free(bins);
}

