#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/types.h>
#include <barrelfish/cap_predicates.h>
#include <bench/bench.h>

#include "mdb_bench.h"

static cycles_t measure_insert_one(struct cte *ctes, size_t count)
{
    // insert all except first
    for (int i = 1; i < count; i++) {
        INS(&ctes[i]);
    }

    __asm volatile ("" : : : "memory");

    // measure insert time for first
    cycles_t begin = bench_tsc();
    INS(&ctes[0]);
    cycles_t end = bench_tsc();

    return end - begin;
}

static cycles_t measure_remove_one(struct cte *ctes, size_t count)
{
    // insert all except first
    for (int i = 0; i < count; i++) {
        INS(&ctes[i]);
    }

    __asm volatile ("" : : : "memory");

    // measure insert time for first
    cycles_t begin = bench_tsc();
    REM(&ctes[0]);
    cycles_t end = bench_tsc();

    return end - begin;
}

static cycles_t measure_iterate_n(struct cte *ctes, size_t count, size_t steps)
{
    // insert all caps
    for (int i = 0; i < count; i++) {
        INS(&ctes[i]);
    }

    // randomly select start cap
    size_t startpos, mod = 1;
    while (mod < count) { mod <<= 1; }
    do {
        // assuming count is power-of-two
        startpos = rand() % mod;
    } while (startpos >= count);
    struct cte *cte = &ctes[startpos];

    __asm volatile ("" : : : "memory");

    // measure iteration speed
    cycles_t begin = bench_tsc();
    for (int i = 0; i < steps; i++) {
        cte = NEXT(cte);
        if (!cte) {
            return 0;
        }
    }
    cycles_t end = bench_tsc();

    return end - begin;
}

static cycles_t measure_iterate_1(struct cte *ctes, size_t count)
{
    return measure_iterate_n(ctes, count, 1);
}
static cycles_t measure_iterate_10(struct cte *ctes, size_t count)
{
    return measure_iterate_n(ctes, count, 10);
}
static cycles_t measure_iterate_100(struct cte *ctes, size_t count)
{
    return measure_iterate_n(ctes, count, 100);
}
static cycles_t measure_iterate_1000(struct cte *ctes, size_t count)
{
    return measure_iterate_n(ctes, count, 1000);
}

static cycles_t measure_has_copies(struct cte *ctes, size_t count)
{
    for (int i = 0; i < count; i++) {
        INS(&ctes[i]);
    }

    // randomly select start cap
    size_t pos, mod = 1;
    while (mod < count) { mod <<= 1; }
    do {
        // assuming count is power-of-two
        pos = rand() % mod;
    } while (pos >= count);
    struct cte *cte = &ctes[pos];

    __asm volatile ("" : : : "memory");

    cycles_t begin = bench_tsc();
    HASCOP(cte);
    cycles_t end = bench_tsc();

    return end - begin;
}

static cycles_t measure_has_ancestors(struct cte *ctes, size_t count)
{
    for (int i = 0; i < count; i++) {
        INS(&ctes[i]);
    }

    // randomly select start cap
    size_t pos, mod = 1;
    while (mod < count) { mod <<= 1; }
    do {
        // assuming count is power-of-two
        pos = rand() % mod;
    } while (pos >= count);
    struct cte *cte = &ctes[pos];

    __asm volatile ("" : : : "memory");

    cycles_t begin = bench_tsc();
    HASANC(cte);
    cycles_t end = bench_tsc();

    return end - begin;
}

static cycles_t measure_has_descendants(struct cte *ctes, size_t count)
{
    for (int i = 0; i < count; i++) {
        INS(&ctes[i]);
    }

    // randomly select start cap
    size_t pos, mod = 1;
    while (mod < count) { mod <<= 1; }
    do {
        // assuming count is power-of-two
        pos = rand() % mod;
    } while (pos >= count);
    struct cte *cte = &ctes[pos];

    __asm volatile ("" : : : "memory");

    cycles_t begin = bench_tsc();
    HASDESC(cte);
    cycles_t end = bench_tsc();

    return end - begin;
}

#ifndef OLD_MDB
static cycles_t measure_query_address(struct cte *ctes, size_t count)
{
    for (int i = 0; i < count; i++) {
        INS(&ctes[i]);
    }

    // randomly select a cap from which to take the address
    size_t pos, mod = 1;
    while (mod < count) { mod <<= 1; }
    do {
        // assuming count is power-of-two
        pos = rand() % mod;
    } while (pos >= count);
    genpaddr_t base = ctes[pos].cap.u.ram.base;
    struct cte *result;

    __asm volatile ("" : : : "memory");

    cycles_t begin = bench_tsc();
    mdb_find_cap_for_address(base, &result);
    cycles_t end = bench_tsc();

    return end - begin;
}
#endif

struct measure_opt measure_opts[] = {
    { "insert_one", measure_insert_one, },
    { "remove_one", measure_remove_one, },
    { "iterate_1", measure_iterate_1, },
    { "iterate_10", measure_iterate_10, },
    { "iterate_100", measure_iterate_100, },
    { "iterate_1000", measure_iterate_1000, },
    { "has_copies", measure_has_copies, },
    { "has_ancestors", measure_has_ancestors, },
    { "has_descendants", measure_has_descendants, },
#ifndef OLD_MDB
    { "query_address", measure_query_address, },
#endif
    { NULL, NULL, },
};
