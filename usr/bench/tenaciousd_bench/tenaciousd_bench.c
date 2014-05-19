/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <storage/storage.h>
#include <tenaciousd/log.h>

#define ITERATIONS      2
#define VSA_SIZE        (10*1024*1024)          // 10MB

static struct storage_vsa vsa;
static struct storage_vsic vsic;
static uint64_t durations[ITERATIONS][2];

int main(int argc, char *argv[])
{
    // XXX: Need to support multiple backend drivers eventually
    errval_t err = storage_vsic_driver_init(argc, (const char **)argv, &vsic);
    assert(err_is_ok(err));

    err = storage_vsa_alloc(&vsa, VSA_SIZE);
    assert(err_is_ok(err));

    struct tenaciousd_log *log = tenaciousd_log_new(&vsa, &vsic);
    assert(log != NULL);

    size_t size = 32;
    struct tenaciousd_log_entry *entry = tenaciousd_log_entry_new(log, &size);
    assert(entry != NULL);

    char *str = (char *)entry->data;
    strcpy(str, "This is a test...");

    for(int i = 0; i < ITERATIONS; i++) {
        uint64_t start = rdtsc();

        err = tenaciousd_log_append(log, entry);
        assert(err_is_ok(err));

        uint64_t middle = rdtsc();
        durations[i][0] = middle - start;

        err = vsic.ops.wait(&vsic);
        assert(err_is_ok(err));

        uint64_t end = rdtsc();
        durations[i][1] = end - middle;
    }

    err = tenaciousd_log_delete(log);
    assert(err_is_ok(err));

    for(int i = 0; i < ITERATIONS; i++) {
        printf("iteration %d = %" PRIu64 " %" PRIu64 "\n",
               i, durations[i][0], durations[i][1]);
    }

    return 0;
}
