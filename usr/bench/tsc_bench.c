/**
 * \file
 * \brief Microbenchmark to report the cost
 * of reading from the timestamp counter.
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
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>

struct timestamp {
    uint64_t time0;
    uint64_t time1;
};

#define MAX_COUNT 1000

static void tsc_bench(void)
{
    bench_init();

    struct timestamp *timestamp = malloc(sizeof(struct timestamp) * MAX_COUNT);
    assert(timestamp);

    timestamp[0].time0 = bench_tsc();
    for (int i = 0; i < MAX_COUNT - 1; i++) {
        timestamp[i].time1 = timestamp[i + 1].time0 = bench_tsc();
    }
    timestamp[MAX_COUNT - 1].time1 = bench_tsc();

    for (int i = 0; i < MAX_COUNT - 1; i++) {
        printf("Iteration %d: time0 %ld time1 %ld difference %ld\n",
               i, timestamp[i].time0, timestamp[i].time1,
               timestamp[i].time1 - timestamp[i].time0);
    }
}

#if 0
static void counter_reply(struct hpet_client_response *st, struct capref cap,
                          uint64_t index)
{
    errval_t err;
    void* buf;
    err = vspace_map_one_frame_attr(&buf, BASE_PAGE_SIZE, cap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame_attr failed");
        printf("vspace_map_one_frame_attr failed in counter_reply\n");
    }

    bench_init();
}

static void _connected(struct hpet_client_response *st)
{
    errval_t err = st->call_vtbl->counter_request(st);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to send counter request");
        printf("Failed to send counter request\n");
        exit(EXIT_FAILURE);
    }
}

static void hpet_bench(uint64_t iterations)
{
    errval_t err;

    /* Connect to HPET driver */
    iref_t iref;
    err = chips_blocking_lookup(chips_get_context(), "hpet", &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to lookup server");
        exit(EXIT_FAILURE);
    }
    assert(iref != 0);

    static struct hpet_client_response_vtbl crv = {
        ._disconnect = NULL,
        ._connected  = _connected,
        .counter_reply = counter_reply,
    };
    static struct hpet_client_response hcr = {
        .f = &crv,
    };

    hpet_connect(iref, &hcr, 0);
}
#endif

int main(int argc, char *argv[])
{
    tsc_bench();
    printf("client done\n");
    return 0;
}
