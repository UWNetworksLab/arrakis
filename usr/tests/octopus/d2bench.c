/**
 * \file
 * \brief Simple Barrier test
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <octopus/octopus.h>
#include <skb/skb.h>

#define MAX_ITERATIONS 500
struct timestamp {
    cycles_t time0;
    cycles_t time1;
    cycles_t server;
    uint8_t busy;
};
struct timestamp timestamps[MAX_ITERATIONS] = { { 0, 0, 0, 0 } };
//static size_t records[] = { 0, 8, 16, 256, 512, 768, 1000, 1500, 2000, 2500,
//        4000, 5000, 6000, 7000, 8000, 9000, 10000, 12000, 20000 };

static size_t records[] = { 0, 1, 50000, 100000, 150000, 200000, 400000, 600000, 800000, 1000000, 1200000,
1400000 };

static size_t add_records[] = { 0, 1, 50000, 100000, 150000, 200000, 400000, 600000, 800000, 1000000, 1200000,
1400000 };

static void variable_records(void)
{
    size_t exps = sizeof(records) / sizeof(size_t);
    for (size_t i = 1; i < exps; i++) {
        printf("# Run experiment with %lu records:\n", records[i]);

        for (size_t j = records[i - 1]; j < records[i]; j++) {
            errval_t err = oct_set("object%lu { attr: 'object%lu' }", j, j);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "set");
                exit(0);
            }
        }

        errval_t error_code;
        char* data = NULL;

        struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
        assert(cl != NULL);
        octopus_trigger_id_t tid;


        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            size_t get_nr = bench_tsc() % records[i];
            char buf[100];
            sprintf(buf, "object%lu", get_nr);

            timestamps[k].time0 = bench_tsc();
            cl->call_seq.get(cl, buf, NOP_TRIGGER, &data, &tid, &error_code);
            timestamps[k].time1 = bench_tsc();
            if (err_is_fail(error_code)) {
                DEBUG_ERR(error_code, "get");
                exit(0);
            }
            free(data);
        }

        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            printf(
                    "%lu %"PRIuCYCLES" %"PRIuCYCLES" %d %lu\n",
                    k,
                    timestamps[k].time1 - timestamps[k].time0
                            - bench_tscoverhead(), timestamps[k].server,
                    timestamps[k].busy, records[i]);
        }

    }
}

static void add_record(void) {
    size_t exps = sizeof(add_records) / sizeof(size_t);
    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    assert(cl != NULL);

    errval_t error_code;
    char* ret = NULL;
    char* record = "rec_ { attribute: 1 }";
    octopus_trigger_id_t tid;

    for (size_t i = 1; i < exps; i++) {
        printf("# Run add_record with %lu records:\n", add_records[i]);

        for (size_t j = add_records[i - 1]; j < add_records[i]; j++) {
            //printf("add to system: %s\n", record);
            cl->call_seq.set(cl, record, SET_SEQUENTIAL, NOP_TRIGGER, false, &ret, &tid, &error_code);
            assert(ret == NULL);
            if(err_is_fail(error_code)) { DEBUG_ERR(error_code, "add"); exit(0); }
        }

        char to_add[100];
        sprintf(to_add, "zzz { attribute: 1 }");
        char* data;
        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            timestamps[k].time0 = bench_tsc();
            cl->call_seq.set(cl, to_add, SET_DEFAULT, NOP_TRIGGER, false, &data, &tid, &error_code);
            timestamps[k].time1 = bench_tsc();
            if (err_is_fail(error_code)) {
                DEBUG_ERR(error_code, "set");
                exit(0);
            }
            free(data);

            cl->call_seq.del(cl, to_add, NOP_TRIGGER, &tid, &error_code);
            if (err_is_fail(error_code)) {
                DEBUG_ERR(error_code, "del");
                exit(0);
            }
            if (timestamps[k].busy == 1) k--; // ignore the ones that were busy on send
        }

        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            printf(
                    "%lu %"PRIuCYCLES" %"PRIuCYCLES" %d %lu\n",
                    k,
                    timestamps[k].time1 - timestamps[k].time0
                    - bench_tscoverhead(), timestamps[k].server,
                    timestamps[k].busy, add_records[i]);
        }
    }

}

static void one_record(void)
{
    errval_t err = oct_set("object0");
    assert(err_is_ok(err));

    errval_t error_code;
    char* data = NULL;

    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    octopus_trigger_id_t tid;

    for (size_t i = 0; i < MAX_ITERATIONS; i++) {

        timestamps[i].time0 = bench_tsc();
        cl->call_seq.get(cl, "object0", NOP_TRIGGER, &data, &tid, &error_code);
        timestamps[i].time1 = bench_tsc();

        assert(err_is_ok(error_code));
        free(data);
        for(size_t j=0; j<1<<20; j++) {}
    }

    for (size_t i = 0; i < MAX_ITERATIONS; i++) {
        printf("%lu %"PRIuCYCLES" %"PRIuCYCLES" %d\n", i,
                timestamps[i].time1 - timestamps[i].time0 - bench_tscoverhead(),
                timestamps[i].server, timestamps[i].busy);
    }
}

static void unnamed_record(void)
{
    errval_t err = oct_set("object0 { attr1: 'bla', attr2: 12.0 }");
    assert(err_is_ok(err));

    octopus_trigger_id_t tid;

    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    assert(cl != NULL);

    char* data = NULL;
    errval_t error_code;
    for (size_t i = 0; i < MAX_ITERATIONS; i++) {

        timestamps[i].time0 = bench_tsc();
        cl->call_seq.get(cl, "_ { attr1: 'bla', attr2: 12.0 }", NOP_TRIGGER, &data, &tid, &error_code);
        timestamps[i].time1 = bench_tsc();

        assert(err_is_ok(error_code));
        free(data);

        for(size_t j=0; j<1<<20; j++) {}
    }

    for (size_t i = 0; i < MAX_ITERATIONS; i++) {
        printf("%lu %"PRIuCYCLES" %"PRIuCYCLES" %d\n", i,
                timestamps[i].time1 - timestamps[i].time0 - bench_tscoverhead(),
                timestamps[i].server, timestamps[i].busy);
    }
}

int main(int argc, char** argv)
{
    bench_init();
    oct_init();

    if (0) one_record();
    if (0) variable_records();
    if (0) add_record();
    if (0) unnamed_record();

    /*
    debug_printf("1000000000 cycles in ms: %lu\n", bench_tsc_to_ms(1000000000));
    debug_printf(" 100000000 cycles in ms: %lu\n", bench_tsc_to_ms( 100000000));
    debug_printf("  10000000 cycles in ms: %lu\n", bench_tsc_to_ms(  10000000));
    debug_printf("   3333333 cycles in ms: %lu\n", bench_tsc_to_ms(   3333333));
    debug_printf("   2800000 cycles in ms: %lu\n", bench_tsc_to_ms(   2800000));
    debug_printf("   2700000 cycles in ms: %lu\n", bench_tsc_to_ms(   2700000));*/

}
