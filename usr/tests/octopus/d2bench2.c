/**
 * \file
 * \brief Benchmarking random workload for get/set queries with no
 * specified record name.
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
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <octopus/octopus.h>
#include <skb/skb.h>

#define MAX_ITERATIONS 500
struct timestamp {
    cycles_t time0;
    cycles_t time1;
    uint8_t busy;
    size_t count;
};
struct timestamp timestamps[MAX_ITERATIONS] = {{ 0, 0, 0, 0 }};
static size_t records[] = { 0, 8, 16, 256, 512, 768, 1000, 1500, 2000, 2500,
        4000, 5000, 6000, 7000, 8000, 9000, 10000, 12000, 20000  };

static inline uint64_t get_cycle_counter(void)
{
#ifdef __x86__
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
#else
    return 0;
#endif
}

static uint32_t my_random(void)
{
    static bool seeded = false;
    static uint32_t m_z = 0xdeadbeef;
    static uint32_t m_w = 0;
    if (!seeded) {
        m_w = (uint32_t) get_cycle_counter();
        seeded = true;
    }

    m_z = 36969 * (m_z & 65535) + (m_z >> 16);
    m_w = 18000 * (m_w & 65535) + (m_w >> 16);

    return (m_z << 16) + m_w;  // 32-bit result
}

static char* attrs[] = {
        "weight",
        "value",
        "attr",
        "bench",
        "rand",
        "type",
        "lock",
        "barrier",
        "lock",
        "time",
        "range",
        "irq",
        "port",
        "iref",
        "description",
/*        "portrange",
        "from",
        "to",
        "version",
        "owner",*/
};

static char* values[] = {
        "'test description'",
        "'value'",
        "'name'",
        "'1500-12333'",
        "'x == 2'",
        "11.0",
        "21.0",
        "1.12312",
        "2.34221",
        "9.123",
        "12",
        "849456",
        "1235",
        "1111",
        "2937",
};

static char buf[255];
static void construct_record(size_t attributes) {
    static char* name = "record_";

    int pos = 0;
    pos += sprintf(buf+pos, "%s {", name);
    uint64_t map = 0; // ensure attributes are unique

    for(size_t i=0; i<attributes;) {

        size_t idx = my_random() % (sizeof(attrs) / sizeof(char*));
        if ( (map & (1 << idx)) == 0) {
            if(i > 0) {
                pos += sprintf(buf+pos, ", ");
            }
            map |= 1 << idx;
            pos += sprintf(buf+pos, "%s: ", attrs[idx]);

            idx = my_random() % (sizeof(values) / sizeof(char*));
            pos += sprintf(buf+pos, "%s", values[idx]);
            
            i++;
        }

    }

    pos += sprintf(buf+pos, "}");

}

static void no_name_get_worstcase(void)
{
    // reference record to measure retrieve time
    errval_t err = oct_set("zzz { weight: 999, value: 'high', attr: 999, bench: 1234.123, rand: '1' }");
    char* query = "_ { weight: 999, value: 'high', attr: 999, bench: 1234.123, rand: '1' }";
    assert(err_is_ok(err));

    size_t exps = sizeof(records) / sizeof(size_t);
    for (size_t i = 1; i < exps; i++) {
        printf("# Run no_name_get_worstcase with %lu records:\n", records[i]);
        octopus_trigger_id_t tid;

        struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
        assert(cl != NULL);
        errval_t error_code;
        char* record;

        for (size_t j = records[i - 1]; j < records[i]; j++) {
            construct_record(5);
            cl->call_seq.set(cl, buf, SET_SEQUENTIAL, NOP_TRIGGER, false, &record, &tid, &error_code);
            if(err_is_fail(error_code)) { DEBUG_ERR(error_code, "set"); exit(0); }
        }

        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            timestamps[k].time0 = bench_tsc();
            cl->call_seq.get(cl, query, NOP_TRIGGER, &record, &tid, &error_code);
            timestamps[k].time1 = bench_tsc();
            if (err_is_fail(error_code)) { DEBUG_ERR(error_code, "get"); exit(0); }
            //timestamps[k].count = atoll(strrchr(record, '}')+1);
            free(record);
        }

        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            printf("%lu %"PRIuCYCLES" %u %lu %lu\n",
                    k,
                    timestamps[k].time1 - timestamps[k].time0 - bench_tscoverhead(),
                    timestamps[k].busy, timestamps[k].count, records[i]);
        }

    }
}

int main(int argc, char** argv)
{
    oct_init();
    bench_init();

    no_name_get_worstcase();
}

