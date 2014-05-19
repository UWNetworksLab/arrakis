/**
 * \file
 * \brief library for benchmarking the network related code
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef NETBENCH_H_
#define NETBENCH_H_

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

#if 0
#define CONVERT_TO_SEC

#ifdef CONVERT_TO_SEC
float in_seconds(uint64_t cycles);
float in_seconds(uint64_t cycles)
{
    float ans;
    ans = cycles / MACHINE_CLOCK_SPEED;
    ans = ans / MACHINE_CLK_UNIT;
    return ans;
}
#else
#define PU PRIu64
uint64_t in_seconds(uint64_t cycles);
uint64_t in_seconds(uint64_t cycles)
{
    return cycles;
}

#endif // CONVERT_TO_SEC

#endif // 0

// To print the seconds
#define PU "f"

#define NAME_SIZE               64

enum Benchmark_states {
    BMS_INACTIVE = 0,
    BMS_START_REQUEST = 1,
    BMS_RUNNING = 2,
    BMS_STOP_REQUEST = 3,
    BMS_STOPPED = 4,
};


enum Recorded_DataTypes {
    RDT_COUNT,
    RDT_SUM,
    RDT_MAX,
    RDT_MIN,
    EVENT_ELEMENTS  // MUST BE THE LAST ELEMENT!!
};

struct netbench_details {
    uint8_t status;
    uint32_t event_elements;
    uint32_t total_events;
    uint64_t *stats;
    char name[NAME_SIZE];
};

// Utility functions
uint64_t my_avg(uint64_t sum, uint64_t n);

#ifndef LIBRARY

float in_seconds(uint64_t cycles);
void netbench_reset(struct netbench_details *nbp);
struct netbench_details *netbench_alloc(char *name, uint32_t total_events);

void netbench_record_event(struct netbench_details *nbp,
        uint32_t event_type, uint64_t value);
void netbench_record_event_no_ts(struct netbench_details *nbp,
        uint8_t event_type);
void netbench_record_event_simple(struct netbench_details *nbp,
        uint8_t event_type, uint64_t ts);

void netbench_print_event_stat(struct netbench_details *nbp,
        uint8_t event_type, char *event_name, int type);
void netbench_print_all_stats(struct netbench_details *nbp);

#else

#define netbench_reset(a)
#define netbench_alloc(a, b)
#define netbench_record_event(a, b, c)
#define netbench_record_event_no_ts(a, b)
#define netbench_record_event_simple(a, b, c)   while(0) { (void)c; }
#define netbench_print_event_stat(a, b, c, d)
#define netbench_print_all_stats(a)

static inline float in_seconds(uint64_t cycles)
{
    return cycles;
}

#endif

__END_DECLS

#endif // CONTMNG_H_
