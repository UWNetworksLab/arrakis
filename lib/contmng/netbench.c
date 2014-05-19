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

#ifndef NETBENCH_C_
#define NETBENCH_C_

#include <contmng/netbench.h>
#include <string.h>

#define MACHINE_CLK_UNIT    (1000000)

#if !defined(__scc__)
#define MACHINE_CLOCK_SPEED  (2800)
#else
#define MACHINE_CLOCK_SPEED  (533)
#endif // !defined(__scc__)
#define IN_SECONDS(x)   (((x)/(MACHINE_CLOCK_SPEED))/(MACHINE_CLK_UNIT))

#define CONVERT_TO_SEC

//#ifdef CONVERT_TO_SEC


// For recording statistics
float in_seconds(uint64_t cycles)
{
    float ans;
    ans = cycles / MACHINE_CLOCK_SPEED;
    ans = ans / MACHINE_CLK_UNIT;
    return ans;
}

#if 0
uint64_t in_seconds(uint64_t cycles)
{
    return cycles;
}
#endif // CONVERT_TO_SEC

uint64_t my_avg(uint64_t sum, uint64_t n)
{
    if (n == 0) {
        return sum;
    }
    return (sum/n);
}


// reset the statistics
void netbench_reset(struct netbench_details *nbp)
{
    memset(nbp->stats, 0, (nbp->total_events * nbp->event_elements *
                sizeof(uint64_t)));
    nbp->status = 0;
}


// Allocate the memory for storing netbench numbers
struct netbench_details *netbench_alloc(char *name, uint32_t total_events)
{
    struct netbench_details *nbp = (struct netbench_details *)
        malloc(sizeof(struct netbench_details));
    if (nbp == NULL) {
        printf("ERROR: malloc failed in alloc_netbench\n");
        abort();
    }

    memset(nbp, 0, sizeof(struct netbench_details));
    nbp->total_events = total_events;
    nbp->event_elements = EVENT_ELEMENTS;

    nbp->stats = malloc(total_events * (nbp->total_events *
                nbp->event_elements * sizeof(uint64_t)));

    if (nbp->stats == NULL) {
        printf("ERROR: malloc failed in alloc_netbench\n");
        abort();
    }


    netbench_reset(nbp);
    strncpy(nbp->name, name, 63);
    return nbp;
}/* end function: netbench_alloc */


void netbench_record_event(struct netbench_details *nbp,
        uint32_t event_type, uint64_t value)
{
    if (nbp == NULL) {
        return;
    }

    if (nbp->stats == NULL) {
        return;
    }

    if (nbp->status == 0) {
        return;
    }
    assert(event_type < nbp->total_events);

    ++nbp->stats[(event_type * nbp->event_elements) + RDT_COUNT];
    nbp->stats[(event_type * nbp->event_elements) + RDT_SUM] += value;

    // if first element
    if (nbp->stats[(event_type * nbp->event_elements) + RDT_COUNT] == 1) {
        nbp->stats[(event_type * nbp->event_elements) + RDT_MAX] = value;
        nbp->stats[(event_type * nbp->event_elements) + RDT_MIN] = value;
        return;
    }
    if (value > nbp->stats[(event_type * nbp->event_elements) + RDT_MAX]) {
        nbp->stats[(event_type * nbp->event_elements) + RDT_MAX] = value;
    }
    if (value < nbp->stats[(event_type * nbp->event_elements) + RDT_MIN]) {
        nbp->stats[(event_type * nbp->event_elements) + RDT_MIN]= value;
    }
} // end function: netbench_record_event

void netbench_record_event_no_ts(struct netbench_details *nbp,
        uint8_t event_type)
{
    if (nbp == NULL) {
        return;
    }

    if (nbp->stats == NULL) {
        return;
    }

    if (nbp->status == 0) {
        return;
    }
    assert(event_type < nbp->total_events);
    ++nbp->stats[(event_type * nbp->event_elements) + RDT_COUNT];
}

// Takes time difference
void netbench_record_event_simple(struct netbench_details *nbp,
        uint8_t event_type, uint64_t ts)
{
    uint64_t delta = rdtsc() - ts;
    netbench_record_event(nbp, event_type, delta);
}


void netbench_print_event_stat(struct netbench_details *nbp,
        uint8_t event_type, char *event_name, int type)
{
    if (nbp == NULL) {
        return;
    }

    if (nbp->stats == NULL) {
        return;
    }

    if (nbp->status == 0) {
        return;
    }

    assert(event_type < nbp->total_events);

    if (type == 1) {
        printf("Event %20s (%"PRIu8"): N[%"PRIu64"], AVG[%"PU"], "
          "MAX[%"PU"], MIN[%"PU"], TOTAL[%"PU"]\n", event_name, event_type,
          nbp->stats[(event_type * nbp->event_elements) + RDT_COUNT],
          in_seconds(my_avg(nbp->stats[(event_type * nbp->event_elements)
                    + RDT_SUM],
          nbp->stats[(event_type * nbp->event_elements) + RDT_COUNT])),
          in_seconds(nbp->stats[(event_type * nbp->event_elements) + RDT_MAX]),
          in_seconds(nbp->stats[(event_type * nbp->event_elements) + RDT_MIN]),
          in_seconds(nbp->stats[(event_type * nbp->event_elements) + RDT_SUM])
          );
    }
    else {
    printf("Event %20s (%"PRIu8"): N[%"PRIu64"], AVG[%"PRIu64"], "
          "MAX[%"PRIu64"], MIN[%"PRIu64"], TOTAL[%"PRIu64"]\n", event_name,
          event_type,
          nbp->stats[(event_type * nbp->event_elements) + RDT_COUNT],
          (my_avg(nbp->stats[(event_type * nbp->event_elements) + RDT_SUM],
                 nbp->stats[(event_type * nbp->event_elements) + RDT_COUNT])),
          (nbp->stats[(event_type * nbp->event_elements) + RDT_MAX]),
          (nbp->stats[(event_type * nbp->event_elements) + RDT_MIN]),
          (nbp->stats[(event_type * nbp->event_elements) + RDT_SUM]));
    }
} // end function: print_event_stat

void netbench_print_all_stats(struct netbench_details *nbp)
{
    if (nbp == NULL) {
        return;
    }

    if (nbp->stats == NULL) {
        return;
    }

    if (nbp->status == 0) {
        return;
    }

    for (int i = 0; i < nbp->total_events; ++i) {
        netbench_print_event_stat(nbp, i, "S", 0);
        netbench_print_event_stat(nbp, i, "C", 1);
    } // end for: each event
}

#endif // NETBENCH_C_

