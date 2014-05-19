/**
 * \file
 * \brief User-space IPI Microbenchmark.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h>

#define ITERATIONS      100
#define IPI_IRQ         18

static int ipi_dest = 0;
static bool is_starter = false;
static bool ipi_seen = false;

static void ipi_handler(void *arg)
{
    // consume IDC message
    struct idc_recv_msg msg;
    idc_endpoint_poll(arg, &msg, NULL);

    // Wakeup benchmark thread
    ipi_seen = true;

    if(!is_starter) {
        // Send interrupt further to dest
        sys_debug_send_ipi(ipi_dest, 0, IPI_IRQ + 32);
    }
}

static void benchmark(void)
{
    static uint64_t durations[ITERATIONS];
    static uint64_t sendcost[ITERATIONS];

    for(int i = 0; i < ITERATIONS; i++) {
        uint64_t begin = rdtsc();
        ipi_seen = false;
        sys_debug_send_ipi(ipi_dest, 0, IPI_IRQ + 32);
        uint64_t sent = rdtsc();
        while (!ipi_seen) {
            messages_wait_and_handle_next();
        }
        uint64_t end = rdtsc();
        durations[i] = end - begin;
        sendcost[i] = sent - begin;
    }

    uint64_t sumd = 0, sumcost = 0;
    for(int i = 0; i < ITERATIONS; i++) {
        printf("durations[%d] = %lu\n", i, durations[i]);
        printf("sendcost[%d] = %lu\n", i, sendcost[i]);
        sumd += durations[i];
        sumcost += sendcost[i];
    }

    printf("Average IPI latency for %u tries: %lu ticks.\n",
           ITERATIONS, (sumd / ITERATIONS) / 2);
    printf("Average IPI send cost for %u tries: %lu ticks.\n",
           ITERATIONS, sumcost / ITERATIONS);
}

static void victim(void)
{
    uint64_t tsc1, tsc2;
    static uint64_t delay[ITERATIONS];
    int i = 0;

    tsc1 = rdtsc();
    while (i < ITERATIONS) {
        tsc2 = rdtsc();
        if (tsc2 - tsc1 > 100) {
            if (messages_handle_next_if_present() && ipi_seen) {
                ipi_seen = false;
                delay[i++] = tsc2 - tsc1;
            }
        }
        tsc1 = tsc2;
    }

    uint64_t sum = 0;
    for(i = 0; i < ITERATIONS; i++) {
        printf("delay[%d] = %lu\n", i, delay[i]);
        sum += delay[i];
    }

    printf("Average receiver delay for %u tries: %lu ticks.\n",
           ITERATIONS, sum / ITERATIONS);
}

int main(int argc, char *argv[])
{
    bool run_victim = false;

    // Register IRQ
    USER_PANIC("irq_handle(IPI_IRQ, ipi_handler, NULL);");

    for(int i = 1; i < argc; i++) {
        if(!strncmp(argv[i], "dest=", 5)) {
            ipi_dest = atoi(argv[i] + 5);
        } else if(!strncmp(argv[i], "start", 5)) {
            is_starter = true;
        } else if(!strncmp(argv[i], "core=", 5)) {
            // NOP
        } else if(!strcmp(argv[i], "overhead")) {
            run_victim = true;
        } else {
            printf("ignoring unknown option: %s\n", argv[i]);
        }
    }

    if(is_starter) {
        // Wait a little bit for all other domains to come up
        for(int i = 0; i < 0xfffff; i++) {
            thread_yield();
        }

        benchmark();
    } else if (run_victim) {
        victim();
    } else {
        // Hang around
        messages_handler_loop();
    }

    return 0;
}
