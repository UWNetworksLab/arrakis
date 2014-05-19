/**
 * \file
 * \brief BOMP barrier synchronization microbenchmark
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <omp.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

#define PERIOD          2500000000UL
#define ITERATIONS	10
#define STACK_SIZE      (64 * 1024)

struct workcnt {
  uint64_t	cnt;
} __attribute__ ((aligned (64)));

int main(int argc, char *argv[])
{
  static struct workcnt workcnt[32];
  static struct workcnt exittime[ITERATIONS];
  int nthreads;
  int iterations = 0;
  uint64_t last;

  /* uint64_t last = rdtsc(); */
  /* while(rdtsc() < last + PERIOD) { */
  /*     thread_yield(); */
  /* } */

  if(argc == 2) {
      nthreads = atoi(argv[1]);
      backend_span_domain(nthreads, STACK_SIZE);
      bomp_custom_init();
      omp_set_num_threads(nthreads);
  } else {
      assert(!"Specify number of threads");
  }

#if CONFIG_TRACE
    errval_t err = trace_control(TRACE_EVENT(TRACE_SUBSYS_BOMP,
                                             TRACE_EVENT_BOMP_START, 0),
                                 TRACE_EVENT(TRACE_SUBSYS_BOMP,
                                             TRACE_EVENT_BOMP_STOP, 0), 0);
    assert(err_is_ok(err));

    trace_event(TRACE_SUBSYS_BOMP, TRACE_EVENT_BOMP_START, 0);
#endif

    /* bomp_synchronize(); */
    last = rdtsc();

  for(int iter = 0;; iter = (iter + 1) % ITERATIONS) {
  // Do some work
#pragma omp parallel
    for(uint64_t i = 0;; i++) {
#pragma omp barrier
      workcnt[omp_get_thread_num()].cnt++;

#pragma omp master
      if(rdtsc() >= last + PERIOD) {
#if CONFIG_TRACE
          trace_event(TRACE_SUBSYS_BOMP, TRACE_EVENT_BOMP_STOP, 0);

          char *buf = malloc(4096*4096);
          trace_dump(buf, 4096*4096, NULL);
          printf("%s\n", buf);
          abort();
#endif

          printf("%s, %lu: threads %d (%s), progress ", argv[0], rdtsc(), omp_get_num_threads(), omp_get_dynamic() ? "dynamic" : "static");
          for(int n = 0; n < 32; n++) {
              printf("%lu ", workcnt[n].cnt);
          }
          printf("\n");
          last += PERIOD;
          iterations++;
          if(iterations == 25) {
              printf("client done\n");
              abort();
          }

          if(exittime[iter].cnt == 0) {
              exittime[iter].cnt = i + 3;
              exittime[(iter + ITERATIONS - 2) % ITERATIONS].cnt = 0;
          }
      }

      if(exittime[iter].cnt != 0 && exittime[iter].cnt == i) {
          break;
      }
    }
  }
}
