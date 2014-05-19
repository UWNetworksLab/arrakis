/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <stdint.h>
#include <omp.h>
#include <arch/x86/barrelfish_kpi/asm_inlines_arch.h>

#define WORK_PERIOD	5000000000UL
#define STACK_SIZE      (64 * 1024)

int main(int argc, char *argv[])
{
  uint64_t now, start;
  volatile uint64_t workcnt, workload = 0;
  int64_t workmax = 1000;
  int64_t i;

  if(argc == 1) {
    printf("calibrating...\n");

    do {
      workload = 0;
      workmax *= 2;

      start = rdtsc();

      for(i = 0; i < workmax; i++) {
	workload++;
      }

      now = rdtsc();
    } while(now - start < WORK_PERIOD);

    // Compute so the max number of CPUs would calc for WORK_PERIOD
    workmax *= omp_get_num_procs();

    printf("workmax = %ld\n", workmax);
    return 0;
  } else {
    workmax = atol(argv[1]);
  }

  int nthreads = omp_get_max_threads();

  if(argc == 3) {
      nthreads = atoi(argv[2]);

      backend_span_domain(nthreads, STACK_SIZE);
      bomp_custom_init();
      omp_set_num_threads(nthreads);
  }

  printf("threads %d, workmax %ld, CPUs %d\n", nthreads, workmax,
	 omp_get_num_procs());

  start = rdtsc();

  // Do some work
#pragma omp parallel for private(workcnt)
  for(i = 0; i < workmax; i++) {
    workcnt++;
  }

  now = rdtsc();

  printf("%s: threads %d, compute time %lu ticks\n", argv[0], nthreads, now - start);

  for(;;);
  return 0;
}
