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
#include <assert.h>
#include <stdint.h>
#include <omp.h>
#include <arch/x86/barrelfish_kpi/asm_inlines_arch.h>

#define GANG_SCHEDULING
#undef MEASURE_SYNC
#define MEASURE

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

#pragma omp parallel private(i,workload)
      for(i = 0; i < workmax; i++) {
#pragma omp barrier
          workload++;
      }

      now = rdtsc();
    } while(now - start < WORK_PERIOD);

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

#ifdef MEASURE_SYNC
  uint64_t waits[16] = {
      0, 1000, 1000000, 1000000000, 500, 5000000, 5000000000, 3000000,
      0, 1000, 1000000, 1000000000, 500, 5000000, 5000000000, 3000000
  };
  uint64_t ts[16][10];

  printf("before sync:\n");

#pragma omp parallel private(workcnt)
  {
      for(int j = 0; j < waits[omp_get_thread_num()]; j++) {
          workcnt++;
      }

      for(int j = 0; j < 10; j++) {
          ts[omp_get_thread_num()][j] = rdtsc();
      }
  }

  for(int j = 0; j < 10; j++) {
      printf("timestamp %d: ", j);

      for(int n = 1; n < nthreads; n++) {
          printf("%ld ", ts[n][j] - ts[n - 1][j]);
      }

      printf("\n");
  }

  printf("after sync:\n");

#pragma omp parallel
  {
      bomp_synchronize();

      for(int j = 0; j < 10; j++) {
          ts[omp_get_thread_num()][j] = rdtsc();
      }
  }

  for(int j = 0; j < 10; j++) {
      printf("timestamp %d: ", j);

      for(int n = 1; n < nthreads; n++) {
          printf("%ld ", ts[n][j] - ts[n - 1][j]);
      }

      printf("\n");
  }
#endif

#ifdef GANG_SCHEDULING
#pragma omp parallel
  {
      bomp_synchronize();
  }
#endif

  start = rdtsc();

#ifdef MEASURE
#       define MAXTHREADS      16
#       define WORKMAX         10000

  static uint64_t starta[MAXTHREADS][WORKMAX];
  static uint64_t end1[MAXTHREADS][WORKMAX];
  static uint64_t end2[MAXTHREADS][WORKMAX];
#endif

  // Do some work
#pragma omp parallel private(workcnt,i)
  for(i = 0; i < workmax; i++) {
#ifdef MEASURE
      starta[omp_get_thread_num()][i < WORKMAX ? i : WORKMAX] = rdtsc();
#endif
      workcnt++;
#ifdef MEASURE
      end1[omp_get_thread_num()][i < WORKMAX ? i : WORKMAX] = rdtsc();
#endif

#pragma omp barrier

#ifdef MEASURE
      end2[omp_get_thread_num()][i < WORKMAX ? i : WORKMAX] = rdtsc();
#endif
  }

  now = rdtsc();

#ifdef MEASURE
  printf("avg compute time: ");
  for(int n = 0; n < nthreads; n++) {
      uint64_t sum = 0, min = end1[0][0], max = 0;

      for(i = 0; i < WORKMAX; i++) {
          uint64_t val = end1[n][i] - starta[n][i];
          sum += val;
          min = val < min ? val : min;
          max = val > max ? val : max;
      }

      printf("%lu(%lu,%lu) ", sum / WORKMAX, min, max);
  }
  printf("\n");

#if 0
  printf("wait time dump:\n");
  for(i = 0; i < WORKMAX; i++) {
      for(int n = 0; n < nthreads; n++) {
          uint64_t val = end2[n][i] - end1[n][i];
          printf("%lu ", val);
      }
      printf("\n");
  }
#endif

  printf("avg wait time: ");
  for(int n = 0; n < nthreads; n++) {
      uint64_t sum = 0, min = end2[0][0], max = 0;

      for(i = 0; i < WORKMAX; i++) {
          uint64_t val = end2[n][i] - end1[n][i];
          sum += val;
          min = val < min ? val : min;
          max = val > max ? val : max;
      }

      printf("%lu(%lu,%lu) ", sum / WORKMAX, min, max);
  }
  printf("\n");
#endif

  printf("%s: threads %d, compute time %lu ticks\n", argv[0], nthreads, now - start);

  for(;;);
  return 0;
}
