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
#include <barrelfish/barrelfish.h>
#include <string.h>

#undef GANG_SCHEDULING

#define PERIOD          2500000000UL
#define STACK_SIZE      (64 * 1024)
#define ITERATIONS      120

extern uint64_t stuck[64];
static uint64_t workcnt[32][ITERATIONS];

int main(int argc, char *argv[])
{
  int nthreads = omp_get_max_threads();

  if(argc == 2) {
      nthreads = atoi(argv[1]);
      backend_span_domain(nthreads, STACK_SIZE);
      bomp_custom_init();
      omp_set_num_threads(nthreads);
  }

  printf("threads %d, CPUs %d\n", nthreads, omp_get_num_procs());

  volatile uint64_t exittime[ITERATIONS] = { 0 };

  // Do some work
#pragma omp parallel
  {
#ifdef GANG_SCHEDULING
      bomp_synchronize();
#endif
      for(int i = 0; i < ITERATIONS; i++) {
          uint64_t start = rdtsc();
          uint64_t workcn = 0;

          for(uint64_t n = 0;; n++) {
#pragma omp barrier
              workcn++;

              if(omp_get_thread_num() == 0 && exittime[i] == 0
                 && rdtsc() >= start + PERIOD) {
                  exittime[i] = n + 3;
              }

              if(exittime[i] != 0 && exittime[i] == n) {
                  n++;
                  break;
              }
          }

          /* char buf[64]; */
          /* sprintf(buf, "%d: %lu(%lu)\n", omp_get_thread_num(), workcn, */
          /*         stuck[omp_get_thread_num()]); */
          /* sys_print(buf, strlen(buf)); */
          /* stuck[omp_get_thread_num()] = 0; */
          workcnt[omp_get_thread_num()][i] = workcn;
      }
  }

  char buf[64];
  for(int i = 0; i < ITERATIONS; i++) {
      for(int n = 0; n < nthreads; n++) {
          sprintf(buf, "%lu ", workcnt[n][i]);
          sys_print(buf, strlen(buf));
      }
      sys_print("\n", 1);
  }

      /* sys_print("\n", 1); */

      /* char buf[128], buf1[128]; */
      /* sprintf(buf, "iterations in %lu ticks: ", PERIOD); */
      /* for(int i = 0; i < nthreads; i++) { */
      /*     sprintf(buf1, "%lu ", workcnt[i]); */
      /*     strcat(buf, buf1); */
      /* } */
      /* sprintf(buf1, "\n"); */
      /* strcat(buf, buf1); */
      /* sys_print(buf, strlen(buf)); */
  /* } */

  for(;;);
  return 0;
}
