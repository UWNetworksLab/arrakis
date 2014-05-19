/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
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

#define PERIOD          2500000000UL
#define ITERATIONS	10
#define STACK_SIZE      (64 * 1024)

struct workcnt {
  uint64_t	cnt;
} __attribute__ ((aligned (64)));

static inline uint64_t rdtsc(void)
{
    uint64_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return (edx << 32) | eax;
}

int main(int argc, char *argv[])
{
  static struct workcnt workcnt[32];
  int nthreads;

  if(argc == 2) {
      nthreads = atoi(argv[1]);
      backend_span_domain(nthreads, STACK_SIZE);
      bomp_custom_init();
      omp_set_num_threads(nthreads);
  } else {
      assert(!"Specify number of threads");
  }

  uint64_t glast = rdtsc();
  for(;;) {
      // Do some work
#pragma omp parallel
      {
          uint64_t last = glast;

          for(;;) {
              workcnt[omp_get_thread_num()].cnt++;

              if(rdtsc() >= last + PERIOD) {
                  break;
              }
          }
      }

      printf("%s: %lu: threads %d (%s), progress ", argv[0], rdtsc(), omp_get_num_threads(), omp_get_dynamic() ? "dynamic" : "static");
      for(int n = 0; n < 32; n++) {
          printf("%lu ", workcnt[n].cnt);
      }
      printf("\n");
      fflush(stdout);
      glast += PERIOD;
  }
}
