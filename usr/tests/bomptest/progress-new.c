/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <stdint.h>
#include <omp.h>

#define ITERATIONS	1000000
#define STACK_SIZE      (64 * 1024)
#define MAXTHREADS	16

static inline uint64_t rdtsc(void)
{
    uint64_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return (edx << 32) | eax;
}

int main(int argc, char *argv[])
{
  int nthreads = 1;
  static uint64_t starta[MAXTHREADS][ITERATIONS];

  if(argc == 2) {
      nthreads = atoi(argv[1]);
      backend_span_domain(14, STACK_SIZE);
      bomp_custom_init();
      omp_set_num_threads(nthreads);
  } else {
      assert(!"Specify number of threads");
  }

  // Do some work
#pragma omp parallel
  {
      bomp_synchronize();

      for(uint64_t i = 0; i < ITERATIONS; i++) {
          uint64_t lasta = rdtsc();
#pragma omp barrier
          starta[omp_get_thread_num()][i] = rdtsc() - lasta;
    }
  }

  static uint64_t hgram[15] = { 0 };

  for(int i = 0; i < ITERATIONS; i++) {
      for(int n = 0; n < nthreads; n++) {
          uint64_t val = starta[n][i];
          for(int j = 0; j < 15; j++) {
              val /= 10;
              if(val == 0) {
                  hgram[j]++;
                  break;
              }
          }
      }
  }

  uint64_t val = 1;
  for(int i = 0; i < 15; i++) {
      val *= 10;
      if(hgram[i] > 0) {
          printf("%lu\t%lu\n", val, hgram[i]);
      }
  }

  for(;;);
  return 0;
}
