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

#define GANG_SCHEDULING
#define MEASURE_BARRIER

#define PERIOD          2500000000UL
#define ITERATIONS	10
#define STACK_SIZE      (64 * 1024)

static inline uint64_t rdtsc(void)
{
    uint64_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return (edx << 32) | eax;
}

int main(int argc, char *argv[])
{
  int nthreads;

  if(argc == 2) {
      nthreads = atoi(argv[1]);
      backend_span_domain(14, STACK_SIZE);
      bomp_custom_init();
      omp_set_num_threads(nthreads);
  } else {
      assert(!"Specify number of threads");
  }

  volatile uint64_t workcnt[32] = { 0 };
  uint64_t last = rdtsc();
#ifndef CPU_BOUND
  volatile uint64_t exittime[ITERATIONS] = { 0 };
#endif

  for(int iter = 0;; iter = (iter + 1) % ITERATIONS) {
#ifdef CPU_BOUND
    volatile bool exitnow = false;
#else

#ifdef MEASURE_BARRIER
#	define MAXTHREADS	16
#	define WORKMAX		5000000
    static uint64_t starta[MAXTHREADS][WORKMAX];
#endif
#endif

#ifdef GANG_SCHEDULING
#pragma omp parallel
  {
      bomp_synchronize();
  }
#endif

  // Do some work
#pragma omp parallel
    for(uint64_t i = 0;; i++) {
#ifndef CPU_BOUND
#	ifdef MEASURE_BARRIER
      uint64_t lasta = rdtsc();
#	endif
#	pragma omp barrier
#	ifdef MEASURE_BARRIER
      if(i < WORKMAX) {
	starta[omp_get_thread_num()][i] = rdtsc() - lasta;
      }
#	endif
#endif
      workcnt[omp_get_thread_num()]++;

#pragma omp master
      if(rdtsc() >= last + PERIOD) {
        printf("%lu: threads %d (%s), progress ", rdtsc(), nthreads, "static");
        for(int n = 0; n < 32; n++) {
          printf("%lu ", workcnt[n]);
        }
        printf("\n");
        last += PERIOD;

#ifndef CPU_BOUND
        if(exittime[iter] == 0) {
          exittime[iter] = i + 3;
          exittime[(iter + ITERATIONS - 2) % ITERATIONS] = 0;
        }
      }

      if(exittime[iter] != 0 && exittime[iter] == i) {
        break;
      }
#else

        exitnow = true;
      }

      if(exitnow) {
        break;
      }
#endif
    }

#ifndef CPU_BOUND
  static uint64_t hgram[15] = { 0 };

    printf("exittime = %lu\n", exittime[iter]);
    assert(exittime[iter] <= WORKMAX);
    uint64_t endtime = exittime[iter] < WORKMAX ? exittime[iter] : WORKMAX;
    for(int i = 0; i < endtime; i++) {
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
        printf("%lu\t%lu\n", val, hgram[i]);
    }
#endif
  }
}
