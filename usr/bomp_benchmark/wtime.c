/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "wtime.h"
//#include <sys/time.h>
#ifndef POSIX
#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/cpu.h>
#else

#include <stdint.h>

static inline uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}

#endif

void wtime(double *t)
{
  uint64_t tsc;
  static uint64_t sec = 0;
//  struct timeval tv;
//  gettimeofday(&tv, (void *)0);
  tsc = rdtsc();
  if (sec == 0) sec = tsc;
  *t = (tsc - sec);
}

    
