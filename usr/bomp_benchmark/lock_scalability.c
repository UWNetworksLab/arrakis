/**
 * \file
 * \brief Lock scalability benchmark.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <omp.h>
#include <stdlib.h>

// Use spinlocks if defined, mutexes otherwise
#define SPINLOCKS

#ifdef POSIX
#include <pthread.h>
#include <stdint.h>

#ifdef SPINLOCKS
/** \brief spinlock */
typedef volatile uint64_t spinlock_t __attribute__ ((aligned(64)));

static inline void acquire_spinlock(spinlock_t * volatile lock)
{
    __asm__ __volatile__(
                         "0:\n\t"
                         "xor %%rax,%%rax\n\t"
                         "lock bts %%rax,(%0)\n\t"
                         "jc 0b\n\t"
                         : : "S" (lock) : "rax"
                        );
}

static inline void release_spinlock(spinlock_t * volatile lock)
{
    *lock = 0;
}
#endif

static inline uint64_t rdtsc(void)
{
    uint64_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return (edx << 32) | eax;
}
#endif

int main(int argc, char *argv[])
{
        int i=0;

	bomp_custom_init();
        omp_set_num_threads(4);

#ifndef POSIX
#ifndef SPINLOCKS
        static struct thread_mutex lock = THREAD_MUTEX_INITIALIZER;
#else
        static spinlock_t lock = 0;
#endif
#else
#ifdef SPINLOCKS
        static spinlock_t lock = 0;
#else
        static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
#endif
#endif

        uint64_t begin = rdtsc();

        #pragma omp parallel 
        {
#pragma omp for private(i)
		   for(i=0;i<1000000;i++)
		   {
#ifdef SPINLOCKS
                       acquire_spinlock(&lock);
                       release_spinlock(&lock);
#else
                       thread_mutex_lock(&lock);
                       thread_mutex_unlock(&lock);
#endif
		   }
	}

        uint64_t end = rdtsc();

        printf("took %lu\n", end - begin);
}

