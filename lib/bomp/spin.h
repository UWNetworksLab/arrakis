/**
 * \file
 * \brief Spinning synchronizations
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BOMP_SPIN_H
#define BOMP_SPIN_H

/* #include <barrelfish/barrelfish.h> */
/* #include <string.h> */
#include <omp.h>

/** \brief spinlock */
// why 64-bit? this means we need an extra prefix on the lock ops... -AB
typedef volatile unsigned long bomp_lock_t;

static inline void bomp_lock(bomp_lock_t *lock)
{
    __asm__ __volatile__("0:\n\t"
                         "cmpq $0, %0\n\t"
                         "je 1f\n\t"
                         "pause\n\t"
                         "jmp 0b\n\t"
                         "1:\n\t"
                         "lock btsq $0, %0\n\t"
                         "jc 0b\n\t"
                         : "+m" (*lock) : : "memory", "cc");
}

static inline void bomp_unlock(bomp_lock_t *lock)
{
    *lock = 0;
}

static inline void bomp_lock_init(bomp_lock_t *lock)
{
    /* nop */
}

struct bomp_barrier {
    unsigned max;
    volatile unsigned cycle;
    volatile unsigned counter;
};

static inline void bomp_barrier_init(struct bomp_barrier *barrier , int count)
{
    barrier->max     = count;
    barrier->cycle   = 0;
    barrier->counter = 0;
}

static inline void bomp_clear_barrier(struct bomp_barrier *barrier)
{
    /* nop */
}

uint64_t stuck[64];

static inline void bomp_barrier_wait(struct bomp_barrier *barrier)
{
    int cycle = barrier->cycle;
    if (__sync_fetch_and_add(&barrier->counter, 1) == barrier->max - 1) {
        barrier->counter = 0;
        barrier->cycle = !barrier->cycle;
    } else {
        uint64_t waitcnt = 0;

        while (cycle == barrier->cycle) {
            waitcnt++;
        }

        if(waitcnt > 10000000) {
            stuck[omp_get_thread_num()]++;
            /* char buf[128]; */
            /* sprintf(buf, "thread %d stuck in barrier\n", */
            /*         omp_get_thread_num()); */
            /* sys_print(buf, strlen(buf)); */
        }
    }                                       
}

#endif /* BOMP_SPIN_H */
