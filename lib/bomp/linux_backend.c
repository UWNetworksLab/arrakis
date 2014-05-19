/**
 * \file
 * \brief Implementation of backend functions on Linux
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <pthread.h>
#include <numa.h>
#include <stdio.h>
#include "backend.h"
#include "omp.h"

void backend_set_numa(unsigned id)
{
    struct bitmask *bm = numa_allocate_cpumask();
    numa_bitmask_setbit(bm, id);
    numa_sched_setaffinity(0, bm);
    numa_free_cpumask(bm);
}

void backend_run_func_on(int core_id, void* cfunc, void *arg)
{
    pthread_t pthread;
    int r = pthread_create(&pthread, NULL, cfunc, arg);
    if (r != 0) {
        printf("pthread_create failed\n");
    }
}

static pthread_key_t pthread_key;

void *backend_get_tls(void)
{
    return pthread_getspecific(pthread_key);
}
 
void backend_set_tls(void *data)
{
    pthread_setspecific(pthread_key, data);
}

void *backend_get_thread(void)
{
    return (void *)pthread_self();
}

static int remote_init(void *dumm)
{
    return 0;
}

void backend_span_domain_default(int nos_threads)
{
    /* nop */
}

void backend_span_domain(int nos_threads, size_t stack_size)
{
    /* nop */
}

void backend_init(void)
{
    int r = pthread_key_create(&pthread_key, NULL);
    if (r != 0) {
        printf("pthread_key_create failed\n");
    }
}

void backend_create_time(int cores)
{
    /* nop */
}

void backend_thread_exit(void)
{
}

struct thread *backend_thread_create_varstack(bomp_thread_func_t start_func,
                                              void *arg, size_t stacksize)
{
    start_func(arg);
    return NULL;
}
