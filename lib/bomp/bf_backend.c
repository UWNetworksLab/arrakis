/**
 * \file
 * \brief Implementation of backend functions on barrelfish
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/sys_debug.h>
#include <barrelfish/resource_ctrl.h>
#include "backend.h"
#include "omp.h"

static rsrcid_t my_rsrc_id;

static const char *my_manifest =
    "B 1\n"                     // Normal phase
    "G 80 160 80 480\n";        // Gang phase

void GOMP_barrier(void);
bool GOMP_single_start(void);

void backend_set_numa(unsigned id)
{
    /* nop */
}

void *backend_get_tls(void)
{
    return thread_get_tls();
}

void backend_set_tls(void *data)
{
    thread_set_tls(data);
}

void *backend_get_thread(void)
{
    return thread_self();
}

static size_t thread_stack_size = 0;

void backend_run_func_on(int core_id, void* cfunc, void *arg)
{
    int actual_id = core_id + disp_get_core_id();
    thread_func_t func = (thread_func_t)cfunc;
    errval_t err = domain_thread_create_on_varstack(actual_id, func, arg,
                                                    thread_stack_size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "domain_thread_create_on failed");
        printf("domain_thread_create_on failed on %d\n", actual_id);
        assert(err_is_ok(err));
    }
}

static struct thread_sem init_sem = THREAD_SEM_INITIALIZER;

static int remote_init(void *dumm)
{
    errval_t err = rsrc_join(my_rsrc_id);
    assert(err_is_ok(err));

    thread_sem_post(&init_sem);
    return 0;
}

void backend_span_domain_default(int nos_threads)
{
    backend_span_domain(nos_threads, THREADS_DEFAULT_STACK_BYTES);
}

static uint64_t create_time;

static int cores_initialized = 1;

static void domain_init_done(void *arg, errval_t err)
{
    assert(err_is_ok(err));
    cores_initialized++;
}

void backend_span_domain(int nos_threads, size_t stack_size)
{
    int my_core_id = disp_get_core_id();

    // Remember default stack size
    thread_stack_size = stack_size;

    // Submit manifest (derived from program)
    errval_t err = rsrc_manifest(my_manifest, &my_rsrc_id);

    if(err_is_fail(err)) {
        DEBUG_ERR(err, "rsrc_manifest");
        abort();
    }

#ifdef __x86_64__
    create_time = rdtsc();
#endif

    /* Span domain to all cores */
    for (int i = my_core_id + 1; i < nos_threads + my_core_id; i++) {
        err = domain_new_dispatcher(i, domain_init_done, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to span domain");
            printf("Failed to span domain to %d\n", i);
            assert(err_is_ok(err));
        }
    }

    while(cores_initialized < nos_threads) {
        thread_yield();
    }

#ifdef __x86_64__
    create_time = rdtsc() - create_time;
#endif

    /* Run a remote init function on remote cores */
    for (int i = my_core_id + 1; i < nos_threads + my_core_id; i++) {
        err = domain_thread_create_on(i, remote_init, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "domain_thread_create_on failed");
            printf("domain_thread_create_on failed on %d\n", i);
            assert(err_is_ok(err));
        }
        thread_sem_wait(&init_sem);
    }
}

void backend_create_time(int cores)
{
    printf("Createtime %d %" PRIu64 "\n", cores, create_time);
}

void backend_init(void)
{
}

void backend_thread_exit(void)
{
    thread_exit();
}

struct thread *backend_thread_create_varstack(bomp_thread_func_t start_func,
                                              void *arg, size_t stacksize)
{
    struct thread *t = thread_create_varstack(start_func, arg, stacksize);
    errval_t err = thread_detach(t);
    assert(err_is_ok(err));
    return t;
}

void bomp_synchronize(void)
{
    /* if(GOMP_single_start()) { */
        errval_t err = rsrc_phase(my_rsrc_id, 1);
        assert(err_is_ok(err));
    /* } */
}
