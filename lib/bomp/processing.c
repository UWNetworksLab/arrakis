/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "libbomp.h"
#include "backend.h"

static int count = 0;
volatile unsigned g_thread_numbers = 1;
static struct bomp_thread_local_data **g_array_thread_local_data;

static void bomp_set_tls(void *xdata)
{
    struct bomp_thread_local_data *local;
    struct bomp_work *work_data = (struct bomp_work*)xdata;

    /* Populate the Thread Local Storage data */
    local = calloc(1, sizeof(struct bomp_thread_local_data));
    assert(local != NULL);
    local->thr = backend_get_thread();
    local->work = work_data;
    g_array_thread_local_data[work_data->thread_id] = local;
    backend_set_tls(local);
}

static int bomp_thread_fn(void *xdata)
{
    struct bomp_work *work_data = xdata;

    backend_set_numa(work_data->thread_id);

    bomp_set_tls(work_data);
    work_data->fn(work_data->data);

    /* Wait for the Barrier */
    bomp_barrier_wait(work_data->barrier);
    return 0;
}

#define THREAD_OFFSET   0
/* #define THREAD_OFFSET   12 */

void bomp_start_processing(void (*fn) (void *), void *data, unsigned nthreads)
{
    /* Create Threads and ask them to process the function specified */
    /* Let them die as soon as they are done */
    unsigned i;
    struct bomp_work *xdata;
    struct bomp_barrier *barrier;

    g_thread_numbers = nthreads;

    char *memory = calloc(1, nthreads * sizeof(struct bomp_thread_local_data *)
                            + sizeof(struct bomp_barrier)
                            + nthreads * sizeof(struct bomp_work));
    assert(memory != NULL);

    g_array_thread_local_data = (struct bomp_thread_local_data **)memory;
    memory += nthreads * sizeof(struct bomp_thread_local_data *);


    /* Create a barier for the work that will be carried out by the threads */
    barrier = (struct bomp_barrier *)memory;
    memory += sizeof(struct bomp_barrier);
    bomp_barrier_init(barrier, nthreads);

    /* For main thread */
    xdata = (struct bomp_work * )memory;
    memory += sizeof(struct bomp_work);

    xdata->fn = fn;
    xdata->data = data;
    xdata->thread_id = 0;
    xdata->barrier = barrier;
    bomp_set_tls(xdata);

    for (i = 1; i < nthreads; i++) {
        xdata = (struct bomp_work *)memory;
        memory += sizeof(struct bomp_work);

        xdata->fn = fn;
        xdata->data = data;
        xdata->thread_id = i;
        xdata->barrier = barrier;

        /* Create threads */
        backend_run_func_on(i + THREAD_OFFSET, bomp_thread_fn, xdata);
    }
}

void bomp_end_processing(void)
{
    /* Cleaning of thread_local and work data structures */
    int i = 0;
    count++;

    bomp_barrier_wait(g_array_thread_local_data[i]->work->barrier);

    /* Clear the barrier created */
    bomp_clear_barrier(g_array_thread_local_data[i]->work->barrier);

    // XXX: free(g_array_thread_local_data); why not? -AB
    g_array_thread_local_data = NULL;
    g_thread_numbers = 1;
}
