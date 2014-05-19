/** \file
 *  \brief Example application using threads - a more complex 
 *         example using synchronisation
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>

#include <barrelfish/threads.h>


// data to pass to each thread
struct my_thread_data {
    int t_num;
    struct thread *t_id;
    struct thread_mutex *mutex;
    int *start_cntr;
    struct thread_sem *sem;
    int ret;
};

// the code that each thread runs
static int my_thread(void *data)
{
    struct my_thread_data *t_data = data;
    int thread_num = t_data->t_num;

    thread_mutex_lock(t_data->mutex);
    (*(t_data->start_cntr))++;
    thread_mutex_unlock(t_data->mutex);

    printf("this is thread %d saying hello\n", thread_num);

    thread_sem_post(t_data->sem);

    return thread_num;
}

int main(int argc, char *argv[])
{
    errval_t err;
    int num_threads = 0;

    // # of threads to start
    if (argc == 2) {
        num_threads = atoi(argv[1]);
        debug_printf("starting %d threads\n", num_threads);
    } else {
        printf("usage %s num_threads\n", argv[0]);
        return EXIT_FAILURE;
    }


    // setup lock, counter, and semaphore
    struct thread_mutex mutex = THREAD_MUTEX_INITIALIZER;
    thread_mutex_init(&mutex);
    int start_cntr = 0;
    struct thread_sem sem = THREAD_SEM_INITIALIZER;
    thread_sem_init(&sem, 0);

    // set thread argument data
    struct my_thread_data *t_data;
    t_data = malloc(num_threads*sizeof(struct my_thread_data));
    assert(t_data != NULL);


    // start threads
    for (int i = 0; i < num_threads; i++) {
        t_data[i].t_num = i;
        t_data[i].mutex = &mutex;
        t_data[i].start_cntr = &start_cntr;        
        t_data[i].sem = &sem;

        t_data[i].t_id = thread_create(my_thread, &(t_data[i]));
        if (t_data[i].t_id == NULL) {
            debug_printf("ERROR: starting thread %d\n", i);
        }
        debug_printf("started thread %d\n", i);
    }

    // wait until all started
    while (start_cntr != num_threads) {
        thread_yield();
    }

    debug_printf("all threads started\n");

    // wait until all finished
    for (int i = 0; i < num_threads; i++) {
        thread_sem_wait(&sem);
    }

    debug_printf("all threads finished\n");

    // cleanup: join all threads    
    for (int i = 0; i < num_threads; i++) {
        err = thread_join(t_data[i].t_id, &(t_data[i].ret));
        if (err_is_ok(err)) {
            debug_printf("joined thread %d, return value: %d\n", 
                         i, t_data[i].ret);
        } else {
            DEBUG_ERR(err, "in thread_join for thread %d", i);
        }
    }

    debug_printf("finished.\n");

    return EXIT_SUCCESS;
}

