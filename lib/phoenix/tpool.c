/* Copyright (c) 2007-2009, Stanford University
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of Stanford University nor the names of its 
*       contributors may be used to endorse or promote products derived from 
*       this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY STANFORD UNIVERSITY ``AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL STANFORD UNIVERSITY BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/ 

#include <barrelfish/barrelfish.h>
#include <assert.h>

#include "atomic.h"
#include "memory.h"
#include "tpool.h"
#include "stddefines.h"

typedef struct {
    struct thread_sem           sem_run;
    unsigned int    *num_workers_done;
    struct thread_sem           *sem_all_workers_done;
    thread_func     *thread_func;
    void            **thread_func_arg;
    void            **ret;
    int             *num_workers;
    int             *die;
} thread_arg_t;

struct tpool_t {
    int             num_threads;
    int             num_workers;
    int             die;
    thread_func     thread_func;
    struct thread_sem           sem_all_workers_done;
    unsigned int    num_workers_done;
    void            **args;
    struct thread       **threads;
    thread_arg_t    *thread_args;
};

static int thread_loop (void *);

tpool_t* tpool_create (int num_threads)
{
    int             i, ret;
    tpool_t         *tpool;
    /* pthread_attr_t  attr; */

    tpool = phoenix_mem_calloc (1, sizeof (tpool_t));
    if (tpool == NULL) 
        return NULL;

    tpool->num_threads = num_threads;
    tpool->num_workers = num_threads;

    tpool->args = (void **)phoenix_mem_malloc (sizeof (void *) * num_threads);
    if (tpool->args == NULL) 
        goto fail_args;

    // Barrelfish: tpool->threads still allocated but not actually
    // used except for first idle thread, which I believe is created
    // by mistake on core 0, as it is never actually used.
    tpool->threads = (struct thread **)phoenix_mem_malloc (sizeof (struct thread *) * num_threads);
    if (tpool->threads == NULL) 
        goto fail_threads;

    tpool->thread_args = (thread_arg_t *)phoenix_mem_malloc (
        sizeof (thread_arg_t) * num_threads);
    if (tpool->thread_args == NULL) 
        goto fail_thread_args;

    thread_sem_init(&tpool->sem_all_workers_done, 0);

    /* CHECK_ERROR (pthread_attr_init (&attr)); */
    /* CHECK_ERROR (pthread_attr_setscope (&attr, PTHREAD_SCOPE_SYSTEM)); */
    /* CHECK_ERROR (pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED)); */

    /* Span domain to all cores */
    int my_core_id = disp_get_core_id();
    errval_t err;
    for (i = my_core_id + 1; i < num_threads + my_core_id; i++) {
        err = domain_new_dispatcher(i, NULL, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to span domain");
            printf("Failed to span domain to %d\n", i);
            assert(err_is_ok(err));
        }
    }

    tpool->die = 0;
    for (i = 0; i < num_threads; ++i) {
        /* Initialize thread argument. */
        thread_sem_init(&(tpool->thread_args[i].sem_run), 0);
        tpool->thread_args[i].sem_all_workers_done = 
            &tpool->sem_all_workers_done;
        tpool->thread_args[i].num_workers_done = 
            &tpool->num_workers_done;
        tpool->thread_args[i].die = &tpool->die;
        tpool->thread_args[i].thread_func = &tpool->thread_func;
        tpool->thread_args[i].thread_func_arg = &tpool->args[i];
        tpool->thread_args[i].ret = (void **)phoenix_mem_malloc (sizeof (void *));
        CHECK_ERROR (tpool->thread_args[i].ret == NULL);
        tpool->thread_args[i].num_workers = &tpool->num_workers;

        if(i < num_threads - 1) {
            do {
                err = domain_thread_create_on(i + my_core_id + 1, thread_loop,
                                              &tpool->thread_args[i]);
                if (err_no(err) == LIB_ERR_NO_SPANNED_DISP) {
                    thread_yield();
                }
            } while(err_no(err) == LIB_ERR_NO_SPANNED_DISP);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "domain_thread_create_on failed");
                printf("domain_thread_create_on failed on %d\n", i);
                assert(err_is_ok(err));
            }
        } else {
            tpool->threads[i] = thread_create(thread_loop, &tpool->thread_args[i]);
            err = thread_detach(tpool->threads[i]);
            assert(err_is_ok(err));
        }
    }

    return tpool;

fail_all_workers_done:
    phoenix_mem_free (tpool->thread_args);
fail_thread_args:
    phoenix_mem_free (tpool->threads);
fail_threads:
    phoenix_mem_free (tpool->args);
fail_args:

    return NULL;
}

int tpool_set (
    tpool_t *tpool, thread_func thread_func, void **args, int num_workers)
{
    int             i;
    
    assert (tpool != NULL);

    tpool->thread_func = thread_func;

    assert (num_workers <= tpool->num_threads);
    tpool->num_workers = num_workers;

    for (i = 0; i < num_workers; ++i)
    {
        tpool->args[i] = args[i];
    }
    

    return 0;
}

int tpool_begin (tpool_t *tpool)
{
    int             i, ret;

    assert (tpool != NULL);

    if (tpool->num_workers == 0)
        return 0;

    tpool->num_workers_done = 0;

    for (i = 0; i < tpool->num_workers; ++i) {
        thread_sem_post(&(tpool->thread_args[i].sem_run));
    }

    return 0;
}

int tpool_wait (tpool_t *tpool)
{
    int             ret;

    assert (tpool != NULL);

    if (tpool->num_workers == 0)
        return 0;

    thread_sem_wait(&tpool->sem_all_workers_done);

    return 0;
}

void** tpool_get_results (tpool_t *tpool)
{
    int             i;
    void            **rets;

    assert (tpool != NULL);

    rets = (void **)phoenix_mem_malloc (sizeof (void *) * tpool->num_threads);
    CHECK_ERROR (rets == NULL);

    for (i = 0; i < tpool->num_threads; ++i) {
        rets[i] = *(tpool->thread_args[i].ret);   
    }

    return rets;
}

int tpool_destroy (tpool_t *tpool)
{
    int             i;
    int             result;
    
    assert (tpool != NULL);
    assert (tpool->die == 0);

    result = 0;
    tpool->num_workers = tpool->num_threads;
    tpool->num_workers_done = 0;
    
    for (i = 0; i < tpool->num_threads; ++i) {
        phoenix_mem_free (tpool->thread_args[i].ret);

        tpool->die = 1;
        thread_sem_post(&tpool->thread_args[i].sem_run);
    }

    thread_sem_wait(&tpool->sem_all_workers_done);

    phoenix_mem_free (tpool->args);
    phoenix_mem_free (tpool->threads);
    phoenix_mem_free (tpool->thread_args);

    phoenix_mem_free (tpool);

    return result;
}

static int thread_loop (void *arg)
{
    thread_arg_t    *thread_arg = arg;
    thread_func     thread_func;
    void            *thread_func_arg;
    void            **ret;
    int             num_workers_done;

    assert (thread_arg);

    while (1)
    {
        thread_sem_wait(&thread_arg->sem_run);
        if (*thread_arg->die) {
            break;
        }

        thread_func = *(thread_arg->thread_func);
        thread_func_arg = *(thread_arg->thread_func_arg);
        ret = thread_arg->ret;

        /* Run thread function. */
        *ret = (*thread_func)(thread_func_arg);

        num_workers_done = fetch_and_inc(thread_arg->num_workers_done) + 1;
        if (num_workers_done == *thread_arg->num_workers)
        {
            /* Everybody's done. */
            thread_sem_post(thread_arg->sem_all_workers_done);
        }
    }

    num_workers_done = fetch_and_inc(thread_arg->num_workers_done) + 1;
    if (num_workers_done == *thread_arg->num_workers)
    {
        /* Everybody's done. */
        thread_sem_post(thread_arg->sem_all_workers_done);
    }

    return 0;
}
