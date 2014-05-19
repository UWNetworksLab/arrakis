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

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>

#include "memory.h"
#include "taskQ.h"
#include "queue.h"
#include "synch.h"
#include "locality.h"

static int num_strands_per_chip = 0;

typedef struct {
    task_t              task; 
    queue_elem_t        queue_elem;
} tq_entry_t;

typedef struct {
    mr_lock_t  parent;
    uintptr_t  chksum;
    mr_lock_t  *per_thread;
} tq_lock_t;

struct taskQ_t {
    int             num_queues;
    int             num_threads;
    queue_t         **queues;
    queue_t         **free_queues;
    tq_lock_t       *locks;
    /* putting all seeds together may lead to extra coherence traffic among cpus
     * if it's a problem we can pad it by l1 line size */
    /* per-thread random seed */
    unsigned int    *seeds;
 };

typedef int (*dequeue_fn)(taskQ_t *, int, int, queue_elem_t**);

static inline taskQ_t* tq_init_normal(int numThreads);
static inline void tq_finalize_normal(taskQ_t* tq);
static inline int tq_dequeue_normal(
    taskQ_t* tq, task_t* task, int lgrp, int tid);
static inline int tq_dequeue_normal_seq (
    taskQ_t* tq, task_t* task, int lgrp, int tid);
static inline int tq_dequeue_normal_internal (
    taskQ_t* tq, task_t* task, int lgrp, int tid, dequeue_fn dequeue_fn);

static queue_t* tq_alloc_queue(void);
static void tq_free_queue(queue_t* q);
static int tq_queue_init(taskQ_t* tq, unsigned int idx);
static void tq_queue_destroy(taskQ_t* tq, unsigned int idx);
static void tq_empty_queue(queue_t* q);

taskQ_t* tq_init (int num_threads)
{
    return tq_init_normal(num_threads);
}

void tq_reset (taskQ_t* tq, int num_threads)
{
}

/**
 * Initialize task queue for a normal machine
 */
static inline taskQ_t* tq_init_normal(int numThreads)
{
    int             i;
    taskQ_t         *tq = NULL;

    tq = phoenix_mem_calloc(1, sizeof(taskQ_t));
    if (tq == NULL) {
        return NULL;
    }

    /* XXX should this be local? */
    num_strands_per_chip = loc_get_lgrp_size ();
    tq->num_threads = numThreads;
    tq->num_queues = tq->num_threads / num_strands_per_chip;

    if (tq->num_queues == 0)
        tq->num_queues = 1;

    tq->queues = (queue_t **)phoenix_mem_calloc (tq->num_queues, sizeof (queue_t *));
    if (tq->queues == NULL) goto fail_queues;

    tq->free_queues = (queue_t **)phoenix_mem_calloc (
        tq->num_queues, sizeof (queue_t *));
    if (tq->free_queues == NULL) goto fail_free_queues;

    tq->locks = (tq_lock_t *)phoenix_mem_calloc (tq->num_queues, sizeof (tq_lock_t));
    if (tq->locks == NULL) goto fail_locks;

    tq->seeds = (unsigned int*)phoenix_mem_calloc(
        tq->num_threads, sizeof(unsigned int));
    if (tq->seeds == NULL) goto fail_seeds;
    mem_memset(tq->seeds, 0, sizeof(unsigned int) * tq->num_threads);

    for (i = 0; i < tq->num_queues; ++i)
        if (!tq_queue_init(tq, i))
            goto fail_tq_init;

    return tq;

fail_tq_init:
    /* destroy all queues that have been allocated */
    i--;
    while (i >= 0) {
        tq_queue_destroy(tq, i);
        --i;
    }
    phoenix_mem_free(tq->seeds);
fail_seeds:
    phoenix_mem_free(tq->locks);
fail_locks:
    phoenix_mem_free(tq->free_queues);
fail_free_queues:
    phoenix_mem_free(tq->queues);
fail_queues:
    phoenix_mem_free(tq);
    return NULL;
}

/**
 * Destroys an initialized queue (i.e. free queue and alloc queue) in task queue
 * @param tq    tq to index
 * @param idx   index of queue to destroy in tq
 */
static void tq_queue_destroy(taskQ_t* tq, unsigned int idx)
{
    int             j;
    uintptr_t       chksum;

    assert (idx < tq->num_queues);

    tq_empty_queue(tq->queues[idx]);
    tq_free_queue(tq->queues[idx]);

    tq_empty_queue(tq->free_queues[idx]);
    tq_free_queue(tq->free_queues[idx]);

    /* free all lock data associated with queue */
    chksum = 0;
    for (j = 0; j < tq->num_threads; j++) {
        chksum += (uintptr_t)tq->locks[idx].per_thread[j];
        lock_free_per_thread(tq->locks[idx].per_thread[j]);
    }

    lock_free (tq->locks[idx].parent);

    phoenix_mem_free (tq->locks[idx].per_thread);
    tq->locks[idx].per_thread = NULL;
}

/**
 * Initialize a queue for a given index in the task queue
 * @return zero on failure, nonzero on success
 */
static int tq_queue_init(taskQ_t* tq, unsigned int idx)
{
    int     j;

    assert (idx < tq->num_queues);

    tq->queues[idx] = tq_alloc_queue();
    if (tq->queues[idx] == NULL) return 0;

    tq->free_queues[idx] = tq_alloc_queue();
    if (tq->free_queues[idx] == NULL) goto fail_free_queue;

    tq->locks[idx].parent = lock_alloc();

    tq->locks[idx].per_thread = (mr_lock_t *)phoenix_mem_calloc(
        tq->num_threads, sizeof(mr_lock_t));
    if (tq->locks[idx].per_thread == NULL) goto fail_priv_alloc;

    tq->locks[idx].chksum = 0;
    for (j = 0; j < tq->num_threads; ++j) {
        mr_lock_t   per_thread;
        per_thread = lock_alloc_per_thread(tq->locks[idx].parent);
        tq->locks[idx].per_thread[j] = per_thread;
        tq->locks[idx].chksum += (uintptr_t)per_thread;
    }

    return 1;

fail_priv_alloc:
    lock_free(tq->locks[idx].parent);
    tq_free_queue(tq->free_queues[idx]);
    tq->free_queues[idx] = NULL;
fail_free_queue:
    tq_free_queue(tq->queues[idx]);
    tq->queues[idx] = NULL;

    return 0;
}

/**
 * Allocates an initialized queue
 * @return NULL on failure, initialized queue pointer on success
 */
static queue_t* tq_alloc_queue(void)
{
    queue_t *q;

    q = (queue_t*) phoenix_mem_malloc (sizeof(queue_t));
    if (q == NULL) {
        return NULL;
    }

    queue_init(q);

    return q;
}

/**
 * Frees an initialized queue that was allocated on the heap.
 */
static void tq_free_queue(queue_t* q)
{
    phoenix_mem_free(q);
}

/**
 * Empties out a queue in the task queue by dequeuing and freeing
 * every task.
 */
static void tq_empty_queue(queue_t* q)
{
    do {
        tq_entry_t      *entry;
        queue_elem_t    *queue_elem;

        if (queue_pop_front (q, &queue_elem) == 0)
            break;

        entry = queue_entry (queue_elem, tq_entry_t, queue_elem);
        assert (entry != NULL);
        phoenix_mem_free (entry);
    } while (1);
}

static inline void tq_finalize_normal(taskQ_t* tq)
{
    int i;

    assert (tq->queues != NULL);
    assert (tq->free_queues != NULL);
    assert (tq->locks != NULL);

    /* destroy all queues */
    for (i = 0; i < tq->num_queues; ++i) {
        tq_queue_destroy(tq, i);
    }

    /* destroy all first level pointers in tq */
    phoenix_mem_free (tq->queues);
    phoenix_mem_free (tq->free_queues);
    phoenix_mem_free (tq->locks);
    phoenix_mem_free (tq->seeds);

    /* finally kill tq */
    phoenix_mem_free (tq);
}

void tq_finalize (taskQ_t* tq)
{
    tq_finalize_normal(tq);
}

/* Queue TASK at LGRP task queue with locking.
   LGRP is a locality hint denoting to which locality group this task 
   should be queued at. If LGRP is less than 0, the locality group is 
   randomly selected. TID is required for MCS locking. */
int tq_enqueue (taskQ_t* tq, task_t *task, int lgrp, int tid)
{
    tq_entry_t      *entry;
    int             index;

    assert (tq != NULL);
    assert (task != NULL);

    entry = (tq_entry_t *)phoenix_mem_malloc (sizeof (tq_entry_t));
    if (entry == NULL) {
        return -1;
    }

    mem_memcpy (&entry->task, task, sizeof (task_t));

    index = (lgrp < 0) ? rand_r(&tq->seeds[tid]) : lgrp;
    index %= tq->num_queues;

    lock_acquire (tq->locks[index].per_thread[tid]);
    queue_push_back (tq->queues[index], &entry->queue_elem);
    lock_release (tq->locks[index].per_thread[tid]);

    return 0;
}

/* Queue TASK at LGRP task queue without locking.
   LGRP is a locality hint denoting to which locality group this task
   should be queued at. If LGRP is less than 0, the locality group is
   randomly selected. */
int tq_enqueue_seq (taskQ_t* tq, task_t *task, int lgrp)
{
    tq_entry_t      *entry;
    int             index;

    assert (task != NULL);

    entry = (tq_entry_t *)phoenix_mem_malloc (sizeof (tq_entry_t));
    if (entry == NULL) {
        return -1;
    }

    mem_memcpy (&entry->task, task, sizeof (task_t));

    index = (lgrp < 0) ? rand() % tq->num_queues : lgrp % tq->num_queues;
    queue_push_back (tq->queues[index], &entry->queue_elem);

    return 0;
}

/**
 * Safely dequeues an element from the normal queue and queues onto free queue
 * @param tq        taskQ to operate on
 * @param idx       index of queue to use
 * @param tid       task id
 * @param qe        queue element of element we operated on
 * @return nonzero if normal queue was empty
 */
static inline int tq_elem_into_free_seq (
    taskQ_t* tq, int idx, int tid, queue_elem_t** qe)
{
    queue_elem_t    *queue_elem = NULL;
    int             ret;

    ret = queue_pop_front (tq->queues[idx], &queue_elem);
    if (ret != 0)
        queue_push_back (tq->free_queues[idx], queue_elem);

    *qe = queue_elem;

    return ret;
}

/**
 * Safely dequeues an element from the normal queue and queues onto free queue
 * @param tq        taskQ to operate on
 * @param idx       index of queue to use
 * @param tid       task id
 * @param qe        queue element of element we operated on
 * @return nonzero if normal queue was empty
 */
static inline int tq_elem_into_free (
    taskQ_t* tq, int idx, int tid, queue_elem_t** qe)
{
    int             ret;

    lock_acquire (tq->locks[idx].per_thread[tid]);
    ret = tq_elem_into_free_seq (tq, idx, tid, qe);
    lock_release (tq->locks[idx].per_thread[tid]);

    return ret;
}

static inline int tq_dequeue_normal_seq (
    taskQ_t* tq, task_t* task, int lgrp, int tid)
{
    return tq_dequeue_normal_internal (
        tq, task, lgrp, tid, tq_elem_into_free_seq);
}

static inline int tq_dequeue_normal(
    taskQ_t* tq, task_t* task, int lgrp, int tid)
{
    return tq_dequeue_normal_internal (
        tq, task, lgrp, tid, tq_elem_into_free);
}

static inline int tq_dequeue_normal_internal (
    taskQ_t* tq, task_t* task, int lgrp, int tid, dequeue_fn dequeue_fn)
{
    int             i, ret, index;
    queue_elem_t    *queue_elem;
    tq_entry_t      *entry;

    assert (task != NULL);

    mem_memset (task, 0, sizeof (task_t));

    index = (lgrp < 0) ? rand_r(&tq->seeds[tid]) : lgrp;
    index %= tq->num_queues;
    ret = (*dequeue_fn)(tq, index, tid, &queue_elem);

   /* Do task stealing if nothing on our queue.
      Cycle through all indexes until success or exhaustion */
    for (i = (index + 1) % tq->num_queues;
        (ret == 0) && (i != index);
        i = (i + 1) % tq->num_queues)
    {
        ret = (*dequeue_fn)(tq, i, tid, &queue_elem);
    }

    if (ret == 0) {
        /* There really is no more work. */
        return 0;
    }

    entry = queue_entry (queue_elem, tq_entry_t, queue_elem);
    assert (entry != NULL);

    mem_memcpy (task, &entry->task, sizeof (task_t));

    return 1;
}

int tq_dequeue (taskQ_t* tq, task_t *task, int lgrp, int tid)
{
    return tq_dequeue_normal(tq, task, lgrp, tid);
}
