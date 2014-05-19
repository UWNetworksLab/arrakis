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
#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <inttypes.h>

#include "map_reduce.h"
#include "memory.h"
#include "processor.h"
#include "defines.h"
#include "scheduler.h"
#include "synch.h"
#include "taskQ.h"
#include "queue.h"
#include "stddefines.h"
#include "iterator.h"
#include "locality.h"
#include "struct.h"
#include "tpool.h"

#if !defined(_LINUX_) && !defined(_SOLARIS_) && !defined(BARRELFISH)
#error OS not supported
#endif

/* Begin tunables. */
//#define INCREMENTAL_COMBINER

#define DEFAULT_NUM_REDUCE_TASKS    256
#define EXTENDED_NUM_REDUCE_TASKS   (DEFAULT_NUM_REDUCE_TASKS * 128)
#define DEFAULT_CACHE_SIZE          (64 * 1024)
//#define DEFAULT_CACHE_SIZE        (8 * 1024)
#define DEFAULT_KEYVAL_ARR_LEN      10
#define DEFAULT_VALS_ARR_LEN        10
#define L2_CACHE_LINE_SIZE          64
/* End tunables. */

/* Debug printf */
#ifdef dprintf
#undef dprintf
#define dprintf(...) //printf(__VA_ARGS__)
#endif

#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define OUT_PREFIX "[Phoenix] "

/* A key and a value pair. */
typedef struct 
{
    /* TODO add static assertion to make sure this fits in L2 line */
    union {
        struct {
            int         len;
            int         alloc_len;
            int         pos;
            keyval_t    *arr;
        };
        char pad[L2_CACHE_LINE_SIZE];
    };
} keyval_arr_t;

/* Array of keyvals_t. */
typedef struct 
{
    int len;
    int alloc_len;
    int pos;
    keyvals_t *arr;
} keyvals_arr_t;

/* Thread information.
   Denotes the id and the assigned CPU of a thread. */
typedef struct 
{
    union {
        struct {
            struct thread *tid;
            int curr_task;
        };
        char pad[L2_CACHE_LINE_SIZE];
    };
} thread_info_t;

typedef struct
{
    uintptr_t work_time;
    uintptr_t user_time;
    uintptr_t combiner_time;
} thread_timing_t;

typedef struct {
    task_t          task;
    queue_elem_t    queue_elem;
} task_queued;

/* Internal map reduce state. */
typedef struct
{
    /* Parameters. */
    int num_map_tasks;              /* # of map tasks. */
    int num_reduce_tasks;           /* # of reduce tasks. */
    int chunk_size;                 /* # of units of data for each map task. */
    int num_procs;                  /* # of processors to run on. */
    int num_map_threads;            /* # of threads for map tasks. */
    int num_reduce_threads;         /* # of threads for reduce tasks. */
    int num_merge_threads;          /* # of threads for merge tasks. */
    float key_match_factor;         /* # of values likely to be matched 
                                       to the same key. */

    bool oneOutputQueuePerMapTask;      /* One output queue per map task? */
    bool oneOutputQueuePerReduceTask;   /* One output queue per reduce task? */

    int intermediate_task_alloc_len;

    /* Callbacks. */
    map_t map;                      /* Map function. */
    reduce_t reduce;                /* Reduce function. */
    combiner_t combiner;            /* Combiner function. */
    partition_t partition;          /* Partition function. */     
    splitter_t splitter;            /* Splitter function. */
    locator_t locator;              /* Locator function. */
    key_cmp_t key_cmp;              /* Key comparator function. */

    /* Structures. */
    map_reduce_args_t * args;       /* Args passed in by the user. */
    thread_info_t * tinfo;          /* Thread information array. */

    keyvals_arr_t **intermediate_vals;
                                    /* Array to send to reduce task. */

    keyval_arr_t *final_vals;       /* Array to send to merge task. */
    keyval_arr_t *merge_vals;       /* Array to send to user. */

    uintptr_t splitter_pos;         /* Tracks position in array_splitter(). */

    /* Policy for mapping threads to cpus. */
    sched_policy    *schedPolicies[TASK_TYPE_TOTAL];


    taskQ_t         *taskQueue;     /* Queues of tasks. */
    tpool_t         *tpool;         /* Thread pool. */
} mr_env_t;

#ifdef TIMING
static pthread_key_t emit_time_key;
#endif
struct tls {
    tpool_t     *tpool;
    mr_env_t    *env;
};

/* Data passed on to each worker thread. */
typedef struct
{
    int             cpu_id;             /* CPU this thread is to run. */
    int             thread_id;          /* Thread index. */
    TASK_TYPE_T     task_type;          /* Assigned task type. */
    int             merge_len;
    keyval_arr_t    *merge_input;
    int             merge_round;
    mr_env_t        *env;
} thread_arg_t;

static inline mr_env_t* env_init (map_reduce_args_t *);
static void env_fini(mr_env_t* env);
static inline void env_print (mr_env_t* env);
static inline void start_workers (mr_env_t* env, thread_arg_t *);
static inline void *start_my_work (thread_arg_t *);
static inline void emit_inline (mr_env_t* env, void *, void *);
static inline mr_env_t* get_env(void);
static inline int getCurrThreadIndex (TASK_TYPE_T);
static inline int getNumTaskThreads (mr_env_t* env, TASK_TYPE_T);
static inline void insert_keyval (
    mr_env_t* env, keyval_arr_t *, void *, void *);
static inline void insert_keyval_merged (
    mr_env_t* env, keyvals_arr_t *, void *, void *);

static int array_splitter (void *, int, map_args_t *);
static void identity_reduce (void *, iterator_t *itr);
static inline void merge_results (mr_env_t* env, keyval_arr_t*, int);

static void *map_worker (void *);
static void *reduce_worker (void *);
static void *merge_worker (void *);

static int gen_map_tasks (mr_env_t* env);
static int gen_map_tasks_split(mr_env_t* env, queue_t* q);
static int gen_reduce_tasks (mr_env_t* env);

static void map(mr_env_t* mr);
static void reduce(mr_env_t* mr);
static void merge(mr_env_t* mr);

#ifndef INCREMENTAL_COMBINER
static void run_combiner (mr_env_t* env, int thread_idx);
#endif

int 
map_reduce_init ()
{
    struct tls *tls = calloc(1, sizeof(tls));
    thread_set_tls(tls);
    /* CHECK_ERROR (pthread_key_create (&tpool_key, NULL)); */

    /* CHECK_ERROR (pthread_setspecific (tpool_key, NULL)); */

    return 0;
}

int
map_reduce (map_reduce_args_t * args)
{
    struct timeval begin, end;
    mr_env_t* env;

    assert (args != NULL);
    assert (args->map != NULL);
    assert (args->key_cmp != NULL);
    assert (args->unit_size > 0);
    assert (args->result != NULL);

    get_time (&begin);

    /* Initialize environment. */
    env = env_init (args);
    if (env == NULL) {
       /* could not allocate environment */
       return -1;
    }
    //env_print (env);
    env->taskQueue = tq_init (env->num_map_threads);
    assert (env->taskQueue != NULL);

    /* Reuse thread pool. */
    struct tls *tls = thread_get_tls();
    if(tls == NULL) {
        tls = calloc(1, sizeof(struct tls));
        thread_set_tls(tls);
    }
    env->tpool = tls->tpool;
    if (env->tpool == NULL) {
        tpool_t *tpool;

        tpool = tpool_create (env->num_map_threads);
        CHECK_ERROR (tpool == NULL);

        env->tpool = tpool;
        tls->tpool = tpool;
    }

#ifdef TIMING
    CHECK_ERROR (pthread_key_create (&emit_time_key, NULL));
#endif
    tls->env = env;

    get_time (&end);

#ifdef TIMING
    fprintf (stderr, "library init: %u\n", time_diff (&end, &begin));
#endif

    /* Run map tasks and get intermediate values. */
    get_time (&begin);
    map (env);
    get_time (&end);

#ifdef TIMING
    fprintf (stderr, "map phase: %u\n", time_diff (&end, &begin));
#endif

    dprintf("In scheduler, all map tasks are done, now scheduling reduce tasks\n");
    
    /* Run reduce tasks and get final values. */
    get_time (&begin);
    reduce (env);
    get_time (&end);

#ifdef TIMING
    fprintf (stderr, "reduce phase: %u\n", time_diff (&end, &begin));
#endif

    dprintf("In scheduler, all reduce tasks are done, now scheduling merge tasks\n");

    get_time (&begin);
    merge (env);
    get_time (&end);

#ifdef TIMING
    fprintf (stderr, "merge phase: %u\n", time_diff (&end, &begin));
#endif

    /* Cleanup. */
    get_time (&begin);
    env_fini(env);
    get_time (&end);

#ifdef TIMING
    fprintf (stderr, "library finalize: %u\n", time_diff (&end, &begin));
    CHECK_ERROR (pthread_key_delete (emit_time_key));
#endif

    return 0;
}

int map_reduce_finalize ()
{
    tpool_t *tpool;

    struct tls *tls = thread_get_tls();
    tpool = tls->tpool;
    CHECK_ERROR (tpool_destroy (tpool));

    return 0;
}

/**
 * Frees memory used by map reduce environment once map reduce has completed.
 * Frees environment pointer.
 */
static void env_fini (mr_env_t* env)
{
    int i;

    tq_finalize (env->taskQueue);

    for (i = 0; i < TASK_TYPE_TOTAL; i++)
        sched_policy_put(env->schedPolicies[i]);

    phoenix_mem_free (env);
}

/* Setup global state. */
static mr_env_t* 
env_init (map_reduce_args_t *args) 
{
    mr_env_t    *env;
    int         i;
    int         num_procs;

    env = phoenix_mem_malloc (sizeof (mr_env_t));
    if (env == NULL) {
       return NULL;
    }

    mem_memset (env, 0, sizeof (mr_env_t));

    env->args = args;

    /* 1. Determine paramenters. */

    /* Determine the number of processors to use. */
    num_procs = proc_get_num_cpus ();
    if (args->num_procs > 0)
    {
        /* Can't have more processors than there are physically present. */
        CHECK_ERROR (args->num_procs > num_procs);
        num_procs = args->num_procs;
    }
    env->num_procs = num_procs;

    env->oneOutputQueuePerMapTask = false;
    env->oneOutputQueuePerReduceTask = args->use_one_queue_per_task;

    /* Determine the number of threads to schedule for each type of task. */
    env->num_map_threads = (args->num_map_threads > 0) ? 
        args->num_map_threads : num_procs;

    env->num_reduce_threads = (args->num_reduce_threads > 0) ? 
        args->num_reduce_threads : num_procs;

    env->num_merge_threads = (args->num_merge_threads > 0) ? 
        args->num_merge_threads : env->num_reduce_threads;

    if (env->oneOutputQueuePerReduceTask == false) {
        env->num_merge_threads = env->num_reduce_threads / 2;
    }

    /* Assign at least one merge thread. */
    env->num_merge_threads = MAX(env->num_merge_threads, 1);

    env->key_match_factor = (args->key_match_factor > 0) ? 
        args->key_match_factor : 2;

    /* Set num_map_tasks to 0 since we cannot anticipate how many map tasks
       there would be. This does not matter since map workers will loop
       until there is no more data left. */
    env->num_map_tasks = 0;

    if (args->L1_cache_size > 0)
    {
        env->chunk_size = args->L1_cache_size / args->unit_size;
        env->num_reduce_tasks = (int)
            ( (env->key_match_factor * args->data_size) /
                args->L1_cache_size);
    }
    else
    {
        env->chunk_size = DEFAULT_CACHE_SIZE / args->unit_size;
        env->num_reduce_tasks = (int) 
            ( (env->key_match_factor * args->data_size) / 
                DEFAULT_CACHE_SIZE );
    }

    if (env->num_reduce_tasks <= 0) env->num_reduce_tasks = 1;
    if (env->chunk_size <= 0) env->chunk_size = 1;

    if (env->oneOutputQueuePerReduceTask == false) 
    {
        env->num_reduce_tasks = EXTENDED_NUM_REDUCE_TASKS;
    } 
    else 
    {
        env->num_reduce_tasks = DEFAULT_NUM_REDUCE_TASKS;
    }
    env->num_merge_threads = MIN (
        env->num_merge_threads, env->num_reduce_tasks / 2);

    if (env->oneOutputQueuePerMapTask) 
        env->intermediate_task_alloc_len = 
            args->data_size / env->chunk_size + 1;
    else
        env->intermediate_task_alloc_len = env->num_map_threads;

    /* Register callbacks. */
    env->map = args->map;
    env->reduce = (args->reduce) ? args->reduce : identity_reduce;
    env->combiner = args->combiner;
    env->partition = (args->partition) ? args->partition : default_partition;
    env->splitter = (args->splitter) ? args->splitter : array_splitter;
    env->locator = args->locator;
    env->key_cmp = args->key_cmp;

    /* 2. Initialize structures. */

    env->intermediate_vals = (keyvals_arr_t **)phoenix_mem_malloc (
        env->intermediate_task_alloc_len * sizeof (keyvals_arr_t*));

    for (i = 0; i < env->intermediate_task_alloc_len; i++)
    {
        env->intermediate_vals[i] = (keyvals_arr_t *)phoenix_mem_calloc (
            env->num_reduce_tasks, sizeof (keyvals_arr_t));
    }

    if (env->oneOutputQueuePerReduceTask)
    {
        env->final_vals = 
            (keyval_arr_t *)phoenix_mem_calloc (
                env->num_reduce_tasks, sizeof (keyval_arr_t));
    }
    else
    {
        env->final_vals =
            (keyval_arr_t *)phoenix_mem_calloc (
                env->num_reduce_threads, sizeof (keyval_arr_t));
    }

    for (i = 0; i < TASK_TYPE_TOTAL; i++) {
        /* TODO: Make this tunable */
        env->schedPolicies[i] = sched_policy_get(SCHED_POLICY_STRAND_FILL);
    }

    return env;
}

void env_print (mr_env_t* env)
{
    printf (OUT_PREFIX "num_reduce_tasks = %u\n", env->num_reduce_tasks);
    printf (OUT_PREFIX "num_procs = %u\n", env->num_procs);
    printf (OUT_PREFIX "proc_offset = %u\n", env->args->proc_offset);
    printf (OUT_PREFIX "num_map_threads = %u\n", env->num_map_threads);
    printf (OUT_PREFIX "num_reduce_threads = %u\n", env->num_reduce_threads);
    printf (OUT_PREFIX "num_merge_threads = %u\n", env->num_merge_threads);
}

/**
 * Execute the same function as worker.
 * @param th_arg    arguments
 */
static void *start_my_work (thread_arg_t* th_arg)
{
    switch (th_arg->task_type) {
    case TASK_TYPE_MAP:
        return map_worker (th_arg);
        break;
    case TASK_TYPE_REDUCE:
        return reduce_worker (th_arg);
        break;
    case TASK_TYPE_MERGE:
        return merge_worker (th_arg);
        break;
    default:
        assert (0);
        break;
    }
}

static void start_thread_pool (
    tpool_t *tpool, TASK_TYPE_T task_type, thread_arg_t** th_arg_array, int num_workers)
{
    thread_func     thread_func;

    switch (task_type) {
    case TASK_TYPE_MAP:
        thread_func = map_worker;
        break;
    case TASK_TYPE_REDUCE:
        thread_func = reduce_worker;
        break;
    case TASK_TYPE_MERGE:    
        thread_func = merge_worker;
        break;
    default:
        assert (0);
        break;
    }

    CHECK_ERROR (tpool_set (tpool, thread_func, (void **)th_arg_array, num_workers));
    CHECK_ERROR (tpool_begin (tpool));
}

/** start_workers()
 *  thread_func - function pointer to process splitter data
 *  splitter_func - splitter function pointer
 *  splitter_init - splitter_init function pointer
 *  runs map tasks in a new thread on each the available processors.
 *  returns pointer intermediate value array 
 */
static void 
start_workers (mr_env_t* env, thread_arg_t *th_arg)
{
    int             thread_index;
    TASK_TYPE_T     task_type;
    int             num_threads;
    int             cpu;
    intptr_t        ret_val;
    thread_arg_t    **th_arg_array;
    void            **rets;
#ifdef TIMING
    uint64_t        work_time = 0;
    uint64_t        user_time = 0;
    uint64_t        combiner_time = 0;
#endif

    assert(th_arg != NULL);

    task_type = th_arg->task_type;
    num_threads = getNumTaskThreads (env, task_type);

    env->tinfo = (thread_info_t *)phoenix_mem_calloc (
        num_threads, sizeof (thread_info_t));
    th_arg->env = env;

    th_arg_array = (thread_arg_t **)phoenix_mem_malloc (
        sizeof (thread_arg_t *) * num_threads);
    CHECK_ERROR (th_arg_array == NULL);

    for (thread_index = 0; thread_index < num_threads; ++thread_index) {

        cpu = sched_thr_to_cpu (env->schedPolicies[task_type], thread_index + env->args->proc_offset);
        th_arg->cpu_id = cpu;
        th_arg->thread_id = thread_index;

        th_arg_array[thread_index] = phoenix_mem_malloc (sizeof (thread_arg_t));
        CHECK_ERROR (th_arg_array[thread_index] == NULL);
        mem_memcpy (th_arg_array[thread_index], th_arg, sizeof (thread_arg_t));
    }

    start_thread_pool (
        env->tpool, task_type, &th_arg_array[1], num_threads - 1);

    dprintf("Status: All %d threads have been created\n", num_threads);

    ret_val = (intptr_t)start_my_work (th_arg_array[0]);
#ifdef TIMING
    thread_timing_t *timing = (thread_timing_t *)ret_val;
    work_time += timing->work_time;
    user_time += timing->user_time;
    combiner_time += timing->combiner_time;
    phoenix_mem_free (timing);
#endif
    phoenix_mem_free (th_arg_array[0]);

    /* Barrier, wait for all threads to finish. */
    CHECK_ERROR (tpool_wait (env->tpool));
    rets = tpool_get_results (env->tpool);

    for (thread_index = 1; thread_index < num_threads; ++thread_index)
    {
#ifdef TIMING
        ret_val = (intptr_t)rets[thread_index - 1];
        thread_timing_t *timing = (thread_timing_t *)ret_val;
        work_time += timing->work_time;
        user_time += timing->user_time;
        combiner_time += timing->combiner_time;
        phoenix_mem_free (timing);
#endif
        phoenix_mem_free (th_arg_array[thread_index]);
    }

    phoenix_mem_free (th_arg_array);
    phoenix_mem_free (rets);

#ifdef TIMING
    switch (task_type)
    {
        case TASK_TYPE_MAP:
            fprintf (stderr, "map work time: %" PRIu64 "\n",
                                        work_time / num_threads);
            fprintf (stderr, "map user time: %" PRIu64 "\n", 
                                        user_time / num_threads);
            fprintf (stderr, "map combiner time: %" PRIu64 "\n", 
                                        combiner_time / num_threads);
            break;

        case TASK_TYPE_REDUCE:
            fprintf (stderr, "reduce work time: %" PRIu64 "\n",
                                        work_time / num_threads);
            fprintf (stderr, "reduce user time: %" PRIu64 "\n", 
                                        user_time / num_threads);
            break;

        case TASK_TYPE_MERGE:
            fprintf (stderr, "merge work time: %" PRIu64 "\n",
                                        work_time / num_threads);

        default:
            break;
    }
#endif

    phoenix_mem_free(env->tinfo);
    dprintf("Status: All tasks have completed\n"); 
}

typedef struct {
    uint64_t            run_time;
    int                 lgrp;
} map_worker_task_args_t;

/**
 * Dequeue the latest task and run it
 * @return true if ran a task, false otherwise
 */
static bool map_worker_do_next_task (
    mr_env_t *env, int thread_index, map_worker_task_args_t *args)
{
    struct timeval  begin, end;
    int             alloc_len;
    int             curr_task;
    task_t          map_task;
    map_args_t      thread_func_arg;
    bool            oneOutputQueuePerMapTask;
    int             lgrp = args->lgrp;

    oneOutputQueuePerMapTask = env->oneOutputQueuePerMapTask;

    alloc_len = env->intermediate_task_alloc_len;

    /* Get new map task. */
    if (tq_dequeue (env->taskQueue, &map_task, lgrp, thread_index) == 0) {
        /* no more map tasks */
        return false;
    }

    curr_task = env->num_map_tasks++;
    env->tinfo[thread_index].curr_task = curr_task;

    thread_func_arg.length = map_task.len;
    thread_func_arg.data = (void *)map_task.data;

    dprintf("Task %d: cpu_id -> %d - Started\n", curr_task, th_arg->cpu_id);

    /* Perform map task. */
    get_time (&begin);
    env->map (&thread_func_arg);
    get_time (&end);

#ifdef TIMING
    args->run_time = time_diff (&end, &begin);
#endif

    dprintf("Task %d: cpu_id -> %d - Done\n", curr_task, th_arg->cpu_id);

    return true;
}

/** 
 * map_worker()
 * args - pointer to thread_arg_t
 * returns 0 on success
 * This runs thread_func() until there is no more data from the splitter().
 * The pointer to results are stored in return_values array.
 */
static void *
map_worker (void *args)
{
    assert (args != NULL);

    struct timeval          begin, end;
    struct timeval          work_begin, work_end;
    uintptr_t               user_time = 0;
    thread_arg_t            *th_arg = (thread_arg_t *)args;
    mr_env_t                *env = th_arg->env;
    int                     thread_index = th_arg->thread_id;
    int                     num_assigned = 0;
    map_worker_task_args_t  mwta;
#ifdef TIMING
    uintptr_t               work_time = 0;
    uintptr_t               combiner_time = 0;
#endif

    env->tinfo[thread_index].tid = thread_self();

    /* Bind thread. */
    CHECK_ERROR (proc_bind_thread (th_arg->cpu_id) != 0);

    struct tls *tls = thread_get_tls();
    if(tls == NULL) {
        tls = calloc(1, sizeof(struct tls));
        thread_set_tls(tls);
    }

    tls->env = env;
#ifdef TIMING
    CHECK_ERROR (pthread_setspecific (emit_time_key, 0));
#endif

    mwta.lgrp = loc_get_lgrp();

    get_time (&work_begin);
    while (map_worker_do_next_task (env, thread_index, &mwta)) {
        user_time += mwta.run_time;
        num_assigned++;
    }
    get_time (&work_end);

#ifdef TIMING
    work_time = time_diff (&work_end, &work_begin);
#endif

    get_time (&begin);

    /* Apply combiner to local map results. */
#ifndef INCREMENTAL_COMBINER
    if (env->combiner != NULL)
        run_combiner (env, thread_index);
#endif

    get_time (&end);

#ifdef TIMING
    combiner_time = time_diff (&end, &begin);
#endif

    dprintf("Status: Total of %d tasks were assigned to cpu_id %d\n", 
        num_assigned, th_arg->cpu_id);

    /* Unbind thread. */
    CHECK_ERROR (proc_unbind_thread () != 0);

#ifdef TIMING
    thread_timing_t *timing = calloc (1, sizeof (thread_timing_t));
    uintptr_t emit_time = (uintptr_t)pthread_getspecific (emit_time_key);
    timing->user_time = user_time - emit_time;
    timing->work_time = work_time - timing->user_time;
    timing->combiner_time = combiner_time;
    return (void *)timing;
#else
    return (void *)0;
#endif
}

typedef struct {
    struct iterator_t   itr;
    uint64_t            run_time;
    int                 num_map_threads;
    int                 lgrp;
} reduce_worker_task_args_t;

/**
 * Dequeue next reduce task and do it
 * TODO Refactor this even more. It is still gross.
 * @return true if did work, false otherwise
 */
static bool reduce_worker_do_next_task (
    mr_env_t *env, int thread_index, reduce_worker_task_args_t *args)
{
    struct timeval  begin, end;
    intptr_t        curr_reduce_task = 0;
    keyvals_t       *min_key_val, *next_min;
    task_t          reduce_task;
    int             num_map_threads;
    int             curr_thread;
    int             lgrp = args->lgrp;

    /* Get the next reduce task. */
    if (tq_dequeue (env->taskQueue, &reduce_task, lgrp, thread_index) == 0) {
        /* No more reduce tasks. */
        return false;
    }

    curr_reduce_task = (intptr_t)reduce_task.id;

    env->tinfo[thread_index].curr_task = curr_reduce_task;

    num_map_threads =  args->num_map_threads;

    args->run_time = 0;
    min_key_val = NULL;
    next_min = NULL;

    do {
        for (curr_thread = 0; curr_thread < num_map_threads; curr_thread++) {
            keyvals_t       *curr_key_val;
            keyvals_arr_t   *thread_array;

            /* Find the next array to search. */
            thread_array = 
                &env->intermediate_vals[curr_thread][curr_reduce_task];

            /* Check if the current processor array has been 
               completely searched. */
            if (thread_array->pos >= thread_array->len) {
                continue;
            }

            /* Get the next key in the processor array. */
            curr_key_val = &thread_array->arr[thread_array->pos];

            /* If the key matches the minimum value, 
               add the value to the list of values for that key. */
            if (min_key_val != NULL &&
                !env->key_cmp(curr_key_val->key, min_key_val->key)) {
                CHECK_ERROR (iter_add (&args->itr, curr_key_val));
                thread_array->pos += 1;
                --curr_thread;
            }
            /* Find the location of the next min. */
            else if (next_min == NULL || 
                env->key_cmp(curr_key_val->key, next_min->key) < 0)
            {
                next_min = curr_key_val;
            }
        }

        if (min_key_val != NULL) {
            keyvals_t       *curr_key_val;

            if (env->reduce != identity_reduce) {
                get_time (&begin);
                env->reduce (min_key_val->key, &args->itr);
                get_time (&end);
#ifdef TIMING
                args->run_time += time_diff (&end, &begin);
#endif
            } else {
                env->reduce (min_key_val->key, &args->itr);
            }

            /* Free up memory */
            iter_rewind (&args->itr);
            while (iter_next_list (&args->itr, &curr_key_val)) {
                val_t   *vals, *next;

                vals = curr_key_val->vals;
                while (vals != NULL) {
                    next = vals->next_val;
                    phoenix_mem_free (vals);
                    vals = next;
                }
            }

            iter_reset(&args->itr);
        }

        min_key_val = next_min;
        next_min = NULL;

        /* See if there are any elements left. */
        for(curr_thread = 0;
            curr_thread < num_map_threads &&
            env->intermediate_vals[curr_thread][curr_reduce_task].pos >=
            env->intermediate_vals[curr_thread][curr_reduce_task].len;
            curr_thread++);
    } while (curr_thread != num_map_threads);

    /* Free up the memory. */
    for (curr_thread = 0; curr_thread < num_map_threads; curr_thread++) {
        keyvals_arr_t   *arr;

        arr = &env->intermediate_vals[curr_thread][curr_reduce_task];
        if (arr->alloc_len != 0)
            phoenix_mem_free(arr->arr);
    }

    return true;
}

static void *
reduce_worker (void *args)
{
    assert(args != NULL);

    struct timeval              work_begin, work_end;
    uintptr_t                   user_time = 0;
    thread_arg_t                *th_arg = (thread_arg_t *)args;
    int                         thread_index = th_arg->thread_id;
    mr_env_t                    *env = th_arg->env;
    reduce_worker_task_args_t   rwta;
    int                         num_map_threads;
#ifdef TIMING
    uintptr_t                   work_time = 0;
#endif

    env->tinfo[thread_index].tid = thread_self();

    /* Bind thread. */
    CHECK_ERROR (proc_bind_thread (th_arg->cpu_id) != 0);

    struct tls *tls = thread_get_tls();
    if(tls == NULL) {
        tls = calloc(1, sizeof(struct tls));
        thread_set_tls(tls);
    }
    tls->env = env;
#ifdef TIMING
    CHECK_ERROR (pthread_setspecific (emit_time_key, 0));
#endif

    if (env->oneOutputQueuePerMapTask)
        num_map_threads = env->num_map_tasks;
    else
        num_map_threads = env->num_map_threads;

    /* Assuming !oneOutputQueuePerMapTask */
    CHECK_ERROR (iter_init (&rwta.itr, env->num_map_threads));
    rwta.num_map_threads = num_map_threads;
    rwta.lgrp = loc_get_lgrp();

    get_time (&work_begin);

    while (reduce_worker_do_next_task (env, thread_index, &rwta)) {
        user_time += rwta.run_time;
    }

    get_time (&work_end);

#ifdef TIMING
    work_time = time_diff (&work_end, &work_begin);
#endif

    iter_finalize (&rwta.itr);

    /* Unbind thread. */
    CHECK_ERROR (proc_unbind_thread () != 0);

#ifdef TIMING
    thread_timing_t *timing = calloc (1, sizeof (thread_timing_t));
    uintptr_t emit_time = (uintptr_t)pthread_getspecific (emit_time_key);
    timing->user_time = user_time - emit_time;
    timing->work_time = work_time - timing->user_time;
    return (void *)timing;
#else
    return (void *)0;
#endif
}

/** merge_worker()
* args - pointer to thread_arg_t
* returns 0 on success
*/
static void *
merge_worker (void *args) 
{
    assert(args != NULL);

    struct timeval  work_begin, work_end;
    thread_arg_t    *th_arg = (thread_arg_t *)args;
    int             thread_index = th_arg->thread_id;
    mr_env_t        *env = th_arg->env;
    int             cpu;
#ifdef TIMING
    uintptr_t       work_time = 0;
#endif

    env->tinfo[thread_index].tid = thread_self();

    /* Bind thread.
       Spread out the merge workers as much as possible. */
    if (env->oneOutputQueuePerReduceTask)
        cpu = th_arg->cpu_id * (1 << (th_arg->merge_round - 1));
    else
        cpu = th_arg->cpu_id * (1 << th_arg->merge_round);

    CHECK_ERROR (proc_bind_thread (cpu) != 0);

    struct tls *tls = thread_get_tls();
    if(tls == NULL) {
        tls = calloc(1, sizeof(struct tls));
        thread_set_tls(tls);
    }
    tls->env = env;

    /* Assumes num_merge_threads is modified before each call. */
    int length = th_arg->merge_len / env->num_merge_threads;
    int modlen = th_arg->merge_len % env->num_merge_threads;

    /* Let's make some progress here. */
    if (length <= 1) {
        length = 2;
        modlen = th_arg->merge_len % 2;
    }

    int pos = thread_index * length + 
                ((thread_index < modlen) ? thread_index : modlen);

    if (pos < th_arg->merge_len) {

        keyval_arr_t *vals = &th_arg->merge_input[pos];

        dprintf("Thread %d: cpu_id -> %d - Started\n", 
                    thread_index, th_arg->cpu_id);

        get_time (&work_begin);
        merge_results (th_arg->env, vals, length + (thread_index < modlen));
        get_time (&work_end);

#ifdef TIMING
        work_time = time_diff (&work_end, &work_begin);
#endif

        dprintf("Thread %d: cpu_id -> %d - Done\n", 
                    thread_index, th_arg->cpu_id);
    }

    /* Unbind thread. */
    CHECK_ERROR (proc_unbind_thread () != 0);

#ifdef TIMING
    thread_timing_t *timing = calloc (1, sizeof (thread_timing_t));
    timing->work_time = work_time;
    return (void *)timing;
#else
    return (void *)0;
#endif
}

/**
 * Split phase of map task generation, creates all tasks and throws in single
 * queue.
 *
 * @param q     queue to place tasks into
 * @return number of tasks generated, or 0 on error
 */
static int gen_map_tasks_split (mr_env_t* env, queue_t* q)
{
    int                 cur_task_id;
    map_args_t          args;
    task_queued         *task = NULL;

    /* split until complete */
    cur_task_id = 0;
    while (env->splitter (env->args->task_data, env->chunk_size, &args))
    {
        task = (task_queued *)phoenix_mem_malloc (sizeof (task_queued));
        task->task.id = cur_task_id;
        task->task.len = (uint64_t)args.length;
        task->task.data = (uint64_t)args.data;

        queue_push_back (q, &task->queue_elem);

        ++cur_task_id;
    }

    if (task == NULL) {
        /* not enough memory, undo what's been done, error out */
        queue_elem_t    *queue_elem;

        while (queue_pop_front (q, &queue_elem))
        {
            task = queue_entry (queue_elem, task_queued, queue_elem);
            assert (task != NULL);
            phoenix_mem_free (task);
        }

        return 0;
    }

    return cur_task_id;
}

/**
 * User provided own splitter function but did not supply a locator function.
 * Nothing to do here about locality, so just try to put consecutive tasks
 * in the same task queue.
 */
static int gen_map_tasks_distribute_lgrp (
    mr_env_t* env, int num_map_tasks, queue_t* q)
{
    queue_elem_t    *queue_elem;
    int             tasks_per_lgrp;
    int             tasks_leftover;
    int             num_lgrps;
    int             lgrp;

    num_lgrps = env->num_map_threads / loc_get_lgrp_size();
    if (num_lgrps == 0) num_lgrps = 1;

    tasks_per_lgrp = num_map_tasks / num_lgrps;
    tasks_leftover = num_map_tasks - tasks_per_lgrp * num_lgrps;

    /* distribute tasks across locality groups */
    for (lgrp = 0; lgrp < num_lgrps; ++lgrp)
    {
        int remaining_cur_lgrp_tasks;

        remaining_cur_lgrp_tasks = tasks_per_lgrp;
        if (tasks_leftover > 0) {
            remaining_cur_lgrp_tasks++;
            tasks_leftover--;
        }
        do {
            task_queued *task;

            if (queue_pop_front (q, &queue_elem) == 0) {
                /* queue is empty, everything is distributed */
                break;
            }

            task = queue_entry (queue_elem, task_queued, queue_elem);
            assert (task != NULL);

            if (tq_enqueue_seq (env->taskQueue, &task->task, lgrp) < 0) {
                phoenix_mem_free (task);
                return -1;
            }

            phoenix_mem_free (task);
            remaining_cur_lgrp_tasks--;
        } while (remaining_cur_lgrp_tasks);

        if (remaining_cur_lgrp_tasks != 0) {
            break;
        }
    }

    return 0;
}

/**
 * We can try to queue tasks based on locality info
 */
static int gen_map_tasks_distribute_locator (
    mr_env_t* env, int num_map_tasks, queue_t* q)
{
    queue_elem_t    *queue_elem;

    while (queue_pop_front (q, &queue_elem))
    {
        task_queued *task;
        int         lgrp;
        map_args_t  args;

        task = queue_entry (queue_elem, task_queued, queue_elem);
        assert (task != NULL);

        args.length = task->task.len;
        args.data = (void*)task->task.data;

        if (env->locator != NULL) {
            void    *addr;
            addr = env->locator (&args);
            lgrp = loc_mem_to_lgrp (addr);
        } else {
            lgrp = loc_mem_to_lgrp (args.data);
        }

        task->task.v[3] = lgrp;         /* For debugging. */
        if (tq_enqueue_seq (env->taskQueue, &task->task, lgrp) != 0) {
            phoenix_mem_free (task);
            return -1;
        }

        phoenix_mem_free (task);
    }

    return 0;
}

/**
 * Distributes tasks to threads
 * @param num_map_tasks number of map tasks in queue
 * @param q queue of tasks to distribute
 * @return 0 on success, less than 0 on failure
 */
static int gen_map_tasks_distribute (
    mr_env_t* env, int num_map_tasks, queue_t* q)
{
    if ((env->splitter != array_splitter) && 
        (env->locator == NULL)) {
        return gen_map_tasks_distribute_lgrp (env, num_map_tasks, q);
    } else {
        return gen_map_tasks_distribute_locator (env, num_map_tasks, q);
    }

    return 0;
}

/**
 * Generate all map tasks and queue them up
 * @return number of map tasks created if successful, negative value on error
 */
static int gen_map_tasks (mr_env_t* env)
{
    int             ret;
    int             num_map_tasks;
    queue_t         temp_queue;
    int             num_map_threads;

    queue_init (&temp_queue);

    num_map_tasks = gen_map_tasks_split (env, &temp_queue);
    if (num_map_tasks <= 0) {
        return -1;
    }

    num_map_threads = env->num_map_threads;
    if (num_map_tasks < num_map_threads)
        num_map_threads = num_map_tasks;
    tq_reset (env->taskQueue, num_map_threads);

    ret = gen_map_tasks_distribute (env, num_map_tasks, &temp_queue);
    if (ret == 0) ret = num_map_tasks;

    return num_map_tasks;
}

static int gen_reduce_tasks (mr_env_t* env)
{
    int ret, tid;
    int tasks_per_thread;
    int tasks_leftover;
    uint64_t task_id;
    task_t reduce_task;

    tq_reset (env->taskQueue, env->num_reduce_threads);

    tasks_per_thread = env->num_reduce_tasks / env->num_reduce_threads;
    tasks_leftover = env->num_reduce_tasks - 
        tasks_per_thread * env->num_map_threads;

    task_id = 0;
    for (tid = 0; tid < env->num_reduce_threads; ++tid) {
        int remaining_cur_thread_tasks;

        remaining_cur_thread_tasks = tasks_per_thread;
        if (tasks_leftover > 0) {
            remaining_cur_thread_tasks += 1;
            --tasks_leftover;
        }
        do {
            if (task_id == env->num_reduce_tasks) {
                return 0;
            }

            /* New task. */
            reduce_task.id = task_id;

            /* TODO: Implement locality optimization. */
            ret = tq_enqueue_seq (env->taskQueue, &reduce_task,  -1);
            if (ret < 0) {
                return -1;
            }
            ++task_id;
            --remaining_cur_thread_tasks;
        } while (remaining_cur_thread_tasks);
    }

    return 0;
}

#ifndef INCREMENTAL_COMBINER
static void run_combiner (mr_env_t* env, int thread_index)
{
    assert (! env->oneOutputQueuePerMapTask);

    int i, j;
    keyvals_arr_t *my_output;
    keyvals_t *reduce_pos;
    void *reduced_val;
    iterator_t itr;
    val_t *val, *next;

    CHECK_ERROR (iter_init (&itr, 1));

    for (i = 0; i < env->num_reduce_tasks; ++i)
    {
        my_output = &env->intermediate_vals[thread_index][i];
        for (j = 0; j < my_output->len; ++j)
        {
            reduce_pos = &(my_output->arr[j]);
            if (reduce_pos->len == 0) continue;

            CHECK_ERROR (iter_add (&itr, reduce_pos));

            reduced_val = env->combiner (&itr);

            /* Shed off trailing chunks. */
            assert (reduce_pos->vals);
            val = reduce_pos->vals->next_val;
            while (val)
            {
                next = val->next_val;
                phoenix_mem_free (val);
                val = next;
            }

            /* Update the entry. */
            val = reduce_pos->vals;
            val->next_insert_pos = 0;
            val->next_val = NULL;
            val->array[val->next_insert_pos++] = reduced_val;
            reduce_pos->len = 1;

            iter_reset (&itr);
        }
    }

    iter_finalize (&itr);
}
#endif

/** emit_intermediate()
 *  inserts the key, val pair into the intermediate array
 */
void 
emit_intermediate (void *key, void *val, int key_size)
{
    struct timeval  begin, end;
    int curr_thread = -1;
    int             curr_task;
    bool            oneOutputQueuePerMapTask;
    keyvals_arr_t   *arr;
    mr_env_t        *env;

    get_time (&begin);

    env = get_env();
    if (curr_thread < 0)
        curr_thread = getCurrThreadIndex (TASK_TYPE_MAP);

    oneOutputQueuePerMapTask = env->oneOutputQueuePerMapTask;

    if (oneOutputQueuePerMapTask)
        curr_task = env->tinfo[curr_thread].curr_task;
    else
        curr_task = curr_thread;
   
    int reduce_pos = env->partition (env->num_reduce_tasks, key, key_size);
    reduce_pos %= env->num_reduce_tasks;

    /* Insert sorted in global queue at pos curr_proc */
    arr = &env->intermediate_vals[curr_task][reduce_pos];

    insert_keyval_merged (env, arr, key, val);

    get_time (&end);

#ifdef TIMING
    uintptr_t total_emit_time = (uintptr_t)pthread_getspecific (emit_time_key);
    uintptr_t emit_time = time_diff (&end, &begin);
    total_emit_time += emit_time;
    CHECK_ERROR (pthread_setspecific (emit_time_key, (void *)total_emit_time));
#endif
}

/** emit_inline ()
 *  inserts the key, val pair into the final output array
 */
static inline void 
emit_inline (mr_env_t* env, void *key, void *val)
{
    keyval_arr_t    *arr;
    int             curr_red_queue;
    int thread_index = -1;

    if (thread_index < 0)
        thread_index = getCurrThreadIndex (TASK_TYPE_REDUCE);

    if (env->oneOutputQueuePerReduceTask) {
        curr_red_queue = env->tinfo[thread_index].curr_task;
    }
    else {
        curr_red_queue = thread_index;
    }

    /* Insert sorted in global queue at pos curr_proc */
    arr = &env->final_vals[curr_red_queue];
    insert_keyval (env, arr, key, val);
}

/** emit ()
 */
void
emit (void *key, void *val)
{
    struct timeval begin, end;

    get_time (&begin);

    emit_inline (get_env(), key, val);

    get_time (&end);

#ifdef TIMING
    uintptr_t total_emit_time = (uintptr_t)pthread_getspecific (emit_time_key);
    uintptr_t emit_time = time_diff (&end, &begin);
    total_emit_time += emit_time;
    CHECK_ERROR (pthread_setspecific (emit_time_key, (void *)total_emit_time));
#endif
}

static inline void 
insert_keyval_merged (mr_env_t* env, keyvals_arr_t *arr, void *key, void *val)
{
    int high = arr->len, low = -1, next;
    int cmp = 1;
    keyvals_t *insert_pos;
    val_t *new_vals;

    assert(arr->len <= arr->alloc_len);
    if (arr->len > 0)
        cmp = env->key_cmp(arr->arr[arr->len - 1].key, key);

    if (cmp > 0)
    {
        /* Binary search the array to find the key. */
        while (high - low > 1)
        {
            next = (high + low) / 2;
            if (env->key_cmp(arr->arr[next].key, key) > 0)
                high = next;
            else
                low = next;
        }

        if (low < 0) low = 0;
        if (arr->len > 0 &&
                (cmp = env->key_cmp(arr->arr[low].key, key)) < 0)
            low++;
    }
    else if (cmp < 0)
        low = arr->len;
    else
        low = arr->len-1;

    if (arr->len == 0 || cmp)
    {
        /* if array is full, double and copy over. */
        if (arr->len == arr->alloc_len)
        {
            if (arr->alloc_len == 0)
            {
                arr->alloc_len = DEFAULT_KEYVAL_ARR_LEN;
                arr->arr = (keyvals_t *)
                    phoenix_mem_malloc (arr->alloc_len * sizeof (keyvals_t));
            }
            else
            {
                arr->alloc_len *= 2;
                arr->arr = (keyvals_t *)
                    phoenix_mem_realloc (arr->arr, arr->alloc_len * sizeof (keyvals_t));
            }
        }

        /* Insert into array. */
        memmove (&arr->arr[low+1], &arr->arr[low], 
                        (arr->len - low) * sizeof(keyvals_t));

        arr->arr[low].key = key;
        arr->arr[low].len = 0;
        arr->arr[low].vals = NULL;
        arr->len++;
    }

    insert_pos = &(arr->arr[low]);

    if (insert_pos->vals == NULL)
    {
        /* Allocate a chunk for the first time. */
        new_vals = phoenix_mem_malloc 
            (sizeof (val_t) + DEFAULT_VALS_ARR_LEN * sizeof (void *));
        assert (new_vals);

        new_vals->size = DEFAULT_VALS_ARR_LEN;
        new_vals->next_insert_pos = 0;
        new_vals->next_val = NULL;

        insert_pos->vals = new_vals;
    }
    else if (insert_pos->vals->next_insert_pos >= insert_pos->vals->size)
    {
#ifdef INCREMENTAL_COMBINER
        if (env->combiner != NULL) {
            iterator_t itr;
            void *reduced_val;

            CHECK_ERROR (iter_init (&itr, 1));
            CHECK_ERROR (iter_add (&itr, insert_pos));

            reduced_val = env->combiner (&itr);

            insert_pos->vals->array[0] = reduced_val;
            insert_pos->vals->next_insert_pos = 1;
            insert_pos->len = 1;

            iter_finalize (&itr);
        } else {
#endif
            /* Need a new chunk. */
            int alloc_size;

            alloc_size = insert_pos->vals->size * 2;
            new_vals = phoenix_mem_malloc (sizeof (val_t) + alloc_size * sizeof (void *));
            assert (new_vals);

            new_vals->size = alloc_size;
            new_vals->next_insert_pos = 0;
            new_vals->next_val = insert_pos->vals;

            insert_pos->vals = new_vals;
#ifdef INCREMENTAL_COMBINER
        }
#endif
    }

    insert_pos->vals->array[insert_pos->vals->next_insert_pos++] = val;

    insert_pos->len += 1;
}

static inline void 
insert_keyval (mr_env_t* env, keyval_arr_t *arr, void *key, void *val)
{
    int high = arr->len, low = -1, next;
    int cmp = 1;

    assert(arr->len <= arr->alloc_len);

    /* If array is full, double and copy over. */
    if (arr->len == arr->alloc_len)
    {
        if (arr->alloc_len == 0)
        {
            arr->alloc_len = DEFAULT_KEYVAL_ARR_LEN;
            arr->arr = (keyval_t*)phoenix_mem_malloc(arr->alloc_len * sizeof(keyval_t));
        }
        else
        {
            arr->alloc_len *= 2;
            arr->arr = (keyval_t*)phoenix_mem_realloc(arr->arr, arr->alloc_len * sizeof(keyval_t));
        }
    }

    if (env->oneOutputQueuePerReduceTask == false)
    {
        /* Need to sort. */
        if (arr->len > 0)
            cmp = env->key_cmp(arr->arr[arr->len - 1].key, key);

        if (cmp > 0)
        {
            /* Binary search the array to find the key. */
            while (high - low > 1)
            {
                next = (high + low) / 2;
                if (env->key_cmp(arr->arr[next].key, key) > 0)
                    high = next;
                else
                    low = next;
            }

            if (low < 0) low = 0;
            if (arr->len > 0 && env->key_cmp(arr->arr[low].key, key) < 0)
                low++;
        }
        else
            low = arr->len;


        /* Insert into array. */
        memmove (&arr->arr[low+1], &arr->arr[low], 
            (arr->len - low) * sizeof(keyval_t));

        arr->arr[low].key = key;
        arr->arr[low].val = val;
    }
    else
    {
        /* No need to sort. Just append. */
        arr->arr[arr->len].key = key;
        arr->arr[arr->len].val = val;
    }

    arr->len++;
}

static inline void 
merge_results (mr_env_t* env, keyval_arr_t *vals, int length) 
{
    int data_idx;
    int total_num_keys = 0;
    int i;
    int curr_thread = -1;
    
    if (curr_thread < 0)
        curr_thread = getCurrThreadIndex (TASK_TYPE_MERGE);

    for (i = 0; i < length; i++) {
        total_num_keys += vals[i].len;
    }

    env->merge_vals[curr_thread].len = total_num_keys;
    env->merge_vals[curr_thread].alloc_len = total_num_keys;
    env->merge_vals[curr_thread].pos = 0;
    env->merge_vals[curr_thread].arr = (keyval_t *)
        phoenix_mem_malloc(sizeof(keyval_t) * total_num_keys);

    for (data_idx = 0; data_idx < total_num_keys; data_idx++) {
        /* For each keyval_t. */
        int         min_idx;
        keyval_t    *min_keyval;

        for (i = 0; i < length && vals[i].pos >= vals[i].len; i++);

        if (i == length) break;

        /* Determine the minimum key. */
        min_idx = i;
        min_keyval = &vals[i].arr[vals[i].pos];

        for (i++; i < length; i++) {
            if (vals[i].pos < vals[i].len)
            {
                int cmp_ret;
                cmp_ret = env->key_cmp(
                    min_keyval->key, 
                    vals[i].arr[vals[i].pos].key);

                if (cmp_ret > 0) {
                    min_idx = i;
                    min_keyval = &vals[i].arr[vals[i].pos];
                }
            }
        }

        mem_memcpy (&env->merge_vals[curr_thread].arr[data_idx], 
                        min_keyval, sizeof(keyval_t));
        vals[min_idx].pos += 1;
    }

    for (i = 0; i < length; i++) {
        phoenix_mem_free(vals[i].arr);
    }
}

static inline int 
getNumTaskThreads (mr_env_t* env, TASK_TYPE_T task_type)
{
    int num_threads;

    switch (task_type)
    {
        case TASK_TYPE_MAP:
            num_threads = env->num_map_threads;
            break;

        case TASK_TYPE_REDUCE:
            num_threads = env->num_reduce_threads;
            break;

        case TASK_TYPE_MERGE:
            num_threads = env->num_merge_threads;
            break;

        default:
            assert (0);
            num_threads = env->num_map_threads;
            break;
    }

    return num_threads;
}

/** getCurrThreadIndex()
 *  Returns the processor the current thread is running on
 */
static inline int 
getCurrThreadIndex (TASK_TYPE_T task_type)
{
    int         i;
    int         num_threads;
    mr_env_t    *env;
    struct thread   *mytid;

    env = get_env();
    num_threads = getNumTaskThreads (env, task_type);
    mytid = thread_self();
    for (i = 0; i < num_threads && env->tinfo[i].tid != mytid; i++);

    assert(i != num_threads);

    return i;
}

/** array_splitter()
 *
 */
int 
array_splitter (void *data_in, int req_units, map_args_t *out)
{
    assert(out != NULL);

    mr_env_t    *env;
    int         unit_size;
    int         data_units;

    env = get_env();
    unit_size = env->args->unit_size;
    data_units = env->args->data_size / unit_size;

    /* End of data reached, return FALSE. */
    if (env->splitter_pos >= data_units)
        return 0;

    /* Set the start of the next data. */
    out->data = ((void *)env->args->task_data) + env->splitter_pos*unit_size;

    /* Determine the nominal length. */
    if (env->splitter_pos + req_units > data_units)
        out->length = data_units - env->splitter_pos;
    else
        out->length = req_units;

    env->splitter_pos += out->length;

    /* Return true since the out data is valid. */
    return 1;
}

void 
identity_reduce (void *key, iterator_t *itr)
{
    void        *val;
    mr_env_t    *env;

    env = get_env();
    while (iter_next (itr, &val))
    {
        emit_inline (env, key, val);
    }
}

int 
default_partition (int num_reduce_tasks, void* key, int key_size)
{
    unsigned long hash = 5381;
    char *str = (char *)key;
    int i;

    for (i = 0; i < key_size; i++)
    {
        hash = ((hash << 5) + hash) + ((int)str[i]); /* hash * 33 + c */
    }

    return hash % num_reduce_tasks;
}

/**
 * Run map tasks and get intermediate values
 */
static void map (mr_env_t* env)
{
    thread_arg_t   th_arg;
    int            num_map_tasks;

    num_map_tasks = gen_map_tasks (env);
    assert (num_map_tasks >= 0);

    env->num_map_tasks = num_map_tasks;
    if (num_map_tasks < env->num_map_threads)
        env->num_map_threads = num_map_tasks;

    //printf (OUT_PREFIX "num_map_tasks = %d\n", env->num_map_tasks);

    mem_memset (&th_arg, 0, sizeof(thread_arg_t));
    th_arg.task_type = TASK_TYPE_MAP;

    start_workers (env, &th_arg);
}

/**
 * Run reduce tasks and get final values. 
 */
static void reduce (mr_env_t* env)
{
    int            i;
    thread_arg_t   th_arg;

    CHECK_ERROR (gen_reduce_tasks (env));

    mem_memset (&th_arg, 0, sizeof(thread_arg_t));
    th_arg.task_type = TASK_TYPE_REDUCE;

    start_workers (env, &th_arg);

    /* Cleanup intermediate results. */
    for (i = 0; i < env->intermediate_task_alloc_len; ++i)
    {
        phoenix_mem_free (env->intermediate_vals[i]);
    }
    phoenix_mem_free (env->intermediate_vals);
}

/**
 * Merge all reduced data 
 */
static void merge (mr_env_t* env)
{
    thread_arg_t   th_arg;

    mem_memset (&th_arg, 0, sizeof (thread_arg_t));
    th_arg.task_type = TASK_TYPE_MERGE;

    if (env->oneOutputQueuePerReduceTask) {
        th_arg.merge_len = env->num_reduce_tasks;
    } else {
        th_arg.merge_len = env->num_reduce_threads;
    }
    th_arg.merge_input = env->final_vals;

    if (th_arg.merge_len <= 1) {
        /* Already merged, nothing to do here */
        env->args->result->data = env->final_vals->arr;
        env->args->result->length = env->final_vals->len;

        phoenix_mem_free(env->final_vals);

        return;
    }

    /* have work to merge! */
    while (th_arg.merge_len > 1) {
        th_arg.merge_round += 1;

        /* This is the worst case length, 
           depending on the value of num_merge_threads. */
        env->merge_vals = (keyval_arr_t*) 
            phoenix_mem_malloc (env->num_merge_threads * sizeof(keyval_arr_t));

        /* Run merge tasks and get merge values. */
        start_workers (env, &th_arg);

        phoenix_mem_free (th_arg.merge_input);
        th_arg.merge_len = env->num_merge_threads;

        env->num_merge_threads /= 2;
        if (env->num_merge_threads == 0)
            env->num_merge_threads = 1;

        th_arg.merge_input = env->merge_vals;
    }

    env->args->result->data = env->merge_vals[0].arr;
    env->args->result->length = env->merge_vals[0].len;

    phoenix_mem_free(env->merge_vals);
}

static inline mr_env_t* get_env (void)
{
    struct tls *tls = thread_get_tls();
    if(tls == NULL) {
        tls = calloc(1, sizeof(struct tls));
        thread_set_tls(tls);
    }
    return tls->env;
}
