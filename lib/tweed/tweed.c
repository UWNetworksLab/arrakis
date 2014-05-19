/** \file 
 *  \brief A simple work stealing library based upon both cilk and wool. 
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include "tweed/tweed.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/dispatch.h>
#include "trace/trace.h"
#include <trace_definitions/trace_defs.h>

/** Array of worker descriptors */
static struct worker_desc * workers;
/** Number of workers started */
static int num_workers;
/** Number of dispatchers started */
static volatile int num_dispatchers = 1;
/** Space used to store task for each worker (accessed as a stack) */
static char * task_stack_space;
/** Signals program exit */
static volatile int do_quit = 0;

#ifndef TWEED_LOCK_FREE
// TODO - replace these with mutexes, once they work
#define LOCK(x)   acquire_spinlock(&x);                                                  
#define UNLOCK(x) release_spinlock(&x)
#else
#define LOCK(x)
#define UNLOCK(x)
#endif


struct worker_args {
    int id;
    int origin;
};
static int steal(struct generic_task_desc * _tweed_top_, 
                 struct worker_desc * victim);

/** Start the main task */
static int main_worker (int id, 
                        int(*main_func)(struct generic_task_desc *,void*),
                        void * main_args) {

    workers[id].worker_thr = thread_self();
    workers[id].id = id;
    workers[id].core_id = disp_get_core_id();

    struct generic_task_desc * _tweed_top_ = NULL;
    
    thread_set_tls(&(workers[id]));

    int ret = main_func(_tweed_top_, main_args);
    
    // signal exit to other workers 
    do_quit = 1;

    return ret;
}



/** Work-stealing loop for workers */
static int worker_run (void * data) {
    struct worker_args * args = (struct worker_args*) data;
    int id = args->id;

    errval_t err = thread_detach(thread_self());
    assert(err_is_ok(err));

    free(args);
    workers[id].worker_thr = thread_self();
    workers[id].id = id;
    workers[id].core_id = disp_get_core_id();

    struct generic_task_desc * _tweed_top_ = NULL;
    
    thread_set_tls( &(workers[id]));

    trace_init_disp();

    num_dispatchers += 1;

    // start trying to steal work
    int steal_id = (id+1) % num_workers;
    while(!do_quit) {
        int success = steal(_tweed_top_, &workers[steal_id]);
        if (!success) {
            // try next worker
            steal_id = (steal_id+1) % num_workers;
        }
    }
    exit(0);
    return 0;
}

static int start_worker_thread(void * data) {
    thread_create(&worker_run, data);
    return 0;
}


/** Initialize a worker's data-structures */
static void init_worker(int id, void* stack_start) {
    workers[id].task_desc_stack = (struct generic_task_desc *) stack_start;
    memset(workers[id].task_desc_stack, 0, TWEED_TASK_STACK_SIZE);
    workers[id].bot = NULL; 
    // TODO - when mutexes work - workers[id].lock =                     
    //    (struct thread_mutex *) malloc (sizeof(struct thread_mutex));	
    // thread_mutex_init (workers[id].lock);
}


static void domain_spanned_callback(void *arg, errval_t err)
{
    num_dispatchers++;
}


/** Initialise the tweed library - must be called before any other 
 *  tweed calls
 */
int init_tweed(int workers_requested,
              int(*main_func)(struct generic_task_desc *,void*), 
              void* main_args) {
    int i, err;
    
    if (workers_requested < 1) {
        fprintf(stderr, 
                "Error initalizing tweed - requested less than 1 worker\n");
        return -1;
    }
    
    num_workers = workers_requested;
    workers = (struct worker_desc *) malloc (
	          num_workers * sizeof(struct worker_desc));
    // alloc task stack space for all workers, leave space for alignment
    task_stack_space = malloc (TWEED_TASK_STACK_SIZE * (num_workers + 1));
    char * curr_stack_space = (char*)(((unsigned long)task_stack_space + TWEED_TASK_STACK_SIZE) & ~TWEED_TASK_STACK_MASK);
 
    // Initialize worker data-structures
    for (i=0; i<num_workers; i++) {
        init_worker(i, curr_stack_space);
        curr_stack_space += TWEED_TASK_STACK_SIZE;
    }

    // create dispatchers on all other cores required for num_workers
    for (i=1; i<num_workers; i++) {
        err = domain_new_dispatcher(i + disp_get_core_id(), 
                                    domain_spanned_callback, 
                                    (void*)(uintptr_t)i);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "domain_new_dispatcher failed");
            printf("%d failed\n", i);
        }
    }

    // wait for all dispatchers to come up
    while (num_dispatchers < num_workers) {
        messages_wait_and_handle_next();
    }
    num_dispatchers = 1;  // reset

    // start work stealing threads on newly created domains
    for (i = 1; i < num_workers; i++) {
        struct worker_args * args = (struct worker_args *) malloc (
                                        sizeof(struct worker_args));
        args->id = i;
        args->origin = disp_get_core_id();

        err = domain_thread_create_on(i + disp_get_core_id(), start_worker_thread, 
                                 args);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failed to run a function on remote core");
        }
    }

    // wait for all dispatchers to come up
    while (num_dispatchers < num_workers) {
        messages_wait_and_handle_next();
    }

    // now start the main worker on the current dispatcher
    return main_worker(0, main_func, main_args);
}

static inline tweed_task_func_t steal_task(struct generic_task_desc * task,
                                           struct worker_desc * thief) {
#ifdef TWEED_USE_CAS
    tweed_task_func_t func = task->f.func;
    int success = cmpxchg128((uint64_t *)&(task->f.func),
                             (uint64_t)func, TWEED_TASK_NEW, 
                             (uint64_t)thief, TWEED_TASK_STOLEN);
    return success ? func : NULL;
#else 
    task->balarm = TWEED_TASK_STOLEN;
    mfence();      
    tweed_task_func_t func = task->f.func; 
    task->thief = thief;
    mfence();      
    return func;
#endif
} 



/** Initializes _tweed_top_ to start of this worker's task block
 */
struct generic_task_desc * set_top(void) {
    trace_event(TRACE_SUBSYS_TWEED, TRACE_EVENT_TWEED_LOCKING, 0);
    struct worker_desc * tls = (struct worker_desc *) thread_get_tls(); 
    LOCK(tls->lock); 
    trace_event(TRACE_SUBSYS_TWEED, TRACE_EVENT_TWEED_LOCKING_END, 0);
    tls->bot = workers[tls->id].task_desc_stack; 
    UNLOCK(tls->lock);    
    return workers[tls->id].task_desc_stack;   
}


/** Called when a worker spawns its first task to set its bot value so other
 *  workers can steal tasks from it.
 */
static inline void set_bot(struct generic_task_desc * val) {
    trace_event(TRACE_SUBSYS_TWEED, TRACE_EVENT_TWEED_LOCKING, 0);
    struct worker_desc * tls = (struct worker_desc *) thread_get_tls();  
    LOCK(tls->lock);
    trace_event(TRACE_SUBSYS_TWEED, TRACE_EVENT_TWEED_LOCKING_END, 0);
    tls->bot = val;
    UNLOCK(tls->lock);     
}



/** Called when a worker spawns its first task to set its bot value so other
 *  workers can steal tasks from it.
 */
static inline void atomic_inc(struct generic_task_desc ** bot, int bytes) {
    __sync_fetch_and_add((uint64_t*)bot, (uint64_t) bytes);
}


/** Steal work from another worker's task stack */
static int steal(struct generic_task_desc * _tweed_top_, 
                 struct worker_desc * victim) {
    struct generic_task_desc * stolenTask;
    struct worker_desc * me = (struct worker_desc *) thread_get_tls();
            
    LOCK(victim->lock);      

    stolenTask = victim->bot;
    // check if there is actually work to steal
    if (stolenTask != NULL && stolenTask->balarm == TWEED_TASK_NEW) {
        
        // try to steal task
        tweed_task_func_t func = steal_task(stolenTask, me);
        
        if (func == NULL) {
            // we didn't succeed in the steal, back off
#ifndef TWEED_USE_CAS
            stolenTask->balarm  = TWEED_TASK_INLINED;
            stolenTask->thief = NULL;
#endif
            UNLOCK(victim->lock);
            return 0; // didn't steal anything 
        } else {
            // we have stolen the task, update bot
            atomic_inc(&(victim->bot), stolenTask->size);
            UNLOCK(victim->lock);

            // and run task
            trace_event(TRACE_SUBSYS_TWEED, TRACE_EVENT_TWEED_STEAL, victim->core_id);
            func(_tweed_top_, stolenTask);
            trace_event(TRACE_SUBSYS_TWEED, TRACE_EVENT_TWEED_STEAL_END,
                        victim->core_id);
            
            // signal task completion
            stolenTask->balarm |= TWEED_TASK_COMPLETE;
            return 1;
        }      
    } else {
        UNLOCK(victim->lock);
        return 0; // didn't steal anything   
    }
}

/** Check if syncing task really is stolen */
int sync_stolen(struct generic_task_desc * _tweed_top_) {
#ifndef TWEED_LOCK_FREE
    struct worker_desc * tls = (struct worker_desc *) thread_get_tls(); 
#endif
    LOCK(tls->lock);        
    int ret = ((_tweed_top_->balarm & TWEED_TASK_STOLEN) != 0);
    UNLOCK(tls->lock);            
    return ret;
}

#ifdef TWEED_USE_CAS
#define GET_THIEF(t) t->f.thief
#else
#define GET_THIEF(t) t->thief
#endif

#if defined(TWEED_WAITING)
static inline int waiting(struct generic_task_desc * _tweed_top_) {
    // do nothing, just wait
    return 0;
}
#elif defined(TWEED_LEAPFROG)
static inline int waiting(struct generic_task_desc * _tweed_top_) {
    // steal work from the thief
    return steal(_tweed_top_, (struct worker_desc *) GET_THIEF(_tweed_top_));
}
#elif defined(TWEED_PARK)
#error "NYI"
#else
#error "One of TWEED_WAITING, TWEED_LEAPFROG or TWEED_PARK must be defined"
#endif

/** Handle stolen task */
int handle_stolen_task(struct generic_task_desc * _tweed_top_) {
    trace_event(TRACE_SUBSYS_TWEED, TRACE_EVENT_TWEED_WAIT,
                GET_THIEF(_tweed_top_)->core_id);        

    while ((_tweed_top_->balarm & TWEED_TASK_COMPLETE) == 0) {
        if (!waiting(_tweed_top_)) {
            thread_yield();
        }
    }
    trace_event(TRACE_SUBSYS_TWEED, TRACE_EVENT_TWEED_WAIT_END,
                GET_THIEF(_tweed_top_)->core_id); ; 

    // update bot
    set_bot(_tweed_top_);
    return 0;
}
