/**
 * \file
 * \brief Threads implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/debug.h>
#include <barrelfish/slab.h>
#include <barrelfish/caddr.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/vspace_mmu_aware.h>
#include <barrelfish_kpi/cpu_arch.h>
#include <barrelfish_kpi/domain_params.h>
#include <arch/registers.h>
#include <trace/trace.h>

#include <trace_definitions/trace_defs.h>

#include "arch/threads.h"
#include "threads_priv.h"
#include "init.h"

#if defined(__x86_64__)
#  include "arch/ldt.h"
#endif

#ifdef FPU_LAZY_CONTEXT_SWITCH
#  include <arch/fpu.h>
#endif

/// Maximum number of threads in a domain, used to size VM region for thread structures
// there is no point having MAX_THREADS > LDT_NENTRIES on x86 (see ldt.c)
#define MAX_THREADS 256

/// 16-byte alignment required for x86-64
// FIXME: this should be in an arch header
#define STACK_ALIGNMENT (sizeof(uint64_t) * 2)

/// Static stack and storage for a bootstrap/cleanup thread
// XXX: 16-byte aligned for x86-64
static uintptr_t staticstack[THREADS_DEFAULT_STACK_BYTES / sizeof(uintptr_t)]
__attribute__((aligned(STACK_ALIGNMENT)));
static struct thread staticthread = {
    .stack = staticstack,
    .stack_top = (char *)staticstack + sizeof(staticstack)
};
static struct thread_mutex staticthread_lock = THREAD_MUTEX_INITIALIZER;

/// Storage metadata for thread structures (and TLS data)
static struct slab_alloc thread_slabs;
static struct vspace_mmu_aware thread_slabs_vm;

// XXX: mutex and spinlock protecting thread slabs in spanned domains
/* This ought to be just a mutex. However, thread_create() is called on the
 * inter-disp message handler thread, and if it blocks in a mutex, there is no
 * way to wake it up and we will deadlock. This is a quick-fix workaround:
 *   The spinlock protects the data structure
 *   The mutex avoids unneccessary spinning (it is acquired first when safe)
 */
static spinlock_t thread_slabs_spinlock;
static struct thread_mutex thread_slabs_mutex = THREAD_MUTEX_INITIALIZER;

/// Base and size of the original ("pristine") thread-local storage init data
static void *tls_block_init_base;
static size_t tls_block_init_len;
static size_t tls_block_total_len;

/// Warning already issued about RSP usage.  (Prevent repeated warnings
/// from the same domain -- e.g., when using THC whose stacks appear
/// invalid here).
__attribute__((unused)) static bool stack_warned=0;

/// Wrapper function for most threads, runs given function then deletes itself
static void thread_entry(thread_func_t start_func, void *start_data)
{
    assert((lvaddr_t)start_func >= BASE_PAGE_SIZE);
    start_func(start_data);
    thread_exit();
    assert(!"thread_exit returned");
}

#ifndef NDEBUG
/// Debugging assertions on thread queues
static void check_queue(struct thread *queue)
{
    if (queue == NULL) {
        return;
    }
    struct thread *q = queue;
    int i = 0;

    do {
        assert_disabled(q != NULL);

        // check for NULL next and prev pointers
        assert_disabled((lvaddr_t)q->next > BASE_PAGE_SIZE);
        assert_disabled((lvaddr_t)q->prev > BASE_PAGE_SIZE);

        // check that next and prev pointers are sane
        assert_disabled(q->next->prev == q);
        assert_disabled(q->prev->next == q);

        // advance to next elem
        q = q->next;
        i++;
        assert_disabled(i < MAX_THREADS);
    } while (q != queue);
}
#else /* NDEBUG version */
static inline void check_queue(struct thread *queue) {}
#endif

/**
 * \brief Enqueue a thread in the given queue
 *
 * For safety, should only happen while disabled.
 */
void thread_enqueue(struct thread *thread, struct thread **queue)
{
    assert_disabled(thread != NULL);
    assert_disabled(queue != NULL);
    check_queue(*queue);
    if (*queue == NULL) {
        *queue = thread->prev = thread->next = thread;
    } else {
        assert_disabled((*queue)->prev != NULL);
        thread->prev = (*queue)->prev;
        thread->next = *queue;
        (*queue)->prev = thread;
        assert_disabled(thread->prev != NULL);
        thread->prev->next = thread;
    }

    check_queue(*queue);
}

/**
 * \brief Dequeue the first thread on the given queue
 *
 * For safety, should only happen while disabled.
 *
 * \returns Pointer to thread that was dequeued
 */
struct thread *thread_dequeue(struct thread **queue)
{
    assert_disabled(queue != NULL);
    struct thread *thread = *queue;
    assert_disabled(thread != NULL);
    check_queue(thread);
    if (thread->prev == thread) {
        assert_disabled(thread->next == thread);
        *queue = NULL;
    } else {
        thread->prev->next = thread->next;
        thread->next->prev = thread->prev;
        *queue = thread->next;
    }
    check_queue(*queue);
#ifndef NDEBUG
    thread->prev = thread->next = NULL;
#endif
    return thread;
}

/**
 * \brief Remove a specific thread from a queue
 *
 * Does not check that the thread is in the given queue, which it must be.
 * For safety, should only happen while disabled.
 */
void thread_remove_from_queue(struct thread **queue, struct thread *thread)
{
    assert_disabled(queue != NULL);
    assert_disabled(thread != NULL);
    check_queue(*queue);
    if (thread->prev == thread) {
        assert_disabled(thread->next == thread);
        assert_disabled(*queue == thread);
        *queue = NULL;
    } else {
        thread->prev->next = thread->next;
        thread->next->prev = thread->prev;
        if (*queue == thread) {
            *queue = thread->next;
        }
    }
    check_queue(*queue);
#ifndef NDEBUG
    thread->prev = thread->next = NULL;
#endif
}

/// Refill backing storage for thread region
static errval_t refill_thread_slabs(struct slab_alloc *slabs)
{
    assert(slabs == &thread_slabs);

    struct capref frame;
    size_t size;
    void *buf;
    errval_t err;

    err = slot_alloc(&frame);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    size_t blocksize = sizeof(struct thread) + tls_block_total_len;
    err = vspace_mmu_aware_map(&thread_slabs_vm, frame, blocksize, &buf, &size);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MMU_AWARE_MAP);
    }

    slab_grow(slabs, buf, size);

    return SYS_ERR_OK;
}

/// Initialise the state of a new thread structure
static void thread_init(dispatcher_handle_t disp, struct thread *newthread)
{
    newthread->self = newthread;
#ifndef NDEBUG
    newthread->next = newthread->prev = NULL;
#endif
    newthread->tls_dtv = NULL;
    newthread->disp = disp;
    newthread->coreid = get_dispatcher_generic(disp)->core_id;
    newthread->userptr = NULL;
    memset(newthread->userptrs, 0, sizeof(newthread->userptrs));
    newthread->yield_epoch = 0;
    newthread->wakeup_reason = NULL;
    newthread->return_value = 0;
    thread_cond_init(&newthread->exit_condition);
    thread_mutex_init(&newthread->exit_lock);
    newthread->state = THREAD_STATE_RUNNABLE;
    newthread->detached = false;
    newthread->joining = false;
    newthread->in_exception = false;
    newthread->used_fpu = false;
    newthread->paused = false;
    newthread->slab = NULL;
}

/**
 * \brief Handle FPU context switching
 */
static void fpu_context_switch(struct dispatcher_generic *disp_gen,
                               struct thread * next)
{
#ifdef FPU_LAZY_CONTEXT_SWITCH
    if(disp_gen->fpu_thread == NULL) {
        // FPU wasn't used -- no switching necessary
        return;
    }

    // Switch FPU trap back on if we switch away from FPU thread
    if(disp_gen->fpu_thread == disp_gen->current &&
       disp_gen->fpu_thread != next) {
        fpu_trap_on();
    }
#endif
}

/**
 * \brief Returns false if the stack pointer is out of bounds.
 */
static bool thread_check_stack_bounds(struct thread *thread,
                                      arch_registers_state_t *archregs) {
    lvaddr_t sp = (lvaddr_t) registers_get_sp(archregs);
    return sp > (lvaddr_t)thread->stack ||
           sp <= (lvaddr_t)thread->stack_top;
}

/**
 * \brief Schedule and run the next active thread, or yield the dispatcher.
 *
 * This may only be called from the dispatcher (on its stack and while
 * disabled!).
 *
 * \param disp Dispatcher pointer
 */
void thread_run_disabled(dispatcher_handle_t handle)
{
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    arch_registers_state_t *enabled_area =
        dispatcher_get_enabled_save_area(handle);

    if (disp_gen->current != NULL) {
        assert_disabled(disp_gen->runq != NULL);

        // check stack bounds
        warn_disabled(&stack_warned,
                      thread_check_stack_bounds(disp_gen->current, enabled_area));

        struct thread *next = disp_gen->current->next;
        assert_disabled(next != NULL);
        if (next != disp_gen->current) {
            fpu_context_switch(disp_gen, next);

            // save previous thread's state
            arch_registers_state_t *cur_regs = &disp_gen->current->regs;
            memcpy(cur_regs, enabled_area, sizeof(arch_registers_state_t));
            disp_gen->current = next;
            disp_resume(handle, &next->regs);
        } else {
            // same thread as before
            disp_resume(handle, enabled_area);
        }
    } else if (disp_gen->runq != NULL) {
        fpu_context_switch(disp_gen, disp_gen->runq);
        disp_gen->current = disp_gen->runq;
        disp->haswork = true;
        disp_resume(handle, &disp_gen->runq->regs);
    } else {
        // kernel gave us the CPU when we have nothing to do. block!
        disp->haswork = havework_disabled(handle);
        disp_gen->current = NULL;
        disp_yield_disabled(handle);
    }
}

/** Free all heap/slab-allocated state associated with a thread */
static void free_thread(struct thread *thread)
{
#if defined(__x86_64__) // XXX: gungy segment selector stuff
    assert(thread->thread_seg_selector != 0);
    uint16_t fs;
    __asm("mov %%fs, %0" : "=r" (fs));
    if (thread->thread_seg_selector == fs) {
        assert(thread->disp == curdispatcher());
        struct dispatcher_x86_64 *disp_priv = get_dispatcher_x86_64(thread->disp);
        // we're freeing the current thread; make sure we reload a valid segment
        // selector so that curdispatcher() keeps working!
        __asm volatile("mov %%ax, %%fs"
                       : /* No outputs */
                       : "a" (disp_priv->disp_seg_selector));
    }
    ldt_free_segment(thread->thread_seg_selector);
#endif

    free(thread->stack);
    if (thread->tls_dtv != NULL) {
        free(thread->tls_dtv);
    }

    thread_mutex_lock(&thread_slabs_mutex);
    acquire_spinlock(&thread_slabs_spinlock);
    slab_free(&thread_slabs, thread->slab); // frees thread itself
    release_spinlock(&thread_slabs_spinlock);
    thread_mutex_unlock(&thread_slabs_mutex);
}

/**
 * \brief Creates a new thread that will not be runnable
 *
 * \param start_func Function to run on the new thread
 * \param arg Argument to pass to function
 * \param stacksize Size of stack, in bytes
 *
 * \returns Thread pointer on success, NULL on failure
 */
struct thread *thread_create_unrunnable(thread_func_t start_func, void *arg,
                                        size_t stacksize)
{
    // allocate stack
    assert((stacksize % sizeof(uintptr_t)) == 0);
    void *stack = malloc(stacksize);
    if (stack == NULL) {
        return NULL;
    }

    // allocate space for TCB + initial TLS data
    // no mutex as it may deadlock: see comment for thread_slabs_spinlock
    // thread_mutex_lock(&thread_slabs_mutex);
    acquire_spinlock(&thread_slabs_spinlock);
    void *space = slab_alloc(&thread_slabs);
    release_spinlock(&thread_slabs_spinlock);
    // thread_mutex_unlock(&thread_slabs_mutex);
    if (space == NULL) {
        free(stack);
        return NULL;
    }

    // split space into TLS data followed by TCB
    // XXX: this layout is specific to the x86 ABIs! once other (saner)
    // architectures support TLS, we'll need to break out the logic.
    void *tls_data = space;
    struct thread *newthread = (void *)((uintptr_t)space + tls_block_total_len);

    // init thread
    thread_init(curdispatcher(), newthread);
    newthread->slab = space;

    if (tls_block_total_len > 0) {
        // populate initial TLS data from pristine copy
        assert(tls_block_init_len <= tls_block_total_len);
        memcpy(tls_data, tls_block_init_base, tls_block_init_len);

        // zero-fill remainder
        memset((char *)tls_data + tls_block_init_len, 0,
               tls_block_total_len - tls_block_init_len);

        // create a TLS thread vector
        struct tls_dtv *dtv = malloc(sizeof(struct tls_dtv) + 1 * sizeof(void *));
        assert(dtv != NULL);

        dtv->gen = 0;
        dtv->dtv[0] = tls_data;
        newthread->tls_dtv = dtv;
    }

    // FIXME: make arch-specific
#if defined(__x86_64__)
    // create segment for TCB
    errval_t err = ldt_alloc_segment(newthread, &newthread->thread_seg_selector);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error allocating LDT segment for new thread");
        free_thread(newthread);
        return NULL;
    }
#endif

    // init stack
    newthread->stack = stack;
    newthread->stack_top = (char *)stack + stacksize;

    // waste space for alignment, if malloc gave us an unaligned stack
    newthread->stack_top = (char *)newthread->stack_top
        - (lvaddr_t)newthread->stack_top % STACK_ALIGNMENT;

    // init registers
    registers_set_initial(&newthread->regs, newthread, (lvaddr_t)thread_entry,
                          (lvaddr_t)newthread->stack_top,
                          (lvaddr_t)start_func, (lvaddr_t)arg, 0, 0);

    return newthread;
}

/**
 * \brief Creates a new thread, and makes it runnable
 *
 * \param start_func Function to run on the new thread
 * \param arg Argument to pass to function
 * \param stacksize Size of stack, in bytes
 *
 * \returns Thread pointer on success, NULL on failure
 */
struct thread *thread_create_varstack(thread_func_t start_func, void *arg,
                                      size_t stacksize)
{
    struct thread *newthread = thread_create_unrunnable(start_func, arg, stacksize);
    if (newthread) {
        // enqueue on runq
        dispatcher_handle_t handle = disp_disable();
        struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
        newthread->disp = handle;
        thread_enqueue(newthread, &disp_gen->runq);
        disp_enable(handle);
    }
    return newthread;
}

/**
 * \brief Creates a new thread, and makes it runnable
 *
 * \param start_func Function to run on the new thread
 * \param arg Argument to pass to function
 *
 * \returns Thread pointer on success, NULL on failure
 */
struct thread *thread_create(thread_func_t start_func, void *arg)
{
    return thread_create_varstack(start_func, arg, THREADS_DEFAULT_STACK_BYTES);
}

/**
 * \brief Wait for termination of another thread
 *
 * \param thread        Pointer to thread to wait for
 * \param retval        Pointer to variable to hold return value of thread, or NULL
 *
 * \returns SYS_ERR_OK on success, error code on error.
 */
errval_t thread_join(struct thread *thread, int *retval)
{
    assert(thread != NULL);

    thread_mutex_lock(&thread->exit_lock);
    if(thread->detached) {
        // Thread is detached and thus not joinable
        thread_mutex_unlock(&thread->exit_lock);
        return LIB_ERR_THREAD_JOIN_DETACHED;
    }

    if(thread->joining) {
        // Someone else already joins, that's an error
        thread_mutex_unlock(&thread->exit_lock);
        return LIB_ERR_THREAD_JOIN;
    }

    thread->joining = true;
    if(thread->state != THREAD_STATE_EXITED) { // Possibly wait for thread exit
        thread_cond_wait(&thread->exit_condition, &thread->exit_lock);
    }

    if(retval != NULL) {
        *retval = thread->return_value;
    }

    thread_mutex_unlock(&thread->exit_lock);    // Not really needed
    free_thread(thread);

    return SYS_ERR_OK;
}

/**
 * \brief Detach a thread. Free its state when it terminates.
 *
 * \param thread        Pointer to thread to detach
 *
 * \return SYS_ERR_OK on success.
 */
errval_t thread_detach(struct thread *thread)
{
    assert(thread != NULL);
    thread_mutex_lock(&thread->exit_lock);

    if(thread->joining) {
        // Someone else already joins, that's an error
        thread_mutex_unlock(&thread->exit_lock);
        return LIB_ERR_THREAD_JOIN;
    }

    if(!thread->detached) {
        thread->detached = true;
    } else {
        // Detaching more than once is an error
        thread_mutex_unlock(&thread->exit_lock);
        return LIB_ERR_THREAD_DETACHED;
    }

    if(thread->state == THREAD_STATE_EXITED) {
        // Thread already exited before we detached, clean it up
        free_thread(thread);
        return SYS_ERR_OK;
    }

    thread_mutex_unlock(&thread->exit_lock);
    return SYS_ERR_OK;
}

/**
 * \brief Returns the thread pointer to the currently-running thread
 */
struct thread *thread_self(void)
{
    struct thread *me;
#if defined(__x86_64__) // XXX: AB's silly little arch-specific optimisation
    __asm("movq %%fs:0, %0" : "=r" (me));
#else
    // it's not necessary to disable, but might be once we do migration
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    me = disp_gen->current;
    disp_enable(handle);
#endif
    return me;
}

uintptr_t thread_id(void)
{
    return thread_self()->id;
}

void thread_set_id(uintptr_t id)
{
    struct thread *me = thread_self();
    me->id = id;
}

/**
 * \brief Yield the calling thread
 *
 * Switches to the next runnable thread in this dispatcher, or if none is
 * available, yields the dispatcher.
 */
void thread_yield(void)
{
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    arch_registers_state_t *enabled_area =
        dispatcher_get_enabled_save_area(handle);

    struct thread *me = disp_gen->current;
    struct thread *next = me;
    me->yield_epoch = disp_gen->timeslice;

    do {
        assert_disabled(next != NULL);
        next = next->next;
        if (next == me) {
            break; // Everybody yielded this timeslice
        }
    } while(next->yield_epoch == disp_gen->timeslice);

    if (next != me) {
        fpu_context_switch(disp_gen, next);
        disp_gen->current = next;
        disp_switch(handle, &me->regs, &next->regs);
    } else {
        assert_disabled(disp_gen->runq != NULL);
        assert_disabled(disp->haswork);
        trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_C_DISP_SAVE, 3);
        disp_save(handle, enabled_area, true, CPTR_NULL);
    }
}

/**
 * \brief Yield both the calling thread, and the dispatcher to another domain
 *
 * \param endpoint Endpoint cap to which we wish to yield, or #CAP_NULL
 *                  for an undirected yield
 *
 * Yields the dispatcher, optionally to another specified dispatcher.
 */
void thread_yield_dispatcher(struct capref endpoint)
{
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    arch_registers_state_t *enabled_area =
        dispatcher_get_enabled_save_area(handle);

    assert_disabled(disp_gen->runq != NULL);
    assert_disabled(disp->haswork);

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_C_DISP_SAVE, 1);
    disp_save(handle, enabled_area, true, get_cap_addr(endpoint));
}

/// Function that runs on the static thread/stack to clean up a "real" (alloced) thread
static int cleanup_thread(void *arg)
{
    struct thread *thread = arg;

    // free old thread and its stack
    if (thread != NULL) {
        free_thread(thread);
    }

    // disable and release static thread
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    struct thread *me = disp_gen->current;
    struct thread *ft =
        thread_mutex_unlock_disabled(handle, &disp_gen->cleanupthread_lock);
    assert(ft == NULL);

    // run the next thread, if any
    struct thread *next = me->next;
    thread_remove_from_queue(&disp_gen->runq, me);
    if (next != me) {
        disp_gen->current = next;
        disp_resume(handle, &next->regs);
    } else {
        disp_gen->current = NULL;
        disp->haswork = havework_disabled(handle);
        disp_yield_disabled(handle);
    }

    return 0;
}

/**
 * \brief Terminate the calling thread
 */
void thread_exit(void)
{
    struct thread *me = thread_self();

    thread_mutex_lock(&me->exit_lock);

    // if this is the static thread, we don't need to do anything but cleanup
    if (me == &staticthread) {
        assert(me->detached);
        // disable and release static thread
        dispatcher_handle_t handle = disp_disable();
        struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
        struct dispatcher_shared_generic *disp =
            get_dispatcher_shared_generic(handle);
        assert_disabled(me == &staticthread);
        assert_disabled(me->stack == staticstack);
        struct thread *ft =
            thread_mutex_unlock_disabled(handle, &staticthread_lock);
        assert(ft == NULL);

#ifdef FPU_LAZY_CONTEXT_SWITCH
        // No more FPU usage from here on
        if(disp_gen->fpu_thread == me) {
            disp_gen->fpu_thread = NULL;
            fpu_trap_on();
        }
#endif

        // run the next thread, if any
        struct thread *next = me->next;
        thread_remove_from_queue(&disp_gen->runq, me);
        if (next != me) {
            fpu_context_switch(disp_gen, next);
            disp_gen->current = next;
            disp_resume(handle, &next->regs);
        } else {
            disp_gen->current = NULL;
            disp->haswork = havework_disabled(handle);
            disp_yield_disabled(handle);
        }
    }

    if(me->detached) {
        // otherwise, we use a dispatcher-local thread to perform cleanup
        struct dispatcher_generic *dg = get_dispatcher_generic(curdispatcher());
        thread_mutex_lock(&dg->cleanupthread_lock);
        if(dg->cleanupthread == NULL) {
            dg->cleanupthread =
                thread_create_unrunnable(cleanup_thread, me,
                                         THREADS_DEFAULT_STACK_BYTES);
        }
        thread_init(curdispatcher(), dg->cleanupthread);

        registers_set_initial(&dg->cleanupthread->regs, dg->cleanupthread,
                              (lvaddr_t)cleanup_thread,
                              (lvaddr_t)dg->cleanupthread->stack_top, (lvaddr_t)me,
                              0, 0, 0);

        // Switch to it (on this dispatcher)
        dispatcher_handle_t handle = disp_disable();
        struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);

#ifdef FPU_LAZY_CONTEXT_SWITCH
        // No more FPU usage from here on
        if(disp_gen->fpu_thread == me) {
            disp_gen->fpu_thread = NULL;
            fpu_trap_on();
        }
#endif

        thread_remove_from_queue(&disp_gen->runq, me);
        thread_enqueue(dg->cleanupthread, &disp_gen->runq);
        disp_gen->cleanupthread->disp = handle;
        fpu_context_switch(disp_gen, dg->cleanupthread);
        disp_gen->current = dg->cleanupthread;
        disp_resume(handle, &dg->cleanupthread->regs);
    } else {
        // We're not detached -- wakeup joiner
        me->return_value = 0;   // XXX: Should be an argument to thread_exit()
        me->state = THREAD_STATE_EXITED;
        thread_cond_signal(&me->exit_condition);

        // Disable and unlock exit lock
        dispatcher_handle_t handle = disp_disable();
        struct thread *wakeup =
            thread_mutex_unlock_disabled(handle, &me->exit_lock);
        struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
        struct dispatcher_shared_generic *disp =
            get_dispatcher_shared_generic(handle);

        assert_disabled(wakeup == NULL);

#ifdef FPU_LAZY_CONTEXT_SWITCH
        // No more FPU usage from here on
        if(disp_gen->fpu_thread == me) {
            disp_gen->fpu_thread = NULL;
            fpu_trap_on();
        }
#endif

        // run the next thread, if any
        struct thread *next = me->next;
        thread_remove_from_queue(&disp_gen->runq, me);
        if (next != me) {
            fpu_context_switch(disp_gen, next);
            disp_gen->current = next;
            disp_resume(handle, &next->regs);
        } else {
            disp_gen->current = NULL;
            disp->haswork = havework_disabled(handle);
            disp_yield_disabled(handle);
        }
    }

    USER_PANIC("should never be reached");
}

/**
 * \brief Block the caller, and optionally release a spinlock, while disabled
 *
 * The caller is unconditionally blocked, and placed into the given queue
 * pending a call that will unblock it. After manipulating the queues, and
 * before switching threds, the given spinlock, if specified, is unlocked.
 * This function must only be called while disabled.
 *
 * This function is intended for use by multi-processor thread synchronisation
 * functions.
 *
 * \param disp Dispatcher pointer
 * \param queue (Optional) Queue of threads in which to place caller
 * \param spinlock (Optional) pointer to spinlock
 *
 * \returns Argument passed to thread_unblock, when unblocked
 */
void *thread_block_and_release_spinlock_disabled(dispatcher_handle_t handle,
                                                 struct thread **queue,
                                                 spinlock_t *spinlock)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    struct thread *me = disp_gen->current;
    struct thread *next = me->next;
    assert_disabled(next != NULL);

    assert_disabled(me->state == THREAD_STATE_RUNNABLE);
    me->state = THREAD_STATE_BLOCKED;

    thread_remove_from_queue(&disp_gen->runq, me);
    if (queue != NULL) {
        thread_enqueue(me, queue);
    }

    if (spinlock != NULL) {
        release_spinlock(spinlock);
    }

    if (next != me) {
        assert_disabled(disp_gen->runq != NULL);
        fpu_context_switch(disp_gen, next);
        disp_gen->current = next;
        disp_switch(handle, &me->regs, &next->regs);
    } else {
        assert_disabled(disp_gen->runq == NULL);
        disp_gen->current = NULL;
        disp->haswork = havework_disabled(handle);
        trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_C_DISP_SAVE, 2);
        disp_save(handle, &me->regs, true, CPTR_NULL);
    }

    assert(me->disp == handle); // didn't migrate while asleep
    return me->wakeup_reason;
}

/**
 * \brief Block the calling thread, while disabled
 *
 * The caller is unconditionally blocked, and placed into the given queue
 * pending a call that will unblock it.
 * This function must only be called while disabled.
 *
 * \param disp Dispatcher pointer
 * \param queue Queue of threads in which to place caller
 *
 * \returns Argument passed to thread_unblock, when unblocked
 */
void *thread_block_disabled(dispatcher_handle_t disp, struct thread **queue)
{
    return thread_block_and_release_spinlock_disabled(disp, queue, NULL);
}

/**
 * \brief Block the calling thread, while enabled
 *
 * The caller is unconditionally blocked, and placed into the given queue
 * pending a call that will unblock it.
 * This function must only be called while enabled.
 *
 * \param queue Queue of threads in which to place caller
 *
 * \returns Argument passed to thread_unblock, when unblocked
 */
void *thread_block(struct thread **queue)
{
    return thread_block_disabled(disp_disable(), queue);
}

/**
 * \brief Unblock a single thread from a given queue, while disabled
 *
 * A single thread is removed from the queue of blocked threads, and awoken.
 * This function must only be called while disabled.
 *
 * \param disp   Dispatcher pointer
 * \param queue  Queue of threads from which to unblock one
 * \param reason Value to be returned from thread_block()
 *
 * \returns Pointer to thread to be woken on a foreign dispatcher
 */
struct thread *thread_unblock_one_disabled(dispatcher_handle_t handle,
                                           struct thread **queue,
                                           void *reason)
{
    assert_disabled(queue != NULL);

    // Any threads in queue?
    if (*queue == NULL) {
        return NULL;
    }

    // Wakeup one waiting thread
    struct thread *wakeup = thread_dequeue(queue);
    wakeup->wakeup_reason = reason;
    assert_disabled(wakeup->state == THREAD_STATE_BLOCKED);
    wakeup->state = THREAD_STATE_RUNNABLE;

    /* enqueue on run queue if it's "our" thread, and not paused */
    if (wakeup->disp == handle) {
        if (!wakeup->paused) {
            struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
            thread_enqueue(wakeup, &disp_gen->runq);
        }
        return NULL;
    } else {
        return wakeup;
    }
}

/**
 * \brief Unblock a single thread from a given queue, while enabled
 *
 * A single thread is removed from the queue of blocked threads, and awoken.
 * This function must only be called while enabled.
 *
 * \param queue  Queue of threads from which to unblock one
 * \param reason Value to be returned from thread_block()
 *
 * \returns Pointer to thread to be woken on a foreign dispatcher
 */
struct thread *thread_unblock_one(struct thread **queue, void *reason)
{
    return thread_unblock_one_disabled(disp_disable(), queue, reason);
}

/**
 * \brief Unblock all threads on a given queue, while disabled
 *
 * All threads on the queue of blocked threads are awoken.
 * This function must only be called while disabled.
 *
 * \param disp   Dispatcher pointer
 * \param queue  Queue of threads to unblock
 * \param reason Value to be returned from thread_block()
 *
 * \returns Pointer to list of threads to be woken on a foreign dispatcher
 */
struct thread *thread_unblock_all_disabled(dispatcher_handle_t handle,
                                           struct thread **queue, void *reason)
{
    assert_disabled(queue != NULL);
    struct thread *wakeupq = NULL;

    // Wakeup all waiting threads
    while (*queue != NULL) {
        struct thread *wakeup = thread_unblock_one_disabled(handle, queue, reason);
        if (wakeup != NULL) {
            wakeup->next = wakeupq;
            wakeupq = wakeup;
        }
    }

    return wakeupq;
}

extern int _main(int argc, const char *argv[]);

/// Thread created in new domain that runs main()
static int main_thread(void *params)
{
    struct spawn_domain_params *p = params;
    exit(_main(p->argc, p->argv));
    return EXIT_FAILURE;
}

static bool init_domain_global; // XXX

/// Thread created on static stack in new domain that runs init code
static int bootstrap_thread(struct spawn_domain_params *params)
//int bootstrap_thread(struct spawn_domain_params *params);
//int bootstrap_thread(struct spawn_domain_params *params)
{
    errval_t err;

    // Set libc function pointers
    barrelfish_libc_glue_init();

    if (params == NULL) {
        printf("%s: error in creating a thread, NULL parameters given\n",
                disp_name());
    }
    assert(params != NULL);

    // Do we have TLS data?
    tls_block_init_base = params->tls_init_base;
    tls_block_init_len = params->tls_init_len;
    tls_block_total_len = params->tls_total_len;

    // Initialize subsystems
    err = barrelfish_init_onthread(params);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error during libbarrelfish init");
        exit(EXIT_FAILURE);
        assert(!"exit returned!");
    }

    // Allocate storage region for real threads
    size_t blocksize = sizeof(struct thread) + tls_block_total_len;
    err = vspace_mmu_aware_init(&thread_slabs_vm, MAX_THREADS * blocksize);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace_mmu_aware_init for thread region failed\n");
    }
    slab_init(&thread_slabs, blocksize, refill_thread_slabs);

    if (init_domain_global) {
        // run main() on this thread, since we can't allocate
        if (tls_block_total_len > 0) {
            USER_PANIC("unsupported: use of TLS data in bootstrap domain\n");
        }
        main_thread(params);
    } else {
        // Start real thread to run main()
        struct thread *thread = thread_create(main_thread, params);
        assert(thread != NULL);
    }

    return 0; // ignored
}

/**
 * \brief Initialise thread system while still disabled
 *
 * This function initialises the thread system while the dispatcher is still
 * disabled, before enabling the dispatcher, running the general initialisation
 * code, and calling main().
 *
 * \param disp Dispatcher pointer
 * \param init_domain True if we are a bootstrap domain
 */
void thread_init_disabled(dispatcher_handle_t handle, bool init_domain)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    arch_registers_state_t *enabled_area =
        dispatcher_get_enabled_save_area(handle);

    init_domain_global = init_domain;

    // Create the first thread manually
    struct thread *thread = &staticthread;
    staticthread_lock.locked = true; // XXX: safe while disabled

    // waste space for alignment, if unaligned
    thread->stack_top = (char *)thread->stack_top
        - (lvaddr_t)thread->stack_top % STACK_ALIGNMENT;

    // Initialise the first (static) thread
    thread_init(handle, thread);
    thread->detached = true;

#if defined(__x86_64__)
    // create segment for TCB
    errval_t err = ldt_alloc_segment_disabled(handle, thread,
                                              &thread->thread_seg_selector);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error allocating LDT segment for first thread");
    }
#endif

    uintptr_t param;
    registers_get_param(enabled_area, &param);

    registers_set_initial(&thread->regs, thread, (lvaddr_t)thread_entry,
                          /* TODO: pass stack base and limit, choose in arch
                           * code (possibly setting up other hints on stack) */
                          (lvaddr_t)thread->stack_top,
                          (lvaddr_t)bootstrap_thread, param, 0, 0);

    // Switch to it (always on this dispatcher)
    thread->disp = handle;
    thread_enqueue(thread, &disp_gen->runq);
    disp_gen->current = thread;
    disp->haswork = true;
    disp_resume(handle, &thread->regs);
}

/**
 * \brief Called on the remote core when spanning a domain across cores
 *
 * Runs the provided thread after enqueuing it and enabling the dispatcher
 */
void thread_init_remote(dispatcher_handle_t handle, struct thread *thread)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    thread_enqueue(thread, &disp_gen->runq);
    disp_gen->current = thread;
    disp->haswork = true;
    disp_resume(handle, &thread->regs);
}

/**
 * \brief Prepare to span the current domain
 *
 * This is a kludge. It is called from domain.c when creating a new dispatcher,
 * and is responsible for pre-allocating all the storage that might be needed
 * for thread metadata in the slab allocator. It can go away once we sanely
 * manage the vspace across multiple dispatchers in a domain.
 */
void threads_prepare_to_span(dispatcher_handle_t newdh)
{
    static bool called;

    if (!called) {
        called = true;

        thread_mutex_lock(&thread_slabs_mutex);
        acquire_spinlock(&thread_slabs_spinlock);

        while (slab_freecount(&thread_slabs) < MAX_THREADS - 1) {
            struct capref frame;
            size_t size;
            void *buf;
            errval_t err;

            err = slot_alloc(&frame);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "in slot_alloc while prefilling thread slabs\n");
            }

            size_t blocksize = sizeof(struct thread) + tls_block_total_len;
            err = vspace_mmu_aware_map(&thread_slabs_vm, frame, blocksize,
                                       &buf, &size);
            if (err_is_fail(err)) {
                slot_free(frame);
                if (err_no(err) == LIB_ERR_VSPACE_MMU_AWARE_NO_SPACE) {
                    // we've wasted space with fragmentation
                    // cross our fingers and hope for the best...
                    break;
                }
                USER_PANIC_ERR(err, "in vspace_mmu_aware_map while prefilling "
                               "thread slabs\n");
            }

            slab_grow(&thread_slabs, buf, size);
        }

        release_spinlock(&thread_slabs_spinlock);
        thread_mutex_unlock(&thread_slabs_mutex);
    }
}

/**
 * \brief Pause (suspend execution of) the given thread, and optionally capture its register state
 *
 * The thread will not be run, until a subsequent call to thread_resume()
 */
void thread_pause_and_capture_state(struct thread *thread,
                                    arch_registers_state_t **ret_regs,
                                    arch_registers_fpu_state_t **ret_fpuregs)
{
    assert(thread != NULL);
    dispatcher_handle_t dh = disp_disable();
    struct dispatcher_generic *disp = get_dispatcher_generic(dh);
    if (thread->disp == dh) {
        if (!thread->paused) {
            thread->paused = true;
            if (thread == disp->current) { // doesn't make much sense...
                sys_print("Warning: pausing current thread!\n",100);
                assert_disabled(thread->state == THREAD_STATE_RUNNABLE);
                thread_block_disabled(dh, NULL);
            } else if (thread->state == THREAD_STATE_RUNNABLE) {
                thread_remove_from_queue(&disp->runq, thread);
            }
        }
        if (ret_regs != NULL) {
            *ret_regs = &thread->regs;
        }
        if (ret_fpuregs != NULL) {
            if (thread->used_fpu) {
                // FIXME: this may not be the right FPU state?
                *ret_fpuregs = &thread->fpu_state;
            } else {
                *ret_fpuregs = NULL;
            }
        }
    } else {
        USER_PANIC("NYI: remote dispatcher thread_pause()");
    }
    disp_enable(dh);
}

/**
 * \brief Pause (suspend execution of) the given thread
 *
 * The thread will not be run, until a subsequent call to thread_resume()
 */
void thread_pause(struct thread *thread)
{
    thread_pause_and_capture_state(thread, NULL, NULL);
}

/**
 * \brief Resume execution of a thread previously suspended by thread_pause()
 */
void thread_resume(struct thread *thread)
{
    assert(thread != NULL);
    dispatcher_handle_t dh = disp_disable();
    struct dispatcher_generic *disp = get_dispatcher_generic(dh);
    if (thread->disp == dh) {
        if (thread->paused) {
            thread->paused = false;
            if (thread->state == THREAD_STATE_RUNNABLE) {
                thread_enqueue(thread, &disp->runq);
            }
        }
    } else {
        USER_PANIC("NYI: remote dispatcher thread_resume()");
    }
    disp_enable(dh);
}

/**
 * \brief Set old-style thread-local storage pointer.
 * \param p   User's pointer
 */
void thread_set_tls(void *p)
{
    struct thread *me = thread_self();
    me->userptr = p;
}

void thread_set_tls_key(int key, void *p)
{
    struct thread *me = thread_self();
    me->userptrs[key] = p;
}

/**
 * \brief Return old-style thread-local storage pointer.
 * \return User's pointer, previously passed to thread_set_tls()
 */
void *thread_get_tls(void)
{
    struct thread *me = thread_self();
    return me->userptr;
}

void *thread_get_tls_key(int key)
{
    struct thread *me = thread_self();
    return me->userptrs[key];
}

/**
 * \brief Set the exception handler function for the current thread.
 *        Optionally also change its stack, and return the old values.
 *
 * \param newhandler New exception handler. Pass NULL to disable an existing handler.
 * \param oldhandler If non-NULL, returns previous exception handler
 * \param new_stack_base If non-NULL, sets a new exception handler stack (base)
 * \param new_stack_top  If non-NULL, sets a new exception handler stack (top)
 * \param old_stack_base If non-NULL, returns previous stack base
 * \param old_stack_top If non-NULL, returns previous stack top
 */
errval_t thread_set_exception_handler(exception_handler_fn newhandler,
                                      exception_handler_fn *oldhandler,
                                      void *new_stack_base, void *new_stack_top,
                                      void **old_stack_base, void **old_stack_top)
{
    struct thread *me = thread_self();

    if (oldhandler != NULL) {
        *oldhandler = me->exception_handler;
    }

    if (old_stack_base != NULL) {
        *old_stack_base = me->exception_stack;
    }

    if (old_stack_top != NULL) {
        *old_stack_top = me->exception_stack_top;
    }

    me->exception_handler = newhandler;

    if (new_stack_base != NULL && new_stack_top != NULL) {
        me->exception_stack = new_stack_base;
        me->exception_stack_top = new_stack_top;
    }

    return SYS_ERR_OK;
}

static void exception_handler_wrapper(arch_registers_state_t *cpuframe,
                                      arch_registers_fpu_state_t *fpuframe,
                                      uintptr_t hack_arg, void *addr)
{
    struct thread *me = thread_self();

    assert(me->in_exception);
    assert(me->exception_handler != NULL);

    // XXX: unpack hack arg
    enum exception_type type = hack_arg >> 16;
    int subtype = hack_arg & 0xffff;

    // run handler
    me->exception_handler(type, subtype, addr, cpuframe, fpuframe);

    // resume state
    dispatcher_handle_t dh = disp_disable();
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(dh);
    //memcpy(&me->regs, cpuframe, sizeof(arch_registers_state_t));

#ifdef FPU_LAZY_CONTEXT_SWITCH
    if (fpuframe != NULL) {
        assert_disabled(me->used_fpu);
        arch_registers_fpu_state_t *dest;
        if (disp_gen->fpu_thread == me) {
            dest = dispatcher_get_enabled_fpu_save_area(dh);
        } else {
            dest = &me->fpu_state;
        }
        fpu_copy(dest, fpuframe);
        fpu_trap_on();
    }
#endif

    assert_disabled(me->in_exception);
    me->in_exception = false;

    assert_disabled(disp_gen->current == me);
    disp_resume(dh, cpuframe);
}

#if 0
void thread_debug_regs(struct thread *t);
void thread_debug_regs(struct thread *t)
{
  printf("%d: RIP = %lx, RSP = %lx\n", disp_get_domain_id(),
	 t->regs.rip, t->regs.rsp);
  uint64_t *stack = (uint64_t *)t->regs.rsp;
  printf("%d: ", disp_get_domain_id());
  for(int i = 0; i < 30; i++) {
    printf("%lx ", stack[i]);
  }
  printf("\n");
}
#endif

/**
 * \brief Deliver an exception to the current thread, and resume.
 *
 * This may only be called from the dispatcher (on its stack and while
 * disabled!).
 *
 * \param handle Dispatcher handle
 * \param type   Exception type
 * \param subtype Exception subtype
 * \param addr   Exception address
 * \param regs   CPU register state at time of exception
 */
void thread_deliver_exception_disabled(dispatcher_handle_t handle,
                                       enum exception_type type, int subtype,
                                       void *addr, arch_registers_state_t *regs)
{
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    struct thread *thread = disp_gen->current;
    assert_disabled(thread != NULL);
    assert_disabled(disp_gen->runq != NULL);

    // can we deliver the exception?
    if (thread->exception_handler == NULL || thread->exception_stack_top == NULL
        || thread->in_exception) {
        if (thread->in_exception) {
            sys_print("Can't deliver exception to thread: already in handler\n",
                      100);
        } else {
            sys_print("Can't deliver exception to thread: handler not set\n",
                      100);
        }

        // warn on stack overflow.
        lvaddr_t sp = (lvaddr_t) registers_get_sp(regs);
        if (sp < (lvaddr_t)thread->stack ||
            sp > (lvaddr_t)thread->stack_top) {
            char str[256];
            snprintf(str, sizeof(str), "Error: stack bounds exceeded: sp = 0x%"
                     PRIxPTR " but [bottom, top] = [0x%" PRIxPTR ", 0x%"
                     PRIxPTR "]\n", (lvaddr_t) sp, (lvaddr_t) thread->stack,
                     (lvaddr_t) thread->stack_top);
            sys_print(str, sizeof(str));
        }

        // TODO: actually delete the thread!
        disp_gen->current = NULL;
        thread_remove_from_queue(&disp_gen->runq, thread);
        return;
    }

    thread->in_exception = true;

    lvaddr_t stack_top = (lvaddr_t)thread->exception_stack_top;

    // save thread's state at time of fault on top of exception stack
    stack_top -= sizeof(arch_registers_state_t);
    arch_registers_state_t *cpuframe = (void *)stack_top;
    memcpy(cpuframe, regs, sizeof(arch_registers_state_t));

    arch_registers_fpu_state_t *fpuframe = NULL;
#ifdef FPU_LAZY_CONTEXT_SWITCH
    if (thread->used_fpu) {
        stack_top -= sizeof(arch_registers_fpu_state_t);
        fpuframe = (void *)stack_top;
        fpu_copy(fpuframe, &thread->fpu_state);
    }
#endif

    // align stack
    stack_top -= stack_top % STACK_ALIGNMENT;

    // XXX: sanity-check to ensure we have a sensible amount of exception stack left
    assert_disabled(stack_top > (lvaddr_t)thread->exception_stack + 8192);

    // XXX: pack two small ints together to fit into a single register
    uintptr_t hack_arg = (uintptr_t)type << 16 | (subtype & 0xffff);

    registers_set_initial(&thread->regs, thread,
                          (lvaddr_t)exception_handler_wrapper,
                          stack_top, (lvaddr_t)cpuframe, (lvaddr_t)fpuframe,
                          hack_arg, (lvaddr_t)addr);

    disp_resume(handle, &thread->regs);
}
