/**
 * \file
 * \brief Thread synchronisation primitives.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include "threads_priv.h"

#ifndef TRACE_THREADS
#define trace_event(a,b,c) ((void)0)
#endif


/**
 * \brief Initialise a condition variable
 *
 * \param cond Condition variable pointer
 */
void thread_cond_init(struct thread_cond *cond)
{
    cond->queue = NULL;
    cond->lock = 0;
}

/**
 * \brief Wait for a condition variable
 *
 * This function waits for the given condition variable to be signalled
 * (through thread_cond_signal() or thread_cond_broadcast()) before
 * returning, while atomically unlocking the mutex pointed to by
 * 'mutex'.
 *
 * \param cond  Condition variable pointer
 * \param mutex Optional pointer to mutex to unlock.
 */
void thread_cond_wait(struct thread_cond *cond, struct thread_mutex *mutex)
{
    dispatcher_handle_t disp = disp_disable();

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_COND_WAIT_ENTER,
                (uintptr_t)cond);

    acquire_spinlock(&cond->lock);

    // Release the lock
    if (mutex != NULL) {
        struct thread *wakeup = thread_mutex_unlock_disabled(disp, mutex);

        if(wakeup != NULL) {
            errval_t err = domain_wakeup_on_disabled(wakeup->disp, wakeup, disp);
            assert_disabled(err_is_ok(err));
        }
    }

    // Block on the condition variable and release spinlock
    thread_block_and_release_spinlock_disabled(disp, &cond->queue, &cond->lock);

    // Re-acquire the mutex
    if (mutex != NULL) {
        thread_mutex_lock(mutex);
    }

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_COND_WAIT_LEAVE,
                (uintptr_t)cond);
}

/**
 * \brief Signal a condition variable
 *
 * This function signals the condition variable, and wakes up one
 * thread waiting on it.
 *
 * \param cond Condition variable pointer
 */
void thread_cond_signal(struct thread_cond *cond)
{
    struct thread *wakeup = NULL;
    errval_t err = SYS_ERR_OK;

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_COND_SIGNAL,
                (uintptr_t)cond);

    // Wakeup one waiting thread
    dispatcher_handle_t disp = disp_disable();
    acquire_spinlock(&cond->lock);
    if (cond->queue != NULL) {
        wakeup = thread_unblock_one_disabled(disp, &cond->queue, NULL);
        if(wakeup != NULL) {
            err = domain_wakeup_on_disabled(wakeup->disp, wakeup, disp);
        }
    }
    release_spinlock(&cond->lock);
    disp_enable(disp);

    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "remote wakeup from condition signal");
    }

    if(wakeup != NULL) {
        // XXX: Need directed yield to inter-disp thread
        thread_yield();
    }
}

/**
 * \brief Broadcast signal a condition variable
 *
 * This function signals the condition variable, and wakes up all
 * threads waiting on it.
 *
 * \param cond Condition variable pointer
 */
void thread_cond_broadcast(struct thread_cond *cond)
{
    struct thread *wakeupq = NULL;
    bool foreignwakeup = false;

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_COND_BROADCAST,
                (uintptr_t)cond);

    // Wakeup all waiting threads
    dispatcher_handle_t disp = disp_disable();
    acquire_spinlock(&cond->lock);
    wakeupq = thread_unblock_all_disabled(disp, &cond->queue, NULL);
    release_spinlock(&cond->lock);
    disp_enable(disp);

    foreignwakeup = (wakeupq != NULL);
    // Now, wakeup all on foreign dispatchers
    while (wakeupq != NULL) {
        struct thread *wakeup = wakeupq;
        wakeupq = wakeupq->next;
        errval_t err = domain_wakeup_on(wakeup->disp, wakeup);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "remote wakeup from condition broadcast");
        }
    }

    if(foreignwakeup) {
        // XXX: Need directed yield to inter-disp thread
        thread_yield();
    }
}

/**
 * \brief Initialise a mutex
 *
 * \param mutex Mutex pointer
 */
void thread_mutex_init(struct thread_mutex *mutex)
{
    mutex->locked = 0;
    mutex->holder = NULL;
    mutex->queue = NULL;
    mutex->lock = 0;
}

/**
 * \brief Lock a mutex
 *
 * This blocks until the given mutex is unlocked, and then atomically locks it.
 *
 * \param mutex Mutex pointer
 */
void thread_mutex_lock(struct thread_mutex *mutex)
{
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_MUTEX_LOCK_ENTER,
                (uintptr_t)mutex);

    acquire_spinlock(&mutex->lock);
    if (mutex->locked > 0) {
        thread_block_and_release_spinlock_disabled(handle, &mutex->queue,
                                                   &mutex->lock);
    } else {
        mutex->locked = 1;
        mutex->holder = disp_gen->current;
        release_spinlock(&mutex->lock);
        disp_enable(handle);
    }

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_MUTEX_LOCK_LEAVE,
                (uintptr_t)mutex);
}

/**
 * \brief Lock a mutex
 *
 * This blocks until the given mutex is unlocked, and then atomically locks it.
 *
 * \param mutex Mutex pointer
 */
void thread_mutex_lock_nested(struct thread_mutex *mutex)
{
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_MUTEX_LOCK_NESTED_ENTER,
                (uintptr_t)mutex);

    acquire_spinlock(&mutex->lock);
    if (mutex->locked > 0
        && mutex->holder != disp_gen->current) {
        thread_block_and_release_spinlock_disabled(handle, &mutex->queue,
                                                   &mutex->lock);
    } else {
        mutex->locked++;
        mutex->holder = disp_gen->current;
        release_spinlock(&mutex->lock);
        disp_enable(handle);
    }

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_MUTEX_LOCK_NESTED_LEAVE,
                (uintptr_t)mutex);
}

/**
 * \brief Try to lock a mutex
 *
 * If the given mutex is unlocked, this atomically locks it and returns true,
 * otherwise it returns false immediately.
 *
 * \param mutex Mutex pointer
 *
 * \returns true if lock acquired, false otherwise
 */
bool thread_mutex_trylock(struct thread_mutex *mutex)
{
    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_MUTEX_TRYLOCK,
                (uintptr_t)mutex);

    // Try first to avoid contention
    if (mutex->locked > 0) {
        return false;
    }

    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    bool ret;

    acquire_spinlock(&mutex->lock);
    if (mutex->locked > 0) {
        ret = false;
    } else {
        ret = true;
        mutex->locked = 1;
        mutex->holder = disp_gen->current;
    }
    release_spinlock(&mutex->lock);

    disp_enable(handle);
    return ret;
}

/**
 * \brief Unlock a mutex, while disabled
 *
 * This function unlocks the given mutex. It may only be called while disabled.
 *
 * \param disp Dispatcher pointer
 * \param mutex Mutex pointer
 *
 * \return Pointer to thread to be woken on foreign dispatcher
 */
struct thread *thread_mutex_unlock_disabled(dispatcher_handle_t handle,
                                            struct thread_mutex *mutex)
{
    struct thread *ft = NULL;

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_MUTEX_UNLOCK,
                (uintptr_t)mutex);

    acquire_spinlock(&mutex->lock);
    assert_disabled(mutex->locked > 0);

    if(mutex->locked == 1) {
        // Wakeup one waiting thread
        if (mutex->queue != NULL) {
            // XXX: This assumes dequeueing is off the top of the queue
            mutex->holder = mutex->queue;
            ft = thread_unblock_one_disabled(handle, &mutex->queue, NULL);
        } else {
            mutex->holder = NULL;
            mutex->locked = 0;
        }
    } else {
        mutex->locked--;
    }

    release_spinlock(&mutex->lock);
    return ft;
}

/**
 * \brief Unlock a mutex
 *
 * This unlocks the given mutex.
 *
 * \param mutex Mutex pointer
 */
void thread_mutex_unlock(struct thread_mutex *mutex)
{
    dispatcher_handle_t disp = disp_disable();
    struct thread *wakeup = thread_mutex_unlock_disabled(disp, mutex);
    errval_t err = SYS_ERR_OK;

    if (wakeup != NULL) {
        err = domain_wakeup_on_disabled(wakeup->disp, wakeup, disp);
    }
    disp_enable(disp);

    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "remote wakeup from mutex unlock");
    }

    if(wakeup != NULL) {
        // XXX: Need directed yield to inter-disp thread
        thread_yield();
    }
}

void thread_sem_init(struct thread_sem *sem, unsigned int value)
{
    assert(sem != NULL);

    sem->value = value;
    sem->queue = NULL;
    sem->lock = 0;
}

void thread_sem_wait(struct thread_sem *sem)
{
    assert(sem != NULL);

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_SEM_WAIT_ENTER,
                (uintptr_t)sem);

    dispatcher_handle_t disp = disp_disable();
    acquire_spinlock(&sem->lock);

    if(sem->value < 1) {
        // Not possible to decrement -- wait!
        thread_block_and_release_spinlock_disabled(disp, &sem->queue, &sem->lock);
    } else {
        // Decrement possible
        sem->value--;
        release_spinlock(&sem->lock);
        disp_enable(disp);
    }

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_SEM_WAIT_LEAVE,
                (uintptr_t)sem);
}

bool thread_sem_trywait(struct thread_sem *sem)
{
    assert(sem != NULL);
    bool ret = false;

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_SEM_TRYWAIT,
                (uintptr_t)sem);

    dispatcher_handle_t disp = disp_disable();
    acquire_spinlock(&sem->lock);

    if(sem->value >= 1) {
        // Decrement possible
        sem->value--;
        ret = true;
    }

    release_spinlock(&sem->lock);
    disp_enable(disp);

    return ret;
}

void thread_sem_post(struct thread_sem *sem)
{
    assert(sem != NULL);

    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_SEM_POST, (uintptr_t)sem);

    dispatcher_handle_t disp = disp_disable();
    struct thread *wakeup = NULL;
    errval_t err = SYS_ERR_OK;
    acquire_spinlock(&sem->lock);

    // Wakeup one?
    if(sem->value == 0 && sem->queue != NULL) {
        wakeup = thread_unblock_one_disabled(disp, &sem->queue, NULL);
    } else {
        sem->value++;
    }

    if(wakeup != NULL) {
        err = domain_wakeup_on_disabled(wakeup->disp, wakeup, disp);
        assert_disabled(err_is_ok(err));
    }

    release_spinlock(&sem->lock);
    disp_enable(disp);

    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "remote wakeup from semaphore post");
    }

    if(wakeup != NULL) {
        // XXX: Need directed yield to inter-disp thread
        thread_yield();
    }
}
