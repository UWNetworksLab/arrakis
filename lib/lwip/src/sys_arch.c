/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <lwip/sys.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>

/*
 * TODO:
 * - implement timeout versions of semaphores and mailboxes correctly
 * - implement mboxes of size > 1
 * - implement sys_msleep
 * - implement sys_jiffies
 */


// from example in http://lwip.wikia.com/wiki/Porting_for_an_OS
struct thread_wrapper {
  struct thread_wrapper *next;
  struct thread *thread;  // barrelfish thread
  struct sys_timeouts timeouts;
};


// track threads and timeouts.
struct sys_timeouts lwip_system_timeouts = { .next = NULL }; // Default timeouts list for lwIP
struct thread_wrapper *lwip_system_threads = NULL; // a list of all threads created by lwIP


// wrapper for a condition variable with timeout
static u32_t thread_cond_wait_timeout(struct thread_cond *cond,
                                      struct thread_mutex *mutex,
                                      u32_t timeout)
{
    // TODO: implement timeout properly
    thread_cond_wait(cond, mutex);
    return timeout;
}


static void *last_locker = NULL, *last_user = NULL;
struct thread_mutex *lwip_mutex;

void lwip_mutex_lock(void)
{
    if (lwip_mutex != NULL) {
      	last_locker = __builtin_return_address(0);
        thread_mutex_lock(lwip_mutex);
    }
}

void lwip_mutex_unlock(void)
{
    if (lwip_mutex != NULL) {
    	if(lwip_mutex->locked == 0) {
			printf("mutex NOT locked, called %p, last %p, locked by %p\n", __builtin_return_address(0), last_user, last_locker);
      	} else {
			last_user = __builtin_return_address(0);
      	}
       	 thread_mutex_unlock(lwip_mutex);
    }
}


#if SYS_LIGHTWEIGHT_PROT
static struct thread_mutex lock = THREAD_MUTEX_INITIALIZER;

sys_prot_t sys_arch_protect(void)
{
    thread_mutex_lock(&lock);
    return 0;
}

void sys_arch_unprotect(sys_prot_t pval)
{
    // this is always true (sys_prot_t is uint8_t!) -AB
    //if(pval >= 0) {
        thread_mutex_unlock(&lock);
    //}
}

#endif // SYS_LIGHTWEIGHT_PROT

/* sys_init() must be called before anthing else. */
void sys_init(void)
{
    // Nothing to do
}



// from example in http://lwip.wikia.com/wiki/Porting_for_an_OS
struct sys_timeouts *sys_arch_timeouts(void) {
    struct thread_wrapper *thread = lwip_system_threads;
    struct thread *self = thread_self(); // current thread

    // Search the threads list for the thread that is currently running
    for ( ; thread != NULL; thread = thread->next) {
        if (thread->thread == self) {
            return &thread->timeouts;
        }
    }

    // No match, so just return the system-wide default version
    return &lwip_system_timeouts;
}



/* Semaphore functions. */
sys_sem_t sys_sem_new(u8_t count)
{
    /* debug_printf("sys_sem_new(%d)\n", count); */
    struct thread_sem *newsem = malloc(sizeof(struct thread_sem));
    thread_sem_init(newsem, count);
    return newsem;
}

void sys_sem_signal(sys_sem_t sem)
{
    /* printf("sys_sem_signal\n"); */
    thread_sem_post(sem);
}

u32_t sys_arch_sem_wait(sys_sem_t sem, u32_t timeout)
{
    // TODO: implement timeout correctly

    /* printf("sys_arch_sem_wait\n"); */

    systime_t start, end;
    start = get_system_time();

    thread_sem_wait(sem);

    end = get_system_time();

    return end - start;
}

void sys_sem_free(sys_sem_t sem)
{
    free(sem);
}





u32_t sys_jiffies(void)
{
    /* since power up. */
    assert(!"NYI");
    return 0;
}

/* Mailbox functions. */
sys_mbox_t sys_mbox_new(int size)
{
    // TODO: support mailboxes of size > 1. we only support size 1 right now

    // debug_printf("sys_mbox_new(%d)\n", size);

    sys_mbox_t mbox;
    mbox = (sys_mbox_t)malloc(sizeof(struct bf_sys_mbox));

    if (mbox == NULL) {
        return SYS_MBOX_NULL;
    }
    mbox->msg = NULL;
    mbox->empty = true;

    thread_mutex_init(&mbox->mutex);
    thread_cond_init(&mbox->changed_cond);

//    debug_printf("sys_mbox_new(%p): created of size %d\n", mbox, size);
    return mbox;
}

void sys_mbox_post(sys_mbox_t mbox, void *msg)
{
//    debug_printf("sys_mbox_post(%p)\n", mbox);

    // keep trying until we succeed
    while (1) {
        thread_mutex_lock(&mbox->mutex);
        if (mbox->empty) {
            mbox->msg = msg;
            mbox->empty = false;
            thread_mutex_unlock(&mbox->mutex);
            thread_cond_signal(&mbox->changed_cond);
            break;
        } else {
            // wait until something changes
            thread_cond_wait(&mbox->changed_cond, &mbox->mutex);
            thread_mutex_unlock(&mbox->mutex);
        }
    }

//    debug_printf("sys_mbox_post(%p) done\n", mbox);
}

err_t sys_mbox_trypost(sys_mbox_t mbox, void *msg)
{
//    debug_printf("sys_mboxtry_post(%p)\n", mbox);

    err_t err;
    thread_mutex_lock(&mbox->mutex);
    if (mbox->empty) {
        mbox->msg = msg;
        mbox->empty = false;
        thread_cond_signal(&mbox->changed_cond);
        err = ERR_OK;
    } else {
        err = ERR_MEM;
    }
    thread_mutex_unlock(&mbox->mutex);

    return err;
}

u32_t sys_arch_mbox_fetch(sys_mbox_t mbox, void **msg, u32_t timeout)
{
//    debug_printf("sys_arch_mbox_fetch(%p) timeout %u\n",mbox, timeout);

    u32_t time_left = timeout;
    u32_t res;

    systime_t start, end;
    start = get_system_time();

    // keep trying until we succeed
    while (1) {
        if (time_left <= 0 && timeout != 0) {
            return SYS_ARCH_TIMEOUT;
        }
        thread_mutex_lock(&mbox->mutex);
        if (!mbox->empty) {
            if (msg != NULL) {
                *msg = mbox->msg;
            }
            mbox->empty = true;
            thread_mutex_unlock(&mbox->mutex);
            thread_cond_signal(&mbox->changed_cond);
            break;
        } else {
            // wait until something changes
            if (timeout != 0) {
                time_left -= thread_cond_wait_timeout(&mbox->changed_cond,
                                                      &mbox->mutex,
                                                      time_left);
            } else {
                thread_cond_wait(&mbox->changed_cond, &mbox->mutex);
            }
            thread_mutex_unlock(&mbox->mutex);
        }
    }

    end = get_system_time();

    if (timeout == 0) {
        res = end - start;
    } else {
        res = timeout - time_left;
    }

//    debug_printf("sys_arch_mbox_fetch(%p) done!\n", mbox);
    return res;
}

u32_t sys_arch_mbox_tryfetch(sys_mbox_t mbox, void **msg)
{
    //    assert(!"NYI");
    u32_t res;
    thread_mutex_lock(&mbox->mutex);
    if (!mbox->empty) {
        *msg = mbox->msg;
        mbox->empty = true;
        thread_cond_signal(&mbox->changed_cond);
        res = 0; // success
    } else {
        res = SYS_MBOX_EMPTY;
    }
    thread_mutex_unlock(&mbox->mutex);

    return res;
}

void sys_mbox_free(sys_mbox_t mbox)
{
    assert(mbox != NULL);
    assert(mbox->empty);
    free(mbox);
}

// from example in http://lwip.wikia.com/wiki/Porting_for_an_OS
sys_thread_t sys_thread_new(char *name, void (* thread)(void *arg),
                            void *arg, int stacksize, int prio)
{
    sys_thread_t newthread;
    SYS_ARCH_DECL_PROTECT(old_val);

//    debug_printf("sys_thread_new(%s, %p, %p)\n", name, thread, arg);

    // TODO: this has to get free'd when the thread terminates
    newthread = (sys_thread_t)malloc(sizeof(struct thread_wrapper));
    if (newthread == NULL) {
        return NULL;
    }

    // Need to protect this -- preemption here could be a problem!
    SYS_ARCH_PROTECT(old_val);
    newthread->next = lwip_system_threads;
    lwip_system_threads = newthread;
    SYS_ARCH_UNPROTECT(old_val);

    newthread->timeouts.next = NULL; // initialize the linked list to NULL

    // TODO: do something with the rest of the arguments (name, stacksize, prio)
    newthread->thread = thread_create((thread_func_t)thread, arg);
    if (newthread->thread == NULL) {
        SYS_ARCH_PROTECT(old_val);
        lwip_system_threads = newthread->next;
        SYS_ARCH_UNPROTECT(old_val);
        free(newthread);
        newthread = NULL;
    }

    return newthread;
}

