/*
 * Copyright (c) 2013, 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <pthread.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <errno.h>
#include <string.h>
#include <signal.h>

typedef void (*destructor_fn_t)(void *);
typedef void *(*start_fn_t)(void *);

struct pthread_mutex {
    struct thread_mutex mutex;
    int locked;
};

struct pthread_cond {
    struct thread_cond cond;
};

struct pthread {
    struct thread *thread;
    const void *keys[PTHREAD_KEYS_MAX];
    start_fn_t start_fn;
    void *arg;
    void *retval;
};

struct pthread_attr {
    int stacksize;
};

static pthread_key_t key_index = 0;
static struct thread_mutex key_mutex = THREAD_MUTEX_INITIALIZER;
static destructor_fn_t destructors[PTHREAD_KEYS_MAX];

static int start_pthread(void *arg)
{
    struct pthread *myself = arg;

    // Initialize TLS
    thread_set_tls_key(0, myself);

    // Run the thread
    myself->retval = myself->start_fn(myself->arg);

    // Call all key destructors
    for(pthread_key_t i = 0; i < key_index; i++) {
        if(destructors[i] != NULL) {
            destructors[i]((void *)myself->keys[i]);
        }
    }

    // 'myself' data structure is freed when joined with this thread
    return 0;
}

int pthread_create(pthread_t *pthread, const pthread_attr_t *attr,
                   void *(*start_routine) (void *), void *arg)
{
    size_t stacksize = THREADS_DEFAULT_STACK_BYTES;

    if(attr != NULL) {
        stacksize = (*attr)->stacksize;
    }

    *pthread = malloc(sizeof(struct pthread));
    assert(*pthread != NULL);
    memset(*pthread, 0, sizeof(struct pthread));

    // XXX: attributes are ignored.
    (*pthread)->start_fn = start_routine;
    (*pthread)->arg = arg;

    // Start the thread
    (*pthread)->thread = thread_create_varstack(start_pthread, *pthread,
                                                stacksize);
    return 0;
}

pthread_t pthread_self(void)
{
    pthread_t self = thread_get_tls_key(0);

    // If NULL, we're the first thread, not created via pthreads.
    // Create a pthread structure.
    if(self == NULL) {
        struct pthread *myself = malloc(sizeof(struct pthread));
        assert(myself != NULL);
        memset(myself, 0, sizeof(struct pthread));
        myself->thread = thread_self();
        thread_set_tls_key(0, myself);
        self = myself;
    }

    return self;
}

void *pthread_getspecific(pthread_key_t key)
{
    if(key >= PTHREAD_KEYS_MAX) {
        return NULL;
    }

    return (void *)pthread_self()->keys[key];
}

int pthread_setspecific(pthread_key_t key, const void *val)
{
    if(key >= PTHREAD_KEYS_MAX) {
        return EINVAL;
    }

    pthread_self()->keys[key] = val;
    return 0;
}

int pthread_attr_init(pthread_attr_t *attr)
{
    *attr = malloc(sizeof(struct pthread_attr));
    (*attr)->stacksize = THREADS_DEFAULT_STACK_BYTES;
    // No attributes
    return 0;
}

static struct thread_mutex mutex_mutex = THREAD_MUTEX_INITIALIZER;

int pthread_mutex_init(pthread_mutex_t *mutex,
                       const pthread_mutexattr_t *attr)
{
    // XXX: Attributes ignored.
    *mutex = malloc(sizeof(struct pthread_mutex));
    if(*mutex == NULL) {
        return -1;
    }

    thread_mutex_init(&(*mutex)->mutex);
    (*mutex)->locked = 0;
    return 0;
}

int pthread_mutex_destroy(pthread_mutex_t *mutex)
{
    if(*mutex != PTHREAD_MUTEX_INITIALIZER) {
        free(*mutex);
    }

    return 0;
}

int pthread_mutex_lock(pthread_mutex_t *mutex)
{
    thread_mutex_lock(&mutex_mutex);

    if(*mutex == PTHREAD_MUTEX_INITIALIZER) {
        pthread_mutex_init(mutex, NULL);
    }

    (*mutex)->locked++;
    thread_mutex_unlock(&mutex_mutex);
    thread_mutex_lock(&(*mutex)->mutex);
    return 0;
}

int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
    thread_mutex_lock(&mutex_mutex);

    if(*mutex == PTHREAD_MUTEX_INITIALIZER) {
        pthread_mutex_init(mutex, NULL);
    }

    if((*mutex)->locked == 0) {
        thread_mutex_unlock(&mutex_mutex);
        return 0;
    }

    (*mutex)->locked--;
    thread_mutex_unlock(&mutex_mutex);
    thread_mutex_unlock(&(*mutex)->mutex);
    return 0;
}

int pthread_mutex_trylock(pthread_mutex_t *mutex)
{
    thread_mutex_lock(&mutex_mutex);

    if(*mutex == PTHREAD_MUTEX_INITIALIZER) {
        pthread_mutex_init(mutex, NULL);
    }

    thread_mutex_unlock(&mutex_mutex);

    int retval = (thread_mutex_trylock(&(*mutex)->mutex) ? 0 : EBUSY);

    if(retval != EBUSY) {
        thread_mutex_lock(&mutex_mutex);
        (*mutex)->locked++;
        thread_mutex_unlock(&mutex_mutex);
    }

    return retval;
}

int pthread_cond_init(pthread_cond_t *cond,
			const pthread_condattr_t *attr)
{
    *cond = malloc(sizeof(struct pthread_cond));
    if(*cond == NULL) {
        return -1;
    }

    thread_cond_init(&(*cond)->cond);
    return 0;
}

int pthread_cond_signal(pthread_cond_t *cond)
{
    thread_mutex_lock(&mutex_mutex);
    if(*cond == PTHREAD_COND_INITIALIZER) {
        pthread_cond_init(cond, NULL);
    }
    thread_mutex_unlock(&mutex_mutex);

    thread_cond_signal(&(*cond)->cond);
    return 0;
}

int pthread_cond_timedwait(pthread_cond_t *cond,
                           pthread_mutex_t *mutex,
                           const struct timespec *timeout)
{
    thread_mutex_lock(&mutex_mutex);
    if(*cond == PTHREAD_COND_INITIALIZER) {
        pthread_cond_init(cond, NULL);
    }
    if(*mutex == PTHREAD_MUTEX_INITIALIZER) {
        pthread_mutex_init(mutex, NULL);
    }
    thread_mutex_unlock(&mutex_mutex);

    assert(!"NYI");
}

int pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex)
{
    thread_mutex_lock(&mutex_mutex);
    if(*cond == PTHREAD_COND_INITIALIZER) {
        pthread_cond_init(cond, NULL);
    }
    if(*mutex == PTHREAD_MUTEX_INITIALIZER) {
        pthread_mutex_init(mutex, NULL);
    }
    thread_mutex_unlock(&mutex_mutex);

    thread_cond_wait(&(*cond)->cond, &(*mutex)->mutex);
    return 0;
}

int pthread_join(pthread_t thread, void **retval)
{
    errval_t err = thread_join(thread->thread, NULL);
    assert(err_is_ok(err));

    *retval = thread->retval;
    free(thread);
    return 0;
}

int pthread_key_create(pthread_key_t *key,
                       void (*callback) (void *))
{
    int retval = 0;

    thread_mutex_lock(&key_mutex);

    if(key_index == PTHREAD_KEYS_MAX) {
        retval = EAGAIN;
        goto out;
    }

    *key = key_index;
    destructors[key_index] = callback;
    key_index++;

 out:
    thread_mutex_unlock(&key_mutex);
    return retval;
}

int pthread_setcancelstate(int state, int *oldstate)
{
    // XXX: Not supported
    if(oldstate != NULL) {
        *oldstate = PTHREAD_CANCEL_ENABLE;
    }
    return 0;
}

int pthread_setcanceltype(int type, int *oldtype)
{
    // XXX: Not supported
    if(oldtype != NULL) {
        *oldtype = PTHREAD_CANCEL_DEFERRED;
    }
    return 0;
}

int pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset)
{
    return sigprocmask(how, set, oldset);
}

int pthread_attr_getstacksize(const pthread_attr_t *attr, size_t *stacksize)
{
    *stacksize = (*attr)->stacksize;
    return 0;
}

int pthread_attr_setstacksize(pthread_attr_t *attr, size_t stacksize)
{
    (*attr)->stacksize = stacksize;
    return 0;
}

int pthread_cancel(pthread_t thread)
{
    assert(!"NYI");
}
