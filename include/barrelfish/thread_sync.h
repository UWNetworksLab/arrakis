/**
 * \file
 * \brief Thread synchronization definitions.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_THREAD_SYNC_H
#define LIBBARRELFISH_THREAD_SYNC_H

#include <stdint.h>
#include <barrelfish_kpi/spinlocks_arch.h>

/// A thread of execution
struct thread;

struct thread_mutex {
    volatile int        locked;
    struct thread       *queue;
    spinlock_t          lock;
    struct thread       *holder;
};
#ifndef __cplusplus
#       define THREAD_MUTEX_INITIALIZER \
    { .locked = false, .queue = NULL, .lock = 0 }
#else
#       define THREAD_MUTEX_INITIALIZER                                \
    { false, (struct thread *)NULL, 0, (struct thread *)NULL }
#endif

struct thread_cond {
    struct thread       *queue;
    spinlock_t          lock;
};
#ifndef __cplusplus
#       define THREAD_COND_INITIALIZER \
    { .queue = NULL, .lock = 0 }
#else
#       define THREAD_COND_INITIALIZER \
    { (struct thread *)NULL, 0 }
#endif

struct thread_sem {
    volatile unsigned int       value;
    struct thread               *queue;
    spinlock_t                  lock;
};
#ifndef __cplusplus
#       define THREAD_SEM_INITIALIZER \
    { .value = 0, .queue = NULL, .lock = 0 }
#else
#       define THREAD_SEM_INITIALIZER \
    { 0, (struct thread *)NULL, 0 }
#endif

#endif
