/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SEMAPHORE_H
#define SEMAPHORE_H

#include <barrelfish/thread_sync.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

struct posix_semaphore {
    int pshared;
    struct thread_sem thread_sem;
    uint32_t id;
};

typedef struct posix_semaphore sem_t;

int sem_init(sem_t *sem, int pshared, unsigned int value);
int sem_destroy(sem_t *sem);
int sem_wait(sem_t *sem);
int sem_trywait(sem_t *sem);
int sem_post(sem_t *sem);

__END_DECLS

#endif
