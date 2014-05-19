/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <errno.h>
#include <semaphore.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <string.h>
#include "posixcompat.h"

#include <octopus/init.h>
#include <octopus/semaphores.h>


int sem_init(sem_t *sem, int pshared, unsigned int value)
{
    POSIXCOMPAT_DEBUG("sem_init(%p, %d, %u)\n", sem, pshared, value);

    memset(sem, 0, sizeof(sem_t));

    if(pshared != 0) {
        oct_init();
        sem->pshared = 1;
        /* fprintf(stderr, "sem_init called with pshared != 0. Ignoring.\n"); */

        POSIXCOMPAT_DEBUG("%d: sem_init(%p, %d, %u)\n", disp_get_domain_id(), sem, pshared, value);
        //debug_printf("oct_sem_new\n");
        errval_t err = oct_sem_new(&sem->id, value);
        //debug_printf("sem->id now is: %d\n", sem->id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sem_new reterr");
        }
    } else {
        sem->pshared = 0;
        thread_sem_init(&sem->thread_sem, value);
    }
    return 0;
}

int sem_destroy(sem_t *sem)
{
    POSIXCOMPAT_DEBUG("sem_destroy(%p)\n", sem);
    assert(!"NYI");
    // Nothing needed
    return 0;
}

int sem_wait(sem_t *sem)
{
  	POSIXCOMPAT_DEBUG("%d: sem_wait(%p, %u):\n %p %p %p %p\n", disp_get_domain_id(), sem, sem->id,
	 __builtin_return_address(0),
	 __builtin_return_address(1),
	 __builtin_return_address(2),
     __builtin_return_address(3));

    if(!sem->pshared) {
        thread_sem_wait(&sem->thread_sem);
    } else {
        oct_init();

        errval_t err;
        err = oct_sem_wait(sem->id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sem_wait");
        }
    }

    return 0;
}

int sem_trywait(sem_t *sem)
{
	POSIXCOMPAT_DEBUG("%d: sem_trywait(%p, %u)\n", disp_get_domain_id(), sem, sem->id);

    if(!sem->pshared) {
        if(thread_sem_trywait(&sem->thread_sem)) {
            return 0;
        } else {
            errno = EAGAIN;
            return -1;
        }
    } else {
        oct_init();

        errval_t err = oct_sem_trywait(sem->id);
        if (err_is_ok(err)) {
	  		POSIXCOMPAT_DEBUG("%d: sem_trywait(%p, %u) success!\n", disp_get_domain_id(), sem, sem->id);
            return 0;
        }
        else if (err_no(err) == OCT_ERR_NO_RECORD) {
	  		POSIXCOMPAT_DEBUG("%d: sem_trywait(%p, %u) no success\n", disp_get_domain_id(), sem, sem->id);
            errno = EAGAIN;
            return -1;
        }
        else {
            USER_PANIC_ERR(err, "sem_wait");
        }
    }

    assert(!"Should not reach here");
}

int sem_post(sem_t *sem)
{
  	POSIXCOMPAT_DEBUG("%d: sem_post(%p, %u): %p %p %p %p\n", disp_get_domain_id(), sem, sem->id,
	 __builtin_return_address(0),
	 __builtin_return_address(1),
	 __builtin_return_address(2),
     __builtin_return_address(3));

    if(!sem->pshared) {
        thread_sem_post(&sem->thread_sem);
    } else {
        oct_init();

        errval_t err;
        err = oct_sem_post(sem->id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sem_post");
        }
    }

    return 0;
}
