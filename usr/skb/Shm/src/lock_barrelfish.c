/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "config.h"
#include "memman.h"

static struct thread_mutex shm_mutex = THREAD_MUTEX_INITIALIZER;

int
mutex_lock_failed(plock)
a_mutex_t *plock;
{
    thread_mutex_lock(&shm_mutex);
    int res;
    if (*plock) {
        res = 1;
//	return 1;
    } else{
        *plock = 1;
        res = 0;
    }
    thread_mutex_unlock(&shm_mutex);
//    return 0;
    return res;
}

