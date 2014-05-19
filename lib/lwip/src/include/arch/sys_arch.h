/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SYS_ARCH_H
#define SYS_ARCH_H

#include <stdbool.h>
#include <assert.h>
#include <barrelfish/thread_sync.h>

/// Protection level
typedef u8_t    sys_prot_t;

typedef struct thread_wrapper *sys_thread_t;

typedef struct thread_sem *sys_sem_t;

struct bf_sys_mbox {
    void *msg;
    bool empty;
    struct thread_mutex mutex;
    struct thread_cond changed_cond;
};
typedef struct bf_sys_mbox * sys_mbox_t;


#define SYS_MBOX_NULL   0
#define SYS_SEM_NULL    0

#endif
