/* Copyright (c) 2007-2009, Stanford University
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of Stanford University nor the names of its 
*       contributors may be used to endorse or promote products derived from 
*       this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY STANFORD UNIVERSITY ``AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL STANFORD UNIVERSITY BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/ 

#ifdef MR_LOCK_MCS

#include <stdlib.h>
#include <assert.h>
#include "synch.h"
#include "atomic.h"

typedef struct mcs_lock_priv {
    struct mcs_lock         *mcs_head;
    struct mcs_lock_priv    *next;
    uintptr_t               locked;
} mcs_lock_priv;

typedef struct mcs_lock {
    mcs_lock_priv    *head;
} mcs_lock;

static mr_lock_t mcs_alloc(void)
{
    mcs_lock    *l;

    l = malloc(sizeof(mcs_lock));
    l->head = NULL;

    return l;
}

static mr_lock_t mcs_alloc_per_thread(mr_lock_t l)
{
    mcs_lock_priv    *priv;

    priv = malloc(sizeof(mcs_lock_priv));

    priv->mcs_head = l;
    priv->next = NULL;
    priv->locked = 0;

    return priv;
}

static void mcs_free (mr_lock_t l)
{
    free(l);
}

static void mcs_free_per_thread (mr_lock_t l)
{
    free(l);
}

static void mcs_acquire(mr_lock_t l)
{
    mcs_lock        *mcs;
    mcs_lock_priv   *prev, *priv;

    priv = l;
    mcs = priv->mcs_head;

    assert (priv->locked == 0);

    set_and_flush(priv->next, NULL);

    prev = (void*)(atomic_xchg((uintptr_t)priv, (void*)(&mcs->head)));
    if (prev == NULL) {
        /* has exclusive access on lock */
        return;
    }

    /* someone else has lock */

    /* NOTE: this ordering is important-- if locked after next assignment,
     * we may have a schedule that will spin forever */
    set_and_flush(priv->locked, 1);
    set_and_flush(prev->next, priv);

    while (atomic_read(&priv->locked)) { asm("":::"memory"); }
}

static void mcs_release (mr_lock_t l)
{
    mcs_lock        *mcs;
    mcs_lock_priv   *priv;

    priv = l;
    mcs = priv->mcs_head;

    if (priv->next == NULL) {
        if (cmp_and_swp(
            (uintptr_t)NULL,
            (void*)(&mcs->head), (uintptr_t)priv)) {
            /* we were the only one on the lock, now it's empty */
            return;
        }

        /* wait for next to get thrown on */
        while (((void*)atomic_read(&(priv->next))) == NULL) {
            asm("" ::: "memory");
        }
    }

    set_and_flush(priv->next->locked, 0);
}

mr_lock_ops mr_mcs_ops = {
    .alloc = mcs_alloc,
    .acquire = mcs_acquire,
    .release = mcs_release,
    .free = mcs_free,
    .alloc_per_thread = mcs_alloc_per_thread,
    .free_per_thread = mcs_free_per_thread,
};

#endif /*  MR_LOCK_MCS */
