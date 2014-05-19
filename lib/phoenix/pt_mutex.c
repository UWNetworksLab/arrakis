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

#include <stdlib.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include "synch.h"

static mr_lock_t ptmutex_alloc(void)
{
    struct thread_mutex *m;

    m = malloc(sizeof(struct thread_mutex));
    assert (m != NULL);

    thread_mutex_init(m);
    return m;
}

static void ptmutex_acquire(mr_lock_t l)
{
    thread_mutex_lock(l);
}

static void ptmutex_release(mr_lock_t l)
{
    thread_mutex_unlock(l);
}

static void ptmutex_free(mr_lock_t l)
{
    free(l);
}

static mr_lock_t ptmutex_alloc_per_thread(mr_lock_t l)
{
    return l;
}

static void ptmutex_free_per_thread(mr_lock_t l)
{
}

mr_lock_ops mr_ptmutex_ops = {
    .alloc = ptmutex_alloc,
    .acquire = ptmutex_acquire,
    .release = ptmutex_release,
    .free = ptmutex_free,
    .alloc_per_thread = ptmutex_alloc_per_thread,
    .free_per_thread = ptmutex_free_per_thread,
};


