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

#include <assert.h>
#include <stdio.h>

#include "synch.h"

extern mr_lock_ops mr_mcs_ops;
extern mr_lock_ops mr_ptmutex_ops;


#ifdef MR_LOCK_MCS
#define OPS mr_mcs_ops
#elif defined(MR_LOCK_PTMUTEX)
#define OPS mr_ptmutex_ops
#else
#error No lock type defined
#endif


/* Initialize lock structure.
   Returns pointer to lock structure if successful. */
mr_lock_t lock_alloc (void)
{
    mr_lock_t   mr;

    mr = OPS.alloc();
    assert (mr != NULL);

    return mr;
}

/* Initialize private lock structure.
   Should be called by all the participating threads.
   Returns lock structure to use for locking by thread
 */
mr_lock_t lock_alloc_per_thread(mr_lock_t parent)
{
    mr_lock_t   mr;

    assert (parent != NULL);

    mr = OPS.alloc_per_thread(parent);
    assert (mr != NULL);

    return mr;
}

/* Acquire the lock. */
void lock_acquire (mr_lock_t lock)
{
    assert (lock != NULL);
    OPS.acquire(lock);
}

/* Release the lock. */
void lock_release (mr_lock_t lock)
{
    assert (lock != NULL);
    OPS.release(lock);
}

/* Destroy the lock. */
void lock_free (mr_lock_t lock)
{
    assert (lock != NULL);
    OPS.free(lock);
}

/* Destroy the private lock.
   Returns 0 if successful. */
void lock_free_per_thread (mr_lock_t lock)
{
    assert (lock != NULL);
    OPS.free_per_thread(lock);
}
