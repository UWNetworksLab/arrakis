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

#ifndef QUEUE_H_
#define QUEUE_H_

#include "list.h"

typedef list queue_t;
typedef list_ent queue_elem_t;

#define queue_entry(QUEUE_ELEM, STRUCT, MEMBER)    \
    list_entry(QUEUE_ELEM, STRUCT, MEMBER)

/**
 * Initialize queue for use
 */
static inline void queue_init (queue_t *q)
{
    assert (q != NULL);
    list_init(q);
}

/**
 * Add element to end of queue
 */
static inline void queue_push_back (queue_t *q, queue_elem_t *qe)
{
    assert (q != NULL);
    assert (qe != NULL);

    list_add_tail(q, qe);
}

/**
 * Remove element at end of queue
 * @return 0 on empty, 1 if found
 */
static inline int queue_pop_back (queue_t *q, queue_elem_t **qe)
{
    queue_elem_t    *tail;

    assert (q != NULL);
    assert (qe != NULL);

    tail = list_remove_tail(q);
    if (tail == NULL)
        return 0;

    *qe = tail;

    return 1;
}

/**
 * Put element at front of queue
 */
static inline void queue_push_front (queue_t *q, queue_elem_t *qe)
{
    assert (q != NULL);
    assert (qe != NULL);

    list_add_head(q, qe);
}

/**
 * Remove element from front of queue
 * @return 0 if queue was empty, 1 if successful
 */
static inline int queue_pop_front (queue_t *q, queue_elem_t **qe)
{
    queue_elem_t    *head;

    assert (q != NULL);
    assert (qe != NULL);

    head = list_remove_head(q);
    if (head == NULL)
        return 0;

    *qe = head;

    return 1;
}

#endif /* QUEUE_H_ */
