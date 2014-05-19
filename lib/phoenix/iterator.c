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

#include "iterator.h"
#include "memory.h"

#if defined(_LINUX_)
#include <stdlib.h>
#endif

typedef struct iterator_t iterator_t;

int iter_init (iterator_t *itr, int num_lists)
{
    assert (itr);
    assert (num_lists > 0);

    itr->list_array = 
        (keyvals_t **)phoenix_mem_malloc (sizeof(keyvals_t *) * num_lists);

    if (! itr->list_array) {
        return -1;
    }

    itr->max_list = num_lists;
    itr->next_insert_pos = 0;
    itr->current_list = 0;
    itr->current_index = 0;
    itr->val = NULL;
    itr->size = 0;

    return 0;
}

void iter_reset (iterator_t *itr)
{
    assert (itr);

    itr->next_insert_pos = 0;
    itr->current_list = 0;
    itr->current_index = 0;
    itr->val = NULL;
    itr->size = 0;
}

void iter_rewind (iterator_t *itr)
{
    assert (itr);

    itr->current_list = 0;
    itr->current_index = 0;
    itr->val = itr->list_array[0]->vals;
}

void iter_finalize (iterator_t *itr)
{
    assert (itr);
    assert (itr->list_array);

    phoenix_mem_free (itr->list_array);
}

int iter_add (iterator_t *itr, keyvals_t *list)
{
    assert (itr);
    assert (list->len);

    if (itr->next_insert_pos >= itr->max_list)
    {
        /* Iterator full. */
        return -1;
    }
    
    itr->list_array[itr->next_insert_pos] = list;

    if (itr->next_insert_pos == 0)
    {
        itr->val = list->vals;
    }
    itr->next_insert_pos += 1;
    itr->size += list->len;

    return 0;
}

/* Returns 1 when element exists.
   Returns 0 when endpoint reached. */
int iter_next (iterator_t *itr, void **addr)
{
    assert (itr);

    if (itr->current_index >= itr->val->next_insert_pos)
    {
        /* Should hop to a different chunk. */
        if (itr->val->next_val)
        {
            /* Hop to next chunk on the same list. */
            itr->val = itr->val->next_val;
            itr->current_index = 0;

            /* Prefetch next chunk on the same list. */
            if (itr->val->next_val) {
                __builtin_prefetch (itr->val->next_val->array, 0, 0);
            }
        }
        else if (itr->current_list + 1 < itr->next_insert_pos)
        {
            /* Hop to the first block of next list. */
            itr->current_list += 1;
            itr->val = itr->list_array[itr->current_list]->vals;
            itr->current_index = 0;

            /* Prefetch next chunk on the same list. */
            if (itr->val->next_val) {
                __builtin_prefetch (itr->val->next_val->array, 0, 0);
            }
        }
        else
        {
            /* Endpoint reached. */
            *addr = NULL;
            return 0;
        }
    }

    *addr = itr->val->array[itr->current_index++];

    return 1;
}

int iter_next_list (iterator_t *itr, keyvals_t **list)
{
    assert (itr);

    if (itr->current_list >= itr->next_insert_pos)
    {
        *list = NULL;
        return 0;
    }

    *list = itr->list_array[itr->current_list++];
    return 1;
}

int iter_size (iterator_t *itr)
{
    assert (itr);

    return itr->size;
}
