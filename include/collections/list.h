/**
 * \file
 * \brief Barrelfish collections library list data structure
 */
/*
 * Copyright (c) 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef _LIST_H_
#define _LIST_H_

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/cdefs.h>

#ifdef WIN32
#include <malloc.h>
#include "stdint.h"
#endif // WIN32

#ifdef BARRELFISH
#include <stdint.h>
#include <barrelfish/barrelfish.h>
#endif // BARRELFISH

__BEGIN_DECLS

/*
 * Predicate function.
 * Should return zero for false, non-zero otherwise.
 */
typedef int32_t (*collections_list_predicate)(void *data, void *arg);

/*
 * a function that can be used to release the user-supplied
 * data items.
 */
typedef void (*collections_release_data)(void *data);

/*
 * structure of each element in the
 * linked list.
 */
struct          _collections_listnode;

typedef struct	_collections_listnode {
    //
    // pointer to the previous node.
    //
    struct _collections_listnode *prev;

    //
    // pointer to the next node.
    //
    struct _collections_listnode *next;

    //
    // an abstract data value to store.
    //
    void                         *data;
} collections_listnode;

/*
 * a header to the linked list
 */
typedef struct _collections_header_data {
    // total number of elements.
    uint32_t                 size;

    // comparison function provided by the user.
    collections_release_data data_free;

    // a pointer to keep track of
    // traversing the list.
    collections_listnode    *cur_item;
} collections_header_data;


/*
 * functions ...
 */

void      collections_list_create(collections_listnode **start,
                                  collections_release_data func);
void      collections_list_release(collections_listnode *start);
int32_t   collections_list_insert(collections_listnode *start, void *data);
int32_t   collections_list_insert_tail(collections_listnode *start, void *data);
void     *collections_list_get_ith_item(collections_listnode *start,
                                        uint32_t index);
void     *collections_list_find_if(collections_listnode *start,
                                   collections_list_predicate p, void *arg);
void     *collections_list_remove_if(collections_listnode *start,
                                     collections_list_predicate p, void *key);
uint32_t  collections_list_remove_if_all(collections_listnode *start,
                                         collections_list_predicate p,
                                         void *key);
void     *collections_list_remove_ith_item(collections_listnode *start,
                                           uint32_t index);
uint32_t  collections_list_size(collections_listnode *start);
int32_t   collections_list_traverse_start(collections_listnode *start);
void     *collections_list_traverse_next(collections_listnode *start);
int32_t   collections_list_traverse_end(collections_listnode *start);

/*
 * Visitor function. Should return non-zero to continue iteration.
 */
typedef int (*collections_list_visitor_func)(void *data, void *arg);
/*
 * Visit elements in list with function f until f indicates stop or end of list
 * reached.
 * Return non-zero if end of list reached, 0 otherwise.
 */
int collections_list_visit(collections_listnode *start,
                           collections_list_visitor_func f, void *arg);

__END_DECLS

#endif
