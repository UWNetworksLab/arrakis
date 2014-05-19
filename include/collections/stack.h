/**
 * \file
 * \brief Barrelfish collections library stack
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _COLLECTIONS_STACK_H_
#define _COLLECTIONS_STACK_H_

#include <sys/cdefs.h>

#include <collections/list.h>

__BEGIN_DECLS

struct collections_stack {
    /**
     * total number of elements
     */
    uint32_t num_elements;

    /**
     * elements of the stack kept in a linked list
     */
    collections_listnode *elements;
};

void  collections_stack_create(struct collections_stack **stack);

void *collections_stack_pop(struct collections_stack *stack);

void  collections_stack_push(struct collections_stack *stack, void *element);

void *collections_stack_top(struct collections_stack *stack);

void  collections_stack_release(struct collections_stack *stack);

__END_DECLS

#endif // _COLLECTIONS_STACK_H_
