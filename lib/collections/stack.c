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

#include <collections/list.h>
#include <collections/stack.h>

#include <assert.h>

/**
 * \brief Initialize the stack data structure.
 *
 * \param stack Pointer to a pointer to a stack. Filled-in by function.
 *
 * Example usage:
 * \code
 *  struct collections_stack *stack;
 *  collections_stack_create(&stack);
 * \endcode
 */
void collections_stack_create(struct collections_stack **stack)
{
    *stack = (struct collections_stack *)
             malloc(sizeof(struct collections_stack));
    assert(*stack != NULL);

    (*stack)->num_elements = 0;

    // create a linked list to hold the elements of the stack
    collections_list_create(&(*stack)->elements, NULL);

    return;
}

/**
 * \brief Returns the top element of the stack and removes it.
 */
void *collections_stack_pop(struct collections_stack *stack)
{
    return collections_list_remove_ith_item(stack->elements, 0);
}

/**
 * \brief Push an element to the top of the stack.
 */
void collections_stack_push(struct collections_stack *stack, void *element)
{
    collections_list_insert(stack->elements, element);

    return;
}

/**
 * \brief Returns the top element of the stack, does not remove it.
 */
void *collections_stack_top(struct collections_stack *stack)
{
    return collections_list_get_ith_item(stack->elements, 0);
}

/**
 * \brief Free all memory associated with the stack.
 */
void collections_stack_release(struct collections_stack *stack)
{
    if (stack == NULL) {
        return;
    }

    collections_list_release(stack->elements);
    free(stack);
}
