/** \file
 *  \brief A non-blocking linked list implementation, derived from Tim Harris' `pragmatic implementation`.
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef LINKED_LIST_H_
#define LINKED_LIST_H_

#include <sys/cdefs.h>

__BEGIN_DECLS

struct ll_element {
    struct ll_element *next;
    void* key;
    void* data;
};

/*
 * \brief create a new (empty) linked list. Creation is not thread-safe.
 */
void ll_create(struct ll_element **head, struct ll_element **tail);

/*
 * \brief insert a new element into the linked list.
 */
bool ll_insert(struct ll_element *head, struct ll_element *tail,
                void* new_key, void* new_data);

/*
 * \brief delete an element from the list.
 */
bool ll_delete(struct ll_element *head, struct ll_element *tail, void* key);

/*
 * \brief check if the list contains an element.
 */
bool ll_contains(struct ll_element *head, struct ll_element *tail, void* key);

/*
 * \brief print the content of the list to the screen. For debugging.
 */
void ll_print(struct ll_element *head, struct ll_element *tail);

static inline const bool is_marked_reference(const uintptr_t p)
{
    return p & 0x1;
}

static inline const uintptr_t get_unmarked_reference(const uintptr_t p)
{
    return p & 0xFFFFFFFFFFFFFFFE;
}

static inline const uintptr_t get_marked_reference(const uintptr_t p)
{
    return p | 0x1;
}

__END_DECLS

#endif /* LINKED_LIST_H_ */
