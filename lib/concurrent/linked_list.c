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
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <concurrent/arch/cas.h>
#include <concurrent/linked_list.h>

static struct ll_element *search(struct ll_element *head,
        struct ll_element *tail, struct ll_element **left_node, void* key)
{
    assert(head != NULL);
    assert(tail != NULL);
    assert(left_node != NULL);
    assert(key != NULL);

    *left_node = NULL; // for sanity-check assertion below

    struct ll_element *left_node_next = NULL, *right_node = NULL;
    struct ll_element *current = head, *next = head->next;

    do {
        // empty list?
        if ((current == head) && (next == tail)) {
            (*left_node) = head;
            return tail;
        }

        // find left and right node
        if (!is_marked_reference((uintptr_t) next)) {
            (*left_node) = current;
            left_node_next = next;
        }
        current = (struct ll_element*) get_unmarked_reference(
                (uintptr_t) current->next);

        if (current != tail) {
            next = current->next;

            if (is_marked_reference((uintptr_t) next) || (current->key < key
                    && next != tail)) {
                continue;
            }
        }

        right_node = current;

        // check if nodes are adjacent
        if (left_node_next == right_node) {
            if ((right_node != tail) && is_marked_reference(
                    (uintptr_t) right_node->next)) {
                // start again
                current = head;
                continue;
            } else {
                return right_node;
            }
        }

        // remove marked nodes in between, since nodes are not adjacent
        // TODO: optimize!
        assert(*left_node != NULL);
        if (cas((uintptr_t*) (*left_node)->next, *(uintptr_t*) left_node_next,
                *(uintptr_t*) right_node)) {
            if ((right_node != tail) && is_marked_reference(
                    (uintptr_t) right_node->next)) {
                // start again
                current = head;
                continue;
            } else {
                return right_node;
            }
        }

    } while (1);
    return tail;
}

bool ll_contains(struct ll_element *head, struct ll_element *tail, void* key)
{
    struct ll_element *prev;
    struct ll_element *node = search(head, tail, &prev, key);
    return node != tail && node->key == key;
}

void ll_create(struct ll_element **head, struct ll_element **tail)
{
    (*head) = malloc(sizeof(struct ll_element));
    (**head).key = NULL;
    (*tail) = malloc(sizeof(struct ll_element));
    (**tail).key = NULL;
    (*head)->next = (*tail);
}

bool ll_insert(struct ll_element *head, struct ll_element *tail, void* new_key,
        void* data)
{
    assert(head != NULL);
    assert(tail != NULL);
    assert(new_key != NULL);
    struct ll_element *new_elem = malloc(sizeof(struct ll_element));
    assert(new_elem != NULL);
    new_elem->key = new_key;
    new_elem->data = data;

    struct ll_element *left_node = NULL, *right_node;

    do {
        right_node = search(head, tail, &left_node, new_key);
        assert(right_node != NULL);
        assert(left_node != NULL);

        if ((right_node != tail) && (right_node->key == new_key)) {
            return false;
        }
        new_elem->next = right_node;
    } while (!cas((uintptr_t*) &left_node->next, (uintptr_t) right_node,
            (uintptr_t) new_elem));
    return true;
}

bool ll_delete(struct ll_element *head, struct ll_element *tail, void* key)
{
    assert(head != NULL);
    assert(tail != NULL);
    assert(key != NULL);

    struct ll_element *right_node, *right_node_next, *left_node;

    do {
        right_node = search(head, tail, &left_node, key);
        if (right_node == tail || right_node->key != key) {
            return false;
        }
        right_node_next = right_node->next;
        if (!is_marked_reference((uintptr_t) right_node_next)) {
            if (cas((uintptr_t*) &right_node->next,
                    (uintptr_t) right_node_next, get_marked_reference(
                            (uintptr_t) right_node_next))) {
                break;
            }
        }
    } while (1);
    if (!cas((uintptr_t*) &left_node->next, (uintptr_t) right_node,
            (uintptr_t) right_node_next)) {
        right_node = search(head, tail, &left_node, right_node->key);
    }
    return true;
}

void ll_print(struct ll_element *head, struct ll_element *tail)
{
    printf("LIST {");
    for (struct ll_element *current = head->next; current != tail; current
            = current->next) {
        assert(current->key != NULL);
        printf("key %ld,", (uint64_t) * (uintptr_t*) current->key);

        struct ll_element *next;
        do {
            next = current->next;
            current = (struct ll_element *) get_unmarked_reference((uintptr_t) next);
        } while (is_marked_reference((uintptr_t) next));
    }
    printf("}\n");
}

