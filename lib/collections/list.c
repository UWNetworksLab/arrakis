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

#include <collections/list.h>

/******************************************************
 * a simple linked list implementation.
 ******************************************************/

/*
 * private functions.
 */

static void list_push(collections_listnode *existing,
                      collections_listnode *insert)
{
    insert->next = existing->next;
    insert->prev = existing;
    existing->next = insert;
    insert->next->prev = insert;
}

static void *list_pop(collections_listnode *n)
{
    void *data = n->data;
    n->next->prev = n->prev;
    n->prev->next = n->next;
    n->next = n->prev = NULL;
    return data;
}

/*
 * Insert the data at the head.
 */
static collections_listnode *list_create_node(collections_listnode *start,
                                              collections_listnode *where,
                                              void *data)
{
    collections_listnode *newnode = (collections_listnode *)
                                    malloc(sizeof(collections_listnode));
    newnode->data = data;

    list_push(where, newnode);
    ((collections_header_data *)(start->data))->size ++;
    return newnode;
}

static void list_destroy_node(collections_listnode *start,
                              collections_listnode *node)
{
    list_pop(node);
    free(node);
    ((collections_header_data*)start->data)->size--;
}


/*
 * public functions.
 */

/*
 * Creates a new linked list.
 */
void collections_list_create(collections_listnode **start,
                             collections_release_data func)
{
    collections_listnode *t;

	//
	// this is an empty list containing only the header.
	//
    t = (collections_listnode *)malloc(sizeof(collections_listnode));
    t->next = t->prev = t;
    collections_header_data *h = (collections_header_data *)
                                 malloc(sizeof(collections_header_data));
    h->size = 0;
	h->data_free = func;
	h->cur_item = NULL;
    t->data = (void *)h;

    *start = t;
}

/*
 * Releases all the nodes in the list.
 */
void collections_list_release(collections_listnode *start)
{
    collections_release_data data_free =
        ((collections_header_data*)start->data)->data_free;
    collections_listnode *cur = start->next;

    //
    // travel through the rest of the
    // list and release all the nodes.
    //
    while (cur != start)
    {
        void *data = cur->data;
        if (data != NULL && data_free)
        {
            data_free(data);
        }
        list_destroy_node(start, cur);
        cur = start->next;
    }

    //
    // release the header.
    //
    free(start->data);
    free(start);

    return;
}

/*
 * Inserts an element in the head of the list.
 */
int32_t collections_list_insert(collections_listnode *start, void *data)
{
    collections_listnode *ret = list_create_node(start, start, data);
    if (ret) {
        // success ...
        return 0;
    }
    return 1;
}

/*
 * Inserts an element at the tail of the list.
 */
int32_t collections_list_insert_tail(collections_listnode *start, void *data)
{
    collections_listnode *ret = list_create_node(start, start->prev, data);
    if (ret) {
        // success ...
        return 0;
    }
    return 1;
}

/*
 * Returns the data that matches the user provided key.
 */
void *collections_list_find_if(collections_listnode *start,
                               collections_list_predicate p, void *arg)
{
    collections_listnode *cur = start->next;
    while (cur != start)
    {
        if (p(cur->data, arg))
        {
            return cur->data;
        }

        cur = cur->next;
    }
    return NULL;
}

/**
 * Remove the first item that matches the given predicate and return it.
 *
 * \return The removed item.
 */
void *collections_list_remove_if(collections_listnode *start,
                                 collections_list_predicate p, void *arg)
{
    collections_listnode *cur = start->next;
    while (cur != start)
    {
        if (p(cur->data, arg))
        {
            void *data = cur->data;
            list_destroy_node(start, cur);
            return data;
        }
        cur = cur->next;
    }
    return NULL;
}

/**
 * Remove all the items that match the given predicate.
 *
 * \return The number of items removed.
 */
uint32_t collections_list_remove_if_all(collections_listnode *start,
                                    collections_list_predicate p, void *arg)
{
    uint32_t items_removed = 0;

    collections_listnode *cur = start->next;
    while (cur != start)
    {
        if (p(cur->data, arg))
        {
            list_destroy_node(start, cur);
            items_removed++;
        }
        cur = cur->next;
    }

    return items_removed;
}

void *collections_list_remove_ith_item(collections_listnode *start,
                                       uint32_t item)
{
    void *element;

    uint32_t n = collections_list_size(start);
    if (item >= n) {
        element = NULL;
    } else if (item <= n/2) {

        collections_listnode *cur = start->next;
        while (item != 0) {
            cur = cur->next;
            item--;
        }
        element = cur->data;
        list_destroy_node(start, cur);

    } else {

        collections_listnode *cur = start;
        do {
            cur = cur ->prev;
            item++;
        } while (item !=n);
        element = cur->data;
        list_destroy_node(start, cur);
    }

    return element;
}

void *collections_list_get_ith_item(collections_listnode *start, uint32_t item)
{
    uint32_t n = collections_list_size(start);
    if (item >= n) {
        return NULL;
    }
    else if (item <= n / 2)
    {
        collections_listnode* cur = start->next;
        while (item != 0)
        {
            cur = cur->next;
            item--;
        }
        return cur->data;
    }
    else
    {
        collections_listnode *cur = start;
        do {
            cur = cur->prev;
            item++;
        } while (item != n);
        return cur->data;
    }
}

/*
 * Return the total number of nodes in the list.
 */
uint32_t collections_list_size(collections_listnode *start)
{
    return ((collections_header_data *)(start->data))->size;
}

#if 0
static void* list_front(collections_listnode* start)
{
    return (start->next == start) ? NULL : start->next->data;
}

static void* list_back(collections_listnode* start)
{
    return (start->prev == start) ? NULL : start->prev->data;
}
#endif

/*
 * A set of routines for iteratively traversing through
 * the linked list.
 */

/*
 * Opens the list for traversal.
 */
int32_t	collections_list_traverse_start(collections_listnode *start)
{
    collections_header_data* head = (collections_header_data *) start->data;

    if (head->cur_item) {
        // if the next item is not null, a
        // traversal is already in progress.
        printf("Error: list is already opened for traversal.\n");
        return -1;
    }

    head->cur_item = start;

    return 1;
}

/*
 * Fetches the next item in the list.
 * If all the items have been fetched,
 * returns null.
 */
void *collections_list_traverse_next(collections_listnode *start)
{
    collections_header_data *head = (collections_header_data *) start->data;

    if (!head->cur_item) {
        // asking for the next item without
        // starting the traversal.
        printf("Error: list must be opened for traversal.\n");
        return NULL;
    }

    head->cur_item = head->cur_item->next;
    if (head->cur_item == start)
    {
        return NULL;
    }
    else
    {
        return head->cur_item->data;
    }
}

/*
 * Finishes the list traversal.
 */
int32_t	collections_list_traverse_end(collections_listnode *start)
{
    collections_header_data *head = (collections_header_data *) start->data;

    if (!head->cur_item) {
        // closing without
        // starting the traversal.
        printf("Error: list must be opened before ending.\n");
        return -1;
    }

    head->cur_item = NULL;

    return 1;
}

int collections_list_visit(collections_listnode *start,
                           collections_list_visitor_func func, void *arg)
{
    collections_listnode *cur = start->next;
    while (cur != start && func(cur->data, arg))
    {
        cur = cur->next;
    }
    return cur == start;
}
