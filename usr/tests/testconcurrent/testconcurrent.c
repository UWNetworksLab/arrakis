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
#include <concurrent/arch/cas.h>
#include <concurrent/linked_list.h>
#include <stdio.h>

static struct ll_element *list_head;
static struct ll_element *list_tail;

int main(void) {
    /// cas test
    uint64_t location = 10;
    uint64_t res = cas(&location, 10, 20);
    printf("cas: %ld, %ld\n", res, location);

    /// linked list test
    ll_create(&list_head, &list_tail);
    ll_print(list_head, list_tail);

    uint64_t i = 10;
    ll_insert(list_head, list_tail, &i, NULL);
    ll_print(list_head, list_tail);
    uint64_t j = 20;
    ll_insert(list_head, list_tail, &j, NULL);
    ll_print(list_head, list_tail);
    uint64_t k = 30;
    ll_insert(list_head, list_tail, &k, NULL);
    ll_print(list_head, list_tail);
    uint64_t l = 40;
    ll_insert(list_head, list_tail, &l, NULL);
    ll_print(list_head, list_tail);
    printf("delete: %d\n", ll_delete(list_head, list_tail, &j));
    ll_print(list_head, list_tail);
    printf("delete: %d\n", ll_delete(list_head, list_tail, &j));
    ll_print(list_head, list_tail);
    printf("delete: %d\n", ll_delete(list_head, list_tail, &l));
    ll_print(list_head, list_tail);
}
