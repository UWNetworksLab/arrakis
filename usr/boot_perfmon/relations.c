/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich and Mircosoft Corporation.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "internal.h"

struct list {
    coreid_t id;
    struct boot_perfmon_binding *b;
    struct list *next;
};

struct relations {
    struct list *head;
};

errval_t relations_new(struct relations **retrel)
{
    // Allocate
    struct relations *r = malloc(sizeof(struct relations));
    if (!r) {
        return LIB_ERR_MALLOC_FAIL;
    }

    // Initialize
    r->head = NULL;

    *retrel = r;
    return SYS_ERR_OK;

}

errval_t relations_add(struct relations *r, coreid_t id,
                       struct boot_perfmon_binding *b)
{
    // Allocate
    struct list *list = malloc(sizeof(struct list));
    if (!list) {
        return LIB_ERR_MALLOC_FAIL;
    }

    // Initialize
    list->id = id;
    list->b = b;

    // Add to the head of list
    list->next = r->head;
    r->head = list;

    return SYS_ERR_OK;
}

errval_t relations_get(struct relations *r, coreid_t id,
                       struct boot_perfmon_binding **b)
{
    struct list *walk = r->head;
    while (walk) {
        if (walk->id == id) {
            *b = walk->b;
            return SYS_ERR_OK;
        }
        walk = walk->next;
    }

    USER_PANIC("relations_get: item not found");
}

errval_t relations_iterate(struct relations *r, void *st,
                           relations_iterator_fn func)
{
    errval_t err;

    struct list *walk = r->head;
    while (walk) {
        err = func(st, walk->id, walk->b);
        if (err_is_fail(err)) {
            return err;
        }
        walk = walk->next;
    }

    return SYS_ERR_OK;
}

int relations_count(struct relations *r)
{
    int count = 0;

    struct list *walk = r->head;
    while (walk) {
        count++;
        walk = walk->next;
    }

    return count;
}

errval_t relations_remove(struct relations *r, coreid_t id)
{
    USER_PANIC("NYI");
}
