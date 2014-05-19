/**
 * \file
 * \brief Slot allocator for a single cnode
 *
 * Allocators should be created with worst case slab requirement.
 * The worst case requirement is the number of slots in the cnode / 2.
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>

static errval_t salloc(struct slot_allocator *ca, struct capref *ret)
{
    struct single_slot_allocator *sca = (struct single_slot_allocator*)ca;

    if (sca->a.space == 0) {
        return LIB_ERR_SLOT_ALLOC_NO_SPACE;
    }

    thread_mutex_lock(&ca->mutex);

    // Slot to return
    ret->cnode = sca->cnode;
    ret->slot  = sca->head->slot;

    // Decrement space
    sca->head->space--;
    sca->head->slot++;
    sca->a.space--;

    // Check to free head
    if (sca->head->space == 0) {
        struct cnode_meta *walk = sca->head;
        sca->head = walk->next;
        slab_free(&sca->slab, walk);
    }

    thread_mutex_unlock(&ca->mutex);
    return SYS_ERR_OK;
}

static errval_t sfree(struct slot_allocator *ca, struct capref cap)
{
    struct single_slot_allocator *sca = (struct single_slot_allocator*)ca;
    errval_t err = SYS_ERR_OK;

    if (!cnodecmp(cap.cnode, sca->cnode)) {
        return LIB_ERR_SLOT_ALLOC_WRONG_CNODE;
    }

    thread_mutex_lock(&ca->mutex);

    struct cnode_meta *walk = sca->head;
    struct cnode_meta *prev = NULL;

    // Entire cnode was allocated
    if (!sca->head) {
        sca->head = slab_alloc(&sca->slab);
        sca->head->slot = cap.slot;
        sca->head->space = 1;
        sca->head->next = NULL;
        goto finish;
    }

    // Freeing one before head
    if (cap.slot + 1 == sca->head->slot) {
        sca->head->slot = cap.slot;
        sca->head->space++;
        goto finish;
    }

    // Freeing before head
    if (cap.slot < sca->head->slot) {
        struct cnode_meta *new = slab_alloc(&sca->slab);
        new->slot  = cap.slot;
        new->space = 1;
        new->next  = sca->head;
        sca->head  = new;
        goto finish;
    }

    while (walk != NULL) {
        // Freeing at the edge of walk
        if (cap.slot == walk->slot + walk->space) {
            walk->space++;

            // check if we can merge walk to next
            struct cnode_meta *next = walk->next;
            if (next && next->slot == walk->slot + walk->space) {
                walk->space += next->space;
                walk->next = next->next;
                slab_free(&sca->slab, next);
            }

            goto finish;
        }
        else if (cap.slot < walk->slot + walk->space) {
            err = LIB_ERR_SLOT_UNALLOCATED;
            goto unlock;
        }

        // Freing just before walk->next
        if (walk->next && cap.slot + 1 == walk->next->slot) {
            walk->next->slot = cap.slot;
            walk->next->space++;
            goto finish;
        }

        // Freeing after walk and before walk->next
        if (walk->next && cap.slot < walk->next->slot) {
            struct cnode_meta *new = walk->next;
            walk->next = slab_alloc(&sca->slab);
            walk->next->slot = cap.slot;
            walk->next->space = 1;
            walk->next->next = new;
            goto finish;
        }
        prev = walk;
        walk = walk->next;
    }

    // Freeing after the list
    prev->next = slab_alloc(&sca->slab);
    prev->next->slot = cap.slot;
    prev->next->space = 1;
    prev->next->next = NULL;

 finish:
    sca->a.space++;

 unlock:
    thread_mutex_unlock(&ca->mutex);
    return err;
}

errval_t single_slot_alloc_init_raw(struct single_slot_allocator *ret,
                                    struct capref cap, struct cnoderef cnode,
                                    cslot_t nslots, void *buf, size_t buflen)
{
    /* Generic part */
    ret->a.alloc = salloc;
    ret->a.free  = sfree;
    ret->a.space = nslots;
    ret->a.nslots = nslots;
    thread_mutex_init(&ret->a.mutex);

    /* Specific part */
    ret->cap   = cap;
    ret->cnode = cnode;

    slab_init(&ret->slab, sizeof(struct cnode_meta), NULL);
    if (buflen > 0) {
        // check for callers that do not provide enough buffer space
        #if !defined(NDEBUG)
        //on arm, __builtin_return_address does not take arguments !=0
        #if !defined(__arm__)
        size_t buflen_proper = SINGLE_SLOT_ALLOC_BUFLEN(nslots);
        if (buflen != buflen_proper) {
            debug_printf("******* FIXME: %s buflen=%zu != buflen_proper=%zu"
                         "call stack: %p %p\n",
                         __FUNCTION__, buflen, buflen_proper,
                         __builtin_return_address(0),
                         __builtin_return_address(1));
        }
        #endif
        #endif
        slab_grow(&ret->slab, buf, buflen);
    }

    ret->head = slab_alloc(&ret->slab);
    assert(ret->head != NULL);
    ret->head->slot = 0;
    ret->head->space = nslots;
    ret->head->next = NULL;

    return SYS_ERR_OK;
}

errval_t single_slot_alloc_init(struct single_slot_allocator *ret,
                                 cslot_t nslots, cslot_t *retslots)
{
    errval_t err;

    struct capref cap;
    struct cnoderef cnode;
    err = cnode_create(&cap, &cnode, nslots, &nslots);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE);
    }
    size_t buflen = SINGLE_SLOT_ALLOC_BUFLEN(nslots);
    void *buf = malloc(buflen);
    if (!buf) {
        return LIB_ERR_MALLOC_FAIL;
    }

    err = single_slot_alloc_init_raw(ret, cap, cnode, nslots, buf, buflen);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
    }

    if (retslots) {
        *retslots = nslots;
    }
    return SYS_ERR_OK;
}
