/**
 * \file
 * \brief Slot allocator capable of allocating more than one slot at a time
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

/**
 * \brief Allocate slots out of a cnode
 *
 * \param alloc         Pointer to the metadata
 * \param nslots        Number of slots to allocate
 * \param ret           Pointer used to return the allocated slot
 */
errval_t range_slot_alloc(struct range_slot_allocator *alloc, cslot_t nslots,
                          struct capref *ret)
{
    thread_mutex_lock(&alloc->mutex);

    struct cnode_meta *prev = NULL;
    struct cnode_meta *walk = alloc->meta;

    /* Look for large enough space */
    while(walk != NULL) {
        if (walk->space >= nslots) {
            break;
        }
        prev = walk;
        walk = walk->next;
    }

    /* Space not found */
    if (walk == NULL) {
        thread_mutex_unlock(&alloc->mutex);
        return LIB_ERR_SLOT_ALLOC_NO_SPACE;
    }

    /* Set the cap slot to return */
    ret->cnode = alloc->cnode;
    ret->slot  = walk->slot;

    /* Increament used slots */
    walk->slot  += nslots;
    walk->space -= nslots;

    /* If no more space, free metadata */
    if (walk->space == 0) {
        if (prev != NULL) {
            prev->next = walk->next;
        } else {
            alloc->meta = walk->next;
        }
        slab_free(&alloc->slab, walk);
    }

    thread_mutex_unlock(&alloc->mutex);
    return SYS_ERR_OK;
}

/**
 * \brief Insert the slots after walk
 *
 * \param alloc           Meta data for the allocator
 * \param nslots          Number of slots to insert
 * \param slot            Slot number in the cnode to start inserting from
 * \param walk            Meta data of the #cnode_meta to insert after
 */
static errval_t insert_after(struct range_slot_allocator *alloc, size_t nslots,
                             cslot_t slot, struct cnode_meta *walk)
{
    assert(walk->next == NULL);

    if (slot == (walk->slot + walk->space)) { /* Join */
        walk->space += nslots;
        return SYS_ERR_OK;
    }

    /* Create new meta data */
    walk->next = slab_alloc(&alloc->slab);
    if (walk->next == NULL) {
        return LIB_ERR_SLAB_ALLOC_FAIL;
    }
    walk->next->slot  = slot;
    walk->next->space = nslots;
    walk->next->next  = NULL;
    return SYS_ERR_OK;
}

/**
 * \brief Insert the slots before walk
 *
 * \param alloc           Meta data for the allocator
 * \param nslots          Number of slots to insert
 * \param slot            Slot number in the cnode to start inserting from
 * \param prev            Meta data of the #cnode_meta to insert after
 * \param walk            Meta data of the #cnode_meta to insert before
 */
static errval_t insert_before(struct range_slot_allocator *alloc, size_t nslots,
                              cslot_t slot, struct cnode_meta *prev,
                              struct cnode_meta *walk)
{
    assert(walk != NULL);

    if ((prev != NULL) &&
        (slot == (prev->slot + prev->space)) &&
        ((slot + nslots) == walk->slot)) { /* Join with prev and walk */
        prev->space = prev->space + nslots + walk->space;
        prev->next = walk->next;
        slab_free(&alloc->slab, walk);
        return SYS_ERR_OK;
    }

    if ((slot + nslots) == walk->slot) { /* Join with walk */
        walk->slot = slot;
        walk->space += nslots;
        return SYS_ERR_OK;
    }

    if ((prev != NULL) &&
        (slot == (prev->slot + prev->space))) { /* Join with prev */
        prev->space += nslots;
        return SYS_ERR_OK;
    }

    /* Create new meta data */
    struct cnode_meta *new = slab_alloc(&alloc->slab);
    if (new == NULL) {
        return LIB_ERR_SLAB_ALLOC_FAIL;
    }
    new->slot  = slot;
    new->space = nslots;
    if (prev == NULL) {
        assert(alloc->meta == walk);
        alloc->meta = new;
    } else {
        prev->next = new;
    }
    new->next = walk;

    return SYS_ERR_OK;
}

/**
 * \brief Free slots belonging to the cnode
 *
 * \param alloc         Pointer to the metadata
 * \param cap           Slot to start freeing from
 * \param nslots        Number of slots to free
 */
errval_t range_slot_free(struct range_slot_allocator *alloc, struct capref cap,
                         cslot_t nslots)
{
    errval_t err;
    thread_mutex_lock(&alloc->mutex);

    struct cnode_meta *prev = NULL;
    struct cnode_meta *walk = alloc->meta;

    while(walk != NULL) {
        if ((cap.slot > walk->slot) && (walk->next == NULL)) {
            err = insert_after(alloc, nslots, cap.slot, walk);
            thread_mutex_unlock(&alloc->mutex);
            return err;
        }
        if (cap.slot < walk->slot) {
            err = insert_before(alloc, nslots, cap.slot, prev, walk);
            thread_mutex_unlock(&alloc->mutex);
            return err;
        }
        prev = walk;
        walk = walk->next;
    }

    assert(alloc->meta == NULL);
    alloc->meta = slab_alloc(&alloc->slab);
    if (alloc->meta == NULL) {
        thread_mutex_unlock(&alloc->mutex);
        return LIB_ERR_SLAB_ALLOC_FAIL;
    }
    alloc->meta->slot = cap.slot;
    alloc->meta->space = nslots;
    alloc->meta->next = NULL;
    thread_mutex_unlock(&alloc->mutex);
    return SYS_ERR_OK;
}

/**
 * \brief Constructor for a new instance of a single cnode cspace_allocator
 *
 * \param ret       Instance of the allocator created
 * \param nslots    Desired number of slots the cnode should have
 * \param ret_slots Number of slots actually used
 */
errval_t range_slot_alloc_init(struct range_slot_allocator *ret,
                               cslot_t nslots, cslot_t *retslots)
{
    errval_t err;

    /* Cap for the cnode */
    err = cnode_create(&ret->cnode_cap, &ret->cnode, nslots, &nslots);
    if (err_is_fail(err)) {
        return err;
    }

    if (retslots) {
        *retslots = nslots;
    }

    /* Memory for the slab allocator */
    void *buf = malloc(sizeof(struct cnode_meta) * nslots / 2);
    if (!buf) {
        return LIB_ERR_MALLOC_FAIL;
    }

    slab_init(&ret->slab, sizeof(struct cnode_meta), NULL);
    slab_grow(&ret->slab, buf, sizeof(struct cnode_meta) * nslots / 2);
    thread_mutex_init(&ret->mutex);

    /* Set the fields in the allocator instance */
    ret->meta = slab_alloc(&ret->slab);
    if (ret->meta == NULL) {
        return LIB_ERR_SLAB_ALLOC_FAIL;
    }
    ret->meta->slot = 0;
    ret->meta->space = nslots;
    ret->meta->next = NULL;

    return SYS_ERR_OK;
}
