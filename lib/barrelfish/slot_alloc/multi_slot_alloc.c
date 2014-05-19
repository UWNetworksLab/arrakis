/**
 * \file
 * \brief Slot allocator for two layers of cnodes
 *
 * Warning: This code is coupled with vspace/pinned.c and vspace/pmap_*.
 *
 * The allocator maintains a list of single cnode allocators
 * and one single cnode allocator in reserve.
 * When the last slot in the list is allocated,
 * reserve is added to the list and a new reserve is created.
 *
 * Creating a new reserve requires additional slots and virtual address space.
 * 1 slot for the cnode, and more if the slab space needs to be grown.
 * If the slab has to be grown, then 1 slot for the frame.
 * Mapping in the frame can require additional slots.
 * Refer to code in vspace/pmap_* and vspace/util.c for more detail.
 *
 * Growing requires 2 slots from this file.
 *
 * \warning Due to lack of multithread support,
 * the thread safefy and locking is not properly test.
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
#include "internal.h"

/**
 * \brief slot allocator
 *
 * \param ca   Instance of the allocator
 * \param ret  Pointer to return the allocated slot
 */
errval_t multi_alloc(struct slot_allocator *ca, struct capref *ret)
{
    errval_t err = SYS_ERR_OK;
    struct multi_slot_allocator *mca = (struct multi_slot_allocator*)ca;

    thread_mutex_lock(&ca->mutex);
    assert(ca->space != 0);
    ca->space--;

    /* Try allocating from the list of single slot allocators */
    struct slot_allocator_list *walk = mca->head;
    //struct slot_allocator_list *prev = NULL;
    while(walk != NULL) {
        err = walk->a.a.alloc(&walk->a.a, ret);
        if (err_no(err) != LIB_ERR_SLOT_ALLOC_NO_SPACE) {
            break;
        }
        //prev = walk;
        walk = walk->next;
    }
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC);
    }

    /* If no more slots left, grow */
    if (ca->space == 0) {
        ca->space = ca->nslots;
        /* Pull in the reserve */
        mca->reserve->next = mca->head;
        mca->head = mca->reserve;

        /* Setup a new reserve */
        // Cnode
        struct capref cap;
        struct cnoderef cnode;
        err = mca->top->alloc(mca->top, &cap);
        if (err_is_fail(err)) {
            thread_mutex_unlock(&ca->mutex);
            return err_push(err, LIB_ERR_SLOT_ALLOC);
        }
        thread_mutex_unlock(&ca->mutex); // cnode_create_raw uses ram_alloc
                                         // which may call slot_alloc
        err = cnode_create_raw(cap, &cnode, ca->nslots, NULL);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CNODE_CREATE);
        }
        thread_mutex_lock(&ca->mutex);

        // Buffers
        void *buf = slab_alloc(&mca->slab);
        if (!buf) { /* Grow slab */
            // Allocate slot out of the list
            mca->a.space--;
            struct capref frame;
            err = mca->head->a.a.alloc(&mca->head->a.a, &frame);
            if (err_is_fail(err)) {
                thread_mutex_unlock(&ca->mutex);
                return err_push(err, LIB_ERR_SLOT_ALLOC);
            }

            thread_mutex_unlock(&ca->mutex); // following functions may call
                                             // slot_alloc
            void *slab_buf;
            size_t size;
            err = vspace_mmu_aware_map(&mca->mmu_state, frame,
                                       mca->slab.blocksize, &slab_buf, &size);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_VSPACE_MMU_AWARE_MAP);
            }

            thread_mutex_lock(&ca->mutex);

            // Grow slab
            slab_grow(&mca->slab, slab_buf, size);

            // Try allocating again
            buf = slab_alloc(&mca->slab);
            if (err_is_fail(err)) {
                thread_mutex_unlock(&ca->mutex);
                return err_push(err, LIB_ERR_SLAB_ALLOC_FAIL);
            }
        }

        mca->reserve = buf;
        buf = (char *)buf + sizeof(struct slot_allocator_list);
        size_t bufsize = mca->slab.blocksize - sizeof(struct slot_allocator_list);

        // Allocator
        err = single_slot_alloc_init_raw(&mca->reserve->a, cap, cnode,
                                         mca->a.nslots, buf, bufsize);
        if (err_is_fail(err)) {
            thread_mutex_unlock(&ca->mutex);
            return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
        }
    }

    thread_mutex_unlock(&ca->mutex);
    return SYS_ERR_OK;
}

/**
 * \brief Free an allocated slot
 *
 * \param ca  Instance of the allocator
 * \param cap The slot to free
 *
 * Walks the list of single slot allocators trying to free the slot.
 */
errval_t multi_free(struct slot_allocator *ca, struct capref cap)
{
    errval_t err;
    thread_mutex_lock(&ca->mutex);
    struct multi_slot_allocator *mca = (struct multi_slot_allocator*)ca;
    struct slot_allocator_list *walk = mca->head;

    while(walk != NULL) {
        err = walk->a.a.free(&walk->a.a, cap);
        if (err_is_ok(err)) {
            mca->a.space++;
        }
        if (err_no(err) != LIB_ERR_SLOT_ALLOC_WRONG_CNODE) {
            thread_mutex_unlock(&ca->mutex);
            return err;
        }
        walk = walk->next;
    }

    thread_mutex_unlock(&ca->mutex);
    return LIB_ERR_SLOT_ALLOC_WRONG_CNODE;
}

/**
 * \brief Initializer that does not allocate any space
 *
 * #slot_alloc_init duplicates some of the code below,
 * modify it if making changes here.
 *
 * XXX: top_buf head_buf and reserve_buf each point to a separate buffer of
 * size bufsize bytes which can be used for backing storage. bufsize evidently
 * needs to be >= sizeof(struct cnode_meta) * nslots / 2. Don't ask me why! -AB
 */
errval_t multi_slot_alloc_init_raw(struct multi_slot_allocator *ret,
                                   cslot_t nslots, struct capref top_cap,
                                   struct cnoderef top_cnode,
                                   void *top_buf, void *head_buf,
                                   void *reserve_buf, size_t bufsize)
{
    errval_t err;
    struct capref cap;
    struct cnoderef cnode;

    /* Generic part */
    ret->a.alloc = multi_alloc;
    ret->a.free  = multi_free;
    ret->a.space = nslots;
    ret->a.nslots = nslots;
    thread_mutex_init(&ret->a.mutex);

    ret->head->next = NULL;
    ret->reserve->next = NULL;

    /* Top */
    err = single_slot_alloc_init_raw((struct single_slot_allocator*)ret->top,
                                     top_cap, top_cnode, nslots, top_buf, bufsize);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT);
    }

    /* Head */
    err = ret->top->alloc(ret->top, &cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }
    err = cnode_create_raw(cap, &cnode, nslots, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE);
    }
    err = single_slot_alloc_init_raw(&ret->head->a, cap, cnode, nslots,
                                     head_buf, bufsize);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT);
    }

    /* Reserve */
    err = ret->top->alloc(ret->top, &cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }
    err = cnode_create_raw(cap, &cnode, nslots, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE);
    }
    err = single_slot_alloc_init_raw(&ret->reserve->a, cap, cnode, nslots,
                                     reserve_buf, bufsize);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT);
    }

    /* Slab */
    size_t allocation_unit = sizeof(struct slot_allocator_list) +
                             SINGLE_SLOT_ALLOC_BUFLEN(nslots);
    slab_init(&ret->slab, allocation_unit, NULL);

    return SYS_ERR_OK;
}

/**
 * \brief Initializer that uses memory
 */
errval_t multi_slot_alloc_init(struct multi_slot_allocator *ret,
                               cslot_t nslots, cslot_t *retslots)
{
    errval_t err;
    nslots = ROUND_UP(nslots, DEFAULT_CNODE_SLOTS);
    size_t bufsize = SINGLE_SLOT_ALLOC_BUFLEN(nslots); // XXX?

    ret->top = malloc(sizeof(struct single_slot_allocator));
    if (!ret->top) {
        return LIB_ERR_MALLOC_FAIL;
    }
    void *top_buf = malloc(bufsize);
    if (!top_buf) {
        return LIB_ERR_MALLOC_FAIL;
    }

    ret->head = malloc(sizeof(struct slot_allocator_list));
    if (!ret->head) {
        return LIB_ERR_MALLOC_FAIL;
    }
    ret->head->next = NULL;
    void *head_buf = malloc(bufsize);
    if (!head_buf) {
        return LIB_ERR_MALLOC_FAIL;
    }

    ret->reserve = malloc(sizeof(struct single_slot_allocator));
    if (!ret->reserve) {
        return LIB_ERR_MALLOC_FAIL;
    }
    void *reserve_buf = malloc(bufsize);
    if (!reserve_buf) {
        return LIB_ERR_MALLOC_FAIL;
    }

    size_t allocation_unit = sizeof(struct slot_allocator_list) + bufsize;
    err = vspace_mmu_aware_init(&ret->mmu_state, allocation_unit * nslots * 2);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MMU_AWARE_INIT);
    }

    struct capref cap;
    struct cnoderef cnode;
    err = cnode_create(&cap, &cnode, nslots, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE);
    }
    err = multi_slot_alloc_init_raw(ret, nslots, cap, cnode, top_buf,
                                    head_buf, reserve_buf, bufsize);

    if (retslots) {
        *retslots = nslots;
    }
    return SYS_ERR_OK;
}
