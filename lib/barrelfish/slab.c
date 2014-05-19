/**
 * \file
 * \brief Simple slab allocator.
 *
 * This file implements a simple slab allocator. It allocates blocks of a fixed
 * size from a pool of contiguous memory regions ("slabs").
 */

/*
 * Copyright (c) 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/slab.h>
#include <barrelfish/static_assert.h>

struct block_head {
    struct block_head *next;///< Pointer to next block in free list
};

STATIC_ASSERT_SIZEOF(struct block_head, SLAB_BLOCK_HDRSIZE);

/**
 * \brief Initialise a new slab allocator
 *
 * \param slabs Pointer to slab allocator instance, to be filled-in
 * \param blocksize Size of blocks to be allocated by this allocator
 * \param refill_func Pointer to function to call when out of memory (or NULL)
 */
void slab_init(struct slab_alloc *slabs, size_t blocksize,
               slab_refill_func_t refill_func)
{
    slabs->slabs = NULL;
    slabs->blocksize = SLAB_REAL_BLOCKSIZE(blocksize);
    slabs->refill_func = refill_func;
}


/**
 * \brief Add memory (a new slab) to a slab allocator
 *
 * \param slabs Pointer to slab allocator instance
 * \param buf Pointer to start of memory region
 * \param buflen Size of memory region (in bytes)
 */
void slab_grow(struct slab_alloc *slabs, void *buf, size_t buflen)
{
    /* setup slab_head structure at top of buffer */
    assert(buflen > sizeof(struct slab_head));
    struct slab_head *head = buf;
    buflen -= sizeof(struct slab_head);
    buf = (char *)buf + sizeof(struct slab_head);

    /* calculate number of blocks in buffer */
    size_t blocksize = slabs->blocksize;
    assert(buflen / blocksize <= UINT32_MAX);
    head->free = head->total = buflen / blocksize;
    assert(head->total > 0);

    /* enqueue blocks in freelist */
    struct block_head *bh = head->blocks = buf;
    for (uint32_t i = head->total; i > 1; i--) {
        buf = (char *)buf + blocksize;
        bh->next = buf;
        bh = buf;
    }
    bh->next = NULL;

    /* enqueue slab in list of slabs */
    head->next = slabs->slabs;
    slabs->slabs = head;
}

/**
 * \brief Allocate a new block from the slab allocator
 *
 * \param slabs Pointer to slab allocator instance
 *
 * \returns Pointer to block on success, NULL on error (out of memory)
 */
void *slab_alloc(struct slab_alloc *slabs)
{
    errval_t err;
    /* find a slab with free blocks */
    struct slab_head *sh;
    for (sh = slabs->slabs; sh != NULL && sh->free == 0; sh = sh->next);

    if (sh == NULL) {
        /* out of memory. try refill function if we have one */
        if (!slabs->refill_func) {
            return NULL;
        } else {
            err = slabs->refill_func(slabs);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "slab refill_func failed");
                return NULL;
            }
            for (sh = slabs->slabs; sh != NULL && sh->free == 0; sh = sh->next);
            if (sh == NULL) {
                return NULL;
            }
        }
    }

    /* dequeue top block from freelist */
    struct block_head *bh = sh->blocks;
    assert(bh != NULL);
    sh->blocks = bh->next;
    sh->free--;

    return bh;
}

/**
 * \brief Free a block to the slab allocator
 *
 * \param slabs Pointer to slab allocator instance
 * \param block Pointer to block previously returned by #slab_alloc
 */
void slab_free(struct slab_alloc *slabs, void *block)
{
    if (block == NULL) {
        return;
    }

    struct block_head *bh = (struct block_head *)block;

    /* find matching slab */
    struct slab_head *sh;
    size_t blocksize = slabs->blocksize;
    for (sh = slabs->slabs; sh != NULL; sh = sh->next) {
        /* check if block falls inside this slab */
        uintptr_t slab_limit = (uintptr_t)sh + sizeof(struct slab_head)
                               + blocksize * sh->total;
        if ((uintptr_t)bh > (uintptr_t)sh && (uintptr_t)bh < slab_limit) {
            break;
        }
    }
    assert(sh != NULL);

    /* re-enqueue in slab's free list */
    bh->next = sh->blocks;
    sh->blocks = bh;
    sh->free++;
    assert(sh->free <= sh->total);
}

/**
 * \brief Returns the count of free blocks in the allocator
 *
 * \param slabs Pointer to slab allocator instance
 *
 * \returns Free block count
 */
size_t slab_freecount(struct slab_alloc *slabs)
{
    size_t ret = 0;

    for (struct slab_head *sh = slabs->slabs; sh != NULL; sh = sh->next) {
        ret += sh->free;
    }

    return ret;
}

/**
 * \brief General-purpose slab refill
 *
 * Allocates and maps a number of memory pages to the slab allocator.
 *
 * \param slabs Pointer to slab allocator instance
 * \param bytes (Minimum) amount of memory to map
 */
static errval_t slab_refill_pages(struct slab_alloc *slabs, size_t bytes)
{
    errval_t err;
    struct capref frame_cap;

    err = frame_alloc(&frame_cap, bytes, &bytes);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_CREATE);
    }

    void *buf;
    err = vspace_map_one_frame(&buf, bytes, frame_cap, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    slab_grow(slabs, buf, bytes);
    return SYS_ERR_OK;
}

/**
 * \brief General-purpose implementation of a slab allocate/refill function
 *
 * Allocates and maps a single page (FIXME: make configurable) and adds it
 * to the allocator.
 *
 * \param slabs Pointer to slab allocator instance
 */
errval_t slab_default_refill(struct slab_alloc *slabs)
{
    return slab_refill_pages(slabs, BASE_PAGE_SIZE);
}
