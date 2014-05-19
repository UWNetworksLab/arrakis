/**
 * \file
 * \brief Simple slab allocator
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_SLAB_H
#define LIBBARRELFISH_SLAB_H

#include <sys/cdefs.h>

__BEGIN_DECLS

// forward declarations
struct slab_alloc;
struct block_head;

typedef errval_t (*slab_refill_func_t)(struct slab_alloc *slabs);

struct slab_head {
    struct slab_head *next; ///< Next slab in the allocator
    uint32_t total, free;   ///< Count of total and free blocks in this slab
    struct block_head *blocks; ///< Pointer to free block list
};

struct slot_allocator;

struct slab_alloc {
    struct slab_head *slabs;    ///< Pointer to list of slabs
    size_t blocksize;           ///< Size of blocks managed by this allocator
    slab_refill_func_t refill_func;  ///< Refill function
};

void slab_init(struct slab_alloc *slabs, size_t blocksize,
               slab_refill_func_t refill_func);
void slab_grow(struct slab_alloc *slabs, void *buf, size_t buflen);
void *slab_alloc(struct slab_alloc *slabs);
void slab_free(struct slab_alloc *slabs, void *block);
size_t slab_freecount(struct slab_alloc *slabs);
errval_t slab_default_refill(struct slab_alloc *slabs);

// size of block header
#define SLAB_BLOCK_HDRSIZE (sizeof(void *))
// should be able to fit the header into the block
#define SLAB_REAL_BLOCKSIZE(blocksize) \
    (((blocksize) > SLAB_BLOCK_HDRSIZE) ? (blocksize) : SLAB_BLOCK_HDRSIZE)

/// Macro to compute the static buffer size required for a given allocation
#define SLAB_STATIC_SIZE(nblocks, blocksize) \
        ((nblocks) * SLAB_REAL_BLOCKSIZE(blocksize) + sizeof(struct slab_head))

__END_DECLS

#endif // LIBBARRELFISH_SLAB_H
