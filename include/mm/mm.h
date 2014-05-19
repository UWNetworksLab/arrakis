/**
 * \file
 * \brief Memory manager header
 */

/*
 * Copyright (c) 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_MM_H
#define BARRELFISH_MM_H

#include <sys/cdefs.h>

#include <barrelfish/slab.h>
#include <mm/slot_alloc.h>

__BEGIN_DECLS

enum nodetype {
    NodeType_Dummy,     ///< The whole region doesn't exist, but it has children
    NodeType_Chunked,   ///< This region exists and has been split up
    NodeType_Free,      ///< This leaf region exists and is free
    NodeType_Allocated  ///< This leaf region exists and is allocated
};

/// Node in Memory manager Btree. Private.
// Only appears here so we can know its size
struct mmnode {
    enum nodetype type;     ///< Type of this node
    uint8_t childbits;      ///< Number of children (in bits / power of two)
    struct capref cap;    ///< Cap to this region (invalid for Dummy regions)
    struct mmnode *children[0];///< Child node pointers
};

/// Macro to statically determine size of a node, given the maxchildbits
#define MM_NODE_SIZE(maxchildbits) \
    (sizeof(struct mmnode) + sizeof(struct mmnode *) * (1UL << (maxchildbits)))

/**
 * \brief Memory manager instance data
 *
 * This should be opaque from the perspective of the client, but to allow
 * them to allocate its memory, we declare it in the public header.
 */
struct mm {
    struct slab_alloc slabs;///< Slab allocator used for allocating nodes
    slot_alloc_t slot_alloc;///< Slot allocator for allocating cspace
    void *slot_alloc_inst;  ///< Opaque instance pointer for slot allocator
    struct mmnode *root;    ///< Root node
    genpaddr_t base;        ///< Base address of root node
    enum objtype objtype;   ///< Type of capabilities stored
    uint8_t sizebits;       ///< Size of root node (in bits)
    uint8_t maxchildbits;   ///< Maximum number of children of every node (in bits)
    bool delete_chunked;    ///< Delete chunked capabilities if true
};

void mm_debug_print(struct mmnode *mmnode, int space);
errval_t mm_init(struct mm *mm, enum objtype objtype, genpaddr_t base,
                 uint8_t sizebits, uint8_t maxchildbits,
                 slab_refill_func_t slab_refill_func,
                 slot_alloc_t slot_alloc_func, void *slot_alloc_inst,
                 bool delete_chunked);
void mm_destroy(struct mm *mm);
errval_t mm_add(struct mm *mm, struct capref cap, uint8_t sizebits,
                genpaddr_t base);
errval_t mm_alloc(struct mm *mm, uint8_t sizebits, struct capref *retcap,
                  genpaddr_t *retbase);
errval_t mm_alloc_range(struct mm *mm, uint8_t sizebits, genpaddr_t minbase,
                        genpaddr_t maxlimit, struct capref *retcap,
                        genpaddr_t *retbase);
errval_t mm_realloc_range(struct mm *mm, uint8_t sizebits, genpaddr_t base,
                          struct capref *retcap);
errval_t mm_free(struct mm *mm, struct capref cap, genpaddr_t base,
                 uint8_t sizebits);

/// Structure to record all information about a given memory region
struct mem_cap {
    struct capref cap;        ///< Cap for this region
    uint8_t sizebits;           ///< Size of region in bits
    genpaddr_t base;               ///< Physical base address of region
};

size_t mm_relinquish_all(struct mm *mm, struct mem_cap *ret, size_t retlen);
size_t mm_relinquish_range(struct mm *mm, genpaddr_t base, genpaddr_t limit,
                           struct mem_cap *ret, size_t retlen);

__END_DECLS

#endif /* BARRELFISH_MM_H */
