/**
 * \file
 * \brief Slot management for the memory allocator.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <mm/mm.h>
#include <mm/slot_alloc.h>
#include <stdio.h>

/// Allocate a new cnode if needed
errval_t slot_prealloc_refill(struct slot_prealloc *this)
{
    uint8_t refill = !this->current;
    errval_t err;

    if (this->meta[refill].free == (1UL << this->cnode_size_bits)) {
        return SYS_ERR_OK; // Nop
    }

    /* Check for initialize case */
    // When initializing, the cnode on the other point should be NULL
    if (this->top_used == -1UL) {
        // Allocate a ram cap
        struct capref ram_cap;
        err = mm_alloc(this->mm, this->cnode_size_bits + OBJBITS_CTE, &ram_cap,
                       NULL);
        if (err_is_fail(err)) {
            return err_push(err, MM_ERR_SLOT_MM_ALLOC);
        }

        // Retype to and build the top level cnode
        err = cnode_create_from_mem(this->top_cnode_slot, ram_cap,
                                    &this->top_cnode, this->cnode_size_bits);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CNODE_CREATE);
        }

        this->top_used = 0;
    }

    // Still enough slots in the top cnode
    assert(this->top_used < (1UL << this->cnode_size_bits));

    // Allocate a ram cap
    struct capref ram_cap;
    err = mm_alloc(this->mm, this->cnode_size_bits + OBJBITS_CTE, &ram_cap, NULL);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_SLOT_MM_ALLOC);
    }

    // Retype to and build the next cnode
    struct capref cnode_cap = {
        .cnode = this->top_cnode,
        .slot  = this->top_used++
    };
    err = cnode_create_from_mem(cnode_cap, ram_cap, &this->meta[refill].cap.cnode,
                                this->cnode_size_bits);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE);
    }

    // Set the metadata
    this->meta[refill].cap.slot  = 0;
    this->meta[refill].free      = (1UL << this->cnode_size_bits);

    return SYS_ERR_OK;
}

errval_t slot_alloc_prealloc(void *inst, uint64_t nslots, struct capref *ret)
{
    struct slot_prealloc *this = inst;
    assert(nslots <= (1UL << this->maxslotbits));

    /* Check if enough space */
    if (this->meta[this->current].free < nslots) {
        // Allocate from next cnode
        this->current = !this->current;
    }

    if (this->meta[this->current].free < nslots) {
        return MM_ERR_SLOT_NOSLOTS;
    }

    /* Return next slot and update */
    *ret = this->meta[this->current].cap;
    this->meta[this->current].cap.slot += nslots;
    this->meta[this->current].free -= nslots;

    return SYS_ERR_OK;
}

/**
 * \brief Initialise preallocating slot allocator instance
 *
 * \param this Pointer to area for instance data
 * \param top Location to place top-level CNode
 * \param maxslotbits Maximum size of each allocation (in bits)
 * \param cnode_size_bits Size of CNodes (in bits) to create (>= maxslotbits)
 * \param initial_cnode First cap in an empty cnode to start allocating from
 * \param initial_space Number of slots free in initial cnode
 * \param ram_mm Memory allocator to use for RAM caps when creating new CNodes
 */
errval_t slot_prealloc_init(struct slot_prealloc *this, struct capref top,
                            uint8_t maxslotbits, uint8_t cnode_size_bits,
                            struct capref initial_cnode, uint64_t initial_space,
                            struct mm *ram_mm)
{
    assert(cnode_size_bits >= maxslotbits);
    this->maxslotbits = maxslotbits;
    this->cnode_size_bits = cnode_size_bits;
    this->mm = ram_mm;

    this->top_cnode_slot = top;
    this->top_used = -1UL; /* flag */

    this->current = 0;
    this->meta[0].cap       = initial_cnode;
    this->meta[0].free      = initial_space;
    this->meta[1].free      = 0;

    return SYS_ERR_OK;
}

errval_t slot_alloc_basecn_init(struct slot_alloc_basecn *this)
{
    struct capref cnode_cap;
    errval_t err;

    err = slot_alloc(&cnode_cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    struct capref ram;
    err = ram_alloc_fixed(&ram, BASE_PAGE_BITS, 0, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    err = cnode_create_from_mem(cnode_cap, ram, &this->top_cnode_slot.cnode,
                                DEFAULT_CNODE_BITS);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE);
    }

    this->top_cnode_slot.slot = 0;
    this->free = 0;

    return SYS_ERR_OK;
}

errval_t slot_alloc_basecn(void *inst, uint64_t nslots, struct capref *ret)
{
    struct slot_alloc_basecn *this = inst;
    errval_t err;

    if (nslots > this->free) {
        assert(this->top_cnode_slot.slot < (1UL << DEFAULT_CNODE_BITS));

        /* XXX: Special case for init, need to get memory from basecn */
        struct capref ram;
        err = ram_alloc_fixed(&ram, BASE_PAGE_BITS, 0, 0);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_RAM_ALLOC);
        }

        err = cnode_create_from_mem(this->top_cnode_slot, ram,
                                  &this->cap.cnode,
                                  DEFAULT_CNODE_BITS);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CNODE_CREATE);
        }

        this->top_cnode_slot.slot++;
        this->cap.slot = 0;
        this->free = 1UL << DEFAULT_CNODE_BITS;
    }

    assert(nslots <= this->free);
    *ret = this->cap;
    this->cap.slot += nslots;
    this->free -= nslots;

    return SYS_ERR_OK;
}

/// Requires an instance of range_slot_allocator
errval_t slot_alloc_dynamic(void *alloc, uint64_t nslots, struct capref *ret)
{
    return range_slot_alloc(alloc, nslots, ret);
}
