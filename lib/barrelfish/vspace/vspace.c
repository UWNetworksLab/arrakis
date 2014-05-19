/**
 * \file
 * \brief vspace management
 *
 * A vspace consists of a set of vregions and one pmap.
 * The current vspace is setup by the (domain/dispatcher?) spawning it.
 *
 * Warning: slot_alloc_init calls vregion_map which calls vspace_add_vregion.
 * vspace_add_vregion uses malloc to increase it's slab.
 * Since malloc depends upon slot_alloc_init being called successfully,
 * vspace_add_vregion should have enough initial slab space to not use malloc.
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/core_state.h>
#include "vspace_internal.h"
#include <stdio.h>

/**
 * \brief Initialize the current vspace structure
 *
 * This code is coupled with #pmap_current_init()
 */
errval_t vspace_current_init(bool init_domain)
{
    errval_t err;
    struct vspace *vspace = get_current_vspace();
    struct pmap *pmap = get_current_pmap();

    vspace->pmap = pmap;
    vspace->head = NULL;

    // Setup the layout
    err = vspace_layout_init(&vspace->layout);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_LAYOUT_INIT);
    }

    // Initialize current pmap
    struct capref cap = {
        .cnode = cnode_page,
        .slot  = 0,
    };
    err = pmap_init(pmap, vspace, cap, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_INIT);
    }
    err = pmap_current_init(init_domain);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_CURRENT_INIT);
    }

    // Initialize pinned memory
    err = vspace_pinned_init();
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_PINNED_INIT);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Add a new region into the vspace
 *
 * \param point  The vspace struct
 * \param region The region to add
 *
 * pmap implementation rely on vspace maintaining an ordered list of vregions
 */
errval_t vspace_add_vregion(struct vspace *vspace, struct vregion *region)
{
    // Sanity-check region (TODO: return error?)
    assert(region->size > 0);
    assert(region->base + region->size > region->base);

    if (vspace->head == NULL) {
        vspace->head = region;
        region->next = NULL;
        return SYS_ERR_OK;
    }

    // Insert in order
    struct vregion *walk = vspace->head;
    struct vregion *prev = NULL;
    while (walk != NULL) {
        if (region->base <= walk->base) {
            /* check for overlaps! */
            if (region->base + region->size > walk->base
                || (prev != NULL && prev->base + prev->size > region->base)) {
                return LIB_ERR_VSPACE_REGION_OVERLAP;
            }

            /* add here */
            if (prev == NULL) {
                region->next = vspace->head;
                vspace->head = region;
            } else {
                prev->next = region;
                region->next = walk;
            }
            return SYS_ERR_OK;
        }

        prev = walk;
        walk = walk->next;
    }

    /* add to end of list, checking for overlap with last item */
    assert(prev != NULL);
    if (prev->base + prev->size > region->base) {
        return LIB_ERR_VSPACE_REGION_OVERLAP;
    }
    prev->next = region;
    region->next = NULL;
    return SYS_ERR_OK;
}

/**
 * \brief remove a region from the vspace
 *
 * \param point  The vspace struct
 * \param region The region to remove
 *
 * Library internal function
 */
errval_t vspace_remove_vregion(struct vspace *vspace, struct vregion* region)
{
    assert(vspace != NULL);
    struct vregion *walk = vspace->head;
    struct vregion *prev = NULL;

    while (walk) {
        if (walk == region) {
            if (prev) {
                assert(prev->next == walk);
                prev->next = walk->next;
            } else {
                assert(walk == vspace->head);
                vspace->head = walk->next;
            }
            return SYS_ERR_OK;
        }
        prev = walk;
        walk = walk->next;
    }

    return LIB_ERR_VREGION_NOT_FOUND;
}

/**
 * \brief Initialize a vspace
 *
 * \param vspace The vspace to initialize
 * \param pmap   The pmap to associate with the vspace
 *
 * Initializes a vspace, associating it with a pmap
 */
errval_t vspace_init(struct vspace *vspace, struct pmap *pmap)
{
    errval_t err;

    vspace->pmap = pmap;
    vspace->head = NULL;

    // Setup the layout
    err = vspace_layout_init(&vspace->layout);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_LAYOUT_INIT);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Destroy a vspace
 */
errval_t vspace_destroy(struct vspace *vspace)
{
    USER_PANIC("NYI");
}

/**
 * \brief Get the region corresponding to the given virtual address
 *
 * \param addr  The virtual address
 */
struct vregion* vspace_get_region(struct vspace *vspace, const void *addr)
{
    lvaddr_t lvaddr = (lvaddr_t)addr;
    genvaddr_t genvaddr = vspace_lvaddr_to_genvaddr(lvaddr);

    struct vregion *walk = vspace->head;
    while (walk) {
        if (walk->base <= genvaddr &&
            walk->base + walk->size > genvaddr) {
            return walk;
        }
        walk = walk->next;
    }

    return NULL;
}

/**
 * \brief Page fault handler
 *
 * \param point The vspace page fault occured in
 * \param addr  The faulting address
 * \param type  The fault type
 *
 * Lookup the appropriate vregion and forward the fault to it
 */
errval_t vspace_pagefault_handler(struct vspace *vspace, lvaddr_t lvaddr,
                                  vm_fault_type_t type)
{
    errval_t err;

    genvaddr_t genvaddr =
        vspace_layout_lvaddr_to_genvaddr(&vspace->layout, lvaddr);

    struct vregion *walk = vspace->head;
    while(walk != NULL) {
        genvaddr_t base = vregion_get_base_addr(walk);
        genvaddr_t size = vregion_get_size(walk);
        if (genvaddr >= base && genvaddr < base + size) {
            err = vregion_pagefault_handler(walk, genvaddr, type);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_VREGION_PAGEFAULT_HANDLER);
            }
            return SYS_ERR_OK;
        }
        walk = walk->next;
    }

    return LIB_ERR_VSPACE_PAGEFAULT_ADDR_NOT_FOUND;
}
