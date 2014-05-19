/**
 * \file
 * \brief Slot allocator wrapper
 *
 * Warning: slot_alloc_init calls vregion_map which calls vspace_add_vregion.
 * vspace_add_vregion uses malloc to increase it's slab.
 * Since malloc depends upon slot_alloc_init being called successfully,
 * vspace_add_vregion should have enough initial slab space to not use malloc.
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
#include <barrelfish/core_state.h>
#include <barrelfish/caddr.h>
#include "internal.h"


/**
 * \brief Returns the default slot allocator for the caller
 */
struct slot_allocator *get_default_slot_allocator(void)
{
    struct slot_alloc_state *state = get_slot_alloc_state();
    return (struct slot_allocator*)(&state->defca);
}

/**
 * \brief Default slot allocator
 *
 * \param ret Pointer to the cap to return the allocated slot in
 *
 * Allocates one slot from the default allocator
 */
errval_t slot_alloc(struct capref *ret)
{
    struct slot_allocator *ca = get_default_slot_allocator();
    return ca->alloc(ca, ret);
}

/**
 * \brief slot allocator for the root
 *
 * \param ret Pointer to the cap to return the allocated slot in
 *
 * Allocates one slot from the root slot allocator
 */
errval_t slot_alloc_root(struct capref *ret)
{
    struct slot_alloc_state *state = get_slot_alloc_state();
    struct slot_allocator *ca = (struct slot_allocator*)(&state->rootca);
    return ca->alloc(ca, ret);
}

/**
 * \brief Default slot free
 *
 * \param ret The cap to free
 *
 * Frees the passed in slot.
 *
 * \bug During dispatcher initialization and special domains like
 * init and mem_serv free slots which
 * are not allocated by the default allocator.
 * This function detects such cases and ignores the errors.
 * It maybe ignoring errors that must be caught.
 */
errval_t slot_free(struct capref ret)
{
    struct slot_alloc_state *state = get_slot_alloc_state();

    if (cnodecmp(ret.cnode, cnode_base)) { // Detect frees in basecn
        return SYS_ERR_OK;
    }

    if (cnodecmp(ret.cnode, cnode_root)) {
        struct slot_allocator *ca = (struct slot_allocator*)(&state->rootca);
        return ca->free(ca, ret);
    }

    struct slot_allocator *ca = (struct slot_allocator*)(&state->defca);
    errval_t err = ca->free(ca, ret);
    // XXX: Detect frees in special case of init and mem_serv
    if (err_no(err) == LIB_ERR_SLOT_ALLOC_WRONG_CNODE) {
        return SYS_ERR_OK;
    }
    return err;
}

/**
 * \brief Initialize the slot_allocator
 *
 * Initializes the default and root slot_allocator.
 */
errval_t slot_alloc_init(void)
{
    errval_t err;

    struct slot_alloc_state *state = get_slot_alloc_state();

    /* Default allocator */
    // While initializing, other domains will call into it. Be careful
    struct capref cap;
    struct cnoderef cnode;
    struct multi_slot_allocator *def = &state->defca;

    // Generic
    thread_mutex_init(&def->a.mutex);

    def->a.alloc = multi_alloc;
    def->a.free  = multi_free;
    def->a.space = SLOT_ALLOC_CNODE_SLOTS;
    def->a.nslots = SLOT_ALLOC_CNODE_SLOTS;

    def->top = (struct slot_allocator*)&state->top;
    def->head = &state->head;
    def->head->next = NULL;
    def->reserve = &state->reserve;
    def->reserve->next = NULL;

    // Top
    cap.cnode = cnode_root;
    cap.slot  = ROOTCN_SLOT_SLOT_ALLOC0;
    cnode = build_cnoderef(cap, SLOT_ALLOC_CNODE_BITS);
    err = single_slot_alloc_init_raw((struct single_slot_allocator*)def->top,
                                     cap, cnode,
                                     SLOT_ALLOC_CNODE_SLOTS, state->top_buf,
                                     sizeof(state->top_buf));
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
    }

    // Head
    cap.cnode = cnode_root;
    cap.slot  = ROOTCN_SLOT_SLOT_ALLOC1;
    cnode = build_cnoderef(cap, SLOT_ALLOC_CNODE_BITS);
    err = single_slot_alloc_init_raw(&def->head->a, cap, cnode,
                                     SLOT_ALLOC_CNODE_SLOTS, state->head_buf,
                                     sizeof(state->head_buf));
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
    }

    // Reserve
    cap.cnode = cnode_root;
    cap.slot  = ROOTCN_SLOT_SLOT_ALLOC2;
    cnode = build_cnoderef(cap, SLOT_ALLOC_CNODE_BITS);
    err = single_slot_alloc_init_raw(&def->reserve->a, cap, cnode,
                                     SLOT_ALLOC_CNODE_SLOTS, state->reserve_buf,
                                     sizeof(state->reserve_buf));
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
    }

    // Slab
    size_t allocation_unit = sizeof(struct slot_allocator_list) +
                             SINGLE_SLOT_ALLOC_BUFLEN(SLOT_ALLOC_CNODE_SLOTS);
    slab_init(&def->slab, allocation_unit, NULL);

    // Vspace mgmt
    // Warning: necessary to do this in the end as during initialization,
    // libraries can call into slot_alloc.
    err = vspace_mmu_aware_init(&def->mmu_state,
                                allocation_unit * SLOT_ALLOC_CNODE_SLOTS * 2);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MMU_AWARE_INIT);
    }

    /* Root allocator */
    err = single_slot_alloc_init_raw(&state->rootca, cap_root, cnode_root,
                                     DEFAULT_CNODE_SLOTS, state->root_buf,
                                     sizeof(state->root_buf));
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
    }
    state->rootca.a.space     = DEFAULT_CNODE_SLOTS - ROOTCN_FREE_EP_SLOTS;
    state->rootca.head->space = DEFAULT_CNODE_SLOTS - ROOTCN_FREE_EP_SLOTS;
    state->rootca.head->slot  = ROOTCN_FREE_EP_SLOTS;

    return SYS_ERR_OK;
}
