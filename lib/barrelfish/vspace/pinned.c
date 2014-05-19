/**
 * \file
 * \brief Managing the pinned memory for vspace metadata
 *
 * Warning: This code is coupled with code in slot_alloc/. and pmap_*.
 *
 * Slabs required for various lists in vspace
 * and memobj are backed by this memobj.
 * This memory is pinned since the frames are not tracked
 * and cannot be mapped into multiple vregions.
 *
 * If the slabs maintained in the state are out of memory, it needs to be grown.
 * This file will require 1 slot for frame capability
 * and additional to create the mappings.
 * The amount required can be calculated by refering to the pmap_*.
 *
 * Growing requires 1 slot from this file.
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
#include <barrelfish/core_state_arch.h>
#include "vspace_internal.h"


/**
 * \brief Initialize the pinned region
 *
 * Allocates a region of virtual address space and initializes its state.
 */
errval_t vspace_pinned_init(void)
{
    errval_t err;

    struct pinned_state *state = get_current_pinned_state();
    struct vspace *vspace = get_current_vspace();

    err = memobj_create_pinned(&state->memobj,
                               VSPACE_PINNED_SIZE, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_CREATE_PINNED);
    }

    err = vregion_map(&state->vregion, vspace,
                      (struct memobj*)&state->memobj, 0, VSPACE_PINNED_SIZE,
                      VREGION_FLAGS_READ_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VREGION_MAP);
    }

    state->offset = 0;
    thread_mutex_init(&state->mutex);
    slab_init(&state->vregion_list_slab, VSPACE_PINNED_UNIT *
              sizeof(struct vregion_list), NULL);
    slab_init(&state->frame_list_slab, VSPACE_PINNED_UNIT *
              sizeof(struct memobj_frame_list), NULL);

    return SYS_ERR_OK;
}

/**
 * \brief Allocate some slabs
 *
 * \param retbuf     Pointer to return the allocated memory
 * \param slab_type  Type of slab the memory is allocated for
 *
 * Since this region is used for backing specific slabs,
 * only those types of slabs can be allocated.
 */
errval_t vspace_pinned_alloc(void **retbuf, enum slab_type slab_type)
{
    errval_t err;
    struct pinned_state *state = get_current_pinned_state();

    // Select slab type
    struct slab_alloc *slab;
    switch(slab_type) {
    case VREGION_LIST:
        slab = &state->vregion_list_slab;
        break;
    case FRAME_LIST:
        slab = &state->frame_list_slab;
        break;
    default:
        return LIB_ERR_VSPACE_PINNED_INVALID_TYPE;
    }

    thread_mutex_lock(&state->mutex);

    // Try allocating
    void *buf = slab_alloc(slab);
    if (buf == NULL) {
        // Out of memory, grow
        struct capref frame;
        err = frame_alloc(&frame, BASE_PAGE_SIZE, NULL);
        if (err_is_fail(err)) {
            thread_mutex_unlock(&state->mutex);
            return err_push(err, LIB_ERR_FRAME_ALLOC);
        }
        err = state->memobj.m.f.fill((struct memobj*)&state->memobj,
                                     state->offset, frame,
                                     BASE_PAGE_SIZE);
        if (err_is_fail(err)) {
            thread_mutex_unlock(&state->mutex);
            return err_push(err, LIB_ERR_MEMOBJ_FILL);
        }

        genvaddr_t gvaddr = vregion_get_base_addr(&state->vregion) +
            state->offset;
        void *slab_buf = (void*)vspace_genvaddr_to_lvaddr(gvaddr);
        slab_grow(slab, slab_buf, BASE_PAGE_SIZE);
        state->offset += BASE_PAGE_SIZE;

        // Try again
        buf = slab_alloc(slab);
    }

    thread_mutex_unlock(&state->mutex);

    if (buf == NULL) {
        return LIB_ERR_SLAB_ALLOC_FAIL;
    } else {
        *retbuf = buf;
        return SYS_ERR_OK;
    }
}
