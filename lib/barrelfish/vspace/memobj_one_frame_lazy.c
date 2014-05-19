/**
 * \file
 * \brief memory object which maintains a single frame that is lazily mapped
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "vspace_internal.h"

/**
 * \brief Map the memory object into a region
 *
 * \param memobj  The memory object
 * \param region  The region to add
 */
static errval_t map_region(struct memobj *memobj, struct vregion *vregion)
{
    struct memobj_one_frame_lazy *lazy = (struct memobj_one_frame_lazy*)memobj;

    // Allocate and insert
    struct vregion_list *data = malloc(sizeof(struct vregion_list));
    if (!data) {
        return LIB_ERR_MALLOC_FAIL;
    }
    data->region = vregion;
    struct vregion_list *walk = lazy->vregion_list;
    lazy->vregion_list = data;
    data->next = walk;

    return SYS_ERR_OK;
}

/**
 * \brief Unmap the memory object from a region
 *
 * \param memobj   The memory object
 * \param region  The region to remove
 */
static errval_t unmap_region(struct memobj *memobj, struct vregion *vregion)
{
    USER_PANIC("NYI");

    return SYS_ERR_OK;
}

/**
 * \brief Set the protection on a range
 *
 * \param memobj  The memory object
 * \param region  The vregion to modify the mappings on
 * \param offset  Offset into the memory object
 * \param range   The range of space to set the protection for
 * \param flags   The protection flags
 */
static errval_t protect(struct memobj *memobj, struct vregion *vregion,
                        genvaddr_t offset, size_t range, vs_prot_flags_t flags)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

/**
 * \brief Pin a range
 *
 * \param memobj  The memory object
 * \param region  The vregion to modify the state on
 * \param offset  Offset into the memory object
 * \param range   The range of space to pin
 */
static errval_t pin(struct memobj *memobj, struct vregion *vregion,
                    genvaddr_t offset, size_t range)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

/**
 * \brief Unpin a range
 *
 * \param memobj  The memory object
 * \param region  The vregion to modify the state on
 * \param offset  Offset into the memory object
 * \param range   The range of space to unpin
 */
static errval_t unpin(struct memobj *memobj, struct vregion *vregion,
                      genvaddr_t offset, size_t range)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

/**
 * \brief Page fault handler
 *
 * \param memobj  The memory object
 * \param region  The associated vregion
 * \param offset  Offset into memory object of the page fault
 * \param type    The fault type
 */
static errval_t pagefault(struct memobj *memobj, struct vregion *vregion,
                                 genvaddr_t offset, vm_fault_type_t type)
{
    errval_t err;
    struct memobj_one_frame_lazy *lazy = (struct memobj_one_frame_lazy*)memobj;

    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap     = vspace_get_pmap(vspace);
    genvaddr_t vregion_base  = vregion_get_base_addr(vregion);
    genvaddr_t vregion_off   = vregion_get_offset(vregion);
    vregion_flags_t flags = vregion_get_flags(vregion);

    // XXX: ugly --> need to revoke lazy->frame in order to clean up
    //               all the copies that are created here
    struct capref frame_copy;
    err = slot_alloc(&frame_copy);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_copy(frame_copy, lazy->frame);
    if (err_is_fail(err)) {
        return err;
    }

    err = pmap->f.map(pmap, vregion_base + vregion_off + offset, frame_copy,
                      offset, lazy->chunk_size, flags, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_MAP);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Free up some pages by placing them in the backing storage
 *
 * \param memobj      The memory object
 * \param size        The amount of space to free up
 * \param frames      An array of capref frames to return the freed pages
 * \param num_frames  The number of frames returned
 *
 * This will affect all the vregions that are associated with the object
 */
static errval_t pager_free(struct memobj *memobj, size_t size,
                                  struct capref *frames, size_t num_frames)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

/**
 * \brief Initialize a memory object of type one frame
 *
 * \param memobj  The memory object
 * \param size    Size of the memory region
 * \param flags   Memory object specific flags
 * \param frame   The frame to use
 */
errval_t memobj_create_one_frame_lazy(struct memobj_one_frame_lazy *lazy,
                                      size_t size, memobj_flags_t flags,
                                      struct capref frame, size_t chunk_size)
{
    struct memobj *memobj = &lazy->m;
    /* Generic portion */
    memobj->f.map_region = map_region;
    memobj->f.unmap_region = unmap_region;
    memobj->f.protect = protect;
    memobj->f.pin = pin;
    memobj->f.unpin = unpin;
    memobj->f.pagefault = pagefault;
    memobj->f.pager_free = pager_free;

    memobj->size  = size;
    memobj->flags = flags;

    memobj->type = ONE_FRAME_LAZY;

    /* lazy specific portion */
    lazy->vregion_list = NULL;
    lazy->chunk_size   = chunk_size;
    lazy->frame        = frame;

    return SYS_ERR_OK;
}
