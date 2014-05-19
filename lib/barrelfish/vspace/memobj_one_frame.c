/**
 * \file
 * \brief memory object of type single frame
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
    struct memobj_one_frame *one_frame = (struct memobj_one_frame*)memobj;

    // Allocate and insert
    struct vregion_list *data = malloc(sizeof(struct vregion_list));
    if (!data) {
        return LIB_ERR_MALLOC_FAIL;
    }
    data->region = vregion;
    struct vregion_list *walk = one_frame->vregion_list;
    one_frame->vregion_list = data;
    data->next = walk;

    return SYS_ERR_OK;
}

/**
 * \brief Unmap the memory object from a region
 *
 * \param memobj   The memory object
 * \param region  The region to remove
 *
 * Will delete self using free if no more vregions are mapped in
 */
static errval_t unmap_region(struct memobj *memobj, struct vregion *vregion)
{
    errval_t err;
    struct memobj_one_frame *one_frame = (struct memobj_one_frame*)memobj;

    /* Unmap the affected area in the pmap */
    // XXX: assuming that area is mapped in pmap, i.e. pagefault was called
    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap     = vspace_get_pmap(vspace);
    genvaddr_t vregion_base  = vregion_get_base_addr(vregion);
    genvaddr_t vregion_off   = vregion_get_offset(vregion);

    err = pmap->f.unmap(pmap, vregion_base + vregion_off, memobj->size, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_UNMAP);
    }

    /* Remove the vregion from the list */
    struct vregion_list *walk = one_frame->vregion_list;
    struct vregion_list *prev = NULL;
    while (walk) {
        if (walk->region == vregion) {
            if (prev) {
                prev->next = walk->next;
                free(walk);
                return SYS_ERR_OK;
            } else {
                one_frame->vregion_list = walk->next;
                free(walk);
                free(memobj);
                return SYS_ERR_OK;
            }
        }
        prev = walk;
        walk = walk->next;
    }

    return LIB_ERR_VREGION_NOT_FOUND;
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
}

/**
 * \brief Set a frame for an offset into the memobj
 *
 * \param memobj  The memory object
 * \param offset  Offset into the memory object
 * \param frame   The frame cap for the offset
 * \param size    The size of frame cap
 */
static errval_t fill(struct memobj *memobj, genvaddr_t offset, struct capref frame,
                     size_t size)
{
    struct memobj_one_frame *one_frame = (struct memobj_one_frame*)memobj;

    assert(size == memobj->size);
    one_frame->frame  = frame;
    one_frame->offset = offset;

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
    struct memobj_one_frame *one_frame = (struct memobj_one_frame*)memobj;
    if (offset < one_frame->offset ||
        offset > one_frame->offset + memobj->size) {
        return LIB_ERR_MEMOBJ_WRONG_OFFSET;
    }

    // Map the single frame
    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap     = vspace_get_pmap(vspace);
    genvaddr_t vregion_base  = vregion_get_base_addr(vregion);
    genvaddr_t vregion_off   = vregion_get_offset(vregion);
    vregion_flags_t flags = vregion_get_flags(vregion);

    err = pmap->f.map(pmap, vregion_base + vregion_off, one_frame->frame,
                      one_frame->offset, memobj->size, flags, NULL, NULL);
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
}

/**
 * \brief Initialize a memory object of type one frame
 *
 * \param memobj  The memory object
 * \param size    Size of the memory region
 * \param flags   Memory object specific flags
 * \param frame   The frame to use
 * \param offset  Offset into the frame
 */
errval_t memobj_create_one_frame(struct memobj_one_frame *one_frame,
                                 size_t size, memobj_flags_t flags)
{
    struct memobj *memobj = &one_frame->m;

    /* Generic portion */
    memobj->f.map_region   = map_region;
    memobj->f.unmap_region = unmap_region;
    memobj->f.protect = protect;
    memobj->f.pin   = pin;
    memobj->f.unpin = unpin;
    memobj->f.fill  = fill;
    memobj->f.pagefault  = pagefault;
    memobj->f.pager_free = pager_free;

    memobj->size  = size;
    memobj->flags = flags;

    memobj->type = ONE_FRAME;

    /* one_frame specific portion */
    one_frame->vregion_list = NULL;
    return SYS_ERR_OK;
}

/**
 * \brief Destroy the object
 *
 * \bug NYI
 */
errval_t memobj_destroy_one_frame(struct memobj *memobj)
{
    return SYS_ERR_OK;
}
