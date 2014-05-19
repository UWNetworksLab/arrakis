/**
 * \file
 * \brief memory object of anonymous type.
 * The object maintains a list of frames.
 *
 * The object maintains a list of frames and a list of vregions.
 * The lists are backed by slabs.
 * The slabs may have to be grown,
 * in which case the object will use #vspace_pinned_alloc.
 *
 * morecore uses this memory object so it cannot use malloc for its lists.
 * Therefore, this uses slabs and grows them using the pinned memory.
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
#include "vspace_internal.h"

/**
 * \brief Map the memory object into a region
 *
 * \param memobj   The memory object
 * \param region  The region to add
 */
static errval_t map_region(struct memobj *memobj, struct vregion *vregion)
{
    errval_t err;
    struct memobj_anon *anon = (struct memobj_anon*)memobj;

    // Allocate space
    struct vregion_list *data = slab_alloc(&anon->vregion_slab);
    if (!data) { // Grow
        void *buf;
        err = vspace_pinned_alloc(&buf, VREGION_LIST);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_VSPACE_PINNED_ALLOC);
        }
        slab_grow(&anon->vregion_slab, buf,
                  VSPACE_PINNED_UNIT * sizeof(struct vregion_list));
        data = slab_alloc(&anon->vregion_slab);
        if (!data) {
            return LIB_ERR_SLAB_ALLOC_FAIL;
        }
    }
    data->region = vregion;

    // Insert into the list
    struct vregion_list *walk = anon->vregion_list;
    anon->vregion_list = data;
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
    struct memobj_anon *anon = (struct memobj_anon*)memobj;
    errval_t err;

    /* Unmap the affected area in the pmap */
    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap     = vspace_get_pmap(vspace);
    genvaddr_t vregion_base  = vregion_get_base_addr(vregion);
    genvaddr_t vregion_off   = vregion_get_offset(vregion);
    size_t vregion_size = vregion_get_size(vregion);
    genvaddr_t vregion_end = vregion_off + vregion_size;

    //printf("(%s:%d) unmap(0x%"PRIxGENVADDR", memobj->size = %zd) vregion size = %zd\n", __FILE__, __LINE__, vregion_base + vregion_off, memobj->size, vregion_size);

    // unmap all affected frames
    struct memobj_frame_list *fwalk = anon->frame_list;
    struct memobj_frame_list *fprev = NULL;
    //printf("vregion_off = 0x%"PRIxGENVADDR"\n", vregion_off);
    //printf("vregion_end = 0x%"PRIxGENVADDR"\n", vregion_end);
    err = LIB_ERR_VSPACE_VREGION_NOT_FOUND;
    while (fwalk) {
        //printf("fwalk->offset = %zd\n", fwalk->offset);
        //printf("fwalk->next   = %p\n", fwalk->next);
        if (fwalk->offset < vregion_off) {
            fprev = fwalk;
            fwalk = fwalk->next;
            continue;
        }
        else if (fwalk->offset < vregion_end) {
            err = pmap->f.unmap(pmap, vregion_base + vregion_off, fwalk->size, NULL);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_UNMAP);
            }

            /* Remove the vregion from the list */
            struct vregion_list *prev = NULL;
            for (struct vregion_list *elt = anon->vregion_list; elt != NULL;
                 elt = elt->next) {
                if (elt->region == vregion) {
                    if (prev == NULL) {
                        assert(elt == anon->vregion_list);
                        anon->vregion_list = elt->next;
                    } else {
                        assert(prev->next == elt);
                        prev->next = elt->next;
                    }
                    slab_free(&anon->vregion_slab, elt);
                    err = SYS_ERR_OK;
                }
            }
            vregion_off += fwalk->size;
            fprev = fwalk;
            fwalk = fwalk->next;
        }
    }

    return err; // XXX: not quite the right error
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
    struct vspace *vspace  = vregion_get_vspace(vregion);
    struct pmap *pmap      = vspace_get_pmap(vspace);
    genvaddr_t base        = vregion_get_base_addr(vregion);
    genvaddr_t vregion_off = vregion_get_offset(vregion);
    errval_t err;

    err = pmap->f.modify_flags(pmap, base + vregion_off + offset, range,
                               flags, &range);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
    }

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
 *
 * Pagefault relies on frames inserted in order
 */
static errval_t fill(struct memobj *memobj, genvaddr_t offset, struct capref frame,
                     size_t size)
{
    errval_t err;
    struct memobj_anon *anon = (struct memobj_anon*)memobj;

    assert((offset & BASE_PAGE_MASK) == 0);

    // AB: allow frame to overlap end of memobj; that might have been the most
    // efficient allocation size (even if the end of the frame will be unusable)
    if (offset >= memobj->size) {
        return LIB_ERR_MEMOBJ_WRONG_OFFSET;
    }

    // Allocate
    struct memobj_frame_list *new = slab_alloc(&anon->frame_slab);
    if (!new) { // Grow
        void *buf;
        err = vspace_pinned_alloc(&buf, FRAME_LIST);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_VSPACE_PINNED_ALLOC);
        }
        slab_grow(&anon->frame_slab, buf,
                  VSPACE_PINNED_UNIT * sizeof(struct memobj_frame_list));
        new = slab_alloc(&anon->frame_slab);
        if (!new) {
            return LIB_ERR_SLAB_ALLOC_FAIL;
        }
    }
    new->offset = offset;
    new->frame  = frame;
    new->size   = size;

    {
        struct frame_identity id;
        err = invoke_frame_identify(frame, &id);
        assert(err_is_ok(err));
        new->pa = id.base;
    }

    // Insert in order
    struct memobj_frame_list *walk = anon->frame_list;
    struct memobj_frame_list *prev = NULL;
    while(walk) {
        if (new->offset < walk->offset) {
            if ((prev != NULL && new->offset < prev->offset + prev->size)
                || new->offset + new->size > walk->offset) {
                slab_free(&anon->frame_slab, new);
                return LIB_ERR_MEMOBJ_DUPLICATE_FILL;
            }
            new->next  = walk;
            if (prev != NULL) {
                prev->next = new;
            } else {
                assert(walk == anon->frame_list);
                anon->frame_list = new;
            }
            return SYS_ERR_OK;
        }
        prev = walk;
        walk = walk->next;
    }
    if (prev != NULL) {
        if (new->offset < prev->offset + prev->size) {
            slab_free(&anon->frame_slab, new);
            return LIB_ERR_MEMOBJ_DUPLICATE_FILL;
        }
        prev->next = new;
        new->next = NULL;
    } else {
        assert(anon->frame_list == NULL);
        anon->frame_list = new;
        new->next = NULL;
    }
    return SYS_ERR_OK;
}

/**
 * \brief Unmap/remove one frame from the end of the memobj
 *
 * \param memobj     The memory object
 * \param offset     The offset from which to remove a frame from
 * \param ret_frame  Pointer to return the removed frame
 *
 * This will try to remove one frame at an offset greater than the one
 * specified. Call this function again and again till it returns the
 * LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET error to get all frames.
 */
static errval_t unfill(struct memobj *memobj, genvaddr_t offset,
                       struct capref *ret_frame, genvaddr_t *ret_offset)
{
    errval_t err;
    struct memobj_anon *anon = (struct memobj_anon*)memobj;

    // Walk the ordered list of frames to find one right frame
    struct memobj_frame_list *fwalk = anon->frame_list;
    struct memobj_frame_list *fprev = NULL;
    while (fwalk) {
        if (fwalk->offset < offset) {
            fprev = fwalk;
            fwalk = fwalk->next;
            continue;
        }
        goto cont;
    }

    // The specified offset is too high.
    return LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET;

 cont:

    { // Unmap the frame from all vregions
        struct vregion_list *vwalk = anon->vregion_list;
        while (vwalk) {
            struct vspace *vspace = vregion_get_vspace(vwalk->region);
            struct pmap *pmap     = vspace_get_pmap(vspace);
            genvaddr_t vregion_base  = vregion_get_base_addr(vwalk->region);
            size_t retsize;

            assert((vregion_base + fwalk->offset) % BASE_PAGE_SIZE == 0);
            //printf("(%s:%d) unmap(0x%"PRIxGENVADDR", %zd)\n", __FILE__, __LINE__, vregion_base + fwalk->offset, fwalk->size);
            err = pmap->f.unmap(pmap, vregion_base + fwalk->offset, fwalk->size,
                                &retsize);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_UNMAP);
            }
            assert(retsize == fwalk->size);
            vwalk = vwalk->next;
        }
    }

    // Return the frame
    *ret_offset = fwalk->offset;
    *ret_frame = fwalk->frame;
    if (fprev) {
        fprev->next = fwalk->next;
    } else {
        anon->frame_list = fwalk->next;
    }
    slab_free(&anon->frame_slab, fwalk);
    return SYS_ERR_OK;
}

/**
 * \brief Page fault handler
 *
 * \param memobj  The memory object
 * \param region  The associated vregion
 * \param offset  Offset into memory object of the page fault
 * \param type    The fault type
 *
 * Locates the frame for the offset and maps it in.
 * Relies on fill inserting frames in order.
 */
static errval_t pagefault(struct memobj *memobj, struct vregion *vregion,
                          genvaddr_t offset, vm_fault_type_t type)
{
    errval_t err;
    struct memobj_anon *anon = (struct memobj_anon*)memobj;

    // Walk the ordered list for the frame and map it in
    struct memobj_frame_list *walk = anon->frame_list;
    while (walk) {
        if (offset >= walk->offset && offset < walk->offset + walk->size) {
            struct vspace *vspace = vregion_get_vspace(vregion);
            struct pmap *pmap     = vspace_get_pmap(vspace);
            genvaddr_t base          = vregion_get_base_addr(vregion);
            genvaddr_t vregion_off   = vregion_get_offset(vregion);
            vregion_flags_t flags = vregion_get_flags(vregion);
            err = pmap->f.map(pmap, base + vregion_off + walk->offset,
                              walk->frame, 0, walk->size, flags, NULL, NULL);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_MAP);
            }
            return SYS_ERR_OK;
        }
        walk = walk->next;
    }

    return LIB_ERR_MEMOBJ_WRONG_OFFSET;
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
 * \brief Initialize
 *
 * \param memobj  The memory object
 * \param size    Size of the memory region
 * \param flags   Memory object specific flags
 *
 * This object handles multiple frames.
 * The frames are mapped in on demand.
 */
errval_t memobj_create_anon(struct memobj_anon *anon, size_t size,
                            memobj_flags_t flags)
{
    struct memobj *memobj = &anon->m;

    /* Generic portion */
    memobj->f.map_region   = map_region;
    memobj->f.unmap_region = unmap_region;
    memobj->f.protect      = protect;
    memobj->f.pin          = pin;
    memobj->f.unpin        = unpin;
    memobj->f.fill         = fill;
    memobj->f.unfill       = unfill;
    memobj->f.pagefault    = pagefault;
    memobj->f.pager_free   = pager_free;

    memobj->size  = size;
    memobj->flags = flags;

    memobj->type = ANONYMOUS;

    /* anon specific portion */
    slab_init(&anon->vregion_slab, sizeof(struct vregion_list), NULL);
    slab_init(&anon->frame_slab, sizeof(struct memobj_frame_list), NULL);

    anon->vregion_list = NULL;
    anon->frame_list = NULL;
    return SYS_ERR_OK;
}

/**
 * \brief Destroy the object
 *
 */
errval_t memobj_destroy_anon(struct memobj *memobj)
{
    struct memobj_anon *m = (struct memobj_anon *)memobj;

    errval_t err = SYS_ERR_OK;

    struct vregion_list *vwalk = m->vregion_list;
    while (vwalk) {
        err = vregion_destroy(vwalk->region);
        if (err_is_fail(err)) {
            return err;
        }
        struct vregion_list *old = vwalk;
        vwalk = vwalk->next;
        slab_free(&m->vregion_slab, old);
    }

    struct memobj_frame_list *fwalk = m->frame_list;
    while (fwalk) {
        err = cap_delete(fwalk->frame);
        if (err_is_fail(err)) {
            return err;
        }
        struct memobj_frame_list *old = fwalk;
        fwalk = fwalk->next;
        slab_free(&m->frame_slab, old);
    }
    return err;
}
