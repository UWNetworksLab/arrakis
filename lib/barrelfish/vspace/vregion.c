/**
 * \file
 * \brief vregion management
 *
 * A vregion is backed by a memory object and a vspace.
 * A vregion manages a range of virtual address space.
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
 * \brief Setup a new vregion with alignment constraints in an address space
 *
 * \param vregion  The vregion
 * \param vspace   The vspace to associate with the vregion
 * \param memobj   The memory object to associate with the region
 * \param offset   Offset into the memory object
 * \param size     Size of the memoryg object to use
 * \param flags    Vregion specific flags
 * \param alignment Minimum required alignment of mapping (may be increased)
 */
errval_t vregion_map_aligned(struct vregion *vregion, struct vspace* vspace,
                             struct memobj *memobj, size_t offset, size_t size,
                             vregion_flags_t flags, size_t alignment)
{
    errval_t err;
    struct pmap *pmap = vspace_get_pmap(vspace);

    // Allocate some virtual address space
    genvaddr_t address;
    err = pmap->f.determine_addr(pmap, memobj, alignment, &address);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_DETERMINE_ADDR);
    }

    // Initialize
    vregion->vspace = vspace;
    vregion->memobj = memobj;
    vregion->base   = address;
    vregion->offset = offset;
    vregion->size   = size;
    vregion->flags  = flags;

    // Add to the vspace
    err = vspace_add_vregion(vspace, vregion);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_ADD_REGION);
    }

    // Add to memobj
    err = memobj->f.map_region(memobj, vregion);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_MAP_REGION);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Setup a new vregion anywhere in the address space
 *
 * \param vregion  The vregion
 * \param vspace   The vspace to associate with the vregion
 * \param memobj   The memory object to associate with the region
 * \param offset   Offset into the memory object
 * \param size     Size of the memory object to use
 * \param flags    Vregion specific flags
 */
errval_t vregion_map(struct vregion *vregion, struct vspace *vspace,
                     struct memobj *memobj, size_t offset, size_t size,
                     vregion_flags_t flags)
{
    return vregion_map_aligned(vregion, vspace, memobj, offset, size, flags, 0);
}

/**
 * \brief Setup a new vregion at a specified location
 *
 * \param vregion  The region
 * \param vspace   The vspace to associate with the region
 * \param memobj   The memory object to associate with the region
 * \param offset   Offset into the memory object
 * \param size     Size of the memory object to use
 * \param addr     Address to create the vregion for
 * \param flags    Vregion specific flags
 */
errval_t vregion_map_fixed(struct vregion *vregion, struct vspace *vspace,
                           struct memobj *memobj, size_t offset, size_t size,
                           genvaddr_t addr, vregion_flags_t flags)
{
    errval_t err;

    // FIXME: this check is arch-specific and should involve a call on the pmap
    if (addr % BASE_PAGE_SIZE != 0) {
        return LIB_ERR_VREGION_BAD_ALIGNMENT;
    }

    // Initialize
    vregion->vspace = vspace;
    vregion->memobj = memobj;
    vregion->base   = addr;
    vregion->offset = offset;
    vregion->size   = size;
    vregion->flags  = flags;

    // Add to vspace
    err = vspace_add_vregion(vspace, vregion);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_ADD_REGION);
    }

    // Add to memobj
    err = memobj->f.map_region(memobj, vregion);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_MAP_REGION);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Destroy the given region
 *
 * \return SYS_ERR_OK on success, error code on failure
 *
 * \bug This only works if the memobj type is memobj_one_frame.
 */
errval_t vregion_destroy(struct vregion *vregion)
{
    errval_t err;

    struct vspace *vspace = vregion_get_vspace(vregion);
    if (vspace != NULL) {
        err = vspace_remove_vregion(vspace, vregion);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_VSPACE_REMOVE_REGION);
        }
    }

    struct memobj *memobj = vregion_get_memobj(vregion);
    if (memobj != NULL) {
        err = memobj->f.unmap_region(memobj, vregion);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_UNMAP_REGION);
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Page fault handler
 *
 * \param vregion  The vregion the fault occured in
 * \param addr   The faulting address
 * \param type   The fault type
 *
 * Look up the appropriate memory object and forward the fault to it
 */
errval_t vregion_pagefault_handler(struct vregion *vregion, genvaddr_t addr,
                                   vm_fault_type_t type)
{
    struct memobj *memobj = vregion->memobj;
    genvaddr_t offset = addr - vregion->base;

    return memobj->f.pagefault(memobj, vregion, offset, type);
}
