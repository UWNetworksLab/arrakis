/**
 * \file
 * \brief Code for managing VSpace of a new domain when it is spawned
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include "spawn.h"

/**
 * \brief Initialize the vspace for the domain being spawned
 *
 * \param vnode The pml4 cap for the new domain
 *
 * \bug architecture specific
 */
errval_t spawn_vspace_init(struct spawninfo *si, struct capref vnode,
                           enum cpu_type cpu_type)
{
    errval_t err;
    struct pmap *pmap = NULL;

    si->vspace = malloc(sizeof (struct vspace));
    if (si->vspace == NULL) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }
    pmap = malloc(ARCH_DEFAULT_PMAP_SIZE);
    if (!pmap) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    err = pmap_init(pmap, si->vspace, vnode, &si->pagecn_slot_alloc.a);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_PMAP_INIT);
        goto error;
    }

    err = vspace_init(si->vspace, pmap);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_VSPACE_INIT);
        goto error;
    }

    return SYS_ERR_OK;

 error: // XXX: proper cleanup
    if (si->vspace) {
        free(si->vspace);
    }
    if (pmap) {
        free(pmap);
    }
    return err;
}

/**
 * \brief Map one frame anywhere
 */
errval_t spawn_vspace_map_one_frame(struct spawninfo *si, genvaddr_t *retaddr,
                                    struct capref frame, size_t size)
{
    errval_t err;
    struct vregion *vregion = NULL;
    struct memobj_one_frame *memobj = NULL;

    vregion = malloc(sizeof(struct vregion));
    if (!vregion) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }
    memobj = malloc(sizeof(struct memobj_one_frame));
    if (!memobj) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    err = memobj_create_one_frame(memobj, size, 0);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_CREATE_ANON);
        goto error;
    }
    err = memobj->m.f.fill(&memobj->m, 0, frame, size);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_FILL);
        goto error;
    }
    err = vregion_map(vregion, si->vspace, &memobj->m, 0, size,
                      VREGION_FLAGS_READ_WRITE);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_VSPACE_MAP);
        goto error;
    }
    err = memobj->m.f.pagefault(&memobj->m, vregion, 0, 0);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        goto error;
    }

    *retaddr = vregion_get_base_addr(vregion);
    return SYS_ERR_OK;

 error: // XXX: proper cleanup
    if (vregion) {
        free(vregion);
    }
    if (memobj) {
        free(memobj);
    }
    return err;
}

/**
 * \brief Map one frame at the given addr
 */
errval_t spawn_vspace_map_fixed_one_frame(struct spawninfo *si, genvaddr_t addr,
                                          struct capref frame, size_t size)
{
    errval_t err;
    struct vregion *vregion = NULL;
    struct memobj_one_frame *memobj = NULL;

    vregion = malloc(sizeof(struct vregion));
    if (!vregion) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }
    memobj = malloc(sizeof(struct memobj_one_frame));
    if (!memobj) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    err = memobj_create_one_frame(memobj, size, 0);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_CREATE_ANON);
        goto error;
    }
    err = memobj->m.f.fill(&memobj->m, 0, frame, size);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_FILL);
        goto error;
    }
    err = vregion_map_fixed(vregion, si->vspace, &memobj->m, 0, size, addr,
                            VREGION_FLAGS_READ_WRITE);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_VSPACE_MAP);
        goto error;
    }
    err = memobj->m.f.pagefault(&memobj->m, vregion, 0, 0);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        goto error;
    }

    return SYS_ERR_OK;

 error: // XXX: proper cleanup
    if (vregion) {
        free(vregion);
    }
    if (memobj) {
        free(memobj);
    }
    return err;
}

/**
 * \brief Return memobj and vregion for anonymous type mapping
 */
errval_t spawn_vspace_map_anon_fixed_attr(struct spawninfo *si, genvaddr_t addr,
                                          size_t size, struct vregion **vregion,
                                          struct memobj **memobj,
                                          vregion_flags_t flags)
{
    errval_t err;

    // Allocate space
    *vregion = malloc(sizeof(struct vregion));
    if (!*vregion) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }
    *memobj = malloc(sizeof(struct memobj_anon));
    if (!*memobj) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    // Create the objects
    err = memobj_create_anon((struct memobj_anon*)*memobj, size, 0);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_CREATE_ANON);
        goto error;
    }
    err = vregion_map_fixed(*vregion, si->vspace, *memobj, 0, size, addr,
                            flags);
    if (err_is_fail(err)) {
        // err = LIB_ERR_VSPACE_MAP;
        goto error;
    }

    return SYS_ERR_OK;

 error: // XXX: proper cleanup
    if (*vregion) {
        free(*vregion);
    }
    if (*memobj) {
        free(*memobj);
    }
    return err;
}
