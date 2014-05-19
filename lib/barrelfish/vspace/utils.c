/**
 * \file
 * \brief Helpful utility functions
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

/**
 * \brief Translate a lvaddr_t to genvaddr_t
 */
genvaddr_t vspace_lvaddr_to_genvaddr(lvaddr_t lvaddr)
{
    struct vspace *vspace = get_current_vspace();
    return vspace_layout_lvaddr_to_genvaddr(&vspace->layout, lvaddr);
}

/**
 * \brief Translate a genvaddr_t to lvaddr_t
 */
lvaddr_t vspace_genvaddr_to_lvaddr(genvaddr_t genvaddr)
{
    struct vspace *vspace = get_current_vspace();
    return vspace_layout_genvaddr_to_lvaddr(&vspace->layout, genvaddr);
}

errval_t vspace_unmap(const void *buf)
{
    errval_t err;

    struct vregion *vregion = vspace_get_region(get_current_vspace(), buf);
    assert(vregion);

    err = vregion_destroy(vregion);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VREGION_DESTROY);
    }

    return SYS_ERR_OK;
}

/// Map with an alignment constraint
errval_t vspace_map_anon_nomalloc(void **retaddr, struct memobj_anon *memobj,
                                  struct vregion *vregion, size_t size,
                                  size_t *retsize, vregion_flags_t flags,
                                  size_t alignment)
{
    errval_t err1, err2;
    size = ROUND_UP(size, BASE_PAGE_SIZE);
    if (retsize) {
        *retsize = size;
    }

    // Create a memobj and vregion
    err1 = memobj_create_anon(memobj, size, 0);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_MEMOBJ_CREATE_ANON);
        goto error;
    }
    err1 = vregion_map_aligned(vregion, get_current_vspace(),
                               (struct memobj *)memobj, 0, size,
                               flags, alignment);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_VREGION_MAP);
        goto error;
    }

    *retaddr = (void*)vspace_genvaddr_to_lvaddr(vregion_get_base_addr(vregion));

    return SYS_ERR_OK;

 error:
    if (err_no(err1) !=  LIB_ERR_MEMOBJ_CREATE_ANON) {
        err2 = memobj_destroy_anon((struct memobj *)memobj);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "memobj_destroy_anon failed");
        }
    }
    return err1;
}

/**
 * \brief Wrapper for creating and mapping a memory object of type anonymous.
 *
 * The memory object and vregion are returned so the user can call fill and
 * pagefault on it to create actual mappings.
 */
errval_t vspace_map_anon_aligned(void **retaddr, struct memobj **ret_memobj,
                                 struct vregion **ret_vregion, size_t size,
                                 size_t *retsize, vregion_flags_t flags,
                                 size_t alignment)
{
    errval_t err;
    struct memobj_anon *memobj = NULL;
    struct vregion *vregion = NULL;
    
    // Allocate space
    memobj = malloc(sizeof(struct memobj_anon));
    assert(memobj != NULL);

    vregion = malloc(sizeof(struct vregion));
    assert(vregion != NULL);

    err = vspace_map_anon_nomalloc(retaddr, memobj, vregion, size,
                                   retsize, flags, alignment);
    
    if (err_is_fail(err)) {
        free(memobj);
        free(vregion);
    }
    
    *ret_memobj = (struct memobj *)memobj;
    *ret_vregion = vregion;
    
    return err;
}

/**
 * \brief Wrapper for creating and mapping a memory object of type anonymous.
 *
 * The memory object and vregion are returned so the user can call fill and
 * pagefault on it to create actual mappings.
 */
errval_t vspace_map_anon_attr(void **retaddr, struct memobj **ret_memobj,
                              struct vregion **ret_vregion, size_t size,
                              size_t *retsize, vregion_flags_t flags)
{
    errval_t err;
    struct memobj_anon *memobj = NULL;
    struct vregion *vregion = NULL;
    
    // Allocate space
    memobj = malloc(sizeof(struct memobj_anon));
    assert(memobj != NULL);

    vregion = malloc(sizeof(struct vregion));
    assert(vregion != NULL);

    err = vspace_map_anon_nomalloc(retaddr, memobj, vregion, size,
                                   retsize, flags, 0);
    
    if (err_is_fail(err))
    {
      free(memobj);
      free(vregion);
    }
    
    *ret_memobj = (struct memobj *)memobj;
    *ret_vregion = vregion;
    
    return err;
}

/**
 * \brief Wrapper to create and map an anonymous memory object at a fixed address.
 *
 * The memory object and vregion are returned so the user can call fill and
 * pagefault on it to create actual mappings.
 */
errval_t vspace_map_anon_fixed(genvaddr_t base, size_t size,
                               vregion_flags_t flags,
                               struct vregion **ret_vregion,
                               struct memobj **ret_memobj)
{
    errval_t err1, err2;
    struct memobj *memobj = NULL;
    struct vregion *vregion = NULL;

    // Allocate space
    memobj = malloc(sizeof(struct memobj_anon));
    if (!memobj) {
        err1 = LIB_ERR_MALLOC_FAIL;
        goto error;
    }
    vregion = malloc(sizeof(struct vregion));
    if (!vregion) {
        err1 = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    // Create a memobj and vregion
    err1 = memobj_create_anon((struct memobj_anon*)memobj, size, 0);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_MEMOBJ_CREATE_ANON);
        goto error;
    }
    err1 = vregion_map_fixed(vregion, get_current_vspace(), memobj, 0, size,
                             base, flags);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_VREGION_MAP);
        goto error;
    }

    *ret_vregion = vregion;
    *ret_memobj = memobj;

    return SYS_ERR_OK;

error:
    if (memobj) {
        err2 = memobj_destroy_anon(memobj);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "memobj_destroy_anon failed");
        }
        free(memobj);
    }
    if (vregion) {
        err2 = vregion_destroy(vregion);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "vregion_destroy failed");
        }
        free(vregion);
    }
    return err1;
}

/**
 * \brief Wrapper for creating and mapping a memory object of type one frame
 */
errval_t vspace_map_one_frame(void **retaddr, size_t size, struct capref frame,
                              struct memobj **retmemobj,
                              struct vregion **retvregion)
{
    return vspace_map_one_frame_attr(retaddr, size, frame,
                                     VREGION_FLAGS_READ_WRITE, retmemobj,
                                     retvregion);
}

errval_t vspace_map_one_frame_fixed(lvaddr_t addr, size_t size,
                                    struct capref frame,
                                    struct memobj **retmemobj,
                                    struct vregion **retvregion)
{
    return vspace_map_one_frame_fixed_attr(addr, size, frame,
                                           VREGION_FLAGS_READ_WRITE, retmemobj,
                                           retvregion);
}

errval_t vspace_map_one_frame_fixed_attr(lvaddr_t addr, size_t size,
                                    struct capref frame, vregion_flags_t flags,
                                    struct memobj **retmemobj,
                                    struct vregion **retvregion)
{
    errval_t err1, err2;
    struct memobj *memobj   = NULL;
    struct vregion *vregion = NULL;

    size = ROUND_UP(size, BASE_PAGE_SIZE);

    // Allocate space
    memobj = malloc(sizeof(struct memobj_one_frame));
    if (!memobj) {
        err1 = LIB_ERR_MALLOC_FAIL;
        goto error;
    }
    vregion = malloc(sizeof(struct vregion));
    if (!vregion) {
        err1 = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    // Create mappings
    err1 = memobj_create_one_frame((struct memobj_one_frame*)memobj, size, 0);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_MEMOBJ_CREATE_ONE_FRAME);
        goto error;
    }

    err1 = memobj->f.fill(memobj, 0, frame, size);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_MEMOBJ_FILL);
        goto error;
    }

    err1 = vregion_map_fixed(vregion, get_current_vspace(), memobj, 0, size, addr, flags);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_VREGION_MAP);
        goto error;
    }

    err1 = memobj->f.pagefault(memobj, vregion, 0, 0);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        goto error;
    }

    if (retmemobj) {
        *retmemobj = memobj;
    }
    if (retvregion) {
        *retvregion = vregion;
    }
    return SYS_ERR_OK;

 error:
    if (memobj) {
        err2 = memobj_destroy_one_frame(memobj);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "memobj_destroy_anon failed");
        }
    }
    if (vregion) {
        err2 = vregion_destroy(vregion);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "vregion_destroy failed");
        }
    }
    return err1;
}

/**
 * \brief Wrapper for creating and mapping a memory object
 * of type one frame with specific flags
 */
errval_t vspace_map_one_frame_attr(void **retaddr, size_t size,
                                   struct capref frame, vregion_flags_t flags,
                                   struct memobj **retmemobj,
                                   struct vregion **retvregion)
{
    errval_t err1, err2;
    struct memobj *memobj   = NULL;
    struct vregion *vregion = NULL;

    size = ROUND_UP(size, BASE_PAGE_SIZE);

    // Allocate space
    memobj = malloc(sizeof(struct memobj_one_frame));
    if (!memobj) {
        err1 = LIB_ERR_MALLOC_FAIL;
        goto error;
    }
    vregion = malloc(sizeof(struct vregion));
    if (!vregion) {
        err1 = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    // Create mappings
    err1 = memobj_create_one_frame((struct memobj_one_frame*)memobj, size, 0);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_MEMOBJ_CREATE_ONE_FRAME);
        goto error;
    }

    err1 = memobj->f.fill(memobj, 0, frame, size);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_MEMOBJ_FILL);
        goto error;
    }

    err1 = vregion_map(vregion, get_current_vspace(), memobj, 0, size, flags);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_VREGION_MAP);
        goto error;
    }

    err1 = memobj->f.pagefault(memobj, vregion, 0, 0);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        goto error;
    }

    *retaddr = (void*)vspace_genvaddr_to_lvaddr(vregion_get_base_addr(vregion));
    if (retmemobj) {
        *retmemobj = memobj;
    }
    if (retvregion) {
        *retvregion = vregion;
    }
    return SYS_ERR_OK;

 error:
    if (memobj) {
        err2 = memobj_destroy_one_frame(memobj);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "memobj_destroy_anon failed");
        }
    }
    if (vregion) {
        err2 = vregion_destroy(vregion);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "vregion_destroy failed");
        }
    }
    return err1;
}

errval_t vspace_map_one_frame_one_map(struct memobj_one_frame_one_map *memobj,
                                      struct vregion *vregion, size_t size,
                                      struct capref frame)
{
    errval_t err;

    err = memobj_create_one_frame_one_map(memobj, size, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_CREATE_ONE_FRAME_ONE_MAP);
    }
    err = memobj->m.f.fill(&memobj->m, 0, frame, size);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_FILL);
    }
    err = vregion_map(vregion, get_current_vspace(), &memobj->m, 0, size,
                      VREGION_FLAGS_READ_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VREGION_MAP);
    }
    err = memobj->m.f.pagefault(&memobj->m, vregion, 0, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
    }

    return SYS_ERR_OK;
}
