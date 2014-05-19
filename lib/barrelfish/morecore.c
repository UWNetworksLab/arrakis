/**
 * \file
 * \brief Morecore implementation for malloc
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/core_state.h>
#include <barrelfish/morecore.h>
#include <stdio.h>

/// Amount of virtual space for malloc
#ifdef __x86_64__
#       define HEAP_REGION (2UL * 1024 * 1024 * 1024) /* 2GB */
#else
#       define HEAP_REGION (512UL * 1024 * 1024) /* 512MB */
#endif

typedef void *(*morecore_alloc_func_t)(size_t bytes, size_t *retbytes);
extern morecore_alloc_func_t sys_morecore_alloc;

typedef void (*morecore_free_func_t)(void *base, size_t bytes);
extern morecore_free_func_t sys_morecore_free;

/**
 * \brief Allocate some memory for malloc to use
 *
 * This function will keep trying with smaller and smaller frames till
 * it finds a set of frames that satisfy the requirement. retbytes can
 * be smaller than bytes if we were able to allocate a smaller memory
 * region than requested for.
 */
static void *morecore_alloc(size_t bytes, size_t *retbytes)
{
    errval_t err;
    struct morecore_state *state = get_morecore_state();

    void *buf = NULL;
    size_t mapped = 0;
    size_t step = bytes;
    while (mapped < bytes) {
        struct capref cap;
        err = slot_alloc(&cap);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "slot_alloc failed");
        }

        void *mid_buf = NULL;
        err = vspace_mmu_aware_map(&state->mmu_state, cap, step,
                                   &mid_buf, &step);
        if (err_is_ok(err)) {
            if (buf == NULL) {
                buf = mid_buf;
            }
            mapped += step;
        } else {
            /*
              vspace_mmu_aware_map failed probably because we asked
              for a very large frame, will try asking for smaller one.
             */
            if (err_no(err) == LIB_ERR_FRAME_CREATE_MS_CONSTRAINTS) {
                err = slot_free(cap);
                if (err_is_fail(err)) {
                    debug_err(__FILE__, __func__, __LINE__, err,
                              "slot_free failed");
                    return NULL;
                }
                if (step < BASE_PAGE_SIZE) {
                    // Return whatever we have allocated until now
                    break;
                }
                step /= 2;
                continue;
            } else {
                debug_err(__FILE__, __func__, __LINE__, err,
                          "vspace_mmu_aware_map fail");
                return NULL;
            }
        }
    }

    *retbytes = mapped;
    return buf;
}

static void morecore_free(void *base, size_t bytes)
{
    struct morecore_state *state = get_morecore_state();
    errval_t err = vspace_mmu_aware_unmap(&state->mmu_state,
                                          (lvaddr_t)base, bytes);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace_mmu_aware_unmap");
    }
}

Header *get_malloc_freep(void);
Header *get_malloc_freep(void)
{
    return get_morecore_state()->header_freep;
}

errval_t morecore_init(void)
{
    errval_t err;
    struct morecore_state *state = get_morecore_state();

    thread_mutex_init(&state->mutex);

    err = vspace_mmu_aware_init(&state->mmu_state, HEAP_REGION);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MMU_AWARE_INIT);
    }

    sys_morecore_alloc = morecore_alloc;
    sys_morecore_free = morecore_free;

    return SYS_ERR_OK;
}
