/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <string.h>
#include <barrelfish/bulk_transfer.h>
#ifdef __scc__
#       include <barrelfish_kpi/shared_mem_arch.h>
#endif

/**
 * \brief Destroy bulk data transport on shared memory region
 *
 * \param bt    Pointer to bulk transfer state to be destroyed
 */
void bulk_destroy(struct bulk_transfer *bt)
{
    struct bulk_buf *tmp;

    // Free free list
    while(bt->free_list != NULL){
        tmp = bt->free_list->next;
        free(bt->free_list);
        bt->free_list = tmp;
    }

    // Free alloc list
    while(bt->alloc_list != NULL){
        tmp = bt->alloc_list->next;
        free(bt->alloc_list);
        bt->free_list = tmp;
    }
}

/**
 * \brief Initialize bulk data transport on shared memory region
 *
 * \param mem   Pointer to shared memory region
 * \param size  Size in bytes of region
 * \param block_size    Size in bytes of a buffer block
 * \param bt    Pointer to bulk transfer state to be filled
 *
 * \return Error value.
 */
errval_t bulk_init(void *mem, size_t size, size_t block_size,
                   struct bulk_transfer *bt)
{
    bt->free_list = (struct bulk_buf *)malloc(sizeof(struct bulk_buf));
    assert(bt->free_list != NULL);
    size /= block_size;
    bt->size = size;
    bt->mem = mem;

    memset(mem, 0, size); /* XXX: shouldn't this be the original size? */

    /* all, but the last buffer  */
    struct bulk_buf *current_pbuf = bt->free_list;
    for (int i = 0; i < size - 1; i++) {
        current_pbuf->offset = i * block_size;
        current_pbuf->pool = bt;
        current_pbuf->base = (char *)mem + current_pbuf->offset;
        current_pbuf->size = block_size;

        struct bulk_buf *tmp =
            (struct bulk_buf *)malloc(sizeof(struct bulk_buf));
        assert(tmp != NULL);

        current_pbuf->next = tmp;
        current_pbuf = tmp;
    }

    /* do the last buffer */
    current_pbuf->offset = (size - 1) * block_size;
    current_pbuf->size = block_size;
    current_pbuf->pool = bt;
    current_pbuf->base = (char *)mem + current_pbuf->offset;
    current_pbuf->next = NULL;

    return SYS_ERR_OK;
}

/**
 * \brief Create bulk data transport on allocated shared memory region
 *
 * \param size  Size in bytes of shared memory region to allocate
 * \param block_size    Size in bytes of a buffer block
 * \param shared_mem    Return parameter to allocated shared memory capability
 * \param bt    Pointer to bulk transfer state to be filled
 * \param LMP_only      States that this bulk transfer will be used on in LMP (
 *                      Only applicable to SCC )
 *
 * \return Error value.
 */

errval_t bulk_create(size_t size, size_t block_size, struct capref *shared_mem,
                     struct bulk_transfer *bt, bool LMP_only)
{
    // Create a Frame Capability 
    size_t allocated_size;
#ifdef __scc__
    if (LMP_only) {
        ram_set_affinity(0x0, PRIVATE_MEM_MAX);
    } else {

        ram_set_affinity(SHARED_MEM_MIN + (PERCORE_MEM_SIZE * disp_get_core_id()),
                     SHARED_MEM_MIN + (PERCORE_MEM_SIZE * (disp_get_core_id() + 1)));
/*        printf("^^^^ bulk transfer affinities %"PRIx32", %"PRIx32" \n",
            SHARED_MEM_MIN + (PERCORE_MEM_SIZE * disp_get_core_id()),
            SHARED_MEM_MIN + (PERCORE_MEM_SIZE * (disp_get_core_id() + 1)));
*/
    }

#endif

    errval_t r = frame_alloc(shared_mem, size, &allocated_size);
#ifdef __scc__
    ram_set_affinity(0, 0);
#endif
    if (err_is_fail(r)) {
        return err_push(r, LIB_ERR_FRAME_ALLOC);
    }
    assert(allocated_size >= size);

    // Map the frame in local memory
    void *pool;
#ifdef __scc__
    r = vspace_map_one_frame_attr(&pool, allocated_size, *shared_mem,
                                  VREGION_FLAGS_READ_WRITE_MPB, NULL, NULL);
#else
    r = vspace_map_one_frame(&pool, allocated_size, *shared_mem, NULL, NULL);
#endif
    if (err_is_fail(r)) {
        cap_destroy(*shared_mem);
        return err_push(r, LIB_ERR_VSPACE_MAP);
    }
    assert(pool != NULL);

    return bulk_init(pool, size, block_size, bt);
}

/**
 * \brief Allocate a buffer from a bulk transfer region.
 *
 * \param bt    Pointer to state
 *
 * \return Pointer to allocated bulk buffer or NULL on out of memory
 */
struct bulk_buf *bulk_alloc(struct bulk_transfer *bt)
{
    struct bulk_buf *head = bt->free_list;

    if (head != NULL) {
        bt->free_list = bt->free_list->next;
        head->next = bt->alloc_list;
        bt->alloc_list = head;
        return head;
    } else {
        return NULL;
    }
}

/**
 * \brief copy data from a buffer to a bulk buffer
 *
 * \param bb        bulk buffer to copy the data
 * \param buf       (source) buffer
 * \param buf_size  size of (source) buffer:
 */
void bulk_buf_copy(struct bulk_buf *bb, void *buf, size_t buf_size)
{
    void *dst;
    assert(buf_size <= bb->size);
    dst = bulk_buf_get_mem(bb);
    memcpy(dst, buf, buf_size);
}

/**
 * \brief Frees a buffer to its bulk transfer region.
 *
 * \param buf    Pointer to buffer state
 * \param id    ID of buffer to be freed
 */
errval_t bulk_free(struct bulk_transfer *bt, uintptr_t id)
{
    // Remove from alloc list
    for(struct bulk_buf *i = bt->alloc_list, *prev = NULL;
        i != NULL; prev = i, i = i->next) {
        if(i->offset == id) {
            if(prev == NULL) {
                // Remove from beginning
                bt->alloc_list = i->next;
            } else {
                // Remove in the middle / at the end
                prev->next = i->next;
            }

            // Add to free list
            i->next = bt->free_list;
            bt->free_list = i;
            return SYS_ERR_OK;
        }
    }

    return LIB_ERR_BULK_UNKNOWN_ID;
}
