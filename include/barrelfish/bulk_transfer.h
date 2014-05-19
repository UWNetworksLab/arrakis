/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_TRANSFER_H
#define BULK_TRANSFER_H

#include <sys/cdefs.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/bulk_transfer_arch.h>

__BEGIN_DECLS

struct bulk_transfer;

struct bulk_buf {
    struct bulk_buf *next;
    uintptr_t offset;
    void *base;
    size_t size;
    struct bulk_transfer *pool;
};

struct bulk_transfer {
    struct bulk_buf *free_list, *alloc_list;
    size_t size, block_size;
    void *mem;
};

struct bulk_transfer_slave {
    size_t size;
    void *mem;
};

void bulk_destroy(struct bulk_transfer *bt);
errval_t bulk_init(void *mem, size_t size, size_t block_size,
                   struct bulk_transfer *bt);
errval_t bulk_create(size_t size, size_t block_size, struct capref *shared_mem,
                     struct bulk_transfer *bt, bool LMP_only);
struct bulk_buf *bulk_alloc(struct bulk_transfer *bt);
errval_t bulk_free(struct bulk_transfer *bt, uintptr_t id);

void bulk_buf_copy(struct bulk_buf *bb, void *buf, size_t buf_size);

static inline void *bulk_buf_get_mem(struct bulk_buf *buf)
{
    return buf->base;
}

/// get the ID without any prepare (useful if we're not touching the data)
static inline uintptr_t bulk_buf_get_id(struct bulk_buf *buf)
{
    return buf->offset;
}
/**
 * \brief Prepare a buffer for send.
 *
 * \param buf   Pointer to buffer
 *
 * \return ID for buffer
 */
static inline uintptr_t bulk_prepare_send(struct bulk_buf *buf)
{
    bulk_arch_prepare_send(bulk_buf_get_mem(buf), buf->size);
    return bulk_buf_get_id(buf);
}

/**
 * \brief Prepare a buffer for receive.
 *
 * \param buf   Pointer to buffer
 */
static inline void bulk_prepare_recv(struct bulk_buf *buf)
{
    bulk_arch_prepare_recv(bulk_buf_get_mem(buf), buf->size);
}

static inline void *bulk_slave_buf_get_mem(struct bulk_transfer_slave *bt,
                                           uintptr_t id, size_t *maxlen)
{
    if (id > bt->size) {
        if (maxlen != NULL) {
            *maxlen = 0;
        }
        return NULL;
    } else {
        if (maxlen != NULL) {
            *maxlen = bt->size - id;
        }
        return (char *)bt->mem + id;
    }
}

static inline errval_t bulk_slave_init(void *mem, size_t size,
                                       struct bulk_transfer_slave *bt)
{
    bt->mem = mem;
    bt->size = size;
    return SYS_ERR_OK;
}

static inline void bulk_slave_prepare_send(struct bulk_transfer_slave *bt,
                                           uintptr_t id)
{
    size_t len;
    void *mem = bulk_slave_buf_get_mem(bt, id, &len);
    assert(mem != NULL);
    bulk_arch_prepare_send(mem, len);
}

static inline void bulk_slave_prepare_recv(struct bulk_transfer_slave *bt,
                                           uintptr_t id)
{
    size_t len;
    void *mem = bulk_slave_buf_get_mem(bt, id, &len);
    assert(mem != NULL);
    bulk_arch_prepare_recv(mem, len);
}

__END_DECLS

#endif
