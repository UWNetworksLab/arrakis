/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN // for strdup()
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <storage/vsic.h>
#include <storage/vsa.h>

#include "vfs_blockdevfs.h"

struct megaraid_handle {
    struct storage_vsic vsic;
    struct storage_vsa vsa;
};

errval_t blockdevfs_megaraid_open(void *handle)
{
    // No-Op
    return SYS_ERR_OK;
}

errval_t blockdevfs_megaraid_close(void *handle)
{
    // No-Op
    return SYS_ERR_OK;
}

errval_t blockdevfs_megaraid_flush(void *handle)
{
    struct megaraid_handle *h = handle;

    VFS_BLK_DEBUG("blockdevfs_megaraid_flush() called\n");

    errval_t err = h->vsic.ops.flush(&h->vsic, &h->vsa);
    assert(err_is_ok(err));
    err = h->vsic.ops.wait(&h->vsic);
    assert(err_is_ok(err));
    return SYS_ERR_OK;
}

errval_t blockdevfs_megaraid_read(void *handle, size_t pos, void *buffer,
                                  size_t bytes, size_t *bytes_read)
{
    VFS_BLK_DEBUG("blockdevfs_megaraid_read(%p, %zu, %p, %zu) called\n",
		  handle, pos, buffer, bytes);
    assert(!"NYI");
    return SYS_ERR_OK;
}

errval_t blockdevfs_megaraid_write(void *handle, size_t pos, const void *buffer,
                                   size_t bytes, size_t *bytes_written)
{
    struct megaraid_handle *h = handle;

    VFS_BLK_DEBUG("blockdevfs_megaraid_write(%p, %zu, %p, %zu) called\n",
		  handle, pos, buffer, bytes);

    size_t mycount = STORAGE_VSIC_ROUND(&h->vsic, bytes);
    size_t mypos = 0;
    /* printf("write on my fd called, count = %zu, mycount = %zu\n", count, mycount); */
    errval_t err = h->vsic.ops.write(&h->vsic, &h->vsa, mypos, mycount, (void *)buffer);
    assert(err_is_ok(err));

    *bytes_written = bytes;
    return SYS_ERR_OK;
}

errval_t blockdevfs_megaraid_init(void)
{
    struct megaraid_handle *handle = calloc(1, sizeof(struct megaraid_handle));
    assert(handle != NULL);
    errval_t err = storage_vsic_driver_init(0, NULL, &handle->vsic);
    if(err_no(err) == LIB_ERR_NOT_IMPLEMENTED) {
        return err;
    }
    assert(err_is_ok(err));
    err = storage_vsa_acquire(&handle->vsa, "0", 1 * 1024 * 1024);
    assert(err_is_ok(err));

    struct blockdev_entry *newentry = calloc(1, sizeof(struct blockdev_entry));
    assert(newentry != NULL);
    newentry->open = false;
    newentry->size = 1 * 1024 * 1024;
    newentry->type = blockdev_backend_type_megaraid;
    newentry->path = strdup("megaraid1");
    newentry->backend_handle = handle;

    // append to list
    blockdev_append_entry(newentry);

    return SYS_ERR_OK;
}
