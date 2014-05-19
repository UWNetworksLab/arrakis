/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VFS_OPS_H
#define VFS_OPS_H

#include <vfs/vfs.h>

struct vfs_ops {
    // operations on files
    errval_t (*open)(void *st, const char *path, vfs_handle_t *handle);
    errval_t (*create)(void *st, const char *path, vfs_handle_t *handle);
    errval_t (*remove)(void *st, const char *path);

    // operations on file handles
    errval_t (*read)(void *st, vfs_handle_t handle, void *buffer, size_t bytes,
                     size_t *bytes_read);
    errval_t (*write)(void *st, vfs_handle_t handle, const void *buffer, size_t bytes,
                      size_t *bytes_written);
    errval_t (*truncate)(void *st, vfs_handle_t handle, size_t bytes);
    errval_t (*seek)(void *st, vfs_handle_t handle, enum vfs_seekpos whence,
                     off_t offset);
    errval_t (*tell)(void *st, vfs_handle_t handle, size_t *pos);
    errval_t (*stat)(void *st, vfs_handle_t handle, struct vfs_fileinfo *info);
    errval_t (*close)(void *st, vfs_handle_t handle);
    errval_t (*flush)(void *st, vfs_handle_t handle);

    // manipulation of directories
    errval_t (*mkdir)(void *st, const char *path); // fail if already present
    errval_t (*rmdir)(void *st, const char *path); // fail if not empty
    errval_t (*opendir)(void *st, const char *path, vfs_handle_t *dhandle);
    errval_t (*dir_read_next)(void *st, vfs_handle_t dhandle,
                              char **name, struct vfs_fileinfo *info);
    errval_t (*closedir)(void *st, vfs_handle_t dhandle);

#ifdef WITH_BUFFER_CACHE
    // Buffer cache operations
    errval_t (*get_bcache_key)(void *st, vfs_handle_t handle,
                               char **retkey, size_t *keylen, size_t *retoffset);

    // Block based operations
    errval_t (*read_block)(void *st, vfs_handle_t handle, void *buffer,
                           size_t *bytes_read);
    errval_t (*write_block)(void *st, vfs_handle_t handle, const void *buffer,
                            size_t bytes, size_t *bytes_written);
#endif
};

#endif
