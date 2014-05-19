/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VFS_BACKENDS_H
#define VFS_BACKENDS_H

#include "vfs_ops.h"

struct vfs_mount;

struct vfs_handle {
    struct vfs_mount *mount;
};

errval_t vfs_nfs_mount(const char *uri, void **retst, struct vfs_ops **retops);
errval_t vfs_ramfs_mount(const char *uri, void **retst, struct vfs_ops **retops);
errval_t vfs_blockdevfs_mount(const char *uri, void **retst, struct vfs_ops **retops);
errval_t vfs_fat_mount(const char *uri, void **retst, struct vfs_ops **retops);

void vfs_fopen_init(void);

errval_t buffer_cache_enable(void **st, struct vfs_ops **ops);

#endif
