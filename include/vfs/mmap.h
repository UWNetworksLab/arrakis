/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VFS_MMAP_H
#define VFS_MMAP_H

#include <sys/cdefs.h>

#include <barrelfish/memobj.h>
#include <vfs/vfs.h>

__BEGIN_DECLS

struct memobj_vfs {
    struct memobj_anon anon; // underlying anon memobj that manages the frames
    vfs_handle_t vh; // VFS handle for file
    off_t offset; // offset within file
    size_t filesize; // size to read from file (rest is zero-filled)
};

errval_t memobj_create_vfs(struct memobj_vfs *memobj, size_t size,
                           memobj_flags_t flags, vfs_handle_t vh, off_t offset,
                           size_t filesize);
errval_t memobj_destroy_vfs(struct memobj *memobj);
errval_t memobj_flush_vfs(struct memobj *memobj, struct vregion *vregion);

errval_t vspace_map_file(size_t size, vregion_flags_t flags,
                         vfs_handle_t file, off_t offset, size_t filesize,
                         struct vregion **ret_vregion,
                         struct memobj **ret_memobj);
errval_t vspace_map_file_fixed(genvaddr_t base, size_t size,
                               vregion_flags_t flags, vfs_handle_t file,
                               off_t offset, size_t filesize,
                               struct vregion **ret_vregion,
                               struct memobj **ret_memobj);
errval_t vspace_map_file_aligned(size_t alignment, size_t size,
                                 vregion_flags_t flags, vfs_handle_t file,
                                 off_t offset, size_t filesize,
                                 struct vregion **ret_vregion,
                                 struct memobj **ret_memobj);

__END_DECLS

#endif
