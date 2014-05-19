/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VFS_H
#define VFS_H

#include <errors/errno.h> // for errval_t
#include <stddef.h>
#include <sys/cdefs.h>
#include <sys/types.h>

typedef void *vfs_handle_t;
#define NULL_VFS_HANDLE NULL

/* XXX: remove this for partitioned cache */

#ifdef WITH_SHARED_CACHE
static const char vfs_cache_str[] = "single-shared";
#else
static const char vfs_cache_str[] = "partitioned";
#endif

//#define WITH_BUFFER_CACHE
//#define WITH_WRITE_BACK_CACHE
//#define WITH_META_DATA_CACHE

#ifdef WITH_BUFFER_CACHE
#define BUFFER_CACHE_BLOCK_SIZE      (1U << 12)      // 4KB
#endif

/// Enum defining interpretation of offset argument to #vfs_seek
enum vfs_seekpos {
    VFS_SEEK_SET,   ///< Offset relative to start of file
    VFS_SEEK_CUR,   ///< Offset relative to current position
    VFS_SEEK_END,   ///< Offset relative to end of file
};

enum vfs_filetype {
    VFS_FILE,       ///< Regular file
    VFS_DIRECTORY,  ///< Directory
};

/// Data returned from #vfs_stat
struct vfs_fileinfo {
    enum vfs_filetype type; ///< Type of the object
    size_t size;            ///< Size of the object (in bytes, for a regular file)
};

__BEGIN_DECLS

// initialization
void vfs_init(void);

// operations on files
errval_t vfs_open(const char *path, vfs_handle_t *handle); // fail if it doesn't exist
errval_t vfs_create(const char *path, vfs_handle_t *handle); // ok if already present
errval_t vfs_remove(const char *path);

// operations on file handles
errval_t vfs_read(vfs_handle_t handle, void *buffer, size_t bytes, size_t *bytes_read);
errval_t vfs_write(vfs_handle_t handle, const void *buffer, size_t bytes, size_t *bytes_written);
errval_t vfs_truncate(vfs_handle_t xhandle, size_t bytes);
errval_t vfs_seek(vfs_handle_t handle, enum vfs_seekpos whence, off_t offset);
errval_t vfs_tell(vfs_handle_t handle, size_t *pos);
errval_t vfs_stat(vfs_handle_t handle, struct vfs_fileinfo *info);
errval_t vfs_close(vfs_handle_t handle);
errval_t vfs_flush(vfs_handle_t handle);

// manipulation of directories
errval_t vfs_mkdir(const char *path); // fail if already present
errval_t vfs_rmdir(const char *path); // fail if not empty
errval_t vfs_opendir(const char *path, vfs_handle_t *dhandle);
errval_t vfs_dir_read_next(vfs_handle_t dhandle, char **name /* malloced, optional */,
                           struct vfs_fileinfo *info /* optional */);
errval_t vfs_closedir(vfs_handle_t dhandle);

// manipulation of VFS mounts
errval_t vfs_mount(const char *mountpoint, const char *uri);
errval_t vfs_unmount(const char *mountpoint);

__END_DECLS

#endif
