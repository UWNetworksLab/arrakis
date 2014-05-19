/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN // for strdup()
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>

#include "vfs_ops.h"
#include "vfs_backends.h"

struct vfs_mount {
    const char *mountpoint;
    struct vfs_ops *ops;
    void *st;
    struct vfs_mount *next;
};

static struct vfs_mount *mounts;

static bool mount_matches(const char *mount, const char *path, size_t *matchlen)
{
    size_t len = 0;

    // both inputs must be absolute paths
    assert(mount != NULL && mount[0] == VFS_PATH_SEP);
    assert(path != NULL && path[0] == VFS_PATH_SEP);

    while (mount[len] != '\0' && mount[len] == path[len]) {
        len++;
    }

    if (mount[len] == '\0' &&
        (len == 1 /*root*/ || path[len] == '\0' || path[len] == VFS_PATH_SEP)) {
        assert(matchlen != NULL);
        *matchlen = len;
        return true;
    } else {
        return false;
    }
}

/// find the closest matching mountpoint for a given path
/// return mount, and pointer (within input string) to relative path
static struct vfs_mount *find_mount(const char *path, const char **ret_relpath)
{
    struct vfs_mount *match = NULL;
    size_t len, matchlen = 0;

    // path must be absolute
    if (path == NULL || path[0] != VFS_PATH_SEP) {
        return NULL;
    }

    for (struct vfs_mount *m = mounts; m != NULL; m = m->next) {
        if (mount_matches(m->mountpoint, path, &len)
            && (match == NULL || len > matchlen)) {
            match = m;
            matchlen = len;
        }
    }

    if (match != NULL && ret_relpath != NULL) {
        *ret_relpath = &path[matchlen];
    }

    return match;
}

/**
 * \brief Mount a filesystem into the local VFS
 *
 * \param mountpoint Fully-qualified absolute path to the mount-point
 *   Must be a directory in the existing VFS which is not already a mount-point
 *
 * \param uri URI of source file system to mount.
 *   Currently-supported are:
 *     ramfs://[servicename] where servicename is registered in the name service
 *     nfs://hostip/path
 */
errval_t vfs_mount(const char *mountpoint, const char *uri)
{
    errval_t err;

    // copy mountpoint and normalise it
    assert(mountpoint != NULL);
    char *mp = strdup(mountpoint);
    assert(mp != NULL);
    vfs_path_normalise(mp);

    // sanity-check mountpoint: must start at root and not have .. in it
    if (mp[0] != VFS_PATH_SEP || strncmp(mp, "/../", 4) == 0) {
        free(mp);
        return VFS_ERR_BAD_MOUNTPOINT;
    }

    // sanity-check mountpoint
    // if this is the first mount, it must be for the root, otherwise it must
    // not duplicate an existing mount, and the mount-point must exist
    if (mounts == NULL) {
        if (strcmp(mp, VFS_PATH_SEP_STR) != 0) {
            free(mp);
            return VFS_ERR_BAD_MOUNTPOINT;
        }
    } else {
        struct vfs_mount *parent = find_mount(mp, NULL);
        assert(parent != NULL); // root should always have matched
        if (strcmp(parent->mountpoint, mp) == 0) {
            free(mp);
            return VFS_ERR_MOUNTPOINT_IN_USE;
        }

        // check for existence of mountpoint by attempting to open it
        vfs_handle_t tmp;
        err = vfs_opendir(mp, &tmp);
        if (err_is_fail(err)) {
            free(mp);
            return err_push(err, VFS_ERR_MOUNTPOINT_NOTFOUND);
        }
        vfs_closedir(tmp);
    }

    // parse protocol part of URI
    char *pos = strstr(uri, "://");
    if (pos == NULL) {
        free(mp);
        return VFS_ERR_BAD_URI;
    }

    struct vfs_mount *m = malloc(sizeof(struct vfs_mount));
    assert(m != NULL);

    m->mountpoint = mp;

    size_t len = pos - uri;
    if (strncmp(uri, "nfs", len) == 0) {
#ifndef DISABLE_NFS
        err = vfs_nfs_mount(uri, &m->st, &m->ops);
#else
        err = VFS_ERR_UNKNOWN_FILESYSTEM;
#endif
    } else if (strncmp(uri, "ramfs", len) == 0) {
        err = vfs_ramfs_mount(uri, &m->st, &m->ops);
    } else if (strncmp(uri, "blockdevfs", len) == 0) {
        err = vfs_blockdevfs_mount(uri, &m->st, &m->ops);
    } else if (strncmp(uri, "fat16", len) == 0) {
        err = vfs_fat_mount(uri, &m->st, &m->ops);
    } else if (strncmp(uri, "fat32", len) == 0) {
        err = vfs_fat_mount(uri, &m->st, &m->ops);
    } else {
        debug_printf("VFS: unknown file system %.*s\n", (int)len, uri);
        err = VFS_ERR_UNKNOWN_FILESYSTEM;
    }

    if (err_is_fail(err)) {
        free(m);
        free(mp);
        return err;
    }

    // add to list of mounts
    m->next = mounts;
    mounts = m;

    return SYS_ERR_OK;
}

/**
 * \brief Unmount a filesystem from the local VFS
 *
 * \param mountpoint Fully-qualified absolute path to the existing mount-point
 */
errval_t vfs_unmount(const char *mointpoint)
{
    // TODO: ensure there are no live handles (ie. need refcount on open/close)
    USER_PANIC("vfs_unmount NYI");
}

/**
 * \brief Open the given file, failing if it doesn't exist
 *
 * \param path Fully-qualified absolute path
 * \param handle Return handle, if call succeeds
 */
errval_t vfs_open(const char *path, vfs_handle_t *handle)
{
    const char *relpath = NULL;

    // locate mount point
    struct vfs_mount *m = find_mount(path, &relpath);
    if (m == NULL) {
        return FS_ERR_NOTFOUND;
    }

    // call fs ops func
    assert(m->ops->open != NULL);
    errval_t ret = m->ops->open(m->st, relpath, handle);

    // update handle with mount pointer
    if (err_is_ok(ret)) {
        struct vfs_handle *h = *handle;
        h->mount = m;
    }

    return ret;
}

/**
 * \brief Open the given file, creating it if it doesn't already exist
 *
 * \param path Fully-qualified absolute path
 * \param handle Return handle, if call succeeds
 */
errval_t vfs_create(const char *path, vfs_handle_t *handle)
{
    const char *relpath = NULL;

    // locate mount point
    struct vfs_mount *m = find_mount(path, &relpath);
    if (m == NULL) {
        return FS_ERR_NOTFOUND;
    }

    // call fs ops func
    assert(m->ops != NULL);
    assert(m->ops->create != NULL);
    errval_t ret = m->ops->create(m->st, relpath, handle);

    // update handle with mount pointer
    if (err_is_ok(ret)) {
        struct vfs_handle *h = *handle;
        h->mount = m;
    }

    return ret;
}

/**
 * \brief Remove the given file, fail if not present
 *
 * \param path Fully-qualified absolute path
 */
 errval_t vfs_remove(const char *path)
{
    const char *relpath = NULL;

    // locate mount point
    struct vfs_mount *m = find_mount(path, &relpath);
    if (m == NULL) {
        return FS_ERR_NOTFOUND;
    }

    // call fs ops func
    assert(m->ops->remove != NULL);
    return m->ops->remove(m->st, relpath);
}

/**
 * \brief Read from an open file handle
 *
 * \param handle Handle to an open file, returned from #vfs_open or #vfs_create
 * \param buffer Pointer to buffer of at least #bytes where read data is placed
 * \param bytes Maximum number of bytes to read
 * \param bytes_read Return pointer containing number of bytes actually read
 */
errval_t vfs_read(vfs_handle_t handle, void *buffer, size_t bytes,
                  size_t *bytes_read)
{
    struct vfs_handle *h = handle;
    struct vfs_mount *m = h->mount;

    assert(m->ops->read != NULL);
    return m->ops->read(m->st, handle, buffer, bytes, bytes_read);
}

/**
 * \brief Write to an open file handle
 *
 * \param handle Handle to an open file, returned from #vfs_open or #vfs_create
 * \param buffer Pointer to buffer of #bytes containing data to write
 * \param bytes Maximum number of bytes to write
 * \param bytes_written Return pointer containing number of bytes actually written
 */
errval_t vfs_write(vfs_handle_t handle, const void *buffer, size_t bytes,
                   size_t *bytes_written)
{
    struct vfs_handle *h = handle;
    struct vfs_mount *m = h->mount;
    assert(m->ops->write != NULL);
    return m->ops->write(m->st, handle, buffer, bytes, bytes_written);
}

/**
 * \brief Truncate an open file
 *
 * \param handle Handle to an open file, returned from #vfs_open or #vfs_create
 * \param bytes New size of file
 *
 * If bytes is greater than the existing file size, the file is enlarged with
 * zero bytes.
 */
errval_t vfs_truncate(vfs_handle_t handle, size_t bytes)
{
    struct vfs_handle *h = handle;
    struct vfs_mount *m = h->mount;

    assert(m->ops->truncate != NULL);
    return m->ops->truncate(m->st, handle, bytes);
}

/**
 * \brief Seek to a new position in an open file
 *
 * \param handle Handle to an open file, returned from #vfs_open or #vfs_create
 * \param whence Determines interpretation of offset
 * \param offset Offset in bytes from position specified by #whence
 *
 * See documentation of the enum #vfs_seekpos for the possible values of #whence.
 */
errval_t vfs_seek(vfs_handle_t handle, enum vfs_seekpos whence, off_t offset)
{
    struct vfs_handle *h = handle;
    struct vfs_mount *m = h->mount;

    assert(m->ops->seek != NULL);
    return m->ops->seek(m->st, handle, whence, offset);
}

/**
 * \brief Return the current file pointer of an open file
 *
 * \param handle Handle to an open file, returned from #vfs_open or #vfs_create
 * \param pos Return pointer of current position in file
 */
errval_t vfs_tell(vfs_handle_t handle, size_t *pos)
{
    struct vfs_handle *h = handle;
    struct vfs_mount *m = h->mount;

    assert(m->ops->tell != NULL);
    return m->ops->tell(m->st, handle, pos);
}

/**
 * \brief Return the metadata properties of an open file
 *
 * \param handle Handle to an open file, returned from #vfs_open or #vfs_create
 * \param info Pointer to #vfs_fileinfo structure that will be filled in
 */
errval_t vfs_stat(vfs_handle_t handle, struct vfs_fileinfo *info)
{
    struct vfs_handle *h = handle;
    struct vfs_mount *m = h->mount;

    assert(m->ops->stat != NULL);
    return m->ops->stat(m->st, handle, info);
}

/**
 * \brief Flush file to disk
 * \param handle Handle to an open file
 */
errval_t vfs_flush(vfs_handle_t handle)
{
    struct vfs_handle *h = handle;
    struct vfs_mount *m = h->mount;
    if (m->ops->flush) {
        return m->ops->flush(m->st, handle);
    }
    else {
        return VFS_ERR_NOT_SUPPORTED;
    }
}

/**
 * \brief Close an open file, freeing any associated local state
 *
 * \param handle Handle to an open file, returned from #vfs_open or #vfs_create
 */
errval_t vfs_close(vfs_handle_t handle)
{
    struct vfs_handle *h = handle;
    struct vfs_mount *m = h->mount;

    assert(m->ops->close != NULL);
    return m->ops->close(m->st, handle);
}

/**
 * \brief Open the given directory
 *
 * \param path Fully-qualified absolute path to directory
 * \param dhandle Return handle, if call succeeds
 *
 * This call fails if the path does not exist or is not a directory.
 */
errval_t vfs_opendir(const char *path, vfs_handle_t *dhandle)
{
    const char *relpath = NULL;

    // locate mount point
    struct vfs_mount *m = find_mount(path, &relpath);
    if (m == NULL) {
        return FS_ERR_NOTFOUND;
    }

    // call fs ops func
    assert(m->ops->opendir != NULL);
    errval_t ret = m->ops->opendir(m->st, relpath, dhandle);

    // update handle with mount pointer
    if (err_is_ok(ret)) {
        struct vfs_handle *h = *dhandle;
        h->mount = m;
    }

    return ret;
}

/**
 * \brief Return information about the next entry in the given directory
 *
 * \param dhandle Directory handle returned from #vfs_opendir
 * \param name Return pointer to string, filled-in if non-NULL with a pointer to
 *      a malloced buffer containing the name of the next entry in the directory.
 *      This buffer must be freed by the caller.
 * \param info Optional pointer to #vfs_fileinfo structure that will be filled
 *      in (if non-NULL) with metadata for the given entry.
 *
 * This call fails if the previous entry returned was the last, or if the
 * directory is empty. To return the contents again, the directory must be
 * closed and re-opened (ie. it is not possible to seek in the directory).
 */
errval_t vfs_dir_read_next(vfs_handle_t dhandle, char **name,
                           struct vfs_fileinfo *info)
{
    struct vfs_handle *handle = dhandle;
    struct vfs_mount *m = handle->mount;

    assert(m->ops->dir_read_next != NULL);
    return m->ops->dir_read_next(m->st, dhandle, name, info);
}

/**
 * \brief Close a directory handle obtained from #vfs_opendir
 *
 * \param dhandle Directory handle returned from #vfs_opendir
 */
errval_t vfs_closedir(vfs_handle_t dhandle)
{
    struct vfs_handle *handle = dhandle;
    struct vfs_mount *m = handle->mount;

    assert(m->ops->closedir != NULL);
    return m->ops->closedir(m->st, dhandle);
}

/**
 * \brief Create a new empty directory
 *
 * \param path Fully-qualified absolute path to the new directory
 *
 * This call fails if the parent directory does not exist, or if the given
 * directory already exists (as either a file or directory).
 */
errval_t vfs_mkdir(const char *path)
{
    const char *relpath = NULL;

    // locate mount point
    struct vfs_mount *m = find_mount(path, &relpath);
    if (m == NULL) {
        return FS_ERR_NOTFOUND;
    }

    // call fs ops func
    assert(m->ops->mkdir != NULL);
    return m->ops->mkdir(m->st, relpath);
}

/**
 * \brief Remove an existing empty directory
 *
 * \param path Fully-qualified absolute path to the directory
 *
 * This call fails if the given directory already exists and is not a directory,
 * or is a directory but is not empty.
 */
errval_t vfs_rmdir(const char *path)
{
    const char *relpath = NULL;

    // locate mount point
    struct vfs_mount *m = find_mount(path, &relpath);
    if (m == NULL) {
        return FS_ERR_NOTFOUND;
    }

    // check if this is the mountpoint itself
    if (*relpath == '\0') {
        return VFS_ERR_MOUNTPOINT_IN_USE;
    }

    // call fs ops func
    assert(m->ops->rmdir != NULL);
    return m->ops->rmdir(m->st, relpath);
}

/**
 * \brief Initialise the VFS library
 *
 * This call initialises the VFS library. It must be called prior to any
 * other VFS functions being used. It doesn't need to be a constructor
 * We call it explicitly..
 */
void vfs_init(void)
{
    assert(mounts == NULL);
    errval_t err;

    // init libc glue
    vfs_fopen_init();

    // mount ramfs on root, as a sensible default setup for the time being
    err = vfs_mount("/", "ramfs://");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error mounting ramfs");
        // continue anyway...
    }
}



void vfs_dummy(void);

__attribute__((used))
void vfs_dummy(void) {
    //    debug_printf("vfs_dummy\n");
}
