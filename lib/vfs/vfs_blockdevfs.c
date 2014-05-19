/*
 * Copyright (c) 2009, 2011, ETH Zurich.
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
#include <barrelfish/nameservice_client.h>
#include <barrelfish/bulk_transfer.h>
#include <vfs/vfs_path.h>
#ifdef __scc__
#       include <barrelfish_kpi/shared_mem_arch.h>
#endif

#include "vfs_backends.h"
#include "vfs_blockdevfs.h"

/*
 * This is a simple filesystem that has virtual files for any blockdevice
 * style devices
 *
 * Assumptions:
 * - devices are fixed in size
 * - no concurrent accesses. blockdevfs will only hand out one handle to every
 *   device
 *
 * Currently implemented:
 * - SATA disks over AHCI (/<mountpoint>/ahciX)
 */

static struct blockdev_entry *entries_first = NULL;
static struct blockdev_entry *entries_last = NULL;

void blockdev_append_entry(struct blockdev_entry *entry)
{
    entry->next = NULL; // this is the last element in any case
    if (entries_last == NULL) {
        entries_first = entry;
        entries_last = entry;
        entry->prev = NULL;
    } else {
        entries_last->next = entry;
        entry->prev = entries_last;
        entries_last = entry;
    }
}

struct blockdevfs_handle {
    struct vfs_handle common;
    struct blockdev_entry *entry;
    size_t pos;
};

struct blockdevfs_dirhandle {
    struct vfs_handle common;
    struct blockdev_entry *entry;
};

struct backend_ops {
    errval_t (*open)(void *handle);
    errval_t (*close)(void *handle);
    errval_t (*read)(void *handle, size_t pos, void *buffer, size_t bytes,
                     size_t *bytes_read);
    errval_t (*write)(void *handle, size_t pos, const void *buffer, size_t bytes,
                      size_t *bytes_written);
    errval_t (*flush)(void *handle);
};



struct backend_ops backends[3] = {
    {
        .open = blockdevfs_ahci_open,
        .close = blockdevfs_ahci_close,
        .read = blockdevfs_ahci_read,
        .write = blockdevfs_ahci_write,
        .flush = blockdevfs_ahci_flush
    },
    {
        .open = blockdevfs_ata_open,
        .close = blockdevfs_ata_close,
        .read = blockdevfs_ata_read,
        .write = blockdevfs_ata_write,
        .flush = blockdevfs_ata_flush
    },
    {
        .open = blockdevfs_megaraid_open,
        .close = blockdevfs_megaraid_close,
        .read = blockdevfs_megaraid_read,
        .write = blockdevfs_megaraid_write,
        .flush = blockdevfs_megaraid_flush,
    },
};


static errval_t open(void *st, const char *path, vfs_handle_t *rethandle)
{
    VFS_BLK_DEBUG("blockdevfs_open: entering\n");
    errval_t err;

    char *path_ = (char *)path;
    if (path[0] == VFS_PATH_SEP) {
        path_++; // skip leading seperator
    }

    // search for entry
    struct blockdev_entry *cur = entries_first;
    for (; cur != NULL; cur = cur->next) {
        VFS_BLK_DEBUG("cur->path = %s\n", cur->path);
        if (!strcmp(path_, cur->path)) {
            break;
        }
    }

    if (cur == NULL) {
        return FS_ERR_NOTFOUND;
    }

    if (cur->open) {
        printf("already open\n");
        return FS_ERR_INVALID_FH; //FIXME: proper error code
    }

    // we can open it
    cur->open = true;

    err = backends[cur->type].open(cur->backend_handle);
    if (err_is_fail(err)) {
        cur->open = false;
        printf("failed to open\n");
        return FS_ERR_INVALID_FH; // FIXME: better error code
    }

    struct blockdevfs_handle *handle = malloc(sizeof(struct blockdevfs_handle));
    assert(handle != NULL);
    handle->pos = 0;
    handle->entry = cur;
    *rethandle = handle;

    VFS_BLK_DEBUG("blockdevfs_open: exiting\n");
    return SYS_ERR_OK;
}

// create is not allowed in blockdevfs
static errval_t create(void *st, const char *path, vfs_handle_t *rethandle)
{
    return open(st, path, rethandle);
}

// remove is not allowed in blockdevfs
static errval_t blockdevfs_remove(void *st, const char *path)
{
    return FS_ERR_NOTFILE; //FIXME: more suitable error code
}

static errval_t read(void *st, vfs_handle_t handle, void *buffer, size_t bytes,
        size_t *bytes_read)
{
    struct blockdevfs_handle *h = handle;
    struct blockdev_entry *entry = h->entry;
    VFS_BLK_DEBUG("blockdevfs: read %zu bytes at pos %zu into %p from type %d\n",
            bytes, h->pos, buffer, entry->type);
    VFS_BLK_DEBUG("blockdevfs: %p %p\n",
            blockdevfs_ahci_read, backends[entry->type].read);
    errval_t ret = backends[entry->type].read(entry->backend_handle, h->pos,
            buffer, bytes, bytes_read);

    if (err_is_ok(ret)) {
        h->pos += *bytes_read;
    }

    return ret;
}

static errval_t write(void *st, vfs_handle_t handle, const void *buffer, size_t
        bytes, size_t *bytes_written)
{
    struct blockdevfs_handle *h = handle;
    struct blockdev_entry *entry = h->entry;
    errval_t ret = backends[entry->type].write(entry->backend_handle, h->pos,
            buffer, bytes, bytes_written);

    if (err_is_ok(ret)) {
        h->pos += *bytes_written;
    }

    return ret;
}

// truncate is not allowed in blockdevfs
static errval_t truncate(void *st, vfs_handle_t handle, size_t bytes)
{
    return FS_ERR_NOTFILE; //FIXME: more suitable error code
}

static errval_t tell(void *st, vfs_handle_t handle, size_t *pos)
{
    struct blockdevfs_handle *h = handle;
    *pos = h->pos;
    return SYS_ERR_OK;
}

static errval_t stat(void *st, vfs_handle_t inhandle, struct vfs_fileinfo *info)
{
    struct blockdevfs_handle *h = inhandle;

    info->type = VFS_FILE;
    info->size = h->entry->size;

    return SYS_ERR_OK;
}

static errval_t seek(void *st, vfs_handle_t handle, enum vfs_seekpos whence,
        off_t offset)
{
    struct blockdevfs_handle *h = handle;
    struct vfs_fileinfo info;
    errval_t err;

    switch (whence) {
    case VFS_SEEK_SET:
        assert(offset >= 0);
        h->pos = offset;
        break;

    case VFS_SEEK_CUR:
        assert(offset >= 0 || -offset <= h->pos);
        h->pos += offset;
        break;

    case VFS_SEEK_END:
        err = stat(st, handle, &info);
        if (err_is_fail(err)) {
            return err;
        }
        assert(offset >= 0 || -offset <= info.size);
        h->pos = info.size + offset;
        break;

    default:
        USER_PANIC("invalid whence argument to blockdevfs seek");
    }

    return SYS_ERR_OK;
}

static errval_t close(void *st, vfs_handle_t inhandle)
{
    struct blockdevfs_handle *handle = inhandle;
    struct blockdev_entry *entry = handle->entry;

    errval_t ret = backends[entry->type].close(entry->backend_handle);

    handle->entry->open = false;
    free(handle);

    return ret;
}

static errval_t opendir(void *st, const char *path, vfs_handle_t *rethandle)
{
    struct blockdevfs_dirhandle *handle =
        calloc(1, sizeof(struct blockdevfs_dirhandle));
    assert(handle != NULL);
    handle->entry = entries_first;

    *rethandle = handle;

    return SYS_ERR_OK;
}

static errval_t dir_read_next(void *st, vfs_handle_t inhandle, char **retname,
        struct vfs_fileinfo *info)
{
    struct blockdevfs_dirhandle *handle = inhandle;
    if (handle->entry == NULL) {
        return FS_ERR_INDEX_BOUNDS;
    }

    if (retname != NULL) {
        *retname = strdup(handle->entry->path);
    }

    info->size = handle->entry->size;
    info->type = VFS_FILE;

    handle->entry = handle->entry->next;

    return SYS_ERR_OK;
}

static errval_t closedir(void *st, vfs_handle_t dhandle)
{
    struct blockdevfs_dirhandle *handle = dhandle;
    free(handle);
    return SYS_ERR_OK;
}

// creating/removing dirs is not allowed in blockdevfs
static errval_t mkdir(void *st, const char *path)
{
    return FS_ERR_NOTFILE; //FIXME: more suitable error code
}

static errval_t rmdir(void *st, const char *path)
{
    return FS_ERR_NOTFILE; //FIXME: more suitable error code
}

static errval_t flush(void *st, vfs_handle_t inhandle)
{
    struct blockdevfs_handle *handle = inhandle;
    struct blockdev_entry *entry = handle->entry;

    return backends[entry->type].flush(entry->backend_handle);
}

static struct vfs_ops blockdevfsops = {
    .open = open,
    .create = create,
    .remove = blockdevfs_remove,
    .read = read,
    .write = write,
    .truncate = truncate,
    .seek = seek,
    .tell = tell,
    .stat = stat,
    .close = close,
    .opendir = opendir,
    .dir_read_next = dir_read_next,
    .closedir = closedir,
    .mkdir = mkdir,
    .rmdir = rmdir,
    .flush = flush,
};

errval_t vfs_blockdevfs_mount(const char *uri, void **retst, struct vfs_ops **retops)
{
    errval_t err;

    // skip over protocol part of URI to get service name
    char *service = strstr(uri, "://");
    if (service == NULL) {
        return VFS_ERR_BAD_URI;
    }
    service += 3;

    if (*service == 0) {
        // init all kinds of blockdevice sources
        err = blockdevfs_ahci_init();       // AHCI
        assert(err_is_ok(err));
        err = blockdevfs_ata_init();
        assert(err_is_ok(err));             // Flounder
    } else if(!strcmp(service, "megaraid")) {
      err = blockdevfs_megaraid_init();
      assert(err_is_ok(err));
    } else {
      return VFS_ERR_BAD_URI;
    }

    *retops = &blockdevfsops;
    *retst = NULL;

    return SYS_ERR_OK;
}
