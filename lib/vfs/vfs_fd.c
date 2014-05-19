/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/terminal.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include <vfs/vfs_fd.h>
#include <errno.h>
#include <fcntl.h>
#include <vfs/fdtab.h>

#if 0
#define VFSFD_DEBUG(x...) debug_printf(x)
#define VFSFD_DEBUG_ENABLED
#else
#define VFSFD_DEBUG(x...) ((void)0)
#endif

//XXX: flags are ignored...
int vfsfd_open(const char *pathname, int flags)
{
    vfs_handle_t vh;
    errval_t err;

    char *path = vfs_path_mkabs(pathname);
    assert(path != NULL);

    // If O_CREAT was given, we use vfs_create()
    if(flags & O_CREAT) {
        // If O_EXCL was also given, we check whether we can open() first
        if(flags & O_EXCL) {
            err = vfs_open(path, &vh);
            if(err_is_ok(err)) {
                vfs_close(vh);
                errno = EEXIST;
                return -1;
            }
            assert(err_no(err) == FS_ERR_NOTFOUND);
        }

        err = vfs_create(path, &vh);
    } else {
        // Regular open()
        err = vfs_open(path, &vh);
    }

    free(path);
    if (err_is_fail(err)) {
        VFSFD_DEBUG("open('%s') failed\n", pathname);

        switch(err_no(err)) {
        case FS_ERR_NOTFOUND:
            errno = ENOENT;
            break;

        default:
#ifdef VFSFD_DEBUG_ENABLED
            DEBUG_ERR(err, "vfs_open");
#endif
            break;
        }

        return -1;
    }

    struct fdtab_entry e = {
        .type = FDTAB_TYPE_FILE,
        .handle = vh,
        .epoll_fd = -1,
    };
    int fd = fdtab_alloc(&e);
    VFSFD_DEBUG("open(%s) as fd %d\n", pathname, fd);
    if (fd < 0) {
        vfs_close(vh);
        return -1;
    } else {
        return fd;
    }
}


int vfsfd_read(int fd, void *buf, size_t len)
{
    struct fdtab_entry *e = fdtab_get(fd);
    size_t retlen = 0;
    switch(e->type) {
    case FDTAB_TYPE_FILE:
        {
            errval_t err = vfs_read((vfs_handle_t)e->handle, buf, len, &retlen);
            VFSFD_DEBUG("%d : read(%d, %d) = %lu\n", disp_get_domain_id(), fd, len, retlen);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "error in vfs_read");
                return -1;
            }
        }
        break;

    case FDTAB_TYPE_STDIN:
        retlen = terminal_read((char *)buf, len);
        break;

    case FDTAB_TYPE_STDOUT:
    case FDTAB_TYPE_STDERR:
    case FDTAB_TYPE_AVAILABLE:
    default:
        return -1;
    }

    return retlen;
}

int vfsfd_write(int fd, const void *buf, size_t len)
{
    struct fdtab_entry *e = fdtab_get(fd);
    if (e->type == FDTAB_TYPE_AVAILABLE) {
        return -1;
    }

    size_t retlen = 0;

    switch(e->type) {
    case FDTAB_TYPE_FILE:
        {
            errval_t err = vfs_write((vfs_handle_t)e->handle, buf, len, &retlen);
            VFSFD_DEBUG("write(%d, %d) = %lu\n", fd, len, retlen);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "error in vfs_write");
                return -1;
            }
        }
        break;

    case FDTAB_TYPE_STDOUT:
        /* only support writting to terminal */
        retlen = terminal_write((const char*) buf, len);
        break;

    case FDTAB_TYPE_STDERR:
        retlen = terminal_write((const char*) buf, len);
        break;

    case FDTAB_TYPE_STDIN:
    case FDTAB_TYPE_AVAILABLE:
    default:
        return -1;
    }

    return retlen;
}

int vfsfd_close(int fd)
{
    errval_t err;
    struct fdtab_entry *e = fdtab_get(fd);
    if (e->type == FDTAB_TYPE_AVAILABLE) {
        return -1;
    }

    VFSFD_DEBUG("close(%d)\n", fd);

    switch(e->type) {
    case FDTAB_TYPE_FILE:
        err = vfs_close((vfs_handle_t)e->handle);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error in vfs_close");
            return -1;
        }
        break;

    case FDTAB_TYPE_STDIN:
    case FDTAB_TYPE_STDOUT:
    case FDTAB_TYPE_STDERR:
        // XXX: Should call fclose() when closing last FD
        break;

    default:
        return -1;
    } // end switch

    fdtab_free(fd);
    return 0;
}

off_t vfsfd_lseek(int fd, off_t offset, int whence)
{
    struct fdtab_entry *e = fdtab_get(fd);
    switch(e->type) {
    case FDTAB_TYPE_FILE:
        {
            enum vfs_seekpos vfs_whence;
            errval_t err;
            size_t retpos;

            switch(whence) {
            case SEEK_SET:
                vfs_whence = VFS_SEEK_SET;
                break;

            case SEEK_CUR:
                vfs_whence = VFS_SEEK_CUR;
                break;

            case SEEK_END:
                vfs_whence = VFS_SEEK_END;
                break;

            default:
                return -1;
            }

            err = vfs_seek((vfs_handle_t)e->handle, vfs_whence, offset);
            if(err_is_fail(err)) {
                DEBUG_ERR(err, "vfs_seek");
                return -1;
            }

            err = vfs_tell((vfs_handle_t)e->handle, &retpos);
            if(err_is_fail(err)) {
                DEBUG_ERR(err, "vfs_tell");
                return -1;
            }

            VFSFD_DEBUG("lseek(%d, %lld, %d) = %lu\n",
                         fd, offset, whence, retpos);

            return retpos;
        }
        break;

    default:
        return -1;
    }
}
