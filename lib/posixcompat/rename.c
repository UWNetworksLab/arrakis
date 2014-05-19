/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include <sys/stat.h>
#include <unistd.h>
#include "posixcompat.h"

int rename(const char *old, const char *new)
{
    assert(old != NULL);
    assert(new != NULL);

    // Success if old == new
    if(!strcmp(old, new)) {
        return 0;
    }

    // Find out type of old and new
    struct stat oldstat, newstat;
    int r = stat(old, &oldstat);
    if(r != 0) {
        return -1;
    }
    r = stat(new, &newstat);
    if(r != 0 && errno != ENOENT) {
        return -1;
    }

    if(!(oldstat.st_mode & S_IFREG)) {
        fprintf(stderr, "Only files are supported by rename() at the moment\n");
        return -1;
    }

    POSIXCOMPAT_DEBUG("rename(\"%s\", \"%s\")\n", old, new);

    char *oldpath = vfs_path_mkabs(old);
    assert(oldpath != NULL);
    char *newpath = vfs_path_mkabs(new);
    assert(newpath != NULL);

    // XXX: rename through copy. VFS should offer a relink facility.
    vfs_handle_t newvh, oldvh;
    errval_t err = vfs_open(oldpath, &oldvh);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_open");
        return -1;
    }
    err = vfs_create(newpath, &newvh);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_create");
        return -1;
    }

    char buf[1024];
    size_t retsize, writesize;
    while((err = vfs_read(oldvh, buf, 1024, &retsize))) {
        if(retsize == 0) {
            break;
        }
        err = vfs_write(newvh, buf, retsize, &writesize);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "vfs_write");
        }
        assert(writesize == retsize);
    }
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_read");
    }

    err = vfs_close(oldvh);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_close");
    }
    err = vfs_close(newvh);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_close");
    }

    err = vfs_remove(oldpath);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_remove");
        return -1;
    }

    return 0;
}
