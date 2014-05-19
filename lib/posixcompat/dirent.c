/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "posixcompat.h"

DIR *opendir(const char *pathname)
{
    vfs_handle_t vh;
    errval_t err;

    char *path = vfs_path_mkabs(pathname);
    assert(path != NULL);

    err = vfs_opendir(path, &vh);
    free(path);
    if (err_is_fail(err)) {
        if(err_no(err) == FS_ERR_NOTFOUND) {
            errno = ENOENT;
        }
        POSIXCOMPAT_DEBUG("opendir(%s) not found\n", pathname);
        return NULL;
    }

    POSIXCOMPAT_DEBUG("opendir(%s)\n", pathname);

    DIR *ret = malloc(sizeof(DIR));
    assert(ret != NULL);

    ret->vh = vh;
    return ret;
}

struct dirent *readdir(DIR* dir)
{
    char *name;
    errval_t err;
    err = vfs_dir_read_next(dir->vh, &name, NULL);
    if (err_is_fail(err)) {
        if (err_no(err) != FS_ERR_INDEX_BOUNDS) {
        }
        return NULL;
    }

    strncpy(dir->dirent.d_name, name, sizeof(dir->dirent.d_name));
    free(name);
    dir->dirent.d_name[sizeof(dir->dirent.d_name) - 1] = '\0';
    return &dir->dirent;
}

int closedir(DIR *dir)
{
    vfs_closedir(dir->vh);
    free(dir);
    return(0);
}
