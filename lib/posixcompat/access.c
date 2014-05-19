/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include "posixcompat.h"

int access(const char *pathname, int mode)
{
    vfs_handle_t vh;
    errval_t err;
    int ret;

    char *path = vfs_path_mkabs(pathname);
    assert(path != NULL);

    err = vfs_open(path, &vh);
    if (err_is_fail(err)) {
        if(err_no(err) == FS_ERR_NOTFILE) {
            // Is it a directory?
            err = vfs_opendir(path, &vh);
            if(err_is_ok(err)) {
                vfs_closedir(vh);
                ret = 0;
                goto out;
            }
        }
        POSIXCOMPAT_DEBUG("access(%s) failed\n", pathname);
        ret = -1;
    } else {
        POSIXCOMPAT_DEBUG("access(%s): OK\n", pathname);
        vfs_close(vh);
        ret = 0;
    }

 out:
    free(path);
    return ret;
}
