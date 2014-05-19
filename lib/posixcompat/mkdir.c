/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/stat.h>
#include <errno.h>

#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include "posixcompat.h"

int mkdir(const char *pathname, int mode)
{
    char *path = vfs_path_mkabs(pathname);
    assert(path != NULL);

    POSIXCOMPAT_DEBUG("mkdir(\"%s\", %d)\n", pathname, mode);

    errval_t err = vfs_mkdir(path);
    if(err_is_fail(err)) {
        switch(err_no(err)) {
        case FS_ERR_EXISTS:
            errno = EEXIST;
            break;

        default:
            DEBUG_ERR(err, "unknown vfs_mkdir error");
            break;
        }

        return -1;
    }

    return 0;
}
