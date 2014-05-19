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
#include <sys/stat.h>
#include <unistd.h>
#include "posixcompat.h"
#include <vfs/fdtab.h>

int fstat(int fd, struct stat *buf)
{
    struct fdtab_entry *e = fdtab_get(fd);
    if (e->type != FDTAB_TYPE_FILE) {
        return -1;
    }

    POSIXCOMPAT_DEBUG("fstat(%d)\n", fd);

    struct vfs_fileinfo info;
    errval_t err = vfs_stat((vfs_handle_t)e->handle, &info);
    if (err_is_ok(err)) {
        _posixcompat_vfs_info_to_stat(&info, buf);
        return 0;
    } else {
        DEBUG_ERR(err, "fstat(%d) failed\n", fd);
        return -1;
    }
}
