/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <fcntl.h>
#include <string.h>
#include <vfs/vfs_fd.h>

#include "posixcompat.h"
#include "pty.h"

//XXX: flags are ignored...
int open(const char *pathname, int flags, ...)
{
    /*
     * If the slave side of a pseudo-terminal is opened, call wrapper function.
     *
     * FIXME: Find a more flexible way to call specific open function for
     *        special files.
     */
    if (strncmp(pathname, PTY_PTS_PATH_PREFIX, strlen(PTY_PTS_PATH_PREFIX)) == 0) {
        return pts_open(pathname, flags);
    } else {
        return vfsfd_open(pathname, flags);
    }
}
