/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <vfs/fdtab.h>

#include <assert.h>
#include <errno.h>
#include <unistd.h>

#include "pty.h"

/**
 * \brief Find the pathname of a terminal.
 */
char *ttyname(int fd)
{
    struct fdtab_entry *e = fdtab_get(fd);

    switch(e->type) {
    case FDTAB_TYPE_AVAILABLE:
        {
            errno = EBADF;
            return NULL;
        }

    case FDTAB_TYPE_PTS:
        {
            struct _pty *pty = e->handle;
            return pty->ptsname;
        }

    case FDTAB_TYPE_PTM:
    case FDTAB_TYPE_STDIN:
    case FDTAB_TYPE_STDOUT:
    case FDTAB_TYPE_STDERR:
        {
            /*
             * We do not create a file in the filesystem for these types of
             * file descriptors.
             */
            assert(!"NYI");
            return NULL;
        }

    default:
        {
            errno = ENOTTY;
            return 0;
        }
    }
}
