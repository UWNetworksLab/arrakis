/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <vfs/fdtab.h>

int dup(int oldfd)
{
    return fcntl(oldfd, F_DUPFD, 0);
}

int dup2(int oldfd, int newfd)
{
    if(newfd < 0 || newfd >= MAX_FD) {
        errno = EBADF;
        return -1;
    }

    struct fdtab_entry *e = fdtab_get(oldfd);
    if (e->type == FDTAB_TYPE_AVAILABLE) {
        return -1;
    }

    if(oldfd == newfd) {
        return newfd;
    }

    close(newfd);
    int retfd = fcntl(oldfd, F_DUPFD, newfd);

    if(newfd != retfd) {
        close(retfd);
        return -1;
    }

    return newfd;
}
