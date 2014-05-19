/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <unistd.h>
#include <errno.h>
#include <vfs/fdtab.h>

static struct fdtab_entry fdtab[MAX_FD] = {
    [STDIN_FILENO] = {
        .type = FDTAB_TYPE_STDIN,
        .handle = NULL,
    },
    [STDOUT_FILENO] = {
        .type = FDTAB_TYPE_STDOUT,
        .handle = NULL,
    },
    [STDERR_FILENO] = {
        .type = FDTAB_TYPE_STDERR,
        .handle = NULL,
    },
};

int fdtab_alloc_from(struct fdtab_entry *h, int start)
{
    assert(h != NULL);
    assert(start >= MIN_FD);

    for (int fd = start; fd < MAX_FD; fd++) {
        if (fdtab[fd].type == FDTAB_TYPE_AVAILABLE) {
            fdtab[fd].inherited = 0; // Just precautionary
            memcpy(&fdtab[fd], h, sizeof(struct fdtab_entry));

            return fd;
        }
    }

    // table full
    errno = EMFILE;
    return -1;
}

int fdtab_alloc(struct fdtab_entry *h)
{
    return fdtab_alloc_from(h, MIN_FD);
}

int fdtab_search(struct fdtab_entry *h)
{
    for (int fd = MIN_FD; fd < MAX_FD; fd++) {
        if (fdtab[fd].type == h->type) {
            switch(h->type) {
            case FDTAB_TYPE_LWIP_SOCKET:
                if(fdtab[fd].fd == h->fd) {
                    return fd;
                }
                break;

            default:
                if(fdtab[fd].handle == h->handle) {
                    return fd;
                }
                break;
            }
        }
    }

    return -1;
}

int fdtab_search_alloc(struct fdtab_entry *h)
{
    assert(h != NULL);

    int fd = fdtab_search(h);

    if(fd == -1) {
        return fdtab_alloc(h);
    } else {
        return fd;
    }
}

struct fdtab_entry *fdtab_get(int fd)
{
    static struct fdtab_entry invalid = {
        .type = FDTAB_TYPE_AVAILABLE,
        .handle = NULL,
        .inherited = 0,
    };

    if (fd < MIN_FD || fd >= MAX_FD) {
        return &invalid;
    } else {
        return &fdtab[fd];
    }
}

void fdtab_free(int fd)
{
    assert(fd >= MIN_FD && fd < MAX_FD);
    assert(fdtab[fd].type != FDTAB_TYPE_AVAILABLE);
    fdtab[fd].type = FDTAB_TYPE_AVAILABLE;
    fdtab[fd].handle = NULL;
    fdtab[fd].fd = 0;
    fdtab[fd].inherited = 0;
}
