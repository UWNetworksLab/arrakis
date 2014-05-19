/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef _FDTAB_H
#define _FDTAB_H

#include <sys/cdefs.h>

__BEGIN_DECLS

#define MIN_FD  0
//#define MAX_FD  132
#define MAX_FD  4096

enum fdtab_type {
    FDTAB_TYPE_AVAILABLE,
    FDTAB_TYPE_FILE,
    FDTAB_TYPE_UNIX_SOCKET,
    FDTAB_TYPE_STDIN,
    FDTAB_TYPE_STDOUT,
    FDTAB_TYPE_STDERR,
    FDTAB_TYPE_LWIP_SOCKET,
    FDTAB_TYPE_EPOLL_INSTANCE,
    FDTAB_TYPE_PTM,         ///< master side of pseudo-terminal
    FDTAB_TYPE_PTS,         ///< slave side of pseudo-terminal
};

#include <signal.h>
#include <sys/epoll.h>

struct _epoll_events_list {
    struct _epoll_events_list *prev, *next;
    struct epoll_event event;
    int fd;
};

struct fdtab_entry {
    enum fdtab_type     type;
//    union {
        void            *handle;
        int             fd;
        int             inherited;
//    };
    int epoll_fd;
    struct _epoll_events_list epoll_events;
};

int fdtab_alloc(struct fdtab_entry *h);
int fdtab_alloc_from(struct fdtab_entry *h, int start);
int fdtab_search(struct fdtab_entry *h);
int fdtab_search_alloc(struct fdtab_entry *h);
struct fdtab_entry *fdtab_get(int fd);
void fdtab_free(int fd);

__END_DECLS

#endif // _FDTAB_H
