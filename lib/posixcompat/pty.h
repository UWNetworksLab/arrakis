/*
 * Copyright (c) 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef POSIXCOMPAT_PTY_H
#define POSIXCOMPAT_PTY_H

#include <barrelfish/waitset.h>
#include <collections/list.h>
#include <term/server/server.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <termios.h>

/**
 * Prefix used when creating a dev-file for the slave side of the pseudo-
 * terminal.
 */
#define PTY_PTS_PATH_PREFIX "/dev/pts/"

/**
 * Prefix used when generating a system-wide unique number using octopus.
 */
#define PTY_PTS_OCTOPUS_PREFIX "ptypts"

/**
 * State of the pseudo-terminal.
 */
struct _pty {
    /**
     * Terminal server state of master side.
     */
    struct term_server *ts;

    /**
     * Interface reference for session interface.
     */
    iref_t iref;

    /**
     * Waitsets used for master side.
     */
    struct waitset session_ws;
    struct waitset in_ws;
    struct waitset out_ws;
    struct waitset conf_ws;

    /**
     * Has at least one client connected?
     */
    bool connected;

    /**
     * Mutex used to lock master binding in case of concurrent reads and writes.
     */
    struct thread_mutex mmutex;

    /**
     * Unique number allocated to this pseudo-terminal master and slave pair.
     */
    uint32_t number;

    /**
     * Path of slave dev-file, e.g. /dev/pts/0.
     */
    char *ptsname;

    /**
     * Data received but not yet consumed by a ptm_read().
     */
    char *mreadbuf_start;
    char *mreadbuf_current;
    char *mreadbuf_end;
    size_t mreadbuf_length;

    /**
     * Thread that dispatches the session waitset.
     */
    struct thread *session_thread;

    /**
     * Reference count for the number of file descriptors that refer to this
     * pseudo terminal. If the reference count drops to 0, the state is freed.
     */
    int opencount;

    /**
     * Termios structure of slave.
     */
    struct termios termios;

    /**
     * Winsize structure of slave.
     */
    struct winsize winsize;

    /**
     * File status flags as set by fctnl's F_GETFL and F_SETFL.
     */
    int file_status_flags;
};

/**
 * Wrapper functions for master side.
 */
int     ptm_close(int fd);
ssize_t ptm_read(int fd, void *buf, size_t count);
ssize_t ptm_write(int fd, const void *buf, size_t count);

/**
 * Wrapper functions for slave side.
 */
int     pts_close(int fd);
int     pts_open(const char *path, int oflags);
ssize_t pts_read(int fd, void *buf, size_t count);
ssize_t pts_write(int fd, const void *buf, size_t count);

#endif // POSIXCOMPAT_PTY_H
