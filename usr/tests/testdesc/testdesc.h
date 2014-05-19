/** \file
 *  \brief test inheritance of file descriptors
 */

/*
 * Copyright (c) 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __TESTDESC_H__
#define __TESTDESC_H__

#include <vfs/fdtab.h>

/* This is posixcompat/unixsock.h  */

#include <sys/un.h>
#include <vfs/vfs.h>
#include <if/unixsock_defs.h>

/// Default receive buffer size
#define _UNIX_SOCKET_RECV_BUF_SIZE      4096

enum _unix_socket_mode {
    _UNIX_SOCKET_MODE_CONNECTING,
    _UNIX_SOCKET_MODE_CONNECTED,
};

struct _unix_socket_recv {
    struct _unix_socket_recv *next, *prev;
    uint8_t *msg;
    size_t size, consumed;
};

struct _unix_socket {
    int type;           // Socket type == AF_UNIX
    int protocol;       // Protocol subtype == 0
    bool passive;       // true iff socket is passive (used to listen)
    bool nonblocking;   // True iff socket is non-blocking

    // Local and peer Sockaddr (includes filename)
    struct sockaddr_un sockaddr, peer;

    union {
        struct {
            struct unixsock_binding **backlog;    // Array of new bindings
            int max_backlog;
            iref_t listen_iref;         // IREF we're listening on
        } passive;
        struct {
            struct unixsock_binding *binding;    // The binding
            enum _unix_socket_mode mode;        // See enum _unix_socket_mode
        } active;
    } u;

    // VFS handle to socket file (represented by filename)
    vfs_handle_t vfs_handle;

    // Total size of receive buffer
    ssize_t recv_buf_size, recv_buf_valid;
    struct _unix_socket_recv *recv_list, *recv_list_end;

    // Send buffer
    uint8_t *send_buf;
};

/* end unixsock.h */


struct fd_store {
    int num;
    enum fdtab_type	type;
    void *handle;
};


#endif // __TESTDESC_H__
