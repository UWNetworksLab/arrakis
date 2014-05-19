/*
 * Copyright (c) 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <fcntl.h>
#include <assert.h>
#include <stdarg.h>
#include <errno.h>
#include <vfs/fdtab.h>
#include <lwip/sockets.h>
#include "posixcompat.h"
#include "unixsock.h"
#include "pty.h"

int fcntl(int fd, int cmd, ...)
{
    int retval = 0;
    va_list arg;

    va_start(arg, cmd);

    struct fdtab_entry *e = fdtab_get(fd);
    if (e->type == FDTAB_TYPE_AVAILABLE) {
        va_end(arg);
        return -1;
    }

    switch(cmd) {
    case F_DUPFD:
        {
            int from = va_arg(arg, int);
            retval = fdtab_alloc_from(e, from);

            /* incremet reference count for fds that support it */
            switch (e->type) {
            case FDTAB_TYPE_PTS:
            case FDTAB_TYPE_PTM:
                {
                    struct _pty *state = e->handle;
                    state->opencount++;
                }
                break;

            default:
                /* do nothing */
                break;
	    }
	}
	break;

    case F_GETFD:
        // XXX: No flags are supported ATM
        retval = 0;
        break;

    case F_SETFD:
        {
            int flags = va_arg(arg, int);

            POSIXCOMPAT_DEBUG("fcntl(%d, F_SETFD, %d)\n", fd, flags);

            if(flags & FD_CLOEXEC) {
                // XXX: close-on-exec not supported, since exec() not supported
            } else {
            }
        }
        break;

    case F_SETFL:
        {
            int flags = va_arg(arg, int);

            POSIXCOMPAT_DEBUG("fcntl(%d, F_SETFL, %d)\n", fd, flags);

            switch(e->type) {
            case FDTAB_TYPE_UNIX_SOCKET:
                {
                    struct _unix_socket *us = e->handle;
                    us->nonblocking = flags & O_NONBLOCK ? true : false;
                }
                break;

            case FDTAB_TYPE_LWIP_SOCKET:
                retval = lwip_fcntl(e->fd, cmd, flags);
                break;

            case FDTAB_TYPE_PTS:
            case FDTAB_TYPE_PTM:
                {
                    struct _pty *state = e->handle;
                    state->file_status_flags = flags;
                }
                break;

            default:
                retval = -1;
                break;
            }
        }
        break;

    case F_GETFL:
        {
            POSIXCOMPAT_DEBUG("fcntl(%d, F_GETFL)\n", fd);

            switch(e->type) {
            case FDTAB_TYPE_FILE:
                // no flags set
                retval = 0;
                break;

            case FDTAB_TYPE_LWIP_SOCKET:
                retval = lwip_fcntl(e->fd, cmd, 0);
                break;

            case FDTAB_TYPE_UNIX_SOCKET:
                {
                    // XXX: We only handle non-blocking here
                    struct _unix_socket *us = e->handle;
                    retval = 0;
                    retval |= us->nonblocking ? O_NONBLOCK : 0;
                }
                break;

            case FDTAB_TYPE_PTS:
            case FDTAB_TYPE_PTM:
                {
                    struct _pty *state = e->handle;
                    return state->file_status_flags;
                }

            default:
                assert(!"NYI");
                retval = -1;
                break;
            }
        }
        break;

    default:
        assert(!"NYI");
        retval = -1;
        break;
    }

    va_end(arg);
    return retval;
}
