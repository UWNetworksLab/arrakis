/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <vfs/fdtab.h>

#include "pty.h"

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/ttycom.h>

int ioctl(int fd, unsigned long request,  ...)
{
    va_list arg;
    va_start(arg, request);

    struct fdtab_entry *e = fdtab_get(fd);

    switch (e->type) {
    case FDTAB_TYPE_AVAILABLE:
        {
            errno = EBADF;
            return -1;
        }

    case FDTAB_TYPE_PTS:
        {
            struct _pty *state = e->handle;
            switch (request) {
                case TIOCGWINSZ: /* get window size */
                    {
                        struct winsize *w = va_arg(arg, struct winsize *);
                        memcpy(w, &state->winsize, sizeof(struct winsize));
                        return 0;
                    }

                case TIOCSWINSZ: /* set window size */
                    {
                        struct winsize *w = va_arg(arg, struct winsize *);
                        memcpy(&state->winsize, w, sizeof(struct winsize));
                        return 0;
                    }

                default:
                    assert(!"NYI");
                    errno = EINVAL;
                    return -1;
            }
        }

    default:
        {
            assert("NYI");
            return -1;
        }
    }

    va_end(arg);
}
