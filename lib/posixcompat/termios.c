/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

/*-
 * Copyright (c) 1989, 1993
 *  The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <barrelfish/barrelfish.h>
#include <vfs/fdtab.h>

#include "pty.h"
#include "posixcompat.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#define TTYDEFCHARS
#include <termios.h>

/**
 * \brief Get the parameters associated with the terminal.
 */
int tcgetattr(int fd, struct termios *t)
{
    struct fdtab_entry *e = fdtab_get(fd);

    switch (e->type) {
    case FDTAB_TYPE_AVAILABLE:
        {
            errno = EBADF;
            return -1;
        }

    case FDTAB_TYPE_PTM:
    case FDTAB_TYPE_PTS:
        {
            struct _pty *state = e->handle;
            memcpy(t, &state->termios, sizeof(struct termios));
            return 0;
        }

    case FDTAB_TYPE_STDIN:
    case FDTAB_TYPE_STDOUT:
    case FDTAB_TYPE_STDERR:
        {
            assert(!"NYI");
            return -1;
        }

    default:
        {
            errno = ENOTTY;
            return -1;
        }
    }
}

/**
 * \brief Set the parameters associated with the terminal.
 *
 * \note The optional_actions are ignored.
 */
int tcsetattr(int fd, int optional_actions, const struct termios *t)
{
    struct fdtab_entry *e = fdtab_get(fd);

    switch (e->type) {
    case FDTAB_TYPE_AVAILABLE:
        {
            errno = EBADF;
            return -1;
        }

    case FDTAB_TYPE_PTM:
    case FDTAB_TYPE_PTS:
        {
            struct _pty *state = e->handle;

            /*
             * FIXME: Apart from being updated in the state associated with
             *        the file descriptor, the settings are ignored. They
             *        have no effect on the terminal device.
             */
            memcpy(&state->termios, t, sizeof(struct termios));
            return 0;
        }

    case FDTAB_TYPE_STDIN:
    case FDTAB_TYPE_STDOUT:
    case FDTAB_TYPE_STDERR:
        {
            assert(!"NYI");
            return -1;
        }

    default:
        {
            errno = ENOTTY;
            return -1;
        }
    }
}

/**
 * \brief Send a break for a specific duration.
 */
int tcsendbreak(int fd, int duration)
{
    assert(!"NYI");
    return -1;
}

/**
 * \brief Get output baud rate.
 */
speed_t cfgetospeed(const struct termios *t)
{
    return (t->c_ospeed);
}

/**
 * \brief Get input baud rate.
 */
speed_t cfgetispeed(const struct termios *t)
{
    return (t->c_ispeed);
}

/**
 * \brief Set output baud rate.
 */
int cfsetospeed(struct termios *t, speed_t speed)
{
    t->c_ospeed = speed;
    return 0;
}

/**
 * \brief Set input baud rate.
 */
int cfsetispeed(struct termios *t, speed_t speed)
{
    t->c_ispeed = speed;
    return 0;
}

/**
 * \brief Fill in default values of termios structure.
 *
 * \note This function is not part of POSIX, it's a Barrelfish utility function.
 */
void termios_fill_defaults(struct termios *t)
{
    t->c_iflag  = TTYDEF_IFLAG;
    t->c_oflag  = TTYDEF_OFLAG;
    t->c_cflag  = TTYDEF_CFLAG;
    t->c_lflag  = TTYDEF_LFLAG;
    t->c_ispeed = TTYDEF_SPEED;
    t->c_ospeed = TTYDEF_SPEED;
    memcpy(&t->c_cc, ttydefchars, sizeof(ttydefchars));
}
