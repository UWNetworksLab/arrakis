/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/cdefs.h>
#include <errno.h>
#include <signal.h>
#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include "posixcompat.h"

signalhandler_t signal(int signum, signalhandler_t handler)
{
    POSIXCOMPAT_DEBUG("Warning: signal(%d, %p) ignored\n", signum, handler);
    return SIG_DFL;
}

int sigprocmask(int how, const sigset_t *restrict set, sigset_t *restrict oset)
{
    POSIXCOMPAT_DEBUG("Warning: sigprocmask(%d, %p, %p) ignored\n",
                      how, set, oset);

    if(oset != NULL) {
        // XXX: Return dummy empty set if requested
        sigemptyset(oset);
    }

    return 0;
}

/*-
 * Copyright (c) 1989, 1993
 *      The Regents of the University of California.  All rights reserved.
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
 *
 *      @(#)sigsetops.c 8.1 (Berkeley) 6/4/93
 */

int sigaddset(sigset_t *set, int signo)
{
    if (signo <= 0 || signo > _SIG_MAXSIG) {
        errno = EINVAL;
        return (-1);
    }
    set->__bits[_SIG_WORD(signo)] |= _SIG_BIT(signo);
    return (0);
}

int sigdelset(sigset_t *set, int signo)
{
    if (signo <= 0 || signo > _SIG_MAXSIG) {
        errno = EINVAL;
        return (-1);
    }
    set->__bits[_SIG_WORD(signo)] &= ~_SIG_BIT(signo);
    return (0);
}

int sigemptyset(sigset_t *set)
{
    int i;

    for (i = 0; i < _SIG_WORDS; i++)
        set->__bits[i] = 0;
    return (0);
}

int sigfillset(sigset_t *set)
{
    int i;

    for (i = 0; i < _SIG_WORDS; i++)
        set->__bits[i] = ~0U;
    return (0);
}

int sigismember(const sigset_t *set, int signo)
{
    if (signo <= 0 || signo > _SIG_MAXSIG) {
        errno = EINVAL;
        return (-1);
    }
    return ((set->__bits[_SIG_WORD(signo)] & _SIG_BIT(signo)) ? 1 : 0);
}

int sigaction(int signum, const struct sigaction *act,
              struct sigaction *oldact)
{
    POSIXCOMPAT_DEBUG("Warning: sigaction(%d, %p, %p) ignored\n",
                      signum, act, oldact);
    return 0;
}

int raise(int sig)
{
    assert(!"NYI");
}
