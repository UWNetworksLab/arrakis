/**
 * \file sys/types.h
 */

/*
 * Copyright (c) 2007, 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*-
 * Copyright (c) 1982, 1986, 1991, 1993, 1994
 *      The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
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
 *      @(#)types.h     8.6 (Berkeley) 2/19/95
 * $FreeBSD: head/sys/sys/types.h 210365 2010-07-22 05:42:29Z trasz $
 */

#ifndef _SYS_TYPES_H
#define _SYS_TYPES_H

#include <stdint.h>
#include <sys/cdefs.h>
#include <sys/_types.h>
#include <barrelfish/types.h>

#if __BSD_VISIBLE
typedef unsigned char   u_char;
typedef unsigned short  u_short;
typedef unsigned int    u_int;
typedef unsigned long   u_long;
#ifndef _KERNEL
typedef unsigned short  ushort;         /* Sys V compatibility */
typedef unsigned int    uint;           /* Sys V compatibility */
#endif
#endif

typedef char *          caddr_t;        /* core address */
typedef const char *    c_caddr_t;      /* core address, pointer to const */

//typedef int fd_set;
typedef long ino_t;
typedef long dev_t;

#ifndef _MODE_T_DECLARED
typedef	__mode_t	mode_t;
#define	_MODE_T_DECLARED
#endif

#ifndef _OFF_T_DECLARED
typedef	__off_t		off_t;
#define	_OFF_T_DECLARED
#endif

#ifndef _PID_T_DECLARED
typedef	__pid_t		pid_t;
#define	_PID_T_DECLARED
#endif

#ifndef _CLOCK_T_DECLARED
typedef __clock_t clock_t;
#define _CLOCK_T_DECLARED
#endif

#ifndef _CLOCKID_T_DECLARED
typedef __clockid_t     clockid_t;
#define _CLOCKID_T_DECLARED
#endif

#ifndef _TIME_T_DECLARED
typedef __time_t  time_t;
#define _TIME_T_DECLARED
#endif

struct timespec {
    time_t  tv_sec;         /* seconds */
    long    tv_nsec;        /* and nanoseconds */
};

#ifndef _UID_T_DECLARED
typedef __uid_t   uid_t;
#define _UID_T_DECLARED
#endif

#ifndef _GID_T_DECLARED
typedef __gid_t gid_t;
#define _GID_T_DECLARED
#endif

#ifndef _SA_FAMILY_T_DECLARED
typedef __sa_family_t	sa_family_t;
#define _SA_FAMILY_T_DECLARED
#endif

typedef unsigned long useconds_t;
typedef long suseconds_t;

#endif // _SYS_TYPES_H
