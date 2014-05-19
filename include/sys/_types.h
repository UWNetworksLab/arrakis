/*-
 * Copyright (c) 2002 Mike Barcroft <mike@FreeBSD.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $FreeBSD$
 */

#ifndef _TYPES_H
#define _TYPES_H

#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>


typedef uint8_t         __uint8_t;
typedef uint16_t        __uint16_t;
typedef uint32_t        __uint32_t;
typedef uint64_t        __uint64_t;
typedef int8_t          __int8_t;
typedef int16_t         __int16_t;
typedef int32_t         __int32_t;
typedef int64_t         __int64_t;
typedef uintptr_t       __uintptr_t;

typedef __uint8_t       u_int8_t;       /* unsigned integrals (deprecated) */
typedef __uint16_t      u_int16_t;
typedef __uint32_t      u_int32_t;
typedef __uint64_t      u_int64_t;

typedef __uint8_t       __sa_family_t;
typedef __uint32_t	__socklen_t;

typedef va_list         __va_list;
typedef size_t          __size_t;

typedef __uint32_t	__gid_t;
typedef __uint32_t	__uid_t;
typedef __int32_t	__clock_t;
typedef __int32_t       __clockid_t;    /* clock_gettime()... */

typedef int             __ct_rune_t;    /* arg type for ctype funcs */
typedef __ct_rune_t     __rune_t;       /* rune_t (see above) */
typedef __ct_rune_t     __wchar_t;      /* wchar_t (see above) */
typedef __ct_rune_t     __wint_t;       /* wint_t (see above) */

typedef __uint16_t      __mode_t;       /* permissions */
typedef __int64_t       __off_t;        /* file offset */
typedef __int32_t       __pid_t;        /* process [group] */
typedef __int64_t       __rlim_t;       /* resource limit - intentionally */
                                        /* signed, because of legacy code */
                                        /* that uses -1 for RLIM_INFINITY */
#define _HAVE_SYSTYPES
typedef long            __key_t;        /* IPC key (for Sys V IPC) */

typedef __uint32_t      __dev_t;        /* device number */

typedef __uint32_t      __fixpt_t;      /* fixed point number */

typedef int             __nl_item;

#ifdef __x86_64__
typedef __int64_t	__time_t;
#else // XXX: All other archs get 32-bit __time_t
typedef __int32_t	__time_t;
#endif

typedef union {
    char            __mbstate8[128];
    __int64_t       _mbstateL;      /* for alignment */
} __mbstate_t;


#ifndef __ssize_t_defined
#if defined(__INT_MAX__) && __INT_MAX__ == 2147483647
typedef int _ssize_t;
#else
typedef long _ssize_t;
#endif
#endif
typedef long int ssize_t;


#ifdef CONFIG_NEWLIB

/*
 * Newlib requirements
 */
#include <sys/lock.h>

#ifndef __off_t_defined
typedef long _off_t;
#endif

#ifndef __off64_t_defined
__extension__ typedef long long _off64_t;
#endif

/*
 * We need fpos_t for the following, but it doesn't have a leading "_",
 * so we use _fpos_t instead.
 */
#ifndef __fpos_t_defined
typedef long _fpos_t;   /* XXX must match off_t in <sys/types.h> */
        /* (and must be `long' for now) */
#endif

#ifdef __LARGE64_FILES
#ifndef __fpos64_t_defined
typedef _off64_t _fpos64_t;
#endif
#endif

#define __need_wint_t
#include <stddef.h>

#ifndef __mbstate_t_defined
/* Conversion state information.  */
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    unsigned char __wchb[4];
  } __value;    /* Value so far.  */
} _mbstate_t;
#endif

#ifndef __flock_t_defined
typedef _LOCK_RECURSIVE_T _flock_t;
#endif

#ifndef __iconv_t_defined
/* Iconv descriptor type */
typedef void *_iconv_t;
#endif

#endif /* CONFIG_NEWLIB */

#endif
