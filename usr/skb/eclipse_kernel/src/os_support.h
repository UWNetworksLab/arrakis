/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe system
 *
 * IDENTIFICATION:	os_support.h
 *
 * $Id: os_support.h,v 1.1 2008/06/30 17:43:57 jschimpf Exp $
 *
 * AUTHOR:		Joachim Schimpf, IC-Parc
 *
 * DESCRIPTION:		Operating-system services abstraction layer
 *		
 */

#ifdef _WIN32
#ifndef DLLEXP
#define DLLEXP __declspec(dllexport)
#endif
#else
#define DLLEXP
#endif

#include <sys/types.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef _WIN32

#include <io.h>
#include <fcntl.h>

#ifndef __GNUC__

#define	R_OK	4
#define	W_OK	2
#define	X_OK	1
#define	F_OK	0

#define O_RDWR		_O_RDWR
#define O_CREAT		_O_CREAT
#define O_EXCL		_O_EXCL
#define O_SYNC		0
#define O_RDONLY	_O_RDONLY
#define O_WRONLY	_O_WRONLY
#define O_TRUNC		_O_TRUNC
#define O_APPEND	_O_APPEND

#endif
#endif

#ifdef SEEK_SET
#define LSEEK_SET	SEEK_SET
#define LSEEK_INCR	SEEK_CUR
#define LSEEK_END	SEEK_END
#else
#define LSEEK_SET	0
#define LSEEK_INCR	1
#define LSEEK_END	2
#endif



/* Values for the global variable ec_os_errgrp_ indicating
 * what kind of error number we have in ec_os_errno_ */

#define ERRNO_UNIX	0
#define ERRNO_WIN32	1

#define Set_Sys_Errno(n,grp) { \
	ec_os_errgrp_ = (grp); \
	ec_os_errno_ = (n); }

#ifdef SOCKETS
#ifdef _WIN32
#define Set_Socket_Errno() { \
 	ec_os_errno_ = WSAGetLastError(); \
	ec_os_errgrp_ = ERRNO_WIN32; }
#else
#define Set_Socket_Errno() { \
 	ec_os_errno_ = errno; \
	ec_os_errgrp_ = ERRNO_UNIX; }
#endif
#endif

#ifdef _WIN32
#  include 	<stdlib.h>
#  define MAX_PATH_LEN	_MAX_PATH
#else
#ifdef PATH_IN_LIMITS
#  include 	<limits.h>
#  define MAX_PATH_LEN	PATH_MAX
#else
#  include <sys/param.h>
#  define MAX_PATH_LEN	MAXPATHLEN
#endif
#endif

#ifdef __STDC__
#  define ARGS(x) x
#else
#  define ARGS(x) ()
#endif

#ifdef _WIN32
#ifndef __GNUC__
#define S_IFMT	_S_IFMT
#define S_IFDIR	_S_IFDIR
typedef    struct _stat	struct_stat;
#else
typedef    struct stat	struct_stat;
#endif
#else
typedef    struct stat	struct_stat;
#endif

extern char	ec_version[];
extern int	clock_hz;
extern int	ec_os_errno_;
extern int	ec_os_errgrp_;

void	ec_os_init ARGS((void));
void	ec_os_fini ARGS((void));
char *	expand_filename ARGS((char *in, char *out));
char *	os_filename ARGS((char *in, char *out));
extern DLLEXP	char *		os_filename ARGS((char *in, char *out));
char *	canonical_filename ARGS((char *in, char *out));
long	user_time ARGS((void));	/* ticks */
int	all_times ARGS((double *user, double *system, double *elapsed));
long	ec_unix_time ARGS((void)); /* seconds */
char *	ec_date_string ARGS((char *buf));
int	ec_gethostname ARGS((char *buf, int size));
int	ec_gethostid ARGS((char *buf, int size));
int	get_cwd ARGS((char *buf, int size));
int	ec_rename ARGS((char *, char *));
void	ec_sleep ARGS((double));
void	ec_bad_exit ARGS((char *));
char *	ec_os_err_string ARGS((int err,int grp,char *buf,int size));
char *	ec_env_lookup ARGS((char*, char*, int*));

#ifdef HAVE_GETHOSTID
#ifdef GETHOSTID_UNDEF
#    if (SIZEOF_LONG == 8)
extern int	gethostid();
#    else
extern long	gethostid();
#    endif
#  endif
#else
extern long	gethostid();
#endif

#ifndef HAVE_STRERROR
char	*strerror ARGS((int));
#endif

#ifdef _WIN32
#ifndef __GNUC__
#define bzero(p,n) ZeroMemory(p,n)
int	putenv ARGS((char *));
int	lseek ARGS((int, long, int));
int	fstat ARGS((int handle, struct_stat *buf));
#endif
int	getpid ARGS((void));
int	isatty ARGS((int));
int	isascii ARGS((int));
int	close ARGS((int));
int	read ARGS((int, void *, unsigned int));
int	write ARGS((int, const void *, unsigned int));
int	pipe ARGS((int *));
int	dup ARGS((int));
int	dup2 ARGS((int, int));
int	getpagesize ARGS((void));
int	ec_getch_raw ARGS((int));
int	ec_putch_raw ARGS((int));
int	ec_set_alarm ARGS((double, double, void (*) ARGS((long)), long, double*, double*));
void *	ec_make_thread ARGS((void));
int	ec_start_thread ARGS((void* thread, int (*) ARGS((void*)), void* data));
int	ec_thread_stopped ARGS((void* thread, int* result));
int	ec_thread_wait ARGS((void* thread, int* result, int timeout));
int	ec_thread_terminate ARGS((void* thread, int timeout));
#endif

/*
 * Functions that take filename arguments in ECLiPSe pathname syntax
 */
#ifdef _WIN32
int	ec_chdir ARGS((char *));
int	ec_access ARGS((char *name, int amode));
int	ec_stat ARGS((char *name, struct_stat *buf));
int	ec_rmdir ARGS((char *name));
int	ec_mkdir ARGS((char *name, int));
int	ec_unlink ARGS((char *name));
int	ec_open ARGS((const char *, int, int));
#else
#define	ec_chdir(A) chdir(A)
#define	ec_access(A,B) access(A,B)
#define	ec_stat(A,B) stat(A,B)
#define	ec_rmdir(A) rmdir(A)
#define	ec_mkdir(A,B) mkdir(A,B)
#define	ec_unlink(A) unlink(A)
#define	ec_open(A,B,C) open(A,B,C)
#endif

