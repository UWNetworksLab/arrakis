/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

/*-
 * Copyright (c) 1991, 1993, 1994
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
 * 3. Neither the name of the University nor the names of its contributors
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
 *  @(#)unistd.h    8.12 (Berkeley) 4/27/95
 * $FreeBSD$
 */

#ifndef __BF_UNISTD_H
#define __BF_UNISTD_H

#include <stddef.h>
#include <sys/cdefs.h>
#include <sys/types.h>
#include <pwd.h>
#include <fcntl.h> // for pid_t
#include <sys/socket.h>
#include <sys/select.h>
#include <getopt.h>

#define	R_OK 4
#define	W_OK 2
#define	X_OK 1
#define	F_OK 0

#define	STDIN_FILENO  0
#define	STDOUT_FILENO 1
#define	STDERR_FILENO 2

/*
 * POSIX-style system configuration variable accessors (for the
 * sysconf function).  The kernel does not directly implement the
 * sysconf() interface; rather, a C library stub translates references
 * to sysconf() into calls to sysctl() using a giant switch statement.
 * Those that are marked `user' are implemented entirely in the C
 * library and never query the kernel.  pathconf() is implemented
 * directly by the kernel so those are not defined here.
 */
#define _SC_ARG_MAX             1
#define _SC_CHILD_MAX           2
#define _SC_CLK_TCK             3
#define _SC_NGROUPS_MAX         4
#define _SC_OPEN_MAX            5
#define _SC_JOB_CONTROL         6
#define _SC_SAVED_IDS           7
#define _SC_VERSION             8
#define _SC_BC_BASE_MAX         9 /* user */
#define _SC_BC_DIM_MAX          10 /* user */
#define _SC_BC_SCALE_MAX        11 /* user */
#define _SC_BC_STRING_MAX       12 /* user */
#define _SC_COLL_WEIGHTS_MAX    13 /* user */
#define _SC_EXPR_NEST_MAX       14 /* user */
#define _SC_LINE_MAX            15 /* user */
#define _SC_RE_DUP_MAX          16 /* user */
#define _SC_2_VERSION           17 /* user */
#define _SC_2_C_BIND            18 /* user */
#define _SC_2_C_DEV             19 /* user */
#define _SC_2_CHAR_TERM         20 /* user */
#define _SC_2_FORT_DEV          21 /* user */
#define _SC_2_FORT_RUN          22 /* user */
#define _SC_2_LOCALEDEF         23 /* user */
#define _SC_2_SW_DEV            24 /* user */
#define _SC_2_UPE               25 /* user */
#define _SC_STREAM_MAX          26 /* user */
#define _SC_TZNAME_MAX          27 /* user */

#if __POSIX_VISIBLE >= 199309
#define _SC_ASYNCHRONOUS_IO     28
#define _SC_MAPPED_FILES        29
#define _SC_MEMLOCK             30
#define _SC_MEMLOCK_RANGE       31
#define _SC_MEMORY_PROTECTION   32
#define _SC_MESSAGE_PASSING     33
#define _SC_PRIORITIZED_IO      34
#define _SC_PRIORITY_SCHEDULING 35
#define _SC_REALTIME_SIGNALS    36
#define _SC_SEMAPHORES          37
#define _SC_FSYNC               38
#define _SC_SHARED_MEMORY_OBJECTS 39
#define _SC_SYNCHRONIZED_IO     40
#define _SC_TIMERS              41
#define _SC_AIO_LISTIO_MAX      42
#define _SC_AIO_MAX             43
#define _SC_AIO_PRIO_DELTA_MAX  44
#define _SC_DELAYTIMER_MAX      45
#define _SC_MQ_OPEN_MAX         46
#define _SC_PAGESIZE            47
#define _SC_RTSIG_MAX           48
#define _SC_SEM_NSEMS_MAX       49
#define _SC_SEM_VALUE_MAX       50
#define _SC_SIGQUEUE_MAX        51
#define _SC_TIMER_MAX           52
#endif

#if __POSIX_VISIBLE >= 200112
#define _SC_2_PBS               59 /* user */
#define _SC_2_PBS_ACCOUNTING    60 /* user */
#define _SC_2_PBS_CHECKPOINT    61 /* user */
#define _SC_2_PBS_LOCATE        62 /* user */
#define _SC_2_PBS_MESSAGE       63 /* user */
#define _SC_2_PBS_TRACK         64 /* user */
#define _SC_ADVISORY_INFO       65
#define _SC_BARRIERS            66 /* user */
#define _SC_CLOCK_SELECTION     67
#define _SC_CPUTIME             68
#define _SC_FILE_LOCKING        69
#define _SC_GETGR_R_SIZE_MAX    70 /* user */
#define _SC_GETPW_R_SIZE_MAX    71 /* user */
#define _SC_HOST_NAME_MAX       72
#define _SC_LOGIN_NAME_MAX      73
#define _SC_MONOTONIC_CLOCK     74
#define _SC_MQ_PRIO_MAX         75
#define _SC_READER_WRITER_LOCKS 76 /* user */
#define _SC_REGEXP              77 /* user */
#define _SC_SHELL               78 /* user */
#define _SC_SPAWN               79 /* user */
#define _SC_SPIN_LOCKS          80 /* user */
#define _SC_SPORADIC_SERVER     81
#define _SC_THREAD_ATTR_STACKADDR 82 /* user */
#define _SC_THREAD_ATTR_STACKSIZE 83 /* user */
#define _SC_THREAD_CPUTIME      84 /* user */
#define _SC_THREAD_DESTRUCTOR_ITERATIONS 85 /* user */
#define _SC_THREAD_KEYS_MAX     86 /* user */
#define _SC_THREAD_PRIO_INHERIT 87 /* user */
#define _SC_THREAD_PRIO_PROTECT 88 /* user */
#define _SC_THREAD_PRIORITY_SCHEDULING 89 /* user */
#define _SC_THREAD_PROCESS_SHARED 90 /* user */
#define _SC_THREAD_SAFE_FUNCTIONS 91 /* user */
#define _SC_THREAD_SPORADIC_SERVER 92 /* user */
#define _SC_THREAD_STACK_MIN    93 /* user */
#define _SC_THREAD_THREADS_MAX  94 /* user */
#define _SC_TIMEOUTS            95 /* user */
#define _SC_THREADS             96 /* user */
#define _SC_TRACE               97 /* user */
#define _SC_TRACE_EVENT_FILTER  98 /* user */
#define _SC_TRACE_INHERIT       99 /* user */
#define _SC_TRACE_LOG           100 /* user */
#define _SC_TTY_NAME_MAX        101 /* user */
#define _SC_TYPED_MEMORY_OBJECTS 102
#define _SC_V6_ILP32_OFF32      103 /* user */
#define _SC_V6_ILP32_OFFBIG     104 /* user */
#define _SC_V6_LP64_OFF64       105 /* user */
#define _SC_V6_LPBIG_OFFBIG     106 /* user */
#define _SC_IPV6                118
#define _SC_RAW_SOCKETS         119
#define _SC_SYMLOOP_MAX         120
#endif

#if __XSI_VISIBLE
#define _SC_ATEXIT_MAX          107 /* user */
#define _SC_IOV_MAX             56
#define _SC_PAGE_SIZE           _SC_PAGESIZE
#define _SC_XOPEN_CRYPT         108 /* user */
#define _SC_XOPEN_ENH_I18N      109 /* user */
#define _SC_XOPEN_LEGACY        110 /* user */
#define _SC_XOPEN_REALTIME      111
#define _SC_XOPEN_REALTIME_THREADS 112
#define _SC_XOPEN_SHM           113
#define _SC_XOPEN_STREAMS       114
#define _SC_XOPEN_UNIX          115
#define _SC_XOPEN_VERSION       116
#define _SC_XOPEN_XCU_VERSION   117 /* user */
#endif

#if __BSD_VISIBLE
#define _SC_NPROCESSORS_CONF    57
#define _SC_NPROCESSORS_ONLN    58
#define _SC_CPUSET_SIZE     122
#endif

/* Extensions found in Solaris and Linux. */
#define _SC_PHYS_PAGES      121

struct stat;
extern char **environ;

__BEGIN_DECLS
#if (__XSI_VISIBLE && __XSI_VISIBLE <= 500) || __BSD_VISIBLE
int          chroot(const char *path);
#endif

int          access(const char*pathname,int mode);
unsigned int alarm(unsigned int seconds);
int          chdir(const char*pathname);
int          chown(const char *path, uid_t owner, gid_t group);
int          close(int fd);
int          dup(int oldfd);
int          dup2(int oldfd, int newfd);
int          execv(const char *path, char *const argv[]);
void         _exit(int status);
pid_t        fork(void);
int          fsync(int fd);
int          ftruncate(int fd, int length);
char        *getcwd(char *buf, size_t size);
gid_t        getegid(void);
uid_t        geteuid(void);
gid_t        getgid(void);
int          getgroups(int size, gid_t grouplist[]);
long         gethostid(void);
int          gethostname(char *name, size_t len);
pid_t        getpid(void);
pid_t        getppid(void);
uid_t        getuid(void);
int          initgroups(const char *username, gid_t group);
int          isatty(int fd);
int          link(const char *oldpath, const char *newpath);
off_t        lseek(int fd, off_t offset, int whence);
int          pipe(int pipefd[2]);
int          read(int fd, void *buf, size_t len);
ssize_t      readlink(const char *path, char *buf, size_t bufsize);
int          rmdir(const char*pathname);
void        *sbrk(intptr_t increment);
int          setgid(gid_t gid);
int          setgroups(int size, gid_t grouplist[]);
int          setegid(gid_t gid);
int          seteuid(uid_t uid);
int          setuid(uid_t uid);
pid_t        setsid(void);
int          symlink(const char *oldpath, const char *newpath);
long         sysconf(int name);
char        *ttyname(int fd);
int          unlink(const char*pathname);
int          write(int fd, const void *buf, size_t len);
int          usleep(useconds_t usec);
unsigned int sleep(unsigned int seconds);
__END_DECLS

#endif // __BF_UNISTD_H
