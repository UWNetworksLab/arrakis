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
 * Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Kees Schuerman, ECRC
 * 
 * END LICENSE BLOCK */

/**********************************************************************
**        File: addrmap.c
**      Author: Kees Schuerman
** Description: Address Space Map
**
*
*	The program displays the address map of the machine
*	using the following categories:
*
*	FREE		The area is not mapped and can be mapped.
*			If the flag ADDR_FLAGS_WRITECHECK is specified,
*			it is also writable, otherwise this is not certain.
*	MAPPED		This area is already mapped to this process.
*	UNAVAILABLE	This are is not mapped to this process and cannot
*			be mapped, i.e. it is probably being used by
*			other processes
*
***********************************************************************/

#include "config.h"

#ifdef HAVE_MMAP
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <signal.h>
#include <setjmp.h>

#ifdef SBRK_UNDEF
extern char	*sbrk();
#endif
extern void	exit(int);

#ifdef HAVE_UNISTD_H
#include	<unistd.h>
#endif

#define ADDR_UNAVAILABLE	0
#define ADDR_MAPPED		1
#define ADDR_FREE		2
#define ADDR_FREE_VAR		3
#define ADDR_TRY	-1

#define ADDR_FLAGS_WRITECHECK	1
#define ADDR_FLAGS_SHARED	2

#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif
#ifndef MAP_VARIABLE
#define MAP_VARIABLE 0
#endif
#ifndef MAP_FAILED
#define MAP_FAILED ((capaddr_t) (-1))
#endif
#if defined(MAP_ANONYMOUS) || defined(MAP_ANON)
#  ifndef MAP_FILE
#  define MAP_FILE 0
#  endif
#  ifndef MAP_ANONYMOUS
#  define MAP_ANONYMOUS MAP_ANON
#  endif
#endif

extern char	*shared_mem_base(void);

typedef struct addr_range {
    struct addr_range * next;
    char * addr;
    unsigned size;
    unsigned state;
} addr_range_t;

static jmp_buf addr_env;

static addr_range_t * addr_spc_map;
static addr_range_t * addr_range;
static char	    * addr_range_addr;

static void	_addr_map(char *from, char *to, long int increment),
		_addr_init(char *from),
		_write_address(char *a),
		_write_range(char *from, char *to, int state);

#ifdef HAVE_SIGACTION
typedef struct sigaction sig_action_t;
#else
#ifdef HAVE_SIGVEC
typedef struct sigvec sig_action_t;
#define sa_handler sv_handler
#define sa_mask sv_mask
#define sa_flags sv_flags
#else
typedef struct {
	RETSIGTYPE (*sa_handler)();
	int sa_mask;
	int sa_flags;
} sig_action_t;
#endif
#endif

static sig_action_t sigsegv_act;
static sig_action_t sigbus_act;

static unsigned long addr_pagesize;
static int addr_map_fd;
static long addr_increment;

static volatile char * addr = 0;
static volatile int addr_state = ADDR_FREE;
static volatile int addr_flags = 0;
static volatile int addr_map_mode = MAP_FIXED;

ec_layout(int flags, char *from, char *to, long int increment)
{

    if (flags & ADDR_FLAGS_SHARED)
	write(2,  "\nshared\n", 8);
    else if (flags & ADDR_FLAGS_WRITECHECK)
	write(2,  "\nwrite check\n", 13);
    addr_flags = flags;
    _addr_init(from);
    _addr_map(from, to, increment);
}

static void
_addr_init(char *from)
{
#if defined(HAVE_SYSCONF) && defined(SYSCONF_PAGE)
    addr_pagesize = sysconf(SYSCONF_PAGE);
#else
#ifdef HAVE_GETPAGESIZE
    addr_pagesize = getpagesize();
#else
    addr_pagesize = 4096;
#endif
#endif

    if (addr_flags & ADDR_FLAGS_SHARED) {
	(void) unlink("heap.map");
	addr_map_fd = open("heap.map", O_RDWR|O_CREAT|O_EXCL, 0700);
	if (addr_map_fd == -1 ||
		ftruncate(addr_map_fd, (off_t) addr_pagesize) == -1) {
	    (void) write(2, "AdrMap: Cannot use the map file\n", 32);
	    exit(-1);
	}
    } else
#ifdef MAP_ANONYMOUS
	addr_map_fd = -1;
#else
	if ((addr_map_fd = open("/dev/zero", O_RDWR)) == -1) {
	    (void) write(2, "AdrMap: Cannot open /dev/zero\n", 31);
	    exit(-1);
	}
#endif

    addr_spc_map = (addr_range_t *) 0;

    addr = addr_range_addr = from;
    addr_range = (addr_range_t *) 0;
}

/*ARGSUSED*/
static void
_sigsegv_handler(int sig)
{
    int		addr_state_save;
    int		map_flags, map_prot;
    capaddr_t		map_result;
#ifdef HAVE_SIGPROCMASK
    sigset_t	sig_mask;

    (void) sigemptyset(&sig_mask);
    (void) sigaddset(&sig_mask, sig);
    (void) sigprocmask(SIG_UNBLOCK, &sig_mask, (sigset_t *) 0);
#else
    int		sig_mask;

    sig_mask = ~sigmask(sig);
    (void) sigblock(sig_mask);
#endif

    map_prot = PROT_READ|PROT_WRITE;
    if(addr_flags & ADDR_FLAGS_SHARED) {
	map_prot |= PROT_EXEC;
	map_flags = 
#ifdef MAP_ANONYMOUS
		    MAP_FILE|
#endif
		    		MAP_SHARED|MAP_NORESERVE|addr_map_mode;
    } else {
	map_flags = 
#ifdef MAP_ANONYMOUS
		    MAP_ANONYMOUS|
#endif
				    MAP_PRIVATE|addr_map_mode;
    }
    map_result = mmap((capaddr_t) addr, 
		   (size_t) addr_pagesize, 
		   map_prot,
		   map_flags,
		   addr_map_fd, 
		   (off_t) 0);
    if ((addr_state != ADDR_TRY) && map_result == addr) {
	if (addr_flags & ADDR_FLAGS_WRITECHECK &&
	    !(addr_flags & ADDR_FLAGS_SHARED)) {
	    addr_state_save = addr_state;
	    addr_state = ADDR_TRY;
	    *addr = 0xff;
	    addr_state = addr_state_save;
	}
	(void) munmap((capaddr_t) addr,(size_t) addr_pagesize);
	if (addr_state != ADDR_FREE) {
	    if (addr_range_addr != addr)
		_write_range(addr_range_addr, (char *) addr, addr_state);
	    addr_range_addr = (char *) addr;
	}
	addr_state = ADDR_FREE;
    }
    else if (addr_state != ADDR_UNAVAILABLE) {
	if (addr_range_addr != addr)
	    _write_range(addr_range_addr, (char *) addr, addr_state);
	addr_range_addr = (char *) addr;
	addr_state = ADDR_UNAVAILABLE;
    }

    addr += addr_increment;

    longjmp(addr_env,0);
}


static void
_save_handler(int sig, sig_action_t *action)
{
#ifdef HAVE_SIGACTION
    (void) sigaction(sig,
              (struct sigaction *) 0,
              action);
#else
#ifdef HAVE_SIGVEC
    (void) sigvec(sig, (struct sigvec *) 0, action);
#else
    /* no flags for signal() */
#endif
#endif
}

static void
_install_handler(int sig, sig_action_t *action)
{
    sig_action_t	act;
    int			res;

    act.sa_handler = _sigsegv_handler;
#ifdef HAVE_SIGACTION
    (void) sigemptyset(&act.sa_mask);
#ifdef SA_RESETHAND
    act.sa_flags = action->sa_flags & ~SA_RESETHAND;
#else
    act.sa_flags = action->sa_flags;
#endif
    res = sigaction(sig, &act, (struct sigaction *) 0);
#else
#ifdef HAVE_SIGVEC
    act.sa_mask = 0;
    act.sa_flags = action->sa_flags & ~SV_RESETHAND;
    res = sigvec(sig, &act, (struct sigvec *) 0);
#else
    action->sa_handler = (RETSIGTYPE (*)()) signal(sig, act.sa_handler);
#endif
#endif
    if (res < 0)
	exit(-1);
}


static void
_restore_handler(int sig, sig_action_t *action)
{
    int		res;

#ifdef HAVE_SIGACTION
    res = sigaction(sig, action, (struct sigaction *) 0);
#else
#ifdef HAVE_SIGVEC
    res = sigvec(sig, action, (struct sigvec *) 0);
#else
    res = signal(sig, action->sa_handler);
#endif
#endif
    if (res < 0)
	exit(-1);
}


static void
_addr_map(char *from, char *to, long int increment)
{
    char byte;

    addr_increment = (increment == 0) ? addr_pagesize: increment;
    (void) write(2, "page size = ", 12);
    _write_address((char *) addr_pagesize);
    if (from != (char *) 0 || to != (char *) 0 || increment != 0) {
	(void) write(2, "\nstep      = ", 14);
	_write_address((char *) addr_increment);
	(void) write(2, "\nstart     = ", 14);
	_write_address(from);
	(void) write(2, "\nend       = ", 14);
	_write_address(to);
    }
    (void) write(2, "\n\n", 2);

    _save_handler(SIGSEGV, &sigsegv_act);
    _save_handler(SIGBUS, &sigbus_act);

    _install_handler(SIGSEGV, &sigsegv_act);
    _install_handler(SIGBUS, &sigbus_act);

    (void) setjmp(addr_env);

    while (1) {
	if (to != (char *) 0 && addr + addr_increment > to ||
	     ((unsigned long) (addr - (char *) 0)) > 
		      ((unsigned long) ((addr + addr_increment)  - (char *) 0))) {
	    _write_range(addr_range_addr, (char *) addr + addr_increment, addr_state);

	    _restore_handler(SIGSEGV, &sigsegv_act);
	    _restore_handler(SIGBUS, &sigbus_act);
	    if (addr_flags & ADDR_FLAGS_SHARED) {
		(void) close(addr_map_fd);
		(void) unlink("heap.map");
	    }
	    return;
	}
	byte = *addr;
	if (addr_state != ADDR_MAPPED) {
	    if (addr_range_addr != addr)
		_write_range(addr_range_addr, (char *) addr, addr_state);
	    addr_range_addr = (char *) addr;
	    addr_state = ADDR_MAPPED;
	}
        addr += addr_increment;
    }
}

/* Output the address without allocating any memory */
static void
_write_address(char *a)
{
    char	buf[2*sizeof(char *)];
    int		i;
    int		c;
    int		n = 2*sizeof(char *);

    for (i = 0; i < n; i++) {
	c = ((unsigned long) a & ((unsigned long) 0xf << 4 * (n - i - 1))) >> 4 * (n - i - 1);
	if (c < 10)
	    buf[i] = c + '0';
	else
	    buf[i] = c - 10 + 'a';
    }
    (void) write(2, buf, n);
}

static void
_write_range(char *from, char *to, int state)
{
    unsigned long size = (to-from);
    _write_address(from);
    (void) write(2, " <-> ", 5);
    _write_address(to);
    (void) write(2, " (", 2);
    _write_address((char*) size);
    (void) write(2, ") ", 2);
    switch (state) {
    case ADDR_FREE:
	(void) write(2, "FREE\n", 5);
	break;

    case ADDR_FREE_VAR:
	(void) write(2, "FREE for MAP_VARIABLE\n", 22);
	break;

    case ADDR_MAPPED:
	(void) write(2, "MAPPED\n", 7);
	break;

    case ADDR_UNAVAILABLE:
	(void) write(2, "UNAVAILABLE\n", 12);
	break;

    }
}
#endif	/* HAVE_MMAP */
