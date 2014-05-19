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
 * Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, ECRC.
 * 
 * END LICENSE BLOCK */

/*---------------------------------------------------------------------
 * IDENTIFICATION	shared_mem.c
 *
 * VERSION		$Id: shared_mem.c,v 1.2 2007/07/03 00:10:25 jschimpf Exp $
 *
 * AUTHOR		Joachim Schimpf
 *
 * CONTENTS		Shared memory allocation
 *---------------------------------------------------------------------*/

#include "config.h"

#include <sys/types.h>

//asq:
#include <stdint.h>
#include <barrelfish_kpi/types.h>

#ifdef HAVE_MMAP
#include <sys/mman.h>
#endif
#include <sys/stat.h>
//#include <fcntl.h>
#include <errno.h>

#include "memman.h"

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

#ifdef HAVE_UNISTD_H
#include	<unistd.h>
#endif

#ifdef SBRK_UNDEF
extern char	*sbrk();
#endif

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


#ifdef HAVE_STRING_H
#include <string.h>
#else
extern char	*strcpy();
#endif

static generic_ptr shared_sbrk(word size, int align, struct heap_descriptor *hd);
static int shared_release(generic_ptr address, word size, struct heap_descriptor *hd);


/*---------------------------------------------------------------------
 * Shared memory
 *---------------------------------------------------------------------*/

#undef KB
#undef MB
#define MB			1048576
#define KB			1024
#undef VIRTUAL_SHARED_DEFAULT
#define VIRTUAL_SHARED_DEFAULT	(64*MB)
#define MAP_INCREMENT_DEFAULT	(16*KB)
#define RoundTo(n,unit)		((n) - ((n) - 1) % (unit) -1 + (unit))


#ifdef HAVE_MMAP
char *
shared_mem_init(
	int create,			/* create/attach shared memory flag */
	char *mapfile,			/* file to map to (NULL:private on swap) */
	char *start_shared_area,	/* start address for mapping (or NULL) */
	word size,			/* maxium size in bytes (0:automatic) */
	word incr,			/* mapping increment (multiple of pagesize) */
	void (*panic_func)(const char*, const char*),		/* function to call when out of memory */
	struct heap_descriptor *hd)	/* output: heap descriptor */
{
    hd->panic = panic_func;
    hd->more = shared_sbrk;
    hd->less = shared_release;

    if (!size) size = VIRTUAL_SHARED_DEFAULT;
    if (!incr) incr = MAP_INCREMENT_DEFAULT;
    if (!mapfile) create = 1;

    if (create)		/* setup shared memory */
    {
	/* With shared memory we map everything at once to avoid
	 * synchronisation problems when extending the mapping.
	 * When mapping to /dev/zero we extend the mapping incrementally
	 * otherwise a lot of swap space is reserved
	 */
#if (defined(_PA_RISC1_0) || defined(_PA_RISC1_1))
	int map_mode = (!mapfile && start_shared_area)? MAP_FIXED: MAP_VARIABLE;
#else
	int map_mode = start_shared_area? MAP_FIXED: MAP_VARIABLE;
#endif
	int map_default = mapfile ? size : incr;
	char *map_result;

	if (!mapfile)	/* no shared memory */
	{
#ifdef MAP_ANONYMOUS
	    hd->map_fd = -1;
#else
	    hd->map_fd = open("/dev/zero", O_RDWR);
#endif
	}
	else
	{
	    /* The file must not exist yet. This is to make sure that
	     * 1. slaves cannot accidentally attach to an old version.
	     * 2. The file is empty and contains zeros after truncating.
	     */
	    hd->map_fd = open(mapfile, O_RDWR|O_CREAT|O_EXCL, 0700);
	}
	if (hd->map_fd == -1
#ifdef MAP_ANONYMOUS
	    && mapfile
#endif
	    )
	{
	    perror("ECLiPSe: can't create shared map file");
	    return (char *) -1;
	}

	if (mapfile && ftruncate(hd->map_fd, (off_t) size) == -1)
	{
	    perror("ECLiPSe: can't extend shared map file");
	    return (char *) -1;
	}
_retry_map_:
	map_result = (char *) mmap(start_shared_area, map_default,
	    PROT_READ|PROT_WRITE|PROT_EXEC,
#ifdef MAP_ANONYMOUS
	    (mapfile? MAP_FILE: MAP_ANONYMOUS)|
#endif
	    (mapfile? MAP_SHARED: MAP_PRIVATE)|
	    MAP_NORESERVE|
	    map_mode,
	    hd->map_fd, (off_t) 0);
	if ((capaddr_t) map_result == MAP_FAILED)
	{
	    if (map_mode == MAP_FIXED)
	    {
		map_mode = MAP_VARIABLE;
		goto _retry_map_;
	    }
	    perror("ECLiPSe: can't map shared memory");
	    return (char *) -1;
	}
	start_shared_area = map_result;
	hd->shared_header = (struct shm_desc *) start_shared_area;
	hd->shared_header->mapfile = mapfile;	/* preliminary, see below */
	hd->shared_header->incr = incr;
	hd->shared_header->start = start_shared_area;
	hd->shared_header->lim = start_shared_area + map_default;
	hd->shared_header->brk = start_shared_area + sizeof(struct shm_desc);
	hd->shared_header->stop = start_shared_area + size;
	a_mutex_init(&hd->shared_header->lock);
	hd->heap = &hd->shared_header->heap;
	hd->pages = &hd->shared_header->pages;
	hd->shared_header->mapfile = mapfile ?
		strcpy(hd->shared_header->mapfile_buf, mapfile) : mapfile;
	pagemanager_init(hd);
	alloc_init(hd);
	hd->shared_header->processes = 1; /* done last, for synchronisation */
    }
    else		/* connect to existing shared memory in mapfile */
    {
	struct stat st;
	struct shm_desc *tmp_header;
	char *lim;

	for(;;)		/* wait for the mapfile to appear */
	{
	    hd->map_fd = open(mapfile, O_RDWR, 0600);
	    if (hd->map_fd != -1)
		break;
	    if (errno != ENOENT)
	    {
		perror("ECLiPSe: can't open shared map file");
		return (char *) -1;
	    }
	    sleep(1);
	}
	do {		/* wait for the mapfile to be non-empty */
	    (void) fstat(hd->map_fd, &st);
	} while (!st.st_size);

	/* preliminarily map admin page to look at the shared_header */
	tmp_header = (struct shm_desc *) mmap((capaddr_t) 0, incr,
	    PROT_READ,
#ifdef MAP_ANONYMOUS
	    MAP_FILE|
#endif
	    MAP_SHARED,
	    hd->map_fd, (off_t) 0);
	if ((capaddr_t) tmp_header == MAP_FAILED)
	{
	    perror("ECLiPSe: can't map shared memory");
	    return (char *) -1;
	}
	while (tmp_header->processes == 0)
	{
	    sleep(1);		/* wait for creator to finish initialisation */
	}
	if (start_shared_area)
	{
	    if (tmp_header->start != start_shared_area)
	    {
		errno = EINVAL;
		perror("ECLiPSe: cannot map shared memory at required address");
		return (char *) -1;
	    }
	}
	else	/* use the start address provided in the mapfile */
	{
	    start_shared_area = tmp_header->start;
	}
	lim = tmp_header->lim;
	(void) munmap((capaddr_t) tmp_header, incr);

	/* Now do the real mapping */
	if ((char *) mmap(start_shared_area,
	    lim - start_shared_area,
	    PROT_READ|PROT_WRITE|PROT_EXEC,
#ifdef MAP_ANONYMOUS
	    MAP_FILE|
#endif
	    MAP_NORESERVE|
	    MAP_SHARED|MAP_FIXED, 
	    hd->map_fd, (off_t) 0) != start_shared_area)
	{
	    perror("ECLiPSe: can't map shared memory");
	    return (char *) -1;
	}
	hd->shared_header = (struct shm_desc *) start_shared_area;
	hd->heap = &hd->shared_header->heap;
	hd->pages = &hd->shared_header->pages;
	a_mutex_lock(&hd->shared_header->lock);
	hd->shared_header->processes++;
	a_mutex_unlock(&hd->shared_header->lock);
    }
    return start_shared_area;
}
#endif /* HAVE_MMAP */

void
shared_mem_release(struct heap_descriptor *hd)
{
    if (!hd->shared_header)
    {
	pagemanager_fini(hd);
	return;
    }
    a_mutex_lock(&hd->shared_header->lock);
    if (--hd->shared_header->processes == 0 && hd->shared_header->mapfile)
    {
	if (unlink(hd->shared_header->mapfile) == -1 && errno != ENOENT)
	{
	    perror("ECLiPSe warning: can't get rid of shared map file");
	}
    }
    a_mutex_unlock(&hd->shared_header->lock);
}


#ifdef HAVE_MMAP
static generic_ptr
shared_sbrk(word size, int align, struct heap_descriptor *hd)
{
    char *p;
    word avail;

    /* already inside the lock
    a_mutex_lock(&hd->shared_header->lock);
    */
    p = hd->shared_header->brk;
    if ((word) p % align)
    {
	p += align - ((word) p % align);
    }
    avail = hd->shared_header->lim - p;
    if (size > avail)
    {
	word needed = RoundTo(size - avail, hd->shared_header->incr);
	if (hd->shared_header->lim + needed > hd->shared_header->stop)
	{
	    errno = ENOMEM;
	    perror("ECLiPSe: shared heap overflow");
	    p = (char *) -1;
	    goto _return_;
	}
	if ((char *) mmap(hd->shared_header->lim, needed,
		PROT_READ|PROT_WRITE|PROT_EXEC,
#ifdef MAP_ANONYMOUS
		(hd->shared_header->mapfile? MAP_FILE: MAP_ANONYMOUS)|
#endif
		(hd->shared_header->mapfile? MAP_SHARED: MAP_PRIVATE)|MAP_FIXED, 
		hd->map_fd,
		(off_t) (hd->shared_header->lim - hd->shared_header->start))
	    != hd->shared_header->lim)
	{
	    perror("ECLiPSe: can't map more shared memory");
	    p = (char *) -1;
	    goto _return_;
	}
	hd->shared_header->lim += needed;
    }
    hd->shared_header->brk = p + size;
_return_:
    /*
    a_mutex_unlock(&hd->shared_header->lock);
    */
    return (generic_ptr) p;
}

static int
shared_release(generic_ptr address, word size, struct heap_descriptor *hd)
{
    return 1;
}
#endif /* HAVE_MMAP */


/*---------------------------------------------------------------------
 * Save and restore
 * They return -1 and set errno on error, otherwise 0
 *---------------------------------------------------------------------*/

int
shared_mem_save(struct heap_descriptor *hd, int fd)
{
    if (!hd->shared_header)
    {
	errno = EINVAL;
	return -1;
    }
    return write(fd, hd->shared_header->start,
	hd->shared_header->brk - hd->shared_header->start);
}

int
shared_mem_restore(struct heap_descriptor *hd, int fd)
{
#ifdef HAVE_MMAP
    char mapfile_buf[MAX_PATH_LEN];
    char *start;
    char *lim;					/* current mapping limit */
    int processes;
    char *mapfile;
#endif

    if (!hd->shared_header)
    {
	errno = EINVAL;
	return -1;
    }

#ifdef HAVE_MMAP
    start = hd->shared_header->start;
    lim = hd->shared_header->lim;
    processes = hd->shared_header->processes;
    mapfile = hd->shared_header->mapfile;
    if (mapfile)
	(void) strcpy(mapfile_buf, hd->shared_header->mapfile);

    /* read the shared memory descriptor */
    if (read(fd, start, sizeof(struct shm_desc)) != sizeof(struct shm_desc))
	return -1;

    if (start != hd->shared_header->start)
    {
	errno = EINVAL;
	return -1;
    }
    hd->shared_header->processes = processes;
    hd->shared_header->mapfile = mapfile ?
	    strcpy(hd->shared_header->mapfile_buf, mapfile_buf) : mapfile;

    if (mapfile)			/* Adjust the memory mapping */
    {
	/* with shared memory map everything at once */
	hd->shared_header->lim = hd->shared_header->stop;
    }
    if (hd->shared_header->lim > lim)
    {
	if ((char *) mmap(lim,
	    hd->shared_header->lim - lim,
	    PROT_READ|PROT_WRITE|PROT_EXEC,
#ifdef MAP_ANONYMOUS
	    (mapfile? MAP_FILE: MAP_ANONYMOUS)|
#endif
	    MAP_NORESERVE|
	    (mapfile? MAP_SHARED: MAP_PRIVATE)|MAP_FIXED, 
	    hd->map_fd, (off_t) 0) != lim)
	{
	    return -1;
	}
    }
    else if (hd->shared_header->lim < lim)
    {
	if (munmap(hd->shared_header->lim, lim - hd->shared_header->lim) < 0)
	{
	    return -1;
	}
    }

    /* Now read the rest of the data and return */
    return read(fd, start + sizeof(struct shm_desc),
		hd->shared_header->brk - start - sizeof(struct shm_desc));
#endif /* HAVE_MMAP */
}

/*---------------------------------------------------------------------
 * Auxliliary functions
 *---------------------------------------------------------------------*/

/*
 * Check whether a given address is in this heap
 */

int
address_in_heap(struct heap_descriptor *hd, generic_ptr ptr)
{
    if (!hd->shared_header)
	return 0;
    return ptr >= (generic_ptr) hd->shared_header->start
	&& ptr <  (generic_ptr) hd->shared_header->brk;
}
