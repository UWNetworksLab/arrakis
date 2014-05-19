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
 * Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, ECRC.
 * 
 * END LICENSE BLOCK */

/*---------------------------------------------------------------------
 * IDENTIFICATION	private_mem.c
 *
 * VERSION		$Id: private_mem.c,v 1.2 2007/07/03 00:10:25 jschimpf Exp $
 *
 * AUTHOR		Joachim Schimpf
 *
 * CONTENTS		- Private memory allocation
 *---------------------------------------------------------------------*/

#include	"config.h"
#ifdef HAVE_UNISTD_H
#include	<unistd.h>
#endif
#ifdef HAVE_WINDOWS_H
#include	"windows.h"
#endif

#ifdef HAVE_MMAP
#define USE_MMAP
#endif

#ifdef USE_MMAP

#include <sys/mman.h>
#include <fcntl.h>	/* for O_RDWR */

#ifndef MAP_FAILED
#define MAP_FAILED ((void *) -1)
#endif

#ifdef MAP_ANONYMOUS
#define HAVE_MAP_ANONYMOUS
#else
#ifdef MAP_ANON
#define MAP_ANONYMOUS MAP_ANON
#define HAVE_MAP_ANONYMOUS
#else
#define MAP_ANONYMOUS 0
#endif
#endif

#else

#ifdef SBRK_UNDEF
extern char *sbrk();
#endif

#endif


#include	"memman.h"


struct prmem {
	/* first word reserved to be compatible with shared memory's
	 * application_header
	 */
	generic_ptr first_word_in_heap;
	struct page_admin pages;
	struct heap heap;
};

struct heap_descriptor private_heap;


/*---------------------------------------------------------------------
 * Primitive allocation function for private memory
 *---------------------------------------------------------------------*/

/*ARGSUSED*/
static generic_ptr
_private_sbrk(word size, int align, struct heap_descriptor *hd)
{
#ifdef HAVE_WINDOWS_H
    generic_ptr address = VirtualAlloc(NULL,size,MEM_COMMIT,PAGE_READWRITE);
    return address ? address : (generic_ptr) -1;
#else
#ifdef USE_MMAP
    generic_ptr address = mmap((void*) 0, size,
	    PROT_READ|PROT_WRITE,
	    MAP_ANONYMOUS|MAP_PRIVATE,
	    hd->map_fd, (off_t) 0);
    return address == MAP_FAILED ? (generic_ptr) -1 : address;
#else
    register word difference = (word) sbrk(0) % align;
    if (difference)
    {
#ifdef DEBUG_HEAP
    	(void) write(2, "WARNING: misaligned brk\n", 24);
#endif
	(void) sbrk(align - difference);
    }
    return (generic_ptr) sbrk(size);
#endif
#endif
}

static int
_private_release(generic_ptr address, word size, struct heap_descriptor *hd)
{
#ifdef HAVE_WINDOWS_H
    return VirtualFree(address, 0, MEM_RELEASE);
#else
#ifdef USE_MMAP
    return !munmap(address, size);
#else
    return 1;
#endif
#endif
}

char *
private_mem_init_desc(
	void (*panic_fct)(const char*, const char*),
	struct heap_descriptor *hd)
{
    struct prmem *private_memory;
    /* 
     * Set up the private heap
     */
#ifdef USE_MMAP
#ifdef HAVE_MAP_ANONYMOUS
    hd->map_fd = -1;
#else
    hd->map_fd = open("/dev/zero", O_RDWR);
#endif
#endif
    private_memory = (struct prmem *)
	_private_sbrk(sizeof(struct prmem), 1, hd);
    if (private_memory == (struct prmem *) -1)
    	return (char *) -1;

    hd->shared_header = 0;
    hd->pages = &private_memory->pages;
    hd->heap = &private_memory->heap;
    hd->panic = panic_fct;
    hd->more = _private_sbrk;
    hd->less = _private_release;
    pagemanager_init(hd);
    alloc_init(hd);
    return (char *) private_memory;
}

void
private_mem_init(void (*panic_fct)(const char*, const char*))
{
    Disable_Int();
    (void) private_mem_init_desc(panic_fct, &private_heap);
    Enable_Int();
}

void
private_mem_release_desc(struct heap_descriptor *hd)
{
    struct prmem dummy;
    char *prmem_struct_addr;
    pagemanager_fini(hd);
    /* sorry, hack to find lost address of the above allocated structure */
    prmem_struct_addr = (char*)hd->pages - ((char*)&dummy.pages - (char*)&dummy);
    _private_release(prmem_struct_addr, sizeof(struct prmem), NULL);
}

void
private_mem_release()
{
    Disable_Int();
    (void) private_mem_release_desc(&private_heap);
    Enable_Int();
}


/*---------------------------------------------------------------------
 * Private heap interface
 *---------------------------------------------------------------------*/

/* 
 * Header-less allocation
 */

generic_ptr
hp_alloc_size(word bytes_needed)
{
    return alloc_size(&private_heap, bytes_needed);
}

void
hp_free_size(generic_ptr ptr, word size)
{
    free_size(&private_heap, ptr, size);
}

generic_ptr 
hp_realloc_size(generic_ptr ptr, word oldsize, word newsize)
{
    return realloc_size(&private_heap, ptr, oldsize, newsize);
}


/*---------------------------------------------------------------------
 * Allocate memory that remembers its own size
 * We need a double header for alignment.
 * It gives us also space for a magic number.
 *---------------------------------------------------------------------*/

generic_ptr
hp_alloc(word size)
{
    return h_alloc(&private_heap, size);
}

void
hp_free(generic_ptr ptr)
{
    h_free(&private_heap, ptr);
}

generic_ptr
hp_resize(generic_ptr ptr, word newsize)
{
    return h_realloc(&private_heap, ptr, newsize);
}


/*---------------------------------------------------------------------
 * Debugging and statistics
 *---------------------------------------------------------------------*/

int
hp_statistics(int what)
{
    return alloc_statistics(&private_heap, what);
}

