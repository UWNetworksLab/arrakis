
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
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * SEPIA C SOURCE MODULE
 *
 * VERSION	$Id: mem.c,v 1.1 2008/06/30 17:43:57 jschimpf Exp $
 */

/*
* IDENTIFICATION	mem.c
*
* AUTHOR		Joachim Schimpf (complete rewrite)
*
* DESCRIPTION		Basic memory management
*
* CONTENTS
*
*		INITIALISATION
*				mem_init()
*
*		MAPPED STACKS
*				alloc_stack_pairs()
*				adjust_stacks()
*
*		ABSTRACT DATA TYPES
*				unbounded buffer
*				unbounded stack
*				temporary area
*
*		C LIBRARY REPLACEMENTS
*                               malloc()
*                               realloc()
*                               free()
*                               cfree()
*                               calloc()
*/

#include <string.h>

#include	"config.h"
#ifdef _WIN32
#include <windows.h>
#endif
#include	"sepia.h"
#include	"types.h"
#include	"embed.h"
#include	"error.h"
#include	"mem.h"
#include	"dict.h"
#include	"io.h"
#include	"emu_export.h"
#include	"os_support.h"

#include	<sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include	<sys/mman.h>
#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif
#endif
//#include	<fcntl.h>
#include	<errno.h>
#include	<stdio.h>

#ifdef HAVE_UNISTD_H
#include	<unistd.h>
#endif

#ifdef STDC_HEADERS
#include	<stdlib.h>
#else
extern void	exit();
#endif

#ifdef SBRK_UNDEF
extern char	*sbrk();
#endif

#if defined(lint) || defined(PRINTAM)
#define DEBUG_HEAP
#endif

#define STACK_PAGESIZE	(32 * system_pagesize)

#define UWORDS_PER_PAGE	(BYTES_PER_PAGE/sizeof(uword))

#define RoundTo(n,unit) ((n) - ((n) - 1) % (unit) -1 + (unit))

#if defined(MAP_ANON) && !defined(MAP_ANONYMOUS)
# define MAP_ANONYMOUS MAP_ANON
#endif

#ifndef MAP_ANONYMOUS
static int stack_map_fd = 0;		/* must be statically initialised */
#endif

uword	*start_of_stacks,	/* only for mapped stacks, otherwise 0 */
	*end_of_stacks;

int	system_pagesize;
int	map_alignment;
int	own_pid;
int	mem_is_initialized = 0;		/*  must be statically initialised */


void
	malloc_init(void);

struct heap_descriptor global_heap;


/*---------------------------------------------------------------------
 * Mapped stacks
 *
 * start_of_stacks must be aligned to the system pagesize. When a saved
 * state is run on machines with different pagesizes, the stack must be
 * aligned to the largest pagesize (map_alignment must be big enough).
 *---------------------------------------------------------------------*/

/*ARGSUSED*/
static void
_unmap_at(char *addr,	/* page-aligned */
	size_t bytes)	/* multiple of pagesize */
{
    switch(ec_options.allocation)
    {
    case ALLOC_VIRTUAL:
    {
#ifdef _WIN32
	if (!VirtualFree(addr,bytes,MEM_DECOMMIT))
	{
	    ec_options.user_panic("failed to free stack space","_unmap_at()");
	}
#else
#ifndef HAVE_MMAP
	return;		/* ALLOC_VIRTUAL needs HAVE_MMAP */
#else
	char *ad;
#ifdef HAVE_MAP_NORESERVE
#if 0
/* The Solaris manual does not clearly specify whether re-mapping an
 * already mapped (and used) area with MAP_NORESERVE really frees the
 * swap space. It seems to work though, so we don't call the unmap().
 */
	if (munmap(addr, bytes) == -1)
	{
	    (void )ec_outfs(current_err_,"INTERNAL WARNING: can't unmap stack page\n");
	    ec_flush(current_err_);
	}
#endif
	ad = mmap((capaddr_t) addr, bytes, PROT_READ|PROT_WRITE,
#ifdef MAP_ANONYMOUS
	    MAP_FIXED|MAP_PRIVATE|MAP_NORESERVE|MAP_ANONYMOUS, -1,
#else
	    MAP_FIXED|MAP_PRIVATE|MAP_NORESERVE, stack_map_fd,
#endif
	    (off_t) 0);
#else
	/* remapping with PROT_NONE should free the swap space */
	ad = mmap((capaddr_t) addr, bytes, PROT_NONE,
#ifdef MAP_ANONYMOUS
	    MAP_FIXED|MAP_PRIVATE|MAP_ANONYMOUS, -1,
#else
	    MAP_FIXED|MAP_PRIVATE, stack_map_fd,
#endif
	    (off_t) 0);
#endif
	if (ad != addr)
	    ec_options.user_panic("failed to re-reserve stack space","_unmap_at()");
#endif
#endif
	break;
    }

    case ALLOC_FIXED:
    {
#if defined(_WIN32) || !defined(HAVE_MMAP)
	ec_panic("ALLOC_FIXED not supported","_map_at()");
#else
	char *ad;
	ad = mmap((capaddr_t) addr, bytes, PROT_NONE,
#ifdef MAP_ANONYMOUS
	    MAP_FIXED|MAP_PRIVATE|MAP_ANONYMOUS, -1,
#else
	    MAP_FIXED|MAP_PRIVATE, stack_map_fd,
#endif
	    (off_t) 0);
	if (ad != addr)
	    ec_options.user_panic("failed to re-reserve stack space","_unmap_at()");
#endif
	break;
    }
    }
}

/*ARGSUSED*/
static int
_map_at(char *addr,	/* page-aligned */
	size_t bytes)	/* multiple of pagesize */
{
    switch(ec_options.allocation)
    {
    case ALLOC_VIRTUAL:
#ifdef _WIN32
	if (!VirtualAlloc(addr,bytes,MEM_COMMIT,PAGE_READWRITE))
	{
	    return 0;
	    /* Could use GetLastError for better info */
	}
#else
#ifndef HAVE_MMAP
	return 0;	/* ALLOC_VIRTUAL needs HAVE_MMAP */
#else
#ifdef HAVE_MAP_NORESERVE
	return 1;	/* pages already mapped PROT_READ|PROT_WRITE */
#else
	if ((char *) mmap(addr, bytes, PROT_READ|PROT_WRITE,
#ifdef MAP_ANONYMOUS
		MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED, -1,
#else
		MAP_PRIVATE|MAP_FIXED, stack_map_fd,
#endif
		(off_t) 0) != addr)
	{
	    if (errno == EAGAIN || errno == ENOMEM)
		/* soft error: we may be able to continue */
		return 0;

	    perror("ECLiPSe: cannot map new stack page");
	    ec_panic(MEMORY_P, (char *) 0);
	}
#endif
#endif
#endif
    	break;

    case ALLOC_FIXED:
#if defined(_WIN32) || !defined(HAVE_MMAP)
	ec_panic("ALLOC_FIXED not supported","_map_at()");
#else
	if ((char *) mmap(addr, bytes, PROT_READ|PROT_WRITE,
#ifdef MAP_ANONYMOUS
		MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED, -1,
#else
		MAP_PRIVATE|MAP_FIXED, stack_map_fd,
#endif
		(off_t) 0) != addr)
	{
	    if (errno == EAGAIN || errno == ENOMEM)
		/* soft error: we may be able to continue */
		return 0;

	    perror("ECLiPSe: cannot map new stack page");
	    ec_panic(MEMORY_P, (char *) 0);
	}
#endif
	break;
    }
    return 1;
}


static void
_report_adjustment(char *change, char *name, word bytes)
{
    if (GlobalFlags & GC_VERBOSE)
    {
	p_fprintf(log_output_,
#if SIZEOF_CHAR_P==SIZEOF_LONG
	    "GC: %s %s stack by %ld bytes\n",
#else
	    "GC: %s %s stack by %d bytes\n",
#endif
	    change, name, bytes);
	ec_flush(log_output_);
    }
}


/*
 * grow and/or shrink the stacks such that the specified
 * stack maxima can be accommodated. 
 *
 * If partition_hint is nonzero, then no gap is left between the two stacks;
 * the gap space is divided somewhere close to partition_hint.
 */
int
adjust_stacks(struct stack_struct *descr, uword *lower_max, uword *upper_max, uword *partition_hint)
{
    register uword diff;

    
    if (diff = (uword) lower_max % STACK_PAGESIZE)
	lower_max += (STACK_PAGESIZE - diff)/sizeof(uword);
	/* lower_max could wrap to 0 here! */
    upper_max -=
	((uword) upper_max % STACK_PAGESIZE)/sizeof(uword);
    
    if (lower_max > upper_max)
	return 0;			/* stacks clash */

    if (partition_hint)
    {
	if (partition_hint >= upper_max)
	    lower_max = upper_max;
	else if (partition_hint <= lower_max)
	    upper_max = lower_max;
	else
	    lower_max = upper_max = partition_hint -
	    	((uword) partition_hint % STACK_PAGESIZE)/sizeof(uword);
    }
    
    /*
     * Now adjust the actual memory mapping to match the new lower_max/
     * upper_max. Shrinking is done first to avoid overlap problems.
     */
    if (upper_max > descr[1].end)	/* must shrink upper stack	*/
    {
	_report_adjustment("shrink", descr[1].name,
		(char*) upper_max - (char*) descr[1].end);
	_unmap_at((char*)descr[1].end, (char*)upper_max - (char*)descr[1].end);
	descr[1].end = upper_max;
    }
    if (lower_max < descr[0].end)	/* must shrink lower stack */
    {
	_report_adjustment("shrink", descr[0].name,
		(char*)descr[0].end - (char*)lower_max);
	_unmap_at((char*)lower_max, (char*)descr[0].end - (char*)lower_max);
	descr[0].end = lower_max;
    }
    if (upper_max < descr[1].end)	/* must grow upper stack	*/
    {
	_report_adjustment("grow", descr[1].name,
		(char*) descr[1].end - (char*) upper_max);
	if (!_map_at((char*) upper_max, (char*) descr[1].end - (char*) upper_max))
	    return 0;
	descr[1].end = upper_max;
	if (upper_max < descr[1].peak)
	    descr[1].peak = upper_max;
    }
    if (lower_max > descr[0].end)	/* must grow lower stack */
    {
	_report_adjustment("grow", descr[0].name,
	        (char*) lower_max - (char*) descr[0].end);
	if (!_map_at((char*) descr[0].end, (char*) lower_max - (char*) descr[0].end))
	    return 0;
	descr[0].end = lower_max;
	if (lower_max > descr[0].peak)
	    descr[0].peak = lower_max;
    }
    return 1;				/* ok */
}


/*
 * PARALLEL eclipse
 * Require fixed positions
 * use ALLOC_FIXED
 *
 * Development eclipse
 * prefer eager deallocation, lazy allocation
 * use ALLOC_FIXED
 *
 * Embedded eclipse
 * require variable position
 * Prefer no deallocation or deallocation from calling program
 * depending on size either eager or lazy alloc
 * if small sizes use ALLOC_PRE else use ALLOC_VIRTUAL
 *
 * There are really 3 independent features involved:
 *
 *	- FIXED_ADDRESS
 *	- EAGER_ALLOC
 *	- EAGER_DEALLOC
 *
 * This corresponds to the current allocation options as follows:
 *
 *		FIXED_ADDRESS	EAGER_ALLOC	EAGER_DEALLOC
 * ALLOC_PRE		-	+		-
 * ALLOC_FIXED		+	-		+
 * ALLOC_VIRTUAL	-	-		+
 */
void
alloc_stack_pairs(int nstacks, char **names, uword *bytes, struct stack_struct **descr)
{
    uword bytes_allocated = 0;
    int npairs = 0;
    uword *stack_base;
    int i;

    for (i=0 ; i<nstacks ; i++)
    {
	if (bytes[i]) /* RoundTo only works for n > 0 */
	    bytes[i] = RoundTo(bytes[i], STACK_PAGESIZE);
	bytes_allocated += bytes[i];
	descr[i]->name = names[i];
    }

    switch(ec_options.allocation)
    {
    case ALLOC_VIRTUAL:
	/* just reserve the address space without grabbing swap */
#ifdef _WIN32
	stack_base = (uword *) VirtualAlloc(NULL,bytes_allocated,MEM_RESERVE,
				PAGE_READWRITE);
	if (NULL == stack_base)
	    ec_panic("Cannot reserve stack space","alloc_stack_pairs()");
#else
#ifdef HAVE_MMAP
    	stack_base = (uword *) mmap((capaddr_t) 0,(size_t) bytes_allocated,
#ifdef HAVE_MAP_NORESERVE
		    PROT_READ|PROT_WRITE,
#else
		    PROT_NONE,
#endif
#ifdef MAP_ANONYMOUS
		    MAP_PRIVATE|MAP_NORESERVE|MAP_ANONYMOUS, -1,
#else
		    MAP_PRIVATE|MAP_NORESERVE, stack_map_fd,
#endif
		    (off_t) 0);
	if (stack_base == (uword *) -1)
	    ec_panic("Cannot reserve stack space","alloc_stack_pairs()");
#else
	ec_panic("ALLOC_VIRTUAL not supported","alloc_stack_pairs()");
#endif
#endif
	break;

    case ALLOC_PRE:
	/* allocate and get swap space now */
#ifdef _WIN32
	stack_base = (uword *) VirtualAlloc(NULL,bytes_allocated,MEM_COMMIT,
				PAGE_READWRITE);
	if (NULL == stack_base)
	    ec_panic("Cannot allocate stack space","alloc_stack_pairs()");
#else
#ifdef HAVE_MMAP
    	stack_base = (uword *) mmap((capaddr_t) 0,(size_t) bytes_allocated,
		    PROT_READ|PROT_WRITE,
#ifdef MAP_ANONYMOUS
		    MAP_PRIVATE|MAP_ANONYMOUS, -1,
#else
		    MAP_PRIVATE, stack_map_fd,
#endif
		    (off_t) 0);
#else
    	stack_base = (uword *) sbrk(bytes_allocated);
#endif
	if (stack_base == (uword *) -1)
	    ec_panic("Cannot allocate stack space","alloc_stack_pairs()");
#endif
	break;

    case ALLOC_FIXED:
#if defined(_WIN32) || !defined(HAVE_MMAP)
	ec_panic("ALLOC_FIXED not supported","alloc_stack_pairs()");
#else
	/* fix position allocate later */
    	stack_base = end_of_stacks;
	end_of_stacks += bytes_allocated / sizeof(uword);
	if ((uword) stack_base % STACK_PAGESIZE)
	    ec_panic("Cannot allocate stacks (not page-aligned)","alloc_stack_pairs()");
#endif
	break;

    default:
	ec_panic("Cannot allocate stacks (unknown allocation option)","alloc_stack_pairs()");
    }

    for (i=0 ; i<nstacks ; i++)
    {
	descr[i]->start = descr[i]->end = descr[i]->peak = stack_base;
	stack_base += bytes[i]/sizeof(uword);
    }
}


void
dealloc_stack_pairs(struct stack_struct *lower, struct stack_struct *upper)
{
    generic_ptr addr = lower[0].start;
    uword bytes = (char*) upper[1].start - (char*) addr;

    switch(ec_options.allocation)
    {
    case ALLOC_VIRTUAL:
    case ALLOC_PRE:
#if _WIN32
	if (!VirtualFree(addr, 0 /*!*/, MEM_RELEASE))
	{
	    ec_options.user_panic("failed to release stacks","dealloc_stack_pair()");
	}
#else
#ifdef HAVE_MMAP
	if (munmap(addr, (size_t) bytes) != 0)
	{
	    ec_options.user_panic("failed to release stacks","dealloc_stack_pair()");
	}
#endif
#endif
	break;
    case ALLOC_FIXED:
	/* cannot deallocate */
	break;
    }
}


/*---------------------------------------------------------------------
 * Abstract data type "Unbounded Stack"
 *
 * This is an abstract data type implementing a stack of arbitrary,
 * mixed size objects (in units of uword). An object that is pushed
 * with a single stack_push() call is guaranteed to be stored in
 * a consecutive memory block, but separately pushed objects may not
 * be adjacent (the stack is segmented). Therfore, care must be taken
 * when using the stack_top() operation. You may only specify offsets
 * from the stack top that correspond to the size of the top object.
 * The stacks will never be relocated, i.e. a pushed object will remain
 * on its place. It is also legal to remember the value of the stack
 * pointer and compare tyhe current stack top to a remembered value.
 * However, there is no fixed relation between the addresses, only
 * equality may be tested.
 *
 * These stacks are not interrupt safe, since they are often
 * used locally only. If such a stack reference is accessible via
 * a global variable, and possibly used reentrantly, the calls to
 * the stack routines must be surrounded by interrupt locks.
 *
 * The stack handle is a pointer to the stack segment that contains
 * the current stack top.
 *
 * CAUTION: The stack handle is overwritten by some stack operations!
 *          It must be passed by reference!
 *---------------------------------------------------------------------*/

static struct stack_header *
_stack_init(struct stack_header *down, struct stack_header *up, uword words_needed)
{
    struct stack_header *stack;
    word bytes_allocated;
    Disable_Int();
    stack = (struct stack_header *) alloc_pagewise(&private_heap,
		words_needed*sizeof(uword) + sizeof(struct stack_header),
		&bytes_allocated);
    Enable_Int();
    stack->top = stack->limit = (uword *) stack + bytes_allocated/sizeof(word);
    stack->down = down;
    stack->up = up;
    return stack;
}

void
stack_create(struct stack_header **pstack, uword words_needed)
{
    *pstack =
	_stack_init((struct stack_header *)0, (struct stack_header *)0, words_needed);
}

int
stack_empty(struct stack_header **pstack)
{
    return (*pstack)->top == (*pstack)->limit;
}

void
stack_push(register struct stack_header **pstack, uword words_needed)
{
    register struct stack_header *stack = *pstack;

/*  stack->top -= words_needed;		is done in the Stack_Push() macro */

    if (stack->top < (uword *)(stack + 1))
    {
	stack->top += words_needed;		/* pop the wrong allocation */
	if (stack->up)
	    *pstack = stack = stack->up;	/* go to the next segment */
	    /* missing: make sure it fits */
	else					/* allocate a new segment */
	    *pstack = stack = stack->up = _stack_init(stack, stack->up, words_needed);

	stack->top -= words_needed;
    }
}

void
stack_pop_to(register struct stack_header **pstack, uword *old_top)
{
    register struct stack_header *stack = *pstack;

    while (old_top < (uword *)(stack + 1) || stack->limit <= old_top)
	*pstack = stack = stack->down;
    stack->top = old_top;
}

void
stack_pop(register struct stack_header **pstack, uword word_offset)
{
    register struct stack_header *stack = *pstack;

#ifdef lint
    stack->top += word_offset;		/* is done in the Stack_Pop() macro */
#endif
    if (stack->top > stack->limit)
    {
	p_fprintf(current_err_,
		"INTERNAL ERROR: stack_pop() crosses segment boundary\n");
	ec_flush(current_err_);
	stack->top = stack->limit;
    }
    /* else (stack->top == stack->limit) */
    if (stack = stack->down)
	*pstack = stack;
}

void
stack_destroy(register struct stack_header **pstack)
{
    register struct stack_header *stack = *pstack;
    register struct stack_header *next;

    while (stack->up)
	stack = stack->up;
    do {
	next = stack->down;
	Disable_Int();
	free_pages(&private_heap, (generic_ptr) stack,
			(stack->limit - (uword *) stack)/UWORDS_PER_PAGE);
	Enable_Int();
    } while (stack = next);
    *pstack = (struct stack_header *) 0;
}


/*---------------------------------------------------------------------
 * Temporary memory allocation
 * - bytewise, no automatic alignment
 *---------------------------------------------------------------------*/

static struct temp_header *
_temp_init(struct temp_header *first, struct temp_header *next, uword bytes_needed)
{
    struct temp_header *temp;
    word bytes_allocated;
    Disable_Int();
    temp = (struct temp_header *) alloc_pagewise(&private_heap,
		bytes_needed + sizeof(struct temp_header),
		&bytes_allocated);
    Enable_Int();
    temp->top = (char *) (temp + 1);
    temp->limit = (char *) temp + bytes_allocated;
    temp->first = first ? first : temp;
    temp->next = next;
    return temp;
}

void
temp_create(struct temp_header **ptemp, uword bytes_needed)
{
    *ptemp =
	_temp_init((struct temp_header *)0, (struct temp_header *)0, bytes_needed);
}

char *
temp_alloc(struct temp_header **ptemp, uword bytes_needed)
{
    register struct temp_header *temp = *ptemp;

    if ((temp->top += bytes_needed) > temp->limit)
    {
	temp->top -= bytes_needed;	/* undo the wrong allocation */
	if (temp->next &&
		(char *)(temp->next + 1) + bytes_needed <= temp->next->limit)
	{
	    /* there is a next block and it's big enough */
	    temp = temp->next;
	    temp->top = (char *)(temp + 1);	/* reinit */
	}
	else	/* insert a new block of sufficient size */
	{
	    temp = temp->next = _temp_init(temp->first, temp->next, bytes_needed);
	}
	*ptemp = temp;
	temp->top += bytes_needed;
    }
    return temp->top - bytes_needed;
}

void
temp_align(struct temp_header **ptemp, uword size)
{
    uword mod = (uword)(*ptemp)->top % size;
    if (mod)
	(void) temp_alloc(ptemp, size - mod);
}

void
temp_reset(struct temp_header **ptemp)
{
    register struct temp_header *temp = (*ptemp)->first;

    temp->top = (char *)(temp + 1);	/* reinit */
    *ptemp = temp;
}

void
temp_destroy(struct temp_header **ptemp)
{
    register struct temp_header *temp = (*ptemp)->first;
    register struct temp_header *next;

    do {
	next = temp->next;
	Disable_Int();
	free_pages(&private_heap, (generic_ptr) temp, (temp->limit - (char *) temp)/BYTES_PER_PAGE);
	Enable_Int();
    } while (temp = next);
    *ptemp = (struct temp_header *) 0;
}

/*---------------------------------------------------------------------
 * Abstract data type "Unbounded Buffer for mixed-size objects".
 * Memory is in units of uword.
 * 
 * Interface:
 * 
 * Buf_Declare(bufdesc)		declare a buffer descriptor and ...
 * register uword *rd, *wr;	... a read and a write pointer.
 * 
 * Buf_Create(bufdesc, 1000)	create a buffer with at least 1000 words
 *
 * Buf_Reinit(bufdesc)		empty and reinitialise the buffer
 *
 * Buf_Destroy(bufdesc)		destroy the buffer, free all memory blocks
 * 
 * wr = BufWriteZ(bufdesc)	set the write pointer to the buffer end
 * 
 * rd = BufReadA(bufdesc)	set the read pointer to the buffer start
 * rd = BufReadZ(bufdesc)	set the read pointer to the buffer end
 * rd = BufReadW(bufdesc, wr)	set the read pointer to the write pointer position
 *
 * Buf_Set_Read(bufdesc, rd)	(re)set the read pointer to rd
 * 
 * Buf_Alloc(bufdesc, wr, 123)	make sure there are 123 consecutive words at wr.
 * 				wr may be changed by this operation.
 * 
 * Buf_Flush(bufdesc, wr)	set the buffer end to wr, needed only before
 *				BufReadZ() or BufWriteZ()
 * 
 * Buf_Check(bufdesc, rd)	make sure that rd points to readable data,
 * 				update rd if necessary. This is only to skip the
 *				block boundaries of the buffer, the user must make
 *				sure that s/he never reads behind the write pointer!
 *
 * BufPos(bufdesc, ptr)		return the index of the buffer word that
 *				ptr points to (starting from 0)
 *
 * NOTE: 	Words that have been allocated with a single Buf_Alloc() are
 *		consecutive and _can_ be read without calling Buf_Check() in
 *		between the words. Buf_Check() _must_ be called at all positions
 *		that correspond to boundaries of Buf_Alloc'd blocks (because they
 *		may not be consecutive) !
 * 
 *---------------------------------------------------------------------*/

static struct buffer_block_header *
_buffer_init(struct buffer_block_header *first, struct buffer_block_header *next,
	uword words_needed, int *is_nonpage_buffer)
{
    word	bytes_allocated;
    struct buffer_block_header	*block;

#ifdef BUFFER_TEST
    bytes_allocated = words_needed*sizeof(uword) + sizeof(struct buffer_block_header);
    block = (struct buffer_block_header *) hp_alloc(bytes_allocated);
#else
    Disable_Int();
    block = (struct buffer_block_header *) alloc_pagewise(&private_heap,
	words_needed*sizeof(uword) + sizeof(struct buffer_block_header),
	&bytes_allocated);
    Enable_Int();
#endif
    block->first = first ? first : block;
    block->next = next;
    block->top = (uword *) (block + 1);
    block->limit = (uword *) block + bytes_allocated/sizeof(uword);
    *is_nonpage_buffer =
    	((uword) block & (BYTES_PER_PAGE-1))	/* not page-aligned */
	|| (bytes_allocated != BYTES_PER_PAGE);	/* not page-sized */
    return block;
}

void
buffer_create(unbounded_buffer *bd, uword minwords)
{
    bd->read_block =
    bd->write_block = _buffer_init((struct buffer_block_header *) 0,
				    (struct buffer_block_header *) 0,
				    minwords, &bd->has_nonpage_buffers);
    bd->write_block_end = bd->write_block->limit;
}

void
buffer_reinit(unbounded_buffer *bd)
{
    bd->read_block =
    bd->write_block = bd->write_block->first;
    bd->write_block_end = bd->write_block->limit;
    bd->write_block->top = (uword *) (bd->write_block + 1);
}

uword *
buffer_alloc(unbounded_buffer *bd, uword *ptr, uword words)
{
    register struct buffer_block_header *block;
#ifdef DEBUG_HEAP
   if (bd->write_block->limit < ptr)
   {
	p_fprintf(current_err_,
		"INTERNAL ERROR: out of range pointer in buffer_alloc()\n");
	ec_flush(current_err_);
    }
#endif
    Buf_Flush(*bd, ptr);
    block = bd->write_block->next;
    if (block && (block->limit - (uword *) (block + 1) >= words))
    {
	/* there is already a block and it's big enough */
	block->top = (uword *) (block + 1);	/* reinit */
    }
    else
    {
	int is_nonpage_buffer;
	block = _buffer_init(bd->write_block->first, bd->write_block->next,
				words, &is_nonpage_buffer);
	bd->write_block->next = block;
	bd->has_nonpage_buffers |= is_nonpage_buffer;
    }
    bd->write_block = block;
    bd->write_block_end = block->limit;
    return block->top;
}

uword *
buffer_next(unbounded_buffer *bd, uword *ptr)
{
    /* precondition: ptr == bd->read_block->top
     * We assume it's ok when read_block == write_block,
     * we cannot check since it may not be flushed
     */
    while (bd->read_block != bd->write_block && ptr == bd->read_block->top)
    {
	bd->read_block = bd->read_block->next;
	ptr = (uword *)(bd->read_block + 1);
    }
    return ptr;
}

void
buffer_setread(unbounded_buffer *bd, uword *ptr)
{
    register struct buffer_block_header *block;
    if (bd->has_nonpage_buffers)
    {
	/* search for the block which contains ptr */
	block = bd->read_block->first;
	while (block && (ptr < (uword *)(block + 1) || block->limit < ptr))
	    block = block->next;
    }
    else
    {
	/* simply round ptr down to the next page boundary */
	block = (struct buffer_block_header *) ((uword) ptr & ~(BYTES_PER_PAGE-1));
    }

#ifdef DEBUG_HEAP
   if (!block || block->first != bd->read_block->first)
   {
	p_fprintf(current_err_,
		"INTERNAL ERROR: out of range pointer in buffer_setread()\n");
	ec_flush(current_err_);
    }
#endif
    bd->read_block = block;
}

void
buffer_destroy(unbounded_buffer *bd)
{
    struct buffer_block_header *this, *next;

    this = bd->write_block->first;
    do {
	next = this->next;
#ifdef BUFFER_TEST
	hp_free((uword *) this);
#else
	Disable_Int();
	free_pages(&private_heap, (generic_ptr) this,
		(this->limit - (uword *) this)/UWORDS_PER_PAGE);
	Enable_Int();
#endif
    } while (this = next);
    bd->write_block = bd->read_block = (struct buffer_block_header *) 0;
    bd->write_block_end = (uword *) 0;
}

uword
buffer_pos(unbounded_buffer *bd, uword *ptr)
{
    struct buffer_block_header *block = bd->write_block->first;
    register uword length = 0;
    while (/* block && */ (ptr < (uword *)(block + 1) || block->limit < ptr))
    {
	length += block->top - (uword *)(block + 1);
	block = block->next;
    }
    return length + (ptr - (uword *)(block + 1));
}


/*---------------------------------------------------------------------
 * Define some "malloc" functions so that we don't have to link
 * the standard C library memory allocator in most cases.
 *---------------------------------------------------------------------*/

#if 0
#ifndef lint

char           *
malloc(unsigned n)
{
    if (!mem_is_initialized)
	malloc_init();
   return (char *) hp_alloc(n);
}

free(char *ptr)
{
    if (ptr != NULL)
	hp_free((generic_ptr) ptr);
}

char           *
realloc(char *ptr, unsigned n)
{
    if (ptr != NULL)
	return (char *) hp_resize((generic_ptr) ptr, (word) n);
    else
	return malloc(n);
}

cfree(char *ptr)
{
   hp_free((generic_ptr) ptr);
}

char           *
calloc(unsigned n, unsigned size)
{
    int			nb = n * size;
    char		*start;
    register long	*p;
    register long	*end;

    if (!mem_is_initialized)
	    malloc_init();
    p = (long *) hp_alloc(nb);
    start = (char *) p;
    end = p + nb/sizeof(long);
    while (p < end)
	*p++ = 0;
    return (start);
}

#endif /* lint */
#endif


/*---------------------------------------------------------------------
 * Simplified shared heap interface
 *---------------------------------------------------------------------*/

/* 
 * Header-less allocation
 */

generic_ptr
hg_alloc_size(word bytes_needed)
{
    return alloc_size(&global_heap, bytes_needed);
}

void
hg_free_size(generic_ptr ptr, word size)
{
    free_size(&global_heap, ptr, size);
}

generic_ptr 
hg_realloc_size(generic_ptr ptr, word oldsize, word newsize)
{
    return realloc_size(&global_heap, ptr, oldsize, newsize);
}

generic_ptr 
hg_alloc(word size)
{
    return h_alloc(&global_heap, size);
}

void
hg_free(generic_ptr ptr)
{
    h_free(&global_heap, ptr);
}

generic_ptr
hg_resize(generic_ptr ptr, word newsize)
{
    return h_realloc(&global_heap, ptr, newsize);
}


int
hg_statistics(int what)
{
    return alloc_statistics(&global_heap, what);
}


/*---------------------------------------------------------------------
 * Debugging and statistics
 *---------------------------------------------------------------------*/

int
p_heap_stat(value vwhat, type twhat, value vval, type tval)
{
    pword result;
    Check_Integer(twhat);
    result.tag.kernel = TINT;
    switch(vwhat.nint)
    {
    case 0:	/* shared allocated */
	result.val.nint = hg_statistics(HEAP_STAT_ALLOCATED);
	break;
    case 1:	/* shared used */
	result.val.nint = hg_statistics(HEAP_STAT_USED);
	break;
    case 2:	/* private allocated */
	result.val.nint = hp_statistics(HEAP_STAT_ALLOCATED);
	break;
    case 3:	/* private used */
	result.val.nint = hp_statistics(HEAP_STAT_USED);
	break;
    default:
	Fail_;
    }
    Return_Unify_Pw(vval, tval, result.val, result.tag);
}

/*---------------------------------------------------------------------
 * Initialisation
 *
 * We create the following memory layout:
 *
 *	0:
 *		<don't care>
 *	brk:
 *		<space for brk to grow (>= ec_options.privatesize bytes)>
 *		<optional gap>
 *	
 *	start_of_stacks:
 *		<global ... trail stack (ec_options.globalsize bytes)>
 *		<local ... control stack (ec_options.localsize bytes)>
 *	end_of_stacks:
 *		<optional gap (on some machines)>
 *
 *	shared_mem_base:
 *		<space for other shared memories (SHARED_MEM_OFFSET_HEAP bytes)>
 *	start_shared_heap:
 *		<shared heap ( ec_options.sharedsize bytes)>
 *
 *		<don't care>
 *	0xffffffff:
 *---------------------------------------------------------------------*/

void
mem_layout(void)
{
    char	*s ;
#ifndef _WIN32
    char	*sh = shared_mem_base();
#endif
    word	map_alignment;
    word	offset;

    fprintf(stderr, "\nAddress Space Layout");
    fprintf(stderr,  "\n====================\n\n");

    fprintf(stderr,    "hostarch     = %s", HOSTARCH);
#ifndef _WIN32
    fprintf(stderr,  "\nsbrk(0)      = 0x%08lx", (unsigned long int)sbrk(0));
#ifdef STACK_BASE
    s = (char *) STACK_BASE;
#else
    s = sbrk(0) + ec_options.privatesize;
    map_alignment = STACK_PAGESIZE > 16*KB ? STACK_PAGESIZE : 16*KB;
    if (offset = (uword) s % map_alignment)
	s += (map_alignment - offset);	/* round up */
    s += ec_options.privatesize;
#endif
    fprintf(stderr,  "\nstacks start = 0x%08lx", (unsigned long int)s);
    s += RoundTo(ec_options.localsize + ec_options.globalsize, STACK_PAGESIZE);
    fprintf(stderr,  "\nstacks end   = 0x%08lx", (unsigned long int)s);
    fprintf(stderr,  "\nshared_base  = 0x%08lx", (unsigned long int)sh);
    sh += SHARED_MEM_OFFSET_HEAP + ec_options.sharedsize;
    fprintf(stderr,  "\nshared end   = 0x%08lx\n", (unsigned long int)sh);
#endif
}

//asq:
//renamed from mem_init(), because it has a conflict with lwip's mem_init()
//which has to be linked to this domaine as well...

void
eclipse_mem_init(int flags)
{
    char *start_shared_heap;

    if (!mem_is_initialized)
	malloc_init();		/* also inits system_pagesize */

    if (flags & INIT_PRIVATE)
    {
	own_pid = getpid();
	/* 
	 * Saved states may be restored on machines with different
	 * pagesizes. Therefore some addresses must be aligned on the
	 * maximum pagesize, e.g. bottom and top of shared heap,
	 * bottom of stacks.
	 * Top of stacks can be page-aligned only because stacks are
	 * deallocated and reallocated on restoring.
	 */
	map_alignment = STACK_PAGESIZE > 16*KB ? STACK_PAGESIZE : 16*KB;

    }

#if !defined(MAP_ANONYMOUS) && defined(HAVE_MMAP)
	if ((stack_map_fd = open("/dev/zero", O_RDWR)) == -1)
	{
	    stack_map_fd = 0;
	    perror("ECLiPSe: Can't open /dev/zero");
	    ec_panic("Can't map stacks" , "eclipse_mem_init");
	}
#endif

    /* 
     * partition the address space above brk for shared heap and stacks
     */

    if (ec_options.allocation == ALLOC_FIXED)
    {
#if HAVE_MMAP
	word offset;
	char *start_stack_area;
	char *start_shared_area = shared_mem_base();
	char *start_private_area = (char *) sbrk(0);
	word stacksize = RoundTo(ec_options.localsize + ec_options.globalsize,
				STACK_PAGESIZE);

	if (offset = (uword) start_private_area % map_alignment)
	    start_private_area += (map_alignment - offset);	/* round up */

#ifdef STACK_BASE
    /* The stack area cannot be computed by the default rule */
    start_stack_area = (char *) STACK_BASE;
#endif
	if (start_shared_area) {
#ifndef STACK_BASE
	    start_stack_area = start_shared_area - stacksize;
#endif
	    if (start_stack_area < start_private_area + ec_options.privatesize) {
		ec_bad_exit("ECLiPSe: No address space for stacks.");
	    }
	    start_shared_heap = start_shared_area + SHARED_MEM_OFFSET_HEAP;
	}
	else
	{
#ifndef STACK_BASE
	    /* we cannot use saved states anyway */
	    start_stack_area = start_private_area + ec_options.privatesize;
#endif
	    if (ec_options.mapfile)
		/* assume shared base is chosen properly by the OS */
		start_shared_heap = start_shared_area;	/* NULL */
	    else
		/* assume shared base is chosen properly by the OS */
		start_shared_heap = start_stack_area + stacksize;
								
	}

	end_of_stacks = start_of_stacks = (uword *) start_stack_area;
	start_shared_heap = shared_mem_init(
		flags & INIT_SHARED, ec_options.mapfile, start_shared_heap,
		 ec_options.sharedsize, map_alignment, ec_panic, &global_heap);
	if (start_shared_heap == (char *) -1)
	{
	    perror("ECLiPSe: can't init shared heap");
	    exit(-1);
	}
#else
    	ec_panic("no way to init heap","eclipse_mem_init()");
#endif
    }
    else
    {
	end_of_stacks = start_of_stacks = (uword *) 0;
	start_shared_heap = private_mem_init_desc(ec_panic, &global_heap);
	if (start_shared_heap == (char *) -1)
	{
	    perror("ECLiPSe: can't init shared heap");
	    exit(-1);
	}
    }

    if (flags & INIT_SHARED)
    {
	alloc_debug_level(&global_heap, ec_options.debug_level);

	shared_data = (struct shared_data_t *)
			hg_alloc_size(sizeof(struct shared_data_t));
	GlobalFlags =
#ifdef PRINTAM
	    /* for better debugging of the system files */
	    VARIABLE_NAMES|SINGLETON_CHECK|
#endif
	    DBGCOMP|
	    GC_ENABLED|GC_ADAPTIVE|
	    MACROEXP|VARIABLE_NAMES;

	a_mutex_init(&SharedDataLock);
	a_mutex_init(&ModuleLock);
	a_mutex_init(&PropertyLock);
	a_mutex_init(&PropListLock);
	a_mutex_init(&ProcedureLock);
	a_mutex_init(&ProcListLock);
	a_mutex_init(&ProcChainLock);
	a_mutex_init(&AssertRetractLock);

	/* Make it visible with HEAP_READY flag off!! */
	*(struct shared_data_t **) start_shared_heap = shared_data;
    }
    else
    {
	/* wait for heap to become fully initialised by first worker */
	while (!(shared_data = *(struct shared_data_t **) start_shared_heap))
	{
	    /* sleep(1); */
	}
    }

#ifdef lint
    {
	(void) hp_free((generic_ptr) 0);
	(void) hp_free_size((generic_ptr) 0, 0);
	(void) hp_realloc_size((generic_ptr) 0, 0, 0);
	(void) hp_resize((generic_ptr) 0, 0);
    }
#endif
}

static void
dummy_delayed_irq_handler(void)
{
}

void
malloc_init(void)
{
    if (mem_is_initialized)
	return;		/* initialization was already forced by malloc() */
    /* 
     * Determine the machine's pagesize
     * (needs to be here since malloc() may be called before mem_init())
     */
# if defined(HAVE_SYSCONF) && defined(SYSCONF_PAGE)
    system_pagesize = sysconf(SYSCONF_PAGE);
# else
#ifdef HAVE_GETPAGESIZE
    system_pagesize = getpagesize();
#else
#  ifdef SYS_PAGESIZE
    system_pagesize = SYS_PAGESIZE;
#  else
    system_pagesize = 4096;
#  endif
# endif
#endif

    irq_lock_init(dummy_delayed_irq_handler);
    private_mem_init(ec_panic);
    alloc_debug_level(&private_heap, ec_options.debug_level);

    mem_is_initialized = 1;
}

void
mem_fini()
{
    shared_mem_release(&global_heap);
    private_mem_release();
    mem_is_initialized = 0;
}
