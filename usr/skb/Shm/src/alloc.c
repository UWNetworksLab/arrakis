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

/*
* IDENTIFICATION	alloc.c
*
* VERSION		$Id: alloc.c,v 1.2 2007/07/03 00:10:25 jschimpf Exp $
*
* AUTHOR		Joachim Schimpf
*
* DESCRIPTION		heap allocator
*
* CONTENTS
*
*	        PAGE LEVEL
*				pagemanager_init()
*				alloc_pagewise()
*				alloc_page()
*				free_pages()
*		BLOCK LEVEL
*				alloc_init()
*				alloc_statistics()
*
*				alloc_size(heap, bytes)
*				free_size(heap, ptr, bytes)
*				realloc_size(heap, ptr, oldbytes, newbytes)
*
*				h_alloc(heap, bytes)
*				h_free(heap, ptr)
*				h_realloc(heap, ptr, newbytes)
*/


#include	"config.h"
#include	"memman.h"

#ifdef HAVE_STRING_H
#  include <string.h>
#  ifdef MEMCPY_STRING
#    define bcopy(s1, s2, n)	(void) memcpy((void *)(s2),(void *)(s1), n)
#  endif
#endif
#ifdef MEMCPY_MEMORY
#  include <memory.h>
#  define bcopy(s1, s2, n)	(void) memcpy((char *)(s2), (char *)(s1), n)
#endif

#define OUT_OF_HEAP	((generic_ptr)(-1))


/* DEBUG_HEAP works only when linked with sepia! */

#ifdef DEBUG_HEAP
#include <stdio.h>
typedef void *          stream_id;
extern  stream_id       current_err_;
void	pr_heap();
#endif

extern void	exit(int);

#define CHECK_HEAP

#ifdef CHECK_HEAP
static void
_print(char *msg)
{
    (void) write(2, msg, strlen(msg));
}
#endif


/*---------------------------------------------------------------------
 * Interrupt Locks
 *---------------------------------------------------------------------*/

#define Lock_Heap(hd) { \
    if (hd->shared_header) { a_mutex_lock(&hd->shared_header->lock); } \
    else { Disable_Int(); }}

#define Unlock_Heap(hd) { \
    if (hd->shared_header) { a_mutex_unlock(&hd->shared_header->lock); } \
    else { Enable_Int(); }}

void (*delayed_irq_func)(void) = 0;	/* to process delayed interrupts	*/

volatile int it_disabled_ = 0;		/* number of nested disables */
volatile int delayed_it_ = 0;		/* flags that something is in the queue */


void
irq_lock_init(void (*irq_func)(void))
{
    it_disabled_ = delayed_it_ = 0;
    delayed_irq_func = irq_func;
}


/*---------------------------------------------------------------------
 * Low-level pagewise memory management
 *
 * The allocation functions call panic() when there is no memory.
 * The pagewise allocation functions do not disable interrupts, it
 * must be done in the caller.
 *
 * Management of free pages:
 *
 * We maintain a bitmap pages->map[] of all memory pages.
 * The bit is set when the corresponding page is free.
 *
 * Additionally, we have free lists pages->free[] for page clusters.
 * pages->free[i] holds a list of i-page cluster descriptor.
 * pages->free[0] holds a list of clusters of size PAGE_LISTS or larger.
 *
 * Allocation of an i-page cluster:
 *	- check the free list for i-page clusters
 *	- if this is empty, split a larger cluster
 *	- if no sufficiently large cluster available, call sbrk()
 *	- reset the free-bits in the bitmap
 * Freeing a cluster:
 *	- set the free-bits in the bitmap
 *	- join with adjacent clusters, if possible
 *	- put resulting free cluster into appropriate free list
 *
 * A page in this context here is just a unit of BYTES_PER_PAGE bytes.
 * It is not strictly necessary that it corresponds to the
 * system_page_size.
 * TODO: Especially for shared memory, the free cluster links should
 * not be in the pages themselves, that causes unnecessary page accesses.
 *---------------------------------------------------------------------*/


#define RoundTo(n,unit) ((n) - ((n) - 1) % (unit) -1 + (unit))

#if (SIZEOF_CHAR_P == 4)
#define USE_BITMAPS
#endif

#ifdef USE_BITMAPS

/* Define bitmasks and their widths for memory parameters. */
#define BITS_IN_WORD_M		0x1f
#define BITS_IN_WORD_W		5
#define WORDS_IN_BLOCK_M	0x3ff
#define WORDS_IN_BLOCK_W	10

#define MapBlock(i)	((i) >> (BITS_IN_WORD_W + WORDS_IN_BLOCK_W))
#define MapWord(i)	(((i) >> BITS_IN_WORD_W) & WORDS_IN_BLOCK_M)
#define MapBit(i)	(0x1L << ((i) & BITS_IN_WORD_M))

/* mask must be unsigned */
#define Page_Parameters(PageAddr, Block, Ptr, Mask) {		\
	register bits32 i = (bits32)(PageAddr) / BYTES_PER_PAGE;\
	Block = MapBlock(i);					\
	if (! pages->map[Block])					\
	    pages->map[Block] = _new_bitmap_block(hd);		\
	Ptr = &(pages->map[Block][MapWord(i)]);			\
	Mask = MapBit(i);					\
}
	
#define Next_Bit(block, ptr, mask) {			\
	if (((mask) <<= 1) == 0)			\
	{						\
	    (mask) = 0x1;				\
	    if ((word)(++(ptr)) % BITMAP_BLOCKSIZE == 0)	\
	    {						\
		++block;				\
		if (! pages->map[block])			\
		    pages->map[block] = _new_bitmap_block(hd);\
		ptr = pages->map[block];			\
	    }						\
	}						\
}

#define Prev_Bit(block, ptr, mask) {			\
	if (((mask) >>= 1) == 0)			\
	{						\
	    (mask) = SIGN_BIT;				\
	    if ((word)((ptr)--) % BITMAP_BLOCKSIZE == 0)	\
	    {						\
		--block;				\
		if (! pages->map[block])			\
		    pages->map[block] = _new_bitmap_block(hd);\
		ptr = pages->map[block] - 1 +		\
		    BITMAP_BLOCKSIZE/sizeof(bits32);	\
	    }						\
	}						\
}

#endif

/*ARGSUSED*/
void
pagemanager_init(struct heap_descriptor *hd)
{
    register int i;
    struct page_admin *pages = hd->pages;
    pages->min_addr = pages->max_addr = 0;
    for (i=0; i<BITMAP_BLOCKS; i++)
	pages->map[i] = (bits32 *) 0;
    for (i=0; i<PAGE_LISTS; i++)
	pages->free[i] = (struct cluster *) 0;
    pages->allocated = 0;
    pages->freed = 0;
    pages->log_page = (struct page_log *) 0;
    pages->log_idx = 0;
}


static generic_ptr
_alloc_aux_page(struct heap_descriptor *hd)
{
    generic_ptr address;
    address = hd->more(BYTES_PER_PAGE, BYTES_PER_PAGE, hd);
    if (address == OUT_OF_HEAP)
    {
	_print("SHM: out of memory in _alloc_aux_page()");
	exit(-1);	/* cannot recover from here ... */
    }
    ++hd->pages->allocated;
    return address;
}

static void
_release_aux_page(struct heap_descriptor *hd, generic_ptr address)
{
    (void) hd->less(address, BYTES_PER_PAGE, hd);
    --hd->pages->allocated;
}

static void _release_logged_pages(struct heap_descriptor *hd);

void
pagemanager_fini(struct heap_descriptor *hd)
{
    int i;
    _release_logged_pages(hd);
    for (i=0; i<BITMAP_BLOCKS; i++)
    {
	if (hd->pages->map[i])
	{
	    _release_aux_page(hd, hd->pages->map[i]);
	    hd->pages->map[i] = 0;
	}
    }
    if (hd->pages->allocated)
    {
	_print("SHM: not all pages were freed in pagemanager_fini()\n");
    }
}


#ifdef USE_BITMAPS
static bits32 *
_new_bitmap_block(struct heap_descriptor *hd)
{
    register int i;		/* careful: bootstrapping problem! */
    bits32 *p = (bits32 *)_alloc_aux_page(hd);
    for (i=0; i < WORDS_PER_PAGE; i++)
	p[i] = 0;
    return p;
}
#endif

static void
_add_to_list(struct heap_descriptor *hd,
	generic_ptr ptr,
	word number_of_pages)	/* should be > 0 */
{
    int list_index = number_of_pages < PAGE_LISTS ? number_of_pages : 0;
    ((struct cluster *)ptr)->next = hd->pages->free[list_index];
    ((struct cluster *)ptr)->addr = ptr;
    ((struct cluster *)ptr)->size = number_of_pages;
    hd->pages->free[list_index] = (struct cluster *)ptr;
}

static void
_remove_from_list(struct heap_descriptor *hd,
	generic_ptr ptr,
	word	number_of_pages)	/* should be > 0 */
{
    register struct cluster **p;
    p = &hd->pages->free[number_of_pages < PAGE_LISTS ? number_of_pages : 0];
    while ((*p) && (*p)->addr != ptr)
    {
	p = &((*p)->next);
    }
#ifdef CHECK_HEAP
    if (*p == 0)
    {
	_print("SHM INTERNAL ERROR: pagecluster missing from free list\n");
	return;
    }
#endif
    *p = (*p)->next;
}

#ifdef CHECK_HEAP
void
_check_address(
	struct heap_descriptor *hd,
	generic_ptr ptr,
	uword size)
{
    if (ptr < hd->pages->min_addr ||
    	((generic_ptr)((char*) ptr + size) > hd->pages->max_addr && hd->pages->max_addr))
    {
	_print("SHM: attempt to free out-of-heap pointer!\n");
    }
}
#endif

#ifdef USE_BITMAPS

void
free_pages(
	struct heap_descriptor *hd,
	generic_ptr ptr,
	word	number_of_pages)	/* should be > 0 */
{
    int	block;
    register bits32 *p, mask;
    char *from, *to;
    struct page_admin *pages = hd->pages;

#if (DEBUG_HEAP > 1)
    fprintf(stderr, "free %d pages\n", number_of_pages);
#endif

#ifdef CHECK_HEAP
    if ((word) ptr % BYTES_PER_PAGE != 0)
    {
	_print("SHM: misaligned pointer in free_pages()\n");
	return;
    }
    _check_address(hd, ptr, number_of_pages*BYTES_PER_PAGE);
#endif

    pages->freed += number_of_pages;
    Page_Parameters(ptr, block, p, mask);
    from = (char *) ptr;
    Prev_Bit(block, p, mask);
    if (*p & mask)			/* adjacent to lower neighbour */
    {
	do {
	    Prev_Bit(block, p, mask);
	    from -= BYTES_PER_PAGE;
	} while (*p & mask);
	Page_Parameters(ptr, block, p, mask);
	Prev_Bit(block, p, mask);
	_remove_from_list(hd, (generic_ptr) from, ((char*)ptr-from)/BYTES_PER_PAGE);
    }
    while (number_of_pages--)		/* update the bitmap */
    {
	Next_Bit(block, p, mask);
	ptr = (generic_ptr) ((char *) ptr + BYTES_PER_PAGE);
#ifdef CHECK_HEAP
	if (mask & *p)
	{
	    _print("SHM: page is already free in free_pages()\n");
	}
#endif
	*p |= mask;
    }
    to = (char *) ptr;
    Next_Bit(block, p, mask);
    if (*p & mask)			/* adjacent to upper neighbour */
    {
	do {
	    Next_Bit(block, p, mask);
	    to += BYTES_PER_PAGE;
	} while (*p & mask);
	_remove_from_list(hd, ptr, (to-(char*)ptr)/BYTES_PER_PAGE);
    }
    _add_to_list(hd, (generic_ptr) from, (to-from)/BYTES_PER_PAGE);
}

#else

#define GenericAdd(p,off) ((generic_ptr)((char*)(p)+(off)))

void
free_pages(
	struct heap_descriptor *hd,
	generic_ptr ptr,
	word	number_of_pages)	/* should be > 0 */
{
    struct cluster *clu, *next;
    generic_ptr to;
    int i;
    struct page_admin *pages = hd->pages;

#if (DEBUG_HEAP > 1)
    fprintf(stderr, "free %d pages\n", number_of_pages);
#endif

#ifdef CHECK_HEAP
    if ((word) ptr % BYTES_PER_PAGE != 0)
    {
	_print("SHM: misaligned pointer in free_pages()\n");
	return;
    }
    _check_address(hd, ptr, number_of_pages*BYTES_PER_PAGE);
#endif

    pages->freed += number_of_pages;
    to = GenericAdd(ptr, number_of_pages*BYTES_PER_PAGE);
    for (i=0; i<PAGE_LISTS; ++i)
    {
	for (clu = hd->pages->free[i]; clu; clu = next)
	{
	    next = clu->next;	/* memorize because clu may be unlinked */
	    if (clu->addr > to
		    || GenericAdd(clu->addr, clu->size*BYTES_PER_PAGE) < ptr)
		continue;
	    if (clu->addr == to)
	    {
		/* adjacent to upper neighbour */
		_remove_from_list(hd, clu->addr, clu->size);
		number_of_pages += clu->size;
		/* ptr unchanged */
		to = GenericAdd(ptr, number_of_pages*BYTES_PER_PAGE);
	    }
	    else if (GenericAdd(clu->addr, clu->size*BYTES_PER_PAGE) == ptr)
	    {
		/* adjacent to lower neighbour */
		_remove_from_list(hd, clu->addr, clu->size);
		number_of_pages += clu->size;
		ptr = clu->addr;
		/* to unchanged */
	    }
	    else	/* overlap, shouldn't happen */
	    {
		_print("SHM: page is already free in free_pages()\n");
		return;
	    }
	}
    }
    _add_to_list(hd, ptr, number_of_pages);
}

#endif


/*
 * We keep an additional log of all the more-requests to the OS.
 * This is used to forcibly free all heap space (allocated or not)
 * when the heap is finalised. We use auxiliary pages for this log.
 */

static void
_log_more_pages(struct heap_descriptor *hd, generic_ptr address, word pages_requested)
{
    struct page_log *log_page = hd->pages->log_page;

    if (pages_requested == 0)
    	return;

    if (log_page)
    {
	int i = hd->pages->log_idx;
	if (address == log_page[i].addr + log_page[i].npages*BYTES_PER_PAGE)
	{
	    /* adjacent to previous logged allocation, amend the record */
	    log_page[i].npages += pages_requested;
	    return;
	}
	if (++i < BYTES_PER_PAGE/sizeof(struct page_log))
	{
	    /* create a new log entry in same block */
	    hd->pages->log_idx = i;
	    log_page[i].addr = address;
	    log_page[i].npages = pages_requested;
	    return;
	}
    }

    /* allocate a new auxiliary page for the log, and initialise */
    hd->pages->log_page = (struct page_log*) _alloc_aux_page(hd);
    hd->pages->log_idx = 1;
    hd->pages->log_page[0].addr = log_page;	/* link to previous */
    hd->pages->log_page[0].npages = 0;
    hd->pages->log_page[1].addr = address;
    hd->pages->log_page[1].npages = pages_requested;
    return;
}


static void
_release_logged_pages(struct heap_descriptor *hd)
{
    struct page_log *log_page = hd->pages->log_page;
    int max = hd->pages->log_idx;
    while (log_page)
    {
	int i;
	struct page_log *old_log_page;
	/* free all the logged page clusters in this log page */
	for(i=max; i>=1; --i)
	{
	    (void) hd->less(log_page[i].addr, log_page[i].npages*BYTES_PER_PAGE, hd);
	    hd->pages->allocated -= log_page[i].npages;
	}
	/* free the log page itself, and proceed to previous one */
	old_log_page = log_page;
	log_page = (struct page_log *) log_page[0].addr;
	_release_aux_page(hd, old_log_page);
	max = BYTES_PER_PAGE/sizeof(struct page_log) - 1;
    }
}


/*
 * Allocate memory in units of pages. The second argument returns the
 * amount of memory that has really been allocated. It is at least as
 * much as was requested, but rounded up to the next page multiple.
 */

generic_ptr
alloc_pagewise(
	struct heap_descriptor *hd,
	word	bytes_needed,
	word	*out_bytes_allocated)
{
    register struct cluster **cluster_list;
    register struct cluster *cluster;
    word bytes_allocated, pages_needed;
#ifdef USE_BITMAPS
    int block;
    bits32 *p, mask;
#endif
    struct page_admin *pages = hd->pages;

    *out_bytes_allocated = bytes_allocated = RoundTo(bytes_needed, BYTES_PER_PAGE);
    pages_needed = bytes_allocated/BYTES_PER_PAGE;

#if (DEBUG_HEAP > 1)
    fprintf(stderr, "alloc %d pages\n", pages_needed);
#endif

    if (pages_needed < PAGE_LISTS)
    {
	if (pages->free[pages_needed])	/* a cluster that fits exactly */
	{
	    pages->freed -= pages_needed;
	    cluster = pages->free[pages_needed];
	    pages->free[pages_needed] = cluster->next;	/* remove from free list */
#ifdef USE_BITMAPS
	    Page_Parameters(cluster->addr, block, p, mask);
	    while (pages_needed--)			/* update the bitmap */
	    {
		*p &= ~mask;
		Next_Bit(block, p, mask);
	    }
#endif
	    return cluster->addr;
	}

	/* no exact fit, set cluster_list to a free list with larger clusters */
	cluster_list = &pages->free[0];	/* try default list first */
	if (! (*cluster_list))		/* else any another larger cluster */
	{
	    word list_index = pages_needed;
	    while (++list_index < PAGE_LISTS)
	    {
		if (pages->free[list_index])		/* found one */
		{
		    cluster_list = &pages->free[list_index];
		    break;
		}
	    }
	}
    }
    else	/* very large block requested, try the default list */
    {
	cluster_list = &pages->free[0];
    }

    /*
     * look for a sufficiently large cluster (at least pages_needed)
     * in cluster_list (which may be empty)
     */
    while (cluster = (*cluster_list))
    {
	if (cluster->size >= pages_needed)
	{
	    pages->freed -= pages_needed;
	    *cluster_list = cluster->next;	/* remove from free list */
	    if (cluster->size > pages_needed)	/* put back the rest, if any */
	    {
		_add_to_list(hd, (generic_ptr)
			((char *) cluster->addr + pages_needed*BYTES_PER_PAGE),
			cluster->size - pages_needed);
	    }
#ifdef USE_BITMAPS
	    Page_Parameters(cluster->addr, block, p, mask);
	    while (pages_needed--)		/* update the bitmap */
	    {
		*p &= ~mask;
		Next_Bit(block, p, mask);
	    }
#endif
	    return cluster->addr;
	}
	cluster_list = &cluster->next;
    }

    /*
     * Nothing appropriate in our free lists,
     * get a sufficiently large cluster from the operating system
     */
    {
	register generic_ptr address;
	word pages_requested, bytes_requested;

	/* allocate pages_needed, but at least MIN_OS_PAGE_REQUEST */
	pages_requested = pages_needed >= MIN_OS_PAGE_REQUEST ?
		pages_needed : MIN_OS_PAGE_REQUEST;

	address = hd->more(pages_requested*BYTES_PER_PAGE, BYTES_PER_PAGE, hd);
	if (address == OUT_OF_HEAP && pages_requested > pages_needed)
	{
	    pages_requested = pages_needed;
	    address = hd->more(pages_requested*BYTES_PER_PAGE, BYTES_PER_PAGE, hd);
	}
	if (address == OUT_OF_HEAP)
	{
	    if (hd->panic)
	    {
		Unlock_Heap(hd);
		(*hd->panic)("Out of swap space", "heap allocation");
	    }
	    return (generic_ptr) 0;
	}
	if ((word) address % BYTES_PER_PAGE != 0)
	{
	    _print("SHM: misaligned pointer returned from OS\n");
	}

	/* update some statistics */
	_log_more_pages(hd, address, pages_requested);
	pages->allocated += pages_requested;
	bytes_requested = pages_requested*BYTES_PER_PAGE;
	if (!pages->min_addr || address < pages->min_addr)
	    pages->min_addr = address;
	if (!pages->min_addr ||
	    (generic_ptr)((char*)address + bytes_requested) > pages->max_addr)
	    pages->max_addr = (generic_ptr)((char*)address + bytes_requested);

	/* put excess pages in the free list */
	if (pages_requested > pages_needed)
	{
	    free_pages(hd, address + bytes_allocated, pages_requested-pages_needed);
	}
	return address;
    }
}

/*
 * Allocate a single page (of size BYTES_PER_PAGE)
 */

generic_ptr
alloc_page(struct heap_descriptor *hd)
{
    word dummy;
    return alloc_pagewise(hd, BYTES_PER_PAGE, &dummy);
}



/*---------------------------------------------------------------------
 * Heap allocator with special handling of small blocks
 * We keep a number of separate free lists and never split larger blocks!
 * Allocation larger than half the pagesize are done pagewise.
 *---------------------------------------------------------------------*/


/* derived constants */

#define Units(n)	(((n)+(BYTES_PER_UNIT-1)) / BYTES_PER_UNIT)

#ifdef DEBUG_HEAP
int alloc_stop;
#endif

void
alloc_init(struct heap_descriptor *hd)
{
    int	i;
    struct heap *heap = hd->heap;
    for (i=0; i <= LARGEST_SMALL_BLOCK; i++)
    {
	heap->small_blocks[i] = (generic_ptr) 0;
	heap->small_allocated[i] = 0;
    }
    for (i=0; i < POWERS; i++)
    {
	heap->powers[i] = (generic_ptr) 0;
	heap->powers_allocated[i] = 0;
    }
    heap->alloc_ptr = alloc_page(hd);
    heap->alloc_free = UNITS_PER_PAGE;
    heap->requested = 0;
    heap->used = 0;
    heap->allocs = 0;
    heap->small_block_pages = 1;
    heap->power_pages = 0;
    hd->debug_level = 0;
}

void
alloc_debug_level(struct heap_descriptor *hd, int debug_level)
{
    hd->debug_level = debug_level;
}

generic_ptr
alloc_size(struct heap_descriptor *hd, word bytes_needed)
{
    register word units_needed = Units(bytes_needed);
    generic_ptr ptr;
    struct heap *heap = hd->heap;

    Lock_Heap(hd);

#ifdef DEBUG_HEAP
    heap->requested += bytes_needed;
    if (++heap->allocs == alloc_stop)
	alloc_stop++;
#endif
    if (units_needed <= LARGEST_SMALL_BLOCK)	/* perfect fit algorithm */
    {
	heap->used += units_needed;
	ptr = heap->small_blocks[units_needed];
	heap->small_allocated[units_needed]++;
	if (ptr)			/* we have one in the free list */
	{
	    heap->small_blocks[units_needed] = *((generic_ptr *) ptr);
	}
	else
	{
	    if (units_needed > heap->alloc_free) /* allocation block exhausted */
	    {
		if (heap->alloc_free)	/* put the rest into a free list */
		{
		    * ((generic_ptr *) heap->alloc_ptr) =
				heap->small_blocks[heap->alloc_free];
		    heap->small_blocks[heap->alloc_free] = heap->alloc_ptr;
		}
		heap->alloc_ptr = alloc_page(hd);
		heap->small_block_pages++;
		heap->alloc_free = UNITS_PER_PAGE;
	    }
	    ptr = heap->alloc_ptr;	/* allocate from the current block */
	    heap->alloc_free -= units_needed;
	    heap->alloc_ptr = (generic_ptr)
		((char *) heap->alloc_ptr + units_needed*BYTES_PER_UNIT);
	}
    }
    else if (units_needed <= LARGEST_POWER_BLOCK) /* allocate in powers of 2 */
    {
	register int index = POWER_FIRST_INDEX;
	register int blocksize = SMALLEST_POWER_BLOCK;

	while (units_needed > blocksize)
	{
	    blocksize <<= 1;
	    index++;
	}
	heap->used += blocksize;
	ptr = heap->powers[index];
	heap->powers_allocated[index]++;
	if (ptr)			/* we have one in the free list */
	{
	    heap->powers[index] = *((generic_ptr *) ptr);
	}
	else if (blocksize <= heap->alloc_free)	/* get from allocation block */
	{
	    ptr = heap->alloc_ptr;
	    heap->alloc_free -= blocksize;
	    heap->alloc_ptr = (generic_ptr)
		((char *) heap->alloc_ptr + blocksize*BYTES_PER_UNIT);
	}
	else	/* get a fresh page and split into blocks of blocksize */
	{
	    unit_type *initptr;
	    ptr = alloc_page(hd);
	    heap->power_pages++;
	    initptr = (unit_type *) ptr + UNITS_PER_PAGE - blocksize;
	    *(unit_type **) initptr = (unit_type *) 0;
	    initptr -= blocksize;
	    while ((generic_ptr) initptr > ptr)
	    {
		*(unit_type **) initptr = initptr + blocksize;
		initptr -= blocksize;
	    }
	    heap->powers[index] = (generic_ptr)(initptr + blocksize);
	}
    }
    else					/* allocate pagewise */
    {
	word bytes_allocated;
	ptr = alloc_pagewise(hd, bytes_needed, &bytes_allocated);
    }

    Unlock_Heap(hd);
    return ptr;
}

void
free_size(struct heap_descriptor *hd, generic_ptr ptr, word size)
{
    register word units = Units(size);
    struct heap *heap = hd->heap;

    Lock_Heap(hd);

#ifdef CHECK_HEAP
    _check_address(hd, ptr, size);

    if (hd->debug_level > 0)
    {
	memset(ptr, 0, size);	/* zero out the freed memory */
    }
#endif

#ifdef DEBUG_HEAP
    heap->requested -= size;
#endif
    if (units <= LARGEST_SMALL_BLOCK)		/* perfect fit algorithm */
    {
	heap->used -= units;
	* ((generic_ptr *) ptr) = heap->small_blocks[units];
	heap->small_blocks[units] = ptr;
	heap->small_allocated[units]--;
    }
    else if (units <= LARGEST_POWER_BLOCK)	/* powers of 2 algorithm */
    {
	register int index = POWER_FIRST_INDEX;
	register int blocksize = SMALLEST_POWER_BLOCK;

	while (units > blocksize)
	{
	    blocksize <<= 1;
	    index++;
	}
	heap->used -= blocksize;
	* ((generic_ptr *) ptr) = heap->powers[index];
	heap->powers[index] = ptr;
	heap->powers_allocated[index]--;
    }
    else					/* pagewise allocation */
    {
	word pages_allocated = (size-1)/BYTES_PER_PAGE + 1;
	free_pages(hd, ptr, pages_allocated);
    }
    Unlock_Heap(hd);
}

/* return the actual size of a memory block */

static word
_true_size(word size)
{
    register word units = Units(size);
    if (units <= LARGEST_SMALL_BLOCK)		/* perfect fit algorithm */
	return units*BYTES_PER_UNIT;
    else if (units <= LARGEST_POWER_BLOCK)	/* powers of 2 algorithm */
    {
	register int blocksize = SMALLEST_POWER_BLOCK;
	while (units > blocksize)
	    blocksize <<= 1;
	return blocksize*BYTES_PER_UNIT;
    }
    else					/* pagewise allocation */
    {
	return RoundTo(size, BYTES_PER_PAGE);
    }
}

generic_ptr
realloc_size(
	struct heap_descriptor *hd,
	generic_ptr ptr,
	word oldsize,
	word newsize)
{
    if (_true_size(oldsize) == _true_size(newsize))
    {
	return ptr;	/* already the right size */
    }
    else		/* grow or shrink */
    {
	generic_ptr new_ptr = alloc_size(hd, newsize);
	if (new_ptr)
	{
	    bcopy((char *) ptr, (char *) new_ptr,
		newsize < oldsize ? newsize : oldsize);
	    free_size(hd, ptr, oldsize);
	}
	return new_ptr;
    }
}


/*---------------------------------------------------------------------
 * Allocate memory that remembers its own size
 * We need a double header for alignment.
 * It gives us also space for a magic number.
 *---------------------------------------------------------------------*/

generic_ptr
h_alloc(struct heap_descriptor *hd, word size)
{
    HEADER *ptr;
    if (!(ptr = (HEADER*) alloc_size(hd, size + sizeof(HEADER))))
	return (generic_ptr) 0;
    ptr->s.size = size;
    ptr->s.magic = hd->heap;
    return (generic_ptr)(ptr + 1);
}

void
h_free(struct heap_descriptor *hd, generic_ptr ptr)
{
    HEADER *h = (HEADER*) ptr - 1;
    if (h->s.magic != hd->heap)
    {
	_print("SHM: invalid header in h_free()\n");
	return;
    }
    h->s.magic = (struct heap *) 0;
    free_size(hd, (generic_ptr) h, h->s.size + sizeof(HEADER));
}

generic_ptr
h_realloc(struct heap_descriptor *hd, generic_ptr ptr, word newsize)
{
    HEADER *h = (HEADER*) ptr - 1;
    word oldsize = h->s.size;

    if (h->s.magic != hd->heap)
    {
	_print("SHM: invalid header in h_realloc()\n");
	return ptr;
    }
    h->s.size = newsize;
    return (generic_ptr) ((HEADER*) realloc_size(hd, (generic_ptr) h,
	oldsize + sizeof(HEADER),
	newsize + sizeof(HEADER)) + 1);
}


/*---------------------------------------------------------------------
 * Debugging and statistics
 *---------------------------------------------------------------------*/

#define FullyUsedPages(hd) (hd->pages->allocated - hd->pages->freed \
	    - hd->heap->small_block_pages - hd->heap->power_pages)

int
alloc_statistics(struct heap_descriptor *hd, int what)
{
    switch(what)
    {
    case HEAP_STAT_ALLOCATED:
	return hd->pages->allocated * BYTES_PER_PAGE;
    case HEAP_STAT_USED:
#ifdef DEBUG_HEAP
	pr_heap(hd);
#endif
	return FullyUsedPages(hd) * BYTES_PER_PAGE
		+ hd->heap->used * BYTES_PER_UNIT;
    default:
	return 0;
    }
}

#ifdef DEBUG_HEAP

void
pr_heap(struct heap_descriptor *hd)
{
    int	i, j, blocksize;
    generic_ptr p;
    struct cluster *cl;
    struct page_admin *pages = hd->pages;
    struct heap *heap = hd->heap;

    p_fprintf(current_err_, "\nSMALL BLOCK MANAGEMENT:\n");
    for (i=1; i <= LARGEST_SMALL_BLOCK; i++)
    {
	for (j=0, p = heap->small_blocks[i]; p; p = *(generic_ptr *)p, j++)
		;
	p_fprintf(current_err_, "%10d byte blocks: %4d allocated, %4d free\n",
		i*BYTES_PER_UNIT, heap->small_allocated[i], j);
    }

    for (i=POWER_FIRST_INDEX, blocksize = SMALLEST_POWER_BLOCK;
	blocksize <= LARGEST_POWER_BLOCK;
	i++, blocksize <<= 1)
    {
	for (j=0, p = heap->powers[i]; p; p = *(generic_ptr *)p, j++)
		;
	p_fprintf(current_err_, "%10u byte blocks: %4d allocated, %4d free\n",
		blocksize*BYTES_PER_UNIT, heap->powers_allocated[i], j);
    }
    p_fprintf(current_err_, "%10u byte chunk free\n", heap->alloc_free * BYTES_PER_UNIT);
    p_fprintf(current_err_, "    #allocs   = %d\n", heap->allocs);
    p_fprintf(current_err_, "    requested = %d bytes\n", heap->requested);
    p_fprintf(current_err_, "    used      = %d bytes\n", heap->used * BYTES_PER_UNIT);
    p_fprintf(current_err_, "    allocated = %d bytes",
    	(heap->small_block_pages + heap->power_pages)*BYTES_PER_PAGE);
    p_fprintf(current_err_, " = %d small pages + %d power pages\n",
	heap->small_block_pages, heap->power_pages);

    p_fprintf(current_err_, "\nPAGE (%d bytes) MANAGEMENT:\n", BYTES_PER_PAGE);
    blocksize = 0;
    for (i=1; i<PAGE_LISTS; i++)
    {
	for (j=0, cl = pages->free[i]; cl; cl = cl->next, j++)
	    blocksize += i;
	if (j)
	    p_fprintf(current_err_, "%10d %4d-page clusters free\n", j, i);
    }
    for (cl = pages->free[0]; cl; cl = cl->next)
    {
	p_fprintf(current_err_, "%10d %4d-page cluster free\n", 1, cl->size);
	blocksize += cl->size;
    }

    p_fprintf(current_err_, "\nTOTAL:\n");
    p_fprintf(current_err_, "    used      = %d bytes\n",
	FullyUsedPages(hd) * BYTES_PER_PAGE + hd->heap->used * BYTES_PER_UNIT);
    p_fprintf(current_err_, "    allocated = %d bytes\n",
    	hd->pages->allocated * BYTES_PER_PAGE);
    p_fprintf(current_err_, "    pages     = %d (%d small + %d power + %d whole + %d free)\n",
	pages->allocated, heap->small_block_pages, heap->power_pages,
	pages->allocated - heap->small_block_pages - heap->power_pages - pages->freed,
	pages->freed);

    ec_flush(current_err_);
}

#endif /* DEBUG_HEAP */

