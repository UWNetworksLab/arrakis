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
 * IDENTIFICATION	memory.h
 *
 * AUTHOR		Joachim Schimpf
 *
 * DESCRIPTION		see alloc.c shared_mem.c private_mem.c lock.s
 *
 * USAGE:		include this file, link with libshm.a
 *---------------------------------------------------------------------*/

#ifndef __ECLIPSE_MEMMAN_H
#define __ECLIPSE_MEMMAN_H

#ifdef HAVE_NO_VOID_PTR
typedef char *generic_ptr;
#else
typedef void *generic_ptr;
#endif

/*---------------------------------------------------------------------
 * Size-dependent values
 *---------------------------------------------------------------------*/

#ifndef __CHAR_UNSIGNED__
typedef char		int8;			/* exactly 8 bit */
#else
typedef signed char	int8;
#endif
typedef unsigned char	uint8;

typedef short		int16;			/* exactly 16 bit */
typedef unsigned short	uint16;

#if (SIZEOF_INT == 4)
typedef int		int32;			/* exactly 32 bit */
typedef unsigned int	uint32;
#endif

#if (SIZEOF_CHAR_P == SIZEOF_INT)
typedef int		word;			/* pointer-sized */
typedef unsigned int	uword;
#else
#if (SIZEOF_CHAR_P == SIZEOF_LONG)
typedef long		word;			/* pointer-sized */
typedef unsigned long	uword;
#endif
#endif


#if (SIZEOF_CHAR_P == 8)
/* Maximal representable address divided by bits per byte. */
/* For -taso we address only the 32-bit memory */
#define MAX_ADDRESS_BYTE	0x20000000
#ifndef SIGN_BIT
#define SIGN_BIT		((uword) 0x8000000000000000L)
#endif
#else
#define MAX_ADDRESS_BYTE	0x20000000
#ifndef SIGN_BIT
#define SIGN_BIT		((uword) 0x80000000L)
#endif
#endif


/* the unit of allocation */

typedef union
{
    struct
    {
	word a1, a2;	/* seems resonable not to allocate smaller */
    }		p;
    double	d;	/* may be same or less than 2 pointer sizes */
} unit_type;

/*---------------------------------------------------------------------
 * Logical page manager
 *---------------------------------------------------------------------*/

#define BYTES_PER_UNIT		sizeof(unit_type)
#define BYTES_PER_PAGE		4096	/* logical page size =< physical */
#define UNITS_PER_PAGE		(BYTES_PER_PAGE/BYTES_PER_UNIT)
#define WORDS_PER_PAGE		(BYTES_PER_PAGE/sizeof(bits32))
#define BITMAP_BLOCKSIZE	BYTES_PER_PAGE
#define BITMAP_BLOCKS		(MAX_ADDRESS_BYTE/BYTES_PER_PAGE/BITMAP_BLOCKSIZE)
#define PAGE_LISTS		32
#define MIN_OS_PAGE_REQUEST	8	/* min pages to get from OS */

typedef uint32 bits32;

struct cluster {
	struct cluster	*next;
	generic_ptr	addr;
	word		size;
	word		dummy;
};

struct page_log {
    generic_ptr addr;
    word npages;
};

struct page_admin {
	word		allocated;		/* # pages gotten from OS */
	word		freed;			/* # pages in free list */
	generic_ptr	min_addr, max_addr;
	struct page_log	*log_page;		/* log of more'd pages */
	word		log_idx;
	struct cluster	*free[PAGE_LISTS];	/* free[i]: i-page-clusters */
						/* free[0]: larger clusters */
	bits32		*map[BITMAP_BLOCKS];	/* bitmap of pages (1 = free) */
};


/*---------------------------------------------------------------------
 * Block manager
 *---------------------------------------------------------------------*/

#define HEAP_STAT_ALLOCATED	0
#define HEAP_STAT_USED		1

#define LARGEST_SMALL_BLOCK	7	/* units */
#define SMALLEST_POWER_BLOCK	8	/* units */
#define SMALLEST_PAGE_BLOCK	(BYTES_PER_PAGE/BYTES_PER_UNIT)	/* units */
#define LARGEST_POWER_BLOCK	(SMALLEST_PAGE_BLOCK/2)
#define POWER_FIRST_INDEX	6
#define POWERS			32

struct heap {
	generic_ptr small_blocks[LARGEST_SMALL_BLOCK+1];
	generic_ptr powers[POWERS];

	generic_ptr alloc_ptr;
	word	alloc_free;		/* in heap_units */

    /* statistics */

	word	small_allocated[LARGEST_SMALL_BLOCK+1];
	word	powers_allocated[POWERS];
	word	requested,		/* in bytes */
		used,			/* small/power only (in heap_units) */
		allocs,
		small_block_pages,
		power_pages;
};


/*---------------------------------------------------------------------
 * Allocation with headers
 *---------------------------------------------------------------------*/

typedef union mem_header
{
	struct
	{
		struct heap	*magic;
		word	size;
	}	s;
	double	dummy;	       /* force alignment of blocks */
} HEADER;


/*---------------------------------------------------------------------
 * Interrupt disabling
 *---------------------------------------------------------------------*/

#define InterruptsDisabled	it_disabled_
#define Disable_Int()		it_disabled_++;
#define Enable_Int() \
	{ if (--it_disabled_ == 0 && delayed_it_) (*delayed_irq_func)(); }
#define InterruptsPending	delayed_it_
#define Set_Interrupts_Pending() delayed_it_ = 1;
#define Clr_Interrupts_Pending() delayed_it_ = 0;

extern volatile int it_disabled_, delayed_it_;
#ifdef __STDC__
extern void (*delayed_irq_func)(void);
#else
extern void (*delayed_irq_func)();
#endif

/*---------------------------------------------------------------------
 * Spin Locks
 *---------------------------------------------------------------------*/

#if (defined(_PA_RISC1_0) || defined(_PA_RISC1_1))
typedef int a_mutex_t[4];
#else
typedef int a_mutex_t;
#endif

/*---------------------------------------------------------------------
 * Heap descriptor, lowest level
 *---------------------------------------------------------------------*/

/* The private memory part */

struct heap_descriptor {
	struct shm_desc	*shared_header;	/* NULL for private memory */
	struct page_admin *pages;
	struct heap	*heap;
	int		map_fd;
#ifdef __STDC__
	generic_ptr	(*more)(word,int,struct heap_descriptor*);
	int		(*less)(generic_ptr,word,struct heap_descriptor*);
	void		(*panic)(const char*, const char*);
#else
	generic_ptr	(*more)();
	generic_ptr	(*less)();
	void		(*panic)();
#endif
	int		debug_level;
};

/* The shared memory part (only if really shared) */

struct shm_desc {
	generic_ptr application_header;	/* must be the first word! */
	char *start;			/* own address */
	char *brk;			/* end of allocated space */
	char *lim;			/* end of the mapped region */
	char *stop;			/* end of the reserved address space */
	int incr;			/* mapping increment in bytes */
	int processes;			/* number of attached processes */
	char *mapfile;			/* file it is mapped to */
	a_mutex_t lock;			/* memory management lock */
	struct heap heap;		/* block manager structure */
	struct page_admin pages;	/* page manager structure */
	char mapfile_buf[1024];		/* string buffer for mapfile name */
};


/*---------------------------------------------------------------------
 * Simplified private heap interface
 *---------------------------------------------------------------------*/

extern struct heap_descriptor private_heap;


/*---------------------------------------------------------------------
 * Function prototypes
 *---------------------------------------------------------------------*/

#ifdef __STDC__

void		pagemanager_init(struct heap_descriptor *);
void		pagemanager_fini(struct heap_descriptor *);
generic_ptr	alloc_page(struct heap_descriptor *);
generic_ptr	alloc_pagewise(struct heap_descriptor *, word, word *);
void		free_pages(struct heap_descriptor *, generic_ptr, word);

void		irq_lock_init(void (*irq_fct)(void));
int		a_mutex_init(a_mutex_t *);
int		a_mutex_lock(volatile a_mutex_t *);
int		a_mutex_unlock(a_mutex_t *);
int		a_mutex_destroy(a_mutex_t *);

void		alloc_init(struct heap_descriptor *);
void		alloc_debug_level(struct heap_descriptor *, int);
generic_ptr	alloc_size(struct heap_descriptor *, word);
void		free_size(struct heap_descriptor *, generic_ptr, word);
generic_ptr	realloc_size(struct heap_descriptor *, generic_ptr, word, word);
generic_ptr	h_alloc(struct heap_descriptor *, word);
void		h_free(struct heap_descriptor *, generic_ptr);
generic_ptr	h_realloc(struct heap_descriptor *, generic_ptr, word);
int		address_in_heap(struct heap_descriptor *, generic_ptr);
int		alloc_statistics(struct heap_descriptor *, int);

generic_ptr	hp_alloc_size(word size);
void		hp_free_size(generic_ptr, word size);
generic_ptr	hp_realloc_size(generic_ptr, word, word);
generic_ptr	hp_alloc(word size);
void		hp_free(generic_ptr);
generic_ptr	hp_resize(generic_ptr, word);
int		hp_statistics(int what);

char		*shared_mem_base(void);
char		*shared_mem_init(int create_flag,
			char* mapfile, char* start,
			word size, word increment,
			void (*panic_fct)(const char*, const char*),
			struct heap_descriptor *hd);
void		shared_mem_release(struct heap_descriptor *hd);
int		shared_mem_save(struct heap_descriptor *hd, int fd);
int		shared_mem_restore(struct heap_descriptor *hd, int fd);
void		private_mem_init(void (*panic_fct)(const char*, const char*));
char *		private_mem_init_desc(void (*panic_fct)(const char*, const char*),
			struct heap_descriptor *hd);
void		private_mem_fini();
void		private_mem_fini_desc(struct heap_descriptor *hd);


#else /* __STDC__ */

void		pagemanager_init();
generic_ptr	alloc_page();
generic_ptr	alloc_pagewise();
void		free_pages();

void		irq_lock_init();
int		a_mutex_init();
int		a_mutex_lock();
int		a_mutex_unlock();
int		a_mutex_destroy();

void		alloc_init();
generic_ptr	alloc_size();
void		free_size();
generic_ptr	realloc_size();
generic_ptr	h_alloc();
void		h_free();
generic_ptr	h_realloc();
int		address_in_heap();
int		alloc_statistics();

generic_ptr	hp_alloc_size();
void		hp_free_size();
generic_ptr	hp_realloc_size();
generic_ptr	hp_alloc();
void		hp_free();
generic_ptr	hp_resize();
int		hp_statistics();

char		*shared_mem_base();
char		*shared_mem_init();
void		shared_mem_release();
int		shared_mem_save();
int		shared_mem_restore();
void		private_mem_init();
char *		private_mem_init_desc();


#endif /* __STDC__ */

#endif /* __ECLIPSE_MEMMAN_H */
