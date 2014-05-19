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
 * VERSION	$Id: dict.c,v 1.3 2008/07/10 01:08:46 jschimpf Exp $
 */

/*
 * IDENTIFICATION	dict.c
 *
 * AUTHOR:		Joachim Schimpf
 *
 * DESCRIPTION		SEPIA dictionary and related routines
 *
 * CONTENTS:
 *
 *	dict_init()
 *
 *		initialise the dictionary data structures and enter
 *		some predefined functors.
 *
 *	dident	enter_dict_n(char *name, int namelength, int arity)
 *
 *		Returns the DID for the functor with given name and arity.
 *		If it is not yet in the dictionary, it is entered. The name
 *		is specified with the length, so it can contain NUL bytes.
 *
 *	dident	enter_dict(char *name, int arity)
 *
 *		Same as enter_dict_n(), but takes a NUL-terminated C string
 *
 *	dident	in_dict(char *name, int arity)
 *
 *		Same as enter_dict(), but makes the entry a permanent one, ie.
 *		it will never be garbage collected. It is safe to store such
 *		DIDs in places that the garbage collector does not know about.
 *
 *	dident	ec_did(char *name, int arity)
 *
 *		Same as in_dict(), for naming like other user functions.
 *
 *	dident	add_dict(dident olddid, int newarity)
 *
 *		Converts a given DID into one for the same name but different
 *		arity. If such an entry does not yet exist, it is created.
 *
 *	dident	check_did_n(char *name, int namelength, int arity)
 *
 *		Returns the DID for the functor with given name and arity.
 *		If it is not yet in the dictionary, D_UNKNOWN is returned.
 *
 *	dident	check_did(dident olddid, int newarity)
 *
 *		Converts a given DID into one for the same name but different
 *		arity. If such an entry does not exist, D_UNKNOWN is returned.
 *
 *	pword  *enter_string_n(char *name, int length, int stability)
 *
 *		Create an atom with the given stability and returns a pointer
 *		to the corresponding string in the heap. This string exists
 *		only as long as a functor with this name exists. That means,
 *		if the string pointer is stored in a place where it is not
 *		known to the garbage collector, the stability has to be
 *		sufficiently high.
 *
 *	dident	bitfield_did(int bitfield)
 *
 *		convert a 19-bit bitfield representation of a DID (as used in
 *		the variable names) to a standard 32-bit DID.
 *
 *	int	next_functor(int *index, dident *did)
 *
 *		support function for traversing the dictionary, see below.
 *
 *	gc_dictionary(arity)
 *
 *		Dictionary garbage collector.
 *
 */


#include	"config.h"
#include	"os_support.h"
#include	"sepia.h"
#include	"types.h"
#include	"embed.h"
#include	"error.h"
#include	"mem.h"
#include	"io.h"
#include	"dict.h"
#include	"emu_export.h"

static dident		_in_dict_opt(char *name, register int length, int hval, int arity, int options);
static void		_std_did_init(void);
static void		_constant_table_init(int);


/*-----------------------------------------------------------------------------

The basic data structure for the dictionary is the struct dict_item.
A dictionary identifier (DID) is simply the address of such a dict_item.
A dict_item contains:

	- arity
	- pointer to a (Sepia-)string representing the name
	- procedure chain
	- property chain
	- collision chain
	- flags

dict_items are allocated in blocks of DICT_ITEM_BLOCK_SIZE (1024) elements.
The addresses of these blocks are kept in a directory array of size
DICT_DIRECTORY_SIZE (512). The maximum number of dictionary entries is thus
DICT_DIRECTORY_SIZE * DICT_ITEM_BLOCK_SIZE (524288).
This scheme is necessary to have a short 19-bit identifier (9 bits directory index,
10 bits block index) for DIDs, which is used to store variable names in the tag.
For all other purposes, a DID is stored directly as its 32-bit-address.

For finding DIDs when their name is given, there is a hash table of size
DICT_HASH_TABLE_SIZE. The hash value is computed from the name only, not
from the arity. Thus all functors with the same name hash onto the same
slot of the hash table (together with other functors whose name happens to
give the same hash value). All colliding entries are kept in a circular
chain, built using the 'next' field of the dict_items. The dict_item that
is referenced from the hash table is marked with the 'head' bit.

The circular collision chain is also used to find a functor that has the
same name but different arity as a given one (e.g. in functor/3), so no
new hashing is needed in this case, see function add_dict().

The strings holding the functor names are allocated separately from the
dict_items. All functors with the same name (but different arities) share
the same string. Note that, due to the current handling of strings in Sepia,
these strings are not only referenced by dict_items, but may also be pointed
to by TSTRG pwords from elsewhere. The dictionary strings look like standard
Sepia strings, but their tag is TBUFFER|IN_DICT|<ref_counter>.
The reference counter counts the number of references from dict_items only,
and is used to free the string when the last functor with this name disappears.
To make sure that referenced strings are not collected, the marking routine
marks the corresponding atom whenever a persistent string is encountered.

-----------------------------------------------------------------------------*/

#define DICT_DIRECTORY_SIZE	2048	
#define DICT_ITEM_BLOCK_SIZE	1024

#define DidBlock(i) ((i) >> 10)
#define DidOffset(i) ((i) & 0x3ff)
#define MakeBitField(block, offs) ((block)<<10|(offs))

/* values for the options for _in_dict_opt() */
#define IN_DICT_CHECK	0
#define IN_DICT_ENTER	1

#define NULL_DID	((dident) D_UNKNOWN)

#define Inc_Ref_Ctr(tag)	{ (tag) += 0x100; }
#define DecRefCtr(tag)		((tag) -= 0x100, (tag) & 0x0fffff00)


/* DICT_HASH_TABLE_SIZE must be a power of 2 (we use masking) */
#define DICT_HASH_TABLE_SIZE     1048576	

/* compute hash value and length of a NULL-terminated string */
#define Hash(id, hash, length) {					\
	register char *str = (id);					\
        for (length = hash = 0; *str; str++, length++)			\
            hash += (hash<<3) + *(unsigned char *)str;			\
        hash &= DICT_HASH_TABLE_SIZE-1;					\
}

/* compute hash value of a string of given length */
#define Hashl(id, hash, n) {						\
	register char *str = (id);					\
	register int length = (n);					\
        for (hash = 0; length > 0; str++, --length)			\
            hash += (hash<<3) + *(unsigned char *)str;			\
        hash &= DICT_HASH_TABLE_SIZE-1;					\
}


/*
 * Compare 2 strings of length length.
 * length is decremented and is 0 if the strings were equal
 */
#define Compare_N_Chars(length, s1, s2) {				\
	register char *aux1 = (s1), *aux2 = (s2);			\
	while (length) {						\
	    if (*aux1++ != *aux2++)					\
		break;							\
	    --length;							\
	}								\
}

#define DidInUse(d)	(DidString(d))


/*
 * TYPEDEFS and GLOBAL VARIABLES
 */

static struct dictionary {
	dident	hash_table[DICT_HASH_TABLE_SIZE];
	dident	directory[DICT_DIRECTORY_SIZE];	/* table of dict_item blocks */
	struct dict_item tag_did[NTYPES+1];	/* to hold type properties */
	a_mutex_t lock;		/* lock for hash table */
	int	dir_index;	/* next free directory slot */
	dident	free_item_list;	/* chain of free dict_items */
	int	items_free;	/* number of elements in this chain */
	int	table_usage;	/* number of hash slots in use */
	int	collisions;	/* number of hash collisions */
	int	gc_countdown;	/* remaining allocations before triggering gc */
	int	gc_interval;	/* remaining allocations before triggering gc */
	int	gc_number;	/* number of garbage collections so far */
	long	gc_time;	/* and the time they took */
	int	string_used;
	int	string_free;
} *dict;


void
dict_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	register int i;
	dict = (struct dictionary *) hg_alloc_size(sizeof(struct dictionary));
	shared_data->dictionary = (void_ptr) dict;
	for (i=0; i< DICT_HASH_TABLE_SIZE; i++)
	    dict->hash_table[i] = NULL_DID;
	for (i=0; i< DICT_DIRECTORY_SIZE; i++)
	    dict->directory[i] = NULL_DID;
	for (i=0; i <= NTYPES; i++)
	{
	    dict->tag_did[i].string = 0;
	    dict->tag_did[i].properties = 0;
	    dict->tag_did[i].macro = 0;
	}
	dict->dir_index = 0;
	dict->free_item_list = NULL_DID;
	dict->items_free = 0;
	dict->string_used = 0;
	dict->string_free = 0;
	dict->table_usage = 0;
	dict->collisions = 0;
	dict->gc_interval = DICT_ITEM_BLOCK_SIZE/16*15;
	/* set initial countdown high enough to make sure the first
	 * collection does not occur too early in the boot phase */
	dict->gc_countdown = 2*dict->gc_interval;
	dict->gc_number = 0;
	dict->gc_time = 0;
	a_mutex_init(&dict->lock);
    }
    if (flags & INIT_PRIVATE)
    {
	int i;

	dict = (struct dictionary *) shared_data->dictionary;
	_std_did_init();

	/* Tag descriptor array (more settings in bip_emu_init()) */
	for (i=0; i <= NTYPES; i++)
	{
	    tag_desc[i].super =
	    tag_desc[i].tag.kernel = (long) i;
	    tag_desc[i].order = 0;
	    tag_desc[i].type_name =
	    tag_desc[i].tag_name = D_UNKNOWN;
	}

	tag_desc[TLIST].tag_name = in_dict("list", 0);
	tag_desc[TCOMP].tag_name = in_dict("structure", 0);
	tag_desc[TSTRG].tag_name = d_.string0;
	tag_desc[TBIG].tag_name = in_dict("bignum", 0);
	tag_desc[TDBL].tag_name = d_.double0;
	tag_desc[TRAT].tag_name = d_.rational0;
	tag_desc[TSUSP].tag_name = d_.goal;
	tag_desc[THANDLE].tag_name = in_dict("handle", 0);
	tag_desc[TNIL].tag_name = d_.nil;
	tag_desc[TINT].tag_name = d_.integer0;
	tag_desc[TDICT].tag_name = d_.atom0;
	tag_desc[TPTR].tag_name = d_.meta0;

	tag_desc[TLIST].super = TCOMP;
	tag_desc[TCOMP].type_name = d_.compound0;
	tag_desc[TSTRG].type_name = d_.string0;
	tag_desc[TBIG].super = TINT;
	tag_desc[TINT].type_name = d_.integer0;
	tag_desc[TDBL].type_name = d_.float0;
	tag_desc[TRAT].type_name = d_.rational0;
	tag_desc[TSUSP].type_name = d_.goal;
	tag_desc[THANDLE].type_name = in_dict("handle", 0);
	tag_desc[TNIL].super = TDICT;
	tag_desc[TDICT].type_name = d_.atom0;
	tag_desc[TPTR].type_name = d_.meta0;
    }

    _constant_table_init(flags);
}


/*
 * Return dict_item for the specified type/tag.
 * It is used to attach properties to types, in particular macros.
 */

dident
transf_did(long int t)
{
    return (dident) &dict->tag_did[tag_desc[TagTypeC(t)].super];
}


/*
 * String allocation for dictionary.
 * These strings are write-once, read-only, except for dictionary gc.
 */

#define StringSize(length) (BufferSizePwords(length+1) * sizeof(pword))

static pword *
alloc_string(int length)
{
    pword *ptr;
    ptr = (pword *) hg_alloc_size((int) StringSize(length));
    return ptr;
}

static void
free_string(pword *ptr)
{
    hg_free_size((generic_ptr) ptr, (int) StringSize(ptr->val.nint));
}



/*
 * return a new dict_item
 *
 * Initializes all fields except .next
 * Free dict_items are in the free list and can be recognised also
 * by having a NULL string field.
 */

static dident
_alloc_dict_item(pword *dict_string, int arity)
{
    register dident dip;

    dip = dict->free_item_list;
    if (!dip)				/* free list empty, allocate a new block */
    {
	register int i;
	if (dict->dir_index == DICT_DIRECTORY_SIZE)
	    ec_panic("dictionary overflow", "atom/functor creation");
	dip =
	dict->free_item_list =
	dict->directory[dict->dir_index] =
	    (dident) hg_alloc_size(sizeof(struct dict_item) * DICT_ITEM_BLOCK_SIZE);
	for (i = 0; i < DICT_ITEM_BLOCK_SIZE; ++i)
	{
	    dip[i].bitfield = MakeBitField(dict->dir_index, i);
	    dip[i].string = (pword *) 0;
	    dip[i].arity = UNUSED_DID_ARITY;
	    dip[i].next = &dip[i+1];
	}
	dip[i-1].next = NULL_DID;
	dict->dir_index++;
	dict->items_free += DICT_ITEM_BLOCK_SIZE;
    }

    dip->string = dict_string;		/* initialize the dict_item */
    Inc_Ref_Ctr(dict_string->tag.kernel);
    dip->arity = arity;
    dip->procedure = 0;
    dip->properties = 0;
    dip->macro = 0;
    dip->attainable = 0;
    dip->module = 0;
    dip->isop = 0;
    dip->head = 0;
    dip->stability = 0;

    dict->free_item_list = dip->next; /* unlink it from the free list */
    dict->items_free--;
    if (--dict->gc_countdown == 0)
    {
	pword pw;
	Make_Atom(&pw, d_.garbage_collect_dictionary);
	ec_post_event(pw);
    }
    return dip;
}


dident
in_dict(char *name, int arity)
{
    register int hval, len;
    register dident dip;
    Hash(name, hval, len);
    dip = _in_dict_opt(name, len, hval, arity, IN_DICT_ENTER);
    Set_Did_Stability(dip, DICT_PERMANENT);
    return dip;
}

dident Winapi
ec_did(const char *name, const int arity)
{
    register int hval, len;
    register dident dip;
    Hash((char *)name, hval, len);
    dip = _in_dict_opt((char *) name, len, hval, arity, IN_DICT_ENTER);
    Set_Did_Stability(dip, DICT_PERMANENT);
    return dip;
}

dident
enter_dict(char *name, int arity)
{
    register int hval, len;
    Hash(name, hval, len);
    return _in_dict_opt(name, len, hval, arity, IN_DICT_ENTER);
}

dident
enter_dict_n(char *name, register long int len, int arity)
{
    register int hval;
    Hashl(name, hval, len);
    return _in_dict_opt(name, (int) len, hval, arity, IN_DICT_ENTER);
}

dident
check_did_n(char *name, long int len, int arity)
{
    register int hval;
    Hashl(name, hval, len);
    return _in_dict_opt(name, (int) len, hval, arity, IN_DICT_CHECK);
}

pword *
enter_string_n(char *name, long int len, int stability)
{
    register int hval;
    register dident dip;
    Hashl(name, hval, len);
    dip = _in_dict_opt(name, (int) len, hval, 0, IN_DICT_ENTER);
    Set_Did_Stability(dip, stability);
    return DidString(dip);
}

dident
bitfield_did(long int bf)
{
    return (dident) (dict->directory[DidBlock(bf)] + DidOffset(bf));
}


/*
 * _in_dict_opt(name, length, hval, arity, options)
 *	options are IN_DICT_CHECK or IN_DICT_ENTER
 *
 * We guarantee that functors with the same name always share their name string!
 *
 * We only lock on dictionary modifications, assuming that dids are
 * never removed under our feet. This means that for dictionary gc's
 * we have to stop all workers!
 */

static dident
_in_dict_opt(char *name,	/* might not be NUL-terminated! */
	register int length,
	int hval,
	int arity,
	int options)
{
    register dident dip;
    register dident start;
    register pword *dict_string;

    start = dict->hash_table[hval];
    dict_string = (pword *) 0;
    if (start)
    {
	dip = start;
	do
	{
	    if (!dict_string)
	    {
		if (DidLength(dip) == length)
		{
		    register long cmp = length;
		    Compare_N_Chars(cmp, name, DidName(dip));
		    if (!cmp)		/* name found */
		    {
			if (DidArity(dip) == arity)
			    return (dident) dip;
			else
			    dict_string = DidString(dip);
		    }
		}
	    }
	    else if (DidString(dip) == dict_string && DidArity(dip) == arity)
		return (dident) dip;
	    dip = dip->next;
	} while (dip != start);
    }
    if (options == IN_DICT_CHECK)
	return (dident) NULL_DID;

    if (!dict_string)	/* a functor with a new name */
    {
	dict->string_used += length+1;
	dict_string = alloc_string(length);
	Set_Buffer_Size(dict_string, length+1);
	dict_string->tag.kernel = TBUFFER|IN_DICT;
	Copy_Bytes((char *)(dict_string+1), name, (int) (length));
	((char *)(dict_string+1))[length] = 0;
	if (start)
	    dict->collisions++;
    }
    dip = _alloc_dict_item(dict_string, arity);
    a_mutex_lock(&dict->lock);
    if (start)
    {
	dip->next = start->next;
	start->next = dip;
    }
    else	/* the first entry in this hash slot */
    {
	dip->next = dip;
	dip->head = 1;
	dict->hash_table[hval] = dip;
	dict->table_usage++;
    }
    a_mutex_unlock(&dict->lock);
    return (dident) dip;
}


dident
add_dict(register dident old_did, register int new_arity)
{
    register dident	dip;

    dip = (dident) old_did;
    do {
	if (DidArity(dip) == new_arity && DidString(dip) == DidString(old_did))
	    return (dident) dip;
	dip = dip->next;
    } while (dip != DidPtr(old_did));

    /* not found, make a new entry */
    dip = _alloc_dict_item(DidString(old_did), new_arity);
    a_mutex_lock(&dict->lock);
    dip->next = DidNext(old_did);
    DidNext(old_did) = dip;
    a_mutex_unlock(&dict->lock);
    return (dident) dip;
}

dident
check_did(register dident old_did, register int new_arity)
{
    register dident	dip = (dident) old_did;

    do {
	if (DidArity(dip) == new_arity && DidString(dip) == DidString(old_did))
	    return (dident) dip;
	dip = dip->next;
    } while (dip != DidPtr(old_did));
    return D_UNKNOWN;
}


/*
 * int next_functor()
 *
 * A support function to scan the dictionary. It is used to implement
 * current_functor/1 and the like.
 * The update semantics of this function is unclear (i.e. if a new
 * functor is entered between successive calls of next_functor(),
 * it will be returned or not, depending of where it is inserted).
 * Note also that dictionary GCs might happen between successive calls
 * to this function, which has similar consequences.
 * However, the function is at least robust and will not crash.
 *
 * To be used like:
 *
 *	int	idx = 0;
 *	dident	did;
 *
 *	while (next_functor(&idx, &did))
 *	{
 *		<use did>
 *	}
 */

int
next_functor(			/* returns 0 when dictionary exhausted	*/
    	int *pidx,		/* in/out: current dict index		*/
	dident *pdid)		/* output: valid did			*/
{
    register dident dip;
    register int idx = *pidx;

    while (DidBlock(idx) < dict->dir_index)
    {
	dip = dict->directory[DidBlock(idx)];
	if (dip)
	{
	    dip += DidOffset(idx);
	    do
	    {
		idx++;
		if (DidInUse(dip))
		{
		    *pdid = (dident) dip;
		    *pidx = idx;
		    return 1;
		}
		dip++;
	    } while (DidOffset(idx));
	}
	else
	    idx = (DidBlock(idx) + 1) * DICT_ITEM_BLOCK_SIZE;
    }
    return 0;
}



/*--------------------------------------------------------------
 * Dictionary garbage collection
 *--------------------------------------------------------------*/

/*
 * _tidy_dictionary()
 */

#define Useful(d)	((d)->attainable || (d)->stability > DICT_VOLATILE \
			|| (d)->procedure || (d)->properties)

#if 0

/*
 * The free list is built such that the oldest dids are reused first in order
 * to quickly fill did blocks again, so that they are more or less read-only
 * afterwards.
 * Another advantage of this scheme is that we can easily detect when a block
 * becomes completely unused, and then free the whole block.
 */

static void
_tidy_dictionary0(void)
{
    int block, idx;
    register dident dip, aux;
    register dident *free_list_tail = &dict->free_item_list;

    *free_list_tail = NULL_DID;
    for (block = 0; block < dict->dir_index; block++)
    {
	dip = dict->directory[block];
	for (idx = 0; idx < DICT_ITEM_BLOCK_SIZE; idx++, dip++)
	{
	    if (DidInUse(dip) && Useful(dip))
	    {
		dip->attainable = 0;
		continue;
	    }
	    else if (DidInUse(dip))		/* a garbage did, unlink it */
	    {
		/* Tidy the collision chain in which dip occurs. This assumes that
		 * all DIDs with this name are in the same chain!
		 */
		register dident prev = dip;
		int head_removed = 0;

		do
		{
		    aux = prev->next;
		    if (Useful(aux))		/* no garbage, skip it */
		    {
			prev = aux;
			continue;
		    }
		    else			/* remove aux */
		    {
			pword *str = DidString(aux);
			prev->next = aux->next;
			aux->next = NULL_DID;
			dict->items_free++;
			if (DecRefCtr(str->tag.kernel) == 0)
			{
			    dict->string_used -= str->val.nint + 1;
			    free_string(str);
			    /*
			    p_fprintf(current_err_, "%s/%d (with string)\n",
							DidName(aux), DidArity(aux));
			    */
			}
			/*
			else
			{
			    p_fprintf(current_err_, "%s/%d\n",
							DidName(aux), DidArity(aux));
			}
			*/
			aux->string = (pword *) 0;
			aux->arity = -1;
			if (aux->head)
			    head_removed = 1;
		    }
		} while (aux != dip);

		if (head_removed)
		{
		    register char *dummy1;
		    register int dummy2;
		    register hval;
		    Hash(DidName(dip), hval, dummy2, dummy1);
		    if (prev != dip)
		    {
			prev->head = 1;
			dict->hash_table[hval] = prev;
		    }
		    else	/* we removed all chain elements */
		    {
			dict->hash_table[hval] = NULL_DID;
			dict->table_usage--;
		    }
		}
	    } /* else: an already unlinked garbage did */
	    *free_list_tail = dip;		/* add it to the free list */
	    free_list_tail = &dip->next;
	}
    }
    *free_list_tail = NULL_DID;
    Succeed_;
}

#endif /* 0 */

/*
 * alternatively, scan through the hash table
 */

static void
_tidy_dictionary(void)
{
    int idx;
    register dident dip;
    register dident prev;

    for (idx = 0; idx < DICT_HASH_TABLE_SIZE; idx++)
    {
	prev = dict->hash_table[idx];
	if (prev)
	{
	    do
	    {
		dip = prev->next;
		if (Useful(dip))
		{
		    dip->attainable = 0;
		    prev = dip;
		}
		else		/* a garbage did, unlink it */
		{
		    pword *str = DidString(dip);
		    prev->next = dip->next;
		    /*
		    p_fprintf(current_err_, "\n%s/%d", DidName(dip), DidArity(dip));
		    */
		    if (DecRefCtr(str->tag.kernel) == 0)
		    {
			dict->collisions--;
			dict->string_used -= str->val.nint + 1;
			free_string(str);
			/*
			p_fprintf(current_err_, " (with string)");
			*/
		    }
		    /* add it to the free list */
#ifdef DEBUG_DICT
		    dip->arity = (word) dip->string;
		    dip->string = (pword *) 0;
#else
		    dip->arity = UNUSED_DID_ARITY;
		    dip->string = (pword *) 0;
		    dip->next = dict->free_item_list;
		    dict->free_item_list = dip;
		    dict->items_free++;
#endif
		    if (dip->head)		/* removing the chain header */
		    {
			if (prev != dip)
			{
			    prev->head = 1;
			    dict->hash_table[idx] = prev;
			}
			else	/* we removed all chain elements */
			{
			    dict->hash_table[idx] = NULL_DID;
			    dict->collisions++;	/* was not a collision */
			    dict->table_usage--;
			}
		    }
		}
	    } while (!dip->head);
	}
    }
}

static void
_mark_dids_from_procs(pri *proc)
{
    for (; proc; proc = PriNext(proc))
    {
	if (proc->module_def)
	    Mark_Did(proc->module_def);
	if (proc->module_ref)
	    Mark_Did(proc->module_ref);
	if (proc->trans_function)
	    Mark_Did(proc->trans_function);
	if (DynamicProc(proc))
	    ec_mark_dids_dyn_code(PriCode(proc));
	/* PriDid does not need to be marked because it has a procedure
	 * and will therefore not be collected */
    }
}

int
ec_gc_dictionary(void)
{
    int		usage_before, garbage, idx = 0;
    dident	d;
    long	gc_time;
    extern int	in_exception(void);
    extern void mark_dids_from_properties(property *prop_list),
		mark_dids_from_stacks(long int arity),
		mark_dids_from_streams(void);

    dict->gc_countdown = dict->gc_interval;

    if (!(GlobalFlags & GC_ENABLED)	/* if switched off */
	|| ec_options.parallel_worker	/* or heap is shared */
	|| g_emu_.nesting_level > 1	/* or when emulators are nested */
	|| in_exception())		/* or inside exception */
    {
	Succeed_;			/* then don't gc (not implemented) */
    }
    
#ifndef PRINTAM
    Disable_Int()
#endif

    if (GlobalFlags & GC_VERBOSE)
    {
	(void) ec_outfs(log_output_,"DICTIONARY GC .");
	ec_flush(log_output_);
    }

    usage_before = dict->dir_index * DICT_ITEM_BLOCK_SIZE -
			dict->items_free;
    gc_time = user_time();

    mark_dids_from_stacks(0L);		/* mark the abstract machine's data */

    while (next_functor(&idx, &d))	/* mark from all the properties */
    {
	if (DidProc(d))
	    _mark_dids_from_procs(DidProc(d));
	if (DidProperties(d))
	    mark_dids_from_properties(DidProperties(d));
    }

    mark_dids_from_streams();		/* mark from the stream descriptors */

    if (GlobalFlags & GC_VERBOSE)
    {
	(void) ec_outfs(log_output_,"."); ec_flush(log_output_);
    }

    _tidy_dictionary();

    gc_time = user_time() - gc_time;
    dict->gc_number++;
    dict->gc_time += gc_time;
    garbage = usage_before - (dict->dir_index * DICT_ITEM_BLOCK_SIZE -
				dict->items_free);

#ifndef PRINTAM
    Enable_Int()
#endif

    if (GlobalFlags & GC_VERBOSE)
    {
	p_fprintf(log_output_, ". %d - %d, (%.1f %%), time: %.3f\n",
	    usage_before,
	    garbage,
	    (100.0*garbage)/usage_before,
	    (float)gc_time/clock_hz);
	ec_flush(log_output_);
    }

    Succeed_;
}



/*--------------------------------------------------------------
 * Statistics and debugging
 *--------------------------------------------------------------*/

/*ARGSUSED*/
int
ec_dict_param(value vwhat, type twhat, value vval, type tval)
{
    pword result;

    result.tag.kernel = TINT;
    switch(vwhat.nint)
    {
    case 0:	/* # entries */
	result.val.nint = dict->dir_index * DICT_ITEM_BLOCK_SIZE -
				dict->items_free;
	break;
    case 1:	/* # free items */
	result.val.nint = dict->items_free;
	break;
    case 2:	/* hash table size */
	result.val.nint = DICT_HASH_TABLE_SIZE;
	break;
    case 3:	/* hash table usage */
	result.val.nint = dict->table_usage;
	break;
    case 4:	/* collisions */
	result.val.nint = dict->collisions;
	break;
    case 5:	/* gc_number */
	result.val.nint = dict->gc_number;
	break;
    case 6:	/* gc time */
	Return_Unify_Float(vval, tval, dict->gc_time/clock_hz);
    case 7:	/* set or get the gc interval */
	if (IsInteger(tval))
	{
	    dict->gc_countdown = dict->gc_interval = vval.nint;
	}
	result.tag.kernel = TINT;
	result.val.nint = dict->gc_interval;
	break;
    default:
	Fail_;
    }
    Return_Unify_Pw(vval, tval, result.val, result.tag);
}


/*
 * auxiliary functions for debugging
 */

#ifdef PRINTAM
#ifndef lint

pr_functors(value v, type t)
{
    register dident dip;
    int index, len;

    Check_Integer(t);
    for (index = 0; index < DICT_HASH_TABLE_SIZE; index++)
    {
	dip = dict->hash_table[index];
	if (dip)
	{
	    len = 0;
	    do {
		len++;
		dip = dip->next;
	    } while (!dip->head);
	    if (dip != dict->hash_table[index])
		p_fprintf(current_output_,"BAD COLLISION LIST\n");
	    if (len >= v.nint)
	    {
		p_fprintf(current_output_,"[%d]", index);
		do {
		    p_fprintf(current_output_," %s/%d",
				DidName(dip), DidArity(dip));
		    dip = dip->next;
		} while (!dip->head);
		p_fprintf(current_output_,"\n");
	    }
	}
    }
    Succeed_;
}

pr_dict(void)
{
    p_fprintf(current_output_, "blocks allocated = %d\n", dict->dir_index);
    p_fprintf(current_output_, "items used = %d\n",
		dict->dir_index*DICT_ITEM_BLOCK_SIZE-dict->items_free);
    p_fprintf(current_output_, "items free = %d\n", dict->items_free);
    p_fprintf(current_output_, "string_used = %d\n", dict->string_used);
    p_fprintf(current_output_, "table_usage = %d/%d\n",
				dict->table_usage, DICT_HASH_TABLE_SIZE);
    p_fprintf(current_output_, "table_usage ratio = %.1f%%\n",
				100.0*dict->table_usage/DICT_HASH_TABLE_SIZE);
    p_fprintf(current_output_, "collisions = %d\n", dict->collisions);
    p_fprintf(current_output_, "collision ratio = %.1f%%\n",
				100.0*dict->collisions/dict->table_usage);
    p_fprintf(current_output_, "gc countdown = %d\n", dict->gc_countdown);
    p_fprintf(current_output_, "gc number = %d\n", dict->gc_number);
    p_fprintf(current_output_, "gc time = %.3f\n",
				(float)dict->gc_time/clock_hz);
    Succeed_;
}


/*
 * 	help debugging: print a dictionary entry
*/
static dident
_pdict(dident entry)
{
    pri	*proc;

    p_fprintf(current_err_, "%s/", DidName(entry));
    p_fprintf(current_err_, "%d", DidArity(entry));
    p_fprintf(current_err_, "\n length=%d ", DidLength(entry));
    p_fprintf(current_err_, "module=%d ", DidModule(entry));
    p_fprintf(current_err_, "macro=%d ", DidMacro(entry));
    p_fprintf(current_err_, "attainable=%d ", DidAttainable(entry));
    p_fprintf(current_err_, "stability=%d ", DidStability(entry));
    p_fprintf(current_err_, "head=%d ", DidPtr(entry)->head);
    p_fprintf(current_err_, "isop=%d", DidIsOp(entry));
    p_fprintf(current_err_, "\n next=%x ", DidPtr(entry)->next);
    p_fprintf(current_err_, "properties=%x ", DidProperties(entry));
    proc = DidPtr(entry)->procedure;
    p_fprintf(current_err_, "\n proc=0x%x", proc);
    if (proc) {
	p_fprintf(current_err_, "(code=0x%x", PriCode(proc));
	p_fprintf(current_err_, " next=0x%x", PriNext(proc));
	p_fprintf(current_err_, " module=%d", PriModule(proc));
	p_fprintf(current_err_, " flags=0x%x", PriFlags(proc));
	p_fprintf(current_err_, " did=0x%x)", PriDid(proc));
    }
    (void) ec_newline(current_err_);
    return entry;
}

#endif /* lint */
#endif /* PRINTAM */


static void
_std_did_init(void)
{
	/* The first did entered is the empty name. This is used for
	 * unknown variable names. It has a zero bitfield representation.
	 */
	d_.empty = 	in_dict("", 0);

	d_.semi0 = 	in_dict(";", 0);
	d_.naf = 	in_dict("\\+", 1);
	d_.not1 = 	in_dict("not", 1);
	d_.fail_if = 	in_dict("fail_if", 1);
	d_.once =	in_dict("once", 1);
	d_.not_unify = 	in_dict("\\=", 2);
	d_.diff_reg =   in_dict("~=",2);
	d_.not_identical = 	in_dict("\\==", 2);
	d_.not_equal =	in_dict("=\\=", 2);

	d_.comment = 	in_dict("/*", 0);
	d_.eocl = 	in_dict( ".", 0);
	d_.eof = 	in_dict( "end_of_file", 0);
	d_.list = 	in_dict( ".", 2);
	d_.rulech0 = 	in_dict(":-",0);
	d_.rulech1 = 	in_dict( ":-", 1);
	d_.rulech2 = 	in_dict( ":-", 2);
	d_.goalch = 	in_dict( "?-", 1);
	d_.grammar = 	in_dict("-->", 2);
	d_.nil = 	in_dict( "[]", 0);
	d_.fail = 	in_dict("fail",0);
	d_.nilcurbr = 	in_dict( "{}", 0);
	d_.nilcurbr1 = 	in_dict( "{}", 1);
	d_.eoi = 	in_dict( "\004", 0);
	d_.cond = 	in_dict( "->", 2);
	d_.ampersand = 	in_dict( "&", 2);
	d_.cut = 	in_dict( "!", 0);
	d_.syscut = 	in_dict( "syscut", 0);
	d_.cut_to = 	in_dict( "cut_to", 1);
        d_.arg =	in_dict("arg", 3);
        d_.subscript =	in_dict("subscript", 2);
	d_.comma = 	in_dict( ",", 2);
	d_.semicolon = 	in_dict( ";", 2);
	d_.colon =	in_dict(":", 2);
	d_.bar = 	in_dict( "|", 2);
	d_.uref = 	in_dict( "_", 0);
      	d_.univ = 	in_dict("=..", 2);
		/* arithmetic */
	d_.plus1 = 	in_dict("+", 1);
	d_.plus = 	in_dict("+", 2);
	d_.minus1 = 	in_dict("-", 1);
	d_.minus = 	in_dict("-", 2);
	d_.times = 	in_dict("*", 2);
	d_.inf = 	in_dict("<", 2);
	d_.sup = 	in_dict(">", 2);
	d_.supq = 	in_dict(">=", 2);
	d_.infq = 	in_dict("=<", 2);
	d_.inf0 = 	in_dict("<", 0);
	d_.sup0 = 	in_dict(">", 0);
	d_.supq0 = 	in_dict(">=", 0);
	d_.infq0 = 	in_dict("=<", 0);
	d_.quotient = 	in_dict("/",2);
	d_.div = 	in_dict("//", 2);
	d_.modulo = 	in_dict("mod", 2);
	d_.equal = 	in_dict("=:=", 2);
	d_.is = 	in_dict("is",2);	
	d_.rshift = 	in_dict(">>", 2);
	d_.lshift = 	in_dict("<<", 2);
	d_.and2 = 	in_dict("/\\",2);
	d_.or2 = 	in_dict("\\/", 2);
	d_.power = 	in_dict("^", 2);
	d_.bitnot = 	in_dict("\\", 1);
	d_.min =	in_dict("min",2);
	d_.minint =	in_dict("minint",1);
	d_.max =	in_dict("max",2);
	d_.maxint =	in_dict("maxint",1);
	d_.abs =	in_dict("abs",1);
	d_.xor2 =	in_dict("xor",2);
	d_.pi =		in_dict("pi",0);
	d_.e =		in_dict("e",0);	
	d_.sin =	in_dict("sin",1);
	d_.cos =	in_dict("cos",1);
	d_.tan =	in_dict("tan",1);
	d_.asin =	in_dict("asin",1);
	d_.acos =	in_dict("acos",1);
	d_.atan =	in_dict("atan",1);
	d_.exp =	in_dict("exp",1);
	d_.sqrt =	in_dict("sqrt",1);
	d_.ln =		in_dict("ln",1);
	d_.fix =	in_dict("fix",1);
	d_.float1 =	in_dict("float",1);
	d_.breal1 =	in_dict("breal",1);
	d_.breal_from_bounds = in_dict("breal_from_bounds",1);
	d_.breal_min = in_dict("breal_min",1);
	d_.breal_max = in_dict("breal_max",1);
	d_.round =	in_dict("round",1);
	d_.floor1 =	in_dict("floor",1);
	d_.rational1 =	in_dict("rational",1);
	d_.numerator1 =	in_dict("numerator",1);
	d_.denominator1 = in_dict("denominator",1);

		/* term comparison */
	d_.unify = 	in_dict("=", 2);
	d_.identical = 	in_dict("==", 2);
	d_.less = 	in_dict("@<", 2);
	d_.lessq = 	in_dict("@=<", 2);
	d_.greater = 	in_dict("@>", 2);
	d_.greaterq = 	in_dict("@>=", 2);

	d_.reset = 	in_dict("reset",0);
	d_.block =	in_dict("block", 3);
	d_.exit_block = in_dict("exit_block",1);
	d_.call = 	in_dict("call", 1);
	d_.call_body = 	in_dict("call", 2);
	d_.metacall = 	in_dict("call", 3);
	d_.go = 	in_dict("go", 0);
	d_.break0 =	in_dict("break", 0);
	d_.local_break = in_dict("local_break", 0);
	d_.compile = 	in_dict("compile",1);
	d_.pcompile = 	in_dict("pcompile", 3);
	d_.error = 	in_dict("error",2);
	d_.syserror = 	in_dict("syserror", 4);
	d_.user = 	in_dict("user", 0);
	d_.true0 = 	in_dict("true", 0);
	d_.default0 = 	in_dict("default", 0);
	d_.read = 	in_dict("read",0);
	d_.write = 	in_dict("write",0);
	d_.update =	in_dict("update",0);
	d_.append =	in_dict("append", 0);
	d_.string =	in_dict("string", 1);
	d_.input = 	in_dict("input",0);
	d_.output = 	in_dict("output",0);
	d_.err = 	in_dict("error",0);
	d_.answer = 	in_dict("answer",0);
	d_.dummy_call =	in_dict("dummy_call",0);
	d_.no_err_handler =	in_dict("no_err_handler",2);
	d_.exit_postponed =	in_dict("exit_postponed",0);
	d_.error_handler =	in_dict("error_handler",2);
	d_.call_explicit =	in_dict("call_explicit",2);
	d_.garbage_collect_dictionary = in_dict("garbage_collect_dictionary",0);
	d_.handle_expired_while_thrown = in_dict("handle_expired_while_thrown",0);

	d_.hang =	in_dict("hang",0);
	d_.nohang =	in_dict("nohang",0);

	d_.warning_output = 	in_dict("warning_output",0);
	d_.log_output = 	in_dict("log_output",0);
	d_.null = 		in_dict("null", 0);
	d_.flush = 		in_dict("flush",0);
	d_.emulate = 		in_dict("Emulate",0);
	d_.abort = 		in_dict("abort",0);
	d_.eerrno =		in_dict("sys_errno", 0);
	d_.cprolog = 		in_dict("cprolog", 0);
	d_.bsi = 		in_dict("bsi", 0);
	d_.quintus = 		in_dict("quintus", 0);
	d_.sicstus = 		in_dict("sicstus", 0);
	d_.var = 		in_dict("var", 1);
	d_.nonground = 		in_dict("nonground", 1);
	d_.ground = 		in_dict("ground", 1);
	d_.on =			in_dict("on", 0);
	d_.off =		in_dict("off", 0);
	d_.prolog =		in_dict("prolog", 0);
	d_.system =		in_dict("system", 0);
	d_.built_in =		in_dict("built_in", 0);

		/* assert */
	d_.clause =	in_dict("clause", 3);

	d_.halt = 	in_dict("halt",0);
	d_.halt0 = 	in_dict("halt0",0);
	d_.debugger =	in_dict("debugger", 0);

		/* declarations */
	d_.dynamic = 	in_dict("dynamic",1);
	d_.abolish = 	in_dict("abolish",1);
	d_.mode = 	in_dict("mode",1);
	d_.delay =	in_dict("delay", 1);
	d_.if2 =		in_dict("if", 2);
	d_.local = 	in_dict("local",1);
	d_.global = 	in_dict("global",1);
	d_.export1 = 	in_dict("export",1);
	d_.import = 	in_dict("import",1);
	d_.from = 	in_dict("from",2);
	d_.module1 = 	in_dict("module", 1);
	d_.module_directive = 	in_dict("module_directive", 4);

		/* module names */
	d_.default_module =	in_dict(ec_options.default_module, 0);
	d_.eclipse_home =	in_dict(ec_eclipse_home, 0);
	d_.kernel_sepia = in_dict("sepia_kernel", 0);
	d_.cn =		in_dict("cn", 0);

	       /* operators */
	d_.local0	= in_dict("local", 0);
	d_.global0	= in_dict("global", 0);

		/* debugger */
	d_.sepia =		in_dict("sepia", 0);
	d_.macro = 		in_dict("macro", 0);
	d_.skip = 		in_dict("skip", 0);
	d_.spy = 		in_dict("spy", 0);
	d_.leash = 		in_dict("leash", 0);
	d_.command = 		in_dict("command", 0); 
	d_.ellipsis =		in_dict("...",0);

		/* modes */
	d_.plus0 =		in_dict("+", 0);
	d_.plusplus =		in_dict("++", 0);
	d_.minus0 =		in_dict("-", 0);
	d_.question =		in_dict("?", 0);

        d_.unify0 = in_dict("=", 0);
        d_.stop = in_dict("stop", 0);
        d_.print = in_dict("print", 0);
        d_.notrace = in_dict("notrace", 0);
        d_.trace = in_dict("trace", 0);
        d_.trace_frame = in_dict("tf", TF_ARITY);
        d_.debug = in_dict("debug", 0);
        d_.nodebug = in_dict("nodebug", 0);
        d_.global_trail_overflow = in_dict("global_trail_overflow", 0);
        d_.local_control_overflow = in_dict("local_control_overflow", 0);

	d_.at2 = in_dict("@", 2);
	d_.lock = in_dict("lock", 1);
	d_.localb = in_dict("local_", 2);
	d_.globalb = in_dict("global_", 2);
	d_.exportb = in_dict("export_", 2);
	d_.import_fromb = in_dict("import_from_", 3);
	d_.write1 = in_dict("write", 1);
	d_.write2 = in_dict("write", 2);
	d_.writeq1 = in_dict("writeq", 1);
	d_.writeq2 = in_dict("writeq", 2);
	d_.read1 = in_dict("read", 1);
	d_.read2 = in_dict("read", 2);
	d_.define_global_macro3 = in_dict("define_global_macro",3);
	d_.define_local_macro3 = in_dict("define_local_macro",3);
	d_.erase_macro1 = in_dict("erase_macro",1);
	d_.trans_term = in_dict("trans_term",2);

        d_.var0 = in_dict("var", 0);
        d_.atom0 = in_dict("atom", 0);
        d_.string0 = in_dict("string", 0);
	d_.float0 = in_dict("float",0);
        d_.integer0 = in_dict("integer", 0);
        d_.double0 = in_dict("double", 0);
        d_.rational0 = in_dict("rational", 0);
        d_.real0 = in_dict("real", 0);
	d_.byte = in_dict("byte", 0);
        d_.compound0 = in_dict("compound", 0);
	d_.universally_quantified = in_dict("universally_quantified", 0);
	d_.suspending = in_dict("suspending", 0);
	d_.suspend_attr = in_dict("suspend", 3);
	d_.constrained = in_dict("constrained", 0);
	d_.meta0 = in_dict("meta", 0);
	d_.free = in_dict("free",0);

	d_.stdin0 = in_dict("stdin", 0);
	d_.stdout0 = in_dict("stdout", 0);
	d_.stderr0 = in_dict("stderr", 0);

	/* macros */
	d_.top_only = in_dict("top_only", 0);
	d_.protect_arg = in_dict("protect_arg", 0);
	d_.clause0 = in_dict("clause", 0);
	d_.goal = in_dict("goal", 0);

	d_.with2 =		in_dict("with", 2);
	d_.with_attributes2 =	in_dict("with attributes", 2);
	d_.apply2 =		in_dict("apply", 2);

	d_.some = in_dict("some", 0);
	d_.all = in_dict("all", 0);

	/* compiler */
	d_.compile_stream = in_dict("compile_stream", 1);
	d_.system_debug = in_dict("system_debug", 0);
	d_.file_query = in_dict("file_query_body", 3);
	d_.external = in_dict("external", 0);
	d_.term = in_dict("term", 0);
	d_.not_not = in_dict("not not", 1);
	d_.softcut = in_dict("*->", 2);
	d_.functor = in_dict("functor", 3);
	d_.integer = in_dict("integer", 1);
        d_.double1 = in_dict("double", 1);
	d_.atom = in_dict("atom", 1);
	d_.atomic = in_dict("atomic", 1);
	d_.nonvar = in_dict("nonvar", 1);
	d_.meta = in_dict("meta", 1);
	d_.number = in_dict("number", 1);
	d_.real = in_dict("real", 1);
	d_.breal = in_dict("breal", 1);
	d_.compound = in_dict("compound", 1);
	d_.free1 = in_dict("free", 1);
	d_.bignum = in_dict("bignum", 1);
	d_.is_event = in_dict("is_event", 1);
	d_.is_handle = in_dict("is_handle", 1);
	d_.is_list = in_dict("is_list", 1);
	d_.is_suspension = in_dict("is_suspension", 1);
	d_.pragma = in_dict("pragma", 1);
	d_.make_suspension = in_dict("make_suspension", 3);
	d_.wake = in_dict("wake", 0);
	d_.state = in_dict("state", 0);
	d_.priority = in_dict("priority", 0);
	d_.invoc = in_dict("invoc", 0);
	d_.module0 = in_dict("module", 0);
}



/*--------------------------------------------------------------------
 * Constant table for storing non-simple ground constants
 * other than strings and atoms.
 * Entries are made
 *	- for constants occurring in code
 *	- explicitly by calling canonical_copy/2
 * There is currently no garbage collection on this table.
 * Terms in this table are made persistent, which means that pointers
 * to these terms (and their subterms) can always be shared and never
 * need to be copied again. This is indicated by the PERSISTENT bit
 * being set in pointers (in)to these persistent heap term.
 * Also, DIDs within these terms are marked as permanent,
 * so the dictionary gc does not need to scan this table.
 *--------------------------------------------------------------------*/

#define CONSTANT_TABLE_MIN_SIZE		256
#define CONSTANT_TABLE_MAX_SIZE		1048576
#define CONSTANT_TABLE_EXPAND_FACTOR	2


typedef struct constant_entry {			/* one table entry */
    	pword			value;
    	uword			hash;
    	struct constant_entry	*next;
} t_constant_entry;

static struct constant_table {			/* the whole table */
	uword			size;
	uword			nentries;
	uword			nreuse;
	t_constant_entry	**htable;
} *constant_table;


/*
 * Initialise the table
 */

static void
_constant_table_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	uword i;
	constant_table = (struct constant_table *) hg_alloc_size(sizeof(struct constant_table));
	shared_data->constant_table = (void_ptr) constant_table;
	constant_table->size = CONSTANT_TABLE_MIN_SIZE;
	constant_table->nentries = 0;
	constant_table->nreuse = 0;
	constant_table->htable = (t_constant_entry **)
		hg_alloc_size(constant_table->size * sizeof(t_constant_entry *));
	for (i=0; i< constant_table->size; i++)
	    constant_table->htable[i] = NULL;
    }
    if (flags & INIT_PRIVATE)
    {
	constant_table = (struct constant_table *) shared_data->constant_table;
    }
}


/*
 * Grow the table
 */

static void
_constant_table_expand(struct constant_table *table)
{
    uword new_size = table->size * CONSTANT_TABLE_EXPAND_FACTOR;
    t_constant_entry **new_htable;
    uword i;

    /* make and initialize a larger table */
    new_htable = (t_constant_entry **)
	    hg_alloc_size(new_size * sizeof(t_constant_entry *));
    for (i = 0; i < new_size; ++i)
    {
	new_htable[i] = NULL;
    }

    /* redistribute the entries from the old table */
    for (i = 0; i < table->size; ++i)
    {
	t_constant_entry *elem;
	for(elem = table->htable[i]; elem; )
	{
	    t_constant_entry **new_slot = &new_htable[elem->hash % new_size];
	    t_constant_entry *next_elem = elem->next;
	    elem->next = *new_slot;
	    *new_slot = elem;
	    elem = next_elem;
	}
    }

    /* free the old table */
    hg_free_size(table->htable, table->size * sizeof(t_constant_entry *));

    /* assign the new one */
    table->htable = new_htable;
    table->size = new_size;
}


/*
 * Lookup/Enter
 *
 * PSUCCEED:		*presult contains the tabled copy of (v,t)
 *			    or (v,t) itself in case of simple terms
 * PFAIL:		the term cannot be tabled in a meaningful way,
 *			    e.g. because it is a bounded real
 *			    (v,t) itself is returned as result anyway
 * INSTANTIATION_FAULT:	the term was nonground
 * other:		a serious problem occurred
 */

int
ec_constant_table_enter(value v, type t, pword *presult)
{
    int res = PSUCCEED;		/* initialise for ec_term_hash() */
    t_constant_entry *pelem;
    t_constant_entry **pslot;
    uword hash;

    /* no point tabling simple (single-pword) things */
    if (IsSimple(t))
    {
	presult->val.all = v.all;
	presult->tag.all = t.all;
	return PSUCCEED;
    }

    /*
     * Bounded reals cannot be shared (when nonzero width)
     * because they must not arithmetically compare equal!
     */
    if (IsInterval(t) &&  (IvlLwb(v.ptr) < IvlUpb(v.ptr)))
    {
	presult->val.all = v.all;
	presult->tag.all = t.all;
	return PFAIL;
    }

    /* compute hash value */
    hash = ec_term_hash(v, t, MAX_U_WORD, &res);
    if (res != PSUCCEED)
    {
	return res;
    }

    /* lookup the entry */
    pslot = &constant_table->htable[hash % constant_table->size];
    for(pelem = *pslot; pelem; pslot = &pelem->next, pelem = *pslot)
    {
	if (pelem->hash == hash
	 && compare_terms(v, t, pelem->value.val, pelem->value.tag) == 0
	 )
	{
	    break;
	}
    }

    if (!pelem)
    {
	/* insert new entry */
	pelem = (t_constant_entry *) hg_alloc_size(sizeof(t_constant_entry));
	if ((res = create_heapterm(&pelem->value, v, t)) != PSUCCEED)
	{
	    hg_free_size(pelem, sizeof(t_constant_entry));
	    return res;
	}

	/*
	 * Mark it as a persistent heap term.
	 * This will also make any DIDs within the term permanent,
	 * so dictionary gc does not need to mark persistent terms.
	 */
	make_heapterm_persistent(&pelem->value);

	pelem->hash = hash;
	pelem->next = *pslot;
	*pslot = pelem;
	++constant_table->nentries;

	/* expand table if too full */
	if (constant_table->nentries > constant_table->size
	 && constant_table->size < CONSTANT_TABLE_MAX_SIZE)
	{
	    _constant_table_expand(constant_table);
	}

    }
    else
    {
	++constant_table->nreuse;
    }

    *presult = pelem->value;
    return PSUCCEED;
}


#ifdef PRINTAM
pr_constant_table(void)
{
    uword entry_count = 0;
    uword max_chain = 0;
    uword used_slots = 0;
    uword i;

    for(i = 0; i < constant_table->size; ++i)
    {
	uword chain_length = 0;
	t_constant_entry *pelem = constant_table->htable[i];
	if (pelem)
	    ++used_slots;
	for(; pelem; pelem = pelem->next)
	{
	    writeq_term(pelem->value.val.all, pelem->value.tag.all);
	    ++chain_length;
	}
	entry_count += chain_length;
	if (chain_length > max_chain)
	    max_chain = chain_length;
    }

    p_fprintf(current_output_, "GROUND CONSTANT TABLE\n");
    p_fprintf(current_output_, "size      = %d\n", constant_table->size);
    p_fprintf(current_output_, "entries   = %d\n", constant_table->nentries);
    p_fprintf(current_output_, "reuse     = %d\n", constant_table->nreuse);
    p_fprintf(current_output_, "max chain = %d\n", max_chain);
    p_fprintf(current_output_, "avg chain = %f\n", ((double) entry_count)/used_slots);
    if (entry_count != constant_table->nentries)
	p_fprintf(current_output_, "!!! Deviating entry count %d\n", entry_count);
    Succeed_;
}
#endif
