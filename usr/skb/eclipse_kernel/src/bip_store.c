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
 * Copyright (C) 1996-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*----------------------------------------------------------------------
 * System:	ECLiPSe Constraint Logic Programming System
 * Version:	$Id: bip_store.c,v 1.1 2008/06/30 17:43:52 jschimpf Exp $
 *
 * Contents:	Built-ins for the store-primitives
 *
 *		This file has been factored out of bip_record.c in 05/2006
 *----------------------------------------------------------------------*/

#include	"config.h"

#include        <stdio.h>   /* for sprintf() */

#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "error.h"
#include        "mem.h"
#include        "dict.h"
#include        "property.h"


/*----------------------------------------------------------------------
 * Heap hash tables ("stores")
 *
 * A "store" is either identified by an (anonymous) handle,
 * or it is the (module-local) property of a functor.
 * Keys must be ground terms, values can be arbitrary terms.
 *
 * store_create(-Handle)
 *	argument is uninstantiated, it creates an anonymous store
 *	and returns a handle for it.
 * local store(+Term)
 *	argument is instantiated (atom or name/arity), it creates a store
 *	as a property (local to the caller module) of the given functor
 *
 * All the subsequent predicates take a Store argument which is either
 * a handle or a term whose functor identifies the store.
 *
 * store_set(+Store, ++Key, +Value) is det
 *	add or replace an entry for Key
 *
 * store_inc(+Store, ++Key) is det
 *	increment an existing integer entry, or initialise to 1
 *
 * store_get(+Store, ++Key, -Value) is semidet
 *	get the entry for Key, or fail
 *
 * store_delete(+Store, ++Key) is det
 *	delete the entry for key, if any
 *
 * store_contains(+Store, ++Key) is semidet
 *	succeed if Store contains an entry for Key
 *
 * stored_keys(+Store, -Keys)
 *	get a list of all keys in Store
 *
 * stored_keys_and_values(+Store, -KeysValues)
 *	get a list of all Key-Value pairs
 *
 * store_erase(+Store) is det
 *	delete all entries
 *
 * store_count(+Store, -Count) is det
 *	get number of entries
 *
 * current_store(+Store) is det
 * current_store(-Store) is nondet
 *	get/check named stores
 *
 *
 * Following the naming scheme of lib(m_map) we could redundantly have:
 *
 * store_insert(+Store, ++Key, +Value) is semidet
 *	fail if already in store
 * store_det_insert(+Store, ++Key, +Value) is det
 *	abort if already in store
 * store_update(+Store, ++Key, +Value) is semidet
 *	fail if not already in store
 * store_det_update(+Store, ++Key, +Value) is det
 *	abort if not already in store
 * store_lookup(+Store, ++Key, -Value) is det
 *	abort if not in store
 * store_remove(+Store, ++Key, -Value) is semidet
 *	get+delete, fail if not in store
 * store_det_remove(+Store, ++Key, -Value) is det
 *	get+delete, abort if not in store
 *----------------------------------------------------------------------*/

#define HTABLE_MIN_SIZE		16
#define HTABLE_MAX_SIZE		2097152
#define HTABLE_EXPAND_FACTOR	4


/* METHODS */

void htable_free(t_heap_htable *obj);
static t_heap_htable * _copy_heap_htable(t_heap_htable *obj);
static void _mark_heap_htable(t_heap_htable *obj);
static int _tostr_heap_htable(t_heap_htable *obj, char *buf, int quoted);
static int _strsz_heap_htable(t_heap_htable *obj, int quoted);


/* CLASS DESCRIPTOR (method table) */

t_ext_type heap_htable_tid = {
    (void (*)(t_ext_ptr)) htable_free,
    (t_ext_ptr (*)(t_ext_ptr)) _copy_heap_htable,
    (void (*)(t_ext_ptr)) _mark_heap_htable,
    (int (*)(t_ext_ptr,int)) _strsz_heap_htable,
    (int (*)(t_ext_ptr,char *,int)) _tostr_heap_htable,
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _copy_heap_htable,
    0,	/* get */
    0	/* set */
};


/* PROLOG INTERFACE */

/*
 * Get a pointer to the hash table either from a handle
 * or from the HTABLE_PROP property of a functor
 */
#define Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj)		\
	if (IsTag(thandle.kernel, THANDLE)) {				\
	    Get_Typed_Object(vhandle, thandle, &heap_htable_tid, obj);	\
	} else {							\
	    dident name_did;						\
	    int err;							\
	    pword *prop;						\
	    Get_Key_Did(name_did, vhandle, thandle);			\
	    prop = get_modular_property(name_did, HTABLE_PROP, vmod.did, tmod, LOCAL_PROP, &err); \
	    if (!prop) {						\
		Bip_Error(err == PERROR ? NO_LOCAL_REC : err);		\
	    }								\
	    obj = (t_heap_htable *) prop->val.wptr;			\
	}


t_heap_htable *
htable_new(int internal)
{
    t_heap_htable *obj;
    uword i;

    /* INSTANCE INITIALISATION */
    if (internal) {
	obj = (t_heap_htable *)
		hp_alloc_size(sizeof(t_heap_htable));
	obj->htable = (t_htable_elem **)
		hp_alloc_size(HTABLE_MIN_SIZE * sizeof(t_htable_elem *));
    } else {
	obj = (t_heap_htable *)
		hg_alloc_size(sizeof(t_heap_htable));
	obj->htable = (t_htable_elem **)
		hg_alloc_size(HTABLE_MIN_SIZE * sizeof(t_htable_elem *));
    }
    
    obj->internal = internal;
    obj->ref_ctr = 1;
    obj->size = HTABLE_MIN_SIZE;
    obj->nentries = 0;
    for (i = 0; i < obj->size; ++i)
    {
	obj->htable[i] = NULL;
    }
    return obj;
}


static int
p_is_store(value vhandle, type thandle, value vmod, type tmod)
{
    int err;
    pword *prop;
    dident name_did;

    Get_Key_Did(name_did, vhandle, thandle);
    prop = get_modular_property(name_did, HTABLE_PROP, vmod.did, tmod, LOCAL_PROP, &err);
    Succeed_If(prop);
}


static int
p_store_create(value vhtable, type thtable)
{
    pword htable;

    Check_Ref(thtable);
    htable = ec_handle(&heap_htable_tid, (t_ext_ptr) htable_new(0));
    Return_Unify_Pw(vhtable, thtable, htable.val, htable.tag);
}


static int
p_store_create_named(value vhtable, type thtable, value vmod, type tmod)
{
    pword *prop;
    dident name_did;
    int err;

    Get_Functor_Did(vhtable, thtable, name_did);
    prop = set_modular_property(name_did, HTABLE_PROP, vmod.did, tmod,
				LOCAL_PROP, &err);
    if (prop)
    {
	prop->tag.kernel = TPTR;
	prop->val.wptr = (uword *) htable_new(0);
	Succeed_;
    }
    else if (err == PERROR)
    {
	Succeed_;
    }
    else
    {
	Bip_Error(err);
    }

}


/*
 * Grow the hash table by HTABLE_EXPAND_FACTOR
 */

static void
_htable_expand(t_heap_htable *obj)
{
    uword new_size = obj->size * HTABLE_EXPAND_FACTOR;
    t_htable_elem **new_htable;
    uword i;

    /* make and initialize a larger table */
    if (obj->internal) {
	new_htable = (t_htable_elem **)
	  hp_alloc_size(new_size * sizeof(t_htable_elem *));
    }
    else {
	new_htable = (t_htable_elem **)
	  hg_alloc_size(new_size * sizeof(t_htable_elem *));
    }
    
    for (i = 0; i < new_size; ++i)
    {
	new_htable[i] = NULL;
    }

    /* redistribute the entries from the old table */
    for (i = 0; i < obj->size; ++i)
    {
	t_htable_elem *elem;
	for(elem = obj->htable[i]; elem; )
	{
	    t_htable_elem **new_slot = &new_htable[elem->hash % new_size];
	    t_htable_elem *next_elem = elem->next;
	    elem->next = *new_slot;
	    *new_slot = elem;
	    elem = next_elem;
	}
    }

    /* free the old table */
    if (obj->internal) {
	hp_free_size(obj->htable, obj->size * sizeof(t_htable_elem *));
    }
    else {
	hg_free_size(obj->htable, obj->size * sizeof(t_htable_elem *));
    }

    /* assign the new one */
    obj->htable = new_htable;
    obj->size = new_size;
}


/*
 * Auxiliary function to look up vkey/tkey with hash value hash
 */

static t_htable_elem *
_htable_find(t_heap_htable *obj, uword hash, value vkey, type tkey, t_htable_elem ***ppslot)
{
    t_htable_elem *pelem;
    t_htable_elem **pslot;
    pslot = &obj->htable[hash % obj->size];
    for(pelem = *pslot; pelem; pslot = &pelem->next, pelem = *pslot)
    {
	if (pelem->hash == hash
	 && compare_terms(vkey, tkey, pelem->key.val, pelem->key.tag) == 0)
	{
	    *ppslot = pslot;
	    return pelem;
	}
    }
    *ppslot = pslot;
    return NULL;
}


/*
 * store_set(+Handle, +Key, +Value)
 *	add or replace an entry for Key
 *
 * we could have variants of this which
 *	- fail if key already exists
 *	- add another entry for key (saves the lookup)
 */

static int
p_store_set(value vhandle, type thandle, value vkey, type tkey, value vval, type tval, value vmod, type tmod)
{
    t_heap_htable *obj;
    uword hash;
    pword copy_key, copy_value;
    t_htable_elem **pslot;
    t_htable_elem *pelem;
    int res = PSUCCEED;

    Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj);
    hash = ec_term_hash(vkey, tkey, MAX_U_WORD, &res);
    if (res != PSUCCEED)
    {
	Bip_Error(res);
    }

    pelem = _htable_find(obj, hash, vkey, tkey, &pslot);
    if (pelem)		/* an entry for key exists already */
    {
	pword copy_value;
	if ((res = create_heapterm(&copy_value, vval, tval)) != PSUCCEED)
	{
	    Bip_Error(res);
	}
	free_heapterm(&pelem->value);
	move_heapterm(&copy_value, &pelem->value);
    }
    else		/* make a new entry for key */
    {
	pelem = (t_htable_elem *) hg_alloc_size(sizeof(t_htable_elem));
	pelem->hash = hash;
	if ((res = create_heapterm(&pelem->key, vkey, tkey)) != PSUCCEED)
	{
	    hg_free_size(pelem, sizeof(t_htable_elem));
	    Bip_Error(res);
	}
	if ((res = create_heapterm(&pelem->value, vval, tval)) != PSUCCEED)
	{
	    free_heapterm(&pelem->key);
	    hg_free_size(pelem, sizeof(t_htable_elem));
	    Bip_Error(res);
	}
	pelem->next = *pslot;
	*pslot = pelem;
	++obj->nentries;

	/* expand table if too full */
	if (obj->nentries > obj->size  &&  obj->size < HTABLE_MAX_SIZE)
	{
	    _htable_expand(obj);
	}
    }
    Succeed_;
}


static int
p_store_inc(value vhandle, type thandle, value vkey, type tkey, value vmod, type tmod)
{
    t_heap_htable *obj;
    uword hash;
    pword copy_key, copy_value;
    t_htable_elem **pslot;
    t_htable_elem *pelem;
    int res = PSUCCEED;

    Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj);
    hash = ec_term_hash(vkey, tkey, MAX_U_WORD, &res);
    if (res != PSUCCEED)
    {
	Bip_Error(res);
    }

    pelem = _htable_find(obj, hash, vkey, tkey, &pslot);
    if (pelem)		/* an entry for key exists already */
    {
	Check_Integer(pelem->value.tag);
	if (pelem->value.val.nint == MAX_S_WORD)
	{
	    Bip_Error(RANGE_ERROR);
	}
	++pelem->value.val.nint;		/* increment */
    }
    else		/* make a new entry for key */
    {
	pelem = (t_htable_elem *) hg_alloc_size(sizeof(t_htable_elem));
	pelem->hash = hash;
	if ((res = create_heapterm(&pelem->key, vkey, tkey)) != PSUCCEED)
	{
	    hg_free_size(pelem, sizeof(t_htable_elem));
	    Bip_Error(res);
	}
	Make_Integer(&pelem->value, 1);		/* initialise to 1 */
	pelem->next = *pslot;
	*pslot = pelem;
	++obj->nentries;

	/* expand table if too full */
	if (obj->nentries > obj->size  &&  obj->size < HTABLE_MAX_SIZE)
	{
	    _htable_expand(obj);
	}
    }
    Succeed_;
}


static int
p_store_contains(value vhandle, type thandle, value vkey, type tkey, value vmod, type tmod)
{
    t_heap_htable *obj;
    t_htable_elem *pelem;
    t_htable_elem **pslot;
    uword hash;
    int res = PSUCCEED;

    Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj);
    hash = ec_term_hash(vkey, tkey, MAX_U_WORD, &res);
    if (res != PSUCCEED)
    {
	Bip_Error(res);
    }
    Succeed_If(_htable_find(obj, hash, vkey, tkey, &pslot));
}


static int
p_store_get(value vhandle, type thandle, value vkey, type tkey, value vval, type tval, value vmod, type tmod)
{
    t_heap_htable *obj;
    t_htable_elem *pelem;
    t_htable_elem **pslot;
    pword elem_value;
    uword hash;
    int res = PSUCCEED;

    Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj);
    hash = ec_term_hash(vkey, tkey, MAX_U_WORD, &res);
    if (res != PSUCCEED)
    {
	Bip_Error(res);
    }
    pelem = _htable_find(obj, hash, vkey, tkey, &pslot);
    if (!pelem)
    {
	Fail_;
    }
    get_heapterm(&pelem->value, &elem_value);
    if (IsRef(elem_value.tag) && elem_value.val.ptr == &elem_value)
    {
	Succeed_;
    }
    Return_Unify_Pw(vval, tval, elem_value.val, elem_value.tag);
}


static int
p_store_delete(value vhandle, type thandle, value vkey, type tkey, value vmod, type tmod)
{
    t_heap_htable *obj;
    t_htable_elem *pelem;
    t_htable_elem **pslot;
    uword hash;
    int res = PSUCCEED;

    Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj);
    hash = ec_term_hash(vkey, tkey, MAX_U_WORD, &res);
    if (res != PSUCCEED)
    {
	Bip_Error(res);
    }
    pelem = _htable_find(obj, hash, vkey, tkey, &pslot);
    if (pelem)
    {
	*pslot = pelem->next;	/* unlink element */
	free_heapterm(&pelem->key);
	free_heapterm(&pelem->value);
	hg_free_size(pelem, sizeof(t_htable_elem));
	--obj->nentries;
    }
    Succeed_;
}


static int
p_store_count(value vhandle, type thandle, value vn, type tn, value vmod, type tmod)
{
    t_heap_htable *obj;
    Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj);
    Return_Unify_Integer(vn, tn, obj->nentries);
}


static int
p_store_info(value vhandle, type thandle, value vmod, type tmod)
{
    t_heap_htable *obj;
    uword entry_count = 0;
    uword max_chain = 0;
    uword used_slots = 0;
    uword i;

    Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj);

    for(i = 0; i < obj->size; ++i)
    {
	uword chain_length = 0;
	t_htable_elem *pelem = obj->htable[i];
	if (pelem)
	    ++used_slots;
	for(; pelem; pelem = pelem->next)
	    ++chain_length;
	entry_count += chain_length;
	if (chain_length > max_chain)
	    max_chain = chain_length;
    }

    p_fprintf(current_err_, "\nStore at 0x%08x", obj);
    p_fprintf(current_err_, "\nref_ctr    %d", obj->ref_ctr);
    p_fprintf(current_err_, "\nsize       %d", obj->size);
    p_fprintf(current_err_, "\nnentries   %d", obj->nentries);
    p_fprintf(current_err_, "\nused slots %d", used_slots);
    p_fprintf(current_err_, "\nmax chain  %d", max_chain);
    p_fprintf(current_err_, "\navg chain  %f", ((double) entry_count)/used_slots);
    if (entry_count != obj->nentries)
	p_fprintf(current_err_, "\n!!! Deviating entry count %d", entry_count);
    ec_newline(current_err_);
    Succeed_;
}


static int
p_stored_keys(value vhandle, type thandle, value vresult, type tresult, value vmod, type tmod)
{
    t_heap_htable *obj;
    t_htable_elem *pelem;
    uword i;
    pword result, *ptail;

    Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj);
    ptail = &result;
    for(i = 0; i < obj->size; ++i)
    {
	for(pelem = obj->htable[i]; pelem; pelem = pelem->next)
	{  
	    pword *pw = TG;
	    Make_List(ptail, pw);
	    Push_List_Frame();
	    ptail = pw+1;
	    get_heapterm(&pelem->key, pw);
	}
    }
    Make_Nil(ptail);
    Return_Unify_Pw(vresult, tresult, result.val, result.tag);
}


static int
p_stored_keys_and_values(value vhandle, type thandle, value vresult, type tresult, value vmod, type tmod)
{
    t_heap_htable *obj;
    t_htable_elem *pelem;
    uword i;
    pword result, *ptail;

    Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj);
    ptail = &result;
    for(i = 0; i < obj->size; ++i)
    {
	for(pelem = obj->htable[i]; pelem; pelem = pelem->next)
	{  
	    pword *pw = TG;
	    Make_List(ptail, pw);
	    Push_List_Frame();
	    ptail = pw+1;
	    Make_Struct(pw, TG);
	    pw = TG;
	    Push_Struct_Frame(d_.minus);
	    get_heapterm(&pelem->key, pw+1);
	    get_heapterm(&pelem->value, pw+2);
	}
    }
    Make_Nil(ptail);
    Return_Unify_Pw(vresult, tresult, result.val, result.tag);
}


static void
_htable_erase(t_heap_htable *obj)
{
    uword i;
    for(i = 0; i < obj->size; ++i)
    {
	t_htable_elem *elem = obj->htable[i];
	if (elem)
	{
	    obj->htable[i] = NULL;
	    do {
		t_htable_elem *next_elem = elem->next;
		if (obj->internal) {
		    hp_free_size(elem, sizeof(t_htable_elem));
                } else {
		    free_heapterm(&elem->key);
		    free_heapterm(&elem->value);
		    hg_free_size(elem, sizeof(t_htable_elem));
		}
		elem = next_elem;
#ifdef DEBUG_RECORD
		p_fprintf(current_err_, "\nfree element");
		ec_flush(current_err_);
#endif
	    } while(elem);
	}
    }
    obj->nentries = 0;
}


static int
p_store_erase(value vhandle, type thandle, value vmod, type tmod)
{
    t_heap_htable *obj;

    Get_Heap_Htable(vhandle, thandle, vmod, tmod, obj);
    _htable_erase(obj);
    Succeed_;
}


void
htable_free(t_heap_htable *obj)	/* obj != NULL */
{
#ifdef DEBUG_RECORD
    p_fprintf(current_err_, "\nlosing reference to htable(0x%x)", obj);
    ec_flush(current_err_);
#endif
    if (--obj->ref_ctr <= 0)
    {
	_htable_erase(obj);
	if (obj->internal) {
	    hp_free_size(obj->htable, obj->size * sizeof(t_htable_elem *));
	    hp_free_size(obj, sizeof(t_heap_htable));
	} else {
	    hg_free_size(obj->htable, obj->size * sizeof(t_htable_elem *));
	    hg_free_size(obj, sizeof(t_heap_htable));
	}
#ifdef DEBUG_RECORD
	p_fprintf(current_err_, "\nhtable_free(0x%x)", obj);
	ec_flush(current_err_);
#endif
    }
}


static t_heap_htable *
_copy_heap_htable(t_heap_htable *obj)	/* obj != NULL */
{
    ++obj->ref_ctr;
    return obj;
}


static void
_mark_heap_htable(t_heap_htable *obj)	/* obj != NULL */
{
    uword i;
#ifdef DEBUG_RECORD
    p_fprintf(current_err_, "\n_mark_heap_htable(0x%x)", obj);
    ec_flush(current_err_);
#endif
    for(i = 0; i < obj->size; ++i)
    {
	t_htable_elem *elem;
	for(elem = obj->htable[i]; elem; elem = elem->next)
	{
	    mark_dids_from_heapterm(&elem->key);
	    mark_dids_from_heapterm(&elem->value);
	}
    }
}


static int
_tostr_heap_htable(t_heap_htable *obj, char *buf, int quoted) /* obj != NULL */
{
#define STRSZ_STORE 20
    sprintf(buf, "'STORE'(16'%08x)", (int)(word) obj);	/* possibly truncated */
    return STRSZ_STORE;
}


static int
_strsz_heap_htable(t_heap_htable *obj, int quoted)	/* obj != NULL */
{
    return STRSZ_STORE;
}


/*----------------------------------------------------------------------
 * Short-lived hash tables based upon the store_*() routines.
 * The tables are used internally by ECLipSe and must be explicitly
 * allocated/deallocated.
 *----------------------------------------------------------------------*/

/*
 * store_set(obj, vkey, tkey, valpw)
 *	Store the target of pword pointer 'valpw' in the store 'obj'
 *	for the key with value 'vkey' and type 'tkey'. A heap copy
 *	of the target of 'valpw' is not made - it is assumed suitable
 *	allocation has already been performed.
 *
 *	This routine adds an element to the store, assuming that no
 *	entry for the given key exists.
 */

int
store_set(t_heap_htable *obj, value vkey, type tkey, pword *valpw)
{
    t_htable_elem *pelem;
    t_htable_elem **pslot;
    uword hash;
    int res = PSUCCEED;
 
    hash = ec_term_hash(vkey, tkey, MAX_U_WORD, &res);
    if (res != PSUCCEED) {
	Bip_Error(res);
    }
     
    /* Store the element */
    pelem = (t_htable_elem *) hp_alloc_size(sizeof(t_htable_elem));
    pelem->hash = hash;
    pelem->key.val = vkey;
    pelem->key.tag = tkey;
    pelem->value = *valpw;
    pslot = &obj->htable[hash % obj->size];
    pelem->next = *pslot;
    *pslot = pelem;
    ++obj->nentries;
 
    /* expand table if too full */
    if (obj->nentries > obj->size  &&  obj->size < HTABLE_MAX_SIZE) {
	_htable_expand(obj);
    }
    Succeed_;
}

/*
 * store_get(obj, vkey, tkey, valpw)
 *	Return a pword reference 'valpw' to the element referenced by
 *	the store 'obj' with key value 'vkey' and key type 'tkey'. A 
 *	global stack copy of the target of 'valpw' is not made.
 *
 *	This routine retrieves an element from the store, assuming that an 
 *	entry exists for the given key.
 */

int
store_get(t_heap_htable *obj, value vkey, type tkey, pword *valpw)
{
    t_htable_elem *pelem;
    t_htable_elem **pslot;
    uword hash;
    int res = PSUCCEED;
 
    hash = ec_term_hash(vkey, tkey, MAX_U_WORD, &res);
    if (res != PSUCCEED) {
	Bip_Error(res);
    }
    pelem = _htable_find(obj, hash, vkey, tkey, &pslot);
    if (pelem) {
	*valpw = pelem->value;
	Succeed_;
    }
     
    Fail_;
}

/* Fail_ is not found but successfully entered in table! */
/*
 * store_get_else_set(obj, vkey, tkey, valpw)
 *	Return a pword reference 'valpw' to the element referenced by
 *	the store 'obj' with key value 'vkey' and key type 'tkey'. A 
 *	global stack copy of the target of 'valpw' is not made.
 *
 *	This routine retrieves an element from the store, if an 
 *	entry exists for the given key. If it does, the routine 
 *	returns 'PSUCCEED'.
 *	If no entry exists, then the target of pword pointer 'valpw' is
 *	stored in the store 'obj' for the key with value 'vkey' and 
 *	type 'tkey'. A heap copy of the target of 'valpw' is not made 
 *	- it is assumed suitable allocation has already been performed.
 *	In this case, the entry is created and the routine returns 'PFAIL'.
 */


int
store_get_else_set(t_heap_htable *obj, value vkey, type tkey, pword *valpw)
{
    t_htable_elem *pelem;
    t_htable_elem **pslot;
    uword hash;
    int res = PSUCCEED;
 
    hash = ec_term_hash(vkey, tkey, MAX_U_WORD, &res);
    if (res != PSUCCEED) {
	Bip_Error(res);
    }
    pelem = _htable_find(obj, hash, vkey, tkey, &pslot);
    if (pelem) {
	*valpw = pelem->value;
	Succeed_;
    }
     
    /* Store the element */
    pelem = (t_htable_elem *) hp_alloc_size(sizeof(t_htable_elem));
    pelem->hash = hash;
    pelem->key.val = vkey;
    pelem->key.tag = tkey;
    pelem->value = *valpw;
    pelem->next = *pslot;
    *pslot = pelem;
    ++obj->nentries;
 
    /* expand table if too full */
    if (obj->nentries > obj->size  &&  obj->size < HTABLE_MAX_SIZE) {
	_htable_expand(obj);
    }
    Fail_;
}


/*----------------------------------------------------------------------
 * Initialisation
 *----------------------------------------------------------------------*/

void
bip_store_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("store_create", 1), p_store_create, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("store_create_named_", 2), p_store_create_named, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("store_count_", 3), p_store_count, B_SAFE);
	(void) built_in(in_dict("store_erase_", 2), p_store_erase, B_SAFE);
	(void) built_in(in_dict("store_set_",4), p_store_set, B_SAFE);
	(void) built_in(in_dict("store_delete_",3), p_store_delete, B_SAFE);
	(void) built_in(in_dict("store_contains_",3), p_store_contains, B_SAFE);
	(void) local_built_in(in_dict("is_store_",2), p_is_store, B_SAFE);
	(void) built_in(in_dict("store_inc_",3), p_store_inc, B_SAFE);
	(void) built_in(in_dict("store_info_",2), p_store_info, B_SAFE);
	(void) built_in(in_dict("store_get_",4), p_store_get, B_UNSAFE|U_FRESH);
	built_in(in_dict("stored_keys_",3), p_stored_keys, B_UNSAFE|U_FRESH)
	    ->mode = BoundArg(2,GROUND);
	(void) built_in(in_dict("stored_keys_and_values_",3), p_stored_keys_and_values, B_UNSAFE|U_FRESH);
    }
}

