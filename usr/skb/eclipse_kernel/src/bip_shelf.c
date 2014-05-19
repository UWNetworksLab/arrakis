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
 * Version:	$Id: bip_shelf.c,v 1.1 2008/06/30 17:43:52 jschimpf Exp $
 *
 * Contents:	Built-ins for the shelf-primitives
 *
 *		This file has been factored out of bip_record.c in 05/2006
 *----------------------------------------------------------------------*/

#include	"config.h"

#include        <stdio.h>   /* for sprintf() */

#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include	"dict.h"
#include	"property.h"


/*----------------------------------------------------------------------
 * Prolog heap arrays
 *----------------------------------------------------------------------*/

/* INSTANCE TYPE DECLARATION */

typedef struct {
    long		ref_ctr;
    pword		array[ 1 /* + arity */ ];
} t_heap_array;


/* METHODS */

static void _free_heap_array(t_heap_array *obj);
static t_heap_array * _copy_heap_array(t_heap_array *obj);
static void _mark_heap_array(t_heap_array *obj);
static int _heap_arr_set(t_ext_ptr h, int i, pword pw);
static pword _heap_arr_get(t_ext_ptr h, int i);
static int _tostr_heap_arr(t_heap_array *obj, char *buf, int quoted);
static int _strsz_heap_arr(t_heap_array *obj, int quoted);

static void
_free_heap_array(t_heap_array *obj)	/* obj != NULL */
{
    if (--obj->ref_ctr <= 0)
    {
	pword *p = obj->array;
	int arity = DidArity(p[0].val.did);
	int i;
	for (i = arity; i > 0; --i)
	{
	    free_heapterm(&p[i]);
	}
	hg_free_size(obj, sizeof(t_heap_array) + arity*sizeof(pword));
#ifdef DEBUG_RECORD
	p_fprintf(current_err_, "\n_free_heap_array(0x%x)", obj);
	ec_flush(current_err_);
#endif
    }
}

static t_heap_array *
_copy_heap_array(t_heap_array *obj)	/* obj != NULL */
{
    ++obj->ref_ctr;
    return obj;
}

static void
_mark_heap_array(t_heap_array *obj)	/* obj != NULL */
{
    pword *p = obj->array;
    int i = DidArity(p[0].val.did);
    mark_dids_from_pwords(p, p + 1);
    for (; i > 0; --i)
    {
	mark_dids_from_heapterm(&p[i]);
    }
}


/* CLASS DESCRIPTOR (method table) */

t_ext_type heap_array_tid = {
    (void (*)(t_ext_ptr)) _free_heap_array,
    (t_ext_ptr (*)(t_ext_ptr)) _copy_heap_array,
    (void (*)(t_ext_ptr)) _mark_heap_array,
    (int (*)(t_ext_ptr,int)) _strsz_heap_arr,
    (int (*)(t_ext_ptr,char *,int)) _tostr_heap_arr,
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _copy_heap_array,
    _heap_arr_get,	/* get */
    _heap_arr_set	/* set */
};


/* PROLOG INTERFACE */

/*
 * shelf_create(+Name/Arity, +FieldInit, -Handle)
 * shelf_create(+InitTerm, -Handle)
 * shelf_get/xget(+Handle, +Index, -Field)	Index=0: get whole term
 * shelf_set/xset(+Handle, +Index, +Field)	Index=0: set whole term
 * shelf_abolish(+Handle)
 *
 * shelf_name(+NameTerm, +Handle, +Module)	give the shelf a name
 */


/*
 * Get a pointer to the shelf either from a handle
 * or from the SHELF_PROP property of a functor
 */
#define Get_Shelf(vhandle, thandle, vmod, tmod, obj)			\
	if (IsTag(thandle.kernel, THANDLE)) {				\
	    Get_Typed_Object(vhandle, thandle, &heap_array_tid, obj);	\
	} else {							\
	    dident name_did;						\
	    int err;							\
	    pword *prop;						\
	    Get_Key_Did(name_did, vhandle, thandle);			\
	    prop = get_modular_property(name_did, SHELF_PROP, vmod.did, tmod, LOCAL_PROP, &err); \
	    if (!prop) {						\
		Bip_Error(err == PERROR ? NO_LOCAL_REC : err);		\
	    }								\
	    obj = (t_heap_array *) prop->val.wptr;			\
	}


static int
p_shelf_create3(value vkey, type tkey, value vinit, type tinit, value vbag, type tbag)
{
    dident key_did;
    pword *p, bag;
    t_heap_array *obj;
    int i;
    Check_Ref(tbag);
    Get_Functor_Did(vkey, tkey, key_did);
    i = DidArity(key_did);
    if (i < 1)
	{ Bip_Error(RANGE_ERROR); }

    /* INSTANCE INITIALISATION */
    obj = (t_heap_array *) hg_alloc_size(
			    sizeof(t_heap_array) + i*sizeof(pword));
    obj->ref_ctr = 1;
    p = obj->array;
    for (; i > 0; --i)
    {
	int err = create_heapterm(&p[i], vinit, tinit);
	Return_If_Not_Success(err);
    }
    p[0].val.did = key_did;
    p[0].tag.kernel = TDICT;

    bag = ec_handle(&heap_array_tid, (t_ext_ptr) obj);
    Return_Unify_Pw(vbag, tbag, bag.val, bag.tag);
}


static int
p_shelf_create2(value vinit, type tinit, value vbag, type tbag)
{
    pword bag;
    pword *pheap, *pglobal;
    t_heap_array *obj;
    int i, err;

    Check_Ref(tbag);
    Check_Structure(tinit);
    pglobal = vinit.ptr;
    i = DidArity(pglobal->val.did);

    /* INSTANCE INITIALISATION */
    obj = (t_heap_array *) hg_alloc_size(
			    sizeof(t_heap_array) + i*sizeof(pword));
    obj->ref_ctr = 1;
    pheap = obj->array;
    pheap[0] = pglobal[0];
    for (; i > 0; --i)
    {
	pword *parg = &pglobal[i];
	Dereference_(parg);
	err = create_heapterm(&pheap[i], parg->val, parg->tag);
	Return_If_Not_Success(err);
    }
    bag = ec_handle(&heap_array_tid, (t_ext_ptr) obj);
    Return_Unify_Pw(vbag, tbag, bag.val, bag.tag);
}


static int
p_shelf_name(value vname, type tname, value vhandle, type thandle, value vmod, type tmod)
{
    pword *prop;
    dident name_did;
    int err;

    Get_Functor_Did(vname, tname, name_did);
    prop = set_modular_property(name_did, SHELF_PROP, vmod.did, tmod,
				LOCAL_PROP, &err);
    if (prop)
    {
	t_heap_array *obj;
	Get_Typed_Object(vhandle, thandle, &heap_array_tid, obj);
	prop->tag.kernel = TPTR;
	prop->val.wptr = (uword *) heap_array_tid.copy(obj);
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


static int
p_shelf_set(value vhandle, type thandle, value vi, type ti, value vval, type tval, value vmod, type tmod)
{
    t_heap_array *obj;
    pword pw;
    pw.val = vval;
    pw.tag = tval;
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    return _heap_arr_set(obj, vi.nint, pw);
}

static int
p_shelf_get(value vhandle, type thandle, value vi, type ti, value vval, type tval, value vmod, type tmod)
{
    t_heap_array *obj;
    pword pw;
    pw.val = vval;
    pw.tag = tval;
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    if (vi.nint < 0 || vi.nint > DidArity(obj->array[0].val.did))
	{ Bip_Error(RANGE_ERROR); }
    pw = _heap_arr_get(obj, vi.nint);
    if (IsRef(pw.tag))
    {
	Succeed_;	/* nothing to unify */
    }
    Return_Unify_Pw(vval, tval, pw.val, pw.tag);
}

static int
p_shelf_inc(value vhandle, type thandle, value vi, type ti, value vmod, type tmod)
{
    t_heap_array *obj;
    pword *pw;
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    if (vi.nint < 1 || vi.nint > DidArity(obj->array[0].val.did))
	{ Bip_Error(RANGE_ERROR); }
    pw = &obj->array[vi.nint];
    Check_Integer(pw->tag);
    if (pw->val.nint == MAX_S_WORD)
    {
	Bip_Error(RANGE_ERROR);
    }
    ++pw->val.nint;
    Succeed_;
}

static int
p_shelf_dec(value vhandle, type thandle, value vi, type ti, value vmod, type tmod)
{
    t_heap_array *obj;
    pword *pw;
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    if (vi.nint < 1 || vi.nint > DidArity(obj->array[0].val.did))
	{ Bip_Error(RANGE_ERROR); }
    pw = &obj->array[vi.nint];
    Check_Integer(pw->tag);
    if (pw->val.nint <= 0)
    {
	Fail_;
    }
    --pw->val.nint;
    Succeed_;
}


static int
_heap_arr_set(t_ext_ptr h,
	int i,
	pword pw)	/* expected to be dereferenced */
{
    pword copy_pw;
    pword *pheap;
    int err, arity;

    pheap = ((t_heap_array*)h)->array;
    arity = DidArity(pheap[0].val.did);
    if (i >= 1 && i <= arity)
    {
	if ((err = create_heapterm(&copy_pw, pw.val, pw.tag)) != PSUCCEED)
	    { Bip_Error(err); }
	a_mutex_lock(&SharedDataLock);
	free_heapterm(&pheap[i]);
	move_heapterm(&copy_pw, &pheap[i]);
	a_mutex_unlock(&SharedDataLock);
    }
    else if (i == 0)
    {
	if (IsStructure(pw.tag) && pw.val.ptr->val.did == pheap[0].val.did)
	{
	    pword *aux = TG;
	    Push_Struct_Frame(pheap[0].val.did);
	    for (i=1; i<=arity; ++i)
	    {
		pword *parg = &pw.val.ptr[i];
		Dereference_(parg);
		if ((err = create_heapterm(aux+i, parg->val, parg->tag)) != PSUCCEED)
		{
		    TG = aux;
		    Bip_Error(err);
		}
	    }
	    a_mutex_lock(&SharedDataLock);
	    for (i=1; i<=arity; ++i)
	    {
		free_heapterm(&pheap[i]);
		move_heapterm(aux+i, &pheap[i]);
	    }
	    a_mutex_unlock(&SharedDataLock);
	    TG = aux;
	}
	else { Bip_Error(RANGE_ERROR); }
    }
    else
	{ Bip_Error(RANGE_ERROR); }
    
    Succeed_;
}


static pword
_heap_arr_get(t_ext_ptr h, int i)	/* assumed to return dereferenced result */
{
    pword result;
    pword *pheap;
    int arity;

    pheap = ((t_heap_array*)h)->array;
    arity = DidArity(pheap[0].val.did);
    a_mutex_lock(&SharedDataLock);
    if (i > 0  &&  i <= arity)
    {
	get_heapterm(&pheap[i], &result);
    }
    else				/* get the whole array-term */
    {
	Make_Struct(&result,TG);
	Push_Struct_Frame(pheap[0].val.did);
	for (i=1; i<=arity; ++i)
	{
	    get_heapterm(&pheap[i], &result.val.ptr[i]);
	}
    }
    a_mutex_unlock(&SharedDataLock);
    return result;
}


static int
_tostr_heap_arr(t_heap_array *obj, char *buf, int quoted) /* obj != NULL */
{
#define STRSZ_SHELF 20
    sprintf(buf, "'SHELF'(16'%08x)", (int)(word) obj);	/* possibly truncated */
    return STRSZ_SHELF;
}


static int
_strsz_heap_arr(t_heap_array *obj, int quoted)	/* obj != NULL */
{
    return STRSZ_SHELF;
}


/*----------------------------------------------------------------------
 * Initialisation
 *----------------------------------------------------------------------*/

void
bip_shelf_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("shelf_create", 3), p_shelf_create3, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("shelf_create", 2), p_shelf_create2, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("shelf_get_",4), p_shelf_get, B_UNSAFE|U_FRESH);
	(void) built_in(in_dict("shelf_set_",4), p_shelf_set, B_SAFE);
	(void) built_in(in_dict("shelf_inc_",3), p_shelf_inc, B_SAFE);
	(void) built_in(in_dict("shelf_dec_",3), p_shelf_dec, B_SAFE);
	(void) built_in(in_dict("shelf_abolish", 1), p_handle_free, B_SAFE|U_NONE);
	(void) local_built_in(in_dict("shelf_name",3), p_shelf_name, B_SAFE);
    }
}
