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
 * Copyright (C) 1989-2007 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: bip_record.c,v 1.2 2008/07/16 17:12:32 kish_shen Exp $
 */

/* ********************************************************************
 *
 *	ECLiPSe built-ins for the indexed database
 *
 ******************************************************************** */

#include    "config.h"

#include <stdio.h>  /* for sprintf() */

#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include	"dict.h"
#include	"property.h"
#include	"module.h"




static dident	d_visible_;



/*----------------------------------------------------------------------
 * Recorded database primitives
 *
 * Data structure is a circular doubly linked list with one dummy
 * element as header. The header is referred to by the IDB_PROP
 * property (but could also be passed around as a handle of type
 * heap_rec_header_tid).
 * 
 * Individual recorded iterms are identified by their list element
 * and handles of type heap_rec_tid are used as "db references".
 * They are always created as part of a record-list, but can continue
 * to exist independently when their db-reference was obtained and
 * they were subsequently erased from the list.
 *----------------------------------------------------------------------*/


/* INSTANCE TYPE DECLARATION */

typedef struct record_elem {
    uword		ref_ctr;	/* one count for list membership */
    struct record_elem	*next, *prev;	/* NULL if not in list */
    uword		hash;
    pword		term;		/* TEND for header cell */
} t_heap_rec;


/* METHODS */


/* Allocation of both header and proper elements */

static t_heap_rec *
_rec_create(void)
{
    t_heap_rec *obj = (t_heap_rec *) hg_alloc_size(sizeof(t_heap_rec));
    obj->ref_ctr = 1;
    obj->next = obj->prev = obj;
    obj->term.val.nint = 0;
    obj->term.tag.kernel = TEND;	/* remains TEND for header cells */
    return obj;
}


t_ext_ptr
ec_record_create(void)
{
    return (t_ext_ptr) _rec_create();
}


/* Lose a reference to an element */

static void
_rec_free_elem(t_heap_rec *this)
{
    if (--this->ref_ctr <= 0)
    {
	if (this->term.tag.kernel == TEND)
	    ec_panic("Trying to free record list header", "_rec_free_elem()");

#ifdef DEBUG_RECORDS
	p_fprintf(current_err_, "\n_rec_free_elem(0x%x)", this);
	ec_flush(current_err_);
#endif
	free_heapterm(&this->term);
	hg_free_size((generic_ptr) this, sizeof(t_heap_rec));
    }
}


/* Remove and lose all elements from header's list (but note that the
 * elements may survive if db-references to them still exist) */

static void
_rec_free_elems(t_heap_rec *header)
{
    t_heap_rec *this = header->next;
    if (header->term.tag.kernel != TEND)
	ec_panic("Not a record list header", "_rec_free_all()");

    while (this != header)
    {
	t_heap_rec *next = this->next;
	this->prev = this->next = 0;
	_rec_free_elem(this);
	this = next;
    }
    header->next = header->prev = header;
}


/* Lose a reference to the whole list identified by header */

static void
_rec_free_all(t_heap_rec *header)
{
    if (--header->ref_ctr <= 0)
    {
#ifdef DEBUG_RECORDS
	p_fprintf(current_err_, "\n_rec_free_all(0x%x)", header);
	ec_flush(current_err_);
#endif
	_rec_free_elems(header);
	hg_free_size((generic_ptr) header, sizeof(t_heap_rec));
    }
}


static t_heap_rec *
_rec_copy_elem(t_heap_rec *this)	/* this != NULL */
{
    ++this->ref_ctr;
    return this;
}


static void
_rec_mark_elem(t_heap_rec *this)	/* this != NULL */
{
    mark_dids_from_heapterm(&this->term);
}


static void
_rec_mark_all(t_heap_rec *header)	/* header != NULL */
{
    t_heap_rec *this = header->next;
    if (header->term.tag.kernel != TEND)
	ec_panic("Not a record list header", "_rec_mark_all()");
    while (this != header)
    {
	_rec_mark_elem(this);
	this = this->next;
    }
}

static int
_rec_tostr_elem(t_heap_rec *obj, char *buf, int quoted)	/* obj != NULL */
{
#define STRSZ_DBREF 20
    sprintf(buf, "'DBREF'(16'%08lx)", (unsigned long int)obj);
    return STRSZ_DBREF;
}

static int
_rec_strsz_elem(t_heap_rec *obj, int quoted) /* obj != NULL */
{
    return STRSZ_DBREF;
}


static int
_rec_tostr_all(t_heap_rec *obj, char *buf, int quoted) /* obj != NULL */
{
#define STRSZ_REC 18
    sprintf(buf, "'REC'(16'%08lx)", (unsigned long int)obj);
    return STRSZ_REC;
}

static int
_rec_strsz_all(t_heap_rec *obj, int quoted) /* obj != NULL */
{
    return STRSZ_REC;
}



/* CLASS DESCRIPTOR (method table) */
t_ext_type heap_rec_tid = {
    (void (*)(t_ext_ptr)) _rec_free_elem,
    (t_ext_ptr (*)(t_ext_ptr)) _rec_copy_elem,
    (void (*)(t_ext_ptr)) _rec_mark_elem,
    (int (*)(t_ext_ptr,int)) _rec_strsz_elem,
    (int (*)(t_ext_ptr,char*,int)) _rec_tostr_elem,
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _rec_copy_elem,
    0,	/* get */
    0	/* set */
};

t_ext_type heap_rec_header_tid = {
    (void (*)(t_ext_ptr)) _rec_free_all,
    (t_ext_ptr (*)(t_ext_ptr)) _rec_copy_elem,
    (void (*)(t_ext_ptr)) _rec_mark_all,
    (int (*)(t_ext_ptr,int)) _rec_strsz_all,
    (int (*)(t_ext_ptr,char*,int)) _rec_tostr_all,
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _rec_copy_elem,
    0,	/* get */
    0	/* set */
};


/*----------------------------------------------------------------------
 * PROLOG INTERFACE
 *----------------------------------------------------------------------*/


/* get the record header from either the functor key or a handle */

static int
_get_rec_list(value vrec, type trec, value vmod, type tmod, t_heap_rec **pheader)
{
    if (SameTypeC(trec, THANDLE))
    {
	Get_Typed_Object(vrec, trec, &heap_rec_header_tid, *pheader);
    }
    else
    {
	dident key_did;
	pword *prop;
	int err;
	Get_Key_Did(key_did,vrec,trec)
	prop = get_modular_property(key_did, IDB_PROP, vmod.did, tmod, VISIBLE_PROP, &err);
	if (!prop)
	    return err == PERROR ? NO_LOCAL_REC : err;
	*pheader = (t_heap_rec *) prop->val.ptr;
        if (!IsTag(prop->tag.kernel,TPTR) || !IsTag((*pheader)->term.tag.kernel,TEND))
	    ec_panic("Not a valid record-property", "_get_rec_list()");
    }
    return PSUCCEED;
}


/*
 * is_record(Key)@Module checks whether Key is a record key (or handle)
 * on which recorded terms have been (and still are) stored.
 */

static int
p_is_record_body(value vrec, type trec, value vmod, type tmod)
{
    t_heap_rec *header;
    int		err;

    a_mutex_lock(&PropertyLock);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header);
    if (err == NO_LOCAL_REC || err == STALE_HANDLE)
	err = PFAIL;
    else if (err == PSUCCEED && header->next == header)
	err = PFAIL;
    a_mutex_unlock(&PropertyLock);
    return err;
}

  
/* record_create(-Handle) creates an anonymous record */

static int
p_record_create(value vrec, type trec)
{
    pword rec;
    Check_Ref(trec);
    rec = ec_handle(&heap_rec_header_tid, (t_ext_ptr) _rec_create());
    Return_Unify_Pw(vrec, trec, rec.val, rec.tag);
}


static int
p_local_record_body(value vkey, type tkey, value vmod, type tmod)
{
    pword	*prop, *p;
    dident	key_did;
    int		err;

    Get_Functor_Did(vkey, tkey, key_did);
    
    a_mutex_lock(&PropertyLock);

    prop = set_modular_property(key_did, IDB_PROP, vmod.did, tmod,
				LOCAL_PROP, &err);
    if (!prop)
    {
	a_mutex_unlock(&PropertyLock);
	if (err == PERROR)
	    { Succeed_; }	/* exists already */
	else
	    Bip_Error(err);
    }
    prop->val.wptr = (uword *) _rec_create();
    prop->tag.kernel = TPTR;
    a_mutex_unlock(&PropertyLock);
    Succeed_;
}


static int
p_global_record_body(value vkey, type tkey, value vmod, type tmod)
{
    pword	*prop, *p;
    dident	key_did;
    int		err;

    Get_Functor_Did(vkey, tkey, key_did);
    
    a_mutex_lock(&PropertyLock);

    prop = set_modular_property(key_did, IDB_PROP, vmod.did, tmod,
				GLOBAL_PROP, &err);
    if (!prop)
    {
	a_mutex_unlock(&PropertyLock);
	Bip_Error((err == PERROR) ? LOCAL_REC : err);
    }
    prop->val.wptr = (uword *) _rec_create();
    prop->tag.kernel = TPTR;
    a_mutex_unlock(&PropertyLock);
    Succeed_;
}


static int
p_abolish_record_body(value vkey, type tkey, value vmod, type tmod)
{
    dident	key_did;
    int		err;
    
    if (IsHandle(tkey))
    {
	return p_handle_free(vkey, tkey);
    }
    else
    {
	Get_Functor_Did(vkey, tkey, key_did);

	err = erase_modular_property(key_did, IDB_PROP, vmod.did,tmod, LOCAL_PROP);

	if (err < 0)
	{
	    Bip_Error((err == PERROR) ? NO_LOCAL_REC : err);
	}
	else
	    Succeed_;
    }
}


/* record[az](+Key, ?Term)@Module */

static int
p_recorda_body(value vrec, type trec, value vterm, type tterm, value vmod, type tmod)
{
    t_heap_rec *obj, *header;
    pword copy_pw;
    int err = PSUCCEED;

    if ((err = create_heapterm(&copy_pw, vterm, tterm)) != PSUCCEED)
	{ Bip_Error(err); }

    a_mutex_lock(&PropertyLock);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header);
    if (err != PSUCCEED) goto _unlock_return_err_;

    obj = _rec_create();
    move_heapterm(&copy_pw, &obj->term);
    obj->next = header->next;
    obj->prev = header;
    header->next->prev = obj;
    header->next = obj;

_unlock_return_err_:
    a_mutex_unlock(&PropertyLock);
    return err;
}

static int
p_recordz_body(value vrec, type trec, value vterm, type tterm, value vmod, type tmod)
{
    t_heap_rec *obj, *header;
    pword copy_pw;
    int err = PSUCCEED;

    if ((err = create_heapterm(&copy_pw, vterm, tterm)) != PSUCCEED)
	{ Bip_Error(err); }

    a_mutex_lock(&PropertyLock);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header);
    if (err != PSUCCEED) goto _unlock_return_err_;

    obj = _rec_create();
    move_heapterm(&copy_pw, &obj->term);
    obj->next = header;
    obj->prev = header->prev;
    header->prev->next = obj;
    header->prev = obj;

_unlock_return_err_:
    a_mutex_unlock(&PropertyLock);
    return err;
}


/* record[az](+Key, ?Term, -DbRef)@Module */

static int
p_recorda3_body(value vrec, type trec, value vterm, type tterm, value vdref, type tdref, value vmod, type tmod)
{
    t_heap_rec *obj, *header;
    pword copy_pw, ref_pw;
    int err = PSUCCEED;

    if ((err = create_heapterm(&copy_pw, vterm, tterm)) != PSUCCEED)
	{ Bip_Error(err); }

    a_mutex_lock(&PropertyLock);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header);
    if (err != PSUCCEED) goto _unlock_return_err_;

    obj = _rec_create();
    move_heapterm(&copy_pw, &obj->term);
    obj->next = header->next;
    obj->prev = header;
    header->next->prev = obj;
    header->next = obj;
    obj = _rec_copy_elem(obj);
    a_mutex_unlock(&PropertyLock);
    ref_pw = ec_handle(&heap_rec_tid, (t_ext_ptr) obj);
    Return_Unify_Pw(vdref, tdref, ref_pw.val, ref_pw.tag);

_unlock_return_err_:
    a_mutex_unlock(&PropertyLock);
    return err;
}

static int
p_recordz3_body(value vrec, type trec, value vterm, type tterm, value vdref, type tdref, value vmod, type tmod)
{
    t_heap_rec *obj, *header;
    pword copy_pw, ref_pw;
    int err = PSUCCEED;

    if ((err = create_heapterm(&copy_pw, vterm, tterm)) != PSUCCEED)
	{ Bip_Error(err); }

    a_mutex_lock(&PropertyLock);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header);
    if (err != PSUCCEED) goto _unlock_return_err_;

    obj = _rec_create();
    move_heapterm(&copy_pw, &obj->term);
    obj->next = header;
    obj->prev = header->prev;
    header->prev->next = obj;
    header->prev = obj;
    obj = _rec_copy_elem(obj);
    a_mutex_unlock(&PropertyLock);
    ref_pw = ec_handle(&heap_rec_tid, (t_ext_ptr) obj);
    Return_Unify_Pw(vdref, tdref, ref_pw.val, ref_pw.tag);

_unlock_return_err_:
    a_mutex_unlock(&PropertyLock);
    return err;
}


/* recorded_list(+Key, -Terms)@Module */

static int
_rec_all(t_heap_rec *header, int dbrefs_only, pword *result)
{
    t_heap_rec *el;

    for (el = header->next; el != header; el = el->next)
    {
	pword *car = TG;
	Make_List(result, car);
	Push_List_Frame();
	if (dbrefs_only)
	    *car = ec_handle(&heap_rec_tid, (t_ext_ptr) _rec_copy_elem(el));
	else
	    get_heapterm(&el->term, car);
	result = car+1;
    }
    Make_Nil(result);
    Succeed_;
}

/* filter for recorded terms: simple tests to reduce the recorded terms
   returned to the ECLiPSe level, which needs to be unified with the filter
   term. The filter test performs simple comparison on the arguments of the first
   argument of a recorded term against the filter, This is designed to
   speed up the matching of dynamic predicates, which are recorded as (H ?- B),
   so that `filtering' is performed on the head H
*/
static int
_filtered_rec_term(uword ftag, pword *farg, dident fdid, pword *term)
{
    pword *f, *targ;
    int i;
    if (ftag == TCOMP)
    {
	targ = term->val.ptr;
	if (fdid != (targ++)->val.did) return 0;
    } else if (ftag == TLIST)
    {
	targ = term->val.ptr;
    } else
	return 1;

    if (IsRef(farg->tag)) return 1;
    if (IsRef(targ->tag)) return 1;
    if (DifferType(farg->tag, targ->tag)) return 0;
    switch (TagType(farg->tag))
    {
    case TINT:
    case TDICT:
	if (targ->val.all != farg->val.all) return 0;
	break;
    case TNIL:
	return 1;
	break;
    case TCOMP:
	targ = targ->val.ptr;
	farg = farg->val.ptr;
	if (farg->val.did != targ->val.did) return 0;
	i = DidArity(farg->val.did);
    _check_rec_args:
	do
	{
	    f = ++farg;
	    ++targ;
	    Dereference_(f);
	    if (IsRef(f->tag)) continue;
	    if (IsRef(targ->tag)) continue;
	    if (DifferType(f->tag, targ->tag)) return 0;
	    switch (TagType(f->tag))
	    {
	    case TINT:
	    case TDICT:
		if (f->val.all != targ->val.all) return 0;
		break;
	    case TCOMP:
		if (f->val.ptr->val.did != targ->val.ptr->val.did) return 0;
		break;
	    default:
		continue;
	    }
	} while (--i > 0);
	break;
    case TLIST:
	targ = targ->val.ptr-1;
	farg = farg->val.ptr-1;
	i = 1; /* just check the head */
	goto _check_rec_args;
    }
    return 1;
}

static int
_rec_all_filtered(t_heap_rec *header, value vfilter, type tfilter, pword *result)
{
    t_heap_rec *el;
    pword *farg = NULL;
    dident fdid = NULL;
    uword ftag = TagType(tfilter);

    if (ftag == TCOMP)
    {
	farg = vfilter.ptr;
	fdid = (farg++)->val.did;
	Dereference_(farg);
    } else if (ftag == TLIST)
    { 
	farg = vfilter.ptr;
	Dereference_(farg);
    }

    for (el = header->next; el != header; el = el->next)
    {
	if (IsRef(tfilter) || IsRef(el->term.tag) || 
	    (SameType(tfilter, el->term.tag) && (
	     (!ISPointer(el->term.tag.kernel) && vfilter.all == el->term.val.all) 
	     ||  _filtered_rec_term(ftag, farg, fdid, &(el->term))) )
	   )
	{
	    pword *car = TG;
	    Make_List(result, car);
	    Push_List_Frame();
	    *car = ec_handle(&heap_rec_tid, (t_ext_ptr) _rec_copy_elem(el));
	    result = car+1;
	}
    }
    Make_Nil(result);
    Succeed_;
}


static int
p_recorded_list_body(value vrec, type trec, value vl, type tl, value vmod, type tmod)
{
    t_heap_rec *header;
    pword list;
    int err;

    Check_Output_List(tl);
    a_mutex_lock(&PropertyLock);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header);
    if (err == PSUCCEED)
    {
	_rec_all(header, 0, &list);
	a_mutex_unlock(&PropertyLock);
	Return_Unify_Pw(vl, tl, list.val, list.tag);
    }
    else if (err == NO_LOCAL_REC)
    {
	a_mutex_unlock(&PropertyLock);
	Return_Unify_Nil(vl, tl);
    }
    else
    {
	a_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
}


/* recorded_refs(+Key, ?Filter, -Refs)@Module */

static int
p_recorded_refs_body(value vrec, type trec, value vfilter, type tfilter, value vl, type tl, value vmod, type tmod)
{
    t_heap_rec *header;
    pword list;
    int err;

    Check_Output_List(tl);
    a_mutex_lock(&PropertyLock);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header);
    if (err == PSUCCEED)
    {
	_rec_all_filtered(header, vfilter, tfilter, &list);
	a_mutex_unlock(&PropertyLock);
	Return_Unify_Pw(vl, tl, list.val, list.tag);
    }
    else if (err == NO_LOCAL_REC)
    {
	a_mutex_unlock(&PropertyLock);
	Return_Unify_Nil(vl, tl);
    }
    else
    {
	a_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
}


/* erase_all(+Key)@Module */

static int
p_erase_all_body(value vrec, type trec, value vmod, type tmod)
{
    t_heap_rec *header;
    int err;

    a_mutex_lock(&PropertyLock);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header);
    if (err == PSUCCEED)
    {
	_rec_free_elems(header);
    }
    else if (err == NO_LOCAL_REC)
    {
	err = PSUCCEED;
    }
    a_mutex_unlock(&PropertyLock);
    Bip_Error(err);
}


/* referenced_record(+DbRef, -Term) */

static int
p_referenced_record(value vrec, type trec, value vl, type tl)
{
    t_heap_rec *obj;
    pword result;

    Get_Typed_Object(vrec, trec, &heap_rec_tid, obj);
    get_heapterm(&obj->term, &result);
    if (IsRef(result.tag) && result.val.ptr == &result)
    {
	Succeed_;
    }
    Return_Unify_Pw(vl, tl, result.val, result.tag);
}


/* erase(+DbRef) */

static int
p_erase(value vrec, type trec)
{
    t_heap_rec *obj;
    pword result;

    Get_Typed_Object(vrec, trec, &heap_rec_tid, obj);
    a_mutex_lock(&PropertyLock);
    if (obj->next)
    {
	obj->next->prev = obj->prev;
	obj->prev->next = obj->next;
	obj->prev = obj->next = 0;
	_rec_free_elem(obj);
	a_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    else /* was already removed from record-list */
    {
	a_mutex_unlock(&PropertyLock);
	Fail_;
    }
}


/*
 * Two predicates for stepping through the recorded list:
 * first_recorded(+Key, -Ref)@Module is semidet
 * next_recorded(+Ref, -Ref) is semidet
 * These cannot be used for logica update semantics!
 */

static int
p_first_recorded(value vrec, type trec, value vdref, type tdref, value vmod, type tmod)
{
    t_heap_rec *header;
    pword ref_pw;
    int err;

    a_mutex_lock(&PropertyLock);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header);
    if (err == PSUCCEED)
    {
	t_heap_rec *obj = header->next;
	if (obj == header)
	{
	    a_mutex_unlock(&PropertyLock);
	    Fail_;
	}
	obj = _rec_copy_elem(obj);
	a_mutex_unlock(&PropertyLock);
	ref_pw = ec_handle(&heap_rec_tid, (t_ext_ptr) obj);
	Return_Unify_Pw(vdref, tdref, ref_pw.val, ref_pw.tag);
    }
    else if (err == NO_LOCAL_REC)
    {
	a_mutex_unlock(&PropertyLock);
	Fail_;
    }
    else
    {
	a_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
}


static int
p_next_recorded(value vref1, type tref1, value vref2, type tref2)
{
    t_heap_rec *obj;
    pword ref_pw;

    Get_Typed_Object(vref1, tref1, &heap_rec_tid, obj);
    a_mutex_lock(&PropertyLock);
    obj = obj->next;
    if (!obj || IsTag(obj->term.tag.kernel,TEND))
    {
	a_mutex_unlock(&PropertyLock);
	Fail_;
    }
    obj = _rec_copy_elem(obj);
    a_mutex_unlock(&PropertyLock);
    ref_pw = ec_handle(&heap_rec_tid, (t_ext_ptr) obj);
    Return_Unify_Pw(vref2, tref2, ref_pw.val, ref_pw.tag);
}


/*----------------------------------------------------------------------
 * Special purpose record meta_attribute/0 for storing attribute-name
 * mapping.  We assume the record contains [Name|Index] pairs.
 *----------------------------------------------------------------------*/

static t_heap_rec	*rec_meta_attribute_;

int
meta_index(dident wd)
{
    t_heap_rec *this;

    for (this = rec_meta_attribute_->next; this != rec_meta_attribute_; this = this->next)
    {
	if (this->term.val.ptr[0].val.did == wd)
	    return this->term.val.ptr[1].val.nint;
    }
    return 0;
}


dident
meta_name(int slot)
{
    t_heap_rec *this;

    for (this = rec_meta_attribute_->next; this != rec_meta_attribute_; this = this->next)
    {
	if (this->term.val.ptr[1].val.nint == slot)
	    return this->term.val.ptr[0].val.did;
    }
    return D_UNKNOWN;
}


/*----------------------------------------------------------------------
 * the subsequent BIPs fail on error and set the global variable
 *----------------------------------------------------------------------*/

#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

/*
 * Check if key is a valid key for records
 */

/* ARGSUSED */
static int
p_valid_key(value v, type t)
{
    Error_If_Ref(t);
    if (IsAtom(t) || IsStructure(t) || IsNil(t) || IsList(t))
	{ Succeed_; }
    Check_Typed_Object_Handle(v, t, &heap_rec_header_tid);
    Succeed_;
}

#undef Bip_Error
#define Bip_Error(N) return(N);

/*----------------------------------------------------------------------
 * End of fail on error BIPs
 *----------------------------------------------------------------------*/

void
bip_record_init(int flags)
{
    pri		*pd;
    type	t;
    value	v1, v2;
    int		res;

    d_visible_ = in_dict("visible", 0);

    if (flags & INIT_SHARED)
    {
	(void) local_built_in(in_dict("valid_key", 1),
				 p_valid_key, B_SAFE|U_SIMPLE);
	(void) exported_built_in(in_dict("erase_all_body", 2),
				 p_erase_all_body, B_UNSAFE);
	(void) exported_built_in(in_dict("is_record_body", 2),
				 p_is_record_body, B_UNSAFE);
	(void) exported_built_in(in_dict("recorda_body", 3),
				 p_recorda_body, B_UNSAFE);
	(void) exported_built_in(in_dict("recordz_body", 3),
			         p_recordz_body, B_UNSAFE);
	(void) exported_built_in(in_dict("recorda_body", 4),
				 p_recorda3_body, B_UNSAFE);
	(void) exported_built_in(in_dict("recordz_body", 4),
				 p_recordz3_body, B_UNSAFE);
	exported_built_in(in_dict("recorded_list_body", 3),
			p_recorded_list_body, B_UNSAFE|U_FRESH)
	    -> mode = BoundArg(2, NONVAR);
	exported_built_in(in_dict("recorded_refs_body", 4),
			p_recorded_refs_body, B_UNSAFE|U_FRESH)
	    -> mode = BoundArg(2, NONVAR);
	exported_built_in(in_dict("referenced_record", 2),
				 p_referenced_record, B_UNSAFE|U_FRESH)
	    -> mode = BoundArg(2, NONVAR);
	(void) exported_built_in(in_dict("erase", 1), p_erase, B_UNSAFE);
	(void) exported_built_in(in_dict("record_create", 1),
				 p_record_create, B_UNSAFE);
	(void) exported_built_in(in_dict("local_record_body", 2),
				 p_local_record_body, B_UNSAFE);
	(void) local_built_in(in_dict("global_record_body", 2),
				 p_global_record_body, B_UNSAFE);
	(void) exported_built_in(in_dict("abolish_record_body", 2),
				 p_abolish_record_body, B_UNSAFE);
	(void) local_built_in(in_dict("first_recorded_", 3),
				 p_first_recorded, B_UNSAFE);
	(void) local_built_in(in_dict("next_recorded", 2),
				 p_next_recorded, B_UNSAFE);
    }

    t.kernel = ModuleTag(d_.kernel_sepia);
    v1.did = in_dict("meta_attribute", 0);
    v2.did = d_.kernel_sepia;
    if (flags & INIT_SHARED)
    {
	(void) p_local_record_body(v1, tdict, v2, t);
    }
    rec_meta_attribute_ = (t_heap_rec *) get_modular_property(v1.did,
			IDB_PROP, v2.did, t, LOCAL_PROP, &res)->val.ptr;
}


