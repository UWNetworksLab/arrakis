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
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: property.c,v 1.1 2008/06/30 17:43:58 jschimpf Exp $
 */

/*
 * IDENTIFICATION:	property.c
 *
 * DESCRIPTION:		property list handling and
 *			term copying routines
 *
 * CONTENTS:
 *	properties:	set_property()
 *			get_property()
 *			erase_property()
 *			set_modular_property()
 *			get_modular_property()
 *			erase_modular_property()
 *
 *	term copying:	create_heapterm()
 *			get_heapterm()
 *			free_heapterm()
 *			move_heapterm()
 *			make_heapterm_persistent()
 *
 * AUTHOR:		bruno, joachim
 *
 * This version implements the following semantics of property lists:
 * -	There is no difference between module independent properties
 *	and module dependent global properties.
 *	Therefore the same routines can be used for both.
 * -	Independent/global properties can be created, accessed, modified
 *	and erased from everywhere. We always work on the visible property
 *	(except when a local is created it may hide a global one).
 * -	When a module is erased, its local properties are erased as well.
 */


#include "config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#else
extern char *strcpy();
#endif

#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "io.h"
#include "module.h"
#include "property.h"
#include "emu_export.h"


extern void	erase_all_records(pword *prop);

static void	free_prop_value(int, pword*);

void
	mark_dids_from_array(pword *prop_value),
	mark_dids_from_pwords(pword *from, register pword *to),
	mark_dids_from_heapterm(pword *root),
	mark_dids_from_properties(property *prop_list);

extern void		handle_copy_anchor(pword*,pword*,int);

extern pword		*transf_meta_out(value val, type tag, pword *top, dident mod, pword *presult),
			*transf_meta_in(pword *pw, dident mod, int *err);

extern pword		*p_meta_arity_;

static int		_fill_procedures(register pword *env, dident mod, type tmod);

#define Property_Error(err_ptr, err_no)	\
    *err_ptr = err_no;			\
    return 0;

static void
_rem_from_module_entry(property *m, module_item *pm)
{
    register property *p, **prev;
    prev = &(pm->properties);
    p = *prev;
    while (p != m)
    {
	if (!p) return;	/* should not happen, but ... */
	prev = &p->next_prop;
	p = *prev;
    }
    *prev = p->next_prop;
}

/*
 * create a new module-independent property descriptor
 */

pword *
set_property(dident functor, int property_name)
{
    int	err;
    /* the module is not used */
    return set_modular_property(functor, property_name,
			    d_.default_module, tdict, GLOBAL_PROP, &err);
}


/*
 * create a new property descriptor
 *
 * flag is one of {GLOBAL_PROP, LOCAL_PROP}.
 * the module is not important, but must de different from D_UNKNOWN.
 * If a descriptor already exists, NULL is returned, else
 * the return value is a pointer to the property value of the new descriptor.
 * A local definition hides an existing global one.
 *
 * A global descriptor is always created, even when only local properties
 * exist. It is the one in the property chain. If no global property
 * exists, its module field contains D_UNKNOWN, otherwise it holds the
 * definition module (which is not further used for globals).
 * The global descriptor is the head of a circular list of local properties.
 * The property_value field of any descriptor is initialised with a TEND tag.
 *
 * If an error occurs, nil is returned and the integer referenced by
 * err_ref is set to the error number. If the value returned is non nil,
 * it points to a valid property and *err_ref is not changed.
 * It is guaranty that err_ref will not be accessed if there is no
 * error (i.e. 0 can be passed if it shure there is no property and
 * that the module access is ok)
 *
 * Since this function returns a pointer into a property descriptor,
 * it must only be called inside an interrupt protected area !!!
 */

pword *
set_modular_property(dident functor, int property_name, dident module, type mod_tag, int flag, int *err_ref)
{
    register property	*p, *head;
    module_item		*pm;

    if (flag == LOCAL_PROP && IsLocked(module)
	&& !IsModuleTag(module, mod_tag))
    {
	Property_Error(err_ref, LOCKED);
    }

    /* get pointer to property list from atom */
    a_mutex_lock(&PropListLock);
    head = p = DidProperties(functor);

    while (p && p->name != property_name)	/* find the right one	*/
    {
	head = p;
	p = p->next_prop;
    }

    if (!p)					/* no such property yet	*/
    {
	p = (property *) hg_alloc_size(sizeof(property));
	p->name = property_name;
	p->next_prop = (property *) NULL;
	p->next_mod = p;
	p->module = D_UNKNOWN;
	if (head)
	    head->next_prop = p;
	else
	    DidProperties(functor) = p;
    }

    if (flag == GLOBAL_PROP)
    {
	if (p->module == D_UNKNOWN)
	{
	    p->module = module;			/* fill unused descriptor */
	    p->property_value.tag.kernel = TEND;
	    a_mutex_unlock(&PropListLock);
	    return &p->property_value;
	}
	else
	{
	    a_mutex_unlock(&PropListLock);
	    Property_Error(err_ref, PERROR)/* global exists already */
	}
    }

    /* else if (flag == LOCAL_PROP) */
    head = p;	
    for(p = head->next_mod; p != head; p = p->next_mod)
    {
	if (p->module == module)
	{
	    a_mutex_unlock(&PropListLock);
	    Property_Error(err_ref, PERROR); /* a local exists	*/
	}
    }

    /* insert a new descriptor at the beginning	*/
    p = (property *) hg_alloc_size(sizeof(property));
    p->name = property_name;
    p->module = module;
    p->property_value.tag.kernel = TEND;
    p->next_mod = head->next_mod;
    head->next_mod = p;
    a_mutex_unlock(&PropListLock);
    
    a_mutex_lock(&ModuleLock);
    pm = (module_item *) (get_property(module, MODULE_PROP))->val.ptr;
    p->next_prop = pm->properties;
    pm->properties = p;
    a_mutex_unlock(&ModuleLock);

    return &p->property_value;
}


/*
 * get a module independent or the global property
 */

pword * 
get_property(dident functor, int property_name)
{
    int	err;
    
    return get_modular_property(functor, property_name,
				D_UNKNOWN, tdict, GLOBAL_PROP, &err);
}


/*
 * get a property
 * flag is one of {VISIBLE_PROP, GLOBAL_PROP, LOCAL_PROP}.
 *
 * If an error occurs, nil is returned and the integer referenced by
 * err_ref is set to the error number. If the value returned is non nil,
 * it points to a valid property and *err_ref indicates which property
 * was returned (GLOBAL_PROP or LOCAL_PROP).
 *
 * Since this function returns a pointer into a property descriptor,
 * it must only be called inside an interrupt protected area !!!
 */

pword *
get_modular_property(dident functor, int property_name, dident module, type mod_tag, int which, int *res)
{
	register property	*p, *m;

	if (which != GLOBAL_PROP && IsLocked(module)
	    && !IsModuleTag(module, mod_tag))
	{
	    Property_Error(res, LOCKED);
	}

	/* scan property list until an entry for property is found or end */
	a_mutex_lock(&PropListLock);
	for (p = DidProperties(functor); p; p = p->next_prop)
	{
	    if (p->name == property_name)
	    {
		if (which != GLOBAL_PROP)
		    for (m = p->next_mod; m != p; m = m->next_mod)
		    {
			if (m->module == module) {
			    *res = LOCAL_PROP;
			    a_mutex_unlock(&PropListLock);
			    return(&m->property_value);	/* return the local */
			}
		    }

		a_mutex_unlock(&PropListLock);
		if (which != LOCAL_PROP  &&  p->module != D_UNKNOWN) {
		    *res = GLOBAL_PROP;
		    return(&p->property_value);	/* return the global */
		}
		else
		{
		    Property_Error(res, PERROR); /* no global */
		}
	    }
	}
	a_mutex_unlock(&PropListLock);
	Property_Error(res, PERROR);
}


/*
 * Quick routine to get a module-independent property.
 * Does not return a pointer into the property, therefore no lock
 * necessary around call.
 */
int
get_simple_property(dident functor, int property_name, pword *result)
{
    property	*p;

    a_mutex_lock(&PropListLock);
    for (p = DidProperties(functor); p; p = p->next_prop)
    {
	if (p->name == property_name)
	{
	    a_mutex_unlock(&PropListLock);
	    *result = p->property_value;
	    return PSUCCEED;
	}
    }
    a_mutex_unlock(&PropListLock);
    return PFAIL;
}


/*
 * erase a module independent or the global property
 */

int
erase_property(dident functor, int property_name)
{
	return erase_modular_property(functor, property_name,
				      D_UNKNOWN, tdict, GLOBAL_PROP);
}


/*
 * erase a property
 * flag is one of {VISIBLE_PROP, GLOBAL_PROP, LOCAL_PROP}.
 * This function can return a valid Prolog error code.
 * a successful erase may return PSUCCEED or PFAIL. The later
 * is return if the property has been completely removed for functor
 * i.e the global and all locals.
 */

int
erase_modular_property(dident functor, int property_name, dident module, type mod_tag, int which)
{
	register property	*p, **prev_p;
	int			res;
	module_item		*pm;

	if (which != GLOBAL_PROP && IsLocked(module)
	    && !IsModuleTag(module, mod_tag))
	{
	    return LOCKED;
	}

	/* this lookup must be before the lock */
	if (which != GLOBAL_PROP)
	    pm = (module_item *) (get_property(module, MODULE_PROP))->val.ptr;

	a_mutex_lock(&PropListLock);
	/* get pointer to property list from atom */
	prev_p = &(DidProperties(functor));
	p = *prev_p;

	/* scan property list until an entry for property is found or end */
	while (p)
	{
	    if (p->name == property_name)
	    {
		if (which != GLOBAL_PROP)
		{
		    register property	 *m, **prev_m;

		    prev_m = &(p->next_mod);
		    m = *prev_m;

		    while (m != p)	/* scan module list */
		    {
			if (m->module == module)
			{			/* erase the local	*/
			    *prev_m = m->next_mod;

			    _rem_from_module_entry(m, pm);
			    free_prop_value(property_name, &m->property_value);
			    hg_free_size((generic_ptr) m, sizeof(property));

			    if (p->next_mod == p && p->module == D_UNKNOWN)
			    {	/* all erased, remove head descriptor	*/
				*prev_p = p->next_prop;
				hg_free_size((generic_ptr) p, sizeof(property));
                              /* this is not an error, it is a message
                                 to notify that the property is erased
                                 completely */
                              res = PFAIL;
			      goto _unlock_return_;
			    }
			    res = PSUCCEED;
			    goto _unlock_return_;
			}
			prev_m = &(m->next_mod);
			m = *prev_m;
		    }
		}
		if (which != LOCAL_PROP  &&  p->module != D_UNKNOWN)
		{				/* erase the global	*/
		    free_prop_value(property_name, &p->property_value);
		    if (p->next_mod == p)
		    {		/* no locals: remove global descriptor	*/
			*prev_p = p->next_prop;
			hg_free_size((generic_ptr) p, sizeof(property));
                      /* this is not an error, it is a message to notify
                         that the property is erased completely       */
			res = PFAIL;
			goto _unlock_return_;
		    }
		    else
			p->module = D_UNKNOWN;	/* just mark it unused	*/
		    res = PSUCCEED;
		    goto _unlock_return_;
		}
		res = PERROR;
		goto _unlock_return_;		/* should give a warning */
	    }
	    prev_p = &(p->next_prop);
	    p = *prev_p;
	}
	res = PERROR;
_unlock_return_:
	a_mutex_unlock(&PropListLock);
        return(res);
}


/*
 * this is to be called from erase_module
 * prop_list is a list of module dependent (local) property descriptors
 * linked with the next_prop field
 */

void
erase_module_props(property *prop_list)
{
    register property *p;

    while(prop_list)
    {
	p = prop_list->next_mod;

	while (p->next_mod != prop_list)
	    p = p->next_mod;
	p->next_mod = prop_list->next_mod;

	p = prop_list;
	prop_list = prop_list->next_prop;
	free_prop_value((int) p->name, &p->property_value);
	hg_free_size((generic_ptr) p, sizeof(property));
    }
}


/*
 * free all space associated to the property value
 */

static void
free_prop_value(int prop_name, pword *prop_value)
{
    switch(prop_name)
    {
    case GLOBVAR_PROP:
	if (IsGlobalPrologRef(prop_value)) {
	    ec_ref_destroy((ec_ref) prop_value->val.wptr);
	    prop_value->val.wptr = NULL;
	}
	/* If we are erasing the last global ref, decrement the global index */
	else if (IsGlobalPrologRefIndex(prop_value) &&
		prop_value->val.nint == (GlobalVarIndex - 1))
	{
	    GlobalVarIndex--;
	}
	else
	{
	    free_heapterm(prop_value);
	}
	break;

    case ARRAY_PROP:
	free_array(prop_value);
	break;

    case IDB_PROP:
    {
	extern t_ext_type heap_rec_header_tid;
	heap_rec_header_tid.free((t_ext_ptr)prop_value->val.wptr);
	break;
    }

    case HTABLE_PROP:
    {
	extern t_ext_type heap_htable_tid;
	heap_htable_tid.free((t_ext_ptr)prop_value->val.wptr);
	break;
    }

    case SHELF_PROP:
    {
	extern t_ext_type heap_array_tid;
	heap_array_tid.free((t_ext_ptr)prop_value->val.wptr);
	break;
    }

    case MODULE_PROP:
    case TRANS_PROP:
    case WRITE_TRANS_PROP:
    case GOAL_TRANS_PROP:
    case WRITE_GOAL_TRANS_PROP:
    case CLAUSE_TRANS_PROP:
    case WRITE_CLAUSE_TRANS_PROP:
	hg_free((generic_ptr)prop_value->val.ptr);
	break;

    case EVENT_PROP:
    case STREAM_PROP:
    case PREFIX_PROP:
    case INFIX_PROP:
    case POSTFIX_PROP:
    case SYSCALL_PROP:
	break;

    default:
	p_fprintf(current_err_, "Unknown property type %d in free_prop_value()\n", prop_name);
	ec_flush(current_err_);
	break;
    }
}


/*
 * Support function for the dictionary garbage collector.
 * Mark all DIDs that occur in the given property list
 * (ie. treat all the properties a single functor).
 */

void
mark_dids_from_properties(property *prop_list)
{
    for (; prop_list; prop_list = prop_list->next_prop)
    {
	register property *p = prop_list;
	do
	{
	    if (p->module != D_UNKNOWN)
	    {
		switch (p->name)
		{
		case ARRAY_PROP:
		    mark_dids_from_array(&p->property_value);
		    break;

		case GLOBVAR_PROP:
		    mark_dids_from_heapterm(&p->property_value);
		    break;

		case HTABLE_PROP:
		    {
			extern t_ext_type heap_htable_tid;
			heap_htable_tid.mark_dids((t_ext_ptr)p->property_value.val.wptr);
		    }
		    break;

		case SHELF_PROP:
		    {
			extern t_ext_type heap_array_tid;
			heap_array_tid.mark_dids((t_ext_ptr)p->property_value.val.wptr);
		    }
		    break;

		case IDB_PROP:
		    {
			extern t_ext_type heap_rec_header_tid;
			heap_rec_header_tid.mark_dids((t_ext_ptr)p->property_value.val.wptr);
		    }
		    break;

		case TRANS_PROP:
		case WRITE_TRANS_PROP:
		case GOAL_TRANS_PROP:
		case WRITE_GOAL_TRANS_PROP:
		case CLAUSE_TRANS_PROP:
		case WRITE_CLAUSE_TRANS_PROP:
		    {
			macro_desc *md = (macro_desc *) p->property_value.val.ptr;
			Mark_Did(md->trans_function);
			Mark_Did(md->module);
		    }
		    break;

		case MODULE_PROP:
		    {
			module_item *m = (module_item *) p->property_value.val.ptr;
			register didlist *scan;
			for (scan = m->imports; scan; scan = scan->next)
			{
			    Mark_Did(scan->name);
			}
		    }
		    break;

		case STREAM_PROP:	/* just an integer */
		    break;

		case PREFIX_PROP:	/* did */
		case INFIX_PROP:	/* did */
		case POSTFIX_PROP:	/* did */
		case SYSCALL_PROP:	/* did or integer */
		case EVENT_PROP:	/* pri */
		    mark_dids_from_pwords(&p->property_value, &p->property_value + 1);
		    break;

		default:
		    p_fprintf(current_err_, "Unknown property type %d in mark_dids_from_properties()\n", p->name);
		    ec_flush(current_err_);
		    break;
		}
	    }
	    p = p->next_mod;
	} while (p != prop_list);
    }
}


/******* the following functions are for test purposes *******

smp(functorv, functorp, propv, propp, modv, modp, flagv, flagp, v, t)
value functorv, propv, modv, flagv, v;
type  functorp, propp, modp, flagp, t;
{
	int	flag, err;
	pword *pw;

	if (flagv.did == in_dict("local", 0))
	{
	    flag = LOCAL_PROP;
	}
	else
	{
	    flag = GLOBAL_PROP;
	}
	pw = set_modular_property(functorv.did, propv.nint,
				  modv.did, modp, flag, &err);
	if (!pw) { Bip_Error(err); }
	pw->val.all = v.all;
	pw->tag.all = t.all;
	Succeed_;
}

sip(functorv, functorp, propv, propp, v, t)
value functorv, propv, v;
type  functorp, propp, t;
{
	pword	*pw;

	pw = set_property(functorv.did, propv.nint);
	if (!pw) { Bip_Error(err); }
	pw->val.all = v.all;
	pw->tag.all = t.all;
	Succeed_;
}

gmp(functorv, functorp, propv, propp, modv, modp, v, t)
value functorv, propv, modv, v;
type  functorp, propp, modp, t;
{
	pword	*pw;
	int	err;

	pw = get_modular_property(functorv.did, propv.nint, modv.did, modp
				  VISIBLE_PROP, &err);
	if (!pw) { Bip_Error(err); }
	Return_Unify_Pw(v, t, pw->val, pw->tag);
}

gip(functorv, functorp, propv, propp, v, t)
value functorv, propv, v;
type  functorp, propp, t;
{
	pword	*pw;
	int	err;

	pw = get_property(functorv.did, propv.nint);
	if (!pw) { Bip_Error(err); }
	Return_Unify_Pw(v, t, pw->val, pw->tag);
}

emp(functorv, functorp, propv, propp, modv, modp)
value functorv, propv, modv;
type  functorp, propp, modp;
{
	return erase_modular_property(functorv.did, propv.nint, modv.did,
				      modp, VISIBLE_PROP);
}

eip(functorv, functorp, propv, propp)
value functorv, propv;
type  functorp, propp;
{
	return erase_property(functorv.did, propv.nint);
}

****** end of test functions ******/


/*---------------------------------------------------------
 * low level routines for copying prolog terms:
 *
 * A "root" pword must always exist. For properties it is
 * the pword in the property descriptor. If the copied term
 * is simple (or persistent), no further memory is needed.
 * Otherwise there is a single memory block connected to the
 * root pword. The root value points to the second pword of
 * the memory block, the first pword holds the data size.
 * The size is used when the term is copied back to the stack
 * to avoid the need for recursive traversal (_copy_block()).
 * CAUTION: if the root pword has the PERSISTENT bit set, it may
 * or may not point to a complete memory block with header. It
 * may be the result of copying an already persistent subterm
 * (then it probably has no header), or the result of making a proper
 * copy persistent (then the header is there, but we don't know it).
 *
 * The block which contains the actual copied term is followed
 * by a table containing pointers into the copy, marking all
 * the embedded external handles (their anchor frames, i.e.
 * the TEXTERN/TPTR pair). This table is used when freeing the
 * copy, to adjust the handle's reference counts.
 *
 *			+-------+  \
 *			|	|  |
 *			: table :   >  <num_h> words
 *			|	|  |
 *			+-------+ <
 *			|	|  |
 *			|-------|  |
 *			|	|  |
 *			: term  :   >  <size> bytes (multiple of pwords)
 * +--------+		|	|  |
 * | T...   |		|-------|  |
 * |--------|		|	|  |
 * |    -------------->	+-------+ <
 * +--------+		| num_h	|  |
 *    root		|-------|   >  1 pword header
 *			| size	|  |
 *			+-------+ <--- hg_alloc_size'd area
 * 
 * The format of heap terms must be identical to global stack terms
 * (at least for ground terms) for the following reasons:
 * - Ground heap terms can be referred to from the stacks, either as
 *   a whole or in part. They can only be freed if we know that there
 *   are no such references. The root word never has such references.
 * - Nonground heap terms must NOT be referred to from the stacks,
 *   and their variables must never be bound.  Their only use is for
 *   copying back to the global stack.  They can be freed at any time.
 * - Ground heap terms may be compared against stack terms via
 *   compare_terms().
 * ---------------------------------------------------------*/

#define IsNonpersistentHeaptermRoot(root) \
 		(ISPointer((root)->tag.kernel) \
 		&& !IsSelfRef(root) \
		&& !IsPersistent((root)->tag))

#define HeaptermHeader(pw)	((pw)-1)

#define HeaptermSize(pw)	HeaptermHeader(pw)->val.nint
#define HeaptermNumHandles(pw)	HeaptermHeader(pw)->tag.kernel

#define HeaptermHandleTable(pw)	((value*)((pw) + HeaptermSize(pw)/sizeof(pword)))


/*
 * Two-pass heap copying algorithm, copying with cycles, and preserving
 * sharing of subtrees, including suspensions, buffers, variables, handles.
 * We do two depth-first traversals.
 *
 * First pass: Every node that may have multiple references is marked with
 * ALREADY_SEEN at the first encounter, and with NEED_FWD on the second.
 * We use the two GC bits for this, and don't trail these bit settings.
 *
 * Second pass: On encountering a node marked with
 *   ALREADY_SEEN
 *	copy node, reset the marker
 *   ALREADY_SEEN|NEED_FWD
 *	copy node, reset the marker, create (trailed) a forwarding pointer.
 *   FORWARDed
 *	use the forwarded pointer for the copy
 *
 * Untrail to remove the forwarding pointers.
 *
 * A nasty complication is caused by list (cons) cells: they have no header
 * which could be used for the mark bits/forwarding pointer. We have to use
 * the first (car) cell for this. Unfortunately, this could contain a simple
 * variable (TVAR_TAG+self_ref), which may need marking/forwarding itself.
 * We solve this by not marking these variables (effectively assuming they
 * are always marked ALREADY_SEEN|NEED_FWD), and using two different tags
 * for forwarding pointers, TFORWARD for variables, TFORWARD2 for lists.
 */

#define ALREADY_SEEN	MARK
#define NEED_FWD	LINK

#if 1
#define Assert(test) { \
    	if (!(test)) { \
	    p_fprintf(current_err_, "Internal error in heap copying"); \
	    ec_flush(current_err_); \
	} \
}
#else
#define Assert(test)
#endif


/* 
 * copy the given term to the pword pointed to by dest.
 * When space for structures, mutables etc is needed, it is allocated
 * using the top pointer. top is incremented and the new value is the
 * return code.
 * 
 * v, t is supposed to be dereferenced
 *
 * This routine does some (trailed) modifications of the term, so
 * don't forget to untrail after calling it!
 */

/* This macro is just an optimisation to reduce recursive calls */

#define Copy_Term_To_Heap(v, t, top, handle_slot, dest) \
	if (IsSimple(t)) {\
	    dest->val.all = v.all;\
	    dest->tag.all = t.all;\
	} else {\
	    top = _copy_term_to_heap(v, t, top, handle_slot, dest);\
	    if (!top) return top;\
	}

static pword *
_copy_term_to_heap(value v, type t, register pword *top, value **handle_slot, register pword *dest)
{
    register pword *pw, *arg_pw;
    register long arity;
    dident fdid;
    int dead;

    for(;;)			/* tail recursion loop	*/
    {
	switch(TagType(t))
	{
	case TVAR_TAG:			/* a simple variable, possibly a car */
	    arg_pw = v.ptr;
	    if (!IsTag(arg_pw->tag.kernel, TFORWARD2))
	    {
		Assert(t.kernel == arg_pw->tag.kernel);
		arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
		Trail_(arg_pw);		/* install forwarding pointer */
		arg_pw->val.ptr = dest;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    dest->val.ptr = dest;	/* make a new simple variable */
	    dest->tag.kernel = TREF;
	    return top;

	case TFORWARD:			/* a previously copied variable */
	    dest->val.ptr = v.ptr;
	    dest->tag.kernel = TREF;
	    return top;

	case TFORWARD2:			/* a previously copied cons cell */
	    arg_pw = v.ptr;		/* of which we need only the car */
	    Dereference_(arg_pw);	/* which could be a reference!!! */
	    *dest = *arg_pw;
	    return top;

	case TUNIV:
	case TNAME:
	    dest->val.ptr = top;
	    dest->tag.kernel = TREF;
	    arg_pw = v.ptr;
	    Assert(t.kernel == arg_pw->tag.kernel);
	    Assert(t.kernel & ALREADY_SEEN);
	    if (t.kernel & NEED_FWD)
	    {
		arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
		t.kernel = arg_pw->tag.kernel;
		Trail_Tag(arg_pw);
		arg_pw->val.ptr = top;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    else
	    {
		arg_pw->tag.kernel &= ~ALREADY_SEEN;
		t.kernel = arg_pw->tag.kernel;
	    }
	    dest = top++;
	    dest->val.ptr = dest;
	    dest->tag.kernel = t.kernel;
	    return top;

	case TMETA:
	    dest->val.ptr = top;	/* reference to copied variable	*/
	    dest->tag.kernel = TREF;
	    arg_pw = v.ptr;
	    Assert(t.kernel == arg_pw->tag.kernel);
	    Assert(t.kernel & ALREADY_SEEN);
	    if (t.kernel & NEED_FWD)
	    {
		arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
		t.kernel = arg_pw->tag.kernel;
		Trail_Tag(arg_pw);
		arg_pw->val.ptr = top;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    else
	    {
		arg_pw->tag.kernel &= ~ALREADY_SEEN;
		t.kernel = arg_pw->tag.kernel;
	    }
	    dest = top;
	    top += 2;
	    dest->val.ptr = dest;	/* create a self reference	*/
	    dest++->tag.kernel = t.kernel;
	    arg_pw = MetaTerm(arg_pw);	/* and copy meta information	*/
	    arity = 1;
	    break;

	case TSUSP:
	    arg_pw = v.ptr;
	    dest->tag = t;
	    t.kernel = arg_pw->tag.kernel;
	    if (IsTag(t.kernel,TFORWARD))
	    {
		dest->val = arg_pw->val;	/* was already copied		*/
		return top;
	    }
	    Assert(SameTypeC(t, TDE));
	    Assert(t.kernel & ALREADY_SEEN);
	    arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
	    dead = SuspDead(arg_pw);
	    dest->val.ptr = top;
	    dest = top;
	    top += SUSP_HEADER_SIZE;
	    dest[SUSP_LD].val.ptr = (pword *) 0;
	    dest[SUSP_FLAGS].tag.all = arg_pw[SUSP_FLAGS].tag.all;
	    /* should not put the pri into the heap, but did and module */
	    dest[SUSP_PRI].val.all = arg_pw[SUSP_PRI].val.all;
	    dest[SUSP_INVOC].tag.all = 0L;
	    if (t.kernel & NEED_FWD)
	    {
		Trail_Pword(arg_pw);
		arg_pw->val.ptr = dest;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    if (dead)
		return top;
	    top += SUSP_SIZE - SUSP_HEADER_SIZE;
	    Init_Susp_State(dest, SuspPrio(arg_pw));
	    dest += SUSP_GOAL;		/* copy goal and module */
	    arg_pw += SUSP_GOAL;
	    arity = SUSP_SIZE - SUSP_GOAL;
	    break;

	case THANDLE:
	    arg_pw = v.ptr;
	    dest->tag = t;
	    t.kernel = arg_pw->tag.kernel;
	    if (IsTag(t.kernel,TFORWARD))
	    {
		dest->val = arg_pw->val;	/* was already copied		*/
		return top;
	    }
	    Assert(SameTypeC(t, TEXTERN));
	    if (!ExternalClass(arg_pw)->copy)
		goto _copy_heap_error_;
	    Assert(t.kernel & ALREADY_SEEN);
	    arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
	    dest->val.ptr = top;		/* make the copy */
	    *(*handle_slot)++ = dest->val; 	/* Enter into handle table */
	    handle_copy_anchor(arg_pw, top, 0);
	    if (t.kernel & NEED_FWD)
	    {
		Trail_Pword(arg_pw);		/* install forwarding pointer */
		arg_pw->val.ptr = top;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    return top + HANDLE_ANCHOR_SIZE;

	case TINT:
	case TNIL:
	case TDICT:
	case TPTR:
	case TPROC:
#ifdef UNBOXED_DOUBLES
	case TDBL:
#endif
_copy_simple_:
	    dest->val.all = v.all;	/* the simple types	*/
	    dest->tag.all = t.all;
	    return top;

	case TIVL:
	case TSTRG:
#ifndef UNBOXED_DOUBLES
	case TDBL:
#endif
	    if (IsPersistent(t))
	    {
		Assert(!(TG_ORIG <= v.ptr && v.ptr < TG));
		goto _copy_simple_;
	    }
	    arg_pw = v.ptr;
	    dest->tag = t;
	    t.kernel = arg_pw->tag.kernel;
	    if (IsTag(t.kernel,TFORWARD))
	    {
		dest->val = arg_pw->val;	/* was already copied		*/
		return top;
	    }
	    Assert(SameTypeC(t, TBUFFER));
	    Assert(t.kernel & ALREADY_SEEN);
	    arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
	    dest->val.ptr = top;		/* make the copy */
	    arity = BufferPwords(arg_pw);
	    Set_Buffer_Size(top, BufferSize(arg_pw));
	    top++->tag.kernel = TBUFFER;
	    pw = arg_pw + 1;
	    do				/* copy arity/sizeof(pword) pwords */
		*top++ = *pw++;
	    while(--arity > 1);
	    if (t.kernel & NEED_FWD)
	    {
		Trail_Pword(arg_pw);		/* install forwarding pointer */
		arg_pw->val.ptr = dest->val.ptr;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    return top;

	case TLIST:
	    if (IsPersistent(t))
	    {
		Assert(!(TG_ORIG <= v.ptr && v.ptr < TG));
		goto _copy_simple_;
	    }
	    arg_pw = v.ptr;
	    if (!(arg_pw->tag.kernel & ALREADY_SEEN))
	    {
		if (IsTag(arg_pw->tag.kernel,TFORWARD2))
		{
		    dest->val = arg_pw->val;	/* cons cell already copied */
		    dest->tag.kernel = TLIST;
		    return top;
		}
		else if (IsTag(arg_pw->tag.kernel,TFORWARD))
		{
		    /* the car was already copied and forwarded */
		    dest->val.ptr = top;	/* allocate the cons cell copy */
		    dest->tag.kernel = TLIST;
		    dest = top;
		    top += 2;

		    dest->val.ptr = arg_pw->val.ptr; /* use forwarded car */
		    dest->tag.kernel = TREF;

		    arg_pw->val.ptr = dest;	/* update forwarding pointer, no need to trail */
		    arg_pw->tag.kernel = Tag(TFORWARD2);

		    ++dest;			/* go and copy the cdr */
		    ++arg_pw;
		    arity = 1;
		}
		else
		{
		    p_fprintf(current_err_, "INTERNAL ERROR in copy_term_to_heap()\n");
		    goto _copy_heap_error_;
		}
	    }
	    else if (arg_pw->tag.kernel & NEED_FWD)
	    {
		arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);

		pw = arg_pw;		/* get and remember the car pword */
		Dereference_(pw);
		v.all = pw->val.all;
		t.all = pw->tag.all;

		Trail_Pword(arg_pw);	/* install forwarding pointer */
		arg_pw->val.ptr = top;
		arg_pw->tag.kernel = Tag(TFORWARD2);

		dest->val.ptr = top;	/* allocate the cons cell copy */
		dest->tag.kernel = TLIST;

		dest = top;		/* copy car (already overwritten) */
		top += 2;
		Copy_Term_To_Heap(v, t, top, handle_slot, dest);

		++dest;			/* go copy the cdr */
		++arg_pw;
		arity = 1;
	    }
	    else
	    {
		arg_pw->tag.kernel &= ~ALREADY_SEEN;
		dest->val.ptr = top;	/* allocate the cons cell copy */
		dest->tag.kernel = TLIST;
		dest = top;		/* go copy car + cdr */
		top += 2;
		arity = 2;
	    }
	    break;

	case TCOMP:
	    if (IsPersistent(t))
	    {
		Assert(!(TG_ORIG <= v.ptr && v.ptr < TG));
		goto _copy_simple_;
	    }
	    arg_pw = v.ptr;
	    if (IsTag(arg_pw->tag.kernel,TFORWARD))
	    {
		dest->val = arg_pw->val;	/* was already copied		*/
		dest->tag.kernel = TCOMP;
		return top;
	    }
	    dest->val.ptr = top;		/* begin the copy */
	    dest->tag.kernel = TCOMP;
	    fdid = arg_pw->val.did;
	    arity = DidArity(fdid);
	    dest = top;
	    top += arity +1;
	    dest->val.did = fdid;
	    dest->tag.kernel = TDICT;
	    Assert(arg_pw->tag.kernel & ALREADY_SEEN);
	    if (arg_pw->tag.kernel & NEED_FWD)
	    {
		arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
		Trail_Pword(arg_pw);		/* install forwarding pointer */
		arg_pw->val.ptr = dest;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    else
	    {
		arg_pw->tag.kernel &= ~ALREADY_SEEN;
	    }
	    ++dest;
	    ++arg_pw;
	    break;	/* (arg_pw,arity,top,dest) */

/* EXTENSION SLOT HERE */

	default:
	    if (TagType(t) >= 0 && TagType(t) <= NTYPES)
	    {
		top = tag_desc[TagType(t)].copy_to_heap(v, t, top, dest);
		return top;
	    }
_copy_heap_error_:
	    return NULL;
	}

	for(;;)		/* copy <arity> pwords beginning at <arg_pw>	*/
	{
	    pw = arg_pw++;
	    Dereference_(pw);
	    if (--arity == 0)
		break;
	    Copy_Term_To_Heap(pw->val, pw->tag, top, handle_slot, dest);
	    dest += 1;
	}
	v.all = pw->val.all;
	t.all = pw->tag.all;
    }
}


/*
 * Return amount of memory needed to make a heap copy of the given pword.
 * The pword itself is not counted, so simple types yield size 0.
 * The size returned is in bytes.
 * When an unknown type is encountered, the error flag *perr is set
 * and we return immediately.
 *
 * This routine leaves behind ALREADY_SEEN/NEED_FWD bits in some tags.
 * These bit settings are not trailed, therefore they must be undone
 * carefully in _copy_term_to_heap(), even in the case of an error!
 */

static long
_copy_size(value v, type t, word size, word *num_handles, int *perr)
{
    register pword *pw, *arg_pw;
    register long arity;

    for(;;)			/* tail recursion loop	*/
    {
	switch(TagType(t))
	{
	case TVAR_TAG:			/* simple variable (self ref) */
	    /* Why is this not treated like the other variables, i.e. using
	     * the ALREADY_SEEN/NEED_FWD bits? Because this variable could be
	     * the car of a cons cell, and there is a conflict between using
	     * the bits for marking the variable and marking the list cell.
	     * We need the bits for marking list cells, so we don't mark simple
	     * variables and always assume ALREADY_SEEN|NEED_FWD for them.
	     */
	    return size;

	case TUNIV:
	case TNAME:
	    if (t.kernel & ALREADY_SEEN)
	    {
		v.ptr->tag.kernel |= NEED_FWD;
		return size;
	    }
	    v.ptr->tag.kernel |= ALREADY_SEEN;
	    return size + sizeof(pword);

	case TMETA:
	    if (t.kernel & ALREADY_SEEN)
	    {
		v.ptr->tag.kernel |= NEED_FWD;
		return size;
	    }
	    v.ptr->tag.kernel |= ALREADY_SEEN;
	    size += 2L * sizeof(pword);
	    arg_pw = MetaTerm(v.ptr);
	    arity = 1;
	    break;

	case TSUSP:
	    arg_pw = v.ptr;
	    if (arg_pw->tag.kernel & ALREADY_SEEN)
	    {
		arg_pw->tag.kernel |= NEED_FWD;
		return size;
	    }
	    Assert(SameTypeC(arg_pw->tag, TDE));
	    arg_pw->tag.kernel |= ALREADY_SEEN;
	    if (SuspDead(arg_pw))
		return size + ((long) SUSP_HEADER_SIZE * sizeof(pword));
	    size += (long) SUSP_SIZE * sizeof(pword);
	    arity = SUSP_SIZE - SUSP_GOAL;
	    arg_pw += SUSP_GOAL;
	    break;

	case THANDLE:
	    arg_pw = v.ptr;
	    if (arg_pw->tag.kernel & ALREADY_SEEN)
	    {
		arg_pw->tag.kernel |= NEED_FWD;
		return size;
	    }
	    Assert(SameTypeC(arg_pw->tag, TEXTERN));
	    if (!ExternalClass(arg_pw)->copy)
		goto _copy_size_error_;
	    arg_pw->tag.kernel |= ALREADY_SEEN;
	    (*num_handles)++;
	    return size + HANDLE_ANCHOR_SIZE * sizeof(pword);

	case TINT:
	case TNIL:
	case TDICT:
	case TPTR:
	case TPROC:
#ifdef UNBOXED_DOUBLES
	case TDBL:
#endif
	    return size;

	case TIVL:
	case TSTRG:
#ifndef UNBOXED_DOUBLES
	case TDBL:
#endif
	    if (IsPersistent(t))
		return size;
	    arg_pw = v.ptr;
	    if (arg_pw->tag.kernel & ALREADY_SEEN)
	    {
		arg_pw->tag.kernel |= NEED_FWD;
		return size;
	    }
	    Assert(SameTypeC(arg_pw->tag, TBUFFER));
	    arg_pw->tag.kernel |= ALREADY_SEEN;
	    return size + BufferPwords(arg_pw) * sizeof(pword);

	case TLIST:
	    if (IsPersistent(t))
		return size;
	    arg_pw = v.ptr;
	    if (arg_pw->tag.kernel & ALREADY_SEEN)
	    {
		arg_pw->tag.kernel |= NEED_FWD;
		return size;
	    }
	    arg_pw->tag.kernel |= ALREADY_SEEN;
	    arity = 2;
	    size += 2L * sizeof(pword);
	    break;

	case TCOMP:
	    if (IsPersistent(t))
		return size;
	    arg_pw = v.ptr;
	    if (arg_pw->tag.kernel & ALREADY_SEEN)
	    {
		arg_pw->tag.kernel |= NEED_FWD;
		return size;
	    }
	    arg_pw->tag.kernel |= ALREADY_SEEN;
	    arity = DidArity(arg_pw->val.did);
	    ++arg_pw;
	    size += (arity+1)*sizeof(pword);
	    break;

/* EXTENSION SLOT HERE: compute size from v, t */

	default:
	    if (TagType(t) >= 0 && TagType(t) <= NTYPES)
		return size + tag_desc[TagType(t)].copy_size(v, t);

_copy_size_error_:
	    *perr = 1;
	    return size;
	}

	for(;;)		/* count <arity> pwords beginning at <arg_pw>	*/
	{
	    pw = arg_pw++;
	    Dereference_(pw);
	    if (--arity == 0)
		break;
	    if (!IsSimple(pw->tag) && !IsVar(pw->tag))
	    {
		size = _copy_size(pw->val, pw->tag, size, num_handles, perr);
		if (*perr)
		    return size;
	    }
	}
	v.all = pw->val.all;
	t.all = pw->tag.all;
    }
}


/*
 * copy a consecutive heap block of <size> bytes beginning
 * at <from> to destination <to>, relocating all the pointers
 */

static void
_copy_block(register pword *from, register pword *to, word size)
{
    word offset = (char *) to - (char *) from;
    pword *start = from;
    pword *end = from + size/sizeof(pword);
    register long i;

    while(from < end)
    {
	if (ISPointer(from->tag.kernel))
	{
	    /* relocate pointers when within the copied block */
	    if (start <= from->val.ptr  &&  from->val.ptr < end) {
		to->val.str = from->val.str + offset;
		to->tag.kernel = IsRef(from->tag) ? from->tag.kernel
					: Tag(from->tag.kernel);
	    } else {
		*to = *from;
	    }
	    to++; from++;
	}
	else if (!ISSpecial(from->tag.kernel))
	    *to++ = *from++;
	else
	    switch (TagType(from->tag))
	    {
	    case TDE:
		if (!SuspDead(from))
		{
		    to[SUSP_LD].val.ptr = LD;
		    to[SUSP_FLAGS].tag.all = from[SUSP_FLAGS].tag.all;
		    to[SUSP_PRI].val.all = from[SUSP_PRI].val.all;
		    to[SUSP_INVOC].tag.all = 0L;
		    Init_Susp_State(to, SuspPrio(from));
		    Update_LD(to)
		    to += SUSP_GOAL;
		    from += SUSP_GOAL;
		}
		else
		{
		    to[SUSP_LD].val.ptr = (pword *) 0;
		    to[SUSP_FLAGS].tag.all = from[SUSP_FLAGS].tag.all;
		    to[SUSP_PRI].val.all = from[SUSP_PRI].val.all;
		    to[SUSP_INVOC].tag.all = 0L;
		    to += SUSP_HEADER_SIZE;
		    from += SUSP_HEADER_SIZE;
		}
		break;

	    case TEXTERN:
	    {
		handle_copy_anchor(from, to, 1);
		to += HANDLE_ANCHOR_SIZE;
		from += HANDLE_ANCHOR_SIZE;
		break;
	    }

	    case TBUFFER:
		i = BufferPwords(from);
		do
		    *to++ = *from++;
		while (--i > 0);
		break;

/* EXTENSION SLOT HERE */

	    default:
		p_fprintf(current_err_,
			"INTERNAL ERROR: illegal tag (%d) in _copy_block()\n",
			from->tag.kernel);
		break;
	    }
    }
}


/*
 * Make a heapterm persistent (i.e. it will never be freed again)
 *
 * - set PERSISTENT bits in all its internal (non-variable) pointer tags
 * - mark any DIDs within the term as DICT_PERMANENT
 *   (so dictionary gc does not need to mark persistent terms)
 *
 * Heapterms can only be made persistent if:
 *	- they are fully ground
 *	- do not contain handles
 */

void
make_heapterm_persistent(pword *root)
{
    if (IsNonpersistentHeaptermRoot(root))
    {
	pword *pw = root->val.ptr;
	pword *start = pw;
	pword *end = pw + HeaptermSize(pw) / sizeof(pword);

	/* CAUTION: by setting the PERSISTENT bit in root->tag we lose the
	 * information that root->val.ptr points to a complete heap-copy block
	 * (with header). It now looks identical to a persistent pointer into
	 * the middle of a persistent heap term! Hopefully we will never
	 * need this information again...  */
	root->tag.kernel |= PERSISTENT;	/* mark whole term as persistent */

	while(pw < end)
	{
	    if (ISPointer(pw->tag.kernel))
	    {
		Assert(!IsRef(pw->tag));	/* no variables */
		Assert(!IsHandle(pw->tag));	/* no handles */
		if (!IsPersistent(pw->tag))
		{
		    /* no pointers to non-persistent other heapterms */
		    Assert(start <= pw->val.ptr  &&  pw->val.ptr < end);
		    /* mark pointer to subterm as persistent */
		    pw->tag.kernel |= PERSISTENT;
		}
		pw++;
	    }
	    else if (IsTag(pw->tag.kernel, TBUFFER))
	    {
		pw += BufferPwords(pw);
	    }
	    else if (IsAtom(pw->tag))	/* atom or functor */
	    {
		Set_Did_Stability(pw->val.did, DICT_PERMANENT);
		pw++;
	    }
	    else if (IsString(pw->tag) && StringInDictionary(pw->val))
	    {
		dident a = check_did_n(StringStart(pw->val), StringLength(pw->val), 0);
		Assert(a != D_UNKNOWN);
		Set_Did_Stability(a, DICT_PERMANENT);
		pw++;
	    }
	    else
	    {
		Assert(!ISSpecial(pw->tag.kernel));
		pw++;
	    }
	}
    }
}


/*
 * Make a copy of the given term on the global stack.
 * Share ground subterm with the original.
 * Make a list of metaterm.copy pairs iff meta != 0
 */

static int
_copy_term(value v, type t, register pword *dest, register pword *meta, int marked_vars_only)
{
	register pword *pw, *arg_pw, *arg;
	register long arity;
	dident fdid;
	int	copied = 0;
	pword *save_tg = TG;

	switch(TagType(t))
	{
	case TVAR_TAG:
	    if (marked_vars_only && !(t.kernel & MARK))
	    	goto _global_var_;
	    /* CAUTION: t may have MARK bit set */
	    dest->val.ptr = dest;
	    dest->tag.kernel = TREF;
	    Trail_(v.ptr);
	    v.ptr->val.ptr = dest;
	    v.ptr->tag.kernel = Tag(TFORWARD);
	    return 1;

	case TFORWARD:
	    dest->val = v.ptr->val;	/* was already copied		*/
	    dest->tag.kernel = TREF;
	    return 1;

	case TUNIV:
	case TNAME:
	    if (marked_vars_only && !(t.kernel & MARK))
	    	goto _global_var_;
	    /* CAUTION: t may have MARK bit set */
	    dest->val.ptr = TG;
	    dest->tag.kernel = TREF;
	    Trail_Tag(v.ptr);
	    v.ptr->val.ptr = TG;
	    v.ptr->tag.kernel = Tag(TFORWARD);
	    dest = TG++;
	    Check_Gc
	    dest->val.ptr = dest;
	    dest->tag.kernel = t.kernel & ~MARK;
	    return 1;

	case TMETA:
	    if (marked_vars_only && !(t.kernel & MARK))
	    	goto _global_var_;
	    /* CAUTION: t may have MARK bit set */
	    Trail_Tag(v.ptr);			/* make forwarding pointer */
	    v.ptr->tag.kernel = Tag(TFORWARD);
	    if (meta)
	    {
		arg = TG;			/* allocate 2 list elements */
		TG += 4;
		Check_Gc

		arg[0].val.ptr = v.ptr;		/* pointer to metaterm */
		arg[0].tag.kernel = TREF;
		v.ptr->val.ptr =
		dest->val.ptr =
		arg[1].val.ptr = &arg[1];	/* free variable */
		dest->tag.kernel =
		arg[1].tag.kernel = TREF;

		arg[2].val.ptr = arg;		/* list cell holding pair */
		arg[2].tag.kernel = TLIST;
		arg[3] = *meta;
		meta->val.ptr = &arg[2];
		meta->tag.kernel = TLIST;
	    }
	    else
	    {
		v.ptr->val.ptr =
		dest->val.ptr = dest;		/* free variable */
		dest->tag.kernel = TREF;
	    }
	    return 1;

	case TSUSP:
	    dest->tag.all = t.all;
	    if (SameTypeC(v.ptr->tag, TDE))
	    {
		if (SuspDead(v.ptr))
		{
		    /* A dead suspension is ground, no need to copy it. */
		    dest->val.ptr = v.ptr;
		    return 0;
		}
		else	/* active */
		{
		    dest->val.ptr = TG;
		    arg = TG;
		    TG += SUSP_SIZE;
		    Check_Gc
		    arg[SUSP_LD].val.ptr = LD;
		    arg[SUSP_FLAGS].tag.all = v.ptr[SUSP_FLAGS].tag.all;
		    arg[SUSP_PRI].val.all = v.ptr[SUSP_PRI].val.all;
		    arg[SUSP_INVOC].tag.all = 0L;
		    Init_Susp_State(arg, SuspPrio(v.ptr));
		    Update_LD(arg)
		    Trail_Pword(v.ptr);
		    v.ptr->val.ptr = arg;
		    v.ptr->tag.kernel = Tag(TFORWARD);
		    arg += SUSP_GOAL;
		    arg_pw = v.ptr += SUSP_GOAL;
		    arity = SUSP_SIZE-SUSP_GOAL;
		    copied = 1;
		}
	    }
	    else if (IsForward(v.ptr->tag))	/* already copied */
	    {
		dest->val.ptr = v.ptr->val.ptr;
		return 1;
	    }
	    else
	    {
		p_fprintf(current_err_,"bad type in _copy_term: 0x%x\n",t.kernel);
		return 1;
	    }
	    break;

	case TLIST:
	    dest->val.ptr = TG;
	    dest->tag.kernel = TLIST;
	    arg = TG;
	    TG += 2;
	    Check_Gc
	    arg_pw = v.ptr;
	    arity = 2;
	    break;

	case TCOMP:
	    dest->val.ptr = TG;
	    dest->tag.kernel = TCOMP;
	    arg_pw = v.ptr;
	    fdid = arg_pw++->val.did;
	    arity = DidArity(fdid);
	    arg = TG;
	    TG += arity +1;
	    Check_Gc
	    arg->val.did = fdid;
	    arg++->tag.kernel = TDICT;
	    break;

	case TEXTERN:
	case TBUFFER:
	case TPTR:
	case TDE:
	    p_fprintf(current_err_,"ECLiPSe: bad type in _copy_term: 0x%x\n",t.kernel);
	    return 1;

/* EXTENSION SLOT HERE */

_global_var_:	/* A variable that doesn't get copied */
	    dest->val.ptr = v.ptr;
	    dest->tag.kernel = TREF;
    	    return 0;

	case THANDLE:
	default:	/* simple, ground stuff */
	    dest->val.all = v.all;
	    dest->tag.all = t.all;
	    return 0;
	}

	while(arity--)	/* copy <arity> pwords beginning at <arg_pw>	*/
	{
	    pw = arg_pw++;
	    Dereference_(pw);
	    if (IsSimple(pw->tag))
		*arg = *pw;
	    else
		copied |= _copy_term(pw->val, pw->tag, arg, meta, marked_vars_only);
	    arg += 1;
	}
	if (!copied)
	{
	    dest->val.all = v.all;	/* share the original */
	    dest->tag.all = t.all;
	    TG = save_tg;		/* pop the copy */
	}
	return copied;
}


/*
 * A support function for the dictionary garbage collector
 */

void
mark_dids_from_heapterm(pword *root)
{
    if (IsNonpersistentHeaptermRoot(root))
    {
	mark_dids_from_pwords(root->val.ptr,
		root->val.ptr + HeaptermSize(root->val.ptr)/sizeof(pword));
    }
    else
	mark_dids_from_pwords(root, root + 1);
}


/*---------------------------------------------------------
 * interface
 *---------------------------------------------------------*/

/*
 * free_heapterm(root,v,t) - free a heap term
 */

void
free_heapterm(pword *root)
{
    if (IsNonpersistentHeaptermRoot(root))
    {
	value *handle_slot;
	word count;

	/* free the term's embedded HANDLEs */

	/* First slot available in HANDLE table */
	handle_slot = HeaptermHandleTable(root->val.ptr);

	for( count = 0; count < HeaptermNumHandles(root->val.ptr); 
	     count++, handle_slot++ ) 
	{
            Assert(IsTag(handle_slot->ptr->tag.kernel, TEXTERN));
            if ( ExternalClass(handle_slot->ptr)->free
                && ExternalData(handle_slot->ptr))
            {
                ExternalClass(handle_slot->ptr)->free(ExternalData(handle_slot->ptr));
            }
	}	

	/* free the heap copy itself and its handle table */
	hg_free_size((generic_ptr) HeaptermHeader(root->val.ptr), 
		     HeaptermSize(root->val.ptr) + 
		     HeaptermNumHandles(root->val.ptr) * sizeof(value) + 
		     sizeof(pword));
    }
    root->tag.kernel = TEND;
}


/*
 * create_heapterm(root,v,t) - copy a prolog term to the general heap
 *
 * root points to a prolog word on the heap,
 * this is overwritten with a heap copy of v, t.
 * IMPORTANT: Stack pointers must be exported!
 */

int
create_heapterm(pword *root, value v, type t)
{
    pword **old_tt = TT;
    pword *pw = (pword*) 0, *top;
    value *handle_slot = (value *)0;
    word size, num_handles = 0;
    int err = 0;

    /* CAUTION: _copy_size() sets ALREADY_SEEN/NEED_FWD bits which are being
     * reset in _copy_term_to_heap(). We can not allow aborting in between
     * because we must not leave behind any ALREADY_SEEN/NEED_FWD bits.
     * Even if _copy_size() finds an error (err=1), we still need to call
     * _copy_term_to_heap() in order to reset all the marker bits!
     * This is ensured by wrapping into Disable_Exit()/Enable_Exit().
     */
    Disable_Exit();

    /* Find out how much space we are going to need, and allocate it */
    size = _copy_size(v, t, 0L, &num_handles, &err);
    Assert(TT == old_tt);
    if (size > 0)
    {
	pw = (pword *) hg_alloc_size(size + num_handles * sizeof(value) + sizeof(pword));
	pw++;
	HeaptermSize(pw) = size;		/* the first word holds the size */
	HeaptermNumHandles(pw) = num_handles;	/* the second holds the number of HANDLEs */
        handle_slot = HeaptermHandleTable(pw);	/* First slot available in HANDLE table */
    }

    /* Now make the copy and reset the bits. The function leaves forwarding
     * pointers (trailed), which are removed by subsequent untrailing.
     * Before calling, if t is a variable's tag, we reload it from memory
     * in order to pick up any bits that were set in it by _copy_size().
     */
    top = _copy_term_to_heap(v, IsRef(t) ? v.ptr->tag : t, pw, &handle_slot, root);
    Untrail_Variables(old_tt);

    /* Mark bits are now reset, we can release the Exit-protection */
    Enable_Exit();

    /* If there was a problem, throw the incomplete copy away */
    if (err)
    {
	if (size > 0)
	{
	    hg_free_size((generic_ptr) HeaptermHeader(pw), HeaptermSize(pw) + 
				HeaptermNumHandles(pw) * sizeof(value) + sizeof(pword));
	}
	return TYPE_ERROR;
    }

    if ((int8*)top - (int8*)pw != size)
	return PERROR;

    return PSUCCEED;
}


/*
 * like create_heapterm, but takes a C string as argument
 */

void
set_string_n(pword *root, char *string, int len)		/* string\0 + length */
{
    word size = BufferSizePwords(len+1) * sizeof(pword);
    pword *pw = (pword *) hg_alloc_size(size + sizeof(pword));
    pw++;
    HeaptermSize(pw) = size;
    HeaptermNumHandles(pw) = 0;	/* no handles */
    Set_Buffer_Size(pw, len+1);
    pw->tag.kernel = TBUFFER;
    Copy_Bytes((char *)(pw + 1), string, len);
    ((char *)(pw + 1))[len] = 0;
    root->val.ptr = pw;
    root->tag.kernel = TSTRG;
}

void
set_string(pword *root, char *string)		/* NUL-terminated string */
{
    set_string_n(root, string, strlen(string));
}


/*
 * get_heapterm(root, result) - get a prolog term from the heap
 *
 * root points to the heap word representing the term.
 * The result is stored in the pword referenced by result,
 * if the term is complex, space is allocated on the global stack.
 * IMPORTANT: Stack pointers must be exported!
 */

void
get_heapterm(pword *root, pword *result)
{

    if (ISPointer(root->tag.kernel))
    {
	if (IsSelfRef(root))
	    result->val.ptr = result;		/* if free var on heap	*/
	else if (IsPersistent(root->tag))
	    result->val.all = root->val.all;
	else	/* copy back to the stack */
	{
	    pword *orig = root->val.ptr;
	    pword *dest;
	    word size = HeaptermSize(orig);

	    result->val.ptr = dest = TG;	/* push complex term	*/
	    TG += size/sizeof(pword);
	    Check_Gc;
	    _copy_block(orig, dest, size);
	}
    }
    else
    {
	result->val.all = root->val.all;
    }
    result->tag.all = root->tag.all;
}


/*
 * Move the root of a heap term
 */
void
move_heapterm(pword *root_old, pword *root_new)
{
    if (ISPointer(root_old->tag.kernel) && IsSelfRef(root_old))
	root_new->val.ptr = root_new;
    else
	root_new->val.all = root_old->val.all;
    root_new->tag.all = root_old->tag.all;
}


/*
 * copy_term(+Term, -Copy)
 * copy_term(+Term, -Copy, -MetaList)
 */

static int
p_copy_simple_term(value v, type t, value vc, type tc)
{
    pword	result;
    pword	**old_tt = TT;

    (void) _copy_term(v, t, &result, (pword *) 0, 0);
    Untrail_Variables(old_tt);
    if (!(IsRef(result.tag) && IsSelfRef(&result)))
    {
	Return_Unify_Pw(vc, tc, result.val, result.tag)
    }
    Succeed_
}

static int
p_copy_term3(value v, type t, value vc, type tc, value vl, type tl)
{
    pword	result, list;
    pword	**old_tt = TT;
    Prepare_Requests

    list.tag.kernel = TNIL;
    (void) _copy_term(v, t, &result, &list, 0);
    Untrail_Variables(old_tt);
    if (!(IsRef(result.tag) && IsSelfRef(&result)))
    {
	Request_Unify_Pw(vc, tc, result.val, result.tag)
	Return_If_Failure
    }
    Return_Unify_Pw(vl, tl, list.val, list.tag)
}


/* auxiliary function for copy_term_vars/4 */

static void
_mark_variables_trailed(value val, /* a dereferenced argument */
			type tag)
          
{
    register int arity;
    register pword *arg_i;

    for (;;)
    {
	if (IsRef(tag))
	{
	    if (val.ptr->tag.kernel & MARK)
	    	return;
	    if (IsVar(tag))		/* mark the variable */
		{ Trail_(val.ptr) }
	    else
		{ Trail_Tag(val.ptr) }
	    val.ptr->tag.kernel |= MARK;
	    return;
	}
	else if (IsList(tag))
	{
	    arity = 2;
	}
	else if (IsStructure(tag))
	{
	    arity = DidArity(val.ptr->val.did);
	    val.ptr++;
	}
	else
	    return;
 
	for(;arity > 1; arity--)
	{
	    arg_i = val.ptr++;
	    Dereference_(arg_i);
	    _mark_variables_trailed(arg_i->val,arg_i->tag);
	}
	arg_i = val.ptr;		/* tail recursion */
	Dereference_(arg_i);
	val.all = arg_i->val.all;
	tag.all = arg_i->tag.all;
    }
}

static int
p_copy_term_vars(value vvars, type tvars, value v, type t, value vc, type tc, value vl, type tl)
{
    pword	result, list;
    pword	**old_tt = TT;
    Prepare_Requests

    list.tag.kernel = TNIL;
    _mark_variables_trailed(vvars, tvars);
    if (TT != old_tt)
    {
	(void) _copy_term(v, t, &result, &list, 1);
	Untrail_Variables(old_tt);
    }
    else	/* nothing to do */
    {
	result.val = v;
	result.tag = t;
    }
    if (!(IsRef(result.tag) && IsSelfRef(&result)))
    {
	Request_Unify_Pw(vc, tc, result.val, result.tag)
	Return_If_Failure
    }
    Return_Unify_Pw(vl, tl, list.val, list.tag)
}

static int
p_term_size(value v, type t, value vs, type ts)
{
    long size;
    pword root;

    Check_Output_Integer(ts);
    create_heapterm(&root, v, t);
    if (ISPointer(root.tag.kernel))
    {
	if (IsSelfRef(&root))
	    size = 0;
	else if (IsPersistent(root.tag))
	    size = 0;
	else
	    size = HeaptermSize(root.val.ptr);
    }
    else
    {
	size = 0;
    }
    free_heapterm(&root);
    Return_Unify_Integer(vs, ts, size);

}


/*---------------------------------------------------
 * the subsequent functions should no longer be used
 *---------------------------------------------------*/

/*
 * move a compound term to the destination.
 * Only on pass is needed, but this is a dangerous function since
 * we assume that there is enough free space at the destination !
 * The return value is the next free location.
 */

pword *
move_term(pword *pw, pword *dest)
{
    pword	**old_tt = TT;
    pword	*top;

    top = _copy_term_to_heap(pw->val, pw->tag, dest + 1, (value **)0, dest);
    Untrail_Variables(old_tt);
    return top;
}

/*---------------------------------------------------------------------------
 *
 * Prolog term <==> Database format conversion routines
 *
 *	pword *		term_to_dbformat(pword *term)
 *
 *	pword *		dbformat_to_term(char *buffer)
 *
 * These routines are used to convert Prolog terms into the external database
 * format and vice versa.
 * The main differences of the external format compared to standard term
 * representation are:
 *
 *	- no absolute addresses, but relative offsets
 *	- no dictionary references, but explicit strings
 *	- no alignment, more compact byte representation
 *	- a breadth-first, prefix representation
 *	- machine-independent (byte order, word size)
 *
 * Format description:
 *
 * <external_term> ::	<termsize>	<simple_term>+
 *
 * <simple_term> ::
 *	TNIL
 *	TINT		<int>
 *	TSTRG		<length>	<name>
 *	TDICT		<arity>		<length>	<name>
 *	TLIST		<offset>
 *	TCOMP		<offset>
 *	TVAR_TAG	<offset>
 *	TNAME		<offset>	<length>	<name>
 *	TMETA		<offset>	<length>	<name>
 *	TUNIV		<offset>	<length>	<name>
 *	TSUSP		<offset>
 *	TDE		<flags> 
 *
 * <flags>	::	<word>
 * <float>	::	<word>
 * <termsize>	::	<word>
 * <int>	::	<compact>
 * <arity>	::	<compact>
 * <length>	::	<compact>
 * <offset>	::	<compact>
 * <tag>	::	<byte>
 *
 * <word>	::	<byte> <byte> <byte> <byte>		(MSB first)
 * <compact>	::	<1byte>* <0byte>
 * <1byte>	::	<byte>					(byte >= 0x80)
 * <0byte>	::	<byte>					(byte <  0x80)
 * <name>	::	<byte>*
 *
 * An <offset> field holds a relative address (in words). When the term is
 * restored, the start address of the restored term is added to the relative
 * address to obtain the absolute one. Note that this is not an offset into
 * the external representation!
 *
 * During conversion to external format, in the original term the MARK bit is
 * used to mark variables that have already been encountered. Their value
 * field is temporarily overwritten with the proper <offset>. These destructive
 * modifications are trailed and are undone at the end of the conversion.
 *----------------------------------------------------------------------------*/

#define QUEUE_MASK_META		0x80000000
#define QUEUE_MASK		(QUEUE_MASK_META)
#define EnQueue_(pw, arity, mark) {					\
	if (queue_head) {						\
	    queue_tail[1].val.ptr = (pword *) hg_alloc_size(2*sizeof(pword));\
	    queue_tail = queue_tail[1].val.ptr;				\
	} else								\
	    queue_tail = queue_head = (pword *) hg_alloc_size(2*sizeof(pword));\
	queue_tail[0].val.ptr = (pw);					\
	queue_tail[0].tag.kernel = (arity|(mark));			\
	queue_tail[1].val.ptr = (pword *) 0;				\
}

#define DeQueue_(pw, arity, mark) {			\
	register pword *elem = queue_head;		\
	(pw) = elem[0].val.ptr;				\
	(arity) = elem[0].tag.kernel;			\
	(mark) = (arity) & QUEUE_MASK;			\
	(arity) = (arity) & ~QUEUE_MASK;		\
	queue_head = elem[1].val.ptr;			\
	hg_free_size((generic_ptr)elem, 2*sizeof(pword));	\
}

#define EmptyQueue() (!queue_head)


#define Reserve_Space(nbytes)				\
	if ((dest + nbytes) > (char *) TG) {		\
	    TG += (dest + nbytes + 32 - (char*)TG) / sizeof(pword);	\
	    Check_Gc;					\
	}

#define Store_Byte(byte) *dest++ = (char) (byte)
#define Store_Int32(word) {\
	    register unsigned long aux = (word);		\
	    *dest++ = (char) (aux >> 24);			\
	    *dest++ = (char) (aux >> 16);			\
	    *dest++ = (char) (aux >> 8);			\
	    *dest++ = (char) (aux);				\
	}
#ifdef OLD_FORMAT
#define Store_Int(word) \
	if ((unsigned long)(word) < 0xff) *dest++ = (char) (word);	\
	else {							\
	    *dest++ = (char) 0xff;					\
	    Store_Int32(word);					\
	}
#else
#define Store_Int(w) { \
	word aux = (word) (w); \
	if (-64 <= aux && aux <= 63) { \
	    *dest++ = aux & 0x7f; \
	} else { \
	    uword rev = 0; \
	    int k = 0; \
	    do { \
		rev = (rev << 7) | (aux & 0x7f); \
		aux >>= 7; \
		++k; \
	    } while (!(-64 <= aux && aux <= 63)); \
	    *dest++ = 0x80 | (aux & 0x7f); \
	    while (--k) { \
		*dest++ = (rev & 0x7f) | 0x80; \
		rev >>= 7; \
	    } \
	    *dest++ = rev; \
	} \
}
#endif

#ifdef OLD_FORMAT
#define Store_String(length, string) {		\
	register char *source = (string);	\
	register long ctr = (length);		\
	while (ctr-- >= 0) *dest++ = *source++;	\
}
#else
#define Store_String(length, string) {		\
	register char *source = (string);	\
	register long ctr = (length);		\
	while (ctr-- > 0) *dest++ = *source++;	\
}
#endif
#define Align() while ((long) dest % sizeof(pword)) *dest++ = (char) 0;

#define LoadByte	*buf++
#define Load_Byte(n)	(n) = LoadByte
#define Load_Int32(n) {				\
	(n) = LoadByte;				\
	(n) = ((n) << 8) | ((LoadByte) & 0xff);	\
	(n) = ((n) << 8) | ((LoadByte) & 0xff);	\
	(n) = ((n) << 8) | ((LoadByte) & 0xff);	\
}
#define BITS_PER_WORD (8*SIZEOF_LONG)
#ifdef OLD_FORMAT
#define Load_Int(n)				\
	{ if (((n) = (unsigned char)(LoadByte)) == 0xff) Load_Int32(n); }
#else
#define Load_Int(n) { /* n must be of type (signed) word */ \
	word i = LoadByte; \
	int shift = BITS_PER_WORD-7; \
	n = i & 0x7f; \
	while (i & 0x80) { \
	    i = LoadByte; \
	    n = ((n) << 7) | (i & 0x7f); \
	    shift -= 7; \
	} \
	if (shift > 0) \
	    n = (n << shift) >> shift; /* sign extend */ \
}
#endif

/* Write an EXDR Nat */
#define Store_Nat(n) 					\
	if ((n) == (word)(char)(n)) {			\
	    *dest++ = (char)((n) | 0x80);		\
	} else {					\
	    Store_Int32((n));				\
	}

/* Combined macro for Get and Load of a Nat
 * The macro is combined since it must be responsible
 * for the loading of either a single byte or a 4 byte
 * integer/
 */
#define GetLoad_Nat(n) 					\
	Get_Next(1);					\
	(n) = LoadByte;					\
	if (n & 0x80) {					\
	    n = n & 0x7f;				\
	} else {					\
	    Get_Next(3);				\
	    (n) = ((n) << 8) | ((LoadByte) & 0xff);	\
	    (n) = ((n) << 8) | ((LoadByte) & 0xff);	\
	    (n) = ((n) << 8) | ((LoadByte) & 0xff);	\
	}

#define WordOffset(pw, offset)	((pword*)((uword*)(pw) + (offset)))
#define Words(pwords)	((sizeof(pword)/sizeof(uword))*(pwords))


typedef union {
	double	as_dbl;
	struct ieee_parts {
#ifdef WORDS_BIGENDIAN 
		uint32 mant1;
		uint32 mant0;
#else
		uint32 mant0;
		uint32 mant1;
#endif
	} as_struct;
} ieee_double;

/* dest is assumeed to equal buf on entry
 * res is set as the result of operations performed by the macro 
 * perr is set for non-fatal errors - a valid EXDR term is written
 */
#define Write_String_Or_Ref(nst, strhm, sval)				\
    {									\
	pword id;							\
	if (strhm) {							\
	    Make_Integer(&id, strhm->nentries);				\
	    res = store_get_else_set(strhm, sval, tstrg, &id);		\
	    if (res < PSUCCEED) {					\
		*perr = res;						\
		res = PFAIL; /* Write the 'S'tring form instead */	\
	    }								\
	} else {							\
	    res = PFAIL;						\
	}								\
	if (res == PSUCCEED) {						\
	    Store_Byte('R');						\
	    Store_Nat(id.val.nint);					\
	    res = ec_outf(nst, buf, dest - buf);			\
	} else {							\
	    Store_Byte('S');						\
	    Store_Nat(StringLength(sval));				\
	    if ((res = ec_outf(nst, buf, dest - buf)) == PSUCCEED) {	\
		res = ec_outf(nst, StringStart(sval), StringLength(sval)); \
	    }								\
	}								\
    }

/*
 * pword * term_to_dbformat(term)
 *
 * Convert a general term into external format. This is created on the global
 * stack in form of a Sepia string. The return value is a pointer to this
 * string. For the reverse conversion, only the string contents is needed,
 * not its header! The sharing of variables and suspensions is preserved.
 */

pword *
term_to_dbformat(pword *parg, dident mod)
{
    pword **save_tt = TT;
    register long arity = 1, len;
    register long curr_offset = 0, top_offset = 2;	/* in 'word's */
    register pword *queue_tail = (pword *) 0;
    pword *queue_head = (pword *) 0;
    register pword *pw;
    register char *dest, *stop;
    pword *header;
    temp_area	meta_attr;
    int		flag = 0;

    Temp_Create(meta_attr, 4 * ATTR_IO_TERM_SIZE * sizeof(pword));
    header = TG;
    dest = (char *) (header + 1) + 4;	/* space for the TBUFFER pword and for
					 * the external format header	*/

    for(;;)	/* handle <arity> consecutive pwords, starting at <parg> */
    {
	do	/* handle the pword pointed to by parg */
	{
	    pw = parg;

	    /* I need here a slightly modified version of Dereference_(pw)
	     * that stops also at MARKed words. Not very nice, I know.
	     */
	    while (IsRef(pw->tag) && !(pw->tag.kernel & MARK) && !IsSelfRef(pw))
		pw = pw->val.ptr;

	    Reserve_Space(6);

	    if (pw->tag.kernel & MARK)
	    {
		if (SameTypeC(pw->tag,TDE))		/* a suspension */
		{
		    Store_Byte(Tag(pw->tag.kernel));
		    Store_Int32((pw[SUSP_FLAGS].tag.kernel & ~MARK));
		    if (SuspDead(pw)) {
			curr_offset += Words(SUSP_HEADER_SIZE-1);
			parg += SUSP_HEADER_SIZE-1;
			arity -= SUSP_HEADER_SIZE-1;
		    } else {
			Store_Byte(SuspPrio(pw));
			curr_offset += Words(SUSP_GOAL-1);
			parg += SUSP_GOAL-1;
			arity -= SUSP_GOAL-1;
		    }
		}
		else if (pw->val.nint == curr_offset)	/* a nonstd variable */
		{
		    Store_Byte(Tag(pw->tag.kernel));
		    Store_Int(pw->val.nint);
		    if (!IsNamed(pw->tag.kernel))
		    {
			Store_Byte(0);
		    }
		    else		/* store its name */
		    {
			dident vdid = TagDid(pw->tag.kernel);
			len = DidLength(vdid);
			Store_Int(len);
			Reserve_Space(len);
			Store_String(len, DidName(vdid));
		    }
		}
		else	/* just a reference to an already encountered variable */
		{
		    Store_Byte(Tag(TVAR_TAG));
		    Store_Int(pw->val.nint);
		}
	    }
	    else switch (TagType(pw->tag))
	    {
	    case TINT:
#if SIZEOF_CHAR_P > 4
		if (pw->val.nint < -2147483648 || 2147483648 <= pw->val.nint)
		{
		    /* store as a bignum (to be readable on 32bit machines) */
		    len = tag_desc[pw->tag.kernel].string_size(pw->val, pw->tag, 1);
		    Store_Byte(TBIG);
		    Store_Int(len);
		    Reserve_Space(len+1);
		    stop = dest+len;
		    dest += tag_desc[pw->tag.kernel].to_string(pw->val, pw->tag,
			dest, 1);
		    while (dest <= stop)	/* pad and terminate */
		    	*dest++ = 0;
		    break;
		}
#endif
		Store_Byte(TINT);
#ifdef OLD_FORMAT
		Store_Int32(pw->val.nint);
#else
		Store_Int(pw->val.nint);
#endif
		break;

	    case TNIL:
		Store_Byte(Tag(pw->tag.kernel));
		break;

	    case TDICT:
		len = DidLength(pw->val.did);
		Store_Byte(TDICT);
		Store_Int(DidArity(pw->val.did));
		Store_Int(len);
		Reserve_Space(len);
		Store_String(len, DidName(pw->val.did));
		break;

	    case TDBL:
	    {
		ieee_double d;
		d.as_dbl = Dbl(pw->val);
		Store_Byte(TDBL);
		Store_Byte(sizeof(double)-1);	/* backward compat */
		Reserve_Space(sizeof(double));
		Store_Int32(d.as_struct.mant1);
		Store_Int32(d.as_struct.mant0);
		break;
	    }

	    case TIVL:
	    {
		ieee_double dlwb, dupb;
		dlwb.as_dbl = IvlLwb(pw->val.ptr);
		dupb.as_dbl = IvlUpb(pw->val.ptr);
		Store_Byte(TIVL);
		Reserve_Space(2*sizeof(double));
		Store_Int32(dlwb.as_struct.mant1);
		Store_Int32(dlwb.as_struct.mant0);
		Store_Int32(dupb.as_struct.mant1);
		Store_Int32(dupb.as_struct.mant0);
		break;
	    }

	    case TSTRG:
		len = StringLength(pw->val);
		Store_Byte(TSTRG);
		Store_Int(len);
		Reserve_Space(len);
		Store_String(len, StringStart(pw->val));
		break;

	    case TVAR_TAG:	/* standard variable */
		Store_Byte(Tag(TVAR_TAG));
		Store_Int(curr_offset);
		Trail_(pw);
		pw->val.nint = curr_offset;
		pw->tag.kernel |= MARK;
		break;

	    case TNAME:
	    case TUNIV:
		Store_Byte(Tag(TVAR_TAG));
		Store_Int(top_offset);
		Trail_Tag(pw);
		pw->val.nint = top_offset;
		pw->tag.kernel |= MARK;
		top_offset += 2;
		EnQueue_(pw, 1, 0);
		break;

	    case TMETA:
		Store_Byte(Tag(TVAR_TAG));
		Store_Int(top_offset);
		Trail_Tag(pw);
		pw->val.nint = top_offset;
		pw->tag.kernel |= MARK;
		top_offset += 4;
		EnQueue_(pw, 2, QUEUE_MASK_META);
		break;

	    case TSUSP:
		Store_Byte(Tag(TSUSP));
		pw = pw->val.ptr;
		if (pw->tag.kernel & MARK)	/* not the first encounter */
		{
		    Store_Int(pw->val.nint);
		}
		else
		{
		    Store_Int(top_offset);
		    Trail_Pword(pw);
		    pw->tag.kernel |= MARK;
		    pw->val.nint = top_offset;
		    if (SuspDead(pw))
		    {
			top_offset += Words(SUSP_HEADER_SIZE);	/* for TDE */
			EnQueue_(pw, SUSP_HEADER_SIZE, 0);
		    }
		    else
		    {
			top_offset += Words(SUSP_SIZE);	/* for TDE */
			EnQueue_(pw, SUSP_SIZE, 0);
		    }
		}
		break;

	    case TLIST:
		Store_Byte(Tag(TLIST));
		Store_Int(top_offset);
		top_offset += 4;
		EnQueue_(pw->val.ptr, 2, 0);
		break;

	    case TCOMP:
		Store_Byte(Tag(TCOMP));
		Store_Int(top_offset);
		if (flag) {
		    pword pw_out;
		    (void) transf_meta_out(pw->val, pw->tag,
			    (pword *) TempAlloc(meta_attr, ATTR_IO_TERM_SIZE * sizeof(pword)),
			    D_UNKNOWN, &pw_out);
		    pw = pw_out.val.ptr;
		    len = 1 + DidArity(pw->val.did);
		    EnQueue_(pw, len, 0);
		} else {
		    len = 1 + DidArity(pw->val.ptr->val.did);
		    EnQueue_(pw->val.ptr, len, 0);
		}
		top_offset += 2*len;
		break;

	    default:
		if (TagType(pw->tag) >= 0 && TagType(pw->tag) <= NTYPES)
		{
		    len = tag_desc[TagType(pw->tag)].string_size(pw->val, pw->tag, 1);
		    Store_Byte(Tag(pw->tag.kernel));
		    Store_Int(len);
		    Reserve_Space(len+1);
		    stop = dest+len;
		    dest += tag_desc[TagType(pw->tag)].to_string(pw->val, pw->tag,
			dest, 1);
		    while (dest <= stop)	/* pad and terminate */
		    	*dest++ = 0;
		}
		else
		{
		    p_fprintf(current_err_,
			"bad type in term_to_dbformat: 0x%x\n",
			pw->tag.kernel);
		}
		break;
	    }
	    curr_offset += Words(1);
	    ++parg;
	} while (--arity);
	if (EmptyQueue())
	    break;
	DeQueue_(parg, arity, flag);
    }
					/* # bytes of external representation */
    Store_Byte(0);			/* add a terminating 0		*/
    Set_Buffer_Size(header, dest - (char*) header - sizeof(pword));
    header->tag.kernel = TBUFFER;
    Align();				/* align the global stack pointer */
    TG = (pword *) dest;
    dest = (char *) (header + 1);	/* fill in the external format header */
    Store_Int32(top_offset);		/* (size of term after restoring) */
    Untrail_Variables(save_tt);
    Temp_Destroy(meta_attr);
    return header;
}

/*
 * pword *dbformat_to_term(buf)
 *
 * Decode a term in database format (in the buffer pointed to by buf),
 * construct it on the global stack and return its address.
 * Return NULL if there is no space to construct the term.
 */

pword *
dbformat_to_term(register char *buf, dident mod, type tmod)
{
    register pword *pw;
    pword	*p;
    pword *base, *top;
    pword	*r;
    pword	meta;
    word	n, t;
    int		res;

    meta.tag.kernel = TNIL;
    Load_Int32(n);
    base = pw = TG;
    TG = WordOffset(TG, n);
    if (GlobalStackOverflow)
    	return (pword *)0;
    top = TG;

    while (pw < top)
    {
	Load_Byte(t);
	switch (TagTypeC(t))
	{
	case TINT:	/* value */
#ifdef OLD_FORMAT
	    Load_Int32(n);
#else
	    Load_Int(n);
#endif
	    pw->val.nint = n;
	    pw++->tag.kernel = t;
	    break;

	case TNIL:	/* */
	    pw++->tag.kernel = t;
	    break;

	case TVAR_TAG:	/* offset */
	    Load_Int(n);
	    pw->val.ptr = WordOffset(base, n);
	    pw++->tag.kernel = TREF;
	    break;

	case TUNIV:	/* offset, length, "string\0" */
	case TNAME:
	case TMETA:
	    Load_Int(n);
	    pw->val.ptr = WordOffset(base, n);
	    Load_Int(n);
	    if (n)
	    {
		pw++->tag.kernel = DidTag(t, enter_dict_n(buf, n, 0));
#ifdef OLD_FORMAT
		buf += n + 1;
#else
		buf += n;
#endif
	    }
	    else
		pw++->tag.kernel = RefTag(t);	/* no name */
	    if (TagTypeC(t) == TMETA) {
		p = TG;
		TG += 2;
		Check_Gc
		p[0].val.ptr = pw;
		p[0].tag.kernel = TREF;
		p[1] = meta;
		meta.val.ptr = p;
		meta.tag.kernel = TLIST;
	    }
	    break;

	case TSUSP:
	case TCOMP:
	case TLIST:
	    Load_Int(n);
	    pw->val.ptr = WordOffset(base, n);
	    pw++->tag.kernel = t;
	    break;

	case TDICT:	/* arity, length, "string\0" */
	    Load_Int(n);
	    Load_Int(t);
	    pw->val.did = enter_dict_n(buf, t, (int) n);
	    pw++->tag.kernel = TDICT;
#ifdef OLD_FORMAT
	    buf += t + 1;
#else
	    buf += t;
#endif
	    break;

	case TDBL:	/* length, double */
	    {
		ieee_double d;
		Load_Byte(n);	/* backward compatibility */
		Load_Int32(d.as_struct.mant1);
		Load_Int32(d.as_struct.mant0);
		Make_Double(pw, d.as_dbl);
		pw++;
	    }
	    break;

	case TIVL:	/* double, double */
	    {
		ieee_double dlwb, dupb;
		Load_Int32(dlwb.as_struct.mant1);
		Load_Int32(dlwb.as_struct.mant0);
		Load_Int32(dupb.as_struct.mant1);
		Load_Int32(dupb.as_struct.mant0);
		Push_Interval(pw->val.ptr, dlwb.as_dbl, dupb.as_dbl);
		pw++->tag.kernel = TIVL;
	    }
	    break;

	case TSTRG:	/* length, "string" */
	    {
		register char *string;
		Load_Int(n);
		Make_Stack_String(n, pw->val, string);
		pw++->tag.kernel = TSTRG;
#ifdef OLD_FORMAT
		while (n-- >= 0) *string++ = *buf++;
#else
		while (n-- > 0) *string++ = *buf++;
		*string = 0;
#endif
	    }
	    break;

	case TDE:
	    pw[SUSP_LD].val.ptr = LD;
	    Update_LD(pw)
	    Load_Int32(n);
	    pw[SUSP_FLAGS].tag.kernel = n;
	    pw[SUSP_PRI].val.ptr = (pword *) 0;		/* missing */
	    pw[SUSP_INVOC].tag.kernel = 0L;
	    if (!SuspDead(pw)) {
		Load_Byte(n);
		Init_Susp_State(pw, n);
		pw += SUSP_GOAL;
	    } else {
		pw += SUSP_HEADER_SIZE;
	    }
	    break;

	default:
	    if (t >= 0 && t <= NTYPES)
	    {
		Load_Int(n);
		pw->tag.kernel = t;	/* from_string() may change tag! */
		if (tag_desc[t].from_string(buf, pw, 10) != PSUCCEED)
		{
		    /* this can happen e.g. if we try to read a bignum
		     * in an Eclipse that doesn't support them */
		    Make_Nil(pw);
		    p_fprintf(current_err_,
			"dbformat_to_term: cannot represent constant of type %s\n",
			DidName(tag_desc[t].tag_name));
		}
		++pw;
		buf += n+1;
	    }
	    else
	    {
		Make_Nil(pw);
		p_fprintf(current_err_,
			"bad type in dbformat_to_term: 0x%x\n", t);
		pw++; buf++;
	    }
	    break;
	}
    }
    p = &meta;
    while (IsList(p->tag)) {
	p = p->val.ptr;
	pw = (p++)->val.ptr;
	r = transf_meta_in(pw, mod, &res);
	if (!r) {
	    p_fprintf(current_err_,
		    "unknown attribute in dbformat_to_term: ");
	    (void) ec_pwrite(0, 2, current_err_, pw->val, pw->tag, 1200, 0,
		    mod, tdict);
	    (void) ec_newline(current_err_);
	    return (pword *) 0;
	}
	pw->val.ptr = r;
    }
    res = _fill_procedures(LD, mod, tmod);
    return (res == PSUCCEED) ? base : 0;
}

/*
 * Fill in pri's in the newly read suspensions
 */
static int
_fill_procedures(register pword *env, dident mod, type tmod)
{
    register pword	*p;
    dident		pd;
    dident		module_ref;
    pri			*proc;

    while (env > (pword *)0) 
    {
	if (!(SuspDead(env))) 
	{
	    proc = DelayProc(env);
	    if (!proc) {
		p = env + SUSP_GOAL;
		Dereference_(p);
		pd = p->val.ptr->val.did;
		p = env + SUSP_MODULE;
		Dereference_(p);
		module_ref = p->val.did;
		/* Create the module if it did not exist */
		if (!IsModule(module_ref))
		    (void) ec_create_module(module_ref);
		proc = visible_procedure(pd, module_ref,
		    (module_ref == mod) ? tmod : tdict, PRI_CREATE|PRI_REFER);
		if (!proc) {
		    int err;
		    Get_Bip_Error(err);
		    p_fprintf(current_err_,
			    "locked module in dbformat_to_term: %s\n",
			    DidName(module_ref));
		    return err;
		}
		env[SUSP_PRI].val.wptr = (uword *) proc;
	    }
	}
	env = DelayPrevious(env);
    }
    return PSUCCEED;
}

static int
p_term_to_bytes(value v, type t, value vs, type ts, value vm, type tm)
{
    pword pw, *result;
    Check_Output_String(ts);
    Check_Atom(tm);
    pw.val.all = v.all;
    pw.tag.all = t.all;
    result = term_to_dbformat(&pw, vm.did);
    Return_Unify_String(vs, ts, result);
}

static int
p_bytes_to_term(value vs, type ts, value v, type t, value vmod, type tmod)
{
    pword *result;

    Check_Atom(tmod);
    Check_String(ts);
    result = dbformat_to_term(StringStart(vs), vmod.did, tmod);
    if (!result)
    {
	value va;
	va.did = d_.abort;
	Bip_Throw(va, tdict);
    }
    Return_Unify_Pw(v, t, result->val, result->tag);
}


/*---------------------------------------------------------------------------
 * Serialisation of ground terms for communication with other languages
 *
 * ExdrTerm	::=	'V' Version 'C'? Term
 * Term		::=	(Integer|Double|String|List|Nil|Struct|Variable)
 * Integer	::=	('B' <byte> | 'I' XDR_int | 'J' XDR_long)
 * Double	::=	'D' XDR_double
 * String	::=	('S' Length <byte>* | 'R' Index)
 * List		::=	'[' Term (List|Nil)
 * Nil		::=	']'
 * Struct	::=	'F' Arity String Term*
 * Variable	::=	'_'
 * Length	::=	XDR_nat
 * Index	::=	XDR_nat
 * Arity	::=	XDR_nat
 * Version	::=	<byte>
 * XDR_int	::=	<4 bytes, msb first>
 * XDR_long	::=	<8 bytes, msb first>
 * XDR_double	::=	<8 bytes, ieee double, exponent first>
 * XDR_nat	::=	<8 bits: 1 + seven bits unsigned value>
 *			| XDR_int			// >= 0
 *
 * NOTE: Eclipse integers are wordsized (TINT) or bignums (TBIG). 
 * Values between 2^31..2^63-1 and -2^63+1..-2^31 can be TINT or TBIG,
 * depending on machine's wordsize.
 * On the other hand, EXDR 'I' format is always 32 bits and 'J' 64 bits.
 * As an additional complication, TINT and EXDR I,J are two's complement
 * representations, but TBIGs are sign/magnitude.
 * The code must therefore deal with
 *	TINT <--> I
 *	TINT <--> J
 *	TBIG (one limb) <--> J
 *	TBIG (two limbs) <--> J
 *---------------------------------------------------------------------------*/

/*
 * write_exdr/2 fails if the term cannot be represented in EXDR format.
 * The execute_rpc/1 predicate in kernel.pl relies on that.
 * Note also that we are careful to always write a complete EXDR term,
 * even when we fail. This is to avoid the recipient of the term crashing.
 */

#define EXDR_VERSION	2

#define Negate_32_32(_lo, _hi) \
	_lo = -(_lo); \
	_hi = _lo ? ~(_hi) : -(_hi);


static int
_write_exdr(stream_id nst, pword *pw, t_heap_htable *strhm, int *perr)
{
    int		arity, res;
    pword	*arg;
    value	val;
    char	buf[10];
    char	*dest;
    ieee_double	d;

    for(;;)
    {
	Dereference_(pw);
	if (IsRef(pw->tag))
	{
	    return ec_outfc(nst, '_');
	}
	switch (TagType(pw->tag))
	{
	case TDICT:			/* like atom/0 structure */
	    dest = buf;
	    Store_Byte('F');
	    Store_Nat(0);
	    val.ptr = DidString(pw->val.did);
	    Write_String_Or_Ref(nst, strhm, val);
	    return res;

	case TCOMP:
	    dest = buf;
	    arity = DidArity(pw->val.ptr->val.did); 
	    arg = pw->val.ptr;
	    Store_Byte('F');
	    Store_Nat(arity);
	    val.ptr = DidString(arg->val.did);
	    Write_String_Or_Ref(nst, strhm, val);
	    if (res != PSUCCEED) return res;
	    ++arg;
	    break;

	case TLIST:
	    for (;;)
	    {
		if ((res = ec_outfc(nst, '[')) != PSUCCEED) return res;
		pw = pw->val.ptr;		/* write car */
		if ((res = _write_exdr(nst, pw, strhm, perr)) != PSUCCEED) return res;
		++pw;
		Dereference_(pw);		/* check cdr */
		if (IsNil(pw->tag))		/* proper end */
		{
		    return ec_outfc(nst, ']');
		}
		else if (!IsList(pw->tag))	/* improper list, truncate */
		{
		    *perr = PFAIL;
		    return ec_outfc(nst, ']');
		}
	    }

	case TNIL:
	    return ec_outfc(nst, ']');

	case TINT:
	    dest = buf;
	    if (pw->val.nint == (word)(char)pw->val.nint) /* use 'B' format */
	    {
		Store_Byte('B');
		Store_Byte(pw->val.nint);
		return ec_outf(nst, buf, 2);
	    }
#if (SIZEOF_LONG > 4)
	    if ((int32) pw->val.nint != pw->val.nint)	/* need 'J' format */
	    {
		int32 lo, hi;
		Store_Byte('J');
		lo = (int32) pw->val.nint;
		hi = (int32) (pw->val.nint >> 32);
		Store_Int32(hi);
		Store_Int32(lo);
		return ec_outf(nst, buf, 9);
	    }
#endif
	    Store_Byte('I');
	    Store_Int32(pw->val.nint);
	    return ec_outf(nst, buf, 5);

#if SIZEOF_LONG <= 4
	case TBIG:
	{
	    int32 *limbs = (int32*) BufferStart(pw->val.ptr);
	    int32 lo, hi;
	    if (BufferSize(pw->val.ptr) > 8)
	    {
		*perr = PFAIL;
		return ec_outfc(nst, '_');
	    }
	    lo = limbs[0];
	    hi = BufferSize(pw->val.ptr) > 4 ? limbs[1] : 0L;
	    if (BigNegative(pw->val.ptr))
	    {
		Negate_32_32(lo, hi);
		if (hi >= 0)
		{
		    *perr = PFAIL;
		    return ec_outfc(nst, '_');
		}
	    }
	    else
	    {
		if (hi < 0)
		{
		    *perr = PFAIL;
		    return ec_outfc(nst, '_');
		}
	    }
	    dest = buf;
	    Store_Byte('J');
	    Store_Int32(hi);
	    Store_Int32(lo);
	    return ec_outf(nst, buf, 9);
	}
#endif

	case TSTRG:
	    dest = buf;
	    Write_String_Or_Ref(nst, strhm, pw->val);
	    return res;

	case TDBL:
	    dest = buf;
	    d.as_dbl = Dbl(pw->val);
	    Store_Byte('D');
	    Store_Int32(d.as_struct.mant1);
	    Store_Int32(d.as_struct.mant0);
	    return ec_outf(nst, buf, 9);

	default:
	    *perr = PFAIL;
	    return ec_outfc(nst, '_');
	}
	for (; arity > 1; arity--,arg++)
	{
	    if ((res = _write_exdr(nst, arg, strhm, perr)) != PSUCCEED)
	    	return res;
	}
	pw = arg;		/* tail recursion optimised */
    }
}


int p_write_exdr(value vs, type ts, value v, type t)
{
    int res, err;
    pword vt;
    char buf[2];
    char *dest = buf;
    t_heap_htable *strhm = NULL;

    stream_id nst = get_stream_id(vs, ts, SWRITE, &res);
    if (nst == NO_STREAM)
    	return res;
    if (!IsWriteStream(nst))
	return STREAM_MODE;
    Store_Byte('V');
    Store_Byte(EXDR_VERSION);
    if ((res = ec_outf(nst, buf, 2)) != PSUCCEED)
    	return res;
    if (StreamMode(nst) & SCOMPRESS)
    {
	if ((res = ec_outfc(nst, 'C')) != PSUCCEED)
	    return res;
	strhm = htable_new(HTABLE_INTERNAL);
    }
    vt.val.all = v.all;
    vt.tag.all = t.all;
    err = PSUCCEED;
    res = _write_exdr(nst, &vt, strhm, &err);
    if (strhm)
	htable_free(strhm);
    if (res != PSUCCEED)
    	return res;		/* fatal error, exdr incomplete */
    if (err != PSUCCEED)
    	return err;		/* non-fatal, exdr sane but wrong */
    Succeed_;
}


#define Get_Next(n) {					\
    buf = (char *) StreamPtr(nst);			\
    if (StreamBuf(nst) + StreamCnt(nst) >= (unsigned char*) (buf + n))	\
	StreamPtr(nst) = (unsigned char*) (buf + n);	\
    else {						\
	long _l;					\
    	buf = ec_getstring(nst, n, &_l);		\
	if (_l < n) buf = 0;				\
    }							\
}

static int
_read_exdr(stream_id nst, t_heap_htable *strhm, pword *pw)
{
    long arity, len;
    char *buf;
    ieee_double d;
    pword *arg, key, valpw;
    int res;
    dident functor;

    for (;;)
    {
	Get_Next(1);
	switch(*buf)
	{
	case '_':
	    Make_Var(pw);
	    return PSUCCEED;

	case 'B':
	    Get_Next(1);
	    Load_Byte(len);
	    Make_Integer(pw, len);
	    return PSUCCEED;

	case 'I':
	    Get_Next(4);
	    Load_Int32(len);
	    Make_Integer(pw, len);
	    return PSUCCEED;

	case 'J':
	{
	    int32 hi, lo;
	    Get_Next(8);
	    Load_Int32(hi);
	    Load_Int32(lo);
#if (SIZEOF_LONG >= 8)
	    Make_Integer(pw, ((long) hi << 32) + (uint32) lo);
#else
	    arg = TG;
	    Push_Buffer(8);
	    if (hi < 0)		/* convert to sign/magnitude */
	    {
		Negate_32_32(lo, hi);
	    	arg->tag.kernel |= BIGSIGN;
	    }
	    ((int32 *) BufferStart(arg))[0] = lo;
	    if (hi)		/* need two limbs */
	    {
		((int32 *) BufferStart(arg))[1] = hi;
	    }
	    else		/* need only one limb */
	    {
		Trim_Buffer(arg, 4);
	    }
	    pw->tag.kernel = TBIG;
	    pw->val.ptr = arg;
#endif
	    return PSUCCEED;
	}

	case 'D':
	    Get_Next(8);
	    Load_Int32(d.as_struct.mant1);
	    Load_Int32(d.as_struct.mant0);
	    Make_Float(pw, d.as_dbl);
	    return PSUCCEED;

	case ']':
	    Make_Nil(pw);
	    return PSUCCEED;

	case 'R':
	    if (!strhm) return BAD_FORMAT_STRING;
	    GetLoad_Nat(len);
	    Make_Integer(&key, len);
	    res = store_get(strhm, key.val, key.tag, pw);
            if (res != PSUCCEED) return res;
	    /* What is retrieved from the store may be a string,
	     * or a dictionary entry!
	     */
	    if (!IsString(pw->tag)) {
		pw->val.ptr = DidString(pw->val.did);
		pw->tag.kernel = TSTRG;
	    }
	    return PSUCCEED;

	case 'S':
	    GetLoad_Nat(len);
	    Get_Next(len);
	    pw->tag.kernel = TSTRG;
	    pw->val.ptr = TG;
	    Push_Buffer(len+1);
	    Copy_Bytes(StringStart(pw->val), buf, len);
	    StringStart(pw->val)[len] = 0;
	    if (strhm) {
		Make_Integer(&key, strhm->nentries);
		return store_set(strhm, key.val, key.tag, pw);
	    }
	    return PSUCCEED;

	case 'F':
	    GetLoad_Nat(arity);
	    Get_Next(1);
	    if (arity < 0 ) return BAD_FORMAT_STRING;
	    Load_Byte(len);
	    if ( len == 'S') {
		GetLoad_Nat(len);
		Get_Next(len);
		functor = enter_dict_n(buf, len, arity);
		if (strhm) {
		    Make_Integer(&key, strhm->nentries);
		    Make_Atom(&valpw, functor);
		    res = store_set(strhm, key.val, key.tag, &valpw);
		    if (res != PSUCCEED) return res;
		}
	    } else if (len == 'R') {
		if (!strhm) return BAD_FORMAT_STRING;
		GetLoad_Nat(len);
		Make_Integer(&key, len);
		res = store_get(strhm, key.val, key.tag, &valpw);
		if (res != PSUCCEED) return res;
		/* What is retrieved from the store may be a string,
		 * or a dictionary entry with correct/incorrect arity.
		 */
		if (IsString(valpw.tag)) {
		    functor = enter_dict_n(StringStart(valpw.val), 
						StringLength(valpw.val), arity);
		} else if (DidArity(valpw.val.did) == arity) {
		    functor = valpw.val.did;
		} else {
		    functor = add_dict(valpw.val.did, arity);
		}
            } else return BAD_FORMAT_STRING;
	    if (arity == 0) {
		if (functor == d_.nil) {
		    Make_Nil(pw);
		} else {
		    Make_Atom(pw, functor);
		}
		return PSUCCEED;
	    }
	    arg = TG;
	    if (functor == d_.list) {
		Make_List(pw, arg);
		Push_List_Frame();
	    } else {
		Make_Struct(pw, arg);
		Push_Struct_Frame(functor);
		++arg;
	    }
	    break;

	case '[':
	    arity = 2;
	    arg = TG;
	    Make_List(pw, arg);
	    Push_List_Frame();
	    break;

	default:
	    return BAD_FORMAT_STRING;
	}
	for (; arity > 1; arity--,arg++)
	{
	    if ((res = _read_exdr(nst, strhm, arg)) != PSUCCEED)
	    	return res;
	}
	pw = arg;		/* tail recursion optimised */
    }
}

int p_read_exdr(value vs, type ts, value v, type t)
{
    char *buf;
    pword vt;
    int res;
    t_heap_htable *strhm = NULL;

    stream_id nst = get_stream_id(vs, ts, SREAD, &res);
    if (nst == NO_STREAM)
    	return res;
    if (nst == null_)
	return PEOF;
    if (!(IsReadStream(nst)))
	return STREAM_MODE;
    Get_Next(3);
    if (!buf)
    	return PEOF;
    if (*buf++ != 'V')
    	return NOT_DUMP_FILE;
    if (*buf++ > EXDR_VERSION)
    	return BAD_DUMP_VERSION;
    if (*buf == 'C')		/* is it compressed exdr format? */
    {
	strhm = htable_new(HTABLE_INTERNAL);
    }
    else
    {
	res = ec_ungetch(nst);
	if (res != PSUCCEED) return res;
    }
    res = _read_exdr(nst, strhm, &vt);
    if (strhm)
	htable_free(strhm);
    if (res != PSUCCEED) {
    	return res;
    }
    if (!(IsRef(vt.tag) && vt.val.ptr == &vt))
    {
	Return_Unify_Pw(v, t, vt.val, vt.tag);
    }
    Succeed_
}

/*
 * Routines to convert from to simple types and xdr format
 * used by VB interface since it VB has no bit manipulation stuff
 */
void Winapi
ec_double_xdr(double *d, char *dest)
{
	ieee_double id;

	id.as_dbl = *d;
	Store_Int32(id.as_struct.mant1);
	Store_Int32(id.as_struct.mant0);
}

void Winapi
ec_xdr_double(char *buf, double *d)
{
	ieee_double id;
	
	Load_Int32(id.as_struct.mant1);
	Load_Int32(id.as_struct.mant0);
	*d = id.as_dbl;
}
void Winapi
ec_int32_xdr(int32 *l, char *dest)
{
	Store_Int32(*l);
}

void Winapi
ec_xdr_int32(char *buf, int32 *l)
{
	Load_Int32(*l);
}
/*---------------------------------------------------------------------------
 * Init
 *---------------------------------------------------------------------------*/

void
bip_prop_init(int flags)
{
    if (!(flags & INIT_SHARED))
	return;
    (void) built_in(in_dict("write_exdr", 2),
				p_write_exdr,	B_SAFE);
    (void) built_in(in_dict("read_exdr", 2),
				p_read_exdr,	B_UNSAFE|U_FRESH);
    (void) exported_built_in(in_dict("term_to_bytes_", 3),
				p_term_to_bytes,	B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("bytes_to_term_", 3),
				p_bytes_to_term,	B_UNSAFE|U_FRESH);
    (void) exported_built_in(in_dict("term_size", 2),
				p_term_size,		B_UNSAFE|U_SIMPLE);
    exported_built_in(in_dict("copy_term_vars", 4),
				p_copy_term_vars,	B_UNSAFE);
    exported_built_in(in_dict("copy_simple_term", 2),
				p_copy_simple_term,	B_UNSAFE|U_FRESH)
		->mode  = BoundArg(2, NONVAR);	/* it *could* be a nonvar */
    exported_built_in(in_dict("copy_term", 3),
				p_copy_term3,		B_UNSAFE|U_UNIFY)
		->mode  = BoundArg(2, NONVAR)	/* it *could* be a nonvar */
			| BoundArg(3, NONVAR);
}
