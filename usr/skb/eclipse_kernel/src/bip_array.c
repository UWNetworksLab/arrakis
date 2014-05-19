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
 * VERSION	$Id: bip_array.c,v 1.1 2008/06/30 17:43:51 jschimpf Exp $
 */

/****************************************************************************
 *
 *	SEPIA Built-in Predicates for arrays and global variables
 *
 *	name		C func		type		file
 *	----------------------------------------------------------------
 *	make_array_	p_make_array_	B_SAFE
 *	setval_body	p_setval_body	B_SAFE
 *	getval_body	p_getval_body	B_UNSAFE
 *	incval_body	p_incval_body	B_SAFE
 *	decval_body	p_decval_body	B_SAFE
 *	array_info	p_array_info    B_UNSAFE
 *
 *****************************************************************************/


/*
 * Arrays are implemented as values of the property ARRAY_PROP.
 * The tag part holds a type (using the general type of prolog objects) 
 * and the value part holds relevant information for this type:
 * If the arity of the dictionary entry is greater than 0:
 * - TINT: integer array. The second word is a pointer to the array.
 * - TDBL: double float array. The second word is a pointer to the array.
 * - TSTRG: byte array. The second word is a pointer to the array.
 * - TCOMP: prolog array. The second word is a pointer to the array.
 * the header of an array looks as follows (in dident):
 * did    (backpointer and arity can be deduced (questionable approach))
 * dim1
 * ...
 * dimn
 * contents ...
 *
 * Global variables are implemented as the property GLOBVAR_PROP.
 * The property value is the value of the global variable.
 */



#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "emu_export.h"
#include "property.h"
#include "module.h"

#if defined(PRINTAM) || defined(LASTPP)

#include "opcode.h"

extern char	*inst_name[];
char		*vm_inst_flag_;
unsigned long	*vm_inst_ctr_;

#endif /* PRINTAM */

/* set and get ARRAY_PROP */
#define NewArrayItem(did, mod, mod_tag, vis, perr)\
    set_modular_property(did, ARRAY_PROP, mod, mod_tag, vis, perr)

#define NewGlobVarItem(did, mod, mod_tag, scope, perr)\
    set_modular_property(did, GLOBVAR_PROP, mod, mod_tag, scope, perr)

#define VisibleAV(did, prop, mod, mod_tag, perr)\
    get_modular_property(did, prop, mod, mod_tag, VISIBLE_PROP, perr)

#define EraseAV(did, prop, mod, mod_tag, vis)\
    erase_modular_property(did, prop, mod, mod_tag, vis)

int
    p_setval_body(value a, type ta, value v, type t, value vmod, type tmod),
    p_make_array_(value v, type t, value vt, type tt, value vscope, type tscope, value vmod, type tmod),
    p_erase_array_body(value val1, type tag1, value vmod, type tmod),
    p_erase_array_(value val1, type tag1, value vscope, type tscope, value vmod, type tmod);

static int
    p_xget(value vhandle, type thandle, value vi, type ti, value vval, type tval),
    p_xset(value vhandle, type thandle, value vi, type ti, value vval, type tval),
    p_array_info(value varr, type tarr, value vopt, type topt, value vmod, type tmod),
    p_getval_body(value a, type ta, value v, type t, value vmod, type tmod),
    p_incval_body(value a, type ta, value vmod, type tmod),
    p_decval_body(value a, type ta, value vmod, type tmod),
    p_test_and_setval_body(value a, type ta, value vc, type tc, value v, type t, value vmod, type tmod);

static dident	d_reference_;
static dident	d_reference1_;
static dident	d_global_reference_;
static dident	d_global_reference_index_;

pword	*get_array_header(dident adid),
	*get_kernel_array(dident adid),
	*get_visible_array_header(dident adid, value vm, type tm, int *res);

pword	*p_installation_dir_;	/* accessed from megalog! */


/*
 * For aligning arrays
 */

#define RoundUp(n) ((n) - ((n)-1)%sizeof(maxelsize) - 1 + sizeof(maxelsize))

typedef union {
    uword	w;
    long	l;
    double	d;
} maxelsize;



void
bip_array_init(int flags, char *installation_dir)
{
    pri		*pd;
    value	v1;

    if (flags & INIT_SHARED)
    {
	GlobalVarIndex = 0;
	local_built_in(in_dict("array_info", 3), p_array_info, B_UNSAFE)
	    -> mode = BoundArg(1, GROUND) | BoundArg(2, GROUND);
	(void) local_built_in(in_dict("make_array_", 4),
			      p_make_array_, B_SAFE);
	(void) exported_built_in(in_dict("erase_array_body", 2),
				 p_erase_array_body, B_SAFE);
	(void) local_built_in(in_dict("erase_array_", 3),
				 p_erase_array_, B_SAFE);
	pd = exported_built_in(in_dict("test_and_setval_body", 4), p_test_and_setval_body, B_SAFE);
	pd = exported_built_in(in_dict("setval_body", 3), p_setval_body, B_SAFE);
	pd = exported_built_in(in_dict("getval_body", 3), p_getval_body, B_UNSAFE|U_FRESH);
	pd -> mode = BoundArg(2, NONVAR);
	pd = exported_built_in(in_dict("incval_body",2), p_incval_body, B_UNSAFE);
	pd = exported_built_in(in_dict("decval_body",2), p_decval_body, B_UNSAFE);
	built_in(in_dict("xget",3), p_xget, B_UNSAFE)->mode = GROUND;
	built_in(in_dict("xset",3), p_xset, B_SAFE);
    }

    if (flags & INIT_PRIVATE)
    {
	value	vv, vm, vn, vt;

	d_reference_ = in_dict("reference", 0);
	d_reference1_ = in_dict("reference", 1);
	d_global_reference_ = in_dict("global_reference", 0);
	d_global_reference_index_ = in_dict("global_reference_index", 0);

#ifdef DFID
	/* Initialization of predefined global Prolog variables */
	vv.did = d_.local0;
	vm.did = d_.kernel_sepia;

	/* temporary: use old style globvar-index for DFID variables */
	vt.did = d_global_reference_index_;

	/* global var 0 - unused (used to be postponed list) */
	GlobalVarIndex++;
	vm.did = in_dict("dfid", 0);
	/* global var 1 - DfidDepth */
	vn.did = in_dict("depth", 0);
	(void) p_make_array_(vn, tdict, vt, tdict, vv, tdict, vm, tdict);
	/* global var 2 - MaxDepth */
	vn.did = in_dict("max_depth", 0);
	(void) p_make_array_(vn, tdict, vt, tdict, vv, tdict, vm, tdict);
	/* global var 3 - DepthLimit */
	vn.did = in_dict("depth_limit", 0);
	(void) p_make_array_(vn, tdict, vt, tdict, vv, tdict, vm, tdict);
	/* global var 4 - DepthOV */
	vn.did = in_dict("depth_ov", 0);
	(void) p_make_array_(vn, tdict, vt, tdict, vv, tdict, vm, tdict);
#endif
    }

    /*
     * Initialize some global Prolog variables in sepia_kernel
     * that need to be accessed from C as well.
     */
    v1.nint = 0;
    p_installation_dir_ = init_kernel_var(flags,
		in_dict("sepiadir", 0), v1, tint);
    if (flags & INIT_SHARED)
    {
	set_string(p_installation_dir_, installation_dir);
    }

#if defined(PRINTAM) || defined(LASTPP)
    if (flags & INIT_SHARED)
    {	/* some facilities for statistics and debugging */
	register int	i;
	pword		*pw;

	/* array of flags for every VM instruction	*/
	(void) make_kernel_array(in_dict("vm_inst_flag",1),
			NUMBER_OP, d_.byte, d_.global0);

	/* array of VM instruction counters	*/
	(void) make_kernel_array(in_dict("vm_inst_ctr",1),
			NUMBER_OP, d_.integer0, d_.global0);

	/* array of VM instruction names	*/
	(void) make_kernel_array(in_dict("vm_inst_name",1),
			NUMBER_OP, d_.prolog, d_.global0);

	pw = get_kernel_array(in_dict("vm_inst_name",1))->val.ptr + 1;
	for(i=0; i<NUMBER_OP; i++, pw++)
	{
	    pw->tag.kernel = TNIL;		/* must be initialised */
	    set_string(pw, inst_name[i]);
	}
    }
    vm_inst_flag_ = (char*)
	(get_kernel_array(in_dict("vm_inst_flag",1))->val.ptr + 1);
    vm_inst_ctr_ = (unsigned long*)
	(get_kernel_array(in_dict("vm_inst_ctr",1))->val.ptr + 1);
#endif /* PRINTAM */

#ifdef lint
    {
	pword	*pw;
	int	r;
	pw = get_array_header(d_.nil);	/* dummy calls for lint */
	pw = get_visible_array_header(d_.nil, pw->val, pw->tag, &r);
    }
#endif

}
/*
 * function to initialise sepia_kernel global variable from within C
 */
pword *
init_kernel_var(int flags, dident vdid, value v, type t)
{
    int	res;
    pword module;

    module.tag.kernel = ModuleTag(d_.kernel_sepia);
    module.val.did = d_.kernel_sepia;
    if (flags & INIT_SHARED)
    {
	value v_name, v_type, v_vis;
	v_name.did = vdid;
	v_type.did = d_.prolog;
	v_vis.did = d_.local0;
	(void) p_make_array_(v_name, tdict, v_type, tdict,
			     v_vis, tdict, module.val, module.tag);
	(void) p_setval_body(v_name, tdict, v, t, module.val, module.tag);
    }
    return get_modular_property(vdid, GLOBVAR_PROP,
		module.val.did, module.tag, VISIBLE_PROP, &res);
}



pword *
get_kernel_array(dident adid)
{
    int res;
    pword module;
    if (DidArity(adid) != 1)
	return 0;
    module.tag.kernel = ModuleTag(d_.kernel_sepia);
    module.val.did = d_.kernel_sepia;
    return get_modular_property(adid, ARRAY_PROP,
		module.val.did, module.tag, VISIBLE_PROP, &res);
}

int
make_kernel_array(dident adid, int length, dident atype, dident avisib)
{
    pword module;
    pword buf[5];

    if (DidArity(adid) != 1)
	return RANGE_ERROR;
    module.tag.kernel = ModuleTag(d_.kernel_sepia);
    module.val.did = d_.kernel_sepia;
    buf[0].val.ptr = &buf[3];
    buf[0].tag.kernel = TCOMP;
    buf[1].val.did = atype;
    buf[1].tag.kernel = TDICT;
    buf[2].val.did = avisib;
    buf[2].tag.kernel = TDICT;
    buf[3].val.did = adid;		/*  must be arity 1 !!! */
    buf[3].tag.kernel = TDICT;
    buf[4].val.nint = (long) length;
    buf[4].tag.kernel = TINT;
    return p_make_array_(buf[0].val, buf[0].tag, buf[1].val, buf[1].tag,
		    buf[2].val, buf[2].tag, module.val, module.tag);
}

/*
 * this function is used to implement the macros in external.h
 */

pword *
get_array_header(dident adid)
{
    if (DidArity(adid) > 0)
	return get_property(adid, ARRAY_PROP);
    else
	return get_property(adid, GLOBVAR_PROP);
}

pword *
get_visible_array_header(dident adid, value vm, type tm, int *res)
{
    if (DidArity(adid) > 0)
	return get_modular_property(adid, ARRAY_PROP,
				    vm.did, tm, VISIBLE_PROP, res);
    else
	return get_modular_property(adid, GLOBVAR_PROP,
				    vm.did, tm, VISIBLE_PROP, res);
}


/*
 * erase(Array/Dim)
 *	erase the given array
 */
int
p_erase_array_body(value val1, type tag1, value vmod, type tmod)
{
    value	vscope;

    vscope.did = d_.nil; /* visible (not local nor global) */
    return (p_erase_array_(val1, tag1, vscope, tdict, vmod, tmod));
}

/*
  erase_array_(Array, Module, Visibility)
*/
/*ARGSUSED*/
int
p_erase_array_(value val1, type tag1, value vscope, type tscope, value vmod, type tmod)
{
    dident	adid;
    int		prop;
    int		err;
    int		scope = (vscope.did == d_.local0 ? LOCAL_PROP
			 : (vscope.did == d_.global0 ? GLOBAL_PROP
			    : VISIBLE_PROP));

    Get_Functor_Did(val1, tag1, adid);
    
    if (DidArity(adid) > 0)
	prop = ARRAY_PROP;
    else
	prop = GLOBVAR_PROP;
    if ((err = EraseAV(adid, prop, vmod.did, tmod, scope))
	< PSUCCEED)
    {
	if (err == PERROR)
	    err = NOGLOBAL;
        Bip_Error(err);
    }
    Succeed_;
}

static int 
p_test_and_setval_body(value a, type ta, value vc, type tc, value v, type t, value vmod, type tmod)
{
    int		err;

    Error_If_Ref(ta);
    if (IsAtom(ta) || IsNil(ta))
    {
    	pword *pw;
	pword copy_pw;
	
	a_mutex_lock(&PropertyLock);
    	if (!(pw = VisibleAV(IsNil(ta) ? d_.nil : a.did,
				GLOBVAR_PROP, vmod.did, tmod, &err)))
    	{
	    if (err == PERROR)
		err = NOGLOBAL;
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
        }
	if (IsGlobalPrologRefIndex(pw) || IsGlobalPrologRef(pw))
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(TYPE_ERROR);
	}
	if (compare_terms(vc, tc, pw->val, pw->tag))
	{
	    a_mutex_unlock(&PropertyLock);
	    Fail_;
	}
        err = create_heapterm(&copy_pw, v, t);
	if (err != PSUCCEED)
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	free_heapterm(pw);
        move_heapterm(&copy_pw, pw);
	a_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    else
    {
        Bip_Error(TYPE_ERROR);
    }
}

int 
p_setval_body(value a, type ta, value v, type t, value vmod, type tmod)
{
    int		err;
    pword	copy_pw;

    Error_If_Ref(ta);
    if (IsAtom(ta) || IsNil(ta))
    {
    	pword *pw;

	a_mutex_lock(&PropertyLock);
    	if (!(pw = VisibleAV(IsNil(ta) ? d_.nil : a.did,
				GLOBVAR_PROP, vmod.did, tmod, &err)))
    	{
	    if (err == PERROR)
		err = NOGLOBAL;
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
        }
	if (IsGlobalPrologRef(pw))
	{
	    copy_pw.val.all = v.all;
	    copy_pw.tag.all = t.all;
	    ec_ref_set((ec_ref) pw->val.wptr, copy_pw);
	    a_mutex_unlock(&PropertyLock);
	    Succeed_;
	}
	else if (IsGlobalPrologRefIndex(pw))
	{
	    (void) ec_assign(&GLOBVAR[pw->val.nint], v, t);
	    a_mutex_unlock(&PropertyLock);
	    Succeed_;
	}
        err = create_heapterm(&copy_pw, v, t);
	if (err != PSUCCEED)
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	free_heapterm(pw);
        move_heapterm(&copy_pw, pw);
	a_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    if (IsStructure(ta) || IsList(ta))
    {
    	uword	*adr;
    	uword	kind;
   
	a_mutex_lock(&PropertyLock);
    	if (!(adr = get_elt_address(a, ta, &kind, vmod.did, tmod, &err)))
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
    	}
	err = PSUCCEED;
	switch (kind)
	{
	case TCOMP:
	    free_heapterm((pword *)adr);
	    err = create_heapterm((pword *)adr,v,t);
	    break;
	case TSTRG:
	    if (IsRef(t)) err = INSTANTIATION_FAULT;
	    else if (!IsInteger(t)) err = TYPE_ERROR;
	    else *((unsigned char *) adr) = (v.nint & 0XFF);
	    break;
	case TINT:
	    if (IsRef(t)) err = INSTANTIATION_FAULT;
	    else if (!IsInteger(t)) err = TYPE_ERROR;
	    else *((word *) adr) = v.nint;
	    break;
	case TDBL:
	    if (IsRef(t)) err = INSTANTIATION_FAULT;
	    else if (!IsDouble(t)) err = TYPE_ERROR;
	    else *((double *) adr) = Dbl(v);
	    break;
	}
	a_mutex_unlock(&PropertyLock);
	return err;
    }
    else
    {
        Bip_Error(TYPE_ERROR);
    }
}


/* make it fail if no global variable associated	*/
static int
p_getval_body(value a, type ta, value v, type t, value vmod, type tmod)
{
    int		err;
    dident	wd;

    Error_If_Ref(ta);
    if (IsNil(ta))
	wd = d_.nil;
    else
	wd = a.did;
    if (IsAtom(ta) || IsNil(ta))
    {
    	pword	*p;
	pword	result;
	
	a_mutex_lock(&PropertyLock);
    	if (!(p = VisibleAV(wd, GLOBVAR_PROP, vmod.did, tmod, &err)))
    	{
	    if (err == PERROR)
		err = NOGLOBAL;
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
    	get_heapterm(p, &result);
	a_mutex_unlock(&PropertyLock);

	if (IsRef(result.tag) && result.val.ptr == &result)
	{
	    Succeed_;		/* a free variable	*/
	}
	else if (IsGlobalPrologRef(&result))
	{
	    result = ec_ref_get((ec_ref) result.val.wptr);
	    Return_Unify_Pw(v, t, result.val, result.tag);
	}
	else if (IsGlobalPrologRefIndex(&result))
	{
	    if (!IsSimple(GLOBVAR[result.val.nint].tag)
	       && GLOBVAR[result.val.nint].val.ptr >= TG
	       && GLOBVAR[result.val.nint].val.ptr < B_ORIG )
	    {
		Fail_;
	    }
	    Return_Unify_Pw(v, t, GLOBVAR[result.val.nint].val,
		GLOBVAR[result.val.nint].tag);
	}
	else
	{
	    Return_Unify_Pw(v,t,result.val,result.tag);
	}
    }
    else if (IsStructure(ta) || IsList(ta))
    {
	uword	*adr;
	uword	kind;
	pword	result;

	a_mutex_lock(&PropertyLock);
	if (!(adr = get_elt_address(a, ta, &kind, vmod.did, tmod, &err)))
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	switch (kind)
	{
	case TCOMP:
	    get_heapterm((pword *)adr, &result);
	    if (IsRef(result.tag) && result.val.ptr == &result)
	    {
		a_mutex_unlock(&PropertyLock);
		Succeed_;		/* a free variable	*/
	    }
	    break;
	case TSTRG:
	    result.val.nint = (word) *((unsigned char *) adr);
	    result.tag.kernel = TINT;
	    break;
	case TINT:
	    result.val.nint = (word) *((word *) adr);
	    result.tag.kernel = TINT;
	    break;
	case TDBL:
	    Make_Float(&result, *((double *) adr))
	    break;
	}
	a_mutex_unlock(&PropertyLock);
	Return_Unify_Pw(v,t,result.val,result.tag);
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }
}


static int
p_incval_body(value a, type ta, value vmod, type tmod)
{
    pword	*p;
    int		err;
    dident	wd;
    
    Error_If_Ref(ta);
    if (IsNil(ta))
	wd = d_.nil;
    else
	wd = a.did;
    if (IsAtom(ta) || IsNil(ta))
    {
	a_mutex_lock(&PropertyLock);
    	if (!(p = VisibleAV(wd, GLOBVAR_PROP, vmod.did, tmod, &err)))
	{
	    if (err == PERROR)
		err = NOGLOBAL;
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	if((!IsInteger(p->tag)))
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(TYPE_ERROR);
	}
	p->val.nint++;
	a_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    if (IsStructure(ta) || IsList(ta))
    {
    	uword	*adr;
    	uword	kind;
	pword	*pi;
	
	a_mutex_lock(&PropertyLock);
    	if (!(adr = get_elt_address(a, ta, &kind, vmod.did, tmod, &err)))
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
    	}
	if (kind == TINT)
	{
	    (*((int *) adr))++;
	}
	else if (kind == TCOMP)
	{
	    pi = (pword *) adr;
	    if (IsInteger(pi->tag))
	    {
		pi->val.nint++;
	    }
	    else
	    {
		a_mutex_unlock(&PropertyLock);
		Bip_Error(TYPE_ERROR);
	    }
	}
	else
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(TYPE_ERROR);
        }
	a_mutex_unlock(&PropertyLock);
    	Succeed_;
    }
    else
    {
        Bip_Error(TYPE_ERROR);
    }
}

static int
p_decval_body(value a, type ta, value vmod, type tmod)
{
    pword	*p;
    int		err;
    dident	wd;

    Error_If_Ref(ta);
    if (IsNil(ta))
	wd = d_.nil;
    else
	wd = a.did;
    if (IsAtom(ta) || IsNil(ta))
    {
	a_mutex_lock(&PropertyLock);
    	if (!(p = VisibleAV(wd, GLOBVAR_PROP, vmod.did, tmod, &err)))
	{
	    if (err == PERROR)
		err = NOGLOBAL;
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	if((!IsInteger(p->tag)))
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(TYPE_ERROR);
	}
	p->val.nint--;
	a_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    if (IsStructure(ta) || IsList(ta))
    {
    	uword	*adr;
    	uword	kind;
	pword	*pi;
   
	a_mutex_lock(&PropertyLock);
    	if (!(adr = get_elt_address(a, ta, &kind, vmod.did, tmod, &err)))
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
    	}
	if (kind == TINT)
	{
	    (*((int *) adr))--;
	}
	else if (kind == TCOMP)
	{
	    pi = (pword *) adr;
	    if (IsInteger(pi->tag))
	    {
		pi->val.nint--;
	    }
	    else
	    {
		a_mutex_unlock(&PropertyLock);
		Bip_Error(TYPE_ERROR);
	    }
	}
	else
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(TYPE_ERROR);
        }
	a_mutex_unlock(&PropertyLock);
    	Succeed_;
    }
    else
    {
        Bip_Error(TYPE_ERROR);
    }
}

/*
 * array_info(+Array, ?OptionList, +Module)
 *
 * The arguments of Array will be unified with the dimension sizes,
 * OptionList is unified with a two element list [<type>, <visibility>]
 */

static int
p_array_info(value varr, type tarr, value vopt, type topt, value vmod, type tmod)
{
    pword	*prop;
    pword	*pw = (pword *) 0;
    int		i, arity, err, prop_name;
    dident	wdid, vis;
    uword	*w;
    value	v;
    Prepare_Requests

    Check_Output_List(topt);
    switch (TagType(tarr))
    {
    case TLIST:
    	wdid = d_.list;
	pw = varr.ptr;
	break;
    case TCOMP:
    	wdid = varr.ptr->val.did;
	pw = varr.ptr + 1;
	break;
    case TDICT:
    	wdid = varr.did;
	break;
    case TNIL:
    	wdid = d_.nil;
	break;
    default:
	Bip_Error(IsRef(tarr) ? INSTANTIATION_FAULT : TYPE_ERROR);
    }

    a_mutex_lock(&PropertyLock);
    arity = DidArity(wdid);
    prop_name = arity ? ARRAY_PROP : GLOBVAR_PROP;
    prop = VisibleAV(wdid, prop_name, vmod.did, tmod, &err);
    if (!prop)
    {
	if (err == PERROR) /* no array */
	    err = PFAIL;
	a_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
    vis = (err == LOCAL_PROP) ? d_.local0 : d_.global0;

    if (arity == 0)
    {
	if (IsGlobalPrologRef(prop) || IsGlobalPrologRefIndex(prop))
	    wdid = d_reference_;
	else
	    wdid = d_.prolog;
    }
    else
    {
	switch(TagType(prop->tag))		/* get the type */
	{
	case TCOMP:
	    wdid = d_.prolog;
	    break;
	case TSTRG:
	    wdid = d_.byte;
	    break;
	case TINT:
	    wdid = d_.integer0;
	    break;
	case TDBL:
	    wdid = d_.float0;
	    break;
	default:
	    p_fprintf(current_err_,
		    "internal error: array structure corrupted\n");
	    ec_flush(current_err_);
	}

	w = ((uword *)(prop -> val.ptr) + 1);	/* unify the dimensions */
	for(i = 0; i < arity; i++)
	{
	    v.nint = (word) *w++;
	    Request_Unify_Pw(pw->val, pw->tag, v, tint);
	    pw++;
	}
    }
    a_mutex_unlock(&PropertyLock);

    pw = TG;				/* make options list */
    TG += 4;
    Check_Gc;
    pw[0].val.did = wdid;		/* [type, visibility] */
    pw[0].tag.kernel = TDICT;
    pw[1].val.ptr = &pw[2];
    pw[1].tag.kernel = TLIST;
    pw[2].val.did = vis;
    pw[2].tag.kernel = TDICT;
    pw[3].tag.kernel = TNIL;
    Request_Unify_List(vopt, topt, pw);
    Return_Unify
}


/* get_elt_address must be called in an interrupt protected area */

uword *
get_elt_address(value v, type t, uword *kind, dident mod_did, type mod_tag, int *perr)
{
    pword	*pw, *q, *h, *p;
    int		ndim1, ndim2, i, n;
    dident	arraydid;
    uword	*w;

    if (IsList(t))
    	arraydid = d_.list;
    else
   	arraydid = v.ptr->val.did;
    ndim1 = DidArity(arraydid);
    if (IsList(t))
    	p = h = v.ptr - 1;
    else
    	p = h = v.ptr;
    for (i=0; i < ndim1; i++)
    {
	q = ++h;
	Dereference_(q)
	if(IsRef(q->tag))
	{
	    *perr = INSTANTIATION_FAULT;
	    return 0;
	}
	if(DifferTypeC(q->tag,TINT))
	{
	    *perr = TYPE_ERROR;
	    return 0;
	}
    }

    if (!(pw = VisibleAV(arraydid, ARRAY_PROP, mod_did, mod_tag, perr)))
    {
	if (*perr == PERROR)
	    *perr = NOGLOBAL;
	return 0;
    }
    *kind = pw->tag.kernel;
    pw = pw->val.ptr;
    ndim2 = DidArity((pw)->val.did);
    n = 0;
    w = ((uword *) pw) + 1;
    for(i = 0; i < ndim2; i++)
    {
	q = ++p;
	Dereference_(q)
	n *= *w;
	if(*w++ <= q->val.nint || q->val.nint < 0)
	{
	    *perr = RANGE_ERROR;
	    return 0;
	}
	n += q->val.nint;
    }
    w = (uword *)pw + RoundUp((ndim2+1)*sizeof(uword))/sizeof(uword);
    switch (*kind)
    {
    case TCOMP:		return (uword *) (((pword *) w) + n);
    case TSTRG:		return (uword *) (((unsigned char *)w) + n);
    case TINT:		return (uword *) (((word *)w) + n);
    case TDBL:		return (uword *) (((double *)w) + n);
    default:		return (uword *) 0;
    }
}

/* get_first_elt must be called in an interrupt protected area		*/
word
get_first_elt(pword *p, pword *q, uword *kind, uword *size, dident vmod_did, type mod_tag)
{
    dident mydid;
    uword *w;
    int i, n, err;

    Dereference_(p)
    if (IsRef(p->tag))
	return(INSTANTIATION_FAULT);
    if (DifferTypeC(p->tag,TDICT))
	return(TYPE_ERROR);
    Dereference_(q)
    if (IsRef(q->tag))
	return(INSTANTIATION_FAULT);
    if (DifferTypeC(q->tag,TINT))
	return(TYPE_ERROR);
    mydid = check_did(p->val.did, (int) q->val.nint);
    if (mydid == D_UNKNOWN)
	return(NOGLOBAL);
    if (!(p = VisibleAV(mydid, ARRAY_PROP, vmod_did, mod_tag, &err)))
    {
	if (err == PERROR)
	    err = NOGLOBAL;
	return (err);
    }
    *size = 4;
    *kind = p->tag.kernel;
    if(q->val.nint == 0)
    {
	return((word) (&(p->val)));
    }
    switch (*kind)
    {
    case TCOMP:		*size = sizeof(pword); break;
    case TSTRG:		*size = sizeof(char); break;
    case TINT:		*size = sizeof(word); break;
    case TDBL:		*size = sizeof(double); break;
    }
    p = p->val.ptr;
    w = ((uword *) p) + 1;
    n = DidArity(mydid);
    for(i = 0; i < n ; i++)
	*size *= *w++;
    w = (uword *)p + RoundUp((n+1)*sizeof(uword))/sizeof(uword);
    return((word) w);
}


/*
 * free all the memory occupied by the array
 */

free_array(pword *prop_value)
{
    uword *array_header = (uword *) prop_value->val.ptr;

    if (IsStructure(prop_value->tag))
    {
	int	dim = DidArity(array_header[0]);
	pword	*array_contents = (pword *)
	    (array_header + RoundUp((dim+1)*sizeof(uword))/sizeof(uword));
	uword	size;

	for (size = 1; dim > 0; --dim)	/* compute number of elements */
	    size *= array_header[dim];

	for (; size > 0; --size)
	    free_heapterm(array_contents++);
    }
    hg_free((generic_ptr) array_header);
}


/*
 * Support function for the dictionary garbage collector:
 * Mark all DID's inside the array (applies only to 'prolog' arrays)
 */

void
mark_dids_from_array(pword *prop_value)
{
    extern void mark_dids_from_heapterm(pword *root);

    if (IsStructure(prop_value->tag))
    {
	uword	*array_header = (uword *) prop_value->val.ptr;
	int	dim = DidArity(array_header[0]);
	pword	*array_contents = (pword *)
	    (array_header + RoundUp((dim+1)*sizeof(uword))/sizeof(uword));
	register uword	size;

	for (size = 1; dim > 0; --dim)	/* compute number of elements */
	    size *= array_header[dim];

	for (; size > 0; --size)
	    mark_dids_from_heapterm(array_contents++);
    }
}

static int
p_xset(value vhandle, type thandle, value vi, type ti, value vval, type tval)
{
    pword pw;
    pw.val = vval;
    pw.tag = tval;
    Check_Type(thandle, THANDLE);
    Check_Type(vhandle.ptr->tag, TEXTERN);
    Check_Integer(ti);
    if (!(ExternalData(vhandle.ptr)))
	Bip_Error(STALE_HANDLE);
    if (!ExternalClass(vhandle.ptr)->set)
    	{ Bip_Error(UNIMPLEMENTED); }
    return ExternalClass(vhandle.ptr)->set(ExternalData(vhandle.ptr), vi.nint, pw);
}

static int
p_xget(value vhandle, type thandle, value vi, type ti, value vval, type tval)
{
    pword pw;
    pw.val = vval;
    pw.tag = tval;
    Check_Type(thandle, THANDLE);
    Check_Type(vhandle.ptr->tag, TEXTERN);
    Check_Integer(ti);
    if (!(ExternalData(vhandle.ptr)))
	Bip_Error(STALE_HANDLE);
    if (!ExternalClass(vhandle.ptr)->get)
    	{ Bip_Error(UNIMPLEMENTED); }
    pw = ExternalClass(vhandle.ptr)->get(ExternalData(vhandle.ptr), vi.nint);
    Return_Unify_Pw(vval, tval, pw.val, pw.tag);
}


/* The following builtins use the global error variable ! */
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

/*
  Create an array v of type vt in module vmod, vscope can
  be local or global.

  vt = prolog			nonlogical variable
  vt = global_reference		reference (via ec_ref)
  vt = global_reference_index 	reference (via GLOBVAR[] array)
*/
/*ARGSUSED*/
int
p_make_array_(value v, type t, value vt, type tt, value vscope, type tscope, value vmod, type tmod)
{
    int		ndim, size, i, nitem, err;
    pword	*p, *pp, *spw;
    type	tag;
    dident	arraydid;
    uword	*w;
    int		header_size;
    int		scope = (vscope.did == d_.local0 ? LOCAL_PROP : GLOBAL_PROP);

    Check_Module(tmod, vmod);
    Check_Module_Access(vmod, tmod);
    /* no need to check for tscope, system use only */

    if (IsAtom(t) || IsNil(t))	/* global variable */
    {
	dident		wd;
	pword		init_pw;

	if (IsNil(t))
	    wd = d_.nil;
	else
	    wd = v.did;

#ifdef GLOBALREFS_ARE_ECREFS
	if (IsStructure(tt) && vt.ptr[0].val.did == d_reference1_)
	{
	    init_pw = vt.ptr[1];
	    err = ec_constant_table_enter(vt.ptr[1].val, vt.ptr[1].tag, &init_pw);
	    if (err != PSUCCEED)
	    {
		Bip_Error(err == PFAIL ? UNIMPLEMENTED : err);
	    }
	    vt.did = d_global_reference_;
	}
	else
#endif
	{
	    Check_Atom(tt);
	    Make_Integer(&init_pw, 0);
	}
#ifdef GLOBALREFS_ARE_ECREFS
	if (vt.did == d_global_reference_index_)
#else
	if (vt.did == d_global_reference_index_
	 || vt.did == d_global_reference_)
#endif
	{
	    if (GlobalVarIndex >= GLOBAL_VARS_NO) {
		Bip_Error(RANGE_ERROR);
	    }
	}
#ifdef GLOBALREFS_ARE_ECREFS
	else if (vt.did != d_global_reference_ && vt.did != d_.prolog)
#else
	else if (vt.did != d_.prolog)
#endif
	{
	    Bip_Error(RANGE_ERROR);
	}
	if (VisibleAV(wd, GLOBVAR_PROP, vmod.did, tmod, &err)) {
	    Bip_Error(ARRAY_EXISTS);
	}
	a_mutex_lock(&PropertyLock);
	if (!(p = NewGlobVarItem(wd, vmod.did, tmod, scope, &err)))
	{
	    /* trying to define a global when there is a global or
	       a local when there is a local here			*/
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(ARRAY_EXISTS);
	}
#ifdef GLOBALREFS_ARE_ECREFS
	if (vt.did == d_global_reference_)
	{
	    p->val.wptr = (uword *) ec_ref_create(init_pw);
	    p->tag.kernel = GlobalPrologRefTag;
	} else if (vt.did == d_global_reference_index_)
#else
	if (vt.did == d_global_reference_
	 || vt.did == d_global_reference_index_)
#endif
	{
	    p->val.nint = GlobalVarIndex;
	    GlobalVarIndex++;
	    p->tag.kernel = GlobalPrologRefIndexTag;
	} else {
	    p->val.ptr = p;
	    p->tag.kernel = TREF;
	}
	a_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    else if (IsList(t))
    {
    	arraydid = d_.list;
    }
    else
    {
	Check_Structure(t);
	arraydid = v.ptr->val.did;
    }

    Check_Atom(tt);
    if (vt.did == d_.prolog)
    {
	tag.kernel = TCOMP;
	size = sizeof(pword);
    }
    else if(vt.did == d_.byte)
    {
	tag.kernel = TSTRG;
	size = 1;
    }
    else if(vt.did == d_.integer0)
    {
	tag.kernel = TINT;
	size = sizeof(word);
    }
    else if(vt.did == d_.float0)
    {
	tag.kernel = TDBL;
	size = sizeof(double);
    }
    else
    {
	Bip_Error(RANGE_ERROR);
    }

    ndim = DidArity(arraydid);
    nitem = 1;

    /* compute the number of items which will be held by the array */
    if (IsList(t))
    	p = v.ptr - 1;
    else
     	p = v.ptr;
    for(i = 0; i < ndim; i++)
    {
	spw = ++p;
	Dereference_(spw);
	Check_Integer(spw->tag);
	if (spw->val.nint <= 0)
	{
	    Bip_Error(RANGE_ERROR);
	}
	nitem *= spw->val.nint;
    }

    /* We might need padding to properly align the array */
    header_size = RoundUp((ndim+1)*sizeof(uword));
    
    a_mutex_lock(&PropertyLock);
    if (!(p = NewArrayItem(arraydid, vmod.did, tmod, scope, &err)))
    {
	/* trying to define a global when there is a global or
	   a local when there is a local here				*/
	a_mutex_unlock(&PropertyLock);
	Bip_Error(ARRAY_EXISTS);
    }

    /* grab space for this array */
    p->tag.all = tag.all;			/* type of the array */
    p->val.ptr = (pword *)hg_alloc(size*nitem + header_size);
    p = p->val.ptr;
    /* initialize the header of the array */
    
    p->val.did = arraydid;	/* thus backward pointer and
				   the number of dimensions		*/
    w = ((uword *) p) + 1;     /* skip did information			*/
    
    if (IsList(t))
    	pp = v.ptr - 1;
    else
     	pp = v.ptr;
    for(i = 0; i < ndim; i++)
    {
	spw = ++pp;
	Dereference_(spw);
	*w++ = spw->val.nint;	/* size of each dimension */
    }

    /* initialize the elements */
    w = (uword *)p + header_size/sizeof(uword);
    switch (tag.kernel)
    {
    case TCOMP:
	p = (pword *) w;
	for(i = 0; i < nitem; i++)
	{
	    p->val.ptr = p;
	    (p++)->tag.kernel = TREF;
	}
	break;
    case TSTRG:
	{
	    unsigned char *s = (unsigned char *) w;
	    for(i = 0; i < nitem ; i++) *s++ = 0;
	}
	break;
    case TINT:
	{
	    word *s = (word *) w;
	    for(i = 0; i < nitem ; i++) *s++ = 0;
	}
	break;
    case TDBL:
	{
	    double *s = (double *) w;
	    for(i = 0; i < nitem ; i++) *s++ = 0.0;
	}
	break;
    }
    a_mutex_unlock(&PropertyLock);
    Succeed_;
}
