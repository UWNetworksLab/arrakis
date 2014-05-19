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
 * SEPIA C SOURCE MODULE
 *
 * VERSION	$Id: operator.c,v 1.2 2008/07/20 17:05:47 jschimpf Exp $
 */

/*
 * IDENTIFICATION		operator.c
 *
 * DESCRIPTION			implemetation of the operator using
 *				the property list
 *
 * CONTENTS:
 *
 * AUTHOR		VERSION	 DATE	REASON
 * Emmanuel van Rossum		900315	created the file
 *
 */

#include	"config.h"
#include	"sepia.h"
#include	"types.h"
#include	"embed.h"
#include	"error.h"
#include	"mem.h"
#include	"dict.h"
#include 	"emu_export.h"
#include	"property.h"
#include	"module.h"



/* flags in the isop field of atoms, not set means no operator,
   set means may be an operator						*/
#define	IS_PREFIX_OP	1
#define	IS_INFIX_OP	2
#define	IS_POSTFIX_OP	4

#define FixToProp(fixity) ((fixity) == IS_PREFIX_OP ? PREFIX_PROP : (fixity) == IS_INFIX_OP ? INFIX_PROP : POSTFIX_PROP)

#define PropToFix(prop) ((prop) == PREFIX_PROP ? IS_PREFIX_OP : (prop) == INFIX_PROP ? IS_INFIX_OP : IS_POSTFIX_OP)

/* 2 followings get or create a property prop (can be PREFIX_PROP or
   INFIX_PROP or POSTFIX_PROP) for did did in module mod with visibility vis */
#define OperatorItem(did, mod, mod_tag, vis, prop, perr) \
    get_modular_property(did, prop, mod, mod_tag, vis, perr)
#define NewOperatorItem(did, mod, mod_tag, vis, prop, perr) \
    set_modular_property(did, prop, mod, mod_tag, vis, perr)


static dident	didassoc[MAX_ASSOC+1];

static int
    _get_assoc(dident assoc),
    _insert_op(int scope, long int preced, long int assoc, dident oper, dident module, type mod_tag),
    _erase_op(dident oper, long int assoc, int scope, dident module, type mod_tag),
    p_op_(value vi, type ti, value vprec, type tprec, value vassoc, type tassoc, value v_op, type t_op, value vm, type tm),
    p_is_prefix_op(value vp, type tp, value assoc, type ta, value name, type tn, value vv, type tv, value module, type tm),
    p_is_postfix_op(value vp, type tp, value assoc, type ta, value name, type tn, value vv, type tv, value module, type tm),
    p_is_infix_op(value vp, type tp, value assoc, type ta, value name, type tn, value vv, type tv, value module, type tm),
    p_abolish_op_(value v_op, type t_op, value v_assoc, type t_assoc, value v_mod, type t_mod),
    p_legal_current_op(value v_prec, type t_prec, value v_assoc, type t_assoc, value v_op, type t_op, value v_mod, type t_mod);

static opi	*_visible_anyfix_op(int fixity, dident atom, dident module, type mod_tag, int *res);


/*
 * Operator Initialization
 */
/*ARGSUSED*/
void
op_init(int flags)
{
    if (!(flags & INIT_PRIVATE))
	return;
    /* initialize the associativity table */
    didassoc[FX] = in_dict("fx", 0);
    didassoc[FY] = in_dict("fy", 0);
    didassoc[XF] = in_dict("xf", 0);
    didassoc[YF] = in_dict("yf", 0);
    didassoc[XFX] = in_dict("xfx", 0);
    didassoc[XFY] = in_dict("xfy", 0);
    didassoc[YFX] = in_dict("yfx", 0);
    didassoc[FXX] = in_dict("fxx", 0);
    didassoc[FXY] = in_dict("fxy", 0);
}

void
bip_op_init(int flags)
{
    if (!(flags & INIT_SHARED))
	return;
    (void) local_built_in(in_dict("op_", 5),	p_op_, B_SAFE);
    (void) local_built_in(in_dict("is_prefix_op", 5), p_is_prefix_op, B_SAFE);
    (void) local_built_in(in_dict("is_postfix_op", 5), p_is_postfix_op, B_SAFE);
    (void) local_built_in(in_dict("is_infix_op", 5), p_is_infix_op, B_SAFE);
    (void) local_built_in(in_dict("abolish_op_", 3),p_abolish_op_, B_SAFE);
    (void) local_built_in(in_dict("legal_current_op", 4),
		   p_legal_current_op, B_SAFE);
}

/*
 *	returns the (unsigned) associativity associated to the 
 *	Prolog one (did).
 *	return NIL_OP if 'assoc' is not in the table.
 */
static int
_get_assoc(dident assoc)
{
    long iassoc = MAX_ASSOC;
    
    while (iassoc > NIL_OP && didassoc[iassoc] != assoc)
	iassoc--;
    return (iassoc);
}

/*
 * The locking done here is rather useless:
 * We are not allowed to return a pointer to a (shared heap) opi
 * without holding the lock. But this would be just too much to fix...
 * Most of the other properties have the same bug.
 */
static opi *
_visible_anyfix_op(int fixity, dident atom, dident module, type mod_tag, int *res)
{
    opi		*operator_prop;

    if (atom == D_UNKNOWN
	|| !(DidIsOp(atom) & fixity))
    {
	*res = PERROR;
	return 0;
    }

    a_mutex_lock(&PropertyLock);

    operator_prop = OperatorItem(atom, module, mod_tag,
				 VISIBLE_PROP, FixToProp(fixity), res);
    if (!operator_prop)
    {
	a_mutex_unlock(&PropertyLock);
	*res = PERROR;
	return 0;
    }
    if (operator_prop->tag.kernel != TEND)
    {
	a_mutex_unlock(&PropertyLock);	/* THIS IS WRONG! */
	return operator_prop;
    }
    else
    {
	a_mutex_unlock(&PropertyLock);
	*res = PERROR;
	return 0;
    }
}

/*
 * visible_prefix_op(atom, module) return a pointer to the visible
 * prefix operator desriptor defined under atom and visible from module
 * if there is such an operator; return 0 otherwise.
 */
opi*
visible_prefix_op(dident atom, dident module, type mod_tag, int *res)
{
    opi		*desc;

    desc = _visible_anyfix_op(IS_PREFIX_OP, atom, module, mod_tag, res);
    if (desc && !GetOpiPreced(desc)) {
	*res = PERROR;
	return (opi *) 0;
    } else
	return desc;
}

/*
 * visible_infix_op(atom, module) return a pointer to the visible
 * infix operator desriptor defined under atom and visible from module
 * if there is such an operator; return 0 otherwise.
 */
opi*
visible_infix_op(dident atom, dident module, type mod_tag, int *res)
{
    opi		*desc;

    desc = _visible_anyfix_op(IS_INFIX_OP, atom, module, mod_tag, res);
    if (desc && !GetOpiPreced(desc)) {
	*res = PERROR;
	return (opi *) 0;
    } else
	return desc;
}

/*
 * visible_postfix_op(atom, module) return a pointer to the visible
 * postfix operator desriptor defined under atom and visible from module
 * if there is such an operator; return 0 otherwise.
 */
opi*
visible_postfix_op(dident atom, dident module, type mod_tag, int *res)
{
    opi		*desc;

    desc = _visible_anyfix_op(IS_POSTFIX_OP, atom, module, mod_tag, res);
    if (desc && !GetOpiPreced(desc)) {
	*res = PERROR;
	return (opi *) 0;
    } else
	return desc;
}

/*
 * visible_op(functor, module) return a pointer to the visible operator
 * under functor (an infix operator if functor is arity 2, an unary operator
 * if functor is of arity 1 (if a prefix and a postfix are visible,
 * the prefix is returned).
 * return 0 if no operator is visible from module under functor.
 * NOTE : when there is a prefix/postfix conflict, a local declaration
 * should be return when there is one (e.g. local postfix and global prefix).
 */
opi*
visible_op(dident functor, dident module, type mod_tag, int *res)
{
    opi		*operator_prop;
    int		arity;
    dident	atom = add_dict(functor, 0);

    if ((arity = DidArity(functor)) == 1)
    {
	/* look for a unary operator: first try FX,FY then XF,YF */
	operator_prop = visible_prefix_op(atom, module, mod_tag, res);
	/* visible_prefix_op() also finds FXX and FXY: ignore them here */
	if (!operator_prop || IsPrefix2(operator_prop))
	{
	    /* no unary prefix, look for postfix */
	    operator_prop = visible_postfix_op(atom, module, mod_tag, res);
	}
    }
    else if (arity == 2)
    {
	/* look for a binary operator, first try XFX,XFY,YFX then FXX,FXY */
	operator_prop = visible_infix_op(atom, module, mod_tag, res);
	if (!operator_prop)
	{
	    /* no infix, look for binary prefix */
	    operator_prop = visible_prefix_op(atom, module, mod_tag, res);
	    if (operator_prop && !IsPrefix2(operator_prop))
	    	operator_prop = (opi *) 0;
	}
    }
    else /* arity != 1 && arity != 2 so it is not an operator		*/
    {
	*res = PERROR; /* means no operator */
	return 0;
    }
    return operator_prop;
}

/*
 * is_visible_op(atom, module, mod_tag) returns 1 iff there is an
 * operator attached to 'atom', returns 0 otherwise.
 *
 * Must be called in an interrupt protected area.
 */
int
is_visible_op(dident atom, dident module, type mod_tag)
{
    opi		*operator_prop;
    int		err = PERROR;
    int		res;
    
    if (atom == D_UNKNOWN || !DidIsOp(atom))
    {
	Set_Bip_Error(PERROR);
	return 0;
    }

    a_mutex_lock(&PropertyLock);

    /* DidIsOp may be out of date, ie. it may be set even when there
     * is no longer such an operator. That's why we have to check.	*/
    if ((DidIsOp(atom) & IS_PREFIX_OP)
	&& (operator_prop = OperatorItem(atom, module, mod_tag,
					 VISIBLE_PROP, PREFIX_PROP, &res))
	&& operator_prop->tag.kernel != TEND
	&& GetOpiPreced(operator_prop))
    {
	a_mutex_unlock(&PropertyLock);
	return 1;
    }
    else if ((DidIsOp(atom) & IS_INFIX_OP)
	     && (operator_prop = OperatorItem(atom, module, mod_tag,
					      VISIBLE_PROP, INFIX_PROP,&res))
	     && operator_prop->tag.kernel != TEND
	    && GetOpiPreced(operator_prop))
    {
	a_mutex_unlock(&PropertyLock);
	return 1;
    }
    else if ((DidIsOp(atom) & IS_POSTFIX_OP)
	     && (operator_prop = OperatorItem(atom, module, mod_tag,
					      VISIBLE_PROP, POSTFIX_PROP,&res))
	     && operator_prop->tag.kernel != TEND
	    && GetOpiPreced(operator_prop))
    {
	a_mutex_unlock(&PropertyLock);
	return 1;
    }
    else
    {
	Set_Bip_Error(err);
	a_mutex_unlock(&PropertyLock);
	return 0;
    }
}


/* The following builtins use the global error variable ! */
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

/*
  op_(Visibility, Precedence, Associativity, Operator, Module)	
  It inserts in the operator table an operator, whose name is
  Operator of precedence Precedence and of associativity Associativity.
  Visibility can only be global or local. Module is significant
  only for a local operator.
  A null precedence will erase/hide a previous/global declaration. 
  */
/*ARGSUSED*/
static int
p_op_(value vi, type ti, value vprec, type tprec, value vassoc, type tassoc, value v_op, type t_op, value vm, type tm)
{
    long	iassoc;
    int		scope = (vi.did == d_.local0 ? LOCAL_PROP : GLOBAL_PROP);

    /* vi is supplied by the system so no need to test it.		*/
    Check_Module(tm, vm);
    Check_Atom_Or_Nil(v_op, t_op);
    Check_Atom_Or_Nil(vassoc, tassoc);
    Check_Integer(tprec);

    if ((vprec.nint > 1200) || (vprec.nint < 0))
    {
        Bip_Error(RANGE_ERROR);
    }
    iassoc = _get_assoc(vassoc.did);
    if (iassoc == NIL_OP)
    {
	Bip_Error(RANGE_ERROR);
    }

    if (vprec.nint == 0 && scope == GLOBAL_PROP)
	/* precedence 0 is used to erase the operator but if it is
	   local, the descriptor is kept to hide a global operator	*/
	return _erase_op(v_op.did, iassoc, scope, vm.did, tm);
    else
	return _insert_op(scope, vprec.nint, iassoc, v_op.did, vm.did, tm);
}

/*
  abolish_op_(atom, assoc, module)
  abolish the declaration of the operator 'atom' of associativity
  'assoc' visible from 'module'.
 */
static int
p_abolish_op_(value v_op, type t_op, value v_assoc, type t_assoc, value v_mod, type t_mod)
{
    long	iassoc;

    Check_Atom_Or_Nil(v_op, t_op);
    Check_Atom_Or_Nil(v_assoc, t_assoc);
    Check_Module(t_mod, v_mod);

    iassoc = _get_assoc(v_assoc.did);
    if (iassoc == NIL_OP)
    {
	Bip_Error(RANGE_ERROR);
    }
    
    return _erase_op(v_op.did, iassoc, VISIBLE_PROP, v_mod.did,t_mod);
}

/*
  _insert_op( scope, preced, assoc, oper, module, mod_tag)
  An insertion is made in the operator property list if there are
  no conflict of associativity (postfix and infix).
  However a local postfix/infix hide a global one so that
  the conflict is impossible between a local and a global.
  A local operator can not be modified in a locked module if the
  module tag is not signed.
  The precedence 0 is used to hide a global operator.
*/
static int
_insert_op(int scope, long int preced, long int assoc, dident oper, dident module, type mod_tag)
{
    opi		*operator_prop;
    int		prop_type;
    int		arity;
    int		res = PERROR;

    switch (assoc)
    {
    case XF:
    case YF:
	prop_type = POSTFIX_PROP;
	arity = 1;
	break;
    case FX:
    case FY:
	prop_type = PREFIX_PROP;
	arity = 1;
	break;
    case FXX:
    case FXY:
	prop_type = PREFIX_PROP;
	arity = 2;
	break;
    case XFX:
    case XFY:
    case YFX:
	prop_type = INFIX_PROP;
	arity = 2;
	break;
    }

    a_mutex_lock(&PropertyLock);

#define REDEF_OPERATOR -73
#define HIDING_OPERATOR -74

    operator_prop = OperatorItem(oper, module, mod_tag, scope, prop_type, &res);

    if (operator_prop)		/* same scope operator exists already */
    {
	if (preced && (GetOpiAssoc(operator_prop) != assoc ||
			GetOpiPreced(operator_prop) != preced)) {
	    res = REDEF_OPERATOR;
	} else {
	    res = PSUCCEED;
	}
    }
    else
    {
	if (res != PERROR)
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(res);
	}
	/* No proper scope operator exists yet */
	/* For locals, check hiding */
	if (scope == LOCAL_PROP &&
	    OperatorItem(oper, module, mod_tag, GLOBAL_PROP, prop_type, &res))
	    res = HIDING_OPERATOR;
	else
	    res = PSUCCEED;

	operator_prop = NewOperatorItem(oper, module, mod_tag,
					scope, prop_type, &res);
	if (!operator_prop)
	{
	    a_mutex_unlock(&PropertyLock);
	    Bip_Error(res);
	}
    }

    /* now update the descriptor					*/
    operator_prop->tag.kernel = TDICT;
    Set_Opi_Assoc(operator_prop, assoc);
    Set_Opi_Preced(operator_prop, preced);
    OpiDid(operator_prop) = add_dict(oper, arity);
    DidIsOp(oper) |= PropToFix(prop_type);

    a_mutex_unlock(&PropertyLock);
    if (res < 0)
	{Bip_Error(res)}
    return res;
}

/*
 * _erase_op(oper, module) erase the definition of an operator
 */
static int
_erase_op(dident oper, long int assoc, int scope, dident module, type mod_tag)
{
    opi		*operator_prop;
    int		prop_type;
    int		res;
    
    switch (assoc)
    {
    case XF:
    case YF:
	prop_type = POSTFIX_PROP;
	break;
    case FX:
    case FY:
    case FXX:
    case FXY:
	prop_type = PREFIX_PROP;
	break;
    case XFX:
    case XFY:
    case YFX:
	prop_type = INFIX_PROP;
	break;
    }

    a_mutex_lock(&PropertyLock);
    operator_prop = OperatorItem(oper, module, mod_tag, scope, prop_type,&res);
    if (!operator_prop)
    {
	if (res == PERROR)
	    res = UNDEF_OPERATOR;
	a_mutex_unlock(&PropertyLock);
	Bip_Error(res);
    }
    else if (operator_prop->tag.kernel == TEND)
    {
	a_mutex_unlock(&PropertyLock);
	Bip_Error(UNDEF_OPERATOR);
    }
    if (erase_modular_property(oper, prop_type, module, mod_tag, scope)
	== PFAIL) /* the property is completely erased for that atom	*/
    {
	DidIsOp(oper) &= ~PropToFix(prop_type);
    }
    a_mutex_unlock(&PropertyLock);
    Succeed_;
}

/*
  legal_current_op(?Precedence, ?Assoc, +Operator_atom, +Module)
  checks that all arguments are valid for current_op_body/4.
  */
static int
p_legal_current_op(value v_prec, type t_prec, value v_assoc, type t_assoc, value v_op, type t_op, value v_mod, type t_mod)
{
    if (!IsRef(t_op))			/* Operator name		*/
    {
	Check_Atom_Or_Nil(v_op, t_op);
#ifdef lint
	/* v_op is set in Check_Atom_Or_Nil but not used		*/
	if (v_op.nint) return v_op.nint;
#endif /* lint */
    }
    Check_Module(t_mod, v_mod);		/* module			*/
    Check_Module_Access(v_mod, t_mod);

    if (IsAtom(t_assoc))		/* Associativity		*/
    {
	if (_get_assoc(v_assoc.did) == NIL_OP)
	{
	    Bip_Error(RANGE_ERROR);
	}
    }
    else if (!IsRef(t_assoc))
    {
	Bip_Error(TYPE_ERROR);
    }
    
    if (IsInteger(t_prec))		/* Precedence			*/
    {
	if (v_prec.nint < 0 || v_prec.nint > 1200)
	{
	    Bip_Error(RANGE_ERROR);
	}
    }
    else if (!IsRef(t_prec))
    {
	Bip_Error(TYPE_ERROR);
    }
    Succeed_;
}

/*
	is_prefix_op(Precedence, Associativity, Name, Visib, Module)
	Name and Module must be instantiated.
	Associativity and Precedence are either instantiated or a variable.
	If there is an operator of this type in the operator table, 
	It succeeds and instantiates the precedence.
*/
/*ARGSUSED*/ /* check is already made in p_illegal_current_op		*/
static int
p_is_prefix_op(value vp, type tp, value assoc, type ta, value name, type tn, value vv, type tv, value module, type tm)
{
    opi    	*desc;
    int		res;
    Prepare_Requests;

    if (IsNil(tn))
    {
	name.did = d_.nil;
	tn = tdict;
    }

    if (desc = _visible_anyfix_op(IS_PREFIX_OP, name.did, module.did, tm, &res))
    {
        Request_Unify_Integer(vp, tp, GetOpiPreced(desc));
	Request_Unify_Atom(assoc, ta, didassoc[GetOpiAssoc(desc)]);
	Request_Unify_Atom(vv, tv,
			    (res == LOCAL_PROP ? d_.local0 : d_.global0));
	Return_Unify;
    }
    Fail_;
}

/*
	is_postfix_op(Precedence, Associativity, Name, Visib, Module)
	Name and Module must be instantiated.
	Associativity and Precedence are either instantiated or a variable.
	If there is an operator of this type in the operator table, 
	It succeeds and instantiates the precedence.
*/
/*ARGSUSED*/ /* check is already made in p_illegal_current_op		*/
static int
p_is_postfix_op(value vp, type tp, value assoc, type ta, value name, type tn, value vv, type tv, value module, type tm)
{
    opi    	*desc;
    int		res;
    Prepare_Requests

    if (IsNil(tn))
    {
	name.did = d_.nil;
	tn = tdict;
    }
    if (desc = _visible_anyfix_op(IS_POSTFIX_OP, name.did, module.did, tm, &res))
    {
        Request_Unify_Integer(vp, tp, GetOpiPreced(desc));
	Request_Unify_Atom(assoc, ta, didassoc[GetOpiAssoc(desc)]);
	Request_Unify_Atom(vv, tv,
			    (res == LOCAL_PROP ? d_.local0 : d_.global0));
	Return_Unify;
    }
    Fail_;
}

/*
	is_infix_op(Precedence, Associativity, Name, Visib, Module)
	Name and Module must be instantiated.
	Associativity and Precedence are either instantiated or a variable.
	If there is an operator of this type in the operator table, 
	It succeeds and instantiates the precedence.
*/
/*ARGSUSED*/ /* check is already made in p_illegal_current_op		*/
static int
p_is_infix_op(value vp, type tp, value assoc, type ta, value name, type tn, value vv, type tv, value module, type tm)
{
    opi    	*desc;
    int		res;
    Prepare_Requests

    if (IsNil(tn))
    {
	name.did = d_.nil;
	tn = tdict;
    }
    if (desc = _visible_anyfix_op(IS_INFIX_OP, name.did, module.did, tm, &res))
    {
        Request_Unify_Integer(vp, tp, GetOpiPreced(desc));
	Request_Unify_Atom(assoc, ta, didassoc[GetOpiAssoc(desc)]);
	Request_Unify_Atom(vv, tv,
			    (res == LOCAL_PROP ? d_.local0 : d_.global0));
	Return_Unify;
    }
    Fail_;
}
