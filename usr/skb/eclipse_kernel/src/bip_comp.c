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
 * VERSION	$Id: bip_comp.c,v 1.2 2008/07/10 01:08:46 jschimpf Exp $
 */

/****************************************************************************
 *
 *		SEPIA Built-in Predicates: Term comparison
 *
 *
 *	name		C func		type
 *	----------------------------------------------------------------
 *	=/2				B_EXPANDED	in emu.c
 *	\=/2				B_EXPANDED	in emu.c
 *	==/2				B_EXPANDED	in emu.c
 *	\==/2				B_EXPANDED	in emu.c
 *	@</2		p_termless	B_FUNCTION
 *	@=</2		p_termlesseq	B_FUNCTION
 *	@>/2		p_termgreater	B_FUNCTION
 *	@>=/2		p_termgreatereq	B_FUNCTION
 *	compare/3	p_compare	B_UNSAFE	Quintus-compatible
 *	occurs/2	p_occurs	B_UNSAFE
 *	variant/2	p_variant	B_UNSAFE
 *	instance/2	p_instance	B_UNSAFE
 *	nonground/1	p_nonground	B_FUNCTION
 *
 *****************************************************************************/

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include 	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include 	"dict.h"
#include 	"opcode.h"
#include 	"emu_export.h"		/* to perform a binding */


#define LESS		-1
#define EQUAL		 0
#define GREATER		 1
#define TRUE		1
#define FALSE		0
#define Set_Bit(mask,pw)	(pw)->tag.kernel |= (mask);
#define Clr_Bit(mask,pw)	(pw)->tag.kernel &= ~(mask);
#define Marked(tag)		((tag).kernel & MARK)


static int	p_termless(value v1, type t1, value v2, type t2),
		p_termlesseq(value v1, type t1, value v2, type t2),
		p_termgreater(value v1, type t1, value v2, type t2),
		p_termgreatereq(value v1, type t1, value v2, type t2),
		p_unify(value v1, type t1, value v2, type t2, value vl, type tl),
		p_acyclic_term(value v, type t),
		p_compare(value vr, type tr, value v1, type t1, value v2, type t2),
		p_ground(value v, type t),
		p_nonground(value v, type t),
		p_occurs(value vs, type ts, value vt, type tt),
		p_variant_simple(value v1, type t1, value v2, type t2),
		p_instance_simple(value v1, type t1, value v2, type t2),
		p_compare_instances(value vr, type tr, value v1, type t1, value v2, type t2),
		p_compare_instances4(value vr, type tr, value v1, type t1, value v2, type t2, value vl, type tl),
		p_merge5(value vk, type tk, value vo, type to, value v1, type t1, value v2, type t2, value v, type t),
		p_number_merge5(value vk, type tk, value vo, type to, value v1, type t1, value v2, type t2, value v, type t),
		p_sort4(value vk, type tk, value vo, type to, value v1, type t1, value v2, type t2),
		p_number_sort4(value vk, type tk, value vo, type to, value v1, type t1, value v2, type t2);

static int	_instance(int rel, value v1, type t1, value v2, type t2, pword *meta);


void
bip_comp_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	(void) built_in(d_.less,		p_termless,	B_SAFE);
	(void) built_in(d_.lessq,		p_termlesseq,	B_SAFE);
	(void) built_in(d_.greater,		p_termgreater,	B_SAFE);
	(void) built_in(d_.greaterq,		p_termgreatereq,B_SAFE);
	exported_built_in(in_dict("unify", 3), p_unify,
	    B_UNSAFE|U_UNIFY) -> mode =
	    BoundArg(1, NONVAR) | BoundArg(2, NONVAR) | BoundArg(3, NONVAR);
	built_in(in_dict("compare", 3),	p_compare,	B_UNSAFE|U_SIMPLE)
	    -> mode = BoundArg(1, CONSTANT);
	built_in(in_dict("compare_instances", 3),
				p_compare_instances,	B_UNSAFE|U_SIMPLE)
	    -> mode = BoundArg(1, CONSTANT);
	exported_built_in(in_dict("compare_instances", 4),
				p_compare_instances4,	B_UNSAFE|U_UNIFY)
	    -> mode = BoundArg(1, CONSTANT) |
			BoundArg(4, NONVAR);
	(void) built_in(in_dict("occurs", 2),	p_occurs,	B_UNSAFE);
	(void) exported_built_in(in_dict("variant_simple", 2),p_variant_simple,	B_UNSAFE);
	(void) exported_built_in(in_dict("instance_simple", 2),p_instance_simple,	B_UNSAFE);
	(void) built_in(d_.nonground,		p_nonground,	B_SAFE);
	(void) built_in(d_.ground,		p_ground,	B_SAFE);
	(void) built_in(in_dict("acyclic_term",1),	p_acyclic_term,	B_SAFE);
	built_in(in_dict("merge", 5), 	p_merge5, 	B_UNSAFE|U_UNIFY)
	    -> mode = BoundArg(3, NONVAR) | BoundArg(4, NONVAR) | BoundArg(5, NONVAR);
	built_in(in_dict("number_merge", 5), 	p_number_merge5, 	B_UNSAFE|U_UNIFY)
	    -> mode = BoundArg(3, NONVAR) | BoundArg(4, NONVAR) | BoundArg(5, NONVAR);
	built_in(in_dict("sort", 4), 	p_sort4, 	B_UNSAFE|U_UNIFY)
	    -> mode = BoundArg(3, NONVAR) | BoundArg(4, NONVAR);
	built_in(in_dict("number_sort", 4), 	p_number_sort4, 	B_UNSAFE|U_UNIFY)
	    -> mode = BoundArg(3, NONVAR) | BoundArg(4, NONVAR);
    }
}


/*
 * @</2 - term ordering
 */
static int 
p_termless(value v1, type t1, value v2, type t2)
{
	Succeed_If(compare_terms(v1, t1, v2, t2) < 0);
}

/*
 * @=</2 - term ordering
 */
static int 
p_termlesseq(value v1, type t1, value v2, type t2)
{
	Succeed_If(compare_terms(v1, t1, v2, t2) <= 0);
}

/*
 * @>/2 - term ordering
 */
static int
p_termgreater(value v1, type t1, value v2, type t2)
{
	Succeed_If(compare_terms(v1, t1, v2, t2) > 0);
}

/*
 * @>=/2 - term ordering
 */
static int
p_termgreatereq(value v1, type t1, value v2, type t2)
{
	Succeed_If(compare_terms(v1, t1, v2, t2) >= 0);
}

/*
 * unify(Term1, Term2, List)
 *
 *	Unify the two terms and return in List the list of metaterms
 *	and their mates encountered during the unification
 */
static int
p_unify(value v1, type t1, value v2, type t2, value vl, type tl)
{
    pword	*list = (pword *) 0;
    int		res;

    res = ec_unify_(v1, t1, v2, t2, &list);
    if (res == PSUCCEED) {
	if (list == (pword *) 0) {
	    Return_Unify_Nil(vl, tl)
	} else {
	    Return_Unify_List(vl, tl, list)
	}
    } else
	return res;
}

/*
 *	compare(Res, Term1, Term2)
 *		Res == '<' iff Term1 @< Term2
 *		Res == '>' iff Term1 @> Term2
 *		Res == '=' iff Term1 == Term2
 *	Quintus-compatible, hence no errors.
 */
static int
p_compare(value vr, type tr, value v1, type t1, value v2, type t2)
{
	int             i;
	dident 		res;

	i = compare_terms(v1, t1, v2, t2);
	if (i < 0)
		res = d_.inf0;
	else if (i > 0)
		res = d_.sup0;
	else
		res = d_.unify0;

	Return_Unify_Atom(vr,tr,res);
}


/*
 * compare two sepia strings, given their value parts
 * return value is like strcmp()
 */
int
compare_strings(value v1, value v2)
{
    register long		len = StringLength(v1);
    register unsigned char	*s1 = (unsigned char *) StringStart(v1);
    register unsigned char	*s2 = (unsigned char *) StringStart(v2);
    register int		res;

    if (len > StringLength(v2))
	len = StringLength(v2);
    while (len--)
	if (res = *s1++ - *s2++)
	    return res;

    return StringLength(v1) - StringLength(v2);
}

/*
 * compare two Prolog terms, returns <0 if T1 < T2, 0 if T1 = T2, >0 if T1 > T2
 */
int
compare_terms(value v1, type t1, value v2, type t2)
{
	dident          wdid, wdid2;
	pword		*arg1, *arg2;
	int		arity, res;

_compare_loop_:
	if (IsRef(t1))
	{
	    return IsRef(t2) ? v1.ptr - v2.ptr : LESS;
	}
	else if (IsRef(t2))
	{
	    return GREATER;
	}
	else if (res = tag_desc[TagType(t1)].order - tag_desc[TagType(t2)].order)
	{
	    return res;		/* types are ordered */
	}
	else			/* compare the values */
	{
	    double d1, d2;

	    switch (TagType(t1))
	    {
	    case TINT:
		if (IsTag(t2.kernel,TINT))	/* TINT x TINT */
		{
		    return v1.nint < v2.nint ? LESS :
			v1.nint > v2.nint ? GREATER : 0;
		}
		/* else fall through */

	    case TBIG:
		/* this case handles TINT x TBIG, TBIG x TINT, TBIG x TBIG */
		(void) arith_compare(v1, t1, v2, t2, &res);
		return res;

	    case TSTRG:
		return compare_strings(v1, v2);

	    case TNIL:
		return IsNil(t2) ? EQUAL :
			strcmp(DidName(d_.nil), DidName(v2.did));

	    case TDICT:
		return IsNil(t2) ? strcmp(DidName(v1.did), DidName(d_.nil)) :
			strcmp(DidName(v1.did), DidName(v2.did));

	    case TLIST:
		if (IsList(t2))
		{
		    if (v1.ptr == v2.ptr)
			return EQUAL;
		    arity = 2;
		    goto _compare_args_;
		}
		else	/* TCOMP */
		{
		    wdid2 = v2.ptr->val.did;	/* wdid2 != d_.list */
		    arity = DidArity(wdid2);
		    if (2 != arity)
			return 2 - arity;
		    else
			return strcmp(DidName(d_.list), DidName(wdid2));
		}

	    case TCOMP:
		if (IsList(t2))
		{
		    wdid = v1.ptr->val.did; /* wdid != d_.list */
		    arity = DidArity(wdid);
		    if (arity != 2)
			return arity -2;
		    else
			return strcmp(DidName(wdid), DidName(d_.list));
		}
		else	/* TCOMP */
		{
		    if (v1.ptr == v2.ptr)
			return EQUAL;
		    wdid = (v1.ptr++)->val.did;
		    arity = DidArity(wdid);
		    wdid2 = (v2.ptr++)->val.did;
		    if (wdid != wdid2)
			if (arity != DidArity(wdid2))
			    return arity - DidArity(wdid2);
			else
			   return strcmp(DidName(wdid), DidName(wdid2));
		    if (arity == 0)
			return EQUAL;
_compare_args_:						/* (arity, v1, v2) */
		    for(;;)
		    {
			arg1 = v1.ptr++;
			arg2 = v2.ptr++;
			Dereference_(arg1)
			Dereference_(arg2)
			if (--arity == 0)
			    break;
			res = compare_terms
				(arg1->val, arg1->tag,
				 arg2->val, arg2->tag);
			if (res != EQUAL)
			    return res;
		    }
		    /* remove tail recursion */
		    v1.all = arg1->val.all;
		    t1.all = arg1->tag.all;
		    v2.all = arg2->val.all;
		    t2.all = arg2->tag.all;
		    goto _compare_loop_;
		}

	    default:
		return tag_desc[TagType(t1)].compare(v1, v2);
	    }
	}
}


/*
 * MU-Prolog's occurs/2: occurs(Simple, Term) is true if Simple is a variable
 * or a constant and it occurs in the term Term.
 */
static int
p_occurs(value vs, type ts, value vt, type tt)
{
	if (!IsRef(ts) && !IsDouble(ts) && !IsSimple(ts))
	{
		Bip_Error(TYPE_ERROR);
	}
	Succeed_If(ec_occurs(vs, ts, vt, tt));
}

/* returns true if the first (simple) term occurs in the second one */
int
ec_occurs(value vs, type ts, value vterm, type tterm)
{
	int		arity;
	pword		*arg;

    for(;;)
    {
	if (IsRef(tterm))
		return (IsRef(ts) && vs.all == vterm.all);
	switch (TagType(tterm))
	{
	case TCOMP:
		arity = DidArity(vterm.ptr->val.did); 
		vterm.ptr++;
		break;

	case TLIST:
		arity = 2;
		break;

	case TNIL:
		return (IsNil(ts));

	case TSTRG:
		return IsString(ts) && !compare_strings(vs, vterm);

	case TDBL:
		return IsDouble(ts) && Dbl(vs) == Dbl(vterm);

	default:
		return SameType(ts, tterm) && SimpleEq(ts.kernel, vs, vterm);
	}

	for (; arity > 1; arity--)
	{
		arg = vterm.ptr++;
		Dereference_(arg);
		if (ec_occurs(vs, ts, arg->val, arg->tag))
			return 1;
	}
	arg = vterm.ptr;	/* tail recursion optimised */
	Dereference_(arg);
	vterm = arg->val;
	tterm = arg->tag;
    }
}

#ifdef OC
/* returns true if the first (compound) term occurs in the second one */
int
occurs_compound(pword *comp, pword *term)
{
	int		arity;
	pword		*arg;

    for(;;)
    {
	switch (TagType(term->tag))
	{
	case TCOMP:
		if (comp == term)
			return 1;
		term = term->val.ptr;
		arity = DidArity(term->val.did); 
		term++;
		break;

	case TLIST:
		if (comp == term)
			return 1;
		arity = 2;
		term = term->val.ptr;
		break;

	default:
		return 0;
	}

	for (; arity > 1; arity--)
	{
		arg = term++;
		Dereference_(arg);
		if (occurs_compound(comp, arg))
			return 1;
	}
	/* tail recursion optimised */
	Dereference_(term);
    }
}
#endif

/*
 * variant(Term1, Term2)
 * instance(Instance, Term)
 * compare_instances(Rel, Term1, Term2)
 *
 * Uses the common routine _instance(), which does the work in a single
 * pass through the two terms. The complexity is linear in the size of
 * the larger term. Failures are detected early (the return value is 0).
 *
 * Algorithm:
 *	var-var pairs: bind the variables together and instantiate
 *		with a unique constant (TVARNUM with self-ref)
 *	var-nonvar pairs: bind the variable to the nonvariable. Such a
 *		binding means that one term is more general than the other.
 *		Therefore, for variant test, it causes failure, for instance
 *		test only variables on one side may be bound.
 *		The nonvariable term is instantiated with TVARNUMs.
 *
 * The results may be counter-intuitive when the two terms share variables.
 * Our exact definition is: A term subsumes another one iff by binding some
 * of its variables it can be made to unify with the other one (the instance).
 * e.g. the following succeed:
 * 
 *	instance(s(Y, X), s(X, Y))	with substitution X=Y
 *	instance(s(a, X), s(X, X))	with substitution X=a
 *
 *	instance(f(X), X)	succeeds iff occur check disabled
 */

#define ANY_INST	7
#define LT		4
#define EQ		2
#define GT		1

static int
p_variant_simple(value v1, type t1, value v2, type t2)
{
	int		code;
	pword		**save_tt = TT;

	code = _instance(EQ, v1, t1, v2, t2, (pword *) 0);
	Untrail_Variables(save_tt);
	Succeed_If(code);
}

static int
p_instance_simple(value v1, type t1, value v2, type t2)
{
	int		code;
	pword		**save_tt = TT;

	code = _instance(LT, v1, t1, v2, t2, (pword *) 0);
	Untrail_Variables(save_tt);
	Succeed_If(code);
}

/*
 *	compare_instances(Res, Term1, Term2)
 *	compare_instances(Res, Term1, Term2, MetaList)
 *		Res == '<' iff Term1 is an instance of Term2
 *		Res == '>' iff Term2 is an instance of Term1
 *		Res == '=' iff Term1 is a variant of Term2
 *	fails if none of the above applies
 */
static int
p_compare_instances(value vr, type tr, value v1, type t1, value v2, type t2)
{
	type	tdummy;
	tdummy.kernel = TEND;
	return p_compare_instances4(vr, tr, v1, t1, v2, t2, v2, tdummy);
}

static int
p_compare_instances4(value vr, type tr,
	value v1, type t1,
	value v2, type t2,
	value vl, type tl)	/* when tl == TEND we don't want the list */
{
	int             code;
	dident 		res;
	pword		list;
	pword		**save_tt = TT;

	list.tag.kernel = TNIL;

	if (IsRef(tr))
	{
	    code = _instance(ANY_INST,v1,t1,v2,t2, tl.kernel==TEND ? 0 : &list);
	    if (code == 0)
		{ Fail_; }
	    if (code & EQ)
		res = d_.unify0;
	    else if (code & LT)
		res = d_.inf0;
	    else /* if (code & GT) */
		res = d_.sup0;
	    Untrail_Variables(save_tt);
	    Bind_Var(vr, tr, res, TDICT)
	}
	else if (IsAtom(tr))
	{
	    if (vr.did == d_.unify0)
	    {
		if (!_instance(EQ,v1,t1,v2,t2, tl.kernel==TEND ? 0 : &list))
		    { Fail_; }
	    }
	    else if (vr.did == d_.inf0)
	    {
		code = _instance(EQ|LT,v1,t1,v2,t2, tl.kernel==TEND? 0: &list);
		if (code != LT) {Fail_; }
	    }
	    else if (vr.did == d_.sup0)
	    {
		code = _instance(EQ|GT,v1,t1,v2,t2, tl.kernel==TEND? 0: &list);
		if (code != GT) {Fail_; }
	    }
	    else
		{ Bip_Error(RANGE_ERROR); }
	    Untrail_Variables(save_tt);
	}
	else
	    { Bip_Error(TYPE_ERROR); }

	if (tl.kernel==TEND)
	{
	    Succeed_
	}
	Return_Unify_Pw(vl, tl, list.val, list.tag)
}

/*
 * Instantiate all variables in a term to unique terms. It is like
 * numbervars(), but it uses terms with the special tag TVARNUM.
 */
static void
_instantiate(value v1, type t1)
{
    int		arity;
    pword	*arg1;

    for (;;)
    {
	if (IsRef(t1))
	{
	    if (IsVar(t1)) Trail_(v1.ptr) else Trail_Tag(v1.ptr);
	    v1.ptr->tag.kernel = TVARNUM;
	    return;
	}
	else if (IsStructure(t1))
	{
	    arity = DidArity(v1.ptr->val.did);
	    v1.ptr++;
	}
	else if (IsList(t1))
	    arity = 2;
	else
	    return;

	for (;;)
	{
	    arg1 = v1.ptr++;
	    Dereference_(arg1);
	    if (--arity == 0)
		break;
	    _instantiate(arg1->val, arg1->tag);
	}
	v1.all = arg1->val.all;		/* tail recursion */
	t1.all = arg1->tag.all;
    }
}


/*
 * General instance check
 * Untrail after calling!
 */

static int
_instance(int rel,		/* relation type asked for */
	value v1, type t1,
	value v2, type t2,
	pword *meta)		/* output list of meta pairs */
{
    int		arity;
    pword	*arg1, *arg2;

    for (;;)
    {
	if (meta && (IsMeta(t1) || IsMeta(t2)))	/* make list of meta pairs */
	{
	    arg1 = TG;
	    TG += 4;
	    Check_Gc
	    arg1[0].val.ptr = v1.ptr;
	    arg1[0].tag.kernel = IsTag(t1.kernel, TVARNUM) ? TREF : t1.kernel;
	    arg1[1].val.ptr = v2.ptr;
	    arg1[1].tag.kernel = IsTag(t2.kernel, TVARNUM) ? TREF : t2.kernel;
	    arg1[2].val.ptr = arg1;
	    arg1[2].tag.kernel = TLIST;
	    arg1[3] = *meta;
	    meta->val.ptr = &arg1[2];
	    meta->tag.kernel = TLIST;
	}
	if (IsRef(t1))
	{
	    if (IsRef(t2))		/* var - var */
	    {
		if (v1.ptr != v2.ptr)
		{
		    if (IsVar(t1)) Trail_(v1.ptr) else Trail_Tag(v1.ptr);
		    if (IsVar(t2)) Trail_(v2.ptr) else Trail_Tag(v2.ptr);
		    v1.ptr->tag.kernel = TVARNUM;
		    v2.ptr->tag.kernel = TREF;
		    v2.ptr->val.ptr = v1.ptr;
		}
		return rel;
	    }
	    else			/* var - nonvar */
	    {
		/* Ground the term we bind to in order to make sure that
		 * the variables inside are not bound later by mistake.
		 * This also makes simple occur check possible.
		 */
		_instantiate(v2, t2);
		if (!IsTag(v1.ptr->tag.kernel, TVARNUM))
		{
		    if (IsVar(t1)) Trail_(v1.ptr) else Trail_Tag(v1.ptr);
		}
		else if (OccurCheckEnabled())
		    return 0;
		/* t1 is now trailed, bind it */
		v1.ptr->val.all = v2.all;
		v1.ptr->tag.all = t2.all;
		return rel & ~(LT|EQ);
	    }
	}
	else if (IsRef(t2))		/* nonvar - var */
	{
	    /* see comment above */
	    _instantiate(v1, t1);
	    if (!IsTag(v2.ptr->tag.kernel, TVARNUM))
	    {
		if (IsVar(t2)) Trail_(v2.ptr) else Trail_Tag(v2.ptr);
	    }
	    else if (OccurCheckEnabled())
		return 0;
	    /* t2 is now trailed, bind it */
	    v2.ptr->val.all = v1.all;
	    v2.ptr->tag.all = t1.all;
	    return rel & ~(GT|EQ);
	}
	else if (IsTag(t1.kernel, TVARNUM))
	{
	    if (IsTag(t2.kernel, TVARNUM) && v1.ptr == v2.ptr)
	    {
		return rel;
	    }
	    if (OccurCheckEnabled() && ec_occurs(v1, t1, v2, t2))
		return 0;
	    /* t1 is already trailed */
	    v1.ptr->val.all = v2.all;
	    v1.ptr->tag.all = t2.all;
	    return rel & ~(GT|EQ|LT);	/* not instances, but still unify */
	}
	else if (IsTag(t2.kernel, TVARNUM))
	{
	    if (OccurCheckEnabled() && ec_occurs(v2, t2, v1, t1))
		return 0;
	    /* t2 is already trailed */
	    v2.ptr->val.all = v1.all;
	    v2.ptr->tag.all = t1.all;
	    return rel & ~(GT|EQ|LT);	/* not instances, but still unify */
	}

	switch (TagType(t1))
	{
	case TLIST:
	    if (!IsTag(t2.kernel, TLIST))
		return 0;
	    arity = 2;
	    break;

	case TCOMP:
	    if (!IsTag(t2.kernel, TCOMP) || v1.ptr->val.did != v2.ptr->val.did)
		return 0;
	    arity = DidArity(v1.ptr->val.did);
	    v1.ptr++;
	    v2.ptr++;
	    break;

	case TSTRG:
	    return (IsString(t2) && !compare_strings(v1, v2)) ? rel : 0;

	case TINT:
	case TDICT:
	case TNIL:
	case TPTR:
#ifdef UNBOXED_DOUBLES
	case TDBL:
#endif
	    return (SameType(t1, t2) && SimpleEq(t1.kernel, v1, v2)) ? rel : 0;

	default:
	    if (TagType(t1) >= 0 && TagType(t1) <= NTYPES)
	    {
		if (SameType(t1, t2) &&
			tag_desc[TagType(t1)].equal(v1.ptr, v2.ptr))
		    return rel;
		else
		    return 0;
	    }
	    p_fprintf(current_err_,
		"_instance(): unknown tag (%x) encountered\n", t1.kernel);
	    ec_flush(current_err_);
	    return 0;
	}

	if (v1.ptr == v2.ptr)		/* detect sharing */
	    return rel;

	for (;;)
	{
	    arg1 = v1.ptr++;
	    arg2 = v2.ptr++;
	    Dereference_(arg1);
	    Dereference_(arg2);
	    if (--arity == 0)
		break;
	    rel = _instance(rel, arg1->val, arg1->tag,
			    arg2->val, arg2->tag, meta);

	    if (rel == 0)		/* fail early */
		return rel;
	}

	v1.all = arg1->val.all;		/* tail recursion */
	t1.all = arg1->tag.all;
	v2.all = arg2->val.all;
	t2.all = arg2->tag.all;
    }
}


/*
	nonground/1
	succeeds if the term is not fully instantiated
*/

static int
p_nonground(value v, type t)
{
    Succeed_If(ec_nonground(v, t))
}

int
p_ground(value v, type t)
{
    Succeed_If(!ec_nonground(v, t))
}


/*
 * Check if a term is cyclic. We mark the target of every TLIST or TCOMP
 * pointer, and if we encouter it withing its descendants, we know we have
 * a cycle and stop. This algorithm is very naive and simple. It is not
 * tail recursive and therefore may nest deeply. It also does not detect
 * shared (already traversed) subtrees, and thus traverses them again.
 */

static int
_cyclic_term(value val, type tag)	/* expects a dereferenced argument */
          
{
    pword *arg_i;
    int arity;

    if (IsList(tag))
    {
	if (val.ptr->tag.kernel & MARK)
	    return 1;
	arity = 2;
	arg_i = val.ptr;
    }
    else if (IsStructure(tag))
    {
	if (val.ptr->tag.kernel & MARK)
	    return 1;
	arity = DidArity(val.ptr->val.did);
	arg_i = val.ptr + 1;
    }
    else
	return 0;

    val.ptr->tag.kernel |= MARK;
    for(; arity > 0; arity--,arg_i++)
    {
	pword *pw = arg_i;
	Dereference_(pw);
	if (IsCompound(pw->tag) && _cyclic_term(pw->val, pw->tag))
	{
	    val.ptr->tag.kernel &= ~MARK;
	    return 1;
	}
    }
    val.ptr->tag.kernel &= ~MARK;
    return 0;
}

static int
p_acyclic_term(value v, type t)
{
    Succeed_If(!_cyclic_term(v, t));
}



/*
 * FUNCTION NAME:       p_sort4()
 *
 * PARAMETERS:          vk,tk	sorting key, if 0 the whole term is the key
 *			vo,to	one of the atoms <,=<,>,>=
 *			v1,t1	a list or nil (the input list)
 *			v2,t2	list, nil or variable (the sorted list)
 *
 * DESCRIPTION:         sort(+Key, +Order, +Random, ?Sorted)
 *			The sorting method is natural merge. It takes advantage
 *			of existing order or reverse order in the input list.
 *			The worst case time complexity is n*log(n).
 *			Space on the global stack is only needed for the
 *			resulting list. The sort is stable, ie. if the input
 *			list contains elements with the equal keys, their
 *			order in the output list is the same as in the input
 *			list. This is important if we want to (key)sort a list
 *			according to multiple keys.
 */

#define ASCENDING	1
#define DESCENDING	(-1)

static int
p_sort4(value vk, type tk, value vo, type to, value v1, type t1, value v2, type t2)
{
    register pword 	*list;
    register int	reverse, keep_duplicates;
    int			err;

    Check_Output_List(t2);	/* type checks	*/
    Check_List(t1);
    Check_Atom(to);

    if(IsInteger(tk) && vk.nint < 0)	/* range checks	*/
    {
	Bip_Error(RANGE_ERROR)
    }

    if(vo.did == d_.inf0) {
	reverse = FALSE;
	keep_duplicates = FALSE;
    } else if(vo.did == d_.infq0) {
	reverse = FALSE;
	keep_duplicates = TRUE;
    } else if(vo.did == d_.sup0) {
	reverse = TRUE;
	keep_duplicates = FALSE;
    } else if(vo.did == d_.supq0) {
	reverse = TRUE;
	keep_duplicates = TRUE;
    } else {
	Bip_Error(RANGE_ERROR)
    }

    if(IsNil(t1))		/* empty list -> return []	*/
    {
	Return_Unify_Nil(v2, t2)
    }
    list = ec_keysort(v1, vk, tk, reverse, keep_duplicates, FALSE, &err);
    if (!list) {
	Bip_Error(err)
    } else {
	Return_Unify_List(v2, t2, list);
    }
}


/*
 * FUNCTION NAME:       p_number_sort4()
 *
 * PARAMETERS:          vk,tk	sorting key, if 0 the whole term is the key
 *			vo,to	one of the atoms <,=<,>,>=
 *			v1,t1	a list or nil (the input list)
 *			v2,t2	list, nil or variable (the sorted list)
 *
 * DESCRIPTION:         sort(+Key, +Order, +Random, ?Sorted)
 *			The sorting method is natural merge. It takes advantage
 *			of existing order or reverse order in the input list.
 *			The worst case time complexity is n*log(n).
 *			Space on the global stack is only needed for the
 *			resulting list. The sort is stable, ie. if the input
 *			list contains elements with the equal keys, their
 *			order in the output list is the same as in the input
 *			list. This is important if we want to (key)sort a list
 *			according to multiple keys.
 */

static int
p_number_sort4(value vk, type tk, value vo, type to, value v1, type t1, value v2, type t2)
{
    register pword 	*list;
    register int	reverse, keep_duplicates;
    int			err;

    Check_Output_List(t2);	/* type checks	*/
    Check_List(t1);
    Check_Atom(to);

    if(IsInteger(tk) && vk.nint < 0)	/* range checks	*/
    {
	Bip_Error(RANGE_ERROR)
    }

    if(vo.did == d_.inf0) {
	reverse = FALSE;
	keep_duplicates = FALSE;
    } else if(vo.did == d_.infq0) {
	reverse = FALSE;
	keep_duplicates = TRUE;
    } else if(vo.did == d_.sup0) {
	reverse = TRUE;
	keep_duplicates = FALSE;
    } else if(vo.did == d_.supq0) {
	reverse = TRUE;
	keep_duplicates = TRUE;
    } else {
	Bip_Error(RANGE_ERROR)
    }

    if(IsNil(t1))		/* empty list -> return []	*/
    {
	Return_Unify_Nil(v2, t2)
    }
    list = ec_keysort(v1, vk, tk, reverse, keep_duplicates, TRUE, &err);
    if (!list) {
	Bip_Error(err)
    } else {
	Return_Unify_List(v2, t2, list);
    }
}


/*
 * Return a dereferenced pointer to argument k (whole term if 0)
 * of term pw.  On error, return NULL and error code in *perr.
 */
static inline pword *
_get_key(pword *pw, value vk, type tk, int *perr)
{
    Dereference_(pw);
    if (!IsInteger(tk) || vk.nint != 0)
    {
	pword *ec_chase_arg(value vn, type tn, value vt, type tt, int *perr);

	if (pw = ec_chase_arg(vk, tk, pw->val, pw->tag, perr))
	{
	    Dereference_(pw);
	}
    }
    return pw;
}


pword *
ec_keysort(value v1, value vk, type tk, int reverse, int keep_duplicates, int number_sort, int *err)
{
    register pword 	*h1, *h2, *comp_ptr, *append;
    pword 		*key_ptr1, *key_ptr2, *old_tg, *next_append;
    pword		list1, list2;
    int         	comp, sequence;

    old_tg = Gbl_Tg;		/* to reset TG on errors	*/

    /*
     * We first split the list (v1, t1) into two lists list1 and list2.
     * The list cells are copied, the elements and tails of the
     * copied lists are dereferenced.
     */

    h1 = v1.ptr;
    append = &list1;
    next_append = &list2;
    h2 = Gbl_Tg;
    Gbl_Tg +=2;
    Check_Gc;
    comp_ptr = h1;
    Dereference_(comp_ptr);
    if (!(key_ptr1 = _get_key(comp_ptr, vk, tk, err)))
    {
	TG = old_tg;
	return 0;
    }
    if (number_sort && !IsNumber(key_ptr1->tag))
    {
        TG = old_tg;
	*err = IsRef(key_ptr1->tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
	return 0;
    }
    *h2 = *comp_ptr;
    sequence = 0;
    h1++;
    Dereference_(h1);
    while(! IsRef(h1->tag) && IsList(h1->tag))
    {
	h1 = h1->val.ptr;
	comp_ptr = h1;
	Dereference_(comp_ptr);
	if (!(key_ptr2 = _get_key(comp_ptr, vk, tk, err)))
	{
	    TG = old_tg;
	    return 0;
	}
	if (number_sort)
	{
	    if (!IsNumber(key_ptr2->tag))
	    {
	        TG = old_tg;
		*err = IsRef(key_ptr2->tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
		return 0;
	    }
	    comp = BIEq;
	    *err = arith_compare(key_ptr1->val, key_ptr1->tag,
				 key_ptr2->val, key_ptr2->tag, &comp);
	    if (*err == PDELAY)
	    {
		Gbl_Tg = old_tg;
		*err = ARITH_EXCEPTION;
		return 0;
	    }
	} else {
	    comp = compare_terms(key_ptr1->val, key_ptr1->tag,
				 key_ptr2->val, key_ptr2->tag); 
	}
	key_ptr1 = key_ptr2;
	if(reverse)
	    comp = -comp;
	/*
	 * To make the sort stable, we must treat elements with equal keys
	 * as an ascending sequence.
	 */
	if(comp || keep_duplicates)
	{
	    Gbl_Tg += 2;
	    Check_Gc;
	    *(Gbl_Tg - 2) = *comp_ptr;
	    if(! sequence)
		if(comp <= 0)
		    sequence = ASCENDING;
		else
		    sequence = DESCENDING;
	    else if((comp > 0) &&  (sequence == ASCENDING))
	    {
		/* end of ascending sequence */
		append->tag.kernel = TLIST | MARK;
		append->val.ptr = h2;
		while(h2 < (Gbl_Tg - 4))
		{
		    h2 += 2;
		    (h2-1)->tag.kernel = TLIST;
		    (h2-1)->val.ptr = h2;
		}
		append = next_append;
		next_append = h2 + 1;
		h2 = Gbl_Tg - 2;
		sequence = 0;
	    }
	    else if ((comp <= 0) && (sequence == DESCENDING))
	    {
		/* end of descending sequence */
		append->tag.kernel = TLIST | MARK;
		append->val.ptr = (Gbl_Tg - 4);
		comp_ptr = Gbl_Tg - 3;
		while(comp_ptr > h2 + 2)
		{
		    comp_ptr->tag.kernel = TLIST;
		    comp_ptr->val.ptr = comp_ptr - 3;
		    comp_ptr -= 2;
		}
		append = next_append;
		next_append = comp_ptr;
		h2 = Gbl_Tg - 2;
		sequence = 0;
	    }
	}
	h1++;
	Dereference_(h1);
    }  	/* while(! IsRef(h1->tag) && IsList(h1->tag)) */

    if(IsRef(h1->tag))
    {
	Gbl_Tg = old_tg;
	*err = INSTANTIATION_FAULT;
	return 0;
    }
    else if(! IsNil(h1->tag))
    {
	Gbl_Tg = old_tg;
	*err = TYPE_ERROR;
	return 0;
    }
    if(sequence != DESCENDING)
    {
	append->tag.kernel = TLIST | MARK;
	append->val.ptr = h2;
	while(h2 < (Gbl_Tg - 2))
	{
	    h2 += 2;
	    (h2-1)->tag.kernel = TLIST;
	    (h2-1)->val.ptr = h2;
	}
	(Gbl_Tg - 1)->tag.kernel = TNIL;
	append = (Gbl_Tg - 1);
	next_append->tag.kernel = TNIL;
    }
    else 
    {
	append->tag.kernel = TLIST | MARK;
	append->val.ptr = (Gbl_Tg - 2);
	comp_ptr = Gbl_Tg - 1;
	while(comp_ptr > h2 + 2)
	{
		comp_ptr->tag.kernel = TLIST;
		comp_ptr->val.ptr = comp_ptr - 3;
		comp_ptr -=2;
	}
	comp_ptr->tag.kernel = TNIL;
	append = comp_ptr;
	next_append->tag.kernel = TNIL;
    }
    if(IsNil(list2.tag))
    	return list1.val.ptr;

    Set_Bit(MARK, append);
    Set_Bit(MARK, next_append);

    /*
     * Start merging:
     * We have two non-empty list in list1 and list2. They consist of
     * ascending sequences. The end of every sequence is MARKed.
     * list2 has the same number of sequences as list1 or one less.
     */
    do
    {
	append = &list1;
	next_append = &list2;
	h1 = list1.val.ptr;
	h2 = list2.val.ptr;

	do
	{   /* merge lists h1 and h2, appending the result at append */
	    for(;;)
	    {
		/* no need to check that key spec was OK for these terms */
		key_ptr1 = _get_key(h1, vk, tk, err);
		key_ptr2 = _get_key(h2, vk, tk, err);
		if (number_sort)
		{
		    comp = BIEq;
		    *err = arith_compare(key_ptr1->val, key_ptr1->tag,
					 key_ptr2->val, key_ptr2->tag, &comp);
		    if (*err == PDELAY)
		    {
			Gbl_Tg = old_tg;
			*err = ARITH_EXCEPTION;
			return 0;
		    }
		} else {
		    comp = compare_terms(key_ptr1->val, key_ptr1->tag,
					 key_ptr2->val, key_ptr2->tag);
		}
		if(reverse)
		    comp = -comp;
		
		if (comp < 0 || ! comp && keep_duplicates)
		{
		    append->val.ptr = h1;	/* link element h1	*/
		    append = h1 + 1;
		    if (!Marked((h1+1)->tag))
		    {
			h1 = (h1+1)->val.ptr;
			continue;
		    }
		    /* end of sequence 1	*/
		    h1 = IsList((h1+1)->tag) ? (h1+1)->val.ptr : (pword *) 0;
		    append->tag.kernel = TLIST;	/* and reset mark	*/
		    append->val.ptr = h2;
		    while (!Marked((h2+1)->tag)) 
			h2 = (h2+1)->val.ptr;
		    append = h2 + 1;
		    h2 =  IsList(append->tag) ? append->val.ptr : (pword *) 0;
		}
		else if (comp > 0)
		{
		    append->val.ptr = h2;	/* link element h2	*/
		    append = h2 + 1;
		    if (!Marked((h2+1)->tag))
		    {
			h2 = (h2+1)->val.ptr;
			continue;
		    }
		    /* end of sequence 2	*/
		    h2 = IsList((h2+1)->tag) ? (h2+1)->val.ptr : (pword *) 0;
		    append->tag.kernel = TLIST;	/* and reset mark	*/
		    append->val.ptr = h1;
		    while (!Marked((h1+1)->tag))
			h1 = (h1+1)->val.ptr;
		    append = h1 + 1;
		    h1 =  IsList(append->tag) ? append->val.ptr : (pword *) 0;
		}
		else /* comp == 0 && !keep_duplicates */
		{
		    if (!Marked((h2+1)->tag))	/* skip element h2	*/
		    {
			h2 = (h2+1)->val.ptr;
			continue;
		    }
		    Clr_Bit(MARK, h2+1);
		    /* end of sequence 2	*/
		    h2 = IsList((h2+1)->tag) ? (h2+1)->val.ptr : (pword *) 0;
		    append->val.ptr = h1;
		    while (!Marked((h1+1)->tag))
			h1 = (h1+1)->val.ptr;
		    append = h1 + 1;
		    h1 =  IsList(append->tag) ? append->val.ptr : (pword *) 0;
		}
		break;
	    } /* for(;;) */

	    comp_ptr = append;
	    append = next_append;
	    next_append = comp_ptr;
	} while (h1 && h2);

	if (h1 /* && !h2 */)	/* a single sequence is left	*/
	{
	    append->tag.kernel = MARK|TLIST;
	    append->val.ptr = h1;
	    while (!Marked((h1+1)->tag))
		h1 = (h1+1)->val.ptr;
	    append = next_append;
	    next_append = h1 + 1;
	}
	append->tag.kernel = MARK|TNIL;

    } while (append != &list2);

    Clr_Bit(MARK, next_append);	/* no MARK bits may be left behind !	*/

#ifdef DEBUG_SORT

    /* check if the list is really sorted */

    h1 = list1.val.ptr;
    h2 = h1 + 1;

    while(IsList(h2->tag))
    {
	h2 = h2->val.ptr;
	/* no need to check that key spec was OK for these terms */
	key_ptr1 = _get_key(h1, vk, tk, err);
	key_ptr2 = _get_key(h2, vk, tk, err);
	comp = compare_terms(key_ptr1->val, key_ptr1->tag,
				    key_ptr2->val, key_ptr2->tag);
	if(reverse)
	    comp = -comp;
	if (comp > 0)
	{
	    p_fprintf(current_err_,"INTERNAL ERROR 1 in sort/4\n");
	    ec_flush(current_err_);
	}
	else if (comp == 0 && !keep_duplicates)
	{
	    p_fprintf(current_err_,"INTERNAL ERROR 2 in sort/4\n");
	    ec_flush(current_err_);
	}
	h1 = h2;
	h2 = h1 + 1;
    }
    if(!IsNil(h2->tag))
    {
	p_fprintf(current_err_,"INTERNAL ERROR 3 in sort/4\n");
	ec_flush(current_err_);
    }

    /* check if there are no mark bits left */

    for(h1 = old_tg; h1 < Gbl_Tg; h1++)
	if (Marked(h1->tag))
	{
	    p_fprintf(current_err_,"INTERNAL ERROR 4 in sort/4\n");
	    ec_flush(current_err_);
	}

#endif /* DEBUG_SORT */

    return list1.val.ptr;
}


/*
 * FUNCTION NAME:       p_merge5()
 *			p_number_merge5()
 *
 * PARAMETERS:          vk,tk	sorting key, if 0 the whole term is the key
 *			vo,to	one of the atoms <,=<,>,>=
 *			v1,t1	a list or nil (input list)
 *			v2,t2	a list or nil (input list)
 *			v,t	list, nil or variable (the merged list)
 *
 * DESCRIPTION:         merge(+Key, +Order, +List1, +List2, ?Merged)
 *			Merge two sorted lists. The input lists need
 *			to be already sorted according to the specified
 *			ordering, otherwise the result is undefined.
 *			When keys are identical, their original order within
 *			List1 or List2 should be preserved in Merged, and
 *			List1's elements should come before List2's elements.
 */

static int
_merge(value vk, type tk, value vo, type to,
	value v1, type t1, value v2, type t2, value v, type t,
	int number_sort)
{
    pword 	*old_tg = TG;
    pword 	*h1, *h2, *key_ptr1, *key_ptr2, *append;
    pword 	result;
    int		reverse, keep_duplicates, comp, err;

    Check_Output_List(t);	/* type checks	*/
    Check_List(t1);
    Check_List(t2);
    Check_Atom(to);

    if(IsInteger(tk) && vk.nint < 0)	/* range checks	*/
    {
	Bip_Error(RANGE_ERROR)
    }

    if(vo.did == d_.inf0) {	/* ordering options */
	reverse = FALSE;
	keep_duplicates = FALSE;
    } else if(vo.did == d_.infq0) {
	reverse = FALSE;
	keep_duplicates = TRUE;
    } else if(vo.did == d_.sup0) {
	reverse = TRUE;
	keep_duplicates = FALSE;
    } else if(vo.did == d_.supq0) {
	reverse = TRUE;
	keep_duplicates = TRUE;
    } else {
	Bip_Error(RANGE_ERROR)
    }

    if (IsNil(t1))
    {
    	Return_Unify_Pw(v2, t2, v, t);
    }
    else if (IsNil(t2))
    {
    	Return_Unify_Pw(v1, t1, v, t);
    }

    append = &result;
    h1 = v1.ptr;
    h2 = v2.ptr;
    if (!(key_ptr1 = _get_key(h1, vk, tk, &err))
     || !(key_ptr2 = _get_key(h2, vk, tk, &err)))
    {
	goto _merge_error_;
    }

    for(;;)	/* (h1, key_ptr1, h2, key_ptr2) */
    {
	if (number_sort)
	{
	    /* some of these type tests are redundant */
	    if (!IsNumber(key_ptr1->tag) || !IsNumber(key_ptr2->tag))
	    {
		err = IsRef(key_ptr1->tag) ? INSTANTIATION_FAULT : IsRef(key_ptr2->tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
		goto _merge_error_;
	    }
	    comp = BIEq;
	    err = arith_compare(key_ptr1->val, key_ptr1->tag,
				key_ptr2->val, key_ptr2->tag, &comp);
	    if(err == PDELAY)
	    {
	        err = ARITH_EXCEPTION;
		goto _merge_error_;
	    }
	}
	else
	{
	    comp = compare_terms(key_ptr1->val, key_ptr1->tag,
				key_ptr2->val, key_ptr2->tag);
	}
	if(reverse)
	    comp = -comp;
	
	if (comp < 0 || ! comp && keep_duplicates)
	{
	    Make_List(append, TG);
	    append = TG;
	    Push_List_Frame();
	    *append++ = *h1++;		/* copy element h1 */
	    Dereference_(h1);
	    if (!IsList(h1->tag))
	    {
		if (IsNil(h1->tag))
		{
		    Make_List(append, h2);
		    break;
		}
		err = IsRef(h1->tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
		goto _merge_error_;
	    }
	    h1 = h1->val.ptr;
	    if (!(key_ptr1 = _get_key(h1, vk, tk, &err)))
		goto _merge_error_;
	}
	else if (comp > 0)
	{
	    Make_List(append, TG);
	    append = TG;
	    Push_List_Frame();
	    *append++ = *h2++;		/* copy element h2 */
	    Dereference_(h2);
	    if (!IsList(h2->tag))
	    {
		if (IsNil(h2->tag))
		{
		    Make_List(append, h1);
		    break;
		}
		err = IsRef(h2->tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
		goto _merge_error_;
	    }
	    h2 = h2->val.ptr;
	    if (!(key_ptr2 = _get_key(h2, vk, tk, &err)))
		goto _merge_error_;
	}
	else /* comp == 0 && !keep_duplicates */
	{
	    Make_List(append, TG);
	    append = TG;
	    Push_List_Frame();
	    *append++ = *h1++;		/* copy element h1 */
	    Dereference_(h1);
	    h2++;			/* skip element h2 */
	    Dereference_(h2);
	    if (!IsList(h1->tag))
	    {
		if (IsNil(h1->tag))
		{
		    *append = *h2;
		    break;
		}
		err = IsRef(h1->tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
		goto _merge_error_;
	    }
	    if (!IsList(h2->tag))
	    {
		if (IsNil(h2->tag))
		{
		    *append = *h1;
		    break;
		}
		err = IsRef(h2->tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
		goto _merge_error_;
	    }
	    h1 = h1->val.ptr;		/* both tails are lists */
	    h2 = h2->val.ptr;
	    if (!(key_ptr1 = _get_key(h1, vk, tk, &err))
	     || !(key_ptr2 = _get_key(h2, vk, tk, &err)))
	    {
		goto _merge_error_;
	    }
	}
    } /* for(;;) */

    Return_Unify_Pw(result.val, result.tag, v, t);

_merge_error_:		/* (err,old_tg) */
    TG = old_tg;
    Bip_Error(err);
}


static int
p_merge5(value vk, type tk, value vo, type to, value v1, type t1, value v2, type t2, value v, type t)
{
    return _merge(vk, tk, vo, to, v1, t1, v2, t2, v, t, 0);
}


static int
p_number_merge5(value vk, type tk, value vo, type to, value v1, type t1, value v2, type t2, value v, type t)
{
    return _merge(vk, tk, vo, to, v1, t1, v2, t2, v, t, 1);
}

