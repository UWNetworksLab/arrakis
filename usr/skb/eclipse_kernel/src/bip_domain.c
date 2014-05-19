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
 * Copyright (C) 1993-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: bip_domain.c,v 1.1 2008/06/30 17:43:51 jschimpf Exp $
 */

/****************************************************************************
 *
 *		SEPIA Auxiliary Predicates for Finite Domain Constraints.
 *
 *
 *****************************************************************************
 *
 * Author: Micha Meier
 *
 * History:
 *	Jan 1993 Created the file. It contains hardcoded pieces that have
 *		shown to be vital to the finite domain constraints.
 *
 */

/*
 * INCLUDES:
 */
#include        "config.h"
#include	"sepia.h"
#include	"types.h"
#include        "embed.h"
#include	"mem.h"
#include	"dict.h"
#include	"error.h"
#include	"emu_export.h"
#include	"fd.h"

/*
 * LOCAL MACROS
 */
#define DOM_NONE	0
#define DOM_BOTH	3
#define DOM_1		1
#define DOM_2		2

#define NOT_MOVED	0
#define MOVED		1
#define MOVE_BOTH	2

#define INPUT_ATOMIC	1
#define OUTPUT_ATOMIC	2
#define OUTPUT_INTERVAL 1

#define LT		1
#define GT		2
#define EQ		3

#define WAIT_1		1
#define WAIT_2		2

#define RANGE_EQ	0
#define RANGE_GE	1
#define RANGE_ONLY	2

/*
 * EXTERNAL VARIABLE DEFINITIONS:
 */

int		domain_slot;

/*
 * EXTERNAL VARIABLE DECLARATIONS:
 */

/*
 * STATIC VARIABLE DEFINITIONS:
 */
static int	p_dom_range(value vd, type td, value vmi, type tmi, value vma, type tma),
		p_dom_check_in(value ve, type te, value vd, type td),
		p_fd_init(void),
		p_lt_test(value vh, type th, value vmi, type tmi, value vma, type tma),
		p_make_extreme(value vt, type tt, value vm, type tm),
		p_linear_term_range_ge(value vt, type tt, value vres, type tres, value vmi, type tmi, value vma, type tma, value vnew, type tnew, value voff, type toff),
		p_linear_term_range_eq(value vt, type tt, value vres, type tres, value vmi, type tmi, value vma, type tma, value vnew, type tnew, value voff, type toff),
		p_linear_term_range_only(value vt, type tt, value vres, type tres, value vmi, type tmi, value vma, type tma, value vnew, type tnew, value voff, type toff),
		_linear_term_range(value vt, type tt, value vres, type tres, value vmi, type tmi, value vma, type tma, value vnew, type tnew, value voff, type toff, int ge),
		p_ex_insert_suspension(value vt, type tt, value vs, type ts, value vl, type tl),
		p_gec_insert_suspension(value vx, type tx, value vk, type tk, value vy, type ty, value vs, type ts),
		p_gec_start(value vx, type tx, value vk, type tk, value vy, type ty, value vc, type tc, value vd, type td, value ve, type te, value vres, type tres),
		p_gec_ent_start(value vx, type tx, value vk, type tk, value vy, type ty, value vc, type tc, value vd, type td, value ve, type te, value vres, type tres),
		p_gec_test(value vx, type tx, value vk, type tk, value vy, type ty, value vc, type tc, value vres, type tres),
		p_gec_comp(value vx, type tx, value vk, type tk, value vy, type ty, value vc, type tc, value vres, type tres),
		p_ineq_test(value vt, type tt, value vres, type tres, value vvar, type tvar, value vval, type tval),
		p_index_values(value vi, type ti, value vt, type tt, value vv, type tv, value vsi, type tsi, value vsv, type tsv, value vres, type tres, value vnewi, type tnewi, value vnewv, type tnewv, value vs, type ts, value vnsv, type tnsv),
		p_attr_instantiate(value va, type ta, value vc, type tc),
		p_prune_woken_goals(value val, type tag),
		p_dvar_remove_smaller(value vvar, type tvar, value vm, type tm),
		p_dvar_remove_greater(value vvar, type tvar, value vm, type tm),
		p_dom_union(value vd1, type td1, value vd2, type td2, value vu, type tu, value vs, type ts),
		p_dom_intersection(value vd1, type td1, value vd2, type td2, value vi, type ti, value vs, type ts),
		p_dom_difference(value vd1, type td1, value vd2, type td2, value vi, type ti, value vs, type ts),
		p_dom_compare(value vc, type tc, value vd1, type td1, value vd2, type td2),
		p_dvar_replace(value vvar, type tvar, value vn, type tn),
		p_dvar_remove_element(value vvar, type tvar, value vel, type tel),
		p_integer_list_to_dom(value vl, type tl, value vd, type td),
		p_sdelta(value l1, type t1, value l2, type t2, value l3, type t3),
		p_remove_element(value vvar, type tvar, value vel, type tel, value vres, type tres);

static int	dom_remove_smaller(pword*,long);
static int	dom_remove_greater(pword*,long);
static pword	*insert_interval(long,long,pword*);
static pword	*_dom_intersection(pword*,pword*,word*);
static word	_dom_value(pword*);
static int	_domain_changed(pword*,long,int);
static int	_remove_element(pword*,long,long);

static dident	d_interval,
		d_delay,
		d_dom,
		d_fd_par,
		d_min0,
		d_max0;


void
bip_domain_init(int flags)
{
    d_interval = in_dict("..", 2);
    d_delay = in_dict("delay", 2);
    d_dom = in_dict("dom", 2);
    d_max0 = in_dict("max", 0);
    d_min0 = in_dict("min", 0);
    d_fd_par = in_dict("fd_parameters",1);

    if (flags & INIT_SHARED)
    {
	/* this array is used to save the slot parameters in saved states */
	 (void) make_kernel_array(d_fd_par, 1, d_.integer0, d_.local0);
    }
    else /* get the slot parameters from the saved state */
    {
	word *fd_parameters = (word *) (get_kernel_array(d_fd_par)->val.ptr + 1);
	domain_slot = fd_parameters[0];
    }

    if (!(flags & INIT_SHARED))
	return;

    (void) exported_built_in(in_dict("fd_init", 0), p_fd_init, B_SAFE);
    (void) exported_built_in(in_dict("dom_check_in", 2), p_dom_check_in, B_UNSAFE);
    exported_built_in(in_dict("dom_compare", 3), p_dom_compare, B_UNSAFE)
	-> mode = BoundArg(1, CONSTANT);
    exported_built_in(in_dict("dvar_remove_smaller", 2), p_dvar_remove_smaller,
	B_UNSAFE|U_SIMPLE) -> mode = BoundArg(2, CONSTANT);
    exported_built_in(in_dict("dvar_remove_greater", 2), p_dvar_remove_greater,
	B_UNSAFE|U_SIMPLE) -> mode = BoundArg(2, CONSTANT);
    exported_built_in(in_dict("dom_range", 3), p_dom_range, B_UNSAFE|U_GROUND)
	-> mode = BoundArg(2, CONSTANT)|BoundArg(3, CONSTANT);
    exported_built_in(in_dict("dom_intersection", 4), p_dom_intersection,
	B_UNSAFE|U_GROUND) -> mode = BoundArg(3, GROUND)|BoundArg(4, CONSTANT);
    exported_built_in(in_dict("dom_union", 4), p_dom_union,
	B_UNSAFE|U_GROUND) -> mode = BoundArg(3, GROUND)|BoundArg(4, CONSTANT);
    exported_built_in(in_dict("dom_difference", 4), p_dom_difference,
	B_UNSAFE|U_GROUND) -> mode = BoundArg(3, GROUND)|BoundArg(4, CONSTANT);
    (void) exported_built_in(in_dict("lt_test", 3), p_lt_test,
	B_UNSAFE|U_UNIFY);
    exported_built_in(in_dict("linear_term_range_only", 6),
	p_linear_term_range_only, B_UNSAFE|U_UNIFY) -> mode =
	BoundArg(2, CONSTANT);
    exported_built_in(in_dict("linear_term_range_eq", 6),
	p_linear_term_range_eq, B_UNSAFE|U_UNIFY) -> mode =
	BoundArg(2, CONSTANT);
    exported_built_in(in_dict("linear_term_range_ge", 6),
	p_linear_term_range_ge, B_UNSAFE|U_UNIFY) -> mode =
	BoundArg(2, CONSTANT);
    exported_built_in(in_dict("make_extreme", 2), p_make_extreme, B_UNSAFE|U_UNIFY)
	-> mode = BoundArg(1, NONVAR);
    (void) exported_built_in(in_dict("prune_woken_goals", 1),
	p_prune_woken_goals, B_UNSAFE);
    (void) exported_built_in(in_dict("ex_insert_suspension", 3),
	p_ex_insert_suspension, B_UNSAFE);
    exported_built_in(in_dict("gec_start", 7), p_gec_start, B_UNSAFE|U_GROUND)
	-> mode = BoundArg(5, CONSTANT);
    exported_built_in(in_dict("gec_ent_start", 7), p_gec_ent_start,
	B_UNSAFE|U_GROUND) -> mode = BoundArg(5, CONSTANT);
    exported_built_in(in_dict("gec_test", 5), p_gec_test, B_UNSAFE|U_GROUND)
	-> mode = BoundArg(5, CONSTANT);
    exported_built_in(in_dict("gec_comp", 5), p_gec_comp, B_UNSAFE|U_GROUND)
	-> mode = BoundArg(5, CONSTANT);
    (void) exported_built_in(in_dict("gec_insert_suspension", 4),
	p_gec_insert_suspension, B_UNSAFE);
    exported_built_in(in_dict("ineq_test", 4), p_ineq_test, B_UNSAFE|U_UNIFY)
	-> mode = BoundArg(2, CONSTANT) | BoundArg(3, NONVAR) |
	BoundArg(4, CONSTANT);
    exported_built_in(in_dict("index_values", 10), p_index_values,
	B_UNSAFE|U_UNIFY) -> mode = BoundArg(6, CONSTANT) | BoundArg(7, NONVAR);
    (void) exported_built_in(in_dict("attr_instantiate", 2), p_attr_instantiate,
	B_UNSAFE);
    exported_built_in(in_dict("remove_element", 3), p_remove_element,
	B_UNSAFE|U_SIMPLE) -> mode = BoundArg(3, CONSTANT);
    exported_built_in(in_dict("dvar_remove_element", 2), p_dvar_remove_element,
	B_UNSAFE|U_SIMPLE) -> mode = BoundArg(3, CONSTANT);
    exported_built_in(in_dict("integer_list_to_dom", 2), p_integer_list_to_dom,
	B_UNSAFE|U_GROUND) -> mode = BoundArg(2, CONSTANT);
    (void) exported_built_in(in_dict("dvar_replace", 2), p_dvar_replace,
	B_UNSAFE);
    exported_built_in(in_dict("sdelta", 3), p_sdelta,
	B_UNSAFE|U_GROUND) -> mode = BoundArg(3, GROUND);
}

static int
p_fd_init(void)
{
    word *fd_parameters = (word *) (get_kernel_array(d_fd_par)->val.ptr + 1);
    domain_slot   = fd_parameters[0] = meta_index(in_dict("fd", 0));
    Succeed_;
}

static int
p_dom_range(value vd, type td, value vmi, type tmi, value vma, type tma)
{
    long		min, max;
    Prepare_Requests;

    Check_Domain(vd, td)
    Check_Output_Integer(tmi)
    Check_Output_Integer(tma)
    if (dom_range(vd.ptr, &min, &max)) {
	Fail_
    }
    Request_Unify_Integer(vmi, tmi, min)
    Request_Unify_Integer(vma, tma, max)
    Return_Unify
}

static int
p_dom_check_in(value ve, type te, value vd, type td)
{
    Check_Domain(vd, td)
    Check_Element(ve, te)
    Succeed_If(!dom_check_in(ve.nint, te, vd.ptr))
}

/* attr_instantiate(Attr, Val)	*/
/*ARGSUSED*/
static int
p_attr_instantiate(value va, type ta, value vc, type tc)
{
    register pword	*d;
    long		min, max;
    int			res;
    int			atomic;

    d = va.ptr + DOMAIN_OFF;
    Dereference_(d);
    d = d->val.ptr;
    if (dom_check_in(vc.nint, tc, d)) {
	Fail_;
    }
    atomic = dom_range(d, &min, &max);
    d = va.ptr + ANY_OFF;
    Dereference_(d);
    res = p_schedule_woken(d->val, d->tag);
    if (res != PSUCCEED) {
	Bip_Error(res)
    }
    if (!atomic) {
	d = va.ptr + MIN_OFF;
	Dereference_(d);
	if (vc.nint > min) {
	    res = p_schedule_woken(d->val, d->tag);
	} else {
	    res = p_schedule_postponed(d->val, d->tag);
	}
	if (res != PSUCCEED) {
	    Bip_Error(res)
	}

	d = va.ptr + MAX_OFF;
	Dereference_(d);
	if (vc.nint < max) {
	    res = p_schedule_woken(d->val, d->tag);
	} else {
	    res = p_schedule_postponed(d->val, d->tag);
	}
	if (res != PSUCCEED) {
	    Bip_Error(res)
	}
    }
    Succeed_
}


/*	lt_test(+H, +Min, +Max)	*/
/*ARGSUSED*/
static int
p_lt_test(value vh, type th, value vmi, type tmi, value vma, type tma)
{
    long	min, max, n, n1, k;
    pword	*p;
    pword	*var;
    int		res = RES_NO_CHANGE;

    if (IsInteger(th)) {
	Succeed_
    }

    p = vh.ptr + 1;
    Dereference_(p);
    k = p->val.nint;
    p = vh.ptr + 2;
    Dereference_(p);
    if (IsInteger(p->tag)) {
	Succeed_
    }
    var = p->val.ptr;
    Var_Domain(var, p);
    (void) dom_range(p, &min, &max);
    if (k > 0) {
	n = -vma.nint/k + max;
	if (n <= min)
	    ;
	else if (n < max) {
	    res |= RES_MIN;
	}
	else /* if (n == max) */
	{
	    Bind_Var(var->val, var->tag, n, TINT)
	    Succeed_;
	}
	if (vmi.nint < vma.nint) {	/* equality */
	    n1 = -vmi.nint/k + min;
	    if (n1 >= max)
		;
	    else if (n1 > min) {
		if (res & RES_MIN) {
		    if (n == n1) {
			Bind_Var(var->val, var->tag, n, TINT)
			Succeed_;
		    }
		}
		res |= RES_MAX;
	    }
	    else /* if (n1 == max)  */
	    {
		Bind_Var(var->val, var->tag, n1, TINT)
		Succeed_;
	    }
	}
    }
    else {
	if (vmi.nint < vma.nint) {	/* equality */
	    n1 = -vmi.nint/k + max;
	    if (n1 <= min)
		;
	    else if (n1 < max) {
		res |= RES_MIN;
	    }
	    else /* if (n1 == max) */
	    {
		Bind_Var(var->val, var->tag, n1, TINT)
		Succeed_;
	    }
	}
	n = -vma.nint/k + min;
	if (n >= max)
	    ;
	else if (n > min) {
	    if (res & RES_MIN) {
		if (n == n1) {
		    Bind_Var(var->val, var->tag, n, TINT)
		    Succeed_;
		}
	    }
	    res |= RES_MAX;
	}
	else /* if (n == max) */
	{
	    Bind_Var(var->val, var->tag, n, TINT)
	    Succeed_;
	}
    }
    if (res & RES_MIN) {
	min = dom_remove_smaller(p, k > 0 ? n : n1);
	if (!min) {
	    Fail_
	}
    }
    if (res & RES_MAX) {
	min = dom_remove_greater(p, k > 0 ? n1 : n);
	if (!min) {
	    Fail_
	}
    }
    if (res) {
	k = _domain_changed(var, min, res);
	Check_Return(k)
    }
    Succeed_;
}

static int
p_make_extreme(value vt, type tt, value vm, type tm)
{
    register pword	*p, *s, *t;
    pword		*unif1, *l1, *unif2, *l2;
    pword		*var;
    word		k;
    long		min, max;
    int			minimize;

    if (IsNil(tt)) {
	Succeed_
    }
    Check_List(tt)
    Check_Atom(tm)
    if (vm.did == d_min0)
	minimize = 1;
    else if (vm.did == d_max0)
	minimize = 0;
    else {
	Bip_Error(RANGE_ERROR)
    }
    unif1 = l1 = TG++;
    unif2 = l2 = TG++;
    Check_Gc;
    p = vt.ptr;
    for (;;)
    {
	s = p++;
	Dereference_(s);
	if (IsStructure(s->tag)) {
	    s = s->val.ptr + 1;
	    t = s + 1;
	    Dereference_(t);
	    if (!IsInteger(t->tag)) {
		Dereference_(s);
		k = s->val.nint;
		var = t->val.ptr;
		Var_Domain(var, t);
		(void) dom_range(t, &min, &max);
		l1->tag.kernel = TLIST;
		l1->val.ptr = TG;
		l2->tag.kernel = TLIST;
		l2->val.ptr = TG + 2;
		l1 = TG;
		TG += 4;
		Check_Gc;
		l2 = l1 + 2;
		l1->val.ptr = var;
		(l1++)->tag.kernel = TREF;
		l2->tag.kernel = TINT;
		if (k > 0 && minimize || k < 0 && !minimize)
		    (l2++)->val.nint = min;
		else
		    (l2++)->val.nint = max;
	    }
	}
	Dereference_(p)
	if (!IsList(p->tag))
	    break;
	p = p->val.ptr;
    }
    l1->tag.kernel = TNIL;
    l2->tag.kernel = TNIL;
    Return_Unify_Pw(unif1->val, unif1->tag, unif2->val, unif2->tag)
}

/*    linear_term_range_ge(+Term, -Res, -Min, -Max, -NewTerm, -Offset) */
static int
p_linear_term_range_ge(value vt, type tt, value vres, type tres, value vmi, type tmi, value vma, type tma, value vnew, type tnew, value voff, type toff)
{
    return _linear_term_range(vt, tt, vres, tres, vmi, tmi, vma, tma,
	vnew, tnew, voff, toff, RANGE_GE);
}

/*    linear_term_range_eq(+Term, -Res, -Min, -Max, -NewTerm, -Offset) */
static int
p_linear_term_range_eq(value vt, type tt, value vres, type tres, value vmi, type tmi, value vma, type tma, value vnew, type tnew, value voff, type toff)
{
    return _linear_term_range(vt, tt, vres, tres, vmi, tmi, vma, tma,
	vnew, tnew, voff, toff, RANGE_EQ);
}

/*    linear_term_range_only(+Term, -Res, -Min, -Max, -NewTerm, -Offset) */
static int
p_linear_term_range_only(value vt, type tt, value vres, type tres, value vmi, type tmi, value vma, type tma, value vnew, type tnew, value voff, type toff)
{
    return _linear_term_range(vt, tt, vres, tres, vmi, tmi, vma, tma,
	vnew, tnew, voff, toff, RANGE_ONLY);
}

static int
_linear_term_range(value vt, type tt, value vres, type tres, value vmi, type tmi, value vma, type tma, value vnew, type tnew, value voff, type toff, int ge)
{
    register word	min = 0;
    register word	max = 0;
    register pword	*p;
    register pword	*s;
    register pword	*r;
    register word	k;
    register word	maxel = 0;
    word		vars = 0;
    pword		*var;
    pword		*var1;
    pword		*var2;
    word		k1, k2;
    word		sum = 0;
    pword		*last = 0;
    int			constno = 0;

    if (!IsNil(tt)) {
	p = vt.ptr;
	for (;;)
	{
	    s = p++;
	    Dereference_(s);
	    if (IsInteger(s->tag)) {
		k = s->val.nint;
		min += k;
		max += k;
		sum += k;
	    }
	    else {
		s = s->val.ptr + 2;
		r = s - 1;
		Dereference_(r);
		k = r->val.nint;
		Dereference_(s);
		if (IsInteger(s->tag)) {
		    k *= s->val.nint;
		    min += k;
		    max += k;
		    sum += k;
		    constno++;
		}
		else {
		    long	mi, ma;

		    if (k) {
			if (!(vars++)) {
			    k1 = k;
			    var = p - 1;
			    var1 = s;
			}
			else if (vars == 2) {
			    k2 = k;
			    var2 = s;
			}
		    }
		    if (!IsMeta(s->tag)) {
			Bind_Var(vres, tres, RES_EVAL, TINT)
			Bind_Var(vnew, tnew, vt.ptr, tt.kernel)
			Succeed_
		    }
		    Var_Attr(s->val.ptr, s)
		    if (!IsStructure(s->tag)) {
			Bind_Var(vres, tres, IsRef(s->tag) ? RES_EVAL : RES_ERROR, TINT)
			Bind_Var(vnew, tnew, vt.ptr, tt.kernel)
			Succeed_
		    }
		    Attr_Domain(s, s)
		    if (dom_range(s, &mi, &ma)) {
			Bind_Var(vres, tres, RES_ERROR, TINT)
			Bind_Var(vnew, tnew, vt.ptr, tt.kernel)
			Succeed_
		    }
		    if (k > 0) {
			min += k * mi;
			max += k * ma;
			k = k * (ma - mi);
			if (k > maxel)
			    maxel = k;
		    } else if (k < 0) {
			min += k * ma;
			max += k * mi;
			k = k * (mi - ma);
			if (k > maxel)
			    maxel = k;
		    }
		    if (constno > 0 && last) {
			if (last < GB && last->val.ptr < GB) {
			    Trail_Pword(last)
			}
			last->val.ptr = p - 1;
			last->tag.kernel = TLIST;
		    }
		    constno = 0;
		    if (ge != RANGE_ONLY)
			last = p;
		}
	    }
	    Dereference_(p);
	    if (!IsList(p->tag))
		break;
	    p = p->val.ptr;
	}
	if (constno && last) {
	    if (last < GB && last->val.ptr < GB) {
		Trail_Pword(last)
	    }
	    last->tag.kernel = TNIL;
	}
    }
    if (ge == RANGE_ONLY) {
	Bind_Var(vmi, tmi, min, TINT)
	Bind_Var(vma, tma, max, TINT)
	Bind_Var(vres, tres, RES_SOLVED, TINT)
	Succeed_;
    }
    if (max < 0) {
	Fail_
    }
    else if (max == 0 && min < 0) {		/* maximum	*/
	/* create a term because of entailment variant */
	p = TG;
	TG += 2;
	Check_Gc;
	p[0].val.nint = sum;
	p[0].tag.kernel = TINT;
	p[1].val.ptr = var;
	p[1].tag.kernel = TLIST;
	Bind_Var(vnew, tnew, p, TLIST)
	Bind_Var(vres, tres, 0, TINT)
    }
    else if (max == 0) {			/* = 0		*/
	Bind_Var(vres, tres, 1, TINT)
    }
    else if (min == 0) {			/* >= 0		*/
	/* create a term because of entailment variant */
	p = TG;
	TG += 2;
	Check_Gc;
	p[0].val.nint = sum;
	p[0].tag.kernel = TINT;
	p[1].val.ptr = var;
	p[1].tag.kernel = TLIST;
	Bind_Var(vnew, tnew, p, TLIST)
	Bind_Var(vres, tres, 2, TINT)
    }
    else if (min > 0) {				/* > 0		*/
	Bind_Var(vres, tres, 3, TINT)
    }
    else if (ge == RANGE_GE && vars == 2 && (k1 == 1 || k2 == 1)) {
	if (k1 != 1) {
	    s = var1;
	    var1 = var2;
	    var2 = s;
	    k2 = k1;
	}
	Bind_Var(vnew, tnew, var1, TREF)
	Bind_Var(vma, tma, var2, TREF)
	Bind_Var(vmi, tmi, k2, TINT)
	Bind_Var(voff, toff, sum, TINT)
	Bind_Var(vres, tres, RES_SIMPLE, TINT)
    }
    else if (ge == RANGE_EQ && vars == 2 && sum == 0 && k1*k2 == -1) {
	Bind_Var(vmi, tmi, var1, TREF)
	Bind_Var(vma, tma, var2, TREF)
	Bind_Var(vres, tres, RES_SIMPLE, TINT)
    }
    else
    {
	p = TG;
	TG += 2;
	Check_Gc;
	p[0].val.nint = sum;
	p[0].tag.kernel = TINT;
	p[1].val.ptr = var;
	p[1].tag.kernel = TLIST;
	Bind_Var(vnew, tnew, p, TLIST)
	if (vars == 1) {
	    /* one variable left	*/
	    Bind_Var(vmi, tmi, min, TINT)
	    Bind_Var(vma, tma, max, TINT)
	    Bind_Var(vres, tres, 4, TINT)
	}
	else if (maxel <= max && maxel <= -min)	/* nothing to update	*/
	{
	    Bind_Var(vres, tres, 5, TINT)
	}
	else {					/* something to update	*/
	    Bind_Var(vmi, tmi, min, TINT)
	    Bind_Var(vma, tma, max, TINT)
	    Bind_Var(vres, tres, maxel <= max ? 7 : 8, TINT)
	}
    }
    Succeed_
}

/* p is the val.ptr of dom/2 */
int
dom_range(register pword *p, long int *mi, long int *ma)
{
    register pword	*s;
    register pword	*t;
    register word	max;

    p++;
    Dereference_(p);
    p = p->val.ptr;
    s = p++;
    Dereference_(s);
    if (IsInteger(s->tag))
	*mi = max = s->val.nint;
    else if (!IsFdInterval(s->val, s->tag))
	return 1;
    else {
	s = s->val.ptr + 1;
	t = s++;
	Dereference_(t);
	*mi = t->val.nint;
	Dereference_(s);
	max = s->val.nint;
    }
    Dereference_(p);
    while (IsList(p->tag))
    {
	p = p->val.ptr;
	s = p++;
	Dereference_(s);
	if (IsInteger(s->tag))
	    max = s->val.nint;
	else if (IsFdInterval(s->val, s->tag)){
	    s = s->val.ptr + 2;
	    Dereference_(s);
	    max = s->val.nint;
	} else
	    return 1;
	Dereference_(p);
    }
    if (!IsInteger(s->tag))
	return 1;
    *ma = max;
    return 0;
}

/*    ex_insert_suspension(List, Susp, Ge) */
/*ARGSUSED*/
static int
p_ex_insert_suspension(value vt, type tt, value vs, type ts, value vl, type tl)
{
    register pword	*p;
    register pword	*s;
    register pword	*r;
    register word	k;
    int			res;

    if (!IsNil(tt)) {
	p = vt.ptr;
	for (;;)
	{
	    s = p++;
	    Dereference_(s);
	    if (!IsInteger(s->tag)) {
		s = s->val.ptr + 2;
		r = s - 1;
		Dereference_(r);
		k = r->val.nint;
		Dereference_(s);
		if (!IsInteger(s->tag)) {
		    if (vl.nint == 0) {
			res = insert_suspension(s, k > 0 ? MAX_OFF : MIN_OFF,
				vs.ptr, domain_slot);
			Check_Return(res)
		    } else {	/* equality */
			res = insert_suspension(s, MIN_OFF,
			    vs.ptr, domain_slot);
			Check_Return(res)
			res = insert_suspension(s, MAX_OFF,
			    vs.ptr, domain_slot);
			Check_Return(res)
		    }
		}
	    }
	    Dereference_(p);
	    if (!IsList(p->tag))
		break;
	    p = p->val.ptr;
	}
    }
    Succeed_
}

/*  gec_insert_suspension(X, K, Y, Susp) */
/*ARGSUSED*/
static int
p_gec_insert_suspension(value vx, type tx, value vk, type tk, value vy, type ty, value vs, type ts)
{
    int			res;

    if (IsRef(tx)) {
	res = insert_suspension(vx.ptr, MAX_OFF, vs.ptr, domain_slot);
	Check_Return(res)
    }
    if (IsRef(ty)) {
	res = insert_suspension(vy.ptr, vk.nint > 0 ? MAX_OFF : MIN_OFF,
		vs.ptr, domain_slot);
	Check_Return(res)
    }
    Succeed_
}

/*
	X + K*Y + C >= D
    
    K is known to be an integer, X, Y, C, and D may be anything.
    If we can convert it to the form Var1 + K*Var2 + C >= 0, we continue,
    otherwise we signal an error.
*/
static int
p_gec_start(value vx, type tx, value vk, type tk, value vy, type ty, value vc, type tc, value vd, type td, value ve, type te, value vres, type tres)
{
    register pword	*p;

    if (!IsInteger(tc) || !IsInteger(td)) {
	goto _gec_err_;
    }
    if (IsMeta(tx)) {
	Var_Domain_Check(vx.ptr, p)
	if (!p) goto _gec_err_;
    } else if (!IsInteger(tx)) {
	goto _gec_err_;
    }
    if (IsMeta(ty)) {
	Var_Domain_Check(vy.ptr, p)
	if (!p) goto _gec_err_;
    } else if (!IsInteger(ty)) {
	goto _gec_err_;
    }
    vc.nint -= vd.nint;
    Bind_Var(ve, te, vc.nint, TINT)
    return p_gec_comp(vx, tx, vk, tk, vy, ty, vc, tc, vres, tres);

_gec_err_:
    if (!IsInteger(tc) || vd.nint == 0) {
	Bind_Var(ve, te, vc.ptr, tc.kernel)
	Bind_Var(vres, tres, RES_AGAIN, TINT)
    } else {
	Bind_Var(ve, te, vd.ptr, td.kernel)
	Bind_Var(vres, tres, RES_AGAIN_NEG, TINT)
    }
    Succeed_
}

/*
	>=(X + K*Y + C, D, Bool)
    
    K is known to be an integer, X, Y, C, and D may be anything.
    If we can convert it to the form Var1 + K*Var2 + C >= 0, we continue,
    otherwise we signal an error.
*/
static int
p_gec_ent_start(value vx, type tx, value vk, type tk, value vy, type ty, value vc, type tc, value vd, type td, value ve, type te, value vres, type tres)
{
    register pword	*p;

    if (!IsInteger(tc) || !IsInteger(td)) {
	goto _gec_ent_err_;
    }
    if (IsMeta(tx)) {
	Var_Domain_Check(vx.ptr, p)
	if (!p) goto _gec_ent_err_;
    } else if (!IsInteger(tx)) {
	goto _gec_ent_err_;
    }
    if (IsMeta(ty)) {
	Var_Domain_Check(vy.ptr, p)
	if (!p) goto _gec_ent_err_;
    } else if (!IsInteger(ty)) {
	goto _gec_ent_err_;
    }
    vc.nint -= vd.nint;
    Bind_Var(ve, te, vc.nint, TINT)
    return p_gec_test(vx, tx, vk, tk, vy, ty, vc, tc, vres, tres);

_gec_ent_err_:
    if (!IsInteger(tc) || vd.nint == 0) {
	Bind_Var(ve, te, vc.ptr, tc.kernel)
	Bind_Var(vres, tres, RES_AGAIN, TINT)
    } else {
	Bind_Var(ve, te, vd.ptr, td.kernel)
	Bind_Var(vres, tres, RES_AGAIN_NEG, TINT)
    }
    Succeed_
}

/*ARGSUSED*/
static int
p_gec_comp(value vx, type tx, value vk, type tk, value vy, type ty, value vc, type tc, value vres, type tres)
{
    register long	c;
    register pword	*dom1, *dom2;
    register long	k = vk.nint;
    register long	m;
    long		minx, maxx, miny, maxy;
    long		newminx;
    int			ret = RES_SOLVED;

    if (IsInteger(tx)) {
	c = vx.nint + vc.nint;
	if (IsInteger(ty)) {
	    if (c + k * vy.nint >= 0) {
		Bind_Var(vres, tres, RES_SOLVED, TINT)
		Succeed_
	    } else {
		Fail_
	    }
	}
	Var_Domain(vy.ptr, dom2);
_x_inst_:
	if (dom_range(dom2, &miny, &maxy)) {
	    Bind_Var(vres, tres, RES_ERROR, TINT)
	    Succeed_
	}
	if (k < 0) {
	    /* don't divide negative numbers */
	    c = (c >= 0) ? c / (-k) : -((-c - k - 1)/(-k));
	    if (c < miny) {
		Fail_
	    } else if (c < maxy) {
		if (c == miny) {
		    Bind_Var(vy, ty, c, TINT)
		    Bind_Var(vres, tres, ret, TINT)
		} else {
		    miny = dom_remove_greater(dom2, c);
		    if (!miny) {
			Fail_
		    }
		    m = _domain_changed(vy.ptr, miny, RES_MAX);
		    Check_Return(m)
		    Bind_Var(vres, tres, RES_WAKE, TINT)
		}
	    } else {
		Bind_Var(vres, tres, ret, TINT)
	    }
	} else {
	    c = (c >= 0) ? -(c/k) : (-c + k - 1) / k;
	    if (c > maxy) {
		Fail_
	    } else if (c > miny) {
		if (c == maxy) {
		    Bind_Var(vy, ty, c, TINT)
		    Bind_Var(vres, tres, ret, TINT)
		} else {
		    miny = dom_remove_smaller(dom2, c);
		    if (!miny) {
			Fail_
		    }
		    m = _domain_changed(vy.ptr, miny, RES_MIN);
		    Check_Return(m)
		    Bind_Var(vres, tres, RES_WAKE, TINT)
		}
	    } else {
		Bind_Var(vres, tres, ret, TINT)
	    }
	}
	Succeed_
    }
    else if (IsInteger(ty)) {
	c = -k * vy.nint - vc.nint;
	Var_Domain(vx.ptr, dom1);
_y_inst_:
	if (dom_range(dom1, &minx, &maxx)) {
	    Bind_Var(vres, tres, RES_ERROR, TINT)
	    Succeed_
	}
	if (c > maxx) {
	    Fail_
	} else if (c > minx) {
	    if (c == maxx) {
		Bind_Var(vx, tx, c, TINT)
		Bind_Var(vres, tres, ret, TINT)
	    } else {
		minx = dom_remove_smaller(dom1, c);
		if (!minx) {
		    Fail_
		}
		m = _domain_changed(vx.ptr, minx, RES_MIN);
		Check_Return(m)
		Bind_Var(vres, tres, RES_WAKE, TINT)
	    }
	} else {
	    Bind_Var(vres, tres, ret, TINT)
	}
	Succeed_
    }
    /* both variables */
    c = vc.nint;
    Var_Domain(vy.ptr, dom2);
    if (vx.ptr == vy.ptr) {
	/* equal */
	k++;
	if (k == 0) {
	    if (c >= 0) {
		Bind_Var(vres, tres, RES_SOLVED, TINT)
		Succeed_;
	    } else {
		Fail_
	    }
	}
	goto _x_inst_;
    }
    if (dom_range(dom2, &miny, &maxy)) {
	Bind_Var(vres, tres, RES_ERROR, TINT)
	Succeed_
    }
    Var_Domain(vx.ptr, dom1);
    if (dom_range(dom1, &minx, &maxx)) {
	Bind_Var(vres, tres, RES_ERROR, TINT)
	Succeed_
    }
    if (k > 0)
	m = (maxx + c >= 0) ? -((maxx + c)/k) : (-maxx -c + k - 1) / k;
    else
	m = (maxx + c >= 0) ? (maxx + c) / (-k) : -((-maxx - c - k - 1)/(-k));
    newminx = -k * (k > 0 ? maxy : miny) - c;
    if (m > miny && m < maxy)
    {
	register long	s;

	if (k > 0)
	    s = dom_remove_smaller(dom2, m);
	else
	    s = dom_remove_greater(dom2, m);
	if (!s) {
	    Fail_
	}
	if (s == 1) { /* there was a hole in the domain */
	    miny = _dom_value(dom2);
	    Bind_Var(vy, ty, miny, TINT)
	    c = -k * miny - vc.nint;
	    goto _y_inst_;
	}
	m = _domain_changed(vy.ptr, s, k > 0 ? RES_MIN : RES_MAX);
	Check_Return(m)
	if (newminx > minx) {
	    s = dom_remove_smaller(dom1, newminx);
	    if (!s) {
		Fail_
	    }
	    if (s == 1) {
		minx = _dom_value(dom1);
		Bind_Var(vx, tx, minx, TINT)
		c = minx + vc.nint;
		ret = RES_WAKE;
		goto _x_inst_;
	    }
	    m = _domain_changed(vx.ptr, s, RES_MIN);
	    Check_Return(m)
	    Bind_Var(vres, tres, RES_DELAY_WAKE, TINT)
	} else {
	    Bind_Var(vres, tres, RES_DELAY_WAKE, TINT)
	}
    }
    else if (k > 0 && m == maxy || k < 0 && m == miny) {
	Bind_Var(vy, ty, m, TINT)
	c = -k * m - vc.nint;
	goto _y_inst_;
    }
    else if (newminx > maxx) {
	Fail_
    }
    else if (newminx > minx) {
	minx = dom_remove_smaller(dom1, newminx);
	if (!minx) {
	    Fail_
	}
	m = _domain_changed(vx.ptr, minx, RES_MIN);
	Check_Return(m)
	Bind_Var(vres, tres, RES_DELAY_WAKE, TINT)
    }
    else if (minx + k * (k > 0 ? miny : maxy) + c >= 0) {
	Bind_Var(vres, tres, RES_SOLVED, TINT)
    }
    else {
	Bind_Var(vres, tres, RES_NO_CHANGE, TINT)
    }
    Succeed_
}

/*ARGSUSED*/
static int
p_gec_test(value vx, type tx, value vk, type tk, value vy, type ty, value vc, type tc, value vres, type tres)
{
    register long	c;
    register pword	*dom1, *dom2;
    register long	k = vk.nint;
    register long	m;
    long		minx, maxx, miny, maxy;
    long		newminx;
    int			ret = RES_SOLVED;

    if (IsInteger(tx)) {
	c = vx.nint + vc.nint;
	if (IsInteger(ty)) {
	    if (c + k * vy.nint < 0)
		ret = RES_FAIL;
	    Bind_Var(vres, tres, ret, TINT)
	    Succeed_
	}
	Var_Domain(vy.ptr, dom2);
_x_inst_test_:
	if (dom_range(dom2, &miny, &maxy)) {
	    Bind_Var(vres, tres, RES_ERROR, TINT)
	    Succeed_
	}
	if (k < 0) {
	    /* don't divide negative numbers */
	    c = (c >= 0) ? c / (-k) : -((-c - k - 1)/(-k));
	    if (c < miny) {
		ret = RES_FAIL;
	    } else if (c < maxy) {
		ret = RES_DELAY;
	    }
	} else {
	    c = (c >= 0) ? -(c/k) : (-c + k - 1) / k;
	    if (c > maxy) {
		ret = RES_FAIL;
	    } else if (c > miny) {
		ret = RES_DELAY;
	    }
	}
	Bind_Var(vres, tres, ret, TINT)
	Succeed_
    }
    else if (IsInteger(ty)) {
	c = -k * vy.nint - vc.nint;
	Var_Domain(vx.ptr, dom1);
	if (dom_range(dom1, &minx, &maxx)) {
	    Bind_Var(vres, tres, RES_ERROR, TINT)
	    Succeed_
	}
	if (c > maxx) {
	    ret = RES_FAIL;
	} else if (c > minx) {
	    ret = RES_DELAY;
	}
	Bind_Var(vres, tres, ret, TINT)
	Succeed_
    }
    /* both variables */
    c = vc.nint;
    Var_Domain(vy.ptr, dom2);
    if (vx.ptr == vy.ptr) {
	/* equal */
	k++;
	if (k == 0) {
	    if (c < 0)
		ret = RES_FAIL;
	    Bind_Var(vres, tres, ret, TINT)
	    Succeed_;
	}
	goto _x_inst_test_;
    }
    if (dom_range(dom2, &miny, &maxy)) {
	Bind_Var(vres, tres, RES_ERROR, TINT)
	Succeed_
    }
    Var_Domain(vx.ptr, dom1);
    if (dom_range(dom1, &minx, &maxx)) {
	Bind_Var(vres, tres, RES_ERROR, TINT)
	Succeed_
    }
    if (k > 0)
	m = (maxx + c >= 0) ? -((maxx + c)/k) : (-maxx -c + k - 1) / k;
    else
	m = (maxx + c >= 0) ? (maxx + c) / (-k) : -((-maxx - c - k - 1)/(-k));
    newminx = -k * (k > 0 ? maxy : miny) - c;
    if (m > miny && m < maxy)
	ret = RES_DELAY;
    else if (newminx > maxx)
	ret = RES_FAIL;
    else if (minx + k * (k > 0 ? miny : maxy) + c >= 0)
	ret = RES_SOLVED;
    else
	ret = RES_DELAY;
    Bind_Var(vres, tres, ret, TINT)
    Succeed_
}

/*    ineq_test(+Term, -Res, -Var, -Val) */
static int
p_ineq_test(value vt, type tt, value vres, type tres, value vvar, type tvar, value vval, type tval)
{
    register word	sum = 0;
    register pword	*p;
    register pword	*s;
    register pword	*r;
    register word	k;
    pword		*var;
    word		kvar = 0;

    if (IsNil(tt)) {
	Bind_Var(vres, tres, RES_SOLVED, TINT)
	Succeed_
    }
    p = vt.ptr;
    for (;;)
    {
	s = p++;
	Dereference_(s);
	if (IsInteger(s->tag)) {
	    sum += s->val.nint;
	}
	else {
	    s = s->val.ptr + 2;
	    r = s - 1;
	    Dereference_(r);
	    k = r->val.nint;
	    Dereference_(s);
	    if (IsInteger(s->tag))
		sum += k * s->val.nint;
	    else if (!IsMeta(s->tag)) {
		Bind_Var(vres, tres, RES_EVAL, TINT)
		Succeed_
	    } else {
		Var_Attr(s->val.ptr, r)
		if (!IsStructure(r->tag)) {
		    Bind_Var(vres, tres, IsRef(r->tag) ? RES_EVAL : RES_ERROR, TINT)
		    Succeed_
		}
		if (kvar) {
		    Bind_Var(vvar, tvar, var, TREF)
		    Bind_Var(vval, tval, s, TREF)
		    Bind_Var(vres, tres, RES_DELAY, TINT)
		    Succeed_
		}
		else {
		    kvar = k;
		    var = s;
		}
	    }
	}
	Dereference_(p);
	if (!IsList(p->tag))
	    break;
	p = p->val.ptr;
    }
    if (kvar == 0) {
	if (sum != 0) {
	    Bind_Var(vres, tres, RES_SOLVED, TINT)
	    Succeed_
	} else {
	    Fail_
	}
    }
    k = sum/kvar;
    if (k * kvar == sum) {
	k = _remove_element(var, -k, (long) TINT);
	Check_Return(k);
	if (k == RES_FAIL) {
	    Fail_
	}
	Bind_Var(vres, tres, k, TINT)
	Succeed_
    }
    else {
	Bind_Var(vres, tres, RES_SOLVED, TINT)
    }
    Succeed_
}

/* for element/3:
   index_values(Index, Term, Value, SI, SV, Res, NewI, NewV, SizeI, SizeV) */
/*ARGSUSED*/
static int
p_index_values(value vi, type ti, value vt, type tt, value vv, type tv, value vsi, type tsi, value vsv, type tsv, value vres, type tres, value vnewi, type tnewi, value vnewv, type tnewv, value vs, type ts, value vnsv, type tnsv)
{
    word	size = 0;
    word	sizev = 0;
    pword	*p;
    pword	*v;
    pword	*s;
    pword	*t;
    pword	*newi;
    pword	*newv;
    pword	dom[5];
    pword	*vlist, *ilist;
    word	from, to;
    word	i;
    long	firsti, lasti;
    int		updi, updv;
    word	isize, vsize;
    int		res = 0;
    word	lastv;
    word	lastv2;
    word	lastiv;
    word	lastiv2;
    uword	lastt = TEND;
    uword	lastt2 = TEND;
    uword	lastit = TEND;
    uword	lastit2 = TEND;

    if (IsInteger(ti)) {
	Bind_Var(vnewi, tnewi, vi.nint, TINT)
	p = &vt.ptr[vi.nint];
	Dereference_(p);
	Bind_Var(vnewv, tnewv, p->val.nint, p->tag.all)
	Bind_Var(vres, tres, RES_INSTANTIATED, TINT)
	Succeed_
    }
    Var_Domain(vi.ptr, p);
    p++;
    s = p + 1;
    Dereference_(p);	/* I domain list */
    Dereference_(s);
    isize = s->val.nint;
    if (!IsMeta(tv)) {
	v = dom;
	dom[1].val.ptr = dom + 3;
	dom[1].tag.kernel = TLIST;
	dom[3].val.nint = vv.nint;
	dom[3].tag.kernel = tv.kernel;
	dom[4].tag.kernel = TNIL;
	vsize = 1;
    } else {
	Var_Domain(vv.ptr, v);
	s = v + 2;
	Dereference_(s);
	vsize = s->val.nint;
    }
    if (vsize != vsv.nint) {
	updi = 1;
	newi = ilist = Gbl_Tg++;
    } else
	updi = 0;
    if (isize != vsi.nint) {
	updv = 1;
	newv = vlist = Gbl_Tg++;
    } else
	updv = 0;
    Check_Gc
    while (IsList(p->tag))
    {
	p = p->val.ptr;
	s = p++;
	Dereference_(s);
	if (IsInteger(s->tag))
	    from = to = s->val.nint;
	else {
	    s = s->val.ptr + 1;
	    t = s++;
	    Dereference_(t);
	    Dereference_(s);
	    from = t->val.nint;
	    to = s->val.nint;
	}
	lasti = to + 1;
	for (i = from; i <= to; i++) {
	    pword *argp = &vt.ptr[i];
	    Dereference_(argp);
	    /* lasti* used to speed up domain inclusion */
	    if (updi && (ElemEq(argp, lastiv, lastit) ||
		ElemEq(argp, lastiv2, lastit2) ||
		!dom_check_in(argp->val.nint, argp->tag, v)))
	    {
		/* add to the new index domain */
		if (i - 1 == lasti)
		    lasti++;
		else if (i - 1 > lasti) {
		    newi = insert_interval(firsti, lasti, newi);
		    firsti = lasti = i;
		}
		else
		    firsti = lasti = i;
		size++;
		if (!ElemEq(argp, lastiv, lastit) &&
		    !ElemEq(argp, lastiv2, lastit2))
		{
		    lastiv2 = lastiv;
		    lastit2 = lastit;
		    lastiv = argp->val.nint;
		    lastit = argp->tag.kernel;
		}
	    }
	    if (updv && !(updi && !ElemEq(argp, lastiv, lastit) &&
		!ElemEq(argp, lastv, lastt) &&
		!ElemEq(argp, lastv2, lastt2)))
	    {
		/* add to the value list */
		newv->val.ptr = Gbl_Tg;
		newv->tag.kernel = TLIST;
		newv = Gbl_Tg;
		Gbl_Tg += 2;
		Check_Gc;
		newv->val.nint = argp->val.nint;
		newv++->tag.kernel = argp->tag.kernel;
		lastv2 = lastv;
		lastt2 = lastt;
		lastv = argp->val.nint;
		lastt = argp->tag.kernel;
		sizev++;
	    }
	}
	if (lasti <= to)
	    newi = insert_interval(firsti, lasti, newi);
	Dereference_(p);
    }
    if (updi && size == 0 || updv && sizev == 0) {
	Fail_
    }
    if (updv)
	newv->tag.kernel = TNIL;
    if (updi)
	newi->tag.kernel = TNIL;
    if (!updi)
	size = isize;
    else if (size == isize)
	updi = 0;
    Bind_Var(vs, ts, size, TINT)
    if (!updi && sizev <= 3 && sizev == vsize) {
	Bind_Var(vnsv, tnsv, sizev, TINT)
	Bind_Var(vres, tres, 0, TINT)
	Succeed_
    }
    if (size == 1) {
	Bind_Var(vnewi, tnewi, firsti, TINT)
	p = &vt.ptr[firsti];
	Dereference_(p);
	Bind_Var(vnewv, tnewv, p->val.nint, p->tag.all)
	Bind_Var(vres, tres, RES_INSTANTIATED, TINT)
	Succeed_
    }
    if (updi) {
	Bind_Var(vnewi, tnewi, ilist->val.all, ilist->tag.all)
	res += 1;
    }
    if (updv && !(updi && sizev <= 3 && sizev == vsize)) {
	value		v1;
	int		err;
	word		ssv;
	pword		*vints;
	pword		sorted;
	pword		key;

	if (!updi && sizev == 1) {
	    Bind_Var(vnewv, tnewv, lastv, lastt)
	    Bind_Var(vres, tres, RES_INSTANTIATED, TINT)
	    Succeed_
	}
	v1.ptr = vlist->val.ptr;
	p = v + 1;
	Dereference_(p);
	key.val.nint = 0;
	key.tag.kernel = TINT;
	sorted.val.ptr = ec_keysort(v1, key.val, key.tag, 0, 0, 0, &err);
	sorted.tag.kernel = TLIST;
	vints = _dom_intersection(p, &sorted, &ssv);
	if (ssv == 0) {
	    Fail_
	} else {
	    Bind_Var(vnsv, tnsv, ssv, TINT)
	}
	if (ssv != vsize || ssv == 1) {
	    if (!updi && ssv == 1) {
		Bind_Var(vnewv, tnewv, lastv, lastt)
		res = RES_INSTANTIATED;
	    } else {
		Bind_Var(vnewv, tnewv, vints->val.all, vints->tag.all)
		res += 2;
	    }
	}
    } else {
	Bind_Var(vnsv, tnsv, vsize, TINT)
    }
    Bind_Var(vres, tres, res, TINT)
    Succeed_
}

/* p is the val.ptr of dom/2 */
int
dom_check_in(long int e, type tag, register pword *p)
{
    register pword	*s;
    register pword	*t;
    register int	res;
    value		v1;

    p++;
    Dereference_(p);
    v1.nint = e;
    if (IsInteger(tag))
    {
	while (IsList(p->tag))
	{
	    p = p->val.ptr;
	    s = p++;
	    Dereference_(s);
	    if (!IsFdInterval(s->val, s->tag)) {
		if (IsInteger(s->tag)) {
		    if (s->val.nint == e)
			return 0;
		    else if (s->val.nint > e)
			return 1;
		} else {
		    res = compare_terms(s->val, s->tag, v1, tag);
		    if (res == 0)
			return 0;
		    else if (res > 0)
			return 1;
		}
	    } else {
		s = s->val.ptr + 1;
		t = s++;
		Dereference_(t);
		Dereference_(s);
		if (t->val.nint > e)
		    return 1;
		else if (s->val.nint >= e)
		    return 0;
	    }
	    Dereference_(p);
	}
    }
    else
    {
	while (IsList(p->tag))
	{
	    p = p->val.ptr;
	    s = p++;
	    Dereference_(s);
	    if (!IsFdInterval(s->val, s->tag)) {
		res = compare_terms(s->val, s->tag, v1, tag);
		if (res == 0)
		    return 0;
		else if (res > 0)
		    return 1;
	    }
	    Dereference_(p);
	}
    }
    return 1;
}

pword *
insert_interval(long int first, long int last, pword *newi)
{
    newi->val.ptr = Gbl_Tg;
    newi->tag.kernel = TLIST;
    newi = Gbl_Tg;
    Gbl_Tg += 2;
    Check_Gc
    if (first == last) {
	newi->val.nint = first;
	newi++->tag.kernel = TINT;
    } else if (first + 1 == last) {
	newi->val.nint = first;
	newi++->tag.kernel = TINT;
	newi->val.ptr = Gbl_Tg;
	newi->tag.kernel = TLIST;
	newi = Gbl_Tg;
	Gbl_Tg += 2;
	Check_Gc
	newi->val.nint = last;
	newi++->tag.kernel = TINT;
    } else if (first < last) {
	pword		*p = Gbl_Tg;

	Gbl_Tg += 3;
	Check_Gc
	newi->val.ptr = p;
	newi++->tag.kernel = TCOMP;
	p[0].val.did = d_interval;
	p[0].tag.kernel = TDICT;
	p[1].val.nint = first;
	p[1].tag.kernel = TINT;
	p[2].val.nint = last;
	p[2].tag.kernel = TINT;
    }
    return newi;
}

/* dom_intersection(Dom1, Dom2, Inters, NewSize) */
static int
p_dom_intersection(value vd1, type td1, value vd2, type td2, value vi, type ti, value vs, type ts)
{
    register pword	*d1, *d2;	/* list pointers */
    register pword	*p;
    word		size;
    dident		dd;
    Prepare_Requests;

    Check_Domain(vd1, td1)
    Check_Domain(vd2, td2)
    dd = vd1.ptr->val.did;
    d1 = vd1.ptr + 1;
    Dereference_(d1);
    d2 = vd2.ptr + 1;
    Dereference_(d2);
    if (IsNil(d1->tag) || IsNil(d2->tag)) {
	Fail_;
    }
    d1 = _dom_intersection(d1, d2, &size);
    if (size == 0) {
	Fail_;
    }
    p = Gbl_Tg;
    Gbl_Tg += 3;
    Check_Gc;
    p[0].val.did = dd;
    p[0].tag.all = TDICT;
    p[1].val.ptr = d1->val.ptr;
    p[1].tag.all = d1->tag.all;
    p[2].val.nint = size;
    p[2].tag.all = TINT;
    Request_Unify_Integer(vs, ts, size);
    Request_Unify_Structure(vi, ti, p);
    Return_Unify;
}

static pword *
_dom_intersection(
    	register pword *d1,		/* input: list pointers */
	register pword *d2,
	word *dsize)			/* output: intersection size */
    			       
{
    register pword	*s1, *s2;
    register pword	*t1, *t2;
    register pword	*p;
    long		from1, from2, fromi, fromj;
    long		to1, to2, toi, toj;
    long		tag1, tag2;
    word		size = 0;
    pword		*ints;		/* result */
    int			res;
    int			was_int = 0;

    p = ints = Gbl_Tg;
    Gbl_Tg++;
    Check_Gc;
    d1 = d1->val.ptr;
    s1 = d1++;
    Dereference_(s1);
    if (IsInteger(s1->tag)) {
	tag1 = TINT;
	from1 = to1 = s1->val.nint;
    } else if (!IsFdInterval(s1->val, s1->tag)) {
	tag1 = s1->tag.kernel;
    } else {
	s1 = s1->val.ptr + 1;
	t1 = s1++;
	Dereference_(t1);
	Dereference_(s1);
	from1 = t1->val.nint;
	to1 = s1->val.nint;
	tag1 = TINT;
    }
    d2 = d2->val.ptr;
    s2 = d2++;
    Dereference_(s2);
    if (IsInteger(s2->tag)) {
	tag2 = TINT;
	from2 = to2 = s2->val.nint;
    } else if (!IsFdInterval(s2->val, s2->tag)) {
	tag2 = s2->tag.kernel;
    } else {
	s2 = s2->val.ptr + 1;
	t2 = s2++;
	Dereference_(t2);
	Dereference_(s2);
	from2 = t2->val.nint;
	to2 = s2->val.nint;
	tag2 = TINT;
    }
    for (;;)
    {
	if (IsTag(tag1, TINT) && IsTag(tag2, TINT)) {
	    fromi = from1 > from2 ? from1 : from2;
	    if (to1 > to2) {
		toi = to2;
		res = 1;
	    } else {
		res = to1 < to2 ? -1 : 0;
		toi = to1;
	    }
	    if (fromi <= toi) {
		if (was_int) {
		    if (fromi <= toj + 1) {	/* merge */
			if (toi > toj)
			    toj = toi;
		    }
		    else {
			p = insert_interval(fromj, toj, p);
			size += toj - fromj + 1;
			fromj = fromi;
			toj = toi;
		    }
		} else {
		    fromj = fromi;
		    toj = toi;
		    was_int = 1;
		}
	    }
	} else {
	    res = compare_terms(s1->val, s1->tag, s2->val, s2->tag);
	    if (!res) {
		if (was_int) {
		    p = insert_interval(fromj, toj, p);
		    size += toj - fromj + 1;
		    was_int = 0;
		}
		p->val.ptr = Gbl_Tg;
		p->tag.kernel = TLIST;
		p = Gbl_Tg;
		Gbl_Tg += 2;
		Check_Gc
		p->val.all = s1->val.all;
		p++->tag.kernel = s1->tag.kernel;
		size++;
	    }
	}
	if (res <= 0) {
	    Dereference_(d1);
	    if (IsNil(d1->tag))
		break;
	    d1 = d1->val.ptr;
	    s1 = d1++;
	    Dereference_(s1);
	    if (IsInteger(s1->tag)) {
		tag1 = TINT;
		from1 = to1 = s1->val.nint;
	    } else if (!IsFdInterval(s1->val, s1->tag)) {
		tag1 = s1->tag.kernel;
	    } else {
		s1 = s1->val.ptr + 1;
		t1 = s1++;
		Dereference_(t1);
		Dereference_(s1);
		from1 = t1->val.nint;
		to1 = s1->val.nint;
		tag1 = TINT;
	    }
	}
	if (res >= 0) {
	    Dereference_(d2);
	    if (IsNil(d2->tag))
		break;
	    d2 = d2->val.ptr;
	    s2 = d2++;
	    Dereference_(s2);
	    if (IsInteger(s2->tag)) {
		tag2 = TINT;
		from2 = to2 = s2->val.nint;
	    } else if (!IsFdInterval(s2->val, s2->tag)) {
		tag2 = s2->tag.kernel;
	    } else {
		s2 = s2->val.ptr + 1;
		t2 = s2++;
		Dereference_(t2);
		Dereference_(s2);
		from2 = t2->val.nint;
		to2 = s2->val.nint;
		tag2 = TINT;
	    }
	}
    }
    if (was_int) {
	p = insert_interval(fromj, toj, p);
	size += toj - fromj + 1;
    }
    p->tag.all = TNIL;
    *dsize = size;
    if (size == 0)
	return 0;
    return ints;
}

/* dom_compare(Comp, Dom1, Dom2) */
static int
p_dom_compare(value vc, type tc, value vd1, type td1, value vd2, type td2)
{
    register pword	*d1, *d2;	/* list pointers */
    register pword	*s1, *s2;
    register pword	*t1, *t2;
    register long	tag1, tag2;
    long		from1, from2;
    long		to1, to2;
    int			res = EQ;
    int			comp;
    int			next = 0;
    int			move;

    Check_Output_Atom(tc);
    Check_Domain(vd1, td1)
    Check_Domain(vd2, td2)
    d1 = vd1.ptr + 1;
    Dereference_(d1);
    d2 = vd2.ptr + 1;
    Dereference_(d2);
    if (IsNil(d1->tag)) {
	if (IsNil(d2->tag)) {
	    Return_Unify_Atom(vc, tc, d_.unify0)
	} else {
	    Return_Unify_Atom(vc, tc, d_.inf0)
	}
    } else if (IsNil(d2->tag)) {
	Return_Unify_Atom(vc, tc, d_.sup0)
    }
    d1 = d1->val.ptr;
    s1 = d1++;
    Dereference_(s1);
    if (!IsFdInterval(s1->val, s1->tag)) {
	tag1 = s1->tag.kernel;
	from1 = to1 = s1->val.nint;
    } else {
	s1 = s1->val.ptr + 1;
	t1 = s1++;
	Dereference_(t1);
	Dereference_(s1);
	from1 = t1->val.nint;
	to1 = s1->val.nint;
	tag1 = TINT;
    }
    d2 = d2->val.ptr;
    s2 = d2++;
    Dereference_(s2);
    if (!IsFdInterval(s2->val, s2->tag)) {
	tag2 = s2->tag.kernel;
	from2 = to2 = s2->val.nint;
    } else {
	s2 = s2->val.ptr + 1;
	t2 = s2++;
	Dereference_(t2);
	Dereference_(s2);
	from2 = t2->val.nint;
	to2 = s2->val.nint;
	tag2 = TINT;
    }
    move = DOM_BOTH;
    for (;;)
    {
	if (move == DOM_BOTH && IsTag(tag1, TINT) && IsTag(tag2, TINT)) {
	    if (from1 < from2) {
		res &= GT;
		if (next == WAIT_2) {
		    Fail_
		}
	    } else if (from1 > from2) {
		res &= LT;
		if (next == WAIT_1) {
		    Fail_
		}
	    } else
		next = 0;
	    if (to1 < to2) {
		if (to1 >= from2) {
		    from2 = to1 + 1;
		} else
		    next = WAIT_1;
		comp = -1;
	    } else if (to1 > to2) {
		if (to2 >= from1) {
		    from1 = to2 + 1;
		} else
		    next = WAIT_2;
		comp = 1;
	    } else {
		comp = 0;
	    }
	} else if (move == DOM_BOTH) {
	    comp = compare_terms(s1->val, s1->tag, s2->val, s2->tag);
	    if (comp < 0) {
		if (next == WAIT_2) {
		    Fail_
		}
		res &= GT;
		next = WAIT_1;
	    }
	    else if (comp > 0) {
		if (next == WAIT_1) {
		    Fail_
		}
		res &= LT;
		next = WAIT_2;
	    } else
		next = 0;
	} else if (move == DOM_1) {
	    if (next == WAIT_2) {
		Fail_
	    }
	    res &= GT;
	    break;
	} else if (move == DOM_2) {
	    if (next == WAIT_1) {
		Fail_
	    }
	    res &= LT;
	    break;
	} else
	    break;
	if (!res) {
	    Fail_;
	}
	if (comp <= 0) {
	    Dereference_(d1);
	    if (IsNil(d1->tag))
		move &= DOM_2;
	    else {
		d1 = d1->val.ptr;
		s1 = d1++;
		Dereference_(s1);
		if (IsInteger(s1->tag)) {
		    from1 = to1 = s1->val.nint;
		    tag1 = TINT;
		} else if (!IsFdInterval(s1->val, s1->tag)) {
		    tag1 = s1->tag.kernel;
		} else {
		    s1 = s1->val.ptr + 1;
		    t1 = s1++;
		    Dereference_(t1);
		    Dereference_(s1);
		    from1 = t1->val.nint;
		    to1 = s1->val.nint;
		    tag1 = TINT;
		}
	    }
	}
	if (comp >= 0) {
	    Dereference_(d2);
	    if (IsNil(d2->tag))
		move &= DOM_1;
	    else {
		d2 = d2->val.ptr;
		s2 = d2++;
		Dereference_(s2);
		if (IsInteger(s2->tag)) {
		    from2 = to2 = s2->val.nint;
		    tag2 = TINT;
		} else if (!IsFdInterval(s2->val, s2->tag)) {
		    tag2 = s2->tag.kernel;
		} else {
		    s2 = s2->val.ptr + 1;
		    t2 = s2++;
		    Dereference_(t2);
		    Dereference_(s2);
		    from2 = t2->val.nint;
		    to2 = s2->val.nint;
		    tag2 = TINT;
		}
	    }
	}
    }
    if (!res) {
	Fail_;
    }
    Return_Unify_Atom(vc, tc, (res == EQ) ? d_.unify0 : (
				(res == LT) ? d_.inf0 : d_.sup0))
}

/* dom_union(Dom1, Dom2, Union, NewSize) */
static int
p_dom_union(value vd1, type td1, value vd2, type td2, value vu, type tu, value vs, type ts)
{
    register pword	*d1, *d2;	/* list pointers */
    register pword	*s1, *s2;
    register pword	*t1, *t2;
    register pword	*p;
    long		from1, from2, fromi;
    long		to1, to2, toi;
    register long	tag1, tag2;
    word		size = 0;
    word		size1, size2;
    pword		*ints;		/* result */
    dident		dd;
    int			next;
    int			res;
    int			was_int = 0;
    int			can_leave = 0;
    Prepare_Requests;

    next = DOM_NONE;
    Check_Domain(vd1, td1)
    Check_Domain(vd2, td2)
    dd = vd1.ptr->val.did;
    d1 = vd1.ptr + 1;
    t1 = d1 + 1;
    Dereference_(d1);
    Dereference_(t1);
    size1 = t1->val.nint;
    d2 = vd2.ptr + 1;
    t2 = d2 + 1;
    Dereference_(d2);
    Dereference_(t2);
    size2 = t2->val.nint;
    if (IsNil(d1->tag)) {
	if (IsNil(d2->tag)) {
	    Fail_
	} else {
	    Request_Unify_Integer(vs, ts, size2);
	    Request_Unify_Structure(vu, tu, vd2.ptr);
	    Return_Unify;
	}
    } else {
	d1 = d1->val.ptr;
	s1 = d1++;
	Dereference_(s1);
	if (!IsFdInterval(s1->val, s1->tag)) {
	    tag1 = s1->tag.kernel;
	    from1 = to1 = s1->val.nint;
	} else {
	    s1 = s1->val.ptr + 1;
	    t1 = s1++;
	    Dereference_(t1);
	    Dereference_(s1);
	    from1 = t1->val.nint;
	    to1 = s1->val.nint;
	    tag1 = TINT;
	}
	next |= DOM_1;
    }
    if (IsNil(d2->tag)) {
	Request_Unify_Integer(vs, ts, size1);
	Request_Unify_Structure(vu, tu, vd1.ptr);
	Return_Unify;
    } else {
	d2 = d2->val.ptr;
	s2 = d2++;
	Dereference_(s2);
	if (!IsFdInterval(s2->val, s2->tag)) {
	    tag2 = s2->tag.kernel;
	    from2 = to2 = s2->val.nint;
	} else {
	    s2 = s2->val.ptr + 1;
	    t2 = s2++;
	    Dereference_(t2);
	    Dereference_(s2);
	    from2 = t2->val.nint;
	    to2 = s2->val.nint;
	    tag2 = TINT;
	}
	next |= DOM_2;
    }
    p = ints = Gbl_Tg;
    Gbl_Tg++;
    Check_Gc;
    for (;;)
    {
	if (IsTag(tag1, TINT) && IsTag(tag2, TINT)) {
	    if (next == DOM_BOTH && from1 <= from2 || next == DOM_1)
		res = -1;
	    else
		res = 1;
	} else if (next == DOM_BOTH)
	    res = compare_terms(s1->val, s1->tag, s2->val, s2->tag);
	else if (next == DOM_1)
	    res = -1;
	else
	    res = 1;
	if (res <= 0) {
	    if (IsTag(tag1, TINT)) {
		if (was_int) {
		    if (from1 <= toi + 1) {	/* merge */
			if (to1 > toi)
			    toi = to1;
			can_leave = 0;
		    } else {
			p = insert_interval(fromi, toi, p);
			size += toi - fromi + 1;
			fromi = from1;
			toi = to1;
			can_leave = 1;
		    }
		} else {
		    fromi = from1;
		    toi = to1;
		    was_int = 1;
		    can_leave = 0;
		}
		size1 -= to1 - from1 + 1;
	    } else {	/* atomic */
		if (was_int) {
		    p = insert_interval(fromi, toi, p);
		    size += toi - fromi + 1;
		    was_int = 0;
		}
		p->val.ptr = Gbl_Tg;
		p->tag.kernel = TLIST;
		p = Gbl_Tg;
		Gbl_Tg += 2;
		Check_Gc
		size++;
		p->val.all = s1->val.all;
		p++->tag.kernel = s1->tag.kernel;
		size1--;
		can_leave = 1;
	    }
	    Dereference_(d1);
	    if (!(next & DOM_2) && (can_leave || IsNil(d1->tag))) {
		size += size1;
		break;
	    }
	    if (IsNil(d1->tag)) {
		next &= ~DOM_1;
	    } else {
		d1 = d1->val.ptr;
		s1 = d1++;
		Dereference_(s1);
		if (IsInteger(s1->tag)) {
		    from1 = to1 = s1->val.nint;
		    tag1 = TINT;
		} else if (!IsFdInterval(s1->val, s1->tag)) {
		    tag1 = s1->tag.kernel;
		} else {
		    s1 = s1->val.ptr + 1;
		    t1 = s1++;
		    Dereference_(t1);
		    Dereference_(s1);
		    from1 = t1->val.nint;
		    to1 = s1->val.nint;
		    tag1 = TINT;
		}
	    }
	}
	if (res >= 0) {
	    if (IsTag(tag2, TINT)) {
		if (was_int) {
		    if (from2 <= toi + 1) {	/* merge */
			if (to2 > toi)
			    toi = to2;
			can_leave = 0;
		    } else {
			p = insert_interval(fromi, toi, p);
			size += toi - fromi + 1;
			fromi = from2;
			toi = to2;
			can_leave = 1;
		    }
		} else {
		    fromi = from2;
		    toi = to2;
		    was_int = 1;
		    can_leave = 1;
		}
		size2 -= to2 - from2 + 1;
	    } else if (res > 0) {	/* atomic */
		if (was_int) {
		    p = insert_interval(fromi, toi, p);
		    size += toi - fromi + 1;
		    was_int = 0;
		}
		p->val.ptr = Gbl_Tg;
		p->tag.kernel = TLIST;
		p = Gbl_Tg;
		Gbl_Tg += 2;
		Check_Gc
		size++;
		p->val.all = s2->val.all;
		p++->tag.kernel = s2->tag.kernel;
		size2--;
		can_leave = 1;
	    } else
		size2--;
	    Dereference_(d2);
	    if (!(next & DOM_1) && (can_leave || IsNil(d2->tag))) {
		size += size2;
		d1 = d2;
		break;
	    }
	    if (IsNil(d2->tag)) {
		next &= ~DOM_2;
		continue;
	    }
	    d2 = d2->val.ptr;
	    s2 = d2++;
	    Dereference_(s2);
	    if (IsInteger(s2->tag)) {
		from2 = to2 = s2->val.nint;
		tag2 = TINT;
	    } else if (!IsFdInterval(s2->val, s2->tag)) {
		tag2 = s2->tag.kernel;
	    } else {
		s2 = s2->val.ptr + 1;
		t2 = s2++;
		Dereference_(t2);
		Dereference_(s2);
		from2 = t2->val.nint;
		to2 = s2->val.nint;
		tag2 = TINT;
	    }
	}
    }
    if (was_int) {
	p = insert_interval(fromi, toi, p);
	size += toi - fromi + 1;
    }
    *p = *d1;
    if (size == 0) {
	Fail_;
    }
    p = Gbl_Tg;
    Gbl_Tg += 3;
    Check_Gc;
    p[0].val.did = dd;
    p[0].tag.all = TDICT;
    p[1].val.ptr = ints->val.ptr;
    p[1].tag.all = TLIST;
    p[2].val.nint = size;
    p[2].tag.all = TINT;
    Request_Unify_Integer(vs, ts, size);
    Request_Unify_Structure(vu, tu, p);
    Return_Unify;
}

/* dom_difference(Dom1, Dom2, Diff, NewSize) */
static int
p_dom_difference(value vd1, type td1, value vd2, type td2, value vi, type ti, value vs, type ts)
{
    register pword	*d1, *d2;	/* list pointers */
    register pword	*s1, *s2;
    register pword	*t1, *t2;
    register pword	*p;
    register long	tag1, tag2;
    long		from1, from2;
    long		to1, to2, toi;
    word		size = 0;
    word		size1;
    pword		*diff;		/* result */
    dident		dd;
    int			res;
    int			was_int = 0;
    Prepare_Requests;
    d1 = vd1.ptr + 1;

    Check_Domain(vd1, td1)
    Check_Domain(vd2, td2)
    dd = vd1.ptr->val.did;
    d1 = vd1.ptr + 1;
    t1 = d1 + 1;
    Dereference_(d1);
    Dereference_(t1);
    size1 = t1->val.nint;
    d2 = vd2.ptr + 1;
    Dereference_(d2);
    if (IsNil(d1->tag)) {
	Fail_;
    }
    else if (IsNil(d2->tag)) {
	t1 = vd1.ptr + 2;
	Dereference_(t1);
	size = t1->val.nint;
	Request_Unify_Integer(vs, ts, size);
	Request_Unify_Structure(vi, ti, vd1.ptr);
	Return_Unify;
    }
    p = diff = Gbl_Tg;
    Gbl_Tg++;
    Check_Gc;
    d1 = d1->val.ptr;
    s1 = d1++;
    Dereference_(s1);
    if (IsInteger(s1->tag)) {
	tag1 = TINT;
	from1 = to1 = s1->val.nint;
	size1--;
	was_int = 1;
    } else if (!IsFdInterval(s1->val, s1->tag)) {
	tag1 = s1->tag.kernel;
	size1--;
    } else {
	s1 = s1->val.ptr + 1;
	t1 = s1++;
	Dereference_(t1);
	Dereference_(s1);
	from1 = t1->val.nint;
	to1 = s1->val.nint;
	tag1 = TINT;
	size1 -= to1 - from1 + 1;
	was_int = 1;
    }
    d2 = d2->val.ptr;
    s2 = d2++;
    Dereference_(s2);
    if (!IsFdInterval(s2->val, s2->tag)) {
	tag2 = s2->tag.kernel;
	to2 = from2 = s2->val.nint;
    } else {
	s2 = s2->val.ptr + 1;
	t2 = s2++;
	Dereference_(t2);
	Dereference_(s2);
	from2 = t2->val.nint;
	to2 = s2->val.nint;
	tag2 = TINT;
    }
    for (;;)
    {
	if (IsTag(tag1, TINT) && IsTag(tag2, TINT)) {
	    if (from1 < from2) {
		toi = to1 < from2 ? to1 : from2 - 1;
		p = insert_interval(from1, toi, p);
		size += toi - from1 + 1;
	    }
	    if (to1 > to2) {
		if (from1 <= to2)
		    from1 = to2 + 1;
		res = 1;
		was_int = 1;
	    } else {
		res = to1 < to2 ? -1 : 0;
		was_int = 0;
	    }
	} else {
	    res = compare_terms(s1->val, s1->tag, s2->val, s2->tag);
	    Dereference_(d2);
	    if (res < 0 || res > 0 && IsNil(d2->tag)) {
		if (IsTag(tag1, TINT)) {
		    p = insert_interval(from1, to1, p);
		    size += to1 - from1 + 1;
		    was_int = 0;
		} else {
		    p->val.ptr = Gbl_Tg;
		    p->tag.kernel = TLIST;
		    p = Gbl_Tg;
		    Gbl_Tg += 2;
		    Check_Gc
		    p->val.all = s1->val.all;
		    p++->tag.kernel = s1->tag.kernel;
		    size++;
		}
	    }
	}
	if (res >= 0) {
	    Dereference_(d2);
	    if (IsNil(d2->tag)) {
		size += size1;
		break;
	    }
	    d2 = d2->val.ptr;
	    s2 = d2++;
	    Dereference_(s2);
	    if (IsInteger(s2->tag)) {
		tag2 = TINT;
		from2 = to2 = s2->val.nint;
	    } else if (!IsFdInterval(s2->val, s2->tag)) {
		tag2 = s2->tag.kernel;
	    } else {
		s2 = s2->val.ptr + 1;
		t2 = s2++;
		Dereference_(t2);
		Dereference_(s2);
		from2 = t2->val.nint;
		to2 = s2->val.nint;
		tag2 = TINT;
	    }
	}
	if (res <= 0) {
	    Dereference_(d1);
	    if (IsNil(d1->tag))
		break;
	    d1 = d1->val.ptr;
	    s1 = d1++;
	    Dereference_(s1);
	    if (IsInteger(s1->tag)) {
		tag1 = TINT;
		from1 = to1 = s1->val.nint;
		size1--;
		was_int = 1;
	    } else if (!IsFdInterval(s1->val, s1->tag)) {
		tag1 = s1->tag.kernel;
		size1--;
	    } else {
		s1 = s1->val.ptr + 1;
		t1 = s1++;
		Dereference_(t1);
		Dereference_(s1);
		from1 = t1->val.nint;
		to1 = s1->val.nint;
		tag1 = TINT;
		size1 -= to1 - from1 + 1;
		was_int = 1;
	    }
	}
    }
    if (was_int) {
	p = insert_interval(from1, to1, p);
	size += to1 - from1 + 1;
    }
    Dereference_(d1);
    *p = *d1;
    if (size == 0) {
	Fail_;
    }
    p = Gbl_Tg;
    Gbl_Tg += 3;
    Check_Gc;
    p[0].val.did = dd;
    p[0].tag.all = TDICT;
    p[1].val.ptr = diff->val.ptr;
    p[1].tag.all = TLIST;
    p[2].val.nint = size;
    p[2].tag.all = TINT;
    Request_Unify_Integer(vs, ts, size);
    Request_Unify_Structure(vi, ti, p);
    Return_Unify;
}


/* dvar_remove_smaller(Var, Min) */
static int
p_dvar_remove_smaller(value vvar, type tvar, value vm, type tm)
{
    register pword	*v;
    register pword	*p;
    long		oldsize, size;

    Check_Integer(tm)
    if (!IsMeta(tvar)) {
	Check_Integer(tvar)
	Succeed_If(vvar.nint >= vm.nint)
    }
    Check_Dvar(vvar.ptr, v);
    Attr_Domain(v, v);
    p = v + 2;
    Dereference_(p);
    oldsize = p->val.nint;
    size = dom_remove_smaller(v, vm.nint);
    Check_Return(size)
    if (!size) {
	Fail_
    }
    if (size < oldsize)
	oldsize = _domain_changed(vvar.ptr, size, RES_MIN);
    Check_Return(oldsize)
    Succeed_
}

/* dvar_remove_greater(Var, Max) */
static int
p_dvar_remove_greater(value vvar, type tvar, value vm, type tm)
{
    register pword	*v;
    register pword	*p;
    long		oldsize, size;

    Check_Integer(tm)
    if (!IsMeta(tvar)) {
	Check_Integer(tvar)
	Succeed_If(vvar.nint <= vm.nint)
    }
    Check_Dvar(vvar.ptr, v);
    Attr_Domain(v, v);
    p = v + 2;
    Dereference_(p);
    oldsize = p->val.nint;
    size = dom_remove_greater(v, vm.nint);
    Check_Return(size)
    if (!size) {
	Fail_
    }
    if (size < oldsize)
	oldsize = _domain_changed(vvar.ptr, size, RES_MAX);
    Check_Return(oldsize)
    Succeed_
}

int
dom_remove_greater(register pword *p, register long int max)
{
    register pword	*s;
    register pword	*t;
    register pword	*r;
    register pword	*u;
    pword		*newd;
    pword		*dom;
    word		size = 0;
    value		v0;

    dom = p++;
    Dereference_(p);
    newd = r = Gbl_Tg;
    Gbl_Tg++;
    Check_Gc
    while (IsList(p->tag))
    {
	p = p->val.ptr;
	s = p++;
	Dereference_(s);
	if (IsInteger(s->tag)) {
	    if (s->val.nint  <= max) {
		r->tag.kernel = TLIST;
		r->val.ptr = Gbl_Tg;
		r = Gbl_Tg;
		Gbl_Tg += 2;
		Check_Gc
		r->val.nint = s->val.nint;
		r++->tag.kernel = TINT;
		size++;
	    }
	    else
		break;
	} else if (!IsFdInterval(s->val, s->tag))
	    return TYPE_ERROR;
	else {
	    u = s;
	    s = s->val.ptr + 1;
	    t = s++;
	    Dereference_(t);
	    Dereference_(s);
	    if (t->val.nint <= max) {
		if (s->val.nint <= max) {
		    r->tag.kernel = TLIST;
		    r->val.ptr = Gbl_Tg;
		    r = Gbl_Tg;
		    Gbl_Tg += 2;
		    Check_Gc
		    *r++ = *u;
		    size += s->val.nint - t->val.nint + 1;
		}
		else {
		    r = insert_interval(t->val.nint, max, r);
		    size += max - t->val.nint + 1;
		    break;
		}
	    }
	    else
		break;
	}
	Dereference_(p);
    }
    r->tag.kernel = TNIL;
    if (size) {
	(void) ec_assign(dom + 1, newd->val, newd->tag);
	v0.nint = size;
	(void) ec_assign(dom + 2, v0, tint);
    }
    return size;
}

/* p is the val.ptr of dom/2 */
int
dom_remove_smaller(register pword *p, register long int min)
{
    register pword	*s;
    register pword	*t;
    register pword	*r;
    pword		*newd;
    pword		*dom;
    word		size;
    value		v0;

    dom = p++;
    s = p + 1;
    Dereference_(p);
    Dereference_(s);
    size = s->val.nint;
    while (IsList(p->tag))
    {
	p = p->val.ptr;
	s = p++;
	Dereference_(s);
	if (IsInteger(s->tag)) {
	    if (s->val.nint  >= min) {
		newd = p - 1;
		break;
	    } else
		size--;
	} else if (!IsFdInterval(s->val, s->tag))
	    return TYPE_ERROR;
	else {
	    s = s->val.ptr + 1;
	    t = s++;
	    Dereference_(t);
	    Dereference_(s);
	    if (s->val.nint < min)
		size -= s->val.nint - t->val.nint + 1;
	    else {
		if (t->val.nint >= min) {
		    newd = p - 1;
		    break;
		}
		else {
		    newd = r = Gbl_Tg;
		    Gbl_Tg++;
		    Check_Gc
		    r = insert_interval(min, s->val.nint, r);
		    size -= min - t->val.nint;
		    *r = *p;
		    newd = newd->val.ptr;
		    break;
		}
	    }
	}
	Dereference_(p);
    }
    if (size) {
	v0.ptr = newd;
	(void) ec_assign(dom + 1, v0, tlist);
	v0.nint = size;
	(void) ec_assign(dom + 2, v0, tint);
    }
    return size;
}

/* dvar_remove_element(DVar, El) */
static int
p_dvar_remove_element(value vvar, type tvar, value vel, type tel)
{
    register pword	*d;
    int			res;

    Check_Element(vel, tel)
    if (!IsMeta(tvar)) {
	Check_Element(vvar, tvar)
	Succeed_If(compare_terms(vvar, tvar, vel, tel))
    }
    Check_Dvar(vvar.ptr, d);
    res = _remove_element(vvar.ptr, vel.nint, tel.kernel);
    Check_Return(res);
    if (res == RES_FAIL) {
	Fail_
    }
    Succeed_
}

static int
_remove_element(pword *var, long int el, long int tag)
{
    int			res;
    register pword	*v;
    pword		inst;

    Var_Domain(var, v);
    res = dom_remove_element(v, el, tag, &inst);
    switch (res)
    {
    case RES_FAIL:
	return RES_FAIL;

    case RES_NO_CHANGE:
	return RES_SOLVED;

    case RES_INSTANTIATED:
	Bind_Var(var->val, var->tag, inst.val.all, inst.tag.kernel)
	return RES_SOLVED;

    case RES_MIN:
	/* We don't know the size, but we know it is > 1 */
	res = _domain_changed(var, 2L, RES_MIN);
	return res < 0 ? res : RES_WAKE;

    case RES_MAX:
	res = _domain_changed(var, 2L, RES_MAX);
	return res < 0 ? res : RES_WAKE;

    case RES_ANY:
	res = _domain_changed(var, 2L, 0);
	return res < 0 ? res : RES_WAKE;

    default:
	return res;
    }
}

static int
p_remove_element(value vvar, type tvar, value vel, type tel, value vres, type tres)
{
    int			res;

    if (!IsMeta(tvar)) {
	if (IsRef(tvar) || IsFdInterval(vvar, tvar)) {
	    Bind_Var(vres, tres, RES_ERROR, TINT)
	    Succeed_
	}
	Succeed_If(!SameType(tvar,tel) || !IsNil(tvar) && vvar.all != vel.all)
    }
    res = _remove_element(vvar.ptr, vel.nint, tel.kernel);
    Check_Return(res);
    if (res == RES_FAIL) {
	Fail_
    }
    Bind_Var(vres, tres, res, TINT)
    Succeed_
}

int
dom_remove_element(register pword *p, register long int el, long int tag, pword *inst)
{
    register pword	*s;
    register pword	*t;
    register pword	*r;
    register pword	*u;
    pword		*newd;
    pword		*dom;
    value		v0;
    type		t0;
    int			st = 1;
    int			res = RES_NO_CHANGE;
    pword		*elem;
    int			comp;
    word		size;

    dom = p++;
    Dereference_(p);
    s = dom + 2;
    Dereference_(s);
    size = s->val.nint;
    newd = r = Gbl_Tg;
    Gbl_Tg++;
    Check_Gc
    v0.nint = el;
    t0.kernel = tag;
    while (IsList(p->tag))
    {
	p = p->val.ptr;
	s = p++;
	Dereference_(s);
	if (!IsFdInterval(s->val, s->tag)) {
	    if (IsInteger(s->tag) && IsTag(tag, TINT)) {
		if (s->val.nint == el)
		    comp = 0;
		else if (s->val.nint < el)
		    comp = -1;
		else
		    comp = 1;
	    } else
		comp = compare_terms(s->val, s->tag, v0, t0);
	    if (!comp) {
		*r = *p;
		res = st ? RES_MIN : RES_MAX;
		if (st && size == 2) {
		    Dereference_(p);
		    if (!IsList(p->tag))
			return RES_FAIL;
		    p = p->val.ptr;
		    Dereference_(p);
		    elem = p;
		}
		break;
	    }
	    else if (comp > 0)
		break;
	    else {
		r->tag.kernel = TLIST;
		r->val.ptr = Gbl_Tg;
		r = Gbl_Tg;
		Gbl_Tg += 2;
		Check_Gc
		elem = s;
		r->val.nint = s->val.nint;
		r++->tag.kernel = s->tag.kernel;
	    }
	} else {
	    u = s;
	    s = s->val.ptr + 1;
	    t = s++;
	    Dereference_(t);
	    Dereference_(s);
	    if (IsTag(tag, TINT)) {
		if (s->val.nint < el)
		    comp = 1;
		else
		    comp = 0;
	    }
	    else
		comp = 1;
	    if (comp)
	    {			/* interval is before the element */
		r->tag.kernel = TLIST;
		r->val.ptr = Gbl_Tg;
		r = Gbl_Tg;
		Gbl_Tg += 2;
		Check_Gc
		*r++ = *u;
	    }
	    else {
		if (t->val.nint <= el) {
		    if (t->val.nint < el) {
			elem = t;
			r = insert_interval(t->val.nint, el - 1, r);
			res = RES_ANY;
		    } else {
			elem = s;
			res = st ? RES_MIN : RES_ANY;
		    }
		    if (s->val.nint > el) {
			r = insert_interval(el + 1, s->val.nint, r);
			if (!res)
			    res = RES_ANY;
		    } else
			res = RES_MAX;
		    break;
		}
		else
		    break;		/* interval is after the element */
	    }
	}
	Dereference_(p);
	st = 0;
    }
    Dereference_(p);
    *r = *p;
    if (res != RES_NO_CHANGE) {
	if (size <= 1)
	    return RES_FAIL;
	else if (size == 2) {
	    *inst = *elem;
	    return RES_INSTANTIATED;
	}
	if (res == RES_MAX && !IsNil(p->tag))
	    res = RES_ANY;
	(void) ec_assign(dom + 1, newd->val, newd->tag);
	v0.nint = size - 1;
	(void) ec_assign(dom + 2, v0, tint);
	return res;
    }
    else
	return RES_NO_CHANGE;
}

static int
p_dvar_replace(value vvar, type tvar, value vn, type tn)
{
    register pword	*dom;
    register pword	*s;
    register word	size;

    Check_Meta(tvar)
    Check_Domain(vn, tn);
    Check_Dvar(vvar.ptr, dom)

    s = vn.ptr + 2;
    Dereference_(s);
    size = s->val.nint;
    if (size == 0) {
	Fail_
    }

    s = dom = dom->val.ptr + DOMAIN_OFF;
    Dereference_(s);
    s = s->val.ptr + 2;
    Dereference_(s);
    if (s->val.nint == size) {
	Succeed_
    } else if (s->val.nint < size) {
	Bip_Error(RANGE_ERROR)
    }
    return ec_assign(dom, vn, tn);
}

static word
_dom_value(register pword *p)
{
    p++;
    Dereference_(p);
    p = p->val.ptr;
    Dereference_(p);
    if (IsInteger(p->tag))
	return p->val.nint;
    else {
	p = p->val.ptr + 1;
	Dereference_(p);
	return p->val.nint;
    }
}

/*
 * Take care of suspended lists and variables after a domain update.
 * If the domain is a singleton, instantiate the variable. Schedule
 * the appropriate lists and reset them in the domain.
 */
static int
_domain_changed(pword *var, long int size, int which)
{
    register pword	*attr;
    register pword	*p;
    word		val;
    int			res;

    if (size == 0)
	return PFAIL;
    Var_Attr(var, attr);
    if (size == 1)
    {
	/* get the element */
	Attr_Domain(attr, p)
	val = _dom_value(p);
	Bind_Var(var->val, var->tag, val, TINT);

	/* schedule the lists, otherwise attr_instantiate in the unify-handler
	 * could think that no waking is necessary, because it sees a domain
	 * which is already reduced.  */

	attr = attr->val.ptr;
	p = attr + ANY_OFF;
	Dereference_(p);
	res = p_schedule_woken(p->val, p->tag);
	Check_Return(res);

	if (which & RES_MIN) {
	    p = attr + MIN_OFF;
	    Dereference_(p);
	    res = p_schedule_woken(p->val, p->tag);
	    Check_Return(res);
	}
	if (which & RES_MAX) {
	    p = attr + MAX_OFF;
	    Dereference_(p);
	    res = p_schedule_woken(p->val, p->tag);
	    Check_Return(res);
	}
    }
    else	/* schedule and update the suspension lists */
    {
	attr = attr->val.ptr;
	res = ec_schedule_susps(attr + ANY_OFF);
	Check_Return(res);

	if (which & RES_MIN) {
	    res = ec_schedule_susps(attr + MIN_OFF);
	    Check_Return(res);
	}
	if (which & RES_MAX) {
	    res = ec_schedule_susps(attr + MAX_OFF);
	    Check_Return(res);
	}
    }
    return notify_constrained(var);
}

static int
p_prune_woken_goals(value val, type tag)	/* must be dereferenced */
{
    register word	arity;
    register int	res;
    register pword	*arg;

    for (;;)
    {
	if (IsList(tag))
	    arity = 2;
	else if (IsStructure(tag))
	{
	    arity = DidArity(val.ptr->val.did);
	    val.ptr++;
	}
	else if IsMeta(tag) {
	    arg = MetaTerm(val.ptr);
	    Dereference_(arg);
	    if (!IsStructure(arg->tag))
		{ Succeed_; }
	    arg = arg->val.ptr + domain_slot;
	    Dereference_(arg);
	    if (!IsStructure(arg->tag))
		{ Succeed_; }
	    arg = arg->val.ptr;
	    res = ec_prune_suspensions(arg + MIN_OFF);
	    Check_Return(res);
	    res = ec_prune_suspensions(arg + MAX_OFF);
	    Check_Return(res);
	    return ec_prune_suspensions(arg + ANY_OFF);
	}
	else
	{
	    Succeed_;
	}
 
	for( ;arity > 1; arity--)
	{
	    arg = val.ptr++;
	    Dereference_(arg);
	    res = p_prune_woken_goals(arg->val, arg->tag);
	    Check_Return(res);
	}
	arg = val.ptr;		/* tail recursion */
	Dereference_(arg);
	val.all = arg->val.all;
	tag.all = arg->tag.all;
    }
}


static int
p_integer_list_to_dom(value vl, type tl, value vd, type td)
{
    pword	*p;
    pword	*l;
    pword	*s, *t;
    pword	*ints;
    pword	*el;
    word	from, to;
    word	num;
    word	size = 0;

    if (!IsRef(td)) {
	Check_Domain(vd, td)
    }
    if (IsList(tl)) {
	l = vl.ptr;
	el = l++;
	Dereference_(el);
	if (IsInteger(el->tag))
	    from = to = el->val.nint;
	else if (!IsFdInterval(el->val, el->tag)) {
	    Bip_Error(TYPE_ERROR)
	} else {
	    s = el->val.ptr + 1;
	    t = s++;
	    Dereference_(t);
	    Check_Integer(t->tag);
	    Dereference_(s);
	    Check_Integer(s->tag);
	    from = t->val.nint;
	    to = s->val.nint;
	    if (from > to) {
		Bip_Error(RANGE_ERROR)
	    }
	}
    } else {
	Check_List(tl)
    }

    p = ints = Gbl_Tg;
    Gbl_Tg++;
    Check_Gc;

    if (IsNil(tl)) {
	ints->tag.kernel = TNIL;
	l = ints;
    }

    for (;;) {
	Dereference_(l);
	if (IsList(l->tag)) {
	    l = l->val.ptr;
	    el = l++;
	    Dereference_(el);
	    if (IsInteger(el->tag)) {
		num = el->val.nint;
		if (num == to + 1)
		    to = num;
		else if (num > to) {
		    p = insert_interval((long) from, (long) to, p);
		    size += to - from + 1;
		    from = to = num;
		} else {
		    Bip_Error(RANGE_ERROR)
		}
	    }
	    else if (!IsFdInterval(el->val, el->tag)) {
		Bip_Error(TYPE_ERROR)
	    } else {
		s = el->val.ptr + 1;
		t = s++;
		Dereference_(t);
		Check_Integer(t->tag);
		Dereference_(s);
		Check_Integer(s->tag);
		num = t->val.nint;
		if (num == to + 1)
		    to = s->val.nint;
		else if (num > to) {
		    p = insert_interval((long) from, (long) to, p);
		    size += to - from + 1;
		    from = num;
		    to = s->val.nint;
		} else if (num >= from) {/* overlapping ranges */
                    if (to < s->val.nint) to = s->val.nint;
                    else if (s->val.nint < num) {
                       Bip_Error(RANGE_ERROR)
		    }
                } else {
		    Bip_Error(RANGE_ERROR)
		}
		if (num > to) {
		    Bip_Error(RANGE_ERROR)
		}
	    }
	}
	else if (IsNil(l->tag))
	    break;
	else {
	    Check_List(l->tag)
	}
    }
    if (!IsNil(tl)) {
	p = insert_interval((long) from, (long) to, p);
	p->tag.kernel = TNIL;
	size += to - from + 1;
    }

    p = Gbl_Tg;
    Gbl_Tg += 3;
    Check_Gc;
    p[0].tag.kernel = TDICT;
    p[0].val.did = d_dom;
    p[1].tag.kernel = ints->tag.kernel;
    p[1].val.all = ints->val.all;
    p[2].tag.kernel = TINT;
    p[2].val.nint = size;
    Return_Unify_Structure(vd, td, p);
}


/*
 * sdelta(+L1, +L2, ?L1minusL2), used by library(conjunto)
 */

static int
p_sdelta(value l1, type t1, value l2, type t2, value l3, type t3)
{
  pword result_pw;
  pword *result = &result_pw;
  pword *head1, *head2, *p;
  int comp;

  Check_List(t1);
  Check_List(t2);
  Check_Output_List(t3);

  for(;;)
  {
    if (!IsList(t1)) {
      Make_Nil(result);
      break;
    }
    if (!IsList(t2)) {
      result->tag = t1;
      result->val = l1;
      break;
    }

    head1 = l1.ptr; Dereference_(head1);
    head2 = l2.ptr; Dereference_(head2);

    comp = compare_terms(head1->val, head1->tag, head2->val, head2->tag);
    
    if (comp == 0) { /* The element is removed from both lists */
      p = l1.ptr + 1; Dereference_(p); l1 = p->val; t1 = p->tag;
      p = l2.ptr + 1; Dereference_(p); l2 = p->val; t2 = p->tag;
    } else if (comp > 0) { /* The head of the second list is removed */
      p = l2.ptr + 1; Dereference_(p); l2 = p->val; t2 = p->tag;
    } else { /* The head of the first list is moved in the result */
      Make_List(result, TG);
      result = TG;
      Push_List_Frame();
      *result++ = *head1;
      p = l1.ptr + 1; Dereference_(p); l1 = p->val; t1 = p->tag;
    }
  }
  Return_Unify_Pw(l3, t3, result_pw.val, result_pw.tag);
}

