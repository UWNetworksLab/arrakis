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
 * VERSION	$Id: bip_delay.c,v 1.3 2008/08/03 09:57:16 jschimpf Exp $
 */

/****************************************************************************
 *
 *		SEPIA Built-in Predicates for Coroutining.
 *
 *
 *****************************************************************************/

#define BAD_RESTORE_WL		-274
/*
 * INCLUDES:
 */
#include	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "debug.h"
#include        "error.h"
#include        "dict.h"
#include	"emu_export.h"
#include	"property.h"

/*
 * EXTERNAL VARIABLE DEFINITIONS:
 */
pword		*p_meta_arity_;

/*
 * STATIC VARIABLE DEFINITIONS:
 */
static int	p_delayed_goals(value vres, type tres),
		p_last_suspension(value v, type t),
		p_new_delays(value v1, type t1, value vres, type tres),
		p_nonground3(value vn, type tn, value vterm, type tterm, value vlist, type tlst),
		p_meta_bind(value vmeta, type tmeta, value vterm, type tterm),
		p_nonground2(value val, type tag, value vvar, type tvar),
		p_term_variables(value vterm, type tterm, value vlist, type tlst),
		p_replace_attribute(value vmeta, type tmeta, value vterm, type tterm, value vm, type tm),
		p_kill_suspension(value vsusp, type tsusp, value vt, type tt),
		p_setuniv(value v, type t),
		p_suspensions(value vres, type tres),
		p_new_suspensions(value vlast, type tlast, value vres, type tres),
		p_suspension_to_goal(value vsusp, type tsusp, value vgoal, type tgoal, value vmod, type tmod),
		p_suspensions_to_goals(value vSusps, type tSusps, value vGoals, type tGoals, value vLink, type tLink),
		p_current_suspension(value vres, type tres, value vlast, type tlast),
		p_insert_suspension(value vvars, type tvars, value vsusp, type tsusp, value vn, type tn, value vsl, type tsl),
                p_enter_suspension_list(value vn, type tn, value vatt, type tatt, value vsusp, type tsusp),
		p_add_attribute(value vv, type tv, value va, type ta, value vm, type tm),
		p_get_attribute(value vv, type tv, value va, type ta, value vm, type tm),
		p_get_attributes(value vv, type tv, value va, type ta, value vm, type tm, value vmod, type tmod),
		p_get_postponed(value v, type t),
		p_get_postponed_nonempty(value v, type t),
		p_postpone_suspensions(value vpos, type tpos, value vattr, type tattr),
		p_reinit_postponed(value vold, type told),
		p_reset_postponed(value vold, type told),
		p_relax_priority(value vp, type tp, value vwl, type twl),
		p_restore_relaxed_priority(value vwl, type twl),
		p_set_priority(value vp, type tp),
		p_set_priority2(value vp, type tp, value vt, type tt),
		p_get_priority(value vp, type tp),
		p_first_woken(value pv, type pt, value v, type t),
		p_last_scheduled(value vg, type tg),
		p_new_scheduled(value vold, type told, value vl, type tl),
		p_notify_constrained(value v, type t),
		p_init_suspension_list(value vpos, type tpos, value vattr, type tattr),
		p_undo_meta_bind(value vp, type tp, value vv, type tv),
		p_do_meta_bind(value vp, type tp, value vt, type tt),
		p_meta_index(value vname, type tname, value vi, type ti),
		p_get_suspension_data(value vs, type ts, value vwhat, type twhat, value v, type t),
		p_set_suspension_data(value vs, type ts, value vwhat, type twhat, value v, type t),
		p_get_suspension_number(value vs, type ts, value vn, type tn),
		p_set_suspension_number(value vs, type ts, value vn, type tn);

int		p_merge_suspension_lists(value vpos1, type tpos1, value vattr1, type tattr1, value vpos2, type tpos2, value vattr2, type tattr2),
		p_schedule_woken(value vl, type tl),
		p_schedule_suspensions(value vpos, type tpos, value vattr, type tattr),
		p_set_suspension_priority(value vsusp, type tsusp, value vprio, type tprio);

static pword	*_make_goal_list(pword *last, register int undelay);
static int	modify_attribute(value vv, type tv, value va, type ta, value vm, type tm, int replace);


static type	tref;
static dident	d_qualified_goal_,
		d_es_2_,
		d_postponed_;
	
/*
 * LOCAL MACROS
 */

#define Get_Suspension(vsusp, tsusp, susp)	\
    if (IsRef(tsusp))				\
	{ Bip_Error(INSTANTIATION_FAULT); }	\
    if (!IsSusp(tsusp))				\
	{ Bip_Error(TYPE_ERROR); }		\
    (susp) = (vsusp).ptr;


/*
 * FUNCTION DEFINITIONS:
 */
void
bip_delay_init(int flags)
{
    value	v;

    tref.kernel = TREF;
    d_qualified_goal_ = in_dict("qualified_goal", 0);
    d_es_2_ = in_dict("es", 2);
    d_postponed_ = in_dict("postponed", 0);
    if (flags & INIT_SHARED)
    {
	built_in(in_dict("delayed_goals",1),	p_delayed_goals,
		B_UNSAFE|U_GLOBAL) -> mode = BoundArg(1, NONVAR);
	built_in(in_dict("nonground", 3), p_nonground3,	B_UNSAFE|U_GLOBAL)
	    -> mode = BoundArg(2, NONVAR) | BoundArg(3, NONVAR);
	built_in(in_dict("term_variables", 2), p_term_variables,
	    B_UNSAFE|U_GLOBAL) -> mode = BoundArg(2, NONVAR);
	local_built_in(in_dict("meta_bind", 2), p_meta_bind, B_UNSAFE|U_UNIFY)
	    -> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
	local_built_in(in_dict("undo_meta_bind", 2), p_undo_meta_bind, B_UNSAFE|U_GLOBAL) -> mode = BoundArg(2, NONVAR);
	(void) local_built_in(in_dict("do_meta_bind", 2), p_do_meta_bind, B_UNSAFE);
	exported_built_in(in_dict("meta_index", 2), p_meta_index, B_UNSAFE|U_SIMPLE)
	    -> mode = BoundArg(1, CONSTANT) | BoundArg(2, CONSTANT);
	(void) built_in(in_dict("insert_suspension", 4), p_insert_suspension,
		B_UNSAFE);
	(void) built_in(in_dict("enter_suspension_list", 3), p_enter_suspension_list,
		B_UNSAFE);
	built_in(in_dict("set_suspension_data", 3),
		p_set_suspension_data, B_SAFE);
	built_in(in_dict("get_suspension_data", 3),
		p_get_suspension_data, B_UNSAFE|U_UNIFY)
	    -> mode = BoundArg(2, NONVAR);
	(void) exported_built_in(in_dict("set_suspension_number", 2),
		p_set_suspension_number, B_SAFE);
	exported_built_in(in_dict("get_suspension_number", 2),
		p_get_suspension_number, B_UNSAFE|U_SIMPLE)
	    -> mode = BoundArg(2, CONSTANT);
	exported_built_in(in_dict("suspensions_to_goals", 3),
		p_suspensions_to_goals, B_UNSAFE|U_UNIFY)
	    -> mode = BoundArg(2, NONVAR);
	built_in(in_dict("suspension_to_goal", 3), p_suspension_to_goal,
		B_UNSAFE|U_UNIFY)
	    -> mode = BoundArg(2, NONVAR) | BoundArg(3, CONSTANT);
	(void) exported_built_in(in_dict("kill_suspension", 2), p_kill_suspension,
		B_UNSAFE);
	(void) exported_built_in(in_dict("replace_attribute", 3),
		p_replace_attribute,	B_UNSAFE);
	(void) exported_built_in(in_dict("last_suspension", 1),
		p_last_suspension, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("notify_constrained", 1),
		p_notify_constrained, B_UNSAFE);
	b_built_in(in_dict("current_suspension",2),	p_current_suspension,
		d_.kernel_sepia) -> mode = BoundArg(1, NONVAR);
	built_in(in_dict("suspensions",1),	p_suspensions,
		B_UNSAFE|U_GLOBAL) -> mode = BoundArg(1, NONVAR);
	exported_built_in(in_dict("new_suspensions",2),	p_new_suspensions,
		B_UNSAFE|U_GLOBAL) -> mode = BoundArg(2, NONVAR);
	exported_built_in(in_dict("new_delays",2),p_new_delays,
		B_UNSAFE|U_GLOBAL) -> mode = BoundArg(2, NONVAR);
	exported_built_in(in_dict("first_woken", 2), p_first_woken,
		B_UNSAFE|U_GLOBAL) -> mode = BoundArg(2, NONVAR);
	(void) built_in(in_dict("nonground", 2), p_nonground2,
		B_UNSAFE|U_UNIFY);
	(void) built_in(in_dict("schedule_woken", 1), p_schedule_woken,
		B_SAFE);
	(void) built_in(in_dict("init_suspension_list", 2),
		p_init_suspension_list, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("merge_suspension_lists", 4),
		p_merge_suspension_lists, B_SAFE);
	(void) built_in(in_dict("schedule_suspensions", 2),
		p_schedule_suspensions, B_SAFE);
	(void) built_in(in_dict("postpone_suspensions", 2),
		p_postpone_suspensions, B_SAFE);
	(void) built_in(in_dict("set_suspension_priority", 2),
		p_set_suspension_priority, B_SAFE);
	(void) local_built_in(in_dict("get_postponed", 1),
		p_get_postponed, B_UNSAFE|U_GLOBAL);
	(void) local_built_in(in_dict("get_postponed_nonempty", 1),
		p_get_postponed_nonempty, B_UNSAFE|U_GLOBAL);
	(void) local_built_in(in_dict("reinit_postponed", 1),
		p_reinit_postponed, B_UNSAFE|U_GLOBAL);
	(void) local_built_in(in_dict("reset_postponed", 1),
		p_reset_postponed, B_UNSAFE|U_GLOBAL);

	/* these two are used in Grace */
	exported_built_in(in_dict("last_scheduled", 1), p_last_scheduled, B_UNSAFE|U_GLOBAL) -> mode = BoundArg(1, NONVAR);
	exported_built_in(in_dict("new_scheduled", 2), p_new_scheduled, B_UNSAFE|U_GLOBAL) -> mode = BoundArg(2, NONVAR);

	(void) built_in(in_dict("get_priority", 1), p_get_priority, B_UNSAFE);
	(void) exported_built_in(in_dict("set_priority", 1), p_set_priority, B_UNSAFE);
	(void) exported_built_in(in_dict("set_priority", 2), p_set_priority2, B_UNSAFE);
	(void) local_built_in(in_dict("relax_priority", 2), p_relax_priority, B_UNSAFE);
	(void) local_built_in(in_dict("restore_relaxed_priority", 1), p_restore_relaxed_priority, B_UNSAFE);
	(void) exported_built_in(in_dict("add_attribute", 3), p_add_attribute,
		B_UNSAFE);
	exported_built_in(in_dict("get_attribute", 3), p_get_attribute,
		B_UNSAFE|U_GLOBAL) -> mode = BoundArg(2, NONVAR);
	exported_built_in(in_dict("get_attributes", 4), p_get_attributes,
		B_UNSAFE|U_GLOBAL) ->
		mode = BoundArg(2, NONVAR) | BoundArg(4, CONSTANT);
	(void) exported_built_in(in_dict("setuniv", 1), p_setuniv, B_UNSAFE);
    }
	
    /* Global variable meta_arity holds the current number of attribute slots */
    v.nint = 1;
    p_meta_arity_ = init_kernel_var(flags, in_dict("meta_arity", 0), v, tint);
}


/* p_delayed_goals: delayed_goals/1
 * one argument gets bound to the list
 * of delayed goals.
 */

static int
p_delayed_goals(value vres, type tres)
{
    pword	result;

    /* if invoked with [], do a more efficient check only */
    if (IsNil(tres)) {
	pword *env = LD;
	while (env) {
	    if(!SuspDead(env)) {
		Fail_
	    }
	    env = SuspPrevious(env);
	}
	Succeed_;
    }
    if (result.val.ptr = _make_goal_list((pword *) 0, 0))
	result.tag.kernel = TLIST;
    else
	result.tag.kernel = TNIL;
    Return_Unify_Pw(result.val, result.tag, vres, tres);
}

/*
 * last_suspension(-LD) - auxiliary predicate
 * returns the current top of delayed goals list
 */

static int
p_last_suspension(value v, type t)
{  value v2;
   type  t2;
   Check_Ref(t)
   v2.ptr = LD;
   t2.kernel = TSUSP;
   Return_Unify_Pw(v,t,v2,t2);
}


/*
 * new_delays(+Old_LD, -List)
 * return list of delayed goals created since Old_LD was saved
 * the goals are marked as woken!
 */

/*ARGSUSED*/
static int
p_new_delays(value v1, type t1, value vres, type tres)
{
    pword	result, *susp;
    Get_Suspension(v1, t1, susp)
    if (IsNil(tres))	/* just check for delayed goals */
    {
	register pword *env = LD;
	while (env > susp) 
	{
	    if(!SuspDead(env)) 
	    {
		Fail_;
	    }
	    env = SuspPrevious(env);
	}
	Succeed_;
    }
    else if (IsRef(tres) || IsList(tres))
    {
	if (result.val.ptr = _make_goal_list(susp, 1))
	    result.tag.kernel = TLIST;
	else
	    result.tag.kernel = TNIL;
	Return_Unify_Pw(result.val, result.tag, vres, tres);
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }
}


static pword *
_make_goal_list(pword *last, register int undelay)
{
    pword		*env = LD;
    register pword	*pw, *head = (pword *) 0;

    while (env > last) 
    {
	if(!SuspDead(env)) 
	{
	    if (undelay)
	    {
		Set_Susp_Dead(env);
	    }
	    pw = Gbl_Tg;
	    Gbl_Tg += 2;		/* allocate list */
	    Check_Gc
	    *pw = env[SUSP_GOAL];
	    if (head)
	    {
		(pw+1)->val.ptr = head;		/* prepend to list	*/
		(pw+1)->tag.kernel = TLIST;
	    }
	    else				/* first one		*/
		(pw+1)->tag.kernel = TNIL;
	    head = pw;				/* update the list head	*/
	}
	env = SuspPrevious(env);
    }
    return head;
}


/*
 * suspensions(?List)
 * suspensions(+Old, ?List)
 *
 * return the global list of suspensions (possibly starting from Old)
 * leaving out the woken ones.
 */

static int
_suspensions(value vres, type tres, pword *last)
{
    pword	result;
    pword	*env = LD;

    if (IsNil(tres))
    {
	while (env > last)
	{
	    if (!SuspDead(env))
	    {
		Fail_
	    }
	    env = SuspPrevious(env);
	}
	Succeed_;
    }
    else if (!(IsRef(tres) || IsList(tres)))
    {
	Bip_Error(TYPE_ERROR);
    }

    result.tag.kernel = TNIL;
    while (env > last) 
    {
	if (!SuspDead(env))
	{
	    register pword *pw = TG;
	    Push_List_Frame();
	    pw[0].val.ptr = env;
	    pw[0].tag.kernel = TSUSP;
	    pw[1] = result;
	    Make_List(&result, pw);
	}
	env = SuspPrevious(env);
    }
    Return_Unify_Pw(result.val, result.tag, vres, tres);
}

static int
p_suspensions(value vres, type tres)
{
    return _suspensions(vres, tres, (pword *) 0);
}

/*
 * Backtracking external
 * current_suspension(-S, State)
 */
static int
p_current_suspension(value vres, type tres, value vlast, type tlast)
{
    pword *de = IsTag(tlast.kernel, TSUSP) ? SuspPrevious(vlast.ptr) : LD;
    while (de)
    {
	if (!SuspDead(de))
	{
	    value vsusp;
	    type tsusp;
	    vsusp.ptr = de;
	    tsusp.kernel = TSUSP;
	    Remember(2, vsusp, tsusp);
	    Return_Unify_Pw(vres, tres, vsusp, tsusp);
	}
	de = SuspPrevious(de);
    }
    Cut_External;
    Fail_;
}

static int
p_new_suspensions(value vlast, type tlast, value vres, type tres)
{
    pword *susp;
    Get_Suspension(vlast, tlast, susp)
    return _suspensions(vres, tres, susp);
}


/*
 * Bind a metaterm without raising an event
 */
static int
p_meta_bind(value vmeta, type tmeta, value vterm, type tterm)
{
    if (IsMeta(tmeta)) {
	return meta_bind(vmeta.ptr, vterm, tterm);
    }
    else if (IsRef(tmeta)) {
	Bip_Error(INSTANTIATION_FAULT);
    }
    else {
	Bip_Error(TYPE_ERROR);
    }
}



/*
 * Count the structures on the global stack
 */
int
global_stat(void)
{
    pword	*tg = TG_ORIG;
    long	arity;
    long	gsize = 2 * (Gbl_Tg - tg);
    long	size_de = 0;	/* delayed goals */
    long	size_mt = 0;	/* metaterms */
    long	size_hb = 0;	/* heap buffers and strings */
    long	size_st = 0;	/* structures */
    long	size_ls = 0;	/* lists */
    long	size_re = 0;	/* rest */

    while (tg < Gbl_Tg)
    {
	switch (TagType(tg->tag))
	{
	case TDE:
	    size_de += 2 * SUSP_SIZE;
	    tg += SUSP_SIZE;
	    break;

	case TEXTERN:
	    size_hb += 2 * 2;
	    tg += 2;
	    break;

	case TBUFFER:
	    size_hb += 2 * BufferPwords(tg);
	    tg += BufferPwords(tg);
	    break;

	case TDICT:
	    arity = DidArity(tg->val.did);
	    if (arity)
		size_st += 2 * (arity + 1);
	    else
		size_re += 2;
	    tg += arity + 1;
	    break;

	case TMETA:
	    size_mt += 4 + 2 * DidArity(tg[1].val.ptr->val.did);
	    tg += 2;
	    break;

	case TLIST:
	    size_ls += 4;
	    tg++;
	    break;

	default:
	    tg++;
	    size_re += 2;
	}
    }
    p_fprintf(current_err_, "DE = %9d \t%5.1f %%\nMT = %9d \t%5.1f %%\nST = %9d \t%5.1f %%\nLS = %9d \t%5.1f %%\nHB = %9d \t%5.1f %%\nRE = %9d \t%5.1f %%\nTotal = %d\n",
	size_de, (100.0 * size_de)/gsize,
	size_mt, (100.0 * size_mt)/gsize,
	size_st, (100.0 * size_st)/gsize,
	size_ls, (100.0 * size_ls)/gsize,
	size_hb, (100.0 * size_hb)/gsize,
	size_re, (100.0 * size_re)/gsize,
	gsize);
    ec_flush(current_err_);
    Succeed_;
}


static int
p_suspension_to_goal(value vsusp, type tsusp, value vgoal, type tgoal, value vmod, type tmod)
{
    register pword *susp;
    Prepare_Requests;

    Check_Output_Structure(tgoal);
    Check_Output_Atom(tmod);
    Get_Suspension(vsusp, tsusp, susp)
    if (SuspDead(susp))	/* fail for dead suspensions */
	{ Fail_; }

    Request_Unify_Pw(vgoal, tgoal, susp[SUSP_GOAL].val, susp[SUSP_GOAL].tag)
    Request_Unify_Pw(vmod, tmod, susp[SUSP_MODULE].val, susp[SUSP_MODULE].tag)
    Return_Unify
}


/*
 * suspensions_to_goals(+ListOfSusps, -ListOfGoals, -Link)
 * Convert a list of suspensions to the corresponding difference list of goals
 */

static int
p_suspensions_to_goals(value vSusps, type tSusps, value vGoals, type tGoals, value vLink, type tLink)
{
    pword result, *where = &result;
    Prepare_Requests;

    result.tag.kernel = TNIL;
    while(IsList(tSusps))
    {
	pword *susp, *list;
	/* deref missing */
	Get_Suspension((vSusps.ptr)->val, (vSusps.ptr)->tag, susp);
	if (!SuspDead(susp))
	{
	    Make_List(where, TG);
	    where = TG;
	    Push_List_Frame();
	    *where++ = susp[SUSP_GOAL];	/*** CAR ***/
	}
	list = vSusps.ptr + 1;		/*** CDR ***/
	Dereference_(list);
	vSusps = list->val;
	tSusps = list->tag;
    }
    if (IsNil(result.tag)) {		/* no suspensions found */
	where = TG++;
	Check_Gc;
	Make_Ref(&result, where);
    }
    Make_Var(where);
    Request_Unify_Pw(vLink, tLink, where->val, where->tag);
    Request_Unify_Pw(result.val, result.tag, vGoals, tGoals);
    Return_Unify;
}


static int
p_kill_suspension(value vsusp, type tsusp, value vt, type tt)
{
    register pword *susp;

    if (IsRef(tsusp))	/* For convenience when using demons first iteration */
	{ Succeed_; }
    if (!IsSusp(tsusp))
	{ Bip_Error(TYPE_ERROR); }
    susp = vsusp.ptr;
    Check_Integer(tt)
    
    if (!SuspDead(susp))
    {
	/* trail depending on the vt arg; this is necessary to make
	 * some user actions non-backtrackable
	 */
	if (vt.nint) {
	    Set_Susp_Dead(susp);
	} else {
	    Set_Susp_Dead_Untrailed(susp);
	}
    }
    Succeed_;
}


/*
 * insert_suspension(+TermWithVariables, +Suspension, +Position, +Module)
 *
 * Module does not need to be a module, just an attribute slot name.
 */
static int
p_insert_suspension(value vvars, type tvars, value vsusp, type tsusp, value vn, type tn, value vsl, type tsl)
{
    pword	*susp;
    int		slot;
    int		res;

    Get_Suspension(vsusp, tsusp, susp)
    Check_Integer(tn);
    if (vn.nint < 1) {
	Bip_Error(RANGE_ERROR)
    }
    if (IsInteger(tsl)) {
	slot = vsl.nint;
	if (slot <= 0 || slot > p_meta_arity_->val.nint) {
	    Bip_Error(RANGE_ERROR)
	}
    } else if (IsAtom(tsl)) {
	slot = meta_index(vsl.did);
	if (slot == 0) {
	    Bip_Error(UNDEF_ATTR);
	}
    }
    else {
	Bip_Error(TYPE_ERROR)
    }
    res = deep_suspend(vvars, tvars, (int) vn.nint, susp, slot);
    if (res < 0) {
	Bip_Error(res)
    }
    Succeed_;
}


static int
p_nonground2(value val, type tag, value vvar, type tvar)
{
    pword *pw;

    if (pw = ec_nonground(val, tag))
    {
	Return_Unify_Pw(vvar, tvar, pw->val, pw->tag);
    }
    else
    {
	Fail_;
    }
}


/*
 * Build a list of <vars_needed> distinct variables in the term val/tag.
 * The return value is <vars_needed> minus the number of variables found.
 * Already encountered variables are marked by a trailed binding to [],
 * Therefore untrailing is needed after a call to _collect_vars().
 */

static int
_collect_vars(
    	value val, type tag,
	long int vars_needed,		/* > 0 */
	pword *list)
{
    register int arity;
    register pword *arg_i;

    for (;;)
    {
        if (IsRef(tag))
        {
	    register pword *el = TG;
	    TG += 2;
	    Check_Gc;
	    el[0].val.ptr = val.ptr;
	    el[0].tag.kernel = TREF;
	    el[1] = *list;
	    list->val.ptr = el;
	    list->tag.kernel = TLIST;
	    if (IsVar(tag))		/* mark the variable */
		{ Trail_(val.ptr) }
	    else
		{ Trail_Tag(val.ptr) }
	    val.ptr->tag.kernel = TNIL;
	    return vars_needed-1;
        }
        else if (IsList(tag))
            arity = 2;
        else if (IsStructure(tag))
        {
            arity = DidArity(val.ptr->val.did);
            val.ptr++;
        }
        else
            return vars_needed;

        for(;arity > 1; arity--)
        {
            arg_i = val.ptr++;
            Dereference_(arg_i);
	    vars_needed = _collect_vars(arg_i->val, arg_i->tag, vars_needed, list);
	    if (vars_needed == 0)
		return vars_needed;
        }      
        arg_i = val.ptr;                /* tail recursion */
        Dereference_(arg_i);
        val.all = arg_i->val.all;
        tag.all = arg_i->tag.all;
    }
}


static int
p_nonground3(value vn, type tn, value vterm, type tterm, value vlist, type tlst)
{
    pword list;
    pword **old_tt = TT;

    Check_Integer(tn)
    Check_Output_List(tlst)
    if (vn.nint <= 0L)
	{ Bip_Error(RANGE_ERROR); }

    list.tag.kernel = TNIL;
    if (_collect_vars(vterm, tterm, vn.nint, &list) == 0)
    {
	Untrail_Variables(old_tt);
	Return_Unify_List(vlist, tlst, list.val.ptr)
    }
    else	/* not enough variables found */
    {
	Fail_;
    }
}

static int
p_term_variables(value vterm, type tterm, value vlist, type tlst)
{
    pword list;
    pword **old_tt = TT;

    Check_Output_List(tlst)

    list.tag.kernel = TNIL;
    (void) _collect_vars(vterm, tterm, MAX_S_WORD, &list);
    Untrail_Variables(old_tt);
    Return_Unify_Pw(vlist, tlst, list.val, list.tag)
}



/*
 * Change all variables in a term to TUNIVs
 */

static int
_setuniv(value v, type t)
{
    register int   arity, err;

    for(;;)	/* tail recursion loop */
    {
	switch (TagType(t))
	{
	case TVAR_TAG:
	{
	    register pword *pw = v.ptr;
	    Trail_If_Needed(pw);
	    if (pw > Gbl_Tg)		/* if local, globalize first */
	    {
		pw = Gbl_Tg++;
		Check_Gc;
		v.ptr->val.ptr = pw->val.ptr = pw;
	    }
	    pw->tag.kernel = RefTag(TUNIV);
	    Succeed_;
	}
	case TNAME:
	    Trail_Tag_If_Needed_Gb(v.ptr);
	    v.ptr->tag.kernel = TagNameField(t.kernel) | RefTag(TUNIV);
	    Succeed_;
	case TUNIV:
	     /* there may be duplicates in the argument, that is not wrong */
	    Succeed_;

	case TMETA:
	    Succeed_;	/* ? */

	case TNIL:
	case TINT:
	case TDICT:
	case TSTRG:
	case TPTR:
	case TPROC:
	case TSUSP:
	case THANDLE:
	    Succeed_;
	case TLIST:
	    arity = 2;
	    break;
	case TCOMP:
	    arity = DidArity(v.ptr->val.did);
	    v.ptr++;
	    break;
	default:
	    Bip_Error(UNIFY_OVNI);
	}

	for (; arity > 1; arity--)
	{
	    pword *next = v.ptr++;
	    Dereference_(next);
	    if (err = _setuniv(next->val, next->tag))
		Bip_Error(err);
	}
	Dereference_(v.ptr);		/* tail recursion optimised */
	t.all = v.ptr->tag.all;
	v.all = v.ptr->val.all;
    }
}

static int
p_setuniv(value v, type t)
{
   if (IsRef(t))
      return(_setuniv(v, v.ptr->tag));	/* needed due to Puts_named_variable */
   else
      return(_setuniv(v, t));
}

/* Destructively replace the attribute of a metaterm. This allows
 * more efficient trailing than to replace the element of the
 * metaterm structure.
 */
static int
p_replace_attribute(value vmeta, type tmeta, value vterm, type tterm, value vm, type tm)
{
    return modify_attribute(vmeta, tmeta, vterm, tterm, vm, tm, 1);
}

/*
 * Add an attribute to a variable. Unless it is already hard there,
 * we just supply the new data, otherwise the handler is invoked
 * to merge the two attributes.
 */
static int
p_add_attribute(value vv, type tv, value va, type ta, value vm, type tm)
{
    return modify_attribute(vv, tv, va, ta, vm, tm, 0);
}

static int
modify_attribute(value vv, type tv, value va, type ta, value vm, type tm, int replace)
{
    int		slot;
    pword	*var;
    pword	*attr;
    pword	*mt;
    pword	*nva;
    long	nta;

    if (IsInteger(tm))
    {
	slot = vm.nint;
	if (slot <= 0 || slot > p_meta_arity_->val.nint) {
	    return(RANGE_ERROR);
	}
    }
    else if (IsAtom(tm))
    {
	slot = meta_index(vm.did);
	if (slot == 0) {
	    return(UNDEF_ATTR);
	}
    }
    else {
	return(TYPE_ERROR);
    }
    if (IsVar(ta) && va.ptr > TG) {	/* a local variable */
	attr = TG++;
	Check_Gc;
	attr->val.ptr = attr;
	attr->tag.kernel = TREF;
	Bind_(va.ptr, attr->val.ptr, attr->tag.kernel);
	nva = attr->val.ptr;
	nta = attr->tag.kernel;
    } else {
	nva = va.ptr;
	nta = ta.kernel;
    }
    if (IsMeta(tv)) {
	int		i, arity;

	var = MetaTerm(vv.ptr);
	Dereference_(var);
	var = var->val.ptr;
	if ((arity = DidArity(var->val.did)) < slot) {
	/* we must increase the attribute size */

	    mt = add_attribute(tv.kernel, nva, nta, slot);
	    /* copy the other attributes */
	    attr = MetaTerm(mt)->val.ptr;
	    for (i = 1; i <= arity; i++)
		attr[i] = var[i];
	    var = MetaTerm(vv.ptr);
	    if (vv.ptr < GB && !NewLocation(var->val.ptr)) {
		Trail_Pword(var);
	    }
	    var->val.ptr = attr;
	    var->tag.kernel = TCOMP;
	    return PSUCCEED;
	}
	var += slot;
	if (replace) {
	    /* this code is a specialisation of ec_assign() */
	    if (!NewLocation(var) && !NewValue(var->val, var->tag))
	    {
		Trail_Pword(var);
	    }
	    var->tag.kernel = nta;
	    var->val.ptr = nva;
	    return PSUCCEED;
	} else {
	    Dereference_(var);
	    if (IsVar(var->tag) || IsName(var->tag)) {
	    /* insert the attribute into an existing empty slot */
		Return_Unify_Pw(var->val, var->tag, va, ta);
	    } else {
	    /* the slot is not empty, let the handler handle it */
		mt = add_attribute(TREF, nva, nta, slot);
		Return_Unify_Pw(vv, tv, mt->val, mt->tag);
	    }
	}
    } else if (IsVar(tv) || IsName(tv)) {
    /* bind the free variable to a fresh metaterm */
	mt = add_attribute(tv.kernel, nva, nta, slot);
	Return_Unify_Pw(vv, tv, mt->val, tref);
    } else {
	if (replace)
	    return TYPE_ERROR;
    /* a nonvariable, let the handler handle it */
	mt = add_attribute(TREF, nva, nta, slot);
	Return_Unify_Pw(vv, tv, mt->val, mt->tag);
    }
}

static pword *
get_attribute(value vv, type tv, value vm, type tm, int *err)
{
    int		slot;
    pword	*var;

    if (IsInteger(tm))
    {
	slot = vm.nint;
	if (slot <= 0 || slot > p_meta_arity_->val.nint) {
	    *err = RANGE_ERROR;
	    return 0;
	}
    }
    else if (IsAtom(tm))
    {
	slot = meta_index(vm.did);
	if (slot == 0) {
	    *err = UNDEF_ATTR;
	    return 0;
	}
    }
    else {
	*err = TYPE_ERROR;
	return 0;
    }
    if (IsMeta(tv)) {
	var = MetaTerm(vv.ptr);
	Dereference_(var);
	var = var->val.ptr;
	if (DidArity(var->val.did) < slot) {
	    *err = PFAIL;
	    return 0;
	}
	var += slot;
	Dereference_(var);
	return var;
    } else if (IsVar(tv) || IsName(tv)) {
	*err = PFAIL;
	return 0;
    } else {
	*err = TYPE_ERROR;
	return 0;
    }
}

/*
 * Return the given attribute, for completeness only.
 */
static int
p_get_attribute(value vv, type tv, value va, type ta, value vm, type tm)
{
    pword	*var;
    int		err;

    var = get_attribute(vv, tv, vm, tm, &err);
    if (var == 0) {
	if (err == PFAIL) {
	    Fail_;
	} else {
	    Bip_Error(err);
	}
    }
    Return_Unify_Pw(va, ta, var->val, var->tag)
}

/*
 * SICStus-like $get_attributes/3
 */
static int
p_get_attributes(value vv, type tv, value va, type ta, value vm, type tm, value vmod, type tmod)
{
    pword	*var;
    pword	*mask;
    int		err;
    Prepare_Requests;

    var = get_attribute(vv, tv, vmod, tmod, &err);
    if (var == 0) {
	if (err == PFAIL) {
	    Request_Unify_Integer(vm, tm, 0)
	    Return_Unify;
	} else {
	    Bip_Error(err);
	}
    }
    if (IsRef(var->tag)) {
	Request_Unify_Integer(vm, tm, 0)
    } else if (IsStructure(var->tag)) {
	mask = var->val.ptr + 1;
	Dereference_(mask);
	Request_Unify_Pw(va, ta, var->val, var->tag)
	Request_Unify_Integer(vm, tm, mask->val.nint)
    }
    Return_Unify;
}

/*
 *	undo_meta_bind(Pair, AttrVar)
 * Undo the binding before the pre-unification handler is called.
 */
/*ARGSUSED*/
static int
p_undo_meta_bind(value vp, type tp, value vv, type tv)
{
    vp.ptr->tag.kernel = RefTag(TMETA);
    vp.ptr->val.ptr = vp.ptr;
    Return_Unify_Pw(vv, tv, vp, tref);
}

/*
 *	do_meta_bind(Pair, Term)
 * Do the binding after the pre-unification handler is called.
 */
/*ARGSUSED*/
static int
p_do_meta_bind(value vp, type tp, value vt, type tt)
{
    vp.ptr->val.all = vt.all;
    vp.ptr->tag.all = tt.all;
    Succeed_;
}

/*
 *	set_suspension_number(Susp, N)
 * Set the invocation number of a suspension. The debugger uses positive
 * numbers and this predicate uses the negative ones to make the difference.
 */
static int
p_set_suspension_number(value vs, type ts, value vn, type tn)
{
    Check_Type(ts, TSUSP)
    Check_Integer(tn)
    if (vn.nint < 0) {
	Bip_Error(RANGE_ERROR)
    }
    if (ValidInvoc(SuspDebugInvoc(vs.ptr))) {
	Fail_;
    }
    SuspDebugInvoc(vs.ptr) = -vn.nint;
    Succeed_;
}

/*
 *	get_suspension_number(Susp, N)
 * Return the invoc of the suspension, fail if it has a debug invoc.
 */
static int
p_get_suspension_number(value vs, type ts, value vn, type tn)
{
    long	n;

    Check_Type(ts, TSUSP)
    Check_Output_Integer(tn)
    if ((n = SuspDebugInvoc(vs.ptr)) > 0) {
	Fail_;
    }
    Return_Unify_Integer(vn, tn, -n)
}

static int
p_get_suspension_data(value vs, type ts, value vwhat, type twhat, value v, type t)
{
    Check_Output_Type(ts, TSUSP)
    Check_Atom(twhat);
    if (IsRef(ts))
	{ Fail_; }
    if (vwhat.did == d_.state)
    {
	long n = SuspDead(vs.ptr) ? 2 : SuspScheduled(vs.ptr) ? 1 : 0;
	Return_Unify_Integer(v, t, n);
    }
    if (SuspDead(vs.ptr))
	{ Fail_; }
    if (vwhat.did == d_.priority)
    {
	Return_Unify_Integer(v, t, SuspPrio(vs.ptr))
    }
    else if (vwhat.did == d_.invoc)
    {
	Return_Unify_Integer(v, t, SuspDebugInvoc(vs.ptr))
    }
    else if (vwhat.did == d_.goal)
    {
	Return_Unify_Pw(v, t, vs.ptr[SUSP_GOAL].val, vs.ptr[SUSP_GOAL].tag);
    }
    else if (vwhat.did == d_.module0)
    {
	Return_Unify_Pw(v, t, vs.ptr[SUSP_MODULE].val, vs.ptr[SUSP_MODULE].tag);
    }
    else if (vwhat.did == d_.spy)
    {
	Return_Unify_Atom(v, t, PriFlags(SuspProc(vs.ptr)) & DEBUG_SP ? d_.on : d_.off);
    }
    else if (vwhat.did == d_.skip)
    {
	Return_Unify_Atom(v, t, PriFlags(SuspProc(vs.ptr)) & DEBUG_SK ? d_.on : d_.off);
    }
    else if (vwhat.did == d_qualified_goal_)
    {
	pword *pw = TG;
	Push_Struct_Frame(d_.colon);
	Make_Atom(&pw[1], PriModule(SuspProc(vs.ptr)));
	pw[2] = vs.ptr[SUSP_GOAL];
	Return_Unify_Structure(v, t, pw);
    }
    Bip_Error(RANGE_ERROR);
}

static int
p_set_suspension_data(value vs, type ts, value vwhat, type twhat, value v, type t)
{
    Check_Output_Type(ts, TSUSP)
    Check_Atom(twhat);
    Check_Integer(t);
    if (IsRef(ts) || SuspDead(vs.ptr))	/* ignore if dead/nonexistent */
	{ Succeed_; }
    if (vwhat.did == d_.priority)
    {
	if (SuspPrio(vs.ptr) != v.nint)
	{
	    if (v.nint < 1 ||  v.nint > SUSP_MAX_PRIO)
		{ Bip_Error(RANGE_ERROR); }
	    Set_Susp_Prio(vs.ptr, v.nint);
	}
    }
    else if (vwhat.did == d_.invoc)
    {
	SuspDebugInvoc(vs.ptr) = v.nint;
    }
    else { Bip_Error(RANGE_ERROR); }
    Succeed_;
}


/*
 * Distribute the suspensions in the list to the global woken lists
 */
int
p_schedule_woken(value vl, type tl)
{
    register pword	*p, *next;

    if (IsStructure(tl) && vl.ptr->val.did == d_.minus) {
	next = vl.ptr + 1;
	Dereference_(next);
	if (IsList(next->tag))
	    next = next->val.ptr;
	else if (IsRef(next->tag)) {
	    Succeed_
	} else {
	    Bip_Error(TYPE_ERROR)
	}
    } else if (IsList(tl))
	next = vl.ptr;
    else if (IsNil(tl) || IsRef(tl)) {
	Succeed_
    } else {
	Bip_Error(TYPE_ERROR)
    }

    /* simplified version of ec_schedule_susps without 
     * list cleanup (since the list is not needed anymore).
     */
    for (;;)
    {
	p = next++;
	Dereference_(p);
	if (!IsTag(p->tag.kernel, TSUSP)) {
	    Bip_Error(TYPE_ERROR)
	}
	p = p->val.ptr;

	if (!SuspDead(p) && !SuspScheduled(p))
	{
	    /* schedule this suspension */
	    pword *q = WLFirst(WL) + SuspPrio(p) - 1;
	    pword *new = TG;
	    Push_List_Frame()
	    new[0].val.ptr = p;
	    new[0].tag.kernel = TSUSP;
	    new[1] = *q;
	    if (IsNil(q->tag) || q->val.ptr < GB) {
		Trail_Pword(q)
	    }
	    Make_List(q, new);
	    Set_Susp_Scheduled(p);
	}
	Dereference_(next);
	if (!IsList(next->tag)) {
	    Succeed_
	}
	next = next->val.ptr;
    }
}


/*
 * get_postponed(-EventStruct)
 *	return the postponed goals structure es(postponed, Susps)
 *
 * get_postponed_nonempty(-EventStruct)
 *	return the postponed goals structure es(postponed, Susps)
 *	if Susps is not empty, and reinitialise to es(postponed, []).
 *	If Susps is empty, fail.
 *
 * reinit_postponed(-OldSusps)
 *	return the postponed suspension list and reinitialise.
 *
 * reset_postponed(+OldSusps)
 *	reset the postponed suspension list to the given old value.
 */
int
ec_init_postponed(void)
{
    pword *pw = TG;
    Push_Struct_Frame(d_es_2_);
    Make_Atom(pw+1, d_postponed_);
    Make_Nil(pw+2);
    Make_Struct(&PostponedList, pw);
    Succeed_;
}

static int
p_get_postponed(value v, type t)
{
    Bind_(v.ptr, PostponedList.val.ptr, PostponedList.tag.kernel);
    Succeed_;
}

static int
p_get_postponed_nonempty(value v, type t)
{
    int result;
    pword new_struct;

    pword *pw = &PostponedList.val.ptr[2];	/* fail if list empty */
    Dereference_(pw);
    if (IsNil(pw->tag))
	{ Fail_; }
    						/* return nonempty one */
    Bind_(v.ptr, PostponedList.val.ptr, PostponedList.tag.kernel);

    pw = TG;					/* reinitialise */
    Push_Struct_Frame(d_es_2_);
    Make_Atom(pw+1, d_postponed_);
    /*Make_Nil(pw+2);*/
    Make_Stamp(pw+2);				/* a timestamped [] */
    Make_Struct(&new_struct, pw);
    return ec_assign(&PostponedList, new_struct.val, new_struct.tag);
}

static int
p_reinit_postponed(value vold, type told)
{
    pword *pw = &PostponedList.val.ptr[2];	/* return old suspension list */
    Bind_(vold.ptr, pw->val.ptr, pw->tag.kernel);
    Dereference_(pw);
    if (!IsNil(pw->tag))			/* reinitialise */
    {
	pword empty;
	Make_Stamp(&empty);			/* a timestamped [] */
	ec_assign(pw, empty.val, empty.tag);
    }
    Succeed_;
}

static int
p_reset_postponed(value vold, type told)
{
    /* we expect that the postponed list is already empty at this point */
#ifdef PRINTAM
    pword *pw = &PostponedList.val.ptr[2];
    Dereference_(pw);
    if (!IsNil(pw->tag))
    {
	p_fprintf(current_err_, "ECLiPSe kernel warning: postponed list not empty in reset_postponed/1");
	ec_flush(current_err_);
    }
#endif
    if (!IsNil(told))				/* reset if necessary */
    {
	return ec_assign(&PostponedList.val.ptr[2], vold, told);
    }
    Succeed_;
}


/*
 * postpone_suspensions(+Pos, +Attr)
 * Put a whole suspension list into the global postponed-list
 */

int
p_postpone_suspensions(value vpos, type tpos, value vattr, type tattr)
{
    Check_Integer(tpos);
    Check_Structure(tattr);
    if (vpos.nint < 1 || vpos.nint > DidArity(vattr.ptr->val.did))
    {
	Bip_Error(RANGE_ERROR);
    }
    return p_schedule_postponed(vattr.ptr[vpos.nint].val, vattr.ptr[vpos.nint].tag);
}


int
p_schedule_postponed(value vl, type tl)
{
    pword	*p, *next, *ppp;
    pword	newpp;
    int		change = 0;

    if (IsStructure(tl) && vl.ptr->val.did == d_.minus) {
	next = vl.ptr + 1;
	Dereference_(next);
	if (IsList(next->tag))
	    next = next->val.ptr;
	else if (IsRef(next->tag)) {
	    Succeed_
	} else {
	    Bip_Error(TYPE_ERROR)
	}
    } else if (IsList(tl))
	next = vl.ptr;
    else if (IsNil(tl) || IsRef(tl)) {
	Succeed_
    } else {
	Bip_Error(TYPE_ERROR)
    }

    /* Partial garbage collection: remove dead stuff at the
     * beginning of the postponed-list
     */
    ppp = &PostponedList.val.ptr[2];
    Dereference_(ppp);
    newpp = *ppp;
    while (IsList(ppp->tag))
    {
	ppp = ppp->val.ptr;
	p = ppp++;
	Dereference_(p);
	if (!IsTag(p->tag.kernel, TSUSP)) {
	    Bip_Error(TYPE_ERROR)
	}
	p = p->val.ptr;
	/* This if peculiar to the postponed-list: we can remove scheduled
	 * suspensions (even if demons) because the list will never be
	 * woken twice (it is scrapped after having been woken).
	 */
	if (!SuspDead(p) && !SuspScheduled(p))
	    break;
	Dereference_(ppp);
	newpp = *ppp;
	change = 1;
    }

    /* Move live suspensions to the postponed-list.
     * No input list cleanup (since the list is not needed anymore).
     */
    for (;;)
    {
	p = next++;
	Dereference_(p);
	if (!IsTag(p->tag.kernel, TSUSP)) {
	    Bip_Error(TYPE_ERROR)
	}
	p = p->val.ptr;

	/* This if peculiar to the postponed-list: no need to move an
	 * already scheduled suspension there, because the rationale
	 * of the postponed list is only to guarantee (one) future waking.
	 */
	if (!SuspDead(p) && !SuspScheduled(p))
	{
	    pword *new = TG;
	    Push_List_Frame()
	    new[0].val.ptr = p;
	    new[0].tag.kernel = TSUSP;
	    new[1] = newpp;
	    Make_List(&newpp, new);
	    change = 1;
	}
	Dereference_(next);
	if (!IsList(next->tag)) {
	    break;
	}
	next = next->val.ptr;
    }

    if (change)
    	ec_assign(&PostponedList.val.ptr[2], newpp.val, newpp.tag);
    Succeed_
}


/*
 * Demon-aware suspension lists:
 *
 * init_suspension_list(+Pos, +Attr)
 * enter_suspension_list(+Pos, +Attr, +Susp)
 * merge_suspension_lists(+Pos1, +Attr1, +Pos2, +Attr2)
 * schedule_suspensions(+Pos, +Attr)
 *
 * If these lists were guaranteed to only ever get manipulated by
 * special procedures, we could get rid of all the dereferencing.
 */

#define SUSP_LIST_CLEANUP

static
int
p_init_suspension_list(value vpos, type tpos, value vattr, type tattr)
{
    pword	*arg;
    Check_Integer(tpos);
    Check_Structure(tattr);
    if (vpos.nint < 1 || vpos.nint > DidArity(vattr.ptr->val.did))
    {
	Bip_Error(RANGE_ERROR);
    }
    arg = &vattr.ptr[vpos.nint];
    Dereference_(arg);
    Check_Ref(arg->tag);
    Return_Bind_Var(arg->val, arg->tag, 0, TNIL);
}

/*
 * enter_suspension_list(+Positiion, +Attribute, +Suspension)
 */
static int
p_enter_suspension_list(value vn, type tn, value vatt, type tatt, value vsusp, type tsusp)
{
    pword	*susp, *att;
    int		res;

    Check_Integer(tn);
    Check_Structure(tatt);

    Get_Suspension(vsusp, tsusp, susp)
    att = vatt.ptr;
    if ((int) vn.nint <= 0 || DidArity(att->val.did) < (int) vn.nint) {
	Bip_Error(RANGE_ERROR);
    }
    res = ec_enter_suspension(att + (int) vn.nint, susp);
    if (res < 0) {
	Bip_Error(res);
    }
    Succeed_;
}


/*
 * merge_suspension_lists(+Pos1, +Attr1, +Pos2, +Attr2)
 *
 * Destructively append list1 (argument Pos1 of Attr1) to
 * the end of list2 (argument Pos2 of Attr2).
 * Currently neither cleanup nor duplicate removal.
 */
int
p_merge_suspension_lists(value vpos1, type tpos1, value vattr1, type tattr1, value vpos2, type tpos2, value vattr2, type tattr2)
{
    pword	*list1, *list2;
    pword	*last;
    Check_Integer(tpos1);
    Check_Integer(tpos2);
    Check_Structure(tattr1);
    Check_Structure(tattr2);
    if (vpos1.nint < 1 || vpos1.nint > DidArity(vattr1.ptr->val.did)
     || vpos2.nint < 1 || vpos2.nint > DidArity(vattr2.ptr->val.did))
    {
	Bip_Error(RANGE_ERROR);
    }
    last = list2 = &vattr2.ptr[vpos2.nint];
    Dereference_(list2);
    if (IsList(list2->tag))		/* find the end of list2 */
    {
	list2 = list2->val.ptr;
	for (;;)
	{
	    last = ++list2;
	    Dereference_(list2);
	    if (!IsList(list2->tag))
		break;
	    list2 = list2->val.ptr;
	}
    }
    if (!IsNil(list2->tag))
    {
	Bip_Error(TYPE_ERROR)
    }
    /* last now points to the end of list2 */

    list1 = &vattr1.ptr[vpos1.nint];		/* append list1 */
    Dereference_(list1);
    if (IsList(list1->tag))
    {
	list1 = list1->val.ptr;
	if (last < GB) {
	    Trail_Pword(last)		/* trail the [] */
	}
	Make_List(last, list1);
    }
    else if (!IsNil(list1->tag))
    {
	Bip_Error(TYPE_ERROR)
    }
    Succeed_;
}


/*
 * ec_schedule_susp(+Susp)
 *
 * Schedule a suspension for waking.  Susp should be the val pointer from
 * the TSUSP cell, not a pointer to the TSUSP cell.
 */

int
ec_schedule_susp(pword *susp)
{
    if (!SuspDead(susp) && !SuspScheduled(susp))
    {
	/* schedule this suspension */
	pword *q = WLFirst(WL) + SuspPrio(susp) - 1;
	pword *new = TG;
	Push_List_Frame()
	new[0].val.ptr = susp;
	new[0].tag.kernel = TSUSP;
	new[1] = *q;
	if (IsNil(q->tag) || q->val.ptr < GB) {
	    Trail_Pword(q)
	}
	Make_List(q, new);
	Set_Susp_Scheduled(susp);
    }

    Succeed_
}


/*
 * schedule_suspensions(+Pos, +Attr)
 *
 * Schedule a suspension list (argument Pos of Attr) for waking.
 * All so far unscheduled suspensions are put into the woken lists
 * according to their priority. The input list is cleaned up,
 * only live demons remain in it.
 */

int
ec_schedule_susps(pword *next)
{
    pword	*last_live, *p;
    int		found_dead = 0;

    last_live = next;
    Dereference_(next);
    if (IsList(next->tag)) {
	next = next->val.ptr;
    } else if (IsNil(next->tag) || IsRef(next->tag)) {
	Succeed_
    } else {
	Bip_Error(TYPE_ERROR)
    }

    for (;;)
    {
	p = next;			/* get the suspension */
	Dereference_(p);
	if (!IsTag(p->tag.kernel, TSUSP)) {
	    Bip_Error(TYPE_ERROR)
	}
	p = p->val.ptr;

	if (!SuspDead(p) && !SuspScheduled(p))
	{
	    /* schedule this suspension */
	    pword *q = WLFirst(WL) + SuspPrio(p) - 1;
	    pword *new = TG;
	    Push_List_Frame()
	    new[0].val.ptr = p;
	    new[0].tag.kernel = TSUSP;
	    new[1] = *q;
	    if (IsNil(q->tag) || q->val.ptr < GB) {
		Trail_Pword(q)
	    }
	    Make_List(q, new);
	    Set_Susp_Scheduled(p);
	}

#ifdef SUSP_LIST_CLEANUP
	if (SuspDead(p) || !SuspDemon(p))
	{
	    found_dead = 1;		/* it can be removed */
	    ++next;
	}
	else
	{
	    if (found_dead)		/* unlink garbage */
	    {
		if (last_live < GB && last_live->val.ptr < GB) {
		    Trail_Pword(last_live)
		}
		if (next < GB)		/* To reduce future trailing ... */
		{
		    pword *new = TG;	/* use fresh copy of the list cell */
		    Push_List_Frame();
		    new[0] = next[0];
		    new[1] = next[1];
		    next = new;
		}
		Make_List(last_live, next);
		found_dead = 0;
	    }
	    last_live = ++next;		/* proceed to next one */
	}
#else
	++next;
#endif

	Dereference_(next);
	if (!IsList(next->tag))
	    break;
	next = next->val.ptr;
    }

#ifdef SUSP_LIST_CLEANUP
    if (found_dead)			/* unlink tail garbage */
    {
	if (last_live < GB && last_live->val.ptr < GB) {
	    Trail_Pword(last_live)
	}
	Make_Stamp(last_live);	/* a timestamped [] */
    }
#endif
    Succeed_
}


/*
 * This is basically a subset of ec_schedule_susps:
 * It does not schedule, but only cleans up the list.
 */
int
ec_prune_suspensions(pword *next)
{
    pword	*last_live, *p;
    int		found_dead = 0;

    last_live = next;
    Dereference_(next);
    if (IsList(next->tag)) {
	next = next->val.ptr;
    } else if (IsNil(next->tag) || IsRef(next->tag)) {
	Succeed_
    } else {
	Bip_Error(TYPE_ERROR)
    }

    for (;;)
    {
	p = next;			/* get the suspension */
	Dereference_(p);
	if (!IsTag(p->tag.kernel, TSUSP)) {
	    Bip_Error(TYPE_ERROR)
	}
	p = p->val.ptr;

	/* This is the important condition: */
	if (SuspDead(p) || (!SuspDemon(p) && SuspScheduled(p)))
	{
	    found_dead = 1;		/* it can be removed */
	    ++next;
	}
	else
	{
	    if (found_dead)		/* unlink garbage */
	    {
		if (last_live < GB && last_live->val.ptr < GB) {
		    Trail_Pword(last_live)
		}
		if (next < GB)		/* To reduce future trailing ... */
		{
		    pword *new = TG;	/* use fresh copy of the list cell */
		    Push_List_Frame();
		    new[0] = next[0];
		    new[1] = next[1];
		    next = new;
		}
		Make_List(last_live, next);
		found_dead = 0;
	    }
	    last_live = ++next;		/* proceed to next one */
	}

	Dereference_(next);
	if (!IsList(next->tag))
	    break;
	next = next->val.ptr;
    }

    if (found_dead)			/* unlink tail garbage */
    {
	if (last_live < GB && last_live->val.ptr < GB) {
	    Trail_Pword(last_live)
	}
	Make_Stamp(last_live);	/* a timestamped [] */
    }
    Succeed_
}


int
p_schedule_suspensions(value vpos, type tpos, value vattr, type tattr)
{
    Check_Integer(tpos);
    Check_Structure(tattr);
    if (vpos.nint < 1 || vpos.nint > DidArity(vattr.ptr->val.did))
    {
	Bip_Error(RANGE_ERROR);
    }
    return ec_schedule_susps(&vattr.ptr[vpos.nint]);
}


/*
 * set_suspension_priority(+Susp, +Prio)
 *
 * Change a suspension's priority. This only has an effect as long
 * as the suspension has not been scheduled for waking.
 */
int
p_set_suspension_priority(value vsusp, type tsusp, value vprio, type tprio)
{
    Check_Integer(tprio)
    Check_Type(tsusp, TSUSP)
    if (SuspDead(vsusp.ptr))
    {
	Bip_Error(TYPE_ERROR);
    }
    if (SuspPrio(vsusp.ptr) != (unsigned) vprio.nint)
    {
	Set_Susp_Prio(vsusp.ptr, vprio.nint);
    }
    Succeed_;
}


static int
p_get_priority(value vp, type tp)
{
    Check_Output_Integer(tp)
    Return_Unify_Integer(vp, tp, WP)
}

static int
p_set_priority(value vp, type tp)
{
    int prio;
    Check_Integer(tp)
    prio = vp.nint > SUSP_MAX_PRIO ? SUSP_MAX_PRIO : vp.nint;
    Set_WP(prio)
    Succeed_
}

static int
p_set_priority2(value vp, type tp, value vt, type tt)
{
    int prio;
    Check_Integer(tp)
    Check_Integer(tt)
    prio = vp.nint > SUSP_MAX_PRIO ? SUSP_MAX_PRIO : vp.nint;
    if (vt.nint) {
	Set_WP(prio)
    } else
	WP = prio;
    Succeed_
}


p_relax_priority(value vp, type tp, value vwl, type twl)
{
    int prio;
    Check_Integer(tp)
    prio = vp.nint > SUSP_MAX_PRIO ? SUSP_MAX_PRIO : vp.nint;
    if (WP >= prio) {
	Return_Unify_Nil(vwl, twl)	/* nothing to do */
    }

    if (WL < GB) {
	Trail_Pword(&TAGGED_WL)
    }
    wl_init(SUSP_MAX_PRIO);		/* saves old WP and WL, sets new WL */
    Set_WP(prio)
    Return_Unify_Pw(vwl, twl, TAGGED_WL.val, TAGGED_WL.tag)
}

p_restore_relaxed_priority(value vwl, type twl)
{
    if (IsStructure(twl)) {
	int i;
	pword *p = WLFirst(vwl.ptr);
	for (i=0; i<SUSP_MAX_PRIO; ++i) {
	    pword *plist = &p[i];
	    Dereference_(plist);
	    if (!IsNil(plist->tag)) {
		Bip_Error(BAD_RESTORE_WL);
	    }
	}
	Set_WP(WLPreviousWP(WL)->val.nint);
	if (WL < GB) {
	    Trail_Pword(&TAGGED_WL)
	}
	TAGGED_WL = *WLPrevious(WL);
    }
    Succeed_;
}


static
p_first_woken(value pv, type pt, value v, type t)
{
    pword	*p;

    Check_Integer(pt);
    if (pv.nint < 1 || pv.nint > SUSP_MAX_PRIO) {
	Bip_Error(RANGE_ERROR)
    }
    p = first_woken((int) pv.nint);
    if (!p) {
	Fail_;
    } else {
	Return_Unify_Pw(p->val, p->tag, v, t)
    }
}

/*
 * Similar to last_suspension/1 - returns a structure with the
 * current state of the waking scheduler
 */
static int
p_last_scheduled(value vg, type tg)
{
    register pword	*p = TG;
    int			i;

    i = DidArity(WL->val.did);
    TG += i + 1;		/* + functor */
    Check_Gc
    p->val.did = WL->val.did;
    p->tag.all = TDICT;
    for (; i > 0; i--) {
	p[i].val.all = WL[i].val.all;
	p[i].tag.kernel = WL[i].tag.kernel;
    }
#if 0
    WLPrevious(p)->tag.all = TGCONST;
#else
    WLPrevious(p)->tag.all = TNIL;
#endif
    Return_Unify_Structure(vg, tg, p)
}

/*
 *  last_scheduled(+OldWL, -NewWoken)
 * Similar to new_delays/2 - returns a list of suspensions
 * that have been woken (scheduled) since the OldWL.
 */
static int
p_new_scheduled(value vold, type told, value vl, type tl)
{
    register pword	*o;
    register pword	*n;
    register pword	*s;
    register pword	*u;
    pword		*old;
    pword		*new;
    pword		*list;
    pword		*l;
    pword		*save_l;
    pword		*save_tg;
    int			i;
    long		max;

    Check_Structure(told);
#if 0
    if (WLPrevious(WL)->val.ptr != WLPrevious(vold.ptr)->val.ptr) {
	Fail_;		/* not the same nesting level */
    }
#endif
    max = WLMaxPrio(WL);
    old = WLFirst(vold.ptr);
    new = WLFirst(WL);
    l = list = TG++;
    Check_Gc;
    for (i = 0; i < max; i++) {
	n = new++;
	o = old++;		/* no references allowed */
	if (IsList(n->tag) && (!IsList(o->tag) ||
				n->val.ptr != o->val.ptr)) {
	    while (IsList(o->tag)) {
		o = o->val.ptr;
		s = o;
		Dereference_(s);
		if (!SuspDead(s->val.ptr))
		    break;
		o++;
		Dereference_(o);
	    }
	    save_tg = TG;
	    save_l = l;
	    for (;;) {
		n = n->val.ptr;
		s = n++;
		Dereference_(s);
		Dereference_(n);
		if (IsSusp(s->tag)) {
		    u = s->val.ptr;
		    if (!SuspDead(u)) {
			l->val.ptr = TG;
			l->tag.all = TLIST;
			l = TG;
			TG += 2;
			Check_Gc;
			*l++ = *s;
		    }
		} 
		if (IsNil(n->tag)) {
		    /* we are at the end of new and we didn't find old */
		    if (!IsNil(o->tag)) {
			/* an old one is missing from the new one; this means
			 * that it was just woken and there is nothing new */
			 TG = save_tg;
			 l = save_l;
		    }
		    break;
		} else if (n->val.ptr == o) {
		    break;
		}
	    }
	}
    }
    l->tag.all = TNIL;
    Return_Unify_Pw(vl, tl, list->val, list->tag)
}

static int
p_meta_index(value vname, type tname, value vi, type ti)
{
    if (IsInteger(ti))
    {
	dident name = meta_name(vi.nint);
	if (name == D_UNKNOWN) { Fail_; }
	Return_Unify_Atom(vname, tname, name);
    }
    if (IsAtom(tname))
    {
	int i = meta_index(vname.did);
	if (i == 0) { Fail_; }
	Return_Unify_Integer(vi, ti, i);
    }
    Bip_Error(TYPE_ERROR);
}


static int
p_notify_constrained(value v, type t)
{
    if (!IsMeta(t)) {
	Succeed_
    }
    return notify_constrained(v.ptr);
}

