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
 * VERSION	$Id: bip_control.c,v 1.3 2008/09/01 11:44:54 jschimpf Exp $
 */

/****************************************************************************
 *
 *		SEPIA Built-in Predicates: Logic, control, events, debugging
 *
 *
 *****************************************************************************/



#include	"config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include        "io.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"debug.h"
#include	"module.h"
#include        "property.h"


int 		p_exit(value v, type t);

static int	p_dbgcomp(void),
		p_nodbgcomp(void),
		p_timestamp_init(value vstruct, type tstruct, value varg, type targ),
		p_timestamp_update(value vstruct, type tstruct, value varg, type targ),
		p_timestamp_older(value vstruct1, type tstruct1, value varg1, type targ1, value vstruct2, type tstruct2, value varg2, type targ2),
		p_timestamp_age(value vstruct, type tstruct, value varg, type targ, value vstate, type tstate),
		p_request_fail_event(value v, type t, value varg, type targ, value vevent, type tevent),
		p_request_fail_event4(value v, type t, value varg, type targ, value vevent, type tevent, value vitem, type titem),
		p_request_fail_write(value v, type t, value varg, type targ, value vstream, type tstream, value vterm, type tterm),
		p_failure_culprit(value vf, type tf, value vi, type ti),
		p_new_invoc(value v, type t),
		p_disable_tracing(void),
		p_events_nodefer(void),
		p_events_defer(void),
		p_raise_init_event(void),
		p_current_td(value v, type t),
		p_of_interest(value vport, type tport, value vinvoc, type tinvoc, value vdepth, type tdepth, value vproc, type tproc, value vbrkpt, type tbrkpt),
		p_trace_mode(value v, type t, value vmode, type tmode),
		p_tracing(void),
		p_delay_port_susps(value v, type t),
		p_get_fail_info(value vi, type ti, value vf, type tf),
		p_susp_to_tf(value vs, type ts, value vf, type tf),
		p_make_tf(value vpush, type tpush, value vi, type ti, value vg, type tg, value vm, type tm, value vlm, type tlm, value vp, type tp, value vf, type tf),
		p_pop_tf(void),
		p_get_tf_prop(value vf, type tf, value vwhat, type twhat, value v, type t),
#ifdef PRINTAM
		p_systrace(void),
#endif
		p_spied(value v1, type t1, value v2, type t2, value vm, type tm),
		p_global_flags(value vc, type tc, value vs, type ts, value v, type t),
		p_vm_flags(value vc, type tc, value vs, type ts, value v, type t),
		p_sys_flags(value vf, type tf, value vval, type tval),
		p_extension(value vext, type text, value v, type t),
		p_prof(value v, type t, value vf, type tf, value vs, type ts);

stream_id	profile_stream_;
int		profile_flags_;

static dident
	d_chip_,
	d_current_,
	d_objects_,
	d_development_,
	d_seduce_,
	d_opium_,
	d_occur_check_,
	d_old_,
	d_dfid_,
	d_megalog_,
	d_parallel_,
	d_mps_;


void
bip_control_init(int flags)
{
    d_chip_ = in_dict("chip",0);
    d_objects_ = in_dict("objects",0);
    d_development_ = in_dict("development",0);
    d_seduce_ = in_dict("seduce",0);
    d_opium_ = in_dict("opium",0);
    d_occur_check_ = in_dict("occur_check", 0);
    d_dfid_ = in_dict("dfid", 0);
    d_megalog_ = in_dict("megalog",0);
    d_parallel_ = in_dict("parallel",0);
    d_mps_ = in_dict("mps",0);
    d_current_ = in_dict("current",0);
    d_old_ = in_dict("old",0);

    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("dbgcomp", 0),		p_dbgcomp,	B_SAFE);
	(void) built_in(in_dict("nodbgcomp", 0),	p_nodbgcomp,	B_SAFE);
#ifdef PRINTAM
	(void) built_in(in_dict("systrace", 0),	p_systrace,	B_SAFE);
#endif
	(void) local_built_in(in_dict("exit0", 1),	p_exit,		B_SAFE);
	(void) local_built_in(in_dict("spied_", 3),	p_spied,	B_SAFE);
	(void) local_built_in(in_dict("global_flags", 3), p_global_flags, B_SAFE|U_SIMPLE);
	(void) local_built_in(in_dict("vm_flags", 3), p_vm_flags, B_SAFE|U_SIMPLE);
	(void) local_built_in(in_dict("sys_flags", 2), p_sys_flags, B_SAFE|U_SIMPLE);
	(void) b_built_in(in_dict("extension", 2), p_extension,
			  d_.kernel_sepia);
	(void) exported_built_in(in_dict("prof", 3), p_prof, B_SAFE);

	(void) built_in(in_dict("timestamp_init", 2), p_timestamp_init, B_UNSAFE);
	(void) built_in(in_dict("timestamp_update", 2), p_timestamp_update, B_UNSAFE);
	(void) built_in(in_dict("timestamp_older", 4), p_timestamp_older, B_UNSAFE);
	(void) built_in(in_dict("timestamp_age", 3), p_timestamp_age, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("request_fail_event", 3), p_request_fail_event, B_SAFE);
	(void) built_in(in_dict("request_fail_event", 4), p_request_fail_event4, B_SAFE);
	(void) built_in(in_dict("request_fail_write", 4), p_request_fail_write, B_SAFE);
	(void) built_in(in_dict("events_nodefer", 0), p_events_nodefer, B_SAFE);
	(void) built_in(in_dict("events_defer", 0), p_events_defer, B_SAFE);

	(void) local_built_in(in_dict("failure_culprit", 2), p_failure_culprit, B_UNSAFE);
	(void) local_built_in(in_dict("new_invoc", 1), p_new_invoc, B_UNSAFE);
	(void) local_built_in(in_dict("tracing", 0), p_tracing, B_SAFE);
	(void) local_built_in(in_dict("disable_tracing", 0), p_disable_tracing, B_SAFE);
	(void) local_built_in(in_dict("raise_init_event", 0), p_raise_init_event, B_SAFE);
	(void) local_built_in(in_dict("current_td", 1), p_current_td, B_UNSAFE);
	(void) local_built_in(in_dict("trace_mode", 2), p_trace_mode, B_SAFE);
	(void) local_built_in(in_dict("of_interest", 5), p_of_interest, B_SAFE);
	(void) local_built_in(in_dict("get_fail_info", 2), p_get_fail_info, B_UNSAFE);
	(void) local_built_in(in_dict("susp_to_tf", 2), p_susp_to_tf, B_UNSAFE);
	(void) local_built_in(in_dict("make_tf", 7), p_make_tf, B_UNSAFE);
	(void) local_built_in(in_dict("pop_tf", 0), p_pop_tf, B_UNSAFE);
	(void) local_built_in(in_dict("get_tf_prop", 3), p_get_tf_prop, B_UNSAFE);
	(void) local_built_in(in_dict("delay_port_susps", 1), p_delay_port_susps, B_UNSAFE);
    }
}


#ifdef PRINTAM
static int
p_systrace(void)
{
	VM_FLAGS ^= TRACE;
	Succeed_;
}
#endif

int
p_exit(value v, type t)
{
    extern void ec_cleanup1(int exit_code);

    Check_Integer(t);
    ec_cleanup1((int) v.nint);
    exit((int) v.nint);
    Succeed_;		/* for lint */
}


static int
p_dbgcomp(void)
{
    GlobalFlags = (GlobalFlags & ~GOALEXPAND)|DBGCOMP;
    Succeed_;
}

static int
p_nodbgcomp(void)
{
	GlobalFlags &= ~(DBGCOMP|VARIABLE_NAMES|SINGLETON_CHECK);
	GlobalFlags |= GOALEXPAND;
	Succeed_;
}

/*ARGSUSED*/
static int
p_prof(value v, type t, value vf, type tf, value vs, type ts)
{
    int			res;
    stream_id		nst;

    Check_Atom(t);
    if (v.did == d_.on) {
	nst = get_stream_id(vs, ts, SWRITE, &res);
	if (nst == NO_STREAM) {
	    Bip_Error(res)
	}
	if (VM_FLAGS & PROFILING) {
	    Fail_;
	}
	profile_stream_ = nst;
	profile_flags_ = vf.nint;
	VM_FLAGS |= PROFILING;
    } else if (v.did == d_.off) {
	VM_FLAGS &= ~PROFILING;
	profile_flags_ = 0;
    } else {
	Bip_Error(RANGE_ERROR)
    }
    Succeed_;
}


/*
	spied_(Name, Arity, Module)
	just tells you whether a predicate is spied
*/
static int
p_spied(value v1, type t1, value v2, type t2, value vm, type tm)
{
	dident		wdid;
	pri		*proc;
	int		err;

	Check_Module(tm, vm);
	Get_Did(v1, t1, v2, t2, wdid);
	/* is functor/arity defined */
	proc = visible_procedure(wdid, vm.did, tm, PRI_DONTIMPORT);
	if (! proc)
	{
	    Get_Bip_Error(err);
	    Bip_Error(err);
	}
	Succeed_If(PriFlags(proc) & DEBUG_SP);
}


/*
 * manipulation of the global status words GlobalFlags and VM_FLAGS
 * vc,tv	mask defining bits to clear
 * vs,ts	mask defining bits to set
 * vc,tc	returns the new value of the flag word
 */

int
global_flags(int clear, int set)
{
    return (GlobalFlags = (GlobalFlags & ~clear) | set);
}

static int
p_global_flags(value vc, type tc, value vs, type ts, value v, type t)
{
	Check_Integer(tc)
	Check_Integer(ts)
	Return_Unify_Integer(v, t,
		global_flags((int) vc.nint, (int) vs.nint));
}

#define ALLOWED_TO_CLR	ALLOWED_TO_SET
#define ALLOWED_TO_SET	(TRACE|STATISTICS|GLOBAL_NO_IT|WAS_EXIT|NO_EXIT)

static int
p_vm_flags(value vc, type tc, value vs, type ts, value v, type t)
{
	int was_global_no_it;
	Check_Integer(tc)
	Check_Integer(ts)

	Disable_Int();

	was_global_no_it = VM_FLAGS & GLOBAL_NO_IT;
	VM_FLAGS = (VM_FLAGS & ~(ALLOWED_TO_CLR & vc.nint)) |
			(ALLOWED_TO_SET & vs.nint);

	/* keep disabled or re-enable or re-enable twice */
	if (!(VM_FLAGS & GLOBAL_NO_IT))
	{
	    Enable_Int();
	}
	if (was_global_no_it)
	{
	    Enable_Int();
	}

	Return_Unify_Integer(v,t,VM_FLAGS);
}



/*
 * FUNCTION NAME:		auxiliary for get/set_flag/2
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 */
static int
p_sys_flags(value vf, type tf, value vval, type tval)
{
    Check_Integer(tf);
    switch (vf.nint)
    {
    case 1:				/* print_depth */
	if (IsRef(tval))
	{
	    Return_Unify_Integer(vval, tval, PrintDepth);
	}
	else
	{
	    Check_Integer(tval);
	    PrintDepth = vval.nint;
	    Succeed_;
	}

    case 2:				/* load_release_delay */
	if (IsRef(tval))
	{
	    Return_Unify_Integer(vval, tval, LoadReleaseDelay);
	}
	else
	{
	    Check_Integer(tval);
	    LoadReleaseDelay = vval.nint;
	    Succeed_;
	}

    case 3:				/* publishing_param */
	if (IsRef(tval))
	{
	    Return_Unify_Integer(vval, tval, PublishingParam);
	}
	else
	{
	    Check_Integer(tval);
	    PublishingParam = vval.nint;
	    Succeed_;
	}


    default:
	Bip_Error(RANGE_ERROR)
    }
}

#define MAX_EXTENSIONS		       12
/*
 * FUNCTION NAME:		extension/2 predicate
 *
 * PARAMETERS:
 *
 * DESCRIPTION:			This function implements the nondeterministic
 *				Prolog predicate extension/2. Due to the
 *				various #ifdef's possibility, it checks
 *				integers from a certain interval and if
 *				an extensions of that number exists, the
 *				appropriate name is returned.
 */
static int
p_extension(value vext, type text, value v, type t)
{
    dident	ext = (dident)0;
    int		i;
    value	v1;

    Check_Integer(t);
    Check_Output_Atom(text);

    for (i = v.nint; i < MAX_EXTENSIONS; i++)
    {
	switch(i)
	{
	case 0:
	    break;
#ifdef CHIP
	case 1:
	    ext = d_chip_;
	    break;
#endif
#ifdef OBJECTS
	case 2:
	    ext = d_objects_;
	    break;
#endif
#ifdef PRINTAM
	case 3:
	    ext = d_development_;
	    break;
#endif
#ifdef SEDUCE
	case 4:
	    ext = d_seduce_;
	    break;
#endif
#ifdef OPIUM
	case 5:
	    ext = d_opium_;
	    break;
#endif
#ifdef OC
	case 6:
	    ext = d_occur_check_;
	    break;
#endif
#ifdef DFID
	case 7:
	    ext = d_dfid_;
	    break;
#endif
	case 8:
	    /* a dummy is provided in lib7.c if megalog is not used */
	    if (megalog_present ())
		ext = d_megalog_;
	    break;
	case 9:
	    if (par_present())
		ext = d_parallel_;
	    break;
	case 10:
	    if (mps_present())
		ext = d_mps_;
	    break;
	}
	if (ext) {
	    v1.nint = i + 1;
	    Remember(2, v1, tint);
	Return_Unify_Atom(vext, text, ext);
	}
    }
    Cut_External;
    Fail_;
}


/*
 * Enable/disable synchronous event handling.
 * In addition, the handling is disabled automatically
 * when entering the handler for an event which has the defers-property set.
 */

static int
p_events_defer(void)
{
    /* no nesting: fail if already deferred */
    if (VM_FLAGS & EVENTS_DEFERRED)
    	{ Fail_; }
    VM_FLAGS |= EVENTS_DEFERRED;
    Succeed_;
}

static int
p_events_nodefer(void)
{
    VM_FLAGS &= ~EVENTS_DEFERRED;
    if (EVENT_FLAGS & EVENT_POSTED) {
	Fake_Overflow;
    }
    Succeed_;
}


/*
 * request_fail_event(+Structure, +TimeStampArg, +Event)
 * request_fail_event(+Item, +Structure, +TimeStampArg, +Event)
 *	If the TimeStamp is old, update it and trail posting of Event.
 *	Otherwise just succeed.
 *	The event will only be posted if the timestamp is still old
 *	at untrailing time (i.e. there hasn't been a cut in the meantime).
 *	Item should be a term whose lifetime is somehow related with the
 *	event. When Item becomes garbage, the fail_event trail frame also
 *	becomes garbage (it contains a weak pointer to Item). This mechanism
 *	should only be used when the event has already been disabled anyway
 *	via event_disable/1 (otherwise it would depend on GC timing whether
 *	an event is posted or not). If Item is atomic, the trail frame's
 *	lifetime depends _only_ on the timestamp.
 */

static void
_post_fail_event(pword *pitem, word *pdata, int size, int undo_context)
{
    /*
     * Only post the fail event if we are actually failing. Do nothing when
     * this function is called during GC (happens when Item is garbage)
     */
    if (undo_context == UNDO_FAIL)
    {
	if (ec_post_event(*(pword *)pdata) != PSUCCEED)
	{
	    p_fprintf(current_err_, "\nEvent queue overflow in request_fail_event/1");
	    ec_flush(current_err_);
	}
    }
}


static int
p_request_fail_event(value v, type t, value varg, type targ, value vevent, type tevent)
{
    value vitem;
    vitem.nint = 0;
    return p_request_fail_event4(vitem, tint, v, t, varg, targ, vevent, tevent);
}


static int
p_request_fail_event4(value vitem, type titem, value v, type t,
	value varg, type targ, value vevent, type tevent)
{
    pword event;
    pword *pstamp;

    Check_Structure(t);
    Check_Integer(targ);

    if (IsAtom(tevent) || IsInteger(tevent) || IsHandle(tevent))
    {
	event.val.all = vevent.all;
	event.tag.all = tevent.all;
    }
    else if (IsRef(tevent))
    {
	Bip_Error(INSTANTIATION_FAULT);
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }

    if (varg.nint < 1 || varg.nint > DidArity(v.ptr[0].val.did))
    {
	Bip_Error(RANGE_ERROR);
    }
    pstamp = v.ptr + varg.nint;
    if (!ISPointer(pstamp->tag.kernel))
    {
	Bip_Error(TYPE_ERROR);
    }

    ec_trail_undo(_post_fail_event,
    	ISPointer(titem.kernel) ? vitem.ptr : NULL, pstamp,
	(word*) &event, sizeof(pword)/sizeof(word), TRAILED_PWORD);
    Succeed_;
}


/*
 * The function called during untrailing of a fail-write-request.
 * Because this is called during failure, and the engine is not in
 * a proper state, this must be a simple form of write, i.e. no
 * macro/attribute transformations. Also, since we have no module,
 * no operators etc.
 */

static void
_fail_write_stream(pword *pitem, word *pdata, int size, int flags)
{
    int res;
    pword *data = (pword *) pdata;
    stream_id out = get_stream_id(data[0].val, data[0].tag, SWRITE, &res);
    if (out != NO_STREAM)
    {
	res = ec_pwrite(0, FULLDEPTH|VAR_NUMBERS|NO_MACROS|CANONICAL,
	    out, data[1].val, data[1].tag, 1200, 1,
	    d_.default_module, tdict);
    }
    if (res != PSUCCEED)
    {
	p_fprintf(current_err_, "\nCouldn't write in request_fail_write/4 (%d)", res);
	ec_flush(current_err_);
    }
}


static int
p_request_fail_write(value v, type t, value varg, type targ, value vstream, type tstream, value vterm, type tterm)
{
    pword data[2];
    pword *pstamp;

    Check_Integer(targ);
    if (!IsStructure(t))
    {
	Bip_Error(TYPE_ERROR);
    }
    if (varg.nint < 1 || varg.nint > DidArity(v.ptr[0].val.did))
    {
	Bip_Error(RANGE_ERROR);
    }
    pstamp = v.ptr + varg.nint;
    if (!ISPointer(pstamp->tag.kernel))
    {
	Bip_Error(TYPE_ERROR);
    }

    data[0].val.all = vstream.all;
    data[0].tag.all = tstream.all;
    data[1].val.all = vterm.all;
    data[1].tag.all = tterm.all;
    ec_trail_undo(_fail_write_stream, NULL, pstamp, (word *) &data,
	sizeof(data)/sizeof(word), TRAILED_PWORD);
    Succeed_;
}


/*
 * timestamp_init(+Struct, +Arg)
 *	Initialise a timestamp Struct[Arg] to be old.
 *
 * timestamp_update(+Struct, +Arg)
 *	Initialise or update a timestamp Struct[Arg] to be current.
 *
 * timestamp_age(+Struct, +Arg, -Age)
 *	Check timestamp Struct[Arg]: Age is the atom 'old' or 'current'.
 *
 * timestamp_older(+Struct1, +Arg1, +Struct2, +Arg2)
 *	Succeed if timestamp Struct1[Arg1] is older than Struct2[Arg2]
 */


static int
p_timestamp_init(value vstruct, type tstruct, value varg, type targ)
{
    pword *pstamp;
    Check_Integer(targ);
    Check_Structure(tstruct);
    if (varg.nint < 1 || varg.nint > DidArity(vstruct.ptr[0].val.did))
    {
	Bip_Error(RANGE_ERROR);
    }
    pstamp = vstruct.ptr + varg.nint;
    if (!NewLocation(pstamp) && !NewValue(pstamp->val, pstamp->tag))
    {
	Trail_Pword(pstamp);
    }
    Init_Stamp(pstamp);
    Succeed_;
}


static int
p_timestamp_update(value vstruct, type tstruct, value varg, type targ)
{
    pword *pstamp;
    Check_Integer(targ);
    Check_Structure(tstruct);
    if (varg.nint < 1 || varg.nint > DidArity(vstruct.ptr[0].val.did))
    {
	Bip_Error(RANGE_ERROR);
    }
    pstamp = vstruct.ptr + varg.nint;
    /*
     * If the current value is already a timestamp,
     * !NewValue(pstamp->val, pstamp->tag) is the same as OldStamp(pstamp)
     */
    if (!NewLocation(pstamp) && !NewValue(pstamp->val, pstamp->tag))
    {
	Trail_Pword(pstamp);
    }
    Make_Stamp(pstamp);
    Succeed_;
}


static int
p_timestamp_age(value vstruct, type tstruct, value varg, type targ, value vstate, type tstate)
{
    pword *pstamp;
    Check_Ref(tstate);	/* it makes no sense to use it as a test! */
    Check_Integer(targ);
    Check_Structure(tstruct);
    if (varg.nint < 1 || varg.nint > DidArity(vstruct.ptr[0].val.did))
    {
	Bip_Error(RANGE_ERROR);
    }
    pstamp = vstruct.ptr + varg.nint;
    Check_Ref(pstamp->tag);
    Return_Unify_Atom(vstate, tstate, OldStamp(pstamp) ? d_old_ : d_current_);
}


static int
p_timestamp_older(value vstruct1, type tstruct1, value varg1, type targ1, value vstruct2, type tstruct2, value varg2, type targ2)
{
    Check_Integer(targ1);
    Check_Integer(targ2);
    Check_Structure(tstruct1);
    Check_Structure(tstruct2);
    if (varg1.nint < 1 || varg1.nint > DidArity(vstruct1.ptr[0].val.did)
     || varg2.nint < 1 || varg2.nint > DidArity(vstruct2.ptr[0].val.did))
    {
	Bip_Error(RANGE_ERROR);
    }
    Check_Ref(vstruct1.ptr[varg1.nint].tag);
    Check_Ref(vstruct2.ptr[varg2.nint].tag);
    Succeed_If(vstruct1.ptr[varg1.nint].val.ptr < vstruct2.ptr[varg2.nint].val.ptr);
}


/*----------------------------------------------------------------------
 * New tracer
 *----------------------------------------------------------------------*/

/*
 * Disable tracing for all subgoals of the current top trace stack frame.
 * e.g. because we are inside the tracer code itself. This is done by
 * marking the top frame.
 */
static int
p_disable_tracing(void)
{
    if (TD) {
	Set_Tf_Flag(TD, TF_INTRACER);
#ifdef PRINTAM
	if (VM_FLAGS & TRACE) {
	    /* switch abstract instruction tracing off while in the debugger,
	     * but reenable it in cont_debug/0, based on TF_SYSTRACE flag
	     */
	    VM_FLAGS &= ~TRACE;
	    Set_Tf_Flag(TD, TF_SYSTRACE);
	}
#endif
    }
    Succeed_;
}


static int
p_tracing(void)
{
    Succeed_If(Tracing);
}


/*
 * This is called from within the DEBUG_CALL_EVENT handler and raises the
 * DEBUG_INIT_EVENT if it is the first event in a new debug session.
 */
static int
p_raise_init_event(void)
{
    if (TRACEMODE & TR_STARTED)
    {
	TRACEMODE &= ~TR_STARTED;
	Bip_Error(DEBUG_INIT_EVENT)
    }
    Succeed_;
}


/*
 * return the current debug stack
 */
static int
p_current_td(value v, type t)
{
    Return_Unify_Pw(v, t, TAGGED_TD.val, TAGGED_TD.tag);
}


/*
 * generate the next incocation number
 */
static int
p_new_invoc(value v, type t)
{
    word i = NINVOC++;
    Return_Unify_Integer(v, t, i);
}


/*
 * failure_culprit(-FailureCulpritInvoc, -LastInvoc)
 * return the information that the engine has stored about the
 * goal responsible for the most recent failure.
 */
static int
p_failure_culprit(value vf, type tf, value vi, type ti)
{
    Prepare_Requests;
    if (FCULPRIT < 0)
    	{ Fail_; }
    Request_Unify_Integer(vf, tf, FCULPRIT);
    Request_Unify_Integer(vi, ti, NINVOC-1);
    Return_Unify;
}


/*
 * Check whether the given data matches the current prefilter conditions
 */
static int
p_of_interest(value vport, type tport, value vinvoc, type tinvoc, value vdepth, type tdepth, value vproc, type tproc, value vbrkpt, type tbrkpt)
{
    word flags = vproc.priptr ? PriFlags(vproc.priptr) : DEBUG_TR;
    word port = IsInteger(tport) ? vport.nint : OTHER_PORT;
    /* Honour breakpoints only at CALL ports. This could be changed. */
    word brkpt = (port == CALL_PORT ? vbrkpt.nint : 0);
    Check_Integer(tinvoc);
    Check_Integer(tdepth);
    Succeed_If(PortWanted(port) && OfInterest(flags, vinvoc.nint, vdepth.nint, brkpt));
}


/*
 * Set the tracer modes and prefiltering parameters
 */
static int
p_trace_mode(value v, type t, value vmode, type tmode)
{
    switch (v.nint) {
    case 0:				/* creep */
	JMININVOC = 0; JMAXINVOC = MAX_INVOC;
	JMINLEVEL = 0; JMAXLEVEL = MAX_DEPTH;
	PORTFILTER = ANY_NOTIFIES;
	TRACEMODE = TR_TRACING;
	break;
    case 1:				/* jump(Invoc) */
	JMININVOC= JMAXINVOC = vmode.nint;
	JMINLEVEL = 0; JMAXLEVEL = MAX_DEPTH;
	PORTFILTER = ANY_NOTIFIES;
	TRACEMODE = TR_TRACING;
	break;
    case 2:				/* leap */
	JMININVOC = 0; JMAXINVOC = MAX_INVOC;
	JMINLEVEL = 0; JMAXLEVEL = MAX_DEPTH;
	PORTFILTER = ANY_NOTIFIES;
	TRACEMODE = TR_TRACING|TR_LEAPING;
	break;
    case 3:				/* skip(Depth) */
	JMININVOC = 0; JMAXINVOC = MAX_INVOC;
	JMINLEVEL = 0; JMAXLEVEL = vmode.nint;
	PORTFILTER = ANY_NOTIFIES &
		~(PortFilterBit(NEXT_PORT)|PortFilterBit(ELSE_PORT));
	TRACEMODE = TR_TRACING;
	break;
    case 4:				/* jump(Level) */
	JMININVOC = 0; JMAXINVOC = MAX_INVOC;
	JMINLEVEL = JMAXLEVEL = vmode.nint;
	PORTFILTER = ANY_NOTIFIES;
	TRACEMODE = TR_TRACING;
	break;
    case 5:				/* zap(port), nodebug */
	PORTFILTER = vmode.nint;
	if (PORTFILTER == 0)		/* nodebug */
	    TRACEMODE = 0;
	break;

    case 6:
	JMINLEVEL = vmode.nint;
	break;
    case 7:
	JMAXLEVEL = vmode.nint;
	break;
    case 8:
	JMININVOC = vmode.nint;
	break;
    case 9:
	JMAXINVOC = vmode.nint;
	break;
    case 11:
	TRACEMODE = vmode.nint ? TR_TRACING|TR_LEAPING : TR_TRACING;
	break;


    /*
     * Init the tracer state.  Resets all the counters and global settings.
     * Also sets the TR_STARTED flag which is then used to trigger
     * the DEBUG_INIT_EVENT from within the first DEBUG_CALL_EVENT. This
     * is done with raise_init_event/0, which resets the TR_STARTED flag.
     */
    case 12:				/* reset */
	/* Initialisation is only allowed while there is no trace stack yet. */
	if (TD) { Fail_; }
	TracerInit;
	break;

    case 13:				/* toggle systrace */
#ifdef PRINTAM
    	if (TD) {
	    Flip_Tf_Flag(TD, TF_SYSTRACE);
	}
#endif
	break;

    default:
	Bip_Error(RANGE_ERROR);
    }
    Succeed_;
}


/*
 * Retrieve information about the last failure
 * The engine keeps a limited size stack in the array FTRACE[MAX_FAILTRACE]
 * with the procedure identifiers of all the procedures that failed during
 * the last failure. FTRACE[0] is the goal that caused the failure and
 * FTRACE[1..FDROP] are the ancestors that failed as a result.
 * get_fail_info(+I, -Frame) retrieves FTRACE[I] in the form of a
 * fake trace frame.
 */
static int
p_get_fail_info(value vi, type ti, value vf, type tf)
{
    pword goal;
    pword *pw;
    dident pdid, mdid;
    int i;
    if (vi.nint >= MAX_FAILTRACE)
    {
    	Return_Unify_Nil(vf, tf);
    }
    pdid = FTRACE[vi.nint].proc->did;	/* make a dummy goal */
    if (DidArity(pdid) > 0)
    {
	pw = TG;
	if (pdid == d_.list)
	{
	    Push_List_Frame();
	    Make_Atom(pw+0, d_.ellipsis);
	    Make_Atom(pw+1, d_.ellipsis);
	    Make_List(&goal,pw);
	}
	else
	{
	    Push_Struct_Frame(pdid);
	    for(i=1; i <= DidArity(pdid); ++i)
	    {
		Make_Atom(pw+i, d_.ellipsis);
	    }
	    Make_Struct(&goal,pw);
	}
    }
    else if (pdid == d_.nil)
    {
	Make_Nil(&goal);
    }
    else
    {
	Make_Atom(&goal,pdid);
    }
    mdid = PriModule(FTRACE[vi.nint].proc);
    if (mdid == D_UNKNOWN) mdid = FTRACE[vi.nint].proc->module_ref;
    Make_Partial_Dbg_Frame(pw, FTRACE[vi.nint].invoc, goal, SUSP_MAX_PRIO,
    		FTRACE[vi.nint].proc, FTRACE[vi.nint].source_pos.file, 
                FTRACE[vi.nint].source_pos.line,
                FTRACE[vi.nint].source_pos.from,
		FTRACE[vi.nint].source_pos.to, mdid);
    Return_Unify_Structure(vf, tf, pw);
}


/*
 * Construct a trace frame from a suspension
 */
static int
p_susp_to_tf(value vs, type ts, value vf, type tf)
{
    pword *pw;

    Check_Type(ts, TSUSP);
    Make_Partial_Dbg_Frame(pw, SuspDebugInvoc(vs.ptr), vs.ptr[SUSP_GOAL],
		SuspPrio(vs.ptr), SuspProc(vs.ptr), d_.empty, 0, 0, 0,
		SuspModule(vs.ptr));
    Return_Unify_Structure(vf, tf, pw);
}

/*
 * Complete a trace frame with choicepoint stamp and proc, if possible
 * If tg/vg is not a proper goal, we use true/0 for proc
 */
static int
p_make_tf(value vpush, type tpush, value vi, type ti, value vg, type tg, value vm, type tm, value vlm, type tlm, value vp, type tp, value vf, type tf)
{
    pword *pw;
    pri *proc = NULL;
    uword depth;
    word invoc = vi.nint;
    dident goal_did;
    extern pri *true_proc_;
    Check_Integer(tpush);
    Check_Integer(tp);
    Check_Output_Integer(ti);
    Check_Ref(tf);

    /*
     * Find the predicate corresponding to the "goal term". If there is none
     * (can happen when invoked from the trace_xxx_port builtins), we
     * put the pri of true/0 into the frame. Otherwise we'd have to check
     * for NULLs everwhere. It is only used for flags and home module.
     */
    goal_did = IsStructure(tg) ? vg.ptr->val.did
    		: IsAtom(tg) ? vg.did
    		: IsList(tg) ? d_.list
		: IsNil(tg) ? d_.nil
		: D_UNKNOWN;
    if (goal_did)
	proc = qualified_procedure(goal_did, vlm.did, vm.did, tm);
    if (!proc)
    {
	Set_Bip_Error(0);			/* reset error code */
    	proc = true_proc_;
    }
    if (IsRef(ti))				/* new invoc if none given */
    {
    	invoc = NINVOC++;
	Bind_Var(vi, ti, invoc, TINT);
    }
    depth = TD ? DLevel(TD)+1 : 0;
    if (vpush.nint)				/* push */
    {
	if (!NewLocation(TD)) { Trail_Pword(&TAGGED_TD); }
	Push_Dbg_Frame(pw, invoc, vg, tg, depth, vp.nint, proc, d_.empty, 0, 0, 0, vm.did);
    }
    else					/* don't push */
    {
	Make_Dbg_Frame(pw, invoc, vg, tg, depth, vp.nint, proc, d_.empty, 0, 0, 0, vm.did);
	pw[TF_ANCESTOR] = TAGGED_TD;
    }
    Set_Tf_Flag(TD, TF_INTRACER);		/* assume we are within tracer */
    Return_Unify_Structure(vf, tf, pw);
}

static int
p_pop_tf(void)
{
    Pop_Dbg_Frame();
    /* Since we've popped the top frame, we need to set the TF_INTRACER bit
     * in the newly exposed top frame (we assume we are within tracer code) */
    if (TD) {
	Set_Tf_Flag(TD, TF_INTRACER);
    }
    Succeed_;
}


/*
 * get_tf_prop(+Frame, +What, -Info)
 * get hidden information from the trace frame
 */
static int
p_get_tf_prop(value vf, type tf, value vwhat, type twhat, value v, type t)
{
    Check_Structure(tf);
    Check_Atom(twhat);
    if (vwhat.did == d_.break0)	/* breakpoint flag */
    {
	Return_Unify_Integer(v, t, TfFlags(vf.ptr) & TF_BREAK);
    }
    else if (vwhat.did == d_.spy)
    {
	pri *proc = vf.ptr[TF_PROC].val.priptr;
	Return_Unify_Atom(v, t, !proc ? d_.off :
	    	PriFlags(proc) & DEBUG_SP ? d_.on : d_.off);
    }
    else if (vwhat.did == d_.skip)
    {
	pri *proc = vf.ptr[TF_PROC].val.priptr;
	Return_Unify_Atom(v, t, !proc ? d_.off :
		PriFlags(proc) & DEBUG_SK ? d_.on : d_.off);
    }
    else if (vwhat.did == d_.module0)	/* definition module */
    {
	dident mod;
	pri *proc = vf.ptr[TF_PROC].val.priptr;

	if (!proc)			/* shouldn't happen */
	    { Bip_Error(NOENTRY); }

	/* if no definition module known (yet), return the descriptor module */
	mod = PriHomeModule(proc);
	if ( mod == D_UNKNOWN )
	    mod = PriModule(proc);
	Return_Unify_Atom(v, t, mod);
    }
    else if (vwhat.did == d_.question)	/* internal */
    {
	Return_Unify_Integer(v, t, TfFlags(vf.ptr) >> 8);
    }
    Bip_Error(RANGE_ERROR);
}


/*
 * Returns a list of the recently created suspensions which have
 * invocation number DBG_DELAY_INVOC or higher.  DBG_DELAY_INVOC
 * must always be set when the DEBUG_SUSP_EVENT is raised, to mark
 * the oldest suspension that needs to be traced.
 * Since the filter conditions can be changed interactively at every
 * DELAY port, we don't prefilter the list here any further!
 */

int
p_delay_port_susps(value v, type t)
{
    pword list;
    pword *pld = LD;
    if (!DBG_DELAY_INVOC || !LD)
    {
	p_fprintf(current_err_, "\nUnexpected state in delay_port_susps/1");
	ec_flush(current_err_);
	Return_Unify_Nil(v, t);
    }
    Make_Nil(&list);
    while(pld && SuspDebugInvoc(pld) >= DBG_DELAY_INVOC)
    {
	/* if alread dead, it's too late to trace the DELAY */
	if(!SuspDead(pld))
	{
	    pword *pw = TG;
	    Push_List_Frame();
	    pw[0].val.ptr = pld;
	    pw[0].tag.kernel = TSUSP;
	    pw[1] = list;
	    Make_List(&list, pw);
	    pld = SuspPrevious(pld);
	}
    }
    DBG_DELAY_INVOC = 0;
    Return_Unify_Pw(v, t, list.val, list.tag);
}


/*
 * Make a new suspension. Return code is PSUCCEED or DEBUG_SUSP_EVENT.
 * An external can call this function several times. If any of the calls
 * returned DEBUG_SUSP_EVENT, then the external should return DEBUG_SUSP_EVENT.
 * itself. If this is not done, the DELAY ports will not be traced.
 */

int Winapi
ec_make_suspension(pword goal, int prio, void *proc, pword *psusp)
{
    pword *susp = TG;
    psusp->val.ptr = susp;
    psusp->tag.kernel = TSUSP;
    TG += SUSP_SIZE;
    Check_Gc
    susp[SUSP_MODULE].val.did = ((pri*)proc)->module_ref;
    susp[SUSP_MODULE].tag.kernel = ModuleTag(((pri*)proc)->module_ref);
    if (prio == 0)
    	prio = PriPriority(((pri*)proc));	/* use procedure's setting */
    Init_Susp_Header(susp, ((pri*)proc));
    Init_Susp_State(susp, prio);		/* priority */
    susp[SUSP_GOAL].val.all = goal.val.all;
    susp[SUSP_GOAL].tag.all = goal.tag.all;
    if (Tracing && AnyPortWanted)
    {
	/* the tracer is on, assign an invocation number */
	Set_Susp_DebugInvoc(susp, NINVOC);
	++NINVOC;
	/* only if the port is of interest, raise the debug event */
	if (PortWanted(DELAY_PORT) && OfInterest(PriFlags(((pri*)proc)), NINVOC-1, DLevel(TD)+1, 0))
	{
	    if (DBG_DELAY_INVOC == 0) {
		DBG_DELAY_INVOC = NINVOC-1;
	    }
	    return DEBUG_SUSP_EVENT;
	}
    }
    return PSUCCEED;
}

