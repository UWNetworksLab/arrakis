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
 * VERSION	$Id: bip_db.c,v 1.6 2008/09/01 23:56:07 jschimpf Exp $
 */

/****************************************************************************
 *
 *		SEPIA Built-in Predicates: Database
 *
 *
 *****************************************************************************/
/*
 * IDENTIFICATION               bip_db.c
 *
 * DESCRIPTION
 *
 * CONTENTS:
 *
 * AUTHOR       VERSION  DATE   REASON
 * periklis             26.9.89 Major revision for the logical update semantics
 * Dominique
 *
 */


#include 	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include        "io.h"
#include	"opcode.h"
#include	"gencode.h"
#include	"dict.h"
#include	"database.h"
#include	"emu_export.h"
#include	"module.h"
#include 	"debug.h"	/* for external definitions */
#include	"property.h"

#define MAX_KILLS		50
#define MAX_KILLED_SIZE		1000

#define Add_Did(vname, tname, varity, tarity, d)		\
	if (IsRef(tname) || IsRef(tarity)) {			\
		Bip_Error(INSTANTIATION_FAULT)			\
	}							\
	if (IsNil(tname)) d = add_dict(d_.nil, (int) varity.nint);\
	else {							\
	    if ((!IsAtom(tname)) || (!IsInteger(tarity))) {	\
		Bip_Error(TYPE_ERROR)				\
	    }							\
	    d = add_dict(vname.did, (int) varity.nint);		\
	}

#define Get_Macro_Did(vproc, tproc, wd)		\
	if (IsStructure(tproc) && vproc.ptr->val.did == d_type_) {\
	    int res = _type_did(vproc.ptr+1, &(wd));\
	    Return_If_Error(res);\
	} else {\
	    Get_Functor_Did(vproc, tproc, wd)\
	}


extern void
    add_proc_to_chain(pri *p, proc_duet **chain),
    reclaim_abolished_procedures(void);

extern vmcode par_fail_code_[];


extern t_ext_type heap_rec_header_tid;

static int
#ifdef DBGING_DYN_DB
    p_print_gc(void),
#endif /* DBGING_DYN_DB */
    p_abolish(value n, type tn, value a, type ta, value vm, type tm),
    p_current_functor(value valn, type tagn, value vala, type taga, value vopt, type topt, value valsn, type tagsn),
    p_dynamic_create(value v1, type t1, value v2, type t2, value vm, type tm),
    p_dynamic_source(value v1, type t1, value v2, type t2, value vsrc, type tsrc, value vm, type tm),
    p_is_dynamic(value v1, type t1, value v2, type t2, value vm, type tm),
    p_is_built_in(value val, type tag, value vm, type tm),
    p_is_predicate(value val, type tag, value vm, type tm),
    p_module_predicates(value vwhich, type twhich, value v, type t, value vm, type tm),
    p_external(value vp, type tp, value vf, type tf, value vm, type tm),
    p_b_external(value vp, type tp, value vf, type tf, value vm, type tm),
    p_external_body(value vpred, type tpred, value vmod, type tmod),
    p_load_eco(value vfile, type tfile, value vopt, type topt, value vmod, type tmod, value vout, type tout),
#ifdef PRINTAM
    p_vm_statistics(value v, type t),
#endif
#ifndef NOALS
    p_als(value val, type tag, value vm, type tm),
#endif
    p_store_pred(value vproc, type tproc, value vcode, type tcode, value vsize, type tsize, value vbrktable, type tbrktable, value vflags, type tflags, value vfid, type tfid, value vlid, type tlid, value vbid, type tbid, value vm, type tm),
    p_retrieve_code(value vproc, type tproc, value vcode, type tcode, value vm, type tm),
    p_decode_code(value vcode, type tcode, value v, type t),
    p_functor_did(value vspec, type tspec, value v, type t),
    p_set_proc_flags(value vproc, type tproc, value vf, type tf, value vv, type tv, value vm, type tm),
    p_proc_flags(value vn, type tn, value vc, type tc, value vf, type tf, value vm, type tm, value vp, type tp),
    p_define_macro(value vproc, type tproc, value vtrans, type ttrans, value vprop, type tprop, value vmod, type tmod),
    p_erase_macro(value vproc, type tproc, value vmod, type tmod),
    p_erase_macro3(value vproc, type tproc, value vprop, type tprop, value vmod, type tmod),
    p_illegal_macro(value v1, type t1, value v2, type t2, value v3, type t3, value v4, type t4, value v5, type t5),
    p_is_macro(value v1, type t1, value v2, type t2, value v3, type t3, value v4, type t4, value v5, type t5, value v6, type t6),
    p_visible_term_macro(value v1, type t1, value v2, type t2, value v3, type t3, value v4, type t4, value v5, type t5, value v6, type t6),
    p_visible_goal_macro(value vgoal, type tgoal, value vtrans, type ttrans, value vlm, type tlm, value vcm, type tcm),
    p_trimcore(void),
    p_mode(value pv, type pt, value mv, type mt),
    p_bound_arg(value vproc, type tproc, value va, type ta, value vb, type tb, value mv, type mt);

static int	_type_did(pword*, dident*);

static	dident
		d_autoload_,
		d_auxiliary_,
		d_demon_,
		d_deprecated_,
		d_dynamic_,
		d_static_,
		d_invisible_,
		d_imported_,
		d_reexported_,
		d_exported_,
		d_parallel_,
		d_start_tracing_,
		d_plusminus,
		d_minusplus,
		d_constant,
		d_constant2,
		d_nonvar,
		d_ground,
		d_a1,
		d_y1,
		d_ymask,
		d_align,
		d_table2,
		d_refm,
		d_edesc,
		d_try_table2,
		d_t1,
		d_w1,
		d_pw1,
		d_mv1,
                d_an1,
		d_nv1,
		d_par_fail,
		d_init2,
		d_val1,
		d_tag1,
		d_opc1,
		d_proc1,
		d_functor1,
		d_ref1,
                d_ref2,
		d_source_file_,
		d_source_line_,
		d_source_offset_,
		d_tags,
		d_trace_meta_,
		d_type0_,
		d_type_;


#define PREDLIST_UNDECLARED	0
#define PREDLIST_LOCAL		1
#define PREDLIST_EXPORTED	2
#define PREDLIST_REEXPORTED	3
#define PREDLIST_EXREEX		4
#define PREDLIST_DEFINED	5	/* LOCAL or EXPORTED */
#define PREDLIST_UNDEFINED	6	/* LOCAL or EXPORTED */
#define PREDLIST_NOMODULE	7
#define PREDLIST_NOEXPORT	8
#define PREDLIST_DEPRECATED	9
#define PREDLIST_SIZE		10

static	dident
		d_predlist_option[PREDLIST_SIZE];


/*
When a clause is asserted, its birth tag is set to the value of
DynGlobalClock When a clause is retracted, its death tag is set
to it.  After both actions, DynGlobalClock is incremented by one.
Whenever a call to a dynamic procedure is made it 'sees' only the
currently living clauses, i.e. the ones for which
birth < (DynGlobalClock at time of call) <= death.
*/

/* DynKilledCodeSize keeps a count of the size of 'retracted' code.
When this exceeds a set value, the dynamic database garbage collector
is invoked.
*/


void
bip_db_init(int flags)
{
    pri		* proc;

    d_autoload_ = in_dict("autoload", 0);
    d_auxiliary_ = in_dict("auxiliary", 0);
    d_trace_meta_ = in_dict("trace_meta", 0);
    d_demon_ = in_dict("demon", 0);
    d_deprecated_ = in_dict("deprecated", 0);
    d_static_ = in_dict("static", 0);
    d_dynamic_ = in_dict("dynamic", 0);
    d_invisible_ = in_dict("invisible", 0);
    d_imported_ = in_dict("imported", 0);
    d_reexported_ = in_dict("reexported", 0);
    d_exported_ = in_dict("exported", 0);
    d_parallel_ = in_dict("parallel", 0);
    d_start_tracing_ = in_dict("start_tracing", 0);
    d_plusminus = in_dict("+-", 0);
    d_minusplus = in_dict("-+", 0);
    d_constant = in_dict("constant", 0);
    d_constant2 = in_dict("constant", 2);
    d_nonvar = in_dict("nonvar", 0);
    d_ground = in_dict("ground", 0);
    d_a1 = in_dict("a", 1);
    d_y1 = in_dict("y", 1);
    d_ymask = in_dict("ymask", 1);
    d_align = in_dict("align", 1);
    d_table2 = in_dict("table", 2);
    d_edesc = in_dict("edesc", 1);
    d_try_table2 = in_dict("try_table", 2);
    d_t1 = in_dict("t", 1);
    d_w1 = in_dict("w", 1);
    d_pw1 = in_dict("pw", 1);
    d_nv1 = in_dict("nv", 1);
    d_mv1 = in_dict("mv", 1);
    d_an1 = in_dict("an", 1);
    d_val1 = in_dict("val", 1);
    d_tag1 = in_dict("tag", 1);
    d_opc1 = in_dict("o", 1);
    d_functor1 = in_dict("functor", 1);
    d_proc1 = in_dict("proc", 1);
    d_type0_ = in_dict("type", 0);
    d_type_ = in_dict("type", 1);
    d_init2 = in_dict("init", 2);
    d_ref1 = in_dict("ref", 1);
    d_ref2 = in_dict("ref", 2);
    d_refm = in_dict("refm", 2);
    d_tags = in_dict("tags", 0);
    d_par_fail = in_dict("par_fail", 0);
    d_source_file_ = in_dict("source_file", 0);
    d_source_line_ = in_dict("source_line", 0);
    d_source_offset_ = in_dict("source_offset", 0);

    d_predlist_option[PREDLIST_UNDECLARED] = in_dict("undeclared",0);
    d_predlist_option[PREDLIST_LOCAL] = in_dict("local",0);
    d_predlist_option[PREDLIST_EXPORTED] = in_dict("exported",0);
    d_predlist_option[PREDLIST_REEXPORTED] = in_dict("reexported",0);
    d_predlist_option[PREDLIST_EXREEX] = in_dict("exported_reexported",0);
    d_predlist_option[PREDLIST_DEFINED] = in_dict("defined",0);
    d_predlist_option[PREDLIST_UNDEFINED] = in_dict("undefined",0);
    d_predlist_option[PREDLIST_NOMODULE] = in_dict("no_module",0);
    d_predlist_option[PREDLIST_NOEXPORT] = in_dict("no_export",0);
    d_predlist_option[PREDLIST_DEPRECATED] = in_dict("deprecated",0);

    if (!(flags & INIT_SHARED))
	return;

    DynGlobalClock = 1;
    DynKilledCodeSize = 0;
    DynNumOfKills = 0;
    DynamicProcedures = 0;

#ifndef NOALS
    exported_built_in(in_dict("als_", 2), p_als, B_SAFE);
#endif
#ifdef PRINTAM
    (void) built_in(in_dict("vm_statistics", 1), p_vm_statistics, B_UNSAFE|U_SIMPLE);
#endif
    (void) built_in(in_dict("load_eco", 4), p_load_eco, B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("store_pred", 9), p_store_pred, B_UNSAFE);
    exported_built_in(in_dict("retrieve_code", 3), p_retrieve_code, B_UNSAFE)
	-> mode = BoundArg(2, GROUND);
    (void) exported_built_in(in_dict("decode_code", 2), p_decode_code, B_UNSAFE);
    (void) exported_built_in(in_dict("functor_did", 2), p_functor_did, B_UNSAFE);

#ifdef DBGING_DYN_DB
    (void) built_in(in_dict("print_gc", 0), p_print_gc, B_SAFE);
#endif /* DBGING_DYN_DB */

    (void) local_built_in(in_dict("trimcore0", 0), p_trimcore, B_SAFE);
    (void) exported_built_in(in_dict("abolish_", 3), p_abolish, B_SAFE);
    (void) local_built_in(in_dict("dynamic_create_", 3), p_dynamic_create, B_SAFE);
    (void) exported_built_in(in_dict("dynamic_source_", 4), p_dynamic_source, B_UNSAFE|U_SIMPLE);
    exported_built_in(in_dict("is_dynamic_", 3), p_is_dynamic, B_SAFE);
    (void) local_built_in(in_dict("is_built_in_", 2), p_is_built_in, B_SAFE);
    proc = exported_built_in(in_dict("is_predicate_", 2),
					  p_is_predicate, B_SAFE);
    b_built_in(in_dict("current_functor", 4),
	       p_current_functor, d_.kernel_sepia)
	-> mode = BoundArg(1, CONSTANT) | BoundArg(2, CONSTANT);
    (void) exported_built_in(in_dict("external_", 3), p_external, B_SAFE);
    (void) exported_built_in(in_dict("b_external_", 3), p_b_external, B_SAFE);
    (void) exported_built_in(in_dict("external_body", 2),
		      p_external_body, B_SAFE);
    (void) exported_built_in(in_dict("b_external_body", 2),
		      p_external_body, B_SAFE);
    local_built_in(in_dict("local_proc_flags", 5), p_proc_flags, B_UNSAFE|U_GROUND)
	-> mode = BoundArg(3, GROUND);
    (void) local_built_in(in_dict("set_proc_flags", 4), p_set_proc_flags, B_UNSAFE);
    (void) local_built_in(in_dict("dict_param", 2), ec_dict_param, B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("garbage_collect_dictionary", 0),
					ec_gc_dictionary, B_SAFE);
    (void) exported_built_in(in_dict("mode_", 2), p_mode, B_SAFE|U_SIMPLE);
    exported_built_in(in_dict("bound_arg_", 4), p_bound_arg, B_SAFE|U_SIMPLE)
	-> mode = BoundArg(3, NONVAR);
    (void) exported_built_in(in_dict("define_macro_", 4), p_define_macro, B_UNSAFE);
    (void) exported_built_in(in_dict("erase_macro_", 2), p_erase_macro, B_UNSAFE);
    (void) exported_built_in(in_dict("erase_macro_", 3), p_erase_macro3, B_UNSAFE);
    (void) exported_built_in(in_dict("is_macro", 6), p_is_macro, B_SAFE);
    (void) local_built_in(in_dict("visible_term_macro", 6), p_visible_term_macro, B_SAFE);
    (void) local_built_in(in_dict("illegal_macro", 5), p_illegal_macro, B_SAFE);
    (void) local_built_in(in_dict("visible_goal_macro", 4), p_visible_goal_macro, B_UNSAFE);
    local_built_in(in_dict("module_predicates", 3), p_module_predicates, B_UNSAFE)
	-> mode = BoundArg(2, GROUND);
#ifdef lint
    (void) als((long)0);
#endif
}

#ifdef DBGING_DYN_DB
static int
p_print_gc(void) /* print debugging information for the garbage collector */
{
p_fprintf(current_err_, "bip_db.c/p_print_gc: \n");
p_fprintf(current_err_, "DynGlobalClock: ");
p_fprintf(current_err_, "%d \n", DynGlobalClock);
p_fprintf(current_err_, "DynKilledCodeSize: ");
p_fprintf(current_err_, "%d \n", DynKilledCodeSize);
}
#endif /* DBGING_DYN_DB */



/* ********************************************************************
			STATIC AND DYNAMIC CODE
 * ******************************************************************* */


static int
p_load_eco(value vfile, type tfile, value vopt, type topt, value vmod, type tmod, value vout, type tout)
{
    stream_id nst;
    char *file;
    int	res;
    pword mod_pw;

    Get_Name(vfile, tfile, file);
    Check_Integer(topt);
    Check_Atom_Or_Nil(vmod, tmod);

    nst = ec_open_file(file, SREAD, &res);
    if (nst == NO_STREAM)
    {
	Bip_Error(res);
    }
    mod_pw.val.all = vmod.all;
    mod_pw.tag.all = tmod.all;
    res = ec_load_eco_from_stream(nst, vopt.nint, &mod_pw);
    (void) ec_close_stream(nst);
    if (res != PSUCCEED)
	return res;
    Return_Unify_Pw(mod_pw.val, mod_pw.tag, vout, tout);
}


#ifndef NOALS
/*
	als_(Name/Arity, Module)
		It prints on the current ouput stream the abstract
		code of the specified procedure.
*/
static int
p_als(value val, type tag, value vm, type tm)
{
    dident	wdid;
    vmcode	*code = 0;
    vmcode	*label = 0;
    int		res;
    vmcode	*save_code;
    pri		*proc;
    unsigned	dflags;
    int		err;

    extern vmcode *print_am(register vmcode *code, vmcode **label, int *res, int option);

    Check_Module(tm, vm);
#ifdef PRINTAM
    if (!IsRef(tag) && IsInteger(tag))
	code = (vmcode *) val.nint;
    else
#endif
    {
	Get_Proc_Did(val, tag, wdid);
	proc = visible_procedure(wdid, vm.did, tm, 0);
	if (proc)
	{
	    if (IsLocked(proc->module_def)) {
		Bip_Error(LOCKED)
	    }
	    code = PriCode(proc);
	    dflags = PriFlags(proc);
	    p_fprintf(current_output_, "\n%s", DidName(wdid));
	    p_fprintf(current_output_, "/%d", DidArity(wdid));
	    if (PriCodeType(proc) != VMCODE)
	    {
		(void) ec_outf(current_output_, "\ta built-in procedure\n", 22);
		Fail_;
	    }
	}
	else /* procedure not visible */
	{
	    Get_Bip_Error(err);
	    Bip_Error(err);
	}
    }
    if (code)
    {
#ifdef PRINTAM
	p_fprintf(current_output_, " (%d):", code);
#else
	(void) ec_outfs(current_output_, " :");
#endif
	(void) ec_newline(current_output_);
	save_code = code;
	do
	    code = print_am(code, &label, &res, 1);
	while (code || (code = label));
	if (res == PFAIL)
	    {Fail_}
	Succeed_;
    }
    else
    {
	Bip_Error(NOENTRY);
    }
}

#if defined(PRINTAM) || defined(LASTPP)
int
als(long int addr)	/* for use with dbx */
{
    value vaddr, vmod;
    vaddr.nint = addr;
    vmod.did = d_.default_module;
    return p_als(vaddr, tint, vmod, tdict);
}
#endif /* PRINTAM */
#endif /* NOALS */

#ifdef PRINTAM
static int
p_vm_statistics(value v, type t)
{
    if (IsRef(t))
    {
	Return_Unify_Atom(v,t, (VM_FLAGS & STATISTICS) ? d_.on : d_.off);
    }
    else
    {
	Check_Atom(t);
	if (v.did == d_.on)
	{
	    VM_FLAGS |= STATISTICS;
	}
	else if (v.did == d_.off)
	{
	    VM_FLAGS &= ~STATISTICS;
	}
	else
	{
	    Bip_Error(RANGE_ERROR);
	}
	Succeed_;
    }
}

#endif /* PRINTAM */

/*
	is_predicate/1
	succeeds if this predicate is defined: predicate can be any
	predicate (prolog, builtin, external)
*/
static int
p_is_predicate(value val, type tag, value vm, type tm)
{
    dident  d;
    pri    *proc;
    int     err;

    Check_Module(tm, vm);
    Get_Proc_Did(val, tag, d);
    proc = visible_procedure(d, vm.did, tm, PRI_DONTIMPORT);
    if (!proc)
    {
	Get_Bip_Error(err);
	switch(err) {

	case IMPORT_PENDING:
	    Succeed_;		/* assume it's defined... */

	case NOENTRY:
	    Fail_;

	default:
	    Bip_Error(err);
	}
    }
    Succeed_If(proc->flags & CODE_DEFINED)
}


static int
p_module_predicates(value vwhich, type twhich, value v, type t, value vm, type tm)
{
    pri *pd;
    pword result;
    pword *list = &result;
    pword *pw;
    int which;

    Check_Atom(twhich);
    Check_Output_List(t);
    Check_Module(tm, vm);
    for(which=0;;)
    {
	if (vwhich.did == d_predlist_option[which])
	    break;
	if (++which >= PREDLIST_SIZE)
	    { Bip_Error(RANGE_ERROR); }
    }
    switch(which)
    {
    case PREDLIST_EXREEX:
    case PREDLIST_EXPORTED:
    case PREDLIST_REEXPORTED:
	break;
    default:
	Check_Module_Access(vm, tm);
	break;
    }
    a_mutex_lock(&ProcedureLock);
    a_mutex_lock(&ModuleLock);
    pd = ModuleItem(vm.did)->procedures;
    a_mutex_unlock(&ModuleLock);

    for (; pd; pd = pd->next_in_mod)
    {
	switch(which)
	{
	case PREDLIST_UNDECLARED:
	    if (PriScope(pd) == DEFAULT  &&  PriReferenced(pd))
		break;
	    continue;
	case PREDLIST_LOCAL:
	    if (PriScope(pd) == LOCAL && PriFlags(pd) & CODE_DEFINED)
		break;
	    continue;
	case PREDLIST_EXPORTED:
	    if (PriScope(pd) == EXPORT && PriFlags(pd) & (CODE_DEFINED|AUTOLOAD))
		break;
	    continue;
	case PREDLIST_REEXPORTED:
	    if (PriScope(pd) == IMPEXP && PriFlags(pd) & (CODE_DEFINED|AUTOLOAD))
		break;
	    continue;
	case PREDLIST_EXREEX:
	    if (PriAnyExp(pd) && PriFlags(pd) & (CODE_DEFINED|AUTOLOAD))
		break;
	    continue;
	case PREDLIST_DEFINED:
	    if ((PriScope(pd) == LOCAL  ||  PriScope(pd) == EXPORT)
		    && PriFlags(pd) & CODE_DEFINED)
		break;
	    continue;
	case PREDLIST_UNDEFINED:
	    if ((PriScope(pd) == LOCAL  ||  PriScope(pd) == EXPORT)
		    && !(PriFlags(pd) & CODE_DEFINED))
		break;
	    continue;
	case PREDLIST_NOMODULE:
	    /* find references (import/quali) to predicates
	     * whose home module does not exist (yet) */
	    if (PriIsProxy(pd)  &&  !IsModule(PriHomeModule(pd)))
		break;
	    continue;
	case PREDLIST_NOEXPORT:
	    /* find references (import/quali) to predicates
	     * that are not exported from their home module (yet) */
	    if (PriIsProxy(pd)  &&  IsModule(PriHomeModule(pd)))
	    {
		type module_tag;
		module_tag.kernel = ModuleTag(PriDid(pd));
		if (!visible_procedure(PriDid(pd), PriHomeModule(pd),
			module_tag, PRI_DONTIMPORT|PRI_EXPORTEDONLY))
		{
		    Set_Bip_Error(0);
		    break;
		}
	    }
	    continue;
	case PREDLIST_DEPRECATED:
	    if (PriIsProxy(pd)  &&  PriFlags(pd) & PROC_DEPRECATED)
		break;
	    continue;
	}
	Make_List(list, TG);
	list = TG;
	Push_List_Frame();
	Make_Struct(list, TG);
	++list;
	if (which == PREDLIST_NOMODULE || which == PREDLIST_NOEXPORT)
	{
	    /* build a qualified predspec HM:Pred because the problem
	     * is actually in the home module, not the lookup module */
	    pw = TG;
	    Push_Struct_Frame(d_.colon);
	    Make_Atom(&pw[1], PriHomeModule(pd));
	    Make_Struct(&pw[2], TG);
	}
	pw = TG;
	Push_Struct_Frame(d_.quotient);
	Make_Atom(&pw[1], add_dict(PriDid(pd), 0));
	Make_Integer(&pw[2], DidArity(PriDid(pd)));
    }
    Make_Nil(list);
    a_mutex_unlock(&ProcedureLock);
    Return_Unify_Pw(v, t, result.val, result.tag);
}


/*
 *	current_functor(?Name, ?Arity, +Option, +DictIndex) - backtrackable built-in
 *
 *	The last argument is used to Remember() the position in the dictionary.
 *	We either backtrack through the whole dictionary or, when the
 *	name is known, through the respective collision chain.
 *
 *	Option = 0	all functors
 *	Option = 1	functors with properties only
 *	Option = 2	functors with predicates only
 */
/*ARGSUSED*/
static int
p_current_functor(value valn, type tagn, value vala, type taga, value vopt, type topt, value valsn, type tagsn)
{
    dident functor, atom;
    value vnext;

    vnext.all = valsn.all;
    if (IsRef(tagn))	/* we have to backtrack through the whole dictionary */
    {
	while (next_functor((int *) &vnext.nint, &functor))
	{
	    if (vopt.nint == 1L && !DidProperties(functor))
		continue;
	    if (vopt.nint == 2L && !DidProc(functor))
		continue;

	    if (IsRef(taga))
	    {
		 Bind_Var(vala, taga, DidArity(functor), TINT);
	    }
	    else if (!(IsInteger(taga) && DidArity(functor) == vala.nint))
	    {
		continue;
	    }

	    atom = add_dict(functor, 0);
	    Bind_Var(valn, tagn, atom, (atom == d_.nil ? TNIL : TDICT));

	    Remember(4, vnext, tint);
	    Succeed_;
	}
    }
    else if (IsAtom(tagn)		/* name known, we can optimise this case */
	|| (IsNil(tagn) && (valn.did = d_.nil)))	/* I really mean '=' ! */
    {
	if (IsInteger(tagsn))	/* initial call */
	{
	    if (IsInteger(taga)) /* name and arity are known, just check */
	    {
		Cut_External;
		Succeed_If(check_did(valn.did, (int) vala.nint) != D_UNKNOWN)
	    }
	    else if (!IsRef(taga))
	    {
		Cut_External;
		Fail_;
	    }
	    functor = valn.did;	/* return the atom first */
	}
	else			/* find the next functor with this name */
	{
	    functor = (dident) DidNext(valsn.did);
	    while (functor != valn.did)
	    {
		if (DidString(functor) == DidString(valn.did)
		    && (vopt.nint == 0L
		     || vopt.nint == 1L && DidProperties(functor)
		     || vopt.nint == 2L && DidProc(functor)))
		    break;
		functor = (dident) DidNext(functor);
	    }
	    if (functor == valn.did)	/* wrapped around the chain, stop */
	    {
		Cut_External;
		Fail_;
	    }
	}
	/* return the arity of functor and remember functor */
	vnext.did = functor;
	Remember(4, vnext, tdict);
	/* IsRef(taga) */
	Bind_Var(vala, taga, DidArity(functor), TINT);
	Succeed_;
    }
    Cut_External;
    Fail_;
}


/****************************************************************
 * Dynamic definitions of external predicates
 * They rely on the "ec_getaddress" function (in bip_load.c)
 * ec_getaddress returns either the address of a C object or -1.
 ****************************************************************
 */

#if defined(HAVE_DLOPEN) || defined(HAVE_NLIST) || defined(_WIN32)

/*
 * external_(pred, function, module)
 * b_external_(pred,function,module)
 *
 * pred: atom or atom/arity
 * function: name of a C function ('_' added if needed)
 * module: source module
 */

static
_external(value vp, type tp, value vf, type tf, value vm, type tm, int nondet)
{
    char	*name;
    dident	wdid;
    word	c_address;
    uint32	new_flags;
    int		err;
    pri		*pd;

    Check_Module(tm, vm);
    Get_Name(vf, tf, name);		/* name of the c function */
    Error_If_Ref(tp);
    if (IsAtom(tp))
	wdid = vp.did;
    else
    {
	Get_Proc_Did(vp, tp, wdid);
    }

    c_address = ec_getaddress(name);
    if (!c_address)
    {
	Bip_Error(NOCODE)
    }
    pd = local_procedure(wdid, vm.did, tm, PRI_CREATE);
    if (!pd)
    {
	Get_Bip_Error(err);
	Bip_Error(err);
    }
    new_flags = VMCODE|ARGFIXEDWAM|EXTERN|(GlobalFlags & DBGCOMP ? DEBUG_DB : 0);
    err = pri_compatible_flags(pd, CODETYPE|ARGPASSING|EXTERN|DEBUG_DB, new_flags);
    if (err != PSUCCEED)
    {
	Bip_Error(err);
    }
    pri_change_flags(pd, CODETYPE|ARGPASSING|EXTERN|DEBUG_DB, new_flags);
    return b_built_code(pd, c_address, nondet);
}

static
p_external(value vp, type tp, value vf, type tf, value vm, type tm)
{
    return _external(vp, tp, vf, tf, vm, tm, 0);
}

static
p_b_external(value vp, type tp, value vf, type tf, value vm, type tm)
{
    return _external(vp, tp, vf, tf, vm, tm, 1);
}


static int
p_external_body(value vpred, type tpred, value vmod, type tmod)
{
    dident	wdid;
    pri		*pd;
    int		err;

    Check_Module(tmod, vmod);
    Get_Proc_Did(vpred, tpred, wdid);

    pd = visible_procedure(wdid, vmod.did, tmod, PRI_CREATE);
    if (!pd)
    {
	Get_Bip_Error(err);
	Bip_Error(err);
    }
    err = pri_compatible_flags(pd, CODETYPE|EXTERN, VMCODE|EXTERN);
    if (err != PSUCCEED)
    {
	Bip_Error(err);
    }
    pri_init_code(pd, VMCODE);
    pri_change_flags(pd, EXTERN, EXTERN);
    Succeed_;
}

#else
Not_Available_Built_In(p_external)
Not_Available_Built_In(p_b_external)
Not_Available_Built_In(p_external_body)
#endif


/* ********************************************************************
			DYNAMIC CODE
 * ******************************************************************* */


/* How to get the source-record pointer from the code or pri */
#define DynCodeSrcHandle(code) ((pword *)((code)[2]))
#define DynCodeSrcRecord(code) ((t_ext_ptr)ExternalData(DynCodeSrcHandle(code)))

static vmcode *
_init_dynamic1(pri *pd, t_ext_ptr source_record)
{
	vmcode		*code, *start;
	pword		*pw;
	pri_code_t	pricode;

	Allocate_Default_Procedure((long) (4/*code*/ + 4/*anchor*/), PriDid(pd));
	pw = (pword *)(code + 4);
	/* commented out 2008-04 -- does not seem to be needed
	  if ((uword)pw % sizeof(pword) != 0)
	  ec_panic("code block insufficiently aligned", "ec_make_dyn_proc()");
	*/
	start = code;

	Store_3(Call_dynamic, pd, pw)
	Store_i(Code_end)

	/* handle anchor for the source record */
	pw[0].val.ptr = (pword *) &heap_rec_header_tid;
	pw[0].tag.kernel = TEXTERN;
	pw[1].val.ptr = (pword *) source_record;
	pw[1].tag.kernel = TPTR;

	return start;
}


void
ec_free_dyn_code(vmcode *code)
{
    heap_rec_header_tid.free(DynCodeSrcRecord(code));
    reclaim_procedure(ProcHeader(code));
}


void
ec_mark_dids_dyn_code(vmcode *code)
{
    heap_rec_header_tid.mark_dids(DynCodeSrcRecord(code));
}



/*
	is_dynamic/2		non standard
	test whether a predicate (Name/Arity) is dynamic
*/
static int
p_is_dynamic(value v1, type t1, value v2, type t2, value vm, type tm)
{
    dident  wdid;
    pri    *procindex;
    int    err;

    Check_Module(tm, vm);
    Get_Did(v1, t1, v2, t2, wdid);
    if (wdid == D_UNKNOWN)
    {
	Fail_;
    }
    procindex = visible_procedure(wdid, vm.did, tm, PRI_DONTWARN);
    if (!procindex)
    {
	Get_Bip_Error(err);
	Bip_Error(err);
    }
    Succeed_If(DynamicProc(procindex));
}

/*
 *	is_built_in_/3		non standard
 *	test whether a predicate (Name/Arity) is a built_in
 */
static int
p_is_built_in(value val, type tag, value vm, type tm)
{
    dident  d;
    pri    *procindex;
    int     err;

    Check_Module(tm, vm);
    Get_Proc_Did(val, tag, d);
    procindex = visible_procedure(d, vm.did, tm, PRI_DONTWARN);
    if (!procindex)
    {
	Get_Bip_Error(err);
	Bip_Error(err);
    }
    Succeed_If(procindex->flags & SYSTEM);
}


/*
 * 	proc_flags(Name/Arity, Code, Value, Module, Private)
 * Return the corresponding property of the procedure so that it
 * can be processed in Prolog. System use only.
 */
static int
p_proc_flags(value vn, type tn, value vc, type tc, value vf, type tf, value vm, type tm, value vp, type tp)
{
    dident	wd;
    uint32	flags;
    pri		*proc;
    vmcode	*code;
    int		source;
    int		err;
    pword	*s;
    pword	result;
    type	tt;
    uword	brk_table_offset;
    uword	brk_filter = 0;
    Prepare_Requests;

#ifdef lint
    Check_Integer(tc);
#endif
    Check_Module(tm, vm);
    Get_Proc_Did(vn, tn, wd);
    tt.all = ModuleTag(vm.did);
    proc = visible_procedure(wd, vm.did, tt, PRI_DONTWARN);
    if (! proc)
    {
	Set_Bip_Error(0);
	Fail_;
    }
    flags = PriFlags(proc);
    if (PriScope(proc) == DEFAULT  &&  !PriReferenced(proc))
    {
	Set_Bip_Error(0);
	Fail_;
    }
    if (vc.nint == 7 || UnauthorizedAccess(vm.did, tm) && !PriExported(proc))
    {
	Request_Unify_Atom(vp, tp, d_.local0)
    } else {
	Request_Unify_Atom(vp, tp, d_.global0)
    }

    /* do we have information about the source? */
    code = PriCode(proc);
    source = 
	(!(proc->flags & EXTERN)
	 &&
	 !DynamicProc(proc)
	 &&
	 proc->flags & CODE_DEFINED
	 &&
	 ProcFid(code) != D_UNKNOWN);

    switch (vc.nint)
    {
    case 0:		/* definition module	*/
	if (proc->module_ref == D_UNKNOWN) {
	    Fail_;
	}
	Request_Unify_Atom(vf, tf, proc->module_ref);
	break;

    case 1:		/* PriFlags		*/
	Request_Unify_Integer(vf, tf, proc->flags);
	break;

    case 2:		/* statistics (obsolete)	*/
	Fail_;

    case 3:		/* source file		*/
	if (source) {
	    Request_Unify_Atom(vf, tf, (dident) ProcFid(code))
	}
	else {
	    Fail_;
	}
	break;

    case 4:		/* source line		*/
	/* line == 0 indicates no source line&offset information */
	if (source && ProcLid(code)) {
	    Request_Unify_Integer(vf, tf, ProcLid(code));
	}
	else {
	    Fail_;
	}
	break;

    case 5:		/* source offset	*/
	/* line == 0 indicates no source line&offset information */
	if (source && ProcLid(code)) {
	    Request_Unify_Integer(vf, tf, ProcBid(code));
	}
	else {
	    Fail_;
	}
	break;

    case 6:		/* mode			*/
	s = Gbl_Tg;
	if ((err = get_mode(PriMode(proc), wd)) < 0) {
	    Bip_Error(err);
	}
	Request_Unify_Pw(vf, tf, s->val, s->tag);
	break;

    case 7:		/* code start			*/
	Request_Unify_Integer(vf, tf, (word) code);
	break;

    case 8:		/* inlining predicate (goal macro)	*/
	if (!proc->trans_function) {
	    Fail_;
	}
	s = TG;
	Push_Struct_Frame(d_.quotient);
	Make_Atom(&s[1], add_dict(proc->trans_function, 0));
	Make_Integer(&s[2], DidArity(proc->trans_function));
	Request_Unify_Structure(vf, tf, s);
	break;

    case 9:		/* auxiliary */
	Request_Unify_Atom(vf, tf, flags & PROC_AUXILIARY? d_.on: d_.off);
    	break;
    case 10:		/* call_type */
	/* This flag should have more (and more appropriately named) values,
	 * taking into account both the setting of CODETYPE and ARGPASSING.
	 */
	Request_Unify_Atom(vf, tf, (flags & ARGPASSING) == ARGFIXEDWAM ? d_.prolog: d_.external);
    	break;
    case 11:		/* debugged */
	Request_Unify_Atom(vf, tf, flags & DEBUG_DB? d_.on: d_.off);
    	break;
    case 12:		/* declared */
	Request_Unify_Atom(vf, tf, PriScope(proc)!=DEFAULT? d_.on: d_.off);
    	break;
    case 13:		/* autoload */
	Request_Unify_Atom(vf, tf, flags & AUTOLOAD? d_.on: d_.off);
    	break;
    case 14:		/* defined */
	Request_Unify_Atom(vf, tf, flags & CODE_DEFINED? d_.on: d_.off);
    	break;
    case 15:		/* leash */
	Request_Unify_Atom(vf, tf, flags & DEBUG_TR? d_.stop: d_.notrace);
    	break;
    case 16:		/* deprecated */
	Request_Unify_Atom(vf, tf, flags & PROC_DEPRECATED? d_.on: d_.off);
    	break;
    case 17:		/* skip */
	Request_Unify_Atom(vf, tf, flags & DEBUG_SK? d_.on: d_.off);
    	break;
    case 18:		/* spy */
	Request_Unify_Atom(vf, tf, flags & DEBUG_SP? d_.on: d_.off);
    	break;
    case 19:		/* start_tracing */
	Request_Unify_Atom(vf, tf, flags & DEBUG_ST? d_.on: d_.off);
    	break;
    case 20:		/* stability */
	Request_Unify_Atom(vf, tf, flags & PROC_DYNAMIC? d_dynamic_: d_static_);
    	break;
    case 21:		/* tool */
	Request_Unify_Atom(vf, tf, flags & TOOL? d_.on: d_.off);
    	break;
    case 22:		/* type */
	Request_Unify_Atom(vf, tf, flags & SYSTEM? d_.built_in: d_.user);
    	break;
    case 23:		/* visibility */
	switch(PriScope(proc))
	{
	case LOCAL:	wd = d_.local0; break;
	case EXPORT:	wd = d_exported_; break;
	case IMPORT:	wd = d_imported_; break;
	case IMPEXP:	wd = d_reexported_; break;
	default:	Fail_;
	}
	Request_Unify_Atom(vf, tf, wd);
    	break;
    case 24:		/* priority */
	Request_Unify_Integer(vf, tf, PriPriority(proc));
    	break;
    case 25:		/* demon */
	Request_Unify_Atom(vf, tf, flags & PROC_DEMON? d_.on: d_.off);
    	break;
    case 26:		/* parallel */
	Request_Unify_Atom(vf, tf, flags & PROC_PARALLEL? d_.on: d_.off);
    	break;
    case 27:		/* invisible */
	if (!(flags & DEBUG_INVISIBLE)) {
	    Fail_;	/* show flag only if set */
	}
	Request_Unify_Atom(vf, tf, d_.on);
    	break;
    case 28:		/* code_type */
	Request_Unify_Atom(vf, tf, flags & EXTERN ? d_.external: d_.prolog);
    	break;

    case 29:		/* code_size */
	if (PriCodeType(proc) != VMCODE) {
	    Fail_;
	}
	Request_Unify_Integer(vf, tf, ProcCodeSize(code));
    	break;

    case 30:		/* break_lines */
    	brk_filter = BREAKPOINT;
	/* fall through */

    case 31:		/* port_lines */
	if (!(flags & DEBUG_DB) || PriCodeType(proc) != VMCODE) {
	    Fail_;
	}
	s = &result;
	brk_table_offset = ProcBrkTableOffset(code);
	if (brk_table_offset)
	{
	    for(code += brk_table_offset; *code; ++code)
	    {
		if (((*(vmcode**)code)[0] & brk_filter) == brk_filter)
		{
		    Make_List(s, TG);
		    s = TG;
		    Push_List_Frame();
		    Make_Struct(&s[0], TG);
		    Push_Struct_Frame(d_.colon);
		    /* this relies on the order of words from a break-port word as follows:
		       break-port word, file path (dident), line (int)
		    */ 
		    Make_Atom(&s[3], ((dident*)(*(vmcode**)code))[1]);	/* file */
		    Make_Integer(&s[4], (*(vmcode**)code)[2]);		/* line */
		    s = &s[1];
		}
	    }
	}
	Make_Nil(s);
	Request_Unify_Pw(vf, tf, result.val, result.tag);
    	break;

    case 32:		/* port_calls */
	if (!(flags & DEBUG_DB) || PriCodeType(proc) != VMCODE) {
	    Fail_;
	}
	s = &result;
	brk_table_offset = ProcBrkTableOffset(code);
	if (brk_table_offset)
	{
	    for(code += brk_table_offset; *code; ++code)
	    {
		if (((*(vmcode**)code)[0] & brk_filter) == brk_filter)
		{
		    Make_List(s, TG);
		    s = TG;
		    Push_List_Frame();
		    Make_Struct(&s[0], TG);
		    Push_Struct_Frame(d_.colon);
		    /* this relies on the order of words from a break-port word as follows:
		       Proc, break-port word
		    */
		    /* module:name/arity */
		    Make_Atom(&s[3], PriHomeModule((pri*)(*(vmcode**)code)[-1])); 
		    Make_Struct(&s[4], TG);
		    Push_Struct_Frame(d_.quotient);
		    Make_Atom(&s[6], add_dict(PriDid((pri*)(*(vmcode**)code)[-1]),0)); 
		    Make_Integer(&s[7], DidArity(PriDid((pri*)(*(vmcode**)code)[-1])));
		    s = &s[1];
		}
	    }
	}
	Make_Nil(s);
	Request_Unify_Pw(vf, tf, result.val, result.tag);
    	break;

    case 33:		/* trace_meta */
	Request_Unify_Atom(vf, tf, flags & DEBUG_TRMETA? d_.on: d_.off);
    	break;

    default:
	Bip_Error(RANGE_ERROR);
    }
    Return_Unify;
}

/*
 * FUNCTION NAME:	p_mode(pv, pt, mv, mt)
 *
 * PARAMETERS:	-the mode declaration in the form pred(+, -, ?, ++, ...)
 *		-module
 *
 * DESCRIPTION:		The Prolog built-in predicate mode_/2, body of the
 *			tool mode/1.
 *		
 */
static int
p_mode(value pv, type pt, value mv, type mt)
{
	int	arity, i, err, mode;
	long	mode_decl;
	pword	*arg, *term, *pred;
	pri	*proc;
	dident	wd;
	pword	pd;


	Check_Module(mt, mv);
	pd.val = pv;
	pd.tag = pt;
	pred = &pd;
	do
	{
	    Error_If_Ref(pred->tag);
	    if (IsStructure(pred->tag)) {
		pred = pred->val.ptr;
		wd = pred->val.did;
		pred++;
	    }
	    else if (IsList(pred->tag)) {
		wd = d_.list;
		pred = pred->val.ptr;
	    }
	    else {
		Bip_Error(TYPE_ERROR);
	    }
	    if (wd == d_.comma)
	    {
		term = pred;
		pred++;
		Dereference_(term);
		Dereference_(pred);
		Error_If_Ref(term->tag);
		if (IsStructure(term->tag)) {
		    term = term->val.ptr;
		    wd = term->val.did;
		    term++;
		}
		else if (IsList(term->tag)) {
		    wd = d_.list;
		    term = term->val.ptr;
		}
		else {
		    Bip_Error(TYPE_ERROR);
		}
	    }
	    else
	    {
		term = pred;
		pred = 0;
	    }
	    proc = local_procedure(wd, mv.did, mt, PRI_CREATE);
	    if (!proc)
	    {
		Get_Bip_Error(err);
		Bip_Error(err);
	    }
	    arity = DidArity(wd);
	    /* initialize with previous modes so that builtin bindings
	       are not erased */
	    mode_decl = PriMode(proc);
	    for (i = 1; i <= arity; i++)
	    {
		arg = term++;
		Dereference_(arg);
		Check_Atom(arg->tag);
		if (arg->val.did == d_.plus0)
		    mode = NONVAR;
		else if (arg->val.did == d_.plusplus)
		    mode = GROUND;
		else if (arg->val.did == d_.minus0)
		    mode = OUTPUT;
		else if (arg->val.did == d_.question)
		    mode = ANY;
		else if (arg->val.did == d_plusminus)
		    mode = NOALIAS_INST;
		else if (arg->val.did == d_minusplus)
		    mode = NOALIAS;
		else
		{
		    Bip_Error(TYPE_ERROR);
		}
		Set_Mode(i, mode_decl, mode);
	    }
	    err = pri_change_mode(proc, mode_decl);
	    if (err != PSUCCEED) { Bip_Error(err); }
	}
	while (pred);
	Succeed_;
}

/*
 *	bound_arg(F/A, Arg, Bound, Module)
 * Return the built-in binding property for the given argument, fail if
 * this argument is not changed by the builtin.
 */
static int
p_bound_arg(value vproc, type tproc, value va, type ta, value vb, type tb, value mv, type mt)
{
	pri	*proc;
	dident	wd;
	int	err;

	Check_Module(mt, mv);
	Check_Integer(ta);
	Check_Output_Atom(tb);
	Get_Proc_Did(vproc, tproc, wd);
	if (!(proc = visible_procedure(wd, mv.did, mt, 0)))
	{
	    Get_Bip_Error(err);
	    Bip_Error(err);
	}
	if (DidArity(wd) > 6) {
	    Fail_;
	}
	switch (ArgBinding(va.nint, proc->mode))
	{
	case ANY:
		Fail_;

	case CONSTANT:
		Return_Unify_Atom(vb, tb, d_constant);

	case NONVAR:
		Return_Unify_Atom(vb, tb, d_nonvar);

	case GROUND:
		Return_Unify_Atom(vb, tb, d_ground);
	}
	Fail_;
}


/*----------------------------------------------------------------------
 * Builtins related to macros
 *----------------------------------------------------------------------*/

static int
_macro_options(value vprop, type tprop, int *pmtype, int *pflag)
{
    *pmtype = TRANS_PROP;
    *pflag = 0;

    if (IsRef(tprop))			/* we need at least one	*/
    {
	Bip_Error(INSTANTIATION_FAULT);
    }
    else if (IsList(tprop))
    {
	pword		*pw;
	dident		arg;
	pword		*list = vprop.ptr;
	for(;;)			/* loop through the list	*/
	{
	    pw = list++;
	    Dereference_(pw);		/* get the list element	*/
	    Check_Atom(pw->tag);
	    arg = pw->val.did;
	    if (arg == d_.top_only)
		*pflag |= TR_TOP;
	    else if (arg == d_.protect_arg)
		*pflag |= TR_PROTECT;
	    else if (arg == d_.clause0) {
		*pflag |= TR_CLAUSE;
		*pmtype = CLAUSE_TRANS_PROP;
	    }
	    else if (arg == d_.term)
		;
	    else if (arg == d_.goal) {
		*pflag |= TR_GOAL;
		*pmtype = GOAL_TRANS_PROP;
	    }
	    else if (arg == d_.write)
		*pflag |= TR_WRITE;
	    else if (arg == d_.read)
		;
	    else if (arg == d_.global0)
		*pflag |= TR_GLOBAL;
	    else if (arg == d_.local0)
		;
	    else
	    {
		Bip_Error(RANGE_ERROR);
	    }
	    Dereference_(list);		/* get the list tail	*/
	    if (IsRef(list->tag))
	    {
		Bip_Error(INSTANTIATION_FAULT);
	    }
	    else if (IsList(list->tag))
		list = list->val.ptr;
	    else if (IsNil(list->tag))
		break;			/* end of the list	*/
	    else
	    {
		Bip_Error(TYPE_ERROR);
	    }
	}
    }
    else if (!IsNil(tprop))
    {
	Bip_Error(TYPE_ERROR);
    }
    if (*pflag & TR_WRITE)
    	*pmtype += 1;
    Succeed_;
}


/*
 * Define a goal macro for a procedure proc
 * - proc must be defined in m
 * - if not, a local proc is created in m
 * - the transformation trans will later be looked up in
 *   the definition module of proc
 */

static int
_define_goal_macro(dident proc_did, dident trans_did, value vm, type tm)
{
    pri *proc_pri;
    int err;

    /*
     * First look up the predicate proc in module m

     */
    proc_pri = local_procedure(proc_did, vm.did, tm, PRI_CREATE);
    if (!proc_pri)
    {
	Get_Bip_Error(err);
	Bip_Error(err);
    }

    /*
     * Setting to =/2 erases the goal macro
     */
    if (trans_did == d_.unify)
    	trans_did = D_UNKNOWN;

    /*
     * set the transformation fields in all descriptors
     */
    err = pri_change_trans_function(proc_pri, trans_did);
    if (err != PSUCCEED) { Bip_Error(err); }

    /* this is needed to force the compiler to call the transformations */
    if (trans_did != D_UNKNOWN)
	DidMacro(proc_pri->did) = 1;

    Succeed_;
}

static int
_erase_goal_macro(dident proc_did, value vm, type tm)
{
    pri *proc_pri;

    /*
     * First look up the predicate proc in module m
     */
    proc_pri = local_procedure(proc_did, vm.did, tm, 0);
    if (!proc_pri)
    {
	int err;
	Get_Bip_Error(err);
	Bip_Error(err);
    }

    /*
     * clear the transformation fields in all descriptors
     */
    return pri_change_trans_function(proc_pri, D_UNKNOWN);

    /* don't know whether we can clear the DidMacro flag */
}


static int
p_define_macro(value vproc, type tproc, value vtrans, type ttrans, value vprop, type tprop, value vmod, type tmod)
{
	dident			 dp, dt, lookup_module;
	int			 flag, mtype, err;
	pword			*list;
	macro_desc		*md;
	pword			*prop;

	Get_Macro_Did(vproc, tproc, dp)
	if (IsStructure(ttrans) && vtrans.ptr[0].val.did == d_.colon)
	{
	    pword *pw = &vtrans.ptr[1];
	    Dereference_(pw);
	    Check_Atom(pw->tag);
	    lookup_module = pw->val.did;
	    pw = &vtrans.ptr[2];
	    Dereference_(pw);
	    vtrans.all = pw->val.all;
	    ttrans.all = pw->tag.all;
	}
	else
	{
	    lookup_module = vmod.did;
	}
	Get_Proc_Did(vtrans, ttrans, dt)
	if (DidArity(dt) < 2 || DidArity(dt) > 5)	
	{
	    Bip_Error(RANGE_ERROR);
	} 
	err = _macro_options(vprop, tprop, &mtype, &flag);
	if (err != PSUCCEED)
	{
	    Bip_Error(err);
	} 

	/* multiple combinations not allowed */
	if ((flag & (TR_GOAL|TR_CLAUSE)) == (TR_GOAL|TR_CLAUSE)) {
	    Bip_Error(RANGE_ERROR);
	}
	/* write macros currently compatible with top_only, goal and protect */
	if ((flag & TR_WRITE) &&
	    (flag & ~(TR_GLOBAL|TR_TOP|TR_GOAL|TR_CLAUSE|TR_WRITE|TR_PROTECT)))
	{
	    Bip_Error(RANGE_ERROR);
	}
	if ((flag & (TR_GOAL|TR_WRITE)) == TR_GOAL)
	{
	    if (flag & TR_GLOBAL)
	    {
		Bip_Error(RANGE_ERROR);
	    }
	    /* goal macros are treated specially */
	    return _define_goal_macro(dp, dt, vmod, tmod);
	}
	else
	{
	    /* we define the source transformation */
	    prop = set_modular_property(dp, mtype,
		    vmod.did, tmod,
		    flag & TR_GLOBAL ? GLOBAL_PROP : LOCAL_PROP, &err);
	    if (prop == (pword *) NULL)
	    {
		if (err != PERROR)
		{
		    Bip_Error(err);
		}
		if (flag & TR_GLOBAL)
		{
		    Bip_Error(GLOBAL_TR_EXISTS);
		}
		else
		{
		    Bip_Error(TR_IN_MOD);
		}
	    }
	    DidMacro(dp) = 1;
	    md = (macro_desc *) hg_alloc(sizeof(macro_desc));
	    prop->tag.kernel = TPTR;
	    prop->val.ptr = (pword *) md;

	    md->trans_function = dt;
	    md->module = lookup_module;
	    md->flags = flag;
	}

	Succeed_;
}


static int
p_erase_macro (value vproc, type tproc, value vmod, type tmod)
{
	dident	dp;
	int	i;
	int	err1, err2 = NO_TR, rem = 1;

        Get_Macro_Did(vproc, tproc, dp);

	/* If all return PFAIL or PERROR, the macro bit can be cleared */
	for (i = TRANS_PROP; i <= WRITE_CLAUSE_TRANS_PROP; i++) {
	    err1 = erase_modular_property(dp, i, vmod.did, tmod, VISIBLE_PROP);
	    if (err1 == PSUCCEED) {
		err2 = PSUCCEED;
		rem = 0;
	    }
	    else if (err1 >= PERROR) {
		err2 = PSUCCEED;
	    }
	    else {
		Bip_Error(err1)
	    }
	}
	/* this is no longer possible because the DidMacro bit indicates also
	 * the presence of goal transformations in the procedure descriptors
	    if (rem)
		DidMacro(dp) = 0;
	 */
	Bip_Error(err2);
}

static int
p_erase_macro3(value vproc, type tproc, value vprop, type tprop, value vmod, type tmod)
{
	dident	wdid;
	int	propid, flag, err;

        Get_Macro_Did(vproc, tproc, wdid);
	err = _macro_options(vprop, tprop, &propid, &flag);
	if (err != PSUCCEED)
	{
	    Bip_Error(err);
	} 

	if ((flag & (TR_GOAL|TR_WRITE)) == TR_GOAL)
	{
	    return _erase_goal_macro(wdid, vmod, tmod);
	}
	else	/* erase the property */
	{
	    err = erase_modular_property(wdid, propid, vmod.did, tmod,
		    flag & TR_GLOBAL ? GLOBAL_PROP : LOCAL_PROP);
	    if (err < PERROR) {
		Bip_Error(err)
	    }
	    /* don't know whether we can clear the DidMacro flag here */
	}
	Succeed_;
}


static int
_type_did(pword *pw, dident *pd)
{
    int i;
    Dereference_(pw);
    Check_Atom_Or_Nil(pw->val, pw->tag);
    for (i=0; i<= NTYPES; i++)
    {
	if (i != TPTR && pw->val.did == tag_desc[i].type_name) {
	    *pd = TransfDid(i);
	    Succeed_;
	}
    }
    Bip_Error(RANGE_ERROR);
}

/* Check the arguments of current_macro_body/5
	illegal_macro(Functor, Pred, List, PredModule, Error)
 */
/*ARGSUSED*/
static int
p_illegal_macro(value v1, type t1, value v2, type t2, value v3, type t3, value v4, type t4, value v5, type t5)
{
/* 1 */
    if (IsStructure(t1) && v1.ptr->val.did == d_.quotient)
    {
	pword	*pw;

	pw = v1.ptr + 1;
	Dereference_(pw)
	if (!IsRef(pw->tag) && !IsAtom(pw->tag)) {
	    Return_Unify_Integer(v5, t5, -(TYPE_ERROR))
	}
	pw = v1.ptr + 2;
	Dereference_(pw)
	if (!IsRef(pw->tag) && !IsInteger(pw->tag)) {
	    Return_Unify_Integer(v5, t5, -(TYPE_ERROR))
	}
    }
    else if (IsStructure(t1) && v1.ptr->val.did == d_type_)
    {
	pword	*pw;

	pw = v1.ptr + 1;
	Dereference_(pw)
	if (!IsRef(pw->tag) && !IsAtom(pw->tag) && !IsNil(pw->tag)) {
	    Return_Unify_Integer(v5, t5, -(TYPE_ERROR))
	}
    }
    else if (!IsRef(t1)) {
	Return_Unify_Integer(v5, t5, -(TYPE_ERROR))
    }
/* 2 */
    if (IsStructure(t2) && v2.ptr->val.did == d_.quotient)
    {
	pword	*pw;

	pw = v2.ptr + 1;
	Dereference_(pw)
	if (!IsRef(pw->tag) && !IsAtom(pw->tag)) {
	    Return_Unify_Integer(v5, t5, -(TYPE_ERROR))
	}
	pw = v2.ptr + 2;
	Dereference_(pw)
	if (!IsRef(pw->tag) && !IsInteger(pw->tag)) {
	    Return_Unify_Integer(v5, t5, -(TYPE_ERROR))
	}
    }
    else if (!IsRef(t2)) {
	Return_Unify_Integer(v5, t5, -(TYPE_ERROR))
    }
/* 3 */
    if (!IsRef(t3) && !IsList(t3) && !IsNil(t3)) {
	Return_Unify_Integer(v5, t5, -(TYPE_ERROR))
    }
/* 4 */
    if (!IsRef(t4)) {
	if (!IsAtom(t4) || !IsModule(v4.did)) {
	    Return_Unify_Integer(v5, t5, -(MODULENAME))
	}
    }
    Fail_;
}


/*
 *  Macro lookup function, two variants:
 *
 *  is_macro(+Functor, -Pred, -OptionList, -PredModule, +Module, +Type)
 *	Functor is N/A or type(T), specifying which macro to look up.
 *
 *   visible_term_macro(+Term, -Pred, -OptionList, -PredModule, +Module, +Type)
 *	Term is arbitrary term, for which we try to find a macro.
 *
 *  Type is an integer specifying the property type, see property.h
 */

static int
_is_macro(dident wdid, value v2, type t2, value v3, type t3, value v4, type t4, value v5, type t5, value v6, type t6)
{
    pword	*pwd;
    pword	*p;
    macro_desc	*md;
    dident	trans_lookup_mod;
    pri		*proc;
    type	tmod;
    int		err;
    int		flags;
    Prepare_Requests;

    Check_Integer(t6);
    pwd = get_modular_property(wdid, v6.nint, v5.did, t5, VISIBLE_PROP, &err);
    if (!pwd) {
	if (err != PERROR) {
	    Bip_Error(err)
	}
	Fail_;
    }

    md = (macro_desc *) pwd->val.ptr;
    pwd = Gbl_Tg;
    Gbl_Tg += 3;
    Check_Gc;
    pwd[0].val.did = d_.quotient;
    pwd[0].tag.kernel = TDICT;
    pwd[1].val.did = add_dict(md->trans_function, 0);
    pwd[1].tag.kernel = TDICT;
    pwd[2].val.nint = DidArity(md->trans_function);
    pwd[2].tag.kernel = TINT;
    Request_Unify_Structure(v2, t2, pwd);

    /* find trans_function's definition module (needed for qualified call) */
    tmod.all = ModuleTag(md->module);
    proc = visible_procedure(md->trans_function, md->module, tmod, PRI_DONTWARN);
    if (!proc || PriScope(proc) == DEFAULT)
    {
	Set_Bip_Error(0);
	Request_Unify_Atom(v4, t4, md->module);
    }
    else
    {
	Request_Unify_Atom(v4, t4, proc->module_ref);
    }

    /* build an option list from the flags */
    flags = md->flags;
    pwd = Gbl_Tg;

    p = Gbl_Tg;
    Gbl_Tg += 2;
    p[0].val.did = flags & TR_GLOBAL ? d_.global0 : d_.local0;
    p[0].tag.kernel = TDICT;
    p[1].val.ptr = Gbl_Tg;
    p[1].tag.kernel = TLIST;

    if (flags & TR_PROTECT) {
	p = Gbl_Tg;
	Gbl_Tg += 2;
	p[0].val.did = d_.protect_arg;
	p[0].tag.kernel = TDICT;
	p[1].val.ptr = Gbl_Tg;
	p[1].tag.kernel = TLIST;
    }
    if (flags & TR_TOP) {
	p = Gbl_Tg;
	Gbl_Tg += 2;
	p[0].val.did = d_.top_only;
	p[0].tag.kernel = TDICT;
	p[1].val.ptr = Gbl_Tg;
	p[1].tag.kernel = TLIST;
    }
    if (flags & TR_WRITE) {
	p = Gbl_Tg;
	Gbl_Tg += 2;
	p[0].val.did = d_.write;
	p[0].tag.kernel = TDICT;
	p[1].val.ptr = Gbl_Tg;
	p[1].tag.kernel = TLIST;
    }
    if (flags & TR_CLAUSE) {
	p = Gbl_Tg;
	Gbl_Tg += 2;
	p[0].val.did = d_.clause0;
	p[0].tag.kernel = TDICT;
	p[1].val.ptr = Gbl_Tg;
	p[1].tag.kernel = TLIST;
    }
    if (flags & TR_GOAL) {
	p = Gbl_Tg;
	Gbl_Tg += 2;
	p[0].val.did = d_.goal;
	p[0].tag.kernel = TDICT;
	p[1].val.ptr = Gbl_Tg;
	p[1].tag.kernel = TLIST;
    }
    p[1].tag.kernel = TNIL;
    Check_Gc;
    Request_Unify_List(v3, t3, pwd);
    Return_Unify;
}

/*ARGSUSED*/
static int
p_is_macro(value v1, type t1, value v2, type t2, value v3, type t3, value v4, type t4, value v5, type t5, value v6, type t6)
{
    dident	wdid;
    Get_Macro_Did(v1, t1, wdid);
    return _is_macro(wdid, v2, t2, v3, t3, v4, t4, v5, t5, v6, t6);
}

static int
p_visible_term_macro(value v1, type t1, value v2, type t2, value v3, type t3, value v4, type t4, value v5, type t5, value v6, type t6)
{
    int		res;
    dident	wdid;

    /* first look for a functor-specific macro */
    switch (TagType(t1)) {
    case TDICT:	wdid = v1.did; break;
    case TNIL:	wdid = d_.nil; break;
    case TLIST:	wdid = d_.list; break;
    case TCOMP:	wdid = v1.ptr->val.did; break;
    default:	wdid = D_UNKNOWN;
    }
    if (wdid != D_UNKNOWN)
    {
	res = _is_macro(wdid, v2, t2, v3, t3, v4, t4, v5, t5, v6, t6);
	if (res != PFAIL)
	    return res;		/* PSUCCEED or error */
    }

    /* if none, look for a type macro */
    return _is_macro(TransfDid(t1.kernel), v2, t2, v3, t3, v4, t4, v5, t5, v6, t6);
}


/*
 * visible_goal_macro(+Goal, -TransPred, -TransLookupMod, +LookupMod)
 *
 * Lookup a goal macro (inine transformation) for Goal. If there is none, fail.
 */

static int
p_visible_goal_macro(value vgoal, type tgoal, value vtrans, type ttrans, value vtlm, type ttlm, value vlm, type tlm)
{

    dident proc_did;
    pri *proc_pri;
    pword *pw;
    Prepare_Requests;

    switch (TagType(tgoal)) {
    case TDICT:	proc_did = vgoal.did; break;
    case TNIL:	proc_did = d_.nil; break;
    case TLIST:	proc_did = d_.list; break;
    case TCOMP:	proc_did = vgoal.ptr->val.did; break;
    default:	Fail_;
    }

    /*
     * Check whether there is a visible procedure with a transformation.
     */
    if (!DidMacro(proc_did) || !IsAtom(tlm) || !IsModule(vlm.did) /*this can happen!*/) {
    	Fail_;
    }
    proc_pri = visible_procedure(proc_did, vlm.did, tlm, 0);
    if (!proc_pri) {
	Set_Bip_Error(0); /* reset error code from visible_procedure() */
	Fail_;
    }
    if (!proc_pri->trans_function) {
	Fail_;
    }

    /*
     * We treat the transformation like a call to the predicate itself.
     * That may help to detect errors due to later redefinition.
     */
    Pri_Set_Reference(proc_pri);

    /*
     * Return transformation functor and lookup module
     */
    pw = TG;
    Push_Struct_Frame(d_.quotient);
    Make_Atom(&pw[1], add_dict(proc_pri->trans_function, 0));
    Make_Integer(&pw[2], DidArity(proc_pri->trans_function));
    Request_Unify_Structure(vtrans, ttrans, pw);
    Request_Unify_Atom(vtlm, ttlm, proc_pri->module_ref);
    Return_Unify;
}


/* The following builtins use the global error variable ! */
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)



/*
 * dynamic_create_(+Name, +Arity, +SrcHandle, +Module)
 * create a dynamic predicate Name/Arity, whose source is stored in SrcHandle
 * fails on error with bip_error
 */

static int
p_dynamic_create(value v1, type t1, value v2, type t2, value vm, type tm)
{
    dident	wdid;
    pri		*proc;
    int		ndebug;				/* current dbg mode */
    int		err;
    pri_code_t	pricode;
    extern t_ext_ptr	ec_record_create(void);

    Check_Module(tm, vm);
    Add_Did(v1, t1, v2, t2, wdid);
    ndebug = (GlobalFlags & DBGCOMP) ? 0 : DEBUG_DB;

    a_mutex_lock(&ProcedureLock);
    proc = local_procedure(wdid, vm.did, tm, PRI_CREATE);
    if (!proc)
    {
	a_mutex_unlock(&ProcedureLock);
	Get_Bip_Error(err);
	Bip_Error(err);
    }
    /* we redefine a procedure defined in the module		*/
    if (DynamicProc(proc))
    {
	a_mutex_unlock(&ProcedureLock);
	Bip_Error(ALREADY_DYNAMIC);
    }
    if (proc->flags & CODE_DEFINED)
    {
	a_mutex_unlock(&ProcedureLock);
	Bip_Error(ALREADY_DEFINED);
    }
    err = pri_compatible_flags(proc, ARGPASSING|PROC_DYNAMIC|EXTERN|TOOL|PROC_PARALLEL|DEBUG_DB, ARGFIXEDWAM|PROC_DYNAMIC|ndebug);
    if (err != PSUCCEED)
    {
	a_mutex_unlock(&ProcedureLock);
	Bip_Error(err);
    }
    pri_change_flags(proc, ARGPASSING|PROC_DYNAMIC|EXTERN|TOOL|PROC_PARALLEL|DEBUG_DB, ARGFIXEDWAM|PROC_DYNAMIC|ndebug);
    pricode.vmc = _init_dynamic1(proc, ec_record_create());
    pri_define_code(proc, VMCODE, pricode);
    a_mutex_unlock(&ProcedureLock);
    Succeed_;
}


/*
 * dynamic_source_(+Name, +Arity, -SrcHandle, +Module)
 * retrieve the record handle under which the source is stored
 */

static int
p_dynamic_source(value v1, type t1, value v2, type t2, value vsrc, type tsrc, value vm, type tm)
{
    dident	wdid;
    pri		*proc;
    pword	ref_pw;

    Check_Module(tm, vm);
    Add_Did(v1, t1, v2, t2, wdid);
    proc = visible_procedure(wdid, vm.did, tm, 0);
    if (!proc) {
	int err;
	Get_Bip_Error(err);
	if (err == NOENTRY)
	    err = ACCESSING_UNDEF_DYN_PROC;
	Bip_Error(err);
    }
    if (PriScope(proc) != DEFAULT && PriModule(proc) != PriHomeModule(proc))
    {
	Bip_Error(ACCESSING_NON_LOCAL);
    }
    if (!DynamicProc(proc))
    {
	if (PriFlags(proc) & CODE_DEFINED)
	{
	    Bip_Error(NOT_DYNAMIC);
	}
	else
	{
	    Bip_Error(ACCESSING_UNDEF_DYN_PROC);
	}
    }

    /* Create a THANDLE pointer to the anchor inside the code block
     * (taken from the 2nd * parameter of the [Call_dynamic proc handle]
     * instruction).  This is only legal if it is guaranteed that the
     * pointer does not live longer than the code block (otherwise we
     * have to use ec_handle() to create a global stack anchor.
     */
    
    ref_pw.val.ptr = DynCodeSrcHandle(PriCode(proc));
    ref_pw.tag.kernel = THANDLE;
    Return_Unify_Pw(vsrc, tsrc, ref_pw.val, ref_pw.tag);
}


/*
	abolish_(Name, Arity, Module)
	Remove a predicate from the procedure table if the predicate
	is at least declared.
	Error checking MUST already have been done (with p_check_abolish).
	Reports error INCONSISTENCY by failing (use get_bip_error()).
*/
/*ARGSUSED*/
static int
p_abolish(value n, type tn, value a, type ta, value vm, type tm)
{
    dident	d;
    pri		*proc, *global;
    int		err;

    Check_Integer(ta);
    Check_Atom(tn);
    Check_Module(tm, vm);
    if(a.nint < 0)
    {
	Bip_Error(RANGE_ERROR);
    }
    d = check_did(n.did, (int) a.nint);
    if (d == D_UNKNOWN)
    {
	Bip_Error(NOENTRY);
    }
    a_mutex_lock(&ProcedureLock);
    proc = local_procedure(d, vm.did, tm, 0);
    if (!proc)
    {
	a_mutex_unlock(&ProcedureLock);
	Get_Bip_Error(err);
	Bip_Error(err);
    }
    pri_abolish(proc);
    a_mutex_unlock(&ProcedureLock);
    Succeed_;
}

/*
 * set_proc_flags(Name/Arity, Flag, Value, Module)
 *	set the specified flag of the procedure
 *	fail when error (get_bip_error/1 may then returns NOENTRY if
 *	functor/arity is not a defined procedure or LOCKED if
 *	module is locked, RANGE_ERROR if wrong flags or flags value.
 *	Type checking is made on the modules and flags.
 */
static int
p_set_proc_flags(value vproc, type tproc, value vf, type tf, value vv, type tv, value vm, type tm)
{
	uint32	new_flags = 0, changed_flags = 0;
	dident	wdid;
	pri	*proc;
	int	err, use_local_procedure = 0;

	Check_Module(tm, vm);
	Get_Proc_Did(vproc, tproc, wdid);
	Check_Atom(tf);

	if (vf.did == d_.leash)
	{
	    Check_Atom(tv);
	    changed_flags = DEBUG_TR;
	    if (vv.did == d_.stop)
		new_flags = DEBUG_TR;
	    else if (vv.did == d_.print)
		new_flags = DEBUG_TR;
	    else if (vv.did == d_.notrace)
		new_flags = 0;
	    else
	    {
		Bip_Error(RANGE_ERROR);
	    }
	}
	else if (vf.did == d_.priority)
	{
	    Check_Integer(tv);
	    if (vv.nint < 1 || vv.nint > SUSP_MAX_PRIO)
	    {
		Bip_Error(RANGE_ERROR);
	    }
	    changed_flags = PROC_PRIORITY;
	    new_flags = PriPriorityFlags(vv.nint);
	}
	else if (vf.did == d_.spy)
	{
	    Check_Atom(tv);
	    if (vv.did == d_.on) {
		changed_flags = new_flags = DEBUG_SP|DEBUG_TR;
	    } else if (vv.did == d_.off) {
		changed_flags = DEBUG_SP;
		new_flags = 0;
	    } else {
		Bip_Error(RANGE_ERROR);
	    }
	}
	else if (vf.did == d_type0_)	/* set the system-flag */
	{
	    Check_Atom(tv)
	    if (vv.did != d_.built_in) {
		Bip_Error(RANGE_ERROR);
	    }
	    use_local_procedure = 1;
	    changed_flags = new_flags = SYSTEM;
	}
	else if (vf.did == d_source_file_)
	{
	    Check_Atom(tv)
	    use_local_procedure = 1;
	}
	else if (vf.did == d_source_line_ || vf.did == d_source_offset_)
	{
	    Check_Integer(tv)
	    if (vv.nint < 0)
	    {
		Bip_Error(RANGE_ERROR);
	    }
	    use_local_procedure = 1;
	}
	else if (vf.did == d_.break0)
	{
	    Check_Integer(tv);
	    if (vv.nint < 0)
	    {
		Bip_Error(RANGE_ERROR);
	    }
	}
	else
	{
	    /*
	     * all the others are simple on/off flags
	     */
	    Check_Atom(tv);
	    if (vf.did == d_.skip) {
		changed_flags = DEBUG_SK;
	    } else if (vf.did == d_start_tracing_) {
		changed_flags = DEBUG_ST;
	    } else if (vf.did == d_.system) {
		changed_flags = SYSTEM;
		use_local_procedure = 1;
	    } else if (vf.did == d_invisible_) {
		changed_flags = DEBUG_INVISIBLE;
		use_local_procedure = 1;
	    } else if (vf.did == d_.debug) {
		changed_flags = DEBUG_DB;
		use_local_procedure = 1;
	    } else if (vf.did == d_trace_meta_) {
		changed_flags = DEBUG_TRMETA;
		use_local_procedure = 1;
	    } else if (vf.did == d_autoload_) {
		changed_flags = AUTOLOAD;
		use_local_procedure = 1;
	    } else if (vf.did == d_auxiliary_) {
		changed_flags = PROC_AUXILIARY;
		use_local_procedure = 1;
	    } else if (vf.did == d_parallel_) {
		changed_flags = PROC_PARALLEL;
		use_local_procedure = 1;
	    } else if (vf.did == d_demon_) {
		changed_flags = PROC_DEMON;
		use_local_procedure = 1;
	    } else if (vf.did == d_deprecated_) {
		changed_flags = PROC_DEPRECATED;
		use_local_procedure = 1;
	    }
	    else
	    {
		Bip_Error(RANGE_ERROR);
	    }
	    if (vv.did == d_.on)
		new_flags = changed_flags;
	    else if (vv.did == d_.off)
		new_flags = 0;
	    else
	    {
		Bip_Error(RANGE_ERROR);
	    }
	}

	a_mutex_lock(&ProcedureLock);
	proc = visible_procedure(wdid, vm.did, tm, 0);
	if (!proc)
	{
	    Get_Bip_Error(err);
	    goto _unlock_return_err_;
	}

	if (proc->module_ref != vm.did)
	{
	    /* Some flags should only be changeable from the
	     * procedure's definition module */
	    if (use_local_procedure)
	    {
		err = ACCESSING_NON_LOCAL;
		goto _unlock_return_err_;
	    }
	    /* Try to get the definition module descriptor */
	    proc = pri_home(proc);
	    if (!proc)
	    {
		Get_Bip_Error(err);
		goto _unlock_return_err_;
	    }
	}

	if (changed_flags)
	{
	    /*
	     * Some additional restrictions on flag changes
	     */
	    if (DynamicProc(proc) && (new_flags & PROC_PARALLEL))
	    {
		err = ALREADY_DYNAMIC;
		goto _unlock_return_err_;
	    }
	    /* disallow clearing skip-flag in locked modules */
	    if ((DEBUG_SK & PriFlags(proc) & changed_flags & ~new_flags)
		&& IsLocked(proc->module_def)
		&& (proc->module_def != vm.did || !IsModuleTag(vm.did,tm)))
	    {
		err = LOCKED;
		goto _unlock_return_err_;
	    }
	    err = pri_compatible_flags(proc, changed_flags, new_flags);
	    if (err != PSUCCEED)
		goto _unlock_return_err_;

	    pri_change_flags(proc, changed_flags, new_flags);
	}
	else /* changing information stored in code header or breakport */
	{
	    if (!(PriFlags(proc) & CODE_DEFINED))
	    {
		err = NOENTRY;
		goto _unlock_return_err_;
	    }
	    if (vf.did == d_source_file_)
	    {
		ProcFid(PriCode(proc)) = vv.did;
	    }
	    else if (vf.did == d_source_line_)
	    {
		ProcLid(PriCode(proc)) = vv.nint;
	    }
	    else if (vf.did == d_source_offset_)
	    {
		ProcBid(PriCode(proc)) = vv.nint;
	    }
	    else if (vf.did == d_.break0)
	    {/* toggle the breakpoint flag of the port word in a debug_scall, pointed to by
                the port table */
		vmcode * code;
		uword offset;

		code = PriCode(proc);
		offset = ProcBrkTableOffset(code);
		if (offset == 0)
		{
		    err = RANGE_ERROR;
		    goto _unlock_return_err_;
		}
		code += ProcBrkTableOffset(code);
		while (*code != 0)
		{
		    /* this relies on the order of words from a break-port word as follows:
		       break-port word, file path (dident), line (int)
		    */ 
		    if (*(((vmcode *)(*code))+2)/* breakport line */ == vv.nint)
		    {
			**((vmcode **)code) ^= BREAKPOINT;
			break;
		    }
		    code++;
		}
		if (*code == 0) /* no match found */
		{
		    err = RANGE_ERROR;
		    goto _unlock_return_err_;
		}
	    }
	}
	a_mutex_unlock(&ProcedureLock);
	Succeed_;

_unlock_return_err_:
	a_mutex_unlock(&ProcedureLock);
	Set_Bip_Error(err);
	Fail_;
}

#undef Bip_Error
#define Bip_Error(err)	return(err);

/*
 * store_pred(+PredSpec, +CodeListOrArray, +Size, +BTablePos, +FlagBits, +File, +Line, +Offset, +Module)
 *
 * Create the predicate PredSpec with the VM-code specified in CodeList.
 * Size is the code size in units of vmcode. BTable is the offset to the start of the 
 * port/break table, which are addresses to the port words in the predicate for setting
 * breakpoints (=0 if no table). File, Line and Offset gives source information:
 * the source file path (atom), the first line for the predicate, and the offset in
 * bytes to the predicate. These should all be set to 0 if there is no source info 
 */


#define Store_Ref(pw1, base)  			\
   if (IsInteger(pw1->tag))  			\
   {  						\
     Store_d(base + pw1->val.nint)		\
   }						\
   else						\
   {						\
     Check_Atom(pw1->tag);			\
     if (pw1->val.did == d_.fail)		\
     {						\
       Store_d(&fail_code_[0]);			\
     }						\
     else if (pw1->val.did == d_par_fail)       \
     {						\
       Store_d(&par_fail_code_[0]);    		\
     }						\
     else					\
     {						\
       Bip_Error(RANGE_ERROR);			\
     }						\
   }


#ifdef DONT_USE_GROUND_CONSTANT_TABLE
/* auxiliary function to give all DIDs in a ground term the stability setting */

static int
_set_did_stability(
    	value v, type t,	/* expects a dereferenced argument */
	int stability)
{
    int arity;
    pword *arg_i;

    for (;;)
    {
	if (IsRef(t))
	    return INSTANTIATION_FAULT;
	else if (IsAtom(t))
	{
	    Set_Did_Stability(v.did, stability);
	    return PSUCCEED;
	}
	else if (IsString(t) && StringInDictionary(v))
	{
	    dident a = check_did_n(StringStart(v), StringLength(v), 0);
	    if (a != D_UNKNOWN)
	    {
		Set_Did_Stability(a, stability);
	    }
	    else
	    {
		Print_Err("No atom corresponding to persistent string");
	    }
	    return PSUCCEED;
	}
	else if (IsList(t))
	    arity = 2;
	else if (IsStructure(t))
	{
	    Set_Did_Stability(v.ptr->val.did, stability);
	    arity = DidArity(v.ptr->val.did);
	    v.ptr++;
	}
	else
	    return PSUCCEED;
 
	for(;arity > 1; arity--)
	{
	    int res;
	    arg_i = v.ptr++;
	    Dereference_(arg_i);
	    res = _set_did_stability(arg_i->val, arg_i->tag, stability);
	    if (res != PSUCCEED)
	    	return res;
	}
	arg_i = v.ptr;		/* tail recursion */
	Dereference_(arg_i);
	v.all = arg_i->val.all;
	t.all = arg_i->tag.all;
    }
}
#endif


static int
p_store_pred(value vproc, type tproc, value vcode, type tcode, value vsize, type tsize, value vbrktable, type tbrktable, value vflags, type tflags, value vfid, type tfid, value vlid, type tlid, value vbid, type tbid, value vm, type tm)
{
    dident		wdid;
    register pword	*codeptr, *pw1;
    vmcode		*base, *code, *top;
    uint32		flags;
    int			err;
    pri			*proc;
    pri_code_t		pricode;
    word		codetype, codelen;

    Check_Integer(tsize);
    Check_Integer(tbrktable);
    Error_If_Ref(tcode);
    if (IsList(tcode)) {
	codetype = TLIST;
	codeptr = vcode.ptr;
    } else if (IsStructure(tcode)) {
	codetype = TCOMP;
	codelen = DidArity(vcode.ptr->val.did);
	codeptr = vcode.ptr + 1;
    } else {
	Bip_Error(TYPE_ERROR);
    }
    Check_Module(tm, vm);
    /*
    Check_Module_And_Access(vm, tm);
    */
    Get_Proc_Did(vproc, tproc, wdid);
    Check_Integer(tflags);

    if (IsInteger(tfid)) {
	/* fid set to 0 if there is no source information */
	Allocate_Default_ProcedureBTable(vsize.nint, wdid, vbrktable.nint);
    } else {
	Check_Atom(tfid);
	Check_Integer(tlid);
	Check_Integer(tbid);
	code = AllocateCodeBlockBTable(vsize.nint, vbrktable.nint, 0L, vbid.nint, vfid.did, vlid.nint, Cid(-1L, wdid));
	Set_Did_Stability(vfid.did, DICT_CODE_REF);
    }

    /*
     * Traverse the code list, convert the elements and store them away
     */

    base = code;
    top = base + vsize.nint;

    for(;;)			/* loop through the code list/array	*/
    {
        if (code > top) 
        {
	  Bip_Error(RANGE_ERROR);
        }

	pw1 = codeptr++;
	Dereference_(pw1);		/* get the list element	*/
	if (IsRef(pw1->tag))		/* check it		*/
	{
	    Bip_Error(INSTANTIATION_FAULT);
	}
	else if (IsSimple(pw1->tag))	/* atom, integer, float: store value */
	{
	    if (IsAtom(pw1->tag))
		{ Set_Did_Stability(pw1->val.did, DICT_CODE_REF); }
	    Store_d(pw1->val.nint)
	}
	else if (IsString(pw1->tag))	/* string: store pointer to heap copy */
	{
	    value heap_string;
	    heap_string.ptr = enter_string_n(StringStart(pw1->val),
				StringLength(pw1->val), DICT_CODE_REF);
	    Store_d(heap_string.nint)
	}
	else if (IsStructure(pw1->tag))
	{
	    dident d;

	    pw1 = pw1->val.ptr;
	    d = pw1++->val.did;
	    Dereference_(pw1);
	    
	    if (d == d_opc1)		/* o(N) */
	    {
		Check_Integer(pw1->tag);
		Store_i(pw1->val.nint)
	    }
	    else if (d == d_a1)		/* a(N) */
	    {
		Check_Integer(pw1->tag);
		Store_d(Address(pw1->val.nint))
	    }
	    else if (d == d_t1 || d == d_pw1) /* t/pw(N) */
	    {
		Check_Integer(pw1->tag);
		Store_d(Esize(pw1->val.nint))
	    }
	    else if (d == d_y1)		/* y(N) */
	    {
		{
		    Check_Integer(pw1->tag);
		    Store_d(Esize(pw1->val.nint))
		}
	    }
	    else if (d == d_ymask)	/* ymask(IntList) */
	    {
		word i, firsti;
		uword mask = 0;
		pword *elem;
		Check_List(pw1->tag);	/* require ordered list of integers */
		pw1 = pw1->val.ptr;
		elem = pw1++;
		Dereference_(elem);
		Check_Integer(elem->tag);
		firsti = elem->val.nint;
		Dereference_(pw1);
		while (IsList(pw1->tag))
		{
		    pw1 = pw1->val.ptr;
		    elem = pw1++;
		    Dereference_(elem);
		    Check_Integer(elem->tag);
		    i = elem->val.nint;
		    /* 32 is the maximum number of extra consecutive slots
		     * that can be initialised with a single instruction.
		     * (the first slot's bit is implicit)
		     */
		    if (i <= firsti || i > firsti+32)
		    {
			Bip_Error(RANGE_ERROR);
		    }
		    /* make sure 1 is of the right length */
		    mask |= ((uword) 1) << (i-firsti-1);
		    Dereference_(pw1);
		}
		Check_Nil(pw1->tag);
		Store_d(mask);		/* store the init-mask */
	    }
	    else if (d == d_w1)		/* w(N) */
	    {
		Check_Integer(pw1->tag);
		Store_d(pw1->val.nint * sizeof(word))
	    }
	    else if (d == d_nv1)	/* nv(Word) */
	    {
		Check_Atom(pw1->tag);
		Set_Did_Stability(pw1->val.did, DICT_CODE_REF);
		Store_d(DidTag(TNAME, pw1->val.did));
	    }
	    else if (d == d_mv1)	/* mv(Word) */
	    {
		Check_Atom(pw1->tag);
		Set_Did_Stability(pw1->val.did, DICT_CODE_REF);
		Store_d(DidTag(TMETA, pw1->val.did));
	    }
	    else if (d == d_an1)        /* an(Atom) */
	    {
	        word i;
	        Check_Atom(pw1->tag);
	        i = (word) meta_index(pw1->val.did);
		Store_d(Esize(i));
	    }
#ifdef DONT_USE_GROUND_CONSTANT_TABLE
	    else if (d == d_tag1)	/* tag(GroundTerm) */

	    {
		if (IsAtom(pw1->tag)  &&  pw1->val.did == vm.did) {
		    Store_d(ModuleTag(pw1->val.did));
		} else {
		    Store_d(pw1->tag.all);
		}
	    }
	    else if (d == d_val1)	/* val(GroundTerm) */
	    {
		int res;
		pword ground_copy;
		res = _set_did_stability(pw1->val, pw1->tag, DICT_CODE_REF);
		if (res != PSUCCEED) { Bip_Error(res); }
		res = create_heapterm(&ground_copy, pw1->val, pw1->tag);
		if (res != PSUCCEED) { Bip_Error(res); }
		Store_d(ground_copy.val.all);
	    }
#else
	    else if (d == d_tag1)	/* tag(GroundTerm) */
	    {
		pword ground_copy;
		err = ec_constant_table_enter(pw1->val, pw1->tag, &ground_copy);
		if (err == PSUCCEED) {
		    if (IsAtom(ground_copy.tag)  &&  ground_copy.val.did == vm.did) {
			Store_d(ModuleTag(ground_copy.val.did));
		    } else {
			Store_d(ground_copy.tag.all);
		    }
		} else if (err == PFAIL) {
		    Store_d(pw1->tag.all);
		} else {
		    Bip_Error(err)
		} 
	    }
	    else if (d == d_val1)	/* val(GroundTerm) */
	    {
		pword ground_copy;
		err = ec_constant_table_enter(pw1->val, pw1->tag, &ground_copy);
		if (err == PSUCCEED) {
		    Store_d(ground_copy.val.all);
		} else if (err == PFAIL) {
		    int res = create_heapterm(&ground_copy, pw1->val, pw1->tag);
		    if (res != PSUCCEED) { Bip_Error(res); }
		    Store_d(ground_copy.val.all);
		} else {
		    Bip_Error(err)
		}
	    }
#endif
	    else if (d == d_proc1)	/* proc(N/A) or proc(M:N/A) */
	    {
		dident pdid;
		if (IsStructure(pw1->tag) && pw1->val.ptr[0].val.did == d_.colon)
		{
		    pword *pmod, *pproc;
		    pmod = &pw1->val.ptr[1];
		    pproc = &pw1->val.ptr[2];
		    Dereference_(pmod);
		    Check_Atom(pmod->tag);
		    Dereference_(pproc);
		    Get_Proc_Did(pproc->val, pproc->tag, pdid);
		    Store_d(qualified_procedure(pdid, pmod->val.did, vm.did, tm));
		}
		else
		{
		    Get_Proc_Did(pw1->val, pw1->tag, pdid);
		    Store_d(visible_procedure(pdid, vm.did, tm, PRI_CREATE|PRI_REFER));
		}
	    }
	    else if (d == d_functor1)	/* functor(N/A) */
	    {
		dident pdid;
		Get_Functor_Did(pw1->val, pw1->tag, pdid);
		Set_Did_Stability(pdid, DICT_CODE_REF);
		Store_d(pdid);
	    }
	    else if (d == d_ref1)	/* ref(atom or displacement) */
	    {
	        Store_Ref(pw1, base);
	    }
	    else if (d == d_refm)	/* refm(displacement,marker) */
	    {
		/* Temporary hack to create pointers with one of their
		 * low bits set for marking purposes. */
		Store_d((word)(base + pw1[0].val.nint) + pw1[1].val.nint)
	    }
	    else if (d == d_align)	/* align(multiple of words) */
	    {
		int i;
		if (pw1->val.nint < 1 || pw1->val.nint > 2 /*arbitrary*/)
		{
		    Bip_Error(RANGE_ERROR);
		}
		while ((code - (vmcode*)0) % pw1->val.nint)
		{
		    Store_i(Nop)
		}
	    }
	    else if (d == d_table2)  /* table(Table,Size) Size in words */
	    {
                pword *elem, *pw;
		pword result;
		int err;

		Check_List(pw1->tag);
		pw = &result;
	        while (IsList(pw1->tag))      /* list of Key-ref(Ref) pairs */
		{
		  pw1 = pw1->val.ptr;
		  elem = pw1++;
		  Dereference_(elem);
		  if (IsStructure(elem->tag) && (elem->val.ptr->val.did == d_.minus)) 
		  {
		    value key;

		    elem = elem->val.ptr + 1;
		    Dereference_(elem);
		    Get_Functor_Did(elem->val, elem->tag, key.did);
		    Set_Did_Stability(key.did, DICT_CODE_REF);
		    Make_List(pw, TG);
		    pw = TG;
		    Push_List_Frame();
		    Make_Struct(&pw[0], TG);
		    Push_Struct_Frame(d_.minus);
		    Make_Integer(&pw[3], key.nint);
		    pw[4] = *(++elem);  /* value */
		    pw = &pw[1];
		    Dereference_(pw1);
		  }
		}
		Make_Nil(pw);
		if (!IsNil(result.tag)) 
		{
		  pword key;

		  key.val.nint = 1;
		  key.tag.kernel = TINT;
		  result.val.ptr = ec_keysort(result.val, key.val, key.tag, 0, 1, 0, &err);
		  if (!result.val.ptr) 
		  {
		    Bip_Error(err);
		  }
		}
		pw1 = &result;	
	        while (IsList(pw1->tag))      /* list of Key-ref(Ref) pairs */
		{
		  pw1 = pw1->val.ptr;
		  elem = pw1++;
		  Dereference_(elem);
		  elem = elem->val.ptr + 1;
		  Store_d(elem->val.nint);   /* store key */
		  elem++;
		  Dereference_(elem);
		  if (IsStructure(elem->tag) && (elem->val.ptr->val.did == d_ref1)) 
		  {
		    elem = elem->val.ptr + 1;
		    Dereference_(elem);
		    Store_Ref(elem, base);     /* store value */
		  } 
		  else
		  {
		    Bip_Error(TYPE_ERROR);
		  }
		  Dereference_(pw1);
		}
	    }
	    else { Bip_Error(RANGE_ERROR); }
	}
	else { Bip_Error(TYPE_ERROR); }

	if (codetype == TLIST) {
	    Dereference_(codeptr);	/* get the list tail	*/
	    if (IsRef(codeptr->tag))
		{ Bip_Error(INSTANTIATION_FAULT); }
	    else if (IsList(codeptr->tag))
		codeptr = codeptr->val.ptr;
	    else if (IsNil(codeptr->tag))
		break;			/* end of the list	*/
	    else { Bip_Error(TYPE_ERROR); }
	} else { /* codetype == TCOMP */
	    if (--codelen == 0)
	    	break;
	}
    }

    a_mutex_lock(&ProcedureLock);

    proc = local_procedure(wdid, vm.did, tm, PRI_CREATE);
    if (!proc)
    {
	a_mutex_unlock(&ProcedureLock);
	Get_Bip_Error(err);
	Bip_Error(err);
    }
    /* Set ECO_FLAGS according to flags argument.
     * Keep DEBUG_SK if set, because it was probably done by a preceding skipped-directive.
     * Always clear TOOL flag.
     */
    flags = (uint32)((vflags.nint & ECO_FLAGS) | (PriFlags(proc) & (DEBUG_SK)));
    err = pri_compatible_flags(proc, CODETYPE|TOOL|ECO_FLAGS, VMCODE|flags);
    if (err != PSUCCEED)
    {
	a_mutex_unlock(&ProcedureLock);
	Bip_Error(err);
    }
    pri_change_flags(proc, TOOL|ECO_FLAGS, flags);
    pricode.vmc = base;
    pri_define_code(proc, VMCODE, pricode);
    a_mutex_unlock(&ProcedureLock);

    Succeed_;
}


static int
p_decode_code(value vcode, type tcode, value v, type t)
{
    dident d;
    word w;
    pword *pw1;

    if (IsAtom(tcode) && vcode.did == d_tags)	/* tags -> tags/NTYPES */
    {
	int i;
	pword *pw = TG;
	Push_Struct_Frame(add_dict(d_tags,NTYPES));
	for (i = 0; i < NTYPES; i++)
	{
	  if (tag_desc[i].tag_name == d_.nil) 
	  {
	    Make_Nil(&pw[i+1]);
	  }
          else
	  {
	    Make_Atom(&pw[i+1], tag_desc[i].tag_name);
	  }
	}
	Return_Unify_Structure(v, t, pw);
    }
    Check_Structure(tcode);

    pw1 = vcode.ptr;
    d = pw1++->val.did;
    if (d == d_constant2)	/* constant(Tag,Val) -> Term */
    {
	pword c;
	pword *pw2 = pw1+1;
	Dereference_(pw1);
	Check_Integer(pw1->tag);
	c.val.nint = pw1->val.nint;
	Dereference_(pw2);
	Check_Integer(pw2->tag);
	c.tag.kernel = pw2->val.nint;
	Return_Unify_Pw(v, t, c.val, c.tag);
    }
    else if (d == d_init2)	/* init(word,word) -> IntList */
    {
	int slot;
	uword mask;
	pword *pw, result;

	pw = pw1+1;
	Dereference_(pw1);
	Check_Integer(pw1->tag);
	slot = pw1->val.nint / (word) sizeof(pword);
	Dereference_(pw);
	Check_Integer(pw->tag);
	/* only the lower 32 bits of the mask are significant */
	mask = (uword) (pw->val.nint & (unsigned) 0xffffffff);

	Make_List(&result,TG);
	pw = TG;
	Push_List_Frame();
	Make_Integer(&pw[0],slot);
	while (mask)
	{
	    ++slot;
	    if (mask & 1)
	    {
		Make_List(&pw[1], TG);
		pw = TG;
		Push_List_Frame();
		Make_Integer(&pw[0],slot);
	    }
	    mask >>= 1;
	}
	Make_Nil(&pw[1]);
	Return_Unify_Pw(v, t, result.val, result.tag);
    }
    else if (d == d_edesc)	/* edesc(Edesc) -> Size or BitList */
    {
	uword edesc; 
	Dereference_(pw1);
	Check_Integer(pw1->tag);
	edesc = pw1->val.nint;
	if (EdescIsSize(edesc))
	{
	    /* it's an environment size, positive or -1 */
	    Return_Unify_Integer(v, t, (word)edesc/(word)sizeof(pword));
	}
	else
	{
	    /* decode environment activity map into a list of slot numbers */
	    pword result;
	    pword *pw = &result;
	    uword pos = 1;
	    uword *eam_ptr = EdescEamPtr(edesc);
	    do {
		int i = EAM_CHUNK_SZ;
		uword eam = EamPtrEam(eam_ptr);
		for(;eam;--i) {
		    if (eam & 1) {
			Make_List(pw, TG);
			pw = TG;
			Push_List_Frame();
			Make_Integer(&pw[0], pos);
			pw = &pw[1];
		    }
		    eam >>= 1;
		    pos++;
		}
		pos += i;
	    } while (EamPtrNext(eam_ptr));
	    Make_Nil(pw);
	    Return_Unify_Pw(v, t, result.val, result.tag);
	}
    }
    else if (d == d_table2)	/* table(Address,Size) -> ListOfPairs */
    {
	int i;
	pword result;
	pword *pw, *ptable;

	ptable = pw1++;
	Dereference_(ptable);
	Check_Integer(ptable->tag);	/* table address */
	ptable = ptable->val.ptr;
	Dereference_(pw1);
	Check_Integer(pw1->tag);	/* number of entries */

	pw = &result;
	for (i=0; i<pw1->val.nint; ++i)
	{
	    Make_List(pw, TG);
	    pw = TG;
	    Push_List_Frame();
	    Make_Struct(&pw[0], TG);
	    Push_Struct_Frame(d_.minus);
	    Make_Integer(&pw[3], ptable[i].val.nint);
	    Make_Integer(&pw[4], ptable[i].tag.kernel);
	    pw = &pw[1];
	}
	Make_Nil(pw);
	Return_Unify_Pw(v, t, result.val, result.tag);
    }
    else if (d == d_try_table2)	/* try_table(Word,Word) -> ListOfIntegers */
    {
	int i;
	pword result;
	pword *pw, *ptable;

	ptable = pw1++;
	Dereference_(ptable);
	Check_Integer(ptable->tag);	/* table address */
	Dereference_(pw1);
	Check_Integer(pw1->tag);	/* number of entries - 1 */

	pw = &result;
	for (i=0; i<=pw1->val.nint; ++i)
	{
	    Make_List(pw, TG);
	    pw = TG;
	    Push_List_Frame();
	    Make_Integer(pw, ptable->val.wptr[i]);
	    pw = &pw[1];
	}
	Make_Nil(pw);
	Return_Unify_Pw(v, t, result.val, result.tag);
    }
    else if (d == d_ref2) /* ref(Address,Base) -> atom or displacement */
    {
        pword *base;

	base = pw1+1;
        Dereference_(pw1);
	Check_Integer(pw1->tag);     /* absolute address of reference */
	if (pw1->val.wptr == (uword *) &fail_code_[0])
	{
	    Return_Unify_Atom(v, t, d_.fail);
	}
	else if (pw1->val.wptr == (uword *) &par_fail_code_[0])
	{
	    Return_Unify_Atom(v, t, d_par_fail);
	}
	
	Dereference_(base);
	Check_Integer(base->tag);   /* base address of predicate */
	Return_Unify_Integer(v, t, pw1->val.wptr - base->val.wptr);

    }
    if (DidArity(d) != 1)
	{ Bip_Error(RANGE_ERROR); }
    Dereference_(pw1);
    Check_Integer(pw1->tag);
    if (d == d_opc1)		/* o(Word) -> Number */
    {
	Return_Unify_Integer(v, t, Get_Int_Opcode(&(pw1->val.nint)));
    }
    else if (d == d_w1)		/* w(Word) -> Number */
    {
	Return_Unify_Integer(v, t, pw1->val.nint / (word) sizeof(word));
    }
    else if (d == d_a1)		/* a(Word) -> Number */
    {
	Return_Unify_Integer(v, t, pw1->val.ptr - &A[0]);
    }
    else if (d == d_y1 || d == d_t1 || d == d_pw1) /* y/t/pw(Word) -> Number */
    {
	Return_Unify_Integer(v, t, pw1->val.nint / (word) sizeof(pword));
    }
    else if (d == d_proc1)	/* proc(Word) -> N/A or M:N/A */
    {
	pword *pw = TG;
	dident pdid = PriDid(pw1->val.priptr);
	Push_Struct_Frame(d_.quotient);
	Make_Atom(&pw[1], add_dict(pdid, 0));
	Make_Integer(&pw[2], DidArity(pdid));
	if (PriScope(pw1->val.priptr) == QUALI)
	{
	    Push_Struct_Frame(d_.colon);
	    Make_Atom(&pw[4], pw1->val.priptr->module_ref);
	    Make_Struct(&pw[5], pw);
	    pw = &pw[3];
	}
	Return_Unify_Structure(v, t, pw);
    }
    else if (d == d_functor1)	/* functor(Word) -> N/A */
    {
	pword *pw = TG;
	Push_Struct_Frame(d_.quotient);
	Make_Atom(&pw[1], add_dict(pw1->val.did, 0));
	Make_Integer(&pw[2], DidArity(pw1->val.did));
	Return_Unify_Structure(v, t, pw);
    }
    else if (d == d_.atom)	/* atom(Word) -> '...' */
    {
	if (DidArity(pw1->val.did) != 0)
	    { Bip_Error(RANGE_ERROR); }
	Return_Unify_Atom(v, t, pw1->val.did);
    }
    else if (d == d_.string)	/* string(Word) -> "..." */
    {
	Return_Unify_String(v, t, pw1->val.ptr);
    }
#ifdef TFLOAT
    else if (d == d_.float1)	/* float(Word) -> x.y */
    {
	Return_Unify_Float(v, t, pw1->val.real);
    }
#endif
    else if (d == d_nv1 || d == d_mv1)	/* nv(Word) -> 'VarName' */
    {
	Return_Unify_Atom(v, t, TagDid(pw1->val.nint));
    }
    Bip_Error(RANGE_ERROR);
}


static int
p_functor_did(value vspec, type tspec, value v, type t)
{
    dident d;
    Get_Functor_Did(vspec, tspec, d);
    Return_Unify_Integer(v, t, (word) d);
}


static int
p_retrieve_code(value vproc, type tproc, value vcode, type tcode, value vm, type tm)
{
    dident	wdid;
    vmcode	*code_block, *code;
    int		err;
    pri		*proc;
    pword	block_list;
    pword	*p_block_list, *pcode;
    word	size;

    Check_Output_List(tcode);
    Check_Module(tm, vm);
    Get_Proc_Did(vproc, tproc, wdid);

    proc = visible_procedure(wdid, vm.did, tm, 0);
    if (!proc)
    {
	Get_Bip_Error(err);
	Bip_Error(err);
    }

    p_block_list = &block_list;
    code_block = ProcHeader(PriCode(proc));
    while (code_block)
    {
	pword	*p_struct;
	word	i;

	code = CodeStart(code_block);
	switch(BlockType(code_block))
	{
	case GROUND_TERM:
#if 0
	    Make_List(p_block_list, TG);	/* new list element */
	    p_block_list = TG;
	    Push_List_Frame();
	    Make_Struct(p_block_list, TG);
	    ++p_block_list;
	    p_struct = TG;

	    Push_Struct_Frame(in_dict("term",2));
	    pcode = ProcStruct(code);
	    /* we return pcode->val.ptr instead if pcode because that's
	     * the address that occurs in the ..._constant instructions */
	    Make_Integer(&p_struct[1], pcode->val.ptr);
	    p_struct[2] = *pcode;
#endif
	    break;

	case PARALLEL_TABLE:
	    break;

	case HASH_TABLE:
	case UNDEFINED_PROC:
	case DYNAMIC_PROC:
	    p_fprintf(current_err_,
	    	"retrieve_code/3: can't handle block type %d (ignoring)\n",
		BlockType(code_block));
	    ec_flush(current_err_);
	    break;

	default:			/* normal code block */
	    Make_List(p_block_list, TG);	/* new list element */
	    p_block_list = TG;
	    Push_List_Frame();
	    Make_Struct(p_block_list, TG);
	    ++p_block_list;
	    p_struct = TG;

	    Push_Struct_Frame(in_dict("code",2));
	    Make_Integer(&p_struct[1],code);
	    pcode = &p_struct[2];
	    size = ProcCodeSize(code);
	    for (i=0; i<size; ++i)
	    {
		Make_List(pcode, TG);
		pcode = TG;
		Push_List_Frame();
		Make_Integer(pcode, code[i]);
		++pcode;
	    }
	    Make_Nil(pcode);
	    break;
	}
	code_block = * (vmcode **) code_block;
    }
    Make_Nil(p_block_list);

    Return_Unify_Pw(vcode, tcode, block_list.val, block_list.tag);
}


/*
 * Clean up all memory areas whre there might be some unused stuff.
 */
static int
p_trimcore(void)
{
    reclaim_abolished_procedures();
    (void) trim_global_trail(TG_SEG);
    (void) trim_control_local();
    Succeed_;
}

int
get_mode(uint32 mode_decl, dident wd)
{
    int			arity;
    int			mode;
    pword		*p = TG++;

    arity = DidArity(wd);

    if (arity == 0)
    {
	Check_Gc;
	Make_Atom(p, wd)
	return PSUCCEED;
    }
    else if (wd == d_.list)
    {
	Make_List(p,TG);
	p = TG;
	Push_List_Frame();
    }
    else
    {
	Make_Struct(p,TG);
	p = TG+1;
	Push_Struct_Frame(wd);
    }

    while (arity--)
    {
	p->tag.kernel = TDICT;
	Next_Mode(mode_decl, mode);
	switch (mode)
	{
	case NONVAR:
	    p->val.did = d_.plus0;
	    break;

	case GROUND:
	    p->val.did = d_.plusplus;
	    break;

	case OUTPUT:
	    p->val.did = d_.minus0;
	    break;

	case NOALIAS_INST:
	    p->val.did = d_plusminus;
	    break;

	case NOALIAS:
	    p->val.did = d_minusplus;
	    break;

	default:
	    p->val.did = d_.question;
	}
	p++;
    }
    return PSUCCEED;
}
/* Bip_Error() redefined to return() !! */
