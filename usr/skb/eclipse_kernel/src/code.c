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
 * VERSION	$Id: code.c,v 1.9 2008/09/01 11:44:54 jschimpf Exp $
 */

/********************************************************************
 *
 *
 * File code.c
 *
 * This file is intended for the initialization of fixed, handcoded
 * sequences of abstract machine code.
 *
 ***********************************************************************/

#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "error.h"
#include "dict.h"
#include "emu_export.h"
#include "opcode.h"
#include "gencode.h"
#include "debug.h"
#include "module.h"
#include "database.h"

/* global definition */
#define Kernel_Proc(d, flag, ccode)					\
	pd = global_procedure(d, d_.kernel_sepia, tm);			\
	pd->flags |= SYSTEM|flag;						\
	pricode.vmc = ccode;						\
	pri_define_code(pd, VMCODE, pricode);
#define Local_Kernel_Proc(d, flag, ccode)					\
	pd = local_procedure(d, d_.kernel_sepia, tm, PRI_CREATE);	\
	pd->flags |= SYSTEM|flag;						\
	pricode.vmc = ccode;						\
	pri_define_code(pd, VMCODE, pricode);
#define Exported_Kernel_Proc(d, flag, ccode)					\
	pd = export_procedure(d, d_.kernel_sepia, tm);			\
	pd->flags |= SYSTEM|flag;						\
	pricode.vmc = ccode;						\
	pri_define_code(pd, VMCODE, pricode);

#define Store_Var_Alloc(size, arg, var)				\
				Store_4(			\
					Get_variableNAML,	\
					Esize(size),		\
					Address(arg),		\
					Esize(var))

#define KernelPri(d) \
	visible_procedure(d, d_.kernel_sepia, tm, PRI_CREATE|PRI_REFER)


/*
 * CAUTION: only static code that is never redefined may use
 * an array to hold the code. Otherwise the system would
 * try to free the code space to the code heap on recompilation.
 *
 * The first dummy procedure is there to fool the profiler:
 * All code fragments which do not belong to a particular procedure
 * account for this dummy procedure (assuming the C compiler allocates
 * all the following arrays consecutively).
 */

vmcode dummy_procedure_code_[PROC_PREFIX_SIZE+3]; /* should be the first */
vmcode fail_return_env_0_[3];
vmcode eval_code_[15];
vmcode slave_code_[2];
vmcode slave_fail_code_[25];
vmcode restore_code_[3];
vmcode restore_debug_code_[3];
vmcode trace_exit_code_[3];
vmcode return_code_[2];
vmcode it_code_[20];
vmcode it_block_code_[21];
vmcode recurs_code_[15];
vmcode boot_code_[16];
vmcode fail_code_[2];

/*
 * Special backtrack codes that are used to identify certain frames
 * on the control stack. They may not be used for other purposes.
 */

vmcode it_fail_code_[3];	   /* interrupt emulator invocation frame */
vmcode stop_fail_code_[3];	   /* recursive emulator invocation frame */
vmcode exception_fail_code_[3];	   /* exception frame */
vmcode catch_unint_fail_code_[11]; /* catch frame with events deferred */
vmcode external_fail_code_[2];	   /* choicepoint of backtracking external */
vmcode soft_cut_code_[2];	   /* softly cut choice point */
vmcode gc_fail_code_[2];	   /* gc dummy choicepoint */
vmcode par_fail_code_[2];	   /* parallel choicepoint */

/*
 * code arrays for static procedures with proper header.
 * They are used instead of heap-allocated space only when the code
 * is referenced by direct pointers other than the one in the pri.
 */

vmcode syserror_code_[PROC_PREFIX_SIZE+13];
vmcode true_code_[PROC_PREFIX_SIZE+2];
vmcode cut_to_code_[PROC_PREFIX_SIZE+4];
vmcode comma_body_code_[PROC_PREFIX_SIZE+30];
vmcode semic_body_code_[PROC_PREFIX_SIZE+18];
vmcode cond_body_code_[PROC_PREFIX_SIZE+35];
vmcode cond3_body_code_[PROC_PREFIX_SIZE+49];
vmcode softcut5_body_code_[PROC_PREFIX_SIZE+50];
vmcode call2_code_[PROC_PREFIX_SIZE+10];
vmcode call_with_cut_code_[PROC_PREFIX_SIZE+2];
vmcode call_at_code_[PROC_PREFIX_SIZE+4];
vmcode gc_code_[PROC_PREFIX_SIZE+8];
vmcode exit_block_code_[PROC_PREFIX_SIZE+8];
vmcode wake_code_[PROC_PREFIX_SIZE+5];
vmcode idle_code_[PROC_PREFIX_SIZE+4];
vmcode fork_code_[PROC_PREFIX_SIZE+49];
vmcode wb_code_[PROC_PREFIX_SIZE+15];

/*
 * These are pointers into the arrays above
 */

vmcode	*bip_error_code_,
	*auto_gc_code_,
	*catch_fail_code_,
	*do_exit_block_code_,
	*sync_it_code_,
	*do_idle_code_,
	*idle_ret_code_,
	*fork_unify_code_,
	*meta_exit_simple_code_,
	*meta_last_exit_simple_code_,
	*prolog_error_code_,
	*wb_fail_code_,
	*do_call_code_;


pri	*true_proc_,
	*arity_proc_,
	*softcut_proc_,
	*cut_to_proc_,
	*identical_proc_,
        *not_identical_proc_,
        *inequality_proc_,
        *not_ident_list_proc_,
        *minus_proc_,
        *add_proc_,
        *sub_proc_,
        *mul_proc_,
        *quot_proc_,
        *div_proc_,
        *rem_proc_,
        *fdiv_proc_,
        *mod_proc_,
        *and_proc_,
        *or_proc_,
        *xor_proc_,
        *bitnot_proc_,
	*lt_proc3_,
	*le_proc3_,
	*gt_proc3_,
	*ge_proc3_,
	*eq_proc3_,
	*ne_proc3_,
        *arg_proc_,
        *make_suspension_proc_,
	*cut_to_stamp_proc_,
	*fail_proc_;


/*
 * make_function_bip()
 * make_test_bip()
 *
 * Create descriptor and code stubs for those built-ins that are implemented
 * by a single abstract machine instruction.  The code sequence is only used
 * for metacalling and waking.  Other calls are inlined by the compiler.
 */

pri *
make_function_bip(dident did1, int opc, uint32 flags, uint32 mode, int argdesc)
{
    vmcode	*code;
    type	tm;
    pri_code_t	pricode;
    pri		*pd;
    word	 i, arity = DidArity(did1);
    Allocate_Default_Procedure(arity+7, did1);
    Exported_Kernel_Proc(did1, flags|EXTERN|ARGFLEXWAM|DEBUG_DB|DEBUG_DF, code);
    PriMode(pd) = mode;
    Store_i(opc);
	for(i=1; i<arity; ++i) {
	    Store_d(Address(i));
	}
	Store_d(Address(arity+1));
	if (argdesc >= 0) {
	    Store_d(argdesc);
	}
    /*
     * The previous instruction leaves the function result in argument
     * register A[arity+1], which then needs to be unified with A[arity].
     */
    Store_3(Get_valueAMAM,Address(arity),Address(arity+1))
    Store_i(Retd_nowake);	/* because inlined calls don't wake either */
    Store_i(Code_end);
    return pd;
}

pri *
make_test_bip(dident did1, int opc, uint32 flags, uint32 mode, int argdesc, int vis)
{
    vmcode	*code;
    type	tm;
    pri_code_t	pricode;
    pri		*pd;
    word	 i, arity = DidArity(did1);
    Allocate_Default_Procedure(arity+4, did1);
    if (vis == EXPORT) {
	Exported_Kernel_Proc(did1, flags|EXTERN|ARGFLEXWAM|DEBUG_DB|DEBUG_DF, code);
    } else {
	Local_Kernel_Proc(did1, flags|EXTERN|ARGFLEXWAM|DEBUG_DB|DEBUG_DF, code);
    }
    PriMode(pd) = mode;
    Store_i(opc);
	for(i=1; i<=arity; ++i) {
	    Store_d(Address(i));
	}
	if (argdesc >= 0) {
	    Store_d(argdesc);
	}
    Store_i(Retd_nowake);	/* because inlined calls don't wake either */
    Store_i(Code_end);
    return pd;
}


vmcode *
allocate_code_block(long int size, uword btablepos, uword link, uword bid, uword fid, uword btype, uword cid)
{
    vmcode	*code;

    code = (vmcode *) hg_alloc(((int)size + PROC_PREFIX_SIZE) * sizeof(vmcode));
    Make_Prefix(link, btablepos, size, bid, fid, btype, cid)
    return code;
}


reclaim_ground_structure(vmcode *code_header)
{
    extern void free_heapterm();

    free_heapterm(ProcStruct(CodeStart(code_header)));
    hg_free((generic_ptr) code_header);
}


/*
 * Initialisation of code that is defined on the WAM level.
 * Code arrays and pointers in private memory have to be initialised always.
 * Heap-allocated code and PRIs only if (flags & INIT_SHARED).
 */

void
code_init(int flags)
{
    extern dident	d_call_susp_;

    dident		did1;
    register vmcode	*code;
    vmcode		*aux, *aux1;
    register pri	*pd;
    type		tm;
    pri_code_t		pricode;

    tm.kernel = ModuleTag(d_.kernel_sepia);
    /*
     * dummy procedure
     * Its code should precede all procedureless code fragments
     * so that the profiler accounts them for this procedure.
     * (it can also be used for other purposes)
     */
    code = &dummy_procedure_code_[0];
    Make_Default_Prefix(d_.dummy_call);
    if (flags & INIT_SHARED)
    {
	Local_Kernel_Proc(d_.dummy_call, ARGFIXEDWAM | DEBUG_DB, code);
    }
    Store_2(Undefined, pd)
    Store_i(Code_end)

  if (flags & INIT_SHARED)
  {

/*
 * The debugger needs the procedure descriptor of (;)/2, that's why
 * we have a prelimiary definition here. It's overwritten in kernel.pl
 */
    pd = global_procedure(d_.comma, d_.kernel_sepia, tm);
    pd->flags |= SYSTEM|TOOL;
    pd = global_procedure(d_.semicolon, d_.kernel_sepia, tm);
    pd->flags |= SYSTEM|TOOL;
    pd = global_procedure(d_.cond, d_.kernel_sepia, tm);
    pd->flags |= SYSTEM|TOOL;
    pd = local_procedure(d_.softcut, d_.kernel_sepia, tm, PRI_CREATE);
    pd->flags |= SYSTEM|TOOL;

  }

/*
 * definition of call/2
 *	call(Goal, Module)
 * (untraced_call/2 is supposed to be an untraceable call/2,
 * it's the body of call/1 and made untraceable in kernel.pl)
 */
    did1 = in_dict("untraced_call", 2);
    code = &call2_code_[0];
    Make_Default_Prefix(did1);
    if (flags & INIT_SHARED)
    {
	Local_Kernel_Proc(did1, ARGFIXEDWAM|DEBUG_DF|DEBUG_TRMETA, code);
	Kernel_Proc(d_.call_body, ARGFIXEDWAM|DEBUG_DF, code);
	Exported_Kernel_Proc(in_dict("trace_body",2), ARGFIXEDWAM|DEBUG_ST|DEBUG_SP|DEBUG_TRMETA, code);
	Exported_Kernel_Proc(in_dict("debug_body",2), ARGFIXEDWAM|DEBUG_ST|DEBUG_TRMETA, code);
    }
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_2(SavecutAM, Address(4))
    Store_i(Meta_jmp)
    Store_i(Code_end)	/* not really, see below */
/*
 * The following code is dynamically inserted (by the Metacall instruction)
 * after a metacalled builtin.
 * It generates the EXIT_PORT for the builtin and pops its arguments
 * together with the dummy environment.
 * It is in the code block of call/2 (for the profiler).
 */
   meta_exit_simple_code_ = code;
    Store_i(Exitd_nowake);	/* Do not wake here (like compiled sequence) */
   meta_last_exit_simple_code_ = code;
    Store_i(Exitd);		/* Do wake */
    Store_i(Code_end);

/*
 *	call_with_cut(Goal,CallerModule,LookupModule,SaveCut) 
 */
    did1 = in_dict("call_with_cut", 4);
    code = &call_with_cut_code_[0];
    Make_Default_Prefix(did1);
    if (flags & INIT_SHARED)
    {
	Exported_Kernel_Proc(did1, ARGFIXEDWAM|DEBUG_DF, code);
    }
    Store_i(Meta_jmp)		/* (Goal,CallerMod,LookupMod,Cut) */
    Store_i(Code_end)

/*
 *	@(Goal,CallerModule,LookupModule) - the tool body of @/2
 */
    did1 = in_dict("@", 3);
    code = &call_at_code_[0];
    Make_Default_Prefix(did1);
    if (flags & INIT_SHARED)
    {
	Exported_Kernel_Proc(did1, ARGFIXEDWAM|DEBUG_DB|DEBUG_DF, code);
    }
    do_call_code_ = code;
    Store_2(SavecutAM, Address(4))
    Store_i(Meta_jmp)		/* (Goal,CallerMod,LookupMod,Cut) */
    Store_i(Code_end)

/*
 *	:@(LookupModule,Goal,CallerModule) - the tool body of :/2
 */
    did1 = in_dict(":@", 3);
    Allocate_Default_Procedure(4L, did1);
    Exported_Kernel_Proc(did1, ARGFIXEDWAM|DEBUG_DB|DEBUG_DF, code);
    Store_2(SavecutAM, Address(4))
    Store_i(Explicit_jmp)	/* (LookupMod,Goal,CallerMod,Cut) */
    Store_i(Code_end)

/*
 *	wake/0
 *		Call all woken lists whose priority is higher than WP
 */
    code = &wake_code_[0];
    Make_Default_Prefix(d_.wake);
    if (flags & INIT_SHARED)
    {
	Kernel_Proc(d_.wake, ARGFIXEDWAM, code);
    }
    Store_2(Wake_init, Esize(1))
    Store_i(Wake)
    Store_i(Exit)
    Store_i(Code_end)

/*
 *	Goal1 , Goal2
 *	','(Goal1, Goal2, Module, Cut) :-
 *		call(Goal1, Module, Module, Cut),
 *		call(Goal2, Module, Module, Cut).
 */
    did1 = in_dict(",",4);
    code = &comma_body_code_[0];
    Make_Default_Prefix(did1);
    if (flags & INIT_SHARED)
    {
	Local_Kernel_Proc(did1, ARGFIXEDWAM, code);
    }
    Store_Var_Alloc(3, 2, 3);				/* Goal2 -> Y3 */
    Store_3(MoveAML, Address(3), Esize(2))		/* Module -> Y2 */
    Store_3(MoveAML, Address(4), Esize(1))		/* Cut -> Y1 */
    Store_3(MoveAMAM, Address(3), Address(2))
    Store_2(Metacall,Esize(3))
    Store_3(MoveLAM, Esize(3), Address(1))
    Store_3(MoveLAM, Esize(2), Address(2))
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_3(MoveLAM, Esize(1), Address(4))
    Store_i(Deallocate)
    Store_i(Meta_jmp)
    Store_i(Code_end)

/*
 *	Goal1 -> Goal2
 *	'->'(Goal1, Goal2, Module, Cut) :-
 *		call(Goal1, Module, Module, []).
 *		!,
 *		call(Goal2, Module, Module, Cut).
 */
    did1 = in_dict("->",4);
    code = &cond_body_code_[0];
    Make_Default_Prefix(did1);
    if (flags & INIT_SHARED)
    {
	Local_Kernel_Proc(did1, ARGFIXEDWAM, code);
    }
    Store_Var_Alloc(4, 2, 4);				/* Goal2 -> Y4 */
    Store_3(MoveAML, Address(3), Esize(3))		/* Module -> Y3 */
    Store_3(MoveAML, Address(4), Esize(2))		/* Cut -> Y2 */
    Store_i(Savecut)
    Store_3(MoveAMAM, Address(3), Address(2))
    Store_2(SavecutAM, Address(4))
    Store_2(Metacall,Esize(4))
    Store_2(Cut, Esize(4))
    Store_3(MoveLAM, Esize(4), Address(1))
    Store_3(MoveLAM, Esize(3), Address(2))
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_3(MoveLAM, Esize(2), Address(4))
    Store_i(Deallocate)
    Store_i(Meta_jmp)
    Store_i(Code_end)

/*
 *	Goal1 ; Goal2
 *	;(Goal1, Goal2, Module, Cut) :-
 *		call(Goal1, Module, Module, Cut).
 *	;(Goal1, Goal2, Module, Cut) :-
 *		call(Goal2, Module, Module, Cut).
 */
    did1 = in_dict(";",4);
    code = &semic_body_code_[0];
    Make_Default_Prefix(did1);
    if (flags & INIT_SHARED)
    {
	Local_Kernel_Proc(did1, ARGFIXEDWAM, code);
    }
    Store_3(Try_me_else, NO_PORT, 4)
    aux = code++;
    Store_3(MoveAMAM, Address(3), Address(2))
    Store_i(Meta_jmp)
    *(vmcode**)aux = code;
    Store_2(Trust_me, NEXT_PORT)
    Store_3(MoveAMAM, Address(2), Address(1))
    Store_3(MoveAMAM, Address(3), Address(2))
    Store_i(Meta_jmp)
    Store_i(Code_end);

/*
 *	Goal1 -> Goal2 ; Goal3
 *	;(Goal1, Goal2, Module, Cut, Goal3) :-
 *		call(Goal1, Module, Module, []).
 *		!,
 *		call(Goal2, Module, Module, Cut).
 *	;(Goal1, Goal2, Module, Cut, Goal3) :-
 *		call(Goal3, Module, Module, Cut).
 */
    did1 = in_dict(";", 5);
    code = &cond3_body_code_[0];
    Make_Default_Prefix(did1);
    if (flags & INIT_SHARED)
    {
	Local_Kernel_Proc(did1, ARGFIXEDWAM, code);
    }
    Store_3(Try_me_else, NO_PORT, 5)
    aux = code++;
    Store_Var_Alloc(4, 2, 4);				/* Goal2 -> Y4 */
    Store_3(MoveAML, Address(4), Esize(3))		/* Cut -> Y3 */
    Store_3(MoveAML, Address(3), Esize(2))		/* Module -> Y2 */
    Store_i(Savecut)
    Store_3(MoveAMAM, Address(3), Address(2))
    Store_2(SavecutAM, Address(4))
    Store_2(Metacall,Esize(4))
    Store_2(Cut, Esize(4))
    Store_3(MoveLAM, Esize(4), Address(1))
    Store_3(MoveLAM, Esize(2), Address(2))
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_3(MoveLAM, Esize(3), Address(4))
    Store_i(Deallocate)
    Store_i(Meta_jmp)
    *(vmcode**)aux = code;
    Store_2(Trust_me, NEXT_PORT)
    Store_3(MoveAMAM, Address(5), Address(1))
    Store_3(MoveAMAM, Address(3), Address(2))
    Store_i(Meta_jmp)
    Store_i(Code_end);


/*
 *	Goal1 *-> Goal2 ; Goal3
 *	softcut(Goal1, Goal2, Module, Cut, Goal3) :-
 *		call(Goal1, Module, Module, []).
 *		softcut,
 *		call(Goal2, Module, Module, Cut).
 *	softcut(Goal1, Goal2, Module, Cut, Goal3) :-
 *		call(Goal3, Module, Module, Cut).
 */
    did1 = in_dict("softcut", 5);
    code = &softcut5_body_code_[0];
    Make_Default_Prefix(did1);
    if (flags & INIT_SHARED)
    {
	Local_Kernel_Proc(did1, ARGFIXEDWAM, code);
    }
    Store_3(Try_me_else, NO_PORT, 5)
    aux = code++;
    Store_Var_Alloc(4, 2, 4);				/* Goal2 -> Y4 */
    Store_3(MoveAML, Address(4), Esize(3))		/* Cut -> Y3 */
    Store_3(MoveAML, Address(3), Esize(2))		/* Module -> Y2 */
    Store_2(SavecutL, Esize(1))
    Store_3(MoveAMAM, Address(3), Address(2))
    Store_2(SavecutAM, Address(4))
    Store_2(Metacall,Esize(4))
    Store_2(SoftcutL, Esize(1))
    Store_3(MoveLAM, Esize(4), Address(1))
    Store_3(MoveLAM, Esize(2), Address(2))
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_3(MoveLAM, Esize(3), Address(4))
    Store_i(Deallocate)
    Store_i(Meta_jmp)
    *(vmcode**)aux = code;
    Store_2(Trust_me, NEXT_PORT)
    Store_3(MoveAMAM, Address(5), Address(1))
    Store_3(MoveAMAM, Address(3), Address(2))
    Store_i(Meta_jmp)
    Store_i(Code_end);


/*
 * cut_to/1, also used for metacalled !/0
 */
    code = cut_to_code_;
    Make_Default_Prefix(d_.cut_to);
    if (flags & INIT_SHARED)
    {
	Exported_Kernel_Proc(d_.cut_to, ARGFIXEDWAM | DEBUG_DB | DEBUG_DF, code);
    }
    Store_2(CutAM, Address(1))
    Store_i(Retd);
    Store_i(Code_end);


/*
 * Backtrack codes for special control frames
 */

    code = &it_fail_code_[0];
    Store_2(Exit_emulator, PFAIL)
    Store_i(Code_end);

    code = &stop_fail_code_[0];
    Store_2(Exit_emulator, PFAIL)
    Store_i(Code_end);

    code = &exception_fail_code_[0];
    Store_i(Continue_after_exception)
    Store_i(Failure)
    Store_i(Code_end);

    code = &external_fail_code_[0];
    Store_i(Refail)
    Store_i(Code_end);

    code = &gc_fail_code_[0];
    Store_i(Refail)
    Store_i(Code_end);

    code = &soft_cut_code_[0];
    Store_i(Refail)
    Store_i(Code_end);

/*
 * The fail code of dead parallel choicepoints
 */
    code = &par_fail_code_[0];
    Store_i(Refail)
    Store_i(Code_end);

/*
 * query_emulc(Goal, Module) :- not not call(Goal, Module).
 * Discard all stacks, just return succeed or fail.
 */
    code = &eval_code_[0];
    Store_2(Allocate, Esize(1))
    Store_i(Savecut)
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_2(SavecutAM, Address(4))
    Store_2(Metacall, Esize(1))
    Store_2(Cut,Esize(1))
    Store_2(Exit_emulator, PSUCCEED)
    Store_i(Code_end);

/*
 * slave_emulc()
 */
    code = &slave_code_[0];
    Store_i(Failure)			/* execute slave_fail_code_ */
    Store_i(Code_end);

    code = &slave_fail_code_[0];
    Store_2(Fail_clause, Esize(2))	/* invoke the scheduler */
    Store_2(Allocate, Esize(1))
    Store_i(Savecut)
    Store_3(Put_atomAM, Address(1), in_dict("slave",0))
    Store_4(Put_constantAM, Address(2), ModuleTag(d_.kernel_sepia),
							d_.kernel_sepia)
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_2(SavecutAM, Address(4))
    Store_2(Metacall, Esize(1))
    Store_i(Failure)
    Store_i(Code_end);

/*
 * sub_emulc(Goal, Module) :- call(Goal, Module), !.
 * sub_emulc(Goal, Module) :- fail.
 * Cut, but keep the global and trail.
 */
    code = &recurs_code_[0];
    Store_2(Allocate, Esize(1))
    Store_i(Savecut)
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_2(SavecutAM, Address(4))
    Store_2(Metacall, Esize(1))
    Store_2(Cut,Esize(1))
    Store_2(Exit_emulator, PKEEP)
    Store_i(Code_end);


    code = &boot_code_[0];
    Store_2(Allocate, Esize(0))
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_3(Put_integerAM, Address(2), 0)
    Store_2(Put_variableAM, Address(4))
    Store_3(CallP, DidPtr(in_dict("load_eco",4))->procedure, 0)
    Store_2(Exit_emulator, PSUCCEED)
    Store_i(Code_end);

/*
 * Auxiliary code for synchronous event handling
 */
    code = &restore_code_[0];
    Store_d(Esize(-1))
    Store_i(Continue_after_event)	/* entry point for restoring status */
    Store_i(Code_end);

    code = &restore_debug_code_[0];
    Store_d(Esize(-1))
    Store_i(Continue_after_event_debug)	/* entry point for restoring status */
    Store_i(Code_end);

    code = &trace_exit_code_[0];
    Store_d(Esize(0))
    Store_i(Debug_exit)
    Store_i(Code_end);

    code = &return_code_[0];
    Store_i(Ret_nowake);		/* no Retd: event may leave chp! */
    Store_i(Code_end);			/* no wake: argument registers valid! */

/*
 * This fail_code_ is used by the fail cases of switch instructions and the like
 */
    code = &fail_code_[0];
    Store_i(Failure)
    Store_i(Code_end);


/*
 * &fail_return_env_0_[1] is used as a return address with
 * environment size 0, and for triggering failure after return
 */
    code = &fail_return_env_0_[0];
    Store_d(Esize(0))
    Store_i(Failure)
    Store_i(Code_end);


/*
 * block/4 and exit_block/1
 */

  if (flags & INIT_SHARED)
  {
    did1 = in_dict("block", 4);
    Allocate_Default_Procedure(16L, did1);
    Exported_Kernel_Proc(did1, ARGFIXEDWAM | DEBUG_DF | DEBUG_TRMETA, code);
    Store_2(Catch, 0)
    Store_2(Allocate, Esize(1))
    Store_i(Savecut)
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_2(SavecutAM, Address(4))
    Store_2(Metacall, Esize(1))
    Store_2(Cut_single, 0)	/* if the Goal was deterministic */
    Store_i(Exit)
    Store_i(Code_end);

    did1 = in_dict("block_atomic", 4);
    Allocate_Default_Procedure(16L, did1);
    Exported_Kernel_Proc(did1, ARGFIXEDWAM | DEBUG_DF | DEBUG_TRMETA, code);
    Store_2(Catch, 1)
    Store_2(Allocate, Esize(1))
    Store_i(Savecut)
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_2(SavecutAM, Address(4))
    Store_2(Metacall, Esize(1))
    Store_2(Cut_single, 0)	/* if the Goal was deterministic */
    Store_i(Exit)
    Store_i(Code_end);
  }

    code = &exit_block_code_[0];
    Make_Default_Prefix(d_.exit_block);
    if (flags & INIT_SHARED)
    {
	Kernel_Proc(d_.exit_block, ARGFIXEDWAM | DEBUG_DF | DEBUG_DB,code);
    }
    do_exit_block_code_ = code;
    Store_i(Throw)
    Store_3(MoveAMAM, Address(2), Address(3))
    Store_2(SavecutAM, Address(4))
    Store_i(Meta_jmp)
    Store_i(Code_end);


/*
 * code for syserror(Err, Goal, ContextMod, LookupMod)
 * also referenced directly from the emulator
 */
    code = &syserror_code_[0];
    Make_Default_Prefix(d_.syserror);
    if (flags & INIT_SHARED)
    {
	Local_Kernel_Proc(d_.syserror, ARGFIXEDWAM | DEBUG_DB, code);
    }
    prolog_error_code_ = code;
    Store_2(Allocate, 0)
    Store_3(Fastcall, CALL_PORT, 0)
    Store_i(Exit)
    Store_i(Code_end)	/* continues below */
/*
 * Code for calling error handlers inside builtins.
 * The exception frame has already been pushed!
 * Disallow tracing (NO_PORT) for the time being, because when the builtin
 * raised the exception inside a shallow condition, and the handler fails,
 * the Continue_after_exception instruction is currently not able to trace
 * the fail port and adjust the tracer stack correctly.
 */
   bip_error_code_ = code;
    Store_3(Fastcall, NO_PORT, 0)
    Store_i(Continue_after_exception)
    Store_i(Retd_nowake);
    Store_i(Code_end)	/* continues below */
#if SIMPLIFY
    Store_d(Esize(0))
   exception_cont_code_ = code;
    Store_i(Continue_after_exception)
    Store_i(Retd_nowake);
    Store_i(Code_end);
#endif


/*
 * code sequence for calling interrupt handlers
 */
    code = &it_code_[0];
    Store_2(Allocate, Esize(1))
    Store_i(Savecut)
    Store_2(Handler_call,0)
    Store_2(Cut,Esize(1))
    Store_2(Exit_emulator, PSUCCEED)
    Store_i(Code_end);
    sync_it_code_ = code;
    Store_2(Allocate, Esize(1))
    Store_i(Savecut)
    Store_2(Handler_call,0)
    Store_2(Cut,Esize(1))
    Store_i(Exitd)
    Store_i(Code_end)

/*
 * code sequence for calling interrupt handlers inside an
 * exit_block protected execution. Simulates:
 *
 * it(Sig) :-
 *	block(handler(Sig), Tag, postpone_exit(Tag), sepia_kernel).
 */
    code = &it_block_code_[0];
    Store_4(Put_constantAM, Address(4), ModuleTag(d_.kernel_sepia),
							d_.kernel_sepia)
    Store_3(Put_structureAM, Address(3), in_dict("postpone_exit",1))
    Store_2(Push_variableAM, Address(2))
    Store_2(Catch, 0)			/* (Sig, Tag, Recov, Mod) */
    Store_2(Allocate, Esize(1))
    Store_i(Savecut)
    Store_2(Handler_call,0)
    Store_2(Cut,Esize(1))
    Store_2(Exit_emulator, PSUCCEED)
    Store_i(Code_end);

/*
 * true/0 is here because we want its procedure identifier
 */

    code = &true_code_[0];
    Make_Default_Prefix(d_.true0);
    if (flags & INIT_SHARED)
    {
	Kernel_Proc(d_.true0, ARGFIXEDWAM | DEBUG_DF | DEBUG_DB, code);
    }
    Store_i(Retd)
    Store_i(Code_end);

/*
 * Backtrack fail code for catch that allows handling of fail-events.
 * Note that the events are triggered in a state where the choicepoint
 * is still present (this state may be required by the event handlers).
 * After all the handlers succeeded (or one of them failed), the choicepoint
 * is popped and failure continues normally. Remaining bug: when a handler
 * fails while other events are still posted, those other events will be
 * executed later in the wrong context. The only way I can see to fix that
 * is to somehow distinguish fail-undo events (always succeed) from
 * retry-events (may fail) and always handle all the former ones first.
 */

    code = &catch_unint_fail_code_[0];
    Store_i(Nop)
    catch_fail_code_ = code;
    /* Leave the choice point */
    Store_2(Retry_me_else, NO_PORT)
	aux = code++;	/* alternative is ReFail */
    Store_2(Allocate, Esize(0))
    /* Trigger pending fail-events */
    Store_3(CallP, DidPtr(d_.true0)->procedure, 0)
	*(vmcode**)aux = code;
    Store_i(Refail)
    Store_i(Code_end);

/*
 * garbage_collect/0
 */
    code = &gc_code_[0];
    did1 = in_dict("garbage_collect", 0);
    Make_Default_Prefix(did1)
    if (flags & INIT_SHARED)
    {
	Kernel_Proc(did1, ARGFIXEDWAM|DEBUG_DF, code);
    }
    Store_2(Gc, 1);
    Store_i(Ret)
    Store_i(Code_end);

    /* the following sequence is executed on global stack soft overflow
     * i.e. TG > TG_SL. This is in the same code block as garbage_collect/0
     * so it accounts for garbage_collect/0 in the profiler.
     */
    auto_gc_code_ = code;
    Store_2(Gc, 0);
    Store_i(Ret)
    Store_i(Code_end);

/*
 * idle/0
 * Dummy procedure where the engine spends its time while scheduling.
 */
    code = &idle_code_[0];
    did1 = in_dict("idle", 0);
    Make_Default_Prefix(did1)
    if (flags & INIT_SHARED)
    {
	Local_Kernel_Proc(did1, ARGFIXEDWAM|DEBUG_DF, code);
    }
    do_idle_code_ = code;
    Store_2(JmpdA, do_idle_code_);
    idle_ret_code_ = code;
    Store_i(Retd_nowake)	/* No event handling here: After a job
				 * installation the state is not clean! */
    Store_i(Code_end);


/*
 * fork/2
 * To create parallel choicepoints with arbitrary many alternatives.
 */
    code = &fork_code_[0];
    did1 = in_dict("fork", 2);
    Make_Default_Prefix(did1)
    if (flags & INIT_SHARED)
    {
	Kernel_Proc(did1, ARGFIXEDWAM | DEBUG_DF | DEBUG_DB, code);
    }
    Store_2(Integer_range_switchAM, Address(1))
    aux = code++;
    Store_d(1);			/* table size */
    Store_2d(fail_code_, aux+4)
    Store_3(Put_structureAM, Address(3), did1)
    Store_2(Push_local_valueAM, Address(1))
    Store_2(Push_local_valueAM, Address(2))
    Store_3(Put_integerAM, Address(1), 5)
    Store_3(MoveAMAM, Address(3), Address(2))
    Store_3(Put_atomAM, Address(3), d_.kernel_sepia);
    Store_3(Put_atomAM, Address(4), d_.kernel_sepia);
    Store_2(JmpdA, prolog_error_code_)
    aux1 = code;
    Store_4(Try_parallel, 1, 2, 0)
    Store_2(Retry_seq, 0)
    Store_2(Fail_clause, Esize(2))
    Store_2(Try_clause, 0)
    fork_unify_code_ = code;
    Store_3(Get_valueAMAM,Address(1),Address(2))
    Store_i(Ret)
    Store_i(Code_end);
    *(vmcode**)aux = code;
    *code++ = 1; *code++ = (vmcode) fail_code_;
    *code++ = 1; *code++ = (vmcode) aux1;
    *code++ = 1; *code++ = (vmcode) fork_unify_code_;
    Store_i(Code_end);

/*
 * worker_boundary/0
 * Create a dummy parallel choicepoint that can be
 * backtracked over only by the worker that created it.
 */
    code = &wb_code_[0];
    did1 = in_dict("worker_boundary", 0);
    Make_Default_Prefix(did1);
    if (flags & INIT_SHARED)
    {
	Exported_Kernel_Proc(did1, ARGFIXEDWAM|DEBUG_DB|DEBUG_DF, code);
    }
    aux = code;
    Store_3(Try_parallel, 1, 0)
    code++;
    wb_fail_code_ = code;
    Store_i(Retry_seq)
    code++;
    Store_2(Fail_clause, Esize(2))
    Store_i(Try_clause)
    code++;
    Store_i(Ret)
    Store_i(Code_end);
    ((vmcode**)aux)[3] = code;
    ((vmcode**)aux)[5] = code;
    ((vmcode**)aux)[9] = code;
    *(vmcode**)code++ = &fail_code_[0];
    *(vmcode**)code++ = &aux[10];
    Store_i(Code_end);


/*-----------------------------------------------------------------
 * Define predicates in WAM code that cannot be defined in Prolog.
 * Their code has no other references and is allocated on the heap.
 *-----------------------------------------------------------------*/

  if (flags & INIT_SHARED)
  {
/*
 * par_true/0
 * Create a dummy parallel choicepoint that can be used to
 * reduce the amount of incremental stack copying.
 */
    did1 = in_dict("par_true", 0);
    Allocate_Default_Procedure(15L, did1);
    Exported_Kernel_Proc(did1, ARGFIXEDWAM|DEBUG_DB|DEBUG_DF, code);
    aux = code;
    Store_3(Try_parallel, 1, 0)
    code++;
    Store_i(Retry_seq)
    code++;
    Store_2(Fail_clause, Esize(2))
    Store_i(Try_clause)
    code++;
    Store_i(Ret)
    Store_i(Code_end);
    ((vmcode**)aux)[3] = code;
    ((vmcode**)aux)[5] = code;
    ((vmcode**)aux)[9] = code;
    *(vmcode**)code++ = &fail_code_[0];
    *(vmcode**)code++ = &aux[10];
    Store_i(Code_end);

/*
 *	call_suspension(+Suspension)
 */
    Allocate_Default_Procedure(2L, d_call_susp_);
    Exported_Kernel_Proc(d_call_susp_, ARGFIXEDWAM|DEBUG_DB|DEBUG_DF, code);
    Store_i(Suspension_jmp)
    Store_i(Code_end)

/*
 * repeat/0
 */
    did1 = in_dict("repeat", 0);
    Allocate_Default_Procedure(9L, did1);
    Kernel_Proc(did1, ARGFIXEDWAM | DEBUG_DF | DEBUG_DB, code);
    aux = code;
    Store_4(Try, NO_PORT, 0, aux + 7)
    Store_3(Retry_me_else, NEXT_PORT, aux + 4)
    Store_i(Retn)
    Store_i(Code_end);

/*
 * clause/5
 */
    did1 = in_dict("clause",5);
    Allocate_Default_Procedure(4L, did1);
    Local_Kernel_Proc(did1, ARGFIXEDWAM, code);
    Store_i(Clause);
    Store_i(Retd);
    Store_i(Code_end);

/*
 * guard(Goal, Result, Module)
 */
    did1 = in_dict("guard", 3);
    Allocate_Default_Procedure(34, did1);
    Exported_Kernel_Proc(did1, ARGFIXEDWAM | DEBUG_DB | DEBUG_DF, code);
    Store_3(Try_me_else, NO_PORT, 0)
    aux = code++;
    Store_Var_Alloc(2, 2, 2);	/* 4 words */
    Store_i(Savecut)
    Store_3(MoveAMAM,Address(3),Address(2))
    Store_2(SavecutAM, Address(4))
    Store_2(Metacall, Esize(1))
    Store_3(MoveLAM, Esize(2), Address(1))
    Store_2(GuardL, Esize(1))
    aux1 = code++;
    Store_3(Get_atomAM, Address(1), d_.true0)
    Store_i(Exitc)
    *(vmcode**)aux1 = code;
    Store_2(Trust_me, NEXT_PORT)
    Store_3(Get_atomAM, Address(1), d_.question)
    Store_i(Retd);
    *(vmcode**)aux = code;
    Store_i(Refail);
    Store_i(Code_end);

/*
 * module_directive/4
 * dummy code for checking the module in top.pl until this procedure is
 * really defined
 */
    Allocate_Default_Procedure(2L, d_.module_directive);
    Local_Kernel_Proc(d_.module_directive, ARGFIXEDWAM, code);
    Store_i(Retd);
    Store_i(Code_end);

/*
 * boot_error/2
 */
    did1 = in_dict("boot_error", 2);
    Allocate_Default_Procedure(70L, did1);
    Local_Kernel_Proc(did1, ARGFIXEDWAM , code);
    pd = KernelPri(in_dict("write_", 2));

    Store_Var_Alloc(2, 2, 1)	/* 4 words */
    aux = code+1;
    Store_2(Set_bp, 0);
    Store_3(Get_integerAM, Address(1), 170);

    Store_i(Restore_bp);
    Store_3(Put_variableAML, Address(1), Esize(2))
    Store_3(CallP, KernelPri(in_dict("errno_id", 1)), Esize(2));
    aux1 = code+1;
    Store_2(Branch, 0);

    *(vmcode**)aux = code;
    Store_3(Put_variableAML, Address(2), Esize(2))
    Store_3(CallP, KernelPri(in_dict("error_id", 2)), Esize(2));

    *(vmcode**)aux1 = code;
    Store_3(MoveLAM, Esize(2), Address(1));
    Store_3(Put_atomAM, Address(2), d_.kernel_sepia);
    Store_3(CallP, pd, Esize(2));

    Store_3(Put_atomAM, Address(1), in_dict(" in ",0));
    Store_3(Put_atomAM, Address(2), d_.kernel_sepia);
    Store_3(CallP, pd, Esize(2));

    Store_3(MoveLAM, Esize(1), Address(1));
    Store_3(Put_atomAM, Address(2), d_.kernel_sepia);
    Store_3(CallP, KernelPri(in_dict("writeq_", 2)), Esize(2));

    Store_3(Put_atomAM, Address(1), in_dict("\n\n",0));
    Store_3(Put_atomAM, Address(2), d_.kernel_sepia);
    Store_3(CallP, pd, Esize(2));

    Store_3(Put_integerAM, Address(1), -1);
    Store_2(ChainP, DidPtr(in_dict("exit0", 1))->procedure);
    Store_i(Code_end);

/*
 * yield/4
 */
    did1 = in_dict("yield", 4);
    Allocate_Default_Procedure(13L, did1);
    Local_Kernel_Proc(did1, ARGFIXEDWAM , code);
    Store_3(Put_integerAM, Address(0), PYIELD)
    Store_2(Bounce, 0); /* exits the emulator and bounce over the trampoline */
    Store_3(Get_valueAMAM,Address(1),Address(3))
    Store_3(Get_valueAMAM,Address(2),Address(4))
    Store_i(Retd);
    Store_i(Code_end);


/*
 * Create the built-ins that are implemented by a single abstract machine instruction
 */
    make_test_bip(d_.fail, Failure, 0, 0, -1, EXPORT);
    make_test_bip(d_.unify, Get_valueAMAM, U_UNIFY, BoundArg(1, NONVAR) | BoundArg(2, NONVAR), -1, EXPORT);

    make_test_bip(in_dict("set_bip_error",1), BI_SetBipError, 0, 0, -1, EXPORT);
    make_function_bip(in_dict("get_bip_error",1), BI_GetBipError, U_SIMPLE, BoundArg(1,CONSTANT), -1);
    make_function_bip(in_dict("get_cut",1), SavecutAM, U_SIMPLE, BoundArg(1,CONSTANT), -1);

    make_test_bip(in_dict("sys_return",1), BI_Exit, 0, 0, -1, LOCAL);
    make_test_bip(in_dict("cut_to_stamp",2), BI_CutToStamp, 0, 0, 0, EXPORT);
    make_test_bip(in_dict("cont_debug",0), BI_ContDebug, 0, 0, -1, LOCAL);

    make_test_bip(d_.free1, BI_Free, 0, 0, -1, EXPORT);
    make_test_bip(d_.is_suspension, BI_IsSuspension, 0, 0, -1, EXPORT);
    make_test_bip(d_.is_event, BI_IsEvent, 0, 0, -1, EXPORT);
    make_test_bip(d_.is_handle, BI_IsHandle, 0, 0, -1, EXPORT);
    make_test_bip(d_.var, BI_Var, 0, 0, -1, EXPORT);
    make_test_bip(d_.nonvar, BI_NonVar, 0, 0, -1, EXPORT);
    make_test_bip(d_.meta, BI_Meta, 0, 0, -1, EXPORT);
    make_test_bip(d_.atom, BI_Atom, 0, 0, -1, EXPORT);
    make_test_bip(d_.integer, BI_Integer, 0, 0, -1, EXPORT);
    make_test_bip(d_.rational1, BI_Rational, 0, 0, -1, EXPORT);
    make_test_bip(d_.real, BI_Real, 0, 0, -1, EXPORT);
    make_test_bip(d_.float1, BI_Float, 0, 0, -1, EXPORT);
    make_test_bip(d_.breal, BI_Breal, 0, 0, -1, EXPORT);
    make_test_bip(d_.string, BI_String, 0, 0, -1, EXPORT);
    make_test_bip(d_.number, BI_Number, 0, 0, -1, EXPORT);
    make_test_bip(d_.atomic, BI_Atomic, 0, 0, -1, EXPORT);
    make_test_bip(d_.compound, BI_Compound, 0, 0, -1, EXPORT);
    make_test_bip(d_.is_list, BI_IsList, 0, 0, -1, EXPORT);
    make_test_bip(d_.bignum, BI_Bignum, 0, 0, -1, EXPORT);
    make_test_bip(in_dict("callable",1), BI_Callable, 0, 0, -1, EXPORT);

    make_function_bip(in_dict("-",2), BI_Minus, U_SIMPLE, BoundArg(2,CONSTANT), 4);
    make_function_bip(in_dict("+",3), BI_Add, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("-",3), BI_Sub, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("*",3), BI_Mul, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("/",3), BI_Quot, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("//",3), BI_Div, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("rem",3), BI_Rem, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("div",3), BI_FloorDiv, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("mod",3), BI_FloorRem, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("/\\",3), BI_And, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("\\/",3), BI_Or, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("xor", 3), BI_Xor, PROC_DEMON|U_SIMPLE, BoundArg(3,CONSTANT), 16);
    make_function_bip(in_dict("\\",2), BI_Bitnot, U_SIMPLE, BoundArg(2,CONSTANT), 4);

    make_function_bip(in_dict("arity",2), BI_Arity, U_SIMPLE, BoundArg(2,CONSTANT), 4);
    make_function_bip(in_dict("arg",3), BI_Arg, PROC_DEMON|U_UNIFY, BoundArg(2, NONVAR) | BoundArg(3, NONVAR), 16);

    make_test_bip(in_dict("make_suspension",4), BI_MakeSuspension, U_UNIFY|DEBUG_INVISIBLE, BoundArg(3, NONVAR), 0, EXPORT);

    make_test_bip(d_.identical, BI_Identical, 0, 0, -1, EXPORT);
    make_test_bip(d_.not_identical, BI_NotIdentical, 0, 0, -1, EXPORT);
    make_test_bip(d_.diff_reg, BI_Inequality, PROC_DEMON, 0, -1, EXPORT);
    make_test_bip(in_dict("\\==",3), BI_NotIdentList, 0, BoundArg(3, NONVAR), -1, EXPORT);

    make_test_bip(in_dict("<",3), BI_Lt, PROC_DEMON, 0, 0, EXPORT);
    make_test_bip(in_dict(">",3), BI_Gt, PROC_DEMON, 0, 0, EXPORT);
    make_test_bip(in_dict("=<",3), BI_Le, PROC_DEMON, 0, 0, EXPORT);
    make_test_bip(in_dict(">=",3), BI_Ge, PROC_DEMON, 0, 0, EXPORT);
    make_test_bip(in_dict("=:=",3), BI_Eq, PROC_DEMON, 0, 0, EXPORT);
    make_test_bip(in_dict("=\\=",3), BI_Ne, PROC_DEMON, 0, 0, EXPORT);

  } /* end if (flags & INIT_SHARED) */


/*-----------------------------------------------------------------
 * Initialize global (non-shared) pointers to procedure identifiers
 *-----------------------------------------------------------------*/

#define KernelProc(d) local_procedure(d, d_.kernel_sepia, tm, 0)

    true_proc_ = KernelProc(d_.true0);
    cut_to_proc_ = KernelProc(d_.cut_to);
    softcut_proc_ = KernelProc(d_.softcut);
    cut_to_stamp_proc_ = KernelProc(in_dict("cut_to_stamp", 2));
    fail_proc_ = KernelProc(d_.fail);
    identical_proc_ = KernelProc(d_.identical);
    not_identical_proc_ = KernelProc(d_.not_identical);
    not_ident_list_proc_ = KernelProc(in_dict("\\==",3));
    inequality_proc_ = KernelProc(d_.diff_reg);
    minus_proc_ = KernelProc(in_dict("-",2));
    add_proc_ = KernelProc(in_dict("+",3));
    sub_proc_ = KernelProc(in_dict("-",3));
    mul_proc_ = KernelProc(in_dict("*",3));
    quot_proc_ = KernelProc(in_dict("/",3));
    div_proc_ = KernelProc(in_dict("//",3));
    rem_proc_ = KernelProc(in_dict("rem",3));
    fdiv_proc_ = KernelProc(in_dict("div",3));
    mod_proc_ = KernelProc(in_dict("mod",3));
    and_proc_ = KernelProc(in_dict("/\\",3));
    or_proc_ = KernelProc(in_dict("\\/",3));
    xor_proc_ = KernelProc(in_dict("xor",3));
    bitnot_proc_ = KernelProc(in_dict("\\",2));
    lt_proc3_ = KernelProc(in_dict("<",3));
    gt_proc3_ = KernelProc(in_dict(">",3));
    le_proc3_ = KernelProc(in_dict("=<",3));
    ge_proc3_ = KernelProc(in_dict(">=",3));
    eq_proc3_ = KernelProc(in_dict("=:=",3));
    ne_proc3_ = KernelProc(in_dict("=\\=",3));
    arg_proc_ = KernelProc(in_dict("arg",3));
    arity_proc_ = KernelProc(in_dict("arity",2));
    make_suspension_proc_ = KernelProc(in_dict("make_suspension",4));
}


/* 
 * generates necessary WAM instruction for a C built_in.
 * pd is supposed to be of the valid type (consistency check already made)
 */

/*ARGSUSED*/
int
b_built_code(pri *pd, word function, int nondet)
{
	vmcode         *code, *aux;
	pri_code_t	pricode;
	unsigned        arity;
	dident		did1 = pd->did;

	arity = DidArity(did1);
	Allocate_Default_Procedure((long) (4 + (nondet?7:0)), did1);
	pricode.vmc = code;
	pd->flags |= EXTERN;
	pri_define_code(pd, VMCODE, pricode);

	if (nondet)
	{
	    Store_4(Try, NO_PORT, arity, 0)
	    aux = code;
	    Store_3(Retry_me_else, (pd->flags & DEBUG_DB)?NEXT_PORT:NO_PORT, aux);
	    *(aux - 1) = (vmcode) code;
	}
	switch(arity)
	{
	    case 0: Store_3(External0, pd, function); break;
	    case 1: Store_3(External1, pd, function); break;
	    case 2: Store_3(External2, pd, function); break;
	    case 3: Store_3(External3, pd, function); break;
	    default: Store_3(External, pd, function);
	}
	Store_i(Code_end)

	Succeed_;
}
