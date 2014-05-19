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
 * SEPIA SOURCE FILE
 *
 * VERSION	$Id: emu.c,v 1.10 2008/11/13 14:44:58 jschimpf Exp $
 */

/*
 * IDENTIFICATION	emu.c
 *
 * DESCRIPTION		the C emulator for SEPIA's abstract machine
 *
 * CONTENTS:		emulc()
 *
 */

 /*
  * INCLUDES:
  */

#define IN_C_EMULATOR	/* before includes ! */
#undef USE_LAST_FLAG

#include "config.h"
#include "sepia.h"
#undef	SP
#undef	TT
#undef	TG
#undef	E
#undef	EB
#undef	GB
#undef	S
#undef	B
#undef	PP

#if defined(_WIN32) && defined(__GNUC__)
/* work around gcc bug */
#undef TagTypeC
#define TagTypeC(item_tag)		((int8) ((item_tag)&0xff))
#endif
#include "types.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "io.h"
#include "emu_export.h"
#include "embed.h"

#include "opcode.h"
#include "database.h"
#include "module.h"
#include "debug.h"
#include "property.h"

#if defined(PROFILE) && !defined(__GNUC__)
/* on sunos5, gcc inserts funny marking labels that confuse the assembler */
#undef MARK
#define MARK
#include <prof.h>
#define Mark_Prof(x)    MARK(x)
#else
#define Mark_Prof(x)
#endif

/*
 * There are three variants of the emulator:
 *	!THREADED		uses switch()
 *	THREADED && POSTPRO	postprocess the assembler output to turn
 *				the emulator into a threaded code one
 *	THREADED && __GNUC__	use gnu's && operator and computed gotos
 *				to make a threaded code emulator
 */

#if defined(THREADED) && defined(__GNUC__)

#define Next_Pp			goto *PP++->emu_addr;
#define Case(Opcode, Oplab)	case Opcode: Oplab: Mark_Prof(Opcode)

#else /* !THREADED || (THREADED && POSTPRO) */

#define Next_Pp			goto _loop_
#define Case(Opcode, Oplab)	case Opcode: Mark_Prof(Opcode)

#endif

#define E_Case(Opcode, Oplab)	case Opcode: Mark_Prof(Opcode)


/*
 * LOCAL TYPES:	the abstract machine code as seen from the emulator
 */

typedef union s_code_item {
    vmcode		inst;
    long		offset;
    pword		*arg;
    pword               *ptr;
    long		nint;
    char		*str;
    float		real;
    dident		did;
    value		val;
    uword		all;		/* as for value */
    long                kernel;         /* for tags */
    pri			*proc_entry;
    int			(*func)();
    union s_code_item	*code;
#if defined(__GNUC__) && defined(THREADED)
    void		*emu_addr;
#endif
} code_item;

typedef code_item	*emu_code;


/*----------------------------------------------------------------------
 * Mapping of abstract machine registers to C variables
 * This is important for performance!
 *----------------------------------------------------------------------*/
/*
 * The PP register: we are using tricks to be able to access
 * it from within sigprof_handler() via Int_Pp
 */
#ifdef __GNUC__
#  ifdef i386
#define Declare_Pp	register emu_code pp asm("%esi");
#define Restore_Pp
#define Import_Pp	pp = (emu_code) g_emu_.pp;
#ifdef HAVE_UCONTEXTGREGS
#define Int_Pp		(((ucontext_t*) context)->uc_mcontext.gregs[REG_ESI])
#else
#define Int_Pp		0
#endif
#  else
#  ifdef __x86_64
#define Declare_Pp	register emu_code pp asm("%r13");
#define Restore_Pp
#define Import_Pp	pp = (emu_code) g_emu_.pp;
#ifdef HAVE_UCONTEXTGREGS
#define Int_Pp		(((ucontext_t*) context)->uc_mcontext.gregs[REG_R13])
#else
#define Int_Pp		0
#endif
#  else
#  ifdef sparc
register emu_code	pp	asm("%g5");
#define Declare_Pp
#define Restore_Pp	pp = (emu_code) g_emu_.pp;
#define Import_Pp
#define Int_Pp		pp
#  else
#  ifdef __alpha__
register emu_code	pp	asm("$12");
#define Declare_Pp
#define Restore_Pp	pp = (emu_code) g_emu_.pp;
#define Import_Pp
#define Int_Pp		pp
#  else
#define Declare_Pp	register emu_code pp;
#define Restore_Pp
#define Import_Pp	pp = (emu_code) g_emu_.pp;
#define Int_Pp		0
#  endif
#  endif
#  endif
#  endif
#else
#define Declare_Pp	register emu_code pp;
#define Restore_Pp
#define Import_Pp	pp = (emu_code) g_emu_.pp;
#define Int_Pp		0
#endif

#define PP		pp
#define Export_Pp	g_emu_.pp = (vmcode *) pp;

#ifdef FEW_REGISTERS	/* leave EB,GB,E,TG in the global structure */

#define Declare_Eb
#define	EB		g_emu_.eb
#define Declare_Gb
#define	GB		g_emu_.gb
#define Declare_E
#define	E		g_emu_.e
#define Declare_Tg
#define	TG		g_emu_.tg

#define Export_B_Sp_Tg_Tt	g_emu_.sp = sp; \
				Export_Pp g_emu_.vm_flags |= EXPORTED;
#define Export_B_Sp_Tg_Tt_Eb_Gb	Export_B_Sp_Tg_Tt
#define Import_Tg_Tt		Import_None
#define Import_B_Sp_Tg_Tt_Eb_Gb	sp = g_emu_.sp; Import_Tg_Tt

#else		/* !FEW_REGISTERS: shadow EB,GB,E,TG in local variables */

#define Declare_Eb	register pword *eb;
#define	EB		eb
#define Declare_Gb	register pword *gb;
#define	GB		gb
#define Declare_E	register pword *e;
#define	E		e
#define Declare_Tg	register pword *tg;
#define	TG		tg

#define Export_B_Sp_Tg_Tt	g_emu_.sp=sp; g_emu_.e=e; g_emu_.tg=tg;\
				Export_Pp g_emu_.vm_flags |= EXPORTED;
#define Export_B_Sp_Tg_Tt_Eb_Gb	g_emu_.eb=eb; g_emu_.gb=gb; Export_B_Sp_Tg_Tt
#define Import_Tg_Tt		tg=g_emu_.tg; Import_None
#define Import_B_Sp_Tg_Tt_Eb_Gb	eb=g_emu_.eb; gb=g_emu_.gb; sp=g_emu_.sp; e=g_emu_.e; Import_Tg_Tt

#endif /* FEW_REGISTERS */

#define Declare_Sp	register pword *sp;
#define	SP		sp
#define Declare_S	register pword *s;
#define	S		s
#define TT		g_emu_.tt
#define B		g_emu_.b

#define Export_All	Export_B_Sp_Tg_Tt_Eb_Gb
#define Import_None	Restore_Pp g_emu_.vm_flags &= ~EXPORTED;
#define Import_All	Import_Pp Import_B_Sp_Tg_Tt_Eb_Gb


 /*
  * EXTERNAL VARIABLE DECLARATIONS:
  */

#if defined(PRINTAM) || defined(LASTPP)
extern uword		*vm_inst_ctr_;
extern char		*vm_inst_flag_;
#endif /* PRINTAM */

#ifdef SAVEDSTATES
extern int	p_restore();
extern int	p_save();
#endif

extern void
		ec_handle_async(void),
		eng_msg_loop(),
		get_job(),
		sch_load_report(),
		end_of_oracle();

extern vmcode	*print_am(register vmcode *code, vmcode **label, int *res, int option);

extern vmcode	
		*bip_error_code_,
		*prolog_error_code_,
		*do_exit_block_code_,
		*fork_unify_code_,
		*sync_it_code_,
		*meta_exit_simple_code_,
		*meta_last_exit_simple_code_,
		*do_call_code_,
		cut_to_code_[],
		comma_body_code_[],
		gc_fail_code_[],
		semic_body_code_[],
		cond_body_code_[],
		cond3_body_code_[],
		softcut5_body_code_[],
		*auto_gc_code_,
		fail_return_env_0_[],
		restore_code_[],
		restore_debug_code_[],
		trace_exit_code_[],
		return_code_[];

extern pri	**default_error_handler_,
		**interrupt_handler_,
		**error_handler_;


 /*
  * EXTERNAL VARIABLE DEFINITIONS: 
  */


 /*
  * DEFINES:
  */

#define Start_Countdown() \
	    Disable_Int(); \
	    EVENT_FLAGS |= COUNT_DOWN; \
	    Enable_Int();
#define Stop_Countdown() \
	    Disable_Int(); \
	    EVENT_FLAGS &= ~COUNT_DOWN; \
	    Enable_Int();

#define MODE_READ	0
#define MODE_WRITE	1

#if (defined(vax) || defined(CHIP) || defined(OBJECTS))
#define SPLIT_SWITCH
/* The main emulator switch is split into two when there are extension	*/
/* instructions or when the C compiler can't handle big switches	*/
#endif

#define ISVar(t)	IsTag(t, TVAR_TAG)

/* This macro creates a module that can be used to make a qualified
 * call to the procedure proc in any module. If the original call was
 * qualified, we can use the unmarked home module, if the original call
 * used the visible procedure, we use the caller module (but it must be
 * marked to enable :/2 to call a possibly local procedure).
 */
#define Make_Lookup_Module(pw, proc) \
	if (PriScope(proc) == QUALI) { \
	    Make_Atom(pw, PriHomeModule(proc)); \
	} else { \
	    Make_Marked_Module(pw, PriModule(proc)); \
	}

#define Make_Marked_Module(pw, mdid) \
	(pw)->val.did = mdid; \
	(pw)->tag.kernel = ModuleTag(mdid);

/*
 * CAUTION: redefinition of this macro should care about coming back
 * in the main loop of the emulator
 */
#define Fail		goto _do_fail_;

#define RetCodeAddr(e)	((pword *) ((pword **) e + 1))
#define ERetCode	*((emu_code *) ((pword **) E + 1))
#define RetEnv(e)	*((pword **) e)
#define ERetEnv		RetEnv(E)
#define Pop_Ret_Code	PP = *((emu_code *) SP);\
			SP = (pword *) (((emu_code *) SP) + 1);
#define Read_Ret_Code	PP = *((emu_code *) SP);
#define Push_Ret_Code(x) SP = (pword *) (((emu_code *) SP) - 1);\
			*((emu_code *) SP) = (x);
#define Push_Ret_Code_To_Eb(x) SP = (pword *) (((emu_code *) EB) - 1);\
			*((emu_code *) SP) = (x);
#define Repush_Ret_Code	SP = (pword *) (((emu_code *) SP) - 1);\
			*((emu_code *)SP) = *(((emu_code *)SP) + 1);
#define Pop_Env		SP = E;\
			E = *((pword **) SP);\
			SP = (pword *)(((pword **) SP) + 1);

#define Push_Env	SP = (pword *) (((pword **) SP) - 1);\
			*((pword **) SP) = E;\
			E = SP;

/*
#define Deterministic	(VM_FLAGS & DET)
#define Set_Det		VM_FLAGS |= DET;
#define Clr_Det		VM_FLAGS &= ~DET;
*/
#define Deterministic	emu_flags 
#define Set_Det		emu_flags = 1;
#define Clr_Det		emu_flags = 0;

#ifdef lint

#define ByteOffsetPlus(pw,off)	((pw) + (off)/sizeof(pword))
#define ByteOffsetMinus(pw,off)	((pw) - (off)/sizeof(pword))

#else /* !lint */

#define ByteOffsetPlus(pw,off)	((pword *) ((int8 *) (pw) + (off)))
#define ByteOffsetMinus(pw,off)	((pword *) ((int8 *) (pw) - (off)))

#endif /* lint */

#define Alloc_Env	Push_Env\
			SP = ByteOffsetMinus(SP, PP++->offset);\
			Check_Local_Overflow

/*#define Move_Pw(s,d)	d->val.all=s->val.all; d->tag.all=s->tag.all;*/
#define Move_Pw(s,d)    *d = *s;

/*
 * move an arbitrary prolog word to a location on the global stack.
 * a local variable is globalized (like Write_local_value)
 * the 'from' argument is modified,
 * the 'to' argument is incremented
 * The 'check' argument is here to make an occur check in Write_value
 */
#define Move_Pw_To_Global_Stack(from, to, check)\
	Dereference_Pw(from)			\
	if (IsRef((from)->tag) && IsLocal(from)) {	\
	    Trail_If_Needed_Eb(from);		\
	    from->val.ptr = to;			\
	    to->val.ptr = to;			\
	    (to++)->tag.kernel = TREF;		\
	} else {				\
	    check				\
	    *(to++) = *from;			\
	}

#define Get_Local(p)	p = ByteOffsetMinus(E, PP++->offset);

#define Get_Temporary(p) p = ByteOffsetPlus(SP, PP++->offset);
#define Get_Temporary_Offs(off, p) \
			p = ByteOffsetPlus(SP, (PP+(off))->offset);

#define Get_Argument(d)	d = (PP++->arg);

#define Dereference_Pw_Tag(pw, t) \
		while(ISRef(((t) = (pw)->tag.kernel)) && pw->val.ptr != pw) {\
		    pw = pw->val.ptr;\
		}

#define Dereference_Pw(pw) \
			while(IsRef(pw->tag) && pw->val.ptr != pw) {\
			    pw = pw->val.ptr;\
			}

#define Set_Val(pw,v)		pw->val.ptr = v;

#define DELAY_SLOT		1	/* first extension */
#define DELAY_INST		1
#define DELAY_BOUND		3

/* bind a standard variable (*pw) to nonvariable v,t */

#define Bind_(pw,v,t) \
			Trail_If_Needed(pw)\
			pw->val.all = v;pw->tag.kernel = t;

#define Bind_Tag(pw,t)	Trail_If_Needed(pw) pw->tag.kernel = t;


/* bind a nonstandard variable (*pw1) to nonvariable v,t	*/

#define Bind_CRef_pw1_Tag(t) \
			tmp1 = (t);\
			goto _bind_nonstandard_; 

#define Bind_CRef_pw1(v,t) \
			pw2 = (pword *)(v); tmp1 = (t); \
			goto _bind_nonstandard_; 


/* bind a standard or nonstandard variable (*pw1) to nonvariable v,t */

#define Bind_Ref_pw1(tvar,v,t) \
                        if(ISVar(tvar)) { \
			   Bind_(pw1,v,t) \
			} else { \
			   Bind_CRef_pw1(v,t) \
			}

#define Bind_Ref_pw1_Tag(tvar,t) \
                        if(ISVar(tvar)) { \
			   Bind_Tag(pw1,t) \
			} else { \
			   Bind_CRef_pw1_Tag(t) \
			}

/* The suffix is needed because float comparison is not bitwise comparison */
#define Unify_Simple_pw1(type,suffix,t)\
			Dereference_Pw_Tag(pw1,t)\
			if(ISVar(t)) {\
			    Bind_(pw1,PP++->all,type) \
			} else if(!IsTag(t,type)) {\
			   if(ISRef(t)) {\
			        Bind_CRef_pw1(PP++->all,type)\
			   } else { Fail }\
			} else if(pw1->val.suffix != PP++->suffix) {\
			        Fail\
			}



/* argument is a register variable that holds the value to cut to
 * and which is destructively changed by this macro!
 * Caution: During resetting of PPB we access data above B. This is
 * only safe as long as async interrupts in the emulator are prevented.
 */
#define Cut_To(Old_B_Reg) {				\
	B.args = (Old_B_Reg);				\
	(Old_B_Reg) = (Top(Old_B_Reg) - 1)->frame.args;	\
	EB = Chp(Old_B_Reg)->sp;			\
	GB = Chp(Old_B_Reg)->tg;			\
	while (LCA >= GB) {				\
	    Export_B_Sp_Tg_Tt;				\
	    do_cut_action();				\
	    Import_Tg_Tt;				\
	}						\
	Cut_To_Parallel(B.args);			\
}

#define Cut_Last(pw) {					\
	B.args = pw = (B.top - 1)->frame.args;		\
	pw = (Top(pw) - 1)->frame.args;			\
	EB = Chp(pw)->sp;				\
	GB = Chp(pw)->tg;				\
}

#ifdef PB_MAINTAINED
#define Cut_To_Parallel(Old_B_Reg) { 			\
	if (Old_B_Reg < PB) {				\
	    Export_B_Sp_Tg_Tt;				\
	    if (cut_across_pb(Old_B_Reg)) {		\
		Import_Tg_Tt;				\
	    } else {					\
		Import_Tg_Tt;				\
		Next_Pp;				\
	    }						\
	}						\
}
#else /* PB_MAINTAINED */
#define Cut_To_Parallel(Old_B_Reg) {			\
	if (Old_B_Reg < PPB) {				\
	    do						\
		PPB = BPar(PPB)->ppb;			\
	    while (Old_B_Reg < PPB);			\
	    Export_B_Sp_Tg_Tt;				\
	    if (cut_public()) {				\
		Import_Tg_Tt;				\
	    } else {					\
		Import_Tg_Tt;				\
		Next_Pp;				\
	    }						\
	}						\
}
#endif /* PB_MAINTAINED */


#ifdef NEW_ORACLE

#define DEBUG_ORACLE

#define O_FROM_ORACLE	1
#define O_NOCREATE	2

/*
#undef O_SHALLOW
#define O_SHALLOW	0
*/

#endif /* NEW_ORACLE */


/* PP points to 1st clause, back_code to 2nd alternative.
 * They are updated according to laternative number n.
 * For the last alternative, back_code is set to NULL.
 */

#define Find_Alternative(n) {					\
	int alt; long tmp2;					\
	for (alt = (n)-1; alt; --alt) {				\
	    tmp2 = back_code->inst;				\
	    if (SameCode(tmp2, Retry_me_else)) {		\
		PP = back_code + 3;				\
		back_code = back_code[2].code;			\
	    } else if (SameCode(tmp2, Retry_me_inline)) {	\
		PP = back_code + 4;				\
		back_code = back_code[2].code;			\
	    } else if (SameCode(tmp2, Retry) || SameCode(tmp2,Retry_inline)) {	\
		PP = back_code[2].code;				\
		back_code = back_code + 3;			\
	    } else if (SameCode(tmp2, Trust) || SameCode(tmp2,Trust_inline)) {	\
		PP = back_code[2].code;				\
		back_code = (emu_code) 0;			\
		break;						\
	    } else if (SameCode(tmp2, Trust_me)) {		\
		PP = back_code + 2;				\
		back_code = (emu_code) 0;			\
		break;						\
	    } else if (SameCode(tmp2, Trust_me_inline) {	\
		PP = back_code + 3;				\
		back_code = (emu_code) 0;			\
		break;						\
	    } else if (SameCode(tmp2, Retrylab)) {		\
		PP = back_code[2].code;				\
		back_code = back_code[3].code;			\
	    } else {						\
		p_fprintf(current_err_,				\
		    "INTERNAL ERROR following oracle\n");	\
	    }							\
	}							\
}

/* on the PDL there are pointers (low bit 0)
 * and encoded counters (<unifications left> * 2 + 1)
 */
#define Pdl_Push_Pair(pw1, pw2) \
	SP = (pword *) (((pword **) SP) - 2);\
	*((pword **) SP) = pw1;\
	*(((pword **) SP) + 1) = pw2;\
	Check_Local_Overflow

#define Pdl_Push_Frame(pw1, pw2, arity) \
	SP = (pword *) (((pword **) SP) - 3);\
	*((word *) SP) = ((arity) << 1) - 3;\
	*(((pword **) SP) + 1) = pw1;\
	*(((pword **) SP) + 2) = pw2;\
	Check_Local_Overflow

/* get next pair of pointers from the (non-empty) PDL	*/

#define Pdl_Next(pw1, pw2, arity) \
	arity = *((word *) SP);\
	if (arity & 1) {	/* a frame */	\
		pw1 = ((pword *) *(((pword **) SP) + 1)) + 1;\
		*(((pword **) SP) + 1) = pw1;\
		pw2 = ((pword *) *(((pword **) SP) + 2)) + 1;\
		*(((pword **) SP) + 2) = pw2;\
		if ((arity -= 2) > 1)\
		    *((word *) SP) = arity;\
		else\
		    SP = (pword *) (((pword **)SP) + 1);\
	} else {		/* a pair */	\
	    pw1 = ((pword *) arity) + 1;\
	    pw2 = ((pword *) *(((pword **) SP) + 1)) + 1;\
	    SP = (pword *) (((pword **)SP) + 2);\
	}

/* Facility for stopping whenever TG crosses tg_trap */
#ifdef DEBUG_TRAP_TG
pword *tg_trap = MAX_U_WORD;	/* set this via dbx */
int tg_above_trap = 0;		/* true while TG is above tg_trap */
#define Trap_Tg \
    if (tg_above_trap) {					\
	if (TG <= tg_trap) { tg_above_trap = 0; emu_break(); }	\
    } else {							\
	if (TG > tg_trap) { tg_above_trap = 1; emu_break(); }	\
    }
#else
#define Trap_Tg
#endif

/* brute force check of the whole global stack after every failure */
#ifdef DEBUG_CHECK_GLOBAL_STACK
#define Debug_Check_Global Export_B_Sp_Tg_Tt check_global(); Import_None
#else
#define Debug_Check_Global
#endif

#if defined(PRINTAM) || defined(LASTPP)

#define MAX_BACKTRACE 1024

static vmcode      *dummy_l = NULL;	/* dummy arg for print_am()  */
static int 	    dummy_r;		/* dummy arg for print_am()  */
static emu_code	stop_address = 0; /* for address breakpoints in the emulator */
vmcode *ec_backtrace[MAX_BACKTRACE];	/* record recent PP values   */
int bt_index = 0;
int bt_max = MAX_BACKTRACE;

#define Begin_Execution(iptr)				\
    if(VM_FLAGS & (TRACE | STATISTICS)) {			\
	if(VM_FLAGS & STATISTICS)				\
	    vm_inst_ctr_[iptr->inst]++;			\
	if(VM_FLAGS & TRACE)					\
	    (void) print_am((vmcode *) iptr, &dummy_l, &dummy_r, 0);\
    }							\
    if (iptr == stop_address) {emu_break();}		\
    ec_backtrace[bt_index] = (vmcode *) iptr;		\
    bt_index = (bt_index + 1) % MAX_BACKTRACE;		\
    Trap_Tg

#else /* PRINTAM */

#define Begin_Execution(iptr)

#endif /* PRINTAM */


/*
 * stack overflow handling
 */

#define Check_Local_Overflow					\
	if (SP <= g_emu_.sp_limit) {			\
	    Export_B_Sp_Tg_Tt					\
	    if (local_ov()) goto _local_control_overflow_;	\
	    Import_None						\
	}

#define Check_Control_Overflow					\
	if (B.args >= g_emu_.b_limit) {			\
	    Export_B_Sp_Tg_Tt					\
	    if (control_ov()) goto _local_control_overflow_;	\
	    Import_None						\
	}

#ifdef WIPE_FREE_GLOBAL
#define Wipe(From, To) { pword *_p; \
    	for(_p=(From);_p<(To);++_p) {_p->val.ptr=0; _p->tag.kernel=TEND;} }
#else
#define Wipe(From, To)
#endif


/* These macros can only be used at the end of the abstract instruction */
#define Handle_Events_Call	if (EventPending) goto _handle_events_at_call_;
#define Handle_Events_Return	if (EventPending) goto _handle_events_at_return_;

#define Reset_Unify_Exceptions	MU = (pword *) 0;


/*
 * Interrupts while inside the emulator
 *
 * Interrupts inside the emulator are problematic because the abstract machine
 * stack pointers may be in shadow registers of the emulator (indicated by
 * the EXPORTED bit being reset). Recursive emulators can therefore not be
 * initialised properly. Therefore, when the EXPORTED bit is reset,
 * signals cannot be handled asynchronously and have to be treated like.
 * synchronous events. This is done by posting integers to the event queue
 * and setting the EVENT_POSTED bit in the EVENT_FLAGS register.
 * 
 * The emulator is responsible for checking the EVENT_FLAGS condition
 * bit regularly and calling handlers when it is set.
 * When EXPORTED is set (i.e. the stack pointers are in the global variables),
 * asynchronous interrupt handlers are called directly by _break()
 * or delayed_break().
 *
 * Optimisation: To avoid an extra check of EVENT_FLAGS, a global stack
 * overflow is simulated as well (by setting TG_SL to 0).
 * The event handling routine then checks if we had a true
 * overflow or a faked one, and takes the appropriate action.
 * While TG_SL may have a false value, its true one is always in TG_SLS
 * We have to be careful not to lose this faked overflow, e.g. by resetting
 * TG_SL from a control frame. Use the appropriate macros!
 *
 * When the control flow leaves the emulator (e.g. by calling some C function),
 * the shadow registers have to be exported using an appropriate
 * Export_... macro. If the function is allowed to modify the abstract
 * machine registers, they also must be imported after returning.
 */

#define Poll_Interrupts()			\
	if (EVENT_FLAGS & DEL_IRQ_POSTED) {	\
	    Export_B_Sp_Tg_Tt			\
	    ec_handle_async();			\
	    Import_None				\
	}


/*
 * FUNCTION NAME:	emulc()
 *
 * PARAMETERS:		m		the abstract machine descriptor
 *					(currently still in g_emu_)
 */


func_ptr
ec_emulate(void)		/* (struct machine *m) */
{
    Declare_Pp
    Declare_Sp
    Declare_S
    register pword *pw1;
    Declare_Tg
    Declare_E
    Declare_Eb
    Declare_Gb
    pword *pw2;
    pword *pw3;
    register int emu_flags;
    register uword i;		/* unsigned !		       */
    register word tmp1;		/* signed !			*/
    control_ptr	b_aux;
    dident	val_did;
    int		err_code;
    pword	scratch_pw;	/* scratch space to have a pointer to a pword */
    pword	*pdl;
    pri		*proc, *procb;
    emu_code	back_code;
    double	dbl_res;

#ifdef lint 
    scratch_pw.tag.kernel = TNIL;
    proc = (pri *) 0;
    err_code = 0;
#endif

#if defined(__GNUC__) && defined(THREADED)
    if (!op_addr[0])
    {
	i = 0;
#include "emu_op_addr.h"
	A[0].val.nint = PSUCCEED;
	return (func_ptr) 0;
    }
#endif

    Import_All;			/* B Sp Tg Tt EB Gb E PP */

/*
 * initialize emulator auxiliaries
 */
    Set_Det;			/* should be imported from global vmflags */

    Check_Control_Overflow	/* for the invocation frame */
    Next_Pp;


/*******************************************************************
 * Error in a regular goal: Construct the culprit goal structure
 * from the argument registers.
 *******************************************************************/

_recomp_err_:
	    err_code = RECOMP_FAILED;
#ifdef PRINTAM
	    emu_break();
#endif
	    FO = (char *) 0;
	    val_did = d_.emulate;
	    /* goto _regular_err_; */

_regular_err_:	/* (err_code, val_did), args in arg regs	*/
	    tmp1 = DidArity(val_did);
	    if (tmp1 == 0) {
		Make_Atom(&A[2], val_did);
	    } else {
		S = TG;		/* build goal structure	*/
		TG += tmp1 + 1;
		S->val.did = val_did;
		(S++)->tag.kernel = TDICT;
		pw1 = &A[1];
		for(i = 0; i < tmp1; i++) {
		    pw2 = pw1++;
		    Move_Pw_To_Global_Stack(pw2,S, ;)
		}
		Make_Struct(&A[2], TG - tmp1 - 1);
	    }
	    pw1 = TG++;
	    Check_Gc
	    Make_Var(pw1);
	    Make_Ref(&A[3], pw1);
	    /* The culprit is known to be a kernel predicate, e.g.
	     * block/3, exit_block/1, or emulate/0.
	     * Lookup module can therefore be sepia_kernel.
	     */
	    Make_Atom(&A[4], d_.kernel_sepia);

_regular_err_2_: /* (err_code), goal A2, context module A3, lookup module A4 */
	    Make_Integer(&A[1], -err_code);
	    Push_Ret_Code(PP) 
	    Check_Local_Overflow
	    PP = (emu_code) prolog_error_code_;
	    Next_Pp;


/******************************************************************
 * The diff routine is used to implement the builtins
 *
 *			==/2 	\==/2 	~=/2	\==/3
 * not unifiable	fail	succeed	succeed	succeed with []
 * identical		succeed	fail	fail	fail
 * uncertain		succeed	succeed	delay	succeed with list
 *
 * It works on the terms whose addresses are held by pw1 and pw2,
 * In addition, ~=/2 expects PP to point behind a BI_Inequality,
 * and \==/3 expects PP to point to the last word of a BI_NotIdentList.
 * The value matching instructions are handled like ==/2..
 ******************************************************************/

#define IsIdenticalProc(proc) (proc == identical_proc_)
#define IsNotIdenticalProc(proc) (proc == not_identical_proc_)
#define IsInequalityProc(proc) (proc == inequality_proc_)
#define IsNotIdentListProc(proc) (proc == not_ident_list_proc_)

_diff_:					/* (pw1, pw2, [PP,] proc) */
    Mark_Prof(_diff_)
    pdl = SP;
_do_diff_:
    Dereference_Pw_Tag(pw1,tmp1)	/* dereference the two objects */
    Dereference_Pw(pw2)
    if(pw1 == pw2) goto _diff_cont_;	/* takes care of identical */
                                         /* normal variable */
    if (IsTag(tmp1, TUNIV))
    {
       Trail_Tag(pw1)
       pw1->tag.kernel = TREF;
       pw1->val.ptr    = pw2;
       goto _diff_cont_;
    }
    else if (IsTag(pw2->tag.kernel, TUNIV))
    {
       Trail_Tag(pw2)
       pw2->tag.kernel = TREF;
       pw2->val.ptr    = pw1;
       goto _diff_cont_;
    };

    if (ISRef(tmp1)) 
    {			       /* the first is a normal or cdt variable */
       if(IsRef(pw2->tag)) 
       {		       /* the second as well */ 
 	  if (pw1->val.ptr == pw2->val.ptr)
 	  {
 	     goto _diff_cont_; /* identical cdt var */
 	  }
 	  /* else variables not identical */
	   if (IsIdenticalProc(proc)) 
	       { SP = pdl; Fail; }
	   if (IsNotIdenticalProc(proc)) 
	       { SP = pdl; Next_Pp; }
	  Push_var_delay_unif(pw2->val.ptr, pw2->tag.kernel);
	  Push_var_delay_unif(pw1->val.ptr,pw1->tag.kernel);
	  goto _diff_delay_;
       } 
       if (IsIdenticalProc(proc)) 
	   { SP = pdl; Fail; }
       if (IsNotIdenticalProc(proc)) 
	   { SP = pdl; Next_Pp; }
       Push_var_delay(pw1->val.ptr,pw1->tag.kernel);
       goto _diff_delay_;
    } 
    else if (IsRef(pw2->tag)) 
    {		               /* only the 2nd is a variable*/
       if (IsIdenticalProc(proc)) 
	   { SP = pdl; Fail; }
       if (IsNotIdenticalProc(proc)) 
	   { SP = pdl; Next_Pp; }
       Push_var_delay(pw2->val.ptr,pw2->tag.kernel);
       goto _diff_delay_;
    } 
    else if (TagTypeC(tmp1) != TagType(pw2->tag))
    {
       goto _diff_different_;		/* tags differ */
    }
    else if (ISSimple(tmp1))		/* both are simple  */
    {
	if (SimpleEq(tmp1, pw1->val, pw2->val))
	    goto _diff_cont_;
	else
 	    goto _diff_different_;
    }
    else
    {
	pw1 = pw1->val.ptr;
	pw2 = pw2->val.ptr;
	if (pw1 == pw2) goto _diff_cont_;	/* pointers identical */

	if (TagTypeC(tmp1) > TCOMP)		/* strings, bignums, etc */
	{
	    if (IsTag(tmp1,TSTRG))		/* strings */
	    {
		Compare_Strings(pw1, pw2, err_code);
		if (err_code >= 0)		/* they are not the same strings */
		    goto _diff_different_;
		else
		    goto _diff_cont_;
	    }
	    Export_B_Sp_Tg_Tt
	    err_code = tag_desc[TagTypeC(tmp1)].equal(pw1, pw2);
	    Import_None
	    if (err_code) goto _diff_cont_;
	    else goto _diff_different_;
	} 
	else					/* the compound terms */
	{
	    Poll_Interrupts();			/* because we might be looping */

	    if (IsTag(tmp1,TLIST))		/* lists */
	    {	
_diff_list_:
		Pdl_Push_Pair(pw1, pw2);
		goto _do_diff_;
	    } 
	    else /* if (IsTag(tmp1,TCOMP)) */
	    {
	       if (pw1->val.did != (pw2++)->val.did) 
	       {				/* different functors */
		  /* (arity check implicit) */
		  goto _diff_different_;		
	       }

	       tmp1 = DidArity((pw1++)->val.did);	/* their arity */
	       /* at this point, pw1 and pw2 point to the first subterm */
	       switch(tmp1) 
	       {
	       case 0: goto _diff_cont_;	/* null arity: they unify */
	       case 1: goto _do_diff_;		/* arity 1: directly unify subterms*/ 
	       case 2: goto _diff_list_;	/* 2: we do not push the integer on */
						/* the pdl */
	       default:
		    Pdl_Push_Frame(pw1, pw2, tmp1);
		    goto _do_diff_;
	       }
	    }
	}
    }

_diff_cont_:				/* the terms are equal (so far) */
    if(pdl > SP) {
	Pdl_Next(pw1, pw2, tmp1);
	goto _do_diff_;			/* continue */
    }
    if (!IsIdenticalProc(proc)) 
	Fail
    Next_Pp;
_diff_different_:			/* the terms are different */
    SP = pdl;			/* remove PDL */
    if (IsIdenticalProc(proc)) 
	Fail
    else if (IsNotIdentListProc(proc)) 
    {
	Get_Argument(pw1)		/* unify last argument with [] */
	Dereference_Pw(pw1)
	if (IsVar(pw1->tag))
	{
	    Trail_If_Needed(pw1)
	    pw1->tag.kernel = TNIL;
	    Next_Pp; 
	}
	scratch_pw.tag.kernel = TNIL;
	pw2 = &scratch_pw;
	goto _unify_;			/* (pw1, pw2) */
    }
    Kill_DE;	/* this is for BI_Inequality only! */
    Next_Pp; 
_diff_delay_:				/* (SV, proc, PP points behind args) */
    SP = pdl;				/* remove PDL and delay */
    if (IsInequalityProc(proc)) 
    {
	if (!DE)			/* make a suspension structure */
	{
	    val_did = PriDid(proc);
	    DE = pw1 = TG;
	    TG += SUSP_SIZE;
	    Init_Susp_Header(pw1, proc);
	    Init_Susp_State(pw1, PriPriority(proc));
	    Make_Struct(&pw1[SUSP_GOAL], TG);	/* goal */
	    Make_Atom(&pw1[SUSP_MODULE], PriModule(proc));
	    Make_Atom(TG, val_did);
	    S = TG+1;
	    TG += 3;
	    pw1 = PP[-2].ptr;
	    Move_Pw_To_Global_Stack(pw1, S, ;);
	    pw1 = PP[-1].ptr;
	    Move_Pw_To_Global_Stack(pw1, S, ;);
	    Check_Gc
	}
	err_code = PDELAY | PDELAY_BOUND;
	goto _ndelay_de_sv_;		/* (proc, de, sv, args?) */
    }
    else /* IsNotIdentListProc(proc) */
    {
	Get_Argument(pw1)		/* unify last argument with SV list */
	Dereference_Pw(pw1)
	if (IsVar(pw1->tag))
	{
	    Trail_If_Needed(pw1)
	    pw1->val.ptr = SV;
	    pw1->tag.kernel = TLIST;
	    SV = (pword *) 0;
	    Next_Pp; 
	}
	scratch_pw.val.ptr = SV;
	scratch_pw.tag.kernel = TLIST;
	SV = (pword *) 0;
	pw2 = &scratch_pw;
	goto _unify_;			/* (pw1, pw2) */
    }


/******************************************************************
 * Unification coded in the emulator and using the local stack to handle
 * recursion. It either fails or succeeds, but in both cases it resumes
 * the loop of the emulator.
 * It unifies the prolog words whose addresses are held by pw1 and pw2.
 ******************************************************************/

_unify_:
    Mark_Prof(_unify_)
    pdl = SP;
_do_unify_:
    Dereference_Pw_Tag(pw1,tmp1)	/* dereference the two objects */
    Dereference_Pw(pw2)
    if(ISVar(tmp1)) {			/* the first is a free variable */
	if(IsVar(pw2->tag)) {		/* the second as well */
	    if (pw1 < pw2)
		if (pw1 < TG)
		{
		    Trail_If_Needed(pw2);
		    pw2->val.ptr = pw1;
		}
		else
		{
		    Trail_If_Needed_Eb(pw1);
		    pw1->val.ptr = pw2;
		}
	    else if (pw1 > pw2)
		if (pw2 < TG)
		{
		    Trail_If_Needed(pw1);
		    pw1->val.ptr = pw2;
		}
		else
		{
		    Trail_If_Needed_Eb(pw2);
		    pw2->val.ptr = pw1;
		}
	    else goto _unify_ok_;	/* identical variables */
	} else {		/* only the 1st is free */
	    if (IsRef(pw2->tag)) {
	        Trail_If_Needed(pw1);
		pw1->val.ptr = pw2->val.ptr;
	    } else {
		Occur_Check_Read(pw1, pw2->val, pw2->tag, goto _unify_fail_)
	        Bind_(pw1, pw2->val.all, pw2->tag.kernel)	/* bind it */
	    }
	}
	goto _unify_ok_;
    } else if (IsVar(pw2->tag)) {			/* only the 2nd is free */
        if (ISRef(tmp1)) {
	    Trail_If_Needed(pw2);
	    pw2->val.ptr = pw1->val.ptr;
	} else {
	    Occur_Check_Read(pw2, pw1->val, pw1->tag, goto _unify_fail_)
	    Bind_(pw2, pw1->val.all, tmp1)	/* bind it */
	}
    } else if (ISRef(tmp1)) {
	pw1 = pw1->val.ptr;			/* temporary, because of BIUnify */
	if (IsRef(pw2->tag)) 			/* CRef = CRef */
	{
	    pw2 = pw2->val.ptr;			/* temporary */
	    if (pw1 == pw2) goto _unify_ok_;	/* identical */
	    /* call bind_c() */
	}
	else					/* CRef = Nonvar */
	{
_unify_bind_cref_nvar_:				/* (pw1, tmp1, pw2) */
	    Occur_Check_Read(pw1, pw2->val, pw2->tag, goto _unify_fail_)
	    if (IsTag(tmp1, TNAME)) {
		Trail_Tag_If_Needed_Gb(pw1);
		*pw1 = *pw2;
		goto _unify_ok_;
	    } else if (IsTag(tmp1, TMETA)) {
		Trail_Tag_If_Needed_Gb(pw1);
		*pw1 = *pw2;
		Update_MU(pw1)
		goto _unify_ok_;
	    }
	    /* else call bind_c() */
	}
	Export_B_Sp_Tg_Tt_Eb_Gb
	if (bind_c(pw1, pw2, &MU) == PSUCCEED) {
	    Import_Tg_Tt
	    goto _unify_ok_;
	} else {
	    Import_Tg_Tt
	    goto _unify_fail_;
	}
    } else if (IsRef(pw2->tag)) {		/* Nonvar = CRef */
	tmp1 = pw2->val.nint;			/* ->val temporary */
	pw2 = pw1;
	pw1 = (pword *) tmp1;
	tmp1 = pw1->tag.kernel;
	goto _unify_bind_cref_nvar_;		/* (pw1, tmp1, pw2) */

    } else if (TagTypeC(tmp1) != TagType(pw2->tag)) {
	goto _unify_fail_;		/* different tags --> fail */

    } else if (ISSimple(tmp1)) {	/* simple type? if yes ..*/
	if (SimpleEq(tmp1, pw1->val, pw2->val))
	    goto _unify_ok_;		/* nil or same values */
	else
	    goto _unify_fail_;
    }
    else
    {
	pw1 = pw1->val.ptr;			/* get the pointers */
	pw2 = pw2->val.ptr;
	if (pw1 == pw2) goto _unify_ok_;	/* identical pointers */

	if (TagTypeC(tmp1) > TCOMP)		/* string, bignum etc */
	{
	    if (IsTag(tmp1,TSTRG)) {
		Compare_Strings(pw1, pw2, err_code);
		if(err_code >= 0)		/* they do not match	*/
		    goto _unify_fail_;
		else
		    goto _unify_ok_;
	    }
	    Export_B_Sp_Tg_Tt
	    err_code = tag_desc[TagTypeC(tmp1)].equal(pw1, pw2);
	    Import_None
	    if (err_code) goto _unify_ok_;
	    else goto _unify_fail_;
	}
	else					/* the compound terms */
	{
	    Poll_Interrupts();			/* because we might be looping */
	    if (IsTag(tmp1,TLIST)) {		/* lists */
_unify_list_:
		Pdl_Push_Pair(pw1, pw2);
		goto _do_unify_;		/* but first, the heads */

	    } else { /* if (IsTag(tmp1,TCOMP))  */ /* we have structures */
		if (pw1->val.did != (pw2++)->val.did)
		    goto _unify_fail_;	/* different functors --> fail */

		tmp1 = DidArity((pw1++)->val.did);	/* their arity */
		/* at this point, pw1 and pw2 point to the first subterm */
		switch(tmp1) {
		    case 0: goto _unify_ok_;	/* null arity: they unify */
		    case 1: goto _do_unify_;	/* directly unify subterms */ 
		    case 2: goto _unify_list_;	/* the same as a list	 */
		    default:
			Pdl_Push_Frame(pw1, pw2, tmp1);
			goto _do_unify_;
		}
	    }
	}
    }
_unify_ok_:
    if (pdl <= SP) {
	Occur_Check_Boundary(0)
	Next_Pp;			/* if PDL empty, unification succeeds */
    }
    Pdl_Next(pw1, pw2, tmp1);		/* else get next pair and unify	*/
    goto _do_unify_;

_unify_fail_:				/* if the unification fails */
    Occur_Check_Boundary(0)		/* reset the occur check */
    SP = pdl;			/* remove the PDL */
    Fail;				/* and initiate backtracking */


/*
 * Bind a nonstandard variable (*pw1) to the nonvariable term with tag tmp1
 * and value pw2, then fail or continue with the next instruction.
 * TMETA and TNAME are handled here for efficiency, the rest is given to bind_c()
 */

_bind_nonstandard_:			/* *pw1 = (pw2,tmp1) */
    Mark_Prof(_bind_nonstandard_)
    if (IsTag(pw1->tag.kernel, TNAME)) {
	Trail_Tag_If_Needed_Gb(pw1);
	pw1->val.ptr = pw2;
	pw1->tag.kernel = tmp1;
    } else if (IsTag(pw1->tag.kernel, TMETA)) {
	Trail_Tag_If_Needed_Gb(pw1);
	pw1->val.ptr = pw2;
	pw1->tag.kernel = tmp1;
	Update_MU(pw1)
    } else {
	scratch_pw.val.ptr = pw2;
	scratch_pw.tag.kernel = tmp1;
	Export_B_Sp_Tg_Tt_Eb_Gb
	err_code = bind_c(pw1, &scratch_pw, &MU);
	Import_Tg_Tt
	if (err_code == PFAIL) { Fail; }
    }
    Next_Pp;



/*****************************************************************
     BIP Result management (new abstract machine instr version)
******************************************************************/

/*
 * Construct a goal structure for a builtin that is compiled into one
 * of the I_BI_Xxx instructions, e.g. bi_add(arg,arg,uninit_arg,mask).
 * We assume that PP points behind the instruction, i.e. behind mask.
 * Mask describes the preceding argument words, 2 bits for each argument:
 *
 *     mask   code contains
 *	0	pointer to argument register
 *	1	pointer to uninitialised argument register
 *	2	32-bit integer
 *	3	module did
 * 	?	possible extension: pri (for make_suspension/4)
 *
 * We assume that these predicates have arity>0 and are not tools.
 * We also assume no local stack variables (otherwise need to globalise).
 * CAUTION: this macro also materialises output variables for
 * "uninitialised output" arguments, and stores a ref to them in the
 * output register. This may clobber an input register, which is no
 * problem as long as they are always last and the input is copied first.
 */

#define Push_Bip_Goal(_did,_i,_mask) { \
	(_i) = DidArity(_did)+1;\
	TG->val.did = (_did);\
	TG++->tag.kernel = TDICT;\
	(_mask) = PP[-1].nint;\
	do {\
	    switch((_mask) & 3) {\
	    case 0:\
		*TG = *(PP[-(_i)].ptr);\
		break;\
	    case 1:\
		PP[-(_i)].ptr->val.ptr=TG; PP[-(_i)].ptr->tag.kernel=TREF;\
		TG->val.ptr=TG; TG->tag.kernel=TREF;\
		break;\
	    case 2:\
		TG->val.nint = PP[-(_i)].nint; TG->tag.kernel=TINT;\
		break;\
	    case 3:\
		Make_Marked_Module(TG, PP[-(_i)].did);\
		break;\
	    }\
	    ++TG; (_mask) >>= 2;\
	} while (--(_i)>1);\
}

#define Push_Dummy_Results(_did,_i,_mask) { \
	(_i) = DidArity(_did)+1;\
	(_mask) = PP[-1].nint;\
	while ((_mask) && (_i)>1) {\
	    switch((_mask) & 3) {\
	    case 1:\
		PP[-(_i)].ptr->val.ptr=TG; PP[-(_i)].ptr->tag.kernel=TREF;\
		TG->val.ptr=TG; TG++->tag.kernel=TREF;\
		break;\
	    }\
	    (_mask) >>= 2; --(_i);\
	}\
}


_nbip_res_:	     /* (err_code,proc), args at *PP[-arity-1..-2] */
	Mark_Prof(_nbip_res_)
	Occur_Check_Boundary(0)
	if (err_code == PSUCCEED)
	{
_nbip_succeed_:
	    Reset_DE;	/* demons are responsible to Kill_DE if appropriate */
	    Next_Pp;
_nbip_kill_succeed_:
	    Kill_DE;
	    Next_Pp;
	}
	else if (err_code == PFAIL)
	{
_nbip_fail_:
	    Fail;
	}
	else if ((err_code & ~PDELAY_MASK) == PDELAY)
	{

_npdelay_:				/* (err_code, proc)	*/
	    if (!(GlobalFlags & CORTN))
	    {
		SV = (pword *) 0;
		err_code = INSTANTIATION_FAULT;
		goto _nbip_err_;
	    }
_npdelay_always_:			/* (err_code, proc)	*/
	    Mark_Prof(_npdelay_always_)
	    val_did = PriDid(proc);
	    if (!DE)			/* make a suspension structure */
	    {
		DE = pw1 = TG;
		TG += SUSP_SIZE;
		Init_Susp_Header(pw1, proc);
		Init_Susp_State(pw1, PriPriority(proc));
		Make_Struct(&pw1[SUSP_GOAL], TG);	/* goal */
		Make_Atom(&pw1[SUSP_MODULE], PriModule(proc));
		Push_Bip_Goal(val_did, i, tmp1)
	    }
	    else
	    {
		/* When we redelay a builtin that uses uninitialised output convention,
		 * we have to create a dummy result, which can be unified (without
		 * any effect) with the caller's result argument by the subsequent
		 * get_value instruction(s).
		 */
		Push_Dummy_Results(val_did, i, tmp1)
	    }
	    Check_Gc

	    /*
	     * DE now points to the suspension
	     * Link it to the suspending variables
	     */

	    if (err_code & PDELAY_MASK)	/* delay on argument(s) 1-3 */
	    {
		Export_B_Sp_Tg_Tt_Eb_Gb
		tmp1 = DidArity(PriDid(proc)) + 1;
		if (err_code & (PDELAY_1 & PDELAY_MASK)) {
		    pw1 = &DE[SUSP_GOAL].val.ptr[1];
		    Dereference_Pw(pw1)
		    tmp1 = insert_suspension(pw1, 1, DE, DELAY_SLOT);
		    if (tmp1 < 0) {
			err_code = -tmp1;
			goto _ndelay_err_;
		    }
		}
		if (err_code & (PDELAY_2 & PDELAY_MASK)) {
		    pw1 = &DE[SUSP_GOAL].val.ptr[2];
		    Dereference_Pw(pw1)
		    tmp1 = insert_suspension(pw1, 1, DE, DELAY_SLOT);
		    if (tmp1 < 0) {
			err_code = -tmp1;
			goto _ndelay_err_;
		    }
		}
		if (err_code & (PDELAY_3 & PDELAY_MASK)) {
		    pw1 = &DE[SUSP_GOAL].val.ptr[3];
		    Dereference_Pw(pw1)
		    tmp1 = insert_suspension(pw1, 1, DE, DELAY_SLOT);
		    if (tmp1 < 0) {
			err_code = -tmp1;
			goto _ndelay_err_;
		    }
		}
		Import_Tg_Tt
	    }
	    else	/* suspending_variables points to a list of	*/
	    {		/* pointers to suspending variables		*/
_ndelay_de_sv_:		/* (proc,de,sv,args) */
		pw2 = SV;
		Export_B_Sp_Tg_Tt_Eb_Gb
		while (pw2)
		{
		    pw1 = pw2[0].val.ptr;
		    Dereference_Pw(pw1)
		    tmp1 = insert_suspension(pw1,
			    err_code & PDELAY_BOUND ? DELAY_BOUND: DELAY_INST,
			    DE, DELAY_SLOT);
		    if (tmp1 < 0) {
			err_code = -tmp1;
			goto _ndelay_err_;
		    }
		    if (!IsList(pw2[1].tag))
			break;
		    pw2 = pw2[1].val.ptr;
		}
		Import_Tg_Tt
		SV = (pword *) 0;
	    }
	    if (Tracing && AnyPortWanted && !SuspDebugInvoc(DE))
	    {
		/* We don't currently have a way to trace re-delays */
		Set_Susp_DebugInvoc(DE, NINVOC);
		++NINVOC;
		/* only if the port is of interest, raise the debug event */
		if (Tracing && PortWanted(DELAY_PORT) && OfInterest(PriFlags(((pri*)proc)), NINVOC-1, DLevel(TD)+1, 0)) {
		    if (DBG_DELAY_INVOC == 0) {
			DBG_DELAY_INVOC = NINVOC-1;
		    }
		    err_code = -(DEBUG_SUSP_EVENT);
_ndelay_err_:	/* (err_code,proc,DE) */
		    scratch_pw = DE[SUSP_GOAL];
		    Reset_DE;
		    goto _nbip_err_goal_;
		}
	    }
	    Reset_DE;
	    Next_Pp;
	}
	else if (err_code == PTHROW)
	{
	    Reset_DE;
	    PP = (emu_code) do_exit_block_code_; /* Ball should be in A[1] */
	    Next_Pp;
	}
	else if (err_code > 0)
	{
	    err_code = ILLEGAL_RETURN;
	}
	/* goto _nbip_err_; */

/*******************************************************************
 * Builtin returned an error code
 *******************************************************************/

_nbip_err_:		/* (err_code, proc), args at *PP[-arity-1..-2] */
	Mark_Prof(_nbip_err_)
	Kill_DE;
	err_code = -err_code;
	if (PriFlags(proc) & TOOL)
	{
	    (void) ec_panic("Assertion Failed", "Emulator, nbip_error");
	}

	if (!(procb = error_handler_[err_code]))	/* get the handler */
	    procb = error_handler_[0];

	if (procb->did == d_.true0 && procb->module_ref == d_.kernel_sepia) {
	    Next_Pp;
	}
	else if (procb->did == d_.fail && procb->module_ref == d_.kernel_sepia)
	{
	    Fail
	}
	else
	{

	/* Build culprit goal (before saving argument registers!) */
	    val_did = PriDid(proc);
	    if (DidArity(val_did) > 0) {
		Make_Struct(&scratch_pw, TG);
		Push_Bip_Goal(val_did, i, tmp1);
	    } else {
		Make_Atom(&scratch_pw, val_did);
	    }

_nbip_err_goal_:	/* (err_code, proc,scratch_pw) */
	/* create an exception frame to be able to restore the machine
	 * state partially on SUCCESSful return from error handler.
	 * ( the handler call should behave like a builtin call,
	 * i.e. being determinate, preserving arg regs and DET )
	 * If handler succeeds, restoring is done by Continue_after_exception.
	 * If handler fails, Refail pops the frame and fails again.
	 * MU is saved/restored and WP (priority) is set to 1 in order to
	 * make the exception handler not interfere with waking.
	 */
	    Push_Ret_Code(PP)
	    pw1 = B.args;
	    Exception(pw1)->sp = SP;
	    Exception(pw1)->tg = TG;
	    Exception(pw1)->tt = TT;
	    Exception(pw1)->e = E;
	    Exception(pw1)->ld = LD;
	    Exception(pw1)->eb = EB;
	    Exception(pw1)->gb = GB;
	    EB = SP;
	    GB = TG;
	    Push_Witness
	    Check_Gc;
	    Exception(pw1)->flags = emu_flags;
	    Exception(pw1)->de = DE;
#define STRICT_EXCEPTION
#ifdef STRICT_EXCEPTION
	    Exception(pw1)->mu = MU;
	    MU = (pword *) 0;
	    Exception(pw1)->wp = WP;
	    Set_WP(1);
#endif
	    Save_Tg_Soft_Lim(Exception(pw1)->tg_soft_lim);
	    pw1 = (pword *) (Exception(pw1) + 1);
	    pw2 = &A[1];	/* save arguments	*/
	    for(i = 1; i < NARGREGS; i++) {
		*pw1 = *pw2++;
		if((pw1++)->tag.kernel == TEND)
		    break;
	    }
	    Top(pw1)->backtrack = exception_fail_code_;
	    Top(pw1)->frame.exception = B.exception;
	    B.top = Top(pw1) + 1;
	    Check_Control_Overflow

	/* Now call syserror(Err, Goal, ContextMod, LookupMod) */
	    Make_Integer(&A[1], err_code);	/* error code */
	    A[2] = scratch_pw;			/* culprit goal */
	    Make_Marked_Module(&A[3], PriModule(proc)); /* context module */
	    Make_Lookup_Module(&A[4], proc);	/* lookup module */
	    A[5].tag.kernel = TEND;

#ifdef SIMPLIFY
	    Set_Det /* ? */
	    Push_Ret_Code(PP)
	    Check_Local_Overflow;
	    PP = (emu_code) PriCode(procb);
#else
	    PP = (emu_code) bip_error_code_;
#endif
	    Next_Pp;				/* jump into syserror/4	*/
	}



/*----------------------------------------------------------------------
 * Externals with args in A[i]
 * Args are now dereferenced in A[i]
 * Apart from that, we are in a return state.
 * There may be events pending.
 * proc can't be a tool.
 *----------------------------------------------------------------------*/

_bip_res1_:				/* (err_code,proc) */
	Mark_Prof(_bip_res1_)
	Occur_Check_Boundary(0)
	if (err_code == PSUCCEED)
	{
	    Reset_DE;	/* demons are responsible to Kill_DE if appropriate */
	    Handle_Events_Return
	    Next_Pp;
	}
	else if (err_code == PFAIL)
	{
	    Fail;
	}
	else if ((err_code & ~PDELAY_MASK) == PDELAY)
	{
	    if (!(GlobalFlags & CORTN))
	    {
		SV = (pword *) 0;
		err_code = INSTANTIATION_FAULT;
		goto _bip_err1_;
	    }
	    if (!DE)			/* make a suspension structure */
	    {
		val_did = PriDid(proc);
		tmp1 = DidArity(val_did);
		DE = pw1 = TG;
		TG += SUSP_SIZE + 1 + tmp1;
		Init_Susp_Header(pw1, proc);
		Init_Susp_State(pw1, PriPriority(proc));
		pw1[SUSP_GOAL].val.ptr = pw1 + SUSP_SIZE;	/* goal */
		pw1[SUSP_GOAL].tag.kernel = TCOMP;
		pw1[SUSP_MODULE].tag.kernel = TDICT;
		pw1[SUSP_MODULE].val.did = PriModule(proc);

		S = pw1 + SUSP_SIZE;	/* build goal structure */
		S->val.did = val_did;
		S++->tag.kernel = TDICT;
		for(i = 1; i <= tmp1; i++)
		{
		    pw1 = &A[i];
		    Move_Pw_To_Global_Stack(pw1, S, ;)
		}
		Check_Gc
	    }

	    /*
	     * DE now points to the suspension
	     * Link it to the suspending variables
	     */

	    if (err_code & PDELAY_MASK)	/* delay on argument(s) 1-3 */
	    {
		Export_B_Sp_Tg_Tt_Eb_Gb
		if (err_code & (PDELAY_1 & PDELAY_MASK)) {
		    pw1 = A[1].val.ptr;
		    Dereference_Pw(pw1)
		    tmp1 = insert_suspension(pw1, 1, DE, DELAY_SLOT);
		    if (tmp1 < 0) {
			err_code = tmp1;
			goto _bip_err1_;
		    }
		}
		if (err_code & (PDELAY_2 & PDELAY_MASK)) {
		    pw1 = A[2].val.ptr;
		    Dereference_Pw(pw1)
		    tmp1 = insert_suspension(pw1, 1, DE, DELAY_SLOT);
		    if (tmp1 < 0) {
			err_code = tmp1;
			goto _bip_err1_;
		    }
		}
		if (err_code & (PDELAY_3 & PDELAY_MASK)) {
		    pw1 = A[3].val.ptr;
		    Dereference_Pw(pw1)
		    tmp1 = insert_suspension(pw1, 1, DE, DELAY_SLOT);
		    if (tmp1 < 0) {
			err_code = tmp1;
			goto _bip_err1_;
		    }
		}
		Import_Tg_Tt
	    }
	    else	/* suspending_variables points to a list of	*/
	    {		/* pointers to suspending variables		*/
		pw2 = SV;
		Export_B_Sp_Tg_Tt_Eb_Gb
		for (;;)
		{
		    pw1 = pw2[0].val.ptr;
		    Dereference_Pw(pw1)
		    tmp1 = insert_suspension(pw1,
			    err_code & PDELAY_BOUND ? DELAY_BOUND: DELAY_INST,
			    DE, DELAY_SLOT);
		    if (tmp1 < 0) {
			err_code = tmp1;
			goto _bip_err1_;
		    }
		    if (!IsList(pw2[1].tag))
			break;
		    pw2 = pw2[1].val.ptr;
		}
		Import_Tg_Tt
		SV = (pword *) 0;
	    }
	    Reset_DE;
	    Handle_Events_Return
	    Next_Pp;
	}
	else if (err_code == PTHROW)
	{
	    Reset_DE;
	    PP = (emu_code) do_exit_block_code_; /* Ball should be in A[1] */
	    Next_Pp;
	}
	else if (err_code > 0)
	{
	    err_code = ILLEGAL_RETURN;
	}
	/* goto _bip_err1_; */


/*******************************************************************
 * External returned an error code
 *******************************************************************/

_bip_err1_:			/* (err_code, proc), args in A[] */
	Mark_Prof(_bip_err1_)
	Kill_DE;
	err_code = -err_code;
	val_did = PriDid(proc);
	tmp1 = DidArity(val_did);

	if (!(procb = error_handler_[err_code]))	/* get the handler */
	    procb = error_handler_[0];

	if (procb->did == d_.true0 && procb->module_ref == d_.kernel_sepia) {
	    Handle_Events_Return
	    Next_Pp;
	}
	else if (procb->did == d_.fail && procb->module_ref == d_.kernel_sepia)
	{
	    Fail
	}
	else
	{
	    /* now setup call to syserror(Err, Goal, ContextMod, LookupMod) */

	    pw1 = S = TG;		/* build culprit goal structure */
	    TG += tmp1+1;
	    S->val.did = val_did;
	    S++->tag.kernel = TDICT;
	    for(i = 1; i <= tmp1; i++)
	    {
		pw2 = &A[i];
		Move_Pw_To_Global_Stack(pw2, S, ;)
	    }
	    Check_Gc;

	    Make_Integer(&A[1], err_code);
	    Make_Struct(&A[2], pw1);
	    Make_Marked_Module(&A[3], PriModule(proc));
	    Make_Lookup_Module(&A[4], proc);

	    proc = procb;
	    DBG_PORT = CALL_PORT;
	    goto _handler_call_;		/* (proc,DBG_PORT) */
	}



_local_control_overflow_:	/* still in exported state	*/
	Import_None;
	A[1].val.did = d_.local_control_overflow;
	A[1].tag.kernel = TDICT;
	PP = (emu_code) do_exit_block_code_;
	Next_Pp;

_abort_:
	A[1].val.did = d_.abort;
	A[1].tag.kernel = TDICT;
	PP = (emu_code) do_exit_block_code_;
	Next_Pp;


/************************************************************
 * Event handling
 *
 *	- global stack overflow and garbage collection
 *	- dictionary garbage collection
 *	- synchronous interrupt handling
 *	- waking
 * 
 ************************************************************/

/*
Waking:

In principle, it is enough to wake at Call, Chain and Jmp locations.
Waking at Rets and Exits causes some earlier waking, which is mainly
necessary for getting a reasonable debugger trace.
*/

/*
 * Entry point for the Call-type instructions:
 * - We are just at the end of a Call, Chain or Jmp instruction
 * - PP points to start of procedure (we get the arity from the code header)
 * - return address on top of local stack
 * - argument registers hold the call arguments
 * - in case of a debug event, proc holds the pri of the called procedure
 *
 * We push an environment to save the argument registers and the PP.
 * PP is normally the start address of a procedure, that's why we cannot treat
 * it like a return address. Instead, the procedure we are about to call is
 * virtually prefixed with a Continue_after_event instruction, which restores
 * the arguments and then continues into the procedure.
 */

_handle_events_at_call_:
	Mark_Prof(_handle_events_at_call_)
	tmp1 = CodeArity(PP);			/* number of valid arguments */

/*
 * Entry point for the explicit resuming instructions Res/Ress:
 * - return address on top of local stack, points behind the Res
 * - number of valid argument registers in tmp1
 */
_handle_events_at_res_:				/* (tmp1) */
	Push_Env				/* allocate an environment */

	if (DBG_PRI)
	{
	    PushDynEnvHdr(tmp1+DYNENVDBGSIZE, WAS_CALL, PP);	/* save arity, PP, DE */
	    SP -= DYNENVDBGSIZE;
	    DynEnvDE(e)->tag.kernel = DE?TSUSP:TNIL;
	    DynEnvDE(e)->val.ptr = DE;
	    DynEnvDbgPri(E)->tag.kernel = TPTR;		/* ... and debug info */
	    DynEnvDbgPri(E)->val.wptr = (uword *) DBG_PRI;
	    Make_Integer(DynEnvDbgPort(E), DBG_PORT);
	    Make_Integer(DynEnvDbgInvoc(E), DBG_INVOC);
	    DBG_PRI = 0;	/* DBG_{PRI,PORT,INVOC} now invalid */
	    if (DBG_LINE) {
		Make_Atom(DynEnvDbgPath(E), DBG_PATH);
		Make_Integer(DynEnvDbgLine(E), DBG_LINE);
		Make_Integer(DynEnvDbgFrom(E), DBG_FROM);
		Make_Integer(DynEnvDbgTo(E), DBG_TO);
		DBG_LINE = 0;	/* DBG_{PATH,LINE,FROM,TO} now invalid */
	    } else {
		Make_Atom(DynEnvDbgPath(E), d_.empty);
		Make_Integer(DynEnvDbgLine(E), 0);
		Make_Integer(DynEnvDbgFrom(E), 0);
		Make_Integer(DynEnvDbgTo(E), 0);
	    }
	    PP = (emu_code) &restore_debug_code_[1];
	}
	else
	{
	    PushDynEnvHdr(tmp1, 0, PP);		/* save arity, PP */
	    PP = (emu_code) &restore_code_[1];
	}

	pw1 = &A[1];	/* save the argument registers */
	for (; tmp1; --tmp1)
	    *(--SP) = *pw1++;
	Check_Local_Overflow

    /*  goto _handle_events_at_return_;  */


/*
 * Entry point for the Return-type instructions:
 * - We are about to return to address PP
 * - No argument registers are valid
 *
 * Caution: it is possible that the FakedOverflow was caused by several
 * events. Since we can only call a single Prolog handler here,
 * we must not reset the FakedOverflow in this case.
 */

_handle_events_at_return_:
	Mark_Prof(_handle_events_at_return_)
	Reset_Faked_Overflow;
	Push_Ret_Code(PP)			/* (Re)push a return address */

	if (GlobalOverflow)			/* call the garbage collector */
	{
	    PP = (emu_code) auto_gc_code_;
	    if (MU || EVENT_FLAGS)
		{ Fake_Overflow; }		/* postpone further	*/
	    Next_Pp;				/* no call port		*/
	}
	else if (MU)				/* meta_term_unify */
	{
	    /* We assume that this handler is always Prolog, no tool,
	     * and has arity 1 */
	    proc = error_handler_[-(META_TERM_UNIFY)];
	    PP = (emu_code) PriCode(proc);
	    A[1].val.ptr = MU;
	    A[1].tag.kernel = TLIST;
	    Reset_Unify_Exceptions
	    if (EVENT_FLAGS)
		{ Fake_Overflow; }		/* postpone it further */
	}
	else if (EVENT_FLAGS && !PO)
	{
	    if (EVENT_FLAGS & EVENT_POSTED)
	    {
		if (VM_FLAGS & EVENTS_DEFERRED)
		{
		    /* p_fprintf(log_output_,"event posted but handling deferred %08x\n",VM_FLAGS); ec_flush(log_output_); */
		    Pop_Ret_Code
		    Next_Pp;			/* goto Continue_after_event */
		}
		else
		{
		    /* NOTE: Sync events are only handled in nesting level 1! */
		    next_posted_event(&A[1]);	/* may redo Fake_Overflow */
		    if (IsInteger(A[1].tag))	/* indicates delayed signal */
		    {
			PP = (emu_code) sync_it_code_;
		    }
		    else				/* posted event */
		    {
			if (g_emu_.nesting_level > 1)	/* don't handle now */
			{
			    ec_post_event(A[1]); /* re-post */
			    Pop_Ret_Code
			    Next_Pp;
			}
			else	/* handle posted event now */
			{
			    if (IsTag(A[1].tag.kernel, TPTR))        /* Heap copied event */
			    {
				extern t_ext_type heap_event_tid;
				t_heap_event *event = (t_heap_event *)A[1].val.ptr;
				A[2] = A[3] = event->module;
				if (event->enabled) {
				    Export_B_Sp_Tg_Tt;
				    get_heapterm(&event->goal, &A[1]);
				    Import_Tg_Tt;
				    if (event->defers)
				    {
					/* p_fprintf(log_output_,"event defers others\n"); ec_flush(log_output_); */
					VM_FLAGS |= EVENTS_DEFERRED;
				    }
				} else {
				    Make_Atom(&A[1], d_.true0);
				}
				heap_event_tid.free((t_ext_ptr)event);
				PP = (emu_code) do_call_code_;
			    }
			    else 
			    {
				A[2].tag.kernel = TNIL;
				Make_Atom(&A[3], d_.kernel_sepia);
				Make_Atom(&A[4], d_.kernel_sepia);
				PP = (emu_code) prolog_error_code_;
			    }
			}
		    }
		    if (EVENT_FLAGS & ~EVENT_POSTED)
			{ Fake_Overflow; }
		}
	    }
	    else if (g_emu_.nesting_level == 1)	/* parallelism-related event */
	    {
		Pop_Ret_Code

		if (LOAD < 0)			/* countdown running? */
		{
		    if (++LOAD == 0)		/* delay expired? */
		    {
			Stop_Countdown();
			LOAD = 1;			/* release load now */
			if (LEAF)
			{
			    Export_B_Sp_Tg_Tt
			    sch_load_report(LEAF);
			    Import_None
			}
		    }
		    else
		    {
			Fake_Overflow;		/* retrigger countdown */
		    }
		    if (!(EVENT_FLAGS & ~COUNT_DOWN))
			{ Next_Pp; }		/* countdown only, continue */
		}
		Export_All
		eng_msg_loop();
		Import_All
		Next_Pp;
	    }
	    else				/* don't handle now */
	    {
		Pop_Ret_Code
		Next_Pp;			/* goto Continue_after_event */
	    }
	}
	else					/* no event, just return */
	{
	    Pop_Ret_Code
	    Next_Pp;				/* goto Continue_after_event */
	}

	Next_Pp;



/*******************************************************************
 * THE EMULATOR LOOP
 *******************************************************************/

_loop_:

    Mark_Prof(_loop_)
    Begin_Execution(PP)

    switch((PP++)->inst) {


/***** Data Move Instructions *****/
/************************************/
	Case(MoveAM, I_MoveAM)
	    Get_Argument(pw1)
	    *(--SP) = *pw1;
	    Check_Local_Overflow
	Case(Nop, I_Nop)
	    Next_Pp;

	Case(Move3AMAM, I_Move3AMAM)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	    /* falls through */
	Case(Move2AMAM, I_Move2AMAM)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	    /* falls through */
	Case(MoveAMAM, I_MoveAMAM)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	    Next_Pp;

	Case(Move3LL, I_Move3LL)
	    Get_Local(pw1)
	    Get_Local(pw2)
	    Move_Pw(pw1,pw2)
	    /* falls through */
	Case(Move2LL, I_Move2LL)
	    Get_Local(pw1)
	    Get_Local(pw2)
	    Move_Pw(pw1,pw2)
	    /* falls through */
	Case(MoveLL, I_MoveLL)
	    Get_Local(pw1)
	    Get_Local(pw2)
	    Move_Pw(pw1,pw2)
	    Next_Pp;


/*
Possible additional combined instructions
	Swap	A1<->A2
	Shift	A1<-A2<-A3	(all different)
	Rotate	A1<-A2<-A3(<-A1)
*/
	Case(SwapAMAM, I_SwapAMAM)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    tmp1 = pw1->val.all;
	    pw1->val.all = pw2->val.all;
	    pw2->val.all = tmp1;
	    tmp1 = pw1->tag.all;
	    pw1->tag.all = pw2->tag.all;
	    pw2->tag.all = tmp1;
	    Next_Pp;

	Case(ShiftAMAMAM, I_ShiftAMAMAM)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    *pw1 = *pw2;
	    Get_Argument(pw1)
	    *pw2 = *pw1;
	    Next_Pp;

	Case(ShiftAMAMAMAM, I_ShiftAMAMAMAM)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    *pw1 = *pw2;
	    Get_Argument(pw1)
	    *pw2 = *pw1;
	    Get_Argument(pw2)
	    *pw1 = *pw2;
	    Next_Pp;

	Case(ShiftAMAMAMAMAM, I_ShiftAMAMAMAMAM)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    *pw1 = *pw2;
	    Get_Argument(pw1)
	    *pw2 = *pw1;
	    Get_Argument(pw2)
	    *pw1 = *pw2;
	    Get_Argument(pw1)
	    *pw2 = *pw1;
	    Next_Pp;

	Case(RotAMAMAM, I_RotAMAMAM)
	    Get_Argument(pw1)
	    scratch_pw = *pw1,
	    Get_Argument(pw2)
	    *pw1 = *pw2;
	    Get_Argument(pw1)
	    *pw2 = *pw1;
	    *pw1 = scratch_pw;
	    Next_Pp;

	Case(Get_variableNAML, I_Get_variableNAML)
	    Alloc_Env
	Case(MoveAML, I_MoveAML)
	    Get_Argument(pw1)
	    Get_Local(pw2)
	    Move_Pw(pw1,pw2)
	    Next_Pp;

	Case(MoveNAML, I_MoveNAML)
	    i = PP++->nint;
	    Get_Argument(pw1);
	    Get_Local(pw2);
	    do
	    {
		Move_Pw(pw1, pw2)
		pw1++;
		pw2--;
	    } while (--i > 0);
	    Next_Pp;

	Case(Move3AML, I_Move3AML)
	    Get_Argument(pw1)
	    Get_Local(pw2)
	    Move_Pw(pw1,pw2)
	Case(Move2AML, I_Move2AML)
	    Get_Argument(pw1)
	    Get_Local(pw2)
	    Move_Pw(pw1,pw2)
	    Get_Argument(pw1)
	    Get_Local(pw2)
	    Move_Pw(pw1,pw2)
	    Next_Pp;

	Case(Move3LAM, I_Move3LAM)
	    Get_Local(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	Case(Move2LAM, I_Move2LAM)
	    Get_Local(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	Case(MoveLAM, I_MoveLAM)
	    Get_Local(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	    Next_Pp;

	Case(MoveNLAM, I_MoveNLAM)
	    i = PP++->nint;
	    Get_Local(pw1);
	    Get_Argument(pw2);
	    do
	    {
		Move_Pw(pw1, pw2)
		pw1--;
		pw2++;
	    } while (--i > 0);
	    Next_Pp;

	Case(MoveTMAM, I_MoveTMAM)
	    Get_Temporary(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	    Next_Pp;


/***** Get_value?? instructions *****/
/************************************/

	Case(Get_valueAMAM, I_Get_valueAMAM)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    goto _unify_;

	Case(Get_valueAML, I_Get_valueAML)
	    Get_Argument(pw1)
	    Get_Local(pw2);
	    goto _unify_;

	Case(Get_valueAMTM, I_Get_valueAMTM)
	    Get_Argument(pw1)
	    Get_Temporary(pw2)
	    goto _unify_;

	Case(Get_valueLL, I_Get_valueLL)
	    Get_Local(pw1)
	    Get_Local(pw2);
	    goto _unify_;


/****
    Get_?constant???
    ... unify the argument with a constant.
****/

	Case(Out_get_constantAM, I_Out_get_constantAM)
        Case(Get_constantAM, I_Get_constantAM)		/* AM, val, tag */
            Get_Argument(pw1);
_unify_const_:						/* (pw1,pp) */
	    Dereference_Pw_Tag(pw1,tmp1);
	    if (ISRef(tmp1)) {
		if (ISVar(tmp1)) {
		    Trail_If_Needed(pw1);
		    pw1->val.all = PP++->all;
		    pw1->tag.all = PP++->all;
		    Next_Pp;
		} else {
		    pw2 = PP++->ptr;
		    tmp1 = PP++->kernel;
		    goto _bind_nonstandard_;
		}
	    }
_compare_const_:					/* (tmp1,pw1,pp) */
	    if (!IsTag(tmp1, PP[1].all)) {
		Fail
	    } else if (ISSimple(tmp1)) {
		if (!SimpleEq(tmp1, pw1->val, PP->val)) {
		    Fail
		}
	    } else {
		Export_B_Sp_Tg_Tt
		err_code = tag_desc[TagTypeC(tmp1)].equal(pw1->val.ptr, PP->ptr);
		Import_None
		if (!err_code) { Fail }
	    }
	    PP += 2;
            Next_Pp;


	Case(Out_get_nilAM, I_Out_get_nilAM)
	Case(Get_nilAM, I_Get_nilAM)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1,tmp1)
	    if(ISVar(tmp1)) {
		Bind_Tag(pw1,TNIL)
	    } else if(IsTag(tmp1,TNIL)) {
		Next_Pp;
	    } else if(ISRef(tmp1)) {
		Bind_CRef_pw1_Tag(TNIL);
	    } else
		{ Fail }
	    Next_Pp;

	Case(Get_integer2AM, I_Get_integer2AM)
	    Get_Argument(pw1)
	    Unify_Simple_pw1(TINT, nint, tmp1)
	    /* falls through */
	Case(Out_get_integerAM, I_Out_get_integerAM)
	Case(Get_integerAM, I_Get_integerAM)
	    Get_Argument(pw1)
	    Unify_Simple_pw1(TINT, nint, tmp1)
	    Next_Pp;

#ifdef TFLOAT
	Case(Out_get_floatAM, I_Out_get_floatAM)
	Case(Get_floatAM, I_Get_floatAM)
	    Get_Argument(pw1)
	    Unify_Simple_pw1(TFLOAT, real, tmp1)
	    Next_Pp;
#endif

	Case(Get_atom2AM, I_Get_atom2AM)
	    Get_Argument(pw1)
	    Unify_Simple_pw1(TDICT, did, tmp1)
	    /* falls through */
	Case(Out_get_atomAM, I_Out_get_atomAM)
	Case(Get_atomAM, I_Get_atomAM)
	    Get_Argument(pw1)
	    Unify_Simple_pw1(TDICT, did, tmp1)
	    Next_Pp;

	Case(Out_get_stringAM, I_Out_get_stringAM)
	Case(Get_stringAM, I_Get_stringAM)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1,tmp1)
	    if(ISVar(tmp1)) {
		Bind_(pw1, PP++->all, TSTRG)
	    } else if(IsTag(tmp1,TSTRG)) {
		pw1 = pw1->val.ptr;
		pw2 = PP++->ptr;
		Compare_Strings(pw1, pw2, err_code);
		if(err_code >= 0) { Fail }
	    } else if(ISRef(tmp1)) {
		Bind_CRef_pw1(PP++->all,TSTRG)
	    } else
		{ Fail }
	    Next_Pp;

	Case(Get_atomintegerAMAM, I_Get_atomintegerAMAM)
	    Get_Argument(pw1)
	    Unify_Simple_pw1(TDICT, did, tmp1)
	    Get_Argument(pw1)
	    Unify_Simple_pw1(TINT, nint, tmp1)
	    Next_Pp;

	Case(Get_metaAM, I_Get_metaAM)
	    Get_Argument(pw1)
	    i = (uword) PP++->kernel;
_read_meta_:			/* unify *pw1 with a new meta with tag i */
	    Dereference_Pw_Tag(pw1,tmp1)
	    S = TG;
	    TG += 2;
	    if (ISVar(tmp1)) {
		if (IsLocal(pw1)) {
		    Constructed_Structure(0)
		} else {
		    Constructed_Structure(pw1)
		}
		S->val.ptr = S;
		S->tag.all = i;
		Bind_(pw1, (uword) S, TREF);
	    } else if (ISRef(tmp1)) {	/* this case could be optimized */
		Constructed_Structure(pw1);
		S->val.ptr = S;
		S->tag.all = i;
		Export_B_Sp_Tg_Tt_Eb_Gb
		err_code = bind_c(pw1, S, &MU);
		Import_Tg_Tt
		if (err_code == PFAIL) { Fail; }
	    } else {				/* TMETA = nonvar */
		S->val.all = pw1->val.all;
		S->tag.kernel = tmp1;
		Update_MU(S)
	    }
	    Next_Pp;


	Case(Get_listAM, I_Get_listAM)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1,tmp1)
	    if(ISVar(tmp1)) {
		if (IsLocal(pw1)) {
		    Constructed_Structure(0)
		} else {
		    Constructed_Structure(pw1)
		}
		S = TG;
		TG += 2;
		Bind_(pw1, (uword) S, TLIST)
		PP++;
	    } else if (IsTag(tmp1,TLIST)) {
		S = (pw1->val).ptr;
		PP = PP->code;
	    } else if (ISRef(tmp1)) {
		Constructed_Structure(pw1);
		S = TG;
		TG += 2;
		PP++;
		Bind_CRef_pw1((uword) S, TLIST)
	    } else
		{ Fail }
	    Next_Pp;

	Case(Get_structureAM, I_Get_structureAM)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1,tmp1)
	    if(ISVar(tmp1)) {
		if (IsLocal(pw1)) {
		    Constructed_Structure(0)
		} else {
		    Constructed_Structure(pw1)
		}
		val_did = PP++->did;
		S = TG;
		TG += DidArity(val_did) + 1;
		Bind_(pw1, (uword) S, TCOMP)
		S->val.did = val_did;
		((S)++)->tag.kernel = TDICT;
		PP++;
	    } else if (!IsTag(tmp1,TCOMP)) {
		if(ISRef(tmp1)) {
		   Constructed_Structure(pw1);
  		   val_did = PP++->did;
		   S = TG;
		   TG += DidArity(val_did) + 1;
		   pw2 = S;
		   S->val.did = val_did;
		   ((S)++)->tag.kernel = TDICT;
		   PP++;
		   tmp1 = TCOMP;
		   goto _bind_nonstandard_;	/* (pw1, pw2, tmp1) */
		} else { Fail }
            } else if (pw1->val.ptr->val.did != PP++->did ) {
                Fail
	    } else {
		S = pw1->val.ptr;
		S += 1;
		PP = PP->code;
	    }
	    Next_Pp;

/*** output mode head arguments ***/

	Case(Out_get_listAM, I_Out_get_listAM)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1)
	    S = TG;
	    TG += 2;
	    if(ISVar(tmp1)) {
	        Bind_(pw1, (uword) S, TLIST);
	    } else if(ISRef(tmp1)) {
		Bind_CRef_pw1((uword) S, TLIST);
	    } else { Fail }	/* in case the mode is violated */
	    Next_Pp;

	Case(Out_get_structureAM, I_Out_get_structureAM)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1)
	    val_did = PP++->did;
	    S = TG;
	    TG += DidArity(val_did) + 1;
	    if(ISVar(tmp1)) {
	        Bind_(pw1, (uword) S, TCOMP);
		S->val.did = val_did;
		((S)++)->tag.kernel = TDICT;
	    } else if(ISRef(tmp1)) {
	      pw2 = S;
	      S->val.did = val_did;
	      ((S)++)->tag.kernel = TDICT;
	      tmp1 = TCOMP;
	      goto _bind_nonstandard_;	/* (pw1, pw2, tmp1) */
	    } else { Fail }	/* in case the mode is violated */
	    Next_Pp;



/**** Head nested argument unification instructions ****/
/*******************************************************/

/****
    Read instructions
****/

/**** Read Variable ****/

	Case(Read_void, I_Read_void)
	    S += 1;
	    Next_Pp;

	Case(Read_voidN, I_Read_voidN)
	    S = ByteOffsetPlus(S, PP++->offset);
	    Next_Pp;

	Case(Read_variable, I_Read_variable)
	    *(--SP) = *(S++);
	    Check_Local_Overflow
	    Next_Pp;

	Case(Read_variable2AM, I_Read_variable2AM)
	    Get_Argument(pw1)
	    *pw1 = *(S++);
	    /* falls through */
	Case(Read_variableAM, I_Read_variableAM)
	    Get_Argument(pw1)
	    *pw1 = *(S++);
	    Next_Pp;

	Case(Read_variable2AML, I_Read_variable2AML)
	    Get_Argument(pw1)
	    *pw1 = *(S++);
	    Get_Local(pw1)
	    *pw1 = *(S++);
	    Next_Pp;

	Case(Read_variableNL, I_Read_variableNL)
	    Alloc_Env

	Case(Read_variableL, I_Read_variableL)
	    Get_Local(pw1)
	    *pw1 = *(S++);
	    Next_Pp;

	Case(Read_variable2L, I_Read_variable2L)
	    Get_Local(pw1)
	    *pw1 = *(S++);
	    Get_Local(pw1)
	    *pw1 = *(S++);
	    Next_Pp;


/**** Read Reference ****/

	Case(Read_reference, I_Read_reference)
	    (--SP)->tag.kernel = TREF;
	    SP->val.ptr = S++;
	    Check_Local_Overflow
	    Next_Pp;

	Case(Read_referenceAM, I_Read_referenceAM)
	    Get_Argument(pw1)
	    pw1->val.ptr = S++;
	    pw1->tag.kernel = TREF;
	    Next_Pp;

	Case(Read_referenceNL, I_Read_referenceNL)
	    Alloc_Env

	Case(Read_referenceL, I_Read_referenceL)
	    Get_Local(pw1)
	    pw1->val.ptr = S++;
	    pw1->tag.kernel = TREF;
	    Next_Pp;


/**** Read value ****/

	Case(Read_valueAM, I_Read_valueAM)
	    Get_Argument(pw1)
	    pw2 = S++;
	    goto _unify_;

	Case(Read_valueL, I_Read_valueL)
	    Get_Local(pw1)
	    pw2 = S++;
	    goto _unify_;

	Case(Read_valueTM, I_Read_valueTM)
	    Get_Temporary(pw1)
	    pw2 = S++;
	    goto _unify_;


/**** Read?constant?? ****/

        /* val, tag */

        Case(Read_constant, I_Read_constant)
            pw1 = S++;
	    goto _unify_const_;

	Case(Read_nil, I_Read_nil)
	    pw1 = S++;
	    Dereference_Pw_Tag(pw1,tmp1)
	    if(ISVar(tmp1)) {
		Bind_Tag(pw1,TNIL)
	    } else if((!IsTag(tmp1,TNIL))) {
	        if(ISRef(tmp1)) {
		    Bind_CRef_pw1_Tag(TNIL);
		} else {
		    Fail
		}
	    }
	    Next_Pp;

	Case(Read_integer2, I_Read_integer2)
	    pw1 = S++;
	    Unify_Simple_pw1(TINT, nint, tmp1)
	    /* falls through */
	Case(Read_integer, I_Read_integer)
	    pw1 = S++;
	    Unify_Simple_pw1(TINT, nint, tmp1)
	    Next_Pp;

#ifdef TFLOAT
	Case(Read_float, I_Read_float)
	    pw1 = S++;
	    Unify_Simple_pw1(TFLOAT, real, tmp1)
	    Next_Pp;
#endif

	Case(Read_atom2, I_Read_atom2)
	    pw1 = S++;
	    Unify_Simple_pw1(TDICT, did, tmp1)
	    /* falls through */
	Case(Read_atom, I_Read_atom)
	    pw1 = S++;
	    Unify_Simple_pw1(TDICT, did, tmp1)
	    Next_Pp;

	Case(Read_integeratom, I_Read_integeratom)
	    pw1 = S++;
	    Unify_Simple_pw1(TINT, nint, tmp1)
	    pw1 = S++;
	    Unify_Simple_pw1(TDICT, did, tmp1)
	    Next_Pp;

	Case(Read_atominteger, I_Read_atominteger)
	    pw1 = S++;
	    Unify_Simple_pw1(TDICT, did, tmp1)
	    pw1 = S++;
	    Unify_Simple_pw1(TINT, nint, tmp1)
	    Next_Pp;

	Case(Read_string, I_Read_string)
	    pw1 = S++;
	    Dereference_Pw_Tag(pw1,tmp1)
	    if(ISVar(tmp1)) {
		Bind_(pw1, PP++->all, TSTRG)
	    } else if(!IsTag(tmp1,TSTRG)) {
		if(ISRef(tmp1)) {
		    Bind_CRef_pw1(PP++->all,TSTRG)
		} else {
		    Fail
		}
	    } else {
		pw1 = pw1->val.ptr;
		pw2 = PP++->ptr;
		Compare_Strings(pw1, pw2, err_code);
		if(err_code >= 0) {
		    Fail
		}
	    }
	    Next_Pp;


	Case(Match_meta, I_Match_meta)			/* first */
	    (--SP)->tag.kernel = MODE_READ;
	    SP->val.ptr = S + 1;
	    Check_Local_Overflow
	Case(Match_last_meta, I_Match_last_meta)	/* last */
_match_meta_:
	    Dereference_Pw_Tag(S,tmp1)
	    if (IsTag(tmp1,TMETA)) {
		S = S->val.ptr;
	    } else
		{ Fail }
	    Next_Pp;

	Case(Match_next_metaTM, I_Match_next_metaTM)	/* next */
	    Get_Temporary(pw1)
	    S = (pw1->val.ptr)++;
	    goto _match_meta_;

	Case(Match_metaTM, I_Match_metaTM)		/* alone */
	    Get_Temporary(pw1)
	    pw1->val.ptr = S + 1;
	    goto _match_meta_;


	Case(Read_meta, I_Read_meta)			/* first */
	    (--SP)->tag.kernel = MODE_READ;
	    SP->val.ptr = S + 1;
	    Check_Local_Overflow
	Case(Read_last_meta, I_Read_last_meta)		/* last */
	    pw1 = S;
	    i = (uword) PP++->kernel;
	    PP = PP->code;
	    goto _read_meta_;	/* (pw1, i) */

	Case(Read_next_metaTM, I_Read_next_metaTM)	/* next */
	    Get_Temporary(pw1)
	    /* pw1 = (pw1->val.ptr)++; wrong in the C compiler */
	    S = pw1->val.ptr;
	    pw1->val.ptr = S + 1;
	    pw1 = S;
	    i = (uword) PP++->kernel;
	    PP = PP->code;
	    goto _read_meta_;	/* (pw1, i) */

	Case(Read_metaTM, I_Read_metaTM)		/* alone */
	    Get_Temporary(pw1)
	    pw1->val.ptr = S + 1;
	    pw1 = S;
	    i = (uword) PP++->kernel;
	    PP = PP->code;
	    goto _read_meta_;	/* (pw1, i) */

	Case(Read_attribute, I_Read_attribute)
	    Dereference_Pw(S)
	    S = S->val.ptr;
	    tmp1 = PP++->offset;
	    if (tmp1 > DidArity(S->val.did) * sizeof(pword)) {
		Fail
	    }
	    S = ByteOffsetPlus(S, tmp1);
	    Next_Pp;

	Case(Read_list, I_Read_list)			/* first */
	    (--SP)->tag.kernel = MODE_READ;
	    SP->val.ptr = S + 1;
	    Check_Local_Overflow
	Case(Read_last_list, I_Read_last_list)		/* last */
	    Dereference_Pw_Tag(S,tmp1)
	    if (ISRef(tmp1)) {
		Constructed_Structure(S);
		PP = PP->code;
		pw1 = S;
		S = TG;
		TG = S + 2;
		Bind_Ref_pw1(tmp1, (uword) S, TLIST)
	    } else if (IsTag(tmp1,TLIST)) {
		S = S->val.ptr;
		PP++;
	    } else
		{ Fail }
	    Next_Pp;


	Case(Read_listTM, I_Read_listTM)		/* alone */
	    Get_Temporary(pw2)
	    pw1 = S++;
	    pw2->val.ptr = S;
	    Dereference_Pw_Tag(pw1,tmp1)
	    if (ISRef(tmp1)) {
		Constructed_Structure(pw1);
		pw2->tag.kernel = MODE_READ;
		PP = PP->code;
		S = TG;
		TG = S + 2;
		Bind_Ref_pw1(tmp1, (uword) S, TLIST)
	    } else if (IsTag(tmp1,TLIST)) {
		S = pw1->val.ptr;
		PP++;
	    } else
		{ Fail }
	    Next_Pp;

	Case(Read_next_listTM, I_Read_next_listTM)	/* next */
	    Get_Temporary(pw1)
	    S = (pw1->val.ptr)++;
	    Dereference_Pw_Tag(S,tmp1)
	    if (ISRef(tmp1)) {
		Constructed_Structure(S);
		pw1->tag.kernel = MODE_READ;
		PP = PP->code;
		pw1 = S;
		S = TG;
		TG = S + 2;
		Bind_Ref_pw1(tmp1, (uword) S, TLIST)
	    } else if (IsTag(tmp1,TLIST)) {
		S = S->val.ptr;
		PP++;
	    } else
		{ Fail }
	    Next_Pp;

	Case(Read_structure, I_Read_structure)	/* did lab */
	    (--SP)->tag.kernel = MODE_READ;
	    SP->val.ptr = S + 1;
	    Check_Local_Overflow
	Case(Read_last_structure, I_Read_last_structure) /* did lab */
	    Dereference_Pw_Tag(S,tmp1)
	    if(ISVar(tmp1)) {
		Constructed_Structure(S);
		val_did = PP++->did;
		pw1 = S;
		S = TG;
		TG += DidArity(val_did) + 1;
		Bind_(pw1, (uword) S,TCOMP);
		S->val.did = val_did;
		(S++)->tag.kernel = TDICT;
		PP = PP->code;
	    } else if (!IsTag(tmp1,TCOMP)) {
		if(ISRef(tmp1)) {
		    Constructed_Structure(S);
		    val_did = PP++->did;
		    pw1 = S;
		    S = TG;
		    TG += DidArity(val_did) + 1;
		    pw2 = S;
		    S->val.did = val_did;
		    (S++)->tag.kernel = TDICT;
		    PP = PP->code;
		    tmp1 = TCOMP;
		    goto _bind_nonstandard_;	/* (pw1, pw2, tmp1) */
		} else { Fail }
            } else if (S->val.ptr->val.did != PP->did ) {
		Fail
	    } else {
		S = S->val.ptr + 1;
		PP += 2;
	    }
	    Next_Pp;


	Case(Read_structureTM, I_Read_structureTM) /* did TM lab */
	    Get_Temporary_Offs(1, pw2)
	    pw1 = (S)++;
	    pw2->val.ptr = S;
	    Dereference_Pw_Tag(pw1,tmp1)
	    if(ISVar(tmp1)) {
		Constructed_Structure(pw1);
		pw2->tag.kernel = MODE_READ;
		val_did = PP->did;
		S = TG;
		TG += DidArity(val_did) + 1;
		Bind_(pw1, (uword) S, TCOMP)
		S->val.did = val_did;
		(S++)->tag.kernel = TDICT;
		PP = (PP+2)->code;
	    } else if (!IsTag(tmp1,TCOMP)) {
		if(ISRef(tmp1)) {
		    Constructed_Structure(pw1);
		    pw2->tag.kernel = MODE_READ;
		    val_did = PP->did;
		    S = TG;
		    TG += DidArity(val_did) + 1;
		    pw2 = S;
		    S->val.did = val_did;
		    (S++)->tag.kernel = TDICT;
		    PP = (PP+2)->code;
		    tmp1 = TCOMP;
		    goto _bind_nonstandard_;	/* (pw1, pw2, tmp1) */
		} else { Fail }
            } else if (pw1->val.ptr->val.did != PP->did) {
		Fail
	    } else {
		S = pw1->val.ptr + 1;
		PP += 3;
	    }
	    Next_Pp;


	Case(Read_next_structureTM, I_Read_next_structureTM) /* did TM lab */
	    Get_Temporary_Offs(1, pw2)
	    S = (pw2->val.ptr)++;
	    Dereference_Pw_Tag(S,tmp1)
	    if(ISVar(tmp1)) {
		Constructed_Structure(S);
		pw2->tag.kernel = MODE_READ;
		val_did = PP->did;
		pw1 = S;
		S = TG;
		TG += DidArity(val_did) + 1;
		Bind_(pw1, (uword) S, TCOMP)
		S->val.did = val_did;
		(S++)->tag.kernel = TDICT;
		PP = (PP+2)->code;
	    } else if (!IsTag(tmp1,TCOMP)) {
		if(ISRef(tmp1)) {
		    Constructed_Structure(S);
		    pw2->tag.kernel = MODE_READ;
		    val_did = PP->did;
		    pw1 = S;
		    S = TG;
		    TG += DidArity(val_did) + 1;
		    pw2 = S;
		    S->val.did = val_did;
		    (S++)->tag.kernel = TDICT;
		    PP = (PP+2)->code;
		    tmp1 = TCOMP;
		    goto _bind_nonstandard_;	/* (pw1, pw2, tmp1) */
		} else { Fail }
            } else if (S->val.ptr->val.did != PP->did) {
		Fail
	    } else {
		S = S->val.ptr + 1;
		PP += 3;
	    }
	    Next_Pp;




/**** Write and Push instructions ****/

	Case(Write_variable, I_Write_variable)
	Case(Push_variable, I_Push_variable)
	    (--SP)->tag.kernel = TREF;
	    SP->val.ptr = S;
	    Check_Local_Overflow
	    /* fall through */

	Case(Write_void, I_Write_void)
	Case(Push_void, I_Push_void)
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = TREF;
	    Next_Pp;

	Case(Push_voidN, I_Push_voidN)
	Case(Write_voidN, I_Write_voidN)
	    pw1 = ByteOffsetPlus(S, PP++->offset);
	    while (S < pw1)
	    {
		S->val.ptr = S;
		((S)++)->tag.kernel = TREF;
	    }
	    Next_Pp;
	
	Case(Write_variable2AM, I_Write_variable2AM)
	    Get_Argument(pw1)
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = TREF;
	    /* falls through */
	Case(Write_variableAM, I_Write_variableAM)
	Case(Push_variableAM, I_Push_variableAM)
	    Get_Argument(pw1)
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = TREF;
	    Next_Pp;

	Case(Write_variable2AML, I_Write_variable2AML)
	    Get_Argument(pw1)
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = TREF;
	    Get_Local(pw1)
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = TREF;
	    Next_Pp;

	Case(Write_variableNL, I_Write_variableNL)
	    Alloc_Env

	Case(Write_variableL, I_Write_variableL)
	Case(Push_variableL, I_Push_variableL)
	    Get_Local(pw1)
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = TREF;
	    Next_Pp;

	Case(Write_variable2L, I_Write_variable2L)
	    Get_Local(pw1)
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = TREF;
	    S->val.ptr = S;
	    S->tag.kernel = TREF;
	    Get_Local(pw1)
	    pw1->val.ptr = (S)++;
	    pw1->tag.kernel = TREF;
	    Next_Pp;

	Case(Push_init_variableL, I_Push_init_variableL)
	    Get_Local(pw1)
	    Trail_If_Needed_Eb(pw1)
	    pw1->val.ptr = S;
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = TREF;
	    Next_Pp;

	Case(Write_named_variable, I_Write_named_variable)
	    (--SP)->tag.kernel = TREF;
	    SP->val.ptr = S;
	    Check_Local_Overflow
	    /* fall through */

	Case(Write_named_void, I_Write_named_void)
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Write_named_variableAM, I_Write_named_variableAM)
	    Get_Argument(pw1)
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Write_named_variableNL, I_Write_named_variableNL)
	    Alloc_Env
	Case(Write_named_variableL, I_Write_named_variableL)
	    Get_Local(pw1)
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    ((S)++)->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Push_self_reference, I_Push_self_reference)
	    S->val.ptr = S;
	    S++->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Push_void_reference, I_Push_void_reference)
	    S->tag.kernel = TREF;
	    ((S)++)->val.ptr = TG;
	    TG = ByteOffsetPlus(TG, PP++->offset);
	    Next_Pp;

	Case(Push_reference, I_Push_reference)
	    (--SP)->tag.kernel = S->tag.kernel = TREF;
	    SP->val.ptr = ((S)++)->val.ptr = TG;
	    TG = ByteOffsetPlus(TG, PP++->offset);
	    Check_Local_Overflow
	    Next_Pp;

	Case(Push_referenceAM, I_Push_referenceAM)
	    Get_Argument(pw1)
	    pw1->val.ptr = S->val.ptr = TG;
	    pw1->tag.kernel = S++->tag.kernel = TREF;
	    TG = ByteOffsetPlus(TG, PP++->offset);
	    Next_Pp;

	Case(Push_referenceL, I_Push_referenceL)
	    Get_Local(pw1)
	    pw1->val.ptr = S->val.ptr = TG;
	    pw1->tag.kernel = S++->tag.kernel = TREF;
	    TG = ByteOffsetPlus(TG, PP++->offset);
	    Next_Pp;

	Case(Push_init_referenceL, I_Push_init_referenceL)
	    Get_Local(pw1)
	    Trail_If_Needed_Eb(pw1)
	    pw1->val.ptr = S->val.ptr = TG;
	    S++->tag.kernel = TREF;
	    TG = ByteOffsetPlus(TG, PP++->offset);
	    Next_Pp;

	Case(Write_valueAM, I_Write_valueAM)
	Case(Push_valueAM, I_Push_valueAM)
	    Get_Argument(pw1)
	    Occur_Check_Write(pw1, Fail)
	    *(S++) = *pw1;
	    Next_Pp;

	Case(Write_valueL, I_Write_valueL)
	Case(Push_valueL, I_Push_valueL)
	    Get_Local(pw1)
	    Occur_Check_Write(pw1, Fail)
	    *(S++) = *pw1;
	    Next_Pp;

	Case(Write_valueTM, I_Write_valueTM)
	Case(Push_valueTM, I_Push_valueTM)
	    Get_Temporary(pw1)
	    Occur_Check_Write(pw1, Fail)
	    *(S++) = *pw1;
	    Next_Pp;

	Case(Push_valueG, I_Push_valueG)
	    S->tag.all = TREF;
	    S->val.ptr = ByteOffsetPlus(S, PP++->offset);
	    S++;
	    Next_Pp;

	Case(Push_local_valueAM, I_Push_local_valueAM)
	    Get_Argument(pw1)
_push_local_:
	    Move_Pw_To_Global_Stack(pw1,S, ;)
	    Next_Pp;

	Case(Push_local_valueL, I_Push_local_valueL)
	    Get_Local(pw1)
	    goto _push_local_;

	Case(Push_local_valueTM, I_Push_local_valueTM)
	    Get_Temporary(pw1)
	    goto _push_local_;

	Case(Write_local_valueAM, I_Write_local_valueAM)
	    Get_Argument(pw1)
_write_local_:
	    Move_Pw_To_Global_Stack(pw1,S, Occur_Check_Write(pw1, Fail))
	    Occur_Check_Boundary(0);
	    Next_Pp;

	Case(Write_local_valueL, I_Write_local_valueL)
	    Get_Local(pw1)
	    goto _write_local_;

	Case(Write_local_valueTM, I_Write_local_valueTM)
	    Get_Temporary(pw1)
	    goto _write_local_;


	Case(Push_local_value2AM, I_Push_local_value2AM)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
_push_local2_:
	    Move_Pw_To_Global_Stack(pw1,S, ;)
	    Move_Pw_To_Global_Stack(pw2,S, ;)
	    Next_Pp;

	Case(Push_local_value2L, I_Push_local_value2L)
	    Get_Local(pw1)
	    Get_Local(pw2)
	    goto _push_local2_;

	Case(Write_local_value2AM, I_Write_local_value2AM)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
_write_local2_:
	    Move_Pw_To_Global_Stack(pw1,S, Occur_Check_Write(pw1, Fail))
	    Occur_Check_Boundary(0);
	    Move_Pw_To_Global_Stack(pw2,S, Occur_Check_Write(pw2, Fail))
	    Occur_Check_Boundary(0);
	    Next_Pp;

	Case(Write_local_value2L, I_Write_local_value2L)
	    Get_Local(pw1)
	    Get_Local(pw2)
	    goto _write_local2_;

        /* val, tag !!!!!! */

        Case(Write_constant, I_Write_constant)
        Case(Push_constant, I_Push_constant)
            S->val.all = PP++ -> all;
            ((S)++)->tag.all = PP++ -> all;
            Next_Pp;

	Case(Write_nil, I_Write_nil)
	Case(Push_nil, I_Push_nil)
	    ((S)++)->tag.kernel = TNIL;
	    Next_Pp;

	Case(Write_integer2, I_Write_integer2)
	    S->val.nint = PP++->nint;
	    ((S)++)->tag.kernel = TINT;
	    /* falls through */
	Case(Write_integer, I_Write_integer)
	Case(Push_integer, I_Push_integer)
	    S->val.nint = PP++->nint;
	    ((S)++)->tag.kernel = TINT;
	    Next_Pp;

#ifdef TFLOAT
	Case(Write_float, I_Write_float)
	Case(Push_float, I_Push_float)
	    S->val.real = PP++->real;
	    ((S)++)->tag.kernel = TFLOAT;
	    Next_Pp;
#endif

	Case(Write_did2, I_Write_did2)
	    S->val.did = PP++->did;
	    ((S)++)->tag.kernel = TDICT;
	    /* falls through */
	Case(Write_did, I_Write_did)
	    S->val.did = PP++->did;
	    ((S)++)->tag.kernel = TDICT;
	    Next_Pp;

	Case(Write_integerdid, I_Write_integerdid)
	    S->val.nint = PP++->nint;
	    ((S)++)->tag.kernel = TINT;
	    S->val.did = PP++->did;
	    ((S)++)->tag.kernel = TDICT;
	    Next_Pp;

	Case(Write_didinteger, I_Write_didinteger)
	    S->val.did = PP++->did;
	    ((S)++)->tag.kernel = TDICT;
	    S->val.nint = PP++->nint;
	    ((S)++)->tag.kernel = TINT;
	    Next_Pp;

	Case(Write_string, I_Write_string)
	Case(Push_string, I_Push_string)
	    S->val.str = PP++->str;
	    ((S)++)->tag.kernel = TSTRG;
	    Next_Pp;

	Case(Write_meta, I_Write_meta)
	    pw1 = S;
	    S = TG;
	    TG = S + 2;
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    S->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Write_first_list, I_Write_first_list)
	    (--SP)->tag.kernel = MODE_WRITE;
	    SP->val.ptr = S + 1;
	    Check_Local_Overflow
	    /* falls through */
	Case(Write_list, I_Write_list)
	    pw1 = S;
	    S = TG;
	    TG = S + 2;
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TLIST;
	    Next_Pp;

	Case(Write_next_listTM, I_Write_next_listTM)
	    Get_Temporary(pw1);
	    pw1->val.ptr = S + 1;
	    pw1 = S;
	    S = TG;
	    TG = S + 2;
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TLIST;
	    Next_Pp;

	Case(Write_next_listTMlab, I_Write_next_listTMlab)
	    Get_Temporary(pw1)
	    if(pw1->tag.kernel == MODE_READ) {
		PP = PP->code;
	    } else {
		S = (pw1->val.ptr)++;
		PP++;
		pw1 = S;
		S = TG;
		TG = S + 2;
		pw1->val.ptr = S;
		pw1->tag.kernel = TLIST;
	    }
	    Next_Pp;

	Case(Push_list, I_Push_list)
	    S->val.ptr = TG;
	    ((S)++)->tag.kernel = TLIST;
	    TG += 2;
	    Next_Pp;

	Case(Write_first_structure, I_Write_first_structure)
	    (--SP)->tag.kernel = MODE_WRITE;
	    SP->val.ptr = S + 1;
	    Check_Local_Overflow
	    /* falls through */
	Case(Write_structure, I_Write_structure)
	    S->val.ptr = TG;
	    S->tag.kernel = TCOMP;
	    S = TG;
	    val_did = PP++->did;
	    TG += DidArity(val_did) + 1;
	    S->val.did = val_did;
	    ((S)++)->tag.kernel = TDICT;
	    Next_Pp;

	Case(Write_next_structureTM, I_Write_next_structureTM)
	    val_did = PP++->did;
	    Get_Temporary(pw1); 
	    pw1->val.ptr = S + 1;
	    S->val.ptr = TG;
	    S->tag.kernel = TCOMP;
	    S = TG;
	    TG += DidArity(val_did) + 1;
	    S->val.did = val_did;
	    ((S)++)->tag.kernel = TDICT;
	    Next_Pp;
	    
	Case(Write_next_structureTMlab, I_Write_next_structureTMlab)
	    Get_Temporary_Offs(1, pw1)
	    if(pw1->tag.kernel == MODE_READ) {
		PP = (PP+2)->code;
	    } else {
		S = (pw1->val.ptr)++;
		S->val.ptr = TG;
		S->tag.kernel = TCOMP;
		S = TG;
		val_did = PP->did;
		TG += DidArity(val_did) + 1;
		S->val.did = val_did;
		((S)++)->tag.kernel = TDICT;
		PP+=3;
	    }
	    Next_Pp;

	Case(Push_structure, I_Push_structure)
	    S->val.ptr = TG;
	    ((S)++)->tag.kernel = TCOMP;
	    TG = ByteOffsetPlus(TG, PP++->offset);
	    Next_Pp;

	Case(First, I_First)
	    (--SP)->tag.kernel = MODE_WRITE;
	    SP->val.ptr = S + 1;
	    Check_Local_Overflow
	    Next_Pp;

	Case(NextTM, I_NextTM)
	    Get_Temporary(pw1);
	    pw1->val.ptr = S + 1;
	    Next_Pp;

	Case(ModeTM, I_ModeTM)
	    Get_Temporary(pw1)
	    S = pw1->val.ptr;
	    Next_Pp;

	Case(NextTMlab, I_NextTMlab)
	    Get_Temporary(pw1)
	    if(pw1->tag.kernel == MODE_READ) {
		PP = PP->code;
	    } else {
		S = (pw1->val.ptr)++;
		PP++;
	    }
	    Next_Pp;

	Case(ModeTMlab, I_ModeTMlab)
	    Get_Temporary(pw1)
	    S = pw1->val.ptr;
	    if(pw1->tag.kernel == MODE_READ) {
		PP = PP->code;
	    } else {
		PP++;
	    }
	    Next_Pp;


/**** Regular subgoal arguments instructions ****/

	Case(Put_variableAML, I_Put_variableAML)
	    Get_Argument(pw2)
	    Get_Local(pw1)
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = TREF;
	    pw2->val.ptr = pw1;
	    pw2->tag.kernel = TREF;
	    Next_Pp;

	Case(Put_variable2AM, I_Put_variable2AM)
	    Get_Argument(pw1)
	    pw1->val.ptr = TG;
	    pw1->tag.kernel = TREF;
	    pw1 = TG++;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = TREF;
	    /* falls through */
	Case(Put_global_variableAM, I_Put_global_variableAM)
	Case(Put_variableAM, I_Put_variableAM)
	    Get_Argument(pw1)
	    pw1->val.ptr = TG;
	    pw1->tag.kernel = TREF;
	    pw1 = TG++;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = TREF;
	    Next_Pp;

	Case(Put_global_variable2AML, I_Put_global_variable2AML)
	    Get_Argument(pw1)
	    Get_Local(pw2)
	    pw1->val.ptr = pw2->val.ptr = TG;
	    pw1->tag.kernel = pw2->tag.kernel = TREF;
	    pw1 = TG++;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = TREF;
	    /* falls through */
	Case(Put_global_variableAML, I_Put_global_variableAML)
	    Get_Argument(pw1)
	    Get_Local(pw2)
	    pw1->val.ptr = pw2->val.ptr = TG;
	    pw1->tag.kernel = pw2->tag.kernel = TREF;
	    pw1 = TG++;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = TREF;
	    Next_Pp;

	Case(Put_global_variableL, I_Put_global_variableL)
	    Get_Local(pw1)
	    pw1->val.ptr = TG;
	    pw1->tag.kernel = TREF;
	    pw1 = TG++;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = TREF;
	    Next_Pp;

	Case(Put_named_variableAM, I_Put_named_variableAM)
	    Get_Argument(pw1)
	    pw1->val.ptr = TG;
	    pw1->tag.kernel = TREF;
	    pw1 = TG++;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Put_named_variableAML, I_Put_named_variableAML)
	    Get_Argument(pw1)
	    Get_Local(pw2)
	    pw1->val.ptr = pw2->val.ptr = TG;
	    pw1->tag.kernel = pw2->tag.kernel = TREF;
	    pw1 = TG++;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Put_named_variableL, I_Put_named_variableL)
	    Get_Local(pw1)
	    pw1->val.ptr = TG;
	    pw1->tag.kernel = TREF;
	    pw1 = TG++;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Put_referenceAM, I_Put_referenceAM)
	    Get_Argument(pw1)
	    S = TG;
	    TG = ByteOffsetPlus(TG, PP++->offset);
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    S->tag.kernel = PP++->kernel;
	    Next_Pp;

	/* temporary */
	Case(Put_referenceAML, I_Put_referenceAML)
	    Get_Argument(pw2)
	    Get_Local(pw1)
	    S = TG;
	    TG = ByteOffsetPlus(TG, PP++->offset);
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    pw2->val.ptr = S;
	    pw2->tag.kernel = TREF;
	    S->val.ptr = S;
	    S++->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Put_unsafe_valueAMTM, I_Put_unsafe_valueAMTM)
	    Get_Argument(pw2)
	    Get_Temporary(pw1)
	    /* temporaries are always popped, no matter if nondet or not */
	    goto _globalize_if_needed_;

	Case(Put_unsafe_valueAML, I_Put_unsafe_valueAML)
	    Get_Argument(pw2)
	    Get_Local(pw1)
	    if(E < EB) {
_globalize_if_needed_:
		Dereference_Pw_Tag(pw1,tmp1)
		if(ISVar(tmp1)) {
		    if (pw1 < E && pw1 >= SP && pw1 < EB) {
			pw1->val.ptr = pw2->val.ptr = TG;
			/* pw1->tag.kernel = TREF; */
			pw2->tag.kernel = TREF;
			pw1 = TG++;
			pw1->val.ptr = pw1;
			pw1->tag.kernel = TREF;
		    } else {
			pw2->val.ptr = pw1;
			pw2->tag.kernel = TREF;
		    }
		} else {
		    pw2->val.all = pw1->val.all;
		    pw2->tag.kernel = tmp1;
		}
	    } else {
	        *pw2 = *pw1;
	    }
	    Next_Pp;

        /* AM, tag, val */

        Case(Put_constantAM, I_Put_constantAM)
            Get_Argument(pw1);
            pw1 -> tag.all = PP++ -> all;
            pw1 -> val.all = PP++ -> all;
            Next_Pp;

	Case(Put_nilAM, I_Put_nilAM)
	    Get_Argument(pw1)
	    pw1->tag.kernel = TNIL;
	    Next_Pp;

	Case(Put_integerAM, I_Put_integerAM)
	    Get_Argument(pw1);
	    pw1->val.nint = PP++->nint;
	    pw1->tag.kernel = TINT;
	    Next_Pp;

#ifdef TFLOAT
	Case(Put_floatAM, I_Put_floatAM)
	    Get_Argument(pw1);
	    pw1->val.real = PP++->real;
	    pw1->tag.kernel = TFLOAT;
	    Next_Pp;
#endif

	Case(Put_atomAM, I_Put_atomAM)
	    Get_Argument(pw1);
	    pw1->val.did = PP++->did;
	    pw1->tag.kernel = TDICT;
	    Next_Pp;

	Case(Put_moduleAM, I_Put_moduleAM)
	    Get_Argument(pw1);
	    Make_Marked_Module(pw1, PP->did);
	    ++PP;
	    Next_Pp;

	Case(Put_stringAM, I_Put_stringAM)
	    Get_Argument(pw1);
	    pw1->val.str = PP++->str;
	    pw1->tag.kernel = TSTRG;
	    Next_Pp;

	Case(Put_listAM, I_Put_listAM)
	    Get_Argument(pw1)
	    S = TG;
	    TG = S + 2;
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TLIST;
	    Next_Pp;

	Case(Put_structureAM, I_Put_structureAM)
	    Get_Argument(pw1)
	    S = TG;
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TCOMP;
	    val_did = PP++->did;
	    TG += DidArity(val_did) + 1;
	    S->val.did = val_did;
	    ((S)++)->tag.kernel = TDICT;
	    Next_Pp;

	Case(Puts_variable, I_Puts_variable)
	    (--SP)->tag.kernel = TREF;
	    SP->val.ptr = SP;
	    Check_Local_Overflow
	    Next_Pp;

	Case(Puts_variableL, I_Puts_variableL)
	    Get_Local(pw1)
	    (--SP)->tag.kernel = TREF;
	    SP->val.ptr = pw1;
	    Check_Local_Overflow
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = TREF;
	    Next_Pp;

	Case(Puts_reference, I_Puts_reference)
	    S = TG;
	    TG = ByteOffsetPlus(TG, PP++->offset);
	    (--SP)->val.ptr = S;
	    SP->tag.kernel = TREF;
	    Check_Local_Overflow
	    S->val.ptr = S;
	    S->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Puts_referenceL, I_Puts_referenceL)
	    Get_Local(pw1)
	    S = TG;
	    TG = ByteOffsetPlus(TG, PP++->offset);
	    (--SP)->val.ptr = S;
	    SP->tag.kernel = TREF;
	    Check_Local_Overflow
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    S++->tag.kernel = PP++->kernel;
	    Next_Pp;

	Case(Puts_valueAM, I_Puts_valueAM)
	    Get_Argument(pw1)
	    Dereference_Pw(pw1)
	    *(--SP) = *pw1;
	    Check_Local_Overflow
	    Next_Pp;

	Case(Puts_valueL, I_Puts_valueL)
	    Get_Local(pw1)
	    Dereference_Pw(pw1)
	    *(--SP) = *pw1;
	    Check_Local_Overflow
	    Next_Pp;

	Case(Puts_valueTM, I_Puts_valueTM)
	    Get_Temporary(pw1)
	    Dereference_Pw(pw1)
	    *(--SP) = *pw1;
	    Check_Local_Overflow
	    Next_Pp;

	Case(Puts_valueG, I_Puts_valueG)
            (--SP)->tag.all = TREF;
            SP->val.ptr = ByteOffsetPlus(S, PP++->offset);
	    Check_Local_Overflow
	    Next_Pp;

        /* tag, val */

        Case(Puts_constant, I_Puts_constant)
            (--SP) -> tag.all = PP++ -> all;
            SP -> val.all = PP++ -> all;
            Next_Pp;

	Case(Puts_nil, I_Puts_nil)
	    (--SP)->tag.kernel = TNIL;
	    Check_Local_Overflow
	    Next_Pp;

	Case(Puts_integer, I_Puts_integer)
	    (--SP)->tag.kernel = TINT;
	    SP->val.nint = PP++->nint;
	    Check_Local_Overflow
	    Next_Pp;

#ifdef TFLOAT
	Case(Puts_float, I_Puts_float)
	    (--SP)->tag.kernel = TFLOAT;
	    SP->val.real = PP++->real;
	    Check_Local_Overflow
	    Next_Pp;
#endif

	Case(Puts_atom, I_Puts_atom)
	    (--SP)->tag.kernel = TDICT;
	    SP->val.did = PP++->did;
	    Check_Local_Overflow
	    Next_Pp;

	Case(Puts_string, I_Puts_string)
	    (--SP)->tag.kernel = TSTRG;
	    SP->val.str = PP++->str;
	    Check_Local_Overflow
	    Next_Pp;

	Case(Puts_list, I_Puts_list)
	    S = TG;
	    TG += 2;
	    (--SP)->tag.kernel = TLIST;
	    SP->val.ptr = S;
	    Check_Local_Overflow
	    Next_Pp;

	Case(Puts_structure, I_Puts_structure)
	    S = TG;
	    (--SP)->tag.kernel = TCOMP;
	    SP->val.ptr = S;
	    Check_Local_Overflow
	    val_did = PP++->did;
	    TG += DidArity(val_did) + 1;
	    S->val.did = val_did;
	    ((S)++)->tag.kernel = TDICT;
	    Next_Pp;

	/* this is really the same as Puts_integer, but the parameter type
	 * is different (important for disasm/fcompile). We cannot share the
	 * code because then threaded code disassembles to the same instruction
	 */
	Case(Puts_proc, I_Puts_proc)
	    (--SP)->tag.kernel = TINT;
	    SP->val.nint = PP++->nint;
	    Check_Local_Overflow
	    Next_Pp;


/***********************************************
 * OR-level instructions

 ECLiPSe 5.X compiler:

 Main sequence for clause choicepoints:
     Try_me_else	debug arity elselabel
	 <clause1>
     Retry_me_else	debug elselabel
	 <clause2>
     Trust_me		debug
	 <clause3>

 Sub-sequences:
     Try		debug arity melabel
     Retry		debug melabel
     Trust		debug melabel

 Sub-sequence can share tails via:
     Trylab		debug arity melabel elselabel

     Retrylab		debug melabel elselabel

     Trust		debug melabel

 Inline disjunctions (no subsequences used):
     Try_me_else	debug arity elselabel
	 <branch1>
     Retry_me_inline	debug elselabel EAM
	 <branch2>
     Trust_me_inline	debug EAM
	 <branch3>


 ECLiPSe 6.X compiler:

 Main sequences:
     Try_me_else	debug arity elselabel
	 <branch1>
     Retry_me_inline	debug elselabel EAM
	 <branch2>
     Trust_me_inline	debug EAM
	 <branch3>

 Sub-sequences:
     Try		debug arity melabel
     Retry_inline	debug melabel EAM
     Trust_inline	debug melabel EAM

 ***********************************************/

#define BChpParArgs(top)	((pword *) (ChpPar(BPrev(top)) + 1))
#define BChpArgs(top)		((pword *) (Chp(BPrev(top)) + 1))
#define BLastArg(top)		((pword *) BTop(top) - 1)


	Case(Trust, I_Trust)				/* debug,alt */
	    back_code = PP;
	    DBG_PORT = PP->nint;
	    PP = PP[1].code;
	    goto _trust_me_;

	Case(Trust_me_inline, I_Trust_me_inline)	/* debug,envsize */
	    back_code = PP;
	    DBG_PORT = PP->nint;
	    PP += 2;
	    goto _trust_me_;

	/* Operationally the same as Trust, but points to a branch of an
	 * inline disjunction rather than a clause.
	 * We must make sure that the C compiler does not merge Trust and
	 * Trust_inline, because the opcodes must remain distinguishable! */
	Case(Trust_inline, I_Trust_inline)		/* debug,alt,envsize */
	    back_code = PP;
	    DBG_PORT = PP->nint;
	    PP = PP[1].code;
	    goto _trust_me_;

	Case(Trust_me, I_Trust_me)			/* debug */
	    back_code = PP;
	    DBG_PORT = PP++->nint;
_trust_me_:					/* (back_code,PP,DBG_PORT) */
#ifdef NEW_ORACLE
	    if (FO && NTRY==0)
		goto _recomp_err_;
#endif
	    pw2 = BChpArgs(B.args);
	    Record_Next_Alternative;
_pop_choice_point_:			/* (pw2 points to arguments,DBG_PORT) */
	    /* Tracer hook before failure: save debug stack data to FTRACE */
	    if (TD)	/* find out how deep we fail */
	    {
		FDROP = 0;
		if (!OldStamp(&TD[TF_CHP_STAMP]))
		    FCULPRIT = DInvoc(TD);
		for (pw1 = TD; pw1 && !OldStamp(&pw1[TF_CHP_STAMP]); pw1 = DAncestor(pw1), ++FDROP)
		{
		    /*p_fprintf(log_output_, "\n(%d) %d fail", DInvoc(pw1), DLevel(pw1));*/
		    if (FDROP < MAX_FAILTRACE)
		    {
			FTRACE[FDROP].invoc = DInvoc(pw1);
			FTRACE[FDROP].proc = DProc(pw1);
			FTRACE[FDROP].source_pos.file = DPath(pw1);
			FTRACE[FDROP].source_pos.line = DLine(pw1);
			FTRACE[FDROP].source_pos.from = DFrom(pw1);
			FTRACE[FDROP].source_pos.to = DTo(pw1);
		    }
		}
		RLEVEL = pw1 ? DLevel(pw1) : -1;
		DBG_DELAY_INVOC = 0;		/* if set for DEBUG_DELAY_EVENT */
	    }
	    else { RLEVEL = -1; FDROP = 0; }

	    b_aux.top = BTop(B.args);
	    tmp1 = b_aux.args-pw2;		/* arity */
	    pw1 = &A[1];
	    while (pw2 < b_aux.args) {
		*pw1++ = *pw2++;
	    }
	    pw2 = BPrev(B.args);
	    /* note the order: untrail, then reset stack pointers */
            Untrail_Variables(Chp(pw2)->tt, i, pw1);
            SP = Chp(pw2)->sp;
            E = Chp(pw2)->e;
            LD = Chp(pw2)->ld;
	    Wipe(Chp(pw2)->tg,TG);
	    TG = Chp(pw2)->tg;
	    Adjust_GcTg_and_TgSl(TG);
	    Reset_Unify_Exceptions
            Set_Det
            Reset_DE;
	    Debug_Check_Global

	    /* Tracer hook after failure:
	     * Here we trace one or more FAIL, one or more REDO, and a single
	     * NEXT or ELSE (modulo some of these ports being filtered out).
	     * At this point the true debugger stack may already be empty,
	     * but we still may have to trace some FAIL ports (FDROP>0).
	     * We exploit the choicepoint for state-saving across the call to
	     * the DEBUG_REDO_EVENT handler (which must fail): we keep the
	     * choicepoint around, and arrange for the same Trust* instruction
	     * to be executed once more after handler return. To suppress the
	     * debug handler to be called again, we set the TF_REDO flag in
	     * the top trace frame.
	     */
	    if (FDROP > 0  &&  PortWanted(FAIL_PORT))
		goto _trace_trust_;
	    if (TD)
	    {
		if (RLEVEL != DLevel(TD)  &&  PortWanted(PREDO_PORT))
		    goto _trace_trust_;	/* not 2nd time */
	        if (Unskipped(TD))
		{
		    if (!(TfFlags(TD) & TF_REDO) && (DBG_PORT&PORT_MASK) && PortWanted(DBG_PORT&PORT_MASK))
			goto _trace_trust_;
		    Clr_Tf_Flag(TD, TF_REDO);
		}
	    }
            EB = BChp(pw2)->sp;	/* finish resetting state */
            GB = BChp(pw2)->tg;
	    B.args = pw2;	/* and pop the choicepoint */
            Next_Pp;

_trace_trust_:				/* (DBG_PORT,FDROP,RLEVEL,tmp1) */
	    /* Make it look as if retrying the Trust instructions. Note that
	     * setting the BP field is necessary in exotic cases like notnot,
	     * where the trust instruction is not reached via a failure! */
	    BBp(B.args) = (vmcode *) back_code-1;
	    EB = SP; GB = TG;
	    Push_Witness;
_trace_redo_:				/* (DBG_PORT,FDROP,RLEVEL,tmp1) */
	    if (TD)
		Set_Tf_Flag(TD, TF_REDO);
	    /* After a clause choicepoint we must push an auxiliary
	     * empty environment to be able to make the handler call.
	     * In case of an inline choicepoint we can insert a call directly.
	     * Note that the environment size in front of the Failure
	     * continuation is zero, but the environment will still get
	     * marked correctly because the choicepoints still points to
	     * this alternative and has a correct environment map.
	     */
	    if (!(DBG_PORT & INLINE_PORT))
	    {
		Push_Env
	    }
	    Push_Ret_Code((emu_code)&fail_return_env_0_[1])
	    Check_Local_Overflow
	    Set_Det
	    proc = error_handler_[-(DEBUG_REDO_EVENT)];
	    PP = (emu_code) PriCode(proc);
	    A[1] = TAGGED_TD;
	    Make_Integer(&A[2], FDROP);
	    Make_Integer(&A[3], RLEVEL);
	    Make_Integer(&A[4], FAIL_PORT);
	    Make_Integer(&A[5], DBG_PORT&PORT_MASK);	/* NO/NEXT/ELSE_PORT */
	    Next_Pp;



	/*
	 * We assume that Try_parallel, Retry_seq, and Retry_par
	 * appear always in sequence.
	 *
	 * Note about LOAD register:
	 * LOAD == 0	No unpublished parallel choicepoints
	 * LOAD > 0	At most LOAD unpublished parallel choicepoints
	 *		(not precise because it is not updated at cuts)
	 * LOAD < 0	Delayed load release phase
	 */
/*-----------------------------------------------------------------------*/
	Case(Try_parallel, I_Try_parallel)	/* nalt arity table */
/*-----------------------------------------------------------------------*/
	    tmp1 = PP[1].nint;			/* arity */
	    back_code = PP + 3;			/* &Retry_seq */
#ifdef NEW_ORACLE
	    err_code = 0;
	    if (FO)				/* we are following */
	    {
		if (NTRY > 1) {			/* old counter not expired */
		    NTRY--;
		} else if (FoIsStop(i=FoHeader(FO))) {	/* end of oracle */
		    goto _recomp_err_;
		} else if (FoIsCount(i)) {	/* new counter */
		    NTRY = FoCount(FO,i);
		} else if (!FoIsPar(i)) {
		    goto _recomp_err_;
		} else {			/* follow given alternative */
		    NTRY = 0;
		    err_code = FoIsCreate(i) ? O_FROM_ORACLE
					  : O_FROM_ORACLE|O_NOCREATE;
		    i = FoAlt(FO,i);
		    if (PP[2].code) {		/* static par chp */
			PP = PP[2].code[i].code;
		    } else {
			A[1].val.nint = i;
			A[1].tag.kernel = TINT;
			PP = (emu_code) fork_unify_code_;
		    }
		    goto _try_par_1_;
		}
	    }
#endif
	    if (PP[2].code)			/* static par chp */
	    {
		i = PP[0].nint;			/* nalt */
		PP = PP[2].code[i].code;
	    }
	    else
	    {
		pw2 = pw1 = &A[1];		/* fork/2 */
		Dereference_Pw(pw1)
		/* assume argument is already checked for integer > 1 */
		/* store deref value in chp, otherwise gc/copy problem */
		i = pw2->val.nint = pw1->val.nint;
		pw2->tag.kernel = TINT;
		PP = (emu_code) fork_unify_code_;
	    }
#ifdef NEW_ORACLE
_try_par_1_:	/* (i:alt, tmp1:arity, back_code, err_code) */
	    Record_Alternative(i, O_PAR_ORACLE|(err_code & O_FROM_ORACLE? 0: O_SHALLOW));
	    if (err_code & O_NOCREATE) { Next_Pp; }
#endif
	    /* create the choicepoint */
	    Clr_Det;
	    pw1 = B.args;
	    ChpPar(pw1)->sp = EB = SP;
	    ChpPar(pw1)->tg = GB = TG;
	    Push_Witness
	    Adjust_GcTg_and_TgSl(TG);
	    ChpPar(pw1)->tt = TT;
	    ChpPar(pw1)->e = E;
	    ChpPar(pw1)->ld = LD;
	    ChpPar(pw1)->alt = i;
#ifdef PB_MAINTAINED
	    ChpPar(pw1)->ppb = PB;
#else
	    ChpPar(pw1)->ppb = (pword *) 0;
#endif
#ifdef NEW_ORACLE
	    if (err_code & O_FROM_ORACLE)
	    {
		Fo_Node(FO, &ChpPar(pw1)->node);
	    }
#endif
	    pw1 = (pword *) (ChpPar(pw1) + 1);
	    for (pw2 = &A[1]; tmp1 > 0; tmp1--)
		*pw1++ = *pw2++;
	    Top(pw1)->backtrack = (vmcode *) back_code;	/* &Retry_seq */
	    Top(pw1)->frame = B.any_frame;
	    B.top = Top(pw1) + 1;
#ifdef PB_MAINTAINED
	    PB = B.args;
#endif
	    /* Clr_Det (moved up) */
	    Check_Control_Overflow
#ifdef NEW_ORACLE
	    if (FO && FoEnd(FO))	/* end of oracle following */
	    {
		Export_All
		end_of_oracle();
		Import_All
		Next_Pp;
	    }
#endif
	    if (LEAF && i > 1 && !PO)
	    {
		if (LOAD == 0)
		{
		    if ((LOAD = LoadReleaseDelay) < 0)	/* init countdown */
		    {
			Start_Countdown();
			Fake_Overflow;
		    }
		    else
		    {
			/* LOAD = 1; */
			Export_B_Sp_Tg_Tt
			sch_load_report(LEAF);
			Import_None
		    }
		}
		else if (LOAD < 0)
		{
		    /* We have two Try_parallel in quick succession:
		     * Abort countdown, report the load immediately */
		    Stop_Countdown();
		    LOAD = 2;
		    Export_B_Sp_Tg_Tt
		    sch_load_report(LEAF);
		    Import_None
		}
		else
		    ++LOAD;				/* count chp */
	    }
            Next_Pp;

/*-----------------------------------------------------------------------*/
	Case(Retry_seq, I_Retry_seq)		/* table */
/*-----------------------------------------------------------------------*/
#ifdef NEW_ORACLE
	    if (FO && NTRY==0)
		goto _recomp_err_;
#endif
	    pw1 = BPrev(B.args);
	    tmp1 = ChpPar(pw1)->alt - 1;	/* next alternative number */
	    pw2 = BChpParArgs(B.args);
	    if (PP[0].code)
		PP = PP[0].code[tmp1].code;	/* clause address */
	    else
	    {
		PP = (emu_code) fork_unify_code_; /* it's a fork/2 chp */
		pw2->tag.kernel = TINT;
		pw2->val.nint = tmp1;
	    }
	    Update_Recorded_Alternative(tmp1);
	    if (tmp1 > 1) {
		ChpPar(pw1)->alt = tmp1;
		DBG_PORT = NO_PORT;
		if (LOAD < 0)
		    LOAD = LoadReleaseDelay;	/* reinit countdown */
		goto _read_choice_point_;	/* (pw2,err_code) */
	    } else {
#ifdef PB_MAINTAINED
		PB = ChpPar(pw1)->ppb;
#endif
		DBG_PORT = NO_PORT;
		if (LOAD < 0)
		{
		    Stop_Countdown();		/* exhausted before released */
		    LOAD = 0;
		}
		else if (LOAD > 0)
		{
		    --LOAD;			/* keep load updated */
		}
		goto _pop_choice_point_;	/* (pw2,DBG_PORT) */
	    }

	/* Retry_par &table
	 * is split into two instructions while the handler is in Prolog:
	 * Fail_clause 2
	 * Try_clause &table
	 */
/*-----------------------------------------------------------------------*/
	Case(Fail_clause, I_Fail_clause)	/* envsize(=2) */
/*-----------------------------------------------------------------------*/
#ifdef NEW_ORACLE
	    if (FO && NTRY==0)
		goto _recomp_err_;
#endif
#ifdef PROLOG_SCHED
	    proc = error_handler_[-(FAIL_TO_PAR_CHP)];
	    pw2 = (B.top - 1)->frame.args;	/* partially restore state */
	    Untrail_Variables(ChpPar(pw2)->tt, i, pw1);
	    SP = EB = ChpPar(pw2)->sp;
	    Wipe(ChpPar(pw2)->tg,TG);
	    TG = GB = ChpPar(pw2)->tg;
	    Adjust_GcTg_and_TgSl(TG);
            LD = ChpPar(pw2)->ld;
	    E = ChpPar(pw2)->e;
	    /* no need to restore arguments */
	    Reset_Unify_Exceptions
	    Reset_DE;
	    /* don't reset Det flag */

	    /* Call the handler */

	    Push_Env				/*  Allocate 1 */
	    (--SP)->tag.kernel = TCUT;		/* Y1 = cut */
	    SP->val.ptr = B.args;
	    (--SP)->tag.kernel = TREF;		/* Y2 = Alt */
	    SP->val.ptr = SP;
	    A[3].val.ptr = SP;
	    A[3].tag.kernel = TREF;
	    (--SP)->tag.kernel = TREF;		/* Y3 = FailCnt */
	    SP->val.ptr = SP;
	    A[2].val.ptr = SP;
	    A[2].tag.kernel = TREF;
	    Push_Ret_Code(PP + 1)		/* Try_clause */
	    Check_Local_Overflow

	    A[1].val.ptr = B.args;
	    A[1].tag.kernel = TCUT;

	    Set_Det
	    PP = (emu_code) PriCode(proc);
#else /* if !PROLOG_SCHED */
	    PP++;			/* skip environment size	*/
	    if (LOAD < 0) {
		Stop_Countdown();
	    }
	    LOAD = 0;
	    Export_All
	    get_job();
	    Import_All
#endif /* PROLOG_SCHED */
	    Next_Pp;


/*-----------------------------------------------------------------------*/
	Case(Try_clause, I_Try_clause)		/* table */
/*-----------------------------------------------------------------------*/
#ifdef PROLOG_SCHED
	    BAlt(B.args) = (E - 2)->val.nint;	/* scheduled alternative */
	    pw1 = (E - 1)->val.ptr;		/* cut the handler */
	    Cut_To(pw1)
	    for (tmp1 = (E - 3)->val.nint; tmp1; --tmp1)
	    {
		PPB = BPar(PPB)->ppb;		/* pop */
	    }
#ifdef PB_MAINTAINED
	    PB =
#endif
	    B.args = PPB;
	    Pop_Env
#endif /* PROLOG_SCHED */
	    /* get alternative from oracle or choicepoint */
	    tmp1 = BAlt(B.args);

	    if (tmp1)
	    {
		pw2 = BChpParArgs(B.args);
		if (PP[0].code)
		    PP = PP[0].code[tmp1].code;	/* clause address */
		else
		{
		    PP = (emu_code) fork_unify_code_; /* it's a fork/2 chp */
		    pw2->tag.kernel = TINT;
		    pw2->val.nint = tmp1;
		}
		DBG_PORT = NO_PORT;
		if (PPB < B.args) {
		    goto _pop_choice_point_;	/* (pw2,DBG_PORT) */
		} else {
		    back_code = (emu_code) BBp(B.args);	/* leave unchanged */
		    goto _read_choice_point_;	/* (pw2,DBG_PORT,back_code) */
		}
	    }
	    else /* fail through */
	    {
		PPB = (B.top-1)->frame.chp_par->ppb;
	    	goto _do_refail_;
	    }


/*-----------------------------------------------------------------------*/
	Case(Try_me_else, I_Try_me_else)	/* debug arity alt */
/*-----------------------------------------------------------------------*/
	    tmp1 = PP[1].nint;
	    back_code = PP[2].code;
	    PP += 3;
_make_choice_point_:			/* (arity in tmp1, back_code)	*/
#ifdef NEW_ORACLE
	    err_code = 0;
	    if (FO)				/* we are following */
	    {
		if (NTRY > 1) {			/* old counter not expired */
		    NTRY--;
		} else if (FoIsStop(i=FoHeader(FO))) {	/* end of oracle */
		    goto _recomp_err_;
		} else if (FoIsCount(i)) {	/* new counter */
		    NTRY = FoCount(FO,i);
		} else if (FoIsPar(i)) {
		    goto _recomp_err_;
		} else {			/* follow given alternative */
		    NTRY = 0;
		    err_code = FoIsCreate(i) ? O_FROM_ORACLE
					  : O_FROM_ORACLE|O_NOCREATE;
		    i = FoAlt(FO,i);
		    Find_Alternative(i);	/* update PP and back_code */
		    goto _try_1_;
		}
	    }
	    i=1;
_try_1_:
	    Record_Alternative(i, err_code & O_FROM_ORACLE? 0 : O_SHALLOW);
	    if (err_code & O_NOCREATE) { Next_Pp; }
#endif
	    Clr_Det
	    pw1 = B.args;
	    Chp(pw1)->sp = EB = SP;
	    Chp(pw1)->tg = GB = TG;
	    Push_Witness
	    Chp(pw1)->tt = TT;
	    Chp(pw1)->e = E;
	    Chp(pw1)->ld = LD;
	    pw1 = (pword *) (Chp(pw1) + 1);
	    for (pw2 = &A[1]; tmp1 > 0; tmp1--)
		*pw1++ = *pw2++;
	    Top(pw1)->backtrack = (vmcode *) back_code;
	    Top(pw1)->frame = B.any_frame;
	    B.top = Top(pw1) + 1;
	    Check_Control_Overflow
	    Next_Pp;

	Case(Try, I_Try)		/* debug arity clause */
	    tmp1 = PP[1].nint;
	    back_code = PP + 3;
	    PP = PP[2].code;
	    goto _make_choice_point_;

	Case(Trylab, I_Trylab)		/* debug arity clause alt */
	    tmp1 = PP[1].nint;
	    back_code = PP[3].code;
	    PP = PP[2].code;
	    goto _make_choice_point_;

	Case(Retry_me_inline, I_Retry_me_inline)	/* debug alt envsize */
	    DBG_PORT = PP->nint;
	    back_code = PP[1].code;
	    PP += 3;	/* skip debug-flag, label and env size */
	    goto _retry_me_;		/* (DBG_PORT,back_code) */

	Case(Retry_me_else, I_Retry_me_else)		/* debug alt */
	    DBG_PORT = PP->nint;
	    back_code =  PP[1].code;
	    PP += 2;
_retry_me_:				/*  (PP,DBG_PORT,back_code) */
	    pw2 = BChpArgs(B.args);
	    Record_Next_Alternative;
#ifdef NEW_ORACLE
	    if (FO && NTRY==0)
		goto _recomp_err_;
#endif
_read_choice_point_:			/* (pw2 points to args, DBG_PORT,back_code) */
	    /* Tracer hook before failure: save debug stack data to FTRACE */
	    if (TD)	/* find out how deep we fail */
	    {
		FDROP = 0;
		if (!OldStamp(&TD[TF_CHP_STAMP]))
		    FCULPRIT = DInvoc(TD);
		for (pw1 = TD; pw1 && !OldStamp(&pw1[TF_CHP_STAMP]); pw1 = DAncestor(pw1), ++FDROP)
		{
		    /*p_fprintf(log_output_, "\n(%d) %d fail", DInvoc(pw1), DLevel(pw1));*/
		    if (FDROP < MAX_FAILTRACE)
		    {
			FTRACE[FDROP].invoc = DInvoc(pw1);
			FTRACE[FDROP].proc = DProc(pw1);
			FTRACE[FDROP].source_pos.file = DPath(pw1);
			FTRACE[FDROP].source_pos.line = DLine(pw1);
			FTRACE[FDROP].source_pos.from = DFrom(pw1);
			FTRACE[FDROP].source_pos.to = DTo(pw1);
		    }
		}
		RLEVEL = pw1 ? DLevel(pw1) : -1;
		DBG_DELAY_INVOC = 0;		/* if set for DEBUG_DELAY_EVENT */
	    }
	    else { RLEVEL = -1; FDROP = 0; }

	    b_aux.top = BTop(B.args);
	    tmp1 = b_aux.args-pw2;		/* arity */
	    pw1 = &A[1];
	    while (pw2 < b_aux.args) {
		*pw1++ = *pw2++;
	    }
	    pw2 = BPrev(B.args);
	    Untrail_Variables(Chp(pw2)->tt, i, pw1);
	    SP = EB = Chp(pw2)->sp;
	    Wipe(Chp(pw2)->tg,TG);
	    TG = GB = Chp(pw2)->tg;
	    Push_Witness
	    Adjust_GcTg_and_TgSl(TG);
            LD = Chp(pw2)->ld;
	    E = Chp(pw2)->e;
	    Reset_Unify_Exceptions
	    Clr_Det
	    Reset_DE;
	    Debug_Check_Global

	    /* Tracer hook after failure: call DEBUG_REDO_EVENT handler.
	     * Don't update the alternative if calling the trace handler
	     * vecause the retry instruction will be executed again! */
	    if (FDROP > 0  &&  PortWanted(FAIL_PORT))
	    	goto _trace_redo_;
	    if (TD)
	    {
		if (RLEVEL != DLevel(TD)  &&  PortWanted(PREDO_PORT))
		    goto _trace_redo_;
	        if (Unskipped(TD))
		{
		    if (!(TfFlags(TD) & TF_REDO) && (DBG_PORT&PORT_MASK) && PortWanted(DBG_PORT&PORT_MASK))
			goto _trace_redo_;
		    Clr_Tf_Flag(TD, TF_REDO);
		}
	    }
	    /* not debugging, update the alternative */
	    BBp(B.args) = (vmcode *) back_code;
	    Next_Pp;

	Case(Retry, I_Retry)			/* debug clause */
	    DBG_PORT = PP->nint;
	    back_code = (PP + 2);
	    PP = PP[1].code;
	    goto _retry_me_;		/* (DBG_PORT,back_code) */

	Case(Retrylab, I_Retrylab)		/* debug clause alt */
	    DBG_PORT = PP->nint;
	    back_code = PP[2].code;
	    PP = PP[1].code;
	    goto _retry_me_;		/* (DBG_PORT,back_code) */

	/* Operationally the same as Retry, but points to a branch of an
	 * inline disjunction rather than a clause, and has envsize. */
	Case(Retry_inline, I_Retry_inline)	/* debug branch envsize */
	    DBG_PORT = PP->nint;
	    back_code = PP + 3;
	    PP = PP[1].code;
	    goto _retry_me_;		/* (DBG_PORT,back_code) */


/*
 * super-shallow backtracking instructions
 * for if-then-else with simple condition
 */
	Case(Set_bp, I_Set_bp)
	    pw1 = B.args;
	    Top(pw1)->backtrack = (vmcode *) PP++->code;
	    Top(pw1)->frame.args = pw1;
	    B.top = Top(pw1) + 1;
	    Next_Pp;

	Case(New_bp, I_New_bp)
	    (B.top - 1)->backtrack = (vmcode *) PP++->code;
	    Next_Pp;

	Case(Restore_bp, I_Restore_bp)
	    B.top -= 1;
	    Next_Pp;


#ifdef OLD_DYNAMIC
/*
 * Instructions for the dynamic predicates
 *
 *	(Re)Try_me_dynamic	birth, death, next, arity, gc/source
 *
 * We only make a choicepoint if there is a living alternative.
 * Hence all executed Retry_me_dynamic's belong to living clauses.
 */

        Case(Try_me_dynamic, I_Try_me_dynamic)
	    i = DynGlobalClock;			/* the current clock	*/
	    while (Dead((PP-1), i))
	    {
		PP = (PP+2)->code;		/* skip dead clauses	*/
		if (PP == FAIL) Fail;		/* all dead -> fail	*/
		PP += 1;
	    }
	    back_code = (PP+2)->code;
	    tmp1 = (PP+3)->nint & SRC_CLAUSE_ARITY_MASK;
	    PP += DYNAMIC_INSTR_SIZE - 1; /* start of first living clause */

	    while (back_code != FAIL)	/* look for living alternative	*/
	    {
		if (!Dead(back_code, i))
		{
		    A[++tmp1].val.nint = i;	/* add call clock argument */
		    A[tmp1].tag.kernel = TINT;
		    goto _make_choice_point_;	/* (arity in tmp1, back_code) */
		}
		back_code = (back_code+3)->code;
	    }
	    Next_Pp;				/* single clause	*/


        Case(Retry_me_dynamic, I_Retry_me_dynamic)
			/* get the call clock (the last argument)	*/
	    i = ((pword *)(B.top - 1) - 1)->val.nint;
	    back_code = (PP+2)->code;
	    while (back_code != FAIL)	/* look for living alternative	*/
	    {
		if (!Dead(back_code, i))
		{
		    DBG_PORT = NEXT_PORT;
		    PP += DYNAMIC_INSTR_SIZE - 1;
		    goto _retry_me_;		/* (DBG_PORT,back_code)	*/
		}
		back_code = (back_code+3)->code;
	    }
						/* the last living clause */
	    back_code = PP;
	    PP += DYNAMIC_INSTR_SIZE - 1;
	    DBG_PORT = NEXT_PORT;
	    goto _trust_me_;			/* (back_code,PP,DBG_PORT) */
#endif


/***********************************************
 *	Indexing instructions
 ***********************************************/

	Case(Get_list_argumentsAM, I_Get_list_argumentsAM)
	    Get_Argument(pw1)
	    Dereference_Pw(pw1)
	    S = pw1->val.ptr;
	    Next_Pp;

	Case(Get_structure_argumentsAM, I_Get_structure_argumentsAM)
	    Get_Argument(pw1)
	    Dereference_Pw(pw1)
	    S = pw1->val.ptr + 1;
	    Next_Pp;


	Case(List_switchL, I_List_switchL)
	    Get_Local(pw1)
	    goto _list_switch_;

	Case(List_switchAM, I_List_switchAM)
	    Get_Argument(pw1)
_list_switch_:
	    Dereference_Pw_Tag(pw1,tmp1)
	    if(IsTag(tmp1,TLIST)) {
		PP = PP->code;
		S = pw1->val.ptr;
	    } else if(IsTag(tmp1,TNIL))
		PP = (PP + 1)->code;
	    else if(ISRef(tmp1))
	        PP += 3; /* skip the various labels */
	    else
		PP = (PP + 2)->code;
	    Next_Pp;


	Case(Atom_switchL, I_Atom_switchL)
	    Get_Local(pw1)
	    goto _atom_switch_;

	Case(Atom_switchAM, I_Atom_switchAM)
	    Get_Argument(pw1)
_atom_switch_:
	    Dereference_Pw_Tag(pw1,tmp1)
	    if (!IsTag(tmp1, TDICT)) {
		if (ISRef(tmp1))
		    PP += 3;
		else
		    PP = (PP + 2)->code;
		Next_Pp;
	    }
_fast_search_:				/* binary search (pw1, PP) */
	    Mark_Prof(_fast_search_)
	    i = pw1->val.nint;		/* i is unsigned! */
	    pw1 = (PP++)->ptr;		/* table start		*/
_fast_search1_:	/* i:value, pw1:table start, PP points to table size */
	    {
		int	l,u;

		l = 0;
		u = PP->offset;
		do
		{
		    tmp1 = (l+u)>>1;
		    if ((word)i < (word) pw1[tmp1].val.nint)
			u = tmp1;
		    else if ((word)i > (word) pw1[tmp1].val.nint)
			l = tmp1+1;
		    else
		    {
			PP = (emu_code) pw1[tmp1].tag.all;
			Next_Pp;
		    }
		} while (u > l);
		PP = (PP + 1)->code;	/* default		*/
		Next_Pp;
	    }


	Case(Integer_switchL, I_Integer_switchL)
	    Get_Local(pw1)
	    goto _integer_switch_;

	Case(Integer_switchAM, I_Integer_switchAM)
	    Get_Argument(pw1)
_integer_switch_:
	    Dereference_Pw_Tag(pw1,tmp1)
	    if (IsTag(tmp1, TINT))
		goto _fast_search_;
	    else if (ISRef(tmp1))
		PP += 3;
	    else
		PP = (PP + 2)->code;
	    Next_Pp;


	Case(Functor_switchL, I_Functor_switchL)
	    Get_Local(pw1)
	    goto _functor_switch_;

	Case(Functor_switchAM, I_Functor_switchAM)
	    Get_Argument(pw1)
_functor_switch_:
	    Dereference_Pw_Tag(pw1,tmp1)
	    if (IsTag(tmp1, TCOMP)) {
		pw1 = pw1->val.ptr;		/* get the functor */
		S = pw1 + 1;
		goto _fast_search_;
	    } else if (ISRef(tmp1))
		PP += 3;
	    else
		PP = (PP + 2)->code;
	    Next_Pp;


	Case(Integer_range_switchL, I_Integer_range_switchL)
	    Get_Local(pw1)
	    goto _integer_range_switch_;

	Case(Integer_range_switchAM, I_Integer_range_switchAM)
	    Get_Argument(pw1)
_integer_range_switch_:
	    Dereference_Pw_Tag(pw1,tmp1)
	    if (IsTag(tmp1, TINT))
	    {
		Mark_Prof(_range_search_)
		{
		    i = pw1->val.nint;
		    pw1 = (PP++)->ptr;
		    if ((long) i < pw1->val.nint)
		        PP = (emu_code) (pw1->tag.all);
		    else if ((long) i > (++pw1)->val.nint)
		        PP = (emu_code) (pw1->tag.all);
		    else if (PP->nint == 0)	/* no further table */
		        PP = (PP + 1)->code;
		    else {
			++pw1;
		        goto _fast_search1_;	/* i,pw1,PP */
		    }
		    Next_Pp;
		}
	    }
	    else if (ISRef(tmp1))
		PP += 4;
	    else if (IsTag(tmp1,TBIG))
		PP = (emu_code) PP->ptr[BigNegative(pw1->val.ptr)?0:1].tag.all;
	    else
		PP = (PP + 3)->code;
	    Next_Pp;


	Case(Switch_on_typeL, I_Switch_on_typeL)
	    Get_Local(pw1);
	    Dereference_Pw_Tag(pw1,tmp1)
	    if (ISRef(tmp1)) {
		if (IsTag(tmp1, TMETA)) {
		    S = pw1->val.ptr;		/* so we can skip In_get_metaAM */
		    PP = (PP + TPTR)->code;
		} else
		    PP += NTYPES;
	    } else {
		PP = (PP + TagTypeC(tmp1))->code;
	    }
	    Next_Pp;

	Case(Switch_on_typeAM, I_Switch_on_typeAM)
	    Get_Argument(pw1)
_switch_on_type_:
	    pw2 = pw1;
	    Dereference_Pw_Tag(pw1,tmp1)
	    if (ISRef(tmp1)) {
		if (IsTag(tmp1, TMETA)) {
		    S = pw1->val.ptr;		/* so we can skip In_get_metaAM */
		    PP = (PP + TPTR)->code;
		} else
		    PP += NTYPES;
	    } else {
		pw2->val.all = pw1->val.all;	/* store dereferenced value */
		pw2->tag.kernel = tmp1;
		PP = (PP + TagTypeC(tmp1))->code;
	    }
	    Next_Pp;


/***********************************************
 *	Control instructions 
 ***********************************************/

	Case(Allocate, I_Allocate)
	    Alloc_Env
	    Next_Pp;

	Case(Deallocate, I_Deallocate)
	    if(E < EB)
	    {
		Pop_Env
		if(EB == SP)
		{
		    Repush_Ret_Code;
		}
	    }
	    else
	    {
		Push_Ret_Code_To_Eb(ERetCode)
	        Check_Local_Overflow
		E = ERetEnv;
	    }
	    Set_Det
	    Next_Pp;

	Case(Occur_check_next, I_Occur_check_next)
	    Occur_Check_Boundary(TG)
	    Next_Pp;

	Case(MoveLAMCallfA, I_MoveLAMCallfA)
	    Get_Local(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	    /* falls through */
	Case(CallfA, I_CallfA)
	    Set_Det
	Case(CallA, I_CallA)
	    Push_Ret_Code(PP + 2)
	    Check_Local_Overflow
	Case(JmpdA, I_JmpdA)
	    PP = PP->code;
	    Handle_Events_Call
	    Next_Pp;

	Case(Put_global_variableAMLCallfA, I_Put_global_variableAMLCallfA)
	    Get_Argument(pw1)
	    Get_Local(pw2)
	    pw1->val.ptr = pw2->val.ptr = TG;
	    pw1->tag.kernel = pw2->tag.kernel = TREF;
	    pw1 = TG++;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = TREF;
	    Set_Det
	    Push_Ret_Code(PP + 2)
	    Check_Local_Overflow
	    PP = PP->code;
	    Handle_Events_Call
	    Next_Pp;

	Case(JmpdAs, I_JmpdAs)
	    SP = ByteOffsetMinus(SP, PP++->offset);
	    PP = PP->code;
	    Handle_Events_Call
	    Next_Pp;

	Case(Branchs, I_Branchs)
	    SP = ByteOffsetMinus(SP, PP++->offset);
	Case(Branch, I_Branch)
	    PP = PP->code;
	    Next_Pp;

	Case(MoveLAMCallfP, I_MoveLAMCallfP)
	    Get_Local(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	    /* falls through */
	Case(CallfP, I_CallfP)
	    Set_Det
	Case(CallP, I_CallP)
	    Push_Ret_Code(PP + 2)
	    Check_Local_Overflow
	Case(JmpdP, I_JmpdP)
	    PP = (emu_code) PriCode(PP->proc_entry);
	    Handle_Events_Call
	    Next_Pp;

	Case(Put_global_variableAMLCallfP, I_Put_global_variableAMLCallfP)
	    Get_Argument(pw1)
	    Get_Local(pw2)
	    pw1->val.ptr = pw2->val.ptr = TG;
	    pw1->tag.kernel = pw2->tag.kernel = TREF;
	    pw1 = TG++;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = TREF;
	    Set_Det
	    Push_Ret_Code(PP + 2)
	    Check_Local_Overflow
	    PP = (emu_code) PriCode(PP->proc_entry);
	    Handle_Events_Call
	    Next_Pp;

	Case(MoveLAMChainP, I_MoveLAMChainP)
	    Get_Local(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	    /* falls through */
	Case(ChainP, I_ChainP)
	    if(E < EB) {
		Pop_Env
		if(EB == SP) {Repush_Ret_Code}
	    } else {
		Push_Ret_Code_To_Eb(ERetCode)
	        Check_Local_Overflow
		E = ERetEnv;
	    }
	    PP = (emu_code) PriCode(PP->proc_entry);
	    Set_Det
	    Handle_Events_Call
	    Next_Pp;

	Case(MoveLAMChainA, I_MoveLAMChainA)
	    Get_Local(pw1)
	    Get_Argument(pw2)
	    Move_Pw(pw1,pw2)
	    /* falls through */
	Case(ChainA, I_ChainA)
	    if(E < EB) {
		Pop_Env
		if(EB == SP) {Repush_Ret_Code}
	    } else {
		Push_Ret_Code_To_Eb(ERetCode)
		Check_Local_Overflow
		E = ERetEnv;
	    }
	    PP = PP->code;
	    Set_Det
	    Handle_Events_Call
	    Next_Pp;

	    /*
	     * We used to trigger GCs here, but that was felt to be too
	     * risky since we are not so sure about the machine state.
	     * Now we just expand the global stack if necessary.
	     */
	Case(Gc_test, I_Gc_test)	/* bytes_needed */
	    tmp1 = PP++->offset;
	    TG = ByteOffsetPlus(TG, tmp1);
	    Check_Gc
	    TG = ByteOffsetMinus(TG, tmp1);
	    Next_Pp;

	Case(Gc_testA, I_Gc_testA)	/* bytes_needed, arity */
	    tmp1 = PP->offset;
	    PP += 2;	/* arity is obsolete */
	    TG = ByteOffsetPlus(TG, tmp1);
	    Check_Gc
	    TG = ByteOffsetMinus(TG, tmp1);
	    Next_Pp;

	Case(Space, I_Space)
/* CAUTION: if Space is to be used to grab space, add an overflow check */
	    SP = ByteOffsetMinus(SP, PP++->offset);
	    Next_Pp;

	Case(Initialize, I_Initialize)	/* Initialize firstY, mask	*/
	    Get_Local(pw1)
	    i = (uword) PP++->nint;
	    pw1->val.ptr = pw1;
	    pw1->tag.kernel = TREF;
	    while (i != 0)
	    {
		--pw1;
		if (i & 1)
		{
		    pw1->val.ptr = pw1;
		    pw1->tag.kernel = TREF;
		}
		i = i >> 1;	/* important: i must be unsigned !	*/
	    }
	    Next_Pp;

	Case(Initialize_named, I_Initialize_named)
	/* Initialize firstY, mask, nam1, name2, ...	*/
	    Check_Gc /* cause compiler doesn't generate appropriate Gc_test! */
	    Get_Local(pw1)
	    i = (uword) PP++->nint;
	    S = TG++;
	    pw1->val.ptr = S;
	    pw1->tag.kernel = TREF;
	    S->val.ptr = S;
	    S->tag.kernel = PP++->kernel;
	    while (i != 0)
	    {
		--pw1;
		if (i & 1)
		{
		    S = TG++;
		    pw1->val.ptr = S;
		    pw1->tag.kernel = TREF;
		    S->val.ptr = S;
		    S->tag.kernel = PP++->kernel;
		}
		i = i >> 1;	/* important: i must be unsigned !	*/
	    }
	    Next_Pp;

	Case(JmpA, I_JmpA)
	    if (!Deterministic) {
		Repush_Ret_Code
		Check_Local_Overflow
		Set_Det
	    }
	    PP = PP->code;
	    Handle_Events_Call
	    Next_Pp;

	Case(JmpP, I_JmpP)
	    if (!Deterministic) {
		Repush_Ret_Code
		Check_Local_Overflow
		Set_Det
	    }
	    PP = (emu_code) PriCode(PP->proc_entry);
	    Handle_Events_Call
	    Next_Pp;

	Case(Retd_nowake, I_Retd_nowake)
	    Pop_Ret_Code
	    Next_Pp;

	Case(Retd, I_Retd)
	    Pop_Ret_Code
	    Handle_Events_Return
	    Next_Pp;

	Case(Ret, I_Ret)
	    if (Deterministic) {
		Pop_Ret_Code
		Handle_Events_Return
		Next_Pp;
	    }
	    /* else fall through */
	Case(Retn, I_Retn)
	    Set_Det
	    Read_Ret_Code;
	    Handle_Events_Return
	    Next_Pp;

	Case(Ret_nowake, I_Ret_nowake)
	    if (Deterministic) {
		Pop_Ret_Code
		Next_Pp;
	    }
	    Set_Det
	    Read_Ret_Code;
	    Next_Pp;

	Case(ChaincA, I_ChaincA)
	    pw1 = (E - 1)->val.ptr;
	    Cut_To(pw1)
	    Set_Det
	    /* fall through */
	Case(ChaindA, I_ChaindA)
	    Pop_Env
	    PP = PP->code;
	    Handle_Events_Call
	    Next_Pp;

	Case(ChaincP, I_ChaincP)
	    pw1 = (E - 1)->val.ptr;
	    Cut_To(pw1)
	    Set_Det
	    /* fall through */
	Case(ChaindP, I_ChaindP)
	    Pop_Env
	    PP = (emu_code) PriCode(PP->proc_entry);
	    Handle_Events_Call
	    Next_Pp;

	Case(Exits, I_Exits)
/* CAUTION: if Space is to be used to grab space, add an overflow check */
	    SP = ByteOffsetMinus(SP, PP++->offset);
  	    /* falls through */
	Case(Exit, I_Exit)
	    Set_Det
	    if(E < EB) {
		Pop_Env
		if(EB == SP) {
		    Read_Ret_Code
		} else {
		    Pop_Ret_Code
		}
	    } else {
		SP = EB;
		PP = (emu_code) ERetCode;
		E = ERetEnv;
	    }
	    Handle_Events_Return
	    Next_Pp;

	Case(Exitc, I_Exitc)
	    pw1 = (E - 1)->val.ptr;
	    Cut_To(pw1)
	    Set_Det
	    /* fall through */
	Case(Exitd, I_Exitd)
	    Pop_Env
	    Pop_Ret_Code
	    Handle_Events_Return
	    Next_Pp;

	Case(Exitd_nowake, I_Exitd_nowake)
	    Pop_Env
	    Pop_Ret_Code
	    Next_Pp;

	Case(Savecut, I_Savecut)
	    pw1 = E - 1;
	    pw1->val.ptr = Deterministic ? B.args : BPrev(B.args);
	    pw1->tag.kernel = TCUT; 
	    Next_Pp;

	Case(SavecutL, I_SavecutL)
	    Get_Local(pw1)
	    pw1->val.ptr = B.args;
	    pw1->tag.kernel = TCUT; 
	    Next_Pp;

	Case(SavecutAM, I_SavecutAM)
	    Get_Argument(pw1)
	    pw1->val.ptr = B.args;
	    pw1->tag.kernel = TCUT; 
	    Next_Pp;

	Case(Neckcut_par, I_Neckcut_par)
	    if (Deterministic) Next_Pp;
	    Set_Det
	    pw1 = (B.top - 1)->frame.args;
	    Cut_To(pw1)
	    Next_Pp;

	Case(Neckcut, I_Neckcut)
	    if (Deterministic) Next_Pp;
	    Set_Det
_neckcut_:
	    Cut_Last(pw1)
	    Next_Pp;

	Case(Cut_single, I_Cut_single)
	    if ((B.top - 1)->frame.args != (E - 1)->val.ptr) {
		PP += 1;
		Next_Pp;
	    }
	    /* else fall through to Cut */

	Case(Cut, I_Cut)		/* EnvTrimSize (env. definitely exposed) */
	    pw1 = (E - 1)->val.ptr;
	    Cut_To(pw1)
	    Set_Det
	    SP = ByteOffsetMinus(E, PP++->offset);
	    Next_Pp;

	Case(CutAMN, I_CutAMN)		/* Ai EnvTrimSize */
	    Get_Argument(pw1)
	    goto _cut_and_trim_if_environment_exposed_;

	Case(CutL, I_CutL)		/* Yi EnvTrimSize */
	    Get_Local(pw1)
_cut_and_trim_if_environment_exposed_:
	    pw1 = pw1->val.ptr;
	    Cut_To(pw1)
	    Set_Det	/* needed if instruction gets used in first chunk */
	    pw1 = ByteOffsetMinus(E, PP++->offset);
	    if (pw1 > EB)
		SP = EB;
	    else
		SP = pw1;
	    Next_Pp;

	Case(CutAM, I_CutAM)		/* Ai */
	    Get_Argument(pw1);
	    Dereference_Pw(pw1)
	    pw1 = pw1->val.ptr;
	    Cut_To(pw1)
	    Next_Pp;

	Case(SoftcutL, I_SoftcutL)
	    Get_Local(pw1)
	    pw1 = pw1->val.ptr;
	    if (B.args == pw1)
		goto _neckcut_;
	    (Top(pw1) - 1)->backtrack = soft_cut_code_;
	    Next_Pp;

	Case(GuardL, I_GuardL)		/* Yi, DelayLabel */
        {
            pword **aux_tt = TT;
	    Get_Local(pw1)
	    pw1 = pw1->val.ptr;
            EB = Chp(pw1)->sp;
            GB = Chp(pw1)->tg;
            while (aux_tt < Chp(pw1)->tt)       /* something was trailed */
            {
                S = TrailedLocation(aux_tt);
                if (S < GB || S >= EB)          /* significant trail ? */
		{
		    PP = PP->code;
		    Next_Pp;
		}
                End_Of_Frame(aux_tt, aux_tt)
            }
	    PP++;
            Next_Pp;
        }

#ifdef DFID
	Case(Dfid_testL, I_Dfid_testL)
	    if ((i = DfidDepth->val.nint + 1) > MaxDepth) {
		if (i > DepthLimit) {
		    DepthOV = 1;
		    Fail;
		}
		else {
		    Trail_Word(&MaxDepth, 0, TRAILED_WORD32);
		    MaxDepth = i;
		}
	    }
	    Get_Local(pw1)
	    pw1->tag.kernel = TINT;
	    pw1->val.nint = i;
	    if (DfidDepth < GB) {
		Trail_Pointer(&DfidDepth);
		S = TG++;
		S->tag.kernel = TINT;
		S->val.nint = i;
		DfidDepth = S;
	    }
	    else
		DfidDepth->val.nint = i;
	    Next_Pp;

	Case(Dfid_test, I_Dfid_test)
	    if ((i = DfidDepth->val.nint + 1) > MaxDepth) {
		if (i > DepthLimit) {
		    DepthOV = 1;
		    Fail;
		}
		else {
		    Trail_Word(&MaxDepth, 0, TRAILED_WORD32);
		    MaxDepth = i;
		}
	    }
	    if (DfidDepth < GB) {
		Trail_Pointer(&DfidDepth);
		S = TG++;
		S->tag.kernel = TINT;
		S->val.nint = i;
		DfidDepth = S;
	    }
	    else
		DfidDepth->val.nint = i;
	    Next_Pp;

	Case(Depth, I_Depth)
	    Get_Local(pw1)
	    if (DfidDepth < GB) {
		Trail_Pointer(&DfidDepth);
		S = TG++;
		S->tag.kernel = TINT;
		S->val.nint = pw1->val.nint;
		DfidDepth = S;
	    }
	    else
		DfidDepth->val.nint = pw1->val.nint;
	    Next_Pp;
#endif


/***** In_get_.... ******/

	Case(In_get_constantAM, I_In_get_constantAM)
	    Get_Argument(pw1);
	    Dereference_Pw_Tag(pw1,tmp1);
	    goto _compare_const_;	/* (tmp1,pw1,pp) */

	Case(In_get_nilAM, I_In_get_nilAM)
	    Get_Argument(pw1);
	    Dereference_Pw_Tag(pw1,tmp1);
	    if (!IsTag(tmp1,TNIL))
		{ Fail; }
	    Next_Pp;

	Case(In_get_integerAM, I_In_get_integerAM)
	    Get_Argument(pw1);
	    Dereference_Pw_Tag(pw1,tmp1);
	    if (!IsTag(tmp1,TINT) || (pw1->val.nint != PP++->nint))
		{ Fail; }
	    Next_Pp;

#ifdef TFLOAT
	Case(In_get_floatAM, I_In_get_floatAM)
	    Get_Argument(pw1);
	    Dereference_Pw_Tag(pw1,tmp1);
	    if (!IsTag(tmp1,TFLOAT) || (pw1->val.real != PP++->real))
		{ Fail; }
	    Next_Pp;
#endif

	Case(In_get_atomAM, I_In_get_atomAM)
	    Get_Argument(pw1);
	    Dereference_Pw_Tag(pw1,tmp1);
	    if (!IsTag(tmp1,TDICT) || (pw1->val.did != PP++->did))
		{ Fail; }
	    Next_Pp;

	Case(In_get_stringAM, I_In_get_stringAM)
	    Get_Argument(pw1);
	    Dereference_Pw_Tag(pw1,tmp1);
	    if (!IsTag(tmp1,TSTRG))
	    {
		    Fail;
	    }
	    else
	    {
		    pw1 = pw1->val.ptr;
		    pw2 = PP++->ptr;
		    Compare_Strings(pw1, pw2, err_code);
		    if (err_code >= 0)
		    {
			    Fail;
		    }
	    }
	    Next_Pp;

	Case(In_get_metaAM, I_In_get_metaAM)
	    Get_Argument(pw1);
	    Dereference_Pw_Tag(pw1,tmp1);
	    if (!IsTag(tmp1, TMETA)) {
		Fail;
	    } else {
		S = pw1->val.ptr;
		PP++;
	    }
	    Next_Pp;

	Case(In_get_listAM, I_In_get_listAM)
	    Get_Argument(pw1);
	    Dereference_Pw_Tag(pw1,tmp1);
	    if (!IsTag(tmp1,TLIST))
	    {
		    Fail;
	    }
	    else
	    {
		    S = pw1->val.ptr;
		    PP = PP->code;
	    }
	    Next_Pp;

	Case(In_get_structureAM, I_In_get_structureAM)
	    Get_Argument(pw1);
	    Dereference_Pw_Tag(pw1,tmp1);
	    if (!IsTag(tmp1,TCOMP))
	    {
		    Fail;
	    }
	    else
	    {
		    pw1 = pw1->val.ptr;
		    if (pw1->val.did == (PP++)->did)
		    {
			    S = pw1 + 1;
			    PP = PP->code;
		    }
		    else
		    {
			    Fail;
		    }
	    }
	    Next_Pp;

	Case(Get_matched_valueAMAM, I_Get_matched_valueAMAM)
	    Get_Argument(pw1);
	    Get_Argument(pw2);
	    goto _match_values_;

	Case(Get_matched_valueAMTM, I_Get_matched_valueAMTM)
	    Get_Argument(pw1);
	    Get_Temporary(pw2);
	    goto _match_values_;

	Case(Read_matched_valueAM, I_Read_matched_valueAM)
	    Get_Argument(pw1);
	    pw2 = S++;
	    goto _match_values_;

	Case(Read_matched_valueTM, I_Read_matched_valueTM)
	    Get_Temporary(pw1);
	    pw2 = S++;
	    goto _match_values_;

	Case(Read_matched_valueL, I_Read_matched_valueL)
	    Get_Local(pw1);
	    pw2 = S++;
	    goto _match_values_;

	Case(Get_matched_valueAML, I_Get_matched_valueAML)
	    Get_Argument(pw1);
	    Get_Local(pw2);
_match_values_:
	    Dereference_Pw(pw1);
	    Dereference_Pw(pw2);
	    proc = identical_proc_;
	    goto _diff_;		/* (proc, pw1, pw2) */

	/*
	 * the next instruction can be prefixed to ordinary
	 * Read_... instructions to simulate the corresponding
	 * Read_matched_... instructions
	 */
	Case(Read_test_var, I_Read_test_var)
	    pw1 = S;	/* do not increment S !	*/
	    Dereference_Pw_Tag(pw1,tmp1)
	    if (ISRef(tmp1))
	    {
		Fail;
	    }
	    Next_Pp;


/***********************************************
 * Coroutining instructions
 ***********************************************/

	    /* Explicit resume instruction. When events are pending,
	     * it has the same effects as a call, so there must be an
	     * environment, temporaries popped, etc. Also, the woken
	     * goal (or the GC) may leave choicepoints.
	     */
	Case(Ress, I_Ress)			/* space arity envsize */
	    SP = ByteOffsetMinus(SP, PP++->offset);
	Case(Res, I_Res)			/* arity envsize */
	    if (EventPending) {
		tmp1 = PP[0].nint;
		Push_Ret_Code(PP+2)		/* make it look like a call */
		PP = (emu_code) return_code_;
		goto _handle_events_at_res_;	/* (tmp1) */
	    }
	    PP += 2;
	    Next_Pp;

	Case(Wake_init, I_Wake_init)		/* no args */
	    Push_Env
	    (--SP)->tag.all = TINT;
	    SP->val.nint = WP;
	    Check_Local_Overflow
	    PP += 1;				/* skip envsize */
	    Next_Pp;

	Case(Wake, I_Wake)			/* no args, Y1 = savedWP */
	    tmp1 = (E-1)->val.nint;		/* saved WP */
#ifdef PRINTAM
	    if (!WL || tmp1 > WLMaxPrio(WL) || DE)
	    {
		(void) ec_panic("Assertion Failed", "Emulator");
	    }
#endif
	    /*
	     * first_woken(tmp1 -> pw2)
	     * find the first woken suspension with priority higher than tmp1
	     * and remove it from its list. Note that these lists have been
	     * created by schedule_suspensions, so we know we don't have
	     * references in certain places (but beware of timestamps!)
	     */
	    pw2 = 0;
	    S = WLFirst(WL) - 1;
	    for (i=1; i<tmp1; i++)
	    {
		pw1 = ++S;			/* no references allowed */
		if (IsList(pw1->tag))
		{
		    for (;;) {
			pw1 = pw1->val.ptr;	/* list element */
			pw2 = (pw1++)->val.ptr;	/* TSUSP pword */
			Dereference_(pw1);	/* list tail */
			if (!SuspDead(pw2))
			    break;		/* found one! */
			if (IsNil(pw1->tag)) {
			    pw2 = 0;		/* end of this list */
			    break;
			}
		    }
		    /*
		     * Replace the list head: remove dead suspensions
		     * plus possibly the one we are about to wake
		     */
		    if (S->val.ptr < GB) {
			Trail_Pword(S);
		    }
		    if (IsList(pw1->tag)) {
			S->val.ptr = pw1->val.ptr;
		    } else {
			/* Use a timestamp (which happens to look like a [])
			 * to terminate the list */
			Make_Stamp(S);
		    }
		    if (pw2)
		    	break;
		}
	    }
	    if (!pw2)
	    {
		/* no woken goal found, continue */
		Set_WP(tmp1);
		Next_Pp;
	    }
	    /* We did find a suspension to wake: set priority and call it! */
	    Set_WP(i);
	    PP -= 1;				/* wake loop */
	    if(E >= EB) {
		Push_Ret_Code_To_Eb(ERetCode)
		E = ERetEnv;
		Push_Env
		(--SP)->tag.all = TINT;
		SP->val.nint = tmp1;
		Check_Local_Overflow
	    }
	    goto _susp_wake_;			/* (pw2) */


 	Case(Continue_after_event, I_Continue_after_event)
	    PP = (emu_code) DynEnvVal(E);		/* get continuation */
	    if (DynEnvFlags(E) & WAS_NONDET) {Clr_Det;} else {Set_Det;}

	    if (DynEnvFlags(E) & WAS_CALL) {		/* debug event frame */
		if (DynEnvDE(E)->tag.kernel == TSUSP) DE = DynEnvDE(E)->val.ptr;
		err_code = DynEnvDbgPort(E)->val.nint;		/* port */
		pw1 = E-DYNENVDBGSIZE-1;
		tmp1 = DynEnvSize(E)-DYNENVDBGSIZE-1;
	    } else {
		pw1 = E-1;
		tmp1 = DynEnvSize(E) - 1;
		err_code = 0;
	    }

	    pw2 = &A[1];			/*  restore args */
	    for (; tmp1 > 0; tmp1--)
		*pw2++ = *--pw1;

	    if (E < EB)				/* pop aux environment */
	    {
		Pop_Env
	    }
	    else
	    {
		SP = EB;
		Push_Ret_Code(ERetCode)
		E = ERetEnv;
	    }

	    /* insert hook to trace the exit port */
	    if (err_code & LAST_CALL)
	    {
		Push_Env
		Push_Ret_Code((emu_code) &trace_exit_code_[1]);
	    }
	    Next_Pp;

 	Case(Continue_after_event_debug, I_Continue_after_event_debug)
	    if (DynEnvFlags(E) & WAS_NONDET) {Clr_Det;} else {Set_Det;}
	    if ((emu_code) DynEnvVal(E) == (emu_code) return_code_)
	    {
		(void) ec_panic("Debug Assertion Failed", "Emulator");
		/* can't handle the port, it's inlined */
		DynEnvDbgPort(E)->val.nint &= ~LAST_CALL;	/* port */
		PP = (emu_code) &restore_code_[1];
		Next_Pp;
	    }
	    proc = (pri *) DynEnvDbgPri(E)->val.wptr;		/* pri */
	    err_code = DynEnvDbgPort(E)->val.nint;		/* port */
#ifndef USE_LAST_FLAG
	    DynEnvDbgPort(E)->val.nint |= LAST_CALL;
#endif
	    /*
	    print_port(current_err_, err_code);
	    newline(current_err_);
	    */
	    DBG_INVOC = DynEnvDbgInvoc(E)->val.nint;		/* invoc */
	    if (!DBG_INVOC)
	    	DBG_INVOC = NINVOC++;
	    val_did = PriDid(proc);
	    tmp1 = DidArity(val_did);

	    if (tmp1 == 0) {				/* build goal */
		scratch_pw.val.did = val_did;
		scratch_pw.tag.kernel = (val_did == d_.nil) ? TNIL : TDICT;
	    } else {
		scratch_pw.val.ptr = TG;
		if (val_did == d_.list) {
		    scratch_pw.tag.kernel = TLIST;
		} else {
		    scratch_pw.tag.kernel = TCOMP;
		    TG->val.did = val_did;
		    (TG++)->tag.kernel = TDICT;
		}
		pw1 = E - DYNENVDBGSIZE - 1;
		for(; tmp1 > 0; tmp1--)
		{
		    pw2 = --pw1;
		    Move_Pw_To_Global_Stack(pw2, TG, ;);
		}
	    }

	    A[1] = TAGGED_TD;			/* Old call stack */
	    if (TD < GB) { Trail_Pword(&TAGGED_TD); }
#ifdef USE_FIRST_FLAG
	    if (!(err_code & FIRST_CALL))
	    {
		tmp1 = DLevel(TD);		/* depth */
		TAGGED_TD = TD[TF_ANCESTOR];	/* pop exited frame */
	    }
	    else
#endif
	    {
		tmp1 = TD ? DLevel(TD)+1 : 0;	/* depth */
	    }
	    val_did = PriModule(proc);
	    if (val_did == D_UNKNOWN) val_did = proc->module_ref;
	    Push_Dbg_Frame(pw1, DBG_INVOC, scratch_pw.val, scratch_pw.tag,
	    	tmp1, WP, proc, DynEnvDbgPath(E)->val.did, 
		DynEnvDbgLine(E)->val.nint,
		DynEnvDbgFrom(E)->val.nint,
		DynEnvDbgTo(E)->val.nint, val_did)
#if (TF_BREAK != BREAKPOINT)
Please make sure that TF_BREAK == BREAKPOINT
#endif
	    tmp1 = err_code&BREAKPOINT;	/* == TF_BREAK */
	    Set_Tf_Flag(TD, tmp1)
	    if (OfInterest(PriFlags(proc), DBG_INVOC, tmp1, tmp1))
	    {
		A[2] = TAGGED_TD;			/* New call stack */

		/* if stop point:
		 * call debug event(OldStack,NewStack)
		 */
		proc = error_handler_[(err_code&PORT_MASK) == WAKE_PORT ? -(DEBUG_WAKE_EVENT) : -(DEBUG_CALL_EVENT)];
		PP = (emu_code) PriCode(proc);
		Push_Ret_Code((emu_code) &restore_code_[1]);
		Check_Local_Overflow
	    }
	    else
	    {
		PP = (emu_code) &restore_code_[1];
	    }
	    Next_Pp;


	    /*
	     * Refail is really a cut, but can be somewhat simpler because
	     * it is always followed by a fail.  Resetting of EB/GB proved
	     * necessary because debugger and garbage collector rely on GB
	     * to cache the current topmost choicepoint's TG field.
	     */
	Case(Refail, I_Refail)
_do_refail_:
	    B.any_frame = (B.top-1)->frame;
            EB = BChp(B.args)->sp;
            GB = BChp(B.args)->tg;
#ifdef PB_MAINTAINED
	    while (PB > B.args)
		PB = BPar(PB)->ppb;
#endif
	Case(Failure, I_Failure)
_do_fail_:
	    PP = (emu_code) (B.top - 1)->backtrack;
	    Next_Pp;


/***********************************************
 * Metacall instructions
 ***********************************************/

	Case(Explicit_jmp, I_Explicit_jmp)	/* (LookupM,Goal,CallerM,Cut) */
	    if (Deterministic) {
		Pop_Ret_Code
	    } else {
		Read_Ret_Code
		Set_Det
	    }
	    scratch_pw = A[1];
	    A[1] = A[2];
	    A[2] = A[3];
	    A[3] = scratch_pw;
	    DBG_PORT = CALL_PORT|LAST_CALL;
	    i = PRI_EXPORTEDONLY;
	    goto _anycall_;

	Case(Meta_jmp, I_Meta_jmp)	/* tail-recursive metacall */
	    if (Deterministic) {
		Pop_Ret_Code
	    } else {
		Read_Ret_Code
		Set_Det
	    }
	    DBG_PORT = CALL_PORT|LAST_CALL;
	    i = 0;
	    goto _anycall_;

	Case(Metacall, I_Metacall)	/* (Goal, CallerMod, LookupMod, Cut) */
	    PP++;			/* skip environment size	*/
	    DBG_PORT = CALL_PORT;
	    Set_Det
	    i = 0;
_anycall_:				/* (pw1,DBG_PORT,i) */
#ifdef USE_LAST_FLAG
	    DBG_PORT |= FIRST_CALL;
#else
	    DBG_PORT |= FIRST_CALL|LAST_CALL;
#endif
	    pw1 = &A[3];		/* lookup module	*/
	    tmp1 = pw1->tag.kernel;		/* check lookup module */
	    if (ISRef(tmp1)) {
		Dereference_Pw_Tag(pw1,tmp1)	/* rare case! */
		if (ISRef(tmp1)) {
		    err_code = INSTANTIATION_FAULT;
		    goto _metacall_err_;
		}
	    }
	    if (!IsTag(tmp1,TDICT)) {
		if (IsTag(tmp1,TNIL))
		    pw1->val.did = d_.nil;
		else {
		    err_code = TYPE_ERROR;
		    goto _metacall_err_;
		}
	    }
	    pw2 = pw1;				/* dereferenced lookup module */

	    pw1 = &A[1];			/* check goal	*/
	    Dereference_Pw_Tag(pw1,tmp1)
	    if (IsTag(tmp1,TCOMP)) {
		pw1 = pw1->val.ptr;
		val_did = pw1->val.did;
	    } else if (IsTag(tmp1,TDICT)) {
		val_did = pw1->val.did;
		if (DidArity(val_did) > 0) {
		    err_code = TYPE_ERROR;
		    goto _metacall_err_in_goal_;
		}
	    } else if (IsTag(tmp1,TLIST)) {
		pw1 = pw1->val.ptr - 1;
		val_did = d_.list;
	    } else if (IsTag(tmp1,TNIL)) {
		val_did = d_.nil;
	    } else {
		if (ISRef(tmp1))
		    err_code = INSTANTIATION_FAULT;
		else
		    err_code = TYPE_ERROR;
		goto _metacall_err_in_goal_;
	    }
	    if (!IsModule(pw2->val.did)) {
		err_code = NO_LOOKUP_MODULE;
		goto _metacall_err_;
	    } 
	    Export_B_Sp_Tg_Tt
	    proc = visible_procedure(val_did, pw2->val.did, pw2->tag, i);
	    Import_None
	    if( proc == (pri*) 0) {
		Get_Bip_Error(err_code);
		if (err_code == NOENTRY)
		    err_code = CALLING_UNDEFINED;
		goto _metacall_err_;
	    }
	    DBG_INVOC = 0L;

	    /* first check for special goals ,/2 ;/2 ->/2 !/0	*/
	if (proc->module_ref == d_.kernel_sepia)
	{
	    if(val_did == d_.comma) {
		/* call((Goal1 , Goal2), CM,    LM, Cut)
		 *  ','(Goal1,           Goal2, CM, Cut)	*/
		A[3] = A[2];
		A[1] = *(++pw1);
		A[2] = *(++pw1);
		Push_Ret_Code(PP)
		Check_Local_Overflow;
		PP = (emu_code) CodeStart(comma_body_code_);
		DBG_PORT = NO_PORT;	/* don't trace, treat as inlined */
		goto _exec_prolog_;
	    }
	    else if(val_did == d_.semicolon) {
		Push_Ret_Code(PP)
		Check_Local_Overflow;
		pw2 = ++pw1;
		Dereference_Pw(pw2)
		if (IsStructure(pw2->tag) && (
			( pw2->val.ptr->val.did == d_.cond
			&& (PP = (emu_code) CodeStart(cond3_body_code_)))
		    ||
			( pw2->val.ptr->val.did == d_.softcut
			&& (PP = (emu_code) CodeStart(softcut5_body_code_)))))
		{
		    /*
		     * Map      call((G1->G2;G3), CM, LM, Cut)
		     *  into     ';'(G1,          G2, CM, Cut, G3)
		     * or       call((G1*->G2;G3), CM, LM, Cut)
		     *  into softcut(G1,           G2, CM, Cut, G3)
		     */
		    A[3] = A[2];
		    A[5] = *(++pw1);
		    pw1 = pw2->val.ptr + 1;
		    A[1] = *pw1++;
		    A[2] = *pw1;
		} else {		/* simple disjunction */
		    /* call((G1;G2), CM, LM, Cut)
		     *  ';'(G1,      G2, CM, Cut)	*/
		    A[3] = A[2];
		    A[1] = *pw2;
		    A[2] = *(++pw1);
		    PP = (emu_code) CodeStart(semic_body_code_);
		}
		DBG_PORT = NO_PORT;	/* don't trace, treat as inlined */
		goto _exec_prolog_;
	    } else if(val_did == d_.cond) {	/* simple ->/2 */
		/* call((G1;G2), CM, LM, Cut)
		 * '->'(G1,      G2, CM, Cut)	*/
		A[3] = A[2];
		A[1] = *(++pw1);
		A[2] = *(++pw1);
		Push_Ret_Code(PP)
		Check_Local_Overflow;
		PP = (emu_code) CodeStart(cond_body_code_);
		DBG_PORT = NO_PORT;	/* don't trace, treat as inlined */
		goto _exec_prolog_;
	    } else if(val_did == d_.cut) {	/* !/0 ==> cut_to(Cut) */
		pw2 = &A[4];
		A[1] = *pw2;
		Push_Ret_Code(PP)
		Check_Local_Overflow;
		PP = (emu_code) CodeStart(cut_to_code_);
		goto _exec_prolog_;
	    }

	}
	    pw3 = &A[2];			/* caller module */
	    tmp1 = DidArity(val_did);		/* general metacall	*/

	    /* PriArgPassing(proc) is ARGFIXEDWAM or ARGFLEXWAM */
	    {
_call_structure_reg_:	/* (DBG_PORT, DBG_INVOC, proc, tmp1, pw1, pw3(module)) */
		Mark_Prof(_call_structure_reg_)
		pw2 = &A[0];			/* copy arguments */
		if (PriFlags(proc) & TOOL)
		    pw2[tmp1+1] = *pw3;		 /* must be done first */
		switch((unsigned) tmp1) {
		default:
		    do
			pw2[tmp1] = pw1[tmp1];
		    while (--tmp1 > 6);
		case 6: pw2[6] = pw1[6];
		case 5: pw2[5] = pw1[5];
		case 4: pw2[4] = pw1[4];
		case 3: pw2[3] = pw1[3];
		case 2: pw2[2] = pw1[2];
		case 1: pw2[1] = pw1[1];
		case 0: ;
		}
_call_prolog_:		/* (DBG_INVOC, DBG_PORT, proc) */
		Push_Ret_Code(PP)
		Check_Local_Overflow;
		PP = (emu_code) PriCode(proc);
_exec_prolog_:		/* (DBG_INVOC, DBG_PORT, proc, PP) */

		if ((TD || (PriFlags(proc) & DEBUG_ST)) && DBG_PORT) {
		    if (TD) {
			if (((DBG_PORT&PORT_MASK) == WAKE_PORT ? TracingWakes(DBG_INVOC) : TracingMetacalls(DBG_PORT))
				&& AnyPortWanted && !InvisibleProc(proc)) {
			    goto _metacall_port_;	/* (proc,DBG_XXX) */
			}
		    } else /* if (PriFlags(proc) & DEBUG_ST) */ {
			if (TRACEMODE & TR_STARTED) {
			    /* we abuse the DEBUG_SP bit to init creep/leap mode */
			    TRACEMODE |= (PriFlags(proc) & DEBUG_SP) ?
					    TR_TRACING : TR_LEAPING;
			}
			if (AnyPortWanted) {
			    goto _metacall_port_;	/* (procDBG_XXX) */
			}
		    }
		}
	    	if (PriArgPassing(proc) != ARGFLEXWAM) {
		    Handle_Events_Call
		}
		Next_Pp;
	    }


_metacall_port_:	/* (proc) */
	    tmp1 = CodeArity(PP);		/* number of valid arguments */
	    Push_Env				/* allocate an environment */
	    PushDynEnvHdr(tmp1+DYNENVDBGSIZE, WAS_CALL, PP);	/* save arity, PP */
	    SP -= DYNENVDBGSIZE;
	    DynEnvDE(e)->tag.kernel = DE?TSUSP:TNIL;
	    DynEnvDE(e)->val.ptr = DE;
	    DynEnvDbgPri(E)->tag.kernel = TPTR;		/* ... and debug info */
	    DynEnvDbgPri(E)->val.wptr = (uword *) proc;
	    Make_Integer(DynEnvDbgPort(E), DBG_PORT);
	    Make_Integer(DynEnvDbgInvoc(E), DBG_INVOC);
	    /* If we have source info in the DBG_ fields from a preceding
	     * Debug_esc instruction, use it */
	    if (DBG_LINE) {
		Make_Atom(DynEnvDbgPath(E), DBG_PATH);
		Make_Integer(DynEnvDbgLine(E), DBG_LINE);
		Make_Integer(DynEnvDbgFrom(E), DBG_FROM);
		Make_Integer(DynEnvDbgTo(E), DBG_TO);
		DBG_LINE = 0;	/* DBG_{PATH,LINE,FROM,TO} now invalid */
	    } else {
		Make_Atom(DynEnvDbgPath(E), d_.empty);
		Make_Integer(DynEnvDbgLine(E), 0);
		Make_Integer(DynEnvDbgFrom(E), 0);
		Make_Integer(DynEnvDbgTo(E), 0);
	    }
	    PP = (emu_code) &restore_debug_code_[1];
	    pw1 = &A[1];	/* save the argument registers */
	    for (; tmp1; --tmp1)
		*(--SP) = *pw1++;
	    Check_Local_Overflow
	    if (PriArgPassing(proc) != ARGFLEXWAM) {
		goto _handle_events_at_return_;
	    }
	    Next_Pp;

_metacall_err_in_goal_:	/* (err_code, goal in A1, caller in A2, lookup in A3) */
	    pw1 = TG;		/* error(N, call(Goal), Caller, Lookup) */
	    TG += 2;
	    pw1[0].val.did = d_.call;
	    pw1[0].tag.kernel = TDICT;
	    pw1[1] = A[1];
	    A[1].val.ptr = pw1;
	    A[1].tag.kernel = TCOMP;

_metacall_err_:		/* (err_code, goal in A1, caller in A2, lookup in A3) */
	    A[4] = A[3];		/* error(N, Goal, Caller, Lookup) */
	    A[3] = A[2];
	    A[2] = A[1];
	    goto _regular_err_2_;	/* (err_code, A2, A3, A4)	*/



	Case(Suspension_jmp, I_Suspension_jmp)		/* suspension in A[1] */
	    Pop_Ret_Code
	    goto _susp_call_;

	Case(Suspension_call, I_Suspension_call)	/* suspension in A[1] */
	    PP += 1;			/* skip environment size */
_susp_call_:
	    pw2 = &A[1];
	    Dereference_Pw_Tag(pw2, tmp1)
	    if (!IsTag(tmp1, TSUSP)) {
		Fail;
	    }
	    pw2 = pw2->val.ptr;		/* point to suspension structure */
	    if (SuspDead(pw2)) {
		Next_Pp;		/* ok, already woken	*/
	    }
_susp_wake_:					/* suspension in pw2 */
	    pw3 = &pw2[SUSP_MODULE];
	    proc = (pri*) pw2[SUSP_PRI].val.wptr;
	    pw1 = &pw2[SUSP_GOAL];		/* find the arguments */
	    Dereference_Pw_Tag(pw1, tmp1)
	    if (IsTag(tmp1,TCOMP)) {
		pw1 = pw1->val.ptr;
		tmp1 = DidArity(pw1->val.did);
	    } else if (IsTag(tmp1,TDICT)) {
		tmp1 = 0;
	    } else if (IsTag(tmp1,TLIST)) {
		pw1 = pw1->val.ptr - 1;
		tmp1 = 2;
	    } else if (IsTag(tmp1,TNIL)) {
		tmp1 = 0;
	    }
	    Set_Det
	    DBG_PORT = WAKE_PORT;
	    DBG_INVOC = SuspDebugInvoc(pw2);
	    if (SuspDemon(pw2)) {
		Set_Susp_Delayed(pw2);
		if (PriFlags(proc) & EXTERN)	/* set DE for C externals */
		    DE = pw2;
	    } else {
		Set_Susp_Dead(pw2);
	    }
	    /* PriArgPassing(proc) is ARGFIXEDWAM or ARGFLEXWAM */
	    goto _call_structure_reg_; /* (DBG_PORT,DBG_INVOC,proc,tmp1,pw1,pw3) */


	Case(Handler_call, I_Handler_call)	/* A[1] signal number */
	    pw1 = &A[1];
	    Dereference_Pw(pw1)			/* checks omitted */
	    proc = interrupt_handler_[pw1->val.nint];
	    PP++;				/* skip environment size */
	    DBG_PORT = CALL_PORT;
	    goto _handler_call_;		/* (proc,DBG_PORT) */


	Case(Fastcall, I_Fastcall)	/* (port envsize) */
	    pw1 = &A[1];		/* A[1] error number or event name */
	    Dereference_Pw(pw1);
	    if (IsInteger(pw1->tag))
	    {
		err_code = pw1->val.nint;
		if (err_code < 0)
		{
		    proc = -err_code >= MAX_ERRORS ? 0 : default_error_handler_[-err_code];
		    A[1].val.nint = -err_code;
		    A[1].tag.kernel = TINT;
		}
		else
		    proc = err_code >= MAX_ERRORS ? 0 : error_handler_[err_code];
	    }
	    else if (IsAtom(pw1->tag)  &&  PSUCCEED ==
		get_simple_property(pw1->val.did, EVENT_PROP, &scratch_pw))
	    {
		if (scratch_pw.tag.kernel & EVENT_DEFERS)
		    VM_FLAGS |= EVENTS_DEFERRED;
		proc = (pri*) scratch_pw.val.ptr;
	    }
	    else
	    {
		A[2] = A[1];
		A[1].val.nint = -(EVENT_IGNORED);
		A[1].tag.kernel = TINT;
		proc = error_handler_[-(EVENT_IGNORED)];
	    }
	    if (!proc)
		proc = error_handler_[0];
	    if(proc->did == d_.fail && proc->module_ref == d_.kernel_sepia)
	    {
		Fail
	    }
	    DBG_PORT = PP++->nint;	/* NO_PORT or CALL_PORT		*/
	    PP++;			/* skip environment size	*/

_handler_call_:				/* (proc,DBG_PORT) */
	    DBG_INVOC = 0L;
	    val_did = PriDid(proc);
	    tmp1 = DidArity(val_did);
	    Set_Det
	    /* PriArgPassing(proc) is ARGFIXEDWAM or ARGFLEXWAM */
	    if(PriFlags(proc) & TOOL) {
		pw1 = &A[tmp1+1];
		pw1->val.did = PriModule(proc);
		pw1->tag.kernel = ModuleTag(PriModule(proc));
	    }
	    goto _call_prolog_;	/* (DBG_INVOC, DBG_PORT, proc) */


	Case(Meta_jmpA, I_Meta_jmpA)	/* used to call source of dynamic facts
					 * memory args like clause/3:
					 *		1 - Goal
					 *		2, 3 - Body, Ref
					 */
	    pw1 = &A[1];	/* get arg ptr & arity */
	    Dereference_Pw_Tag(pw1,tmp1)
	    if (IsTag(tmp1,TCOMP)) {
		pw1 = pw1->val.ptr;
		val_did = pw1->val.did;
	    } else if (IsTag(tmp1,TLIST)) {
		pw1 = pw1->val.ptr - 1;
		val_did = d_.list;
	    }
	    tmp1 = DidArity(val_did);		/* fetch args		*/
	    pw2 = &A[1];
	    for(; tmp1 > 0; tmp1--)
		*(pw2++) = *(++pw1);
	    PP = PP->code;
	    Next_Pp;



/* The first instruction of block/4:
 * It is similar to a Try, but it only saves arguments 2, 3, and 4.
 */

/* Doubles can't be thrown because they are not always simple */
#define Throwable(t) (IsSimple(t) ? !IsDouble(t) : IsTag(t.kernel,THANDLE))

	Case(Catch, I_Catch)
	    pw1 = &A[2];
	    Dereference_Pw(pw1);
	    if (!IsRef(pw1->tag) && !Throwable(pw1->tag))
	    {
		val_did = d_.block;
		err_code = TYPE_ERROR;
		Pop_Ret_Code
		goto _regular_err_;
	    }
	    Record_Alternative(1, 0);
	    pw1 = B.args;
	    Chp(pw1)->sp = EB = SP;
	    Chp(pw1)->tg = GB = TG;
	    Push_Witness
	    Chp(pw1)->tt = TT;
	    Chp(pw1)->e = E;
	    Chp(pw1)->ld = LD;
	    pw1 = (pword *) (Chp(pw1) + 1);
	    pw2 = &A[2];
	    *pw1++ = *pw2++;		/* Tag, Recovery, Module */
	    *pw1++ = *pw2++;
	    *pw1++ = *pw2;
	    Top(pw1)->backtrack = 
		!(PP++)->nint ?  catch_fail_code_ : catch_unint_fail_code_;
	    Top(pw1)->frame = B.any_frame;
	    B.top = Top(pw1) + 1;
	    Clr_Det
	    Check_Control_Overflow
	    A[2] = A[4];
	    Next_Pp;


	/*
	 * instructions for calling C builtins and externals
	 */

	Case(ExtCall, I_ExtCall)
	    proc = (PP++)->proc_entry;
	    /* save for the profiler and in case an error is raised */
	    Export_B_Sp_Tg_Tt_Eb_Gb
	    (void) (* PriFunc(proc))( A );
	    Import_Tg_Tt
	    Check_Gc
	    Pop_Ret_Code
	    Handle_Events_Return
	    Next_Pp;


	/*
	 * C externals with ARGFIXEDWAM calling convention
	 */

	Case(External0, I_External0)	/* (proc,address) arity 0 */
	    proc = PP++->proc_entry;
	    Export_B_Sp_Tg_Tt_Eb_Gb
	    err_code = (*(PP->func)) ();
	    goto _end_external_;

	Case(External1, I_External1)	/* (proc,address) arity 1 */
	    proc = PP++->proc_entry;
	    pw1 = &A[1]; Dereference_Pw(pw1);
	    Export_B_Sp_Tg_Tt_Eb_Gb
	    err_code = (*(PP->func)) (pw1->val, pw1->tag);
	    goto _end_external_;

	Case(External2, I_External2)	/* (proc,address) arity 2 */
	    proc = PP++->proc_entry;
	    pw1 = &A[1]; Dereference_Pw(pw1);
	    pw2 = &A[2]; Dereference_Pw(pw2);
	    Export_B_Sp_Tg_Tt_Eb_Gb
	    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag);
	    goto _end_external_;

	Case(External3, I_External3)	/* (proc,address) arity 3 */
	    proc = PP++->proc_entry;
	    pw1 = &A[1]; Dereference_Pw(pw1);
	    pw2 = &A[2]; Dereference_Pw(pw2);
	    S = &A[3]; Dereference_Pw(S);
	    Export_B_Sp_Tg_Tt_Eb_Gb
	    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag);
	    goto _end_external_;

	Case(External, I_External)	/* (proc, address) arity 4..16 */
	    proc = PP++->proc_entry;
	    for (tmp1 = DidArity(PriDid(proc)); tmp1 > 4; --tmp1)
	    {
		S = &A[tmp1]; Dereference_Pw(S); A[tmp1] = *S;
	    }
	    S = &A[4]; Dereference_Pw(S); A[4] = *S;
	    S = &A[3]; Dereference_Pw(S);
	    pw2 = &A[2]; Dereference_Pw(pw2);
	    pw1 = &A[1]; Dereference_Pw(pw1);
	    tmp1 = DidArity(PriDid(proc));
	    Export_B_Sp_Tg_Tt_Eb_Gb
	    switch(tmp1) {
		case 4:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag);
		    break;
		case 5:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag);
		    break;
		case 6:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag);
		    break;
		case 7:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag,
				A[7].val, A[7].tag);
		    break;
		case 8:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag,
				A[7].val, A[7].tag,
				A[8].val, A[8].tag);
		    break;
		case 9:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag,
				A[7].val, A[7].tag,
				A[8].val, A[8].tag,
				A[9].val, A[9].tag);
		    break;
		case 10:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag,
				A[7].val, A[7].tag,
				A[8].val, A[8].tag,
				A[9].val, A[9].tag,
				A[10].val, A[10].tag);
		    break;
		case 11:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag,
				A[7].val, A[7].tag,
				A[8].val, A[8].tag,
				A[9].val, A[9].tag,
				A[10].val, A[10].tag,
				A[11].val, A[11].tag);
		    break;
		case 12:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag,
				A[7].val, A[7].tag,
				A[8].val, A[8].tag,
				A[9].val, A[9].tag,
				A[10].val, A[10].tag,
				A[11].val, A[11].tag,
				A[12].val, A[12].tag);
		    break;
		case 13:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag,
				A[7].val, A[7].tag,
				A[8].val, A[8].tag,
				A[9].val, A[9].tag,
				A[10].val, A[10].tag,
				A[11].val, A[11].tag,
				A[12].val, A[12].tag,
				A[13].val, A[13].tag);
		    break;
		case 14:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag,
				A[7].val, A[7].tag,
				A[8].val, A[8].tag,
				A[9].val, A[9].tag,
				A[10].val, A[10].tag,
				A[11].val, A[11].tag,
				A[12].val, A[12].tag,
				A[13].val, A[13].tag,
				A[14].val, A[14].tag);
		    break;
		case 15:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag,
				A[7].val, A[7].tag,
				A[8].val, A[8].tag,
				A[9].val, A[9].tag,
				A[10].val, A[10].tag,
				A[11].val, A[11].tag,
				A[12].val, A[12].tag,
				A[13].val, A[13].tag,
				A[14].val, A[14].tag,
				A[15].val, A[15].tag);
		    break;
		case 16:
		    err_code = (*(PP->func)) (
				pw1->val, pw1->tag,
				pw2->val, pw2->tag,
				S->val, S->tag,
				A[4].val, A[4].tag,
				A[5].val, A[5].tag,
				A[6].val, A[6].tag,
				A[7].val, A[7].tag,
				A[8].val, A[8].tag,
				A[9].val, A[9].tag,
				A[10].val, A[10].tag,
				A[11].val, A[11].tag,
				A[12].val, A[12].tag,
				A[13].val, A[13].tag,
				A[14].val, A[14].tag,
				A[15].val, A[15].tag,
				A[16].val, A[16].tag);
		    break;
		default:
		    err_code = ARITY_LIMIT;
	    }
_end_external_:
	    Import_Tg_Tt
	    if (Deterministic)
	    {
		Pop_Ret_Code			/* Retd */
	    }
	    else if ((B.top - 1)->backtrack == external_fail_code_)
	    {
		Set_Det				/* Neckcut */
		Cut_Last(pw1)
		Pop_Ret_Code			/* Retd */
	    }
	    else
	    {
		Set_Det				/* Retn */
		Read_Ret_Code;
	    }
	    goto _bip_res1_;			/* (err_code,proc) */


#ifdef SPLIT_SWITCH

	default:
	    break;	/* continue into the second switch	*/

	} /* end first switch */

	switch ((PP-1)->inst)
	{

#endif /* SPLIT_SWITCH */


/*----------------------------------------------------------------------
 * Debug instructions
 *----------------------------------------------------------------------*/

/*
 * Raise a debug-event, i.e. trigger a debugger call
 * in the subsequent Call/Jmp/Chain instruction. Source
 * information may be supplied as quadruple (file,line,from,to)
 * The breakpoint manipulation mechanism relies on the exact
 * order of the [port, file, line, from, to] parameter group!
 */

	Case(Debug_call, I_Debug_call)	/* proc, port, file, line, from, to */
	    if (TD || (PriFlags(PP[0].proc_entry) & DEBUG_ST)) {
		if (TD) {
#ifdef UNTESTED_FIX
		    if (PriFlags(PP[0].proc_entry) & DEBUG_ST)
		    {
			/* we abuse the DEBUG_SP bit to reinit creep/leap mode */
			if (PriFlags(PP[0].proc_entry) & DEBUG_SP)
			    TRACEMODE &= ~TR_LEAPING;
		    }
#endif
		    if (Tracing && AnyPortWanted && !InvisibleProc(PP[0].proc_entry)) {
			DBG_PRI = PP[0].proc_entry;
			DBG_PORT = PP[1].nint;
			DBG_PATH = PP[2].did;
			DBG_LINE = PP[3].nint;
			DBG_FROM = PP[4].nint;
			DBG_TO = PP[5].nint;
			DBG_INVOC = 0L;
			Fake_Overflow;
		    }
		} else /* if (PriFlags(proc) & DEBUG_ST) */ {
		    if (TRACEMODE & TR_STARTED) {
			/* we abuse the DEBUG_SP bit to init creep/leap mode */
			TRACEMODE |= (PriFlags(PP[0].proc_entry) & DEBUG_SP) ?
					    TR_TRACING : TR_LEAPING;
		    }
		    if (AnyPortWanted) {
			DBG_PRI = PP[0].proc_entry;
			DBG_PORT = PP[1].nint;
			DBG_PATH = PP[2].did;
			DBG_LINE = PP[3].nint;
			DBG_FROM = PP[4].nint;
			DBG_TO = PP[5].nint;
			DBG_INVOC = 0L;
			Fake_Overflow;
		    }
		}
	    }
	    PP += 2 + SOURCE_POS_SZ;
	    Next_Pp;


	Case(Debug_exit, I_Debug_exit)
	    if(E < EB) {			/* like Chain */
		Pop_Env
		if(EB == SP) {Repush_Ret_Code}
	    } else {
		Push_Ret_Code_To_Eb(ERetCode)
	        Check_Local_Overflow
		E = ERetEnv;
	    }
	    A[1] = TAGGED_TD;			/* Old call stack */
	    Pop_Dbg_Frame();
	    pw1 = A[1].val.ptr;
	    if (ExitPortWanted && OfInterest(PriFlags(DProc(pw1)), DInvoc(pw1), DLevel(pw1), 0))
	    {
		/* call debug event(OldStack) */
		proc = error_handler_[-(DEBUG_EXIT_EVENT)];
		PP = (emu_code) PriCode(proc);
	    } else {
		PP = (emu_code) return_code_;
	    }
	    Set_Det
	    Next_Pp;


/*
 * Tracing of simple (i.e. implemented via instructions) builtins.
 * They have explicit EXIT_PORT instructions, and all shallow
 * if-then-elses have explicit FAIL_PORT instructions to catch
 * their failures. The problem is to establish whether an EXIT
 * or FAIL belongs to the current topmost trace frame because:
 * - the EXIT/FAIL port instruction may be inside an exception
 *   handler raised by the builtin: this is checked using the
 *   trace frame timestamp
 * - the CALL port may decide not to push a frame: this is
 *   checked by looking whether the frame has the TF_SIMPLE flag
 *   (we can't have nested TF_SIMPLEs without exception frame between)
 * The breakpoint manipulation mechanism relies on the exact
 * order of the [port, file, line, from, to] parameter group!
 */

#define Push_Bip_Debug_Goal(_pp,_did,_i,_mask) { \
	(_i) = DidArity(_did);\
	TG->val.did = (_did);\
	TG++->tag.kernel = TDICT;\
	do {\
	    switch((_mask) & 3) {\
	    case 0:\
		*TG = *(_pp[-(_i)].ptr);\
		break;\
	    case 1:\
		Make_Atom(TG,d_.ellipsis);\
		break;\
	    case 2:\
		TG->val.nint = _pp[-(_i)].nint; TG->tag.kernel=TINT;\
		break;\
	    case 3:\
		Make_Atom(TG, _pp[-(_i)].did);\
		break;\
	    }\
	    ++TG; (_mask) >>= 2;\
	} while (--(_i)>0);\
}

#define Update_Bip_Debug_Goal(_pp,_i,_mask,_pgoal) { \
	(_i) = DidArity(_pgoal[0].val.did);\
	while (_mask) {\
	    ++(_pgoal);\
	    switch((_mask) & 3) {\
	    case 1:\
		*(_pgoal) = *(_pp[-(_i)].ptr);\
		break;\
	    }\
	    --(_i); (_mask) >>= 2;\
	}\
}

	Case(Debug_call_simple, I_Debug_call_simple)	/* proc, port, file, line, from, to, argdesc, argref */
	    if (!Tracing || !AnyPortWanted
			|| (PP[1].nint & NO_ARGS)
			|| InvisibleProc(PP[0].proc_entry))
	    {
		PP += 8;
		Next_Pp;	/* debugger is off */
	    }
	    /*
	     * Construct the called goal: use the information provided by
	     * the (usually subsequent) bi_xxx A_i1...A_iArity instruction,
	     * referenced by the argdesc/argref parameters.
	     */
	    proc = PP[0].proc_entry;
	    val_did = PriDid(proc);
	    tmp1 = DidArity(val_did);
	    if (tmp1 > 0) {
		Make_Struct(&scratch_pw, TG);
		back_code = PP + 9 + PP[7].nint;	/* bi_xxx instruction arguments */
		if (PP[6].nint < 0) {
		    i = back_code[0].nint;	/* bi_xxx instruction's argdesc */
		} else {
		    i = PP[6].nint;		/* debug instruction's argdesc */
		}
		Push_Bip_Debug_Goal(back_code,val_did,tmp1,i);
	    } else {
		Make_Atom(&scratch_pw, val_did);
	    }
	    err_code = PP[1].val.nint;	/* port */
	    back_code = PP;
	    PP += 8;

	    /* Push a trace frame */
	    if (TD < GB) { Trail_Pword(&TAGGED_TD); }
#ifdef USE_FIRST_FLAG
	    /* we'd need to pass the old TD to the handler somehow */
	    ec_panic("USE_FIRST_FLAG unsupported", "emulator");
	    if (!(err_code & FIRST_CALL))
	    {
		tmp1 = DLevel(TD);		/* depth */
		TAGGED_TD = TD[TF_ANCESTOR];	/* pop exited frame */
	    }
	    else
#endif
	    {
		tmp1 = TD ? DLevel(TD)+1 : 0;	/* depth */
	    }

	    Push_Dbg_Frame(pw1, NINVOC, scratch_pw.val, scratch_pw.tag,
		tmp1, WP, proc,
		back_code[2].did, back_code[3].nint, back_code[4].nint, back_code[5].nint, PriModule(proc))
	    NINVOC++;

	    /* Raise an exception to trace the call port, if it is of interest  */
	    err_code &= BREAKPOINT;	/* == TF_BREAK */
	    Set_Tf_Flag(TD, TF_SIMPLE|err_code)
	    if (OfInterest(PriFlags(proc), NINVOC-1, tmp1, err_code))
	    {
		err_code = DEBUG_BIPCALL_EVENT;
		proc = true_proc_;	/* dummy culprit */
		goto _nbip_err_;	/* (err_code, proc) */
	    }
	    Next_Pp;


	Case(Debug_exit_simple_args, I_Debug_exit_simple_args)	/* unused, <ref to debug_call_simple> */
	    if (TD  &&  (TfFlags(TD) & TF_SIMPLE)  &&  !OldStamp(&TD[TF_CHP_STAMP]))
	    {
		if (!(TfFlags(TD) & TF_NOGOAL) 
		  && ExitPortWanted
		  && OfInterest(PriFlags(DProc(TD)), DInvoc(TD), DLevel(TD), 0))
		{
		    /* If the goal had any uninitialised arguments, fill them in now */
		    back_code = PP[1].code + 1;		/* debug_call_simple instruction */
		    if (back_code[6].nint < 0) {
			back_code = back_code + 9 + back_code[7].nint;	/* bi_xxx instruction arguments */
			i = back_code[0].nint;		/* bi_xxx instruction's argdesc */
		    } else {
			i = back_code[6].nint;		/* debug_call_simple instruction's argdesc */
			back_code = back_code + 9 + back_code[7].nint;	/* bi_xxx instruction arguments */
		    }
		    pw1 = DGoal(TD).val.ptr;
		    Update_Bip_Debug_Goal(back_code,tmp1,i,pw1);

		    /* handler will trace the exit and pop the frame */
		    err_code = DEBUG_BIPEXIT_EVENT;
		    proc = true_proc_;	/* dummy culprit */
		    PP += 2;
		    goto _nbip_err_;	/* (err_code, proc) */
		} else {
		    Pop_Dbg_Frame();
		}
	    }
	    PP += 2;
	    Next_Pp;


	Case(Debug_exit_simple, I_Debug_exit_simple)
	    if (TD  &&  (TfFlags(TD) & TF_SIMPLE)  &&  !OldStamp(&TD[TF_CHP_STAMP]))
	    {
		if (!(TfFlags(TD) & TF_NOGOAL) 
		  && ExitPortWanted
		  && OfInterest(PriFlags(DProc(TD)), DInvoc(TD), DLevel(TD), 0))
		{
		    /* handler will trace the exit and pop the frame */
		    err_code = DEBUG_BIPEXIT_EVENT;
		    proc = true_proc_;	/* dummy culprit */
		    goto _nbip_err_;	/* (err_code, proc) */
		} else {
		    Pop_Dbg_Frame();
		}
	    }
	    Next_Pp;

#if 0
	Case(Debug_fail_simple, I_Debug_fail_simple)
	    if (TD  &&  (TfFlags(TD) & TF_SIMPLE)  &&  !OldStamp(&TD[TF_CHP_STAMP]))
	    {
		FCULPRIT = DInvoc(TD);
		if (!(TfFlags(TD) & TF_NOGOAL)
		  && OfInterest(PriFlags(proc), DInvoc(TD), DLevel(TD), 0) )
		{
		    err_code = DEBUG_BIPFAIL_EVENT;
		    proc = true_proc_;	/* dummy culprit */
		    goto _nbip_err_;	/* (err_code,proc) */
		} else {
		    Pop_Dbg_Frame();
		}
	    }
	    Next_Pp;
#endif


/*----------------------------------------------------------------------*/

	Case(Undefined, I_Undefined)		/* (proc) */
	    proc = PP->proc_entry;
	    val_did = PriDid(proc);
	    /* save the (unchecked) caller module into scratch_pw */
	    if (proc->flags & TOOL)
		scratch_pw = A[DidArity(val_did) + 1];
	    else	/* use the descriptor's module */
	    {
		Make_Marked_Module(&scratch_pw, PriModule(proc));
		/* the module tag can be marked safely since a locked
		   module should never call an undefined procedure
		   (if it is a feature, it should be tested first
		   with is_predicate).				     */
	    }
	    /* build a goal structure and put it into A[2] */
	    tmp1 = DidArity(val_did);
	    if(tmp1 == 0) {
		Make_Atom(&A[2], val_did);
	    } else {
		S = TG;		/* build goal structure	*/
		TG += tmp1 + 1;
		S->val.did = val_did;
		(S++)->tag.kernel = TDICT;
		pw1 = &A[1];
		for(i = 0; i < tmp1; i++) {
		    pw2 = pw1++;
		    Move_Pw_To_Global_Stack(pw2,S, ;)
		}
		Make_Struct(&A[2], TG - tmp1 - 1);
		Check_Gc
	    }
	    /* move caller module to A[3] */
	    A[3] = scratch_pw;
	    err_code = (proc->flags & AUTOLOAD) ?
	    		CALLING_AUTOLOAD : CALLING_UNDEFINED;
	    /*
	     * Put lookup module in A[4]: as opposed to Make_Lookup_Module()
	     * the code here prefers to use the home module because that is
	     * the one we need for autoloading if it doesn't exist yet.
	     */
	    if (PriIsProxy(proc)  &&  PriModule(proc) != PriHomeModule(proc))
	    {
		Make_Atom(&A[4], PriHomeModule(proc));
		if (!IsModule(PriHomeModule(proc)))
		    err_code = NO_LOOKUP_MODULE;
	    }
	    else
	    {
		Make_Marked_Module(&A[4], PriModule(proc));
	    }
	    Pop_Ret_Code
	    goto _regular_err_2_; /* (err_code, A2 goal, A3 caller, A4 lookup) */



	Case(Call_dynamic, I_Call_dynamic)	/* (proc,handle) */
	    proc = PP[0].proc_entry;
	    val_did = PriDid(proc);
	    /* build a goal structure and put it into A[2] */
	    tmp1 = DidArity(val_did);
	    if(tmp1 == 0) {
		Make_Atom(&A[2], val_did);
	    } else {
		S = TG;		/* build goal structure	*/
		TG += tmp1 + 1;
		S->val.did = val_did;
		(S++)->tag.kernel = TDICT;
		pw1 = &A[1];
		for(i = 0; i < tmp1; i++) {
		    pw2 = pw1++;
		    Move_Pw_To_Global_Stack(pw2,S, ;)
		}
		Make_Struct(&A[2], TG - tmp1 - 1);
		Check_Gc
	    }
	    A[1].val.ptr = PP[1].ptr;
	    A[1].tag.kernel = THANDLE;
	    Make_Marked_Module(&A[3], PriModule(proc));
	    proc = error_handler_[-(CALLING_DYNAMIC)];
	    PP = (emu_code) PriCode(proc);
	    Next_Pp;



/*
 * The first instruction of exit_block/1:
 * check whether the argument is ok, then find a block frame which
 * has a suitable tag and is an ancestor of this goal,
 * reset the machine and unify the two tags
 */
	Case(Throw, I_Throw)
	    pw1 = &A[1];
	    Dereference_Pw(pw1);
	    if (!Throwable(pw1->tag))
	    {
		    val_did = d_.exit_block;
		    if (IsRef(pw1->tag))
		    {
		      err_code = INSTANTIATION_FAULT;
		    } else {
                      err_code = TYPE_ERROR;
                    }
		    Pop_Ret_Code
		    goto _regular_err_;
	    }
	    /* the exit tag (ball) may disappear, so we save it */
	    scratch_pw.tag.all = pw1->tag.all;
	    scratch_pw.val.all = pw1->val.all;
	    pw1 = B.args;
	    pw2 = E;
	    for (;;)			/* (pw1, pw2, scratch_pw) */
	    {
		if (IsCatchFrame(BTop(pw1)))
		{
		    /* find the first environment older than the catch frame */
		    while (RetCodeAddr(pw2) < BChp(pw1)->sp)
			pw2 = RetEnv(pw2);
		    /* was the block/3 called from this environment? */
		    if (RetCodeAddr(pw2) == BChp(pw1)->sp)
		    {
			pw2 = (pword *)(BChp(pw1) + 1);
			/* we first only check whether the tags would
			 * unify; it is done in the current state, hence
			 * we have to dereference the catch tag
			 */
			Dereference_Pw(pw2);
			/* we are cheating a bit here: SimpleEq() shouldn't be
			 * used for THANDLEs but it does the right thing */
			if ( IsRef(pw2->tag)
			  || ( SameType(pw2->tag, scratch_pw.tag)
			    && SimpleEq(scratch_pw.tag.kernel,
						scratch_pw.val, pw2->val)
			     )
			   )
			{
			    break;		/* they unify */
			}
			pw2 = BChp(pw1)->e;
		    }
		    /* not the right catch frame, skip it */
		    pw1 = BPrev(pw1);
		}
		else if (IsInterruptFrame(BTop(pw1))||IsRecursionFrame(BTop(pw1)))
		{
		/* exit an emulator: restore everything from the invoc frame.
		 * Normally we will continue throwing in an earlier emulator
		 * invocation, but that's not sure because the C code can
		 * choose not to propagate the throw. Therefore we must
		 * restore the engine to a reasonable state now rather
		 * than wait for the catch!
		 */
		    err_code = PTHROW;
		    B.args = pw1;
		    goto _exit_emulator_;	/* (err_code,scratch_pw) */
		}
		else	/* other frame, skip it */
		    pw1 = BPrev(pw1);
	    }

/* we finally found a matching ball !!
 * pw1: top of the catch frame
 * pw2: points to dereferenced Catcher
 * scratch_pw: copy of dereferenced Ball,
 */
	    // If the frame indicates that events are to be deferred
	    // then set the flag
	    if (IsCatchEventsDeferredFrame(BTop(pw1)))
	    {
		VM_FLAGS |= EVENTS_DEFERRED;
	    } 

	    if (TD)	/* find out how deep we fail */
	    {
		pword *td = TD;
		FDROP = 0;
		if (!OlderStampThanGlobalAddress(&TD[TF_CHP_STAMP],BChp(pw1)->tg))
		    FCULPRIT = DInvoc(TD);
		for (; td && !OlderStampThanGlobalAddress(&td[TF_CHP_STAMP],BChp(pw1)->tg); td = DAncestor(td), ++FDROP)
		{
		    /*p_fprintf(log_output_, "\n(%d) %d fail", DInvoc(td), DLevel(td));*/
		    if (FDROP < MAX_FAILTRACE)
		    {
			FTRACE[FDROP].invoc = DInvoc(td);
			FTRACE[FDROP].proc = DProc(td);
			FTRACE[FDROP].source_pos.file = DPath(td);
			FTRACE[FDROP].source_pos.line = DLine(td);
			FTRACE[FDROP].source_pos.from = DFrom(td);
			FTRACE[FDROP].source_pos.to = DTo(td);
		    }
		}
		RLEVEL = td ? DLevel(td) : -1;
		DBG_DELAY_INVOC = 0;		/* if set for DEBUG_DELAY_EVENT */
	    }
	    else { RLEVEL = -1; FDROP = 0; }

	    /*
	     * Before untrailing, cut everything above the catch frame.
	     * This will suppress unnecessary timestamped undo-untrails.
	     */
	    Cut_To(pw1);
	    pw1 = BPrev(B.args);
#ifdef NEW_ORACLE
	    /* this is preliminary, catch-throw not yet properly oracled */
	    if (TO)
		TO = Chp(pw1)->tg - ORC_SIZE;
#endif
	    b_aux.args = pw1;			/* save pw1 temporarily */
	    Untrail_Variables(b_aux.chp->tt, i, pw1);
	    pw1 = b_aux.args;
	    SP = Chp(pw1)->sp;
	    Wipe(Chp(pw1)->tg,TG);
	    TG = Chp(pw1)->tg;
	    E  = Chp(pw1)->e;
            LD = Chp(pw1)->ld;
	    MU = 0;
	    Adjust_GcTg_and_TgSl(TG);
	    pw1 = (pword *)(Chp(pw1) + 1) + 1;		/* skip the ball */
	    A[1] = *pw1++;				/* A1 = recovery goal */
	    A[2] = *pw1++;				/* A2 = module */
	    B.args = pw1 = Top(pw1)->frame.args;	/* pop catch frame */
	    pw1 = (Top(pw1) - 1)->frame.args;
	    EB = Chp(pw1)->sp;
	    GB = Chp(pw1)->tg;
	    Debug_Check_Global

	    if ( FDROP > 0  &&  PortWanted(LEAVE_PORT)
	     || TD  &&  RLEVEL != DLevel(TD)  &&  PortWanted(PREDO_PORT)
	     || Tracing  &&  PortWanted(NEXT_PORT))
	    {
		tmp1 = 2;	/* arity of call(Recov, Module) */
		{
		    Push_Env
		    PushDynEnvHdr(tmp1, 0, PP);		/* save arity, PP */
		    PP = (emu_code) &restore_code_[1];
		    pw1 = &A[1];	/* save the argument registers */
		    for (; tmp1; --tmp1)
			*(--SP) = *pw1++;
		}
		Push_Ret_Code(PP)
		Check_Local_Overflow
		Set_Det

		proc = error_handler_[-(DEBUG_REDO_EVENT)];
		PP = (emu_code) PriCode(proc);
		A[1] = TAGGED_TD;
		Make_Integer(&A[2], FDROP);
		Make_Integer(&A[3], RLEVEL);
		Make_Integer(&A[4], LEAVE_PORT);
		Make_Integer(&A[5], NEXT_PORT);	/* show NEXT port? */
	    }

	    /* now we unify the tags - if the argument of exit_block/1
	     * was a handle that was popped, replace it with an atom.
	     */
	    if (IsTag(scratch_pw.tag.kernel,THANDLE) && scratch_pw.val.ptr >= TG)
	    {
		Make_Atom(&scratch_pw, d_.handle_expired_while_thrown);
	    }
	    pw1 = &scratch_pw;
	    goto _unify_;				/* (pw1, pw2) */


/*
 * Continue_after_exception is executed after a bip error handler
 * succeeded or failed. For failure, it would normally be enough to
 * do a Refail, but in case we fail to a small if-then-else choicepoint
 * we could not restore all the information.
 */
	Case(Continue_after_exception, I_Continue_after_exception)
	    /* pop frames until exception frame found	*/
	    pw1 = (pword *) (B.top - 1);
	    while (Top(pw1)->backtrack != exception_fail_code_ )
		pw1 = (pword *) (Top(pw1)->frame.top - 1);
	    B.top = Top(pw1);		/* similar to Cut_To(pw1) */
	    pw1 = Top(pw1)->frame.args;
	    EB = Exception(pw1)->eb;
	    GB = Exception(pw1)->gb;
	    while (LCA >= GB) {
		Export_B_Sp_Tg_Tt;
		do_cut_action();
		Import_Tg_Tt;
	    }
	    Cut_To_Parallel(pw1);
	    SP = Exception(pw1)->sp;	/* pop the local stack	*/
	    E = Exception(pw1)->e;	/* maybe changed by handler */
	    emu_flags = Exception(pw1)->flags;
	    DE = Exception(pw1)->de;
	    Restore_Tg_Soft_Lim(Exception(pw1)->tg_soft_lim);
#ifdef STRICT_EXCEPTION
	    WP = Exception(pw1)->wp;
	    MU = Exception(pw1)->mu;
	    if (MU) { Fake_Overflow; }
#endif
	    pw1 = (pword *) (Exception(pw1) + 1);
	    pw2 = &A[1];	/* restore args, if any	*/
	    while (pw1 < B.args)
		*pw2++ = *pw1++;
	    B.any_frame = B.top->frame;	/* pop exception frame	*/
	    Next_Pp;


	Case(Exit_emulator, I_Exit_emulator)		/* return code */
	    err_code = PP++->nint;
_exit_emulator_:				/* (err_code[,scratch_pw]) */
	    pw1 = (B.top - 1)->frame.args;
	    SP = (pword *)((emu_code *)Invoc(pw1)->sp + 1);
	    if (err_code == PKEEP) {
		err_code = PSUCCEED;
	    } else {
		if (err_code != PTHROW) {
		    /* for PTHROW, this is done in I_Throw */
		    Untrail_Variables(Invoc(pw1)->tt, i, pw2);
		    Wipe(Invoc(pw1)->tg_before,TG);
		    TG = Invoc(pw1)->tg_before;
		    LD = Invoc(pw1)->ld;
		}
		TAGGED_WL = Invoc(pw1)->wl;
		Restore_Tg_Soft_Lim(Invoc(pw1)->tg_soft_lim);
	    }
	    E = Invoc(pw1)->e;
	    EB = Invoc(pw1)->eb;
	    GB = Invoc(pw1)->gb;
	    Debug_Check_Global
	    if (IsInterruptFrame((B.top - 1)))
	    {
		VM_FLAGS = (VM_FLAGS & ~INT_SAFE_BITS)
				| (Invoc(pw1)->flags & INT_SAFE_BITS);
		destroy_parser_env();
		PARSENV = Invoc(pw1)->parser_env;
		g_emu_.trace_data = Invoc(pw1)->trace_data;
		PostponedList = Invoc(pw1)->postponed_list;
	    }
	    g_emu_.it_buf = Invoc(pw1)->it_buf;
	    g_emu_.nesting_level = Invoc(pw1)->nesting_level;
	    g_emu_.global_variable = Invoc(pw1)->global_variable;

	    WP = Invoc(pw1)->wp;
	    WP_STAMP = Invoc(pw1)->wp_stamp;
	    MU = Invoc(pw1)->mu;
	    SV = Invoc(pw1)->sv;
	    DE = Invoc(pw1)->de;
#ifdef PB_MAINTAINED
	    PB = Invoc(pw1)->pb;
#endif
#ifdef NEW_ORACLE
	    TO = Invoc(pw1)->oracle;
	    FO = Invoc(pw1)->followed_oracle;
	    PO = Invoc(pw1)->pending_oracle;
#endif
	    PPB = Invoc(pw1)->ppb;
	    Set_Bip_Error(Invoc(pw1)->global_bip_error);
	    GCTG = Invoc(pw1)->gctg;
	    PP = (emu_code) Invoc(pw1)->pp;

	    pw2 = &A[0];
	    pw1 = &Invoc(pw1)->arg_0;
	    while(Top(pw1) < B.top - 1)
		*pw2++ = *pw1++;
	    B.args = Top(pw1)->frame.args - SAFE_B_AREA;
	    Export_All
	    re_fake_overflow();		/* after export */
	    A[0].val.nint = err_code;
	    if (err_code == PTHROW)
	    	A[1] = scratch_pw;
	    return (func_ptr) 0;

	Case(Bounce, I_Bounce)	/* bounce over the trampoline */
	    PP++;
	    Export_All
	    /*
	    {
		extern func_ptr compiledcode();
		return (func_ptr) compiledcode;
	    }
	    */
	    return (func_ptr) (PP-1)->func;


	Case(Gc, I_Gc)			/* (forceflag) */
	    tmp1 = PP++->offset;
	    Export_B_Sp_Tg_Tt_Eb_Gb
	    err_code = collect_stacks(0L, tmp1);
	    Import_B_Sp_Tg_Tt_Eb_Gb
#if 0
	    if (err_code > 0)		/* request to leave a choicepoint */
	    {
		pw1 = B.args;
		if (!IsGcFrame(BTop(pw1)))
		{
		    B.chp = Chp(pw1) + 1;
		    B.top->frame.top = Top(pw1);
		    B.top->backtrack = gc_fail_code_;
		    B.top++;
		    Check_Control_Overflow
		    Chp(pw1)->sp = EB = SP;
		    Chp(pw1)->e = E;
		    Clr_Det;
		}
		else	/* reuse the existing one */
		{
		    /* Do not update E and SP fields in the choicepoint,
		     * because that can interfere with subsequent cuts!
		     */
		    pw1 = BPrev(pw1);
		    EB = Chp(pw1)->sp;
		}
		Chp(pw1)->tg = GB = TG;
		Push_Witness
		Chp(pw1)->tt = TT;
		Chp(pw1)->ld = LD;
		GCTG = TG;
	    }
	    else if (err_code < 0)	/* invalidate dummy choicepoint */
	    {
		pw1 = BPrev(B.args);
		Chp(pw1)->tg = GB = BChp(pw1)->tg;
		Chp(pw1)->tt = BChp(pw1)->tt;
		Chp(pw1)->ld = BChp(pw1)->ld;
		while (LCA >= GB)
		{
		    Export_B_Sp_Tg_Tt;
		    do_cut_action();
		    Import_Tg_Tt;
		}
		GCTG = TG;
	    }
#endif
	    Next_Pp;


#ifdef OLD_DYNAMIC
      Case(Clause, I_Clause)	/* Head, Body, Ref, Module, Error */
	    err_code = 0;
	    pw1 = &A[1];		/* clause head	*/
	    Dereference_Pw(pw1);
	    pw2 = &A[4];		/* module	*/
	    Dereference_Pw(pw2);
	    if (IsRef(pw1->tag) || IsRef(pw2->tag))
		err_code = INSTANTIATION_FAULT;
	    else if (!IsAtom(pw2->tag))
		err_code = TYPE_ERROR;
	    else if (!IsModule(pw2->val.did))
		err_code = MODULENAME;
	    else if IsStructure(pw1->tag)		/* find the did	*/
		val_did = pw1->val.ptr->val.did;
	    else if IsAtom(pw1->tag)
		val_did = pw1->val.did;
	    else if IsList(pw1->tag)
		val_did = d_.list;
	    else if IsNil(pw1->tag)
		val_did = d_.nil;
	    else
		err_code = TYPE_ERROR;

	    if (err_code == 0)	/* there is no instantiation fault 
				       or type error */
	    {
		Export_B_Sp_Tg_Tt
		proc = visible_procedure(val_did, pw2->val.did, pw2->tag, 0);
		Import_None
		if (proc)
		{
		    if (proc->module_ref == pw2->val.did)
		    {
			if (DynamicProc(proc))
			{
			    PP = (emu_code) StartOfProcSource(PriCode(proc));
			    Next_Pp;    /* go and execute the source clause*/
			}
			else if (PriFlags(proc) & CODE_DEFINED)
			    err_code = NOT_DYNAMIC;
			else
			    err_code = ACCESSING_UNDEF_DYN_PROC;
		    }
		    else
			err_code = ACCESSING_NON_LOCAL;
		}
		else
		{
		    Get_Bip_Error(err_code);
		    if (err_code == NOENTRY)
			err_code = ACCESSING_UNDEF_DYN_PROC;
		}
	    }
	    /* we have an error */
	    pw1 = &A[5];		/* bind error code */
	    Dereference_Pw(pw1);
	    Trail_If_Needed(pw1);
	    pw1->val.nint = -err_code;
	    pw1->tag.kernel = TINT;
	    Next_Pp;
#endif


/*----------------------------------------------------------------------
 * Abstract machine instructions for compilation of builtins
 *----------------------------------------------------------------------*/

	Case(BI_Exit, I_BI_Exit)
	    err_code = PP->arg->val.nint;
	    goto _exit_emulator_;

         Case(BI_CutToStamp, I_BI_CutToStamp)	/* Ai Aj Mask=0000 */
            Get_Argument(pw2) 
	    Dereference_Pw(pw2);
	    Get_Argument(pw1)
	    Dereference_Pw(pw1);
	    ++PP;
	    if (!IsStructure(pw2->tag) || !IsInteger(pw1->tag)) {
		err_code = TYPE_ERROR;
		proc = cut_to_stamp_proc_;
		goto _nbip_err_;
	    }
	    pw2 = pw2->val.ptr;
	    if (pw1->val.nint < 1 || pw1->val.nint > DidArity(pw2->val.did)) {
		err_code = RANGE_ERROR;
		proc = cut_to_stamp_proc_;
		goto _nbip_err_;
	    }
	    pw2 += pw1->val.nint;
	    if (!IsRef(pw2->tag)) {
		err_code = TYPE_ERROR;
		proc = cut_to_stamp_proc_;
		goto _nbip_err_;
	    }
	    /* We should probably have some extra checks here to guard against
	     * cutting through invocation frames and maybe even blocks. */
	    for(pw1 = B.args; OlderStamp(pw2,pw1); pw1 = BPrev(pw1))
		;
	    Cut_To(pw1);	/* Cut all choicepoints newer than the stamp */
	    Next_Pp;

         Case(BI_SetBipError, I_BI_SetBipError)
	    if (g_emu_.global_bip_error == 0)
	    {
		Get_Argument(pw1)
		Dereference_Pw_Tag(pw1, tmp1);
		if (IsTag(tmp1, TINT))
		    Set_Bip_Error(- pw1->val.nint);
	    }
	    Fail;

         Case(BI_GetBipError, I_BI_GetBipError)	/* Ai(uninit) */
	    Get_Bip_Error(err_code);
	    if (err_code)
	    {
		Get_Argument(pw1)
		Make_Integer(pw1, -err_code);
		Next_Pp;
	    }
	    Fail;

          Case(BI_Free, I_BI_Free)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!(ISVar(tmp1) || IsTag(tmp1,TNAME))) { Fail }
	    Next_Pp;

          Case(BI_Var, I_BI_Var)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!ISRef(tmp1)) { Fail }
	    Next_Pp;

          Case(BI_NonVar, I_BI_NonVar)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (ISRef(tmp1)) { Fail }
	    Next_Pp;

          Case(BI_Atom, I_BI_Atom)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!(IsTag(tmp1, TDICT) || IsTag(tmp1, TNIL))) { Fail }
	    Next_Pp;

          Case(BI_Integer, I_BI_Integer)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!(IsTag(tmp1, TINT) || IsTag(tmp1,TBIG))) { Fail }
	    Next_Pp;

          Case(BI_Bignum, I_BI_Bignum)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!IsTag(tmp1,TBIG)) { Fail }
	    Next_Pp;

	 Case(BI_Float, I_BI_Float)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!IsTag(tmp1,TDBL)) { Fail }
	    Next_Pp;

         Case(BI_Breal, I_BI_Breal)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!(IsTag(tmp1,TIVL))) { Fail }
	    Next_Pp;

         Case(BI_Real, I_BI_Real)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!(IsTag(tmp1,TDBL) || IsTag(tmp1,TIVL)))
		{ Fail }
	    Next_Pp;

         Case(BI_Rational, I_BI_Rational)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!IsTag(tmp1,TRAT)) { Fail }
	    Next_Pp;

         Case(BI_String, I_BI_String)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!IsTag(tmp1,TSTRG)) { Fail }
	    Next_Pp;

         Case(BI_Number, I_BI_Number)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (ISRef(tmp1) || !tag_desc[TagTypeC(tmp1)].numeric) { Fail }
	    Next_Pp;

	 Case(BI_Atomic, I_BI_Atomic)
	 /* break original || into two ifs -- original did not compile
	    correctly on NT with gcc */
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (ISRef(tmp1)) { Fail }
            if (IsTag(tmp1, TLIST) || IsTag(tmp1, TCOMP)) { Fail }
	    Next_Pp;

         Case(BI_Compound, I_BI_Compound)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!(IsTag(tmp1, TLIST) || IsTag(tmp1, TCOMP))) { Fail }
	    Next_Pp;

         Case(BI_Callable, I_BI_Callable)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!(IsTag(tmp1,TCOMP) || IsTag(tmp1,TDICT) ||
	    	IsTag(tmp1,TLIST) || IsTag(tmp1,TNIL))) { Fail }
	    Next_Pp;

         Case(BI_Meta, I_BI_Meta)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!IsTag(tmp1,TMETA)) { Fail }
	    Next_Pp;

         Case(BI_IsSuspension, I_BI_IsSuspension)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!IsTag(tmp1, TSUSP) || SuspDead(pw1->val.ptr)) {
		Fail;
	    }
	    Next_Pp;

	 Case(BI_IsHandle, I_BI_IsHandle)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (!IsTag(tmp1, THANDLE)) {
		Fail;
	    }
	    Next_Pp;

         Case(BI_IsEvent, I_BI_IsEvent)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (IsTag(tmp1, THANDLE) && IsTag(pw1->val.ptr->tag.kernel, TEXTERN)) {
		extern t_ext_type heap_event_tid;
		if (ExternalClass(pw1->val.ptr) != &heap_event_tid) {
		    Fail;
		}
	    } 
	    else {
		if (!(IsAtom(pw1->tag) || IsNil(pw1->tag))) { Fail }
	    }
	    Next_Pp;

         Case(BI_IsList, I_BI_IsList)
	    Get_Argument(pw1)
	    Dereference_Pw_Tag(pw1, tmp1);
	    while (IsTag(tmp1, TLIST)) {
		pw1 = pw1->val.ptr + 1;
		Dereference_Pw_Tag(pw1, tmp1);
	    }
	    if (!IsTag(tmp1, TNIL)) {
		Fail;
	    }
	    Next_Pp;


        /*
         * ==/2, \==/2 and ~=/2 are implemented with the diff routine
         */
         Case(BI_Identical, I_BI_Identical)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    proc = identical_proc_;
	    goto _diff_;			/* (proc, pw1, pw2) */

         Case(BI_NotIdentical, I_BI_NotIdentical)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    proc = not_identical_proc_;
	    goto _diff_;			/* (proc, pw1, pw2) */

         Case(BI_Inequality, I_BI_Inequality)
	    Get_Argument(pw1)
	    Get_Argument(pw2) 
	    proc = inequality_proc_;
	    goto _diff_;			/* (proc, pw1, pw2, PP) */

         Case(BI_NotIdentList, I_BI_NotIdentList)
	    Get_Argument(pw1)
	    Get_Argument(pw2)
	    /* 3rd argument read later! */
	    proc = not_ident_list_proc_;
	    goto _diff_;			/* (proc, pw1, pw2, PP) */

         Case(BI_ContDebug, I_BI_ContDebug)
	    /* Allow normal tracing again, except pred is skipped.
	     * Always allow tracing wakes again.
	     */
	    if (TD)
	    {
		Clr_Tf_Flag(TD, TF_INTRACER);
#ifdef PRINTAM
		if (TfFlags(TD) & TF_SYSTRACE) {
		    /* reenable abstract instruction tracing, if necessary */
		    Clr_Tf_Flag(TD, TF_SYSTRACE);
		    VM_FLAGS |= TRACE;
		}
#endif
	    }
	    Next_Pp;


/*
 * Instructions for the arithmetic builtins
 *
 * bi_minus	&Ai &Ak		2'000100
 * bi_add	&Ai &Aj &Ak	2'010000
 * bi_addi	i   &Aj &Ak	2'010010
 * bi_ge	&Ai &Aj	module	2'110000
 *
 * bi_arg	&Ai &Aj	&Ak	2'010000
 * bi_make_susp	&Ai &Aj	&Ak &Al	2'00000000 or 2'00010000
 *
 * CAUTION: the output argument(s) may be the same as the inputs.
 * Do not store there while the inputs are still needed!
 */

/* pw is assumed dereferenced */
#define NDelay_Check_1(pw)		\
	if (IsRef((pw)->tag)) {	\
		err_code = PDELAY_1;	\
		goto _npdelay_;		\
	}

#define NDelay_Check_2(pw)		\
	if (IsRef((pw)->tag)) {	\
		err_code = PDELAY_2;	\
		goto _npdelay_;		\
	}

#define NCompare_Bip(Proc, BIxx, Op) /* arity 3 */\
	proc = Proc;\
	PP+= 4;\
        pw1 = PP[-4].arg;\
        Dereference_Pw(pw1)\
	NDelay_Check_1(pw1)\
        pw2 = PP[-3].arg;\
        Dereference_Pw(pw2)\
	NDelay_Check_2(pw2)\
	/* don't Kill_DE here since arith_compare() can return PDELAY */\
	if (IsInteger(pw1->tag)) {\
	    if (IsInteger(pw2->tag))\
		if (pw1->val.nint Op pw2->val.nint)\
		    { goto _nbip_kill_succeed_;}\
		else\
		    { goto _nbip_fail_; }\
	    else if (IsDouble(pw2->tag))\
		if ((double)pw1->val.nint Op Dbl(pw2->val))\
		    { goto _nbip_kill_succeed_;}\
		else\
		    { goto _nbip_fail_; }\
	}\
	else if (IsDouble(pw1->tag)) {\
	    if (IsInteger(pw2->tag))\
		if (Dbl(pw1->val) Op (double)pw2->val.nint)\
		    { goto _nbip_kill_succeed_;}\
		else\
		    { goto _nbip_fail_; }\
	    else if (IsDouble(pw2->tag))\
		if (Dbl(pw1->val) Op Dbl(pw2->val))\
		    { goto _nbip_kill_succeed_;}\
		else\
		    { goto _nbip_fail_; }\
	}\
	if (IsNumber(pw1->tag) && IsNumber(pw2->tag)) {\
	    int relation = BIxx; /* don't use a register */ \
	    Export_B_Sp_Tg_Tt\
	    err_code = (long) arith_compare(pw1->val, pw1->tag,\
		pw2->val, pw2->tag, &relation);\
	    Import_Tg_Tt\
	    if (err_code == PDELAY){\
		SV = (pword *) 0;\
		goto _npdelay_always_;\
	    }\
	    if (err_code != PSUCCEED)\
		goto _nbip_err_;\
	    if (relation Op 0) {\
	    	goto _nbip_kill_succeed_;\
	    } else {\
		goto _nbip_fail_;\
	    }\
	}\
	err_code = COMPARE_TRAP;\
	goto _nbip_err_;


#define NGeneric_Arith_Overflow_Bip(BIxx, Op, SignOp, OpNr) /* arity 3 */\
	PP += 4;\
        pw1 = PP[-4].arg;\
        Dereference_Pw(pw1);\
	pw2 = PP[-3].arg;\
        Dereference_Pw(pw2);\
	NDelay_Check_1(pw1)\
	NDelay_Check_2(pw2)\
	Kill_DE;\
	if (IsInteger(pw1->tag)) {\
	    if (IsInteger(pw2->tag)) {\
		register word	n1 = pw1->val.nint;\
		register word	n2 = pw2->val.nint;\
		tmp1 = n1 Op n2;\
		if (((n1 >= 0) SignOp (n2 >= 0)) && \
		    (n1 >= 0) != (tmp1 >= 0)) {\
		    err_code = INTEGER_OVERFLOW;\
		    goto _nbip_err_;\
		} \
		PP[-2].arg->val.nint = tmp1;\
		PP[-2].arg->tag.kernel = TINT;\
		Next_Pp;\
	    }\
	    if (IsDouble(pw2->tag)) {\
		dbl_res = (double)pw1->val.nint Op Dbl(pw2->val);\
		goto _nis_float_check_;\
	    }\
	}\
	else if (IsDouble(pw1->tag)) {\
	    if (IsInteger(pw2->tag)) {\
		dbl_res = Dbl(pw1->val) Op (double)pw2->val.nint;\
		goto _nis_float_check_;\
	    }\
	    if (IsDouble(pw2->tag)) {\
		dbl_res = Dbl(pw1->val) Op Dbl(pw2->val);\
		goto _nis_float_check_;\
	    }\
	}\
	err_code = OpNr;\
	goto _nbin_op_;


#define NInt_Arith_Bip(Proc, BIxx, Op, OpNr) /* arity 3 */\
	proc = Proc;\
	PP += 4;\
        pw1 = PP[-4].arg;\
        Dereference_Pw(pw1);\
	NDelay_Check_1(pw1)\
        pw2 = PP[-3].arg;\
        Dereference_Pw(pw2);\
	NDelay_Check_2(pw2)\
	Kill_DE;\
	if (IsInteger(pw1->tag) && IsInteger(pw2->tag)) {\
	    PP[-2].arg->val.nint = pw1->val.nint Op pw2->val.nint;\
	    PP[-2].arg->tag.kernel = TINT;\
	    Next_Pp;\
	}\
	err_code = OpNr;\
	goto _nbin_op_;



	Case(BI_Minus, I_BI_Minus)
	    proc = minus_proc_;
	    PP += 3;
	    pw1 = PP[-3].arg;
	    Dereference_Pw(pw1);
	    NDelay_Check_1(pw1)
	    if (IsInteger(pw1->tag))
	    {
		if ((tmp1 = -pw1->val.nint) == MIN_S_WORD) {
		    err_code = INTEGER_OVERFLOW;
		    goto _nbip_err_;
		}
		Make_Integer(PP[-2].arg, tmp1);
		Next_Pp;
	    }
	    else if (IsDouble(pw1->tag))
	    {
		Make_Double(PP[-2].arg, -Dbl(pw1->val));
		Next_Pp;
	    }
	    err_code = ARITH_NEG;

_nun_op_:				/* (err_code,pw1,PP,proc) */
	    Export_B_Sp_Tg_Tt_Eb_Gb
	    err_code = un_arith_op(pw1->val, pw1->tag, PP[-2].arg, err_code, TINT);
	    Import_Tg_Tt
	    goto _nbip_res_;


	Case(BI_Addi, I_BI_Addi)
	    proc = add_proc_;
	    PP += 4;
	    pw1 = PP[-4].arg;
	    Dereference_Pw(pw1);
	    NDelay_Check_1(pw1)
	    Kill_DE;
	    if (IsInteger(pw1->tag)) {
		register word	n1 = pw1->val.nint;
		register word	n2 = PP[-3].nint;
		tmp1 = n1 + n2;
		if (((n1 >= 0) == (n2 >= 0)) && 
		    (n1 >= 0) != (tmp1 >= 0)) {
		    err_code = INTEGER_OVERFLOW;
		    goto _nbip_err_;
		} 
		Make_Integer(PP[-2].arg, tmp1);
		Next_Pp;
	    } else if (IsDouble(pw1->tag)) {
		dbl_res = Dbl(pw1->val) + (double)PP[-3].nint;
_nis_float_check_:			/* (dbl_res) */
		if (!GoodFloat(dbl_res))
		{
		    err_code = ARITH_EXCEPTION;
		    goto _nbip_err_;
		}
		Make_Double(PP[-2].arg, dbl_res);
		Next_Pp;
	    }
	    Make_Integer(&scratch_pw, PP[-3].nint);
	    pw2 = &scratch_pw;
	    err_code = ARITH_ADD;

_nbin_op_:		/* (err_code,pw1,pw2,proc,PP) */
	    Export_B_Sp_Tg_Tt_Eb_Gb
	    err_code = bin_arith_op(pw1->val, pw1->tag, pw2->val, pw2->tag, PP[-2].arg, err_code);
	    Import_Tg_Tt
	    goto _nbip_res_;


	Case(BI_Add, I_BI_Add)
	    proc = add_proc_;
	    NGeneric_Arith_Overflow_Bip(BIAdd, +, ==, ARITH_ADD)

	Case(BI_Sub, I_BI_Sub)
	    proc = sub_proc_;
	    NGeneric_Arith_Overflow_Bip(BISub, -, !=, ARITH_SUB)

	Case(BI_Mul, I_BI_Mul)
	    proc = mul_proc_;
	    PP += 4;
	    pw1 = PP[-4].arg;
	    Dereference_Pw(pw1);
	    NDelay_Check_1(pw1)
	    pw2 = PP[-3].arg;
	    Dereference_Pw(pw2);
	    NDelay_Check_2(pw2)
	    Kill_DE		/* it's a demon */
	    if (IsInteger(pw1->tag)) {
		if (IsInteger(pw2->tag))
		{
		    tmp1 = pw1->val.nint;
		    if (tmp1 != 0) {
			tmp1 *= pw2->val.nint;
			if (tmp1 == MIN_S_WORD ||			/* maybe */
			    tmp1/pw1->val.nint != pw2->val.nint)	/* for sure */
			{
			    err_code = INTEGER_OVERFLOW;
			    goto _nbip_err_;
			}
		    }
		    Make_Integer(PP[-2].arg, tmp1);
		    Next_Pp;
		}
		if (IsDouble(pw2->tag)) {
		    dbl_res = (double)pw1->val.nint * Dbl(pw2->val);
		    goto _nis_float_check_;
		}
	    }
	    else if (IsDouble(pw1->tag)) {
		if (IsInteger(pw2->tag)) {
		    dbl_res = Dbl(pw1->val) * (double)pw2->val.nint;
		    goto _nis_float_check_;
		}
		if (IsDouble(pw2->tag)) {
		    dbl_res = Dbl(pw1->val) * Dbl(pw2->val);
		    goto _nis_float_check_;
		}
	    }
	    err_code = ARITH_MUL;
	    goto _nbin_op_;		/* (err_code,pw1,pw2,proc,PP) */

	Case(BI_Quot, I_BI_Quot)
	    proc = quot_proc_;
	    PP += 4;
	    pw1 = PP[-4].arg;
	    Dereference_Pw(pw1);
	    NDelay_Check_1(pw1)
	    pw2 = PP[-3].arg;
	    Dereference_Pw(pw2);
	    NDelay_Check_2(pw2)
	    Kill_DE		/* it's a demon */
	    if (IsInteger(pw2->tag))
	    {
		if (IsInteger(pw1->tag)) {
		    if (GlobalFlags & PREFER_RATIONALS)
		    {
			err_code = ARITH_DIV;
			goto _nbin_op_;		/* (err_code,pw1,pw2,proc,PP) */
		    }
		    else
		    {
			dbl_res = (double)pw1->val.nint / (double)pw2->val.nint;
			goto _nis_float_check_;
		    }
		}
		if (IsDouble(pw1->tag)) {
		    dbl_res = Dbl(pw1->val) / (double)pw2->val.nint;
		    goto _nis_float_check_;
		}
	    }
	    else if (IsDouble(pw2->tag))
	    {
		if (IsInteger(pw1->tag)) {
		    dbl_res = (double)pw1->val.nint / Dbl(pw2->val);
		    goto _nis_float_check_;
		}
		if (IsDouble(pw1->tag)) {
		    dbl_res = Dbl(pw1->val) / Dbl(pw2->val);
		    goto _nis_float_check_;
		}
	    }
	    err_code = ARITH_DIV;
	    goto _nbin_op_;		/* (err_code,pw1,pw2,proc,PP) */

	Case(BI_Div, I_BI_Div)
	    proc = div_proc_;
	    PP += 4;
	    pw1 = PP[-4].arg;
	    Dereference_Pw(pw1);
	    NDelay_Check_1(pw1)
	    pw2 = PP[-3].arg;
	    Dereference_Pw(pw2);
	    NDelay_Check_2(pw2);
	    Kill_DE		/* it's a demon */
	    if (IsInteger(pw1->tag) && IsInteger(pw2->tag))
	    {
		if (pw2->val.nint == 0)
		{
		    err_code = ARITH_EXCEPTION;
		    goto _nbip_err_;
		}
		if (pw1->val.nint == MIN_S_WORD && pw2->val.nint == -1)
		{
		    err_code = INTEGER_OVERFLOW;
		    goto _nbip_err_;
		}
		Make_Integer(PP[-2].arg, pw1->val.nint / pw2->val.nint);
		Next_Pp;
	    }
	    err_code = ARITH_IDIV;
	    goto _nbin_op_;		/* (err_code,pw1,pw2,proc,PP) */

	Case(BI_Rem, I_BI_Rem)
	    proc = rem_proc_;
	    PP += 4;
	    pw1 = PP[-4].arg;
	    Dereference_Pw(pw1);
	    NDelay_Check_1(pw1);
	    pw2 = PP[-3].arg;
	    Dereference_Pw(pw2);
	    NDelay_Check_2(pw2);
	    Kill_DE		/* it's a demon */
	    if (IsInteger(pw1->tag) && IsInteger(pw2->tag))
	    {
		if (pw2->val.nint == 0)
		{
		    err_code = ARITH_EXCEPTION;
		    goto _nbip_err_;
		}
		PP[-2].arg->val.nint =
#if defined(i386) || defined(__x86_64) || defined(__POWERPC__)
		    /* need to check this, causes arith exception on i386 */
		    (/* pw1->val.nint == MIN_S_WORD && */ pw2->val.nint == -1) ? 0L :
#endif
		    /* Assume % truncates towards zero */
		       pw1->val.nint % pw2->val.nint;
		PP[-2].arg->tag.kernel = TINT;
		Next_Pp;
	    }
	    err_code = ARITH_MOD;
	    goto _nbin_op_;		/* (err_code,pw1,pw2,proc,PP) */

	Case(BI_FloorDiv, I_BI_FloorDiv)
	    proc = fdiv_proc_;
	    PP += 4;
	    pw1 = PP[-4].arg;
	    Dereference_Pw(pw1);
	    NDelay_Check_1(pw1);
	    pw2 = PP[-3].arg;
	    Dereference_Pw(pw2);
	    NDelay_Check_2(pw2);
	    Kill_DE		/* it's a demon */
	    if (IsInteger(pw1->tag) && IsInteger(pw2->tag))
	    {
		if (pw2->val.nint == 0)
		{
		    err_code = ARITH_EXCEPTION;
		    goto _nbip_err_;
		}
		if (pw1->val.nint == MIN_S_WORD && pw2->val.nint == -1)
		{
		    err_code = INTEGER_OVERFLOW;
		    goto _nbip_err_;
		}
		tmp1 = pw1->val.nint / pw2->val.nint;
		/* Need to adjust rounding if opposite signs */
		if (((pw1->val.nint ^ pw2->val.nint) < 0) && (pw1->val.nint % pw2->val.nint))
		    --tmp1;
		Make_Integer(PP[-2].arg, tmp1);
		Next_Pp;
	    }
	    err_code = ARITH_FLOORDIV;
	    goto _nbin_op_;		/* (err_code,pw1,pw2,proc,PP) */

	Case(BI_FloorRem, I_BI_FloorRem)
	    proc = mod_proc_;
	    PP += 4;
	    pw1 = PP[-4].arg;
	    Dereference_Pw(pw1);
	    NDelay_Check_1(pw1);
	    pw2 = PP[-3].arg;
	    Dereference_Pw(pw2);
	    NDelay_Check_2(pw2);
	    Kill_DE		/* it's a demon */
	    if (IsInteger(pw1->tag) && IsInteger(pw2->tag))
	    {
		if (pw2->val.nint == 0) {
#ifdef KNUTH_EXTENDED_MOD
		    /* extension according to Knuth Vol 1, 1.2.4 */
		    tmp1 = pw1->val.nint;
#else
		    err_code = ARITH_EXCEPTION;
		    goto _nbip_err_;
#endif
#if defined(i386) || defined(__x86_64) || defined(__POWERPC__)
		/* need to check this, causes arith exception on i386 */
		} else if (/* pw1->val.nint == MIN_S_WORD && */ pw2->val.nint == -1) {
		    tmp1 = 0L;
#endif
		} else {
		    /* Assume % truncates towards zero */
		    tmp1 = pw1->val.nint % pw2->val.nint;
		    /* Need to adjust nonzero results if opposite signs */
		    if (tmp1 && (pw1->val.nint ^ pw2->val.nint) < 0)
			tmp1 += pw2->val.nint;
		}
		Make_Integer(PP[-2].arg, tmp1);
		Next_Pp;
	    }
	    err_code = ARITH_FLOORREM;
	    goto _nbin_op_;		/* (err_code,pw1,pw2,proc,PP) */

	Case(BI_And, I_BI_And)			/* the bit operations */
	    NInt_Arith_Bip(and_proc_, BIAnd, &, ARITH_AND)

	Case(BI_Or, I_BI_Or)
	    NInt_Arith_Bip(or_proc_, BIOr, |, ARITH_OR)

	Case(BI_Xor, I_BI_Xor)
	    NInt_Arith_Bip(xor_proc_, BIXor, ^, ARITH_XOR)

	Case(BI_Bitnot, I_BI_Bitnot)
	    proc = bitnot_proc_;
	    pw1 = PP->arg;
	    PP += 3;
	    Dereference_Pw(pw1);
	    NDelay_Check_1(pw1);
	    if (IsInteger(pw1->tag))
	    {
		Make_Integer(PP[-2].arg, ~ pw1->val.nint);
		Next_Pp;
	    }
	    err_code = ARITH_COM;
	    goto _nun_op_;		/* (err_code,pw1,PP,proc) */


	Case(BI_Lt, I_BI_Lt)	       /* The arithmetic comparisons */
	    NCompare_Bip(lt_proc3_, BILt, <)

	Case(BI_Le, I_BI_Le)
	    NCompare_Bip(le_proc3_, BILe, <=)

	Case(BI_Gt, I_BI_Gt)
	    NCompare_Bip(gt_proc3_, BIGt, >)

	Case(BI_Ge, I_BI_Ge)
	    NCompare_Bip(ge_proc3_, BIGe, >=)

	Case(BI_Eq, I_BI_Eq)
	    NCompare_Bip(eq_proc3_, BIEq, ==)

	Case(BI_Ne, I_BI_Ne)
	    NCompare_Bip(ne_proc3_, BINe, !=)

	Case(BI_Arity, I_BI_Arity)		/* arity(+Term,-N)	*/
	    pw1 = PP->arg;
	    PP += 3;	/* 2 args + desc */
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (IsTag(tmp1, TCOMP)) {
		Make_Integer(PP[-2].arg, DidArity(pw1->val.ptr->val.did));
		Next_Pp;
	    } else if (IsTag(tmp1, TLIST)) {
		Make_Integer(PP[-2].arg, 2);
		Next_Pp;
	    } else if (!ISRef(tmp1)) {
		Make_Integer(PP[-2].arg, 0);
		Next_Pp;
	    }
	    proc = arity_proc_;
	    err_code = PDELAY_1;
	    goto _npdelay_;

	Case(BI_Arg, I_BI_Arg)			/* arg(+N, +Term, -Arg)	*/
	    proc = arg_proc_;
            PP += 4;
	    pw1 = PP[-3].arg;		/* check Term */
	    if (PP[-1].nint & 2) {
		pw2 = &scratch_pw;	/* immediate integer argument */
		Make_Integer(&scratch_pw, PP[-4].nint);
	    } else {
		pw2 = PP[-4].arg;
	    }
_narg_: 
/* pw1 and pw2 must be set correctly before jumping here */
	    Dereference_Pw_Tag(pw1, tmp1);
	    if (IsTag(tmp1, TCOMP))
	    {
		pw1 = pw1->val.ptr;
		i = DidArity(pw1->val.did);
	    }
	    else if (IsTag(tmp1, TLIST))
	    {
		pw1 = pw1->val.ptr - 1;
		i = 2;
	    }
	    else if (ISRef(tmp1))
	    {
		Dereference_Pw_Tag(pw2, tmp1);
		if (ISRef(tmp1) || IsTag(tmp1,TINT) || IsTag(tmp1,TLIST)) {
		    err_code = PDELAY_2;
		    goto _npdelay_;
		}
		else if (IsTag(tmp1,TBIG))
		    err_code = RANGE_ERROR;
		else if (tag_desc[TagTypeC(tmp1)].numeric)
		    err_code = TYPE_ERROR;
		else
		    err_code = ARITH_TYPE_ERROR;
		goto _nbip_err_;
	    }
	    else
	    {
		err_code = TYPE_ERROR;
		goto _nbip_err_;
	    }
	    Dereference_Pw_Tag(pw2, tmp1);	/* check N */
	    if (IsTag(tmp1, TINT))
	    {
		tmp1 = pw2->val.nint;
		if (tmp1 >= 1 && tmp1 <= i)
		{
		    Kill_DE;			/* necessary before success */
		    *PP[-2].arg = pw1[tmp1];
		    Next_Pp;
		}
		else
		    err_code = RANGE_ERROR;
	    }
	    else if (ISRef(tmp1)) {
		err_code = PDELAY_1;
		goto _npdelay_;
	    }
	    else if (IsTag(tmp1,TBIG))
		err_code = RANGE_ERROR;
	    else if (tag_desc[TagTypeC(tmp1)].numeric)
		err_code = TYPE_ERROR;
	    else if (IsTag(tmp1, TLIST))
	    {
		scratch_pw = *pw2;
		pw2 = pw2->val.ptr;
		Dereference_Pw_Tag(pw2,tmp1);	/* car */
		tmp1 = pw2->tag.kernel;
		if (IsTag(tmp1, TINT))
		{
		    tmp1 = pw2->val.nint;
		    if (tmp1 >= 1 && tmp1 <= i)
		    {
			pw1 += tmp1;		/* get argument */
			pw2 = scratch_pw.val.ptr + 1;	/* cdr */
			Dereference_Pw(pw2);
			if (IsTag(pw2->tag.kernel, TNIL))
			{
			    Kill_DE;		/* necessary before success */
			    *PP[-2].arg = *pw1;
			    Next_Pp;
			}
			else
			{
			    /* pw1, pw2 pointing at the right place */
			    goto _narg_;
			}
		    }
		    else
			err_code = RANGE_ERROR;
		}
		else if (ISRef(tmp1)) {
		    err_code = PDELAY_1;
		    goto _npdelay_;			/* (err_code, proc) */
		}
		else if (IsTag(tmp1,TBIG))
		    err_code = RANGE_ERROR;
		else if (tag_desc[TagTypeC(tmp1)].numeric)
		    err_code = TYPE_ERROR;
		else
		    err_code = ARITH_TYPE_ERROR;
	    }
	    else
		err_code = ARITH_TYPE_ERROR;
	    goto _nbip_err_;



	    /*
	     * make_suspension(Goal, Prio, Susp, {Pri|CallerMod})
	     *
	     * Normal call:	make_suspension(Goal, Prio, Susp, CallerMod)
	     *
	     * Specially compiled call from inside a delay clause:
	     *			make_suspension(Goal, Prio, Susp, Pri)
	     */
	Case(BI_MakeSuspension, I_BI_MakeSuspension)
	    proc = make_suspension_proc_;
            PP += 5;
	    pw1 = PP[-5].arg;
	    Dereference_Pw_Tag(pw1, tmp1);	/* check goal argument */
	    if (IsTag(tmp1, TCOMP))
		val_did = pw1->val.ptr->val.did;
	    else if (IsTag(tmp1, TDICT))
		val_did = pw1->val.did;
	    else if (IsTag(tmp1, TLIST))
		val_did = d_.list;
	    else if (IsTag(tmp1, TNIL))
		val_did = d_.nil;
	    else {
		err_code = ISRef(tmp1) ? INSTANTIATION_FAULT : TYPE_ERROR;
		goto _nbip_err_;
	    }
	    pw2 = TG;				/* allocate suspension */
	    TG += SUSP_SIZE;
	    Check_Gc
	    pw3 = PP[-2].arg;
	    Dereference_Pw_Tag(pw3, tmp1);	/* find the pri */
	    if (IsTag(tmp1, TINT))		/* we have the pri already */
	    {
		procb = (pri *) pw3->val.wptr;
		pw2[SUSP_MODULE].val.did = procb->module_ref;
		pw2[SUSP_MODULE].tag.kernel = ModuleTag(procb->module_ref);
	    }
	    else if (IsTag(tmp1, TDICT))	/* we have to look up */
	    {
		if(!IsModule(pw3->val.did)) {
		    TG = pw2;			/* pop incomplete suspension */
		    err_code = MODULENAME;
		    goto _nbip_err_;
		}
		Export_B_Sp_Tg_Tt
		procb = visible_procedure(val_did, pw3->val.did, pw3->tag, 0);
		Import_None
		if (!procb) {
		    TG = pw2;			/* pop incomplete suspension */
		    Get_Bip_Error(err_code);
		    goto _nbip_err_;
		}
		pw2[SUSP_MODULE] = *pw3;
	    }
	    else {
		TG = pw2;			/* pop incomplete suspension */
		err_code = ISRef(tmp1) ? INSTANTIATION_FAULT : TYPE_ERROR;
		goto _nbip_err_;		/* (proc, err_code) */
	    }
	    pw3 = PP[-4].arg;
	    Dereference_Pw_Tag(pw3, tmp1);	/* find the priority */
	    if (IsTag(tmp1, TINT))
	    {
		tmp1 = pw3->val.nint;
		if (tmp1 == 0)			/* use procedure's setting */
		    tmp1 = PriPriority(procb);
		else if (tmp1 < 0 || tmp1 > SUSP_MAX_PRIO)
		    tmp1 = RANGE_ERROR;
	    }
	    else
		tmp1 = ISRef(tmp1) ? INSTANTIATION_FAULT : TYPE_ERROR;
	    if (tmp1 < 0) {
		TG = pw2;			/* pop incomplete suspension */
		err_code = tmp1;
		goto _nbip_err_;			/* (proc, err_code) */
	    }
	    Init_Susp_Header(pw2, procb);
	    Init_Susp_State(pw2, tmp1);		/* priority */
	    pw2[SUSP_GOAL] = *pw1;		/* deref'ed arg 1: goal */


	    if (Tracing && AnyPortWanted)
	    {
		Set_Susp_DebugInvoc(pw2, NINVOC);
		++NINVOC;
		if (PortWanted(DELAY_PORT) && OfInterest(PriFlags(procb), NINVOC-1, DLevel(TD)+1, 0) )
		{
		    err_code = DEBUG_DELAY_EVENT;
		    if (DBG_DELAY_INVOC == 0) {
			DBG_DELAY_INVOC = NINVOC-1;
		    }
		    /* to suppress tracing of the event handler call: */
		    Set_Tf_Flag(TD, TF_INTRACER);
		    goto _nbip_err_;			/* (proc, err_code) */
		}
	    }

	    pw1 = PP[-3].arg;			/* output unification */
	    Dereference_Pw(pw1);
	    if (IsRef(pw1->tag))
	    {
		/* Extra dereference to work around Bug 0855
		 * (an environment variable may have been globalised) */
		pw1 = pw1->val.ptr->val.ptr;
		if (IsVar(pw1->tag))
		{
		    Trail_If_Needed(pw1)
		    pw1->val.ptr = pw2;
		    pw1->tag.kernel = TSUSP;
		}
		else /* if(IsRef(pw1->tag)) */
		{
		    tmp1 = TSUSP;
		    goto _bind_nonstandard_;	/* (pw1, pw2, tmp1) */
		}
	    }
	    else { Fail }
	    Next_Pp;


/* the following instructions should be resurrected for double floats */
#ifndef TFLOAT
	Case(Out_get_floatAM, I_Out_get_floatAM)
	Case(Get_floatAM, I_Get_floatAM)
	Case(Read_float, I_Read_float)
	Case(Write_float, I_Write_float)
	Case(Push_float, I_Push_float)
	Case(Put_floatAM, I_Put_floatAM)
	Case(Puts_float, I_Puts_float)
	Case(In_get_floatAM, I_In_get_floatAM)
#endif
#ifndef OLD_DYNAMIC
        Case(Try_me_dynamic, I_Try_me_dynamic)
        Case(Retry_me_dynamic, I_Retry_me_dynamic)
        Case(Clause, I_Clause)
#endif
/***** not yet implemented *****/
	Case(Escapef, I_Escapef)
	Case(Escape, I_Escape)
/***** pseudoinstructions *****/
	Case(Code_end, I_Code_end)
	Case(Comment, I_Comment)
	default:
#ifdef PRINTAM
	    emu_break();
#endif
	    err_code = UNDEFINED;
	    val_did = d_.emulate;
	    goto _regular_err_;

	} /* end big switch or extension switch */

} /* end emulc() */



#if defined(PRINTAM) || defined(LASTPP)
emu_break(void) {}	/* a dummy function to put a breakpoint in */
#endif /* PRINTAM */


/*-------------------------------------------------- 
 * Signal handler for WAM-level profiling
 *--------------------------------------------------*/

#if defined(__GNUC__) && defined(HAVE_UCONTEXTGREGS)

#include <signal.h>
#define __USE_GNU	/* to get REG_XXX */
#include <ucontext.h>
#ifndef REG_ESI
#define REG_ESI ESI	/* e.g. on Solaris 10 */
#endif


RETSIGTYPE
sigprof_handler(int signr, siginfo_t* dummy, void *context)

#else

RETSIGTYPE
sigprof_handler(void)

#endif
{ 
    extern stream_id	profile_stream_;

    if (VM_FLAGS & PROFILING)
    {
	if (VM_FLAGS & EXPORTED)
	    (void) ec_outfw(profile_stream_, (word) g_emu_.pp);
	else
	{
	    (void) ec_outfw(profile_stream_, (word) Int_Pp);
	}
    }
}

