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
 * VERSION	$Id: emu_c_env.c,v 1.2 2008/07/10 00:33:05 jschimpf Exp $
 */

/*
 * IDENTIFICATION		emu_c_env.c
 *
 * DESCRIPTION			This file contains auxiliary C functions for
 *				the emulator.
 */

#include "config.h"
#include "sepia.h"
#include "types.h"
#include "error.h"
#include "embed.h"
#include "mem.h"
#include "io.h"
#include "dict.h"
#include "emu_export.h"
#include "module.h"
#include "debug.h"
#include "opcode.h"

extern int		*interrupt_handler_flags_;
extern dident		*interrupt_name_;
extern jmp_buf reset;
extern pword		*p_meta_arity_;
extern void		msg_nopoll();
extern void		ec_init_globvars(void);
extern int		ec_init_postponed(void);

void			wl_init(int prio);

#define Bind_Named(pwn, pw)\
                         Trail_Tag_If_Needed_Gb(pwn); \
			 (pwn)->val.ptr = (pw);\
			 (pwn)->tag.kernel = TREF;

#define DELAY_SLOT		1	/* 'suspend' attribute slot */
#define CONSTRAINED_OFF		2	/* 'constrained' list */
/* If you change the above, update ic.c as well. */



/*------------------------------------------
 * the various entry points to the emulator
 *------------------------------------------*/

/*
 * What are the setjmp/longjmps good for?
 * In order to allow exit_block/1 from inside an interrupt emulator, we
 * map it onto the longjmp feature of C. Every emulator call is preceded
 * by a setjmp(), which catches longjmps that are executed while this
 * emulator invocation is active. When a longjmp is caught, we call the
 * emulator again and let it execute an exit_block/1. If the exit_block/1
 * is not caught inside this recursion level, the emulator exits with PTHROW
 * and we have to continue exiting in older emulator invocations by doing
 * another longjmp.
 */

extern vmcode	eval_code_[],
		recurs_code_[],
		slave_code_[],
		boot_code_[],
		it_code_[],
		it_block_code_[],
		*do_exit_block_code_;

extern vmcode	stop_fail_code_[],
		slave_fail_code_[],
		it_fail_code_[];

extern vmcode	*fail_code_,
		*bip_error_code_;

extern st_handle_t eng_root_branch;

/*
 * If we have reinitialised or restored the machine state,
 * we must make sure that the FakedOverflow condition is
 * in the corresponding state.
 */
void
re_fake_overflow(void)
{
    Disable_Int();
    if (MU ||
    	(EVENT_FLAGS && g_emu_.nesting_level == 1 && !PO) ||
	InterruptsPending)
    {
	if (g_emu_.nesting_level > 1) {
	    Interrupt_Fake_Overflow;	/* maybe we are in an interrupt */
	} else {
	    Fake_Overflow;
	}
    }
    else
    {
	Reset_Faked_Overflow;
    }
    Enable_Int();
}

#define EMU_INIT_LD	1
#define EMU_INIT_WL	2
#define EMU_INIT_GV	4

static void
save_vm_status(vmcode *fail_code, int options)
{
    register pword *pw1;
    register control_ptr b_aux;
    register uint32 i;
    extern vmcode fail_return_env_0_[];

    /*
     * Build the invocation frame
     *
     * We leave space for one inline frame (the biggest frame with constant size)
     * on top of the control stack to prevent overwriting useful information
     * in interrupt emulators. Thus we don't have to mask interrupts when
     * building small control frames.
     * We push a dummy return address onto the local stack because
     * the GC relies on the sp-entries in control frames pointing to
     * valid return addresses.
     */

    /* push a dummy return address (needed in the GC) */
    SP = (pword *) (((vmcode **) SP) -1);
    *((vmcode **) SP) = &fail_return_env_0_[1];

    i = VM_FLAGS;
    Disable_Int()			/* will be reset in ..._emulc() */
    B.args += SAFE_B_AREA;		/* leave some free space */
    b_aux.args = B.args;

    b_aux.invoc->tg_before = TG;	/* for restoring TG after exiting */

    b_aux.invoc->wl = TAGGED_WL;
    b_aux.invoc->wp = WP;
    b_aux.invoc->wp_stamp = WP_STAMP;
    if (options & EMU_INIT_WL)
    {
	/* wl_init() must be done between saving tg_before and tg */
	wl_init(SUSP_MAX_PRIO);		/* pushes stuff on TG, sets WL */
	/* don't update timestamp, WP must look "old" */
	WP = DEFAULT_PRIO;
    }

#ifdef NEW_ORACLE
    b_aux.invoc->oracle = TO;
    b_aux.invoc->followed_oracle = FO;
    b_aux.invoc->pending_oracle = PO;
    FO = PO = (char *) 0;
    TO = (pword *) 0;
    /* no oracles in recursive emulators! */
    if (g_emu_.nesting_level == 0  &&  VM_FLAGS & ORACLES_ENABLED)
    {
	O_Push(1, O_PAR_ORACLE);	/* also inits TO */
    }
#endif

    b_aux.invoc->global_variable = g_emu_.global_variable;
    b_aux.invoc->postponed_list = PostponedList;
    if (options & EMU_INIT_GV)
    {
	ec_init_globvars();

	ec_init_postponed();

	/* no need to save/restore POSTED: ignored in nested engines */

    	b_aux.invoc->trace_data = g_emu_.trace_data;
	Make_Integer(&TAGGED_TD, 0);
	FCULPRIT = -1;
	/* FTRACE = NULL; */
    }

    b_aux.invoc->eb = EB;
    b_aux.invoc->sp = EB = SP;
    b_aux.invoc->gb = GB;
    b_aux.invoc->tg = GB = TG;		/* for retry from this frame */
    Push_Witness;			/* must be first new thing on global */
    b_aux.invoc->tt = TT;
    b_aux.invoc->e = E;
    b_aux.invoc->flags = i;
    b_aux.invoc->it_buf = g_emu_.it_buf;
    b_aux.invoc->nesting_level = g_emu_.nesting_level;
    b_aux.invoc->pp = PP;
    b_aux.invoc->mu = MU;
    b_aux.invoc->sv = SV;
    b_aux.invoc->ld = LD;
    b_aux.invoc->de = DE;
    b_aux.invoc->ppb = PPB;
#ifdef PB_MAINTAINED
    b_aux.invoc->pb = PB;
#endif
    b_aux.invoc->node = eng_root_branch;
    Get_Bip_Error(b_aux.invoc->global_bip_error);
    b_aux.invoc->gctg = GCTG;
    GCTG = TG;
    Save_Tg_Soft_Lim(b_aux.invoc->tg_soft_lim);
    b_aux.invoc->parser_env = PARSENV;

    pw1 = &A[0];
    b_aux.invoc->arg_0 = *pw1++;
    b_aux.invoc += 1;
    /* don't save any arguments for the initial frame to make invocation
     * frames identical size for all parallel engines */
    if (g_emu_.nesting_level > 0)
    {
	for(i = 1; i < NARGREGS; i++) {
	    if(pw1->tag.kernel != TEND) {
		*(b_aux.args)++ = *pw1++;
	    } else break;
	}
    }

    b_aux.top->backtrack = fail_code;
    b_aux.top->frame.invoc = B.invoc;
    B.top = b_aux.top + 1;
#ifdef PB_MAINTAINED
    PB =
#endif
    PPB = B.args;

    /*
     * Do some initialisation common to all recursive emulator invocations
     */

    g_emu_.nesting_level++;

    DE = MU = SV = (pword *) 0;

    if (options & EMU_INIT_LD)
	LD = (pword *) 0;

#ifdef OC
    OCB = (pword *) 0;
#endif

    re_fake_overflow();

    Restore_Tg_Soft_Lim(TG + TG_SEG)

    Set_Bip_Error(0);
}


/*
 * Idea for event handling: Have the EventPending check here in the loop
 * and dispatch to next predicate, handler or pred continuation, which
 * all correspond to a C function entry point.
 * This returns PSUCCESS or PFAIL or PTHROW (throw argument is in A1)
 */
static int
_emul_trampoline(void)
{
    extern func_ptr ec_emulate(void);
    continuation_t continuation = ec_emulate;
    do
    {
	continuation = (continuation_t) (*continuation)();
    } while (continuation);
    return A[0].val.nint;
}

static void
_start_goal(value v_goal, type t_goal, value v_mod, type t_mod)
{
    A[1].val.all = v_goal.all;
    A[1].tag.all = t_goal.all;
    A[2].val.all = v_mod.all;
    A[2].tag.all = t_mod.all;
}


/*
 * This is a wrapper round the emulator _emul_trampoline()
 * which catches the longjumps.
 * This procedure must be called with interrupts disabled (Disable_Int)!!!
 */
static int
emulc(void)
{
    jmp_buf	interrupt_buf;
    int jump;

    /*
     * (re)initialise the machine
     */

    jump = setjmp(interrupt_buf);
    
    switch(jump)
    {
    case PFAIL:
	/* We get here when a C++ external want to fail */
    	PP = fail_code_;
	break;
    case PTHROW: 
	/* we get here when a recursive emulator throws or
	 * an external called Exit_Block() (eg. on stack overflow)
	 */
	PP = do_exit_block_code_;
	/* In case we're within Disable_Exit() section,
	 * we must clear the NO_EXIT flag on reentry to *this*
	 * emulator!
	 */
	VM_FLAGS &= ~NO_EXIT;
	/* in case we aborted in polling mode */
	msg_nopoll();
	break;
    case 0:
	/* We are in the first call */
	g_emu_.it_buf = (jmp_buf *) interrupt_buf; /* clean: &interrupt_buf */
	Enable_Int();		/* not earlier, since it may call a
				 * recursive emulator that throws */
	break;
    default:
    	/* We get here when a C++ external wants to raise an error */
    	PP = bip_error_code_;
	break;

    }
    return _emul_trampoline();
}

/*
 * This emulator untrails and pops all stacks before returning.
 * It should be used when it is known that no variable that is
 * older than this emulator can be bound (like for file queries).
 */

main_emulc_noexit(value v_goal, type t_goal, value v_mod, type t_mod)
{
    save_vm_status(&stop_fail_code_[0], EMU_INIT_LD|EMU_INIT_WL);
    PP = &eval_code_[0];
    _start_goal(v_goal, t_goal, v_mod, t_mod);
    return emulc();
}

query_emulc_noexit(value v_goal, type t_goal, value v_mod, type t_mod)
{
    int		result;
    save_vm_status(&stop_fail_code_[0], EMU_INIT_LD|EMU_INIT_WL);
    PP = &eval_code_[0];
    _start_goal(v_goal, t_goal, v_mod, t_mod);
    result = emulc();
    while (result == PYIELD)
    {
	Make_Atom(&A[1], in_dict("Nested emulator yielded",0));
	Make_Integer(&A[2], RESUME_CONT);
    	result = restart_emulc();
    }
    return result;
}

query_emulc(value v_goal, type t_goal, value v_mod, type t_mod)
{
    int		result;

    result = query_emulc_noexit(v_goal, t_goal, v_mod, t_mod);

    if (result == PTHROW)
	longjmp(*g_emu_.it_buf, PTHROW);
    return result;
}

slave_emulc(void)
{
    int		result;

    save_vm_status(&slave_fail_code_[0], EMU_INIT_LD|EMU_INIT_WL);
    PP = &slave_code_[0];

    result = emulc();
    while (result == PYIELD)
    {
	Make_Atom(&A[1], in_dict("Nested emulator yielded",0));
	Make_Integer(&A[2], RESUME_CONT);
    	result = restart_emulc();
    }

    if (result == PTHROW)
	longjmp(*g_emu_.it_buf, PTHROW);
    return result;

}

restart_emulc(void)
{
    Disable_Int();
    return emulc();
}

debug_emulc(value v_goal, type t_goal, value v_mod, type t_mod)
{
    int		result;

    /* we want to see the delayed goals, don't reset LD */
    save_vm_status(&stop_fail_code_[0], EMU_INIT_WL);
    PP = &eval_code_[0];

    _start_goal(v_goal, t_goal, v_mod, t_mod);
    result = emulc();
    while (result == PYIELD)
    {
	Make_Atom(&A[1], in_dict("Nested emulator yielded",0));
	Make_Integer(&A[2], RESUME_CONT);
    	result = restart_emulc();
    }

    if (result == PTHROW)
	longjmp(*g_emu_.it_buf, PTHROW);
    return result;
}


/*
 * This emulator is to be used if the recursive emulator may bind
 * outside variables or leave something useful on the global stack
 */

sub_emulc_noexit(value v_goal, type t_goal, value v_mod, type t_mod)
{
    int result;
    save_vm_status(&stop_fail_code_[0], 0);
    PP = &recurs_code_[0];

    _start_goal(v_goal, t_goal, v_mod, t_mod);
    result = emulc();
    while (result == PYIELD)
    {
	Make_Atom(&A[1], in_dict("Nested emulator yielded",0));
	Make_Integer(&A[2], RESUME_CONT);
    	result = restart_emulc();
    }
    return result;
}

sub_emulc(value v_goal, type t_goal, value v_mod, type t_mod)
{
    int		result;

    result = sub_emulc_noexit(v_goal, t_goal, v_mod, t_mod);

    if (result == PTHROW)
	longjmp(*g_emu_.it_buf, PTHROW);
    return result;
}

/*
 * For booting: the 1st argument is the bootfile name
 */ 

boot_emulc(value v_file, type t_file, value v_mod, type t_mod)
{
    int		result;
    save_vm_status(&stop_fail_code_[0], EMU_INIT_LD);
    PP = &boot_code_[0];
    _start_goal(v_file, t_file, v_mod, t_mod);
    result = emulc();
    while (result == PYIELD)
    {
	Make_Atom(&A[1], in_dict("Nested emulator yielded",0));
	Make_Integer(&A[2], RESUME_CONT);
    	result = restart_emulc();
    }
    return result;
}


/*
 * make an exit_block with the given exit tag
 */

int
return_throw(value v_tag, type t_tag)
{
    A[1].val.all = v_tag.all;
    A[1].tag.all = t_tag.all;
    return PTHROW;
}

longjmp_throw(value v_tag, type t_tag)
{
    A[1].val.all = v_tag.all;
    A[1].tag.all = t_tag.all;
    longjmp(*g_emu_.it_buf, PTHROW);
}


delayed_exit(void)
{
    pword goal, mod;
    goal.val.did = d_.exit_postponed;
    goal.tag.kernel = TDICT;
    mod.val.did = d_.kernel_sepia;
    mod.tag.kernel = ModuleTag(d_.kernel_sepia);
    (void) query_emulc(goal.val, goal.tag, mod.val, mod.tag); /* will do a longjmp */
}


/* 
 * Interrupt emulator:
 *	the 1st argument is the signal number
 * When the exit_block protection is active,
 * the handler is called inside a block/3
 */

int
it_emulc(value v_sig, type t_sig)
{
    int		result;

    /* no handler set, don't bother starting an emulator */
    if (interrupt_handler_flags_[v_sig.nint] != IH_HANDLE_ASYNC)
    	return PSUCCEED;

    save_vm_status(&it_fail_code_[0], EMU_INIT_LD|EMU_INIT_GV);

    PARSENV = (void_ptr) 0;

    if (VM_FLAGS & NO_EXIT) {
	PP = &it_block_code_[0];
    } else {
	PP = &it_code_[0];
    }

    /* in case we interrupted in polling mode */
    msg_nopoll();
    A[1].val.all = v_sig.all;
    A[1].tag.all = t_sig.all;
    result = emulc();
    while (result == PYIELD)
    {
	Make_Atom(&A[1], in_dict("Nested emulator yielded",0));
	Make_Integer(&A[2], RESUME_CONT);
    	result = restart_emulc();
    }
    return result;
}


/*------------------------------------------
 * Synchronous event handling
 *------------------------------------------*/

#ifdef DEBUG_EVENT_Q
#define event_q_assert(ex) {						\
    if (!(ex)) {							\
	(void) p_fprintf(current_err_, "Assertion Failed at ");		\
	(void) p_fprintf(current_err_, "file \"%s\"", __FILE__);	\
	(void) p_fprintf(current_err_, " line %d\n", __LINE__);		\
	(void) ec_panic("Assertion Failed", "Event queue");		\
    }									\
}
#else
#define event_q_assert(ex)
#endif

static pword volatile posted_events_[MAX_STATIC_EVENT_SLOTS];
static int volatile first_posted_ = 0;
static int volatile next_posted_ = 0;

#define IsEmptyDynamicEventQueue()			\
	(g_emu_.dyn_event_q.free_event_slots == 	\
	 g_emu_.dyn_event_q.total_event_slots)

#define IsEmptyStaticEventQueue()			\
	(first_posted_ == next_posted_)

#ifdef PRINTAM
void
print_static_queued_events(void)
{
    int i;

    Disable_Int();
    i = first_posted_;
    p_fprintf(current_err_, "Static event queue:");
    while (i != next_posted_)
    {
	p_fprintf(current_err_, " %d:%x", posted_events_[i].tag.kernel, posted_events_[i].val.ptr);
	i = (i + 1) % MAX_STATIC_EVENT_SLOTS;
    }
    ec_newline(current_err_);
    Enable_Int();
}

void
print_dynamic_queued_events(void)
{
    dyn_event_q_slot_t *slot;
    unsigned long cnt = 0, total;

    Disable_Int();
    slot = g_emu_.dyn_event_q.prehead->next; /* get */
    total = g_emu_.dyn_event_q.total_event_slots - g_emu_.dyn_event_q.free_event_slots;
    p_fprintf(current_err_, "Dynamic event queue: Total: %ld Free: %ld:", 
	g_emu_.dyn_event_q.total_event_slots, g_emu_.dyn_event_q.free_event_slots);
    for( cnt = 0; cnt < total; cnt++, slot = slot->next )
    {
	p_fprintf(current_err_, " %d:%x", slot->event_data.tag.kernel, slot->event_data.val.ptr);
    }
    ec_newline(current_err_);
    Enable_Int();
}
#endif

static int
_post_event_static(pword event, int no_duplicates)
{
    int i;

    Check_Integer(event.tag);

    Disable_Int();

    if (no_duplicates)
    {
	/* if this event is already posted, don't do it again */
	for (i = first_posted_; i != next_posted_; i = (i + 1) % MAX_STATIC_EVENT_SLOTS)
	{
	    if (posted_events_[i].tag.all == event.tag.all
		&& posted_events_[i].val.all == event.val.all)
	    {
		Enable_Int();
		Succeed_;
	    }
	}
    }

    i = (next_posted_ + 1) % MAX_STATIC_EVENT_SLOTS;

    if (i == first_posted_)
    {
	Enable_Int();
	Bip_Error(RANGE_ERROR);	/* queue full */
    }

    posted_events_[next_posted_] = event;
    next_posted_ = i;		/* enter in queue */
    EVENT_FLAGS |= EVENT_POSTED|DEL_IRQ_POSTED;
    Interrupt_Fake_Overflow; 	/* Served in signal handler */
    Enable_Int();

    Succeed_;
}

static int
_post_event_dynamic(pword event, int no_duplicates)
{
    extern t_ext_type heap_event_tid;

    if (IsHandle(event.tag))
    {
	Check_Type(event.val.ptr->tag, TEXTERN);
	if (ExternalClass(event.val.ptr) != &heap_event_tid) {
		Bip_Error(TYPE_ERROR);
	}
        if (!(ExternalData(event.val.ptr))) {
	    Bip_Error(STALE_HANDLE);
	}

	/* If the event is disabled, don't post it to the queue */
	if (!((t_heap_event *)ExternalData(event.val.ptr))->enabled) {
	    Succeed_;
	}
	
	/* Don't put the handle in the queue! */
	event.tag.kernel = TPTR;
	event.val.wptr = heap_event_tid.copy(ExternalData(event.val.ptr));
    }
    else if (IsTag(event.tag.kernel, TPTR))
    {
	/* Assume it'a a TPTR to a t_heap_event (we use this when posting
	 * an event that was stored in a stream descriptor).
	 * As above, if the event is disabled, don't post it to the queue.
	 */
	if (!((t_heap_event *)event.val.ptr)->enabled) {
	    Succeed_;
	}
	event.val.wptr = heap_event_tid.copy(event.val.wptr);
    }
    else if (!IsAtom(event.tag))
    {
	Error_If_Ref(event.tag);
	Bip_Error(TYPE_ERROR);
    }

    /* Events are either atoms or handles (anonymous).
     * Such events go to the dynamic event queue
     */

    Disable_Int();

    if (no_duplicates)
    {
	unsigned long cnt, total;
	/* if this event is already posted, don't do it again */
	dyn_event_q_slot_t *slot = g_emu_.dyn_event_q.prehead->next; /* get */
	
	total = g_emu_.dyn_event_q.total_event_slots - g_emu_.dyn_event_q.free_event_slots;
	for( cnt = 0; cnt < total; cnt++, slot = slot->next )
	{
	    if (slot->event_data.tag.all == event.tag.all
	     && slot->event_data.val.all == event.val.all)
	    {
		/* If the anonymous event handle reference count was bumped
		 * (via the copy ready for queue insertion) decrement it again!
		 */
		if (IsTag(event.tag.kernel, TPTR))
		{
		    heap_event_tid.free(event.val.wptr);
		}
		Enable_Int();
		Succeed_;
	    }
	}
    }

    /* Is the queue full? */
    if (g_emu_.dyn_event_q.free_event_slots != 0) 
    {
	/* No! */
	g_emu_.dyn_event_q.free_event_slots--;
    }
    else
    {
	/* Yes! */
	dyn_event_q_slot_t *slot;

	event_q_assert(g_emu_.dyn_event_q.prehead == 
		       g_emu_.dyn_event_q.tail); /* put == get */

	if ((slot = (dyn_event_q_slot_t *)hp_alloc_size(sizeof(dyn_event_q_slot_t))) == NULL) 
	{
	    Enable_Int();
	    Bip_Error(RANGE_ERROR); /* not enough memory - queue full */
	}
	slot->next = g_emu_.dyn_event_q.tail->next;
	g_emu_.dyn_event_q.tail->next = slot;
	g_emu_.dyn_event_q.total_event_slots++;
	g_emu_.dyn_event_q.prehead = g_emu_.dyn_event_q.prehead->next; /* reflect insertion */
    }

    g_emu_.dyn_event_q.tail = g_emu_.dyn_event_q.tail->next; /* update tail and put */
    g_emu_.dyn_event_q.tail->event_data = event; /* delayed set of old put */
    EVENT_FLAGS |= EVENT_POSTED;
    Fake_Overflow; /* Not served in signal handler */
    Enable_Int();

    Succeed_;
}

int
ec_post_event_unique(pword event)
{
    return _post_event_dynamic(event, 1);
}

int Winapi
ec_post_event(pword event)
{
    return _post_event_dynamic(event, 0);
}

int Winapi
ec_post_event_string(const char *event)
{
    pword pw;
    Make_Atom(&pw, in_dict((char *) event,0));
    return _post_event_dynamic(pw, 0);
}

int Winapi
ec_post_event_int(int event)
{
    pword pw;
    Make_Integer(&pw, event);
    return _post_event_static(pw, 0);
}

void
next_posted_event(pword *out)
{
    int n;

    /* Execute all static event queue entries before 
     * dynamic queue entries.
     * Assumption here is that it's ok to disrespect the 
     * precise post order of interleaved 
     * asynchronously-posted events with all other events.
     * i.e. synchronously-posted events.
     * In addition eventual servicing of dynamic event queue is
     * assumed and so starvation unlikely / not problematic!
     */

    Disable_Int();

    if ((n = next_urgent_event()) != -1)
    {
	Make_Integer(out, n);
    }
    else
    {
	/* Service the dynamic event queue */
	if (!IsEmptyDynamicEventQueue())
	{
	    g_emu_.dyn_event_q.prehead = 
		g_emu_.dyn_event_q.prehead->next; /* get = get->next */
	    *out = g_emu_.dyn_event_q.prehead->event_data; /* Delayed update of get */
	    g_emu_.dyn_event_q.free_event_slots++;
	}
	else
	{
	    /* The queues were empty although flag was set: shouldn't happen */
	    ec_panic("Bogus event queue notification", "next_posted_event()");
	}
    }

    /* If either queue contain events fake the over flow to handle next */
    if (IsEmptyStaticEventQueue() && 
	IsEmptyDynamicEventQueue()) 
    {
	event_q_assert(g_emu_.dyn_event_q.prehead == 
		       g_emu_.dyn_event_q.tail); /* put == get */
	EVENT_FLAGS &= ~EVENT_POSTED;
	event_q_assert(!(EVENT_FLAGS & DEL_IRQ_POSTED));
    }
    else
    {
	event_q_assert(EVENT_FLAGS & EVENT_POSTED);
	Fake_Overflow;
    }

    Enable_Int();
}

/*
 * The following is a hack to allow aborting looping unifications:
 * It is invoked within a possibly infinite emulator loop iff the
 * DEL_IRQ_POSTED flags is set (Poll_Interrupts macro).
 * We pick out the delayed async irqs from the event queue and
 * return them. 
 * If the event is an asynchrously-posted-synchronously-executed
 * event, then we move the event to the dynamic event queue and 
 * seek the next urgent event. EVENT_FLAGS are adjusted and 
 * if no urgent events left, -1 is returned.
 */

int
next_urgent_event(void)
{
    Disable_Int();

    while (!IsEmptyStaticEventQueue())
    {
	int n = posted_events_[first_posted_].val.nint;
	event_q_assert(!IsTag(posted_events_[first_posted_].tag.kernel, TEND));
	event_q_assert(IsInteger(posted_events_[first_posted_].tag));
	/* Remove element from queue */
	first_posted_ = (first_posted_ + 1) % MAX_STATIC_EVENT_SLOTS;
	if (interrupt_handler_flags_[n] == IH_POST_EVENT)
	{
	    /* Post the atom to the dynamic event queue for synchronous
	     * execution.
	     */
	    pword event;
	    Make_Atom(&event, interrupt_name_[n]);
	    if (_post_event_dynamic(event, 0) != PSUCCEED)
		(void) write(2,"\nEvent queue overflow - signal lost\n",36);
	}
	else
	{
	    event_q_assert(interrupt_handler_flags_[n] == IH_HANDLE_ASYNC);
	    if (IsEmptyStaticEventQueue()) 
	    {
    		EVENT_FLAGS &= ~DEL_IRQ_POSTED;
		if (IsEmptyDynamicEventQueue()) 
		{
		    EVENT_FLAGS &= ~EVENT_POSTED;
		}
	    }
	    Enable_Int();
	    return n;
	}
    }
    EVENT_FLAGS &= ~DEL_IRQ_POSTED; /* In case it got set in the meantime */

    Enable_Int();
    return -1;
}


/*
 * Remove a disabled event from the dynamic event queue
 */

void 
purge_disabled_dynamic_events(t_heap_event *event)
{
    dyn_event_q_slot_t *slot, *prev;
    unsigned long cnt = 0, total;
    pword *pevent;

    Disable_Int();

    total = g_emu_.dyn_event_q.total_event_slots - g_emu_.dyn_event_q.free_event_slots;

    if ( total == 0 ) {
	return;
    }

    prev = g_emu_.dyn_event_q.prehead;
    slot = prev->next; /* get */

    /* Process all slots but the tail */
    for( cnt = 1; cnt < total; cnt++ )
    {
	pevent = &slot->event_data;

	if (IsTag(pevent->tag.kernel, TPTR) && pevent->val.wptr == (uword*)event)
	{
	    g_emu_.dyn_event_q.free_event_slots++;
	    prev->next = slot->next;
	    slot->next = g_emu_.dyn_event_q.tail->next; /* insert before put */
	    g_emu_.dyn_event_q.tail->next = slot; /* update put */
	    ExternalClass(pevent->val.ptr)->free(ExternalData(pevent->val.ptr));
	    slot = prev->next;
	    continue;
	}

	prev = slot;
	slot = slot->next;
    }

    /* Special case tail element removal. This also handles the case 
     * where the circular list is full - in either case simply rewind 
     * the tail pointer.
     */
    event_q_assert(slot == g_emu_.dyn_event_q.tail);
    pevent = &slot->event_data;
    if (IsTag(pevent->tag.kernel, TPTR) && pevent->val.wptr == (uword*)event)
    {
	g_emu_.dyn_event_q.free_event_slots++;
	g_emu_.dyn_event_q.tail = prev;
	ExternalClass(pevent->val.ptr)->free(ExternalData(pevent->val.ptr));
    }

    /* If both static and dynamic event queues are 
     * now empty clear the flags 
     */
    if (IsEmptyDynamicEventQueue() &&
	IsEmptyStaticEventQueue()) 
    {
	EVENT_FLAGS &= ~EVENT_POSTED;
	event_q_assert(!(EVENT_FLAGS & DEL_IRQ_POSTED));
    }

    Enable_Int();
}


/*
 * Initialise dynamic event queue
 */

void
ec_init_dynamic_event_queue(void)
{
    int cnt;

    Disable_Int();

    if ((g_emu_.dyn_event_q.prehead = 
	(dyn_event_q_slot_t *)hp_alloc_size(sizeof(dyn_event_q_slot_t))) == NULL) 
    {
	ec_panic(MEMORY_P, "emu_init()");
    }

    g_emu_.dyn_event_q.tail = g_emu_.dyn_event_q.prehead;

    for(cnt = 0; cnt < MIN_DYNAMIC_EVENT_SLOTS - 1; cnt++) 
    {
	if ((g_emu_.dyn_event_q.tail->next = 
	    (dyn_event_q_slot_t *)hp_alloc_size(sizeof(dyn_event_q_slot_t))) == NULL) 
	{
	    ec_panic(MEMORY_P, "emu_init()");
	}
	g_emu_.dyn_event_q.tail = g_emu_.dyn_event_q.tail->next;
    }

    /* Link tail to head to complete circular list creation */
    g_emu_.dyn_event_q.tail->next = g_emu_.dyn_event_q.prehead;

    /* Set tail insertion point */
    /* Empty queue condition: 
     * IsEmptyDynamicEventQueue(). In addition, when queue is empty
     * or full: tail->next (put) == prehead->next (get)
     */
    g_emu_.dyn_event_q.tail = g_emu_.dyn_event_q.prehead;

    /* Dynamic queue is initially empty */
    g_emu_.dyn_event_q.total_event_slots = 
		g_emu_.dyn_event_q.free_event_slots = MIN_DYNAMIC_EVENT_SLOTS;

    Enable_Int();
}


/* Shrink the dynamic event queue to at least
 * MIN_DYNAMIC_EVENT_SLOTS free.
 * Used during GC.
 */

void
trim_dynamic_event_queue(void)
{
    Disable_Int();

    if (g_emu_.dyn_event_q.free_event_slots > MIN_DYNAMIC_EVENT_SLOTS)
    {
	dyn_event_q_slot_t *slot = g_emu_.dyn_event_q.tail->next; /* put */
	unsigned long new_free_slots =	g_emu_.dyn_event_q.free_event_slots / 
					DYNAMIC_EVENT_Q_SHRINK_FACTOR;
	if (new_free_slots < MIN_DYNAMIC_EVENT_SLOTS) {
	    new_free_slots = MIN_DYNAMIC_EVENT_SLOTS;
	}

	if (GlobalFlags & GC_VERBOSE) {
	    p_fprintf(log_output_,	
		      "shrink dynamic event queue from Total: %lu"
		      " Free: %lu to Total: %lu Free: %lu (elements)\n", 
		      g_emu_.dyn_event_q.total_event_slots, 
		      g_emu_.dyn_event_q.free_event_slots, 
		      g_emu_.dyn_event_q.total_event_slots - 
		      (g_emu_.dyn_event_q.free_event_slots - new_free_slots), new_free_slots);
		      ec_flush(log_output_);
	}

	for ( ; g_emu_.dyn_event_q.free_event_slots > new_free_slots 
	      ; g_emu_.dyn_event_q.free_event_slots--, 
		g_emu_.dyn_event_q.total_event_slots-- )
	{
	    g_emu_.dyn_event_q.tail->next = slot->next;
	    hp_free_size((generic_ptr)slot, sizeof(dyn_event_q_slot_t));
	    slot = g_emu_.dyn_event_q.tail->next;
	}
    }

    Enable_Int();
}



/*----------------------------------------------
 * Auxiliary functions for the emulator
 *----------------------------------------------*/

/*
 * UNIFY		var		nonvar		any
 * 
 * with var:		ec_unify()
 * 
 * with nonvar:		Bind_Var()	ec_unify()
 * 
 * with any:		ec_unify()	ec_unify()	ec_unify()
 */

/*
 * ec_unify() -- copy of the general unifier, callable from C code
 *
 * Note that Occur_Check_Boundary(0) is done after return from the builtin.
 */

int
ec_unify_(value v1, type t1,
	value v2, type t2,
	pword **list)		/* list of unified metaterms */
{
    register long arity;
    register pword *pw1, *pw2;

    /* In Request_Unify it may happen that the tag is REF/NAME but
       it has been already bound by a previous Request */
    if (IsRef(t1))
    {
	pw1 = v1.ptr;
	Dereference_(pw1);
	t1.all = pw1->tag.all;
	v1.all = pw1->val.all;
    }
    if (IsRef(t2))
    {
	pw2 = v2.ptr;
	Dereference_(pw2);
	t2.all = pw2->tag.all;
	v2.all = pw2->val.all;
    }
	
    for (;;)
    {
	if(IsVar(t1))
	{
	    if(IsVar(t2)) 		/* both are free:	*/
	    {
		if (v1.ptr < v2.ptr)
		    if (v1.ptr < TG)
		    {
			Trail_If_Needed(v2.ptr);
			v2.ptr->val.ptr = v1.ptr;
		    }
		    else
		    {
			Trail_If_Needed_Eb(v1.ptr);
			v1.ptr->val.ptr = v2.ptr;
		    }
		else if (v1.ptr > v2.ptr)
		    if (v2.ptr < TG)
		    {
			Trail_If_Needed(v1.ptr);
			v1.ptr->val.ptr = v2.ptr;
		    }
		    else
		    {
			Trail_If_Needed_Eb(v2.ptr);
			v2.ptr->val.ptr = v1.ptr;
		    }
		else
		    ;		/* succeed */
	    }
	    else 			/* only t1 is free */
	    {
		Occur_Check_Read(v1.ptr, v2, t2, return PFAIL)
		if (IsRef(t2)) {
		    Trail_If_Needed(v1.ptr);
		    v1.ptr->val.ptr = v2.ptr->val.ptr;
		} else {
		    Bind_(v1.ptr, v2.all, t2.all)
		}
	    }
	    return PSUCCEED;
	}
	else if (IsVar(t2))		/* only t2 is free */
	{
	    Occur_Check_Read(v2.ptr, v1, t1, return PFAIL)
	    if (IsRef(t1)) {
		Trail_If_Needed(v2.ptr);
		v2.ptr->val.ptr = v1.ptr->val.ptr;
	    } else {
		Bind_(v2.ptr, v1.all, t1.all)
	    }
	    return PSUCCEED;
	}
	else if (IsRef(t1))		/* t1 is a nonstandard variable */
	{
	    pword aux_pw;
	    Occur_Check_Read(v1.ptr, v2, t2, return PFAIL)
	    aux_pw.val.all = v2.all;
	    aux_pw.tag.all = t2.all;
	    return bind_c(v1.ptr, &aux_pw, list);
	}
	else if (IsRef(t2))		/* t2 is a nonstandard variable */
	{
	    pword aux_pw;
	    Occur_Check_Read(v2.ptr, v1, t1, return PFAIL)
	    aux_pw.val.all = v1.all;
	    aux_pw.tag.all = t1.all;
	    return bind_c(v2.ptr, &aux_pw, list);
	}
	/* two non-variables */
	else if (TagType(t1) != TagType(t2))
	{
	    return PFAIL;
	}
	else if (IsSimple(t1))
	{
	    if (SimpleEq(t1.kernel, v1, v2))
		return PSUCCEED;
	    else
		return PFAIL;
	}
	else if (IsList(t1))
	{
	    arity = 2;
	}
	else if (IsStructure(t1))
	{
	    if (v1.ptr->val.did != v2.ptr->val.did)
		return PFAIL;
	    if ((arity = DidArity(v1.ptr->val.did)) == 0L)
		return PSUCCEED;
	    v1.ptr++;
	    v2.ptr++;
	}
	else if (IsString(t1))
	{
	    Compare_Strings(v1, v2, arity)
	    if (arity >= 0)
		return PFAIL;
	    else
		return PSUCCEED;
	}
	else
	{
#ifdef PRINTAM
	    if (!(TagType(t1) >= 0 && TagType(t1) <= NTYPES))
	    {
	    p_fprintf(current_err_, "ec_unify(): unknown tag (%x) encountered\n",
			t1.kernel);
	    return PFAIL;
	    }
#endif
	    return tag_desc[TagType(t1)].equal(v1.ptr, v2.ptr) ? PSUCCEED : PFAIL;
	}
	
	/* arity > 0 */
	for (;;)
	{
	    pw1 = v1.ptr++;
	    pw2 = v2.ptr++;
	    Dereference_(pw1);
	    Dereference_(pw2);
	    if (--arity == 0L)
		break;
	    if (ec_unify_(pw1->val, pw1->tag, pw2->val, pw2->tag, list) == PFAIL)
		return PFAIL;
	}
	v1.all = pw1->val.all;
	t1.all = pw1->tag.all;
	v2.all = pw2->val.all;
	t2.all = pw2->tag.all;
    }
}


deep_suspend(value val, type tag,
	int position,		/* must be > 0 */
	pword *susp,		/* must be dereferenced */
	int slot)
{
    register int arity;
    register pword *arg_i;
    int		res;

    for (;;)
    {
	if (IsRef(tag))
	{
	    return insert_suspension(val.ptr, position, susp, slot);
	}
	else if (IsList(tag))
	    arity = 2;
	else if (IsStructure(tag))
	{
	    arity = DidArity(val.ptr->val.did);
	    val.ptr++;
	}
	else
	    return PSUCCEED;
 
	for(;arity > 1; arity--)
	{
	    arg_i = val.ptr++;
	    Dereference_(arg_i);
	    if (IsRef(arg_i->tag))
		res = insert_suspension(arg_i, position, susp, slot);
	    else
		res = deep_suspend(arg_i->val, arg_i->tag, position, 
				susp, slot);
	    if (res != PSUCCEED)
		return res;
	}
	arg_i = val.ptr;		/* tail recursion */
	Dereference_(arg_i);
	val.all = arg_i->val.all;
	tag.all = arg_i->tag.all;
    }
}


pword *
add_attribute(long int tv, pword *va, long int ta, int slot)
{
    register pword *s, *t;

    s = TG;
    TG += 2 + p_meta_arity_->val.nint + 1;
    s[0].val.ptr = s;		/* metaterm */
    s[0].tag.kernel = TagNameField(tv) | RefTag(TMETA);
    s[1].val.ptr = s + 2;
    s[1].tag.kernel = TCOMP;
    s[2].val.did = in_dict("meta", (int) p_meta_arity_->val.nint);
    s[2].tag.kernel = TDICT;
    for (t = &s[3]; t < TG; t++)
    {
	t->val.ptr = t;
	t->tag.kernel = TREF;
    }
    s[slot+2].val.ptr = va;
    s[slot+2].tag.kernel = ta;
    Check_Gc
    return s;
}

/*
 * Create the attribute for the suspend extension.
 * The first a difference list, the others are normal lists.
 */
static pword *
_suspension_attribute(pword *susp, int position)
{
    register pword	*t, *d, *s;
    register int	i;
    register int	arity = DidArity(d_.suspend_attr);

    if (position > arity) {
	position = 1;
    }

    t = TG;
    Push_Struct_Frame(d_.suspend_attr);
    d = TG;
    Push_Struct_Frame(d_.minus);
    s = TG;
    Push_List_Frame();

    s->val.ptr = susp;		/* list element */
    s->tag.kernel = TSUSP;
    Make_Struct(t+1, d);
    if (position == 1)
    {
	Make_List(d+1,s);	/* singleton dlist */
	Make_Ref(d+2,s+1);
	Make_Var(s+1);

	for(i=2; i<=arity; i++)
	{
	    Make_Nil(t+i);
	}
    }
    else
    {
	Make_Var(d+1);		/* empty dlist */
	Make_Ref(d+2,d+1);

	for(i=2; i<=arity; i++)
	{
	    if (i == position) {
		Make_List(t+i,s);
		Make_Nil(s+1);
	    } else {
		Make_Nil(t+i);
	    }
	}
    }
    return t;
}

int
insert_suspension(pword *var,
	int position,		/* must be > 0 */
	pword *susp,		/* must be dereferenced */
	int slot)
{
    register pword *s, *t;
    int			i;

    if (IsMeta(var->tag)) {		/* already a metaterm */

	t = MetaTerm(var)->val.ptr + slot;	/* find the dlist to insert */
	Dereference_(t);
	if (IsRef(t->tag)) {
	    if (slot != DELAY_SLOT)
		return ATTR_FORMAT;
	    s = _suspension_attribute(susp, position);
	    if (!s)
		return RANGE_ERROR;
	    Bind_Var(t->val, t->tag, s, TCOMP);
	    return PSUCCEED;
	} else if (!IsStructure(t->tag))
	    return ATTR_FORMAT;
	t = t->val.ptr;
	if ((DidArity(t->val.did)) < position) {
	    if (slot != DELAY_SLOT)
		return RANGE_ERROR;
	    position = 1;		/* force to the 1st list */
	}
	
	return ec_enter_suspension(t+position, susp);
    }
    else if (IsRef(var->tag)) {
	if (slot != DELAY_SLOT)
	    return ATTR_FORMAT;
	t = _suspension_attribute(susp, position);
	if (!t)
	    return RANGE_ERROR;
	s = add_attribute(var->tag.kernel, t, (long) TCOMP, slot);
	Bind_Var(var->val, var->tag, s, TREF);
    }
    Check_Gc;
    return PSUCCEED;
}

int
ec_enter_suspension(pword *t, pword *susp)
{
    register pword *s, *head;
    pword	   *dlp;

    dlp = t;
    Dereference_(t);
    s = TG;
    TG += 2;			/* make a list cell */
    s[0].val.ptr = susp;
    s[0].tag.kernel = TSUSP;
    if IsRef(t->tag) {		/* first insert */
	s[1].tag.kernel = TNIL;
	Bind_Var(t->val, t->tag, &s[0], TLIST);
    } else {
	if (IsStructure(t->tag)) {		/* it already exists */
	    t = t->val.ptr;
	    if (t->val.did != d_.minus)		/* check the functor */
		return ATTR_FORMAT;
	    head = ++t;
	    Dereference_(head);
	} else if (IsList(t->tag) || IsNil(t->tag)) {
	    /* not a difference list */
	    head = t;
	    t = dlp;
	} else
	    return ATTR_FORMAT;

	/*
	 * dlp is the (undereferenced) difference list pointer (if any)
	 * t is the (undereferenced) list pointer
	 * head is the (dereferenced) list pointer
	 */

	/*
	 * Incomplete garbage collection: Get rid of woken
	 * suspensions at the beginning of the list.
	 */
	while (IsList(head->tag))
	{
	    register pword *psusp = head->val.ptr;
	    Dereference_(psusp);
	    if (!IsTag(psusp->tag.kernel, TSUSP))
		return ATTR_FORMAT;
	    if (!SuspDead(psusp->val.ptr))
		break;
	    head = head->val.ptr + 1;
	    Dereference_(head);
	}

	/* head now points to the rest of the old suspension list */
 
	if (IsList(head->tag) || IsNil(head->tag)) {
	    s[1] = *head;
	    /* t may be TREF, TLIST or TNIL */
	    if (t < GB || !ISPointer(t->tag.kernel) || t->val.ptr < GB)
	    {
		Trail_Pword(t);
	    }
	    t->tag.kernel = TLIST;
	    t->val.ptr = s;
	} else if (!IsRef(head->tag))
	    return ATTR_FORMAT;
	else {				/* empty dlist, replace it */
	    value v;
	    s[1].val.ptr = &s[1];
	    s[1].tag.kernel = TREF;
	    TG += 3;
	    s[2].val.did = d_.minus;	/* new difference list header */
	    s[2].tag.kernel = TDICT;
	    s[3].val.ptr = s;
	    s[3].tag.kernel = TLIST;
	    s[4].val.ptr = &s[1];
	    s[4].tag.kernel = TREF;
	    v.ptr = &s[2];
	    (void) ec_assign(dlp, v, tcomp);
	}
    }
    Check_Gc;
    return PSUCCEED;
}

int
notify_constrained(pword *pvar)
{
    pword	*p;

    if (!IsMeta(pvar->tag)) {
	Succeed_
    }
    p = MetaTerm(pvar->val.ptr);
    p = p->val.ptr + DELAY_SLOT;
    Dereference_(p);
    if (!IsStructure(p->tag)) {
	Succeed_
    }
    return ec_schedule_susps(p->val.ptr + CONSTRAINED_OFF);
}

/*
 * Pick up the first woken goal with priority higher than prio,
 * remove it from its list and set WP to the priority
 */
pword *
first_woken(register int prio)
{
    register int	i;
    register pword	*p = WL;
    register pword	*s;
    register pword	*t;
    register pword	*u;

    if (p == (pword *) 0)
	return 0;
    if (prio > WLMaxPrio(p))
	prio = WLMaxPrio(p) + 1;
    p = WLFirst(p) - 1;
    for (i = 1; i < prio; i++) {
	t = ++p;		/* no references allowed */
	if (IsList(t->tag)) {
	    for (;;) {
		t = t->val.ptr;
		s = t++;
		Dereference_(s);
		Dereference_(t);
		if (IsSusp(s->tag)) {
		    u = s->val.ptr;
		    if (!SuspDead(u))
			break;
		} else
		    p_fprintf(current_err_, "*** woken list %d is corrupted\n", i);
		if (IsNil(t->tag)) {
		    s = 0;
		    break;
		}
	    }
	    /* replace the list head */
	    if (p->val.ptr < GB) {
		Trail_Pword(p);
	    }
	    if (IsList(t->tag))
		p->val.ptr = t->val.ptr;
	    else
	    {
		/* Use a timestamp (which happens to look like a [])
		 * to terminate the list */
		Make_Stamp(p);
	    }
	    if (s) {
		Set_WP(i)
		return s;
	    }
	}
    }
    return 0;
}

/*
 * Initialize the WL structure
 */
void
wl_init(int prio)
{
    register pword	*p = TG;
    int	i;

    if (prio < DEFAULT_PRIO)
	prio = DEFAULT_PRIO;
    i = WLArity(prio);
    TG += i + 1;		/* + functor */
    Check_Gc
    p->val.did = in_dict("woken", i);
    p->tag.kernel = TDICT;
    *WLPrevious(p) = TAGGED_WL;
    WLPreviousWP(p)->val.nint = WP;
    WLPreviousWP(p)->tag.kernel = TINT;
    for (; i >= WL_FIRST; i--)
	p[i].tag.kernel = TNIL;
    Make_Struct(&TAGGED_WL, p);
}

/*
 * binding routine for non-standard variables
 *
 * receives:
 * 	pw1	a non-standard variable
 *		(ie. IsRef(pw1) && !IsVar(pw1))
 *	pw2	a general term, but not a (standard) free variable
 *		(ie. !IsVar(pw2))
 *
 * binds the non-standard variable pw1 to the term referenced by pw2
 */

bind_c(register pword *pw1, register pword *pw2, register pword **list)
{
    switch(TagType(pw1 -> tag))
    {
    case TNAME:			/* a named variable */
	pw1 = pw1->val.ptr;
	switch(TagType(pw2->tag))
	{
	case TNAME:
	    pw2 = pw2->val.ptr;
	    if (pw1 < pw2)
	    {
		Bind_Named(pw2, pw1);
	    }
	    else if (pw1 > pw2)
	    {
		Bind_Named(pw1, pw2);
	    }
	    break;

	case TMETA:
	    pw2 = pw2->val.ptr;
	    if (pw2 > pw1) /* we bind the "wrong" direction, copy the name */
	    {
		Trail_Tag_If_Needed_Gb(pw2)
		pw2->tag.kernel = TagNameField(pw1->tag.kernel) | RefTag(TMETA);
	    }
	    Bind_Named(pw1, pw2);
	    break;

	case TUNIV:
	    pw2 = pw2->val.ptr;
	    Bind_Named(pw1, pw2);
	    break;

	default:
	    Trail_Tag_If_Needed_Gb(pw1);
	    *pw1 = *pw2;
	}
	return PSUCCEED;

    case TMETA:
    {
	pw1 = pw1->val.ptr;
	switch(TagType(pw2->tag))
	{
	case TNAME:
	    pw2 = pw2->val.ptr;
	    if (pw1 > pw2) /* we bind the "wrong" direction, copy the name */
	    {
		Trail_Tag_If_Needed_Gb(pw1)
		pw1->tag.kernel = TagNameField(pw2->tag.kernel) | RefTag(TMETA);
	    }
	    Bind_Named(pw2, pw1);
	    return PSUCCEED;

	case TUNIV:
	    return PFAIL;

	case TMETA:
	    pw2 = pw2->val.ptr;
	    if (pw1 > pw2)
	    {
		Trail_Tag_If_Needed_Gb(pw1)
		pw1->tag.kernel = TREF;
		pw1->val.all = pw2->val.all;
	    }
	    else if (pw1 < pw2)
	    {
		Trail_Tag_If_Needed_Gb(pw2)
		pw2->tag.kernel = TREF;
		pw2->val.all = pw1->val.all;
		pw1 = pw2;
	    }
	    else
		return PSUCCEED;
	    break;

	default:
	    Trail_Tag_If_Needed_Gb(pw1)
	    *pw1 = *pw2;
	}

	pw2 = TG;
	TG += 2;
	Check_Gc;
	pw2[0].val.ptr = pw1;
	pw2[0].tag.kernel = TLIST;
	if (*list) {
	    pw2[1].val.ptr = *list;
	    pw2[1].tag.kernel = TLIST;
	} else {
	    pw2[1].tag.kernel = TNIL;
	    if (list == &MU) {
		Fake_Overflow;
	    }
	}
	*list = pw2;
	return PSUCCEED;
    }

    case TUNIV:
	/* TUNIV variables are all-quantified variables,
	 * so any attempt to constrain them must fail! */
	switch(TagType(pw2->tag))
	{
	case TNAME:
	    pw1 = pw1->val.ptr;
	    pw2 = pw2->val.ptr;
	    Bind_Named(pw2, pw1);
	    return PSUCCEED;
	case TUNIV:
	    if (pw1->val.ptr == pw2->val.ptr)
		return PSUCCEED;
	    /* else */
	default:
	    return PFAIL;
	}

/*
 * EXTENSION SLOT HERE
 */

    default: 
	p_fprintf(current_err_, "bind_c(): unknown tag (%x) encountered\n", 
		pw1->tag.kernel);
	return (PFAIL);
    }
}


/*
 * Instantiate a metaterm without triggering meta_unification events
 */

int
meta_bind(pword *pvar, value v, type t)
{
    if (IsVar(t) && v.ptr >= TG)	/* local -> meta */
    {
	Trail_If_Needed_Eb(v.ptr)
	v.ptr->val.ptr = pvar;
    }
    else				/* bind the metaterm pvar */
    {
	Trail_Tag_If_Needed_Gb(pvar)
	pvar->tag.all = t.all;
	pvar->val.all = v.all;
    }
    Succeed_;
}


/*
 * ec_assign() - destructive assignment to a pword in the global stack
 *
 * Used to implement setarg/3 and the like.
 * It is not allowed to assign to a variable, in order to reduce the
 * confusing side effects caused by this facility [check has been removed].
 * Originally, we had the additional restriction that also the new value
 * of the pword should not be a variable to avoid multiple references
 * to the modified location. However, this proved to be too restrictive
 * for the applications, e.g. in difference lists.
 *
 * This solution should be optimal. Some thoughts about this problem:
 * To optimize space reuse and trailing, we need to know the age of
 * a binding. A binding is always younger than the bound location and
 * also younger than the binding value.
 * If the old binding was already done in the current choicepoint
 * segment (NewValue), we do not have to trail the update.
 * When the value we bind to is in the current choicepoint segment, we
 * can use it as the indicator of the binding age. If it is older, or
 * if we bind to a constant (which has no age), we create an intermediate
 * cell on top of the stack, so that we can later use its address to
 * determine the binding age. 
 */

int				/* returns PSUCCEED */
ec_assign(
    	register pword *argpw,	/* location to be modified */
	value v, type t)	/* the new value and tag */
{
#ifdef PRINTAM
    if (!(TG_ORIG <= argpw && argpw < TG) &&
    	!((void_ptr)&ec_.m <= (void_ptr)argpw &&
	  (void_ptr)argpw < (void_ptr)&ec_.m + sizeof(struct machine)))
    {
	pword *argpw1 = argpw;
	p_fprintf(current_output_,"INTERNAL ERROR: ec_assign of heap term: ");
	Dereference_(argpw1)
	writeq_term(argpw1->val.all, argpw1->tag.all);
	ec_newline(current_output_);
    }
#endif
    if (IsVar(t) && v.ptr > TG)	/* globalize local variables */
    {
	register pword *new = TG++;
	Check_Gc;
	new->val.ptr = new;
	new->tag.kernel = TREF;
	Trail_If_Needed(v.ptr)
	v.ptr = v.ptr->val.ptr = new;
    }

    if (!NewLocation(argpw))		/* not completely deterministic */
    {
	if (!NewValue(v, t))		/* binding age will not be implicit */
	{
	    register pword *new = TG++; /* create an intermediate cell */
	    Check_Gc;
	    new->val.all = v.all;
	    new->tag.all = t.all;
	    v.ptr = new;
	    t.kernel = TREF;
	}
	if (!NewValue(argpw->val, argpw->tag))
	{
					/* old binding wasn't in this sgmt */
	    Trail_Pword(argpw);		/* don't "optimize" this (bug #609) */
	}
    }
    argpw->tag.all = t.all;
    argpw->val.all = v.all;
    Succeed_;
}


/*
 * pword *ec_nonground(val,tag)
 *
 * Check if a term is nonground. Returns a pointer to the first
 * variable encountered, otherwise NULL.
 */

pword *
ec_nonground(value val, type tag)	/* expects a dereferenced argument */
{
    register int arity;
    register pword *arg_i;

    for (;;)
    {
	if (IsRef(tag))
	    return val.ptr;
	else if (IsList(tag))
	    arity = 2;
	else if (IsStructure(tag))
	{
	    arity = DidArity(val.ptr->val.did);
	    val.ptr++;
	}
	else
	    return (pword *) 0;
 
	for(;arity > 1; arity--)
	{
	    register pword *pvar;
	    arg_i = val.ptr++;
	    Dereference_(arg_i);
	    if (pvar = ec_nonground(arg_i->val,arg_i->tag))
		return pvar;
	}
	arg_i = val.ptr;		/* tail recursion */
	Dereference_(arg_i);
	val.all = arg_i->val.all;
	tag.all = arg_i->tag.all;
    }
}

/*---------------------------------------------
 * Cut across PB
 *---------------------------------------------*/

#ifdef PB_MAINTAINED

int
cut_across_pb(old_b)
pword *old_b;		/* old_b < PB */
{
    do
    {
	PB = BPar(PB)->ppb;
    } while (old_b < PB);
    if (old_b < PPB) {
	PPB = PB;
	do
	    PPB = BPar(PPB)->ppb;
	while (old_b < PPB);
	return cut_public();
    }
    return 1;
}

#endif

/*---------------------------------------------
 * Trailing/Untrailing
 *---------------------------------------------*/

/*
 * This function extends the Untrail_Variables() macro.
 * It is called when the trail is neither address nor tag nor value trail.
 *
 * Untrail the extended trail frame that trail_ptr points to.
 * The frame must be popped by the caller !
 *
 * This function (when called during failure) relies on TG/GB having
 * their pre-failure values!
 */

void
untrail_ext(pword **trail_ptr, int undo_context)
{
    switch(TrailedEtype(*trail_ptr))
    {

    case TRAIL_UNDO:
	/* call undo function */
	(* (void(*)(pword*,word*,int,int)) (trail_ptr[TRAIL_UNDO_FUNCT])) (
		trail_ptr[TRAIL_UNDO_ADDRESS],
		(word*) (trail_ptr + TRAIL_UNDO_SIMPLE_HEADER_SIZE),
		TrailedEsize(trail_ptr[TRAIL_UNDO_FLAGS]) - TRAIL_UNDO_SIMPLE_HEADER_SIZE,
		undo_context
	    );
	break;

    case TRAIL_UNDO_STAMPED:
	/*
	 * first reset timestamp
	 * this is not done in gc because the stamp location may already be
	 * marked. The only consequence of this is that the stamp keeps
	 * an extra witness alive.
	 */
	if (undo_context == UNDO_FAIL)
	{
	    trail_ptr[TRAIL_UNDO_STAMP_ADDRESS]->val.ptr = trail_ptr[TRAIL_UNDO_OLDSTAMP];

	    /* do nothing if the trail is redundant according to timestamp */
	    if (!OldStamp(trail_ptr[TRAIL_UNDO_STAMP_ADDRESS]))
		return;
	}
	/* then call undo function */
	(* (void(*)(pword*,word*,int,int)) (trail_ptr[TRAIL_UNDO_FUNCT])) (
		trail_ptr[TRAIL_UNDO_ADDRESS],
		(word*) (trail_ptr + TRAIL_UNDO_STAMPED_HEADER_SIZE),
		TrailedEsize(trail_ptr[TRAIL_UNDO_FLAGS]) - TRAIL_UNDO_STAMPED_HEADER_SIZE,
		undo_context
	    );
	break;

/* EXTENSION SLOT HERE */

    }
}


/*
 * _untrail_cut_action()
 * called only by untrail_ext() during untrailing
 */
static void
_untrail_cut_action(pword *action_frame)
{
    if (action_frame == LCA)
    {
	do_cut_action();
    }
    /* else the action has already been executed by a cut */
}
    

/*
 * do_cut_action() is called at cut time or during untrailing
 * The LCA register is a pointer to a cut action frame with the format:
 *
 *	TDICT	arg/3				don't care functor
 *	TCOMP	<ptr to next (older) action>	chain of cut actions
 *	TINT	<address of C action function>
 *	TAG	VAL				argument for the action
 */
void
do_cut_action(void)
{
    /* call the action function */
    (* (void(*)(value,type)) (LCA[2].val.ptr)) (LCA[3].val, LCA[3].tag);

    /* advance the LCA register */
    if (IsStructure(LCA[1].tag))
	LCA = LCA[1].val.ptr;
    else
	LCA = (pword *) 0;
}


/*
 * schedule_cut_fail_action(function, v, t)
 *
 * create a cut-action frame on the global stack and a corresponding
 * undo-frame on the trail.
 * The cut-action frame is linked into the global list of cut-action frames,
 * starting with the LCA register.
 */
void
schedule_cut_fail_action(
	void	(*function)(value, type),
	value	v,
	type	t)
{
    pword *action_frame = TG;

    TG += 4;
    Check_Gc;
    action_frame[0].val.did = d_.arg;	/* just any arity 3 functor ... */
    action_frame[0].tag.kernel = TDICT;
    action_frame[1].val.ptr = LCA;
    if (LCA)
	action_frame[1].tag.kernel = TCOMP;
    else
	action_frame[1].tag.kernel = TNIL;
    action_frame[2].val.ptr = (pword *) function;
    action_frame[2].tag.kernel = TINT;
    action_frame[3].val.all = v.all;
    action_frame[3].tag.all = t.all;

    Trail_Undo(action_frame, _untrail_cut_action);
    LCA = action_frame;
}

/*
 * C function interfaces for use in extensions
 */

void trail_undo(pword *pw, void (*function) (pword *))
{
    Trail_Undo(pw, function);
}


/*
 * The function to create an (optionally time-stamped) undo trail:
 * 
 * void ec_trail_undo(
 *	function,	address of untrail function
 *	pitem,		address of related item, or NULL
 *			(pointer to pword on heap, or anything elsewhere)
 *	pstamp,		address of time stamp (we only trail if it is old)
 *			or NULL for non-timestamped trail
 *	pdata,		pointer to untrail data or NULL
 *	data_size,	size of untrail data in words (0..2^23)
 *	data_type	TRAILED_PWORD or TRAILED_WORD32
 *   )
 *
 * The untrail function will later be called as follows:
 *
 * void undo(
 *	pitem,		address of related item
 *	pdata,		pointer to untrail data
 *	data_size,	size of untrail data in words
 *	undo_context	UNDO_FAIL or UNDO_GC
 * )
 */

void
ec_trail_undo(
	void	(*function)(pword*,word*,int,int),
	pword	*pitem,
	pword	*pstamp,
	word	*pdata,
	int	data_size,
	int	data_type)
{
    int i;
    uword *traildata = (uword *)TT - data_size;

    /* Disable_Exit macro guards against interruption by an 
     * asynchronous abort leaving a partially complete trail 
     * entry on the top of the stack
     */

    if (pstamp)
    {
	if (!OldStamp(pstamp))	/* trail redundant? */
	    return;

	Disable_Exit();

	TT = (pword **) (traildata - TRAIL_UNDO_STAMPED_HEADER_SIZE);
	Check_Trail_Ov
	TT[TRAIL_UNDO_FLAGS] = (pword *)
    		( TrailedEsizeField(TRAIL_UNDO_STAMPED_HEADER_SIZE + data_size)
		| TrailedEtypeField(TRAIL_UNDO_STAMPED)
		| TRAIL_EXT | (data_type & TRAILED_TYPE_MASK));
	TT[TRAIL_UNDO_STAMP_ADDRESS] = pstamp;
	TT[TRAIL_UNDO_OLDSTAMP] = ISPointer(pstamp->tag.kernel) ? pstamp->val.ptr : 0;
	Make_Stamp(pstamp);
    }
    else
    {
	Disable_Exit();

	TT = (pword **) (traildata - TRAIL_UNDO_SIMPLE_HEADER_SIZE);
	Check_Trail_Ov
	TT[TRAIL_UNDO_FLAGS] = (pword *)
    		( TrailedEsizeField(TRAIL_UNDO_SIMPLE_HEADER_SIZE + data_size)
		| TrailedEtypeField(TRAIL_UNDO)
		| TRAIL_EXT | (data_type & TRAILED_TYPE_MASK));
    }

    TT[TRAIL_UNDO_ADDRESS] = pitem;
    *((void (**)(pword*,word*,int,int)) (TT+TRAIL_UNDO_FUNCT)) = function;

    for(i=0; i<data_size; ++i)
    {
	traildata[i] = ((uword *) pdata)[i];
    }

    Enable_Exit();
}


/*
 * trail the n_pwords pwords starting at pw + offset_pwords
 */
void ec_trail_pwords(pword *pw, int offset_pwords, int n_pwords)
{
    Trail_Pwords(pw, offset_pwords, n_pwords);
}


void disable_exit(void)
{
    Disable_Exit();
}

void enable_exit(void)
{
    Enable_Exit();
}

#define GlobalRef(ref)		((ref) < TG && (ref) >= TG_ORIG)
#define LocalRef(ref)		((ref) < SP_ORIG && (ref) >= SP)
#define TrailRef(ref)		((pword**)(ref) < TT_ORIG && (pword**)(ref) >= TT)
#define MachineRef(ref)		((word*)(&ec_) <= (word*)(ref) && (word*)(ref) < (word*)(&ec_ + 1))

/*
 * This function checks very thoroughly that the pointer is a valid local
 * or global reference.
 */
check_pword(pword *ref)
{
    int		arity;

    if (!(GlobalRef(ref) || LocalRef(ref)
    	|| TrailRef(ref) || address_in_heap(&global_heap, ref)
	|| MachineRef(ref)))
	return 0;
    /* Now we can test the contents */
    switch (TagType(ref->tag))
    {
    case TLIST:
	if (!(GlobalRef(ref->val.ptr) || address_in_heap(&global_heap, ref->val.ptr)))
	    return 0;
	return check_pword(ref->val.ptr) && check_pword(ref->val.ptr+1);

    case TCOMP:
	ref = ref->val.ptr;
	if (!(GlobalRef(ref) || address_in_heap(&global_heap, ref->val.ptr)))
	    return 0;
	if (bitfield_did((long) DidBitField(ref->val.did)) != ref->val.did)
	    return 0;
	arity = DidArity(ref->val.did);
	for (ref++; arity; arity--, ref++)
	    if (!check_pword(ref))
		return 0;
	return 1;

    case TSTRG:
    case TBIG:
#ifndef UNBOXED_DOUBLES
    case TDBL:
#endif
    case TIVL:
	if (!(GlobalRef(ref->val.ptr) || address_in_heap(&global_heap, ref->val.ptr)))
	    return 0;
	return TagType(ref->val.ptr->tag) == TBUFFER;

    case TRAT:
	if (!(GlobalRef(ref->val.ptr) || address_in_heap(&global_heap, ref->val.ptr)))
	    return 0;
	return TagType(ref->val.ptr->tag) == TBIG;

    case TSUSP:
	ref = ref->val.ptr;
	if (!GlobalRef(ref))
	    return 0;
	return TagType(ref->tag) == TDE &&
		(ref->val.ptr == 0 || GlobalRef(ref->val.ptr));

    case TNIL:
    case TINT:
#ifdef UNBOXED_DOUBLES
    case TDBL:
#endif
	return 1;

    case TDICT:
	return bitfield_did((long) DidBitField(ref->val.did)) == ref->val.did;

    case TVAR_TAG:
	if (ref->val.ptr != ref)
	    return check_pword(ref->val.ptr);
	return 1;

    case TNAME:
	if (ref->val.ptr != ref)
	    return check_pword(ref->val.ptr);
	return (IsNamed(ref->tag.kernel) &&
	    address_in_heap(&global_heap, (pword *) TagDid(ref->tag.kernel)));

    case TMETA:
	if (ref->val.ptr != ref)
	    return check_pword(ref->val.ptr);
	return check_pword(ref->val.ptr + 1);

    default:
	return 0;
    }
}

#ifdef PRINTAM
/*---------------------------------------
 * Debugging support
 *---------------------------------------*/

check_arg(pword *pw)
{
    switch (TagType(pw->tag))
    {
    case TCOMP:
	if (SameTypeC(pw->val.ptr->tag, TDICT))
	    return;
	break;
    case TLIST:
	return;
    case TSUSP:
	if (pw->val.ptr < TG && pw->val.ptr >= TG_ORIG)
	    return;
	break;
    case THANDLE:
	if (pw->val.ptr < TG && pw->val.ptr >= TG_ORIG
	 && SameTypeC(pw->val.ptr[0].tag, TEXTERN)
	 && SameTypeC(pw->val.ptr[1].tag, TPTR))
	    return;
	break;
    case TIVL:
    case TBIG:
    case TSTRG:
#ifndef UNBOXED_DOUBLES
    case TDBL:
#endif
	if (SameTypeC(pw->val.ptr->tag, TBUFFER))
	    return;
	break;
    case TRAT:
	if (SameTypeC(pw->val.ptr[0].tag, TBIG) &&
	    SameTypeC(pw->val.ptr[1].tag, TBIG))
	    return;
	break;
    case TNIL:
    case TINT:
    case TDICT:
#ifdef UNBOXED_DOUBLES
    case TDBL:
#endif
	return;
    case TVAR_TAG:
	return;
    case TNAME:
    case TMETA:
    case TUNIV:
	if (pw->val.ptr < TG && pw->val.ptr >= TG_ORIG)
	    return;
	break;
    }
    p_fprintf(current_err_,
	"INTERNAL ERROR: illegal pword encountered: val=%x tag=%x\n",
	pw->val.all, pw->tag.all);
    ec_flush(current_err_);
}


#define InGlobal(p)  ((p) >= min && (p) < max)
#define InHeap(p)  (address_in_heap(&global_heap, (generic_ptr) p))

check_global(void)
{
    check_global1(TG_ORIG, TG);
}

check_global2(pword *max)
{
    check_global1(TG_ORIG, max);
}

check_global1(register pword *min, register pword *max)
{
    register pword *pw = min;
    extern pword    woken_susp_;

    if (g_emu_.nesting_level > 1)
	return;

    while (pw < max)
    {
	switch (TagType(pw->tag))
	{
	case TVAR_TAG:
	case TNAME:
	case TMETA:
	case TUNIV:
	    if (!IsRef(pw->tag))
		goto _problem_;
	    if (!InGlobal(pw->val.ptr))
		goto _problem_;
	    pw++;
	    break;

	case TCOMP:
	    /*
	    if (pw->val.ptr && !InGlobal(pw->val.ptr) && !IsPersistent(pw->tag))
		goto _problem_;
	    */
	    if (pw->val.ptr &&
	    	(!IsAtom(pw->val.ptr->tag) || DidArity(pw->val.ptr->val.did) == 0))
		goto _problem_;
	    pw++;
	    break;

	case TSTRG:
	case TBIG:
#ifndef UNBOXED_DOUBLES
	case TDBL:
#endif
	case TIVL:
	    /*
	    if (!InGlobal(pw->val.ptr) && !IsPersistent(pw->tag)) goto _problem_;
	    */
	    if (DifferTypeC(pw->val.ptr->tag,TBUFFER)) goto _problem_;
	    pw++;
	    break;

	case TRAT:
	    if (!InGlobal(pw->val.ptr) && !IsPersistent(pw->tag)) goto _problem_;
	    if (DifferTypeC(pw->val.ptr[0].tag, TBIG) ||
		DifferTypeC(pw->val.ptr[1].tag, TBIG)) goto _problem_;
	    pw++;
	    break;

	case TSUSP:
	    if (!InGlobal(pw->val.ptr) && pw->val.ptr != &woken_susp_) goto _problem_;
	    if (DifferTypeC(pw->val.ptr->tag,TDE)) goto _problem_;
	    pw++;
	    break;

	case TLIST:
	    if (!InGlobal(pw->val.ptr) && !IsPersistent(pw->tag)) goto _problem_;
	    pw++;
	    break;

	case THANDLE:
	    if (!InGlobal(pw->val.ptr)) goto _problem_;
	    if (DifferTypeC(pw->val.ptr[0].tag, TEXTERN) ||
		DifferTypeC(pw->val.ptr[1].tag, TPTR)) goto _problem_;
	    pw++;
	    break;

	case TNIL:
	case TINT:
	case TDICT:
#ifdef UNBOXED_DOUBLES
	case TDBL:
#endif
	    pw++;
	    break;

	case TBUFFER:
	    pw += BufferPwords(pw);
	    break;

	case TEXTERN:
	    pw += 2;
	    break;

	case TDE:
	    pw += SUSP_SIZE - 2;
	    break;

	default:
	    goto _problem_;
	}
    }
    return;
_problem_:
    p_fprintf(current_err_,
	"INTERNAL ERROR: illegal pword encountered at 0x%x: val=0x%x tag=0x%x\n",
	pw, pw->val.all, pw->tag.all);
    ec_flush(current_err_);
    return;
}

find_in_trail(pword *addr)
{
    pword **tr = TT;
    pword *trailed_item;
    long	i;

    while(tr < TT_ORIG)
    {
	switch((((long) *tr) & 3))
	{
	case TRAIL_ADDRESS:
	    trailed_item = *tr++;
	    break;
	case TRAIL_TAG:
	    trailed_item = *(tr+1);
	    break;
	case TRAIL_MULT:
	    i = (long) *tr;
	    trailed_item = (pword *)((uword *)(*(tr+1)) + TrailedOffset(i));
	    break;
	case TRAIL_EXT:
	    break;
	}
	if (trailed_item == addr)
	{
	    p_fprintf(current_err_,
		"Trail entry found for 0x%x at 0x%x\n", trailed_item, tr);
	    ec_flush(current_err_);
	}
	End_Of_Frame(tr, tr);
    }
}


check_trail(void)
{
    extern vmcode par_fail_code_[];
    control_ptr fp;
    pword **tt = TT;
    pword *tg = TG;
    int print = 0;

    for(fp.args = B.args;;fp.args = BPrev(fp.args))
    {
	if (BPrev(fp.args) == (pword *) (fp.top - 1))
	{
	    /* small if-then-else choicepoint */
	}
	else
	{
	    check_trail2(print, tt, BChp(fp.args)->tt, tg);
	    tt = BChp(fp.args)->tt;
	    tg = BChp(fp.args)->tg;
	    break;
	}

	if (IsInterruptFrame(BTop(fp.args)) || IsRecursionFrame(BTop(fp.args)))
	    break;
    }
    if (print) p_fprintf(current_err_, "BOTTOM\n");
    if (print) ec_flush(current_err_);
}

check_trail1(int print)
{
    check_trail2(print, TT, TT_ORIG, TG);
}

check_trail2(int print, pword **ttptr, pword **ttend, pword *min_tg_when_failing)
{
    long ctr;
    pword *pw;
    while(ttptr < ttend) {
	if (print) p_fprintf(current_err_, "TT=0x%08x: ", ttptr);
	switch((((long) *ttptr) & 3)) {
	case TRAIL_ADDRESS:
	    pw = *ttptr++;
	    if (print) p_fprintf(current_err_, "ADDRESS 0x%08x\n", pw);
	    if (min_tg_when_failing <= pw && pw < (pword*)TT)
		emu_break();
	    break;
	case TRAIL_TAG:
	    pw = *(ttptr+1);
	    if (print) p_fprintf(current_err_, "TAG     0x%08x 0x%08x\n", pw, TrailedTag(*ttptr));
	    if (min_tg_when_failing <= pw && pw < (pword*)TT)
		emu_break();
	    ttptr += 2;
	    break;
	case TRAIL_MULT:
	    ctr = (long) *ttptr++;
	    pw = *ttptr++;
	    ctr = TrailedNumber(ctr);
	    if (print) p_fprintf(current_err_, "MULT    0x%08x %d\n", pw, ctr);
	    if (min_tg_when_failing <= pw && pw < (pword*)TT)
		emu_break();
#if 0
	    if (!check_pword(pw) && !(
		    pw == &POSTED_LAST
		||
		    IsTag(pw->tag.kernel, TDE)
	    	))
		emu_break();
#endif
	    do {
		ttptr++;
	    } while (ctr--);
	    break;
	case TRAIL_EXT:
	    switch(TrailedEtype(*ttptr)) {
	    case TRAIL_UNDO:
		if (print) p_fprintf(current_err_, "UNDO    0x%08x\n", ttptr[TRAIL_UNDO_ADDRESS]);
		break;
	    case TRAIL_UNDO_STAMPED:
		if (print) p_fprintf(current_err_, "UNDO_ST 0x%08x\n", ttptr[TRAIL_UNDO_ADDRESS]);
#if 0
		if (ttptr[TRAIL_UNDO_OLDSTAMP] >= min_tg_when_failing)
		{
		    p_fprintf(current_err_, "UNDO_ST redundant 0x%08x\n", ttptr[TRAIL_UNDO_OLDSTAMP]);
		    ec_flush(current_err_);
		}
#endif
		if (TrailedType(*ttptr) == TRAILED_PWORD)
		{
		    long n_pwords = (TrailedEsize(*ttptr) - TRAIL_UNDO_STAMPED_HEADER_SIZE)/2;
		    pw = (pword *) (ttptr + TRAIL_UNDO_STAMPED_HEADER_SIZE);
		    for(; n_pwords > 0; --n_pwords, ++pw)
		    {
			if (ISPointer(pw->tag.kernel))
			{
			    if (min_tg_when_failing <= pw->val.ptr && pw->val.ptr < (pword*)TT)
				emu_break();
			    if (IsString(pw->tag) && !IsTag(pw->val.ptr->tag.kernel, TBUFFER))
				emu_break();
			}
		    }
		}
		break;
	    }
	    ttptr += TrailedEsize(*ttptr);
	    break;
	}
    }
    if (print) p_fprintf(current_err_, "TT=0x%08x: STOP\n", ttptr);
    if (print) ec_flush(current_err_);
}

#endif /* PRINTAM */
