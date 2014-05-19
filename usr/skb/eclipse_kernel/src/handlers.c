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
 * VERSION	$Id: handlers.c,v 1.2 2008/07/08 22:24:13 jschimpf Exp $
 */

/*
 *
 * Builtins to set up and manipulate handler tables
 *
 */
#include <assert.h>

#include "config.h"

#include <errno.h>
#include <signal.h>

#ifdef HAVE_STRING_H
#include <string.h>
#else
extern char	*strcpy();
#endif

#include <stdio.h>	/* for sprintf() */
#include <stdlib.h>	/* for exit() */

#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "error.h"
#include "dict.h"
#include "emu_export.h"
#include "io.h"
#include "module.h"
#include "property.h"
#include "os_support.h"


#define MAX_HANDLER_ARITY 4

/*
 * Define sig_action_t in a portable way
 */

#ifdef HAVE_SIGACTION
typedef struct sigaction sig_action_t;
#else
#  ifdef HAVE_SIGVEC	/* first try VEC because ACTION does not match STACK */
typedef struct sigvec sig_action_t;
#define sa_handler sv_handler
#define sa_mask sv_mask
#define sa_flags sv_flags
#  else
typedef struct {
	RETSIGTYPE (*sa_handler)(int);
	int sa_mask;
	int sa_flags;
} sig_action_t;
#  endif
#endif


/* SA_INTERRUPT is System V (pre-R4)
 * SVR4 has SA_RESTART instead, meaning the opposite
 */
# ifndef SA_INTERRUPT
#  define SA_INTERRUPT 0
# endif


/*
 * Signal blocking and unblocking
 * We maintain a mask sig_block_mask_ to block all interrupts
 * whose handler might enqueue signals or post events - this is
 * to implement exclusive access to these queue data structures.
 */

#ifdef HAVE_SIGPROCMASK

#define Empty_Sig_Mask(Mask) (void) sigemptyset(&(Mask));
#define Save_Sig_Mask(Mask) \
	(void) sigprocmask(SIG_SETMASK, (sigset_t *) 0, &(Mask));
#define Restore_Sig_Mask(Mask) \
	(void) sigprocmask(SIG_SETMASK, &(Mask), (sigset_t *) 0);
#define Block_Signal(i) { \
	sigset_t mask; \
	sigemptyset(&mask); \
	sigaddset(&mask, i); \
	sigprocmask(SIG_BLOCK, &mask, (sigset_t *) 0); }
#define Unblock_Signal(i) { \
	sigset_t mask; \
	sigemptyset(&mask); \
	sigaddset(&mask, i); \
	sigprocmask(SIG_UNBLOCK, &mask, (sigset_t *) 0); }

#define Init_Block_Mask() Empty_Sig_Mask(sig_block_mask_)
#define	Add_To_Block_Mask(i) sigaddset(&sig_block_mask_, i);
#define	Del_From_Block_Mask(i) sigdelset(&sig_block_mask_, i);
#define Block_Signals(OldMask) \
	(void) sigprocmask(SIG_BLOCK, &sig_block_mask_, &(OldMask));

#else
#ifdef HAVE_SIGVEC

typedef int sigset_t;

#ifndef sigmask
# define sigmask(n)      (1 << ((n) - 1))
#endif

#define Empty_Sig_Mask(Mask) (Mask) = 0;
#define Save_Sig_Mask(Mask) (Mask) = sigblock(0);
#define Restore_Sig_Mask(Mask) (void) sigsetmask(Mask);
#define Block_Signal(i) \
	(void) sigblock(sigmask(i));
#define Unblock_Signal(i) \
	(void) sigsetmask(sigblock(0) & ~sigmask(i));

#define Init_Block_Mask() Empty_Sig_Mask(sig_block_mask_)
#define	Add_To_Block_Mask(i) sig_block_mask_ |= sigmask(i);
#define	Del_From_Block_Mask(i) sig_block_mask_ &= ~sigmask(i);
#define Block_Signals(OldMask) OldMask = sigblock(sig_block_mask_);

#else

typedef int sigset_t;
#define Empty_Sig_Mask(Mask) (Mask) = 0;
#define Save_Sig_Mask(Mask)
#define Restore_Sig_Mask(Mask)
#define Block_Signal(i)
#define Unblock_Signal(i)

#define Init_Block_Mask() Empty_Sig_Mask(sig_block_mask_)
#define	Add_To_Block_Mask(i)
#define	Del_From_Block_Mask(i)
#define Block_Signals(OldMask)

#endif
#endif

#define Unblock_Signals() Restore_Sig_Mask(initial_sig_mask_)

static sigset_t initial_sig_mask_;	/* normal execution sigmask */
static sigset_t	sig_block_mask_;	/* what to block on handler invoc */

static int	spurious = 0;	/* counts nesting of fatal signals */


/*
 * The signal that flags C stack overflow must be executed on a different
 * stack, else the handler can't be called. This stack must be big enough
 * to call an emulator and execute the handler (which should call reset/0).
 */

#ifdef HAVE_SIGALTSTACK
static char		signal_stack[SIGSTKSZ];
static stack_t		sigstack_descr;
#else
# ifdef HAVE_SIGSTACK
# ifndef SV_ONSTACK
#  define SV_ONSTACK 1
# endif
# define SIGSTACK_SIZE	4096
static char		signal_stack[SIGSTACK_SIZE];
static struct sigstack	sigstack_descr;
# endif
#endif


#ifdef SIGBUS
#define IsSIGBUS(n) ((n) == SIGBUS)
#else
#define IsSIGBUS(n) 0
#endif
#ifdef SIGQUIT
#define IsSIGQUIT(n) ((n) == SIGQUIT)
#else
#define IsSIGQUIT(n) 0
#endif

#define FatalSignal(n) \
	((n)==SIGILL || (n)==SIGSEGV || IsSIGBUS(n) || IsSIGQUIT(n))


/*
 * EXTERN declarations
 */

extern pri	*true_proc_,
		*fail_proc_;

extern void		halt_session(void);


/*
 * GLOBAL variable definitions
 */

/* Error handlers */
pri	**error_handler_;
pri	**default_error_handler_;

/*
 * Interrupt handlers
 * There is a flag array and a handler array.
 * The latter is only valid if the flag is IH_HANDLE_ASYNC.
 */
int	*interrupt_handler_flags_ = 0;
pri	**interrupt_handler_ = 0;
dident	*interrupt_name_ = 0;
int	ec_sigalrm;	/* normally SIGALRM, but also defined on Windows */
int	ec_sigio;	/* normally SIGIO, but also defined on Windows */

static dident	d_event_, d_throw_, d_internal_, d_defers_;
static type	kernel_tag_;
static int	user_error = USER_ERROR;


int	p_reset(void);

static int
	p_pause(void),
	p_get_error_handler(value vn, type tn, value vf, type tf, value va, type ta, value vm, type tm),
        p_get_event_handler(value vn, type tn, value vf, type tf, value va, type ta, value vm, type tm),
	p_define_error(value valm, type tagm, value vale, type tage),
	p_get_interrupt_handler_nr(value vn, type tn, value vf, type tf, value va, type ta, value vm, type tm),
	p_interrupt_id_det(value vnum, type tnum, value vname, type tname),
	p_post_events(value v, type t),
	p_set_error_handler(value vn, type tn, value vp, type tp, value vm, type tm),
	p_reset_error_handler(value vn, type tn),
	p_set_default_error_handler(value vn, type tn, value vp, type tp, value vm, type tm),
	p_set_interrupt_handler_nr(value vn, type tn, value vp, type tp, value vm, type tm),
	p_valid_error(value vn, type tn);
static int
	_set_error_array(pri **arr, long int n, dident w, value vm, type tm);

static RETSIGTYPE
	_abort_handler(int),
	_throw_handler(int);

#ifdef SA_SIGINFO
static RETSIGTYPE _break(int, siginfo_t*, void *);
#else
static RETSIGTYPE _break(int);
#endif

/* Profiling handler is defined in emu.c */
#if defined(__GNUC__) && defined(HAVE_UCONTEXTGREGS)
extern RETSIGTYPE sigprof_handler(int, siginfo_t*, void *);
#else
extern RETSIGTYPE sigprof_handler(int);
#endif

#define Check_Error_Number(v,t)				\
	Check_Integer(t)				\
	if ( (v).nint < 1				\
		|| (v).nint >= MAX_ERRORS		\
		|| !ErrorMessage[(v).nint] )		\
	    { Bip_Error(RANGE_ERROR) }

#define Check_Interrupt_Number(v,t)			\
    Check_Integer(t)					\
    if((v).nint <= 0 || (v).nint >= NSIG)		\
	{ Bip_Error(RANGE_ERROR) }


/*----------------------------------------------------------------------
 * Signal handling
 *
 * In a standalone eclipse, we try to catch all signals
 * and handle them in Prolog, if possible.
 *
 * When interrupts (except fatal ones) are disabled by Disable_Int,
 * we delay them by putting them into the delayed_interrupt_ queue
 * and reconsider them in delayed_break() which is invoked by Enable_Int.
 *
 * Signals that are required to be handled "synchronously" (ie. handler
 * was set to event/1, indicated by the IH_POST_EVENT flag)
 * are done by (eventually) posting an event with the signal's Prolog name,
 * e.g. 'alrm' for SIGALRM.
 *
 * Asynchronous signal handling is normally done via a recursive engine
 * by calling it_emulc(). However, this is not possible while the
 * emulator registers are not exported (EXPORTED flag). In that case,
 * we post the signal number (integer) as an event, and the handler
 * will then be executed by the original engine. The DEL_IRQ_POSTED
 * flag is set to indicate that there is an async signal in the queue.
 *----------------------------------------------------------------------*/

#define NBDELAY	32

static int _enqueue_irq(int n), _dequeue_irq(void);
static int delayed_interrupt_[NBDELAY];
static int first_delayed_ = 0, next_delayed_ = 0;

static sigset_t mask_before_blocking_;

void
block_signals(void)
{
    Block_Signals(mask_before_blocking_);
}

void
unblock_signals(void)
{
    Restore_Sig_Mask(mask_before_blocking_);
}


static void
_handle_fatal(int n)
{
    if (spurious > 1)
    {
	exit(-1);		/* we keep everything blocked */
    }
    else if (spurious++ == 1)
    {
	value v1;
	v1.nint = -1;
	(void) p_exit(v1, tint);
    }
    first_delayed_ = next_delayed_ = 0;
    /*
     * In the development system
     * signals will be unblocked upon reinit (handlers_init())
     */
    ec_panic("Fatal signal caught",
	(VM_FLAGS & EXPORTED)? "protected code": "emulator");
}


static void
_handle_async(int n)		 /* may be 0 */
{
    /* not InterruptsDisabled! */

#if defined(SIGIO) || defined(SIGPOLL)
#if !defined(SIGIO)
    if (n == SIGPOLL)
#else
#if !defined(SIGPOLL)
    if (n == SIGIO)
#else
    if (n == SIGIO || n == SIGPOLL)
#endif
#endif
    {
	msg_trigger();
	if (!(interrupt_handler_flags_
	     && (interrupt_handler_flags_[n] == IH_HANDLE_ASYNC
	      || interrupt_handler_flags_[n] == IH_POST_EVENT)))
	{
	    return;
	}
    } 
#endif

    if (FatalSignal(n))
    {
	if (spurious > 0		/* already tried unsuccessfully */
	    || !(VM_FLAGS & EXPORTED))	/* can't get a recursive engine */
	{
	    _handle_fatal(n);
	}
	spurious = 1;			/* first attempt to handle */
	/* invoke Prolog handler below */
    }
    else if (interrupt_handler_flags_[n] == IH_HANDLE_ASYNC)
    {
	if (!(VM_FLAGS & EXPORTED))	/* can't get a recursive engine */
	{
	    if (ec_post_event_int(n) != PSUCCEED)
		(void) write(2,"\nEvent queue overflow - signal lost\n",36);
	    return;
	}
	/* else invoke Prolog handler below */
	
    }
    else if (interrupt_handler_flags_[n] == IH_POST_EVENT)
    {
	if (ec_post_event_int(n) != PSUCCEED)
	    (void) write(2,"\nEvent queue overflow - signal lost\n",36);
	return;
    }
    else
    {
	(void) write(2,"\nUnexpected interrupt type in handle_async()\n",45);
	return;
    }

    /*
     * Invoke the Prolog handler for signal n
     */
    {
	int i;
	value v1, v2;
	v1.nint = n;
	i = it_emulc(v1, tint);
	if (FatalSignal(n))
	    spurious--;
	if (i == PTHROW)
	{
	    if (!ec_options.parallel_worker || g_emu_.nesting_level > 1)
		longjmp(*g_emu_.it_buf, PTHROW);
	    else if (!IsRecursionFrame(BTop(B.args)))
	    {
		pword event;
		Make_Atom(&event, d_.abort);
		(void) ec_post_event(event);
	    }
	    /* else ignore the THROW, we are in an idle worker */
	}
    }
}


#ifdef SA_SIGINFO
static RETSIGTYPE
_break(int n, siginfo_t *si, void *dummy)
#else
static RETSIGTYPE
_break(int n)
#endif
{
    /* signal n should be blocked on handler invocation */

#if !defined(HAVE_SIGACTION) && !defined(HAVE_SIGVEC)
    signal(n,_break);	/* restore signal catcher to _break	*/
#endif

#ifdef SA_SIGINFO
    if (FatalSignal(n) && si) {
	char buf[128];
	sprintf(buf, "Fatal signal (signal=%d, si_code=%d, si_addr=%08x)\n",
			n, si->si_code, si->si_addr);
	write(2, buf, strlen(buf));
    }
#endif

    if (InterruptsDisabled)
    {
	if (FatalSignal(n))
	{
	    _handle_fatal(n);
	}
	else				/* not fatal, delay it */
	{
	    (void) _enqueue_irq(n);	/* incl. Set_Interrupts_Pending */
	}
	return;
    }

    Unblock_Signal(n);
    _handle_async(n);
}


void
delayed_break(void)
{
    int n;
    int saved_errno = errno;	/* to avoid unexpected side effects */

    while (InterruptsPending && (n = _dequeue_irq()) != -1)
    {
	_handle_async(n);
    }
    errno = saved_errno;
}


/*
 * This is called from asynchronous points in the emulator to allow
 * handling urgent events. 
 */
void
ec_handle_async(void)		/* !InterruptsDisabled && EXPORTED */
{
    int n;
    while ((n = next_urgent_event()) != -1)
    {
	_handle_async(n);
    }
}


static int
_dequeue_irq(void)
{
    int n;
    sigset_t saved_mask;

    Block_Signals(saved_mask);
    if (first_delayed_ != next_delayed_)	/* empty? */
    {
	n = delayed_interrupt_[first_delayed_];	/* no: get one	*/
	first_delayed_ = (first_delayed_ + 1) % NBDELAY;
	if (first_delayed_ == next_delayed_)	/* last one? */
	{
	    Clr_Interrupts_Pending();
	}
    }
    else
    {
	n = -1;
	Clr_Interrupts_Pending();
    }
    Restore_Sig_Mask(saved_mask);
    return n;
}

static int
_enqueue_irq(int n)		/* must not be interrupted */
{
    int i = (next_delayed_ + 1) % NBDELAY;

    if (i == first_delayed_)	/* queue full ? */
    {
	(void) write(2,"\nInterrupt queue overflow - signal lost\n",40);
	/* Some machines cannot continue after a signal, they would
	   loop infinitely with queue overflow */
	_handle_fatal(n);
	/*NOTREACHED*/
    }
    delayed_interrupt_[next_delayed_] = n;
    next_delayed_ = i;		/* enter in queue */

    Set_Interrupts_Pending();
    return 0;
}


/* 
 * Handler used for message passing
 * This handler can be used for SIGIO/SIGPOLL when these signals are
 * not needed otherwise. It is not used anymore because we need to
 * handle SIGIO synchronously in order to implement socket events.
 */

RETSIGTYPE
sigmsg_handler(int n)
{
    if (InterruptsDisabled )
    {
	(void) _enqueue_irq(n);
    }
    else
    {
	Unblock_Signal(n);
	msg_trigger();
	return;
    }
}


static RETSIGTYPE
_throw_handler(int i)
{
    pword exit_tag;
#if !defined(HAVE_SIGACTION) && !defined(HAVE_SIGVEC)
    (void) signal(i, _throw_handler);	/* e.g. Windows... */
#endif
    Make_Atom(&exit_tag, interrupt_name_[i]);
    Unblock_Signal(i);	/* won't otherwise be restored because of longjmp */
    (void) longjmp_throw(exit_tag.val, exit_tag.tag);
}


static RETSIGTYPE
_abort_handler(int i)
{
    pword exit_tag;
#if !defined(HAVE_SIGACTION) && !defined(HAVE_SIGVEC)
    (void) signal(i, _abort_handler);	/* e.g. Windows... */
#endif
    Make_Atom(&exit_tag, d_.abort);
    Unblock_Signal(i);	/* won't otherwise be restored because of longjmp */
    (void) longjmp_throw(exit_tag.val, exit_tag.tag);
}


int
_install_int_handler(int i, int how, pri *proc)
{
    int res;
    sig_action_t action;

#ifndef SIGIO
    if (i == ec_sigio)
    {
	Succeed_;	/* this is a fake signal number, do nothing */
    }
#endif
#ifndef SIGALRM
    if (i == ec_sigalrm)
    {
	Succeed_;	/* this is a fake signal number, do nothing */
    }
#endif

    Empty_Sig_Mask(action.sa_mask);
    action.sa_flags = SA_INTERRUPT;

    switch(how)
    {
    case IH_HANDLE_ASYNC:
#if !defined(HAVE_SIGACTION) && !defined(HAVE_SIGVEC)
	Bip_Error(UNIMPLEMENTED);	/* e.g. Windows... */
#else
	if (i == SIGSEGV)
	{
	    /* try to run the SIGSEGV handler on its own stack */
#ifdef HAVE_SIGALTSTACK
	    sigstack_descr.ss_sp = signal_stack;
	    sigstack_descr.ss_size = SIGSTKSZ;
	    sigstack_descr.ss_flags = 0;
	    (void) sigaltstack(&sigstack_descr, (stack_t *) 0);
	    /* We may need SA_SIGINFO for more sophisticated SEGV handling */
	    action.sa_flags = SA_ONSTACK | SA_INTERRUPT;
#else
#  ifdef HAVE_SIGSTACK
	    sigstack_descr.ss_sp = signal_stack + SIGSTACK_SIZE;
	    sigstack_descr.ss_onstack = 0;
	    (void) sigstack(&sigstack_descr, (struct sigstack*)0);
	    action.sa_flags = SA_ONSTACK;
#  endif
#endif
	}
#ifdef SA_SIGINFO
	if (FatalSignal(i)) {
	    action.sa_flags |= SA_SIGINFO;
	}
#endif
	Add_To_Block_Mask(i);
#ifdef SA_SIGINFO
	action.sa_sigaction = _break;
#else
	action.sa_handler = _break;
#endif
#endif
	break;

    case IH_UNCHANGED:
	/* We can't change back from something else to this one */
	Succeed_;

    case IH_SYSTEM_DFL:
	Del_From_Block_Mask(i);
	action.sa_handler = SIG_DFL;
	break;

    case IH_IGNORE:
	Del_From_Block_Mask(i);
	action.sa_handler = (void*)SIG_IGN;
	break;

    case IH_POST_EVENT:
	Add_To_Block_Mask(i);
#ifdef SA_SIGINFO
	action.sa_flags |= SA_SIGINFO;
	action.sa_sigaction = _break;
#else
	action.sa_handler = _break;
#endif
	break;

    case IH_THROW:
	Del_From_Block_Mask(i);
	action.sa_handler = _throw_handler;
	break;

    case IH_ABORT:
	Del_From_Block_Mask(i);
	action.sa_handler = _abort_handler;
	break;

    case IH_HALT:
	Add_To_Block_Mask(i);
	action.sa_handler = (RETSIGTYPE(*)(int)) halt_session;
	break;

    case IH_ECLIPSE_DFL:
	/*
	 * This sets handlers that are needed to implement internal
	 * Eclipse functionality like timers, profiler etc
	 */
	switch(i)
	{
#ifdef SIGPROF
	case SIGPROF:
	    Del_From_Block_Mask(i);
#if defined(__GNUC__) && defined(HAVE_UCONTEXTGREGS)
	    action.sa_flags |= SA_SIGINFO;
	    action.sa_sigaction = sigprof_handler;
#else
	    action.sa_handler = sigprof_handler;
#endif
	    break;
#endif
#if defined(SIGIO)
	case SIGIO:
	    Add_To_Block_Mask(i);
	    action.sa_handler = sigmsg_handler;
	    break;
#endif
#if defined(SIGPOLL) && (!defined(SIGIO) || SIGPOLL != SIGIO)
	case SIGPOLL:
	    Add_To_Block_Mask(i);
	    action.sa_handler = sigmsg_handler;
	    break;
#endif
	default:
	    Del_From_Block_Mask(i);
	    Succeed_;
	}
	break;
    }

#ifdef HAVE_SIGACTION
    res = sigaction(i, &action, (struct sigaction *) 0);
#else
#ifdef HAVE_SIGVEC
    res = sigvec(i, &action, (struct sigvec *) 0);
#else
    res = (int) signal(i, action.sa_handler);
#endif
#endif

    if (res == -1  &&  action.sa_handler != (void*)SIG_IGN)
    {
	Set_Errno;
	Bip_Error(SYS_ERROR);
    }
    errno = 0;	/* something couldn't be ignored, silently accept */
    Succeed_;
}


/*
 * Reset all signal handlers that are set to Eclipse-specific
 * handling, because we are about to shut down Eclipse.
 * Additionally, ignore SIGPIPE, because it might be raised
 * during cleanup of the Eclipse streams.
 */

void
handlers_fini()
{
//asq:
    return;
/*
    int i;

    for(i = 1; i < NSIG; i++)
    {
	if (InterruptName[i] != D_UNKNOWN)
	{
	    switch (interrupt_handler_flags_[i])
	    {
	    case IH_ECLIPSE_DFL:
	    case IH_POST_EVENT:
	    case IH_THROW:
	    case IH_ABORT:
	    case IH_HANDLE_ASYNC:
		(void) _install_int_handler(i, IH_SYSTEM_DFL, 0);
		break;

	    case IH_UNCHANGED:
	    case IH_SYSTEM_DFL:
	    case IH_IGNORE:
	    default:
		break;
	    }
	}
    }
#ifdef SIGPIPE
    (void) _install_int_handler(SIGPIPE, IH_IGNORE, 0);
#endif
*/
}

static int
p_interrupt_id_det(value vnum, type tnum, value vname, type tname)
{
    if (IsInteger(tnum))
    {
	dident int_did;
	if (vnum.nint <= 0 || vnum.nint >= NSIG) {
	    Fail_;
	}
	if ((int_did = interrupt_name_[vnum.nint]) != D_UNKNOWN)
	{
	    Return_Unify_Atom(vname, tname, int_did);
	} else {
	    Return_Unify_Atom(vname, tname, d_.eocl);
	}
    }
    else if (IsAtom(tname))
    {
	int i;
	for (i = 1; i < NSIG; i++)
	{
	    if (interrupt_name_[i] == vname.did)
	    {
		Return_Unify_Integer(vnum, tnum, i);
	    }
	}
    }
    Fail_;
}


/*
 *		define_error(+Message, -ErrorNumber)
 *
 */

static int
p_define_error(value valm, type tagm, value vale, type tage)
{
	int m;

	Check_String(tagm);
	Check_Ref(tage);

	m = user_error++;
	if(m >=  MAX_ERRORS)
	{
	    Bip_Error(RANGE_ERROR);
	}
	ErrorMessage[m] = (char *) hg_alloc((int)StringLength(valm)+1);
	(void) strcpy(ErrorMessage[m], StringStart(valm));
	error_handler_[m] = qualified_procedure(d_.error_handler,
		d_.kernel_sepia, d_.kernel_sepia, kernel_tag_);
	Return_Unify_Integer(vale, tage, m);
}

/*
 * The handler array entries are considered qualified references
 * from sepia_kernel. If no exported handler exists, we create one.
 */
static pri *
_kernel_ref_export_proc(dident pdid, dident mod, type mod_tag)
{
    pri *pd = visible_procedure(pdid, mod, mod_tag, 0);
    if (!pd  ||  PriScope(pd) == LOCAL)
    {
	int err;
	Get_Bip_Error(err);	/* reset error code from visible_procedure() */
	pd = export_procedure(pdid, mod, mod_tag);
	if (!pd)
	    return 0;
    }
    return qualified_procedure(pdid, PriHomeModule(pd),
    				d_.kernel_sepia, kernel_tag_);
}

/*
 * setting a handler for an existing error code
 * p_set_error_handler(vn,tn,vp,tp)	FUNCTION
 * (vn,tn) defines the error code
 * (vp,tp) defines a handler
 */

/*ARGSUSED*/
static int
p_set_error_handler(value vn, type tn, value vp, type tp, value vm, type tm)
{
    dident	pdid;
    int		err, defers = 0;

    Error_If_Ref(tn);
    Check_Module(tm, vm);
    if (IsStructure(tp)  &&  vp.ptr->val.did == d_defers_)
    {
	++vp.ptr;
	Dereference_(vp.ptr);
	tp.all = vp.ptr->tag.all;
	vp.all = vp.ptr->val.all;
	defers = 1;
    }
    Get_Proc_Did(vp, tp, pdid);

    if (IsNumber(tn))
    {
	if (defers)
	    { Bip_Error(UNIMPLEMENTED); }
	Check_Error_Number(vn, tn)
	return _set_error_array(error_handler_, vn.nint, pdid, vm, tm);
    }
    else if (IsAtom(tn))
    {
	pri *proc;
	pword *prop;

	if (DidArity(pdid) > MAX_HANDLER_ARITY)
	{
	    Bip_Error(RANGE_ERROR)
	}
	proc = _kernel_ref_export_proc(pdid, vm.did, tm);
	if (!proc)
	{
	    Get_Bip_Error(err);
	    Bip_Error(err);
	}

	a_mutex_lock(&PropertyLock);
	prop = get_property(vn.did, EVENT_PROP);
	if (!prop)
	    prop = set_property(vn.did, EVENT_PROP);
	prop->tag.kernel = TPROC | (defers? EVENT_DEFERS: 0);
	prop->val.ptr = (pword *) proc;
	a_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }
}

/* post events from a list into the event queue */
static int
p_post_events(value v, type t)
{
    if (IsList(t))
    {
	pword *cdr = v.ptr;
	for(;;)
	{
	    int res;
	    pword *car = cdr++;
	    Dereference_(car);
	    if (IsInteger(car->tag)) {
		Bip_Error(TYPE_ERROR);
	    }
	    /* Integers aren't allowed, let ec_post_event type check rest */
	    res = ec_post_event(*car);
	    if (res != PSUCCEED)
	    {
		Bip_Error(res);
	    }
	    Dereference_(cdr);
	    if (IsRef(cdr->tag))
	    {
		Bip_Error(INSTANTIATION_FAULT);
	    }
	    else if (IsNil(cdr->tag))
		break;
	    else if (IsList(cdr->tag))
		cdr = cdr->val.ptr;
	    else
	    {
		Bip_Error(TYPE_ERROR);
	    }
	}
	Succeed_;
    }
    Check_Nil(t);
}

static int
p_set_default_error_handler(value vn, type tn, value vp, type tp, value vm, type tm)
{
    dident	pdid;
    Check_Error_Number(vn, tn)
    Check_Module(tm, vm);
    Get_Proc_Did(vp, tp, pdid);
    return _set_error_array(default_error_handler_, vn.nint, pdid, vm, tm);
}

/*ARGSUSED*/
static int
_set_error_array(pri **arr, long int n, dident w, value vm, type tm)
{
    pri		*proc;
    int		err;

    if(DidArity(w) > MAX_HANDLER_ARITY && (n < -(DEBUG_CALL_EVENT) || n > -(DEBUG_REDO_EVENT)))
    {
        Bip_Error(RANGE_ERROR)
    }
    if(w == d_.true0)
    {
	arr[n] = true_proc_;
	Succeed_;
    } else if(w == d_.fail) {
	arr[n] = fail_proc_;
	Succeed_;
    } /* else */
    proc = _kernel_ref_export_proc(w, vm.did, tm);
    if(!proc)
    {
	Get_Bip_Error(err);
	Bip_Error(err);
    }
    /* disallow tools here */
    arr[n] = proc;
    Succeed_;
}

static int
p_reset_error_handler(value vn, type tn)
{
    Error_If_Ref(tn);
    if (IsInteger(tn))
    {
	Check_Error_Number(vn,tn)
	error_handler_[vn.nint] = default_error_handler_[vn.nint];
	Succeed_;
    }
    else if IsAtom(tn)
    {
	int err = erase_property(vn.did, EVENT_PROP);
	if (err < 0)
	{
	    Bip_Error(err);
	}
	Succeed_;
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }
}


static int
p_set_interrupt_handler_nr(value vn, type tn, value vp, type tp, value vm, type tm)
{
    dident w;
    pri *proc = 0;
    int err, how;

    Check_Module(tm, vm);
    Check_Interrupt_Number(vn,tn)
    Get_Proc_Did(vp, tp, w);
    if(DidArity(w) > 1)
    {
        Bip_Error(RANGE_ERROR)
    }
    if (w == d_.default0)
	how = IH_SYSTEM_DFL;
    else if (w == d_internal_)
	how = IH_ECLIPSE_DFL;
    else if (w == d_.true0)
	how = IH_IGNORE;
    else if (w == d_event_)
	how = IH_POST_EVENT;
    else if (w == d_throw_)
	how = IH_THROW;
    else if (w == d_.abort)
	how = IH_ABORT;
    else if (w == d_.halt)
	how = IH_HALT;
    else
    {
	how = IH_HANDLE_ASYNC;
	proc = _kernel_ref_export_proc(w, vm.did, tm);
	if(!proc)
	{
	    Get_Bip_Error(err);
	    Bip_Error(err);
	}
    }
    err = _install_int_handler((int) vn.nint, how, proc);
    if (err != PSUCCEED)
    {
	Bip_Error(err);
    }
    interrupt_handler_flags_[vn.nint] = how;
    interrupt_handler_[vn.nint] = proc;
    Succeed_;
}


static p_pause(void)
{
#ifdef SIGSTOP
    reset_ttys_and_buffers();
    (void) kill(0, SIGSTOP);
    Succeed_;
#else
#ifdef SIGSUSP
    reset_ttys_and_buffers();
    (void) kill(0, SIGSUSP);
    Succeed_;
#else
    Bip_Error(NOT_AVAILABLE);
#endif
#endif
}


/*ARGSUSED*/
static int
p_get_interrupt_handler_nr(value vn, type tn, value vf, type tf, value va, type ta, value vm, type tm)
{
    dident	wdid, module;
    pri		*proc;
    Prepare_Requests;

    switch(interrupt_handler_flags_[vn.nint])
    {
    case IH_UNCHANGED:
	Fail_;
    case IH_SYSTEM_DFL:
	wdid = d_.default0;
	module = d_.kernel_sepia;
	break;
    case IH_ECLIPSE_DFL:
	wdid = d_internal_;
	module = d_.kernel_sepia;
	break;
    case IH_IGNORE:
	wdid = d_.true0;
	module = d_.kernel_sepia;
	break;
    case IH_POST_EVENT:
	wdid = d_event_;
	module = d_.kernel_sepia;
	break;
    case IH_THROW:
	wdid = d_throw_;
	module = d_.kernel_sepia;
	break;
    case IH_ABORT:
	wdid = d_.abort;
	module = d_.kernel_sepia;
	break;
    case IH_HALT:
	wdid = d_.halt;
	module = d_.kernel_sepia;
	break;
    case IH_HANDLE_ASYNC:
	proc = interrupt_handler_[vn.nint];
	wdid = PriDid(proc);
	module = PriHomeModule(proc);
	break;
    default:
	Bip_Error(RANGE_ERROR);
    }
    Request_Unify_Atom(vf, tf, add_dict(wdid, 0));
    Request_Unify_Integer(va, ta, DidArity(wdid));
    Request_Unify_Atom(vm, tm, module);
    Return_Unify;
}


int
p_reset(void)
{

    (void) ec_outfs(current_err_, "Aborting execution....\n");
    ec_flush(current_err_);
    ec_panic("reset/0 called",0);
}


/* The following builtins use the global error variable ! */

#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)


/*ARGSUSED*/
static int
p_get_error_handler(value vn, type tn, value vf, type tf, value va, type ta, value vm, type tm)
{
    dident wdid;
    pri *proc;
    Prepare_Requests;

    Check_Error_Number(vn,tn);
    Check_Output_Integer(ta);
    Check_Output_Atom_Or_Nil(vf, tf);
    Check_Output_Atom_Or_Nil(vm, tm);
    proc = error_handler_[vn.nint];
    if(proc == (pri *) 0)
	proc = error_handler_[0];
    wdid = PriDid(proc);
    Request_Unify_Atom(vf, tf, add_dict(wdid, 0));
    Request_Unify_Integer(va, ta, DidArity(wdid));
    Request_Unify_Atom(vm, tm, PriHomeModule(proc));
    Return_Unify;
}

static int
p_get_event_handler(value vn, type tn, value vf, type tf, value va, type ta, value vm, type tm)
{
    dident wdid;
    pri *proc;
    pword *prop;
    Prepare_Requests;

    Error_If_Ref(tn);
    Check_Output_Integer(ta);
    Check_Output_Atom_Or_Nil(vf, tf);
    Check_Output_Atom_Or_Nil(vm, tm);
    if (IsAtom(tn))
    {
      a_mutex_lock(&PropertyLock);
      prop = get_property(vn.did, EVENT_PROP);
      a_mutex_unlock(&PropertyLock);
      if (!prop) Fail_;
      proc = (pri *) prop->val.ptr;
    } 
    else if (IsInteger(tn)) 
    {
      Check_Error_Number(vn,tn);
      proc = error_handler_[vn.nint];
      if(proc == (pri *) 0)
	proc = error_handler_[0];
    }
    else
    {
      Bip_Error(TYPE_ERROR)
    }
    wdid = PriDid(proc);
    Request_Unify_Atom(vf, tf, add_dict(wdid, 0));
    Request_Unify_Integer(va, ta, DidArity(wdid));
    Request_Unify_Atom(vm, tm, PriHomeModule(proc));
    Return_Unify;
}

static int
p_valid_error(value vn, type tn)
{
    Check_Error_Number(vn,tn);
    Succeed_;
}

/* undo redefiinition of Bip_Error() */
#undef Bip_Error
#define Bip_Error(N) return(N);


void
handlers_init(int flags)
{
    register int i;

    first_delayed_ = next_delayed_ = 0;

    d_event_ = in_dict("event",1);
    d_throw_ = in_dict("throw",1);
    d_defers_ = in_dict("defers",1);
    d_internal_ = in_dict("internal",0);
    kernel_tag_.kernel = ModuleTag(d_.kernel_sepia);

    if (flags & INIT_SHARED)
    {
	ErrorHandler =
	    (pri **) hg_alloc(MAX_ERRORS * sizeof(pri *));
	DefaultErrorHandler =
	    (pri **) hg_alloc(MAX_ERRORS * sizeof(pri *));
	DefaultErrorHandler[0] = ErrorHandler[0] =
	    DidPtr(in_dict("boot_error", 2))->procedure;
	for(i = 1; i < MAX_ERRORS; i++)
	{
	    ErrorHandler[i] = (pri *) 0;
	    DefaultErrorHandler[i] = (pri *) 0;
	}

	InterruptHandler =
	    (pri **) hg_alloc(NSIG * sizeof(pri *));
	InterruptHandlerFlags =
	    (int *) hg_alloc(NSIG * sizeof(int));
	InterruptName =
	    (dident *) hg_alloc(NSIG * sizeof(dident));

	for(i = 0; i < NSIG; i++)
	{
	    InterruptHandler[i] = (pri *) 0;
	    InterruptHandlerFlags[i] = IH_UNCHANGED;
	    InterruptName[i] = D_UNKNOWN;
	}

	/*
	 * Assign the prolog names to the signals
	 */

	/* 0 is a pseudo-signal used for parallel abort */
	InterruptHandlerFlags[0] = IH_POST_EVENT;

#ifdef SIGHUP
	InterruptName[SIGHUP] = in_dict("hup", 0);
#endif
	InterruptName[SIGINT] = in_dict("int", 0);
#ifdef SIGQUIT
	InterruptName[SIGQUIT] = in_dict("quit", 0);
#endif
	InterruptName[SIGILL] = in_dict("ill", 0);
#ifdef SIGTRAP
	InterruptName[SIGTRAP] = in_dict("trap", 0);
#endif
	InterruptName[SIGABRT] = in_dict("abrt", 0);
#ifdef SIGEMT
	InterruptName[SIGEMT] = in_dict("emt", 0);
#endif
	InterruptName[SIGFPE] = in_dict("fpe", 0);
#ifdef SIGKILL
	InterruptName[SIGKILL] = in_dict("kill", 0);
#endif
#ifdef SIGBUS
	InterruptName[SIGBUS] = in_dict("bus", 0);
#endif
	InterruptName[SIGSEGV] = in_dict("segv", 0);
#ifdef SIGSYS
	InterruptName[SIGSYS] = in_dict("sys", 0);
#endif
#ifdef SIGPIPE
	InterruptName[SIGPIPE] = in_dict("pipe", 0);
#endif
#ifdef SIGALRM
	ec_sigalrm = SIGALRM;
	InterruptName[SIGALRM] = in_dict("alrm", 0);
#else
	ec_sigalrm = 0;	/* will be properly assigned below */
#endif
	InterruptName[SIGTERM] = in_dict("term", 0);
#ifdef SIGUSR1
	InterruptName[SIGUSR1] = in_dict("usr1", 0);
#endif
#ifdef SIGUSR2
	InterruptName[SIGUSR2] = in_dict("usr2", 0);
#endif
#ifdef SIGCHLD
	InterruptName[SIGCHLD] = in_dict("chld", 0);
#endif
#ifdef SIGCLD
	InterruptName[SIGCLD] = in_dict("chld", 0);	/* old name for CHLD */
#endif
#ifdef SIGWINCH
	InterruptName[SIGWINCH] = in_dict("winch", 0);
#endif
#ifdef SIGURG
	InterruptName[SIGURG] = in_dict("urg", 0);
#endif
#ifdef SIGSUSP
	InterruptName[SIGSUSP] = in_dict("susp", 0);
#endif
#ifdef SIGSTOP
	InterruptName[SIGSTOP] = in_dict("stop", 0);
#endif
#ifdef SIGTSTP
	InterruptName[SIGTSTP] = in_dict("tstp", 0);
#endif
#ifdef SIGCONT
	InterruptName[SIGCONT] = in_dict("cont", 0);
#endif
#ifdef SIGTTIN
	InterruptName[SIGTTIN] = in_dict("ttin", 0);
#endif
#ifdef SIGTTOU
	InterruptName[SIGTTOU] = in_dict("ttou", 0);
#endif
#ifdef SIGVTALRM
	InterruptName[SIGVTALRM] = in_dict("vtalrm", 0);
#endif
#ifdef SIGPROF
	InterruptName[SIGPROF] = in_dict("prof", 0);
#endif
#ifdef SIGXCPU
	InterruptName[SIGXCPU] = in_dict("xcpu", 0);
#endif
#ifdef SIGXFSZ
	InterruptName[SIGXFSZ] = in_dict("xfsz", 0);
#endif
#ifdef SIGPWR
	InterruptName[SIGPWR] = in_dict("pwr", 0);
#endif
#ifdef SIGIOT
	InterruptName[SIGIOT] = in_dict("iot", 0);
#endif
#ifdef SIGWAITING
	InterruptName[SIGWAITING] = in_dict("waiting", 0);
#endif
#ifdef SIGLWP
	InterruptName[SIGLWP] = in_dict("lwp", 0);
#endif
#ifdef SIGPOLL
	InterruptName[SIGPOLL] = in_dict("poll", 0);
#endif
#ifdef SIGIO
	ec_sigio = SIGIO;
	InterruptName[SIGIO] = in_dict("io", 0);	/* after POLL */
#else
	ec_sigio = 0;		/* will be properly assigned below */
#endif
#ifdef SIGLOST
	InterruptName[SIGLOST] = in_dict("lost", 0);
#endif
#ifdef SIGQUIT
	InterruptName[SIGQUIT] = in_dict("quit", 0);
#endif
#ifdef SIGPHONE
	InterruptName[SIGPHONE] = in_dict("phone", 0);
#endif

	/*
	 * If we didn't have SIGALRM defined, find a free number and fake it
	 * (use 14 of possible). We use it for our timer implementation.
	 */
#ifndef SIGALRM
	for(i = 14; i < NSIG; i++)
	{
	    if (InterruptName[i] == D_UNKNOWN)
	    {
		ec_sigalrm = i;
		InterruptName[i] = in_dict("alrm", 0);
		InterruptHandlerFlags[i] = IH_POST_EVENT;
		break;
	    }
	}
	if (!ec_sigalrm)
	    ec_panic("Couldn't find a pseudo-signal number for SIGALRM", "handlers_init()");
#endif
#ifndef SIGIO
	for(i = 1; i < NSIG; i++)
	{
	    if (InterruptName[i] == D_UNKNOWN)
	    {
		ec_sigio = i;
		InterruptName[i] = in_dict("io", 0);
		InterruptHandlerFlags[i] = IH_POST_EVENT;
		break;
	    }
	}
	if (!ec_sigio)
	    ec_panic("Couldn't find a pseudo-signal number for SIGIO", "handlers_init()");
#endif
    }
    if (flags & INIT_PRIVATE)	/* handler arrays already exist */
    {
	/* get a private copy of the array pointers */
	error_handler_ = ErrorHandler;
	default_error_handler_ = DefaultErrorHandler;
	interrupt_handler_ = InterruptHandler;
	interrupt_handler_flags_ = InterruptHandlerFlags;
	interrupt_name_ = InterruptName;
    }
    /*
     * event builtins
     */
    if (flags & INIT_SHARED)
    {
	(void) local_built_in(in_dict("post_events",1),
				p_post_events,			B_SAFE);
	(void) built_in(in_dict("pause",0),	p_pause,	B_SAFE);
	(void) built_in(in_dict("define_error", 2),
				p_define_error,		B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("reset_error_handler", 1),
				p_reset_error_handler,		B_SAFE);
	(void) built_in(in_dict("reset_event_handler", 1),
				p_reset_error_handler,		B_SAFE);

	(void) local_built_in(d_.reset,		p_reset,	B_SAFE);
	(void) exported_built_in(in_dict("set_error_handler_", 3),
				p_set_error_handler,		B_SAFE);
	(void) exported_built_in(in_dict("set_default_error_handler_", 3),
				p_set_default_error_handler,	B_SAFE);
	(void) local_built_in(in_dict("set_interrupt_handler_nr", 3),
				p_set_interrupt_handler_nr,	B_SAFE);
	local_built_in(in_dict("get_error_handler", 4),
				p_get_error_handler,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, CONSTANT) | BoundArg(3, CONSTANT) |
		    BoundArg(4, CONSTANT);
	local_built_in(in_dict("get_event_handler", 4),
				p_get_event_handler,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, CONSTANT) | BoundArg(3, CONSTANT) |
		    BoundArg(4, CONSTANT);
	local_built_in(in_dict("get_interrupt_handler_nr", 4),
				p_get_interrupt_handler_nr, B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, CONSTANT) | BoundArg(3, CONSTANT) |
		    BoundArg(4, CONSTANT);
	(void) local_built_in(in_dict("valid_error", 1),
				p_valid_error,		B_SAFE);
	local_built_in(in_dict("interrupt_id_det", 2),
				p_interrupt_id_det,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
    }
    

    /* This must be done before we install _break() as a handler */
    irq_lock_init(delayed_break);

    if (flags & INIT_PROCESS)
    {
	Init_Block_Mask();
	Save_Sig_Mask(initial_sig_mask_);
    }
    else		/* on reset signals may need to be unblocked  */
    {
	Restore_Sig_Mask(initial_sig_mask_);
    }
    spurious = 0;	/* reset fatal signal nesting indicator */

    errno = 0;		/*  we may have ignored some return values ... */

    user_error = USER_ERROR;
}


