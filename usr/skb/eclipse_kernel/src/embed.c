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
 * Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: embed.c,v 1.1.2.1 2009/01/31 13:32:51 jschimpf Exp $
 *
 *
 * IDENTIFICATION:	embed.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Call interface to embedded eclipse
 */

#include 	"config.h"

#include <errno.h>

#ifdef STDC_HEADERS
#include <stdarg.h>
#include <string.h>
#else
#include <varargs.h>
extern char *   strcat();
extern char *   strcpy();
#endif

#include        "sepia.h"
#include 	"types.h"
#include 	"error.h"
#include 	"mem.h"
#include 	"dict.h"
#include	"module.h"
#include	"emu_export.h"
#include	"embed.h"
#include	"os_support.h"


/*
 * EXTERN declarations
 */

extern int	eclipse_global_init(int init_flags);
extern int	eclipse_boot(char *initfile);
//asq:
extern int	eclipse_mem_init(int flags);


/*
 * Global state
 */

#ifdef _WIN32
static void *resume_thread = NULL;
#endif


/*----------------------------------------------------------------------
 * Setting the initialisation options
 *----------------------------------------------------------------------*/

/* backwards compatibility */
int Winapi
ec_set_option_int(int opt, int val)
{
    return ec_set_option_long(opt, (long) val);
}

int Winapi
ec_set_option_long(int opt, long val)
{
    switch (opt) {
    case EC_OPTION_PARALLEL_WORKER:	ec_options.parallel_worker = (int) val; break;
    case EC_OPTION_ARGC:	ec_options.Argc = (int) val; break;
    case EC_OPTION_LOCALSIZE:	ec_options.localsize = val; break;
    case EC_OPTION_GLOBALSIZE:	ec_options.globalsize = val; break;
    case EC_OPTION_PRIVATESIZE:	ec_options.privatesize = val; break;
    case EC_OPTION_SHAREDSIZE:	ec_options.sharedsize = val; break;
    case EC_OPTION_ALLOCATION:	ec_options.allocation = (int) val; break;
    case EC_OPTION_IO:		ec_options.io_option = (int) val; break;
    case EC_OPTION_INIT:	ec_options.init_flags = val; break;
    case EC_OPTION_DEBUG_LEVEL:	ec_options.debug_level = val; break;
    default:			return RANGE_ERROR;
    }
    return PSUCCEED;
}

int Winapi
ec_set_option_ptr(int opt, void *val)
{
    switch (opt) {
    case EC_OPTION_MAPFILE:	ec_options.mapfile = (char *) val; break;
    case EC_OPTION_ARGV:	ec_options.Argv = (char **) val; break;
    case EC_OPTION_PANIC:	ec_options.user_panic = (void(*)(const char*,const char *)) val; break;
    case EC_OPTION_DEFAULT_MODULE:	ec_options.default_module = (char *) val; break;
    case EC_OPTION_ECLIPSEDIR:	ec_options.eclipse_home = (char *) val; break;
    default:			return RANGE_ERROR;
    }
    return PSUCCEED;
}

/*----------------------------------------------------------------------
 * Initialising an embedded Eclipse
 *----------------------------------------------------------------------*/

int Winapi
ec_init(void)
{
    char *	initfile = (char *) 0;
    char	filename_buf[MAX_PATH_LEN];
    pword	goal,module;
    int		res;

    
    /*----------------------------------------------------------------
     * Make the connection to the shared heap, if any.
     * Because of mmap problems on some machines this should
     * happen AFTER initializing the message passing system.
     *----------------------------------------------------------------*/
//asq:
    eclipse_mem_init(ec_options.init_flags);	/* depends on -c and -m options */

    /*
     * Init the global (shared) eclipse structures, dictionary, code...
     * Maybe load a saved state.
     * Note that we don't have an engine yet!
     */
    eclipse_global_init(ec_options.init_flags);


    /*----------------------------------------------------------------
     * Setup the Prolog engine
     *----------------------------------------------------------------*/
    /*
     * Initialize the Prolog engine
     */
    emu_init(ec_options.init_flags, 0);

    initfile = strcat(strcpy(filename_buf, ec_eclipse_home), "/lib/kernel.eco");
    if (ec_access(initfile, R_OK) < 0)
    {
	initfile = strcat(strcpy(filename_buf, ec_eclipse_home), "/lib/kernel.pl");
	if (ec_access(initfile, R_OK) < 0)
	{
	    ec_panic("Aborting: Can't find boot file! Please check either\na) your program's setting for eclipsedir in ec_set_option(), or\nb) your setting for ECLIPSEDIR environment variable.\n","ec_init()");
	}
    }	    

    res = eclipse_boot(initfile);
    if (res != PSUCCEED)
    	return res;

    goal = ec_term(ec_did("main",1), ec_long(ec_options.init_flags & INIT_SHARED ? 0 : 1));
    module.val.did = ec_.d.kernel_sepia;
    module.tag.kernel = ModuleTag(ec_.d.kernel_sepia);
    if (main_emulc_noexit(goal.val, goal.tag, module.val, module.tag) != PYIELD)
	return PFAIL;
    return PSUCCEED;
}

void
ec_embed_fini(void)
{
#ifdef _WIN32
    if (resume_thread)
    {
	(void) ec_thread_terminate(resume_thread, 3000/*ms timeout*/);
	resume_thread = NULL;
    }
#endif
    hp_free(ec_eclipse_home);
    ec_eclipse_home = 0;
}

/*----------------------------------------------------------------------
 * Posting goals
 *----------------------------------------------------------------------*/

void Winapi
ec_post_goal(const pword goal)
{
    pword *pw;

    if (g_emu_.nesting_level > 1)
	ec_panic("can't post goal to nested engine","ec_post_goal()");

    pw = TG;					/* new list element */
    Push_List_Frame();
    pw[0] = goal;
    Make_Var(&pw[1]);

    Bind_(POSTED_LAST.val.ptr, pw, TLIST);	/* append */
    ec_assign(&POSTED_LAST, pw[1].val, pw[1].tag);
}

static pword
_get_posted_goals(void)
{
    pword posted, empty;

    /* terminate the posted-goals-list and copy its beginning */
    Bind_(POSTED_LAST.val.ptr, 0, TNIL);
    posted = POSTED;

    /* reinitialise the list to an empty difference list */
    Make_Ref(&empty, TG);
    Push_Var();
    ec_assign(&POSTED, empty.val, empty.tag);
    ec_assign(&POSTED_LAST, empty.val, empty.tag);

    return posted;
}

void Winapi
ec_post_string(const char *callstring)
{
    ec_post_goal(ec_term(ec_.d.colon,
	ec_atom(ec_.d.kernel_sepia),
	ec_term(ec_did("exec_string",2), ec_string(callstring), ec_newvar())));
}

void Winapi
ec_post_exdr(int length, const char *exdr_string)
{
    ec_post_goal(ec_term(ec_.d.colon,
	ec_atom(ec_.d.kernel_sepia),
    	ec_term(ec_did("exec_exdr",1), ec_length_string(length, exdr_string))));
}

int Winapi
ec_exec_string(
    	char *callstring,
	ec_ref varsref)		/* NULL is allowed */
{
    pword	vars;
    dident exec_string_2 =  enter_dict("exec_string",2);
    
    vars = ec_newvar();
    if (varsref)
	ec_ref_set(varsref, vars);
    ec_post_goal(ec_term(ec_.d.colon,
	ec_atom(ec_.d.kernel_sepia),
	ec_term(exec_string_2, ec_string(callstring), vars)));
	    
    return ec_resume1(0);
}


/*----------------------------------------------------------------------
 * Resuming Eclipse execution
 *----------------------------------------------------------------------*/

int Winapi
ec_resume(void)
{
    return ec_resume1(0);
}

int Winapi
ec_resume1(ec_ref chp)
{
    return ec_resume2(_get_posted_goals(), chp);
}

int Winapi
ec_resume2(const pword term, ec_ref chp)
{
    int res;
    pword * pw;
    pword tterm;
    /* this assignment is needed to get around a compiler bug on Alpha Linux
       that otherwise corrupts chp
    */
    tterm = term;

    if (g_emu_.nesting_level > 1)
	ec_panic("can't resume nested engine","ec_resume2()");

    if (ec_running())
	return PRUNNING;

    A[1] = tterm;
    Make_Integer(&A[2], RESUME_CONT);
    res = restart_emulc();
    if (res != PYIELD)
	ec_panic("eclipse emulator did not yield properly","ec_resume()");

    if (chp)
	ec_ref_set(chp,A[2]);

    pw = &A[1];
    Dereference_(pw)
    if (IsInteger(pw->tag))
	return pw->val.nint;
    else
	return  TYPE_ERROR;
}

int Winapi
ec_resume_long(long int *to_c)
{
    int res;
    pword * pw;

    if (g_emu_.nesting_level > 1)
	ec_panic("can't resume nested engine","ec_resume_long()");

    if (ec_running())
	return PRUNNING;

    A[1] = _get_posted_goals();
    Make_Integer(&A[2], RESUME_CONT);

    res = restart_emulc();
    if (res != PYIELD)
	ec_panic("eclipse emulator did not yield properly","ec_resume_long()");

    pw = &A[2];
    Dereference_(pw)
    if (IsInteger(pw->tag))
    	*to_c = pw->val.nint;
    else
    	*to_c = 0;

    pw = &A[1];
    Dereference_(pw)
    if (IsInteger(pw->tag))
	return pw->val.nint;
    else
	return  TYPE_ERROR;
}



int Winapi
ec_running(void)
{
#ifdef _WIN32
    int res;
    if (resume_thread  &&  !ec_thread_stopped(resume_thread, &res))
	return 1;
#endif
    return 0;
}

#ifdef _WIN32

/* this will be called in a thread */
static int
restart_emulc_thread(void *dummy_arg_for_thread)
{
    return restart_emulc();
}

#endif

int Winapi
ec_resume_async(void)
{
    if (g_emu_.nesting_level > 1)
	ec_panic("can't resume nested engine","ec_resume2()");

#ifdef _WIN32
    if (!resume_thread)	/* if we don't have a thread yet, make one */
    {
    	resume_thread = ec_make_thread();
	if (!resume_thread)
	    return SYS_ERROR;
    }
    else		/* make sure the thread is not running */
    {
	if (ec_running())
	    return PRUNNING;
    }
#endif

    A[1] = _get_posted_goals();
    Make_Integer(&A[2], RESUME_CONT);

#ifdef _WIN32
    if (!ec_start_thread(resume_thread, restart_emulc_thread, NULL))
	return SYS_ERROR;
#endif

    return PSUCCEED;
}


int Winapi
ec_resume_status(void)
{
    long dummy;
    return ec_resume_status_long(&dummy);
}

int Winapi
ec_resume_status_long(long int *to_c)
{
    return ec_wait_resume_status_long(to_c, 0);
}

int Winapi
ec_wait_resume_status_long(long int *to_c, int timeout)
{
    pword *pw;
    int res;

#ifdef _WIN32
    /* This is supposed to be called only after a resume_async! */
    if (!resume_thread)
    	return PERROR;
    if (!ec_thread_wait(resume_thread, &res, timeout))
	return PRUNNING;
#else
    /* We don't have threads: resume here in order to make resume_async-
     * resume_status sequences work anyway, so we can write portable code.
     */
    res = restart_emulc();
#endif
    if (res != PYIELD)
	ec_panic("eclipse emulator did not yield properly","ec_resume_long()");

    pw = &A[2];
    Dereference_(pw)
    if (IsInteger(pw->tag))
	*to_c = pw->val.nint;
    else
	*to_c = 0;

    pw = &A[1];
    Dereference_(pw)
    if (IsInteger(pw->tag))
	return pw->val.nint;
    else
	return TYPE_ERROR;
}


/*----------------------------------------------------------------------
 * Resuming Eclipse without continuing
 * just create an opportunity for event handling
 * Return values:
 *	PRUNNING
 *		engine not yet ready (previous resume_async)
 *	PFLUSHIO,PWAITIO
 *		nested request from within handler
 *	PSUCCEED
 *		handler finished
 *	PFAIL,PTHROW
 *		should never occur (prevented by yield/3)
 *	PYIELD
 *		programmer error (yield/2 in handler)
 *----------------------------------------------------------------------*/

int Winapi
ec_handle_events(long int *to_c)
{
    int res;
    pword * pw;

    if (g_emu_.nesting_level > 1)
	ec_panic("can't resume nested engine","ec_handle_events()");

    if (ec_running())
	return PRUNNING;

    Make_Nil(&A[1])		/* don't care */
    Make_Integer(&A[2], RESUME_SIMPLE);
    res = restart_emulc();
    if (res != PYIELD)
	ec_panic("eclipse emulator did not yield properly","ec_handle_events()");

    pw = &A[2];
    Dereference_(pw)
    if (IsInteger(pw->tag))
	*to_c = pw->val.nint;
    else
	*to_c = 0;

    pw = &A[1];
    Dereference_(pw)
    if (IsInteger(pw->tag))
	return pw->val.nint;
    else
	return TYPE_ERROR;
}


/*----------------------------------------------------------------------
 * External references:
 *
 * States of external references:
 *
 * EC_REF_C:	hp_allocated, simple value, not in global list
 *
 *	This is the state just after an ec_refs has been created by a
 *	call to ec_refs_create(), or after backtracking to such a point.
 *	It is not "initialised" yet, i.e. no array (structure) for the
 *	n slots has been allocated on the global stack, and it is not
 *	yet known to the garbage collector. The var-field preliminarily
 *	holds the init-value instead of a pointer to a global stack array.
 *	
 * EC_REF_C_P:	hp_allocated, prolog value, in global list
 *
 *	This is the normal working state: the ec_refs is used from the
 *	C program, its var-field points to a global stack array of arity
 *	n, and it is known to the garbage collector via the global list.
 *	The transition from EC_REF_C to EC_REF_C_P happens on the first
 *	access to the ec_refs: a global stack arary is allocated and its
 *	slots initialised with the requested init value.
 *
 * EC_REF_FREE:	deallocated, no value, not in global list
 *
 *	This state only exists temporarily just before deallocation.
 * 
 * Allowed transitions:
 * (none)	--create-->	EC_REF_C
 * EC_REF_C	--init-->	EC_REF_C_P
 * EC_REF_C	--destroy-->	EC_REF_FREE
 * EC_REF_C	--untrail-->	EC_REF_C
 * EC_REF_C_P	--destroy-->	EC_REF_FREE
 * EC_REF_C_P	--untrail-->	EC_REF_C
 *----------------------------------------------------------------------*/

void Winapi
ec_refs_destroy(ec_refs variable)
{
    if (!(variable->refstate & EC_REF_C))
	ec_panic("ec_ref already freed from C","ec_refs_destroy()");
    if (variable->refstate & EC_REF_P)
    {
	/* Unlink the ec_ref to make the global stack array become garbage */
	variable->next->prev = variable->prev;
	variable->prev->next = variable->next;
    }
    variable->refstate = EC_REF_FREE;
    hp_free_size((generic_ptr)variable, sizeof(struct eclipse_ref_));
}

/*ARGSUSED*/
static void
_ec_refs_untrail(pword *parray, word *pdata, int size, int flags)
{
    ec_refs variable = g_emu_.allrefs.next;
    /* Find the ec_ref corresponding to parray in the global list. */
    /* If it's not in there, then it has already been destroyed! */
    while (variable != &g_emu_.allrefs)
    {
	if (variable->var.val.ptr == parray)
	{
	    if (!(variable->refstate == EC_REF_C_P))
		ec_panic("ec_ref already untrailed","_ec_refs_untrail()");
	    variable->refstate &= ~EC_REF_P;
	    variable->next->prev = variable->prev;	/* unlink */
	    variable->prev->next = variable->next;
	    variable->var = *((pword*) pdata);		/* reset value */
	    return;
	}
	variable = variable->next;
    }
}

int Winapi
ec_refs_size(const ec_refs variable)
{
    return variable->size;
}

ec_refs Winapi
ec_refs_create_newvars(int n)
{
    ec_ref new;

    new = hp_alloc_size(sizeof(struct eclipse_ref_));
    new->var = g_emu_.allrefs.var;
    new->refstate = EC_REF_C;
    new->size = n;
    new->next = new->prev = 0;
    return new;
}

ec_refs Winapi
ec_refs_create(int n, const pword initpw)
{
    ec_ref new;

    if (!(IsSimple(initpw.tag) || IsPersistent(initpw.tag)))
	    ec_panic("non-atomic initializer","ec_refs_create()");
    new = hp_alloc_size(sizeof(struct eclipse_ref_));
    new->var = initpw;
    new->refstate = EC_REF_C;
    new->size = n;
    new->next = new->prev = 0;
    return new;
}

static void
_ec_ref_init(ec_refs variable)
{
    pword * pw, initpw;
    int i;
    int n = variable->size;

    if (variable->refstate != EC_REF_C)
    	ec_panic("ec_refs already freed from C","_ec_ref_init()");

    initpw = variable->var;
    variable->refstate = EC_REF_C_P;

    /* Use the global stack array as trail item, so the trail entry */
    /* gets garbage collected together with it. */
    pw = TG;
    ec_trail_undo(_ec_refs_untrail, pw, NULL,
	    (word *) &initpw, sizeof(pword)/sizeof(word), TRAILED_PWORD);

    Make_Struct(&(variable->var), pw);
    Push_Struct_Frame(ec_did("",n));
    if (IsRef(initpw.tag))
    {
	for (i=1; i<=n; i++)
	{ /* brackets important */
	    Make_Var(pw+i);
    	}
    }
    else
    {
	for (i=1; i<=n; i++)
	    pw[i] = initpw;
    }
    variable->next = g_emu_.allrefs.next;
    variable->prev = &g_emu_.allrefs;
    g_emu_.allrefs.next->prev = variable;
    g_emu_.allrefs.next = variable;
}

void Winapi
ec_refs_set(ec_refs variable, int i, const pword w)
{
    if (variable->refstate != EC_REF_C_P)
	_ec_ref_init(variable);
    if (i >= variable->size)
	ec_panic("out of bounds","ec_refs_set()");

    (void) ec_assign(variable->var.val.ptr+i+1, w.val,w.tag);
}

pword Winapi
ec_refs_get(const ec_refs variable, int i)
{
    if (variable->refstate != EC_REF_C_P)
	_ec_ref_init(variable);
    if (i >= variable->size)
	ec_panic("out of bounds","ec_refs_get()");

    return variable->var.val.ptr[i+1];
}


ec_ref Winapi
ec_ref_create(pword initpw)
{
    return (ec_ref) ec_refs_create(1, initpw);
}

ec_ref Winapi
ec_ref_create_newvar(void)
{
    return (ec_ref) ec_refs_create_newvars(1);
}

void Winapi
ec_ref_set(ec_ref variable, const pword w)
{
    ec_refs_set((ec_refs) variable, 0, w);
}

pword Winapi
ec_ref_get(const ec_ref variable)
{
    return ec_refs_get((ec_refs) variable, 0);
}

void Winapi
ec_ref_destroy(ec_ref variable)
{
    ec_refs_destroy((ec_refs) variable);
}


/*----------------------------------------------------------------------
 * Choicepoints and cuts
 *----------------------------------------------------------------------*/

void Winapi
ec_cut_to_chp(ec_ref chp)
{
    ec_post_goal(ec_term(ec_.d.call_explicit,
    			ec_term(ec_.d.cut_to,ec_ref_get(chp)),
			ec_atom(ec_.d.kernel_sepia)));
}


/*----------------------------------------------------------------------
 * C->Prolog and Prolog->C type conversions
 *----------------------------------------------------------------------*/

pword Winapi
ec_atom(const dident a)
{
    pword w;
    if (a == ec_.d.nil)
    {
    	Make_Nil(&w);
    }
    else
    {
	Make_Atom(&w,a);
    }
    return w;
}

int Winapi
ec_get_atom(const pword w, dident *a)
{
    const pword * pw = &w;
    Dereference_(pw);
    if (IsAtom(pw->tag))
	*a = pw->val.did;
    else if (IsNil(pw->tag))
	*a = ec_.d.nil;
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    Succeed_
}

pword Winapi
ec_string(const char *s)
{
	pword w;
	Make_String(&w, (char *) s);
	return w;
}

pword Winapi
ec_length_string(int l, const char *s)
{
	pword w;
	char *s1;
	w.tag.kernel = TSTRG;
	w.val.ptr = TG;
	Push_Buffer(l+1);
	s1 = (char *) BufferStart(w.val.ptr);
	Copy_Bytes(s1, (char *) s, l);
	s1[l] = 0;
	return w;
}

int Winapi
ec_get_string(const pword w, char **s)
{
    const pword *pw = &w;
    Dereference_(pw);

    if (IsString(pw->tag)) 
	*s = StringStart(pw->val);
    else if (IsAtom(pw->tag)) 
	*s = DidName(pw->val.did);
    else if (IsNil(pw->tag)) 
	*s = DidName(ec_.d.nil);
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    Succeed_
}

int Winapi
ec_get_string_length(const pword w, char **s, long int *l)
{
    const pword *pw = &w;
    Dereference_(pw);

    if (IsString(pw->tag)) 
    {
	*s = StringStart(pw->val);
	*l = StringLength(pw->val);
    }
    else if (IsAtom(pw->tag)) 
    {
	*s = DidName(pw->val.did);
	*l = DidLength(pw->val.did);
    }
    else if (IsNil(pw->tag)) 
    {
	*s = DidName(ec_.d.nil);
	*l = 2;
    }
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    Succeed_
}

pword Winapi
ec_long(const long int l)
{
	pword w;
	Make_Integer(&w,l);
	return w;
}

int Winapi
ec_get_long(const pword w, long int *l)
{
    const pword *pw = &w;
    Dereference_(pw);

    if (IsInteger(pw->tag)) 
	*l = pw->val.nint;
    else if (IsBignum(pw->tag)) 
	return RANGE_ERROR;
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    /* convert doubles that are integer? */
    Succeed_
}

pword Winapi
ec_double(const double d)
{
    pword result;

    Make_Double(&result, d);
    return result;
}

int Winapi
ec_get_double(const pword w, double *d)
{
    const pword *pw = &w;
    Dereference_(pw);

    if (IsDouble(pw->tag)) 
	*d = Dbl(pw->val);
    else if (IsInteger(pw->tag)) 
	*d = (double) pw->val.nint;
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    Succeed_
}


#ifdef STDC_HEADERS

pword
ec_term(dident functor, ...)
{
    va_list ap;
    int arity = DidArity(functor);
    pword * pw;
    pword result;
    int i;

    va_start(ap, functor);

    pw = TG;
    Push_Struct_Frame(functor);
    for (i=1 ; i <= arity ; i++)
	pw[i] = va_arg(ap,pword);
    va_end(ap);

    Make_Struct(&result,pw);
    return result;
}

#else

pword
ec_term(va_alist)
va_dcl
{
    va_list ap;
    dident functor;
    int arity;
    pword * pw;
    pword result;
    int i;

    va_start(ap);

    functor = va_arg(ap,dident);
    arity = DidArity(functor);

    pw = TG;
    Push_Struct_Frame(functor);
    for (i=1 ; i <= arity ; i++)
	pw[i] = va_arg(ap,pword);
    va_end(ap);

    Make_Struct(&result,pw);
    return result;
}

#endif

pword Winapi
ec_term_array(const dident functor, const pword *args)
{
    int arity;
    pword * pw;
    pword result;

    arity = DidArity(functor);

    pw = TG;
    Make_Struct(&result,pw);
    Push_Struct_Frame(functor);
    pw++;
    
    while(arity--)
	*pw++ = *args++;

    return result;
}


pword Winapi
ec_matrixofdouble(int n, int m, const double *darr)
{
    dident row_functor = enter_dict("[]", n);
    dident col_functor = enter_dict("[]", m);
    pword *rows, *col;
    pword result;
    int i,j;

    rows = TG;
    Push_Struct_Frame(row_functor);
    for(i=1; i<=n; ++i)
    {
	col = TG;
	Make_Struct(&rows[i], col);
	Push_Struct_Frame(col_functor);
	for(j=1; j<=m; ++j)
	{
	    Make_Double(&col[j], *darr++);
	}
    }
    Make_Struct(&result,rows);
    return result;
}

pword Winapi
ec_arrayofdouble(int n, const double *darr)
{
    dident functor = enter_dict("[]", n);
    pword result;
    pword *row;
    int i;

    row = TG;
    Push_Struct_Frame(functor);
    for(i=1; i<=n; ++i)
    {
    	Make_Double(&row[i], *darr++)
    }
    Make_Struct(&result,row);
    return result;
}


pword Winapi
ec_list(const pword head, const pword tail)
{
    pword * pw;
    pword result;

    pw = TG;
    Push_List_Frame();
    pw[0] = head;
    pw[1] = tail;
    
    Make_List(&result,pw);
    return result;
}

pword Winapi
ec_listofdouble(int length, const double *array)
{
    pword result;
    pword *pw = &result;
    while (length-- > 0)
    {
	Make_List(pw,TG);
	pw = TG;
	Push_List_Frame();
	*pw++ = ec_double(*array++);
    }
    Make_Nil(pw);
    return result;
}

pword Winapi
ec_listoflong(int length, const long int *array)
{
    pword result;
    pword *pw = &result;
    while (length-- > 0)
    {
	Make_List(pw,TG);
	pw = TG;
	Push_List_Frame();
	*pw++ = ec_long(*array++);
    }
    Make_Nil(pw);
    return result;
}

pword Winapi
ec_listofchar(int length, const char *array)
{
    pword result;
    pword *pw = &result;
    while (length-- > 0)
    {
	Make_List(pw,TG);
	pw = TG;
	Push_List_Frame();
	*pw++ = ec_long(*array++);
    }
    Make_Nil(pw);
    return result;
}

pword Winapi
ec_listofrefs(ec_refs refs)
{
    pword result;
    pword *pw = &result;
    int length = refs->size;
    int i;

    if (refs->refstate != EC_REF_C_P)
	_ec_ref_init(refs);

    for (i=1; i<=length; i++)
    {
	Make_List(pw,TG);
	pw = TG;
	Push_List_Frame();
	*pw++ = refs->var.val.ptr[i];
    }
    Make_Nil(pw);
    return result;
}

int Winapi
ec_get_nil(const pword list)
{
    const pword * pw = &list;
    Dereference_(pw);
    return IsNil(pw->tag)? PSUCCEED: PFAIL;
}

int Winapi
ec_is_var(const pword w)
{
    const pword * pw = &w;
    Dereference_(pw);
    return IsRef(pw->tag)? PSUCCEED: PFAIL;
}

int  Winapi
ec_get_list(const pword list, pword *car, pword *cdr)
{
    const pword * pw = &list;
    Dereference_(pw);

    if (IsList(pw->tag))
    {
	*car = pw->val.ptr[0];
	*cdr = pw->val.ptr[1];
	Succeed_
    }
    else if (IsNil(pw->tag))
	Fail_
    else if (IsRef(pw->tag))
    	return INSTANTIATION_FAULT;
    else
    	return TYPE_ERROR;
}

int Winapi
ec_get_arg(const int n, pword term, pword *arg)
{
    pword * pw = &term;
    Dereference_(pw);

    if (IsStructure(pw->tag))
	if (n < 1  ||  n > DidArity(pw->val.ptr->val.did))
	    return RANGE_ERROR;
	else
	    *arg = pw->val.ptr[n];
    else if (IsList(pw->tag))
	if (n < 1  ||  n > 2)
	    return RANGE_ERROR;
	else
	    *arg = pw->val.ptr[n-1];
    else if (IsRef(pw->tag))
    	return INSTANTIATION_FAULT;
    else
    	return TYPE_ERROR;
    Succeed_
}

int Winapi
ec_get_functor(const pword term, dident *d)
{
    const pword * pw = &term;
    Dereference_(pw);

    if (IsStructure(pw->tag))
	*d = pw->val.ptr->val.did;
    else if (IsList(pw->tag))
    	*d = ec_.d.list;
    else if (IsRef(pw->tag))
    	return INSTANTIATION_FAULT;
    else
    	return TYPE_ERROR;
    Succeed_
}

int Winapi
ec_arity(const pword term)
{
    const pword * pw = &term;
    Dereference_(pw);
    if (IsList(pw->tag))
    	return 2;

    if (IsStructure(pw->tag))
    	return DidArity(pw->val.ptr->val.did);

    return 0;
}

pword Winapi
ec_newvar(void)
{
    pword * pw;

    pw = TG++;
    Make_Ref(pw,pw);
    return *pw;
    
}

pword Winapi
ec_nil(void)
{
	pword p;

	Make_Nil(&p);
	return p;
}
	
static void
ec_deref(pword *ppw)	/* dereference in place */
{
    if (IsRef(ppw->tag))
    {
	pword *ppw1 = ppw;
	Dereference_(ppw);
	*ppw1 = *ppw;
    }
}


int Winapi
ec_var_lookup(ec_ref vars, char *name, pword *var)
{
	pword list;
	pword pair;
	pword varname;

	list = ec_ref_get(vars);
	while (ec_deref(&list),IsList(list.tag))
	{
	    if ( PSUCCEED == ec_get_arg(1,list,&pair) &&
		(ec_deref(&pair), IsList(pair.tag)) &&
		PSUCCEED ==  ec_get_arg(1,pair,&varname) &&
		(ec_deref(&varname), IsAtom(varname.tag)) &&
		0 == strcmp(DidName(varname.val.did),name) )
	    {
			ec_get_arg(2,pair,var);
			Succeed_
	    }
	    else
	    {
		    if (PSUCCEED != ec_get_arg(2,list,&list))
		    	{ Fail_; }
	    }
	}
	Fail_
}


/*----------------------------------------------------------------------
 * Support for external C predicates
 *----------------------------------------------------------------------*/

int Winapi
ec_unify(pword pw1, pword pw2)
{
    return ec_unify_(pw1.val, pw1.tag, pw2.val, pw2.tag, &MU);
}


int Winapi
ec_unify_arg(int n, pword term)
{
#ifdef __STDC__
    static type tref = {TREF};
#else
    type tref;
    tref.kernel = TREF;
#endif
    return ec_unify_(A[n].val, A[n].tag, term.val, term.tag, &MU);
}

int Winapi
ec_compare(pword pw1, pword pw2)
{
    pword *ppw1 =  &pw1;
    pword *ppw2 =  &pw2;
    Dereference_(ppw1);
    Dereference_(ppw2);
    return compare_terms(ppw1->val, ppw1->tag, ppw2->val, ppw2->tag);
}

pword Winapi
ec_arg(int n)
{
    return A[n];
}

int Winapi
ec_schedule_suspensions(pword attr, int pos)
{
    Check_Structure(attr.tag);
    if (pos < 1 || pos > DidArity(attr.val.ptr[0].val.did))
    	return RANGE_ERROR;
    return ec_schedule_susps(&(attr.val.ptr[pos]));
}

int Winapi
ec_visible_procedure(dident proc_did, pword module, void **pproc)
{
    pri *proc = visible_procedure(proc_did, module.val.did, module.tag, 0);
    if (!proc)
    {
	int res;
	Get_Bip_Error(res);
	return res;
    }
    *pproc = (void*) proc;
    return PSUCCEED;
}


/*----------------------------------------------------------------------
 * Some predefined external data types
 *----------------------------------------------------------------------*/

/*
 * double []
 */

static pword
_double_arr_get(t_ext_ptr h, int i)
{
    return ec_double(((double*)h)[i]);
}

static int
_double_arr_set(t_ext_ptr h, int i, pword pw)
{
    return ec_get_double(pw, &((double*)h)[i]);
}

t_ext_type ec_xt_double_arr = {
    0, 0, 0, 0, 0, 0, 0,
    _double_arr_get,
    _double_arr_set
};


/*
 * long []
 */

static pword
_long_arr_get(t_ext_ptr h, int i)
{
    return ec_long(((long*)h)[i]);
}

static int
_long_arr_set(t_ext_ptr h, int i, pword pw)
{
    return ec_get_long(pw, &((long*)h)[i]);
}

t_ext_type ec_xt_long_arr = {
    0, 0, 0, 0, 0, 0, 0,
    _long_arr_get,
    _long_arr_set
};


/*
 * char []
 */

static pword
_char_arr_get(t_ext_ptr h, int i)
{
    return ec_long((long) ((char*)h)[i]);
}

static int
_char_arr_set(t_ext_ptr h, int i, pword pw)
{
    long l;
    int err = ec_get_long(pw, &l);
    if (err == PSUCCEED)
    	((char*) h)[i] = (char) l;
    return err;
}

static int
_char_arr_ss(t_ext_ptr h, int quoted)
{
    return strlen((char*) h) + (quoted? 2: 0);
}

static int
_char_arr_tos(t_ext_ptr h, char *buf, int quoted)
{
    char *dest = buf;
    char *src = (char*) h;
    if (quoted)
    {
	*dest++ = '"';
	while (*dest++ = *src++)
	    ;
	*(dest-1) = '"';
	*dest++ = 0;
    }
    else
    {
	while (*dest++ = *src++)
	    ;
    }
    return dest-buf-1;
}

t_ext_type ec_xt_char_arr = {
    0, 0, 0,
    _char_arr_ss,
    _char_arr_tos,
    0, 0,
    _char_arr_get,
    _char_arr_set
};

