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
 * VERSION	$Id: init.c,v 1.1 2008/06/30 17:43:56 jschimpf Exp $
 */

/****************************************************************************
 *
 *	init.c
 *	------
 *
 *	Initialisation routines for ECLiPSe
 *
 *
 ***************************************************************************/

#include 	"config.h"

#include <errno.h>
#include <stdio.h>  /* for sprintf() */
#include <stdlib.h> /* for exit() */

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_SYS_MMAN_H
#include    <sys/mman.h>
#endif



#include        "sepia.h"
#include 	"types.h"
#include	"embed.h"
#include 	"error.h"
#include 	"mem.h"
#include 	"dict.h"
#include	"module.h"
#include	"os_support.h"
#include	"io.h"



/*
 * EXTERN declarations
 */

extern int	io_init(int flags);


extern void	bip_arith_init(int flags),
		bip_array_init(int flags, char *installation_dir),
		bip_comp_init(int flags),
		bip_control_init(int flags),
		bip_db_init(int flags),
		bip_delay_init(int flags),
		bip_domain_init(int flags),
                bip_elipsys_fd_init(int flags),
		bip_emu_init(int flags),
		bip_gc_init(int flags),
		bip_io_init(int flags),
		bip_load_init(int flags),
		bip_misc_init(int flags),
		bip_module_init(int flags),
		bip_op_init(int flags),
		bip_parallel_init(),
		bip_prop_init(int flags),
		bip_record_init(int flags),
		bip_store_init(int flags),
		bip_shelf_init(int flags),
		bip_bag_init(int flags),
		bip_heapevent_init(int flags),
		bip_strings_init(int flags),
		bip_tconv_init(int flags),
		code_init(int flags),
		compiler_init(int flags),
		dict_init(int flags),
		emu_init(int flags, int vm_options),
		error_init(int flags),
		exit_mps(),
		handlers_init(int flags),
		handlers_fini(),
		lex_init(int flags),
		malloc_init(void),
		megalog_boot_init(),
		megalog_init(),
		megalog_end(),
		eclipse_mem_init(int flags),
		mem_fini(void),
		module_init(int flags),
		opaddr_init(void),
		op_init(int flags),
		parallel_init(),
		msg_init(),
		read_init(int flags),
#ifdef SAVEDSTATES
		save_res_init(),
#endif
		setup_mps(),
		worker_init(),
		write_init(int flags);

extern void	kegi_init(),
		user_init();

extern void	short_sleep();

extern void  default_panic(const char *what, const char *where);
extern char * eclipsehome(void);

/*
 * GLOBAL function declarations
 */

void		ec_worker_cleanup(void);

/*
 * LOCAL function declarations
 */

static void	_make_error_message(int err, char *where, char *buf),
		wait_for_flag(volatile int *pflag, int mask);

static char * arg1 = "Embedded ECLiPSE";

/*
 * GLOBAL variable definitions
 */

t_eclipse_data	ec_;

/* TODO: move the following into ec_ on main branch */
char *ec_eclipse_home;		/* canonical, hp_allocated */


/*
 * The ec_options structure is
 * - statically initialised
 * - can be overwritten by the embedding application before ec_init()
 * - is pure input, i.e. must not be changed by eclipse itself
 * - memory pointed to by members is owned by host application, not ECLiPSe
 */

t_eclipse_options ec_options =
{
	/* option_p */
	0,

	/* mapfile */
	(char *) NULL,

	/* parallel_worker */
	0,

	/* io_option */
	SHARED_IO,

	/* Argv,Argc */
	(char **) &arg1,
	1,

	/* rl */
#if defined(HAVE_READLINE)
	 1,
#else
	 0,
#endif

	/* localsize globalsize */
#if defined(HAVE_MMAP) || defined(_WIN32)
	VIRTUAL_STACK_DEFAULT,VIRTUAL_STACK_DEFAULT,
#else
#define KB 1024
#define DEFAULT_LOCAL		200*KB
#define DEFAULT_GLOBAL		750*KB
	DEFAULT_LOCAL,DEFAULT_GLOBAL,
#endif
	/* privatesize,sharedsize */
	VIRTUAL_HEAP_DEFAULT,VIRTUAL_SHARED_DEFAULT,

	/* user_panic */
	default_panic,
	
	/* allocation */
#if defined(HAVE_MMAP) || defined(_WIN32)
#ifdef sun4_0
	ALLOC_FIXED,
#else
	ALLOC_VIRTUAL,
#endif
#else
	ALLOC_PRE,
#endif
	
	/* default module */
	"eclipse",

	/* eclipse_home, input, non-canonical */
	(char *) 0,

	/* init_flags */
	(INIT_SHARED|INIT_PRIVATE|INIT_ENGINE|INIT_PROCESS),

	/* debug_level */
	0
};


/*----------------------------------------------------------------
 * Initialisation
 *
 * init_flags indicates what parts of the system need to be
 * initialised (bit-significant flags):
 *
 *	INIT_SHARED	shared/saveable heap
 *	REINIT_SHARED	heap was restored, some info must be updated
 *	INIT_PRIVATE	C variables, private heap
 *	INIT_ENGINE	abstract machine
 *	INIT_PROCESS	do initialisations that are needed once
 *
 * Initialisation is done in different situations:
 *
 * raw boot		INIT_SHARED|INIT_PRIVATE|INIT_ENGINE|INIT_PROCESS
 * after -r		REINIT_SHARED|INIT_PROCESS|INIT_PRIVATE [|INIT_ENGINE]
 * after -c		INIT_PROCESS|INIT_PRIVATE
 * after restore/1	REINIT_SHARED|INIT_PRIVATE [|INIT_ENGINE]
 * after reset	0 (maybe INIT_PRIVATE)
 *----------------------------------------------------------------*/


int
eclipse_global_init(int init_flags)
{
    int err;

    ec_os_init();

    if (!(init_flags & (INIT_SHARED|REINIT_SHARED)))
    {
	/* if we attach to a heap, it must be fully initialised */
	wait_for_flag(&GlobalFlags, HEAP_READY);
    }

    /*
     * convert pathname to canonical representation
     */
    if (ec_options.eclipse_home)
    {
	char buf[MAX_PATH_LEN];
	(void) canonical_filename(ec_options.eclipse_home, buf);
	if (buf[0] != '/')
	{
	    /* This is mainly to enable the use of -D with relative path */
	    char buf2[MAX_PATH_LEN];
	    get_cwd(buf2, MAX_PATH_LEN);
	    strcat(buf2, buf);
	    ec_eclipse_home = strcpy((char*) hp_alloc(strlen(buf2)+1), buf2);
	}
	else
	{
	    ec_eclipse_home = strcpy((char*) hp_alloc(strlen(buf)+1), buf);
	}
    }
    else
    {
	ec_eclipse_home = strcpy((char*) hp_alloc(strlen(eclipsehome())+1), eclipsehome());
    }

    dict_init(init_flags);
    opaddr_init();
    worker_init(init_flags);
    op_init(init_flags);
    module_init(init_flags);	/* creates modules */
    if ((err = io_init(init_flags)) != PSUCCEED)
    {
	char msg[1024];
	_make_error_message(err, "io_init", msg);
	ec_bad_exit(msg);
    }
    bip_emu_init(init_flags);
    bip_arith_init(init_flags);
    bip_array_init(init_flags, ec_eclipse_home);
    bip_comp_init(init_flags);
    bip_control_init(init_flags);
    bip_db_init(init_flags);
    bip_delay_init(init_flags);
    bip_domain_init(init_flags);
    bip_elipsys_fd_init(init_flags);
    bip_record_init(init_flags);
    bip_store_init(init_flags);
    bip_shelf_init(init_flags);
    bip_bag_init(init_flags);
    bip_heapevent_init(init_flags);
    bip_parallel_init(init_flags);
    bip_gc_init(init_flags);
    bip_io_init(init_flags);
    bip_op_init(init_flags);
    bip_prop_init(init_flags);
    compiler_init(init_flags);
    error_init(init_flags);
    lex_init(init_flags);
    read_init(init_flags);
    write_init(init_flags);
    bip_load_init(init_flags);
    bip_strings_init(init_flags);
    bip_tconv_init(init_flags);
#ifdef SAVEDSTATES
    save_res_init(init_flags);
#endif
    kegi_init(init_flags);
    code_init(init_flags);
    bip_module_init(init_flags);
    if (init_flags & INIT_SHARED) megalog_boot_init();
    user_init(init_flags);
    bip_misc_init(init_flags);
    handlers_init(init_flags);
    msg_init(init_flags);
    if (init_flags & INIT_PRIVATE)
	megalog_init(ec_options.option_p);	/* create shared memory etc */

    return 0;
}

int
eclipse_boot(char *initfile)
{
    value	v1, v2;
    type	t1, t2;
    v1.did = enter_dict(initfile, 0);
    t1.kernel = TDICT;
    v2.did = d_.kernel_sepia;
    t2.kernel = ModuleTag(d_.kernel_sepia);
    return boot_emulc(v1, t1, v2, t2);
}


/*
 * Preliminary I/O routines to be used while our own I/O is not initialised
 */

static void
_make_error_message(int err, char *where, char *buf)
{
    extern char *ec_error_message[];
    if (err == SYS_ERROR)
//XXX: asq: the original line was: not sure, why %. at the end.
//	sprintf(buf, "ECLiPSe: %s (%s) in %.", ec_error_message[err], strerror(errno), where);
    sprintf(buf, "ECLiPSe: %s (%s) in %s", ec_error_message[err], strerror(errno), where);
    else
//XX: asq: same here
//	sprintf(buf, "ECLiPSe: %s in %.", ec_error_message[err], where);
    sprintf(buf, "ECLiPSe: %s in %s", ec_error_message[err], where);
}


/*----------------------------------------------------------------
 * Shutdown code (see also p_exit(), exit/1 and halt/0)
 *
 * Shutdown can be requested either from Prolog (exit/1,halt/0) or
 * from C (ec_cleanup()). In either case, we first do a cleanup at
 * the Prolog level (running finalization goals etc), then the low
 * level cleanup ec_cleanup1().
 * 
 * The cleanup is to be done such that all dynamic resources are
 * freed, and the system can either be reinitialised by ec_init(),
 * or, in the embedded case, the eclipse.[so,dll] can be unloaded,
 * freeing all ECLiPSe-related resources in the process.
 * In particular, we must take care of:
 * - closing all I/O
 * - destroying threads
 * - unloading shared libraries
 * - deallocating the engine stacks
 * - resetting signal handlers
 * - freeing all heap spaces
 * - resetting all static variables to their initial state
 * Because we destroy our shared and private heaps indiscriminately
 * at the end, we need not be too concerned about explicitly deallo-
 * cating all data structures that were previously allocated there.
 * However, we must then be sure that the embedding host does not
 * retain pointers to such (hg/hp_allocated) data. In case ECLiPSe
 * makes any allocations with the system malloc(), these must be
 * freed explicitly otherwise they will constitute a memory leak.
 *----------------------------------------------------------------*/

int
ec_cleanup1(int exit_code)
{
    /*
     * Assume Prolog-level cleanup is already done,
     * either in ec_cleanup() or in exit/1
     */

    if (ec_options.parallel_worker)
    	halt_system(exit_code);

    ec_worker_cleanup();
    return PSUCCEED;
}

int Winapi
ec_cleanup(void)
{
    int res;
    pword goal, module;

    /* Do Prolog-level cleanup code: call cleanup_before_exit/0 */
    Make_Atom(&goal, enter_dict("cleanup_before_exit", 0));
    module.val.did = d_.kernel_sepia;
    module.tag.kernel = ModuleTag(d_.kernel_sepia);
    res = main_emulc_noexit(goal.val, goal.tag, module.val, module.tag);
    if (!(res == PSUCCEED || res == PFAIL))
	return res;

    return ec_cleanup1(0);
}


/*
 * Cleanup one worker
 */
void
ec_worker_cleanup(void)
{
    megalog_end();

    ec_emu_fini();		/* destroy the engine */
    ec_embed_fini();

    bip_load_fini();		/* unload any shared libraries */

    flush_and_close_io(1);	/* shut down I/O system */
    
    ec_os_fini();		/* timers, threads, sockets */

    handlers_fini();		/* undo signal handler settings */

    if (ec_options.parallel_worker)
	exit_mps();

    /* disable interrupts because we cannot serve them properly when
     * all streams are closed */
    Disable_Int();

    /* finally, release all heap memory */
    mem_fini();
}

/*
 * HALT signal handler
 */
int
halt_session(void)
{
	ec_cleanup();
	exit(0);
}

static void
wait_for_flag(volatile int *pflag, int mask) /* volatile is important! */
{
    while (!(*pflag & mask))
	short_sleep(10000);
}

