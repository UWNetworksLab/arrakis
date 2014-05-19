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
  VERSION	$Id: bip_misc.c,v 1.4 2008/10/15 10:09:26 jschimpf Exp $
 */

/****************************************************************************
 *
 *		SEPIA Built-in Predicates: Miscellaneous
 *
 *
 *****************************************************************************/

#ifdef EXTENSIVE_DEBUGGING
#define DEB printf("\n%s, %s, %d", __FILE__, __func__, __LINE__);
#else
#define DEB ;
#endif

// XXX: Had to recreate definition here because cannot
//      include barrelfish/debug.h in this file.
#define USER_PANIC(msg...)                                 \
    user_panic_fn(__FILE__, __func__, __LINE__, msg);      \

//asq:
#include <assert.h>


#include "config.h"

#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <errno.h>
#include <stdio.h>
#include <math.h>

#ifndef _WIN32
#include <sys/time.h>
//#include <sys/times.h>
//#include <pwd.h>
extern void	endpwent(void);
//#include <grp.h>
extern void	endgrent(void);
#else
#include <windows.h>
#include <process.h>
#endif

//#include <signal.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
unsigned int	alarm();
#endif

#ifndef ACCESS_IN_UNISTD
//#include <sys/file.h>
#endif

#ifdef HAVE_SYS_SYSTEMINFO_H
#include <sys/systeminfo.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#else
extern char	*getenv();
extern void	exit();
# ifdef HAVE_RANDOM
#  if (SIZEOF_LONG == 8)
    extern int	random();
#  else
    extern long	random();
#  endif
# endif
#endif


#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "emu_export.h"
#include "os_support.h"

extern int      p_wm_get();
extern int      p_wm_get_ids();
extern int      p_wm_set();
extern int      p_wm_interface();
extern double   elapsed_session_time();
extern int      p_worker_stat_reset();
extern int      p_worker_stat();

static int p_date(value v, type t),
	p_all_times(value vuser, type tuser, value vsys, type tsys, value vreal, type treal),
	p_argc(value v0, type t0),
	p_argv(value v0, type t0, value v1, type t1),
	p_cd(value v, type t),
	p_cd_if_possible(value v, type t),
	p_expand_filename(value vin, type tin, value vout, type tout),
	p_os_file_name(value vecl, type tecl, value vos, type tos),
	p_getcwd(value sval, type stag),
	p_getenv(value v0, type t0, value v1, type t1),
	p_get_sys_flag(value vf, type tf, value vv, type tv),
	p_kill(value pv, type pt, value sv, type st),
	p_local_time(value vy, type ty, value vm, type tm, value vd, type td, value vh, type th, value vmin, type tmin, value vsec, type tsec, value vdst, type tdst, value vunixtime, type tunixtime),
	p_local_time_string(value vunixtime, type tunixtime, value vformat, type tformat, value vs, type ts),
	p_pathname(value sval, type stag, value pathval, type pathtag, value vfile, type tfile),
	p_frandom(value v, type t),
	p_random(value v, type t),
	p_seed(value v, type t),
	p_sleep(value v, type t),
	p_setenv(value v0, type t0, value v1, type t1),
	p_suffix(value sval, type stag, value sufval, type suftag),
	p_session_time(value vtime, type ttime),
	p_get_hr_time(value vtime, type ttime),
	p_set_timer(value vtimer, type ttimer, value vinterv, type tinterv),
	p_get_timer(value vtimer, type ttimer, value vinterv, type tinterv),
	p_start_timer(value vtimer, type ttimer, value vfirst, type tfirst, value vinterv, type tinterv),
	p_stop_timer(value vtimer, type ttimer, value vremain, type tremain, value vinterv, type tinterv),
	p_cputime(value val, type tag),
	p_alarm(value v, type t),
#ifdef _WIN32
	p_system(value v, type t),
#endif
	p_sys_file_flag(value fv, type ft, value nv, type nt, value vv, type vt);

static void
	_fseed(uint32),
	_post_alarm(long);


int	p_heap_stat(value vwhat, type twhat, value vval, type tval);

static dident	d_virtual,
		d_version,
		d_profile;

/*
 * Static variables
 */

static dident	d_hostid_ = D_UNKNOWN;	/* cache for hostid atom */

extern char	ec_version[];
extern int      ec_sigalrm;

static int32	seed;	/* for random generator */

#ifdef _WIN32
static LARGE_INTEGER ticks_per_sec_;
static int have_perf_counter_ = 0;
#endif


void
bip_misc_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("argc",1),	p_argc,	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("argv",2),	p_argv,	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("getenv",2),	p_getenv, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("setenv",2),	p_setenv, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("date",1), 	p_date,	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("local_time",8),  p_local_time,	B_UNSAFE|U_GROUND);
	(void) built_in(in_dict("local_time_string",3),  p_local_time_string,	B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("expand_filename",2),
				p_expand_filename,	B_UNSAFE|U_SIMPLE);
	built_in(in_dict("os_file_name",2), 	p_os_file_name, B_UNSAFE|U_GROUND)
		-> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
	(void) built_in(in_dict("random",1), 	p_random, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("frandom",1), 	p_frandom, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("seed",1),	p_seed, 	B_SAFE);
	(void) built_in(in_dict("sleep",1), 	p_sleep, 	B_UNSAFE);
	(void) built_in(in_dict("kill", 2), 	p_kill, 	B_SAFE);
	(void) built_in(in_dict("suffix", 2), 	p_suffix, B_UNSAFE|U_SIMPLE);
	built_in(in_dict("pathname", 3), 	p_pathname, B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, CONSTANT) | BoundArg(3, CONSTANT);
	(void) built_in(in_dict("getcwd", 1), 	p_getcwd,  B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("cd", 1),		p_cd, 	B_SAFE);
	(void) built_in(in_dict("cd_if_possible", 1),	p_cd_if_possible, 	B_SAFE);
	(void) built_in(in_dict("get_hr_time", 1), p_get_hr_time, 	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("set_timer", 2), p_set_timer, 	B_SAFE);
	(void) built_in(in_dict("get_timer", 2),
				p_get_timer,		B_UNSAFE|U_SIMPLE);
	(void) exported_built_in(in_dict("start_timer", 3), p_start_timer,	B_SAFE);
	exported_built_in(in_dict("stop_timer", 3),
				p_stop_timer,		B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, CONSTANT) | BoundArg(3, CONSTANT);
	(void) local_built_in(in_dict("wm_get", 1), p_wm_get, B_UNSAFE|U_GROUND);
	(void) local_built_in(in_dict("wm_get_ids", 2), p_wm_get_ids, B_UNSAFE|U_GROUND);
	(void) local_built_in(in_dict("wm_set", 3), p_wm_set, B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("wm_interface", 1), p_wm_interface, 
			B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("session_time", 1), p_session_time,
		B_UNSAFE|U_SIMPLE);
	local_built_in(in_dict("all_times", 3), p_all_times, B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(1, CONSTANT) | BoundArg(2, CONSTANT) |
		    BoundArg(3, CONSTANT);
	(void) local_built_in(in_dict("heap_stat", 2),
				p_heap_stat,		B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("get_sys_flag", 2),
				p_get_sys_flag,		B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("sys_file_flag", 3),
				p_sys_file_flag,	B_UNSAFE|U_SIMPLE);
	(void) exported_built_in(in_dict("worker_statistics_reset", 1),
			p_worker_stat_reset, B_SAFE);
	(void) exported_built_in(in_dict("worker_statistics", 2), p_worker_stat,
		B_UNSAFE|U_GROUND);
	(void) built_in(in_dict("cputime",1), p_cputime, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("alarm",1), p_alarm, B_UNSAFE);
#ifdef _WIN32
	(void) local_built_in(in_dict("_system", 1), p_system, B_SAFE);
#endif
    }

    if (flags & INIT_PRIVATE)
    {
	d_virtual = in_dict("virtual", 0);
	d_profile = in_dict("profile", 0);
	d_version = in_dict(ec_version, 0);
    }

    if (flags & INIT_PROCESS)
    {
	/* initialize random generators */
	int rand_init = ec_unix_time() * getpid();
	_fseed((uint32) rand_init);
#ifdef HAVE_RANDOM
	srandom((unsigned) rand_init);
#else
	srand((unsigned) rand_init);
#endif
#ifdef _WIN32
	if (QueryPerformanceFrequency(&ticks_per_sec_))
	    have_perf_counter_ = 1;
#endif
    }
}


/*	argc/1
 *	unifies its argument with the number of argument of the call to sepia.
 */

static int
p_argc(value v0, type t0)
{
    Check_Output_Integer(t0);
    Return_Unify_Integer(v0,t0,ec_options.Argc);
}

/*	argv/2
 *	first argument must be an integer in the range [0..Argc[
 *	unify the second with the specified arg of the call to sepia.
 */

static int
p_argv(value v0, type t0, value v1, type t1)
{
    pword result;

    if (IsInteger(t0))
    {
	if (v0.nint >= 0)	/* get one argument */
	{
	    Check_Output_String(t1);
	    if (v0.nint >= ec_options.Argc) { Bip_Error(RANGE_ERROR); }
	    Make_String(&result, ec_options.Argv[v0.nint]);
	}
	else	/* shift arguments: argv(NegPos, NShift) */
	{
	    int i,j;
	    Check_Integer(t1);
	    i = -v0.nint;
	    j = i + v1.nint;
	    if (j < i || i >= ec_options.Argc || j > ec_options.Argc)
	    	{ Bip_Error(RANGE_ERROR); }
	    while (j < ec_options.Argc)
	    	ec_options.Argv[i++] = ec_options.Argv[j++];
	    ec_options.Argc = i;
	    Succeed_;
	}
    }
    else if (IsAtom(t0))
    {
	int	i;
	pword	*car, *cdr;
	Check_Output_List(t1);
	if (v0.did != d_.all) { Bip_Error(RANGE_ERROR); }
	cdr = &result;
	for (i=0; i<ec_options.Argc; i++)
	{
	    car = TG;
	    Push_List_Frame();
	    Make_List(cdr, car);
	    Make_String(car, ec_options.Argv[i]);
	    cdr = car + 1;
	}
	Make_Nil(cdr);
    }
    else { Bip_Error(TYPE_ERROR); }

    Return_Unify_Pw(v1, t1, result.val, result.tag);
}

/*
 *	getenv/2
 * unifies its second argument with the value associated with the first
 * argument in the environment list (using getenv(3))
 */

#define	TENTATIVE_SIZE 1024

static int
p_getenv(value v0, type t0, value v1, type t1)
{
    int size, buf_size;
    char *name;
    value v;

    Get_Name(v0,t0,name)
    Check_Output_String(t1)
    v.ptr = TG;
    size = TENTATIVE_SIZE;
    do {
	TG = v.ptr;
	Push_Buffer(size);
	buf_size = size;
	if (!ec_env_lookup(name, StringStart(v), &size))
	{
	    Fail_;
	}
    } while (size > buf_size);

    Trim_Buffer(v.ptr, size);
    Return_Unify_String(v1, t1, v.ptr)
}


/*
 * setenv(+Name, +Value)
 */

static int
p_setenv(value v0, type t0, value v1, type t1)
{
    char *name, *new_value;
    pword *old_tg = TG;

    Get_Name(v0, t0, name);

    /* For the value, allow numbers, strings and atoms */
    if (IsNumber(t1))
    {
	/* convert integer to temporary string */
	int len = tag_desc[TagType(t1)].string_size(v1, t1, 1);
	value v_tmp;
	v_tmp.ptr = TG;
	Push_Buffer(len+1);		/* make integer string buffer */
	len = tag_desc[TagType(t1)].to_string(v1, t1, StringStart(v_tmp), 1);
	Trim_Buffer(v_tmp.ptr, len+1);
	new_value = StringStart(v_tmp);
    }
    else
    {
	Get_Name(v1, t1, new_value);
    }

#ifdef _WIN32
    if (!SetEnvironmentVariable(name, new_value))
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	Bip_Error(SYS_ERROR);
    }
#else
#ifdef HAVE_PUTENV
   {
	/*
	 * With putenv(), the "name=value" string becomes part of the
	 * environment.  We use malloc to allocate the string, as it needs
         * to persist after ECLiPSe ends. We check to see that the 
	 * environment variable is not already set to the same value to avoid 
         * multiple copies
	 */
	int len = strlen(name) + 2 + strlen(new_value); /* "name=value\0" */
	char *envstring;

	if (strchr(name, '='))	/* emulate setenv() behaviour */
	{
	    Set_Sys_Errno(EINVAL, ERRNO_UNIX);
	    Bip_Error(SYS_ERROR);
	}
	/* check if the environment variable is already set to new_value */
	envstring = getenv(name);
	if (!envstring || strcmp(envstring, new_value))
	{
	    /* the memory associated with envstring is leaked! */
	    envstring = (char *)malloc(len); 
	    strcat(strcat(strcpy(envstring, name), "="), new_value);
	    if (putenv(envstring))
	    {
		free(envstring);
		Set_Errno
		Bip_Error(SYS_ERROR);
	    }
	}
   }
#else
    /* setenv() copies the strings, old strings are leaked! */
//asq:
//    if (setenv(name, new_value, 1))
    {
	Set_Errno
	Bip_Error(SYS_ERROR);
    }
#endif
#endif

    TG = old_tg;	/* pop any temporary buffers */
    Succeed_;
}


/*
 * unsetenv(+Name)	not sufficiently portable
 */

#if 0
static int
p_unsetenv(value v0, type t0)
{
    char *name;
    Get_Name(v0, t0, name);
#ifdef _WIN32
    if (SetEnvironmentVariable(name, 0))
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	Bip_Error(SYS_ERROR);
    }
#else
    unsetenv(name);
#endif
    Succeed_;
}
#endif


/*	date/1
 *	binds its argument to a string holding
 *	the date and time of the form:
 *	Sun Sep 16 01:03:52 1987\n\0
 *	with fixed field sizes (total: 26 characters)
 */
static int
p_date(value v, type t)
{
    char buf[50];
    value val;

    Check_Output_String(t)
    (void) ec_date_string(buf);
    Cstring_To_Prolog(buf, val);
    Return_Unify_String(v, t, val.ptr);
}


static int
p_local_time(value vy, type ty, value vm, type tm, value vd, type td, value vh, type th, value vmin, type tmin, value vsec, type tsec, value vdst, type tdst, value vunixtime, type tunixtime)
{
    time_t time_utc;
    struct tm time_here;
    Prepare_Requests;

    if (IsRef(tunixtime))
    {
	Check_Integer(ty);
	Check_Integer(tm);
	Check_Integer(td);
	Check_Integer(th);
	Check_Integer(tmin);
	Check_Integer(tsec);
	Check_Output_Integer(tdst);
	time_here.tm_year = vy.nint - 1900;
	time_here.tm_mon = vm.nint - 1;
	time_here.tm_mday = vd.nint;
	time_here.tm_hour = vh.nint;
	time_here.tm_min = vmin.nint;
	time_here.tm_sec = vsec.nint;
	time_here.tm_isdst = IsRef(tdst) ? -1 : vdst.nint ? 1 : 0;

	time_utc = mktime(&time_here);
	if (time_utc == (time_t) -1)
	    { Fail_; }

	Request_Unify_Integer(vunixtime, tunixtime, time_utc);
    }
    else
    {
	Check_Integer(tunixtime)
	time_utc = (time_t) vunixtime.nint;

#ifdef HAVE_LOCALTIME_R
	localtime_r(&time_utc, &time_here);
#else
	time_here = *localtime(&time_utc);
#endif
    }
    Request_Unify_Integer(vy, ty, time_here.tm_year + 1900);
    Request_Unify_Integer(vm, tm, time_here.tm_mon + 1);
    Request_Unify_Integer(vd, td, time_here.tm_mday);
    Request_Unify_Integer(vh, th, time_here.tm_hour);
    Request_Unify_Integer(vmin, tmin, time_here.tm_min);
    Request_Unify_Integer(vsec, tsec, time_here.tm_sec);
    Request_Unify_Integer(vdst, tdst, time_here.tm_isdst ? 1 : 0);
    Return_Unify;
}


static int
p_local_time_string(value vunixtime, type tunixtime, value vformat, type tformat, value vs, type ts)
{
    pword *pw;
    time_t time_utc;
    struct tm time_here;
    value vres;
    char *s, *format;
    int fmtlen, len, max;

    Check_Integer(tunixtime);
    Error_If_Ref(tformat);
    if (IsString(tformat)) {
	format = StringStart(vformat);
	fmtlen = StringLength(vformat);
    } else if (IsAtom(tformat)) {
	format = DidName(vformat.did);
	fmtlen = DidLength(vformat.did);
    } else if (IsNil(tformat)) {
	format = DidName(d_.nil);
	fmtlen = DidLength(d_.nil);
    } else {
	Bip_Error(TYPE_ERROR)
    }
    Check_Output_String(ts);

    time_utc = (time_t) vunixtime.nint;
#ifdef HAVE_LOCALTIME_R
    localtime_r(&time_utc, &time_here);
#else
    time_here = *localtime(&time_utc);
#endif

    /* guess a max length for the buffer */
    max = fmtlen > 100 ? fmtlen * 10 : 1000;
    pw = TG;
    for (;;)
    {
	Push_Buffer(max+1);
	len = strftime((char *) BufferStart(pw), max+1, format, &time_here);
	if (len > 0  ||  fmtlen == 0)
	{
	    Trim_Buffer(pw, len+1);
	    break;
	}
	TG = pw;	/* pop the old buffer */
	max *= 2;
    }
    Return_Unify_String(vs, ts, pw);
}


/*
 * Floating point random generator. This is taken from random2.c
 * by John Burton, available from the net. Part of original comment:
 *
 * PMMMLCG - Prime Modulus M Multiplicative Linear Congruential Generator   *
 *  Modified version of the Random number generator proposed by             *
 *  Park & Miller in "Random Number Generators: Good Ones Are Hard to Find" *
 *  CACM October 1988, Vol 31, No. 10                                       *
 *   - Modifications proposed by Park to provide better statistical         *
 *     properties (i.e. more "random" - less correlation between sets of    *
 *     generated numbers                                                    *
 *   - generator is of the form                                             *
 *         x = ( x * A) % M                                                 *
 *   - Choice of A & M can radically modify the properties of the generator *
 *     the current values were chosen after followup work to the original   *
 *     paper mentioned above.                                               *
 *   - The generator has a period of 2^31 - 1 with numbers generated in the *
 *     range of 0 < x < M                                                   *
 *   - The generator can run on any machine with a 32-bit integer, without  *
 *     overflow.                                                            *
 */

#define RND_A	48271
#define RND_M	2147483647
#define RND_Q	(RND_M / RND_A)
#define RND_R	(RND_M % RND_A)

static void
_fseed(uint32 n)
{
    int32 seed0 = n % RND_M;
    seed = (seed0==0) ? 1 : seed0;	/* seed must be in range 1..2147483646 */
}

static double
frandom(void)
{
    int32 lo,hi,test;
    static double temp = 1.0 / (double)RND_M;

    hi = seed / RND_Q;
    lo = seed % RND_Q;
    test = RND_A * lo - RND_R * hi;
    seed = (test > 0) ? (test) : (test + RND_M);
    return( (double)seed * temp);
}

static int
p_frandom(value v, type t)
{
    double f = frandom();
    Check_Output_Float(t);
    Return_Unify_Float(v, t, f); /* may use several times its arguments */
}


/*
 * p_random()	random/1
 * Binds it argument to a random integer.
 */
static int
p_random(value v, type t)
{
    long n;
#ifdef HAVE_RANDOM
    n = random();		  /* use n, because the following macro */
#else
    n = (rand() << 16) | rand(); /* make a long out of the short(?)*/
    if (n < 0)
	n = -n;
#endif
    Check_Output_Integer(t)
    Return_Unify_Integer(v,t,n); /* may use several times its arguments */
}

/*
 * p_seed()	seed/1
 * Sets the seed for random/1. The argument must be an int.
 */
static int
p_seed(value v, type t)
{
	Check_Integer(t);
#ifdef HAVE_RANDOM
	srandom((unsigned) v.nint);
#else
	srand((unsigned) v.nint);
#endif
	_fseed((uint32) v.nint);		/* for frandom() */
	Succeed_;
}


/*
 * p_sleep()	sleep/1
 *
 * Suspends the process for the given (integer) number of seconds.
 */
static int
p_sleep(value v, type t)
{
    if (IsInteger(t))
	(void) ec_sleep((double) v.nint);
    else if (IsDouble(t))
	(void) ec_sleep(Dbl(v));
    else
	{ Bip_Error(TYPE_ERROR); }
    return(PSUCCEED);
}

/*
 * Get the suffix of a filename (extension).
 */
static int
p_suffix(value sval, type stag, value sufval, type suftag)
{
	char		*p;
	char		*suffix;
	char		c;
	value		v;

	Get_Name(sval, stag, p);
	suffix = 0;

	while (c = *++p)	/* omit the (posible) leading '.' */
		if (c == '/')
		{
			suffix = 0;
			if (*(p + 1))	/* idem */
				p++;
		}
		else if (c == '.')
			suffix = p;
	if (!suffix)
		suffix = p;

	if (IsString(suftag))
	{
		Succeed_If(!strcmp(suffix, StringStart(sufval)));
	}
	else if (IsRef(suftag))
	{
		Cstring_To_Prolog(suffix, v);
                Return_Unify_String(sufval,suftag,v.ptr);
	}
	Bip_Error(TYPE_ERROR);
}

/*
 * Split the pathname into parent path and simple file name.
 */
static int
p_pathname(value sval, type stag, value pathval, type pathtag, value vfile, type tfile)
{
	char		*p;
	char		*path;
	char		*t;
	char		c;
	char		fullname[MAX_PATH_LEN];
	value		v;
	value		vf;
	Prepare_Requests;

	Get_Name(sval, stag, path);
	Check_Output_String(pathtag);
	Check_Output_String(tfile);
	t = p = path = expand_filename(path, fullname);

	if (path[0] == '/' && path[1] == '/')
	    p = &path[2];

	while (c = *p++)
		if (c == '/' && *p)	/* we ignore trailing '/' */
			path = p;

	Make_Stack_String(path - t, v, p);
	while (t < path)
		*p++ = *t++;
	*p = '\0';
	Cstring_To_Prolog(path, vf);

        Request_Unify_String(pathval,pathtag,v.ptr);
        Request_Unify_String(vfile, tfile, vf.ptr);
	Return_Unify;
}


/*
 * expand_filename(+NameIn, ?NameOut)
 *
 * expand ~, ~user and $VAR at the beginning of the filename,
 * and remove extra leading /, ./  from each subpath.
 */

static int
p_expand_filename(value vin, type tin, value vout, type tout)
{
    char *in, out[MAX_PATH_LEN];
    value v;
    Get_Name(vin, tin, in);
    Check_Output_String(tout);
    (void) expand_filename(in, out);
    Cstring_To_Prolog(out, v);
    Return_Unify_String(vout, tout, v.ptr);
}

static int
p_os_file_name(value vecl, type tecl, value vos, type tos)
{
    char *in, out[MAX_PATH_LEN];
    pword pw;

    if (IsRef(tos))			/* internal -> external */
    {
	Get_Name(vecl, tecl, in);
	(void) os_filename(in, out);
	if (IsAtom(tecl))
	    { Make_Atom(&pw, enter_dict(out,0)); }
	else
	    { Make_String(&pw, out); }
	Return_Unify_Pw(vos, tos, pw.val, pw.tag);
    }
    else				/* external -> internal */
    {
	Get_Name(vos, tos, in);
	(void) canonical_filename(in, out);
	if (IsAtom(tos))
	    { Make_Atom(&pw, enter_dict(out,0)); }
	else
	    { Make_String(&pw, out); }
	if (!IsRef(tecl) && DifferType(tecl,tos))
	    { Bip_Error(TYPE_ERROR); }
	Return_Unify_Pw(vecl, tecl, pw.val, pw.tag);
    }
}


/*
 * getcwd/1
 */
static int
p_getcwd(value sval, type stag)
{
	value	v;
	char	*s;
	char	buf[MAX_PATH_LEN];
	int	len;

	Check_Output_String(stag);
	len = get_cwd(buf, MAX_PATH_LEN);
	Make_Stack_String(len, v, s);
	Copy_Bytes(s, buf, len+1);
	Return_Unify_String(sval, stag, v.ptr);
}

static int
p_cd(value v, type t)
{
	char   *name;
	char	buf[MAX_PATH_LEN];

	Get_Name(v,t,name)
	name = expand_filename(name, buf);
	if (ec_chdir(name)) {
		Set_Errno
		Bip_Error(SYS_ERROR)
	}
	Succeed_;
}

/* moved into C from ECLiPSe for compatibility with Windows Vista, where
   checking that a directory has executable flag set does not work
*/
static int
p_cd_if_possible(value v, type t)
{
	char   *name;
	char	buf[MAX_PATH_LEN];

	Get_Name(v,t,name)
	name = expand_filename(name, buf);
	if (ec_chdir(name)) {
	    Fail_;
	}
	Succeed_;
}


static int
p_all_times(value vuser, type tuser, value vsys, type tsys, value vreal, type treal)
{
    double user, sys, elapsed;
    Prepare_Requests
    if (all_times(&user, &sys, &elapsed))
    {
	Set_Errno
	Bip_Error(SYS_ERROR)
    }
    Request_Unify_Float(vuser, tuser, user);
    Request_Unify_Float(vsys, tsys, sys);
    Request_Unify_Float(vreal, treal, elapsed);
    Return_Unify;
}

static int
p_session_time(value vtime, type ttime)
{
    double elapsed, dummy;

    if (ec_options.parallel_worker)
    	elapsed = elapsed_session_time();
    else
	(void) all_times(&dummy, &dummy, &elapsed);

    Return_Unify_Float(vtime, ttime, elapsed);
}


static int
p_get_sys_flag(value vf, type tf, value vv, type tv)
{
    extern dident	d_hostarch_;
    pword		pw;

    Check_Integer(tf);
    switch (vf.nint)
    {
    case 1:	/* hostid */

	if (d_hostid_ == D_UNKNOWN)
	{
	    /* get the hostid and cache it for future calls */
	    char buf[257];
	    int len = ec_gethostid(buf, 257);
	    if (len > 0)
		d_hostid_ = enter_dict_n(buf, len, 0);
	    else
		d_hostid_ = enter_dict_n("?", 1, 0);
	}
	pw.tag.kernel = TSTRG;
	pw.val.ptr = DidString(d_hostid_);
	break;


    case 2:	/* hostname */
	{
	    int len;
	    pw.tag.kernel = TSTRG;
	    pw.val.ptr = TG;
	    Push_Buffer(257);
	    len = ec_gethostname(StringStart(pw.val), 257);
	    if (len < 0) {
		len = 1;
		*StringStart(pw.val) = '?';
	    }
	    Trim_Buffer(pw.val.ptr, len+1);
	    break;
	}

    case 3:	/* pid */
	pw.val.nint = getpid();
	pw.tag.kernel = TINT;
	break;

    case 4:	/* ppid */
//asq:
    pw.val.nint = 0;
/*
#ifdef _WIN32
	pw.val.nint = 0;
#else
	pw.val.nint = getppid();
#endif
*/
	pw.tag.kernel = TINT;
	break;

    case 5:	/* unix_time */
	pw.val.nint = ec_unix_time();
	pw.tag.kernel = TINT;
	break;

    case 6:	/* local_size */
	pw.val.nint = ((char *) SP_ORIG - (char *) B_ORIG) / 1024;
	pw.tag.kernel = TINT;
	break;

    case 7:	/* global_size */
	pw.val.nint = ((char *) TT_ORIG - (char *) TG_ORIG) / 1024;
	pw.tag.kernel = TINT;
	break;
    
    case 8:	/* hostarch */
	pw.tag.kernel = TSTRG;
	pw.val.ptr = DidString(d_hostarch_);
	break;

    case 9:	/* object suffix */
	Make_String(&pw, OBJECT_SUFFIX_STRING);
	break;

    case 10:	/* worker number */
	pw.val.nint = ec_options.parallel_worker;
	pw.tag.kernel = TINT;
	break;

    case 11:	/* current version */
	Make_Atom(&pw, d_version);
	break;

    default:
	Bip_Error(RANGE_ERROR);
    }
    Return_Unify_Pw(vv, tv, pw.val, pw.tag);
}

static int
p_cputime(value val, type tag)
{
	Check_Output_Float(tag);
	Return_Unify_Float(val, tag, ((double) (user_time())) / clock_hz);
}

static void
_post_alarm(long int n)
{
    if (ec_post_event_int(n) != PSUCCEED)
    {
	p_fprintf(current_err_, "ECLiPSe: Could not post alarm event");
	ec_flush(current_err_);
    }
}

static int
p_alarm(value v, type t)
{
    Check_Integer(t);
//asq:
    DEB
    assert(!"not implemented.");
/*
#ifdef _WIN32
    if (!ec_set_alarm((double) v.nint, 0.0, _post_alarm, ec_sigalrm, 0, 0))
    	{ Bip_Error(SYS_ERROR); }
#else
    (void) alarm((unsigned) v.nint);
#endif
*/
    Succeed_;
}


/*
 * Return time in seconds with a high resolution, but undefined epoch.
 * Only good to measure the difference between two time points.
 * This is currently real time on Unix and Windows, not cputime.
 */

static int
p_get_hr_time(value v, type t)
{
    double seconds;
#ifdef _WIN32
    LARGE_INTEGER ticks;
    if (!have_perf_counter_)
	return p_session_time(v, t);

    if (!QueryPerformanceCounter(&ticks))
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	Bip_Error(SYS_ERROR);
    }
    seconds = (double)ticks.QuadPart/(double)ticks_per_sec_.QuadPart;
#else
//asq:
/*
    struct timeval ticks;
    if (gettimeofday(&ticks, NULL))
    	{ Bip_Error(SYS_ERROR); }
    seconds = ticks.tv_sec + ticks.tv_usec/1000000.0;
*/
    seconds = 0;
#endif
    Return_Unify_Float(v, t, seconds);
}


/*
 * set_timer(+Timer, +TimeBetweenInterrupts)
 *
 * Generate a sequence of signals, occuring in
 * intervals specified by the argument.
 * Time is given in seconds.
 * Use set_timer(+Timer, 0) to switch it off.
 */
#ifdef _WIN32

static int
p_start_timer(value vtimer, type ttimer, value vfirst, type tfirst, value vinterv, type tinterv)
{
    double first, interv;

    if (IsInteger(tfirst))
	first = (double) vfirst.nint;
    else if (IsDouble(tfirst))
	first = Dbl(vfirst);
    else if (IsRef(tfirst))
	{ Bip_Error(INSTANTIATION_FAULT); }
    else
	{ Bip_Error(TYPE_ERROR); }

    if (IsInteger(tinterv))
	interv = (double) vinterv.nint;
    else if (IsDouble(tinterv))
	interv = Dbl(vinterv);
    else if (IsRef(tinterv))
	{ Bip_Error(INSTANTIATION_FAULT); }
    else
	{ Bip_Error(TYPE_ERROR); }

    Check_Atom(ttimer)
    if (vtimer.did == d_.real0)
	;
    else if (vtimer.did == d_virtual)
	; /* { Bip_Error(UNIMPLEMENTED); } */
    else if (vtimer.did == d_profile)
	; /* { Bip_Error(UNIMPLEMENTED); } */
    else {
	Bip_Error(RANGE_ERROR)
    }

    if (!ec_set_alarm(first, interv, _post_alarm, ec_sigalrm, 0, 0))
	{ Bip_Error(SYS_ERROR); }
    Succeed_;
}

static int
p_set_timer(value vtimer, type ttimer, value vinterv, type tinterv)
{
    return p_start_timer(vtimer, ttimer, vinterv, tinterv, vinterv, tinterv);
}

static int
p_stop_timer(value vtimer, type ttimer, value vremain, type tremain, value vinterv, type tinterv)
{
    double	remain, old_interv;
    Prepare_Requests

    Check_Output_Float(tremain)
    Check_Output_Float(tinterv)
    Check_Atom(ttimer)
    if (vtimer.did == d_.real0)
	;
    else if (vtimer.did == d_virtual)
	; /* { Bip_Error(UNIMPLEMENTED); } */
    else if (vtimer.did == d_profile)
	; /* { Bip_Error(UNIMPLEMENTED); } */
    else {
	Bip_Error(RANGE_ERROR)
    }

    if (!ec_set_alarm(0.0, 0.0, _post_alarm, ec_sigalrm, &remain, &old_interv))
	{ Bip_Error(SYS_ERROR); }
    Request_Unify_Float(vinterv, tinterv, old_interv) 
    Request_Unify_Float(vremain, tremain, remain)
    Return_Unify
}

static int
p_get_timer(value vtimer, type ttimer, value vinterv, type tinterv)	/* obsolete */
{
    double	remain, old_interv;

    Check_Output_Float(tinterv)
    Check_Atom(ttimer)
    if (vtimer.did == d_.real0)
	;
    else if (vtimer.did == d_virtual)
	{ Bip_Error(UNIMPLEMENTED); }
    else if (vtimer.did == d_profile)
	{ Bip_Error(UNIMPLEMENTED); }
    else {
	Bip_Error(RANGE_ERROR)
    }

    if (!ec_set_alarm(0.0, 0.0, _post_alarm, ec_sigalrm, &remain, &old_interv))
	{ Bip_Error(SYS_ERROR); }
    if (!ec_set_alarm(remain, old_interv, _post_alarm, ec_sigalrm, 0, 0))
	{ Bip_Error(SYS_ERROR); }
    if (old_interv == 0)
       { Fail_; }
    Return_Unify_Float(vinterv, tinterv, old_interv)
}

#else
#if defined(HAVE_SETITIMER)
static int
p_start_timer(value vtimer, type ttimer, value vfirst, type tfirst, value vinterv, type tinterv)
{
	struct itimerval	desc;
	int			timer;

	if (IsInteger(tinterv))
	{
	    desc.it_interval.tv_sec = vinterv.nint;
	    desc.it_interval.tv_usec = 0;
	}
	else if (IsDouble(tinterv))
	{
	    double interv = Dbl(vinterv);
	    desc.it_interval.tv_sec = (long) interv;
	    desc.it_interval.tv_usec =
		(long) ((interv - floor(interv)) * 1000000.0);
	    if (desc.it_interval.tv_sec == 0
		    && desc.it_interval.tv_usec == 0
		    && interv > 0.0)
		desc.it_interval.tv_usec = 1;
	    else if (desc.it_interval.tv_usec > 999999)
		desc.it_interval.tv_usec = 999999;
	    /* the limit is taken from the solaris manual */
	    if (desc.it_interval.tv_sec > 100000000)
		desc.it_interval.tv_sec = 100000000;
	}
	else if (IsRef(tinterv))
	    { Bip_Error(INSTANTIATION_FAULT); }
	else
	    { Bip_Error(TYPE_ERROR); }

	if (IsInteger(tfirst))
	{
	    desc.it_value.tv_sec = vfirst.nint;
	    desc.it_value.tv_usec = 0;
	}
	else if (IsDouble(tfirst))
	{
	    double first = Dbl(vfirst);
	    desc.it_value.tv_sec = (long) first;
	    desc.it_value.tv_usec =
		(long) ((first - floor(first)) * 1000000.0);
	    if (desc.it_value.tv_sec == 0
		    && desc.it_value.tv_usec == 0
		    && first > 0.0)
		desc.it_value.tv_usec = 1;
	    else if (desc.it_value.tv_usec > 999999)
		desc.it_value.tv_usec = 999999;
	    /* the limit is taken from the solaris manual */
	    if (desc.it_value.tv_sec > 100000000)
		desc.it_value.tv_sec = 100000000;
	}
	else if (IsRef(tfirst))
	    { Bip_Error(INSTANTIATION_FAULT); }
	else
	    { Bip_Error(TYPE_ERROR); }

	Check_Atom(ttimer)
	if (vtimer.did == d_.real0)
	    timer = ITIMER_REAL;
	else if (vtimer.did == d_virtual)
	    timer = ITIMER_VIRTUAL;
	else if (vtimer.did == d_profile)
	    timer = ITIMER_PROF;
	else {
	    Bip_Error(RANGE_ERROR)
	}

	if (setitimer(timer, &desc, (struct itimerval *) 0) < 0) {
	    Set_Errno;
	    Bip_Error(SYS_ERROR);
	}
	Succeed_
}

static int
p_set_timer(value vtimer, type ttimer, value vinterv, type tinterv)
{
    return p_start_timer(vtimer, ttimer, vinterv, tinterv, vinterv, tinterv);
}

static int
p_get_timer(value vtimer, type ttimer, value vinterv, type tinterv)
{
	struct itimerval	desc;
	int			timer;

	Check_Output_Float(tinterv)
	Check_Atom(ttimer)
	if (vtimer.did == d_.real0)
	    timer = ITIMER_REAL;
	else if (vtimer.did == d_virtual)
	    timer = ITIMER_VIRTUAL;
	else if (vtimer.did == d_profile)
	    timer = ITIMER_PROF;
	else {
	    Bip_Error(RANGE_ERROR)
	}

	if (getitimer(timer, &desc) < 0) {
	    Set_Errno;
	    Bip_Error(SYS_ERROR);
	}
	if (desc.it_interval.tv_sec == 0 &&
	    desc.it_interval.tv_usec == 0) {
	    Fail_;
	}
	Return_Unify_Float(vinterv, tinterv,
		desc.it_interval.tv_sec + desc.it_interval.tv_usec/1000000.0)
}


/*
 * stop_timer/3 switches the timer off and gets the current state
 * It doesn't fail like get_timer/2
 */

static int
p_stop_timer(value vtimer, type ttimer, value vremain, type tremain, value vinterv, type tinterv)
{
	struct itimerval	old, new;
	int			timer;
	Prepare_Requests

	Check_Output_Float(tremain)
	Check_Output_Float(tinterv)
	Check_Atom(ttimer)
	if (vtimer.did == d_.real0)
	    timer = ITIMER_REAL;
	else if (vtimer.did == d_virtual)
	    timer = ITIMER_VIRTUAL;
	else if (vtimer.did == d_profile)
	    timer = ITIMER_PROF;
	else {
	    Bip_Error(RANGE_ERROR)
	}

	new.it_interval.tv_sec = 0;
	new.it_interval.tv_usec = 0;
	new.it_value.tv_sec = 0;
	new.it_value.tv_usec = 0;
	if (setitimer(timer, &new, &old) < 0) {
	    Set_Errno;
	    Bip_Error(SYS_ERROR);
	}
	Request_Unify_Float(vinterv, tinterv,
		old.it_interval.tv_sec + old.it_interval.tv_usec/1000000.0)
	Request_Unify_Float(vremain, tremain,
		old.it_value.tv_sec + old.it_value.tv_usec/1000000.0)
	Return_Unify
}

#else
//asq:
static int p_set_timer(value vtimer, type ttimer, value vinterv, type tinterv)
{
        DEB
        USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}
static int p_get_timer(value vtimer, type ttimer, value vinterv, type tinterv)
{
        DEB
        USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}

static int p_start_timer(value vtimer, type ttimer, value vfirst, type tfirst, value vinterv, type tinterv)
{
        DEB
        USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}

static int p_stop_timer(value vtimer, type ttimer, value vremain, type tremain, value vinterv, type tinterv)
{
        DEB
        printf("XXX: p_stop_timer() does nothing!\n");
//        assert(!"\nNOT available\n");
    //Bip_Error(NOT_AVAILABLE);
    return (0);
}
/*
Not_Available_Built_In(p_set_timer)
Not_Available_Built_In(p_get_timer)
Not_Available_Built_In(p_start_timer)
Not_Available_Built_In(p_stop_timer)
*/
#endif
#endif



static int
p_kill(value pv, type pt, value sv, type st)
{
	Check_Integer(pt);
	Check_Integer(st);

//asq:
    if (!(pv.nint == 0 || pv.nint == getpid()))
        { Bip_Error(UNIMPLEMENTED); }
    DEB
    assert(!"Not implemented.");
/*
#ifdef _WIN32
	if (!(pv.nint == 0 || pv.nint == getpid()))
	    { Bip_Error(UNIMPLEMENTED); }

	if (!raise((int) sv.nint))
	    { Bip_Error(RANGE_ERROR); }
#else
	if (kill((int) pv.nint, (int) sv.nint) < 0)
	{
	    if (sv.nint == 0L && errno == ESRCH)
	    {
		Fail_;		// just checking for process existence
	    }
	    else
	    {
		Set_Errno;
		Bip_Error(SYS_ERROR);
	    }
	}
#endif
*/
	Succeed_;
}

#ifdef _WIN32
static int
p_system(value v, type t)
{
    int res;
    char *command;
    Get_Name(v, t, command);
    res = system(command);
    if (res == -1)
    {
	Set_Errno;
	Bip_Error(SYS_ERROR);
    }
    Succeed_If(res == 0);
}
#endif

/*ARGSUSED*/
static int
p_sys_file_flag(value fv, type ft, value nv, type nt, value vv, type vt)
{
    struct_stat		buf;
    char		*name;
    char		*str;
    value		val;
    int			acc;

    Get_Name(fv, ft, name);
    if (nv.nint <= 16) {
	if (ec_stat(name, &buf) == -1)
	{
	    errno = 0;
	    Fail_;
	}
    }
    switch (nv.nint)
    {
    case 0:
	Return_Unify_Integer(vv, vt, buf.st_mode);

    case 1:
	Return_Unify_Integer(vv, vt, buf.st_ino);

    case 2:
	Return_Unify_Integer(vv, vt, buf.st_nlink);

    case 3:
	Return_Unify_Integer(vv, vt, buf.st_uid);

    case 4:
	Return_Unify_Integer(vv, vt, buf.st_gid);

    case 5:
	Return_Unify_Integer(vv, vt, buf.st_size);

    case 6:
	if (buf.st_atime < 0) { Fail_; }	/* for Windows pseudo-files */
	Return_Unify_Integer(vv, vt, buf.st_atime);

    case 7:
	if (buf.st_mtime < 0) { Fail_; }	/* for Windows pseudo-files */
	Return_Unify_Integer(vv, vt, buf.st_mtime);

    case 8:
	if (buf.st_ctime < 0) { Fail_; }	/* for Windows pseudo-files */
	Return_Unify_Integer(vv, vt, buf.st_ctime);

    case 9:
	Return_Unify_Integer(vv, vt, buf.st_dev);

#ifdef HAVE_ST_BLKSIZE
    case 10:
	Return_Unify_Integer(vv, vt, buf.st_blocks);

    case 11:
	Return_Unify_Integer(vv, vt, buf.st_blksize);
#endif

    case 12:
	if (buf.st_atime < 0) { Fail_; }	/* for Windows pseudo-files */
	str = ctime(&buf.st_atime);
	Cstring_To_Prolog(str, val);
	Return_Unify_String(vv, vt, val.ptr);

    case 13:
	if (buf.st_mtime < 0) { Fail_; }	/* for Windows pseudo-files */
	str = ctime(&buf.st_mtime);
	Cstring_To_Prolog(str, val);
	Return_Unify_String(vv, vt, val.ptr);

    case 14:
	if (buf.st_ctime < 0) { Fail_; }	/* for Windows pseudo-files */
	str = ctime(&buf.st_ctime);
	Cstring_To_Prolog(str, val);
	Return_Unify_String(vv, vt, val.ptr);

//asq:
/*
#ifndef _WIN32

    case 15:
    {
	struct passwd	*pwd;
	pwd = getpwuid(buf.st_uid);
	if (!pwd) {
	    Fail_;
	}
	endpwent();
	Cstring_To_Prolog(pwd->pw_name, val);
	Return_Unify_String(vv, vt, val.ptr);
    }

    case 16:
    {
	struct group	*grp;
	grp = getgrgid(buf.st_gid);
	if (!grp) {
	    Fail_;
	}
	endgrent();
	Cstring_To_Prolog(grp->gr_name, val);
	Return_Unify_String(vv, vt, val.ptr);
    }
#endif
*/
    case 17:
	acc = R_OK;
	goto _access_;

    case 18:
	acc = W_OK;
	goto _access_;

    case 19:
	acc = X_OK;
_access_:
	if (!ec_access(name, acc)) {
	    Return_Unify_Atom(vv, vt, d_.on)
	} else {
	    errno = 0;
	    Return_Unify_Atom(vv, vt, d_.off)
	}

    default:
	Fail_;
    }
}
