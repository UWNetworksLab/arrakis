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

/*----------------------------------------------------------------------*
 * ECLiPSe system
 *
 * IDENTIFICATION:	os_support.c
 *
 * $Id: os_support.c,v 1.5 2008/07/23 20:37:09 kish_shen Exp $
 *
 * AUTHOR:		Joachim Schimpf, IC-Parc
 *
 * DESCRIPTION:		Operating-system services abstraction layer
 *		
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
 * Config & Prototypes only.
 * Do not include any ECLiPSe-specific stuff in this file!
 *----------------------------------------------------------------------*/
#include <unistd.h>
#include <assert.h>
#include "times.h"

#include "config.h"
#include "os_support.h"

/*----------------------------------------------------------------------*
 * OS-includes
 *----------------------------------------------------------------------*/

#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <stdint.h>
#include <stdio.h>
#include <math.h>	/* for floor() */
#include <ctype.h>	/* for toupper() etc */

#ifdef _WIN32
/* FILETIMEs are in 100 nanosecond units */
#define FileTimeToDouble(t) ((double) (t).dwHighDateTime * 429.4967296 \
			    + (double) (t).dwLowDateTime * 1e-7)
#include <io.h>		/* for _access(),... */
#include <process.h>	/* for _getpid() */
#include <direct.h>	/* for _getcwd() */
#include <sys/timeb.h>	/* for _fstat() */
#include <sys/stat.h>	/* for struct _stat */
#include <conio.h>	/* for _getch */
#include <windows.h>
#else
#include <sys/time.h>
//#include <pwd.h>
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#else
extern char *getenv();
#endif

#ifdef HAVE_NETDB_H
#include <netdb.h>      /* for gethostbyname() */
#endif

#ifdef HAVE_SYS_SYSTEMINFO_H
#include <sys/systeminfo.h> /* for sysinfo() */
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>	/* for uname() */
#endif

#ifdef HAVE_TIMES
#include <sys/times.h>
#endif

#ifdef BSD_TIMES
#include <sys/timeb.h>
#endif

#ifdef HAVE_GETCWD
#include <signal.h>
#endif

#ifdef HAVE_SIOCGIFHWADDR
#include <sys/ioctl.h>
#include <net/if.h>
#endif

/*----------------------------------------------------------------------*
 * Global variables
 *----------------------------------------------------------------------*/

/*
 * ECLiPSe version number. This is here because it is needed for
 * ec_env_lookup() and various executables.
 */
char		ec_version[] = PACKAGE_VERSION;

/*
 * the time when the system was started
 */
#ifdef _WIN32
static double	start_time;	/* in seconds */
#else
#ifdef BSD_TIMES
static time_t	start_time;	/* in seconds */
#else
static clock_t	start_time;	/* in clock ticks */
#endif
#endif

/*
 * the resolution of the system clock
 */
int		clock_hz;

/*
 * Error numbers of OS-errors
 */
int		ec_os_errno_;	/* the operating system error number */
int		ec_os_errgrp_;	/* which group of error numbers it is from */


/*----------------------------------------------------------------------*
 * Initialisation
 *----------------------------------------------------------------------*/

void
ec_os_init(void)
{

#ifdef _WIN32
    DWORD now;
    FILETIME now_time;
    GetSystemTimeAsFileTime(&now_time);
    start_time = FileTimeToDouble(now_time);
    now = now_time.dwLowDateTime;
    clock_hz = CLOCKS_PER_SEC;

#else /* UNIX */
    time_t now = time((time_t *) 0);

//asq:
    start_time = 0;
/*
#ifdef BSD_TIMES
    start_time = now;		// init startup time
#else
    struct tms dummy;
    start_time = times(&dummy);	// init startup time
#endif
*/

#ifdef HAVE_SYSCONF
    clock_hz = sysconf(_SC_CLK_TCK);
#else
#ifdef CLOCK_HZ
    clock_hz = CLOCK_HZ;
#else
    clock_hz = 60;
#endif
#endif

    /* On SUNOS 4.0 the first call to ctime() takes much longer than the
     * subsequent ones since a shared file is opened (see tzsetup(8)).
     * Therefore we call it here once.
     */
    (void) ctime(&now);

#endif

#ifdef _WIN32
    {
	WSADATA wsa_data;
	(void) WSAStartup(MAKEWORD(2,0), &wsa_data);	/* init Winsock */
    }
#endif
}


void
ec_os_fini(void)
{
#ifdef _WIN32
    ec_terminate_alarm();	/* terminate alarm thread, if any */
    (void) WSACleanup();	/* finalise Winsock (once per WSAStartup()) */
#endif
}


/*----------------------------------------------------------------------*
 * Simple wrappers for Windows -> Unix
 *----------------------------------------------------------------------*/

#ifdef _WIN32

#ifndef R_OK
#define R_OK	4
#define W_OK	2
#define X_OK	1
#endif

int
ec_access(char *name, int amode)
{
    /* CAUTION: Windows _access() knows only R_OK and W_OK.
     * http://msdn.microsoft.com/en-us/library/1w06ktdy.aspx
     * To simulate X_OK check we use _stat() and check for _S_IEXEC mode,
     * but even that is fake: it is set when the file name has a .exe
     * extension, and is always set for directories.
     * A cleaner implementation would probably use AccessCheck(),
     * OpenThreadToken(), etc.
     */
    char winname[MAX_PATH_LEN];

    os_filename(name, winname);
    if (_access(winname, amode & (R_OK|W_OK)))
	return -1;
    if (amode & X_OK)
    {
	struct _stat buf;
	if (_stat(winname, &buf))
	    return -1;
	if (!(buf.st_mode & _S_IEXEC))
	    return -1;
    }
    return 0;
}

int
getpid(void)
{
    return _getpid();
}

int
ec_chdir(char *name)
{
    char winname[MAX_PATH_LEN];
    return _chdir(os_filename(name, winname));
}

int
ec_stat(char *name, struct_stat *buf)
{
    char winname[MAX_PATH_LEN];
    return _stat(os_filename(name, winname), (struct _stat *) buf);
}

#ifndef __GNUC__
int
fstat(int handle, struct_stat *buf)
{
    return _fstat(handle, buf);
}
#endif

int
ec_rmdir(char *name)
{
    char winname[MAX_PATH_LEN];
    return _rmdir(os_filename(name, winname));
}

int
ec_mkdir(char *name, int mode)
{
    char winname[MAX_PATH_LEN];
    return _mkdir(os_filename(name, winname));
}

int
ec_unlink(char *name)
{
    char winname[MAX_PATH_LEN];
    return _unlink(os_filename(name, winname));
}

#ifndef __GNUC__
int
lseek(int handle, long offset, int whence)
{
    return _lseek(handle, offset, whence);
}
#endif

#ifndef __GNUC__
int
putenv(char *envstring)
{
    return _putenv(envstring);
}
#endif

int
isatty(int handle)
{
    return _isatty(handle);
}

int
isascii(int c)
{
    return __isascii(c);
}

int
ec_open(const char *name, int oflag, int pmode)
{
    char winname[MAX_PATH_LEN];
    return _open(os_filename((char *) name, winname), oflag|_O_BINARY, pmode);
}

int
dup(int handle)
{
    return _dup(handle);
}

int
dup2(int h1, int h2)
{
    return _dup2(h1, h2);
}

int
close(int handle)
{
    return _close(handle);
}

int
read(int handle, void *buf, unsigned int count)
{
    return _read(handle, buf, count);
}

int
write(int handle, const void *buf, unsigned int count)
{
    return _write(handle, buf, count);
}

int
pipe(int *fd)
{
    /* _O_NOINHERIT is important for pipes used in exec */
    return _pipe(fd, 4096, _O_BINARY|_O_NOINHERIT);
}

int
getpagesize(void)
{
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    return (int) info.dwPageSize;
}
#endif

int
ec_rename(char *old, char *new)
{
    char winold[MAX_PATH_LEN];
    char winnew[MAX_PATH_LEN];
    return rename(os_filename(old, winold), os_filename(new, winnew));
}



/*----------------------------------------------------------------------*
 * Filename conversions
 *----------------------------------------------------------------------*/

/*
 * char *expand_filename(in, out)
 *
 * expand ~, ~user and $VAR at the beginning of the filename.
 * out should point to a buffer of length MAX_PATH_LEN.
 * In addition, it does some editing of the filename to remove unneeded
 * characters at the start of all path components: extra /, ./ are 
 * removed.
 * The return value is a pointer to the expanded filename in out[].
 * It can be used like
 *
 *	char buf[MAX_PATH_LEN];
 *	name = expand_filename(name, buf);
 *
 * No errors are returned. When there was a problem, we just
 * return a copy of the original string.
 * The result is truncated to MAX_PATH_LEN, without warning!
 */

#define Str_Cpy(to, from, to_last) \
	{ while(*(from) && (to)<(to_last)) *(to)++ = *(from)++; }
#define Str_Cpy_Until(to, from, delim, to_last) \
	{ while(*(from) && *(from) != (delim) && (to) < (to_last)) *(to)++ = *(from)++; }

char *
expand_filename(char *in, char *out)
{
    register char *auxp, *inp = in, *outp = out;
    char *dir = (char *) 0;
    char aux[MAX_PATH_LEN];
    char *aux_last = &aux[MAX_PATH_LEN-1];
    char *out_last = outp+MAX_PATH_LEN-1;

    switch(*inp)
    {
    case '/':
	if (*++inp == '/')
	{ /* preserve leading '//' for WIN32 */
	    dir = aux;
	    strcpy(dir, "/\0");
	}
	break;
    case '~':
	if (*++inp == '/' || *inp == '\0')
	{
	    dir = getenv("HOME");
	    if (!dir)
		dir = getenv("HOMEPATH");	/* WinNT */
	    if (dir) /* make sure it is in ECLiPSe format */
		dir = canonical_filename(dir, aux);  
	}
/*
#ifndef _WIN32
	else
	{
	    struct passwd *pass;
	    auxp = aux;
	    Str_Cpy_Until(auxp, inp, '/', aux_last);
	    *auxp = '\0';
	    if (pass = getpwnam(aux))
		dir = pass->pw_dir;
	}
#endif
*/
	break;
    case '$':
	++inp;
	auxp = aux;
	Str_Cpy_Until(auxp, inp, '/', aux_last);
	*auxp = '\0';
	dir = getenv(aux);
	if (dir) /* make sure it is in ECLiPSe format */
	    dir = canonical_filename(dir, aux); 
	break;
    }
    if (dir)
    {
	Str_Cpy(outp, dir, out_last);
    }
    else	/* directory could not be expanded */
	inp = in;

    while(*inp && outp < out_last)
    {
	Str_Cpy_Until(outp, inp, '/', out_last);
	if (*inp == '/' && outp < out_last)
	{
	    *outp++ = *inp++; /* copy the / */
	    while(*inp)  /* edit the path following a / */
	    {
		switch(*inp)
		{
		case '.':
		    if (!(*(inp+1) == '/' || *(inp+1) == '\0')) break;
		case '/':
		    ++inp;
		    continue;
		}
		break; /* exit inner while(*inp) */
	    }
	}
    }
    *outp = '\0';
    return out;
}


char *
canonical_filename(char *in, char *out)
{
#ifdef _WIN32
    char *s = in;
    char *t = out;
    for (;;)
    {
	if (*s == '\0' || *s == '\\' || *s == '/')
	{
	    s = in;		/* no drive letter */
	    break;
	}
	if (*s == ':')		/* copy drive name */
	{
	    *t++ = '/'; 
	    *t++ = '/'; 
	    while (in < s)
	    	*t++ = *in++;
	    ++s;
	    if (*s != '\\' && *s != '/')
		*t++ = '/'; 	/* no separator, insert one */
	    break;
	}
	++s;
    }
    while (*s)			/* copy, replacing \ with / */
    {
	*t = (*s == '\\') ? '/' : *s;
	++s; ++t;
    }

    *t = '\0';
    return out;
#else
    return strcpy(out, in);
#endif
}

char *
os_filename(char *in, char *out)
{
#ifdef _WIN32
    char *eos;
    char *t = out;

    /* interpret //? as a drive name and treat specially */
    if (in[0] == '/' && in[1] == '/' && in[2] != 0 && (in[3] == '/' || in[3] == 0))
    {
	*t++ = in[2];			/* copy drive letter */
	*t++ = ':';			/* followed by : */
	*t++ = '\\';
	if (in[3]==0 || in[4]==0)
	{
	    /* special case //D[/] -> D:\. rather than simply D:  (bug 465) */
	    *t++ = '.'; *t = '\0';
	    return out;
	}
	in += 4;
    }
    else if (*in == '/')		/* one or two non-trimmable slashes */
    {
	*t++ = '\\'; ++in;
	if (*in == '/')
	{
	    *t++ = '\\'; ++in;
	}
    }
    eos = t;
    while (*in)				/* copy rest of path */
    {
	if (*in == '/')
	{
	    *t++ = '\\';
	    ++in;
	}
	else
	{
	    *t++ = *in++;
	    eos = t;			/* to remove trailing slashes */
	}
    }
    *eos = '\0';
    return out;
#else
    return strcpy(out, in);
#endif
}


/*----------------------------------------------------------------------*
 * Directories
 *----------------------------------------------------------------------*/

/*
* For the sps7 there is a special getwd() code. It works on other
* machines as well, however it is not necessary to duplicate everything.
*/
#if !defined(_WIN32) && !defined(HAVE_GETCWD) && !defined(HAVE_GETWD)
#include "getwd.c"
#endif

/*
* Get the current working directory (unix) into a buffer
* and add a trailing "/". If something went wrong, return "./".
* The return code is the string length.
* Different code is needed for different operating systems.
*/

/*ARGSUSED*/
int
get_cwd(char *buf, int size)
{
    char	*s;
    int		len;
    char	buf1[MAX_PATH_LEN];

#ifdef _WIN32
    char	buf2[MAX_PATH_LEN];
    s = _getcwd(buf1, MAX_PATH_LEN);
    /* Make sure drive letter is upper case (bug under cygwin) */
    if (islower(buf1[0]) && buf1[1] == ':')
    	buf1[0] = toupper(buf1[0]);
#if _WIN32_WINNT > 0x400
    /* Get `normalised' path with correct cases for characters in path.
       GetLongPathName() is supported only by Windows NT > 4 
       XP seems to require a call to GetShortPathName(), otherwise
       GetLongPathName() does not always behave correctly.
    */ 
    len = GetShortPathName(buf1, buf2, MAX_PATH_LEN);
    if (len > 0) 
    {
	len = GetLongPathName(buf2, buf1, MAX_PATH_LEN);
	if (len == 0) s = _getcwd(buf1, MAX_PATH_LEN);
    }
#endif
#else
#ifdef HAVE_GETCWD
    /* Signal blocking here is to work around a bug that occurred
     * on Suns when the profiler signal interupts getcwd()
     */
# ifdef HAVE_SIGPROCMASK
    sigset_t old_mask, block_mask;
    (void) sigemptyset(&block_mask);
    (void) sigaddset(&block_mask, SIGPROF);
    (void) sigprocmask(SIG_BLOCK, &block_mask, &old_mask);
# else
#  ifdef HAVE_SIGVEC
    int old_mask = sigblock(sigmask(SIGPROF));
#  endif
# endif
    s = getcwd(buf1, (size_t) MAX_PATH_LEN);
# ifdef HAVE_SIGPROCMASK
    (void) sigprocmask(SIG_SETMASK, &old_mask, (sigset_t *) 0);
# else
#  ifdef HAVE_SIGVEC
    (void) sigsetmask(old_mask);
#  endif
# endif
#else
//asq:
//    s = getwd(buf1);	/* system or our own definition from getwd.c */
    s = getcwd(buf1, sizeof(buf1));

#endif
#endif
    if (s == 0) {	/* return local path if something went wrong */
	errno = 0;
	buf[0] = '.';
	buf[1] = '/';
	buf[2] = '\0';
	return 2;
    }
    len = strlen(canonical_filename(buf1, buf));
    if (buf[len-1] != '/')	/*  add trailing / if needed */
    {
	buf[len++] = '/';
	buf[len] = '\0';
    }
    return len;
}

/*----------------------------------------------------------------------*
 * dlopen
 *----------------------------------------------------------------------*/

/* use the dlopen compatibility code for MacOSX */
#if !defined(HAVE_DLOPEN) && defined(HAVE_MACH_O_DYLD_H)
#include "dlfcn_simple.c"
#endif

/*----------------------------------------------------------------------*
 * Times
 *----------------------------------------------------------------------*/

/*
 * User CPU time in clock ticks.
 * The elapsed time in seconds can be computed as
 *		user_time() / clock_hz
 */
long
user_time(void)
{
#ifdef _WIN32
    FILETIME creation_time, exit_time, kernel_time, user_time;
    LARGE_INTEGER li;

    if (GetProcessTimes(GetCurrentProcess(),
    		&creation_time, &exit_time, &kernel_time, &user_time))
    {
	li.LowPart = user_time.dwLowDateTime;
	li.HighPart = user_time.dwHighDateTime;
	return (long) (li.QuadPart / (10000000/CLOCKS_PER_SEC));
    }
    else
    {
	return (long) clock();
    }
#else
#if defined(HAVE_TIMES)
	struct tms      rusag;

	(void) times(&rusag);
	return rusag.tms_utime;
#else
	/* try at least with the real time */
	return (long) time((time_t *) 0);
#endif
#endif
}


/*
 * Time in seconds since birth of UNIX
 */
long
ec_unix_time(void)
{
    return (long) time((time_t *) 0);
}


/*
 * User-CPU, System-CPU and Elapsed time for this Eclipse process as floats
 */
int
all_times(double *user, double *sys, double *elapsed)	/* in seconds */
{

#ifdef _WIN32

    FILETIME creation_time, exit_time, kernel_time, user_time, now_time;

    if (GetProcessTimes(GetCurrentProcess(),
    		&creation_time, &exit_time, &kernel_time, &user_time))
    {
	*user = FileTimeToDouble(user_time);
	*sys = FileTimeToDouble(kernel_time);
    }
    else
    {
	*user = ((double) clock() / CLOCKS_PER_SEC);
	*sys = 0L;
    }
    GetSystemTimeAsFileTime(&now_time);
    *elapsed = FileTimeToDouble(now_time) - start_time;

#else /* UNIX */

    struct tms		rusag;
#ifdef BSD_TIMES
    struct timeb	realtime;
    /* times() returns nothing useful in BSD, need ftime() for elapsed time */
    (void) ftime(&realtime);
    if (times(&rusag) == -1)
    {
      return(-1);
    }
    *elapsed = (realtime.time - start_time) + (double)realtime.millitm/1000.0;
#else
    clock_t		realtime;
//asq:
//    if ((realtime = times(&rusag)) == (clock_t) -1)
    if(0 == 0)
    {
      return(-1);
    }
    *elapsed = (double) (realtime - start_time) / clock_hz;
#endif

    *user = (double) rusag.tms_utime / clock_hz;
    *sys = (double) rusag.tms_stime / clock_hz;

#endif
    return 0;
}


char *
ec_date_string(char *buf)
{
    time_t ti = (long) time((time_t *) 0);
    strcpy(buf, ctime(&ti));
    return buf;
}


/*----------------------------------------------------------------------*
 * Other system services
 *----------------------------------------------------------------------*/


int
ec_gethostid(char *buf, int len)
{

#ifdef _WIN32

    /*
     * This code taken from
     * http://support.microsoft.com/kb/q118623/
     * It gets (an) Ethernet adapter hardware address, same as
     * ioctl(..., SIOCGIFHWADDR, ...) in Linux.
     */

    typedef struct _ASTAT_
    {
        ADAPTER_STATUS adapt;
        NAME_BUFFER    NameBuff [30];
    } ASTAT;

    NCB		Ncb;
    ASTAT	Adapter;
    LANA_ENUM   lenum;
    int 	i;

    memset( &Ncb, 0, sizeof(Ncb) );
    Ncb.ncb_command = NCBENUM;
    Ncb.ncb_buffer = (UCHAR *)&lenum;
    Ncb.ncb_length = sizeof(lenum);
    if (Netbios( &Ncb ) != 0)
	return -1;

    for(i=0; i < lenum.length ;i++)
    {
	memset( &Ncb, 0, sizeof(Ncb) );
	Ncb.ncb_command = NCBRESET;
	Ncb.ncb_lana_num = lenum.lana[i];
	if (Netbios( &Ncb ) != 0)
	    return -1;

	memset( &Ncb, 0, sizeof (Ncb) );
	Ncb.ncb_command = NCBASTAT;
	Ncb.ncb_lana_num = lenum.lana[i];
	strcpy( Ncb.ncb_callname,  "*               " );
	Ncb.ncb_buffer = (char *) &Adapter;
	Ncb.ncb_length = sizeof(Adapter);
	if (Netbios( &Ncb ) != 0)
	    return -1;

	sprintf(buf, "A#%02x%02x%02x%02x%02x%02x",
	      Adapter.adapt.adapter_address[0],
	      Adapter.adapt.adapter_address[1],
	      Adapter.adapt.adapter_address[2],
	      Adapter.adapt.adapter_address[3],
	      Adapter.adapt.adapter_address[4],
	      Adapter.adapt.adapter_address[5]);

	/* return the first one we can get hold of */
	break;
    }

#else

#if defined(HAVE_SYSINFO) && defined(HAVE_SYS_SYSTEMINFO_H)

    char *bufp = buf;
    if (sysinfo(SI_HW_PROVIDER, buf, len) == -1) {
	ec_os_errgrp_ = ERRNO_UNIX; ec_os_errno_ = errno; errno = 0;
	return -1;
    }
    bufp = buf + strlen(buf);
    *bufp++ = '#';
    if (sysinfo(SI_HW_SERIAL, bufp, len-(bufp-buf)) == -1) {
	ec_os_errgrp_ = ERRNO_UNIX; ec_os_errno_ = errno; errno = 0;
	return -1;
    }

#else
#ifdef HAVE_SIOCGIFHWADDR

    /*
     * This gets (an) Ethernet adapter hardware address
     */

    int sockFd;
    struct ifreq req;
    struct sockaddr_in *sin;

#ifndef IPPROTO_IP
#define IPPROTO_IP 0
#endif

    sockFd = socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);

    memset(&req, 0, sizeof(struct ifreq));
    strcpy(req.ifr_name, "eth0");

    if (ioctl(sockFd, SIOCGIFHWADDR ,&req) == 0)
    {
	sprintf(buf, "L#%02x%02x%02x%02x%02x%02x",
	    req.ifr_hwaddr.sa_data[0]&0xff, req.ifr_hwaddr.sa_data[1]&0xff,
	    req.ifr_hwaddr.sa_data[2]&0xff, req.ifr_hwaddr.sa_data[3]&0xff,
	    req.ifr_hwaddr.sa_data[4]&0xff, req.ifr_hwaddr.sa_data[5]&0xff);
    }
    else /* use gethostid() if it didn't work */
    {
	(void) sprintf(buf, "H#%d", gethostid());
    }
    close(sockFd);


#else

    (void) sprintf(buf, "H#%ld", gethostid());

#endif
#endif
#endif

    return strlen(buf);
}


int
ec_gethostname(char *buf, int size)	/* sets ec_os_errno_/errgrp_ on failure */
{
    int i;
    struct hostent *hp;
//asq:
    strcpy(buf, "barrelfish");
    return(strlen(buf));
/*

#if defined(HAVE_GETHOSTNAME)
    if (gethostname(buf, size)) {
# ifdef _WIN32
	ec_os_errgrp_ = ERRNO_WIN32; ec_os_errno_ = GetLastError();
# else
	ec_os_errgrp_ = ERRNO_UNIX; ec_os_errno_ = errno; errno = 0;
# endif
	return -1;
    }
#else
# if defined(HAVE_SYSINFO) && defined(HAVE_SYS_SYSTEMINFO_H)
// Linux has sysinfo(), but not same interface or sys/systeminfo.h
    if (sysinfo(SI_HOSTNAME, buf, size) == -1) {
	ec_os_errgrp_ = ERRNO_UNIX; ec_os_errno_ = errno; errno = 0;
	return -1;
    }
# else
    // assume uname is defined
    struct utsname utsn;

    if (uname(&utsn) <= -1) {
#  ifdef _WIN32
	ec_os_errgrp_ = ERRNO_WIN32; ec_os_errno_ = GetLastError();
#  else
	ec_os_errgrp_ = ERRNO_UNIX; ec_os_errno_ = errno; errno = 0;
#  endif
	return -1;
    }
    strncpy(buf, utsn.nodename, size);
# endif
#endif 

#ifdef sun4_0
//  There is a bug with the Sun4 static C library version of gethostbyname():
//  it seems to strip the full hostname (with dots) and return the single
//  component (without dots), rather than the other way round
//
    hp = NULL;
#else
    hp = gethostbyname(buf);
#endif
    if (hp != NULL) {
      strncpy(buf, hp->h_name, size);
    }

    return strlen(buf);
*/
}


void
ec_sleep(double seconds)
{
#ifdef _WIN32
    (void) SleepEx((DWORD) (seconds*1000.0), TRUE);
#else
#ifdef HAVE_SELECT
    struct timeval	sleep_time;
    fd_set		rs, ws, es;

    sleep_time.tv_sec = (long) seconds;
    sleep_time.tv_usec = (long) ((seconds - floor(seconds)) * 1000000.0);
    FD_ZERO(&rs);
    FD_ZERO(&ws);
    FD_ZERO(&es);
    (void) select(0, &rs, &ws, &es, &sleep_time);
#else
#ifdef HAVE_SLEEP
    (void) sleep((unsigned) seconds);
#endif
#endif
#endif
}

void
ec_bad_exit(char *msg)
{
#ifdef _WIN32
    FatalAppExit(0, msg);
#else
    (void) write(2, msg, strlen(msg));
    (void) write(2, "\n", 1);
    exit(-1);
#endif
}

#ifndef HAVE_STRERROR
char *
strerror(int n)
{
    extern int	sys_nerr;
    extern char	*sys_errlist[];
    if (n < 0 || n >= sys_nerr)
    	return (char *) 0;
    return sys_errlist[n];
}
#endif

char *
ec_os_err_string(int err, int grp, char *buf, int size)
{
#ifdef _WIN32
    switch (grp)
    {
    case ERRNO_WIN32:
	if (!FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, err,
		0, (LPTSTR) buf, size, NULL))
	{
	    sprintf(buf, "Windows error %d", err);
	}
	return buf;

    case ERRNO_UNIX:
#endif
	if (err == 0)
	    return "";
	else
	{
	    char * message = strerror(err);
	    if (message)
	        return message;
	    sprintf(buf, "Unix error %d", err);
	    return buf;
	}
#ifdef _WIN32
    }
#endif
}

#ifdef _WIN32
int
ec_getch_raw(int unit)
{
    return _getch();
}

int
ec_putch_raw(int c)
{
    _putch(c);
    return 0;
}
#endif

//asq:
int
ec_getch_raw(int unit)
{
    return getchar();
}

int
ec_putch_raw(int c)
{
    putchar(c);
    return 0;
}


/*
 * On SunOS4 with gcc version 2.95.3 20010315 (release) at least, strcmp()
 * seems to have a bug and accesses memory beyond the end of the string s2.
 * This causes segmentation violations when the string happens to be right
 * at the end of mapped memory.
 */

#ifdef sun4_0
int strcmp(char *s1, char *s2)
{
    while (*s1 == *s2) {
	if (!*s1) return 0;
    	++s1; ++s2;
    }
    return *s1 - *s2;
}
#endif


/*----------------------------------------------------------------------
 * Simple thread interface
 *----------------------------------------------------------------------*/

#ifdef _WIN32

typedef struct {
    HANDLE thread_handle;

    /* this event signals that a function and data has been supplied */
    HANDLE start_event;

    /* this event signals that the function has terminated */
    HANDLE done_event;

    /* the function to execute, NULL signals termination request */
    int (* volatile fun)(void *);

    /* argument for the function call: valid iff fun!=NULL */
    void * volatile data;

    /* result of the function call: valid iff fun==NULL */
    int volatile result;

} thread_data;


/* The general thread procedure */

static
DWORD WINAPI
ec_fun_thread(thread_data *desc)
{
    for(;;)
    {
	DWORD res;

	/* wait for a SetEvent() */
	res = WaitForSingleObject(desc->start_event, INFINITE);
	if (res == WAIT_FAILED)
	    ec_bad_exit("ECLiPSe: thread wait failed");

	if (!desc->fun)		/* no function = termination request */
	    ExitThread(1);
	
	desc->result = (*desc->fun)(desc->data);	/* run it ...	*/
	desc->fun = NULL;		/* ... and signal stopping	*/

	if (!SetEvent(((thread_data*)desc)->done_event))
	{
	    ec_bad_exit("ECLiPSe: thread stopping signalling failed");
	}
    }
    return 1;
}

void *
ec_make_thread(void)
{
    DWORD thread_id;
    thread_data *desc = malloc(sizeof(thread_data));

    desc->fun = NULL;
    desc->start_event = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (!desc->start_event)
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return NULL;
    }
    desc->done_event = CreateEvent(NULL, TRUE, FALSE, NULL);
    if (!desc->done_event)
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return NULL;
    }
    desc->thread_handle = CreateThread(NULL, 0,
	(LPTHREAD_START_ROUTINE) ec_fun_thread,
	(LPVOID) desc, 0, &thread_id);
    if (!desc->thread_handle)
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return NULL;
    }
#if 0
    /*
     * The following line should not be needed, but we had problems
     * with the thread not being started otherwise - Windows bug?
     */
    ResumeThread(desc->thread_handle);
#endif
    return (void *) desc;
}


/*
 * Test whether the thread has finished (without waiting).
 * Returns: 1 if thread stopped, 0 otherwise
 * If stopped, *result contains the result of the thread computation>
 */

int
ec_thread_stopped(void *desc, int *result)
{
    if (!((thread_data*)desc)->fun)
    {
	*result = ((thread_data*)desc)->result;
	return 1;
    }
    return 0;
}


/*
 * Wait for the thread to finish (max timeout milliseconds)
 * timeout == -1:	wait indefinitely
 * timeout == 0:	don't wait
 * timeout > 0:		wait timeout milliseconds
 * Returns: 1 if thread stopped, 0 otherwise
 * If stopped, *result contains the result of the thread computation>
 */

int
ec_thread_wait(void *desc, int *result, int timeout)
{
    if (timeout != 0)
    {
	switch(WaitForSingleObject(((thread_data*)desc)->done_event,
		    timeout > 0 ? (DWORD) timeout : INFINITE))
	{
	case WAIT_OBJECT_0:	/* stopping was signalled */
	case WAIT_TIMEOUT:	/* timeout occurred */
	    break;

	default:
	    ec_bad_exit("ECLiPSe: thread wait failed");
	}
    }
    return ec_thread_stopped(desc, result);
}

int
ec_start_thread(void *desc, int (*fun)(void *), void *data)
{
    if (((thread_data*)desc)->fun)
    	return 0;	/* thread still busy */

    if (!ResetEvent(((thread_data*)desc)->done_event))
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return 0;
    }
    ((thread_data*)desc)->data = data;
    ((thread_data*)desc)->fun = fun;
    if (!SetEvent(((thread_data*)desc)->start_event))
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return 0;
    }
    return 1;
}

/*
 * Terminate the thread. This should only be done when already stopped.
 * Returns: 1 if cleanly terminated, 0 if forcibly terminated, -1 error
 */
int
ec_thread_terminate(void *desc, int timeout)
{
    int result;
    DWORD thread_exit_code = 0;

    if (ec_thread_stopped(desc, &result))
    {
	/* restart the thread with NULL function: leads to termination */
	((thread_data*)desc)->fun = 0;
	if (!SetEvent(((thread_data*)desc)->start_event))
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    return -1;
	}
    }
    else
    {
	/* still running, terminate forcibly */
	if (!TerminateThread(((thread_data*)desc)->thread_handle, 0))
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    return -1;
	}
    }
    switch(WaitForSingleObject(((thread_data*)desc)->thread_handle, timeout > 0 ? (DWORD)timeout : INFINITE))
    {
    case WAIT_OBJECT_0:	/* termination was signalled */
	if (!GetExitCodeThread(((thread_data*)desc)->thread_handle, &thread_exit_code))
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    return -1;
	}
	/*thread_exit_code is 0 or 1 */
	break;

    default:
    case WAIT_TIMEOUT:	/* timeout occurred, terminate forcibly */
	if (!TerminateThread(((thread_data*)desc)->thread_handle, 0))
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    return -1;
	}
	/* thread_exit_code is 0, don't bother to wait */
	break;

    case WAIT_FAILED:
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return -1;
    }

    if (!CloseHandle(((thread_data*)desc)->thread_handle) ||
	!CloseHandle(((thread_data*)desc)->start_event) ||
	!CloseHandle(((thread_data*)desc)->done_event))
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return -1;
    }
    free(desc);
    return thread_exit_code;	/* 0 or 1 */
}

#endif


/*----------------------------------------------------------------------
 * Timers
 *----------------------------------------------------------------------*/
#ifdef _WIN32

/*
 * Windows doesn't seem to have timers that you can ask for the
 * remaining time. We therefore store the due time ourselves.
 * The DWORDs are all in milliseconds, the LONGLONG is 100 ns units.
 */
typedef struct {
    HANDLE thread_handle;		/* must be first */

/* signals that the new_xxx settings are valid and should be accepted */
    HANDLE time_set_event;

/* signals that new settings have been accepted and old_xxx are valid */
    HANDLE time_accept_event;

/* input (w for main, r for thread) */
    DWORD new_first;			/* first interval (ms) */
    DWORD new_ivl;			/* future intervals (ms) */
    void (*new_callback)(long);		/* callback function ... */
    long new_cb_arg;			/* ... and its argument */
    int terminate_req;			/* request to terminate alarm thread */

/* output ( w for thread, r for main) */
    DWORD old_remain;			/* remaining time when stopped (ms) */
    DWORD old_ivl;			/* old interval setting (ms) */

/* status (w for thread, r for main) */
    int running;			/* set while timer running */

/* local (r/w for thread) */
    LONGLONG active_due;		/* current due time (100 ns FILETIME) */
    DWORD active_ivl;			/* current future intervals (ms) */
    void (*active_callback)(long);	/* current callback function ... */
    long active_cb_arg;			/* ... and its argument */

} timer_thread;

volatile timer_thread alarm_thread = { /*thread_handle*/ NULL };


/* The timer thread procedure */

DWORD WINAPI
ec_alarm_thread(timer_thread *desc)
{
    DWORD next_timeout = INFINITE;

    for(;;)
    {
	DWORD res;
	FILETIME now_time_ft;
	LARGE_INTEGER now_time;

	res = WaitForSingleObject(desc->time_set_event, next_timeout);
	if (res == WAIT_FAILED)
	{
	    ec_bad_exit("ECLiPSe: timer thread WaitForSingleObject() failed");
	}

	GetSystemTimeAsFileTime(&now_time_ft);
	now_time.LowPart = now_time_ft.dwLowDateTime;
	now_time.HighPart = now_time_ft.dwHighDateTime;

	/*
	 * Check whether the active timer has expired and do the callback
	 * if so. Then either cancel or schedule the next interval.
	 */
	if (desc->running)
	{
	    if (now_time.QuadPart >= desc->active_due)
	    {
		/* the alarm is due */
		(*desc->active_callback)(desc->active_cb_arg);
		if (desc->active_ivl)
		{
		    /* schedule the next interval */
		    desc->active_due = now_time.QuadPart + (LONGLONG)desc->active_ivl*10000;
		    desc->running = 1;
		    next_timeout = desc->active_ivl;
		}
		else
		{
		    /* nothing more to do */
		    desc->active_due = 0;
		    desc->active_ivl = 0;
		    desc->running = 0;
		    next_timeout = INFINITE;
		}
	    }
	    else  /* timed out too early, or SetEvent */
	    {
		desc->running = 1;
		next_timeout = (desc->active_due - now_time.QuadPart)/10000;
		if (next_timeout == 0) next_timeout = 1;
	    }
	}

	/*
	 * If we had a timer_set_event, pick up
	 * the new times and return the old ones.
	 */
	if (res == WAIT_OBJECT_0)
	{
	    if (desc->terminate_req)
	    {
		break;		/* same as ExitThread(1); */
	    }
	    else
	    {
		desc->old_remain = desc->running ? next_timeout : 0;
		desc->old_ivl = desc->active_ivl;

		if (desc->new_first)		/* change settings */
		{
		    desc->active_due = now_time.QuadPart + (LONGLONG)desc->new_first*10000;
		    desc->active_ivl = desc->new_ivl;
		    desc->active_callback = desc->new_callback;
		    desc->active_cb_arg = desc->new_cb_arg;
		    desc->running = 1;
		    next_timeout = desc->new_first;
		}
		else				/* clear settings */
		{
		    desc->active_due = 0;
		    desc->active_ivl = 0;
		    desc->running = 0;
		    next_timeout = INFINITE;
		}
		/* indicate acceptance */
		if (!SetEvent(alarm_thread.time_accept_event))
		{
		    ec_bad_exit("ECLiPSe: timer thread SetEvent() failed");
		}
	    }
	}
    }
    return 1;
}


int
ec_set_alarm(
	double first,			/* new initial interval (0: stop timer) */
	double interv,			/* new periodic interval (0: one shot) */
	void (*callback)(long),		/* callback function ... */
	long cb_arg,			/* ... and its argument */
	double *premain,		/* return time to next timeout */
	double *old_interv)		/* return previous interval setting */
{
    if (!alarm_thread.thread_handle)	/* create thread if not yet there */
    {
	DWORD thread_id;
	alarm_thread.terminate_req = 0;
	alarm_thread.running = 0;
	alarm_thread.time_set_event = CreateEvent(NULL, FALSE, FALSE, NULL);
	if (!alarm_thread.time_set_event)
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    return 0;
	}
	alarm_thread.time_accept_event = CreateEvent(NULL, FALSE, FALSE, NULL);
	if (!alarm_thread.time_accept_event)
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    return 0;
	}
	alarm_thread.thread_handle = CreateThread(NULL, 0,
	    (LPTHREAD_START_ROUTINE) ec_alarm_thread,
	    (LPVOID) &alarm_thread, 0, &thread_id);
	if (!alarm_thread.thread_handle)
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    return 0;
	}
    }

    /* write new parameters into the descriptor */
    alarm_thread.new_first = (DWORD) (first*1000.0);
    if (alarm_thread.new_first==0 && first>0.0) alarm_thread.new_first = 1;
    alarm_thread.new_ivl = (DWORD) (interv*1000.0);
    if (alarm_thread.new_ivl==0 && interv>0.0) alarm_thread.new_ivl = 1;
    alarm_thread.new_callback = callback;
    alarm_thread.new_cb_arg = cb_arg;

    if (!SetEvent(alarm_thread.time_set_event))
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return 0;
    }

    /* wait for thread to accept new times and return old ones */
    switch (WaitForSingleObject(alarm_thread.time_accept_event, 10000))
    {
    case WAIT_FAILED:
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return 0;

    default:
	Set_Sys_Errno(ERROR_TIMEOUT,ERRNO_WIN32);
	return 0;

    case WAIT_OBJECT_0:
	if (premain) *premain = alarm_thread.old_remain / 1000.0;
	if (old_interv) *old_interv = alarm_thread.old_ivl / 1000.0;
	return 1;
    }
}


/*
 * Terminate the alarm thread.
 * Returns: 1 if cleanly terminated, 0 if forcibly terminated, -1 error
 */
int
ec_terminate_alarm()
{
    DWORD thread_exit_code = 0;

    /* send a termination request to the thread */
    alarm_thread.terminate_req = 1;
    if (!SetEvent(alarm_thread.time_set_event))
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return -1;
    }

    /* wait for termination */
    switch(WaitForSingleObject(alarm_thread.thread_handle, 3000))
    {
    case WAIT_OBJECT_0:	/* termination was signalled */
	if (!GetExitCodeThread(alarm_thread.thread_handle, &thread_exit_code))
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    return -1;
	}
	/* thread_exit_code is 0 or 1 */
	break;

    default:
    case WAIT_TIMEOUT:	/* timeout occurred, terminate forcibly */
	if (!TerminateThread(alarm_thread.thread_handle, 0))
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    return -1;
	}
	/* thread_exit_code is 0, don't bother to wait */
	break;

    case WAIT_FAILED:
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return -1;
    }

    if (!CloseHandle(alarm_thread.thread_handle) ||
	!CloseHandle(alarm_thread.time_set_event) ||
	!CloseHandle(alarm_thread.time_accept_event))
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	return -1;
    }

    alarm_thread.thread_handle = NULL;
    return thread_exit_code;	/* 0 or 1 */
}

#endif

/*----------------------------------------------------------------------
 * Registry/Environment lookup
 *
 * Windows:
 *	look up <name> under
 *		HKEY_LOCAL_MACHINE\SOFTWARE\IC-Parc\ECLiPSe
 *	or else
 *		HKEY_LOCAL_MACHINE\SOFTWARE\IC-Parc\ECLiPSe\<version>
 *	or else
 *		as environment variable <name>
 *
 * Unix:
 *	look up <name>
 *		as environment variable <name>_<major>_<minor>
 *		e.g. ECLIPSEDIR_5_10 for version 5.10
 *	or else
 *		as environment variable <name>
 *		e.g. ECLIPSEDIR
 *
 * return NULL if not found, buffer address otherwise.
 *
 * A buffer must be provided by the caller.
 * buflen is a pointer to the length of the buffer provided,
 * it is overwritten with the number of bytes actually returned
 * (CAUTION: this size includes a terminating zero if any).
 * If this number is greater than the buflen initially provided,
 * it is undefined whether any data is returned in the buffer.
 *----------------------------------------------------------------------*/

#ifdef _WIN32

char *
ec_env_lookup(char *name, char *buf, int *buflen)
{
    HKEY key1, key2;
    LONG err;
    DWORD vtype, buflen_dw;
    char *res;
    int len;
    
    err = RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\IC-Parc\\ECLiPSe", 0, KEY_READ, &key1);
    if (err != ERROR_SUCCESS)
    {
	goto _try_env_;
    }
    err = RegOpenKeyEx(key1, ec_version, 0, KEY_READ, &key2);
    if (err != ERROR_SUCCESS)
    {
	(void) RegCloseKey(key1);
	goto _try_env_;
    }
    (void) RegCloseKey(key1);
    buflen_dw = *buflen;
    err = RegQueryValueEx(key2, name, NULL, &vtype, buf, &buflen_dw);
    *buflen = buflen_dw;
    (void) RegCloseKey(key2);
    if (!(err == ERROR_SUCCESS || err == ERROR_MORE_DATA))
    {
	goto _try_env_;
    }
    return buf;

_try_env_:
    len = GetEnvironmentVariable(name, buf, *buflen);
    /* On Windows, we can't tell the difference between an unset variable
     * and an empty string!  Empty strings therefore look like unset.  */
    if (len == 0)
	return NULL;
    /* If buffer was large enough, len is string size without terminator,
     * otherwise required buffer size with terminator! */
    *buflen = len < *buflen ? len + 1 : len;
    return buf;
}

#else

char *
ec_env_lookup(char *name, char *buf, int *buflen)
{
    int i, len;
    char *res, *from, *to;
    char *vname = (char *) malloc(strlen(name)+strlen(ec_version)+2);

    /* construct name_X_Y from name and ec_version X.Y */
    for(from=name,to=vname; *from; ++from,++to)
    	*to = *from;
    *to++ = '_';
    for(from=ec_version; *from; ++from,++to)
    	*to = *from == '.' ? '_' : *from;
    *to = 0;

    if (!(res = getenv(vname)) && !(res = getenv(name)))
    {
	free(vname);
	Set_Sys_Errno(0, ERRNO_UNIX);
	return NULL;
    }
    free(vname);
    len = strlen(res)+1;
    if (len > *buflen)
	strncpy(buf, res, *buflen);
    else
	strncpy(buf, res, len);
    *buflen = len;
    return buf;
}

#endif
