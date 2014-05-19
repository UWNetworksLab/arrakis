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
 * VERSION	$Id: bip_load.c,v 1.1 2008/06/30 17:43:51 jschimpf Exp $
 */

/****************************************************************************
 *
 *		SEPIA Built-in Predicates for dynamic loading
 *
 *
 *****************************************************************************/

#include "config.h"

//asq:
#include <assert.h>

#ifdef _WIN32
#include <windows.h>
#else
//asq:
#include <stdint.h>
#include <barrelfish_kpi/types.h>
#include <sys/types.h>

#include <stdio.h>
#include <errno.h>
//#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#endif

#ifdef STDC_HEADERS
#include	<stdlib.h>
#else
extern char *getenv();
#endif

#ifdef HAVE_STRING_H
#include	<string.h>
#else
extern char	*strcpy();
#endif

#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "dict.h"
#include "emu_export.h"
#include "error.h"
#include "opcode.h"
#include "io.h"
#include "property.h"
#include "module.h"
#include "os_support.h"

#ifdef SBRK_UNDEF
extern char	*sbrk();
#endif

#if defined(HAVE_DLOPEN) || defined(HAVE_NLIST) || defined(_WIN32) || defined(HAVE_MACH_O_DYLD_H)
#define D_DEF
#endif

#if defined(HAVE_DLOPEN) || defined(_WIN32) || defined(D_LOAD) || defined(HAVE_MACH_O_DYLD_H)
#  ifndef D_LOAD
#    define D_LOAD
#  endif
#endif

/* We consider BSD-type dynamic loading with ld -A, or SVR4
 * dynamic linking or AIX (IBM rs6000) using load()
 */

#if (defined(HAVE_DLOPEN) && !defined(sun4_0)) || defined(HAVE_MACH_O_DYLD_H)
# define OS_SUPPORTS_DL
#endif

#if defined(HAVE_DLOPEN) && !defined(sun4_0)
#  include <dlfcn.h>
#elif defined(HAVE_MACH_O_DYLD_H)
#  include "dlfcn_simple.h"
#else
#ifndef _WIN32
# if defined(D_LOAD) || defined(D_DEF)
#  include <sys/file.h>
#  include <a.out.h>

#  ifdef hpux
#    define N_TXTOFF(f, hr)	hr.exec_tfile
#    define TD_SIZE(hr)		(hr.exec_tsize + hr.exec_dsize)
#    define BS_SIZE(hr)		(hr.exec_bsize)
#    define FileHeader		header
#    define AoutHeader		som_exec_auxhdr
#  else
#  if defined(mips) || defined(__alpha)
#    define TD_SIZE(hr)		(hr.tsize + hr.dsize)
#    define BS_SIZE(hr)		(hr.bsize)
#    define FileHeader		filehdr
#    define AoutHeader		aouthdr
#  else
#    define TD_SIZE(hr)		(hr.a_text + hr.a_data)
#    define BS_SIZE(hr)		(hr.a_bss)
#    define AoutHeader		exec
#  endif
#  endif
# endif	/* D_LOAD || D_DEF */
#endif /* _WIN32 */
#endif /* HAVE_DLOPEN */


#define SEPIA_TMP	"/tmp"

unsigned ec_vers = 0;

pword	*p_whoami_;
pword	*p_binary_;

dident	d_hostarch_;


#if defined(D_LOAD) && defined(D_DEF)

/****************************************************************
 * Dynamic loading and related
 ****************************************************************/


/*
 * 	p_load()	dload(file + options)
 *	dynamic loading of an object file.
 *	MUCH system dependent
 */

#ifdef _WIN32

struct dload_info {
  HINSTANCE handle;
  struct dload_info *next;
};

static struct dload_info *dload_list = 0;

static int 
p_load(v, t)
value v;
type t;
{
    char *name;
    char buf1[MAX_PATH_LEN];
    char buf2[MAX_PATH_LEN];
    char winname[MAX_PATH_LEN];
    HINSTANCE dloaded;
    struct dload_info *dli;

    Get_Name(v,t,name)			/* get the name of the file */
    name = expand_filename(name, buf1);
    /* Make an absolute pathname, needed on Windows 95 */
    if (name[0] != '/')
    {
	int len = get_cwd(buf2, MAX_PATH_LEN);
	(void) strcpy(buf2+len, name);
	name = buf2;
    }
    dloaded = LoadLibrary(os_filename(name, winname));
    if (!dloaded)
    {
	Set_Sys_Errno(GetLastError(), ERRNO_WIN32)
	Bip_Error(SYS_ERROR);
    }
    dli = (struct dload_info *) hp_alloc_size(sizeof(struct dload_info));
    dli->handle = dloaded;
    dli->next = dload_list;
    dload_list = dli;
    Succeed_;
}

void
bip_load_fini(void)
{
    while (dload_list)
    {
	struct dload_info *dli = dload_list;
	dload_list = dli->next;
	(void) FreeLibrary(dli->handle);
	hp_free_size(dli, sizeof(struct dload_info));
    }
}

#else
#ifdef OS_SUPPORTS_DL

#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL	0
#endif
/*
 * We have operating system support for dynamic loading, which
 * makes things simpler. The object to be loaded must be a
 * shared object. Compile it with
 *
 *	cc -I... -G -o <name>.so name.c
 */

/*
 * Remember the loaded objects in dload_list, which will be used
 * by external/2 and symbol_address/2.
 */

struct dload_info {
  void *handle;
  struct dload_info *next;
};

static struct dload_info *dload_list = 0;


static int 
p_load(value v, type t)
{
    char buf1[MAX_PATH_LEN];
    char buf2[MAX_PATH_LEN];
    char *name;
    void *dloaded;
    struct dload_info *dli;

    Get_Name(v,t,name)			/* get the name of the file */
    name = expand_filename(name, buf1);
    /* Make an absolute pathname because dlopen sometimes
     * seems to have a wrong idea of the cwd.
     */
    if (name[0] != '/')
    {
	int len = get_cwd(buf2, MAX_PATH_LEN);
	(void) strcpy(buf2+len, name);
	name = buf2;
    }
    dloaded = dlopen(name, RTLD_NOW|RTLD_GLOBAL);
    if (!dloaded)
    {
	ec_outfs(current_err_, dlerror()); ec_newline(current_err_);
	Bip_Error(NO_SHARED_LIB);
    }
    dli = (struct dload_info *) hp_alloc_size(sizeof(struct dload_info));
    dli->handle = dloaded;
    dli->next = dload_list;
    dload_list = dli;
    Succeed_;
}

void
bip_load_fini(void)
{
    while (dload_list)
    {
	struct dload_info *dli = dload_list;
	dload_list = dli->next;
	(void) dlclose(dli->handle);
	hp_free_size(dli, sizeof(struct dload_info));
    }
}

#else /*!OS_SUPPORTS_DL */
#ifdef _AIX
/* In AIX we have to keep track of each dynamically loaded file in order
to use nlist with it in ec_getaddress(). */

struct dload_info {
  char *filename;
  char *entryproc;
  void (*funcp)();
  struct dload_info *next;
};

static struct dload_info *dload_list;

static int 
p_load(value v, type t)
{
    extern char *expand_filename();
    char *name;
    int  res;
    char fullname[MAX_PATH_LEN];
    long tsize;
    char	*tmpdir;
    char	*loader;

    Get_Name(v,t,name)			/* get the name of the file */
    name = expand_filename(name, fullname);
    if(!IsString(p_whoami_->tag)) {
	Bip_Error(TYPE_ERROR)
    }
					/* identifier for temporary */
    tmpdir = getenv("ECLIPSETMP");
    if (!tmpdir)
	tmpdir = SEPIA_TMP;
    loader = getenv("ECLIPSELOADER");
    if (!loader)
	loader = "ld";

    res = _load_once(loader, name, tmpdir);
    if (res < 0)
    {
	Bip_Error(res);
    }
    ec_vers++;
    return(PSUCCEED);
}

_load_once(char *loader, char *vstr, char *tmpdir)
{
    FILE *f;
    extern int	sys_nerr;
    int		res;
    char buf[1024];	/* buf will hold the loader command */
    char *temp, *entryproc; 
    char dummy[MAX_PATH_LEN + 30];
    char expsympath[1024], tmpsympath[1024];
    void (*funcp)();
    struct dload_info *cur;

    temp = (char *) hg_alloc(MAX_PATH_LEN + 30);
    entryproc = (char *) hg_alloc(MAX_PATH_LEN + 30);

    (void) sprintf(temp, "%s/eclipse.%d.%d", tmpdir, getpid(), ec_vers);
    					/* file which will hold linked code */
    (void) sprintf(dummy,"%s/eclipse_dummy.%d.%d",tmpdir,getpid(),ec_vers);
                                        /* dummy file needed to defiine a 
					   known entry point */
    (void) sprintf(entryproc,"eclipse_dummy_entry%d",ec_vers);
                                        /* name of entry procedure */

    /* create dummy entry proc */

    if(!(f = fopen(dummy,"w")))
      {
	if (!errno)
	    errno = sys_nerr;
	Set_Errno
	return(SYS_ERROR);
      }

    (void) fprintf(f,"int %s() \n { return(0); } \n",entryproc);
    (void) fclose(f);

    (void) sprintf(buf,"cd %s; mv %s %s.c; cc -c %s.c; /bin/rm %s.c\n",
	         tmpdir, dummy, dummy, dummy, dummy);

    strcat(dummy,".o");    
    res = system(buf);
    if (res != 0) {
      (void) unlink(dummy);
      Set_Errno
      return(SYS_ERROR);
    }

    (void) sprintf(tmpsympath, "%s/tmpsymXXXXXX",tmpdir);
    mktemp(tmpsympath);

    {
      pword *library;
      Get_Kernel_Var(in_dict("library",0), library);
      
      /* the loader command we will execute */
      (void) sprintf(buf,"echo \"#! %s\" > %s; cat %s/%s/%s>>%s; %s -H512 -T512 %s %s -e %s  -bI:%s -bfl -bgcbypass:2 -o %s -lc",
	     StringStart(p_whoami_->val),tmpsympath,
	     StringStart(library->val), HOSTARCH, "expsymtab", tmpsympath,
	     loader,dummy,vstr,entryproc, tmpsympath,temp);
    }

    res = system(buf);
    (void) unlink(dummy);
    (void) unlink(tmpsympath);

    if(res != 0) {
	(void) unlink(temp);	/* if there was a problem, remove temporary */
	if (!errno)
	    errno = sys_nerr;
	Set_Errno
	return(SYS_ERROR);
    }				/* everything was ok */

    /* Now dynamically link code in temp using load() */
   if (!(funcp=(void (*)())load(temp, 0, NULL)))
     {
       (void) unlink(temp);
       Set_Errno
       return(SYS_ERROR);
     }

    /* keep track of the loaded file and its entrypoint */
    if (ec_vers == 0)
      {
	dload_list = (struct dload_info *) 
	             hg_alloc(sizeof(struct dload_info));
	cur = dload_list;
	cur->next = NULL;
      }
    else
      {
	cur = (struct dload_info *) 
	             hg_alloc(sizeof(struct dload_info));
	cur->next = dload_list;
	dload_list = cur;
      }
    cur->filename = temp;
    cur->funcp = funcp;
    cur->entryproc = entryproc;
    return PSUCCEED;
}

void
bip_load_fini()
{
    if (ec_vers > 0)
    {
	struct dload_info *cur = dload_list;

	while(cur != NULL)
	{
	    unlink(cur->filename);
	    cur = cur->next;
	}
    }
}

#else

static generic_ptr dload_list = 0;

static int 
p_load(value v, type t)
{
    extern char *expand_filename();
    char *name;
    char *end;
    int size, res;
    int fd;
    char buf[1024];	/* buf will hold the loader command */
    char temp[MAX_PATH_LEN + 30];
    char fullname[MAX_PATH_LEN];
    long tsize;
    char	*tmpdir;
    char	*loader;

    Get_Name(v,t,name)			/* get the name of the file */
    name = expand_filename(name, fullname);
    if(!IsString(p_whoami_->tag)) {
	Bip_Error(TYPE_ERROR)
    }
    end = (char *) sbrk(0);			/* end of memory */
					/* identifier for temporary */
    tmpdir = getenv("ECLIPSETMP");
    if (!tmpdir)
	tmpdir = SEPIA_TMP;
    loader = getenv("ECLIPSELOADER");
    if (!loader)
	loader = "ld";
    (void) sprintf(temp, "%s/eclipse.%d.%d", tmpdir, getpid(), ec_vers);
    					/* file which will keep the symbol */
					/* table and the linked code */
    res = _load_once(buf, loader, end, name, temp, &size, &tsize, &fd);
    if (res < 0)
    {
	Bip_Error(res);
    }

    end = (char *) sbrk((int) tsize);

    if(size != read(fd, end, size))	/* read in the code */
    {
	(void) close(fd);
	(void) unlink(temp);
	Set_Errno
	Bip_Error(SYS_ERROR)
    }
    (void) close(fd);				/* that is all */

    if (ec_vers > 0)			/* remove previous temporary if any */
	(void) unlink(StringStart(p_whoami_->val));

    free_heapterm(p_whoami_);
    set_string(p_whoami_, temp);
    ec_vers++;
    return(PSUCCEED);
}

_load_once(char buf[], char *loader, char *end, char *vstr, char *temp,
	int *size, long *tsize, int *fd)
{
    extern int		sys_nerr;
    int			res;
#ifdef FileHeader
    struct FileHeader	filehdr;
#endif
    struct AoutHeader	hr;

    /* the loader command we will execute */
    /* "-N" needed to avoid wasting space and avoid alignment problems */
#ifdef sun4_0
/*
 * There is a bug in SUNOS 4.0: when a file is dynamically loaded with
 * ld -A, the new symbol table which is created contains a wrong
 * _DYNAMIC_ symbol so that when a savecore with this table is made,
 * dbx is unable to work on the resulting binary. We fix it by loading
 * the file aux.o which contains  a reference to the _DYNAMIC_ symbol
 * so that it is defined in the new symbol table.
 */
    {
	pword *library;
	Get_Kernel_Var(in_dict("library",0), library);

	(void) sprintf(buf,
		"%s -N -A %s -T %x -o %s %s/%s/%s %s -lc 1>&2",
		loader,
    		StringStart(p_whoami_->val), end, temp,
		StringStart(library->val), HOSTARCH, "auxiliary.o", vstr);
    }
#else
    (void) sprintf(buf,
#ifdef hpux
		"%s -a archive -N -A %s -R %x -o %s %s /lib/dyncall.o -lc 1>&2",
#else
		"%s -N -A %s -T %x -o %s %s -lc 1>&2",
#endif
		loader,
    		StringStart(p_whoami_->val), end, temp, vstr);
#endif
    res = system(buf);
    if(res != 0) {
	(void) unlink(temp);	/* if there was a problem, remove temporary */
	if (!errno)
	    errno = sys_nerr;
	Set_Errno
	return(SYS_ERROR);
    }				/* everything was ok */
    if((*fd = open(temp, O_RDWR)) < 0) {	/* try to open temp */
        Set_Errno
	return(SYS_ERROR);
    }

    /* read in the header information */
#ifdef FileHeader
    (void) read(*fd, (char *) &filehdr, sizeof(filehdr));
    (void) read(*fd, (char *) &hr, sizeof(hr));
    (void) lseek(*fd, (long) N_TXTOFF(filehdr, hr), L_SET);
#else
    (void) read(*fd, (char *) &hr, sizeof(hr));
    (void) lseek(*fd, (long) N_TXTOFF(hr), L_SET);
#endif
    *size = TD_SIZE(hr);
    *tsize = (((*size + BS_SIZE(hr)) + 511)/ 512) * 512;
    return PSUCCEED;
}

void
bip_load_fini()
{
    if (IsString(p_whoami_->tag) && ec_vers > 0)
	(void) unlink(StringStart(p_whoami_->val));
}

#endif /* _AIX */
#endif /* OS_SUPPORTS_DL */
#endif /* _WIN32 */

#else /* D_LOAD && D_DEF */
Not_Available_Built_In(p_load)
#endif /* D_LOAD && D_DEF */



#ifdef D_DEF

/****************************************************************
 * Dynamic definitions and related
 ****************************************************************/

/*
 *	ec_getaddress(function_name)
 *	fetch the address of a symbol from the symbol table
 *	returns -1 if it was not possible.
 */

#ifdef _WIN32

word
ec_getaddress(char *s)
{
    struct dload_info *dli;

    for (dli = dload_list; dli; dli = dli->next)
    {
	word addr = (word) GetProcAddress(dli->handle, s);
	if (addr)
	    return addr;
    }
    return (word) 0;
}

#else
#ifdef OS_SUPPORTS_DL

static void *myself = (void *) 0;

word
ec_getaddress(char *s)
{
    word addr = 0;

    if (!myself)
    {
	if (!(myself = dlopen((char *) 0, RTLD_LAZY)))
	{
	    return 0;
	}
    }
    addr = (word) dlsym(myself, s);
    if (!addr)
    {
	struct dload_info *dli;
	for (dli = dload_list; dli; dli = dli->next)
	{
	    addr = (word) dlsym(dli->handle, s);
	    if (addr)
		return addr;
	}
    }
    return addr;
}

#else
#ifdef _AIX
/* For AIX we have to return a function descriptor which contains
not only the address of the function, but also the location of 
the table of contents (toc). */

struct func_descriptor {
  int address;
  int toc;
};

word
ec_getaddress(char *s)
{
    struct nlist lis[4];
    struct func_descriptor *fdesc;
    extern char _text[ ], _data[ ];
    int found = 0;
    int n = strlen(s);
    uword *wp = (uword *) hg_alloc( n + 2);
    char *p = (char *) wp;

    *p = '.';
    (void) strcpy(p + 1, s);
    
    lis[0].n_value = 0;
    lis[0].n_name = p;
    lis[1].n_value = 0;
    lis[1].n_name = "_text";
    lis[2].n_value = 0;
    lis[2].n_name = "TOC";
    lis[3].n_name = "";
    n = nlist(StringStart(p_whoami_->val),lis);
    if (lis[0].n_value && lis[1].n_value && lis[2].n_value)
      {
	fdesc = (struct func_descriptor *)
	  hg_alloc(sizeof(struct func_descriptor));
	fdesc->address = (int) _text + lis[0].n_value - lis[1].n_value;
	fdesc->toc = (int) _data + lis[2].n_value;
	found = 1;
      }
#ifdef D_LOAD
    if (!found && (ec_vers > 0))   /* a dynamic load has been performed */
      {
	struct dload_info *cur = dload_list;
	while ((!found)  && cur)
	  {
	    char *real_entryproc = (char *) 
		  hg_alloc((long)strlen(cur->entryproc) + 2);
	    
	    *real_entryproc = '.';
	    (void) strcpy(real_entryproc + 1, cur->entryproc);
	    
	    lis[0].n_value = 0;
	    lis[0].n_name = real_entryproc;
	    lis[1].n_value = 0;
	    lis[1].n_name = p;
	    lis[2].n_name = "";
	    
	    n = nlist(cur->filename,lis);
	    if (lis[0].n_value  && lis[1].n_value)
	      {
		int temp;
		    
		fdesc = (struct func_descriptor *)
		  hg_alloc(sizeof(struct func_descriptor));
		fdesc->address = (* (int *) (cur->funcp)) + 
		  lis[1].n_value - lis[0].n_value;
		temp = (int) (cur->funcp);
		fdesc->toc = *(((int *) temp) + 1);
		found = 1;
	      }
	    else
	      cur = cur->next;
	    hg_free(real_entryproc);
	  }
      }
#endif
    hg_free(wp);
    if (found) 
      return( (int) fdesc);
    else
      return 0;
  }
	
#else

word
ec_getaddress(char *s)
{
    struct nlist lis[2];
    lis[0].n_value = 0;
    lis[0].N_NAME = s;
    lis[1].N_NAME = 0;

    if(nlist(StringStart(p_whoami_->val), lis) < 0 || lis[0].n_value == 0)
    {
	    int n = strlen(s);
	    uword *wp = (uword *) hg_alloc(n + 2);
	    char *p = (char *) wp;
	    *p = '_';
	    (void) strcpy(p + 1, s);
	    lis[0].n_value = 0;
	    lis[0].N_NAME = p;
	    lis[1].N_NAME = 0;
	    n = nlist(StringStart(p_whoami_->val), lis);
	    hg_free((generic_ptr) wp);
	    if(n < 0 || lis[0].n_value == 0)
	    	return 0;
    }
    return(lis[0].n_value);
}

#endif
#endif
#endif

/*
 *	p_call_c()	call_c(foo(a1,...an),Value)
 *	calls the function whose system name is foo after
 *	translating the arguments, and 
 *	unifies Value with the value returned by the function, taken as
 *	an integer.
 */

#define MAX_CALL_C_ARITY	10
static int
p_call_c(value v, type t, value vr, type tr)
{
    word foo, aux;
    int arity;
    pword *p, *pw;
    value arg[MAX_CALL_C_ARITY];
    dident mydid;
    double	f;
    int		res_type;
    value	resv;
    type	rest;

    Error_If_Ref(t)
    if (IsStructure(tr)) {
	mydid = vr.ptr->val.did;
	if (mydid == d_.float1)
	    res_type = TDBL;
	else if (mydid == d_.integer)
	    res_type = TINT;
	else if (mydid == d_.string)
	    res_type = TSTRG;
	else {
	    Bip_Error(RANGE_ERROR)
	}
	resv.all = vr.ptr[1].val.all;
	rest.all = vr.ptr[1].tag.all;
    }
    else if (IsRef(tr) || IsInteger(tr)) {
	res_type = TINT;
	resv.all = vr.all;
	rest.all = tr.all;
    }
    else {
	Bip_Error(TYPE_ERROR)
    }
    if(IsStructure(t))
	mydid = v.ptr->val.did;
    else if(IsAtom(t))
	mydid = v.did;
    else
    {
	Bip_Error(TYPE_ERROR);
    }
    arity = DidArity(mydid);
    mydid = add_dict(mydid, 0);
    if(pw = get_property(mydid, SYSCALL_PROP))
    {
	if(IsInteger(pw->tag))
	{
	    foo = pw->val.nint;
	}
	else
	{
	    foo = ec_getaddress(DidName(mydid));
	    if(!foo)
	    {
		Bip_Error(NOCODE);
	    }
	    pw->tag.kernel = TINT;
	    pw->val.nint = foo;
	}
    }
    else
    {
	foo = ec_getaddress(DidName(mydid));
	if(!foo)
	{
	    Bip_Error(NOCODE);
	}
	pw = set_property(mydid, SYSCALL_PROP);
	pw->tag.kernel = TINT;
	pw->val.nint = foo;
    }
    aux = 0;
    				/* arguments translation */
    while(arity-- > 0)
    {
	p = ++(v.ptr);
	Dereference_(p)
	if(IsRef(p->tag))
	{
	    Bip_Error(TYPE_ERROR);
	}
	else
	{
	    switch (TagType(p->tag))
	    {
	    case TINT:
		arg[aux++] = p->val;
		break;

	    case TDBL:
		arg[aux++].nint = ((long *) &Dbl(p->val))[0];
		arg[aux++].nint = ((long *) &Dbl(p->val))[1];
		break;

	    case TSTRG:
		arg[aux++].str = StringStart(p->val);
		break;
	    case TDICT:
		arg[aux++].str = DidName(p->val.did);
		break;
	    case TCOMP:
		{
		    uword	kind, size;
		    int		err;
		    word	res;
		    type	tm;

		    tm.kernel = ModuleTag(d_.kernel_sepia);

		    p = p->val.ptr;
		    if(p->val.did == d_.quotient)
		    {
			res = get_first_elt(p+1, p+2, &kind, &size,
					    d_.kernel_sepia, tm);
			if (res < 0)
			{
			    Bip_Error(res);
			}
		    }
		    else
		    {
			value	v1;

			v1.all = (word) p;
			res = (word) get_elt_address(v1, tcomp, &kind,
						    d_.kernel_sepia, tm, &err);
			if (!res)
			{
			    Bip_Error(err);
			}
		    }
		    arg[aux++].nint = res;
		}
		break;

		default:
		    Bip_Error(TYPE_ERROR)
	    }
	}
    }
    if (res_type == TDBL)
	switch(aux) {
	    case 0: f =  (* (double (*)()) foo)();
		    break;
	    case 1: f =  (* (double (*)()) foo)(arg[0].nint);
		    break;
	    case 2: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint);
		    break;
	    case 3: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint);
		    break;
	    case 4: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint);
		    break;
	    case 5: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint);
		    break;
	    case 6: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint, arg[5].nint);
		    break;
	    case 7: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint, arg[5].nint,
			    arg[6].nint);
		    break;
	    case 8:
	    case 9:
	    case 10: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint,arg[3].nint,arg[4].nint, arg[5].nint,
			    arg[6].nint,arg[7].nint, arg[8].nint,arg[9].nint);
		    break;
	    default:
		Bip_Error(ARITY_LIMIT)
	}
    else
	switch(aux) {
	    case 0: aux =  (* (int (*)()) foo)();
		    break;
	    case 1: aux =  (* (int (*)()) foo)(arg[0].nint);
		    break;
	    case 2: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint);
		    break;
	    case 3: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint);
		    break;
	    case 4: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint);
		    break;
	    case 5: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint);
		    break;
	    case 6: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint, arg[5].nint);
		    break;
	    case 7: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint, arg[5].nint,
			    arg[6].nint);
		    break;
	    case 8:
	    case 9:
	    case 10: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint,arg[3].nint,arg[4].nint, arg[5].nint,
			    arg[6].nint,arg[7].nint, arg[8].nint,arg[9].nint);
		    break;
	    default:
		Bip_Error(ARITY_LIMIT)
	}
    Set_Errno;	/* in case something went wrong */
    if (res_type == TINT) {
	Return_Unify_Integer(resv, rest, aux);
    } else if (res_type == TDBL) {
	Return_Unify_Float(resv, rest, f);
    }
    else /* if (res_type == TSTRG) */
    {
	value	sv;
	Cstring_To_Prolog((char *) aux, sv);
	Return_Unify_String(resv, rest, sv.ptr);
    }
}

static int
p_symbol_address(value vals, type tags, value vala, type taga)
{
	char	*name;
	word	symbol;

	Get_Name(vals, tags, name);
	Check_Output_Integer(taga);
	symbol = ec_getaddress(name);
	if (!symbol)
	{
		Fail_;
	}
	Return_Unify_Integer(vala, taga, symbol);
}

#else	/* D_DEF */
Not_Available_Built_In(p_symbol_address)
Not_Available_Built_In(p_call_c)
#endif /* D_DEF */



/*
 * Licence checking
 *
 * If there is a pteclipse.so library, we load it dynamically.
 * It contains proper definitions of licence_checkout/6 etc.
 * If there is no pteclipse.so, we use the dummies defined here.
 */

/*ARGSUSED*/
static int
p_licence_checkout(value vfeature, type tfeature, value vpol, type tpol, value vversion, type tversion, value vlicloc, type tlicloc, value vmsg, type tmsg, value vstat, type tstat)
{
    pword pw;
    Prepare_Requests;
    Make_String(&pw, "ECLiPSe licence check failed\n");
    Request_Unify_Pw(vmsg, tmsg, pw.val, pw.tag);
    Request_Unify_Atom(vstat, tstat, d_.err);
    Return_Unify;
}

/*ARGSUSED*/
static int
p_licence_held(value vfeature, type tfeature)
{
    Fail_;
}

/*ARGSUSED*/
static int
p_licence_checkin(value vfeature, type tfeature)
{
    Succeed_;
}

/*ARGSUSED*/
static int
p_licence_heartbeat(value vfeature, type tfeature, value vminutes, type tminutes, value vrec, type trec, value vfrec, type tfrec)
{
    Fail_;
}


static void
_pt_init(int flags)
{
    char pteclipse[MAX_PATH_LEN];

    /* these are the dummies - they may be replaced by pteclipse */
    (void) exported_built_in(in_dict("licence_checkout", 6), p_licence_checkout, B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("licence_checkin", 1), p_licence_checkin, B_SAFE);
    (void) exported_built_in(in_dict("licence_heartbeat", 4), p_licence_heartbeat, B_SAFE);
    (void) exported_built_in(in_dict("licence_held", 1), p_licence_held, B_SAFE);

    strcpy(pteclipse, ec_eclipse_home);	/* check for pteclipse lib */
    strcat(pteclipse, "/lib/");
    strcat(pteclipse, HOSTARCH);
    strcat(pteclipse, "/pteclipse.");
    strcat(pteclipse, OBJECT_SUFFIX_STRING);
    if (ec_access(pteclipse, R_OK) == 0)
    {
	pword pw;
	int (*init_fct)();

	Make_Atom(&pw, in_dict(pteclipse,0));	/* load it */
	if (p_load(pw.val, pw.tag) != PSUCCEED)
	    ec_panic("Can't load library file", pteclipse);

	init_fct = (int(*)()) ec_getaddress("pteclipse_init");
	if (!init_fct)
	    ec_panic("Library file corrupted", pteclipse);

	switch ((*init_fct)(flags))		/* initialise */
	{
	case PSUCCEED:
	    return;
	case PFAIL:
	    ec_panic("Licensing problem", "initialisation");
	    break;
	case UNIMPLEMENTED:
	default:
	    break;	/* pteclipse not available, keep the dummies */
	}
    }
}


/****************************************************************
 * Common Initialization and Finalization
 ****************************************************************/

void
bip_load_init(int flags)
{
    value	dummy_v1;

    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("load",1), p_load, B_SAFE);
	(void) exported_built_in(in_dict("symbol_address", 2),
				p_symbol_address,	B_UNSAFE|U_SIMPLE);
	built_in(in_dict("call_c",2), p_call_c, B_UNSAFE|U_SIMPLE)
		-> mode = BoundArg(2, CONSTANT);

	_pt_init(flags);
    }

    /* whoami and binary are properly initialized in top.pl */
    dummy_v1.nint = 0;
    p_whoami_ = init_kernel_var(flags, in_dict("whoami", 0), dummy_v1, tint);
    p_binary_ = init_kernel_var(flags, in_dict("binary", 0), dummy_v1, tint);

    d_hostarch_ = in_dict(HOSTARCH, 0);

    ec_vers = 0;

    dload_list = 0;
#ifndef _WIN32
#ifdef OS_SUPPORTS_DL
    myself = 0;
#endif
#endif
}


