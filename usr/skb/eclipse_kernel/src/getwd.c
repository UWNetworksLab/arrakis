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
 * Copyright (C) 1988-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: getwd.c,v 1.1 2008/06/30 17:43:55 jschimpf Exp $
*/

/*---------------------------------------------------------------------
 *
 * author: 	M.Dorochevsky
 * modified by: 
 *---------------------------------------------------------------------
 */

#ifdef SYSDIR

/* #include	<sys/types.h> */	/* all included in bip_misc.c */
/* #include	<sys/param.h> */
/* #include	<sys/stat.h> */
#include	<sys/dir.h>

#define		 FILESYSTEM_READ_ERROR	\
"getwd(): Filesystem error: read()\nCurrent working directory maybe changed.\n"



/* char * strcpy(); */



/*
 * Copies the pathname of the current working directory into the
 * buffer path and returns a pointer to this pathname.
 * Path is a buffer of MAX_PATH_LEN characters.
 *
 * If an error occurs, getwd() returns zero ((char *) 0) and leaves
 * an error message in path
 */

char * getwd(char *path)
{
	struct	stat	dotstat, dotdotstat;
	struct	direct	dir;
	int	fd;
	char * ptail;
	unsigned len, pathlen;
	char    statbuf[MAX_PATH_LEN];

	ptail = &(path[MAX_PATH_LEN-1]);
	*ptail = '\0';
	pathlen = 0;
	statbuf[0] = '.';
	statbuf[1] = '.';
	statbuf[2] = '/';

	for(;;) {
	    if(stat(".", &dotstat) < 0) {
		(void) strcpy(path,"getwd(): stat()\n");
		goto err;
	    }
	    if ((fd = open("..",0)) < 0) {
		(void) strcpy(path,"getwd(): open()\n");
		goto err;
	    }
	    if(fstat(fd, &dotdotstat) < 0) {
		(void) strcpy(path,"getwd(): fstat()\n");
		goto err_close;
	    }

	    if ((dotstat.st_dev == dotdotstat.st_dev) &&
		(dotstat.st_ino == dotdotstat.st_ino)) {

		/*
		 * root reached
		 */
		(void) close(fd);
		break;
	    }
	    else {

		/*
		 * root not reached
		 */
		if (pathlen + DIRSIZ + 1 >= MAX_PATH_LEN) {
		    (void) strcpy(path,"getwd(): Pathname too long.\n");
		    goto err_close;	/* path name too long */
		}

		do {
		    if (read(fd,(char *) &dir,sizeof(dir)) != sizeof(dir)) {
			(void) strcpy(path, FILESYSTEM_READ_ERROR);
			goto err_close;
		    }
		    (void) strcpy(statbuf + 3, dir.d_name);
		    (void) stat(statbuf, &dotdotstat);
		} while ((dotdotstat.st_ino != dotstat.st_ino) ||
			 (dotdotstat.st_dev != dotstat.st_dev));
		(void) close(fd);
	        if(chdir("..") < 0) {
		    (void) strcpy(path,"getwd(): fopen()\n");
		    goto err_close;
		}

		/* copy name to path */
		{
		register char * pname;

		pname = dir.d_name;
		len = 0;
		while ((len < DIRSIZ) && (*pname++ != '\0'))
		    len++;

		pathlen += len + 1;
		for (pname = &(dir.d_name[len]); len; len--)
		    *--ptail = *--pname;
		*--ptail = '/';
		}
	    }
	}

	if (pathlen == 0) {
		path[0] = '/';
		path[1] = '\0';
	}
	else {
		register char * pto = path;
		register char * pfrom = ptail;

		for (len = 0; len <= pathlen; len++)
			*pto++ = *pfrom++;

		/* go back to initial working directory */
		(void) chdir(ptail+1);
	}

	return(path);

err_close:	
	(void) close(fd);
err:	
	/* go back to initial working directory */
	if (*ptail)
		(void) chdir(ptail+1);
	return((char *) 0);
}

#endif

