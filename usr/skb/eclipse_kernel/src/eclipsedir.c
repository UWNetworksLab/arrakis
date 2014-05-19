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
 * ECLiPSe C SOURCE MODULE
 *
 * $Id: eclipsedir.c,v 1.1 2008/06/30 17:43:53 jschimpf Exp $
 *
 * Note that this file is used by different executables
 * (eclipse, worker manager)
 *
 */
#include "config.h"
#include "os_support.h"

#ifdef STDC_HEADERS
#include	<stdlib.h>
#endif

/*
 * eclipsehome() returns a pointer to a canonical file name.
 * The string should be considered static.
 */

extern char	*whereami();

static char	*eclipsehome_ = 0;
static char	buf[MAX_PATH_LEN];

char *
eclipsehome(void)
{
    if (!eclipsehome_)
    {
	eclipsehome_ = whereami();
	if (!eclipsehome_)
	{
	    char buf1[MAX_PATH_LEN];
	    int size=MAX_PATH_LEN;
	    if (ec_env_lookup("ECLIPSEDIR", buf1, &size))
	    {
		(void) canonical_filename(buf1, buf);
		if (buf[0] != '/')
		{
		    char buf2[MAX_PATH_LEN];
		    strcpy(buf2, buf);
		    get_cwd(buf, MAX_PATH_LEN);
		    strcat(buf, buf2);
		}
		eclipsehome_ = buf;
	    }
	    else
	    {
#ifdef _WIN32
		eclipsehome_ = "//C/Eclipse";
#else
		eclipsehome_ = "/usr/local/eclipse";
#endif
	    }
	}
    }
    return eclipsehome_;
}

