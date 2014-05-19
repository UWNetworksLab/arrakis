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
 * ECLiPSe Application Example
 *
 * $Id: eg_c_sendmore.c,v 1.1 2008/06/30 17:43:54 jschimpf Exp $
 *
 * IDENTIFICATION:	eg_sendmore.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * DESCRIPTION:
 *	Example of solving a constraint problem from C
 */

#include	"eclipse.h"

#define NVARS	8

main()
{
    dident	plus,times;
    ec_refs	Vars;
    pword	varlist;
    int		i, res;
    
    ec_init();
    plus = ec_did("+",2);
    times = ec_did("*",2);
    ec_exec_string("lib(fd)",0);

    Vars = ec_refs_create_newvars(NVARS);
    varlist = ec_listofrefs(Vars);
    ec_post_goal(
	ec_term(ec_did("::",2),
	    varlist, ec_term(ec_did("..",2), ec_long(0), ec_long(9)))
    );
    ec_post_goal(
	ec_term(ec_did("alldistinct",1), varlist)
    );
    ec_post_goal(
	ec_term(ec_did("##",2), ec_refs_get(Vars,0), ec_long(0))
    );
    ec_post_goal(
	ec_term(ec_did("##",2), ec_refs_get(Vars,4), ec_long(0))
    );
    ec_post_goal(
	ec_term(ec_did("#=",2),
	    ec_term(plus,
		ec_term(times, ec_long(1000), ec_refs_get(Vars,0)),
	    ec_term(plus,
		ec_term(times, ec_long(100), ec_refs_get(Vars,1)),
	    ec_term(plus,
		ec_term(times, ec_long(10), ec_refs_get(Vars,2)),
	    ec_term(plus,
		ec_refs_get(Vars,3),
	    ec_term(plus,
		ec_term(times, ec_long(1000), ec_refs_get(Vars,4)),
	    ec_term(plus,
		ec_term(times, ec_long(100), ec_refs_get(Vars,5)),
	    ec_term(plus,
		ec_term(times, ec_long(10), ec_refs_get(Vars,6)),
		ec_refs_get(Vars,1)
	    ))))))),
	    ec_term(plus,
		ec_term(times, ec_long(10000), ec_refs_get(Vars,4)),
	    ec_term(plus,
		ec_term(times, ec_long(1000), ec_refs_get(Vars,5)),
	    ec_term(plus,
		ec_term(times, ec_long(100), ec_refs_get(Vars,2)),
	    ec_term(plus,
		ec_term(times, ec_long(10), ec_refs_get(Vars,1)),
		ec_refs_get(Vars,7)
	    ))))
	)
    );
    ec_post_goal(
	ec_term(ec_did("labeling",1), varlist)
    );

    res = ec_resume1(0);		/* solve */

    if (res == PSUCCEED)		/* print solution */
    {
	for (i=0; i<NVARS; i++)
	{
	    long sol;
	    res = ec_get_long(ec_refs_get(Vars,i), &sol);
	    if (res == PSUCCEED)
		printf("X%d = %d\n", i, sol);
	    else
		printf("X%d = ?\n");
	}
    }
    else printf("No solution\n");

    ec_refs_destroy(Vars);
    ec_cleanup();
}

