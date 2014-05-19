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
 * $Id: eg_c_mip.c,v 1.1 2008/06/30 17:43:53 jschimpf Exp $
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


#define NCOLS	3
#define NROWS	2

double req[NROWS][NCOLS] = {
    2.2, 1.8, 1.9,
    2.4, 2.0, 2.1
};

double pc[NCOLS] = {
    24.7, 22.4, 19.7
};




static void bounds(vars, lb, ub)
pword vars;
double lb, ub;
{
    ec_post_goal(ec_term(
	ec_did("::",2),
	    vars,
	    ec_term(ec_did("..",2), ec_double(lb), ec_double(ub))));
}

static void eq(lhs, rhs)
pword lhs, rhs;
{
    ec_post_goal(ec_term(ec_did("$=",2), lhs, rhs));
}

static void geq(lhs, rhs)
pword lhs, rhs;
{
    ec_post_goal(ec_term(ec_did("$>=",2), lhs, rhs));
}

static void leq(lhs, rhs)
pword lhs, rhs;
{
    ec_post_goal(ec_term(ec_did("$=<",2), lhs, rhs));
}

static void maximize(obj, objval)
pword obj, objval;
{
    ec_post_goal( ec_term(ec_did("optimize",2),
	ec_term(ec_did("max",1), obj), objval));
}

static pword plus(lhs, rhs)
pword lhs, rhs;
{
    return ec_term(ec_did("+",2), lhs, rhs);
}

static pword times(lhs, rhs)
pword lhs, rhs;
{
    return ec_term(ec_did("*",2), lhs, rhs);
}


main()
{
    ec_refs	Vars;
    ec_ref	Profit;
    pword	varlist;
    
    ec_init();

    ec_post_string("lib(eplex)");

    Vars = ec_refs_create_newvars(NCOLS);
    Profit = ec_ref_create_newvar();
    varlist = ec_listofrefs(Vars);

    bounds(varlist, 0.0, 1e20);

    eq(  times(varlist, ec_listofdouble(NCOLS,pc)),	ec_ref_get(Profit));
    leq( times(varlist, ec_listofdouble(NCOLS,req[0])),	ec_double(8.0));
    leq( times(varlist, ec_listofdouble(NCOLS,req[1])),	ec_double(10.0));

    maximize( times(varlist, ec_listofdouble(NCOLS,pc)), ec_ref_get(Profit));

    if (ec_resume1(0) == PSUCCEED)		/* solve */
    {
	double d;
	int i;

	if (ec_get_double(ec_ref_get(Profit), &d) == PSUCCEED)
	    printf("Profit is %f\n", d);
	else
	    printf("Profit is ?\n");

	for (i=0; i<NCOLS; i++)
	{
	    if (ec_get_double(ec_refs_get(Vars,i), &d) == PSUCCEED)
		printf("X%d = %f\n", i, d);
	    else
		printf("X%d = ?\n");
	}
    }
    else printf("No solution\n");

    ec_refs_destroy(Vars);
    ec_ref_destroy(Profit);
    ec_cleanup();
}

