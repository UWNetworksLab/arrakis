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
 * $Id: eg_c_fail_loop.c,v 1.1 2008/06/30 17:43:53 jschimpf Exp $
 *
 *
 * IDENTIFICATION:	eg1.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Example of minimal main useing external embed interface.
 */

#include	"eclipse.h"


main(int argc, char **argv)
{
    dident	p_1,fail;
    ec_ref X,Start;
    pword call;
    long num;
    int res;

    ec_init();

    /* make a set of facts */
    ec_exec_string("compile_term([p(1),p(2),p(3),p(2),p(1)])",0);

    /* make atoms and functors */
    p_1 = ec_did("p",1);
    fail = ec_did("fail",0);

    /* we will call p(X) and get an instantiation */
    X = ec_ref_create_newvar();

    /* Start will contain choice point before executing p(X) */
    Start = ec_ref_create(ec_nil());

    /* Fail loop */
    ec_post_goal(ec_term(p_1,ec_ref_get(X)));
    while(PSUCCEED == ec_resume1(Start))
    {
	/* on each iteration X is instantiated to a different number */
    	if (PSUCCEED == ec_get_long(ec_ref_get(X),&num))
	{
	    printf("p(%d)\n",num);

	    /* at 3 we want to exit the loop, cutting away other choices */
	    if(num == 3)
		ec_cut_to_chp(Start);
	}

	ec_post_goal(ec_atom(fail));
    }

    ec_ref_destroy(X);
    ec_ref_destroy(Start);

    ec_cleanup();
    exit(0);
}

