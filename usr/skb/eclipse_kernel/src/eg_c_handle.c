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
 * $Id: eg_c_handle.c,v 1.1 2008/06/30 17:43:53 jschimpf Exp $
 *
 *
 * IDENTIFICATION:	minimain.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Example of using handles
 */

#include	"eclipse.h"


double my_array[5] = {1.1, 2.2, 3.3, 4.4, 5.5};

main(argc, argv)
int             argc;
char          **argv;
{
    pword	X;

    ec_init();

    X = ec_newvar();
    ec_post_goal(
	ec_term(ec_did(",",2),
	    ec_term(ec_did("xget",3),
		ec_handle(&ec_xt_double_arr, my_array),
		ec_long(3),
		X),
	    ec_term(ec_did("writeln",1), X)
	)
    );

    ec_resume1(0);

    ec_cleanup();
    exit(0);
}

