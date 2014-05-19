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
 * $Id: eg_c_main.c,v 1.1 2008/06/30 17:43:53 jschimpf Exp $
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
 *	Example of minimal main useing external embed interface.
 */

#include	"eclipse.h"


main(int argc, char **argv)
{
    char 	*s;
    dident	writeln,read,garbage_collect;
    ec_ref Vars,X;
    pword ans;
    
    ec_init();

    Vars = ec_ref_create(ec_nil());
    ec_exec_string("writeln(\"hello world: \"),read(X)",Vars);
    if (PSUCCEED == ec_var_lookup(Vars,"X",&ans) &&
    	PSUCCEED == ec_get_string(ans,&s))
	printf("Answer was %s\n",s);

    ec_ref_destroy(Vars);

    writeln = ec_did("writeln",1);
    read = ec_did("read",1);
    garbage_collect = ec_did("garbage_collect",0);

    X = ec_ref_create_newvar();

    /* writeln("hello again: ") */
    ec_post_goal(ec_term(writeln, ec_string("hello again: ")));
    /* read(X) */
    ec_post_goal(ec_term(read, ec_ref_get(X)));
    /* garbage_collect */
    ec_post_goal(ec_atom(garbage_collect));

    ec_resume1(0); /* pwords not in ec_ref are lost here */

    /* writeln(X) */
    ec_post_goal(ec_term(writeln, ec_ref_get(X)));
    ec_resume1(0);

    ec_ref_destroy(X);

    ec_cleanup();
    exit(0);
}

