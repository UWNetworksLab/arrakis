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
 * Examples for ECLiPSe C externals, from the User Manual
 *
 * $Id: eg_c_external.c,v 1.1 2008/06/30 17:43:53 jschimpf Exp $
 *
 */

#include "eclipse.h"

int
p_isvar()
{
    return ec_is_var(ec_arg(1));
}

int
p_comp()
{
    return ec_unify_arg(1,
    	ec_compare(ec_arg(2),ec_arg(3)) < 0 ? ec_atom(ec_did("<",0)) :
    	ec_compare(ec_arg(2),ec_arg(3)) > 0 ? ec_atom(ec_did(">",0)) :
	ec_atom(ec_did("=",0)));
}

int
p_string_to_list()
{
    pword  list;
    char *s;
    long len;
    int res;

    res = ec_get_string_length(ec_arg(1), &s, &len);
    if (res != PSUCCEED) return res;

    list = ec_nil();	/* the list is built backwards */
    while (len--)
    {
	list = ec_list(ec_long(s[len]), list);
    }
    return ec_unify_arg(2, list);
}


int
p_sumlist()
{
    int res;
    long x, sum = 0;
    pword list, car, cdr;

    for (list = ec_arg(1); ec_get_list(list,&car,&cdr) == PSUCCEED; list = cdr)
    {
	res = ec_get_long(car, &x);
	if (res != PSUCCEED) return res;
	sum += x;
    }
    res = ec_get_nil(list);
    if (res != PSUCCEED) return res;
    return ec_unify_arg(2, ec_long(sum));
}

