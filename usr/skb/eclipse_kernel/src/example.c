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
 * VERSION	$Id: example.c,v 1.1 2008/06/30 17:43:55 jschimpf Exp $
 *
 * Examples for ECLiPSe C externals, from the User Manual
 *
 */

#include "external.h"

int
p_string_to_list(vs, ts, vl, tl)
value		vs, vl;
type		ts, tl;
{
    pword	list;
    pword	*car;
    pword	*cdr;
    int		len;
    char	*s;

    Check_String(ts);
    Check_Output_List(tl);
    len = StringLength(vs);
    s = StringStart(vs);

    cdr = &list;
    while (len--)
    {
	car = TG;
	Push_List_Frame();
	Make_List(cdr, car);
	Make_Integer(car, *s++);
	cdr = car + 1;
    }
    Make_Nil(cdr);

    Return_Unify_Pw(vl, tl, list.val, list.tag);
}


int
p_sumlist(vl, tl, v, t)
value	vl, v;
type	tl, t;
{
    long sum = 0;
    while (IsList(tl))
    {
	pword *car = vl.ptr;
	pword *cdr = car + 1;

	Dereference_(car);
	Check_Integer(car->tag);
	sum += car->val.nint;
	
	Dereference_(cdr);
	tl = cdr->tag;
	vl = cdr->val;
    }
    Check_List(tl);
    Return_Unify_Integer(v, t, sum);
}
