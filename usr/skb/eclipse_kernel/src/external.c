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
 * VERSION	$Id: external.c,v 1.1 2008/06/30 17:43:55 jschimpf Exp $
 */

/*
 * IDENTIFICATION		external.c
 *
 * DESCRIPTION			This file contains the routines which are 
 *				necessary for external predicates, but they 
 *				can be used as well by the built-in predicates.
 *
 */

#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "dict.h"
#include "error.h"
#include "emu_export.h"


int
ec_remember(int n, value v, type t)
{
	control_ptr     p1;
	int             i;
	pword		*q;

	p1.top = B.top - 1;
	i = p1.args - ((pword *) (p1.top->frame.chp + 1));
	if (n > 0 && n <= i)
	{
		p1.any_frame = p1.top->frame;
		p1.args = ((pword *) (p1.chp + 1)) + n - 1;
		if (IsRef(t))
		{
			q = v.ptr;
			Dereference_(q);
			p1.args->val.all = q->val.all;
			p1.args->tag.all = q->tag.all;
		}
		else
		{
			p1.args->val.all = v.all;
			p1.args->tag.all = t.all;
		}
	}
	else
	{
		Bip_Error(RANGE_ERROR);
	}
	return (PSUCCEED);
}

void
cut_external(void)
{
    (B.top-1)->backtrack = external_fail_code_;
}
