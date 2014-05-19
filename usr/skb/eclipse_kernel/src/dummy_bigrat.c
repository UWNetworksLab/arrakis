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
 * $Id: dummy_bigrat.c,v 1.1.2.1 2009/01/03 07:46:08 jschimpf Exp $
 *
 *
 * IDENTIFICATION:	dummy_bigrat.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *		dummy to make it work without GNU gmp library
 */

#include "config.h"
#include "sepia.h"
#include "types.h"
#include "error.h"

void
bigrat_init(void)
{
}

extern int
ec_double_to_int_or_bignum(double f, pword *pres)
{
	if (MIN_S_WORD_DBL <= (f) && (f) < MAX_S_WORD_1_DBL)
	{
	    pres->val.nint = (long) (f);
	    pres->tag.kernel = TINT;
	}
	else
	{
	    Bip_Error(ARITH_EXCEPTION);
	}

	Succeed_;
}

extern int
ec_array_to_big(const void *p, int count, int order, int size, int endian, unsigned nails, pword *result)
{
    Bip_Error(ARITH_EXCEPTION);
}

extern int
ec_big_to_chunks(pword *pw1, uword chunksize, pword *result)
{
    Bip_Error(ARITH_EXCEPTION);
}
