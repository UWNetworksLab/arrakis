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
 * SEPIA C SOURCE MODULE
 *
 * VERSION	$Id: test.c,v 1.1 2008/06/30 17:43:58 jschimpf Exp $
 */

/*
 * IDENTIFICATION		test.c
 *
 * DESCRIPTION:		SEPIA test file
 *				by Micha Meier
 *
 * CONTENTS: 		Sample code to work out the syntax of the
 *			assembler on the corresponding machine. This
 *			should then help to write the macros in emu_config.h.
 *
 */

#ifndef lint
#include "sepia.h"
#include "types.h"

extern int	I_ChainP();

long op_addr[] = {
(long) I_ChainP
};

static long req_env_table[] = {
(long) I_ChainP,
100L
};

test_procedure(i, f)
int	i;
register int	(** f)();
{
	register long		*pp = &req_env_table[0];
	register long		l = op_addr[0];

	switch (i)
	{
	case 0:
		asm("L1:");
		l = *pp++;
		asm("L2:");
		l = q(l);
		asm("L3:");
		l = (**f)(l);
		break;

	case 1:
		i = 2;
		break;

	case 2:
		i = 9;
		break;

	case 3:
		i = 4;
		break;

	case 4:
		i = 1;
		break;
	}
	return i;
}

	
#endif
