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
 * Micha Meier, ECRC
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: gencode.h,v 1.3 2008/07/10 00:33:05 jschimpf Exp $
*
 * IDENTIFICATION		gencode.h
 *
 * DESCRIPTION	
 *
 * Macros to store abstract machine code into memory
 */

/*
 * DEFINES:
 */

/* Size of a Debug_call instruction */
#define DEBUG_LENGTH		7

#define FailCode		fail_code_

#define Esize(size)		((vmcode)((size) * (long)sizeof(pword)))

#define Address(arg)		(&g_emu_.emu_args[arg])


/* macro for generating vmcode data words */
#define Store_d(x)		*code++ = (vmcode) (long) (x);

/* macro for generating vmcode instruction words */
#ifdef THREADED
#define Store_i(x)		*code++ = Op_Value(x);
#else
#define Store_i(x)		Store_d(x)
#endif

#define Store_2(i,x)		Store_i(i) Store_d(x)
#define Store_3(i,x,y)		Store_i(i) Store_d(x) Store_d(y)
#define Store_4(i,x,y,z)	Store_i(i) Store_d(x) Store_d(y) Store_d(z)

#define Store_2d(i,x)		Store_d(i) Store_d(x)
#define Store_3d(i,x,y)		Store_d(i) Store_d(x) Store_d(y)
#define Store_4d(i,x,y,z)       Store_d(i) Store_d(x) Store_d(y) Store_d(z)


/*
 * EXTERNAL VARIABLE DECLARATIONS: 
 */
extern vmcode	fail_code_[];
