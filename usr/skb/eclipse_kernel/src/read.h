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
 * SEPIA INCLUDE FILE
 *
 * VERSION	$Id: read.h,v 1.1 2008/06/30 17:43:58 jschimpf Exp $
 */

/*
 * IDENTIFICATION		read.h
 *
 * DESCRIPTION	
 *	
 *
 * CONTENTS:
 *
 */


/*
 * values for parser options
 */

#define VARNAMES_PLEASE	1
#define LAYOUT_PLEASE	2


Extern int ec_read_term ARGS((stream_id nst, int options, pword *result,
			pword *varlist, int *has_macro, value vm, type tm));
