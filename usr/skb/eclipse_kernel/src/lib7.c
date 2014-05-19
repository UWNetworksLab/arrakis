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
 * Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: lib7.c,v 1.1 2008/06/30 17:43:56 jschimpf Exp $
 */

/*
 * IDENTIFICATION		lib7.c
 *				This is the sepia library that contains
 *				the dummy version of the MegaLog init 
 *				functions.
 *
 * DESCRIPTION
 *
 * AUTHOR	 VERSION   DATE	  REASON
 * Michel Dahmen    3.0	 27.2.92  created the file
 */

/*ARGSUSED*/
void megalog_init(flags) int flags; {}

/*ARGSUSED*/
void megalog_boot_init(flags) int flags; {}

void megalog_end() {}

int  megalog_present() { return (0); }

