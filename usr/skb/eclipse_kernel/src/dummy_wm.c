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
 * Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/**********************************************************************
**      System: Parallel Eclipse
**        File: dummy_wm.c
**      Author: Shyam Mudambi
** Description: Worker Manager Interface stubs for Parallel Eclipse
***********************************************************************/

int start_tkwindow()
{
  return 1;
}

int delete_tkwindow()
{
  return 1;
}

int tk_geval(command)
char *command;
{
   return 0;
}

int tk_OneEvent(flag)
int flag;
{
  return 0;
}

int tk_doidledummy()
{
  return 0;
}

int update_perf_window()
{
  return 0;
}
