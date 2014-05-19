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
**        File: wm.h
**      Author: Shyam Mudambi
** Description: Simple Worker Manager for Parallel Eclipse
**
**********************************************************************/

/* Lookup keys for the name server */

#define WM_PORT_NAME		"Worker_Manager"
#define WM_LOW_APORT_NAME	"Worker_Manager_low_aport"
#define WM_HIGH_APORT_NAME	"Worker_Manager_high_aport"
#define WM_HALT1_APORT_NAME	"Worker_Manager_halt1_aport"
#define WM_HALT2_APORT_NAME	"Worker_Manager_halt2_aport"
#define WORKER_PORT_NAME	"parallel_eclipse"


/* How to get the pid from the bport id */

#define BportPid(bport_id) bport_id

/* wm_get_info infotypes */
#define SCHED_INFO 1
#define WTIMES_INFO 2
#define TRACE_INFO 3
#define WSTAT_INFO 4
#define GET_ROOT 5
#define SET_ROOT 6

#if defined(__STDC__)
    extern void setup_mps(int slaveno, 
			  char *session_key, 
			  char *nsrv_hostname,
			  unsigned nsrv_port_number,
			  int create);
    extern void root_node_register(aport_id_t wm_aport_id,
				   st_handle_t * root_id);
    extern st_handle_t * get_root_id(st_handle_t * leaf);
    extern void exit_mps(void);
#else
    extern void setup_mps();
    extern void root_node_register();
    extern st_handle_t * get_root_id();
    extern void exit_mps();
#endif

