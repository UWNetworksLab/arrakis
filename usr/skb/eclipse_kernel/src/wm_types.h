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
**        File: wm_types.h
**      Author: Shyam Mudambi
** Description: Common header file for worker manager files
**
**********************************************************************/

/* WORKER STATUS */
#define NOT_READY 0
#define AWAKE 1
#define SLEEPING 2

#define MAX_MACHINES 128

typedef struct worker_struct * worker_ptr;
struct worker_struct {
  int index;
  int pid;
  int first;   /* whether this was the first worker created */
  nsrv_name_t bport_name;
  bport_id_t bport_id;
  aport_id_t wm_aport_id;
  aport_id_t halt1_aport_id;
  aport_id_t halt2_aport_id;
  int status;
  struct worker_stat_ext start_wstat, cur_wstat;
  worker_ptr next;
} ;

typedef struct worker_struct worker_t;

typedef struct {
  int num_workers;
  int num_awake;
  int auto_start;
  sstring hostname;
  sstring exec_file;
  sstring heap_map_file;
  worker_ptr list;
} machine_t;

typedef struct {
  int num_machines;
  int total_workers;
  int next_id;
  machine_t machines[MAX_MACHINES];
} hdr_mc_list_t;

