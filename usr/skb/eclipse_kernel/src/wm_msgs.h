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
**        File: wm_msgs.h
**      Author: Shyam Mudambi
** Description: Worker Manager messages header file 
**
**********************************************************************/



/* Worker Manager Simple Message Types */
/* use wm_simple_msg_t */
#define DONE_INIT_OPENS 1
#define ALL_CONNECTED 2
#define SENT_INIT_PORT_NAMES 3
#define ROOT_INITIALISED 4
#define EXITING 5
#define HALT_SYSTEM1 6
#define REQ_TIME 7
#define GOTO_SLEEP 10
#define WAKEUP 11
#define CONFIG_NOTIFY 12
#define STATUS_REQ 13
#define STATUS_NOTIFY 14
#define START_INTERFACE 15
#define REMOVE_INTERFACE 16
#define HOST_STATUS_NOTIFY 17
#define WORKER_INFO_NOTIFY 18
#define START_TRACING 19
#define STOP_TRACING 20
#define SET_ONE_SLEEPING 21
#define REQ_START_TIME 22
#define WSTAT_REQ 23
#define WSTAT_RESET 24
#define SET_TRACE_RET 25
#define GET_TRACE_HEADER 26
#define HALT_SYSTEM2 27
#define HALT_SYSTEM_REQ 28
#define REQ_WM_HOSTNAME 29



/* Worker Maganger Complex Message Types */

/* uses port_name_msg_t structure */
#define PORT_NAME 1
/* uses root_node_msg_t structure */
#define ROOT_NODE_REGISTER 1
/* uses time_msg_t structure */
#define SEND_TIME 1
/* uses config_msg_t structure */
#define ADD_WORKERS 1
#define SLEEP_WORKERS 2
#define WAKEUP_WORKERS 3
/* uses host_status_req_msg_t structure */
#define HOST_STATUS_REQ 1
#define WM_HOSTNAME 2
/* uses worker_info_msg_t structure */
#define WORKER_INFO_SET 1
#define WORKER_INFO_GET 2
/* uses hrtime_msg_t */
#define SEND_START_TIME 1
/* uses wstat_msg_t */
#define WSTAT_RET 1
/* uses trace_msg_t */
#define SET_TRACE_HEADER 1
#define GET_TRACE_RET 2


#define MAXHOSTLEN 257
#define MAX_MACHINES 128
#define MAX_PROCS 256

typedef char sstring [MAX_PATH_LEN];

#define ECLIPSE_WM_INTERFACE  1024

typedef struct {
  int msg_type;
  int wid;
} wm_msg_header_t;

typedef struct {
  wm_msg_header_t header;
  int msg_value;
} wm_simple_msg_t;

typedef struct {
  wm_msg_header_t header;
  int index;
  nsrv_name_t port_name;
  aport_id_t wm_aport_id;
} port_name_msg_t;

typedef struct {
  wm_msg_header_t header;
  st_handle_t node;
} node_msg_t;


typedef struct {
  wm_msg_header_t header;
#ifdef HAVE_GETHRTIME
  hrtime_t start_time;
#else
#ifdef BSD_TIMES
  time_t start_time;
#else
  clock_t start_time;
#endif
#endif
} start_time_msg_t;


typedef struct {
  wm_msg_header_t header;
  double cur_time;
} time_msg_t;


typedef struct {
  wm_msg_header_t header;
  int workers;
  nsrv_name_t hostname;
} config_msg_t;

typedef struct {
  wm_msg_header_t header;
  nsrv_name_t hostname;
} host_name_msg_t;

typedef struct {
  int num_workers;
  int num_awake;
  nsrv_name_t hostname;
} mc_status_t;

typedef struct {
  wm_msg_header_t header;
  int num_machines;
  int total_workers;
  mc_status_t machines[MAX_MACHINES];
} status_msg_t;

typedef struct {
  wm_msg_header_t header;
  int num_awake;
  int num_asleep;
  int awake_ids[MAX_PROCS];
  int asleep_ids[MAX_PROCS];
} host_status_msg_t;

typedef struct {
  wm_msg_header_t header;
  struct worker_stat_ext stat;
} wstat_msg_t;

#ifdef WORKER_TRACING
typedef struct {
  wm_msg_header_t header;
  trace_header_t trace_header;
} trace_msg_t;
#endif

typedef struct {
  wm_msg_header_t header;
  int infotype;
  int req_wid;   /* id of requesting worker */
  int pro_wid;   /* id of providing worker */
  int size;
} worker_info_msg_t;
