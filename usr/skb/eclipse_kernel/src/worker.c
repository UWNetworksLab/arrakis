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
**      System: Parallel Distributed System
**        File: worker.c
**      Author: Shyam Mudambi
**		Liang-Liang Li
** Description: Slave process mps setup and handling routines
***********************************************************************/

#include "config.h"
#include <sys/types.h>
#include <sys/param.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/times.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if HAVE_STRING_H
#  include <string.h>
#  ifdef MEMCPY_STRING
#    define bcopy(s1, s2, n)	(void) memcpy((void *)(s2),(void *)(s1), n)
#  endif
#endif
#ifdef MEMCPY_MEMORY
#  include <memory.h>
#  define bcopy(s1, s2, n)	(void) memcpy((char *)(s2), (char *)(s1), n)
#endif

#ifdef PATH_IN_LIMITS
#  include 	<limits.h>
#  define MAX_PATH_LEN	PATH_MAX
#else
#  include <sys/param.h>
#  define MAX_PATH_LEN	MAXPATHLEN
#endif

#ifdef BSD_TIMES
#include <sys/timeb.h>
#endif

#if !defined(HAVE_GETHOSTID)
#include <sys/utsname.h>	/* for uname() */
#endif

#ifdef HAVE_SYS_SYSTEMINFO_H
#include <sys/systeminfo.h>
#endif

#include "sepia.h"
#include <pds.h>   /* pds.h has to be included before types.h since both
                      define machine word size dependent types */
#include <nsrv.h>
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "memman.h"
#include "trace.h"
#include "wm_msgs.h"
#include "wm.h"


#define ST_HANDLE_DS_DEFINED 1
#include "sch_types.h"
#include "sch_eng_interface.h"


/*------------------------------------------------------*/
/* Types and Constant Definitions */
/*------------------------------------------------------*/

#define ParallelWorker (ec_options.parallel_worker)

#define Notify(text) \
{ if (worker_verbose) { printf("%d: %s\n", ParallelWorker, text); }}

typedef struct worker_struct * worker_ptr;
struct worker_struct {
  int index;
  int pid;
  nsrv_name_t bport_name;
  bport_id_t bport_id;
  aport_id_t wm_aport_id;
  int status;
  worker_ptr next;
} ;

typedef struct worker_struct worker_t;

typedef struct {
  int num_workers;
  worker_ptr list;
} hdr_worker_list_t;

/*------------------------------------------------------*/
/* External Variables */
/*------------------------------------------------------*/

extern RETSIGTYPE sigmsg_handler();

extern void	sch_wake_eng(),
		sch_idle_eng(),
		ec_worker_cleanup(),
		unblock_signals();

/* extern void	exit(); */

extern void short_sleep();
void poll_short_sleep();
#ifdef WORKER_TRACING
extern void trace_types_init();
extern trace_header_t * get_trace_ptr();
#endif

/*------------------------------------------------------*/
/* Global Variables */
/*------------------------------------------------------*/

aport_id_t	wm_halt1_aport_id;  
aport_id_t	wm_halt2_aport_id;  
aport_id_t	wm_high_aport_id;  
aport_id_t	wm_low_aport_id;  /* used in trace.c */

static int worker_verbose = 0;
static hdr_worker_list_t worker_list;

st_handle_t root_id;

nsrv_name_t bdomain_key;
nsrv_name_t session_key;          
static nsrv_name_t my_port_name;
static nsrv_name_t my_signature;
bdomain_t bdomain;


bdomain_id_t domain_id; /* only used if this worker creates a new 
			   shared memory domain */         

status_msg_t status;
host_status_msg_t host_status;

static bport_t	wm_bport;
aport_id_t	aports[TOTAL_APORT_NUMBER];
static bport_t my_bport;
static bport_id_t my_bport_id;
static int my_pid;

#if defined(__STDC__)
static volatile int config_ret = 0;
static volatile int status_ret = 0;
static volatile int worker_info_ret = 0;
static volatile int got_wm_hostname = 0;
static volatile int all_connected = 0;
static volatile int got_halt = 0;
static volatile int got_root_id = 0;
static volatile int root_initialised = 0;
static volatile int sent_init_table = 0;
static volatile int wm_b_connected = 0;
static volatile int open_bports = 0;
static volatile int closed_bports = 0;
static volatile double cur_time = 0.0;
#else
static int config_ret = 0;
static int status_ret = 0;
static int worker_info_ret = 0;
static int got_wm_hostname = 0;
static int all_connected = 0;
static int got_halt = 0;
static int got_root_id = 0;
static int root_initialised = 0;
static int sent_init_table = 0;
static int wm_b_connected = 0;
static int open_bports = 0;
static int closed_bports = 0;
static double cur_time = 0.0;
#endif
static char * map_dir;
nsrv_name_t wm_hostname; /* name of worker manager host */
int local_wm = 0;  /* set if worker manager is on the same host */

/* for sending and rec. worker info */
void_ptr worker_info_buf, worker_info_bufin;
int worker_info_bufsize, worker_info_bufinsize;

/* for receiving worker statistics */
struct worker_stat_ext * wstat_rec_buf;

#ifdef WORKER_TRACING
/* for receiving trace header */
trace_header_t * trace_header_buf;
#endif

/* below used for calculating elapsed session time */
/* wm_start_time is only used if we on the same host as the
   worker manager */
#ifdef HAVE_GETHRTIME
static volatile hrtime_t wm_start_time;
#else
#ifdef BSD_TIMES
static volatile time_t wm_start_time;
#else
static volatile clock_t wm_start_time;
#endif
#endif
extern int clock_hz;

/*-----------------------------------------------------------------*/
/*            Port list handling routines                          */
/*-----------------------------------------------------------------*/

void init_worker_list()
{
  worker_list.num_workers = 0;
  worker_list.list = NULL;
}

void insert_port(index,port_name,wm_aport_id)
int index;
nsrv_name_t port_name;
aport_id_t wm_aport_id;

{
  worker_ptr worker;

  worker = (worker_ptr) hp_alloc(sizeof(worker_t));
  worker->index = index;
  (void) strcpy(worker->bport_name,port_name);
  worker->status = 0;
  worker->wm_aport_id = wm_aport_id;
  
  if(worker_list.list == NULL)
    worker->next = NULL;
  else
    worker->next = worker_list.list;

  worker_list.list = worker;
  worker_list.num_workers++;
}

worker_ptr get_worker(wid)
int wid;
{
  worker_ptr cur;
  
  for(cur = worker_list.list; cur != NULL; cur = cur->next)
    if (cur->index == wid)
      return(cur);
  
  return(0);
}
  

int valid_wid(wid)
int wid;
{
  worker_ptr cur;

  for(cur = worker_list.list; cur != NULL; cur = cur->next)
    if (cur->index == wid)
      return(1);

  return(0);
}

/*-----------------------------------------------------------------*/

void check_nsrv(nret,line,err)
nsrv_ret_t nret;
int line;
int err;

{
  if (nret != NSRV_OK)
    {
      printf("**Worker %d: Name Server (nsrv) Fatal Error**\n",ParallelWorker);
      switch(err)
	{
	case 1: 
	  printf("Looks like your nameserver has crashed.\n");
	  printf("Kill all workers, remove all files in $ECLIPSETMP ");
	  printf("and restart peclipse.\n");
	  break;
	case 2:
	  printf("Could not register information with nsrv.\n");
	  break;
	case 3:
	  printf("nsrv_new_bport_id call failed.\n");
	  break;
	case 4:
	  printf("nsrv_bdomain_look_up failed.\n");
	  break;
	case 5:
	  printf("nsrv_bport_look_up failed.\n");
	  break;
	case 6:
	  printf("nsrv_aport_look_up failed.\n");
	  break;
	default:
	  break;
	}
      printf("pid = %d nret = %d at line %d in file %s\n",
	     getpid(),nret,line,__FILE__);
      exit(1); 
    }
}

void check_nsrv_soft(nret,line,err)
nsrv_ret_t nret;
int line;
int err;
{
  if (nret != NSRV_OK)
    {
      printf("**Worker %d: Name Server (nsrv) Error** \n",ParallelWorker);
      switch(err)
	{
	case 1:
	  printf("Could not deregister nameserver information"); 
	  break;
	case 2:
	  printf("Could not free nameserver ids");
	  break;
	default:
	  break;
	}
      printf("pid = %d nret = %d at line %d in file %s\n",
	     getpid(),nret,line,__FILE__);
      printf("Trying to continue execution..\n");
    }
}

void check_bmsg(bret,line,err)
bmsg_ret_t bret;
int line;
int err;
{
  if (bret != BMSG_OK)
    {
      printf("**Worker %d: MPS B-layer Fatal Error: Aborting!\n",ParallelWorker);
      switch(err)
	{
	case 1:
	  printf("Could not initialize Message Passing System.\n");
	  break;
	case 2:
	  printf("bport_port call failed.\n");
	  break;
	case 3:
	  printf("bport_open call failed.\n");
	  break;
	default:
	  break;
	}
      printf("pid = %d bret = %d at line %d in file %s\n",
	     getpid(),bret,line,__FILE__);
      exit(2);
    }
}

void check_amsg(aret,line,err)
amsg_ret_t aret;
int line;
int err;
{
  if (aret != AMSG_OK)
    {
      printf("**Worker %d: MPS A-Layer Fatal Error: Aborting!",ParallelWorker);
      switch(err)
	{
	case 1: 
	  printf("Could not initialize Message Passing System.\n");
	  break;
	case 2:
	  printf("Could not allocate message buffer.");
	  printf("Probably no memory left.\n");
	  break;
	case 3:
	  printf("Error in amsg_send.\n");
	default:
	  break;
	}
      printf("pid = %d aret = %d at line %d in file %s\n",
	     getpid(),aret,line,__FILE__);
      exit(3);
    }
}

void check_amsg_soft(aret,line,err)
amsg_ret_t aret;
int line;
int err;
{
  if (aret != AMSG_OK)
    {
      printf("**Worker %d: MPS A-Layer Error: Warning",ParallelWorker);
      switch(err)
	{
	case 1: 
	  printf("Could not free message buffer.\n");
	  break;
	case 2:
	  printf("Error in receiving messages.\n");
	  break;
	case 3:
	  printf("Error in amsg_send.\n");
	default:
	  break;
	}
      printf("pid = %d aret = %d at line %d in file %s\n",
	     getpid(),aret,line,__FILE__);
      printf("Trying to continue execution..\n");
    }
}


/*-----------------------------------------------------------*/
/*  Panic and warning routines */
/*-----------------------------------------------------------*/

static bport_id_t nsrv_port_id;


/*ARGSUSED*/
void
worker_bport_notify(port_id,port_primitive)
    bport_id_t port_id;
    bport_primitive_t port_primitive;
{
    switch (port_primitive) {
        case BPORT_OPEN :
/* 	    printf("%d: bport_notify: BPORT_OPEN:%d\n",ParallelWorker,port_id); */
	    open_bports++;
            break;
        case BPORT_CLOSE :
/*	    printf("%d: bport_notify: BPORT_CLOSE:%d\n",ParallelWorker,port_id); */
	    closed_bports++;
            break;
        case BPORT_BLOCK :
            break;
        case BPORT_UNBLOCK :
            break;
        default:
            break;
    }
}

int mem_worker_list(bport_id)
     bport_id_t bport_id;

{
  worker_ptr cur;

  for(cur = worker_list.list; cur != NULL; cur = cur->next)
    if (cur->bport_id == bport_id)
      return(1);

  return(0);
}

void
worker_bport_ack(port_id,port_primitive,ret)
    bport_id_t port_id;
    bport_primitive_t port_primitive;
    bmsg_ret_t ret;
{

    if (ret == BMSG_OK) {
        switch (port_primitive) {
            case BPORT_OPEN :
/*	        printf("%d: bport_ack: BPORT_OPEN:%d\n",ParallelWorker,port_id); */
		if (port_id == wm_bport.bport_id)
		    wm_b_connected = 1;
		else if (mem_worker_list(port_id))
		    open_bports++;
		else
		  nsrv_port_id = port_id;
                break;
            case BPORT_CLOSE :
/*	        printf("%d: bport_ack: BPORT_CLOSE:%d\n",ParallelWorker,port_id); */
	        closed_bports++;
                break;
            case BPORT_BLOCK :
                break;
            case BPORT_UNBLOCK :
                break;
            default:
                break;
        }
      }
    else
      {
	printf("%d: Bport_ack: error in ",ParallelWorker);
	switch (port_primitive) {
	case BPORT_OPEN :
	  printf("opening ");
	  break;
	case BPORT_CLOSE :
	  printf("closing ");
	  break;
	case BPORT_BLOCK :
	  printf("blocking ");
	  break;
	case BPORT_UNBLOCK :
	  printf("unblocking ");
	  break;
	default:
	  break;
        }
	printf(" bport_id %d\n", port_id);
      }
	return;
}



/*-----------------------------------------------------------------*/
/*       Initialise Message Types                                  */
/*-----------------------------------------------------------------*/

amsg_type_t mdt_wm_header;
amsg_type_t mdt_wm_simple;
amsg_type_t mdt_wm_nsrvname;
amsg_type_t mdt_wm_config;
amsg_type_t mdt_wm_node;
amsg_type_t mdt_sthandlet;
amsg_type_t mdt_wm_mcstatus;
amsg_type_t mdt_wm_status;
amsg_type_t mdt_wm_hoststatus;
amsg_type_t mdt_wm_time;
amsg_type_t mdt_wm_starttime;
amsg_type_t mdt_wm_portname;
amsg_type_t mdt_wm_wstat;
#ifdef WORKER_TRACING
amsg_type_t mdt_wm_trace;
amsg_type_t mdt_trace;
#endif
amsg_type_t mdt_wstat;

wm_types_init()
{
  amsg_typedef_t template[20];

  /* wm_msg_header_t */
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = MDT_INT32;
  template[3] = MDT_INT32;
  template[4] = MDT_STRUCT_CLOSE;
  template[5] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 1, 
		       template, &mdt_wm_header), __LINE__, 4);
  
  /* wm_simple_msg_t */
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
  template[3] = MDT_INT32;
  template[4] = MDT_STRUCT_CLOSE;
  template[5] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 2, 
		       template, &mdt_wm_simple), __LINE__, 4);
  
  /* host_status_req_t structures */
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
  template[3] = MDT_NSRVNAME;
  template[4] = MDT_STRUCT_CLOSE;
  template[5] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 3, 
		       template, &mdt_wm_nsrvname), __LINE__, 4);

  /* st_handle_t */
  
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = MDT_APORTID;
  template[3] = MDT_UINT32;
  template[4] = MDT_UINT32;
  template[5] = MDT_STRUCT_CLOSE;
  template[6] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 4, 
		       template, &mdt_sthandlet), __LINE__, 4);
  
  /* node_msg_t */

  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
  template[3] = mdt_sthandlet;
  template[4] = MDT_STRUCT_CLOSE;
  template[5] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 5, 
		       template, &mdt_wm_node), __LINE__, 4);

  /* config_msg_t */
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
  template[3] = MDT_INT32;
  template[4] = MDT_NSRVNAME;
  template[5] = MDT_STRUCT_CLOSE;
  template[6] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 6, 
		       template, &mdt_wm_config), __LINE__, 4);

  /* mc_status_t */
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = MDT_INT32;
  template[3] = MDT_INT32;
  template[4] = MDT_NSRVNAME;
  template[5] = MDT_STRUCT_CLOSE;
  template[6] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 7, 
		       template, &mdt_wm_mcstatus), __LINE__, 4);

  /* mc_status_t */
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
  template[3] = MDT_INT32;
  template[4] = MDT_INT32;
  template[5] = MDT_ARRAY_OF;
  template[6] = MAX_MACHINES;
  template[7] = mdt_wm_mcstatus;
  template[8] = MDT_STRUCT_CLOSE;
  template[9] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 8, 
		       template, &mdt_wm_status), __LINE__, 4);

  /* host_status_msg_t */
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
  template[3] = MDT_INT32;
  template[4] = MDT_INT32;
  template[5] = MDT_ARRAY_OF;
  template[6] = MAX_PROCS;
  template[7] = MDT_INT32;
  template[8] = MDT_ARRAY_OF;
  template[9] = MAX_PROCS;
  template[10] = MDT_INT32;
  template[11] = MDT_STRUCT_CLOSE;
  template[12] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 9, 
		       template, &mdt_wm_hoststatus), __LINE__, 4);


  /* start_time_msg_t */

  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
#ifdef HAVE_GETHRTIME
  template[3] = MDT_DWORD;
#else  
  template[3] = MDT_UINT32;
#endif
  template[4] = MDT_STRUCT_CLOSE;
  template[5] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 10, 
		       template, &mdt_wm_starttime), __LINE__, 4);
  
  /* time_msg_t */

  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
  template[3] = MDT_DP_FLOAT;
  template[4] = MDT_STRUCT_CLOSE;
  template[5] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 11, 
		       template, &mdt_wm_time), __LINE__, 4);
  
  /* port_name_msg_t */ 
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
  template[3] = MDT_INT32;
  template[4] = MDT_NSRVNAME;
  template[5] = MDT_APORTID;
  template[6] = MDT_STRUCT_CLOSE;
  template[7] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 12, 
		       template, &mdt_wm_portname), __LINE__, 4);

  wstat_types_init(&mdt_wstat);
  /* wstat_msg_t */
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
  template[3] = mdt_wstat;
  template[4] = MDT_STRUCT_CLOSE;
  template[5] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 13, 
		       template, &mdt_wm_wstat), __LINE__, 4);

  /* trace_msg_t */
#ifdef WORKER_TRACING
  trace_types_init(&mdt_trace);
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = mdt_wm_header;
  template[3] = mdt_trace;
  template[4] = MDT_STRUCT_CLOSE;
  template[5] = MDT_END;

  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 14, 
		       template, &mdt_wm_trace), __LINE__, 4);
#endif
}

void send_simple_wm_message(port_id,msg_type,msg_value)
aport_id_t port_id;
int msg_type;  
int msg_value;
{
 amsg_t msg;
 amsg_data_t * msg_data;
 wm_simple_msg_t * wm_mess_hdr;
 
 amsg_size_t size;

 size = sizeof(wm_simple_msg_t);

 check_amsg(amsg_alloc(size,
		       &msg_data,
		       &msg),__LINE__, 2);
 
 wm_mess_hdr = (wm_simple_msg_t *) msg_data;
 wm_mess_hdr->header.msg_type = msg_type;
 wm_mess_hdr->header.wid = ParallelWorker;
 wm_mess_hdr->msg_value = msg_value;
 check_amsg_soft(amsg_send(port_id,msg,mdt_wm_simple,1,0),__LINE__, 3);
}

/*-----------------------------------------------------------------*/
/*       Aport Notify Procedures                                   */
/*-----------------------------------------------------------------*/

void
halt1_notify(port_id)
    aport_id_t port_id;
{
    amsg_ret_t ret;
    amsg_t msg;
    amsg_data_t * msg_data;
    amsg_type_t msg_type;
    amsg_count_t msg_count;

    ret = amsg_receive(port_id,
                                &msg,
                                &msg_data,
                                &msg_type,
                                &msg_count, 0);
    if (ret == AMSG_OK) {
       Notify("Got HALT_SYSTEM1 message");
       ec_worker_cleanup();
       exit(1);
    }
}

/*ARGSUSED*/
void
halt2_notify(port_id)
   aport_id_t port_id;
{
   Notify("Got HALT_SYSTEM2 message");
   got_halt = 1;
}

void
wm_notify(port_id)
     aport_id_t port_id;
     
{
  amsg_ret_t ret;
  amsg_t msg;
  amsg_data_t * msg_data;
  amsg_type_t msg_type;
  amsg_count_t msg_count;
  wm_simple_msg_t * wm_simple_msg;
  
  while (( ret = amsg_receive(port_id,
			      &msg,
			      &msg_data,
			      &msg_type,
			      &msg_count,0)) == AMSG_OK) {
    
    if (msg_type == mdt_wm_simple) 
      {
	wm_simple_msg = (wm_simple_msg_t *) msg_data;
	switch(wm_simple_msg->header.msg_type) {
	  
	case SENT_INIT_PORT_NAMES:
	  Notify("Got SENT_INIT_PORT_NAMES message");
	  sent_init_table = 1;
	  break;
	  
	case ALL_CONNECTED:
	  Notify("Got ALL_CONNECTED message");
	  all_connected = 1;
	  break;
	  
	case ROOT_INITIALISED:
	  Notify("Got ROOT_INITIALISED message");
	  root_initialised = 1;
	  break;

	case GOTO_SLEEP:
	  Notify("Got GOTO_SLEEP message");
	  (void) sch_idle_eng(aports[SCH_APORT_NUMBER]);
	  break;
	  
	case WAKEUP:
	  Notify("Got WAKEUP message");
	  (void) sch_wake_eng(aports[SCH_APORT_NUMBER]);
	  break;
	  
	case CONFIG_NOTIFY:
	  Notify("Got CONFIG_NOTIFY message");
	  config_ret = wm_simple_msg->msg_value;
	  break;
	  
	case WSTAT_REQ:
	  {
	    Notify("Got WSTAT_REQ message");
	    send_wstat(wm_simple_msg->header.wid);
	  }
	  break;
       
	case WSTAT_RESET:
	  {
	    int wid = wm_simple_msg->header.wid;
	   
	    Notify("Got WSTAT_RESET message");
	    reset_worker_stat();
	    send_simple_wm_message((get_worker(wid))->wm_aport_id, WSTAT_RET, 0);
	    break;
	  }

	case WSTAT_RET:
	  Notify("Got WSTAT_RET message");
	  worker_info_ret = 1;
	  break;

#ifdef WORKER_TRACING
	case START_TRACING:
	  Notify("Got START_TRACING message");
	  start_tracing();
	  break;
	  
	case STOP_TRACING:
	  Notify("Got STOP_TRACING message");
	  stop_tracing();
	  break;

	case GET_TRACE_HEADER:
	  Notify("Got GET_TRACE_HEADER message");
	  send_trace_header(wm_simple_msg->header.wid,
			    GET_TRACE_RET, get_trace_ptr());
	  break;

	case SET_TRACE_RET:
	  Notify("Got SET_TRACE_HEADER message");
	  worker_info_ret = 1;
	  break;
#endif
	  
	default:
	  break;
	}
      }
    else if (msg_type == mdt_wm_portname)
      {
	/*  PORT_NAME message */
	port_name_msg_t * port_name_msg;
	Notify("Got PORT_NAME message");
	port_name_msg = (port_name_msg_t *) msg_data;
	insert_port(port_name_msg->index,port_name_msg->port_name,
		    port_name_msg->wm_aport_id);
      }
    else if (msg_type == mdt_wm_nsrvname)
      {
	/* WM_HOSTNAME message */
	host_name_msg_t *host_name_msg;
	host_name_msg = (host_name_msg_t *) msg_data;
	(void) strcpy(wm_hostname,host_name_msg->hostname);
	got_wm_hostname = 1;
      }
    else if (msg_type == mdt_wm_node)
      {
	/* SET_ROOT message */
	node_msg_t *node_msg;
	node_msg = (node_msg_t *) msg_data;
	if(node_msg->header.msg_type == SET_ROOT)
	  {
	    Notify("Got SET_ROOT message");
	    root_id = node_msg->node;
	    got_root_id = 1;
	  }
	else if (node_msg->header.msg_type == GET_ROOT)
	  {
	    st_handle_t local;
	    Notify("Got GET_ROOT message");
	    local = node_msg->node;
	    sch_init_lodge(aports[SCH_APORT_NUMBER], node_msg->header.wid,
			   &local);
	  }
      }
    else if (msg_type == mdt_wm_time)
      {
	/*  SEND_TIME message */
	time_msg_t * time_msg;
	Notify("Got SEND_TIME message");
	time_msg = (time_msg_t *) msg_data;
	cur_time = time_msg->cur_time;
      }
    
    else if (msg_type == mdt_wm_starttime)
      {
	/*  SEND_START_TIME message */
	start_time_msg_t * start_time_msg;
	start_time_msg = (start_time_msg_t *) msg_data;
	Notify("Got SEND_START_TIME message");
	wm_start_time = start_time_msg->start_time;
      }
    else if (msg_type == mdt_wm_status)
      {
	/* STATUS_NOTIFY message */
	status_msg_t * status_msg;
	Notify("Got STATUS_NOTIFY message");
	status_msg = (status_msg_t *) msg_data;
	{
	  int i;
	  status.num_machines = status_msg->num_machines;
	  for(i = 0 ; i < status_msg->num_machines; i++)
	    status.machines[i] = status_msg->machines[i];
	}
	status_ret = status_msg->total_workers;
      }
    
    else if (msg_type == mdt_wm_hoststatus)
      {
	/*  HOST_STATUS_NOTIFY */
	host_status_msg_t * host_status_msg;
	int i;
	
	Notify("Got HOST_STATUS_NOTIFY message");
	host_status_msg = (host_status_msg_t *) msg_data;
	host_status.num_awake = host_status_msg->num_awake;
	host_status.num_asleep = host_status_msg->num_asleep;
	for(i = 0; i < host_status.num_awake; i++)
	  host_status.awake_ids[i] = host_status_msg->awake_ids[i];
	for(i = 0; i < host_status.num_asleep; i++)
	  host_status.asleep_ids[i] = host_status_msg->asleep_ids[i];
	
	status_ret = host_status.num_awake + host_status.num_asleep;
      }
    else if (msg_type == mdt_wm_wstat)
      {
	/* WSTAT_RET */
	wstat_msg_t * wstat_msg;
	Notify("Got WSTAT_RET message");
	wstat_msg = (wstat_msg_t *) msg_data;
	*wstat_rec_buf = wstat_msg->stat;
	worker_info_ret = 1;
	break;
      }
#ifdef WORKER_TRACING
    else if (msg_type == mdt_wm_trace)
      {
	trace_msg_t * trace_msg;
	int wid;

	trace_msg = (trace_msg_t *) msg_data;
	wid = trace_msg->header.wid;
	if(trace_msg->header.msg_type == GET_TRACE_RET)
	  {
	    *trace_header_buf = trace_msg->trace_header;
	    worker_info_ret = 1;
	  }
	else if (trace_msg->header.msg_type == SET_TRACE_HEADER)
	  {
	    print_trace(&(trace_msg->trace_header));
	    send_simple_wm_message(get_worker(wid)->wm_aport_id, 
				   SET_TRACE_RET, 0);
	  }
	break;
      }
#endif
    else if (msg_type == MDT_BYTE)
      {
	worker_info_msg_t * worker_info_msg;
	worker_info_msg = (worker_info_msg_t *) msg_data;
	switch (worker_info_msg->header.msg_type)
	  {
	  case WORKER_INFO_SET:
	    Notify("Got WORKER_INFO_SET message");
	    set_worker_info(worker_info_msg->req_wid, 
			    worker_info_msg->infotype, 
			    (void_ptr) (worker_info_msg + 1));
	    send_worker_info(worker_info_msg->req_wid, 
			     worker_info_msg->infotype,
			     0, (void_ptr) NULL); 
	    break;
	    
	  case WORKER_INFO_GET:
	    Notify("Got WORKER_INFO_GET message");
	    get_worker_info(worker_info_msg->infotype, 
			    &worker_info_bufinsize, 
			    &worker_info_bufin);
	    send_worker_info(worker_info_msg->req_wid, 
			     worker_info_msg->infotype,
			     worker_info_bufinsize, worker_info_bufin); 
	    break;
	    
	  case WORKER_INFO_NOTIFY:
	    Notify("Got WORKER_INFO_NOTIFY message");
	      if(worker_info_bufsize >= worker_info_msg->size)
		bcopy((char *) (msg_data + sizeof(worker_info_msg_t)), 
			(char *) worker_info_buf, 
			worker_info_msg->size);
	      else
		bcopy((char *) (msg_data + sizeof(worker_info_msg_t)), 
			(char *) worker_info_buf, 
		       worker_info_bufsize);
	    worker_info_ret = 1;
	    break;
	  }
      }
    check_amsg_soft(amsg_free(msg), __LINE__, 1);
  }
  if (ret != AMSG_NOMESSAGE) {
    check_amsg_soft(ret,__LINE__, 2);
  }
}

void fill_in_domain(ldomain_id, lbdomain, hostname)
bdomain_id_t ldomain_id;
bdomain_t *lbdomain;
char *hostname;

{
  lbdomain->bdomain_id = ldomain_id;
  lbdomain->bdomain_size = 0x00800000;  /* 8 MB */
  if (!shared_mem_base())
    lbdomain->bdomain_start = (bmem_address_t) (shared_mem_base());
  else
    lbdomain->bdomain_start = (bmem_address_t) (shared_mem_base() + 0x00800000);
  sprintf(lbdomain->bdomain_file,"%s/%d.%s.mps.map", 
	  map_dir, my_pid, hostname);
}

void mps_init(create)
int create;
{

  nsrv_ret_t nret;
  char hostname[MAXHOSTLEN];
  nsrv_name_t domain_name;
  bmsg_ret_t bret;

  check_nsrv(nsrv_new_bport_id(my_signature,&my_bport_id),__LINE__, 3);

  mygethostname(hostname);

/* Turn on the define below if you want each worker to create its
   own shared memory domain. This means that all communication will
   take place via sockets i.e. no shared memory communication. */

/*#define TEST_INTER_DOMAIN 1*/
#ifdef TEST_INTER_DOMAIN
  sprintf(domain_name,"%s.%s.%s", WORKER_PORT_NAME, hostname,my_signature);
  check_nsrv(nsrv_new_bdomain_id(my_signature,&domain_id),__LINE__,5);
  fill_in_domain(domain_id,&bdomain,hostname);
  check_bmsg(bmsg_init(my_bport_id,&bdomain, 
		       BDOMAIN_CREATE |
#ifdef DEBUG_MPS
		       BMSG_ALOG_ON |
		       BMSG_ALOG_OPEN |
		       BMSG_ALOG_CLOSE |
		       BMSG_ALOG_MASTER |
#endif
		       BPORT_NOTIFICATION), __LINE__, 1);
  check_nsrv(nsrv_bdomain_register(bdomain_key,domain_name,
				   my_signature,&bdomain),__LINE__,2);
#else /* TEST_INTER_DOMAIN */

  sprintf(domain_name,"%s.%s.%s", WORKER_PORT_NAME, hostname,session_key);
  /* Check if domain exists - it will exist if we are on the
     same host as the worker manager */
  nret = nsrv_bdomain_look_up(bdomain_key,domain_name,&bdomain);

  /* Loop until the domain is created - should have a timeout? */
  while((nret != NSRV_OK) && !create) {
      short_sleep(1000);
      nret = nsrv_bdomain_look_up(bdomain_key,domain_name,&bdomain);
    }
      
  if (nret == NSRV_OK)
      check_bmsg(bmsg_init(my_bport_id,&bdomain,
#ifdef DEBUG_MPS
			   BMSG_ALOG_ON |
			   BMSG_ALOG_OPEN |
			   BMSG_ALOG_CLOSE |
			   BMSG_ALOG_MASTER |
#endif
			   BPORT_NOTIFICATION),__LINE__, 1);
  else if ((nret == NSRV_NOT_REGISTERED)  && create)
    { /* we are the first worker on a new host, so create a new
         domain and register it */
      check_nsrv(nsrv_new_bdomain_id(my_signature,&domain_id),__LINE__,5);
      fill_in_domain(domain_id,&bdomain,hostname);
      check_bmsg(bmsg_init(my_bport_id,&bdomain, 
			   BDOMAIN_CREATE |
#ifdef DEBUG_MPS
			   BMSG_ALOG_ON |
			   BMSG_ALOG_OPEN |
			   BMSG_ALOG_CLOSE |
			   BMSG_ALOG_MASTER |
#endif
			   BPORT_NOTIFICATION), __LINE__, 1);
      check_nsrv(nsrv_bdomain_register(bdomain_key,domain_name,
				       my_signature,&bdomain),__LINE__,2);
    }
  else
    {
      printf("nsrv_bdomain_look_up failed! nret = %d\n",nret);
      exit(1);
    }
#endif /* TEST_INTER_DOMAIN */
  check_bmsg(bport_port(my_bport_id,&my_bport),__LINE__, 2);
  check_nsrv(nsrv_bport_register(session_key,my_port_name,my_signature,&my_bport),__LINE__,2);

  bret = bport_open(&wm_bport);
  if (bret != BMSG_POPENING)
    check_bmsg(bret,__LINE__, 3);
}

mygethostname(host)
     char *host;
{
#if defined(HAVE_GETHOSTNAME)
    (void) gethostname(host,MAXHOSTLEN);
#else
#  if defined(HAVE_SYSINFO) && defined(HAVE_SYS_SYSTEMINFO_H)
    sysinfo(SI_HOSTNAME, host, MAXHOSTLEN);
#  else
    struct utsname ut;
    uname(&ut);
    (void) strcpy(host,ut.nodename);
#  endif
#endif
}

void wait_for_session_table()
{
  while(!sent_init_table) 
    short_sleep(1000);
}

void open_worker_connections()
{
  bport_t rem_bport;
  bmsg_ret_t bret;
  worker_ptr cur;

  for(cur = worker_list.list; cur != NULL; cur = cur->next)
    {
      check_nsrv(nsrv_bport_look_up(session_key,cur->bport_name,&rem_bport),
		 __LINE__,5);
      cur->bport_id = rem_bport.bport_id;
      if(cur->index < ParallelWorker)
	{
	  bret = bport_open(&rem_bport);
	  if (bret != BMSG_POPENING)
	    check_bmsg(bret,__LINE__, 3);
	}
    }

  while (open_bports != (worker_list.num_workers - 1))
    short_sleep(1000);
}

void register_std_aports(aport_ids)
     aport_id_t aport_ids[];
{
  nsrv_name_t aport_name;
  aport_t aport;

  sprintf(aport_name,"%s_halt2_aport",my_port_name);
  aport.aport_id = aport_ids[HALT2_APORT_NUMBER];
  aport.bport_id = my_bport_id;
  aport.bdomain_id = bdomain.bdomain_id;
  check_nsrv(nsrv_aport_register(session_key,aport_name,
				 my_signature,&aport),__LINE__, 2);
  sprintf(aport_name,"%s_halt1_aport",my_port_name);
  aport.aport_id = aport_ids[HALT1_APORT_NUMBER];
  aport.bport_id = my_bport_id;
  aport.bdomain_id = bdomain.bdomain_id;
  check_nsrv(nsrv_aport_register(session_key,aport_name,
				 my_signature,&aport),__LINE__, 2);
  wm_types_init();  /* can only be done after an aport registration 
		       since MDT_NSRVNAME is only defined at this point */
  sprintf(aport_name,"%s_wm_aport",my_port_name);
  aport.aport_id = aport_ids[WM_APORT_NUMBER];
  aport.bport_id = my_bport_id;
  aport.bdomain_id = bdomain.bdomain_id;
  check_nsrv(nsrv_aport_register(session_key,aport_name,
				 my_signature,&aport),__LINE__, 2);


}

void deregister_std_aports()
{
  nsrv_name_t aport_name;

  sprintf(aport_name,"%s_wm_aport",my_port_name);
  check_nsrv_soft(nsrv_aport_deregister(session_key,aport_name,my_signature),
	     __LINE__, 1);

  sprintf(aport_name,"%s_halt1_aport",my_port_name);
  check_nsrv_soft(nsrv_aport_deregister(session_key,aport_name,my_signature),
		  __LINE__, 1);
  sprintf(aport_name,"%s_halt2_aport",my_port_name);
  check_nsrv_soft(nsrv_aport_deregister(session_key,aport_name,my_signature),
		  __LINE__, 1);
}
 

void setup_mps(slave_no,session, nsrv_host_name, nsrv_port_number, create)
     int slave_no;
     char *session;
     char *nsrv_host_name;
     unsigned nsrv_port_number;
     int create;      /* used in mps_init */
{
  extern void eng_port_upcall();
  extern void sch_port_upcall();
  extern void io_port_upcall();
  aport_t wm_aport; /* needed to lookup wm aportids in name server */
  void (* notify_procs [TOTAL_APORT_NUMBER]) ();

  /* preliminary setting of the SIGIO handler for message passing */
#ifdef HAVE_SIGACTION
  {
    struct sigaction sa;
    sa.sa_handler = sigmsg_handler;
    (void) sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    (void) sigaction(SIGIO, &sa, (struct sigaction *) 0);
#ifdef SIGPOLL
    (void) sigaction(SIGPOLL, &sa, (struct sigaction *) 0);
#endif
  }
#else
  signal(SIGIO, sigmsg_handler);
#ifdef SIGPOLL
  signal(SIGPOLL, sigmsg_handler);
#endif
#endif

  map_dir = getenv("ECLIPSETMP");
  if(!map_dir) map_dir = "/tmp";

  check_nsrv(nsrv_init(nsrv_host_name, &nsrv_port_number),__LINE__, 1);

  my_pid = getpid();
  (void) strcpy(session_key,session);
  sprintf(my_port_name,"worker%d",slave_no);
  sprintf(my_signature,"%d",my_pid);
  sprintf(bdomain_key, WORKER_PORT_NAME);
  init_worker_list();
  
  check_nsrv(nsrv_bport_look_up(session_key, WM_PORT_NAME, &wm_bport),
	     __LINE__, 5);

  mps_init(create);

  notify_procs[HALT1_APORT_NUMBER] = halt1_notify;
  notify_procs[HALT2_APORT_NUMBER] = halt2_notify;
  notify_procs[WM_APORT_NUMBER]     = wm_notify;
  notify_procs[SCH_APORT_NUMBER] = sch_port_upcall;
  notify_procs[ENG_APORT_NUMBER] = eng_port_upcall;
  notify_procs[IO_APORT_NUMBER] = io_port_upcall;
  notify_procs[IO_REPLY_APORT_NUMBER] = 0;
  check_amsg(amsg_init(TOTAL_APORT_NUMBER,notify_procs,aports,0),__LINE__, 1);

  /* set port notification levels */
  check_amsg_soft(aport_set_option(aports[IO_APORT_NUMBER],
				   APORT_NOTIFY_LEVEL,
				   (aport_optval_t) 1),__LINE__, 4);
  check_amsg_soft(aport_set_option(aports[ENG_APORT_NUMBER],
				   APORT_NOTIFY_LEVEL,
				   (aport_optval_t) 2),__LINE__, 4);
  check_amsg_soft(aport_set_option(aports[SCH_APORT_NUMBER],
				   APORT_NOTIFY_LEVEL,
				   (aport_optval_t) 2),__LINE__, 4);
  check_amsg_soft(aport_set_option(aports[WM_APORT_NUMBER],
				   APORT_NOTIFY_LEVEL,
				   (aport_optval_t) 3),__LINE__, 4);
  check_amsg_soft(aport_set_option(aports[HALT1_APORT_NUMBER],
				   APORT_NOTIFY_LEVEL,
				   (aport_optval_t) 4),__LINE__, 4);
  check_amsg_soft(aport_set_option(aports[HALT2_APORT_NUMBER],
				   APORT_NOTIFY_LEVEL,
				   (aport_optval_t) 5),__LINE__, 4);

  register_std_aports(aports);

  check_nsrv(nsrv_aport_look_up(session_key, WM_HIGH_APORT_NAME, &wm_aport),
             __LINE__, 6);
  wm_high_aport_id = wm_aport.aport_id;

  check_nsrv(nsrv_aport_look_up(session_key, WM_HALT1_APORT_NAME, &wm_aport),
             __LINE__, 6);
  wm_halt1_aport_id = wm_aport.aport_id;

  check_nsrv(nsrv_aport_look_up(session_key, WM_HALT2_APORT_NAME, &wm_aport),
             __LINE__, 6);
  wm_halt2_aport_id = wm_aport.aport_id;

  check_nsrv(nsrv_aport_look_up(session_key, WM_LOW_APORT_NAME, 
				&wm_aport),__LINE__, 6);
  wm_low_aport_id = wm_aport.aport_id;

  while (!wm_b_connected) poll_short_sleep(1000);

  wait_for_session_table();

  open_worker_connections();

#ifdef WORKER_TRACING
  init_trace_file_names();
#endif

  set_local_wm_flag();
  set_start_time();

  /* Tell wm that my mps setup is done */
  send_simple_wm_message(wm_high_aport_id,DONE_INIT_OPENS,0);

  /* Synchronize with all other workers since even after this point
     the worker is not completely set up - it still needs to lodge at
     the root node. This can cause workers not to exit if the system
     is halted immediately after adding this worker.  Currently the
     worker manager just kills the workers and exits (using a timeout)*/
     
  while(!all_connected)
    poll_short_sleep(1000);

}

void block_wm_aports()
{
  (void) aport_set_option(aports[WM_APORT_NUMBER],
                                APORT_NOTIFY, (aport_optval_t) AMSG_OFF);
  (void) aport_set_option(aports[SCH_APORT_NUMBER],
                                APORT_NOTIFY, (aport_optval_t) AMSG_OFF);
  (void) aport_set_option(aports[ENG_APORT_NUMBER],
                                APORT_NOTIFY, (aport_optval_t) AMSG_OFF);
  (void) aport_set_option(aports[IO_APORT_NUMBER],
                                APORT_NOTIFY, (aport_optval_t) AMSG_OFF);
  (void) bmsg_set_option(BMSG_INTRA_DOMAIN_TRIGGERING, BMSG_ON);
}

/*ARGSUSED*/
void halt_system(exit_code)
int exit_code;
{
  (void) block_wm_aports();
  unblock_signals();
  send_simple_wm_message(wm_halt1_aport_id,HALT_SYSTEM_REQ,exit_code);
  while (1)
    poll_short_sleep(1000);
}
/*ARGSUSED*/
void panic_halt_system(exit_code)
int exit_code;
{
  halt_system(exit_code);
}

void exit_mps()
{
  worker_ptr cur;
  bmsg_ret_t bret;

  send_simple_wm_message(wm_halt2_aport_id,EXITING,0);
  while(!got_halt) {
      poll_short_sleep(1000);
  }
  for(cur = worker_list.list; cur != NULL; cur = cur->next)
    {
      if (cur->index < ParallelWorker)
	{
	  bret = bport_close(cur->bport_id);
	  while (bret != BMSG_NOPORT)
	    {
/*	      printf("slave %d: Cannot close port %d bret = %d\n",
		     ParallelWorker,cur->bport_id,bret);*/
	      poll_short_sleep(100000);
	      bret = bport_close(cur->bport_id);
	    }
	}
    }

  while (closed_bports != (worker_list.num_workers - 1))
      poll_short_sleep(1000);

  bret = bport_close(wm_bport.bport_id);
  while (bret != BMSG_NOPORT)
  {
/*    printf("slave %d: Cannot close port %d bret = %d\n",
	     ParallelWorker,wm_bport.bport_id,bret);*/
      poll_short_sleep(100000);
      bret = bport_close(wm_bport.bport_id);
  }

  while (closed_bports != (worker_list.num_workers))
      poll_short_sleep(1000);

  deregister_std_aports();
  check_nsrv_soft(nsrv_bport_deregister(session_key,my_port_name,my_signature),
	     __LINE__, 1);
  check_nsrv_soft(nsrv_free_bport_id(my_signature,my_bport_id),__LINE__, 2);

  nsrv_exit();
  amsg_exit();
  bmsg_exit();
}

/*-------------------------------------------------------------------*/
/* Scheduler initialization support functions */
/*-------------------------------------------------------------------*/

void send_node_msg(wm_aport_id, msg_type, node)
aport_id_t wm_aport_id;
int msg_type;
st_handle_t * node;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  node_msg_t * node_msg;
  st_handle_t *local_node;
  
  amsg_size_t size;
  
  size = sizeof(node_msg_t);
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__, 2);
  
  node_msg = (node_msg_t *) msg_data;
  node_msg->header.msg_type = msg_type;
  node_msg->header.wid = ParallelWorker;
  
  local_node = (st_handle_t *) &(node_msg->node);
  *local_node = *node;
  check_amsg(amsg_send(wm_aport_id,msg,mdt_wm_node,1,0),__LINE__, 3);
}

st_handle_t *
get_root_id(leaf)
st_handle_t * leaf;
{
  /* wait until first worker has initialised the root */
  while(!root_initialised) poll_short_sleep(1000);

  /* Now send first worker my leaf id  */
  send_node_msg((get_worker(1))->wm_aport_id, GET_ROOT, leaf);

  /* wait until I have received my root id */
  while(!got_root_id) poll_short_sleep(1000);
  return &root_id;
}

/* Only called by first worker.
   Send req_wid the root.
   Called in response to GET_ROOT message (see above)
*/
void wm_init_lodged(req_wid, root)
int req_wid;
st_handle_t *root;
{
  send_node_msg((get_worker(req_wid))->wm_aport_id,SET_ROOT, root);
}

/* Called only by first worker.
   Inform wm that root node has been initialised 
*/
void root_node_register(aport, root)
aport_id_t aport;
st_handle_t *root;

{
  send_node_msg(aport, ROOT_NODE_REGISTER, root);
  root_id = *root;	/* init locally as well */
  got_root_id = 1;
}

/*-----------------------------------------------------------*/
/* Session_time support functions */
/*-----------------------------------------------------------*/

/* get the worker manager hostname and set the local_wm flag 
   if we are on the same host */
int set_local_wm_flag()
{
  char hostname[MAXHOSTLEN];

  mygethostname(hostname);
  got_wm_hostname = 0;
  send_simple_wm_message(wm_high_aport_id,REQ_WM_HOSTNAME,0);
  while (!got_wm_hostname)
    short_sleep(1000);
  if(strcmp(wm_hostname,hostname) == 0)
    local_wm = 1;
  else
    local_wm = 0;
}

/* Set the start time if we on the same host as the worker manager */
int set_start_time()
{
  if(local_wm)
    {
      /* Only useful if the worker manager is on the same host */
      wm_start_time = 0;
      send_simple_wm_message(wm_high_aport_id,REQ_START_TIME,0);
      while(wm_start_time == 0);
    }    
}

double calc_elapsed_time()
{
  double elapsed;

#ifdef HAVE_GETHRTIME

  elapsed = (gethrtime() - wm_start_time) / 1000.0;
  elapsed = elapsed / 1000000.0;

#else
#ifdef BSD_TIMES

  struct timeb	realtime;
  (void) ftime(&realtime);
  elapsed = (realtime.time - wm_start_time) + (double)realtime.millitm/1000.0;

#else

  struct tms		dummy;
  clock_t		realtime;
  
  if ((realtime = times(&dummy)) == (clock_t) -1)
    {
      return(0.0);
    }
  elapsed = (double) (realtime - wm_start_time) / clock_hz;

#endif
#endif
  return(elapsed);
}

/*---------------------------------------------------------------*/
/* Worker Manager builtins */ 
/*---------------------------------------------------------------*/

/* If we are on the same host as the worker manager no message is
   sent */
double elapsed_session_time()
{
  if(local_wm)
    /* can get it locally */
      cur_time = calc_elapsed_time();
  else
    {
      /* have to sent a message */
      cur_time = 0.0;
      send_simple_wm_message(wm_high_aport_id,REQ_TIME,0);
      while(cur_time <= 0.0);
    }
  return(cur_time);
}

int wm_command(command,hostname,workers)
char * command;
char * hostname;
long workers;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  config_msg_t * config_msg;
  amsg_size_t size;
  int reduce_worker = 0;
  
  size = sizeof(config_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__, 2);
  
  config_msg = (config_msg_t *) msg_data;
  
  if(strcmp(command,"add") == 0)
    config_msg->header.msg_type = ADD_WORKERS;
  else if (strcmp(command,"sleep") == 0) {
    config_msg->header.msg_type = SLEEP_WORKERS;
    reduce_worker = 1;
  }
  else if (strcmp(command,"wake") == 0)
    config_msg->header.msg_type = WAKEUP_WORKERS;
  else
    return(0);
  config_msg->header.wid = ParallelWorker;
  
  config_msg->workers = workers;
  (void) strcpy(config_msg->hostname,hostname);
  
  config_ret = -1;
  check_amsg(amsg_send(wm_low_aport_id,msg,mdt_wm_config,1,0),
	     __LINE__, 3);
  while (config_ret < 0) poll_short_sleep(1000);

  if (reduce_worker) (void) sch_reduce_worker(aports[SCH_APORT_NUMBER]);

  return(config_ret == workers);
}


int
wm_host_status(hostname)
char *hostname;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  host_name_msg_t * host_name_msg;
  amsg_size_t size;

  size = sizeof(host_name_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__, 2);
  
  host_name_msg = (host_name_msg_t *) msg_data;
  host_name_msg->header.msg_type = HOST_STATUS_REQ;
  host_name_msg->header.wid = ParallelWorker;

  (void) strcpy(host_name_msg->hostname,hostname);

  status_ret = -1;
  check_amsg(amsg_send(wm_low_aport_id,msg,mdt_wm_nsrvname,1,0),
	     __LINE__, 3);
  while (status_ret < 0) poll_short_sleep(1000);

  return(status_ret);
}

int wm_status()

{

  status_ret = -1;
  send_simple_wm_message(wm_low_aport_id,STATUS_REQ,0);
  while (status_ret < 0) poll_short_sleep(1000);
  
  return(status_ret);
}

int
p_wm_interface(vcommand, tcommand)
value vcommand;
type tcommand;
{
  char *command;

  Get_Name(vcommand,tcommand,command);
  if(strcmp(command,"on") == 0)
    send_simple_wm_message(wm_low_aport_id,START_INTERFACE,0);
  else if (strcmp(command,"off") == 0)
    send_simple_wm_message(wm_low_aport_id,REMOVE_INTERFACE,0);
  Succeed_;
}


int 
p_wm_set(vcommand, tcommand,vhostname,thostname,vworkers,tworkers)
value vcommand;
type tcommand;
value vhostname;
type thostname;
value vworkers;
type tworkers;
{
  char * hostname;
  char *command;

  Get_Name(vcommand,tcommand,command)
  Get_Name(vhostname,thostname,hostname)
  Check_Integer(tworkers);
  Succeed_If(wm_command(command,hostname,vworkers.nint));
}

int 
p_wm_get(vstatus,tstatus)
value vstatus;
type tstatus;

{
  pword  *cur_mc, *prev_mc, *cur_tail, *cur_head;
  int i;

  if (wm_status())
  {
    prev_mc = NULL;
    for(i = 0; i < status.num_machines; i++)
      {
	cur_mc = TG;
	Push_List_Frame()
	cur_head = TG;
	Push_List_Frame()
	Make_List(cur_mc,cur_head);
	Make_Integer(cur_head,status.machines[i].num_workers)
	cur_tail = cur_head + 1;
	cur_head = TG;
	Push_List_Frame()
	Make_List(cur_tail,cur_head);
	Make_Integer(cur_head,status.machines[i].num_awake)
        cur_tail = cur_head + 1;
        cur_head = TG;
	Push_List_Frame()
	Make_List(cur_tail,cur_head);
	Make_String(cur_head,status.machines[i].hostname);
	cur_tail = cur_head + 1;
	Make_Nil(cur_tail)
	cur_tail = cur_mc + 1;
	if (prev_mc == NULL)
	  Make_Nil(cur_tail)
	else
	  {
	    Make_List(cur_tail, prev_mc);
	  }
	prev_mc = cur_mc;
      }
    Return_Unify_List(vstatus,tstatus,cur_mc)
  }
  else
      Return_Unify_Nil(vstatus,tstatus)
}

p_wm_get_ids(vhostname,thostname,vstatus, tstatus)
value vhostname, vstatus;
type thostname, tstatus;
{
  pword *awake_list, *sleep_list, *tail, *stat_list, * prev;
  char * hostname;
  int i;

  Get_Name(vhostname,thostname,hostname)
  if (wm_host_status(hostname))
    {
      stat_list = TG;
      Push_List_Frame()
      tail = stat_list + 1;
      Make_Nil(tail)
      prev = NULL;
      if (host_status.num_asleep > 0)
	{
	  for(i = 0; i < host_status.num_asleep ; i++)
	    {
	      sleep_list = TG;
	      Push_List_Frame()
  	      Make_Integer(sleep_list,host_status.asleep_ids[i])
	      tail = sleep_list + 1;
	      if (prev == NULL)
		Make_Nil(tail)
	      else
		{
		  Make_List(tail,prev);
		}
	      prev = sleep_list;
	    }
	  Make_List(stat_list, sleep_list);
	}
      else
	{
	  Make_Nil(stat_list);
	}

      prev = stat_list;
      stat_list = TG;
      Push_List_Frame()
      tail = stat_list + 1;
      Make_List(tail, prev);

      prev = NULL;
      if (host_status.num_awake > 0)
	{
	  for(i = 0; i < host_status.num_awake; i++)
	    {
	      awake_list = TG;
	      Push_List_Frame()
	      Make_Integer(awake_list,host_status.awake_ids[i])
	      tail = awake_list + 1;
	      if (prev == NULL)
		Make_Nil(tail)
	      else
		{
		  Make_List(tail,prev);
		}
	      prev = awake_list;
	    }
	  Make_List(stat_list,awake_list);
	}
      else
	{
	  Make_Nil(stat_list);
	}

      Return_Unify_List(vstatus,tstatus,stat_list);
    }
  else
      Return_Unify_Nil(vstatus,tstatus)
}

void wm_set_worker_info(wid, infotype, bufsize, buf)
int wid;
int infotype; 
int bufsize;
void_ptr buf;

{
 amsg_t msg;
 amsg_data_t * msg_data;
 worker_info_msg_t * winfo_mess;
 amsg_size_t size;
 int dontack = 0;

 if (wid < 0)   /* dont send acknowledge */
   {
     dontack = 1;
     wid = -wid;
   }
 size = sizeof(worker_info_msg_t) + bufsize;

 check_amsg(amsg_alloc(size,
		       &msg_data,
		       &msg),__LINE__, 2);
 
 winfo_mess = (worker_info_msg_t *) msg_data;
 winfo_mess->header.msg_type = WORKER_INFO_SET;
 winfo_mess->header.wid = ParallelWorker;
 if (dontack)
   winfo_mess->req_wid = -ParallelWorker; 
 else
   winfo_mess->req_wid = ParallelWorker; 
 winfo_mess->pro_wid = wid;
 winfo_mess->infotype = infotype;
 winfo_mess->size = bufsize;
 bcopy((char *) buf, (char *) (winfo_mess + 1), bufsize);

 if (dontack)
   check_amsg_soft(amsg_send(wm_low_aport_id,msg,MDT_BYTE,size,0),__LINE__, 3);
 else   {
   worker_info_ret = -1;
   check_amsg_soft(amsg_send(wm_low_aport_id,msg,MDT_BYTE,size,0),__LINE__, 3);
   while(worker_info_ret < 0) 
     poll_short_sleep(100);
 }
}
  

void
wm_get_worker_info( wid, infotype, bufsize, buf)
int wid;
int infotype;
int bufsize;
void_ptr buf;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  amsg_size_t size;
  worker_info_msg_t * worker_info_mess;

  size = sizeof(worker_info_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__, 2);
  
  worker_info_mess = (worker_info_msg_t *) msg_data;
  worker_info_mess->header.msg_type = WORKER_INFO_GET;
  worker_info_mess->header.wid = ParallelWorker;
  worker_info_mess->infotype = infotype;
  worker_info_mess->pro_wid = wid;
  worker_info_mess->req_wid = ParallelWorker;
  worker_info_buf = buf;
  worker_info_bufsize = bufsize;
  worker_info_ret = -1;
  check_amsg_soft(amsg_send(wm_low_aport_id,msg,MDT_BYTE,size,0),__LINE__, 3);

  while (worker_info_ret < 0) 
    poll_short_sleep(1000);

}

send_worker_info(recwid,infotype,bufsize,buf)
int recwid;
int infotype;
int bufsize;
void_ptr buf;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  amsg_size_t size;
  worker_info_msg_t * worker_info_mess;

  if (recwid > 0)
    {
      size = sizeof(worker_info_msg_t) + bufsize;
      
      check_amsg(amsg_alloc(size,
			    &msg_data,
			    &msg),__LINE__, 2);

      worker_info_mess = (worker_info_msg_t *) msg_data;
      worker_info_mess->header.msg_type = WORKER_INFO_NOTIFY;
      worker_info_mess->header.wid = ParallelWorker;
      worker_info_mess->req_wid = recwid;
      worker_info_mess->pro_wid = ParallelWorker;
      worker_info_mess->infotype = infotype;
      worker_info_mess->size = bufsize;
      bcopy((char *) buf, (char *) (msg_data + sizeof(worker_info_msg_t)),
	bufsize);
      check_amsg_soft(amsg_send(wm_low_aport_id,msg,MDT_BYTE,size,0),__LINE__, 3);
    }
}
  


get_worker_info(infotype,infosize,infoval)
int infotype;
int *infosize;
void_ptr * infoval;

{
  
  switch (infotype) {

  case SCHED_INFO:  /* scheduler */
    sch_get_info(aports[SCH_APORT_NUMBER], infosize, infoval);
    break;

  default:
    printf("%d: Unimplemented infotype = %d\n", ParallelWorker, infotype);
  }
}
  
set_worker_info(req_wid, infotype, infoval)
int req_wid;  /* worker id of requesting worker */
int infotype;
void_ptr infoval;
{
  if (req_wid < 0)
    req_wid = - req_wid;
  switch (infotype) {

  case SCHED_INFO: /* scheduler */
    sch_set_info(aports[SCH_APORT_NUMBER],infoval);
    break;

  default:
    printf("%d: Unimplemented infotype = %d\n", ParallelWorker, infotype);
  }
}

/* set a specific worker to sleep */

p_set_sleeping(vwid, twid)
value vwid;
type twid;

{
  amsg_t msg;
  amsg_data_t * msg_data;
  wm_simple_msg_t * wm_simple_msg;
  amsg_size_t size;
  
  Check_Integer(twid);

  size = sizeof(wm_simple_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__, 2);
  
  wm_simple_msg = (wm_simple_msg_t *) msg_data;
  wm_simple_msg->header.msg_type = SET_ONE_SLEEPING;
  wm_simple_msg->header.wid = ParallelWorker;
  wm_simple_msg->msg_value = vwid.nint;
  check_amsg_soft(amsg_send(wm_low_aport_id,msg,mdt_wm_simple,1,0),__LINE__, 3);

  Succeed_;
}

/* Sleep for n usecs and poll for messages*/

void poll_short_sleep(n)
int n;
{
  bmsg_optval_t optval;
  
  short_sleep(n);
  bmsg_get_option(BMSG_INTRA_DOMAIN_TRIGGERING, &optval);
  if(optval == BMSG_OFF)
    bmsg_trigger(BMSG_INTRA_DOMAIN);
}

/* Ask for worker statistics */

request_wstat(wid, stat)
int wid;
struct worker_stat_ext * stat;
{
  worker_ptr w;

  w = get_worker(wid);
  wstat_rec_buf = stat;
  worker_info_ret = -1;
  send_simple_wm_message(w->wm_aport_id, WSTAT_REQ, 0);
  while (worker_info_ret < 0)
    poll_short_sleep(1000);
}

/* Ask remote worker (wid) to reset worker statistics and wait for ack*/
reset_wstat(wid)
int wid;
{
  worker_ptr w;

  w = get_worker(wid);

  worker_info_ret = -1;
  send_simple_wm_message(w->wm_aport_id, WSTAT_RESET, 0);
  while (worker_info_ret < 0)
    poll_short_sleep(1000);

}

/* reply to WSTAT_REQ message from worker wid */
send_wstat(wid)
int wid;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  wstat_msg_t * wstat_msg;
  amsg_size_t size;
  struct worker_stat_ext * local_stat;
  worker_ptr w;
  
  if (wid != 0) /* not a Worker Manager request */
    w = get_worker(wid);
  size = sizeof(wstat_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__, 2);
  
  wstat_msg = (wstat_msg_t *) msg_data;
  wstat_msg->header.msg_type = WSTAT_RET;
  wstat_msg->header.wid = ParallelWorker;

  local_stat = (struct worker_stat_ext *) &(wstat_msg->stat);  
  get_worker_stat(local_stat);
  check_amsg(amsg_send(wid ? w->wm_aport_id : wm_high_aport_id,
		       msg,mdt_wm_wstat,1,0),__LINE__, 3);
}

/* tracing messages */
#ifdef WORKER_TRACING
get_trace_header(wid, trace_header)
int wid;
trace_header_t * trace_header;
{
  worker_ptr w;
  w = get_worker(wid);

  trace_header_buf = trace_header;
  worker_info_ret = -1;
  send_simple_wm_message(w->wm_aport_id, GET_TRACE_HEADER, 0);
  
  while(worker_info_ret < 0)
    poll_short_sleep(1000);
}

set_trace_header(wid, theader)
int wid;
trace_header_t * theader;
{
  worker_info_ret = -1;
  send_trace_header(wid, SET_TRACE_HEADER, theader);
  while(worker_info_ret < 0)
    poll_short_sleep(1000);
}
  
send_trace_header(wid,msg_type, theader)
int wid;
int msg_type;
trace_header_t * theader;
{
  worker_ptr w = get_worker(wid);
  amsg_t msg;
  amsg_data_t * msg_data;
  trace_msg_t * trace_msg;
  amsg_size_t size;
  
  size = sizeof(trace_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__, 2);
  
  trace_msg = (trace_msg_t *) msg_data;
  trace_msg->header.msg_type = msg_type;
  trace_msg->header.wid = ParallelWorker;
  trace_msg->trace_header = *theader;
  check_amsg(amsg_send(w->wm_aport_id,msg,mdt_wm_trace,1,0),__LINE__, 3);
}
#endif
