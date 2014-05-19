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
**        File: wm.c
**      Author: Shyam Mudambi
**		Liang-Liang Li
** Description: Worker Manager for Parallel Eclipse
***********************************************************************/

/*LINTLIBRARY*/
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/stat.h>
#include <errno.h>
#include <math.h>
#include <fcntl.h>
#include "config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#else
extern char *strcpy();
extern void *memcpy();
#endif

#ifdef BSD_TIMES
#include <sys/timeb.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef PATH_IN_LIMITS
#  include 	<limits.h>
#  define MAX_PATH_LEN	PATH_MAX
#else
#  include <sys/param.h>
#  define MAX_PATH_LEN	MAXPATHLEN
#endif

#if !defined(HAVE_GETHOSTID)
#include <sys/utsname.h>	/* for uname() */
#endif

#ifdef HAVE_SYS_SYSTEMINFO_H
#include <sys/systeminfo.h>
#endif

#include "memman.h"
#include "pds.h"
#include "nsrv.h"

#ifndef _PDS_TYPES_H_
typedef long		*void_ptr;
#endif /* _PDS_TYPES_H */

#include "sch_types.h"
#include "trace.h"
#include "wm_msgs.h"
#include "wm_types.h"

/* #define DEBUG_MPS*/

#if defined(__STDC__)
    extern void	short_sleep(int);
    extern char *eclipsehome(void);
#else /* __STDC__ */
    extern void short_sleep();
    extern char *eclipsehome();
#endif /* __STDC__ */

/*------------------------------------------------------*/
/* Types and Constant Definitions */
/*------------------------------------------------------*/

#define STATIC_PORTS 4

#define WM_PORT_NAME		"Worker_Manager"
#define WORKER_PORT_NAME	"parallel_eclipse"

/* Static Port Numbers */
/* A note by LLL: why do we need four aports of different interrupt levels?
** First we need two separate ports for halt-system with highest priority,
** in order to implement a two-phase halting protocol;
** Then for the service of adding a new worker, there are at least two
** messages involved: work-creation request from somewhere (wm-window or
** within a worker) and creation-done acknowledgment from the just created
** worker. A sort of 'atomic adding' is desired so that messages requesting
** worker-management status get well-defined info, i.e. avoiding such
** wm-status as that with half-added workers. Then having a separate aport
** with higher priority serves this purpose. I.e. adding process can loop
** to wait for the acknowledgement to avoid handling any messages
** (wm-status requests, adding a worker, etc.), until the correspoding
** acknowledgement is received (via a separate but higher-prirority port).
** Therefore come into being four ports: halt2, halt1, high and low.
** Accordingly, we let messages of adding workers as well as those
** wm-status requests or wm-status updates go through the low port (the
** underlying message passing system decrees a mutually exclusive handling
** of the messages from a common aport) while the acknowledgement and other
** wm-status insensitive messages go through the higher one. For example,
** requests of reading various clocks administered by the wm.
** For the detailed grouping, see the high/low-port-notify functions.
** Note also that the higher ports haves to be attached with an asynchronous
** message handler.
*/

#define HALT1_APORT_NUMBER 0
#define HALT2_APORT_NUMBER 1
#define HIGH_APORT_NUMBER 2
#define LOW_APORT_NUMBER 3

#define Notify(text) { if (wm_verbose) { printf(text); }}


/*------------------------------------------------------*/
/* Global Variables */
/*------------------------------------------------------*/

/* Interface (Tk) functions */
int  tk_OneEvent(), tk_geval();

char * map_dir;

st_handle_t root_id;
int root_id_sender;

/* for accessing the command line in when asynchronously adding workers */
char          **Argv;
int             Argc;

/* Variables set by command-line arguments */
int dont_fork; /* set by -wnf flag */
int interactive = 0; /* set by -wmi flag */
int usemcfile = 0; /* set by -wf flag - if set we user .eclipse_machines
		      file for startup configuration */
static int wm_verbose_startup = 0; /* set by -wv flag */
static int num_workers = 0;        /* set by -w flag */
static int wm_verbose = 0;    /* will print each message recd if set to 1 */

/* Name server Global Ids */
nsrv_name_t domain_name;
nsrv_name_t bdomain_key;
nsrv_name_t session_key;          
nsrv_name_t wm_signature;
nsrv_name_t wm_port_name;
nsrv_name_t wm_halt1_aport_name,
	    wm_halt2_aport_name,
	    wm_high_aport_name,
	    wm_low_aport_name;

/* MPS Globals */
bdomain_id_t domain_id;
static bport_id_t wm_bport_id;
static int wm_pid;
bdomain_t bdomain;  /* the domain data structure */
static aport_id_t local_aport_ids[STATIC_PORTS];
static void (* notify [STATIC_PORTS]) ();


hdr_mc_list_t mc_list;  /* Holds all the worker-specific information */
char *init_host = NULL; /* may be set by -h flag */
char *wm_host = NULL;   /* machine on which worker_manager is running */
char * nsrv_host = NULL; /* machine on which name server is running */
unsigned  nsrv_port_number = 0; /* port number of nsrv */
static char *init_exec = NULL; /* may be set by -wx flag */

static char mps_map_file[MAX_PATH_LEN] = "/tmp/mps.map";

/* Volatiles - used for waiting for particular events */
static volatile int init_acks = 0;
static volatile int num_exited = 0;
static volatile int closed_bports = 0;
static volatile int got_root_id = 0;
static volatile int wstat_received;

/* Start time - used for sending session time calcuation */
#ifdef HAVE_GETHRTIME
static hrtime_t start_time;
#else
#ifdef BSD_TIMES
static time_t start_time;
#else
static clock_t start_time;
#endif
#endif
int clock_hz;

#ifdef HAVE_SIGPROCMASK
sigset_t sigio_mask;
#endif

/*-----------------------------------------------------------------*/
/*            Machine list handling routines                          */
/*-----------------------------------------------------------------*/

void init_mc_list()
{
  mc_list.num_machines = 0;
  mc_list.next_id = 1;
  mc_list.total_workers = 0;
}

machine_t *get_mc(hostname)
     char * hostname;
{

 int i;
 int found = 0;

  for(i = 0; (i < mc_list.num_machines) && !found; i++)
    if(strcmp(hostname,mc_list.machines[i].hostname) == 0)
      return(&(mc_list.machines[i]));

 return(NULL);
}

machine_t *add_mc(hostname,exec_file,auto_start)
char * hostname;
char * exec_file;
int auto_start;
{
  int mc_id;
  machine_t * mc;

  if((mc = get_mc(hostname)) == NULL)
    {
      Disable_Int();
      mc_id = mc_list.num_machines;
      mc_list.machines[mc_id].num_workers = 0;
      mc_list.machines[mc_id].num_awake = 0;
      mc_list.machines[mc_id].auto_start = auto_start;
      strcpy(mc_list.machines[mc_id].hostname, hostname);
      strcpy(mc_list.machines[mc_id].exec_file, exec_file);
      sprintf(mc_list.machines[mc_id].heap_map_file,"%s/%d.%s.heap.map",
	      map_dir,wm_pid,hostname);
      mc_list.machines[mc_id].list = NULL;
      mc_list.num_machines++;
      Enable_Int();
      return(&(mc_list.machines[mc_id]));
    }
  else
    {
      Disable_Int();
      mc->auto_start = auto_start;
      strcpy(mc->exec_file,exec_file);
      Enable_Int();
      return(mc);
    }
}
  
int init_worker(hostname,redraw)
char *hostname;
int redraw;

{
  worker_ptr w;
  machine_t *mc;

  mc = get_mc(hostname);

  if (mc == NULL)
    {
      fprintf(stderr,
	      "Error: host %s not in Worker Manager host list\n",hostname);
      return(0);
    }

  w = (worker_ptr) malloc(sizeof(worker_t));
  w->index = mc_list.next_id;
  sprintf(w->bport_name,"worker%d",w->index);
  w->status = NOT_READY;
  w->start_wstat.job_count = -1;
  w->pid = 0;
  Disable_Int();
  if(mc->list == NULL)
    {
      w->next = NULL;
      w->first = 1;
    }
  else
    {
      w->next = mc->list;
      w->first = 0;
    }

  mc->list = w;
  mc->num_workers++;
  mc_list.total_workers++;
  mc->num_awake++;
  mc_list.next_id++;
  Enable_Int();
  if (interactive && redraw) {
    tk_geval("remake_status");
  }
  return(w->index);
}

worker_ptr get_worker(index)
     int index;
{
  worker_ptr w;
  int i;

  w = NULL;
  for(i = 0; (i < mc_list.num_machines) && (w == NULL); i++)
      for(w = mc_list.machines[i].list; (w != NULL) && (w->index != index); 
	  w = w->next);

  if (w == NULL)
    fprintf(stderr, "WM: get_worker - no worker with index %d\n",index);
  return(w);
}

machine_t * get_mc_id(index)
int index;
{
  worker_ptr w;
  int i;

  w = NULL;
  for(i = 0; (i < mc_list.num_machines) && (w == NULL); i++)
      for(w = mc_list.machines[i].list; w != NULL ; w = w->next)
	if (w->index == index) 
	  return(&(mc_list.machines[i]));

  if (w == NULL)
    {
      fprintf(stderr, "WM: get_mc_id - no worker with index %d\n",index);
      return(NULL);
    }
}
  

/*-----------------------------------------------------------------*/
/*               Error handling routines                           */
/*-----------------------------------------------------------------*/

void check_nsrv(nret,line,err)
nsrv_ret_t nret;
int line;
int err;

{
  if (nret != NSRV_OK)
    {
      fprintf(stderr,"**Worker Manager: Name Server (nsrv) Fatal Error**\n");
      switch(err)
	{
	case 1: 
	  fprintf(stderr,"Looks like your nameserver has crashed.\n");
	  fprintf(stderr,"Remove all files in $ECLIPSETMP ");
	  fprintf(stderr,"and restart peclipse.\n");
	  break;
	case 2:
	  fprintf(stderr,"Could not register information with nsrv.\n");
	  break;
	case 3:
	  fprintf(stderr,"nsrv_new_bport_id call failed.\n");
	  break;
	case 4:
	  fprintf(stderr,"nsrv_bdomain_look_up failed.\n");
	  break;
	case 5:
	  fprintf(stderr,"nsrv_new_bdomain_id call failed.\n");
	  break;
	default:
	  break;
	}
      fprintf(stderr,"pid = %d nret = %d at line %d in file %s\n",
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
      fprintf(stderr,"**Worker Manager: Name Server (nsrv) Error** \n");
      switch(err)
	{
	case 1:
	  fprintf(stderr,"Could not deregister nameserver information"); 
	  break;
	case 2:
	  fprintf(stderr,"Could not free nameserver ids");
	  break;
	default:
	  break;
	}
      fprintf(stderr,"pid = %d nret = %d at line %d in file %s\n",
	     getpid(),nret,line,__FILE__);
      fprintf(stderr,"Trying to continue execution..\n");
    }
}

void check_bmsg(bret,line,err)
bmsg_ret_t bret;
int line;
int err;
{
  if (bret != BMSG_OK)
    {
      fprintf(stderr,"**Worker Manager: MPS B-layer Fatal Error: Aborting!\n");
      switch(err)
	{
	case 1:
	  fprintf(stderr,"Could not initialize Message Passing System.\n");
	  break;
	case 2:
	  fprintf(stderr,"bport_port call failed.\n");
	  break;
	default:
	  break;
	}
      fprintf(stderr,"pid = %d bret = %d at line %d in file %s\n",
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
      fprintf(stderr,"**Worker Manager: MPS A-Layer Fatal Error: Aborting!");
      switch(err)
	{
	case 1: 
	  fprintf(stderr,"Could not initialize Message Passing System.\n");
	  break;
	case 2:
	  fprintf(stderr,"Could not allocate message buffer.");
	  fprintf(stderr,"Probably no memory left.\n");
	  break;
	case 3:
	  fprintf(stderr,"Error in amsg_send.\n");
	case 4: 
	  fprintf(stderr,"Error in amsg_type_define\n");
	default:
	  break;
	}
      fprintf(stderr,"pid = %d aret = %d at line %d in file %s\n",
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
      fprintf(stderr,"**Worker Manager: MPS A-Layer Error: Warning");
      switch(err)
	{
	case 1: 
	  fprintf(stderr,"Could not free message buffer.\n");
	  break;
	case 2:
	  fprintf(stderr,"Error in recieving messages.\n");
	  break;
	case 3:
	  fprintf(stderr,"Error in amsg_send.\n");
	default:
	  break;
	}
      fprintf(stderr,"pid = %d aret = %d at line %d in file %s\n",
	     getpid(),aret,line,__FILE__);
      fprintf(stderr,"Trying to continue execution..\n");
    }
}

void wm_abort_error(msg)
char * msg;
{
  fprintf(stderr,"** Worker Manager : Fatal error**\n");
  fprintf(stderr,"%s\n",msg);
}

/*----------------------------------------------------------------*/
/* Signal Handling */
/*----------------------------------------------------------------*/

static void
handle_sigio()
{
    /* unblock SIGIO and SIGPOLL signals */
#ifdef HAVE_SIGPROCMASK
    (void) sigprocmask(SIG_UNBLOCK, &sigio_mask, (sigset_t *) 0);
#else
    {
	int mask;
#ifdef SIGPOLL
	mask = sigblock(sigmask(SIGIO) | sigmask(SIGPOLL));
	sigsetmask(mask & ~(sigmask(SIGIO) | sigmask(SIGPOLL)));
#else
	mask = sigblock(sigmask(SIGIO));
	sigsetmask(mask & ~sigmask(SIGIO));
#endif
    }
#endif
    (void) bmsg_trigger((BMSG_INTER_DOMAIN |
		    BMSG_INTRA_DOMAIN  ));
}


RETSIGTYPE
sigio_handler()
{
    if (InterruptsDisabled) {
        Set_Interrupts_Pending();
    } else {
	handle_sigio();
    }
}

static void
delayed_signal_handler()
{
    Clr_Interrupts_Pending();
    handle_sigio();
}


/*-----------------------------------------------------------*/
/*  Panic and warning routines */
/*-----------------------------------------------------------*/

void
mem_panic(what, where)
    char * what;
    char * where;
{
    fprintf(stderr,"Panic: %s in %s\n", what, where);
    exit(-1);
}

/*ARGSUSED*/
void amsg_warn(msg_warn, culprit)
     amsg_warn_t msg_warn;
     aport_id_t culprit;
{
/*    printf("%d: amsg_warn: ...\n", bport_self());*/
}

/*ARGSUSED*/
void
amsg_panic(msg_panic,culprit)
    amsg_panic_t msg_panic;
    aport_id_t culprit;
{
}

/*ARGSUSED*/
void
amsg_error(msg_error,culprit)
    amsg_error_t msg_error;
    aport_id_t culprit;
{
}

/*ARGSUSED*/
#if defined(__STDC__)
void bmsg_warn(bmsg_warn_t msg_warn,
	       bport_id_t culprit)
#else
void
bmsg_warn(msg_warn, culprit)
    bmsg_warn_t msg_warn;
    bport_id_t culprit;
#endif
{
/*    
    printf("Worker Manager: bmsg_warn: %d :error no: %d ...\n",
           culprit,msg_warn); 
*/
}

/*ARGSUSED*/
#if defined(__STDC__)
void bmsg_panic(bmsg_warn_t msg_panic,
	       bport_id_t culprit)
#else
void
bmsg_panic(msg_panic,culprit)
    bmsg_panic_t msg_panic;
    bport_id_t culprit;
#endif
{
    printf("Worker Manager: bmsg_panic: %d error no: %d\n",
           culprit,msg_panic);
}

/*ARGSUSED*/
#if defined(__STDC__)
void bmsg_error(bmsg_warn_t msg_error,
	       bport_id_t culprit)
#else
void
bmsg_error(msg_error,culprit)
    bmsg_error_t msg_error;
    bport_id_t culprit;
#endif
{
    switch (msg_error) {
        case BMSG_WEP_PDIED :
            if (culprit == NSRV_BPORT_ID)
                printf("Worker Manager: bmsg_error: name server died !\n");
            else
                printf("Worker Manager: bmsg_error: bport %d died !\n", 
		       culprit);
            return;
        default :
            break;
    }
    printf("Worker Manager: unknown bmsg_error: culprit %d, error no %d\n",
           culprit,msg_error);
}

/*ARGSUSED*/
void
bmem_ack(mem_id,mem_primitive,ret)
    bmem_id_t mem_id;
    bmem_primitive_t mem_primitive;
    bmsg_ret_t ret;
{
    printf("%d: bmem_ack: ...\n", bport_self());
}

/*ARGSUSED*/
#if defined(__STDC__)
void
bmem_notify(bport_id_t port_id,
	    bmem_primitive_t mem_primitive,
	    bmem_address_t mem_address,
	    bmem_size_t mem_data_size)
#else
void
bmem_notify(port_id,mem_primitive,mem_address,mem_data_size)
    bport_id_t port_id;
    bmem_primitive_t mem_primitive;
    bmem_address_t mem_address;
    bmem_size_t mem_data_size;
#endif
{
    printf("%d: bmem_notify: ...\n", bport_self());
}

/*----------------------------------------------------------------*/
/* MPS B-layer port notify and acknowledge routines               */
/*----------------------------------------------------------------*/

/*ARGSUSED*/
#if defined(__STDC__)
void
bport_notify(bport_id_t port_id, 
	     bport_primitive_t port_primitive)
#else
void
bport_notify(port_id, port_primitive)
    bport_id_t port_id;
    bport_primitive_t port_primitive;
#endif
{
    switch (port_primitive) {
        case BPORT_OPEN :
            break;
        case BPORT_CLOSE :
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

/*ARGSUSED*/
#if defined(__STDC__)
void
bport_ack(bport_id_t port_id, 
	  bport_primitive_t port_primitive, 
	  bmsg_ret_t ret)
#else
void
bport_ack(port_id, port_primitive, ret)
    bport_id_t port_id;
    bport_primitive_t port_primitive;
    bmsg_ret_t ret;
#endif
{

    if (ret != BMSG_OK) {
        switch (port_primitive) {
            case BPORT_OPEN :
                break;
            case BPORT_CLOSE :
                break;
            case BPORT_BLOCK :
                break;
            case BPORT_UNBLOCK :
                break;
            default:
                break;
        }
	return;
    }
}

void 
bproc_trigger(port)
    bport_t * port;
{
    if (port->bpid == wm_pid)
    {
	(void) bmsg_trigger(BMSG_INTRA_DOMAIN);
    }
    else if (kill(port->bpid, SIGIO) != 0)
    {
	fprintf(stderr, "bport %d died\n", (int) port->bport_id); 
    }
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
amsg_type_t mdt_wm_workerinfo;
amsg_type_t mdt_wm_time;
amsg_type_t mdt_wm_starttime;
amsg_type_t mdt_wm_portname;
amsg_type_t mdt_wm_wstat;
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
  
  /* host_name_msg_t structures */
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
}
    

/*-----------------------------------------------------------------*/
/*       Procedures for sending simple messages                    */
/*-----------------------------------------------------------------*/


amsg_ret_t send_simple_wm_message(port_id,msg_type,msg_value)
     aport_id_t port_id;
     int msg_type;  
     int msg_value;

{
  amsg_t msg;
  amsg_data_t * msg_data;
  wm_simple_msg_t * wm_simple_msg;
  amsg_ret_t aret;
  
  amsg_size_t size;
  
  size = sizeof(wm_simple_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__,2);
  
  wm_simple_msg = (wm_simple_msg_t *) msg_data;
  wm_simple_msg->header.msg_type = msg_type;
  wm_simple_msg->header.wid = 0;
  wm_simple_msg->msg_value = msg_value;
  aret = amsg_send(port_id,msg,mdt_wm_simple, 1, 0);
  return(aret);
}

int broadcast_halt(mtype)
int mtype;
{
  worker_ptr cur;
  int i;

  for(i = 0; i < mc_list.num_machines; i++)
    for(cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
      if(cur->status != NOT_READY) {
         if (mtype == HALT_SYSTEM1) 
	   check_amsg_soft(send_simple_wm_message(cur->halt1_aport_id,mtype,0),
		 __LINE__, 3);
	 else 
	   check_amsg_soft(send_simple_wm_message(cur->halt2_aport_id,mtype,0),
		 __LINE__, 3);
      }
}

int broadcast(mtype,mmsg)
int mtype;
int mmsg;
{
  worker_ptr cur;
  int i;

  for(i = 0; i < mc_list.num_machines; i++)
    for(cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
      if(cur->status != NOT_READY)
	check_amsg_soft(send_simple_wm_message(cur->wm_aport_id,mtype,mmsg),
		 __LINE__, 3);
}

int broadcast_awake(mtype,mmsg)
int mtype;
int mmsg;
{
  worker_ptr cur;
  int i;

  for(i = 0; i < mc_list.num_machines; i++)
    for(cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
      if(cur->status == AWAKE) 
	check_amsg_soft(send_simple_wm_message(cur->wm_aport_id,mtype,mmsg),
		 __LINE__, 3);
}

/* emergency cleanup - use rsh to kill remote workers */
/* Try to remove mapped files */
int
kill_system()
{
  worker_ptr cur;
  int i, local;

  for(i = 0; i < mc_list.num_machines; i++)
    {
      local = (strcmp(mc_list.machines[i].hostname,wm_host) == 0);
      for(cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
	if(cur->pid)
	  if(local)
	    {
	      if(kill(cur->pid,SIGKILL) != 0)
		perror("Kill Failed");
	    }
	  else
	    {
	      char command[1024];
	      sprintf(command,"rsh %s kill -9 %d", 
		      mc_list.machines[i].hostname, cur->pid);
	      system(command);
	    }
      if(ec_unlink(mc_list.machines[i].heap_map_file) != 0)
	perror("Could not remove heap_map_file\n");
    }
  if(ec_unlink(mps_map_file) != 0)
	perror("Could not remove mps_map_file\n");
}

/*-----------------------------------------------------------------*/
/*------------------ Notify Procedures ----------------------------*/
/*-----------------------------------------------------------------*/

/* There are four aports used to communicate with the worker manager-
a. Halt1/2 aports - to halt the system. Either a normal exit or
		    a panic will cause a halt request. Two separate ports
		    to implement a two-phase exit protocol.

b. High wm-aport  - used for start up, and some messages which does not rely
		    the well-being of the wm status.

c. Low wm-aport   - used for all other messages, in particular those
		    requesting the wm status. Therefore this low port messages
		    should not be handled when the manager is in the middle of
		    a worker startup or exiting protocols. I.e. this port
		    should have lower priority.
*/

void
halt2_notify(port_id)
  aport_id_t port_id;
{
    amsg_ret_t ret;
    amsg_t msg;
    amsg_data_t * msg_data;
    amsg_type_t msg_type;
    amsg_count_t msg_count;

    while (( ret = amsg_receive(port_id,
                                &msg,
                                &msg_data,
                                &msg_type,
                                &msg_count, 0)) == AMSG_OK) {
       Notify("WM: Got EXITING message\n");
       num_exited++;
    }
}

/*ARGSUSED*/
void
halt1_notify(port_id)
  aport_id_t port_id;
{
  void exit_system();

  Notify("WM: Got HALT_SYSTEM_REQ message\n");
  exit_system(0,60);
}

/*-----------------------------------------------------------------*/
/* Implements a two-phase exit protocol and each phase uses
   the supplied timeout */
/*-----------------------------------------------------------------*/
void exit_system(exit_code, timeout)
int exit_code;
int timeout;  /* in seconds */

{
  int timer1 = timeout;
  int timer2 = timeout;
  int killed = 0;

  broadcast_halt(HALT_SYSTEM1, 0);
  while (num_exited != mc_list.total_workers && (timer1-- > 0 || dont_fork)) 
     short_sleep(1000000);
  if (timer1 > 0 || dont_fork) {
     broadcast_halt(HALT_SYSTEM2, 0);
     while (closed_bports != mc_list.total_workers && (timer2-- > 0||dont_fork))
        short_sleep(1000000);
  }

  if ((timer1 <= 0 || timer2 <= 0) && !dont_fork) {
     printf("Time out reached - Killing workers and exiting !!\n");
     kill_system();
     killed = 1;
  }

  if (!killed) { /* dont try to deregister after an hard exit.
	            The MPS memory may be in an inconsistent state */
     /* release all name server ids */
     check_nsrv_soft(nsrv_aport_deregister(session_key,
			     wm_high_aport_name, wm_signature), __LINE__, 1);
     check_nsrv_soft(nsrv_aport_deregister(session_key,
			     wm_halt1_aport_name, wm_signature), __LINE__, 1);
     check_nsrv_soft(nsrv_aport_deregister(session_key,
			     wm_halt2_aport_name, wm_signature), __LINE__, 1);
     check_nsrv_soft(nsrv_aport_deregister(session_key,
			     wm_low_aport_name, wm_signature), __LINE__, 1);
     check_nsrv_soft(nsrv_bport_deregister(session_key,
			     wm_port_name, wm_signature), __LINE__, 1);
     check_nsrv_soft(nsrv_free_bport_id(wm_signature,
			     wm_bport_id), __LINE__, 2);
     check_nsrv_soft(nsrv_bdomain_deregister(bdomain_key,
			     domain_name, wm_signature), __LINE__, 1);
     check_nsrv_soft(nsrv_free_bdomain_id(wm_signature, 
			     bdomain.bdomain_id), __LINE__, 2);
  }
  nsrv_exit();
  amsg_exit();
  bmsg_exit();
  exit(exit_code);
}

/*------------------------------------------------------------*/
/* Low WM Port handling routines */
/*------------------------------------------------------------*/
void send_status(w)
worker_ptr w;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  amsg_size_t size;
  status_msg_t *stat_msg;
  int i;

  size = sizeof(status_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__,2);
  
  stat_msg = (status_msg_t *) msg_data;
  stat_msg->header.msg_type = STATUS_NOTIFY;
  stat_msg->header.wid = 0;
  stat_msg->num_machines = mc_list.num_machines;
  stat_msg->total_workers = mc_list.total_workers;

  for(i = 0; i < mc_list.num_machines; i++)
    {
      stat_msg->machines[i].num_workers = mc_list.machines[i].num_workers;
      stat_msg->machines[i].num_awake = mc_list.machines[i].num_awake;
      strcpy(stat_msg->machines[i].hostname, mc_list.machines[i].hostname);
    }
  check_amsg_soft(amsg_send(w->wm_aport_id,msg,mdt_wm_status,1,0),__LINE__,3);
}

/* send the host name of the worker manager machine to worker w */
void send_host_name(w)
worker_ptr w;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  amsg_size_t size;
  host_name_msg_t *host_name_msg;

  size = sizeof(host_name_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__,2);
  
  host_name_msg = (host_name_msg_t *) msg_data;
  host_name_msg->header.msg_type = WM_HOSTNAME;
  host_name_msg->header.wid = 0;
  strcpy(host_name_msg->hostname,wm_host);
  check_amsg_soft(amsg_send(w->wm_aport_id,msg,mdt_wm_nsrvname,1,0),
		  __LINE__,3);
}  

void send_host_status(w,hostname)
worker_ptr w;
char *hostname;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  amsg_size_t size;
  host_status_msg_t *stat_msg;
  int asleep_count, awake_count;
  machine_t *mc;
  worker_ptr lw;

  size = sizeof(host_status_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__,2);
  
  stat_msg = (host_status_msg_t *) msg_data;
  stat_msg->header.msg_type = HOST_STATUS_NOTIFY;
  stat_msg->header.wid = 0;
  
  mc = get_mc(hostname);
  if(mc == NULL)
    {
      stat_msg->num_awake = 0;
      stat_msg->num_asleep = 0;
    }
  else
    {
      lw = mc->list;
      
      awake_count = 0;
      asleep_count = 0;
      while(lw != NULL)
	{
	  if (lw->status == AWAKE)
	    {
	      stat_msg->awake_ids[awake_count] = lw->index;
	      awake_count++;
	    }
	  else if (lw->status == SLEEPING)
	    {
	      stat_msg->asleep_ids[asleep_count] = lw->index;
	      asleep_count++;
	    }
	  lw = lw->next;
	}
      stat_msg->num_awake = awake_count;
      stat_msg->num_asleep = asleep_count;
    }
  check_amsg_soft(amsg_send(w->wm_aport_id,msg,mdt_wm_hoststatus,1,0),
		  __LINE__,3);
}

int send_to_sleep(w,mc)
worker_ptr w;
machine_t *mc;

{
  amsg_ret_t aret;

  aret = send_simple_wm_message(w->wm_aport_id, GOTO_SLEEP,0);
  if (aret != AMSG_OK)
    return(0);
  w->status = SLEEPING;
  mc->num_awake--;
  if (interactive) {
    tk_geval("remake_status");
  }
  return(1);
}

int wakeup(w,mc)
worker_ptr w;
machine_t *mc;

{
  amsg_ret_t aret;

  aret = send_simple_wm_message(w->wm_aport_id, WAKEUP,0);
  if (aret != AMSG_OK)
    return(0);
  w->status = AWAKE;
  mc->num_awake++;
  if (interactive) {
    tk_geval("remake_status");
  }
  return(1);
}

/*ARGSUSED*/  
void
low_wm_notify(port_id)
aport_id_t port_id;
{
  amsg_ret_t ret;
  amsg_t msg;
  amsg_data_t * msg_data;
  amsg_count_t msg_count;
  amsg_type_t msg_type;
  wm_simple_msg_t * wm_simple_msg;
  int msg_value;
  config_msg_t *config_mess;
  int req_type;
  worker_info_msg_t * worker_info_msg;
  host_name_msg_t * host_name_msg;
  worker_ptr w;
  int i, worker_id;
  worker_ptr cur;
  int workers;
  char *hostname;
  machine_t *mc;
  int freemsg;
  
  
  freemsg = 1;

  while (( ret = amsg_receive(port_id,
			      &msg,
			      &msg_data,
			      &msg_type,
			      &msg_count, 0)) == AMSG_OK) {
    
    if(msg_type == mdt_wm_simple) 
      {
	wm_simple_msg = (wm_simple_msg_t *) msg_data;
	worker_id = wm_simple_msg->header.wid;
	msg_value = wm_simple_msg->msg_value;
	switch(wm_simple_msg->header.msg_type) {
	  
	case START_INTERFACE:
	  if(!interactive) 
	    {
	      start_tkwindow();
	      interactive = 1;
	    }
	  break;
	case REMOVE_INTERFACE:
	  if(interactive) 
	    {
	      interactive = 0;
	      delete_tkwindow();
	    }
	  break;
	case SET_ONE_SLEEPING:
	  cur = get_worker(msg_value);
	  mc = get_mc_id(msg_value);
	  if (cur && mc) 
	    send_to_sleep(cur,mc);
	  break;

	case STATUS_REQ:
	  Notify("WM: Got STATUS_REQ message\n");
	  w = get_worker(worker_id);
	  send_status(w);
	  break;

	case START_TRACING:
	  Notify("WM: Got START_TRACING message\n");
	  broadcast_awake(START_TRACING, 0);
	  break;
	  
	case STOP_TRACING:
	  Notify("WM: Got STOP_TRACING message\n");
	  broadcast_awake(STOP_TRACING, 0);
	  break;
	    
	default:
	  printf("Unknown simple Worker Manager message\n");
	  break;
	}
      }
    else if (msg_type == mdt_wm_config)
      {
	config_mess = (config_msg_t *) msg_data;
	req_type = config_mess->header.msg_type;
	worker_id = config_mess->header.wid;
	workers = config_mess->workers;
	hostname = config_mess->hostname;
	mc = get_mc(hostname);
	i = 0;
	if (mc != NULL) 
	  {
	    if (req_type == ADD_WORKERS)
	      {
		Notify("WM: Got ADD_WORKERS message\n");
		for(i = 0; i < workers; i++)
		  if (!add_worker(Argc,Argv,mc))
		    break;
	      }
	    else if (req_type == SLEEP_WORKERS)
	      {
		Notify("WM: Got SLEEP_WORKERS message\n");
		if (workers < mc->num_awake)
		  {
		    cur = mc->list; 
		    for (i = 0; i < workers; i++)
		      {
			while((cur->status == SLEEPING) || (cur->index == worker_id))
			  cur = cur->next;
			if(!send_to_sleep(cur,mc))    break;
		      }
		  }
	      }
	    else /* req_type == WAKEUP_WORKERS */
	      {
		Notify("WM: Got WAKEUP_WORKERS message\n");
		if (workers  <= mc->num_workers - mc->num_awake)
		  {
		    cur = mc->list; 
		    for (i = 0; i < workers; i++)
		      {
			while(cur->status != SLEEPING)
			  cur = cur->next;
			if(!wakeup(cur,mc)) break;
		      }
		  }
	      }
	  }
	w = get_worker(worker_id);
	check_amsg_soft(send_simple_wm_message(w->wm_aport_id,
					       CONFIG_NOTIFY,i),
			__LINE__,3);
      }
    else if (msg_type == mdt_wm_nsrvname)
      {

	host_name_msg = (host_name_msg_t *) msg_data;
	Notify("WM: Got HOST_STATUS_REQ message\n");
	w = get_worker(host_name_msg->header.wid);
	hostname = host_name_msg->hostname;
	send_host_status(w,hostname);
      }
    else if (msg_type == MDT_BYTE)
      {
	worker_info_msg = (worker_info_msg_t *) msg_data;
	if(worker_info_msg->header.msg_type == WORKER_INFO_SET)
	  {
	    Notify("WM: Got WORKER_INFO_SET message\n");
	    w = get_worker(worker_info_msg->pro_wid);
	    check_amsg_soft(amsg_send(w->wm_aport_id,msg,
				      MDT_BYTE,msg_count,0),__LINE__,3);
	  }
	else if(worker_info_msg->header.msg_type == WORKER_INFO_GET) 
	  {
	    Notify("WM: Got WORKER_INFO_GET message\n");
	    w = get_worker(worker_info_msg->pro_wid);
	    check_amsg_soft(amsg_send(w->wm_aport_id,msg,
				      MDT_BYTE,msg_count,0),__LINE__,3);
	  }
	else if(worker_info_msg->header.msg_type == WORKER_INFO_NOTIFY) 
	  {
	    Notify("WM: Got WORKER_INFO_NOTIFY message\n");
	    send_worker_info(worker_info_msg->req_wid, worker_info_msg->size,
			     (void_ptr) (worker_info_msg + 1));
	  }
	freemsg = 0;
      }
    if (freemsg)
      check_amsg_soft(amsg_free(msg),__LINE__,1);
    else
      freemsg = 1; 
  }
  if (ret != AMSG_NOMESSAGE) {
    check_amsg_soft(ret,__LINE__, 2);
  }
  tk_doidledummy();
}

/*------------------------------------------------------------*/
/* High WM port handling routines */
/*------------------------------------------------------------*/
int send_start_time(slave_no)
     int slave_no;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  start_time_msg_t * start_time_msg;
  amsg_size_t size;

  worker_ptr w;

  size = sizeof(start_time_msg_t);
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__,2);
  
  start_time_msg = (start_time_msg_t *) msg_data;
  start_time_msg->header.msg_type = SEND_START_TIME;
  start_time_msg->header.wid = 0;
  start_time_msg->start_time = start_time;

  w = get_worker(slave_no);
  check_amsg_soft(amsg_send(w->wm_aport_id,msg,mdt_wm_starttime,1,0),
		  __LINE__,3);
}

#ifdef HAVE_GETHRTIME

hrtime_t get_time()
{
  return(gethrtime());
}

double relative_elapsed_time(start_time)
hrtime_t start_time;
{
  hrtime_t thr_time;
  double elapsed;

  thr_time = gethrtime();
  elapsed = (thr_time - start_time)/ 1000.0;
  elapsed = elapsed / 1000000.0;
  return(elapsed);
}

#else
#ifdef BSD_TIMES

time_t get_time()
{
  return(time((time_t *) 0));
}

double relative_elapsed_time(start_time)
time_t start_time;
{
  struct timeb	realtime;
  double elapsed;

  /* times() returns nothing useful in BSD, need ftime() for elapsed time */
  (void) ftime(&realtime);
  elapsed = (realtime.time - start_time) + (double)realtime.millitm/1000.0;
  return(elapsed);
}
#else

clock_t get_time()
{
  struct tms dummy;
  return(times(&dummy));
}

double relative_elapsed_time(start_time)
clock_t start_time;
{
  clock_t		realtime;
  struct tms		dummy;
  double                elapsed;
  
  if ((realtime = times(&dummy)) == (clock_t) -1)
    return(0.0);
  elapsed = (double) (realtime - start_time) / clock_hz;
  return(elapsed);
}
#endif
#endif

void send_elapsed_time(slave_no)
     int slave_no;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  time_msg_t * time_msg;
  amsg_size_t size;
  double elapsed;

  worker_ptr w;

  elapsed = relative_elapsed_time(start_time);

  size = sizeof(time_msg_t);
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__,2);
  
  time_msg = (time_msg_t *) msg_data;
  time_msg->header.msg_type = SEND_TIME;
  time_msg->header.wid = 0;

  time_msg->cur_time = elapsed;

  w = get_worker(slave_no);
  check_amsg_soft(amsg_send(w->wm_aport_id,msg,mdt_wm_time,1,0),__LINE__,3);
}


void
high_wm_notify(port_id)
   aport_id_t port_id;

{
    amsg_ret_t ret;
    amsg_t msg;
    amsg_data_t * msg_data;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    wm_simple_msg_t * wm_simple_msg;
    node_msg_t * node_msg;
    worker_ptr w;

    while (( ret = amsg_receive(port_id,
				&msg,
				&msg_data,
				&msg_type,
				&msg_count, 0)) == AMSG_OK) {
      if (msg_type == mdt_wm_simple)
	{
	  wm_simple_msg = (wm_simple_msg_t *) msg_data;
	  switch(wm_simple_msg->header.msg_type) {
	    
	  case DONE_INIT_OPENS:
	    Notify("WM: Got DONE_INIT_OPENS message\n");
	    init_acks++;
	    if (wm_verbose_startup)
	      printf("worker %d is up\n",wm_simple_msg->header.wid);
	    break;
	    
	 
	  case REQ_TIME:
	    Notify("WM: Got REQ_TIME message\n");
	    send_elapsed_time(wm_simple_msg->header.wid);
	    break;
	    
	  case REQ_START_TIME:
	    Notify("WM: Got REQ_START_TIME message\n");
	    send_start_time(wm_simple_msg->header.wid);
	    break;

	  case REQ_WM_HOSTNAME:
	    Notify("WM: Got REQ_WM_HOSTNAME message\n");
	    w = get_worker(wm_simple_msg->header.wid);
	    send_host_name(w);
	    break;

	  default:
	    break;
	  }
	}
      else if (msg_type == mdt_wm_node)
	{   /* ROOT_NODE_REGISTER message */
	  node_msg = (node_msg_t *) msg_data;
	  Notify("WM: Got ROOT_NODE_REGISTER message\n");
	  root_id = node_msg->node;
	  root_id_sender = node_msg->header.wid;
	  got_root_id = 1;
	}
      else if (msg_type == mdt_wm_wstat)
	{
	  /* WSTAT_RET message */
	  wstat_msg_t * wstat_msg;
	  Notify("WM: Got WSTAT_RET message\n");
	  wstat_msg = (wstat_msg_t *) msg_data;
	  get_worker(wstat_msg->header.wid)->cur_wstat = 
	    wstat_msg->stat;
	  wstat_received = 1;
	}
	  
      check_amsg_soft(amsg_free(msg),__LINE__, 1); 
    }

    if (ret != AMSG_NOMESSAGE)
      check_amsg_soft(ret,__LINE__, 2);
}

/*------------------------------------------------------------*/

void
get_arguments(argv, argc, numworkers, nofork)
char *argv[];
int *argc;
int *numworkers;
int *nofork;
{
    int i, j, k;
    *numworkers = 1;
    *nofork = 0;
    for(i=1, j=1; i < *argc; i++) 
    {
        if (strcmp(argv[i],"--") == 0)
	{
	    do {
		argv[j++] = argv[i++];
	    } while (i < *argc);
	}
	else if (strcmp(argv[i],"-wnf") == 0)
	{
	    *nofork = 1;
	}
	else if (strcmp(argv[i],"-wv") == 0)
	{
	    wm_verbose_startup = 1;
	}
	else if (strcmp(argv[i],"-wmi") == 0)
	{
	    interactive = 1;
	}
	else if (strcmp(argv[i],"-wf") == 0)
	{
	  usemcfile = 1;
	}
	else if (strcmp(argv[i],"-wh") == 0)
	{
	    if (++i < *argc)
	      init_host = argv[i];
	    else
	      fprintf(stderr,"Expected hostname after %s\n",argv[i-1]);
	}
	else if (strcmp(argv[i],"-wx") == 0)
	{
	    if (++i < *argc)
	      init_exec = argv[i];
	    else
	      fprintf(stderr,"Expected worker executable after %s\n",argv[i-1]);
	}
	else if (strcmp(argv[i],"-w") == 0)
	{
	    if (++i < *argc)
		*numworkers = atoi(argv[i]);
	    else
		fprintf(stderr,"Expected number of workers after %s\n",argv[i-1]);
	}
	else if (argv[i][0] == '-' && argv[i][1] == 'w' &&
		(k = atoi(&argv[i][2])) > 0)
	{
	    *numworkers = k;
	}
	else
	{
	    argv[j++] = argv[i];
	}
    }
    *argc = j;
    Argc = j;
    Argv = argv;
}

/*----------------------------------------------------------------*/
/* read_mcfile()                                                  */
/*----------------------------------------------------------------*/
/* Read the file ".eclipse_machines" from the current directory,
   if one exists, otherwise from the lib directory.

   Each line in the file should have the following format
   <host_name> <eclipse_exe_file> <num_workers> <auto_start>

   where
   host_name is the name of the machine where workers are to be started.
   eclipse_exec_file is the full path name of the eclipse worker (on the
               specified host)
   num_workers is the number of workers to start initially
   auto_start can be 0 or 1 - if 1 the workers will be started 
             via rsh. Otherwise, only the command line will be printed and
	     user must start the workers manually on the specified host.
*/
void read_mcfile()
{
  FILE * mcfile;
  char lib_mc_file[MAX_PATH_LEN], mcfilename[MAX_PATH_LEN], 
       exec_file[MAX_PATH_LEN];
  struct stat buf;
  sstring hostname;
  int num_workers, i, done, val;
  int auto_start;

  strcpy(mcfilename,".eclipse_machines");

  if ((ec_stat(mcfilename,&buf) == -1) && errno == ENOENT)
    {
      sprintf(lib_mc_file,"%s/lib/%s",eclipsehome(),mcfilename);
      mcfile = fopen(lib_mc_file,"r");
    }
  else
    mcfile = fopen(mcfilename,"r");

  if (mcfile != NULL) {
    done = 0;
    while (!done)  {
	val = fscanf(mcfile, "%s %s %d %d", hostname, exec_file, &num_workers,
		     &auto_start);
	if (val == EOF)
	  done = 1;
	else if (val == 4) {
/*	  printf("%s %s %d %d\n",hostname,exec_file,num_workers,auto_start);*/
	  add_mc(hostname,exec_file,auto_start);
	  if (usemcfile)
	    for(i = 0; i < num_workers; i++)
	      init_worker(hostname,0);	    
	}
	else  {
	  fprintf(stderr,"Illegal format in %s startup file\n", mcfilename);
	  exit(-1);
	}
      }
    fclose(mcfile);
  }
}
    
void init_global_values()
{
  int i;

  start_time = get_time();

#ifdef HAVE_SYSCONF
	clock_hz = sysconf(_SC_CLK_TCK);
#else
#ifdef CLOCK_HZ
	clock_hz = CLOCK_HZ;
#else
	clock_hz = 60;
#endif
#endif

  private_mem_init(mem_panic);

  wm_pid = getpid();

  sprintf(bdomain_key, WORKER_PORT_NAME);
/*  bdomain.bdomain_file = (char *) malloc(MAX_PATH_LEN);*/
  strcpy(wm_port_name, WM_PORT_NAME);

  map_dir = getenv("ECLIPSETMP");
/* If not set use current directory - this will cause problems 
   if the current directory is not writable */
  if (!map_dir) 
    map_dir = getcwd(NULL, MAX_PATH_LEN);

  sprintf(mps_map_file,"%s/%d.mps.map",map_dir,wm_pid);

  wm_host = (char *) hp_alloc_size(MAXHOSTLEN);
  mygethostname(wm_host);

  sprintf(session_key,"%s.%d",wm_host,wm_pid);
  sprintf(wm_signature,"%s.%d",wm_host,wm_pid);

  nsrv_host = getenv("NSRV_HOST");
  if(!nsrv_host)
    nsrv_host = wm_host;

  init_mc_list();

  read_mcfile();
  if (!usemcfile)
    {
      if (!init_host)
	init_host = (char *) wm_host;
      
      if (!init_exec) {
	init_exec = hp_alloc_size(sizeof(sstring));
	sprintf(init_exec,"%s/bin/%s/worker",eclipsehome(),HOSTARCH);
      }
  
      add_mc(init_host,init_exec,1);
      for(i = 1; i <= num_workers; i++) 
	init_worker(init_host,0);
    }
}

mygethostname(host)
     char *host;
{
#if defined(HAVE_GETHOSTNAME)
  gethostname(host,MAXHOSTLEN);
#else
#  if defined(HAVE_SYSINFO) && defined(HAVE_SYS_SYSTEMINFO_H)
  sysinfo(SI_HOSTNAME, host, MAXHOSTLEN);
#  else
  struct utsname ut;
  uname(&ut);
  strcpy(host,ut.nodename);
#  endif
#endif
}

/*-----------------------------------------------------------*/
/* MPS system initialisation routines */
/*-----------------------------------------------------------*/

/* Register the wm's high_aport, halt1/2_aport and low_aport*/
void register_aport_names()
{
  aport_t aport;

  sprintf(wm_high_aport_name,"%s_high_aport",wm_port_name);
  aport.aport_id = local_aport_ids[HIGH_APORT_NUMBER];
  aport.bport_id = wm_bport_id;
  aport.bdomain_id = bdomain.bdomain_id;
  check_nsrv(nsrv_aport_register(session_key,wm_high_aport_name,
				 wm_signature,&aport),__LINE__, 2);
	     
  sprintf(wm_halt2_aport_name,"%s_halt2_aport",wm_port_name);
  aport.aport_id = local_aport_ids[HALT2_APORT_NUMBER];
  check_nsrv(nsrv_aport_register(session_key,wm_halt2_aport_name,
				 wm_signature,&aport),__LINE__, 2);

  sprintf(wm_halt1_aport_name,"%s_halt1_aport",wm_port_name);
  aport.aport_id = local_aport_ids[HALT1_APORT_NUMBER];
  check_nsrv(nsrv_aport_register(session_key,wm_halt1_aport_name,
				 wm_signature,&aport),__LINE__, 2);

  sprintf(wm_low_aport_name,"%s_low_aport",wm_port_name);
  aport.aport_id = local_aport_ids[LOW_APORT_NUMBER];
  check_nsrv(nsrv_aport_register(session_key,wm_low_aport_name,
				 wm_signature,&aport),__LINE__, 2);
  wm_types_init();  /* can only be done after aport registration 
		       since MDT_NSRVNAME is only defined at this point */
}

void fill_in_domain(ldomain_id, lbdomain)
bdomain_id_t ldomain_id;
bdomain_t *lbdomain;

{
  lbdomain->bdomain_id = ldomain_id;
  lbdomain->bdomain_size = 0x00800000;  /* 8 MB */
  if (!shared_mem_base())
      lbdomain->bdomain_start = (bmem_address_t) (shared_mem_base());
  else
      lbdomain->bdomain_start = (bmem_address_t) (shared_mem_base() + 0x00800000);
  strcpy(lbdomain->bdomain_file,mps_map_file);
}

/* initialise message-passing system */
void mps_init()
{
  bport_t my_bport;
  nsrv_ret_t nret;
  char hostname[MAXHOSTLEN];

  /* first set up the b-layer */
  check_nsrv(nsrv_new_bport_id(wm_signature,&wm_bport_id),__LINE__,3);

  gethostname(hostname,MAXHOSTLEN);  

  sprintf(domain_name,"%s.%s.%s", WORKER_PORT_NAME, hostname,session_key);

  nret = nsrv_bdomain_look_up(bdomain_key,domain_name,&bdomain);

  if (nret == NSRV_NOT_REGISTERED)
    {
      /* This is not safe in case two wms try to create
       * the domain at the same time */
      check_nsrv(nsrv_new_bdomain_id(wm_signature,&domain_id),__LINE__,5);
      fill_in_domain(domain_id,&bdomain);
      check_bmsg(bmsg_init(wm_bport_id,&bdomain,BDOMAIN_CREATE | 
#ifdef DEBUG_MPS
			   BMSG_ALOG_ON |
			   BMSG_ALOG_OPEN |
			   BMSG_ALOG_CLOSE |
			   BMSG_ALOG_MASTER |
#endif
			   BPORT_NOTIFICATION ),__LINE__, 1);
      check_nsrv(nsrv_bdomain_register(bdomain_key,domain_name,wm_signature,&bdomain),__LINE__,2);
    }
  else if (nret == NSRV_OK)
      check_bmsg(bmsg_init(wm_bport_id,&bdomain, BPORT_NOTIFICATION),__LINE__,1);
  else 
    {
      check_nsrv(nret, __LINE__, 4);
    }
  check_bmsg(bport_port(wm_bport_id,&my_bport),__LINE__,2);
  check_nsrv(nsrv_bport_register(session_key,wm_port_name,
				 wm_signature,&my_bport),__LINE__,2);

  /* Now set up the a-layer ports */
  notify[HALT1_APORT_NUMBER] = halt1_notify;
  notify[HALT2_APORT_NUMBER] = halt2_notify;
  notify[HIGH_APORT_NUMBER] = high_wm_notify;
  notify[LOW_APORT_NUMBER] = low_wm_notify;

#ifdef HAVE_SIGPROCMASK
  sigemptyset(&sigio_mask);
  sigaddset(&sigio_mask, SIGIO);
#ifdef SIGPOLL
  sigaddset(&sigio_mask, SIGPOLL);
#endif
#endif
#ifdef HAVE_SIGACTION
  {
    struct sigaction sa;
    sa.sa_handler = sigio_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    (void) sigaction(SIGIO, &sa, (struct sigaction *) 0);
#ifdef SIGPOLL
    (void) sigaction(SIGPOLL, &sa, (struct sigaction *) 0);
#endif
    sa.sa_handler = SIG_IGN;
    (void) sigaction(SIGINT,&sa, (struct sigaction *) 0);
  }
#else
  signal(SIGINT, SIG_IGN);
  signal(SIGIO,sigio_handler);
#ifdef SIGPOLL
  signal(SIGPOLL,sigio_handler);
#endif
#endif

  check_amsg(amsg_init(STATIC_PORTS,notify,local_aport_ids,0), __LINE__,1);

  check_amsg_soft(aport_set_option(local_aport_ids[LOW_APORT_NUMBER],
				   APORT_NOTIFY_LEVEL,
				   (aport_optval_t) 1),__LINE__, 4);
  check_amsg_soft(aport_set_option(local_aport_ids[HIGH_APORT_NUMBER],
				   APORT_NOTIFY_LEVEL,
				   (aport_optval_t) 2),__LINE__, 4);
  check_amsg_soft(aport_set_option(local_aport_ids[HALT1_APORT_NUMBER],
				   APORT_NOTIFY_LEVEL,
				   (aport_optval_t) 3),__LINE__, 4);
  check_amsg_soft(aport_set_option(local_aport_ids[HALT2_APORT_NUMBER],
				   APORT_NOTIFY_LEVEL,
				   (aport_optval_t) 4),__LINE__, 4);
  register_aport_names();
}

/*-------------------------------------------------------------*/
/*          Worker startup Routines                            */
/*-------------------------------------------------------------*/

/* Starting up a new worker involves
  i. Creating it : bproc_create(...)
 ii. Waiting until the new worker registers its bport with the name server:
     wait_for_bport_connections(...)
 iii. Waiting until the new worker registers its aports with the name server:
     wait_for_aport_connections.
 iv. Sending the new worker the port names of all other workers
     send_port_names(..)
  v. Sending all the other (old) workers the port name of the new worker
     send_new_port(...)
  vi. Waiting for an acknowledge message from the new worker:
      the DONE_INIT_OPENS message.

This is the protocol implemented in add_worker().

At initial startup, instead of starting workers one by one, we create
all of them (see create_workers()), wait for all the bport and aport
registrations and then send the all the portnames to each worker
(see initialise_workers()).  

*/

/* bproc_create - start a worker */

#define LOCAL_SLAVE_EXTRA_ARGS 7
#define REM_SLAVE_EXTRA_ARGS 10

/*ARGSUSED*/
int bproc_create(hostname,slave_no,lsession_key,first,argc,argv)
char * hostname;
int slave_no;
nsrv_name_t lsession_key;
int first;
int argc;
char ** argv;

{
  bmsg_ret_t ret;
  sstring s_slave_no;
  sstring s_nsrv_port;
  char ** slave_argv;
  int slave_argc;
  int i,j;
  machine_t *mc;
  worker_t *w;
 
  if(gethostbyname(hostname) == NULL)
    {
      fprintf(stderr,"Invalid hostname %s\n",hostname);
      return(0); 
    }
  
  mc = get_mc(hostname);
  ret = fork();
  
  if (ret == 0)  { /* child */
    sprintf(s_slave_no,"%d",slave_no);
    sprintf(s_nsrv_port,"%d",nsrv_port_number);
    if(strcmp(hostname,wm_host) == 0) { /* local worker */
      
      slave_argc = argc + LOCAL_SLAVE_EXTRA_ARGS;
      slave_argv = (char **) hp_alloc_size(sizeof(char *) * 
					   (slave_argc + 1));
      
      /* client needs slave_no, session_key */

      slave_argv[0] = mc->exec_file;
      slave_argv[1] = "-a";
      slave_argv[2] = s_slave_no;
      slave_argv[3] = lsession_key;
      slave_argv[4] = nsrv_host;
      slave_argv[5] = s_nsrv_port;
      slave_argv[6] = first == 1 ? "-m" : "-c";
      slave_argv[7] = mc->heap_map_file;
	  
      for(i = LOCAL_SLAVE_EXTRA_ARGS+1, j = 1 ; i < slave_argc ; i++, j++)
	slave_argv[i] = argv[j];
      
      slave_argv[slave_argc + 1] = (char *) 0;
      
      ret = execvp(mc->exec_file,slave_argv);
      /*NOTREACHED*/      
      perror("Execvp failed");
      exit(4);
    }

    else  
      { /* Non-local worker to be created - use rsh */

/*	fclose(stdout); */
	slave_argc =  argc + REM_SLAVE_EXTRA_ARGS;
	slave_argv = (char **) hp_alloc_size(sizeof(char *) * 
					     (slave_argc + 1));
	slave_argv[0] = "rsh";
	slave_argv[1] = slave_no == 1? "": "-n";  /*slave worker needn't stdin*/
	slave_argv[2] = hostname;
	slave_argv[3] = mc->exec_file;
	slave_argv[4] = "-a";
	slave_argv[5] = s_slave_no;
	slave_argv[6] = lsession_key;
	slave_argv[7] = nsrv_host;
	slave_argv[8] = s_nsrv_port;
	slave_argv[9] = first == 1 ? "-m" : "-c";
	slave_argv[10] = mc->heap_map_file;
	
	for(i = REM_SLAVE_EXTRA_ARGS+1, j = 1 ; i < slave_argc ; i++, j++)
	  slave_argv[i] = argv[j];
	slave_argv[slave_argc + 1] = (char *) 0;
	
	ret = execvp(slave_argv[0],slave_argv);
      
	perror("Execvp failed");
	exit(4);
    }
  }
  else if (ret > 0) 
    {
      if (wm_verbose_startup)
	printf("created worker %d, pid = %d\n",slave_no,ret);
      w = get_worker(slave_no);
      w->pid = ret;
      return(1);
	}
  else
    {
      fprintf(stderr,"Could not fork slave %d\n",slave_no);
      perror("Could not fork");
      return(0);
    }
  /* should never be reached - lint warning fix */
   return(-1);
}


#define TIME_OUT 3000
#define BPORT 1
#define APORT 2

void wait_for_port_connection(w,port_type,dont_time_out)
worker_ptr w;
int port_type;
int dont_time_out; /* do not time out if starting workers manually */
{
  nsrv_ret_t ret; 
  int j=0, time_out=TIME_OUT; /* 30 seconds */

  if (port_type == BPORT) {
     bport_t slave_bport;
     do {
	ret = nsrv_bport_look_up(session_key,w->bport_name,&slave_bport);
	if (ret == NSRV_OK) {
	   w->pid = slave_bport.bpid;
	   w->bport_id = slave_bport.bport_id;
	   return;
	}
	short_sleep(1000);
	j++;
     } while(ret != NSRV_OK && ((j < time_out) || dont_time_out));
  } else {
     nsrv_name_t w_aport_name;
     aport_t slave_aport;
     int wm_connected, halt1_connected, halt2_connected;
     wm_connected = halt1_connected = halt2_connected = 0;
     do {
        if (!wm_connected) {
	   sprintf(w_aport_name,"%s_wm_aport",w->bport_name);
	   ret = nsrv_aport_look_up(session_key,w_aport_name,&slave_aport);
	   if (ret==NSRV_OK) {
	      w->wm_aport_id = slave_aport.aport_id;
	      wm_connected = 1;
	   }
	}
        if (!halt1_connected) {
	   sprintf(w_aport_name,"%s_halt1_aport",w->bport_name);
	   ret = nsrv_aport_look_up(session_key,w_aport_name,&slave_aport);
	   if (ret==NSRV_OK) {
	      w->halt1_aport_id = slave_aport.aport_id;
	      halt1_connected = 1;
	   }
	}
        if (!halt2_connected) {
	   sprintf(w_aport_name,"%s_halt2_aport",w->bport_name);
	   ret = nsrv_aport_look_up(session_key,w_aport_name,&slave_aport);
	   if (ret==NSRV_OK) {
	      w->halt2_aport_id = slave_aport.aport_id;
	      halt2_connected = 1;
	   }
	}
        if (wm_connected&&halt1_connected&&halt2_connected)
	   return;
	short_sleep(1000);
	j++;
     } while (j < time_out || dont_time_out); 
  }
  wm_abort_error("Workers not responding.");
  kill_system();
  exit(5);
}

void wait_for_bport_connections()
{
  int i;
  bmsg_ret_t bret;
  bport_t slave_bport;
  worker_ptr cur;

  for(i = 0; i < mc_list.num_machines; i++)
    for (cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
	wait_for_port_connection(cur,BPORT,dont_fork || 
				 mc_list.machines[i].auto_start);

  /* wait until all workers have made their bport connections */

  for(i = 0; i < mc_list.num_machines; i++)
    for (cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
    {
      do {
	bret = bport_port(cur->bport_id,&slave_bport);
      } while(bret != BMSG_OK);
    }
  Notify("Slave bports opened\n"); 
}

void wait_for_aport_connections()
{
  int i;
  worker_ptr cur;

  for(i = 0; i < mc_list.num_machines; i++)
    for (cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
      wait_for_port_connection(cur,APORT,dont_fork || 
				 mc_list.machines[i].auto_start);

  Notify("Slave a-layer ready \n");
}

void send_port(worker_aport_id,worker)
     aport_id_t worker_aport_id;
     worker_ptr worker;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  port_name_msg_t * port_name_msg;
  amsg_size_t size;

  size = sizeof(port_name_msg_t);
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__, 2);
  
  port_name_msg = (port_name_msg_t *) msg_data;
  port_name_msg->header.msg_type = PORT_NAME;
  port_name_msg->header.wid = 0;
  
  port_name_msg->index = worker->index;
  strcpy(port_name_msg->port_name,worker->bport_name);
  port_name_msg->wm_aport_id = worker->wm_aport_id;
  
  check_amsg_soft(amsg_send(worker_aport_id,msg,mdt_wm_portname,1,0),
		  __LINE__, 3);
}

void send_new_port(new_worker)
     worker_ptr new_worker;
{
  worker_ptr w;
  int i;

  for(i = 0; i < mc_list.num_machines; i++)
    for (w = mc_list.machines[i].list; w != NULL; w = w->next)
    if (w != new_worker)
      send_port(w->wm_aport_id,new_worker);
}

void send_port_names(worker_aport_id)
     aport_id_t worker_aport_id;
{
  amsg_t msg;
  amsg_data_t * msg_data;
  worker_ptr w;
  wm_simple_msg_t * wm_mess_hdr;
  amsg_size_t size;
  int i;

  for(i = 0; i < mc_list.num_machines; i++)
    for (w = mc_list.machines[i].list; w != NULL; w = w->next)
      send_port(worker_aport_id,w);

  send_simple_wm_message(worker_aport_id, SENT_INIT_PORT_NAMES, 
			 mc_list.total_workers);
}


void send_init_port_names()
{
  worker_ptr cur;
  int i;

  for(i = 0; i < mc_list.num_machines; i++)
    for (cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
      send_port_names(cur->wm_aport_id);
}

void synch_with_slaves()
{
  /* a barrier synchronisation, each slave sends an ack and
     once all have done, we send acks back to all slaves,
     After this point, all slaves are connected to each other */

  worker_ptr cur;
  int i;

  while (init_acks != (mc_list.total_workers)) short_sleep(1000);

  for(i = 0; i < mc_list.num_machines; i++)
    for (cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
    {
      check_amsg_soft(send_simple_wm_message(cur->wm_aport_id,ALL_CONNECTED,0),
		 __LINE__, 3);
      cur->status = AWAKE;
    }

}

int print_worker_command(index,file_flag,argc,argv,hostname)
int index;
char * file_flag;
int argc;
char * argv[];
char * hostname;
{
  sstring slave_file;
  int i;
  machine_t *mc;

  mc = get_mc(hostname);

  if (init_exec != NULL)
    strcpy(slave_file,init_exec);
  else 
    sprintf(slave_file,"%s/bin/%s/worker",eclipsehome(),HOSTARCH);

  printf("Slave command is: \n run -a %d %s %s %d %s %s", 
	 index, session_key, nsrv_host, nsrv_port_number,
	 file_flag,mc->heap_map_file);
  
  for(i = 1; i < argc; i++)
    printf(" %s",argv[i]);
  printf("\non host %s\n", hostname);

}

int add_worker(argc,argv,mc)
     int argc;
     char * argv[];
     machine_t *mc;
{
  int worker_index;
  worker_ptr new_worker;
  int ret;
  
  worker_index = init_worker(mc->hostname,1);
  new_worker = get_worker(worker_index);
  
  if (dont_fork || !(mc->auto_start))  {
    printf("Worker manager (session %s) waiting for 1 worker\n",
	   session_key);
    print_worker_command(worker_index,(new_worker->first? "-m" : "-c"),
			 argc,argv,mc->hostname);
    ret = 1;
  }
  else  {
    ret = bproc_create(mc->hostname,worker_index,session_key,
		       new_worker->first,argc,argv);
  }
  if (ret)  {
    new_worker = get_worker(worker_index);
    wait_for_port_connection(new_worker,BPORT);
    wait_for_port_connection(new_worker,APORT);
    
    send_port_names(new_worker->wm_aport_id);
    send_new_port(new_worker);
    
    while (init_acks != (mc_list.total_workers)) short_sleep(1000);
    check_amsg_soft(send_simple_wm_message(new_worker->wm_aport_id,
					   ALL_CONNECTED,0),__LINE__, 3);
    new_worker->status = AWAKE;
    check_amsg_soft(send_simple_wm_message(new_worker->wm_aport_id, 
					   ROOT_INITIALISED,0), __LINE__, 3);
    return(1);
  }
  else
    return(ret);
}

/*-----------------------------------------------------------------*/
/* The worker manager event-handling loop */
/*-----------------------------------------------------------------*/
void wait_for_end()
{
  while (1) 
    {
      if (interactive) 
      {
	(void) aport_set_option(local_aport_ids[LOW_APORT_NUMBER],
				APORT_NOTIFY, (aport_optval_t) AMSG_OFF);
	tk_OneEvent(1);
	update_perf_window();
	(void) aport_set_option(local_aport_ids[LOW_APORT_NUMBER],
				APORT_NOTIFY, (aport_optval_t) AMSG_ON);
	short_sleep(10000);
      }
      else
      {
	short_sleep(1800000000);	/* half an hour :-) */
      }
    }
}

void fork_nameserver()
{
  char nsrv_exec_file[MAX_PATH_LEN];
  char s_nsrv_port_number[128];
#ifdef HAVE_SYSINFO
  char mcname[128];
#endif
  int ret;
  int j = 60; /* number of seconds to wait for nameserver to be up */


  sprintf(nsrv_exec_file,"%s/bin/%s/nsrv",eclipsehome(),HOSTARCH);
  ret = fork();
  if (ret == 0) /* child */
    {
      char *nsrv_argv[6];

      setsid();  /* create a new process group for the name server */
      nsrv_argv[0] = nsrv_exec_file;
      nsrv_argv[1] = "-p";  
      nsrv_argv[2] = map_dir;
#if defined(HAVE_SYSINFO) && defined(HAVE_SYS_SYSTEMINFO_H)
      sysinfo(SI_MACHINE, mcname, 128);
      if(strcmp("DRS 6000",mcname) == 0) {
	nsrv_argv[3] = (char *) 0;
	ret = execvp(nsrv_exec_file,nsrv_argv);
	perror("Could not start (exec) the name server");
	exit(6);
      }
      else
#endif /* HAVE_SYSINFO */
	{
	  nsrv_argv[3] = "-nshm"; /* no shared-memory interaction */
	  nsrv_argv[4] = "-npds"; /* no pds-mps based interaction */
	  nsrv_argv[5] = (char *) 0;
	  ret = execvp(nsrv_exec_file,nsrv_argv);
	  perror("Could not start (exec) the name server");
	  exit(6);
	}
    }
  else if (ret > 0) 
    {
      fprintf(stderr,"Starting Name Server %s pid = %d...",nsrv_exec_file,ret);
      fflush(stderr);      
      while((j > 0) && (nsrv_init(nsrv_host, &nsrv_port_number) != NSRV_OK))
	{
	  short_sleep(1000000);
	  j--;
	}
      if (j == 0)
	{
	  wm_abort_error("Name server not responding.\n Try starting a new name server (nsrv) manually.\n");
	  exit(6);
	}
      else
	fprintf(stderr," ... done\n");
    }
  else {
    perror("Could not fork name serever!");
    exit(6);
  }
}

/* Creation of "initial" workers */
void create_workers(argc,argv)
     int argc;
     char * argv[];
{
  worker_ptr w;
  int i;
  
  if (dont_fork)  
    {
      printf("Worker manager (session %s) waiting for %d worker(s)\n",
	     session_key, mc_list.total_workers);
      for(i = 0; i < mc_list.num_machines; i++)
	for(w = mc_list.machines[i].list; w != NULL; w = w->next)
	  print_worker_command(w->index,w->first ? "-m" : "-c",argc,argv,
			       mc_list.machines[i].hostname);
    }
  else  
    {
      for(i = 0; i < mc_list.num_machines; i++)
	if(mc_list.machines[i].auto_start)
	  {
	    for(w = mc_list.machines[i].list; w != NULL; w = w->next)
	      if (!bproc_create(mc_list.machines[i].hostname,w->index,
				session_key, w->first,argc,argv)) 
		{
		  fprintf(stderr,"Could not create slave: %d\n",w->index);
		  kill_system();
		  exit(7);  
		}
	  }
	else
	  for(w = mc_list.machines[i].list; w != NULL; w = w->next)
	    print_worker_command(w->index,w->first ? "-m" : "-c",
				 argc,argv,mc_list.machines[i].hostname);
    }
}    

/* Implements the initial startup protocol -
   All the "initial" workers (specified via the -w command-line
   option or the .eclipse_machines file) have been started, before
   this routine is called */

void initialise_workers()
{
 worker_ptr w;
 int i;
 
 wait_for_bport_connections();
 wait_for_aport_connections();
 
 send_init_port_names();
 Notify("Sent port names and Root Id\n");
 
 synch_with_slaves();
 
 while(!got_root_id) short_sleep(2000);
 
 for(i = 0; i < mc_list.num_machines; i++)
   for(w = mc_list.machines[i].list; w != NULL; w = w->next)
     if(w->index != root_id_sender)
       check_amsg(send_simple_wm_message(w->wm_aport_id, 
					 ROOT_INITIALISED, 0), __LINE__, 3);
 
}
/*-------------------------------------------------------*/
int main(argc,argv)
     int argc;
     char * argv[];
{
  nsrv_ret_t nret;
  int i;
 
 
  get_arguments(argv, &argc, &num_workers, &dont_fork);
  irq_lock_init(delayed_signal_handler);
  init_global_values();

  nret = nsrv_init(nsrv_host, &nsrv_port_number);
  if (nret != NSRV_OK)
    {
      if (nret == NSRV_NOSERVER)
	{
	  /* try starting a nameserver on the local machine */
	  nsrv_host = wm_host;
	  fork_nameserver();
	}
      else
	check_nsrv(nret,__LINE__, 1);
    }

  mps_init(); 

  create_workers(argc,argv);

  initialise_workers();

  if (interactive)
    start_tkwindow();

  Notify("Waiting for end\n");
  wait_for_end();
}

#ifndef HAVE_RANDOM
long
random()
{
	return (long) rand();
}

srandom(x)
int x;
{
	srand(x);
}
#endif

void
short_sleep(usec)
int usec;
{
#ifdef HAVE_SELECT
    struct timeval sleep_time;
    sleep_time.tv_sec  = usec / 1000000;
    sleep_time.tv_usec = usec % 1000000;
    (void) select(0, 0, 0, 0, &sleep_time);
#endif
}

/* worker info setting and getting functions */
  
send_worker_info(reqwid, bufsize, buf)
int reqwid, bufsize;
void_ptr buf;
{
 
  amsg_t msg;
  amsg_data_t * msg_data;
  worker_info_msg_t * worker_info_msg;
  amsg_size_t size;
  worker_ptr w;
  
  w = get_worker(reqwid);
  
  size = sizeof(worker_info_msg_t) + bufsize;
  
  check_amsg(amsg_alloc(size,
			&msg_data,
			&msg),__LINE__, 2);
  
  worker_info_msg = (worker_info_msg_t *) msg_data;
  worker_info_msg->header.msg_type = WORKER_INFO_NOTIFY;
  worker_info_msg->header.wid = 0;
  worker_info_msg->size = bufsize;
  memcpy((void_ptr) ((worker_info_msg_t *) msg_data + 1), buf, bufsize); 
  check_amsg_soft(amsg_send(w->wm_aport_id,msg,MDT_BYTE,size,0),__LINE__,3);
}

/*-----------------------------------------------------------------*/
/* Worker Statistics support */
/*-----------------------------------------------------------------*/


/* called from update_perf_window (in wm_interface.c) */
/* The return message (WSTAT_RET) updates the current workers
   cur_wstat field. This message is received on the high wm-aport
   since we are busy waiting on the reply. */

get_worker_stats(flag)
int flag;
{
  worker_ptr cur;
  int i;
  struct worker_stat_ext wstat;

  for(i = 0; i < mc_list.num_machines; i++)
    for(cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
      {
	if(flag == 0) /* top-like display */
	  cur->start_wstat = cur->cur_wstat;
	wstat_received = 0;
	check_amsg_soft(send_simple_wm_message(cur->wm_aport_id,
					       WSTAT_REQ, 0),
			__LINE__, 3);
	while(!wstat_received)
	  short_sleep(1000);

	if (flag == 1) /* reset */
	  cur->start_wstat = cur->cur_wstat;
      }
}

/* mdt type definition for worker_stat_ext structure */	
/* mirrored in trace.c */
wstat_types_init(mdt_wstat)
amsg_type_t * mdt_wstat;

{
  amsg_typedef_t template[33];

  /* mdt_wstat */
  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = MDT_INT32;
  template[3] = MDT_INT32;
  template[4] = MDT_INT32;
  template[5] = MDT_INT32;
  template[6] = MDT_INT32;
  template[7] = MDT_INT32;
  template[8] = MDT_INT32;
  template[9] = MDT_INT32;
  template[10] = MDT_INT32;
  template[11] = MDT_INT32;
  template[12] = MDT_INT32;
  template[13] = MDT_INT32;
  template[14] = MDT_INT32;
  template[15] = MDT_INT32;
  template[16] = MDT_INT32;
  template[17] = MDT_INT32;
  template[18] = MDT_INT32;
  template[19] = MDT_ARRAY_OF;
  template[20] = 3;
  template[21] = MDT_DP_FLOAT;
  template[22] = MDT_DP_FLOAT;
  template[23] = MDT_ARRAY_OF; 
  template[24] = MAX_NUM_EVENTS;
  template[25] = MDT_DP_FLOAT;
  template[26] = MDT_INT32;
  template[27] = MDT_INT32;
  template[28] = MDT_ARRAY_OF; 
  template[29] = MAX_NESTING;
  template[30] = MDT_INT32;
  template[31] = MDT_STRUCT_CLOSE;
  template[32] = MDT_END;
  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 100, 
		       template, mdt_wstat), __LINE__, 4);
}

