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
 * Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/**********************************************************************
**      System: Parallel Distributed System
**        File: trace.c
**      Author: Shyam Mudambi
** Description: Tracing routines
***********************************************************************/

#include "config.h"
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>

#ifdef HAVE_SGIHRCLOCK
#include <sys/mman.h>
#include <sys/syssgi.h>
#include <sys/immu.h>
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

#include "os_support.h"
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

#include "trace.h"	/* sets WORKER_TRACING */
#include "wm_msgs.h"
#include "wm.h"

#define ParallelWorker (ec_options.parallel_worker)

struct worker_stat wstat_;
struct worker_stat_ext wstat_ext;

#ifdef WORKER_TRACING

#include <sys/time.h>

extern void	check_amsg();
extern nsrv_name_t session_key;
extern aport_id_t wm_low_aport_id;  

trace_header_t theader;
trace_event_t events[MAX_EVENTS];
volatile int tracing = 0;


/* During tracing, if the buffer overflows, it is dumped onto the
   temp_trace_file (temp_fd) by each worker. After the run, each worker 
   processes its temp_trace_file to convert it to alog format and this
   is stored in trace_file. Lastly all the trace files are appended to
   each other to produce the final log file.
*/

static char *tmpdir;
static char temp_trace_file[MAX_PATH_LEN]; 
static char trace_file[MAX_PATH_LEN];
static trace_header_t copy_theader;
static int temp_fd;

init_trace_file_names()
{
  tmpdir = getenv("ECLIPSETMP");
  if(!tmpdir)
    tmpdir = "/tmp";

  sprintf(temp_trace_file,"%s/%d.%d.temp.log",
	  tmpdir, ParallelWorker,session_key);
  sprintf(trace_file,"%s/%d.%d.gsx.log",
	  tmpdir, ParallelWorker,session_key);
}

p_start_trace()
{
  tracing = -1;
  start_tracing_message();
  while (tracing <= 0) ;

  LOG_EVENT(WORKING);

  Succeed_
}

start_tracing_message()
{
  send_simple_wm_message(wm_low_aport_id, START_TRACING);
}

p_stop_trace()
{
  LOG_EVENT(IDLE);
  stop_tracing_message();
  
  while(tracing > 0) ;

  Succeed_
}

stop_tracing_message()
{
  send_simple_wm_message(wm_low_aport_id, STOP_TRACING);
}

start_tracing()
{

  temp_fd = ec_open(temp_trace_file, O_WRONLY | O_CREAT, 0660);
  if(temp_fd < 0)
    printf("%d: Could not creat trace file %s\n", temp_trace_file);
  
  theader.entries = 0;
  theader.start_time = gethrtime();

  if (tracing >=  0) 
    events[theader.entries % MAX_EVENTS].event = IDLE;
  else
    events[theader.entries % MAX_EVENTS].event = WORKING;

  events[theader.entries % MAX_EVENTS].time = theader.start_time;
  theader.entries++; 
  tracing = 1;
}

log_reset()
{
    int i;
    wstat_.current_activity_start = gethrtime();
    for (i=0; i<MAX_NUM_EVENTS; i++)
	wstat_.cumulated_event_time[i] = 0;
}

log_init(event)
int event;
{
    wstat_.trace_stack_ptr = 0;
    wstat_.current_activity = event;
    log_reset();
}

stop_tracing()
{
  tracing = 0;
}

trace_header_t * get_trace_ptr()
{
  return(&theader);
}

p_write_traces(v_widlist, t_widlist, v_filename, t_filename)
value v_widlist, v_filename;
type t_widlist, t_filename;
{

  pword *p;
  int totalentries = 0;
  int numprocs = 0;
  hrtime_t trace_start = 0;
  int utime, i;
  int gsxid;          /* id for gsx - needs to be consecutive */
  char *logname;
  FILE *logfile;
  char command[1024];
  char worker_trace[MAX_PATH_LEN];
  
  Check_List(t_widlist);
  p = v_widlist.ptr;
  Dereference_(p);
  while(!IsNil(p->tag))
    {
/*      wm_get_worker_info(p->val.nint, TRACE_INFO, 
			 sizeof(trace_header_t), &copy_theader); */
      get_trace_header(p->val.nint, &copy_theader);
      numprocs++;
      if(numprocs == 1)
	trace_start = copy_theader.start_time;
      else if(copy_theader.start_time < trace_start)
	trace_start = copy_theader.start_time;
      totalentries+= copy_theader.entries;
      
      p = p + 1;
      if (!IsNil(p->tag)) {
	p = p->val.ptr;
	Dereference_(p);
      }
    }

  Get_Name(v_filename,t_filename, logname);

/*  sprintf(command,"/bin/rm -f %s", logname);*/
  
  logfile = fopen(logname,"w");

  if(logfile == NULL) 
    {
      (void) fprintf(stderr, "can't open event log file %s for writing\n", logname);
      Fail_
    }
  else
    {
      (void) fprintf(logfile,"-3 0 0 %d 0 0 \n", numprocs);
      (void) fprintf(logfile,"-2 0 0 %d 0 0 \n", totalentries);
      (void) fprintf(logfile,"-6 0 0 0 0 0 \n");
      fclose(logfile);

      p = v_widlist.ptr;
      gsxid = 0;
      Dereference_(p);
      while(!IsNil(p->tag)) {
	copy_theader.start_time = trace_start;
	copy_theader.entries = gsxid++;
/*	wm_set_worker_info(p->val.nint, TRACE_INFO, 
			   sizeof(trace_header_t), &copy_theader);*/
	set_trace_header(p->val.nint, &copy_theader);

	sprintf(worker_trace,"%s/%d.%d.gsx.log",
		tmpdir,p->val.nint,session_key);
	sprintf(command,"cat %s >> %s",
		worker_trace,logname);
	if (system(command) < 0)
	  (void) fprintf(stderr,"command %s failed\n",command);

	if (ec_unlink(worker_trace) < 0) 
	  perror("Could not remove worker trace file");

	p = p + 1;
	if (!IsNil(p->tag)) {
	  p = p->val.ptr;
	  Dereference_(p);
	}
      }
      Succeed_
    }
}

write_trace_buffer(full)
int full;
{
  int i,ub;

  if (temp_fd >= 0) {
    if (full)
      {
	ub = MAX_EVENTS;
	LOG_EVENT_PUSH(TRACE_BUF_OVERFLOW)
      }
    else
      ub = theader.entries % MAX_EVENTS;
    for(i = 0; i < ub; i++) 
      if (write(temp_fd, &events[i],sizeof(trace_event_t)) < 0) {
	Set_Errno;
	return SYS_ERROR ;
      }
    if (full)
      LOG_EVENT_POP
    else
       close(temp_fd);
  }
}



print_trace(trace_header)
trace_header_t * trace_header;
{
  hrtime_t trace_start;
  long utime, hrtime_to_usec();
  int i;
  trace_event_t evt;
  FILE *tf, *temp_log;
  char command[1024];
  int gsxid;  /* process id for gsx (has to be consecutive) */

  copy_theader = *trace_header;
  
  trace_start = copy_theader.start_time;
  gsxid = copy_theader.entries;
  
  write_trace_buffer(0);

  tf = fopen(trace_file,"a");

  if(tf == NULL) 
    (void) fprintf(stderr,"%d: can't open trace file %s\n", 
	    ParallelWorker, trace_file);
  else
    {
      temp_fd = ec_open(temp_trace_file,O_RDONLY, 0660);
      if (temp_fd < 0)
	(void) fprintf(stderr,"%d: can't open trace file %s\n", 
		ParallelWorker, temp_trace_file);
      else 
	{
	  for(i = 0; i < theader.entries ; i++)  {
	    read(temp_fd, &evt, sizeof(trace_event_t));
	    utime = (long) hrtime_to_usec(evt.time - trace_start);
	    if (utime < 0 )
	      {
		printf("%d:Error in logfile utime = %d", ParallelWorker, utime);
		printf(" start_time = %lld, evt = %d, evt_time = %lld\n",
		       trace_start, evt.event, evt.time);
		printf("i = %d entries = %d local_trace_start = %lld \n", 
		       i, theader.entries, theader.start_time);
	      }
	    else
	      (void) fprintf(tf,"%d %d 0 0 0  %d\n",
		      evt.event, gsxid, utime);
	  }
	  close(temp_fd);
	  if (ec_unlink(temp_trace_file) < 0)
	    perror("could not remove temporary trace file"); 
	}     
      fclose(tf);
    }
}

#if defined(HAVE_SGIHRCLOCK)
volatile hrtime_t *timer_addr;
/* cycleval stores the resolution of the hardware clock in picoseconds */
__psunsigned_t cycleval;

/* initialise high-resolution clock */
void init_hrclock()
{
  int fd, poffmask;
__psunsigned_t phys_addr, raddr;

  poffmask = getpagesize() - 1;
  if((phys_addr = syssgi(SGI_QUERY_CYCLECNTR, &cycleval)) == -1
     && errno == ENODEV)
    printf("Error in Configuration! No high-resolution timer!");

  raddr = phys_addr & ~poffmask;
  fd = ec_open("/dev/mmem", O_RDONLY);
  timer_addr = (volatile hrtime_t *)mmap(0, poffmask, PROT_READ,
					 MAP_PRIVATE, fd, (__psint_t)raddr);
  
  timer_addr = (hrtime_t *)((__psunsigned_t)timer_addr + 
			    (phys_addr & poffmask));

}
/* convert time returned by high-resolution  clock to microseconds */
long hrtime_to_usec(hrtime)
hrtime_t hrtime;
{
  unsigned long long temp;
  
  temp = hrtime;
  temp = temp * cycleval / 1000000;  /* cycleval is in picoseconds */
  return((long) temp);
}

double hrtime_to_msec(hrtime)
hrtime_t hrtime;
{
  double temp;
  temp = (hrtime / 1000000000.0);
  return(temp * cycleval);
}

#else /* if HAVE_GETHRTIME */

void init_hrclock()
{ }

/* convert time returned by high-resolution  clock to microseconds */
long hrtime_to_usec(hrtime)
hrtime_t hrtime;
{
  return((long) (hrtime / 1000));
}

double hrtime_to_msec(hrtime)
hrtime_t hrtime;
{
  return((hrtime / 1000000.0));
}

#endif /* HAVE_SGIHRCLOCK */

#else  /* WORKER_TRACING */

/*ARGSUSED*/
p_write_traces(v_widlist, t_widlist, v_filename, t_filename)
value v_widlist, v_filename;
type t_widlist, t_filename;
{
  Succeed_
}

p_start_trace()
{
  Succeed_
}

p_stop_trace()
{
  Succeed_
}

#endif /* WORKER_TRACING */


/*---------------------------------------------------------------------
 * Access to local worker statistics
 * It is collected in the structure wstat_
 *---------------------------------------------------------------------*/

wstat_init()
{
    wstat_.job_count = 0;
    wstat_.copy_from_bytes = 0;
    wstat_.copy_to_bytes = 0;
    wstat_.copy_to_extra = 0;
    wstat_.cut_count = 0;
    wstat_.prune_count = 0;
    wstat_.publish_count = 0;
    wstat_.parallel_chpts = 0;
    wstat_.parallel_alts = 0;
    wstat_.published_chpts = 0;
    wstat_.published_alts = 0;
    wstat_.polls_expired = 0;
    wstat_.polls_succeeded = 0;
    wstat_.rpc_sleep = 0;
    wstat_.copy_more_count = 0;
    wstat_.copy_from_count = 0;
    wstat_.copy_to_count = 0;
#ifdef WORKER_TRACING
    init_hrclock();
#endif
}


/*
 * worker_statistics(+Wid, -Stat)
 * Retrieve the local statistics data of worker Wid
 */

#define WSTAT_ITEMS (13 + MAX_NUM_EVENTS)

p_worker_stat(vwid, twid, vstat, tstat)
value vwid, vstat;
type twid, tstat;
{
    struct worker_stat_ext s;
    pword *p;
    int i;

    Check_Integer(twid);
    if (vwid.nint == ParallelWorker)
        get_worker_stat(&s);
    else if (valid_wid((int) vwid.nint))
      request_wstat((int) vwid.nint, &s);
    else
    {
	Bip_Error(RANGE_ERROR);
    }

    p = TG;
    Push_Struct_Frame(in_dict("data", WSTAT_ITEMS));

    Make_Integer(&p[1], s.job_count);
    Make_Integer(&p[2], s.prune_count);
    Make_Integer(&p[3], s.cut_count);
    Make_Integer(&p[4], s.copy_from_count);
    Make_Integer(&p[5], s.copy_from_bytes);
    Make_Integer(&p[6], s.copy_to_count);
    Make_Integer(&p[7], s.copy_to_bytes);
    Make_Integer(&p[8], s.publish_count);
    Make_Integer(&p[9], s.published_chpts);
    Make_Integer(&p[10], s.published_alts);

    Make_Float(&p[11], s.times[0]);
    Make_Float(&p[12], s.times[1]);
    Make_Float(&p[13], s.times[2]);

    for (i=0; i<MAX_NUM_EVENTS; i++)
    {
	Make_Checked_Double(&p[14+i], s.cumulated_event_time[i]);
    }
    Return_Unify_Structure(vstat, tstat, p);
}

p_worker_stat_reset(vwid, twid)
value vwid;
type twid;
{
    Check_Integer(twid);
    if (vwid.nint == ParallelWorker)
    {
	reset_worker_stat();
    }
    else if (valid_wid((int) vwid.nint))
    {
/*	wm_set_worker_info(vwid.nint, WSTAT_INFO, 0, (void_ptr) 0);*/
      reset_wstat((int) vwid.nint);
    }
    else
    {
	Bip_Error(RANGE_ERROR);
    }
    Succeed_;
}

/* called on the worker that provides the statistics data */

int get_worker_stat(wstat_ext1)
struct worker_stat_ext * wstat_ext1;
{
    /* to force updating the cumulative counters */
    LOG_EVENT_PUSH(TRACE_BUF_OVERFLOW)
    LOG_EVENT_POP
    (void) all_times(&wstat_.times[0], &wstat_.times[1], &wstat_.times[2]);
    convert_to_ext(wstat_,wstat_ext1);
}

int convert_to_ext(w,wext)
struct worker_stat w;
struct worker_stat_ext *wext;
{
  int i;

  wext->job_count = w.job_count;
  wext->copy_from_bytes = w.copy_from_bytes;
  wext->copy_to_bytes = w.copy_to_bytes;
  wext->copy_to_extra = w.copy_to_extra;
  wext->cut_count = w.cut_count;
  wext->prune_count = w.prune_count;
  wext->publish_count = w.publish_count;
  wext->parallel_chpts = w.parallel_chpts;
  wext->parallel_alts = w.parallel_alts;
  wext->published_chpts = w.published_chpts;
  wext->published_alts = w.published_alts;
  wext->polls_expired = w.polls_expired;
  wext->polls_succeeded = w.polls_succeeded;
  wext->rpc_sleep = w.rpc_sleep;
  wext->copy_more_count = w.copy_more_count;
  wext->copy_from_count = w.copy_from_count;
  wext->copy_to_count = w.copy_to_count;
  wext->times[0] = w.times[0];
  wext->times[1] = w.times[1];
  wext->times[2] = w.times[2];
#ifdef WORKER_TRACING
  wext->current_activity_start = hrtime_to_msec(w.current_activity_start);
  for (i=0; i<MAX_NUM_EVENTS; i++)
    wext->cumulated_event_time[i] = hrtime_to_msec(w.cumulated_event_time[i]);
#else
  wext->current_activity_start = 0.0;
  for (i=0; i<MAX_NUM_EVENTS; i++)
    wext->cumulated_event_time[i] = 0.0;
#endif
}

reset_worker_stat()
{
    wstat_init();
#ifdef WORKER_TRACING
    log_reset();
#endif
}

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

#ifdef WORKER_TRACING
trace_types_init(mdt_trace)
amsg_type_t * mdt_trace;
{
  amsg_type_t template[6];

  template[0] = MDT_BEGIN;
  template[1] = MDT_STRUCT_OPEN;
  template[2] = MDT_INT32;
  template[3] = MDT_DWORD;
  template[4] = MDT_STRUCT_CLOSE;
  template[5] = MDT_END;
  check_amsg(amsg_type_define(ECLIPSE_WM_INTERFACE, 101, 
		       template, mdt_trace), __LINE__, 4);
}
#endif
