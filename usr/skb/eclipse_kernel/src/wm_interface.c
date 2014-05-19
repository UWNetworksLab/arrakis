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
**        File: wm_interface.c
**      Author: Shyam Mudambi
** Description: Worker Manager Tcl/Tk Interface functions for Parallel Eclipse
***********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/stat.h>
#include <errno.h>
#include <math.h>
#include <fcntl.h>
#include "config.h"

#ifdef BSD_TIMES
#include <sys/timeb.h>
#endif


#ifdef PATH_IN_LIMITS
#  include 	<limits.h>
#  define MAX_PATH_LEN	PATH_MAX
#else
#  include <sys/param.h>
#  define MAX_PATH_LEN	MAXPATHLEN
#endif

#ifdef HAVE_TK
#include "pds.h"
#include "memman.h"
#include "nsrv.h"

#include "sch_types.h"
#include "trace.h"
#include "wm_msgs.h"
#include "wm_types.h"


/* Tcl/Tk includes */
#include <tcl.h>
#include <tk.h>

/* External variables and procedures */

machine_t *get_mc();
int add_worker();
int send_to_sleep();
int wakeup();
void exit_system();
extern char *eclipsehome();

extern int interactive;
extern hdr_mc_list_t mc_list;
extern char *init_host;
extern int Argc;
extern char **Argv;
int perfon = 0;            /* Set when user turns on or off ther perfmeter */
int snapshot_display = 1;  /* top-like display or an integration (for the
			      perfmeter) */
extern int clock_hz;

/* Tk/Tcl globals */
static char *name;
static char *display;
static Tcl_Interp *interp;
/* End Tk/Tcl globals */

int check_tcl(ret,line)
int ret, line;
{
    if (ret != TCL_OK)
    {
      printf("Tcl function call failed!\n");
      printf("pid = %d ret = %d at line %d in file %s\n",
	     getpid(),ret,line,__FILE__);
      return(ret);
    }
}

int check_tk(code, line)
int code, line;
{
  if (code != TCL_OK) {
    printf("Tcl_Eval failed!\n");
    printf("pid = %d code = %d at line %d in file %s\n",
	     getpid(),code,line,__FILE__);
  }
}

int start_tkwindow()
{
  int code;
  char menu_file[128];
  char lib_menu_file[128];
  struct stat buf;

  strcpy(menu_file,"menu.tcl");

  interp = Tcl_CreateInterp();
  display = getenv("DISPLAY");
  if(display == NULL)
    {
      fprintf(stderr,"No DISPLAY environment variable set! \n");
      goto start_tkwindow_error;
    }
  else
  {
		if (Tcl_AppInit(interp) != TCL_OK) 
		{
			fprintf(stderr, "Tcl_AppInit failed: %s\n", interp->result); 
			goto start_tkwindow_error;
		}
		Tk_GeometryRequest( Tk_MainWindow(interp), 200, 200);
		Tk_SetAppName( Tk_MainWindow(interp),  "Worker Manager" );
		if ((ec_stat(menu_file,&buf) == -1) && errno == ENOENT)
		{
			sprintf(lib_menu_file,"%s/lib/%s",eclipsehome(),menu_file);
			code = Tcl_EvalFile(interp,lib_menu_file);
		}
		else
			code = Tcl_EvalFile(interp,menu_file);
		if(*interp->result != 0)
			printf("%s\n",interp->result);
		if (code != TCL_OK)
			goto start_tkwindow_error;
  }

  return(1);

 start_tkwindow_error:
  interactive = 0;
 return(0);
}

int delete_tkwindow()
{
  tk_geval("destroy .");
  Tcl_DeleteInterp(interp);
}

/*ARGSUSED*/
int AddCmd(clientdata, tkinterp, argc, argv)
ClientData clientdata;
Tcl_Interp *tkinterp;
int argc;
char *argv[];

{
  char * hostname;
  int num_add;
  int i;
  machine_t *mc;

  if (argc != 3)
    {
      tkinterp->result = "Wrong # args";
      return(TCL_ERROR);
    }
  hostname = argv[1];
  num_add = atoi(argv[2]);
/*  printf("hostname = %s num_add = %d", hostname, num_add); */
  mc = get_mc(hostname);
  for (i = 0; i < num_add; i++)
    if (!add_worker(Argc,Argv,mc))
      printf("Could not add worker on host %s\n",hostname);

  return(TCL_OK);
}

/*ARGSUSED*/
int SleepCmd(clientdata, tkinterp, argc, argv)
ClientData clientdata;
Tcl_Interp *tkinterp;
int argc;
char *argv[];

{
  char * hostname;
  int num_sleep;
  int i;
  worker_ptr cur;
  machine_t *mc;

  if (argc != 3)
    {
      tkinterp->result = "Wrong # args";
      return(TCL_ERROR);
    }
  hostname = argv[1];
  num_sleep = atoi(argv[2]);
/*  printf("hostname = %s num_workers = %d", hostname, num_workers); */
  mc = get_mc(hostname);

  if (num_sleep < mc->num_awake)
    {
      cur = mc->list; 
      for (i = 0; i < num_sleep; i++)
	{
	  while(cur->status != AWAKE)
	    cur = cur->next;
	  send_to_sleep(cur,mc);
	}
    }
  else
    printf("Error in SleepCmd, num_sleep = %d, num_awake = %d\n",
	   num_sleep, mc->num_awake);
  
  return(TCL_OK);
}

/*ARGSUSED*/
int WakeupCmd(clientdata, tkinterp, argc, argv)
ClientData clientdata;
Tcl_Interp *tkinterp;
int argc;
char *argv[];

{
  char * hostname;
  int num_wakeup,num_sleeping;
  int i;
  worker_ptr cur;
  machine_t *mc;

  if (argc != 3)
    {
      tkinterp->result = "Wrong # args";
      return(TCL_ERROR);
    }
  hostname = argv[1];
  num_wakeup = atoi(argv[2]);
  mc = get_mc(hostname);
  num_sleeping = mc->num_workers - mc->num_awake;
/*  printf("hostname = %s num_workers = %d", hostname, num_workers); */

  if (num_wakeup  <= num_sleeping)
    {
      cur = mc->list; 
      for (i = 0; i < num_wakeup; i++)
	{
	  while(cur->status != SLEEPING)
	    cur = cur->next;
	  wakeup(cur,mc);
	}
    }
  else
    printf("Error in WakeupCmd, num_wakeup = %d, num_sleeping = %d\n",
	   num_wakeup, num_sleeping);
  
  return(TCL_OK);
}

/*ARGSUSED*/
int HaltCmd(clientdata, tkinterp, argc, argv)
ClientData clientdata;
Tcl_Interp *tkinterp;
int argc;
char *argv[];

{
  exit_system(1,10);  /* will not return! */
  return(TCL_OK);
}

int CloseCmd(clientdata, tkinterp, argc, argv)
ClientData clientdata;
Tcl_Interp *tkinterp;
int argc;
char *argv[];

{
  interactive = 0;
  delete_tkwindow();
  return(TCL_OK);
}

int DisablePerfCmd(clientdata, tkinterp, argc, argv)
ClientData clientdata;
Tcl_Interp *tkinterp;
int argc;
char *argv[];

{
  perfon = 0;
  return(TCL_OK);
}

/*ARGSUSED*/
void DummyCmd(clientdata)
ClientData clientdata;
{
  Tcl_CancelIdleCall(DummyCmd,(ClientData ) NULL);
}


/*
 * This is a copy of tkAppInit.c which provides a default version of
 * Tcl_AppInit(). Since ProTcl is not just an extension, but also
 * a system that should support further extensions, we cannot provide
 * a fixed AppInit function, but we must be able to load programs
 * no matter if they provide their own Tcl_AppInit or not.
 * This is the reason we need a library that provides a default version,
 * taken from vanilla tk.
 */

/* 
 * tkAppInit.c --
 *
 *	Provides a default version of the Tcl_AppInit procedure for
 *	use in wish and similar Tk-based applications.
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*
 * The following variable is a special hack that allows applications
 * to be linked using the procedure "main" from the Tk library.  The
 * variable generates a reference to "main", which causes main to
 * be brought in from the library (and all of Tk and Tcl with it).
 *
 * In ProTcl, we don't want it, because we have our own main already.
 */

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(tkinterp)
Tcl_Interp *tkinterp;		/* Interpreter for application. */
{
    sstring host_workers;
    sstring awake_workers;
    char command[1024];
    int i;
    int Reset_wstatCmd(), Set_refresh_rateCmd();

    if(check_tcl(Tcl_Init(tkinterp),__LINE__) != TCL_OK)
      return(TCL_ERROR);

    if(check_tcl(Tk_Init(tkinterp),__LINE__) != TCL_OK)
      return(TCL_ERROR);

    /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     */

     Tcl_CreateCommand(tkinterp,"apply_add",AddCmd,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand(tkinterp,"apply_sleep",SleepCmd,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand(tkinterp,"apply_wakeup",WakeupCmd,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand(tkinterp,"halt",HaltCmd,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand(tkinterp,"close_window",CloseCmd,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand(tkinterp,"disable_perfon",DisablePerfCmd,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand(tkinterp,"reset_stat",Reset_wstatCmd,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand(tkinterp,"set_refresh_rate",Set_refresh_rateCmd,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    if (check_tcl(Tcl_LinkVar(tkinterp, "num_machines",
			      (char *) &(mc_list.num_machines),
			      TCL_LINK_INT),__LINE__) != TCL_OK)
      return(TCL_ERROR);

    if (check_tcl(Tcl_LinkVar(tkinterp, "snapshot",
			      (char *) &(snapshot_display),
			      TCL_LINK_INT),__LINE__) != TCL_OK)
      return(TCL_ERROR);

    for(i = 0; i < mc_list.num_machines; i++) {
      sprintf(host_workers,"workers(%s)",mc_list.machines[i].hostname);
      if (check_tcl(Tcl_LinkVar(tkinterp, host_workers, 
				(char *) &(mc_list.machines[i].num_workers),
				TCL_LINK_INT),__LINE__) != TCL_OK)
	return(TCL_ERROR);
      sprintf(awake_workers,"awake_workers(%s)",mc_list.machines[i].hostname);
      if (check_tcl(Tcl_LinkVar(tkinterp, awake_workers, 
				(char *) &(mc_list.machines[i].num_awake),
			       TCL_LINK_INT),__LINE__) != TCL_OK) 
	return(TCL_ERROR);
      sprintf(command,"set machines(%d) %s", i, mc_list.machines[i].hostname);
      tk_geval(command);

/*     sprintf(machine_i,"machines(%d)", i); 
       if (check_tcl(Tcl_LinkVar(tkinterp, machine_i,
				(char *) &(mc_list.machines[i].hostname),
			       TCL_LINK_STRING),__LINE__) != TCL_OK) 
	return(TCL_ERROR); */
    }
    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apprc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */

/*    tcl_RcFileName = "~/.wishrc";*/
    return TCL_OK;
}

int tk_geval(command)
char *command;
{
  check_tk(Tcl_GlobalEval(interp,command),__LINE__);
}


int tk_OneEvent(flag)
int flag;
{
  if (!flag)
    Tcl_DoOneEvent(flag); 
  else
    Tcl_DoOneEvent(TK_DONT_WAIT);
}

int tk_doidledummy()
{
  Tcl_DoWhenIdle(DummyCmd, (ClientData) NULL);
}


/*---------------------------------------------------------*/
/* Performance Monitoring routines */
/*---------------------------------------------------------*/
double relative_elapsed_time(); 
double refresh_rate = 2.0; /* in seconds */
#ifdef HAVE_GETHRTIME
hrtime_t perf_start_time;
hrtime_t last_refresh;
hrtime_t get_time();
#else
#ifdef BSD_TIMES
time_t perf_start_time;
time_t last_refresh;
time_t get_time();
#else
clock_t perf_start_time;
clock_t last_refresh;
clock_t get_time();
#endif
#endif

/*ARGSUSED*/
/*void Update_refreshrateCmd(clientdata)
ClientData clientdata; */


int update_perf_window()
{
  char buf[200];
  double cur_time;

  if(perfon)
    {
      if (relative_elapsed_time(last_refresh) > refresh_rate)
	{
	  if(snapshot_display)
	    {
	      get_worker_stats(0);
	      perf_start_time = last_refresh;
	    }
	  else
	    get_worker_stats(2);
	  
	  cur_time = relative_elapsed_time(perf_start_time);
	  sprintf(buf,"%5.2f",relative_elapsed_time(perf_start_time));
	  Tcl_SetVar(interp, "cur_time", buf, TCL_GLOBAL_ONLY);
	  update_tk_perf_values();
	  tk_geval("remake_perf_canvas");
	  last_refresh = get_time();
	}
    }
}

/*ARGSUSED*/
int Set_refresh_rateCmd(clientdata, tkinterp, argc, argv)
ClientData clientdata;
Tcl_Interp *tkinterp;
int argc;
char *argv[];

{
  refresh_rate = atof(argv[1]);
  return(TCL_OK);
}

/*ARGSUSED*/
int Reset_wstatCmd(clientdata, tkinterp, argc, argv)
ClientData clientdata;
Tcl_Interp *tkinterp;
int argc;
char *argv[];

{
  perfon = 1;
  get_worker_stats(1);
  init_tk_perf_values();
  last_refresh = perf_start_time = get_time();
  return(TCL_OK);
}

int init_tk_perf_values()
{
  int i;
  worker_ptr cur;
  char buf[250];

  for(i = 0; i < mc_list.num_machines; i++)
    for(cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
      {
	sprintf(buf,"set worker_stat(%d,scheduling) 0.0",cur->index);
	tk_geval(buf);
	sprintf(buf,"set worker_stat(%d,idling) 0.0",cur->index);
	tk_geval(buf);
	sprintf(buf,"set worker_stat(%d,copying) 0.0",cur->index);
	tk_geval(buf);
	sprintf(buf,"set worker_stat(%d,recomputing) 0.0",cur->index);
	tk_geval(buf);
	sprintf(buf,"set worker_stat(%d,working) 0.0",cur->index);
	tk_geval(buf);
	sprintf(buf,"set worker_stat(%d,user_cpu) 0.0",cur->index);
	tk_geval(buf);
	sprintf(buf,"set worker_stat(%d,system_cpu) 0.0",cur->index);
	tk_geval(buf);
	sprintf(buf,"set worker_stat(%d,other) 0.0", cur->index);
	tk_geval(buf);
	sprintf(buf,"set worker_stat(%d,low_res_elapsed) 0.0",cur->index);
	tk_geval(buf);
	sprintf(buf,"set worker_machine(%d) %s",cur->index,
		mc_list.machines[i].hostname);
	tk_geval(buf);
      }
  Tcl_SetVar(interp, "cur_time", "0.0", TCL_GLOBAL_ONLY);
}

int update_tk_perf_values()
{
  int i;
  worker_ptr cur;
  char buf[250];
  double sch_time, copy_time, work_time, idle_time, elapsed_time,
  recomp_time;
  double user_cpu_time, system_cpu_time, low_res_elapsed_time, other;

  for(i = 0; i < mc_list.num_machines; i++)
    for(cur = mc_list.machines[i].list; cur != NULL; cur = cur->next)
      {
	sch_time = 
	  ((cur->cur_wstat.cumulated_event_time[SCHEDULING] - 
	    cur->start_wstat.cumulated_event_time[SCHEDULING]) +
	  (cur->cur_wstat.cumulated_event_time[SCH_CUT] - 
	    cur->start_wstat.cumulated_event_time[SCH_CUT]) +
 	  (cur->cur_wstat.cumulated_event_time[SCH_BACKTRACK] - 
	    cur->start_wstat.cumulated_event_time[SCH_BACKTRACK]) +
	  (cur->cur_wstat.cumulated_event_time[SCH_ASYNC] - 
	    cur->start_wstat.cumulated_event_time[SCH_ASYNC]))
	    / 1000.0;
	copy_time = 
	  ((cur->cur_wstat.cumulated_event_time[COPY_TO] - 
	    cur->start_wstat.cumulated_event_time[COPY_TO]) +
	  (cur->cur_wstat.cumulated_event_time[COPY_FROM] - 
	    cur->start_wstat.cumulated_event_time[COPY_FROM])) / 1000.0;
	recomp_time = 
	  (cur->cur_wstat.cumulated_event_time[ENG_RECOMPUTING] - 
	    cur->start_wstat.cumulated_event_time[ENG_RECOMPUTING]) / 1000.0;
	work_time = 
	  (cur->cur_wstat.cumulated_event_time[WORKING] - 
	    cur->start_wstat.cumulated_event_time[WORKING]) / 1000.0;
	idle_time = 
	  (cur->cur_wstat.cumulated_event_time[IDLE] - 
	    cur->start_wstat.cumulated_event_time[IDLE])/1000.0;

	elapsed_time = 
	  (sch_time + copy_time + work_time + idle_time + recomp_time);

	if(elapsed_time > 0.0)
	  {
	    sprintf(buf,"set worker_stat(%d,scheduling) %f",
		    cur->index, sch_time / elapsed_time * 100.0);
	    tk_geval(buf);
	    sprintf(buf,"set worker_stat(%d,idling) %f",
		    cur->index, idle_time / elapsed_time * 100.0);
	    tk_geval(buf);
	    sprintf(buf,"set worker_stat(%d,working) %f",
		    cur->index, work_time / elapsed_time * 100.0);
	    tk_geval(buf);
	    sprintf(buf,"set worker_stat(%d,copying) %f",
		    cur->index, copy_time / elapsed_time * 100.0);
	    tk_geval(buf);
	    sprintf(buf,"set worker_stat(%d,recomputing) %f",
		    cur->index, recomp_time / elapsed_time * 100.0);
	    tk_geval(buf);
	  }
	/* User, system and elapsed time (obtained without high-res clock */
	user_cpu_time = 
	  (cur->cur_wstat.times[0] - cur->start_wstat.times[0]);
	system_cpu_time = 
	  (cur->cur_wstat.times[1] - cur->start_wstat.times[1]);
	low_res_elapsed_time = 
	  (cur->cur_wstat.times[2] - cur->start_wstat.times[2]);
	other = low_res_elapsed_time - (user_cpu_time + system_cpu_time);
	if(low_res_elapsed_time > 0.0)
	  {
	    sprintf(buf,"set worker_stat(%d,user_cpu) %f",
		    cur->index, user_cpu_time/low_res_elapsed_time * 100.0);
	    tk_geval(buf);
	    sprintf(buf,"set worker_stat(%d,system_cpu) %f",
		    cur->index, system_cpu_time/low_res_elapsed_time * 100.0);
	    tk_geval(buf);
	    sprintf(buf,"set worker_stat(%d,other) %f",
		    cur->index, other/low_res_elapsed_time * 100.0);
	    tk_geval(buf);
	    sprintf(buf,"set worker_stat(%d,low_res_elapsed) %f",
		    cur->index, low_res_elapsed_time);
	    tk_geval(buf);
	  }
      }
}
#endif /* HAVE_TK */
