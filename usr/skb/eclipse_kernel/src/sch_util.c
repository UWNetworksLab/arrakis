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
**      System: Parallel ECLiPSe Scheduler
**        File: sch_util.c
**      Author: Liang-Liang Li
** Description: Utilities for Prolog manipulations of the scheduler data 
**		structures.
***********************************************************************/

#include <stdio.h>

#include "config.h"
#include "sepia.h"
#include <pds.h> 
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"

/* The scheduler tree definitions and macros */
#define ST_HANDLE_DS_DEFINED 1

#include "sch_types.h"
#include "sch_macros.h"

extern char * smsg_name[];
extern void sch_wake_eng();
extern void sch_idle_eng();

/*
** mapping keys to their scheduler setting instructions.
*/

#define SCHSET_ROOT2BOTTOM	0x00010000	/* root_first */
#define SCHSET_BOTTOM2ROOT	0x00020000	/* bottom_first */
#define SCHSET_LEFT2RIGHT	0x00040000	/* left_first */
#define SCHSET_RIGHT2LEFT	0x00080000	/* right_first */
#define SCHSET_MAX2PUBLISH	0x00100000	/* max_to_publish */
#define SCHSET_RESETCOUNTS	0x00200000	/* msg_counts */
#define SCHSET_IDLING		0x00400000	/* idle_eng */
#define SCHSET_WAKING		0x00800000	/* wake_eng */
#define SCHSET_ASYNC_HDL	0x01000000	/* async_hdl */
#define SCHSET_SYNC_HDL		0x02000000	/* sync_hdl */
#define SCHSET_LOAD_REPORT_E	0x10000000	/* load_report_eager */
#define SCHSET_LOAD_REPORT_L	0x20000000	/* load_report_lazy */
#define SCHSET_MAXVALUE		0x000fffff	/* max_value */
/* 
** int p_sch_set(Int: wid, List: instructions)
** int p_sch_get(Int: wid, Var/List: info)
*/

int p_sch_set(v_wid, t_wid, v_inst, t_inst)
value  v_wid;
type   t_wid;
value v_inst;
type  t_inst;
{
   char *key;
   int instruction = 0;
   pword  *cur_mc, *tmp, *cur_tail;
   pword first;

   Check_Integer(t_wid);

   first.tag = t_inst;
   first.val = v_inst;
   cur_mc = &first;

   while (IsList(cur_mc->tag)) {
      cur_tail = cur_mc->val.ptr + 1;
      cur_mc = cur_mc->val.ptr;
      Dereference_(cur_mc);
      if (IsList(cur_mc->tag)) {
         tmp = cur_mc->val.ptr;
	 Dereference_(tmp);
	 Get_Name(tmp->val,tmp->tag,key);
	 if (strcmp(key,"max_to_publish") == 0) {
	    tmp = cur_mc->val.ptr+1;
	    Dereference_(tmp);
	    if (IsList(tmp->tag)) {
	       tmp = tmp->val.ptr;
	       Dereference_(tmp);
	       if (IsInteger(tmp->tag)) {
	          instruction |= SCHSET_MAX2PUBLISH;
	          instruction |= SCHSET_MAXVALUE & tmp->val.nint;	
	       }
	    }
	 }
      } else {
	 Get_Name(cur_mc->val, cur_mc->tag, key);
         if (strcmp(key,"root_first") == 0)
	    instruction |= SCHSET_ROOT2BOTTOM;
	 else if (strcmp(key,"bottom_first") == 0)
	    instruction |= SCHSET_BOTTOM2ROOT;
	 else if (strcmp(key,"left_first") == 0)
	    instruction |= SCHSET_LEFT2RIGHT;
	 else if (strcmp(key,"right_first") == 0)
	    instruction |= SCHSET_RIGHT2LEFT;
	 else if (strcmp(key,"reset_msg_counts") == 0)
	    instruction |= SCHSET_RESETCOUNTS;
	 else if (strcmp(key,"idle_eng") == 0)
	    instruction |= SCHSET_IDLING;
	 else if (strcmp(key,"wake_eng") == 0)
	    instruction |= SCHSET_WAKING;
	 else if (strcmp(key,"async_hdl") == 0)
	    instruction |= SCHSET_ASYNC_HDL;
	 else if (strcmp(key,"sync_hdl") == 0)
	    instruction |= SCHSET_SYNC_HDL;
	 else if (strcmp(key,"load_report_eager") == 0)
	    instruction |= SCHSET_LOAD_REPORT_E;
	 else if (strcmp(key,"load_report_lazy") == 0)
	    instruction |= SCHSET_LOAD_REPORT_L;
      }
      cur_mc = cur_tail;
   }
   Check_Nil(cur_mc->tag);

   if (instruction) {
      void wm_set_worker_info();
      wm_set_worker_info((int) v_wid.nint, 1, sizeof(int), (void_ptr) &instruction);
   }
   Succeed_;
}

int p_sch_get(v_wid, t_wid, v_info, t_info)
value  v_wid;
type   t_wid;
value v_info;
type  t_info;
{
   /*
   ** The info output is [States, Statistics].
   **
   ** Transform the scheduler info into a plain prolog list INFO:
   **   struct scheduler {
   **	    aport_id_t port;			## States ##
   **	    struct st_handle_ds leaf;
   **	    eng_handle_t engine;
   **	    struct susp_buffer_ds susp_buffer;
   **	    struct edge_buffer_ds edge_buffer;
   **	    int max_to_publish;
   **	    unsigned root_first:1;
   **	    unsigned left_first:1;
   **	    unsigned idling    :1;
   **	    unsigned waking    :1;
   **	    unsigned sync_hdl  :1;
   **	    unsigned async_hdl  :1;
   **	    unsigned lmp       :1;
   **	    unsigned load_report_eager:1;
   **	    int state_donate;			## Statistics ##
   **	    int smsg_count_hdl;
   **	    int smsg_count_snd;
   **	    int smsg_count_intra;
   **	    int smsg_count_intra_shortcut;
   **	    int smsg_subcount[40+1];
   **	};
   */
   scheduler_t scheduler_data[1];
   pword  *cur_mc, *prev_mc, *cur_tail, *cur_head, *statistics;
   int i, bufsize = sizeof(scheduler_t);
   extern void wm_get_worker_info();
   Check_Integer(t_wid);
   wm_get_worker_info((int) v_wid.nint, 1, bufsize, (void_ptr) scheduler_data);

   prev_mc = NULL;
   /* smsg_subcount */
   /* [[n1,v1],[n2,v2] ...] */
   for (i = get_smsg_max()-1; i>0; i--) {
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,smsg_name[i])
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->smsg_subcount[i])
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   if (prev_mc == NULL)
      Make_Nil(cur_tail)
   else {
      Make_List(cur_tail, prev_mc);
   }
   prev_mc = cur_mc;
   }
   /* [smsg_count_intra_shortcut, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"smsg_count_intra_shortcut")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->smsg_count_intra_shortcut)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [smsg_count_intra, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"smsg_count_intra")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->smsg_count_intra)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [smsg_count_snd,v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"smsg_count_snd")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->smsg_count_snd)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [smsg_count_hdl, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"smsg_count_hdl")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->smsg_count_hdl)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [state_donate, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"state_donate")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->state_donate)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   statistics = cur_mc;

   /* [lmp, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"lmp")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->lmp)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_Nil(cur_tail);
   prev_mc = cur_mc;

   /* [async_hdl, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"async_hdl")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->async_hdl)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [load_report_eager, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"load_report_eager")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->load_report_eager)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [waking, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"waking")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->waking)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [idling, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"idling")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->idling)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [left_first, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"left_first")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->left_first)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [root_first, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"root_first")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->root_first)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [max_to_publish, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"max_to_publish")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->max_to_publish)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [edge_buffer, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"edge_buffer")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->edge_buffer.count)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [susp_buffer, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"susp_buffer")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->susp_buffer.count)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [engine, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"engine")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->engine)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [leaf, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"leaf")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head, scheduler_data->leaf.site)
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head, scheduler_data->leaf.edge)
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head, scheduler_data->leaf.knot)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* [port, v] */
   cur_mc = TG;
   Push_List_Frame()
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_mc,cur_head);
   Make_String(cur_head,"port")
   cur_tail = cur_head + 1;
   cur_head = TG;
   Push_List_Frame()
   Make_List(cur_tail,cur_head);
   Make_Integer(cur_head,scheduler_data->port)
   cur_tail = cur_head + 1;
   Make_Nil(cur_tail)
   cur_tail = cur_mc + 1;
   Make_List(cur_tail, prev_mc);
   prev_mc = cur_mc;

   /* pack states and statistics together */
   cur_mc = TG;
   Push_List_Frame()
   Make_List(cur_mc, prev_mc);
   cur_head = TG;
   Push_List_Frame()
   cur_tail = cur_mc + 1;
   Make_List(cur_tail,cur_head);
   Make_List(cur_head, statistics);
   cur_tail = cur_head+1;
   Make_Nil(cur_tail)

   Return_Unify_List(v_info, t_info, cur_mc)
}


void sch_get_info(site,infosize,infoval)
aport_id_t site;
int * infosize;
void_ptr * infoval;
{
   scheduler_t * site_scheduler();
   *infoval = (void_ptr) site_scheduler(site);
   *infosize = sizeof(scheduler_t);
   return;
}

void sch_set_info(site,infoval)
aport_id_t site;
void_ptr infoval;
{
  scheduler_t * site_scheduler();
  scheduler_t *s = (scheduler_t *) site_scheduler(site);
  int i, instruction = * (int *) infoval;

  if (instruction&SCHSET_ROOT2BOTTOM)
     s->root_first = 1;
  if (instruction&SCHSET_BOTTOM2ROOT)
     s->root_first = 0;
  if (instruction&SCHSET_LEFT2RIGHT)
     s->left_first = 1;
  if (instruction&SCHSET_RIGHT2LEFT)
     s->left_first = 0;
  if (instruction&SCHSET_MAX2PUBLISH)
     s->max_to_publish = instruction&SCHSET_MAXVALUE;
  if (instruction&SCHSET_RESETCOUNTS) {
     s->state_donate = 0;
     s->smsg_count_hdl = 0;
     s->smsg_count_snd = 0;
     s->smsg_count_intra = 0;
     s->smsg_count_intra_shortcut = 0;
     for (i = get_smsg_max()-1; i>0; i--)
        s->smsg_subcount[i] = 0;
  }
  if (instruction&SCHSET_ASYNC_HDL)
     s->async_hdl = 1;
  if (instruction&SCHSET_SYNC_HDL)
     s->async_hdl = 0;
  if (instruction&SCHSET_IDLING)
     sch_idle_eng(site);
  if (instruction&SCHSET_WAKING)
     sch_wake_eng(site);
  if (instruction&SCHSET_LOAD_REPORT_E)
     s->load_report_eager = 1;
  if (instruction&SCHSET_LOAD_REPORT_L)
     s->load_report_eager = 0;
  return;
}

/* int p_schtrace_on() */

int p_sch_trace_on()
{
   (void) global_flags(0,SCH_TRACE_FLAG);
   Sch_Trace_Begin();
   Succeed_;
}
