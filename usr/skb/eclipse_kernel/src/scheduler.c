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
**        File: scheduler.c
**      Author: Liang-Liang Li
** Description:
**
***********************************************************************/

#include <stdio.h>

#include "config.h"

#include "pds.h"        /* The Underlying Message Passing System */
#include "memman.h"
#include "trace.h"      /* The tracing events and macros defintions */
#include "error.h"

#include "sch_types.h"  /* The scheduler tree definitions and macros */
#include "sch_eng_interface.h" /* interface: scheduler/engine/worker manage */
#include "sch_macros.h"

/*****  Scheduler sub-functions  *****/
/*************************************/
#if defined(__STDC__)
/* Send scheduler msgs of various sizes:
**	recv-id, msg-type, send-id
** is the smallest size.
** Smsg_sndIJ means the msg sent has extra I tree ids, and J integers.
** this grouping is subject to change when dealing with newly introduced msgs
*/

static void smsg_snd00(st_id_t *, int, st_id_t *);
static void smsg_snd01(st_id_t *, int, st_id_t *, int);
static void smsg_snd02(st_id_t *, int, st_id_t *, int, int);
static void smsg_snd10(st_id_t *, int, st_id_t *, st_id_t *);
static void smsg_snd12(st_id_t *, int, st_id_t *, st_id_t *, int,int);
static void smsg_snd20(st_id_t *, int, st_id_t *, st_id_t *, st_id_t *);
static void smsg_snd21(st_id_t *, int, st_id_t *, st_id_t *, st_id_t *, int);
static void smsg_snd30(st_id_t *, int, st_id_t *, st_id_t *, st_id_t *, st_id_t *);

/* handles scheduler msgs of various types */
static void sch_port_naive_upcall     (site_id_t);
static void sch_async_msg_hdls	       (site_id_t, int);

static void sch_msg_hdl_init_lodge    (st_id_t *, st_id_t *, int);
static void sch_msg_hdl_backtrack     (st_id_t *, st_id_t *);
static void sch_msg_hdl_withered      (st_id_t *, st_id_t *);
static void sch_msg_hdl_dec_corpse    (st_id_t *, st_id_t *);
static void sch_msg_hdl_js_in_vain    (st_id_t *, st_id_t *);
static void sch_msg_hdl_load_report   (st_id_t *, st_id_t *);
static void sch_msg_hdl_set_js_root   (st_id_t *, st_id_t *);
static void sch_msg_hdl_js_again      (st_id_t *, st_id_t *);
static void sch_msg_hdl_lodged        (st_id_t *, st_id_t *);
static void sch_msg_hdl_tell_idle     (st_id_t *, st_id_t *);
static void sch_msg_hdl_idle_told     (st_id_t *, st_id_t *);
static void sch_msg_hdl_idle_eng      (st_id_t *, st_id_t *);
static void sch_msg_hdl_wake_eng      (st_id_t *, st_id_t *);
static void sch_msg_hdl_stop_idle     (st_id_t *, st_id_t *);
static void sch_msg_hdl_lmp	      (st_id_t *, st_id_t *);
static void sch_msg_hdl_reduce_wk_up  (st_id_t *, st_id_t *);
static void sch_msg_hdl_reduce_wk_dn  (st_id_t *, st_id_t *);
static void sch_msg_hdl_engine_migrate(st_id_t *, st_id_t *);

static void sch_msg_hdl_cut_ok        (st_id_t *, st_id_t *, int);

static void sch_msg_hdl_js_success    (st_id_t *, st_id_t *, int, int);

static void sch_msg_hdl_cut           (st_id_t *, st_id_t *, st_id_t *);
static void sch_msg_hdl_chop          (st_id_t *, st_id_t *, st_id_t *);
static void sch_msg_hdl_lodge         (st_id_t *, st_id_t *, st_id_t *);
static void sch_msg_hdl_lodge_idle    (st_id_t *, st_id_t *, st_id_t *);
static void sch_msg_hdl_js_prologue   (st_id_t *, st_id_t *, st_id_t *);

static void sch_msg_hdl_js_trust      (st_id_t *, st_id_t *, st_id_t *, int, int);

static void sch_msg_hdl_js_trav_up    (st_id_t *, st_id_t *, st_id_t *, st_id_t *);
static void sch_msg_hdl_js_trav_dn    (st_id_t *, st_id_t *, st_id_t *, st_id_t *);
static void sch_msg_hdl_js_install_fl (st_id_t *, st_id_t *, st_id_t *, st_id_t *);

static void sch_msg_hdl_straighten    (st_id_t *, st_id_t *, st_id_t *, st_id_t *, int);

static void sch_msg_hdl_js_install    (st_id_t *, st_id_t *, st_id_t *, st_id_t *, st_id_t *);

/* others */
#if defined(MULTI_THREADS)
int simp_lock(char *);
void simp_unlock(char *);
#endif /* MULTI_THREADS */

#else /* __STDC__ */

static void smsg_snd00();
static void smsg_snd01();
static void smsg_snd02();
static void smsg_snd10();
static void smsg_snd12();
static void smsg_snd20();
static void smsg_snd21();
static void smsg_snd30();

static void sch_port_naive_upcall     ();
static void sch_async_msg_hdls	       ();

static void sch_msg_hdl_init_lodge    ();
static void sch_msg_hdl_backtrack     ();
static void sch_msg_hdl_withered      ();
static void sch_msg_hdl_dec_corpse    ();
static void sch_msg_hdl_js_in_vain    ();
static void sch_msg_hdl_load_report   ();
static void sch_msg_hdl_set_js_root   ();

static void sch_msg_hdl_cut_ok        ();
static void sch_msg_hdl_js_again      ();
static void sch_msg_hdl_lodged        ();
static void sch_msg_hdl_tell_idle     ();
static void sch_msg_hdl_idle_told     ();
static void sch_msg_hdl_idle_eng      ();
static void sch_msg_hdl_wake_eng      ();
static void sch_msg_hdl_stop_idle     ();
static void sch_msg_hdl_lmp	      ();
static void sch_msg_hdl_reduce_wk_up  ();
static void sch_msg_hdl_reduce_wk_dn  ();
static void sch_msg_hdl_engine_migrate();

static void sch_msg_hdl_js_success    ();

static void sch_msg_hdl_cut           ();
static void sch_msg_hdl_chop 	       ();
static void sch_msg_hdl_lodge         ();
static void sch_msg_hdl_lodge_idle    ();
static void sch_msg_hdl_js_prologue   ();

static void sch_msg_hdl_js_trust      ();

static void sch_msg_hdl_js_trav_up    ();
static void sch_msg_hdl_js_trav_dn    ();
static void sch_msg_hdl_js_install_fl ();

static void sch_msg_hdl_straighten    ();

static void sch_msg_hdl_js_install    ();

#if defined(MULTI_THREADS)
int  simp_lock		       ();
void simp_unlock	       ();
#endif /* MULTI_THREADS */

#endif /* __STDC__ */

int zero();

/*****  Scheduler Messages Among Nodes/Leaves         *****/
/**********************************************************/

/* Scheduler Message Listing */
#define SMSG_INIT_LODGE       0       /* root, leaf, wm_port */
#define SMSG_BACKTRACK        1       /* parent, leaf */
#define SMSG_STRAIGHTEN       2       /* parent, child, leaf, parent0, nxtcls */
#define SMSG_CUT              3       /* (a) parent, leaf */
#define SMSG_CUT_OK           4       /* leaf, parent, info */
#define SMSG_CHOP             5       /* child, parent, ancestor */
#define SMSG_WITHERED         6       /* parent, child */
#define SMSG_LODGE            7       /* (a) parent, leaf */
#define SMSG_LODGED           8       /* leaf, parent */
#define SMSG_DEC_CORPSE       9       /* oldleaf, newleaf */
#define SMSG_JS_PROLOGUE      10      /* parent, child, leaf */
#define SMSG_JS_TRAV_UP       11      /* parent, child, leaf, coma */
#define SMSG_JS_TRAV_DN       12      /* child, parent, leaf, coma */
#define SMSG_JS_INSTALL       13      /* child, parent, leaf, coma */
#define SMSG_JS_INSTALL_FL    14      /* lodge,child,coma,leaf */
#define SMSG_JS_SUCCESS       15      /* leaf, parent, nxtcls, info */
#define SMSG_JS_TRUST         16      /* leaf, parent, parent0, nxtcls, info */
#define SMSG_JS_IN_VAIN       17      /* leaf, jsroot */
#define SMSG_JS_AGAIN         18      /* parent, leaf */
#define SMSG_LOAD_REPORT      19      /* parent, child */
#define SMSG_SET_JS_ROOT      20      /* child, parent */
#define SMSG_WAKE_ENG         21      /* leaf, leaf */
#define SMSG_IDLE_ENG         22      /* leaf, leaf */
#define SMSG_TELL_IDLE        23      /* parent, leaf */
#define SMSG_LODGE_IDLE       24      /* parent, child, leaf */
#define SMSG_IDLE_TOLD        25      /* leaf, parent */
#define SMSG_STOP_IDLE        26      /* parent, leaf */
#define SMSG_LMP              27      /* child, parent */
#define SMSG_REDUCE_WK_UP     28      /* parent, child */
#define SMSG_REDUCE_WK_DN     29      /* child, parent */
#define SMSG_ENGINE_MIGRATE   30      /* parent, leaf */
#define SMSG_MAXNUM           31      /* last item */

/* Comments 
   1. The argument lists for the handlers of messages are listed as comment.
   2. Some messages are sent upwards the scheduler tree:
	parent (or ancestors as future parent) as 1st argument 
   3. Other messages are sent downwards the scheduler tree:
	 child or leaf as 1st argument.
   4. A leaf can receive any of downward messages.
      (of course, a leaf with specific state may not expect all of them).
   5. All of the arguments are pointers to a sch-tree identifiers, with
      exceptions: SMSG_JS_SUCCESS needs a next-clause argument,
   6. coma -> common ancestor
*/

/* Brief explanations for some of the messages :
   SMSG_STRAIGHTEN: turns a single threaded subtree into a branch;
   SMSG_CHOP: chops a subtree
   SMSG_DEC_CORPSE: decouple the old leaf and its reincarnated new leaf
   SMSG_JS_PROLOGUE: job-search along the lodging path
   SMSG_JS_TRAV_UP: job-request from a just searched subtree
   SMSG_JS_INSTALL: asked to install job-state
   SMSG_JS_INSTALL_FL: fail acknowledgement of installation
   SMSG_JS_IN_VAIN: nothing is found in the current job-search tree
   SMSG_JS_AGAIN: start a new session
   SMSG_JS_TRUST: coupled with SMSG_STRAIGHTEN (reply)
   SMSG_LOAD_REPORT: report richness of a subtree 
   SMSG_SET_JS_ROOT: set a smallest tree for job search
   SMSG_IDLE_ENG: instruct the leaf to let the engine idle
   SMSG_WAKE_ENG: instruct the leaf to wake up the engine
   SMSG_LMP: instruct the left-most path to become LM set 
   SMSG_REDUCE_WK_UP:
   SMSG_REDUCE_WK_DN: wake up suspended job-search message flows.
   SMSG_ENGINE_MIGRATE: the leaf with the engine just copied oracled wants
		        migrate to the new parent
*/

char * smsg_name[SMSG_MAXNUM+1];

static scheduler_t scheduler[1];
static st_knot_t knot_template[1];

/*****  Data Blocks Packing Messages of various sizes  *****/
/***********************************************************/

/* smsg_xy_t: a message with x stree ids and y integers */

typedef struct {
	struct st_id_ds recv;
	pds_int32    type;
	struct st_id_ds send;
} smsg00_t;

typedef struct {
	struct st_id_ds recv;
	pds_int32    type;
	struct st_id_ds send;
	int i1;
} smsg01_t;

typedef struct {
	struct st_id_ds recv;
	pds_int32    type;
	struct st_id_ds send;
	pds_int32 i1;
	pds_int32 i2;
} smsg02_t;

typedef struct {
	struct st_id_ds recv;
	pds_int32    type;
	struct st_id_ds send;
	struct st_id_ds t1;
} smsg10_t;

typedef struct {
	struct st_id_ds recv;
	pds_int32    type;
	struct st_id_ds send;
	struct st_id_ds t1;
	pds_int32 i1;
	pds_int32 i2;
} smsg12_t;

typedef struct {
	struct st_id_ds recv;
	pds_int32    type;
	struct st_id_ds send;
	struct st_id_ds t1;
	struct st_id_ds t2;
} smsg20_t;

typedef struct {
	struct st_id_ds recv;
	pds_int32    type;
	struct st_id_ds send;
	struct st_id_ds t1;
	struct st_id_ds t2;
	pds_int32 i1;
} smsg21_t;

typedef struct {
	struct st_id_ds recv;
	pds_int32    type;
	struct st_id_ds send;
	struct st_id_ds t1;
	struct st_id_ds t2;
	struct st_id_ds t3;
} smsg30_t;

#define ECLIPSE_SCH_INTFCNO	1217

void smsg_type_init(site)
site_id_t site;
{
  amsg_typedef_t mdt_smsg_type[10];
  amsg_type_t mdt_st_id;

  mdt_smsg_type[0] = MDT_BEGIN;
  mdt_smsg_type[1] = MDT_STRUCT_OPEN;
  mdt_smsg_type[2] = MDT_APORTID;
  mdt_smsg_type[3] = MDT_UINT32;
  mdt_smsg_type[4] = MDT_UINT32;
  mdt_smsg_type[5] = MDT_STRUCT_CLOSE;
  mdt_smsg_type[6] = MDT_END;
  if (amsg_type_define(ECLIPSE_SCH_INTFCNO, 10, mdt_smsg_type, &mdt_st_id)
	!= AMSG_OK) {
     error("fail to define st_id type");
  }  

  mdt_smsg_type[0] = MDT_BEGIN;
  mdt_smsg_type[1] = MDT_STRUCT_OPEN;
  mdt_smsg_type[2] = mdt_st_id;
  mdt_smsg_type[3] = MDT_INT32;
  mdt_smsg_type[4] = mdt_st_id;
  mdt_smsg_type[5] = MDT_STRUCT_CLOSE;
  mdt_smsg_type[6] = MDT_END;
  if (amsg_type_define(
       ECLIPSE_SCH_INTFCNO, 11, mdt_smsg_type, &Scheduler(site)->smsg_type[0][0]
                      ) != AMSG_OK) {
     error("fail to define smsg00 type");
  }  

  mdt_smsg_type[0] = MDT_BEGIN;
  mdt_smsg_type[1] = MDT_STRUCT_OPEN;
  mdt_smsg_type[2] = mdt_st_id;
  mdt_smsg_type[3] = MDT_INT32;
  mdt_smsg_type[4] = mdt_st_id;
  mdt_smsg_type[5] = MDT_INT32;
  mdt_smsg_type[6] = MDT_STRUCT_CLOSE;
  mdt_smsg_type[7] = MDT_END;
  if (amsg_type_define(
       ECLIPSE_SCH_INTFCNO, 12, mdt_smsg_type, &Scheduler(site)->smsg_type[0][1]
		      ) != AMSG_OK) {
     error("fail to define smsg01 type");
  }  

  mdt_smsg_type[0] = MDT_BEGIN;
  mdt_smsg_type[1] = MDT_STRUCT_OPEN;
  mdt_smsg_type[2] = mdt_st_id;
  mdt_smsg_type[3] = MDT_INT32;
  mdt_smsg_type[4] = mdt_st_id;
  mdt_smsg_type[5] = MDT_INT32;
  mdt_smsg_type[6] = MDT_INT32;
  mdt_smsg_type[7] = MDT_STRUCT_CLOSE;
  mdt_smsg_type[8] = MDT_END;
  if (amsg_type_define(
       ECLIPSE_SCH_INTFCNO, 13, mdt_smsg_type, &Scheduler(site)->smsg_type[0][2]
		      ) != AMSG_OK) {
     error("fail to define smsg02 type");
  }  

  mdt_smsg_type[0] = MDT_BEGIN;
  mdt_smsg_type[1] = MDT_STRUCT_OPEN;
  mdt_smsg_type[2] = mdt_st_id;
  mdt_smsg_type[3] = MDT_INT32;
  mdt_smsg_type[4] = mdt_st_id;
  mdt_smsg_type[5] = mdt_st_id;
  mdt_smsg_type[6] = MDT_STRUCT_CLOSE;
  mdt_smsg_type[7] = MDT_END;
  if (amsg_type_define(
       ECLIPSE_SCH_INTFCNO, 14, mdt_smsg_type, &Scheduler(site)->smsg_type[1][0]
		      ) != AMSG_OK) {
     error("fail to define smsg10 type");
  }  
 
  mdt_smsg_type[0] = MDT_BEGIN;
  mdt_smsg_type[1] = MDT_STRUCT_OPEN;
  mdt_smsg_type[2] = mdt_st_id;
  mdt_smsg_type[3] = MDT_INT32;
  mdt_smsg_type[4] = mdt_st_id;
  mdt_smsg_type[5] = mdt_st_id;
  mdt_smsg_type[6] = MDT_INT32;
  mdt_smsg_type[7] = MDT_INT32;
  mdt_smsg_type[8] = MDT_STRUCT_CLOSE;
  mdt_smsg_type[9] = MDT_END;
  if (amsg_type_define(
       ECLIPSE_SCH_INTFCNO, 15, mdt_smsg_type, &Scheduler(site)->smsg_type[1][2]
		      ) != AMSG_OK) {
     error("fail to define smsg12 type");
  }  
 
  mdt_smsg_type[0] = MDT_BEGIN;
  mdt_smsg_type[1] = MDT_STRUCT_OPEN;
  mdt_smsg_type[2] = mdt_st_id;
  mdt_smsg_type[3] = MDT_INT32;
  mdt_smsg_type[4] = mdt_st_id;
  mdt_smsg_type[5] = mdt_st_id;
  mdt_smsg_type[6] = mdt_st_id;
  mdt_smsg_type[7] = MDT_STRUCT_CLOSE;
  mdt_smsg_type[8] = MDT_END;
  if (amsg_type_define(
       ECLIPSE_SCH_INTFCNO, 16, mdt_smsg_type, &Scheduler(site)->smsg_type[2][0]
		      ) != AMSG_OK) {
     error("fail to define smsg20 type");
  }  
 
  mdt_smsg_type[0] = MDT_BEGIN;
  mdt_smsg_type[1] = MDT_STRUCT_OPEN;
  mdt_smsg_type[2] = mdt_st_id;
  mdt_smsg_type[3] = MDT_INT32;
  mdt_smsg_type[4] = mdt_st_id;
  mdt_smsg_type[5] = mdt_st_id;
  mdt_smsg_type[6] = mdt_st_id;
  mdt_smsg_type[7] = MDT_INT32;
  mdt_smsg_type[8] = MDT_STRUCT_CLOSE;
  mdt_smsg_type[9] = MDT_END;
  if (amsg_type_define(
       ECLIPSE_SCH_INTFCNO, 17, mdt_smsg_type, &Scheduler(site)->smsg_type[2][1]
		      ) != AMSG_OK) {
     error("fail to define smsg21 type");
  }  
 
  mdt_smsg_type[0] = MDT_BEGIN;
  mdt_smsg_type[1] = MDT_STRUCT_OPEN;
  mdt_smsg_type[2] = mdt_st_id;
  mdt_smsg_type[3] = MDT_INT32;
  mdt_smsg_type[4] = mdt_st_id;
  mdt_smsg_type[5] = mdt_st_id;
  mdt_smsg_type[6] = mdt_st_id;
  mdt_smsg_type[7] = mdt_st_id;
  mdt_smsg_type[8] = MDT_STRUCT_CLOSE;
  mdt_smsg_type[9] = MDT_END;
  if (amsg_type_define(
       ECLIPSE_SCH_INTFCNO, 18, mdt_smsg_type, &Scheduler(site)->smsg_type[3][0]
		      ) != AMSG_OK) {
     error("fail to define smsg30 type");
  }  
}

static void smsg_snd00(recv, type, send)
int type;
st_id_t *recv, *send;
{
   amsg_t      msg;
   amsg_ret_t  ret;
   amsg_size_t datasize = sizeof(smsg00_t);
   smsg00_t *data;
  
   Smsg_Snd_Notify(type, recv, send);
   ret = amsg_alloc(datasize, (amsg_data_t **)&data, &msg);
   assert(ret==AMSG_OK);
   data->type = type;
   data->recv = *recv;
   data->send = *send;
   IntraSiteCheckIn(Site(recv),Site(send));
   ret = amsg_send(
		Site(recv),
		msg,
		Scheduler(Site(send))->smsg_type[0][0],
		1,
		0);
   assert(ret==AMSG_OK);
   return;  
}

static void smsg_snd01(recv, type, send, i)
int type;
st_id_t *recv, *send;
int i;
{
   amsg_t      msg;
   amsg_ret_t  ret;
   amsg_size_t datasize = sizeof(smsg01_t);
   smsg01_t *data;
  
   Smsg_Snd_Notify(type, recv, send);
   ret = amsg_alloc(datasize, (amsg_data_t **)&data, &msg);
   assert(ret==AMSG_OK);
   data->type = type;
   data->recv = *recv;
   data->send = *send;
   data->i1   = i;
   IntraSiteCheckIn(Site(recv),Site(send));
   ret = amsg_send(
		Site(recv),
		msg,
		Scheduler(Site(send))->smsg_type[0][1],
		1,
		0);
   assert(ret==AMSG_OK);
   return;  
}

static void smsg_snd02(recv, type, send, i1,i2)
int type;
st_id_t *recv, *send;
int i1,i2;
{
   amsg_t      msg;
   amsg_ret_t  ret;
   amsg_size_t datasize = sizeof(smsg02_t);
   smsg02_t *data;
  
   Smsg_Snd_Notify(type, recv, send);
   ret = amsg_alloc(datasize, (amsg_data_t **)&data, &msg);
   assert(ret==AMSG_OK);
   data->type = type;
   data->recv = *recv;
   data->send = *send;
   data->i1   = i1;
   data->i2   = i2;
   IntraSiteCheckIn(Site(recv),Site(send));
   ret = amsg_send(
		Site(recv),
		msg,
		Scheduler(Site(send))->smsg_type[0][2],
		1,
		0);
   assert(ret==AMSG_OK);
   return;  
}

static void smsg_snd10(recv, type, send, t1)
int type;
st_id_t *recv, *send, *t1;
{
   amsg_t      msg;
   amsg_ret_t  ret;
   amsg_size_t datasize = sizeof(smsg10_t);
   smsg10_t *data;
  
   Smsg_Snd_Notify(type, recv, send);
   ret = amsg_alloc(datasize, (amsg_data_t **)&data, &msg);
   assert(ret==AMSG_OK);

   data->type = type;
   data->recv = *recv;
   data->send = *send;
   data->t1   = *t1;
   IntraSiteCheckIn(Site(recv),Site(send));
   ret = amsg_send(
		Site(recv),
		msg,
		Scheduler(Site(send))->smsg_type[1][0],
		1,
		0);
   assert(ret==AMSG_OK);
   return;  
}

static void smsg_snd12(recv, type, send, t1, i1,i2)
int type;
st_id_t *recv, *send, *t1;
int i1,i2;
{
   amsg_t      msg;
   amsg_ret_t  ret;
   amsg_size_t datasize = sizeof(smsg12_t);
   smsg12_t *data;
  
   Smsg_Snd_Notify(type, recv, send);
   ret = amsg_alloc(datasize, (amsg_data_t **)&data, &msg);
   assert(ret==AMSG_OK);
   data->type = type;
   data->recv = *recv;
   data->send = *send;
   data->t1   = *t1;
   data->i1   = i1;
   data->i2   = i2;
   IntraSiteCheckIn(Site(recv),Site(send));
   ret = amsg_send(
		Site(recv),
		msg,
		Scheduler(Site(send))->smsg_type[1][2],
		1,
		0);
   assert(ret==AMSG_OK);
   return;  
}

static void smsg_snd20(recv, type, send, t1, t2)
int type;
st_id_t *recv, *send, *t1, *t2;
{
   amsg_t      msg;
   amsg_ret_t  ret;
   amsg_size_t datasize = sizeof(smsg20_t);
   smsg20_t *data;
  
   Smsg_Snd_Notify(type, recv, send);
   ret = amsg_alloc(datasize, (amsg_data_t **)&data, &msg);
   assert(ret==AMSG_OK);

   data->type = type;
   data->recv = *recv;
   data->send = *send;
   data->t1   = *t1;
   data->t2   = *t2;
   IntraSiteCheckIn(Site(recv),Site(send));
   ret = amsg_send(
		Site(recv),
		msg,
		Scheduler(Site(send))->smsg_type[2][0],
		1,
		0);
   assert(ret==AMSG_OK);
   return;  
}

static void smsg_snd21(recv, type, send, t1, t2, i1)
int type;
st_id_t *recv, *send, *t1, *t2;
int i1;
{
   amsg_t      msg;
   amsg_ret_t  ret;
   amsg_size_t datasize = sizeof(smsg21_t);
   smsg21_t *data;
  
   Smsg_Snd_Notify(type, recv, send);
   ret = amsg_alloc(datasize, (amsg_data_t **)&data, &msg);
   assert(ret==AMSG_OK);

   data->type = type;
   data->recv = *recv;
   data->send = *send;
   data->t1   = *t1;
   data->t2   = *t2;
   data->i1   = i1;
   IntraSiteCheckIn(Site(recv),Site(send));
   ret = amsg_send(
		Site(recv),
		msg,
		Scheduler(Site(send))->smsg_type[2][1],
		1,
		0);
   assert(ret==AMSG_OK);
   return;  
}

static void smsg_snd30(recv, type, send, t1, t2, t3)
int type;
st_id_t *recv, *send, *t1, *t2, *t3;
{
   amsg_t      msg;
   amsg_ret_t  ret;
   amsg_size_t datasize = sizeof(smsg30_t);
   smsg30_t *data;
 
   Smsg_Snd_Notify(type, recv, send);
   ret = amsg_alloc(datasize, (amsg_data_t **)&data, &msg);
   assert(ret==AMSG_OK);

   data->type = type;
   data->recv = *recv;
   data->send = *send;
   data->t1   = *t1;
   data->t2   = *t2;
   data->t3   = *t3;
   IntraSiteCheckIn(Site(recv),Site(send));
   ret = amsg_send(
		Site(recv),
		msg,
		Scheduler(Site(send))->smsg_type[3][0],
		1,
		0);
   assert(ret==AMSG_OK);
   return;
}
#define sch_msg_snd_backtrack(parent, leaf)	\
		smsg_snd00((parent),SMSG_BACKTRACK,(leaf))
#define sch_msg_snd_lodged(leaf,parent)		\
		smsg_snd00((leaf),SMSG_LODGED,(parent))
#define sch_msg_snd_withered(recv,child)		\
		smsg_snd00((recv),SMSG_WITHERED,(child))
#define sch_msg_snd_dec_corpse(old,new)		\
		smsg_snd00((old),SMSG_DEC_CORPSE,(new))
#define sch_msg_snd_js_in_vain(leaf,jroot)	\
		smsg_snd00((leaf),SMSG_JS_IN_VAIN,(jroot))
#define sch_msg_snd_load_report(parent,child)	\
		smsg_snd00((parent),SMSG_LOAD_REPORT,(child))
#define sch_msg_snd_set_js_root(child,parent)	\
		smsg_snd00((child),SMSG_SET_JS_ROOT,(parent))
#define sch_msg_snd_js_again(parent,leaf)	\
		smsg_snd00((parent),SMSG_JS_AGAIN,(leaf))
#define sch_msg_snd_idle_eng(leaf,leaf0)	\
		smsg_snd00((leaf),SMSG_IDLE_ENG,(leaf0))
#define sch_msg_snd_wake_eng(leaf,leaf0)	\
		smsg_snd00((leaf),SMSG_WAKE_ENG,(leaf0))
#define sch_msg_snd_idle_told(leaf,parent)	\
		smsg_snd00((leaf),SMSG_IDLE_TOLD,(parent))
#define sch_msg_snd_lmp(child,parent)	\
		smsg_snd00((child),SMSG_LMP,(parent))
#define sch_msg_snd_tell_idle(parent,leaf)	\
		smsg_snd00((parent),SMSG_TELL_IDLE,(leaf))
#define sch_msg_snd_stop_idle(parent,leaf)	\
		smsg_snd00((parent),SMSG_STOP_IDLE,(leaf))
#define sch_msg_snd_reduce_wk_up(parent,child)	\
		smsg_snd00((parent),SMSG_REDUCE_WK_UP,(child))
#define sch_msg_snd_reduce_wk_dn(child, parent)	\
		smsg_snd00((child),SMSG_REDUCE_WK_DN,(parent))
#define sch_msg_snd_engine_migrate(parent,leaf) \
		smsg_snd00((parent),SMSG_ENGINE_MIGRATE,(leaf))

#define sch_msg_snd_cut_ok(leaf,parent,info)		\
		smsg_snd01((leaf),SMSG_CUT_OK,(parent),(info))
#define sch_msg_snd_init_lodge(root, leaf, wm)	\
		smsg_snd01((root),SMSG_INIT_LODGE,(leaf),(wm))

#define sch_msg_snd_js_success(leaf,parent,nxtcls,info)	\
		smsg_snd02((leaf),SMSG_JS_SUCCESS,(parent),(nxtcls),(info))

#define sch_msg_snd_cut(parent, olf, leaf)		\
		smsg_snd10((parent),SMSG_CUT,(olf),(leaf))
#define sch_msg_snd_lodge(parent,child,leaf)		\
		smsg_snd10((parent),SMSG_LODGE,(child),(leaf))
#define sch_msg_snd_chop(child,parent,ancestor)	\
		smsg_snd10((child),SMSG_CHOP,(parent),(ancestor))
#define sch_msg_snd_lodge_idle(parent,child,leaf)	\
		smsg_snd10((parent),SMSG_LODGE_IDLE,(child),(leaf))
#define sch_msg_snd_js_prologue(parent,child,leaf)	\
		smsg_snd10((parent),SMSG_JS_PROLOGUE,(child),(leaf))

#define sch_msg_snd_js_trust(leaf,parent,parent_alt,nxtcls,info)	\
		smsg_snd12((leaf),SMSG_JS_TRUST,(parent),(parent_alt),(nxtcls),(info))

#define sch_msg_snd_js_trav_up(parent,child,leaf,coma)	\
		smsg_snd20((parent),SMSG_JS_TRAV_UP,(child),(leaf),(coma))
#define sch_msg_snd_js_trav_dn(child,parent,leaf,coma)	\
		smsg_snd20((child),SMSG_JS_TRAV_DN,(parent),(leaf),(coma))
#define sch_msg_snd_js_install_fl(lodge,child,leaf,coma)		\
		smsg_snd20((lodge),SMSG_JS_INSTALL_FL,(child),(leaf),(coma))

#define sch_msg_snd_straighten(parent,child,leaf,parent_alt,alt)	\
		smsg_snd21((parent),SMSG_STRAIGHTEN,(child),(leaf),(parent_alt),(alt))

#define sch_msg_snd_js_install(child,parent,lodge,leaf,coma)		\
	smsg_snd30((child), SMSG_JS_INSTALL,  (parent),(lodge),(leaf),(coma))


#if defined(MULTI_THREADS)
int simp_lock(lock)
char *lock;
{
   int ok = 0;
   if (sch_mutex_lock()) {
      if (!*lock) {
	ok = *lock = 1;
      }
      sch_mutex_unlock();
   }
   return(ok);
}

void simp_unlock(lock)
char *lock;
{  
   while (!sch_mutex_lock()) {}
   assert(*lock);
   *lock = 0;
   sch_mutex_unlock();
}
#endif /* MULTI_THREADS */

void sch_port_upcall(p)
site_id_t p;
{
   LOG_EVENT_PUSH(SCH_ASYNC)
   if (Scheduler(p)->async_hdl) {
      sch_async_msg_hdls(p, 1);
   } else {
      sch_port_naive_upcall(p);
   }
   LOG_EVENT_POP;
}

/* UPCALL functions for the Scheduler Port */
static void sch_port_naive_upcall(port)
aport_id_t port;
{
   eng_msg_trigger(Scheduler(port)->engine);
   return;
}

/* UPCALL functions for the Scheduler Port */
static void sch_async_msg_hdls(port, async)
aport_id_t port;
int async;
{
   amsg_ret_t ret;
   amsg_t msg;
   smsg00_t *data;
   amsg_type_t mdt_type;
   amsg_count_t mdt_count;

   st_id_t *recv;
   st_id_t *send;

   eng_msg_trigger(Scheduler(port)->engine);
   while (1) {
     Disable_Int();
     ret = amsg_peek(	port,
			&msg,
			(amsg_data_t * *)&data,
			&mdt_type,
			&mdt_count);
     if (ret == AMSG_NOMESSAGE) {
        eng_msg_reset(Scheduler(port)->engine);
#if !defined(NO_SHORTCUT)
        assert(async || !Intrasite_Smsg(port));
#endif  /* NO_SHORTCUT */
        Enable_Int();
        return;
     }
     Enable_Int();
     assert(ret==AMSG_OK && mdt_count == 1);
     recv = &data->recv;
     if (!Sch_Lock(recv)) {
	assert(async);
	return;
     }
     send = &data->send;

     IntraSiteCheckOut(port,Site(send));

     switch (data->type) {
     case SMSG_BACKTRACK:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_backtrack(recv,send);
       break;
     case SMSG_LODGED:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_lodged(recv,send);
       break;
     case SMSG_WITHERED:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_withered(recv,send);
       break;
     case SMSG_DEC_CORPSE:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_dec_corpse(recv,send);
       break;
     case SMSG_JS_IN_VAIN:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_js_in_vain(recv,send);
       break;
     case SMSG_LOAD_REPORT:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_load_report(recv,send);
       break;
     case SMSG_SET_JS_ROOT:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_set_js_root(recv,send);
       break;
     case SMSG_JS_AGAIN:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_js_again(recv,send);
       break;
     case SMSG_IDLE_ENG:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_idle_eng(recv,send);
       break;
     case SMSG_WAKE_ENG:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_wake_eng(recv,send);
       break;
     case SMSG_TELL_IDLE:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_tell_idle(recv,send);
       break;
     case SMSG_STOP_IDLE:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_stop_idle(recv,send);
       break;
     case SMSG_IDLE_TOLD:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_idle_told(recv,send);
       break;
     case SMSG_LMP:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_lmp(recv,send);
       break;
     case SMSG_REDUCE_WK_UP:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_reduce_wk_up(recv,send);
       break;
     case SMSG_REDUCE_WK_DN:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_reduce_wk_dn(recv,send);
       break;
     case SMSG_ENGINE_MIGRATE:
       assert(mdt_type == Scheduler(port)->smsg_type[0][0]);
       sch_msg_hdl_engine_migrate(recv,send);
       break;

     case SMSG_CUT_OK:
       assert(mdt_type == Scheduler(port)->smsg_type[0][1]);
       sch_msg_hdl_cut_ok(recv,send,((smsg01_t *)data)->i1);
       break;
     case SMSG_INIT_LODGE:
       assert(mdt_type == Scheduler(port)->smsg_type[0][1]);
       sch_msg_hdl_init_lodge(recv,send, ((smsg01_t *)data)->i1);
       break;

     case SMSG_JS_SUCCESS:
       assert(mdt_type == Scheduler(port)->smsg_type[0][2]);
       sch_msg_hdl_js_success(recv, send,
		((smsg02_t *)data)->i1, ((smsg02_t *)data)->i2);
	   break;

     case SMSG_CUT:
       assert(mdt_type == Scheduler(port)->smsg_type[1][0]);
       sch_msg_hdl_cut(recv,send,&(((smsg10_t *)data)->t1));
       break;
     case SMSG_LODGE:
       assert(mdt_type == Scheduler(port)->smsg_type[1][0]);
       sch_msg_hdl_lodge(recv,send,&(((smsg10_t *)data)->t1));
       break;
     case SMSG_CHOP:
       assert(mdt_type == Scheduler(port)->smsg_type[1][0]);
       sch_msg_hdl_chop(recv,send,&(((smsg10_t *)data)->t1));
       break;
     case SMSG_LODGE_IDLE:
       assert(mdt_type == Scheduler(port)->smsg_type[1][0]);
       sch_msg_hdl_lodge_idle(recv,send,&(((smsg10_t *)data)->t1));
       break;
     case SMSG_JS_PROLOGUE:
       assert(mdt_type == Scheduler(port)->smsg_type[1][0]);
       sch_msg_hdl_js_prologue(recv,send, &(((smsg10_t *)data)->t1));
       break;

     case SMSG_JS_TRUST:
       assert(mdt_type == Scheduler(port)->smsg_type[1][2]);
       sch_msg_hdl_js_trust(recv, send, &(((smsg12_t *)data)->t1),
		((smsg12_t *)data)->i1, ((smsg12_t *)data)->i2);
	   break;

     case SMSG_JS_TRAV_UP:
       assert(mdt_type == Scheduler(port)->smsg_type[2][0]);
       sch_msg_hdl_js_trav_up(recv,send,
	&(((smsg20_t *)data)->t1), &(((smsg20_t *)data)->t2));
       break;
     case SMSG_JS_TRAV_DN:
       assert(mdt_type == Scheduler(port)->smsg_type[2][0]);
       sch_msg_hdl_js_trav_dn(recv,send,
	&(((smsg20_t *)data)->t1), &(((smsg20_t *)data)->t2));
       break;
     case SMSG_JS_INSTALL_FL:
       assert(mdt_type == Scheduler(port)->smsg_type[2][0]);
       sch_msg_hdl_js_install_fl(recv,send,
	&(((smsg20_t *)data)->t1), &(((smsg20_t *)data)->t2));
       break;

     case SMSG_STRAIGHTEN:
       assert(mdt_type == Scheduler(port)->smsg_type[2][1]);
       sch_msg_hdl_straighten(recv,send,
	&(((smsg21_t *)data)->t1), &(((smsg21_t *)data)->t2), ((smsg21_t *)data)->i1);
       break;

     case SMSG_JS_INSTALL:
       assert(mdt_type == Scheduler(port)->smsg_type[3][0]);
       sch_msg_hdl_js_install(recv,send,
	&(((smsg30_t *)data)->t1), &(((smsg30_t *)data)->t2), &(((smsg30_t *)data)->t3)); 
	   break;

       default: error("unknown scheduler message");
     }
     /* throw away the handled message */
     ret = amsg_receive(
		port,
		(amsg_t *)0,
		(amsg_data_t * *)0,
		&mdt_type,
		&mdt_count,
		(amsg_option_t) 0
		);
     assert(ret==AMSG_OK);
  }
}


/* Interface functions called by Worker Management */

void sch_create_leaf(site, engine, leaf)
site_id_t site;
eng_handle_t engine;
st_handle_t **leaf;
{
   void smsg_type_init();
   void smsg_stat_init();
   void sch_create_leaf0();

   /* initilize a template for a knot. */
   knot_template->suspended = (st_susp_t *)0;
   PackSite(knot_template,site);
   knot_template->alive_twigs = 0;
   knot_template->nxtcls = 0;
   knot_template->nxtcls_b = 0;
   knot_template->hybrid = 0;
   knot_template->jroot = 0;
   knot_template->proot = 0;
   knot_template->local = 0;
   knot_template->tip = 1;
   knot_template->lock = 0;
   knot_template->trunk.info = 0;

   /* register the various message types which will be sent and/or received
   ** via the scheduler port.
   */
   smsg_type_init(site);

   /* initialise the scheduler datastructure */
   Scheduler(site)->port			= site;
   Scheduler(site)->engine			= engine;
   Scheduler(site)->load_report_eager		= 0;
   Scheduler(site)->async_hdl			= 0;
   Scheduler(site)->idling			= 0;
   Scheduler(site)->waking			= 0;
   Scheduler(site)->lmp				= 0;
   Scheduler(site)->root_first			= 1; 
   Scheduler(site)->left_first			= 1;

   Scheduler(site)->max_to_publish		= 5;  

   Scheduler(site)->edge_buffer.count		= 0;
   Scheduler(site)->edge_buffer.head.trunk.next	=
   Scheduler(site)->edge_buffer.head.trunk.prev	=
		&(Scheduler(site)->edge_buffer.head.trunk);
   Simp_Initlock(&(Scheduler(site)->edge_buffer));

   Scheduler(site)->susp_buffer.count		= 0;
   Scheduler(site)->susp_buffer.next		= (st_susp_t *) 0;
   Simp_Initlock(&(Scheduler(site)->susp_buffer));

#if !defined(NO_SHORTCUT)
   Intrasite_Smsg(site) = 0;
#endif  /* NO_SHORTCUT */

   /* reset scheduler message traffic statistics */
   smsg_stat_init(site);
   InitSchOffset(site);
   *leaf = &(Scheduler(site)->leaf);
   sch_create_leaf0(site, (st_id_t *)*leaf);
   return;
}

void sch_create_leaf0(site, leaf)
site_id_t site;
st_id_t *leaf;
{
   Sch_Alloc_Knot(site,leaf);
   if (Sch_Lock(leaf)) {
      SetNalive(Trunk(leaf), ST_INIT);
      return;
   } else {
     assert(zero());
  }
}


void sch_create_root(site, root_hdl)
site_id_t site;
st_handle_t *root_hdl;
{
   st_id_t *root = (st_id_t *)root_hdl;
   Sch_Alloc_Knot(site,root);
   Knot(root)->tip = 0;
   Knot(root)->jroot  = 1;
   *SupTree(root) = *(root);
   SetAlive(Trunk(root));
   SetRich(Trunk(root));
   SetEldest(Trunk(root));
   return;
}

void sch_genesis(root_hdl,leaf_hdl,twig_hdl,first)
st_handle_t *root_hdl, *leaf_hdl, *twig_hdl;
int first;
{
   st_id_t *root = (st_id_t *)root_hdl;
   st_id_t *leaf = (st_id_t *)leaf_hdl;
   st_id_t *twig = (st_id_t *)twig_hdl;

   Scheduler(Site(leaf))->root = *root_hdl;

   if (first) {
#if defined(INITIAL_SCHTR)
     /* set the scheduler trace */
     (void) global_flags(0, SCH_TRACE_FLAG);
#endif /* INITIAL_SCHTR */
      Add_Alive_Twig_Next(twig,leaf,Knot(root),Trunk(root)->prev);
      Twig(twig)->info = Trunk(leaf)->info = Trunk(root)->info;
      Knot(leaf)->jroot = Knot(root)->jroot;
      Sch_Event_Notify2("S_GENESIS", "null         ", twig, leaf);
   } else {
      *twig = *root;
      Sch_Event_Notify2("NEW_COMER", "null         ", twig, leaf);
   }

   *SupTree(leaf) = *twig;
   return;
}

void sch_init_lodge(site,wm,leaf)
aport_id_t site;
int wm;
st_handle_t *leaf; 
{
   st_id_t *root = (st_id_t *)&Scheduler(site)->root;
   assert(SchRoot(root) && Alive(Trunk(root)));
   if (Sch_Lock(root)) {
      st_id_t twig;
      Add_Lodge_Twig(&twig,(st_id_t *)leaf,Knot(root));
      wm_init_lodged(wm, (st_handle_t *)&twig);
      Sch_Unlock(root);
   } else {
      sch_msg_snd_init_lodge(root,(st_id_t *)leaf, wm);
   }
   return;
}


/* Interface functions called by Engines */

void sch_sync_msg_hdls(leaf_hdl)
const st_handle_t *leaf_hdl;
{
   int ok;
   site_id_t site = Site((st_id_t *)leaf_hdl);

   Sch_Unlock(SiteLeaf(site));
   sch_async_msg_hdls(site, 0);

   Disable_Int();
   ok = Sch_Lock(SiteLeaf(site));
   Enable_Int();
   assert(ok);
   return;
}

void sch_backtrack(leaf_hdl)
const st_handle_t *leaf_hdl;
{
  int ok;
  st_id_t *leaf = (st_id_t *) leaf_hdl;
  st_id_t parent; parent = *SupTree(leaf);
  if (Life(Trunk(leaf))==ST_INIT) { /* ## INITIAL connection */
     SetNalive(Trunk(leaf),ST_LODGED);
     Smsg_ShortCut_End(leaf, &parent,
	  sch_msg_hdl_js_trav_up(&parent,leaf,&parent,leaf),
	  sch_msg_snd_js_trav_up(&parent,leaf,&parent,leaf)
	 );
     goto _return;
  } else {
     assert(Alive(Trunk(leaf)));
     SetNalive(Trunk(leaf),ST_BACKTRACK);

	/* imagine: the leaf is jsroot, and its parent is a worker_boundry
        ** node which has to be taken by a leaf which might be currently
	** suspended to this backtracking leaf. This has happened: all of
	** workers become suddenly inactive. Thus added the following check.
	*/
     if (Suspended(leaf)) {
	void relocate_suspended();
	assert(JsRoot(leaf));
        relocate_suspended(leaf);
     }
     ResetJsRoot(leaf);
     if (Scheduler(Site(leaf))->idling) {	/* ## Idling */
        /* idling bit remains, as not known yet if idle-tell accepted */
	Smsg_ShortCut_End(leaf, &parent,
	     sch_msg_hdl_tell_idle(&parent, leaf),
	     sch_msg_snd_tell_idle(&parent, leaf)
	    );
     } else {
	Smsg_ShortCut_End(leaf, &parent,
	     sch_msg_hdl_backtrack(&parent, leaf),
	     sch_msg_snd_backtrack(&parent, leaf)
	    );
     }
_return:
     Disable_Int();
     ok = Sch_Lock(leaf);
     Enable_Int();
     assert(ok);
     return;
  }
}

void sch_cut(leaf_hdl,parent_hdl)
const st_handle_t *leaf_hdl, *parent_hdl;
{
  st_id_t olf, nlf, parent;
  site_id_t site = Site(leaf_hdl);
  int ok;
  olf    = *(st_id_t *) leaf_hdl;
  parent = *(st_id_t *) parent_hdl;

  if (Suspended(&olf)) {
     void relocate_suspended();
     assert(JsRoot(&olf));
     relocate_suspended(&olf);
  }
  ResetJsRoot(&olf);
  Sch_Alloc_Knot(site,&nlf);
  Reincarnat(&olf,&nlf);
  NewSiteLeaf(&nlf);
  Smsg_ShortCut_End(&olf, &parent,
       sch_msg_hdl_cut(&parent, &olf, &nlf),
       sch_msg_snd_cut(&parent, &olf, &nlf)
      );
  Disable_Int();
  ok = Sch_Lock(SiteLeaf(site)); assert(ok);
  Enable_Int();
  return;
}

/* these two sch-interface functions differ from others:
**     they do return, without checking scheduler message queue
*/

void sch_load_report(leaf_hdl)
const st_handle_t *leaf_hdl;
{
   st_id_t * leaf = (st_id_t *)leaf_hdl;
   if (!Rich(Trunk(leaf))) {
      assert(!JsRoot(leaf));
      SetRich(Trunk(leaf));
      Smsg_ShortCut_Mid(leaf, SupTree(leaf),
 	   sch_msg_hdl_load_report(SupTree(leaf),leaf),
	   sch_msg_snd_load_report(SupTree(leaf),leaf)
	  );
      return;
   } else if (Suspended(leaf)) {
/*
      void loadreport_and_publish();
      st_id_t olf; olf = *leaf;
      assert(JsRoot(leaf));
      loadreport_and_publish(&olf);
      return;
*/
      /* as there exists a bug that a suspended leaf has a chopped
      ** common ancestor which should not happen, I have to comment
      ** out the above code (lll before 3.5.2 release).
      */
      assert(JsRoot(leaf));
      Sch_WakeUp_Suspended(leaf);
      return;
   }
}

void loadreport_and_publish(olf)
st_id_t *olf;
{
   site_id_t site = Site(olf);
   int remains, published, ok=0;
   st_id_t *nlf;
   st_susp_t *x = Suspended(olf);
   int max = (Scheduler(site)->root_first)?Scheduler(site)->max_to_publish:9999;

   /* calculate the number of suspended leaves */
   while (x) {
      ok++;
      x = x->next;
   } 
   if (ok > max) max = ok;
   x = Suspended(olf);
   Suspended(olf) = (st_susp_t *)0;
   if (!(published=eng_publish(LeafEngine(olf), max, &remains))) {
      /* nothing published. it can happen when some ad hoc parallel chpts
      ** are published or engine is crazy with a false load report
      */
      nlf = SiteLeaf(site);
      Suspended(nlf) = x;
      while (!ComnKnot(nlf,olf)) { /* it is safe as no upward messages */
	 SetJsRoot(nlf);
	 nlf = SupTree(nlf);
      }
      return;
   } else {
      st_id_t twig;
      
   /* Published alternatives will wholelly be at the disposal of the current
   ** allocation. Some can be booked owing to asynchronous smsg-handling,
   ** but the relevant installations will not be handled before this function
   ** finishes, since the leaf is locked.
   */
      assert(!(remains && published<max));
      /* lock the published to simplify the handling */
      nlf = SiteLeaf(site);
      do {
	 nlf = SupTree(nlf);
	 ok = Sch_Lock(nlf);
      } while (!ComnKnot(nlf,olf));
      Suspended(olf) = x;
      if (!Alive(Trunk(olf))) { /* already chopped */
	 void relocate_suspended();
	 relocate_suspended(olf); 
	 nlf = SiteLeaf(site);
	 do {
	    nlf = SupTree(nlf);
	    Sch_Unlock(nlf); 
	 } while (!ComnKnot(nlf,olf));
	 return;
      }
      nlf = SiteLeaf(site);
      while (x) {
         while (Exhausted(nlf) && !ComnKnot(nlf,olf)) {
	    nlf = SupTree(nlf);
	    if (!remains) {
               SetPoor(Twig(nlf));
               SetPoor(Trunk(SubTree(nlf)));
	    }
	 }
	 if (Exhausted(nlf)) break;
         Bk_NextClause0(nlf);
         Add_Lodge_Twig(&twig, &(x->leaf), Knot(nlf));
         eng_donate_state(LeafEngine(SiteLeaf(site)), (st_handle_t *)&twig,
                        (st_handle_t *)&(x->coma), (st_handle_t *)&(x->leaf));
         Scheduler(site)->state_donate++;
         Suspended(olf) = x->next;
         Sch_Gc_Suspended(site,x);
	 x = Suspended(olf);
      }
      if (Suspended(olf)) {
	 assert(!remains); 
	 assert(ComnKnot(nlf,olf));
      }
      nlf = SiteLeaf(site);
      do {
         nlf = SupTree(nlf);
         Sch_Unlock(nlf);
      } while (!ComnKnot(nlf,olf));
      return;
   }
}

void sch_load_publish_one(leaf_hdl, alt, parent_hdl, hybrid)
const st_handle_t *leaf_hdl;
st_handle_t *parent_hdl;
int alt, hybrid;
{
  st_id_t * olf    = (st_id_t *)leaf_hdl;
  st_id_t * parent = (st_id_t *)parent_hdl;

  st_id_t nlf;

  assert(Rich(Trunk(olf)));
  /* make sure the engine follows the convention: 
   * when an engine rejects a job request, it is expected to
   * issue a job-report before it actually publishes anything.
   * It is to avoid the situation that suspended request flows
   * are buried within ancestors. A job-report will wake the
   * suspended request flows.
   */
  assert(!JsRoot(olf)||!Suspended(olf));
  Sch_Alloc_Knot(Site(olf), &nlf);
  Add_Alive_Twig_Next(parent,&nlf,Knot(olf),Trunk(olf)->prev);
  Twig(parent)->info = Trunk(&nlf)->info = Trunk(olf)->info;
  if (alt > 0) {
     Knot(olf)->nxtcls = Knot(olf)->nxtcls_b = alt;
  } else { /* a dummy node */
     SetPsRoot(olf);
     if (alt==(-1)) /* a reserve the sequential choice points for
		    ** the local worker
		    */
        SetLocal(olf);
  }
  Hybrid(olf) = hybrid;
  Knot(olf)->tip = 0;
  *SupTree(&nlf) = *parent;

  Sch_Event_Notify3("LF_SPROUT","alt          ",SupTree(parent),parent,&nlf);

  if (Sch_Lock(&nlf)) { 
     Sch_Unlock(olf); 
     NewSiteLeaf(&nlf);
  } else {
     assert(zero());
  }
}

/* Scheduler message handlers */
/* Covention: the subject node/leaf, often named as 'self', has been locked,
	      before the handler is called;
	      they should be unlocked before the handler returns.
*/


/* Up */
static void sch_msg_hdl_backtrack(self, leaf)
st_id_t *self, *leaf;
{
  void sch_msg_hdl_backtrack_();

  Smsg_Hdl_Notify(SMSG_BACKTRACK,self,leaf);
  if (Alive(Trunk(self))) {
     sch_msg_hdl_backtrack_(self, leaf);
     return;
  } else {
     assert(Chopped(Trunk(self)));
     Sch_Unlock(self);
     return;
  }
}

void sch_msg_hdl_backtrack_(self, leaf)
st_id_t *self, *leaf;
{
  int alt = Bk_NextClause(self); alt = Tk_NextClause(self);
  UpdateLmp(self);
  AliveTwigs(self)--;
  SetNalive(Twig(self),ST_LODGED);

/* ## schedule an alternative available 
**    we do not care about special nodes (e.g. proot,sroot) as they
**    created as dummy one, thus with no alternative 
*/ 
  if (alt && (AliveTwigs(self) || !Exhausted(self))) {
     Quit_Chain(Twig(self));
     Join_Chain_Next(Twig(self),Trunk(self)->prev);
     SetAlive(Twig(self));
     AliveTwigs(self)++;
     SetPoor(Twig(self));
     Smsg_ShortCut_End(self, leaf,
          sch_msg_hdl_js_success(leaf, self, alt, Twig(self)->info),
          sch_msg_snd_js_success(leaf, self, alt, Twig(self)->info)
         );
     return;
  } else if (alt || (!AliveTwigs(self)&&(ComnSite(self,leaf)||!Local(self)))) {
     assert(!SchRoot(self));
     SetNalive(Trunk(self), ST_DYING);
     if (!alt && Hybrid(self)) alt = -1;
     Smsg_ShortCut_End(self, SupTree(self),
          sch_msg_hdl_straighten(SupTree(self),self,leaf,self,alt),
          sch_msg_snd_straighten(SupTree(self),self,leaf,self,alt)
         );
     return;
  } else if (!AliveTwigs(self) && Suspended(self)) {
     st_susp_t **x = &Suspended(self);
     do { /* only when the local leaf is idling, this loop can finish
	     without breaking */
	if (ComnSite(self,&((*x)->leaf))) {
	   st_susp_t *y = *x;
	   *x = y->next;
	   SetNalive(Trunk(self), ST_DYING);
	   if (Hybrid(self)) alt = -1;
	   Smsg_ShortCut_Mid(self, SupTree(self),
		sch_msg_hdl_straighten(SupTree(self),self,&(y->leaf),self,alt),
		sch_msg_snd_straighten(SupTree(self),self,&(y->leaf),self,alt)
	       );
	   Sch_Gc_Suspended(Site(self),y);
	   break; 
	} else {
	   x = &((*x)->next);
	}
     } while (*x);
  }
  /* ## job-search prologue. */
  if (ComnSite(self,SupTree(self)) && !PoorSpine(self) && !JsRoot(self) &&
      !(Local(self)&&ComnSite(self,leaf))) {
     Smsg_ShortCut_End(self,SupTree(self),
          sch_msg_hdl_js_prologue(SupTree(self),self,leaf),
          sch_msg_snd_js_prologue(SupTree(self),self,leaf)
         );
  } else {
    st_id_t *coma = self;
    SetPoorSpine(self);
    Sch_JS_Trav_Down(self,coma,leaf);
    Sch_JS_Trav_Up(self,coma,leaf);
  }
}


/* Up */
static void sch_msg_hdl_straighten(self,child,leaf,parent,alt)
st_id_t *self, *child, *leaf, *parent;
int alt;
{
  st_id_t twig;
  Smsg_Hdl_Notify(SMSG_STRAIGHTEN,self,child);
  if (Alive(Twig(self))) {
     AliveTwigs(self)--;
     if (!Exhausted(self)||(AliveTwigs(self)||PsRoot(self)||SchRoot(self))) {
	if (alt) {
	   Add_Alive_Twig_Next(&twig,leaf,Knot(self),Twig(self));
	   Twig(&twig)->info = Twig(self)->info;
	   if (JsRoot(self) && Monad(self) && !Suspended(self)) {
	      SetRich(Twig(&twig));
	      Smsg_ShortCut_Mid(&twig,leaf,
	           sch_msg_hdl_js_trust(leaf,&twig,parent,alt,Twig(&twig)->info),
	           sch_msg_snd_js_trust(leaf,&twig,parent,alt,Twig(&twig)->info)
	          );
	      Smsg_ShortCut_Mid(&twig,leaf,
		   sch_msg_hdl_set_js_root(leaf, &twig),
		   sch_msg_snd_set_js_root(leaf, &twig)
	          );
	   } else {
	      SetPoor(Twig(&twig));
	      Smsg_ShortCut_Mid(&twig,leaf,
	           sch_msg_hdl_js_trust(leaf,&twig,parent,alt,Twig(&twig)->info),
	           sch_msg_snd_js_trust(leaf,&twig,parent,alt,Twig(&twig)->info)
	          );
	   }
	} else if (!Exhausted(self)) {
	   alt = Bk_NextClause(self);
	   alt = Tk_NextClause(self);
	   if (!Exhausted(self)||(AliveTwigs(self)||PsRoot(self)||SchRoot(self))) {
	      Add_Alive_Twig_Next(&twig,leaf,Knot(self),Twig(self));
	      Twig(&twig)->info = Twig(self)->info;
	      SetPoor(Twig(self));
	      Smsg_ShortCut_Mid(&twig, leaf,
                   sch_msg_hdl_js_success(leaf,&twig,alt,Twig(&twig)->info),
                   sch_msg_snd_js_success(leaf,&twig,alt,Twig(&twig)->info)
                  );
	   } else {
	      SetNalive(Trunk(self),ST_DYING);
	      Smsg_ShortCut_Mid(self, SupTree(self),
		   sch_msg_hdl_straighten(SupTree(self),self,leaf,self,alt),
		   sch_msg_snd_straighten(SupTree(self),self,leaf,self,alt)
		  );
	   }
	} else if (PsRoot(self) && !AliveTwigs(self)) {
	   if (!Local(self) || ComnSite(self,leaf)) {
	      SetNalive(Trunk(self),ST_DYING);
	      if (Hybrid(self)) alt = -1;
	      Smsg_ShortCut_Mid(self, SupTree(self),
		   sch_msg_hdl_straighten(SupTree(self),self,leaf,self,alt),
		   sch_msg_snd_straighten(SupTree(self),self,leaf,self,alt)
		  );
	   } else {
	      if (Suspended(self)) {
		 st_susp_t **x = &Suspended(self);
		 do { /* only when the local leaf is idling, this loop can
			 finish without breaking */
		    if (ComnSite(self,&((*x)->leaf))) {
			st_susp_t *y = *x;
			*x = y->next;
			SetNalive(Trunk(self), ST_DYING);
			if (Hybrid(self)) alt = -1;
			Smsg_ShortCut_Mid(self, SupTree(self),
		sch_msg_hdl_straighten(SupTree(self),self,&(y->leaf),self,alt),
		sch_msg_snd_straighten(SupTree(self),self,&(y->leaf),self,alt)
			);
			Sch_Gc_Suspended(Site(self),y);
			break;
		    } else {
			x = &((*x)->next);
		    }
		 } while (*x);
	      }
	      Add_Lodge_Twig(&twig,leaf,Knot(self));
	      Smsg_ShortCut_Mid(&twig, leaf,
		   sch_msg_hdl_lodged(leaf, &twig),
		   sch_msg_snd_lodged(leaf, &twig)
		  );
	   } 
	} else if (Local(self)&&ComnSite(self,leaf)) {
	   st_susp_t *x;
	   Sch_Alloc_Suspended(Site(self), x);
	   x->next = Suspended(self);
	   x->leaf = *(leaf);
	   x->coma = *(self);
	   Suspended(self) = x;
	} else if (!JsRoot(self) && !PoorSpine(self)
				 && ComnSite(self,leaf)
				 && ComnSite(self,SubTree(self))
				 && ComnSite(self,SupTree(self))) {
	   Smsg_ShortCut_Mid(self, SupTree(self),
		sch_msg_hdl_js_prologue(SupTree(self),self,leaf),
		sch_msg_snd_js_prologue(SupTree(self),self,leaf)
		);
	} else {
	   Add_Lodge_Twig(&twig,leaf,Knot(self));
	   Smsg_ShortCut_Mid(&twig, leaf,
                sch_msg_hdl_lodged(leaf, &twig),
                sch_msg_snd_lodged(leaf, &twig)
               );
	}
	SetNalive(Twig(self),ST_CHOPPED);
	Smsg_ShortCut_End(self, SubTree(self),
	     sch_msg_hdl_chop(SubTree(self),self,self),
	     sch_msg_snd_chop(SubTree(self),self,self)
	    );
	return;
     } else { /* need resume sequential chpts: further up */
	SetNalive(Trunk(self),ST_DYING);
	SetNalive(Twig(self),ST_DYING);
	if (!alt && Hybrid(self)) alt = -1;
	Smsg_ShortCut_End(self, SupTree(self),
	     sch_msg_hdl_straighten(SupTree(self),self,leaf,parent,alt),
	     sch_msg_snd_straighten(SupTree(self),self,leaf,parent,alt)
	    );
	return;
     }
  } else {
     assert(!Alive(Trunk(self))); /* i.e. whole subtree chopped */
     if (ComnSite(self,SupTree(self)) && ComnSite(self,leaf) &&
	 !PoorSpine(self) && !PoorSpine(SupTree(self))) {
	Smsg_ShortCut_End(self, SupTree(self),
	     sch_msg_hdl_js_prologue(SupTree(self),self,leaf),
	     sch_msg_snd_js_prologue(SupTree(self),self,leaf)
	    );
     } else {
	Smsg_ShortCut_End(self,SupTree(self),
             sch_msg_hdl_lodge(SupTree(self),self,leaf),
             sch_msg_snd_lodge(SupTree(self),self,leaf)
            );
     }
  }
}

/* Up */
static void sch_msg_hdl_js_prologue(self, child, leaf)
st_id_t *self, *child, *leaf;
{
  Smsg_Hdl_Notify(SMSG_JS_PROLOGUE, self, child);
  if (!Exhausted(self)) { /* it does not care about the twig state */
     st_id_t twig;
     int alt = Bk_NextClause(self); /* value not interesting */
     alt = Tk_NextClause(self);
     Add_Alive_Twig_Next(&twig,leaf,Knot(self),Trunk(self)->prev);
     CheckLmp(&twig);
     SetPoor(Twig(&twig));
     Smsg_ShortCut_End(&twig, leaf,
	  sch_msg_hdl_js_success(leaf,&twig,alt,Twig(&twig)->info),
	  sch_msg_snd_js_success(leaf,&twig,alt,Twig(&twig)->info)
	 );
     return;
  }
  if (Alive(Trunk(self)) && (JsRoot(self) || PoorSpine(self) ||  
             !ComnSite((self),SupTree(self)) || PoorSpine(SupTree(self)) ||
             (ComnSite(self,leaf) && Local(self)))) {
     Smsg_ShortCut_End(self,leaf,
          sch_msg_hdl_js_in_vain(leaf,self),
          sch_msg_snd_js_in_vain(leaf,self)
         );
     return;
  } else if (ComnSite((self),SupTree(self))) {
     Smsg_ShortCut_End(self, SupTree(self),
          sch_msg_hdl_js_prologue(SupTree(self),self,leaf),
          sch_msg_snd_js_prologue(SupTree(self),self,leaf)
         );
     return;
  } else {
     Smsg_ShortCut_End(self, SupTree(self),
          sch_msg_hdl_lodge(SupTree(self),self,leaf),
          sch_msg_snd_lodge(SupTree(self),self,leaf)
         );
     return;
  }
}

/* Down */
static void sch_msg_hdl_js_success(leaf,parent,nxtcls,info)
st_id_t *leaf, *parent;
int nxtcls, info;
{
  Smsg_Hdl_Notify(SMSG_JS_SUCCESS, leaf,parent);
  eng_backtrack(LeafEngine(leaf),(st_handle_t *)parent,(unsigned) nxtcls);

  switch (Life(Trunk(leaf))) {
    case ST_CUT:
      Smsg_ShortCut_Mid(leaf, Corpse(leaf),
	   sch_msg_hdl_dec_corpse(Corpse(leaf),leaf),
	   sch_msg_snd_dec_corpse(Corpse(leaf),leaf)
	  );
      break;
    case ST_BACKTRACK:
    case ST_LODGED:
    case ST_CHOPPED:
      if (ComnNode(SupTree(leaf),parent))
	 break;
      Smsg_ShortCut_Mid(leaf, SupTree(leaf),
	   sch_msg_hdl_withered(SupTree(leaf),leaf),
	   sch_msg_snd_withered(SupTree(leaf),leaf)
	  );
      break;
    default:
      error("illegal leaf type");
  } 
  Trunk(leaf)->info = info;
  assert(!Suspended(leaf));
  *SupTree(leaf) = *parent;
  Sch_Unlock(leaf);
  return;
}

/* Down */
static void sch_msg_hdl_js_trust(leaf,parent,parent_nxtcls,nxtcls,info)
st_id_t *leaf, *parent, *parent_nxtcls;
int nxtcls, info;
{
  Smsg_Hdl_Notify(SMSG_JS_TRUST, leaf,parent);
  /* the leaf could in a lodged state when a 'Local' node becomes resumable */
  assert(Life(Trunk(leaf))==ST_BACKTRACK || Life(Trunk(leaf))==ST_LODGED);
  assert(!Suspended(leaf));
  Smsg_ShortCut_Mid(leaf, SupTree(leaf),
        sch_msg_hdl_withered(SupTree(leaf),leaf),
        sch_msg_snd_withered(SupTree(leaf),leaf)
       );
  if (!ComnNode(parent_nxtcls,SupTree(leaf))) {
     eng_backtrack(LeafEngine(leaf), (st_handle_t *)parent_nxtcls, 0);
  }
  if (nxtcls>0) { 
     assert(nxtcls==1);
     eng_trust(LeafEngine(leaf),(unsigned) nxtcls); /* keep this order with next line */
     eng_undo_publish(LeafEngine(leaf),(st_handle_t *)parent);
  } else {
     eng_undo_publish(LeafEngine(leaf), (st_handle_t *)parent);
     eng_fail(LeafEngine(leaf));
  }
  *SupTree(leaf) = *parent;
  Trunk(leaf)->info = info;
  if (!Rich(Trunk(leaf))) ResetJsRoot(leaf);
  Sch_Unlock(leaf);
  return;
}


static void sch_msg_hdl_dec_corpse(old,new)
st_id_t *old, *new;
{
  Smsg_Hdl_Notify(SMSG_DEC_CORPSE,old,new);
   switch (Life(Trunk(old))) {
      case ST_DYING:
	SetNalive(Trunk(old), ST_DEAD);
	Sch_Unlock(old);
	return;
      case ST_CHOPPED:
	Smsg_ShortCut_End(old, SupTree(old),
	     sch_msg_hdl_withered(SupTree(old),old),
	     sch_msg_snd_withered(SupTree(old),old)
	    );
	Sch_Gc_Knot(old);
	return;
     default: error("illegal leaf type");
   }
}

/* Up */
static void sch_msg_hdl_cut(self, olf, leaf)
st_id_t *self, *olf, *leaf;
{
  st_id_t twig;
  Smsg_Hdl_Notify(SMSG_CUT, self, olf);
  if (Alive(Twig(self))) { /* the cut wins */
     AliveTwigs(self)--;
     Add_Alive_Twig_Next(&twig,leaf,Knot(self),Twig(self));
     Twig(&twig)->info = Twig(self)->info;
     if (JsRoot(self) && Monad(self) && !Suspended(self)) {
        SetRich(Twig(&twig));
        Smsg_ShortCut_Mid(&twig,leaf,
             sch_msg_hdl_cut_ok(leaf,&twig,Twig(&twig)->info),
             sch_msg_snd_cut_ok(leaf,&twig,Twig(&twig)->info)
            );
        Smsg_ShortCut_Mid(&twig,leaf,
             sch_msg_hdl_set_js_root(leaf, &twig),
             sch_msg_snd_set_js_root(leaf, &twig)
            );
     } else {
        SetPoor(Twig(&twig));
        Smsg_ShortCut_Mid(&twig,leaf,
             sch_msg_hdl_cut_ok(leaf,&twig,Twig(&twig)->info),
             sch_msg_snd_cut_ok(leaf,&twig,Twig(&twig)->info)
            );
     }
     SetNalive(Twig(self),ST_CHOPPED); 
     Smsg_ShortCut_End(self, SubTree(self),
	  sch_msg_hdl_chop(SubTree(self), self, self),
	  sch_msg_snd_chop(SubTree(self), self, self)
	 );
     return;
  } else if (Exhausted(self)) {
     Add_Lodge_Twig(&twig,leaf,Knot(self));
     Smsg_ShortCut_End(&twig,leaf,
           sch_msg_hdl_lodged(leaf,&twig),
           sch_msg_snd_lodged(leaf,&twig)
          );
     return;
  } else {
     int alt = Bk_NextClause(self);
     alt = Tk_NextClause(self);
     Add_Alive_Twig_Next(&twig,leaf,Knot(self), Trunk(self)->prev); 
     CheckLmp(&twig);
     SetPoor(Twig(&twig));
     Smsg_ShortCut_End(&twig, leaf,
          sch_msg_hdl_js_success(leaf, &twig, alt, Twig(&twig)->info),
          sch_msg_snd_js_success(leaf, &twig, alt, Twig(&twig)->info)
         );
     return;
  }
}

/* Down */
static void sch_msg_hdl_cut_ok(leaf, parent, info)
st_id_t *leaf, *parent;
int info;
{
   Smsg_Hdl_Notify(SMSG_CUT_OK, leaf, parent);
   assert(Life(Trunk(leaf)) == ST_CUT);
   Smsg_ShortCut_Mid(leaf,Corpse(leaf),
	sch_msg_hdl_dec_corpse(Corpse(leaf),leaf),
	sch_msg_snd_dec_corpse(Corpse(leaf),leaf)
       );
   *SupTree(leaf) = *parent;
   Trunk(leaf)->info = info;
   eng_cut_ok(LeafEngine(leaf), (st_handle_t *)parent);
   assert(!Suspended(leaf));
   Sch_Unlock(leaf);
   return;
}

/* Up */
static void sch_msg_hdl_init_lodge(root, leaf, wm)
st_id_t *root, *leaf;
int wm;
{
  st_id_t twig;
  Smsg_Hdl_Notify(SMSG_INIT_LODGE,root,leaf);
  assert(SchRoot(root) && Alive(Trunk(root)));
  Add_Lodge_Twig(&twig,leaf,Knot(root));
  wm_init_lodged(wm, (st_handle_t *)&twig);
  Sch_Unlock(root);
}

/* Up */
static void sch_msg_hdl_lodge(self, child, leaf)
st_id_t *self, *child, *leaf;
{
  Smsg_Hdl_Notify(SMSG_LODGE, self, child);
  if (AliveTwigs(self)) {
     st_id_t twig;
     if (Exhausted(self)) {
	Add_Lodge_Twig(&twig,leaf,Knot(self));
	Smsg_ShortCut_End(&twig, leaf,
	     sch_msg_hdl_lodged(leaf, &twig),
	     sch_msg_snd_lodged(leaf, &twig)
	    );
	return;
     } else {
	int alt = Bk_NextClause(self); /* value not interesting */
	alt = Tk_NextClause(self);
	Add_Alive_Twig_Next(&twig,leaf,Knot(self),Trunk(self)->prev);
	CheckLmp(&twig);
	SetPoor(Twig(&twig));
	Smsg_ShortCut_End(&twig, leaf,
	     sch_msg_hdl_js_success(leaf,&twig,alt,Twig(&twig)->info),
	     sch_msg_snd_js_success(leaf,&twig,alt,Twig(&twig)->info)
	    );
	return;
     }
  } else if (Alive(Trunk(self))&&Local(self)&&ComnSite(self,leaf)) {
     int alt = Hybrid(self) ? (-1):0;
     SetNalive(Trunk(self), ST_DYING);
     Smsg_ShortCut_End(self,SupTree(self),
	  sch_msg_hdl_straighten(SupTree(self),self,leaf,self,alt),
	  sch_msg_snd_straighten(SupTree(self),self,leaf,self,alt)
	 );
     return;
  } else {
     Smsg_ShortCut_End(self, SupTree(self),
	  sch_msg_hdl_lodge(SupTree(self), self, leaf),
	  sch_msg_snd_lodge(SupTree(self), self, leaf)
	 );
     return;
  }
}

/* Down */
static void sch_msg_hdl_lodged(leaf, parent)
st_id_t *leaf, *parent;
{
   Smsg_Hdl_Notify(SMSG_LODGED, leaf, parent);
   eng_backtrack(LeafEngine(leaf), (st_handle_t *)parent, 0);
   switch (Life(Trunk(leaf))) {
      case ST_CUT:
	Smsg_ShortCut_Mid(leaf, Corpse(leaf),
	     sch_msg_hdl_dec_corpse(Corpse(leaf),leaf),
	     sch_msg_snd_dec_corpse(Corpse(leaf),leaf)
	    );
        SetNalive(Trunk(leaf),ST_LODGED);
        *SupTree(leaf) = *parent;
        Smsg_ShortCut_End(leaf, SupTree(leaf),
             sch_msg_hdl_js_prologue(SupTree(leaf),leaf,leaf),
             sch_msg_snd_js_prologue(SupTree(leaf),leaf,leaf)
            );                          
	return;
      case ST_BACKTRACK:
	if (ComnNode(SupTree(leaf),parent))
	   break;
      case ST_CHOPPED:
      case ST_LODGED:
	Smsg_ShortCut_Mid(leaf, SupTree(leaf),
	     sch_msg_hdl_withered(SupTree(leaf),leaf),
	     sch_msg_snd_withered(SupTree(leaf),leaf)
	    );
	break;
     default:
	error("illegal leaf type"); 
   }   
   SetNalive(Trunk(leaf),ST_LODGED);
   *SupTree(leaf) = *parent;
   if (Scheduler(Site(leaf))->idling) {
      Scheduler(Site(leaf))->idling = 0;
      Smsg_ShortCut_End(leaf,SupTree(leaf),
	   sch_msg_hdl_lodge_idle(SupTree(leaf),leaf,leaf),
	   sch_msg_snd_lodge_idle(SupTree(leaf),leaf,leaf)
	  );
   } else {
      Smsg_ShortCut_End(leaf,SupTree(leaf),
	   sch_msg_hdl_js_trav_up(SupTree(leaf),leaf,SupTree(leaf),leaf),
	   sch_msg_snd_js_trav_up(SupTree(leaf),leaf,SupTree(leaf),leaf)
          );
   }
}

/* Up */
static void sch_msg_hdl_withered(self,child)
st_id_t *self, *child;
{
   Smsg_Hdl_Notify(SMSG_WITHERED, self,child);
   assert(Chopped(Twig(self)) || Lodged(Twig(self)));
   Quit_Chain(Twig(self));
   Sch_Gc_Edge(Site(self),Twig(self));
   if (Twigless(self)) {
      if (Chopped(Trunk(self))) {
         Smsg_ShortCut_End(self, SupTree(self),
	      sch_msg_hdl_withered(SupTree(self),self),
	      sch_msg_snd_withered(SupTree(self),self)
	     );
         Sch_Gc_Knot(self);
         return;
      } else {
	 assert(Dying(Trunk(self)));
      }
   }
   Sch_Unlock(self);
   return;
}


/* Down */
static void sch_msg_hdl_chop(self,parent,ancestor)
st_id_t *self, *parent, *ancestor;
{
   Smsg_Hdl_Notify(SMSG_CHOP,self,parent);

   /* ## NODE ## */
   if (IsNode(self)) {
      st_edge_t *x = Trunk(self)->next;
      st_edge_t *y = (st_edge_t *) 0;
      st_id_t twig; twig = *self;

      SetNalive(Trunk(self),ST_CHOPPED);
      SetExhausted(self); 

      if (Suspended(self)) {
	 void relocate_suspended();
	 assert(JsRoot(self) || Local(self));
	 relocate_suspended(self);
      }
      /* kill alive / idle branches */
      while (x != Trunk(self)) {
	 if (Alive(x) || Dying(x) || Idle(x)) {
	    if (Alive(x)) AliveTwigs(self)--;
	    SetNalive(x,ST_CHOPPED);
	    if (!y && ComnSite(self,&(x->tree))) 
	       y = x;
	    else {
	       PackEdge(&twig,x);
	       Smsg_ShortCut_Mid(&twig, SubTree(&twig),
		    sch_msg_hdl_chop(SubTree(&twig), &twig, ancestor),
		    sch_msg_snd_chop(SubTree(&twig), &twig, ancestor)
		   );
	    }
	 }
	 x = x->next;
      }
      assert(!AliveTwigs(self));
      if (y) {
	 PackEdge(&twig,y);
	 Smsg_ShortCut_End(&twig, SubTree(&twig),
	      sch_msg_hdl_chop(SubTree(&twig), &twig, ancestor),
	      sch_msg_snd_chop(SubTree(&twig), &twig, ancestor)
	     );
      } else {
	 if (Twigless(self)) {
	    Smsg_ShortCut_End(self,SupTree(self),
		 sch_msg_hdl_withered(SupTree(self),self),
		 sch_msg_snd_withered(SupTree(self),self)
		);
	    Sch_Gc_Knot(&twig);
	 } else {
	   Sch_Unlock(self);
	 }
      } 
      return;
   }

   /* ## LEAF ## */
   switch (Life(Trunk(self))) {
     case ST_ALIVE:
       /* who will chop the only alive subtree ? */
       assert(!Suspended(self));
       Scheduler(Site(self))->lmp = 0;
       eng_stop(LeafEngine(self));
     case ST_BACKTRACK:
       if (!Scheduler(Site(self))->idling) {
	  SetNalive(Trunk(self),ST_LODGED);
	  if (ComnSite(SupTree(self),ancestor)) {
	     Smsg_ShortCut_End(self, ancestor,
		  sch_msg_hdl_js_prologue(ancestor, self, self),
		  sch_msg_snd_js_prologue(ancestor, self, self)
		 );
	     return;
	  } else {
	     Smsg_ShortCut_End(self, ancestor,
		  sch_msg_hdl_js_again(ancestor,self),
		  sch_msg_snd_js_again(ancestor,self)
		 );
	     return;
	  }
       } 
       Scheduler(Site(self))->idling = 0;
     case ST_IDLE:
	SetNalive(Trunk(self),ST_LODGED);
	Smsg_ShortCut_End(self,ancestor,
	     sch_msg_hdl_lodge_idle(ancestor,self,self),
	     sch_msg_snd_lodge_idle(ancestor,self,self)
	    );
	return;
     case ST_DYING:
	SetNalive(Trunk(self),ST_CHOPPED);
	Sch_Unlock(self);
	return;
     case ST_DEAD: {
	st_id_t gb;
	gb = *self;
	Smsg_ShortCut_End(self, SupTree(self),
	     sch_msg_hdl_withered(SupTree(self), self),
	     sch_msg_snd_withered(SupTree(self), self)
	    );
	Sch_Gc_Knot(&gb);
	return;
	}
     default:
	error("illegal leaf state");
   } 
}

void relocate_suspended(self)
st_id_t *self;
{
   st_susp_t *s;
   st_id_t *ncoma;
   while (s = Suspended(self)) {
      ncoma = ComnNode(self, &(s->coma)) ? SupTree(self) : &(s->coma);
      Smsg_ShortCut_Mid((self),SupTree(self),
           sch_msg_hdl_js_trav_up(SupTree(self), self, ncoma, &(s->leaf)),
           sch_msg_snd_js_trav_up(SupTree(self), self, ncoma, &(s->leaf))
          );
      Suspended(self) = s->next;
      Sch_Gc_Suspended(Site(self),s);
   }
}

/* Down */
static void sch_msg_hdl_js_in_vain(leaf,jroot)
st_id_t *leaf, *jroot;
{
   Smsg_Hdl_Notify(SMSG_JS_IN_VAIN, leaf,jroot);
   switch (Life(Trunk(leaf))) {
     case ST_BACKTRACK:
      SetNalive(Trunk(leaf),ST_LODGED);
     case ST_LODGED:
      if (Scheduler(Site(leaf))->idling) {
         Scheduler(Site(leaf))->idling = 0;
         Smsg_ShortCut_End(leaf,SupTree(leaf),
	      sch_msg_hdl_lodge_idle(SupTree(leaf),leaf,leaf),
	      sch_msg_snd_lodge_idle(SupTree(leaf),leaf,leaf)
	     );
         return;
      } else {
         Smsg_ShortCut_End(leaf, SupTree(leaf),
	      sch_msg_hdl_js_again(SupTree(leaf), leaf),
	      sch_msg_snd_js_again(SupTree(leaf), leaf)
             );
         return;
      }
     default:
      error("illegal leaf state");
   }
}

/* Up */
static void sch_msg_hdl_js_again(self,leaf)
st_id_t *self, *leaf;
{
   void sch_msg_hdl_js_trav_up_();
   Smsg_Hdl_Notify(SMSG_JS_AGAIN, self, leaf);
   if (Alive(Trunk(self))) {
      sch_msg_hdl_js_trav_up_(self,self,leaf);
      return;
   } else {
      Smsg_ShortCut_End(self, SupTree(self),
	   sch_msg_hdl_lodge(SupTree(self), self, leaf),
	   sch_msg_snd_lodge(SupTree(self), self, leaf)
	  );
      return;
   }
}

/* Down */
static void sch_msg_hdl_js_trav_dn(self,parent,coma,leaf)
st_id_t *self, *parent, *coma, *leaf;
{
   unsigned root_first = Scheduler(Site(self))->root_first;
   Smsg_Hdl_Notify(SMSG_JS_TRAV_DN, self,parent);

repeat:
   if (Alive(Trunk(self))) {
      if (IsNode(self)) {
/** NODE **/
         if (Knot(self)->nxtcls_b) {
            st_id_t lodge;
            if (!root_first) {
#if defined(NON_DIRECT_CHECK)
               Sch_JS_Trav_Down(self,coma,leaf);
#else /* NON_DIRECT_CHECK */
               st_edge_t *x = Trunk(self)->prev;
               while (x != Trunk(self)) {
                  if (ComnSite(self,&(x->tree))) {
                     assert(Alive(x));
                     if (!Alive(x) || !Rich(x) || !Sch_Lock(&(x->tree))) break;
                     if (Knot(&(x->tree))->nxtcls_b) {
                        Sch_Unlock(self);
                        self = &(x->tree);
                        x = Trunk(self)->prev;
                     } else {
                        Sch_Unlock(&(x->tree));
                        break;
                     }
                  } else {
                     x = x->prev;
                  }
               }
#endif /* NON_DIRECT_CHECK */
            }
            Bk_NextClause0(self);
            Add_Lodge_Twig(&lodge, leaf, Knot(self));
            Sch_JS_Install(self, &lodge, coma, leaf);
            assert(zero());
            return;
         }
         if (root_first) {
            SetPoorSpine(self);
         }
         Sch_JS_Trav_Down(self,coma,leaf);
      } else {
/** LEAF **/
         if (Rich(Trunk(self))) {
            int max = (root_first) ? Scheduler(Site(self))->max_to_publish 
				   : 9999;
	    int left;
	    if (!eng_publish(LeafEngine(self), max, &left)) {
	        st_id_t *nlf = SiteLeaf(Site(self));
	        if (!JsRoot(nlf)) SetPoor(Trunk(nlf));
		Sch_JS_Trav_Up(nlf,coma,leaf);
		return;
	    } else {
               st_id_t nlf;
	       /* make sure the engine follows the convention: 
		* when an engine rejects a job request, it is expected to
		* issue a job-report before it actually publishes anything.
		* It is to avoid the situation that suspended request flows
		* are buried within ancestors. A job-report will wake the
		* suspended request flows.
		*/
	       assert(!JsRoot(self)||!Suspended(self));
	       nlf = *SiteLeaf(Site(self));
	       if (!left) { 
		  SetPoor(Trunk(&nlf));
		  SetPoor(Twig(SupTree(&nlf)));
	       }
	       { st_id_t *next = SupTree(&nlf); 
		 st_id_t *last = self;
		 st_id_t *prev;
		 do {
	           if (Sch_Lock(next)) {
		     if (!Exhausted(next)) {
			st_id_t twig;
			Bk_NextClause0(next);
			Add_Lodge_Twig(&twig,leaf,Knot(next));
			eng_donate_state(LeafEngine(&nlf),(st_handle_t *)&twig,
							  (st_handle_t *)coma,
							  (st_handle_t *)leaf);
			Scheduler(Site(self))->state_donate++;
			Sch_Unlock(&twig);
			Sch_Unlock(&nlf);
			return;
	             } else {
		         Sch_Unlock(next);
	             }
	            }
		    prev = next;
	            next = SupTree(next);
	         } while (!ComnKnot(prev,last));
	       }
	       if (Sch_Lock(self)) {
	          Sch_Unlock(&nlf);
	          goto repeat;
	       } else 
	          assert(zero());
	    }
         }
      }
   }
   Sch_JS_Trav_Up(self,coma,leaf);
}

void sch_msg_hdl_js_trav_up_(self, coma, leaf)
st_id_t *self, *coma, *leaf;
{
   unsigned root_first = Scheduler(Site(self))->root_first;
   if (Alive(Trunk(self))) {
      if (!root_first) {
         Sch_JS_Trav_Down(self,coma,leaf);
      }
      if (!Exhausted(self) && ComnNode(self,coma)) {
         st_id_t twig;
	 int alt = Bk_NextClause(self); /* value not interesting */
	 alt = Tk_NextClause(self);
         Add_Alive_Twig_Next(&twig,leaf,Knot(self),Trunk(self)->prev);
	 CheckLmp(&twig);
     	 SetPoor(Twig(&twig));
         Smsg_ShortCut_End(&twig, leaf,
              sch_msg_hdl_js_success(leaf, &twig, alt, Twig(&twig)->info),
              sch_msg_snd_js_success(leaf, &twig, alt, Twig(&twig)->info)
             );
         return;
      }
      if (Bk_NextClause(self)) {
          st_id_t lodge;
          Add_Lodge_Twig(&lodge,leaf,Knot(self));
          Sch_JS_Install(self, &lodge, coma, leaf);
          assert(zero());
          return;
      }
      if (root_first) {
         SetPoorSpine(self);
         Sch_JS_Trav_Down(self,coma,leaf);
      }
      assert(!Rich(Trunk(self)) || JsRoot(self));
   }
   Sch_JS_Trav_Up(self,coma,leaf);
}

/* Up */
static void sch_msg_hdl_js_trav_up(self, child, coma, leaf)
st_id_t *self, *child, *coma, *leaf;
{
   SetPoor(Twig(self));
   Smsg_Hdl_Notify(SMSG_JS_TRAV_UP, self, child);
   sch_msg_hdl_js_trav_up_(self, coma, leaf);
}

static void sch_msg_hdl_js_install(self,parent,lodge,coma,leaf)
st_id_t *self, *parent, *lodge, *coma, *leaf;
{
   Smsg_Hdl_Notify(SMSG_JS_INSTALL, self,parent);
   if (IsNode(self)) {
      Sch_JS_Install(self, lodge, coma, leaf);
      /* returns back only if no alive local subtree found */
   } else if (Alive(Trunk(self))||Idle(Trunk(self))) {
     /* LEAF */
      eng_donate_state(LeafEngine(self),(st_handle_t *)lodge,
		(st_handle_t *)coma,(st_handle_t *)leaf);
      Scheduler(Site(self))->state_donate++;
      Sch_Unlock(self);
      return;
   }
   Smsg_ShortCut_End(self, lodge,
	sch_msg_hdl_js_install_fl(lodge, self, coma, leaf),
	sch_msg_snd_js_install_fl(lodge, self, coma, leaf)
       );
   return;
}


static void sch_msg_hdl_engine_migrate(self, leaf)
st_id_t *self, *leaf;
{
   Smsg_Hdl_Notify(SMSG_ENGINE_MIGRATE, self, leaf);
   assert(ComnNode(SubTree(self),leaf) && Lodged(Twig(self)));
   if (Exhausted(self)) {
      if (ComnSite(self,SupTree(self)) && !PoorSpine(self) && !JsRoot(self)) {
	 Smsg_ShortCut_End(self,SupTree(self),
	      sch_msg_hdl_js_prologue(SupTree(self),self,leaf),
	      sch_msg_snd_js_prologue(SupTree(self),self,leaf)
	     );
	 return;
      } else {
         void sch_msg_hdl_js_trav_up_();
         st_id_t coma; coma = *self;
         sch_msg_hdl_js_trav_up_(self,&coma,leaf);
         return;
      }
   } else {
      int alt = Tk_NextClause(self);
      Quit_Chain(Twig(self));
      Join_Chain_Next(Twig(self),Trunk(self)->prev);
      AliveTwigs(self)++;
      SetAlive(Twig(self));
      SetPoor(Twig(self));
      CheckLmp(self);
      Smsg_ShortCut_End(self,leaf,
	   sch_msg_hdl_js_success(leaf,self,alt,Twig(self)->info),
	   sch_msg_snd_js_success(leaf,self,alt,Twig(self)->info)
	  );
      return;
   }
}

static void sch_msg_hdl_js_install_fl(self, child, coma, leaf)
st_id_t *self, *child, *coma, *leaf;
{
   Smsg_Hdl_Notify(SMSG_JS_INSTALL_FL, self, child);
   if (Exhausted(self)) {
      st_id_t garbage; garbage = *self; 
      Quit_Chain(Twig(self));
      Sch_JS_Trav_Down(self,coma,leaf);
      Sch_JS_Trav_Up(self,coma,leaf);
      Sch_Gc_Edge(Site(&garbage),Twig(&garbage));
   } else {
      st_edge_t *x = Trunk(self)->prev;
      while (x != Trunk(self)) {
	if (Alive(x) || Idle(x)) {
           st_id_t twig; twig = *self;
	   PackEdge(&twig,x);
	   sch_msg_snd_js_install(SubTree(&twig),&twig,self,coma,leaf);
	   Sch_Unlock(self);
	   return;
	} else {
	   x = x->prev;
	}
      }
      assert(zero());
   }
}

static void sch_msg_hdl_load_report(self, child)
st_id_t *self, *child;
{
   Smsg_Hdl_Notify(SMSG_LOAD_REPORT, self, child);
   if (!Alive(Twig(self))) {
      Sch_Unlock(self);
      return;
   }
   /* the assertion below may not hold for the local leaf: when 
   ** the child is a local node, and the local leaf, after finishing
   ** traversal of the whole subtree, will reset it (trunk) poor, but it
   ** will not reset poor the suptree twig as other js_trav leaves as
   ** it does not go up, instead, suspends itself at the local node.
   */
   /*assert(!Rich(Twig(self)));*/
   SetRich(Twig(self));
   if (JsRoot(self) && Monad(self) && Exhausted(self)) {
      assert(Rich(Trunk(self)));
      Smsg_ShortCut_Mid(self, SubTree(self),
	   sch_msg_hdl_set_js_root(SubTree(self), self),
	   sch_msg_snd_set_js_root(SubTree(self), self)
	  );
   }
   if (Suspended(self)) {
      st_susp_t *x;
      assert(JsRoot(self) || Local(self));
      while (x = Suspended(self)) {
	 Smsg_ShortCut_Mid((self),SubTree(self),
	    sch_msg_hdl_js_trav_dn(SubTree(self), self, &(x->coma), &(x->leaf)),
	    sch_msg_snd_js_trav_dn(SubTree(self), self, &(x->coma), &(x->leaf))
	     );
	 Suspended(self) = x->next;
	 Sch_Gc_Suspended(Site(self),x);
      }
   }
   if (!Rich(Trunk(self))) { /* further report up */
      assert(!JsRoot(self)); 
      SetRich(Trunk(self));
      Smsg_ShortCut_End(self, SupTree(self),
	   sch_msg_hdl_load_report(SupTree(self), self),
	   sch_msg_snd_load_report(SupTree(self), self)
	  );
      return;
   } else {
      Sch_Unlock(self);
      return;
   }
}

/* Down */
static void sch_msg_hdl_set_js_root(self, parent)
st_id_t *self, *parent;
{
   Smsg_Hdl_Notify(SMSG_SET_JS_ROOT, self, parent);
   if (Rich(Trunk(self))) {
      SetJsRoot(self);
      /* assert(!Suspended(self) || Local(self)); */
      if (Monad(self) && Exhausted(self) && !Suspended(self)) {  
 	 st_id_t twig;
 	 st_edge_t *x = Trunk(self)->next;
 	 while (x != Trunk(self)) {
    	   if (Alive(x)) {
              twig = *self;
              PackEdge(&twig,x);
	      Smsg_ShortCut_End(&twig, SubTree(&twig),
		   sch_msg_hdl_set_js_root(SubTree(&twig), &twig),
		   sch_msg_snd_set_js_root(SubTree(&twig), &twig)
		  );
	      return;
           }
           x = x->next;
         }
         assert(zero());
      }
   }
   Sch_Unlock(self);       
   return;     
}

/* Up */
static void sch_msg_hdl_stop_idle(self,leaf)
st_id_t *self, *leaf;
{
   Smsg_Hdl_Notify(SMSG_STOP_IDLE, self, leaf);
   if (!Alive(Trunk(self))) {
      if (Chopped(Trunk(self))) {
	 Sch_Unlock(self);
	 return;
      } else {
         assert(Dying(Trunk(self)));
         assert(Idle(Twig(self)));
         SetNalive(Twig(self),ST_LODGED);
         if (ComnSite(self,SupTree(self)) && !PoorSpine(self) &&
					     !PoorSpine(SupTree(self))) {
	    Smsg_ShortCut_End(self, SupTree(self),
	         sch_msg_hdl_js_prologue(SupTree(self),self,leaf),
	         sch_msg_snd_js_prologue(SupTree(self),self,leaf)
	        );
            return;
         } else {
	    Smsg_ShortCut_End(self, SupTree(self),
	         sch_msg_hdl_lodge(SupTree(self),self,leaf),
	         sch_msg_snd_lodge(SupTree(self),self,leaf)
	        );
	    return;
         }
      }
   } else {
      int alt = Bk_NextClause(self);
      alt = Tk_NextClause(self);
      /* the below assert relies on the assumption that an exhausted
      ** monad leaf not allowed to become idle.
      */ 
      assert(alt || AliveTwigs(self) || Local(self));
      assert(Idle(Twig(self)));
      SetNalive(Twig(self),ST_LODGED);
      if (alt && (AliveTwigs(self)||!Exhausted(self))) {
         Quit_Chain(Twig(self));
         Join_Chain_Next(Twig(self),Trunk(self)->prev);
         AliveTwigs(self)++;
         SetAlive(Twig(self));
         SetPoor(Twig(self));
         Smsg_ShortCut_End(self, leaf,
              sch_msg_hdl_js_success(leaf, self, alt, Twig(self)->info),
              sch_msg_snd_js_success(leaf, self, alt, Twig(self)->info)
             );
         return;
      } else if (alt || (!AliveTwigs(self)&&ComnSite(self,leaf))) {
	 SetNalive(Trunk(self),ST_DYING);
	 if (!alt && Hybrid(self)) alt = -1;
         Smsg_ShortCut_End(self, SupTree(self),
              sch_msg_hdl_straighten(SupTree(self),self,leaf,self,alt),
              sch_msg_snd_straighten(SupTree(self),self,leaf,self,alt)
             );
         return;
      } else if (ComnSite(self,SupTree(self)) && !PoorSpine(self) &&
                                                 !PoorSpine(SupTree(self))) {
         Smsg_ShortCut_End(self, SupTree(self),
              sch_msg_hdl_js_prologue(SupTree(self),self,leaf),
              sch_msg_snd_js_prologue(SupTree(self),self,leaf)
             );
         return;
      } else {
         st_id_t *coma = self;
         SetPoorSpine(self);
         Sch_JS_Trav_Down(self,coma,leaf);
         Sch_JS_Trav_Up(self,coma,leaf);
      }
   }
}

/* Up */
static void sch_msg_hdl_tell_idle(self,leaf)
st_id_t *self, *leaf;
{
   Smsg_Hdl_Notify(SMSG_TELL_IDLE, self, leaf);
   if (Alive(Twig(self))) {
      if (Monad(self) && Exhausted(self)) {
         /* cant become idle, has to take over sequential chpts */
         void sch_msg_hdl_backtrack_();
         sch_msg_hdl_backtrack_(self,leaf);
         return;
      }
      UpdateLmp(self);
      AliveTwigs(self)--;
      SetNalive(Twig(self),ST_IDLE); 
      Smsg_ShortCut_End(self, leaf,
	   sch_msg_hdl_idle_told(leaf, self),
	   sch_msg_snd_idle_told(leaf, self)
	  );
      return;
   } else {
      Sch_Unlock(self);
      return;
   }
}

/* Up */
static void sch_msg_hdl_lodge_idle(self,child,leaf)
st_id_t *self, *child, *leaf;
{
   Smsg_Hdl_Notify(SMSG_LODGE_IDLE,self,child);
   if (Alive(Trunk(self))) {
      st_id_t idle;
      Add_Lodge_Twig(&idle,leaf,Knot(self));
      SetNalive(Twig(&idle),ST_IDLE);
      Smsg_ShortCut_End(&idle, leaf,
           sch_msg_hdl_idle_told(leaf, &idle),
           sch_msg_snd_idle_told(leaf, &idle)
          );
      return;
   } else {
      assert(!SchRoot(self));
      Smsg_ShortCut_End(self,SupTree(self),
	   sch_msg_hdl_lodge_idle(SupTree(self),self,leaf),
	   sch_msg_snd_lodge_idle(SupTree(self),self,leaf)
          );
      return;
   }
}

/* Down */
static void sch_msg_hdl_idle_told(leaf, parent)
st_id_t *leaf, *parent;
{
   Smsg_Hdl_Notify(SMSG_IDLE_TOLD,leaf,parent);
   switch (Life(Trunk(leaf))) {
     case ST_BACKTRACK:
       if (Scheduler(Site(leaf))->idling)
	  /* Note: this bit is not reset when telling idle.
	  ** Therefore, if it becomes reset in the meantime,
          ** it must have consumed a waking request.
	  */
	  Scheduler(Site(leaf))->idling = 0;
       else
	  Scheduler(Site(leaf))->waking = 1;
       if (ComnNode(SupTree(leaf),parent))
          break;
     case ST_LODGED:
       Smsg_ShortCut_Mid(leaf, SupTree(leaf),
	    sch_msg_hdl_withered(SupTree(leaf), leaf),
	    sch_msg_snd_withered(SupTree(leaf), leaf)
	   );
       break;

     default:
	error("illegal leaf type");
   }
   *SupTree(leaf) = *parent;
   SetNalive(Twig(leaf),ST_IDLE); 
   if (Scheduler(Site(leaf))->waking) {
      Scheduler(Site(leaf))->waking = 0;
      /* emulate a backtrack */
      SetNalive(Trunk(leaf),ST_BACKTRACK);
      Smsg_ShortCut_End(leaf, SupTree(leaf),
           sch_msg_hdl_stop_idle(SupTree(leaf),leaf),
           sch_msg_snd_stop_idle(SupTree(leaf),leaf)
          );
      return;
   } else {
      eng_backtrack(LeafEngine(leaf), (st_handle_t *)parent, 0);
      Sch_Unlock(leaf);
      return;
   }
}


static void sch_msg_hdl_idle_eng(leaf, leaf0)
st_id_t *leaf, *leaf0;
{
   Smsg_Hdl_Notify(SMSG_IDLE_ENG, leaf, leaf0);
   if (Knot(SiteLeaf(Site(leaf)))!=Knot(leaf)) {
      sch_msg_snd_idle_eng(SiteLeaf(Site(leaf)),leaf);
      Sch_Unlock(leaf);
      return;
   }
   if (Scheduler(Site(leaf))->waking) {
      Scheduler(Site(leaf))->waking = 0;
      Sch_Unlock(leaf);
      return;
   } 
   Scheduler(Site(leaf))->idling = 1;
   if (Alive(Trunk(leaf))) {
      if (JsRoot(leaf)) {
         Sch_WakeUp_Suspended(leaf);
         Sch_Unlock(leaf);
         return;
      } else {
         int left;
         if (!eng_publish(LeafEngine(leaf),99999, &left) || !left) {
	    SetPoor(Trunk(SiteLeaf(Site(leaf))));
	    SetPoor(Twig(SupTree(SiteLeaf(Site(leaf)))));
         }
      }
   }
   if (Life(Trunk(leaf)) != ST_CUT) {
      Smsg_ShortCut_End(leaf, SupTree(leaf),
	   sch_msg_hdl_reduce_wk_up(SupTree(leaf), leaf),
	   sch_msg_snd_reduce_wk_up(SupTree(leaf), leaf)
	  );
      return;
   } else {
      st_id_t *olf = Corpse(leaf);
      Smsg_ShortCut_Mid(olf, SupTree(olf),
           sch_msg_hdl_reduce_wk_up(SupTree(olf), olf),
           sch_msg_snd_reduce_wk_up(SupTree(olf), olf)
          );
      Sch_Unlock(leaf);
      return;
   }
}

static void sch_msg_hdl_reduce_wk_up(self,child)
st_id_t *self, *child;
{
   Smsg_Hdl_Notify(SMSG_REDUCE_WK_UP,self,child);
   if (Alive(Trunk(self)) && JsRoot(self)) {
      if (Suspended(self)) {
	 Sch_WakeUp_Suspended(self);
	 Sch_Unlock(self);
	 return;
      }
      if (!Alive(Twig(self)) && Monad(self) && Exhausted(self)) {
	 st_edge_t *x = Trunk(self)->next;
         while (x != Trunk(self)) {
	    if (Alive(x)) {
	       if (!Rich(x)) break;
	       else {
	          st_id_t twig;
	          twig = *self;
	          PackEdge(&twig,x);
	          Smsg_ShortCut_End(&twig, SubTree(&twig),
		       sch_msg_hdl_reduce_wk_dn(SubTree(&twig),&twig),
		       sch_msg_snd_reduce_wk_dn(SubTree(&twig),&twig)
	              );
	          return;
	       }
	    }
	    x = x->next;
         }
      }
      Sch_Unlock(self);
      return;
   } else {
     assert(!Suspended(self));
     Smsg_ShortCut_End(self,SupTree(self),
           sch_msg_hdl_reduce_wk_up(SupTree(self),self),
           sch_msg_snd_reduce_wk_up(SupTree(self),self)
          );
     return;
   }
}

static void sch_msg_hdl_reduce_wk_dn(self, parent)
st_id_t *self, *parent;
{
   Smsg_Hdl_Notify(SMSG_REDUCE_WK_DN, self, parent);
   if (!Alive(Trunk(self))) {
      Smsg_ShortCut_End(self,SupTree(self),
           sch_msg_hdl_reduce_wk_up(SupTree(self),self),
           sch_msg_snd_reduce_wk_up(SupTree(self),self)
          );
      return;
   }
   if (JsRoot(self)) {
      if (Suspended(self)) {
         Sch_WakeUp_Suspended(self);
         Sch_Unlock(self);
         return;
      }
      if (Monad(self) && Exhausted(self)) {
	 st_edge_t *x = Trunk(self)->next;
	 while (x != Trunk(self)) {
	    if (Alive(x)) {
	       if (!Rich(x)) break;
               else {
                  st_id_t twig;
                  twig = *self;
                  PackEdge(&twig,x);
                  Smsg_ShortCut_End(&twig, SubTree(&twig),
                       sch_msg_hdl_reduce_wk_dn(SubTree(&twig),&twig),
                       sch_msg_snd_reduce_wk_dn(SubTree(&twig),&twig)
                      );
                  return;
               }
            }
            x = x->next;
         }
      }
   }
   Sch_Unlock(self);
   return;
}



static void sch_msg_hdl_wake_eng(leaf, leaf0)
st_id_t *leaf, *leaf0;
{
   Smsg_Hdl_Notify(SMSG_WAKE_ENG, leaf, leaf0);
   if (Knot(SiteLeaf(Site(leaf)))!=Knot(leaf)) {
      sch_msg_snd_wake_eng(SiteLeaf(Site(leaf)),leaf);
      Sch_Unlock(leaf);
      return;
   }
   if (Idle(Trunk(leaf))) {
      /* emulate a backtrack */
      SetNalive(Trunk(leaf),ST_BACKTRACK);
      Smsg_ShortCut_End(leaf, SupTree(leaf),
           sch_msg_hdl_stop_idle(SupTree(leaf),leaf),
	   sch_msg_snd_stop_idle(SupTree(leaf),leaf)
          );
      return;
   }
   if (Scheduler(Site(leaf))->idling) {
      Scheduler(Site(leaf))->idling = 0;
      Sch_Unlock(leaf);
      return;   
   }
   if (Scheduler(Site(leaf))->waking) {
#if defined(SDEBUG)      
#else /* SDEBUG */
   (void) fprintf(stderr, "Warning: waking requests overlapped !");
#endif /* SDEBUG */
      Sch_Unlock(leaf);
      return;
   } else if (Lodged(Trunk(leaf))) {
       Scheduler(Site(leaf))->waking = 1;
       Sch_Unlock(leaf);
       return;
   } else
       (void) fprintf(stderr, "Warning: waking a non-idle engine");
}

/* Down */
static void sch_msg_hdl_lmp(self, parent)
st_id_t *self, *parent;
{
   Smsg_Hdl_Notify(SMSG_LMP, self, parent);
   if (Alive(Trunk(self))) {
      SetEldest(Trunk(self));
      if (IsLeaf(self)) {
	 if (Scheduler(Site(self))->lmp) {
	    Scheduler(Site(self))->lmp = 0;
	    eng_lmp(LeafEngine(self));
	 }
      } else if (AliveTwigs(self)) {
	 /* when equal 0, this the case of engine idling */
	 st_id_t twig;
         st_edge_t *x = Trunk(self)->next;
         while (x != Trunk(self)) {
           if (Alive(x)) {
              twig = *self;
              PackEdge(&twig,x);
	      SetEldest(x);
              Smsg_ShortCut_End(&twig, SubTree(&twig),
                   sch_msg_hdl_lmp(SubTree(&twig), &twig),
                   sch_msg_snd_lmp(SubTree(&twig), &twig)
                  );
              return;
           }
           x = x->next;
         }
         assert(zero());
      }
   }
   Sch_Unlock(self);
   return;
}


void sch_idle_eng(site)
site_id_t site;
{
   /*
   ** 1. to fit with a general tracing utility, the leaf doubled;
   ** 2. the short-up optimization is skipped here.
   */
   sch_msg_snd_idle_eng(SiteLeaf(site), SiteLeaf(site));
   return;
}

void sch_wake_eng(site)
site_id_t site;
{
   /*
   ** 1. to fit with a general tracing utility, the leaf doubled;
   ** 2. the short-up optimization is skipped here.
   */ 
   sch_msg_snd_wake_eng(SiteLeaf(site), SiteLeaf(site));
   return;
}

void sch_reduce_worker(site)
aport_id_t site;
{
   st_id_t *leaf = SiteLeaf(site);
   assert(Alive(Trunk(leaf)));
   if (JsRoot(leaf)) {
      Sch_WakeUp_Suspended(leaf);
      return;
   }
   Smsg_ShortCut_Mid(leaf,SupTree(leaf),
        sch_msg_hdl_reduce_wk_up(SupTree(leaf),leaf),
        sch_msg_snd_reduce_wk_up(SupTree(leaf),leaf)
       );
   return;
}

void sch_engine_migrate(leaf_hdl,where_hdl)
const st_handle_t *leaf_hdl, *where_hdl;
{
   st_id_t *leaf  = (st_id_t *)leaf_hdl;
   st_id_t *where = (st_id_t *)where_hdl;
   assert(!Alive(Trunk(leaf))&&!ComnNode(SupTree(leaf),where));
   Smsg_ShortCut_Mid(leaf,SupTree(leaf),
	sch_msg_hdl_withered(SupTree(leaf),leaf),
	sch_msg_snd_withered(SupTree(leaf),leaf)
       );
   *SupTree(leaf) = *where;
   Smsg_ShortCut_Mid(leaf,where,
	sch_msg_hdl_engine_migrate(where,leaf),
	sch_msg_snd_engine_migrate(where,leaf)
       );
   return;
}


/*ARGSUSED*/
void eng_lmp(engine) 
const eng_handle_t engine;
{
}

#if defined(PROLOG_LMP)
int sch_lmp(leaf)
const st_handle_t *leaf;
{
   if (Eldest(Trunk((st_id_t *) leaf)))
      return(1);
   Scheduler(Site(leaf))->lmp = 1;
   return(0);
}
#endif /* PROLOG_LMP */



void smsg_stat_init(site)
site_id_t site;
{
   int i;

   Scheduler(site)->state_donate     = 0;
   Scheduler(site)->smsg_count_hdl   = 0;
   Scheduler(site)->smsg_count_snd   = 0;
   Scheduler(site)->smsg_count_intra = 0;
   Scheduler(site)->smsg_count_intra_shortcut = 0;
   for (i = SMSG_MAXNUM; i>=0; i--)
      Scheduler(site)->smsg_subcount[i] = 0;

   smsg_name[SMSG_INIT_LODGE]     = "INIT_LODGE   ";
   smsg_name[SMSG_BACKTRACK]      = "BACKTRACK    ";
   smsg_name[SMSG_STRAIGHTEN]     = "STRAIGHTEN   ";
   smsg_name[SMSG_CUT]            = "CUT          ";
   smsg_name[SMSG_CUT_OK]         = "CUT_OK       ";
   smsg_name[SMSG_CHOP]           = "CHOP         ";
   smsg_name[SMSG_WITHERED]       = "WITHERED     ";
   smsg_name[SMSG_LODGE]          = "LODGE        ";
   smsg_name[SMSG_LODGED]         = "LODGED       ";
   smsg_name[SMSG_DEC_CORPSE]     = "DEC_CORPSE   ";
   smsg_name[SMSG_JS_PROLOGUE]    = "JS_PROLOGUE  ";
   smsg_name[SMSG_JS_TRAV_UP]     = "JS_TRAV_UP   ";
   smsg_name[SMSG_JS_TRAV_DN]     = "JS_TRAV_DN   ";
   smsg_name[SMSG_JS_INSTALL]     = "JS_INSTALL   ";
   smsg_name[SMSG_JS_INSTALL_FL]  = "JS_INSTALL_FL";
   smsg_name[SMSG_JS_SUCCESS]     = "JS_SUCCESS   ";
   smsg_name[SMSG_JS_TRUST]       = "JS_TRUST     ";
   smsg_name[SMSG_JS_IN_VAIN]     = "JS_IN_VAIN   ";
   smsg_name[SMSG_JS_AGAIN]       = "JS_AGAIN     ";
   smsg_name[SMSG_LOAD_REPORT]    = "LOAD_REPORT  ";
   smsg_name[SMSG_SET_JS_ROOT]    = "SET_JS_ROOT  ";
   smsg_name[SMSG_IDLE_ENG]	  = "IDLE_ENG     ";
   smsg_name[SMSG_WAKE_ENG]	  = "WAKE_ENG     ";
   smsg_name[SMSG_TELL_IDLE]	  = "TELL_IDLE    ";
   smsg_name[SMSG_IDLE_TOLD]	  = "IDLE_TOLD    ";
   smsg_name[SMSG_LODGE_IDLE]	  = "LODGE_IDLE   ";
   smsg_name[SMSG_STOP_IDLE]	  = "STOP_IDLE    ";
   smsg_name[SMSG_LMP]	          = "LMP          ";
   smsg_name[SMSG_REDUCE_WK_UP]	  = "REDUCE_WK_UP ";
   smsg_name[SMSG_REDUCE_WK_DN]	  = "REDUCE_WK_DN ";
   smsg_name[SMSG_ENGINE_MIGRATE] = "ENGINE_MIGRATE";
   smsg_name[SMSG_MAXNUM]         = "MAXNUM       ";

   return;
}

int zero() {
   int i;
   i = 0;
   return(i);
}

void sch_break() {}

scheduler_t *site_scheduler(site)
aport_id_t site;
{
  return(Scheduler(site));
}

int get_smsg_max()
{
  return(SMSG_MAXNUM);
}


/*
sch_empty_queue0()
{
   static amsg_t msg;
   static smsg00_t *data;
   static amsg_type_t mdt_type;
   static amsg_count_t mdt_count;
   return amsg_peek( scheduler->port,
                        &msg,
                        (amsg_data_t * *)&data,
                        &mdt_type,
                        &mdt_count
                        );
}

sch_empty_queue()
{
   amsg_ret_t ret;
   int event;

   Disable_Int();
   ret = sch_empty_queue0();
   event = sch_msg_event();
   Enable_Int();
   assert(ret != AMSG_OK || event);
   return(ret != AMSG_OK || event);
}
*/
