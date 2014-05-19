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

/***********************************************************************
**      System: Parallel ECLiPSe Scheduler
**        File: sch_eng_interface.h
**      Author: Liang-Liang Li
** Description: Interface Declarations for scheduler, engine, and worker 
**
***********************************************************************/

/* Two Subtrees share Common Root */
#define ComnNode(t1,t2) ((t1)->knot==(t2)->knot && (t1)->site==(t2)->site)

/*****  Interfaces: Worker Management Calls Scheduler  *****/
/***********************************************************/

#if defined(__STDC__)
void sch_port_upcall    (aport_id_t);
void sch_create_leaf    (aport_id_t, eng_handle_t, st_handle_t **);
void sch_create_root    (aport_id_t, st_handle_t *);
void sch_genesis        (st_handle_t *, st_handle_t *, st_handle_t *, int);
void sch_init_lodge	(aport_id_t, int, st_handle_t *);
void sch_idle_eng	(aport_id_t);
void sch_wake_eng 	(aport_id_t);
void sch_get_info	(aport_id_t, int *, void_ptr *);
void sch_set_info	(aport_id_t, void_ptr);
void sch_reduce_worker	(aport_id_t);
#else /* __STDC__ */
void sch_port_upcall    ();
void sch_create_leaf    ();
void sch_create_root    ();
void sch_genesis        ();
void sch_init_lodge	();
void sch_idle_eng       ();
void sch_wake_eng	();
void sch_get_info	();
void sch_set_info	();
void sch_reduce_worker	();
#endif /* __STDC__ */


/*****  Interfaces: Scheduler calls Worker Management *****/
/***********************************************************/

#if defined(__STDC__)
void wm_init_lodged	(int, st_handle_t *);
#else /* __STDC__ */
void wm_init_lodged	();
#endif /* __STDC__ */

/*****  Interfaces: Worker Management Calls Engine  ********/
/***********************************************************/

/*****  Interfaces: Engine Calls Scheduler             *****/
/***********************************************************/

#if defined(__STDC__)
extern void sch_load_report     (const st_handle_t *);
extern void sch_load_publish_one(const st_handle_t *, int, st_handle_t *, int);
extern void sch_backtrack       (const st_handle_t *);
extern void sch_cut             (const st_handle_t *, const st_handle_t *);
extern void sch_engine_migrate  (const st_handle_t *, const st_handle_t *);
#if defined(PROLOG_LMP)
extern int  sch_lmp             (const st_handle_t *);
#endif /* PROLOG_LMP */
extern void sch_sync_msg_hdls   (const st_handle_t *);
#else /* __STDC__ */
extern void sch_load_report     ();
extern void sch_load_publish_one();
extern void sch_backtrack       ();
extern void sch_cut             ();
extern void sch_engine_migrate  ();
#if defined(PROLOG_LMP)
extern int  sch_lmp             ();
#endif /* PROLOG_LMP */
extern void sch_sync_msg_hdls   ();
#endif /* __STDC__ */

/*****  Interfaces: Scheduler Calls Engine            *****/
/**********************************************************/

#if defined(__STDC__)
extern int  eng_publish         (eng_handle_t, int, int *);
extern void eng_backtrack       (eng_handle_t, const st_handle_t *, unsigned);
extern void eng_trust           (eng_handle_t, unsigned);
extern void eng_undo_publish    (eng_handle_t, const st_handle_t *);
extern void eng_cut_ok          (eng_handle_t, const st_handle_t *);
extern void eng_fail            (eng_handle_t);
extern void eng_stop            (eng_handle_t);
extern void eng_lmp             (const eng_handle_t);
extern void eng_donate_state    (eng_handle_t, const st_handle_t *,
				     const st_handle_t *, const st_handle_t *);
extern void eng_msg_trigger     (eng_handle_t);
extern void eng_msg_reset       (eng_handle_t);
#else /* __STDC__ */
extern int  eng_publish         ();
extern void eng_backtrack       ();
extern void eng_trust           ();
extern void eng_undo_publish    ();
extern void eng_cut_ok          ();
extern void eng_fail            ();
extern void eng_stop            ();
extern void eng_lmp             ();
extern void eng_donate_state    ();
extern void eng_msg_trigger     ();
extern void eng_msg_reset       ();
#endif /* __STDC__ */

