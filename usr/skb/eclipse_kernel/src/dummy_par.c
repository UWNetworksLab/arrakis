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

/*
 * ECLiPSe C SOURCE MODULE
 *
 * VERSION	$Id: dummy_par.c,v 1.1 2008/06/30 17:43:53 jschimpf Exp $
 */

/*
 * IDENTIFICATION	dummy_par.c
 *
 * DESCRIPTION		Dummy functions related to parallelism
 *
 */
#include "config.h"
#include "ec_public.h"
#include "error.h"

int eng_root_branch;

int
par_present() { return 0; }

void
get_job() {}

cut_public() {return 0;}

/*VARARGS*/
int
eng_publish()
{ return 0; }

/*VARARGS*/
void
sch_load_report()
{}

/*VARARGS*/
void
parallel_init()
{}

/*VARARGS*/
void
bip_parallel_init()
{}

/*VARARGS*/
void
halt_system()
{}

/*VARARGS*/
void
panic_halt_system()
{}

/*VARARGS*/
void
worker_init()
{}

/*VARARGS*/
int
get_root_id()
{ return 0; }


double
elapsed_session_time()
{ return 0.0; }

/*VARARGS*/
int
p_wm_get()
{ return NOT_AVAILABLE; }

/*VARARGS*/
int
p_wm_get_ids()
{ return NOT_AVAILABLE; }

/*VARARGS*/
int
p_wm_set()
{ return NOT_AVAILABLE; }

/*VARARGS*/
int
p_wm_interface()
{ return NOT_AVAILABLE; }

/*VARARGS*/
int
p_worker_stat()
{ return NOT_AVAILABLE; }

/*VARARGS*/
int
p_worker_stat_reset()
{ return NOT_AVAILABLE; }

/*VARARGS*/
void
short_sleep()
{}

/*VARARGS*/
void
setup_mps()
{}

void
exit_mps()
{}

/*VARARGS*/
void
eng_msg_loop()
{}

void
msg_nopoll()
{}

/*VARARGS*/
void
my_io_aport()
{}

/*VARARGS*/
int
io_rpc()
{ return PSUCCEED; }

/*VARARGS*/
void
worker_bport_ack()
{}

/*VARARGS*/
void
worker_bport_notify()
{}
