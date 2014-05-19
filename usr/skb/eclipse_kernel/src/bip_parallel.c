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

/*
 * VERSION	$Id: bip_parallel.c,v 1.1 2008/06/30 17:43:52 jschimpf Exp $
 */

/* ********************************************************************
 *
 *	ECLiSPe builtins for the parallel version only
 *
 ******************************************************************** */


#include	"config.h"
#include        "sepia.h"
#include        "pds.h"		/* pds.h has to be placed before types.h */
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include        "io.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"property.h"
#include	"module.h"
#include	"opcode.h"


extern int	oracle_size();
extern void	retrieve_oracle();

extern amsg_t	par_goal_msg_;


/*------------------------------------------------------------------
 * Distributed bag primitives
 *
 * dbag_create(-Bag)
 * dbag_enter(+Bag, +Term)
 * dbag_dissolve(+Bag, -List)
 * CAUTION: The bag can no longer be accessed after bag_dissolve/2 !
 *
 * These are similar to the ones in bip_record.c.
 * The difference is that we convert the terms to dbformat (ie a string)
 * and fill them into a message buffer. This message may be sent via
 * the message passing system (if the bag is remote), and the bag itself
 * just consists of a linked list of the received message buffers.
 * dbag_dissolve converts back to terms, builds a list of them, and
 * frees the buffers.
 *------------------------------------------------------------------*/

typedef struct amsg_ref {
	amsg_t msg;
	struct amsg_ref *msg_data_hdr;
} amsg_ref_t;

typedef struct {
	amsg_ref_t first;
	amsg_ref_t *last;
} dbag_descr_t;

static void
dbag_port_upcall(aport_id_t bag_aport_id)
{
    dbag_descr_t *dbag_descr;
    amsg_ret_t ret;
    amsg_count_t size;
    amsg_t msg;
    amsg_data_t *msg_data;

    if (aport_get_option(bag_aport_id, APORT_DATA_PTR,
				(aport_optval_t *) &dbag_descr) != AMSG_OK)
    {
	p_fprintf(current_err_, "aport_get_option() failed\n");
	ec_flush(current_err_);
    }

    while ((ret = amsg_receive(bag_aport_id, &msg, &msg_data,
			    (amsg_type_t *) 0, &size, 0)) != AMSG_NOMESSAGE)
    {
	if (ret != AMSG_OK)
	{
	    p_fprintf(current_err_, "amsg_receive() failed\n");
	    ec_flush(current_err_);
	    continue;
	}

	/*
	 * Handle one message: Add message buffer to the bag
	 */
	dbag_descr->last->msg = msg;
	dbag_descr->last->msg_data_hdr = (amsg_ref_t *) msg_data;
	dbag_descr->last = (amsg_ref_t *) msg_data;
	dbag_descr->last->msg_data_hdr = &dbag_descr->first;
    }
}

static int
p_dbag_create(value vbag, type tbag)
{
    dbag_descr_t *dbag_descr;
    aport_id_t bag_aport_id;

#ifndef lint
    if (sizeof(aport_id_t) > sizeof(value))
    {
	Bip_Error(UNIMPLEMENTED);	/* can't pack aport_id in integer */
    }
#endif

    if (aport_allocate(&bag_aport_id, dbag_port_upcall) != AMSG_OK)
	{ Bip_Error(MPS_ERROR); }
    if (aport_set_option(bag_aport_id, APORT_NOTIFY_LEVEL, (aport_optval_t) 3)
	    != AMSG_OK)
	{ Bip_Error(MPS_ERROR); }

    dbag_descr = (dbag_descr_t *) hp_alloc_size(sizeof(dbag_descr_t));
    dbag_descr->last = dbag_descr->first.msg_data_hdr = &dbag_descr->first;

    if (aport_set_option(bag_aport_id, APORT_DATA_PTR,
				(aport_optval_t) dbag_descr) != AMSG_OK)
	{ Bip_Error(MPS_ERROR); }

    Return_Unify_Integer(vbag, tbag, (long) bag_aport_id);
}

static int
p_dbag_enter(value vbag, type tbag, value vterm, type tterm)
{
    aport_id_t	bag_aport_id;
    pword	term, *term_as_bytes;
    pword	*old_tg = TG;
    amsg_size_t	msg_size;
    amsg_t	msg;
    amsg_data_t *msg_data;

    Check_Integer(tbag);
    bag_aport_id = (aport_id_t) vbag.nint;

    /* encode the term */
    term.val.all = vterm.all;
    term.tag.kernel = tterm.kernel;
    term_as_bytes = term_to_dbformat(&term, D_UNKNOWN);

    /* fill into a message buffer */
    msg_size = BufferSize(term_as_bytes) + sizeof(amsg_ref_t);
    if (amsg_alloc(msg_size, &msg_data, &msg) != AMSG_OK)
    {
	Bip_Error(MPS_ERROR);
    }
    bmem_cpy((generic_ptr) ((char *) msg_data + sizeof(amsg_ref_t)),
	    (generic_ptr) BufferStart(term_as_bytes),
	    (bmem_size_t) BufferSize(term_as_bytes));
    TG = old_tg;	/* pop the temporary stack string */

    /* send the message */
    if (amsg_send(bag_aport_id, msg, MDT_BYTE, (amsg_count_t) msg_size, 0) != AMSG_OK)
    {
	Bip_Error(MPS_ERROR);
    }
    Succeed_;
}

/*
 * Must be called on the worker that created the bag, no check yet!
 */
static int
p_dbag_dissolve(value vdbag, type tdbag, value vl, type tl)
{
    aport_id_t bag_aport_id;
    dbag_descr_t *dbag_descr;
    amsg_t this_msg;
    amsg_ref_t *this_msg_data_hdr;
    pword list;
    register pword *car, *cdr;

    Check_Integer(tdbag);
    bag_aport_id = (aport_id_t) vdbag.nint;
    Check_Output_List(tl);
    if (aport_get_option(bag_aport_id, APORT_DATA_PTR,
				(aport_optval_t *) &dbag_descr) != AMSG_OK)
    {
	Bip_Error(MPS_ERROR);
    }

    this_msg = dbag_descr->first.msg;
    this_msg_data_hdr = dbag_descr->first.msg_data_hdr;
    hp_free_size((generic_ptr) dbag_descr, sizeof(dbag_descr_t));
    cdr = &list;
    while (this_msg_data_hdr != &dbag_descr->first)
    {
	pword *pw1;
	amsg_t old_msg;

        car = TG;
        Push_List_Frame();
        Make_List(cdr, car);
	cdr = car + 1;

	pw1 = dbformat_to_term((char*)(this_msg_data_hdr+1), D_UNKNOWN, tdict);
	if (!pw1)
	{
	    value va;
	    va.did = d_.abort;
	    Bip_Throw(va, tdict);
	}
	car->val.ptr = pw1->val.ptr;
	car->tag.kernel = pw1->tag.kernel;

	old_msg = this_msg;
	this_msg = this_msg_data_hdr->msg;
	this_msg_data_hdr = this_msg_data_hdr->msg_data_hdr;
	(void) amsg_free(old_msg);
    }
    Make_Nil(cdr);
    if (aport_deallocate(bag_aport_id) != AMSG_OK)
    {
	Bip_Error(MPS_ERROR);
    }
    Return_Unify_Pw(vl, tl, list.val, list.tag);
}


/*------------------------------------------------------------------
 * Oracle-related builtins
 *------------------------------------------------------------------*/

#ifdef NEW_ORACLE

/* 
 * get_oracle(+FromChp, +ToChp, -Oracle)
 * install_oracle(+Oracle)
 *
 * These two were used for testing the oracle implementation.
 */
/*ARGSUSED*/
static int
p_get_oracle3(value vfrom, type tfrom, value vto, type tto, value v, type t)
{
    pword *b_aux;
    char *buf;
    int size;

    if (IsRef(tto))
    {
	b_aux = B.args;
	while (!IsParallelFrame(BTop(b_aux)))
	    b_aux = BPrev(b_aux);
    }
    else
	b_aux = vto.ptr;

    size = oracle_size(vfrom.ptr, b_aux);
    buf = (char *) hp_alloc(size);
    retrieve_oracle(buf, size, vfrom.ptr, b_aux);

    Return_Unify_Integer(v, t, (long) buf);
}

static int
p_install_oracle(value v, type t)
{
    Check_Integer(t);
    PO = FO = v.str;
    NTRY = 0;
    Succeed_;
}

/*
 * This is supposed to be called after the initialization goal has been
 * executed and the proper reexecution starts. It sets the FO register.
 */
static int
p_install_pending_oracle(void)
{
    if (FO || !PO)
    {
	 Bip_Error(RECOMP_FAILED);
    }
    FO = PO;
    NTRY = 0;
    Succeed_;
}

/*
 * Check whether we are inside the recomputation phase (including
 * the execution of the initialization goal)
 */
static int
p_recomputing(void)
{
    Succeed_If(PO);
}

/*
 * This is a primitive to help debugging in case recomputation goes wrong.
 * A call to oracle_check(N) inserts special checkpoint entries with integer
 * identifier N into the oracle. During recomputation this identifier is
 * checked, which helps tracking down where the oracle gets out of phase.
 */
static int
p_oracle_check(value v, type t)
{
    Check_Integer(t);
    if (FO)
    {
	int i = FoHeader(FO);
	if (!FoIsChk(i) || FoAlt(FO,i) != v.nint)
	{
	     PO = FO;	/* for debugging and to prevent message handling */
	     FO = (char *) 0;
	     Bip_Error(RECOMP_FAILED);
	}
    }
    if (TO)
    {
	Record_Alternative(v.nint, O_CHK_ORACLE);
    }
    Succeed_;
}

static int
p_set_par_goal(value v, type t)
{
    pword *old_tg = TG;
    pword term, *term_as_bytes;
    amsg_data_t *msg_data;

    if (par_goal_msg_)
	(void) amsg_free(par_goal_msg_);
    
    /* encode the term */
    term.val.all = v.all;
    term.tag.kernel = t.kernel;
    term_as_bytes = term_to_dbformat(&term, D_UNKNOWN);

    /* fill into a message buffer */
    if (amsg_alloc((amsg_size_t) BufferSize(term_as_bytes), &msg_data, &par_goal_msg_)
	!= AMSG_OK)
    {
	Bip_Error(MPS_ERROR);
    }
    bmem_cpy(	(generic_ptr) msg_data,
		(generic_ptr) BufferStart(term_as_bytes),
		(bmem_size_t) BufferSize(term_as_bytes));
    TG = old_tg;	/* pop the temporary stack string */
    Succeed_;
}

static int
p_get_par_goal(value v, type t)
{
    pword *pw1;

    if (!par_goal_msg_) { Fail_; }

    pw1 = dbformat_to_term((char*) amsg_data(par_goal_msg_), D_UNKNOWN, tdict);
    if (!pw1)
    {
	value va;
	va.did = d_.abort;
	Bip_Throw(va, tdict);
    }
    Return_Unify_Pw(v, t, pw1->val, pw1->tag);
}

#endif /* NEW_ORACLE */


/*------------------------------------------------------------------
 * Initialization code
 *------------------------------------------------------------------*/

void
bip_parallel_init(int flags)
{
    if (flags & INIT_SHARED)
    {
#ifdef NEW_ORACLE
	(void) exported_built_in(in_dict("get_oracle", 3),
				p_get_oracle3, B_UNSAFE);
	(void) exported_built_in(in_dict("install_oracle", 1),
				p_install_oracle, B_SAFE);
	(void) exported_built_in(in_dict("install_pending_oracle", 0),
				p_install_pending_oracle, B_SAFE);
	(void) built_in(in_dict("recomputing", 0),
				p_recomputing, B_SAFE);
	(void) exported_built_in(in_dict("oracle_check", 1),
				p_oracle_check, B_UNSAFE);
	(void) local_built_in(in_dict("set_par_goal", 1),
				p_set_par_goal, B_UNSAFE);
	(void) local_built_in(in_dict("get_par_goal", 1),
				p_get_par_goal, B_UNSAFE|U_UNIFY);
#endif
	(void) local_built_in(in_dict("dbag_create", 1),
				p_dbag_create, B_SAFE|U_GROUND);
	(void) local_built_in(in_dict("dbag_enter", 2),
				p_dbag_enter, B_SAFE|U_NONE);
	(void) local_built_in(in_dict("dbag_dissolve", 2),
				p_dbag_dissolve, B_UNSAFE|U_UNIFY);
    }
}


