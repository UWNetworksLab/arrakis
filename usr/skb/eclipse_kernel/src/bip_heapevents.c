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
 * Copyright (C) 1996-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*----------------------------------------------------------------------
 * System:	ECLiPSe Constraint Logic Programming System
 * Version:	$Id: bip_heapevents.c,v 1.1 2008/06/30 17:43:51 jschimpf Exp $
 *
 * Contents:	Built-ins for the heap-event-primitives
 *
 *		This file has been factored out of bip_record.c in 05/2006
 *----------------------------------------------------------------------*/

#include        <stdio.h>   /* for sprintf() */

#include	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include	"dict.h"


static dident	d_defers0_;

/*----------------------------------------------------------------------
 * Prolog heap events
 *----------------------------------------------------------------------*/

/* INSTANCE TYPE DECLARATION */

/* ****** See types.h ****** */

/* METHODS */

static void _free_heap_event(t_heap_event *event);
static t_heap_event * _copy_heap_event(t_heap_event *event);
static void _mark_heap_event(t_heap_event *obj);
static int _heap_event_set(t_ext_ptr h, int i, pword pw);
static pword _heap_event_get(t_ext_ptr h, int i);
static int _tostr_heap_event(t_heap_event *event, char *buf, int quoted);
static int _strsz_heap_event(t_heap_event *event, int quoted);


/* CLASS DESCRIPTOR (method table) */

t_ext_type heap_event_tid = {
    (void (*)(t_ext_ptr)) _free_heap_event,
    (t_ext_ptr (*)(t_ext_ptr)) _copy_heap_event,
    (void (*)(t_ext_ptr)) _mark_heap_event,
    (int (*)(t_ext_ptr,int)) _strsz_heap_event,
    (int (*)(t_ext_ptr,char *,int)) _tostr_heap_event,
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _copy_heap_event,
    0,	/* get */
    0	/* set */
};


static void
_free_heap_event(t_heap_event *event)	/* event != NULL */
{
    /* It is possible for the reference count to drop to -1
     * when freeing an embedded self-reference. The equality
     * test ensures the event is freed once and once only.
     */
    if (--event->ref_ctr == 0)
    {
	free_heapterm(&event->goal);
	hg_free_size(event, sizeof(t_heap_event));
#ifdef DEBUG_RECORD
	p_fprintf(current_err_, "\n_free_heap_event(0x%x)", event);
	ec_flush(current_err_);
#endif
    }
}

static t_heap_event *
_copy_heap_event(t_heap_event *event)	/* event != NULL */
{
    ++event->ref_ctr;
    return event;
}

static void
_mark_heap_event(t_heap_event *event)	/* event != NULL */
{
    /*
     * Since the heap event may contain embedded handles of itself,
     * we have to avoid looping: overwrite event->goal with nil for
     * the duration of the marking, and set it back afterwards.
     * This is safe because dictionary GC is atomic.
     */
    pword pw = event->goal;
    Make_Nil(&event->goal);
    mark_dids_from_heapterm(&pw);
    event->goal = pw;

    mark_dids_from_pwords(&event->module, &event->module + 1);
}

static int
_tostr_heap_event(t_heap_event *event, char *buf, int quoted) /* event != NULL */
{
#define STRSZ_EVENT 20
    sprintf(buf, "'EVENT'(16'%08x)", (int)(word) event);	/* possibly truncated */
    return STRSZ_EVENT;
}

static int
_strsz_heap_event(t_heap_event *event, int quoted)	/* event != NULL */
{
    return STRSZ_EVENT;
}


/* PROLOG INTERFACE */

/*
 * event_create(+Goal, +Options, -EventHandle, +Module)
 * event_retrieve(+EventHandle, -Goal, -Module)
 * event_enable(+Handle)
 * event_disable(+Handle)
 */


static int
p_event_create4(value vevent, type tevent, value vopt, type topt, value vhandle, type thandle, value vmodule, type tmodule)
{
    t_heap_event *event;
    pword hevent;
    int defers = 0;
    int res = PSUCCEED;

    Check_Ref(thandle);
    Check_Goal(tevent);
    Check_List(topt);

    while (IsList(topt))
    {
	pword *pw = vopt.ptr++;
	Dereference_(pw);
	Check_Atom(pw->tag);
	if (pw->val.did == d_defers0_)
	    defers = 1;
	else
	    { Bip_Error(RANGE_ERROR); }
	Dereference_(vopt.ptr);
	topt.all = vopt.ptr->tag.all;
	vopt.ptr = vopt.ptr->val.ptr;
	Check_List(topt);
    }

    /* Disable interrupts - this safeguards our Tom-foolery with
     * the reference counts and guards the event allocation from
     * aborts.
     */
    Disable_Int();

    event = (t_heap_event *)hg_alloc_size( sizeof(t_heap_event) );
    event->ref_ctr = 1;
    event->enabled = 1;
    event->defers = defers;
    event->module.tag = tmodule;
    event->module.val = vmodule;

    hevent = ec_handle(&heap_event_tid, (t_ext_ptr) event);

    /* Unify the handle before the heap copy in case it is embedded within 
     * the event
     */
    res = Unify_Pw(vhandle, thandle, hevent.val, hevent.tag);
    res = res == PSUCCEED ? create_heapterm(&event->goal, vevent, tevent) : res;

    if (res != PSUCCEED) {
	hg_free_size(event, sizeof(t_heap_event));
	Enable_Int();
	Bip_Error(res);
    }

    /* The goal *may* have an embedded reference to the handle,
     * the heap copy will have incremented the reference count to two.
     * As a result we reset it back to one here to ensure we avoid liveness
     * maintained by the embedded internal reference. 
     */
    event->ref_ctr = 1;

    Enable_Int();

    Succeed_;
}


static int
p_event_create(value vevent, type tevent, value vhandle, type thandle, value vmodule, type tmodule)
{
    pword opt;
    Make_Nil(&opt);
    return p_event_create4(vevent, tevent, opt.val, opt.tag, vhandle, thandle, vmodule, tmodule);
}


static int
p_event_retrieve(value vhandle, type thandle, value vgoal, type tgoal, value vmodule, type tmodule)
{
    t_heap_event *event;
    pword goal;

    Prepare_Requests;

    Get_Typed_Object(vhandle, thandle, &heap_event_tid, event);

    get_heapterm(&event->goal, &goal);

    /* Is the event enabled or disabled? */
    if (event->enabled) {
        Request_Unify_Pw(vgoal, tgoal, goal.val, goal.tag);
    } else {
	/* Event disabled, just return the goal as 'true' */
	Request_Unify_Atom(vgoal, tgoal, d_.true0)
    }

    Request_Unify_Pw(vmodule, tmodule, event->module.val, event->module.tag);

    Return_Unify;
}


static int
p_event_enable(value vhandle, type thandle)
{
    t_heap_event *event;

    Get_Typed_Object(vhandle, thandle, &heap_event_tid, event);

    /* If an event is in the event queue but has been disabled
     * then it must be removed before the event is re-enabled.
     */
    if (!event->enabled) 
    {
	purge_disabled_dynamic_events(event);
    }

    event->enabled = 1;

    Succeed_;
}


static int
p_event_disable(value vhandle, type thandle)
{
    t_heap_event *event;

    Get_Typed_Object(vhandle, thandle, &heap_event_tid, event);

    event->enabled = 0;

    Succeed_;
}


/*----------------------------------------------------------------------
 * Initialisation
 *----------------------------------------------------------------------*/

void
bip_heapevent_init(int flags)
{
    d_defers0_ = in_dict("defers", 0);

    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("event_create_", 3), p_event_create, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("event_create_", 4), p_event_create4, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("event_retrieve", 3), p_event_retrieve, B_UNSAFE|U_FRESH);
	(void) built_in(in_dict("event_enable", 1), p_event_enable, B_SAFE|U_NONE);
	(void) built_in(in_dict("event_disable", 1), p_event_disable, B_SAFE|U_NONE);
    }
}

