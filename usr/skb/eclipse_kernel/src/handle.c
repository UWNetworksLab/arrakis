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
 * Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe Kernel Module
 *
 * $Id: handle.c,v 1.1 2008/06/30 17:43:55 jschimpf Exp $
 *
 * Author:	Stefano Novello, IC-Parc
 *		Joachim Schimpf, IC-Parc
 *
 * Contents:	Module to deal with generic handles
 *		from Prolog to external objects
 *
 * Description:
 *
 * |------------|
 * | THANDLE    |                         |-----------|
 * |	   --------+                      |           |
 * |------------|  |   |-----------|      |           |
 *      ...        |   | TPTR      |      |  <data>   |      |----------|
 *                 |   |       ---------> |           |      |  ....    |
 * |------------|  |   | - - - - - |      |-----------|      |  mark()  |
 * | THANDLE    |  +-> | TEXTERN   |                         |  copy()  |
 * |	   ----------> |       ----------------------------> |  free()  |
 * |------------|      |-----------|                         |----------|
 *                     unique anchor         data             type desc
 *                      on global
 *
 * A module that uses handles will associate a data structure with the
 * handle, as well as a type descriptor (method table).
 *
 * Handles to external data are implemented using a unique "anchor"
 * on the global stack. The Prolog code references the anchor via
 * THANDLE pointers.
 *
 * The anchor has two components:
 *
 *	t_ext_type *ExternalClass(p)	points to a method table (class)
 *	t_ext_ptr   ExternalData(p)	points to the external data
 *
 * The ExternalData() field can point to arbitrary external data.
 * The ExternalClass() field points to a user-supplied descriptor
 * which is a table of methods for standard operations on the data.
 * 
 * When the last THANDLE referring to an anchor disappears it will
 * eventually be garbage collected.  When the anchor disappears,
 * the external object's free() method gets invoked.
 *
 * If the object is manually freed before the anchor disappears, the
 * anchor becomes stale. This is marked by overwriting ExternalData()
 * with NULL. Since the anchor is never copied, all accesses via
 * any THANDLE will see that this handle is stale.
 *
 * If an anchor gets physically copied, e.g. by setval, record, etc,
 * the copy() or remote_copy() method is used to inform the external object.
 *
 * Other methods for comparing, printing and gc can be specified.
 */

#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "error.h"
#include "io.h"
#include "dict.h"
#include "database.h"
#include "emu_export.h"


/*
 * Call cleanup method (if any) and mark handle as stale (of not already)
 * (pw)[1].val.ptr is ExternalData(pw) expansion to satisfy gcc-4.x
 */
#define AnchorFree(pw) { \
	if (ExternalData(pw)) { \
	    if (ExternalClass(pw)->free) \
		ExternalClass(pw)->free(ExternalData(pw)); \
	    (pw)[1].val.ptr = (pword *) NULL; \
	} \
}


/*
 * Function to free the handle on untrailing
 */
/*ARGSUSED*/
static void
_handle_cleanup(pword *pw,
	word *pdata,
	int size,	/* unused (untrail calling convention) */
	int flags)	/* unused (untrail calling convention) */
{
	if (!pw || DifferTypeC(pw->tag, TEXTERN))
	{
	    p_fprintf(current_err_, "ECLiPSe: handle_cleanup: invalid handle\n");
	    return;
	}
	AnchorFree(pw);
}


/*
 * Construct a new handle
 */
pword Winapi
ec_handle(const t_ext_type *class, const t_ext_ptr data)
{
	pword handle;
	pword *pw;

	/* push global stack anchor */
	pw = TG;
	TG += HANDLE_ANCHOR_SIZE;
	Check_Gc;
	pw[0].tag.kernel = TEXTERN;
	pw[0].val.ptr = (pword *) class;
	pw[1].tag.kernel = TPTR;
	pw[1].val.ptr = (pword *) data;

	/* Make handle */
	handle.tag.kernel = THANDLE;
	handle.val.ptr = pw;

	/* Trail cleanup */
	ec_trail_undo(_handle_cleanup, pw, NULL, NULL, 0, 0);

	return handle;
}


/*
 * Get the data pointer from a handle (expect the given type)
 */
int Winapi
ec_get_handle(const pword handle, const t_ext_type *cl, t_ext_ptr *data)
{
	const pword * pw = &handle;
	Dereference_(pw);
	Get_Typed_Object(pw->val, pw->tag, cl, *data);
	Succeed_;
}


/*
 * Free the handle eagerly (expect the given type)
 */
int Winapi
ec_free_handle(const pword handle, const t_ext_type *cl)
{
	const pword * pw = &handle;
	Dereference_(pw);
	Check_Typed_Object_Handle(pw->val,pw->tag,(t_ext_type *) cl);
	AnchorFree(pw->val.ptr);
	Succeed_;
}


/*
 * Free the handle eagerly (generic)
 */
int
p_handle_free(value v_handle, type t_handle)
{
	Check_Type(t_handle, THANDLE);
	Check_Type(v_handle.ptr->tag, TEXTERN);
	AnchorFree(v_handle.ptr);
	Succeed_;
}


/*
 * Arrange for the handle to get freed on cut
 */
int
p_handle_free_on_cut(value v_handle, type t_handle)
{
	Check_Type(t_handle, THANDLE);
	Check_Type(v_handle.ptr->tag, TEXTERN);

	schedule_cut_fail_action((void (*)(value,type)) p_handle_free,v_handle,t_handle);
	Succeed_;
}


/*
 * Copy an anchor
 */
void
handle_copy_anchor(
	pword *from,	/* a heap or global stack location */
	pword *to,	/* a heap or global stack location */
	int trail)	/* should be true if *to is a global stack location */
{
	to[0] = from[0];
	if (ExternalClass(from)->copy && ExternalData(from))
	    to[1].val.ptr = (pword *) ExternalClass(from)->copy(ExternalData(from));
	else
	    to[1].val.ptr = (pword *) ExternalData(from);
	to[1].tag.kernel = from[1].tag.kernel;

	/* Trail cleanup */
	if (trail)
	    ec_trail_undo(_handle_cleanup, to, NULL, NULL, 0, 0);
}

