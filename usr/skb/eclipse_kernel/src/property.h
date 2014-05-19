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
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * SEPIA INCLUDE FILE
 *
 * VERSION	$Id: property.h,v 1.1 2008/06/30 17:43:58 jschimpf Exp $
 */
 
/*************************************************************
 *
 *                 property.h
 *
 *************************************************************/
 
#define VISIBLE_PROP		0
#define LOCAL_PROP		1
#define GLOBAL_PROP		2

#define PROPERTY_ERROR		0

#define EVENT_PROP		1
#define HTABLE_PROP		2
#define GLOBVAR_PROP		3
#define ARRAY_PROP		4
#define IDB_PROP		5
#define MODULE_PROP		6
#define SYSCALL_PROP		7
#define STREAM_PROP		8
#define PREFIX_PROP		9
#define INFIX_PROP		10
#define POSTFIX_PROP		11
#define TRANS_PROP		12  /* must be consecutive, write macros odd */
#define WRITE_TRANS_PROP	13  /* some values used in kernel.pl !!!     */
#define GOAL_TRANS_PROP		14
#define WRITE_GOAL_TRANS_PROP	15
#define CLAUSE_TRANS_PROP	16
#define WRITE_CLAUSE_TRANS_PROP	17
#define SHELF_PROP		18

/* Create and set a global variable local to sepia_kernel		*/
#define	Set_Kernel_Var(var_name, vvalue, tvalue)			\
    {									\
	value kv; value v_type; value v_vis;				\
	v_type.did = d_.prolog;						\
	v_vis.did = d_.local0;					\
	kv.did = d_.kernel_sepia;					\
	(void) p_make_array_(var_name, tdict, v_type, tdict,		\
			     v_vis, tdict, kv, tdict);			\
	(void) p_setval_body(var_name, tdict, vvalue, tvalue, kv, tdict);\
    }

#define	Get_Kernel_Var(var_did, pointer)				\
	{								\
	    type kt;							\
	    int	res;							\
	    kt.kernel = ModuleTag(d_.kernel_sepia);			\
	    pointer = get_modular_property(var_did, GLOBVAR_PROP,	\
					   d_.kernel_sepia, kt,		\
					   VISIBLE_PROP, &res);		\
	}

#define TransfTermIn(tr)	((tr) + 4)
#define TransfTermOut(tr)	((tr) + 5)

#define GetPointerToRecord(functor, mod, mod_tag, perr)			\
    get_modular_property(functor, IDB_PROP, mod, mod_tag, VISIBLE_PROP, perr)

#define GlobalPrologRefIndexTag	TGRS
#define IsGlobalPrologRefIndex(p) SameTypeC((p)->tag, GlobalPrologRefIndexTag)

#define GlobalPrologRefTag   TGRL
#define IsGlobalPrologRef(p) SameTypeC((p)->tag, GlobalPrologRefTag)


Extern pword *	get_property ARGS((dident, int));
Extern pword *	set_property ARGS((dident, int));
Extern pword *	get_modular_property ARGS((dident, int, dident, type, int, int*));
Extern pword *	set_modular_property ARGS((dident, int, dident, type, int, int*));
Extern int	erase_property ARGS((dident, int));
Extern int	erase_modular_property ARGS((dident, int, dident, type, int));
Extern void	erase_module_props ARGS((property*));
Extern void	erase_records ARGS((pword*));
Extern void	mark_dids_from_properties ARGS((property*));

Extern void	get_heapterm ARGS((pword*, pword*));
Extern int	create_heapterm ARGS((pword*, value, type));
Extern void	free_heapterm ARGS((pword*));
Extern void	move_heapterm ARGS((pword*, pword*));
Extern void	make_heapterm_persistent ARGS((pword*));
Extern void	mark_dids_from_heapterm ARGS((pword*));
Extern void	set_string ARGS((pword*, char*));
Extern void	set_string_n ARGS((pword*, char*, int));

Extern uword *	get_elt_address ARGS((value, type, uword*, dident, type, int*));
Extern word	get_first_elt ARGS((pword*, pword*, uword*, uword*, dident, type));
Extern pword *	init_kernel_var ARGS((int flags, dident vdid, value v, type t));

