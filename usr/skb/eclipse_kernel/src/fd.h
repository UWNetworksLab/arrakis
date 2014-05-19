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
 * Copyright (C) 1993-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: fd.h,v 1.1 2008/06/30 17:43:55 jschimpf Exp $
 */

/****************************************************************************
 *
 *		SEPIA definitions for finite domain constraints
 *
 *****************************************************************************
 *
 * Author: Micha Meier
 *
 */

/*
 * OFFSETS	Must correspond to define_struct in fd_domain.pl!
 */
#define DOMAIN_OFF		1
#define MIN_OFF			2
#define MAX_OFF			3
#define ANY_OFF			4

/*
 * Return Codes
 */
#define RES_NO_CHANGE		0
#define RES_MIN			1
#define RES_X			RES_MIN
#define RES_MAX			2
#define RES_Y			RES_MAX
#define RES_ANY			(RES_MIN|RES_MAX)	/* also means MIN+MAX */
#define RES_INSTANTIATED	4
#define RES_GROUND		5
#define RES_SOLVED		RES_GROUND
#define RES_FAIL		11
#define RES_ERROR		6
#define RES_AGAIN		RES_ERROR
#define RES_AGAIN_NEG		7
#define RES_INSTANTIATED_X	RES_AGAIN_NEG
#define RES_INSTANTIATED_Y	8
#define RES_EVAL		9
#define RES_SIMPLE		10

#define RES_DELAY		RES_NO_CHANGE
#define RES_DELAY_WAKE		1
#define RES_WAKE		2

#define POSTPONE_PRIO		7

#define Check_Domain(v, t)	if (!IsStructure(t) || v.ptr->val.did != d_dom) { Bip_Error(TYPE_ERROR)}

#define Var_Attr(var, attr)						\
				attr = MetaTerm(var); /* attribute */	\
				Dereference_(attr);			\
				attr = attr->val.ptr + domain_slot;	\
				Dereference_(attr);

#define Attr_Domain(attr, domptr)					\
				domptr = attr->val.ptr + DOMAIN_OFF;	\
				Dereference_(domptr); /* dom/2 */	\
				domptr = domptr->val.ptr;

#define Var_Domain(var, domptr)						\
				Var_Attr(var, domptr)			\
				Attr_Domain(domptr, domptr)

#define Var_Domain_Check(var, domptr)					\
				Var_Attr(var, domptr)			\
				if (!IsStructure(domptr->tag)) {	\
				    domptr = 0;				\
				} else {				\
				    Attr_Domain(domptr, domptr)		\
				}

#define Check_Return(res)	if ((res) < 0) { Bip_Error(res) }
#define Check_Meta(item)			\
	if (!IsMeta(item)) {			\
	    Error_If_Ref(item);			\
	    Bip_Error(TYPE_ERROR)		\
	}

#define Check_Dvar(vptr, p) 						\
				Var_Attr(vptr, p)			\
				if (!IsStructure(p->tag)) {		\
				    Bip_Error(INSTANTIATION_FAULT)	\
				}

#define IsFdInterval(v, t)	(IsCompound(t) && (v).ptr->val.did == d_interval)
#define Check_Element(v, t)	Error_If_Ref(t);		\
				if (IsFdInterval(v, t)) {	\
				    Bip_Error(TYPE_ERROR)	\
				}

/* This may yield an approximate result, so reals are omitted */
#define ElemEq(ptr, v, t)	\
	(IsTag(ptr->tag.kernel, TINT) && IsTag(t, TINT) ? (ptr->val.nint == v)\
	:\
	SameTypeC(ptr->tag, t) && ((ptr->val.nint == v) || IsTag(t,TNIL)))

/*
 * EXTERNAL VARIABLE DECLARATIONS:
 */
extern int	domain_slot;

extern int	ec_assign();
extern int	meta_index();
extern int	make_kernel_array();
extern pword	*get_kernel_array();

extern int	dom_range ARGS((pword*,long*,long*));
extern int	dom_check_in ARGS((long,type,pword*));
extern int	dom_remove_element ARGS((pword*,long,long,pword*));

