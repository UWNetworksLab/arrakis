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
 * Contributor(s): ECRC GmbH
 * 
 * END LICENSE BLOCK */

/*
 * SEPIA INCLUDE FILE
 *
 * VERSION	$Id: module.h,v 1.1 2008/06/30 17:43:57 jschimpf Exp $
 */

/*
 * IDENTIFICATION		module.h	
 *
 * DESCRIPTION	
 *
 *
 * CONTENTS:
 *
 */

#define IsModule(mdid)		(DidModule(mdid)? 1:0)
#define IsLocked(mdid)		(DidModule(mdid) & IS_LOCKED)
#define ModuleTag(mdid)		(TDICT | 0x100)
#define IsModuleTag(mdid, tag) ((tag).kernel == ModuleTag(mdid))
#define UnauthorizedAccess(mod, mod_tag) \
	(IsLocked(mod) && !IsModuleTag(mod, mod_tag))

#define ModuleItem(did) \
   ((module_item *) (get_property(did, MODULE_PROP))->val.ptr)
#define ModuleSyntax(did)	(ModuleItem(did)->syntax)

/* This is obsolete!! */
#define Check_Module(t,v)					\
	Check_Atom_Or_Nil(v, t);				\
	if (! IsModule(v.did))	{Bip_Error(MODULENAME);}

#define Check_Module_Access(v, t)				\
	if (IsLocked(v.did) && (! IsModuleTag(v.did, t)))	\
		{Bip_Error(LOCKED);}

#define Check_Module_And_Access(v, t)				\
	Check_Atom_Or_Nil(v, t);				\
	if (!IsModule(v.did)) {					\
	    Bip_Error(MODULENAME)				\
	} else if (IsLocked(v.did) && (! IsModuleTag(v.did, t))) {\
		Bip_Error(LOCKED)				\
	}


/*
 *	The module structure.
 */

#define UNLOCK_MODULE		1
#define SOFT_LOCK_MODULE	2
#define HARD_LOCK_MODULE	3

#define IS_MODULE		3
#define IS_LOCKED		2

/*
 *	The global and dynamic links.
 */

typedef struct proc_duet
{
        pri			*desc;
        struct proc_duet	*next;
} proc_duet;
