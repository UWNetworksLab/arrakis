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
 * ECLiPSe kernel
 *
 * System:	ECLiPSe Constraint Logic Programming System
 * Author/s:	Rewrite 1/2000 by Joachim Schimpf, IC-Parc
 * Version:	$Id: proc_desc.c,v 1.3 2008/09/01 11:44:54 jschimpf Exp $
 *
 * Contains functions to create/access/modify/remove procedure descriptors
 *
 * Procedure lookup:
 *	visible_procedure
 *	qualified_procedure
 *
 * Procedure creation and visibility declaration:
 *	local_procedure
 *	export_procedure
 *	global_procedure
 *	import_procedure
 *	reexport_procedure
 *
 * Changing procedure properties:
 *	pri_compatible_flags
 *	pri_change_flags
 *	pri_init_code
 *	pri_define_code
 *	pri_change_mode
 *	pri_change_trans_function
 *	Pri_Set_Reference
 *
 * Implementation notes:
 * 
 * A procedure has
 * - a descriptor in the module where it is defined (LOCAL,EXPORT),
 *	this is called the "home" or "definition" descriptor.
 * - a descriptor in every module where it is visible (IMPORT,IMPEXP).
 * - a qualified access descriptor (QUALI) in every module where
 *     there is a compiled qualified access to it via :/2.
 * - a DEFAULT descriptor in every module where it is referenced but the
 *     source of the corresponding definition is not yet known.
 * A "visibility descriptor" is a descriptor other than QUALI.
 * To allow for incremental operation, the descriptors can be created
 * in any order.
 * 
 * Every descriptor has two module fields:
 *    module_def: the module to which the descriptor belongs (always set)
 *    module_ref: the module where the corresponding procedure definition
 *	can be found. For LOCAL,EXPORT this is the same as module_def,
 *	for IMPORT,IMPEXP this is the source of the import, for QUALI it
 *	is the referenced module, for DEFAULT it is D_UNKNOWN.
 *
 * Accesses always go via a descriptor in the module where the access
 * happens.  This is important for erasing modules: since there are no
 * direct inter-module accesses, all descriptors in the erased module
 * can be destroyed together with the module.
 * 
 * Lazy import: import(Module) implements lazy import, i.e. procedures are
 * only imported when an attempt is made to access them (visible_procedure()).
 * Restriction: the exporting module's interface must be known at the time
 * of import(Module), which is always the case for use_module/1.
 * 
 * Delayed export:  all exports (or globalisations) happen only when the
 * procedure is defined (ie. acquires code). This is done to avoid problems
 * with the incremental declaration of procedure properties - assuming that
 * all declarations occur before the clauses, the procedure is only
 * exported when it is fully defined. Implemented by using initially a
 * LOCAL descriptor and change it to EXPORT later. That this needs
 * to be done is indicated by the flag TO_EXPORT.
 * 
 * We allow only the following incremental changes to visibility:
 *    DEFAULT -> LOCAL -> EXPORT
 *    DEFAULT -> IMPORT -> IMPEXP
 * 
 * Reexports:  we require the exported procedure to be already defined at
 * reexportation time.  That means that an IMPEXP descriptor always refers
 * directly to the definition module.  References to an IMPEXP descriptor
 * (from an IMPORT or another IMPEXP descriptor) are always immediately
 * forwarded to the definition module.  Therefore there are no descriptor
 * chains and the definition can always be found in one step. 
 * 
 * Parallel locks policy:
 *     ModuleLock - while a module_item is accessed
 *     ProcListLock - while a did's procedure list is traversed/modified
 *     ProcChainLock - while one of the procedure chains is traversed/modified
*/

#include	"config.h"
#include	"sepia.h"
#include	"types.h"
#include	"embed.h"
#include	"mem.h"
#include 	"error.h"
#include	"opcode.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"database.h"
#include	"module.h"
#include	"property.h"
#include	"gencode.h"

#define a_mutex_lock(x)
#define a_mutex_unlock(x)


#define	ExportImmediately(pd)	\
	((pd)->flags & CODE_DEFINED || (pd)->flags & AUTOLOAD || (pd)->trans_function)


static int	_resolve_import(dident,dident,pri**);
static uint32	_hiding_import(dident,dident,dident*);
static int	_report_error(int, dident, dident, type);
static void	_pri_init_vmcode(pri*,int);
void		remove_procedure(pri*);


/*----------------------------------------------------------------------
 * New descriptors
 *----------------------------------------------------------------------*/

/*
 * Allocate a new procedure descriptor
 */

static pri*
_new_pri(dident functor, dident module)
{
    pri *pd = (pri*) hg_alloc_size(sizeof(pri));
    pd->did = functor;
    pd->flags = NOREFERENCE|DEBUG_DF;
    pd->module_def = module;
    pd->module_ref = pd->trans_function = D_UNKNOWN;
    pd->nextproc = pd->next_in_mod = 0;
    pd->mode = 0;
    pd->code.vmc = 0;
    return pd;
}


/*
 * free a procedure descriptor
 */

static void
_free_pri(pri *pd)
{
    hg_free_size((generic_ptr)pd, sizeof(pri));
}


/*
 * Create a new descriptor and insert it into the functor and
 * module lists as a visibility descriptor.
 * Make sure the code field gets set after calling this!
 * Shared memory locks: must be called with ProcListLock and ModuleLock
 */

static pri*
_new_visible_pri(dident functor, dident module, module_item *module_property, int visibility)
{
    pri *pd = _new_pri(functor, module);
    pd->flags |= VMCODE|ARGFIXEDWAM|visibility
    	|PriPriorityFlags(SUSP_EAGER_PRIO);

    /* insert it at the beginning of the functor list	     */
    pd->nextproc = DidPtr(functor)->procedure;
    DidPtr(functor)->procedure = pd;
    
    /* insert it at the beginning of the module list	     */
    if (!module_property)
	module_property = ModuleItem(module);
    pd->next_in_mod = module_property->procedures;
    module_property->procedures = pd;

    return pd;
}


/*----------------------------------------------------------------------
 * Auxiliary functions
 *----------------------------------------------------------------------*/

/* Get a procedure's definition (home) descriptor, if it exists.  */

pri *
pri_home(pri *pd)
{
    type tm;
    if (pd->module_ref == pd->module_def)
    	return pd;
    if (pd->module_ref == D_UNKNOWN)
    {
	Set_Bip_Error(NOENTRY);
    	return 0;
    }
    tm.kernel = ModuleTag(pd->module_ref);
    return visible_procedure(pd->did, pd->module_ref, tm, PRI_DONTIMPORT);
}


/* Find the visibility descriptor for functor in module, if it exists */

static pri *
_current_visible(dident functor, dident module)
/* Locks: requires ProcListLock. aquires nothing. */
{
    pri		*pd;
    
    for(pd=DidPtr(functor)->procedure; IsVisibilityPri(pd); pd=pd->nextproc)
    {
	if (pd->module_def == module)
	    return pd;
    }
    return 0;
}


/*
 * Find the EXPORT descriptor for functor if imported from
 * export_module. Follow IMPEXP indirection if necessary.
 * Return the actual exporting module in last_module, even if the
 * export descriptor does not exist yet.
 */

static pri *
_find_export(dident functor, dident exporting_module, dident *last_module)
/* Locks: requires ProcListLock. aquires nothing. */
{
    pri *pd;
    for(pd=DidPtr(functor)->procedure; IsVisibilityPri(pd); pd=pd->nextproc)
    {
	if (pd->module_def == exporting_module)
	{
	    switch (PriScope(pd))
	    {
	    case EXPORT:
		*last_module = exporting_module;
		return pd;
	    case IMPEXP:
		return _find_export(functor, pd->module_ref, last_module);
	    default:
		*last_module = exporting_module;
		return 0;
	    }
	}
    }
    *last_module = exporting_module;
    return 0;
}


static int
_export_exists(dident functor, dident exporting_module)
/* Locks: requires ProcListLock. aquires nothing. */
{
    dident dummy;
    return _find_export(functor, exporting_module, &dummy) ? 1 : 0;
}


/*
 * When a new IMPEXP descriptor is created, find all descriptors that point
 * to it and forward their module_ref to the (now known) definition module.
 */
static void
_deref_chains(pri *new_impexp)	/* a new IMPEXP maximally dereferenced */
/* Locks: requires ProcListLock. aquires nothing. */
{
    pri *pd;
    for(pd=DidPtr(new_impexp->did)->procedure; pd; pd=pd->nextproc)
    {
	if (PriIsProxy(pd) && pd->module_ref == new_impexp->module_def)
	{
	    pd->module_ref = new_impexp->module_ref;
	}
    }
}


/*
 * Check whether a procedure is referenced, ie. whether any of
 * its descriptors is referenced
 */

static int
_procedure_referenced(pri *pd)
/* Locks: requires nothing. acquires ProcListLock. */
{
    dident definition_module;

    if (PriReferenced(pd))
    	return 1;
    if (!PriExported(pd))
    	return 0;

    a_mutex_lock(&ProcListLock);
    definition_module = pd->module_def;
    for(pd = DidPtr(pd->did)->procedure; pd; pd = pd->nextproc)
    {
	if (pd->module_ref == definition_module  &&  PriReferenced(pd))
	{
	    a_mutex_unlock(&ProcListLock);
	    return 1;
	}
    }
    a_mutex_unlock(&ProcListLock);
    return 0;
}


/*
 * Add/delete a descriptor from a general-purpose descriptor chain
 */

void
add_proc_to_chain(pri *p, proc_duet **chain)
/* Locks: requires ProcChainLock. aquires nothing. */
{
    proc_duet	*gd;

    gd = (proc_duet *) hg_alloc_size(sizeof(proc_duet));
    gd->desc = p;
    gd->next = *chain;
    *chain = gd;
}

void
delete_proc_from_chain(pri *p, proc_duet **chain)
/* Locks: requires ProcChainLock. aquires nothing. */
{
    proc_duet	*current_gd;

    current_gd = *chain;
    while (current_gd)
    {
	if (current_gd->desc == p)
	{ /* found, so delete it from the chain */
	    *chain = current_gd->next;
	    hg_free_size((generic_ptr) current_gd, sizeof(proc_duet));
	    break;
	}
	chain = &current_gd->next;
	current_gd = current_gd->next;
    }
}


/*
 * Report an error like:
 *	<error message> in arg1/arity in module
 */

static int
_report_error(int err,
	dident arg1,		/* any arity */
	dident module,		/* arity 0 */
	type mod_tag)
{
    int res;
    pword *old_tg = TG;
    pword *tg = TG;
    pword mod, goal;

    Make_Struct(&goal, TG);

    Push_Struct_Frame(d_.syserror); ++tg;
    Make_Integer(tg, -err); ++tg;
    Make_Struct(tg, TG); ++tg;
    tg->val.did = module;
    tg++->tag.all = mod_tag.all;
    tg->val.did = module;
    tg++->tag.all = mod_tag.all;

    Push_Struct_Frame(d_.quotient); ++tg;
    Make_Atom(tg, add_dict(arg1,0)); ++tg;
    Make_Integer(tg, DidArity(arg1));

    mod.val.did = d_.kernel_sepia;
    mod.tag.kernel = ModuleTag(d_.kernel_sepia);
    res = query_emulc(goal.val, goal.tag, mod.val, mod.tag);
    TG = old_tg;
    return res;
}


#ifdef PRINTAM

/*
 * Debugging support: print procedure descriptors
 */

void
print_pri(pri *pd)
{
    switch(PriScope(pd))
    {
    case QUALI:		p_fprintf(current_output_, "QUALI  "); break;
    case LOCAL:		p_fprintf(current_output_, "LOCAL  "); break;
    case EXPORT:	p_fprintf(current_output_, "EXPORT "); break;
    case IMPORT:	p_fprintf(current_output_, "IMPORT "); break;
    case DEFAULT:	p_fprintf(current_output_, "DEFAUL "); break;
    case IMPEXP:	p_fprintf(current_output_, "IMPEXP "); break;
    default:		p_fprintf(current_output_, "?????? "); break;
    }
    p_fprintf(current_output_, "in %12s from %12s",
	DidName(pd->module_def),
	pd->module_ref? DidName(pd->module_ref) : "UNKNOWN");

    p_fprintf(current_output_, " %c%c%c %c %c%c%c%c%c %c%c%c%c%c %c%c%c %01x p%d",
	pd->flags&SYSTEM	? 'S' : '_',
	pd->flags&NOREFERENCE	? 'N' : '_',
	pd->flags&CODE_DEFINED	? 'C' : '_',

	pd->flags&TO_EXPORT	? 'X' : '_',

	pd->flags&PROC_PARALLEL	? 'P' : '_',
	pd->flags&PROC_DEMON	? 'D' : '_',
	pd->flags&TOOL		? 'T' : '_',
	pd->flags&AUTOLOAD	? 'A' : '_',
	pd->flags&PROC_DYNAMIC	? 'Y' : '_',

	pd->flags&DEBUG_TR	? 'T' : '_',
	pd->flags&DEBUG_SP	? 'P' : '_',
	pd->flags&DEBUG_SK	? 'K' : '_',
	pd->flags&DEBUG_DB	? 'D' : '_',
	pd->flags&DEBUG_ST	? 'S' : '_',

	(pd->flags&(CODETYPE)) == VMCODE ? 'v' : 'f',
	(pd->flags&(ARGPASSING)) == ARGFIXEDWAM ? 'a' :
	    (pd->flags&(ARGPASSING)) == ARGFLEXWAM ? 'f' : '?',
	pd->flags&EXTERN	? 'X' : '_',

	pd->flags&(UNIFTYPE),
	PriPriority(pd));

    if (!PriCode(pd))
	p_fprintf(current_output_, " null_code");
    else if ((PriCodeType(pd) == VMCODE) && IsUndefined(PriCode(pd)))
	p_fprintf(current_output_, " undef_code");
    else
	p_fprintf(current_output_, " 0x%x", PriCode(pd));

    ec_newline(current_output_);
}

void
print_procs(char *name, int arity)
{
    dident d = enter_dict(name, arity);
    if (d == D_UNKNOWN)
    {
    	p_fprintf(current_output_,"No such did");
	ec_newline(current_output_);
    }
    else if (! d->procedure)
    {
    	p_fprintf(current_output_,"No procedures");
	ec_newline(current_output_);
    }
    else
    {
	pri *pd;
	for (pd=d->procedure; pd; pd=pd->nextproc)
	    print_pri(pd);
    }
}

void
pri_statistics(void)
{
    int	idx = 0;
    dident mod;
    int count[6];

    while (next_functor(&idx, &mod))
    {
	if (IsModule(mod))
	{
	    pri *pd;
	    int i;
	    for(i=0;i<6;++i) count[i] = 0;

	    for (pd = ModuleItem(mod)->procedures; pd; pd = pd->next_in_mod)
	    {
		switch(PriScope(pd))
		{
		case QUALI:	++count[0]; break;
		case LOCAL:	++count[1]; break;
		case EXPORT:	++count[2]; break;
		case IMPORT:	++count[3]; break;
		case DEFAULT:	++count[4]; break;
		case IMPEXP:	++count[5]; break;
		default:	p_fprintf(current_err_, "Illegal scope %s\n", PriScope(pd)); break;
		}
	    }
	    p_fprintf(log_output_, "\nModule: %s\n", DidName(mod));
	    p_fprintf(log_output_, " QUALI=%d", count[0]);
	    p_fprintf(log_output_, " LOCAL=%d", count[1]);
	    p_fprintf(log_output_, " EXPORT=%d", count[2]);
	    p_fprintf(log_output_, " IMPORT=%d", count[3]);
	    p_fprintf(log_output_, " DEFAULT=%d", count[4]);
	    p_fprintf(log_output_, " IMPEXP=%d", count[5]);
	    ec_newline(log_output_);
	}
    }
}
#endif


/*----------------------------------------------------------------------
 * Comparing and updating two descriptors
 *----------------------------------------------------------------------*/

/*
 * A shadow descriptor is a descriptor whose properties are just copies
 * of the corresponding home descriptor. It can't be changed independently.
 */
#define ShadowDescriptor(pd) \
    (PriScope(pd) == IMPEXP || \
	(PriScope(pd) == IMPORT || PriScope(pd) == QUALI) \
	    && _export_exists(pd->did, pd->module_ref))


/*
 * Used to check compatibility before linking a definition to a use
 * (e.g. on actual import)
 */

static int
_compatible_def_use(pri *def, pri *use)
{
    uint32 conflicts;
    char *reason = NULL;

    /* if not yet referenced, any change is allowed */
    if (!PriReferenced(use))
    	return 1;

    /* don't allow changing certain flags */
    conflicts = (def->flags ^ use->flags) &
	(use->flags & CODE_DEFINED ?
	    PF_DONT_CHANGE_WHEN_DEFINED :
	    PF_DONT_CHANGE_WHEN_REFERENCED);

    if (conflicts)
    {
	if (conflicts & TOOL)
	    reason = "tool declaration";
	else if (conflicts & PROC_DYNAMIC)
	    reason = "static/dynamic";
	else if (conflicts & PROC_DEMON)
	    reason = "demon declaration";
	else if (conflicts & PROC_PARALLEL)
	    reason = "parallel declaration";
	else if (conflicts & (CODETYPE|ARGPASSING|UNIFTYPE))
	    reason = "calling convention";
	else
	    reason = "predicate properties";
    }

    /* other restrictions when already referenced */
    if (def->mode != use->mode)
    	reason = "mode declaration";

    if (def->trans_function != use->trans_function)
    	reason = "inline declaration";

    if (reason)
    {
	p_fprintf(warning_output_,
	    "Definition of %s/%d in module %s is incompatible (%s) with call in module %s\n",
	    DidName(def->did), DidArity(def->did), DidName(def->module_def),
	    reason, DidName(use->module_def));
	ec_flush(warning_output_);
	return 0;
    }

    return 1;
}


/*
 * Copy contents of a definition descriptor to a use (shadow) descriptor.
 * It is assumed that compatibility checks have already been done.
 */

static void
_update_def_use(pri *def, pri *use)
{
    /* Note on memory management of code blocks:
     * Undefined-code blocks are never shared between descriptors,
     * so don't copy pointers to them.
     * Defined-code is shared and pointed to from all descriptors.
     */
    if ((PriCodeType(use) == VMCODE) && !(PriFlags(use) & CODE_DEFINED))
    {
	if (PriFlags(def) & CODE_DEFINED)
	{
	    remove_procedure(use);		/* undefined -> defined */
	    use->code = def->code;
	}
	else if (!use->code.vmc)		/* undefined -> undefined */
	    _pri_init_vmcode(use, PriFlags(def)&TOOL);
	/* else keep undefined-code field */
    }
    else
    {
	if ((PriCodeType(def) == VMCODE) && !(PriFlags(def) & CODE_DEFINED))
	    _pri_init_vmcode(use, PriFlags(def)&TOOL);	/* defined -> undefined */
	else
	    use->code = def->code;		/* defined -> defined */
    }
    use->module_ref = def->module_def;
    use->mode = def->mode;
    use->trans_function = def->trans_function;
    use->flags = (use->flags & DESCRIPTOR_FLAGS) | (def->flags & COMMON_FLAGS);
}


/*
 * Given the home module descriptor of a procedure,
 * update all its uses (import/quali/impexp).
 * It is assumed that compatibility checks have already been done.
 */

static void
_update_all_uses(pri *def) /* must be the definition module descriptor */
/* Locks: requires ProcListLock. acquires nothing. */
{
    pri *use;

    if (!PriExported(def))
    	return;

    for(use = DidPtr(PriDid(def))->procedure; use; use = use->nextproc)
    {
	if (PriIsProxy(use) && use->module_ref == def->module_ref)
	{
	    _update_def_use(def, use);
	}
    }
}


/*
 * In preparation of _update_all_uses(), remove uses (import/quali/impexp)
 * that are incompatible with the definition.
 */

static void
_remove_incompatible_uses(pri *def) /* must be the definition module descriptor */
/* Locks: requires ProcListLock. acquires nothing. */
{
    pri *use;

    if (!PriExported(def))
    	return ;

    for(use = DidPtr(PriDid(def))->procedure; use; use = use->nextproc)
    {
	if (PriIsProxy(use) && use->module_ref == def->module_ref)
	{
	    if (!_compatible_def_use(def, use))
	    {
		/* attempt to undo the impossible def-use link */
		switch (PriScope(use))
		{
		    case IMPORT: Pri_Set_Scope(use, LOCAL); break;
		    case IMPEXP: Pri_Set_Scope(use, EXPORT); break;
		    case QUALI: break;
		}
		use->module_ref = use->module_def;
	    }
	}
    }
}


/* Perform delayed export/globalisation of a procedure if necessary */

static void
_delayed_export(pri *pd)
{
    if (pd->flags & TO_EXPORT)	/* delayed export */
    {
	Pri_Set_Scope(pd, EXPORT);
	pd->flags &= ~TO_EXPORT;
	_remove_incompatible_uses(pd);
    }
}

/*----------------------------------------------------------------------
 * Changing fields in the desciptor
 *----------------------------------------------------------------------*/

/*
 * Check whether the flags specified by 'mask' can be set to the 'new' values.
 * All COMMON_FLAGS can be checked that way, including CODE_TYPE.
 */

int
pri_compatible_flags(pri *pd, uint32 mask, uint32 new)
{
    uint32 illegal_change;

    if (ShadowDescriptor(pd))
    	; /* allow no changes at all */
    else if (pd->flags & CODE_DEFINED)
	mask &= PF_DONT_CHANGE_WHEN_DEFINED;
    else if (_procedure_referenced(pd))
	mask &= PF_DONT_CHANGE_WHEN_REFERENCED;
    else
	return PSUCCEED;

    new &= mask;
    illegal_change = (pd->flags ^ new) & mask;
    if (illegal_change)
    {
	/* make a more precise error message */
	if (illegal_change & SYSTEM)
	    return REDEF_SYS;
	if (illegal_change & TOOL)
	    return TOOL_REDEF;
	return INCONSISTENCY;
    }
    return PSUCCEED;
}


/*
 * Set the flags specified by 'mask' to the 'new' values.
 * Use pri_compatible_flags() beforehand to check whether this is allowed!
 * The flags that can be changed using this procedure are the
 * COMMON_FLAGS except CODETYPE
 * (CODETYPE is managed by pri_init_code() and pri_define_code())
 */

void
pri_change_flags(pri *pd, uint32 mask, uint32 new)
{
    /* do the change in the home descriptor, then distribute it */
    pd->flags = (pd->flags & ~mask) | (new & mask);
    if (new & AUTOLOAD)
    	_delayed_export(pd);
    _update_all_uses(pd);
}


/*
 * Construct the default code for an undefined procedure.
 * (this should probably go elsewhere)
 */
#define UNDEF_CODE_SIZE	3

static vmcode *
_undef_code(pri *pd)
{
    vmcode *code, *start;
    code = (vmcode *) hg_alloc_size(sizeof(vmcode) * (UNDEF_CODE_SIZE + PROC_PREFIX_SIZE));
    /* Make_Procedure_Prefix(link, size, bid, fid, lid, cid, did) */
    Make_Procedure_Prefix(0L, UNDEF_CODE_SIZE, -1L, D_UNKNOWN, UNDEFINED_PROC, -1L, PriDid(pd));
    start = code;
    Store_2(Undefined, pd);
    Store_i(Code_end);
    return start;
}


/*
 * _pri_init_vmcode(), _pri_clear_code()
 * auxiliary functions to set the procedure code field
 */

static void
_pri_init_vmcode(pri *pd, int tool_flag)  /* hopefully a temporary hack... */
{
    pd->code.vmc = _undef_code(pd);
    pd->flags &= ~CODE_DEFINED;
    /* this is important for saving the arguments in the event mechanism */
    if (tool_flag)
	{ Incr_Code_Arity(pd->code.vmc); }
}

static void
_pri_clear_code(pri *pd)
{
    if (pd->flags & CODE_DEFINED)
	if (pd->module_def == pd->module_ref)
	    remove_procedure(pd);		/* sets code to 0 */
	else
	    pd->code.vmc = 0;			/* just a copy of the code field! */
    else
	remove_procedure(pd);			/* sets code to 0 */
}


/*
 * pri_init_code() and pri_define_code() are used to change the code field
 * (together with the CODE_TYPE and the CODE_DEFINED flags).
 * Make sure beforehand (by calling pri_compatible_flags())
 * that changing to code_type is allowed.
 */
void
pri_init_code(pri *pd,				/* any descriptor */
	int code_type)
{
    if (pd->code.vmc)				/* free old code */
    {
    	remove_procedure(pd);
    }
    pd->flags = (pd->flags & ~(CODETYPE|CODE_DEFINED)) | code_type;
    /* do the change in the home descriptor, then distribute it */
    if (code_type == VMCODE)
    	_pri_init_vmcode(pd, PriFlags(pd)&TOOL);
    else
	pd->code.cint = 0;
    _update_all_uses(pd);
}
    
void
pri_define_code(pri *pd,			/* home descriptor only!!! */
	int code_type,
	pri_code_t new_code)
{
    if (pd->code.vmc)				/* free old code */
    {
    	remove_procedure(pd);
    }
    /* do the change in the home descriptor first */
    pd->flags = (pd->flags & ~CODETYPE) | code_type | CODE_DEFINED;
    pd->code = new_code;
    /* remove incompatible uses, then update the others */
    _delayed_export(pd);
    _update_all_uses(pd);
}
    

/* Change a procedure's mode field */

int
pri_change_mode(pri *pd,			/* any descriptor */
	uint32 new_mode)
{
    if (ShadowDescriptor(pd))
    {
	/* allow no changes */
	return pd->mode == new_mode ? PSUCCEED : ACCESSING_NON_LOCAL;
    }
    pd->mode = new_mode;
    _update_all_uses(pd);
    return PSUCCEED;
}
    

/* Change a procedure's inline (goal transformation) field */

int
pri_change_trans_function(pri *pd,		/* any descriptor */
	dident trans_function)
{
    if (ShadowDescriptor(pd))
    {
	/* allow no changes */
	return pd->trans_function == trans_function ? PSUCCEED : ACCESSING_NON_LOCAL;
    }
    pd->trans_function = trans_function;
    _delayed_export(pd);
    _update_all_uses(pd);
    return PSUCCEED;
}


/*----------------------------------------------------------------------
 * Find or create a local procedure in the given module.
 *
 * Possible options:
 *	PRI_CREATE	create the procedure if it doesn't exist
 *
 * We allow
 *	null	-> LOCAL	(if PRI_CREATE)
 *	DEFAULT	-> LOCAL	(if PRI_CREATE)
 *	LOCAL	-> LOCAL
 *	EXPORT	-> EXPORT
 * Error
 *	IMPORT	-> error
 *
 * Shared memory locks: ProcListLock, ModuleLock
 *----------------------------------------------------------------------*/

pri *
local_procedure(dident functor, dident module, type module_tag, int options)
{
    pri		*pd;

    if (UnauthorizedAccess(module, module_tag))
    {
	Set_Bip_Error(LOCKED);
	return 0;
    }
    a_mutex_lock(&ProcListLock);
    pd = _current_visible(functor, module);
    if (pd)
    {
	switch(PriScope(pd))
	{
	case DEFAULT:
	    if (options & PRI_CREATE)
	    {
		Pri_Set_Scope(pd, LOCAL);
		pd->module_ref = module;
	    }
	    else
	    {
		Set_Bip_Error(NOENTRY);
	        pd = 0;
	    }
	    break;

	case IMPORT:
	case IMPEXP:
	    Set_Bip_Error(options & PRI_CREATE? IMPORT_EXISTS:ACCESSING_NON_LOCAL);
	    pd = 0;
	    break;

	case LOCAL:
	case EXPORT:
	    break;
	}
    }
    else if (options & PRI_CREATE)
    {
	if (!(options & PRI_DONTWARN))
	{
	    dident exporting_module;
	    switch (_hiding_import(functor, module, &exporting_module))
	    {
	    case IMPORT:
		p_fprintf(warning_output_,
		    "WARNING: Hiding imported predicate %s/%d from module %s in module %s (use local/1)\n",
		    DidName(functor), DidArity(functor),
		    DidName(exporting_module), DidName(module));
		ec_flush(warning_output_);
		break;
	    case SYSTEM:
		a_mutex_unlock(&ProcListLock);
		Set_Bip_Error(BUILT_IN_REDEF);
		return 0;
	    }
	}
	a_mutex_lock(&ModuleLock);
	pd = _new_visible_pri(functor, module, 0, LOCAL);
	a_mutex_unlock(&ModuleLock);
	_pri_init_vmcode(pd, 0);
	pd->module_ref = module;
    }
    else
    {
	Set_Bip_Error(NOENTRY);
    }
    a_mutex_unlock(&ProcListLock);
    return pd;
}


/*----------------------------------------------------------------------
 * Export a procedure, create if it doesn't exist
 *
 * We allow
 *	null	-> DEFAULT -> EXPORT
 *	DEFAULT	-> EXPORT
 *	LOCAL	-> EXPORT
 *	EXPORT  -> EXPORT
 * Error
 *	IMPORT	-> error
 *	IMPEXP	-> error
 *
 * Shared memory locks: like visible_procedure()
 *----------------------------------------------------------------------*/


pri *
export_procedure(dident functor, dident module, type module_tag)
{
    pri		*pd;

    if (UnauthorizedAccess(module, module_tag))
    {
	Set_Bip_Error(LOCKED);
	return 0;
    }
    a_mutex_lock(&ProcListLock);
    pd = _current_visible(functor, module);
    if (!pd)
    {
	a_mutex_lock(&ModuleLock);
	pd = _new_visible_pri(functor, module, 0, DEFAULT);
	a_mutex_unlock(&ModuleLock);
	_pri_init_vmcode(pd, 0);
    }
    switch(PriScope(pd))
    {
    case DEFAULT:
	Pri_Set_Scope(pd, LOCAL);
	pd->module_ref = module;
	/* fall through */

    case LOCAL:
	if (ExportImmediately(pd))
	{
	    pd->flags &= ~TO_EXPORT;
	    Pri_Set_Scope(pd, EXPORT);
	    _remove_incompatible_uses(pd);
	    _update_all_uses(pd);
	}
	else
	{
	    pd->flags |= TO_EXPORT;
	    /* checking/linking against imports is done later
	     * (end of module interface or code definition) */
	}
	break;

    case IMPORT:
	Set_Bip_Error(IMPORT_EXISTS); pd = 0; break;

    case IMPEXP:
	Set_Bip_Error(REEXPORT_EXISTS); pd = 0; break;

    case EXPORT:
	break;
    }
    a_mutex_unlock(&ProcListLock);
    return pd;
}


/*----------------------------------------------------------------------
 * Globalise a procedure - similar to exporting
 *----------------------------------------------------------------------*/

pri *
global_procedure(dident functor, dident module, type module_tag)
{
    return export_procedure(functor, module, module_tag);
}


#if 0
/*
 * Perform all delayed export/global declarations in module mod
 *	- mark export descriptor
 *	- check compatibility and update import descriptors
 */

void
check_def_use_module_interface(dident mod, type mod_tag)
/* Locks: aquires ProcListLock, ModuleLock. */
{
    pri		*def;

    a_mutex_lock(&ModuleLock);
    a_mutex_lock(&ProcListLock);
    for (def = ModuleItem(mod)->procedures; def; def = def->next_in_mod)
    {
	if (PriScope(def) == LOCAL  &&  PriFlags(def) & TO_EXPORT)
	{
	    _delayed_export(def);
	    _remove_incompatible_uses(def);
	    _update_all_uses(def);
	}
    }
    a_mutex_unlock(&ProcListLock);
    a_mutex_unlock(&ModuleLock);
}
#endif


/*----------------------------------------------------------------------
 * Import a procedure
 *
 * We allow
 *	null	-> DEFAULT -> IMPORT
 *	DEFAULT	-> IMPORT
 *	IMPORT	-> IMPORT (same exporter)
 *	IMPEXP	-> IMPEXP (same exporter)
 * Error
 *	LOCAL	-> error
 *	EXPORT	-> error
 *	IMPORT	-> error if different exporter
 *
 * Shared memory locks: like visible_procedure()
 *----------------------------------------------------------------------*/

pri *
import_procedure(dident functor, dident module, type module_tag, dident exporting_module)
{
    pri		*pd, *exported_pd;

    if (UnauthorizedAccess(module, module_tag))
    {
	Set_Bip_Error(LOCKED);
	return 0;
    }
    a_mutex_lock(&ProcListLock);
    pd = _current_visible(functor, module);
    if (!pd)
    {
	a_mutex_lock(&ModuleLock);
	pd = _new_visible_pri(functor, module, 0, DEFAULT);
	a_mutex_unlock(&ModuleLock);
	_pri_init_vmcode(pd, 0);
    }
    exported_pd = _find_export(functor, exporting_module, &exporting_module);
    switch(PriScope(pd))
    {
    case DEFAULT:
	if (exported_pd)
	{
	    if (_compatible_def_use(exported_pd, pd))
	    {
		_update_def_use(exported_pd, pd);
	    }
	    else
	    {
		Set_Bip_Error(INCONSISTENCY);
		pd = 0;
		break;
	    }
	    Pri_Set_Scope(pd, IMPORT);
	    pd->module_ref = exported_pd->module_def;
	}
	else /* else chain is not yet completely known */
	{
	    Pri_Set_Scope(pd, IMPORT);
	    pd->module_ref = exporting_module;
	}
	break;
    case IMPORT:
    case IMPEXP:
	if (pd->module_ref != exporting_module)
	{
	    Set_Bip_Error(IMPORT_EXISTS);
	    pd = 0;
	}
	/* else ALREADY_IMPORT */
	break;
    case LOCAL :
	if (pd->module_ref != exporting_module)
	{
	    Set_Bip_Error(LOCAL_EXISTS); pd = 0;
	}
	break;
    case EXPORT :
	if (pd->module_ref != exporting_module)
	{
	    Set_Bip_Error(EXPORT_EXISTS); pd = 0;
	}
	break;
    }
    a_mutex_unlock(&ProcListLock);
    return pd;
}


/*----------------------------------------------------------------------
 * Reexport a procedure
 * As opposed to importing, this requires the export descriptor to
 * exist already.
 *
 * We allow
 *	null	-> DEFAULT -> IMPEXP
 *	DEFAULT	-> IMPEXP
 *	IMPORT	-> IMPEXP (same exporter)
 *	IMPEXP	-> IMPEXP (same exporter)
 * Error
 *	LOCAL	-> error
 *	EXPORT	-> error
 *	IMPORT	-> error if different exporter
 *	IMPEXP	-> error if different exporter
 *
 * Shared memory locks: like visible_procedure()
 *----------------------------------------------------------------------*/

pri *
reexport_procedure(dident functor, dident module, type module_tag, dident from_module)
{
    pri		*pd, *exported_pd;

    if (UnauthorizedAccess(module, module_tag))
    {
	Set_Bip_Error(LOCKED);
	return 0;
    }
    a_mutex_lock(&ProcListLock);
    pd = _current_visible(functor, module);
    if (!pd)
    {
	a_mutex_lock(&ModuleLock);
	pd = _new_visible_pri(functor, module, 0, DEFAULT);
	a_mutex_unlock(&ModuleLock);
	_pri_init_vmcode(pd, 0);
    }
    exported_pd = _find_export(functor, from_module, &from_module);
    switch(PriScope(pd))
    {
    case DEFAULT:
	if (exported_pd)
	{
	    if (_compatible_def_use(exported_pd, pd))
	    {
		_update_def_use(exported_pd, pd);
	    }
	    else
	    {
		Set_Bip_Error(INCONSISTENCY);
		pd = 0;
		break;
	    }
	    Pri_Set_Scope(pd, IMPEXP);
	    pd->module_ref = from_module;
	    _deref_chains(pd);				/* because IMPEXP */
	    _remove_incompatible_uses(exported_pd);	/* because EXPORT */
	    _update_all_uses(exported_pd);		/* because EXPORT */
	}
	else /* else chain is not yet completely known */
	{
	    Set_Bip_Error(NOENTRY);
	    pd =0;
	}
	break;
    case IMPORT:
	if (exported_pd && pd->module_ref == from_module)
	{
	    Pri_Set_Scope(pd, IMPEXP);
	    _deref_chains(pd);				/* because IMPEXP */
	    _remove_incompatible_uses(exported_pd);	/* because EXPORT */
	    _update_all_uses(exported_pd);		/* because EXPORT */
	}
	else /* else chain is not yet completely known */
	{
	    Set_Bip_Error(NOENTRY);
	    pd =0;
	}
	break;
    case IMPEXP:
	if (pd->module_ref != from_module)
	{
	    Set_Bip_Error(REEXPORT_EXISTS);
	    pd = 0;
	}
	/* else ALREADY_REEXPORT */
	break;
    case LOCAL :
	Set_Bip_Error(LOCAL_EXISTS); pd = 0; break;
    case EXPORT :
	Set_Bip_Error(EXPORT_EXISTS); pd = 0; break;
    }
    a_mutex_unlock(&ProcListLock);
    return pd;
}


/*----------------------------------------------------------------------
 * Find or create the visible descriptor.
 * This is used for accessing properties or the code of the procedure.
 * We allow:
 *	null,DEFAULT -> resolve imports successfully -> IMPORT
 *	null,DEFAULT -> resolve imports unsuccessfully -> null
 *	null,DEFAULT -> resolve imports unsuccessfully -> DEFAULT (if PRI_CREATE)
 *	null,DEFAULT -> resolve imports with error -> null
 *	LOCAL	-> LOCAL
 *	EXPORT	-> EXPORT
 *	IMPORT	-> IMPORT
 *
 * Locked modules: only allow the exports to be accessed.
 *
 *
 * Possible options
 *	PRI_CREATE		create descriptor if none (forward references)
 *	PRI_REFER		set descriptor's referenced-flag
 *	PRI_DONTIMPORT		don't try to resolve imports
 *	PRI_EXPORTEDONLY	access only exported predicates
 *	PRI_DONTWARN		don't raise IMPORT_CLASH on ambiguous import,
 *				simply return NOENTRY
 *
 * Possible error codes (if returned pri* is null):
 *	NOENTRY		unless PRI_CREATE options set
 *	LOCKED
 *	CONSISTENCY
 *
 * Shared memory locks: Acquires ProcListLock and possibly ModuleLock
 *----------------------------------------------------------------------*/

#define UnauthorizedAccessOption(module, module_tag, exponly) \
	(!IsModuleTag(module, module_tag) && ((exponly) || IsLocked(module)))

pri *
visible_procedure(dident functor, dident module, type module_tag, int options)
{
    int		res;
    pri		*pd;

    a_mutex_lock(&ProcListLock);
    pd = _current_visible(functor, module);
    if (pd)
    {
	switch(PriScope(pd))
	{
	case LOCAL:
	case IMPORT:
	    if (UnauthorizedAccessOption(module, module_tag, options & PRI_EXPORTEDONLY))
	    {
		a_mutex_unlock(&ProcListLock);
		Set_Bip_Error(options & PRI_EXPORTEDONLY? NOENTRY: LOCKED);
		return 0;
	    }
	    /* fall through */
	case EXPORT:
	case IMPEXP:
	    if (options & PRI_REFER)
	    {
		Pri_Set_Reference(pd);
	    }
	    a_mutex_unlock(&ProcListLock);
	    return pd;
	case DEFAULT:
	    break;	/* lazy import */
	}
    }
    if (UnauthorizedAccessOption(module, module_tag, options & PRI_EXPORTEDONLY))
    {
	Set_Bip_Error(options & PRI_EXPORTEDONLY? NOENTRY: LOCKED);
	pd = 0;
    }
    else if (options & PRI_DONTIMPORT)
    {
	dident dummy;
	Set_Bip_Error(_hiding_import(functor, module, &dummy) ? IMPORT_PENDING : NOENTRY);
	pd = 0;
    }
    else
    {
	/* pd == NULL  or  DEFAULT */
	res = _resolve_import(functor, module, &pd);
	switch(res)
	{
	case PSUCCEED:
	    break;

	case IMPORT_CLASH:
	    if (_report_error(IMPORT_CLASH_RESOLVE, functor, module, module_tag) == PSUCCEED)
	    {
		/* handler succeeded, try again */
		return visible_procedure(functor, module, module_tag, options);
	    }
	    if (!(options & PRI_DONTWARN))
	    {
		(void) _report_error(IMPORT_CLASH, functor, module, module_tag);
	    }
	    res = NOENTRY;
	    /* fall through */

	case NOENTRY:
	    if (options & PRI_CREATE)
	    {
		if (!pd)
		{
		    a_mutex_lock(&ModuleLock);
		    pd = _new_visible_pri(functor, module, 0, DEFAULT);
		    a_mutex_unlock(&ModuleLock);
		    _pri_init_vmcode(pd, 0);
		}
		break;
	    }
	    /* fall through */

	default:
	    Set_Bip_Error(res);
	    pd = 0;
	    break;
	}
	if (pd && options & PRI_REFER)
	{
	    Pri_Set_Reference(pd);
	}
    }
    a_mutex_unlock(&ProcListLock);
    return pd;
}


/*----------------------------------------------------------------------
 * Find or create the qualified descriptor (a reference from ref_module
 * to the definition in lookup_module)
 * This is used for making qualified calls.
 *----------------------------------------------------------------------*/

pri *
qualified_procedure(dident functor, dident lookup_module, dident ref_module, type ref_mod_tag)
/* Locks: acquires ProcListLock, ModuleLock. */
{
    pri		*pd, *visible_pd, *home_pd;
    pri		**qualified_chain;
    module_item	*module_property;
    dident	home_module;

    /* If modules are the same, it's the same as visible_procedure() */
    if (lookup_module == ref_module)
    	return visible_procedure(functor, ref_module, ref_mod_tag,
			PRI_CREATE|PRI_REFER);

    /*
     * All the qualified descriptors are at the end of the list.
     * First skip the visibility descriptors, remembering a visible one
     * (if any) and the start of qualified descriptor chain (for appending
     * later on)
     */
    a_mutex_lock(&ProcListLock);
    qualified_chain = &DidPtr(functor)->procedure;
    pd = DidPtr(functor)->procedure;
    visible_pd = 0;
    while(IsVisibilityPri(pd))
    {
	if (pd->module_def == lookup_module)
	    visible_pd = pd;
	qualified_chain = &pd->nextproc;
	pd = pd->nextproc;
    }

    switch (visible_pd ? PriScope(visible_pd) : DEFAULT)
    {
    case DEFAULT:
    case IMPORT:
    case LOCAL:
	home_pd = 0;
	home_module = lookup_module;
	break;

    case EXPORT:
	home_pd = visible_pd;
	home_module = lookup_module;
	break;

    case IMPEXP:
	home_pd = _find_export(visible_pd->did, visible_pd->module_ref, &home_module);
	break;
    }

    /*
     * If there is already an appropriate qualified descriptor, use it.
     */
    while (pd)	/* loop through QUALI descriptors */
    {
	if (pd->module_def == ref_module && pd->module_ref == home_module)
	{
	    a_mutex_unlock(&ProcListLock);
	    return pd;
	}
	pd = pd->nextproc;
    }

    /*
     * Create a new qualified descriptor and link it to the definition
     */

    pd = _new_pri(functor, ref_module);
    Pri_Set_Reference(pd);
    Pri_Set_Scope(pd, QUALI);
    pd->module_ref = home_module;
    if (home_pd)
    {
	_update_def_use(home_pd, pd);
    }
    else	/* undefined procedure for now*/
    {
	pd->flags = (pd->flags & ~CODETYPE)|VMCODE;
	_pri_init_vmcode(pd, 0);
    }

    /* insert it at the beginning of the qualified part of the list	*/
    pd->nextproc = *qualified_chain;
    *qualified_chain = pd;
    a_mutex_unlock(&ProcListLock);
    
    /* insert it at the beginning of the module list */
    a_mutex_lock(&ModuleLock);
    module_property = ModuleItem(ref_module);
    pd->next_in_mod = module_property->procedures;
    module_property->procedures = pd;	
    a_mutex_unlock(&ModuleLock);

    return(pd);
}


/*----------------------------------------------------------------------
 * (*pi) is null or a DEFAULT (referenced) descriptor in module
 * It is updated to an IMPORT if possible.
 * Should be called with ProcListLock
 *
 * Return codes:
 *	PSUCCEED	import was done (*pi updated)
 *	NOENTRY		there was nothing to import
 *	IMPORT_CLASH	don't know which to import
 *	CONSISTENCY	import would be inconsistent
 *----------------------------------------------------------------------*/

static int
_resolve_import(dident functor, dident module, pri **pi)
{
    pri		*pe, *pd;
    module_item	*module_property;
    didlist	*imported_mod;
    dident	exporting_module;

    /* for all the modules imported in module, check whether
       functor is exported					     */
    a_mutex_lock(&ModuleLock);
    module_property = ModuleItem(module);
    imported_mod = module_property->imports;
    pe = 0;
    while(imported_mod)
    {
	pd = _find_export(functor, imported_mod->name, &exporting_module);
	/* pd is an EXPORT, no IMPEXP */
	if (pd)
	{
	    /* Check whether we found two different ones to import. Note that
	     * it is possible to find the same one twice because of reexports.
	     */
	    if (pe && pd->module_ref != pe->module_ref)	/* Ambiguity? */
	    {
		a_mutex_unlock(&ModuleLock);
		return IMPORT_CLASH;
	    }
	    pe = pd;
	}
	imported_mod = imported_mod->next;
    }
    if (!pe)
    {
	a_mutex_unlock(&ModuleLock);
    	return NOENTRY;
    }

    if (*pi)	/* DEFAULT descriptor already exists, check compatibility */
    {
	a_mutex_unlock(&ModuleLock);
	if (!_compatible_def_use(pe, *pi))
	{
	    return INCONSISTENCY;
	}
	Pri_Set_Scope(*pi, IMPORT);
    }
    else	/* no descriptor yet, create one */
    {
	(*pi) = _new_visible_pri(functor, module, module_property, IMPORT);
	a_mutex_unlock(&ModuleLock);
    }

    /* copy the definition */
    _update_def_use(pe, *pi);
    
    return PSUCCEED;
}


/*
 * Check whether functor potentially imports into module and return
 *	SYSTEM if yes and it's a SYSTEM procedure
 *	IMPORT if yes
 *	0 otherwise
 */
static uint32
_hiding_import(dident functor, dident module, dident *exporting_module)
{
    pri		*pd;
    module_item	*module_property;
    didlist	*imported_mod;
    dident	found_module;
    int		found = 0;

    a_mutex_lock(&ModuleLock);
    module_property = ModuleItem(module);
    imported_mod = module_property->imports;
    while(imported_mod)
    {
	pd = _find_export(functor, imported_mod->name, &found_module);
	if (pd)
	{
	    *exporting_module = found_module;
	    if (pd->flags & SYSTEM)
	    {
		a_mutex_unlock(&ModuleLock);
		return SYSTEM;
	    }
	    found = 1;
	}
	imported_mod = imported_mod->next;
    }
    a_mutex_unlock(&ModuleLock);
    return found? IMPORT: 0;
}


void
resolve_pending_imports(pri *procs_in_module)
{
    pri *pd;
    for(pd = procs_in_module; pd; pd = pd->next_in_mod)
    {
	if (PriScope(pd) == DEFAULT)
	    (void) _resolve_import(pd->did, pd->module_def, &pd);
    }
}


/*----------------------------------------------------------------------
 * Abolish (remove) a procedure
 *
 * We allow
 *	null	-> null
 *	DEFAULT	-> DEFAULT
 *	LOCAL	-> DEFAULT
 *	EXPORT  -> DEFAULT
 *	IMPORT	-> DEFAULT
 *
 * The descriptor is made a DEFAULT-descriptor and reinitialised
 * as much as possible. When it was referenced, some properties
 * must be kept otherwise existing calls could become inconsistent.
 *----------------------------------------------------------------------*/

int
pri_abolish(pri *pd)			/* a visibility descriptor */
{
    switch(PriScope(pd))
    {
    case IMPORT:
    case IMPEXP:
    case QUALI:
	return ACCESSING_NON_LOCAL;
    default:
	pri_init_code(pd, PriCodeType(pd));
	pd->flags = (pd->flags & DESCRIPTOR_FLAGS)
	    | (pd->flags & PF_DONT_CHANGE_WHEN_DEFINED);
	break;
    }
    return PSUCCEED;
}


/*
 * In preparation to erasing the whole module, erase and free all the
 * procedure descriptors in this module.
 */
void
erase_module_procs(pri *procs_in_module)
/* Locks: acquires ProcListLock. */
{
    pri *pd, **pf;

    a_mutex_lock(&ProcListLock);
    while(procs_in_module)
    {
	pd = procs_in_module;
	procs_in_module = pd->next_in_mod;
	(void) pri_abolish(pd);			/* abolish the procedure */
	_pri_clear_code(pd);			/* free code field */
	pf = &(DidPtr(pd->did)->procedure);	/* unlink from did-chain */
	while (*pf != pd)
	    pf = &((*pf)->nextproc);
	*pf = pd->nextproc;
	_free_pri(pd);				/* free descriptor */
    }
    a_mutex_unlock(&ProcListLock);
}


/*
 * Reclaim all the blocks that belong to one procedure. All the blocks
 * are linked together with the ProcLink item which is stored at the
 * beginning of each block, right after the memory header.
 */
void
reclaim_procedure(vmcode *code)
{
    vmcode		*next;

    do
    {
	next = (vmcode *) *code;
	if (BlockType(code) == GROUND_TERM)
	{
	    a_mutex_lock(&ProcChainLock);
	    add_proc_to_chain((pri *) code, &CompiledStructures);
	    a_mutex_unlock(&ProcChainLock);
	}
	else if (BlockType(code) == UNDEFINED_PROC)
	    hg_free_size((generic_ptr) code, sizeof(vmcode) * (UNDEF_CODE_SIZE + PROC_PREFIX_SIZE));
	else
	    hg_free((generic_ptr) code);
    }
    while (code = next);
}


/*
 * Reclaim the space occupied by all previously abolished or otherwise replaced
 * procedures. This should be done by a garbage collector only, because
 * some living pointers can still exist to the dead code. To make it
 * simple, we call this function only in the topmost top-level so that
 * there is a reasonable probability that the code is really dead.
 */
void
reclaim_abolished_procedures(void)
{
    proc_duet	*p_duet;
    vmcode	*code;
 
    a_mutex_lock(&ProcChainLock);
    for(;;)
    {
	p_duet = AbolishedProcedures;
	if (!p_duet)
	    break;
	code = (vmcode *) (p_duet->desc);
	reclaim_procedure(ProcHeader(code));
	delete_proc_from_chain((pri *) code, &AbolishedProcedures);
    }    
    for(;;)
    {
	p_duet = CompiledStructures;
	if (!p_duet)
	    break;
	code = (vmcode *) (p_duet->desc);
	reclaim_ground_structure(code);
	delete_proc_from_chain((pri *) code, &CompiledStructures);
    }    
    a_mutex_unlock(&ProcChainLock);
    return;
}


/*
 * Insert the procedure code into the abolished code list.
 */
void
remove_procedure(pri *proc)
{
    vmcode	*code = PriCode(proc);

    if (!code)
    	return;

    if (PriCodeType(proc) == VMCODE)
    {
	if (IsUndefined(code))
	{
	    reclaim_procedure(ProcHeader(code));
	}
	else if (PriFlags(proc) & PROC_DYNAMIC)
	{
#ifdef OLD_DYNAMIC
	    a_mutex_lock(&ProcChainLock);
	    add_proc_to_chain((pri *) code, &AbolishedDynProcedures);
	    /* Mark the abolish clock into the death of the first clause */
	    Death(StartOfAss(code)) = DynGlobalClock;
	    delete_proc_from_chain(proc, &DynamicProcedures);
	    a_mutex_unlock(&ProcChainLock);
#else
	    ec_free_dyn_code(code);
#endif
	    PriFlags(proc) &= ~PROC_DYNAMIC;
	}
	else
	{
	    a_mutex_lock(&ProcChainLock);
	    add_proc_to_chain((pri *) code, &AbolishedProcedures);
	    a_mutex_unlock(&ProcChainLock);
	}
    }
    PriCode(proc) = (vmcode *) 0;	/* just to catch bugs */
}


#ifdef PRINTAM
/*
 * Debugging support: Find out (the brute-force way)
 * which procedure a code address belongs to
 */
pri *ec_code_procedure(vmcode *code)
{
    int	idx = 0;
    dident functor;

    while (next_functor(&idx, &functor))
    {
	pri *pd;
	for(pd=DidPtr(functor)->procedure; IsVisibilityPri(pd); pd=pd->nextproc)
	{
	    if (pd->module_def == pd->module_ref
	     && PriCodeType(pd) == VMCODE
	     && PriCode(pd) <= code
	     && code < PriCode(pd) + ProcCodeSize(PriCode(pd)))
	    {
		return pd;
	    }
	}
    }
    return 0;
}
#endif


/*----------------------------------------------------------------------
 * Functions to enter kernel built-ins
 *----------------------------------------------------------------------*/

static pri *
_define_built_in(dident did1, int (*function) (/* ??? */), long int flags, dident mod, uint32 vis, int nondet)
{
    pri	*pd;
    pri_code_t pricode;
    type tm;

    tm.kernel = ModuleTag(d_.kernel_sepia);
    switch(vis)
    {
    case LOCAL:  pd = local_procedure(did1, mod, tm, PRI_CREATE); break;
    case EXPORT: pd = export_procedure(did1, mod, tm); break;
    default:     return 0;
    }

    pd->flags |= (flags & (UNIFTYPE|PROC_DEMON))|SYSTEM|DEBUG_DB|DEBUG_DF;
    if ((flags & UNIFTYPE) == U_SIMPLE)
	/* by default all simples bind the last argument */
	pd->mode = BoundArg(DidArity(PriDid(pd)), CONSTANT);

    if ((flags & CODETYPE) == VMCODE)
    {
	(void) b_built_code(pd, (word) function, nondet);
    }
    else
    {
	(void) ec_panic("Illegal codetype", "_define_built_in()");		\
    }
    return pd;
}

/*
 * A global built_in in sepia_kernel.
 */
pri *
built_in(dident did1, int (*func) (/* ??? */), long int flags)
{
    return _define_built_in(did1, func, flags, d_.kernel_sepia, EXPORT, 0);
}

/*
 * A local built_in in sepia_kernel.
 */
pri *
local_built_in(dident did1, int (*func) (/* ??? */), long int flags)
{
    return _define_built_in(did1, func, flags, d_.kernel_sepia, LOCAL, 0);
}

/*
 * An exported built_in in sepia_kernel.
 */
pri *
exported_built_in(dident did1, int (*func) (/* ??? */), long int flags)
{
    return _define_built_in(did1, func, flags, d_.kernel_sepia, EXPORT, 0);
}

/*
 * A local external in module
 * Function for C interface
 */
int
ec_external(dident did1, int (*func) (/* ??? */), dident module)
{
    return _define_built_in(did1, func, B_UNSAFE, module, LOCAL, 0)? PSUCCEED: PFAIL;
}

/*
 * Backtracking builtin  definition.
 */
pri *
b_built_in(dident did1, int (*func) (/* ??? */), dident module)
{
    return _define_built_in(did1, func, B_UNSAFE, module, LOCAL, 1);
}
