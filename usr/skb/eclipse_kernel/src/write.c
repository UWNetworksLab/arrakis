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
 * SEPIA C SOURCE MODULE
 *
 * VERSION	$Id: write.c,v 1.1 2008/06/30 17:43:59 jschimpf Exp $
 */

/*
 * IDENTIFICATION		write.c
 *
 * DESCRIPTION:		SEPIA terminal input/output routines
 *				by Dominique Henry de Villeneuve
 *
 * CONTENTS: 		write/1,2
 *			writeq/1,2
 *			write_canonical/1,2
 *			print/1,2
 *			printf_/4
 *			display/1
 *			depth/1
 *
 */

/*
 * INCLUDES:
 */

#include	"config.h"
#include	<math.h>
#include	<stdio.h>
#include	"sepia.h"
#include	"types.h"
#include	"embed.h"
#include	"mem.h"
#include	"error.h"
#include	"dict.h"
#include	"lex.h"
#include 	"io.h"
#include	"emu_export.h"
#include	"module.h"
#include	"property.h"

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#if STDC_HEADERS || HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#  define strchr index
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif


/* 
 * DEFINES
 */

#define 	ATOM		0
#define		OPERATOR	1

#define 	NO		0
#define		YES		1

#define UseDepth(id)		(!((id) & FULLDEPTH))

#define MacrosAllowed(idwrite)	(!((idwrite) & NO_MACROS))
#define GoalMacro(idwrite)	(idwrite & WRITE_GOAL ? TR_GOAL : \
				    (idwrite & WRITE_CLAUSE ? TR_CLAUSE : 0))

#define Handle_Type_Macro(t)						\
	if (MacrosAllowed(idwrite) && DidMacro(TransfDid(t))) {		\
	    pword *tr_res = _write_trafo(TransfDid(t),			\
				GoalMacro(idwrite),			\
				&idwrite, val, tag, module, mod_tag);	\
	    if (tr_res) {						\
		val.all = tr_res->val.all;				\
		tag.all = tr_res->tag.all;				\
		goto _pwrite_;	/* print the transformed term */	\
	    }								\
	}

/*
 * FUNCTION DECLARATIONS:
 */

int 
		    p_write3(value vals, type tags, value val, type tag, value vm, type tm),
		    p_writeq3(value vals, type tags, value val, type tag, value vm, type tm);

static int 
		    p_write(value val, type tag, value vm, type tm),
		    p_writeln(value vals, type tags, value val, type tag, value vm, type tm),
		    p_writeq(value val, type tag, value vm, type tm),
		    p_print(value val, type tag, value vm, type tm),
		    p_print3(value vals, type tags, value val, type tag, value vm, type tm),
		    p_printf5(value vs, type ts, value strval, type strtag, value lval, type ltag, value vm, type tm, value vfc, type tfc, value vse, type tse, value vle, type tle, value verr, type terr),
		    p_write_canonical(value val, type tag, value vm, type tm),
		    p_write_canonical3(value vals, type tags, value val, type tag, value vm, type tm),
		    p_write_term(value vs, type ts, value val, type tag, value vcm, type tcm, value vsm, type tsm, value vdepth, type tdepth, value vm, type tm),
		    p_display(value vs, type ts, value val, type tag),
		    p_output_mode(value val, type tag),
		    p_output_mode_mask(value val, type tag),

		    _get_mode_mask(char *string, int *clr_mask, int *mask),
		    _merge_output_modes(int mask, int remove, int add),
		    _handle_string_size(value v, type t, int quoted_or_base),
		    _handle_to_string(value v, type t, char *buf, int quoted_or_base),
		    _num_string_size(value v, type t, int quoted),
		    _int_to_string(value v, type t, char *buf, int quoted_or_base),
		    _float_to_string(value v, type t, char *buf, int precise),
		    _printf_asterisk(long int asterisk, pword **list, type arg_type, stream_id nst, char *par),
		    _print_var(int idwrite, value v, type t, stream_id str, int depth, dident module, type mod_tag, syntax_desc *sd),
		    _pwrite1(int idwrite, stream_id out, value val, type tag, int maxprec, int depth, dident module, type mod_tag, syntax_desc *sd, register int flags),
		    _is_proper_list(pword *list),
		    _write_args_from_list(int idwrite, stream_id out, pword *list, int depth, dident module, type mod_tag, syntax_desc *sd, int flags),
		    _write_quoted(int idwrite, stream_id out, char *name, register long int len, char quotechar, syntax_desc *sd, int depth),
		    _write_infix(int idwrite, stream_id out, dident d, register int flags, dident module, type mod_tag, syntax_desc *sd, pword *right, int depth),
		    _write_atom(int idwrite, stream_id out, dident d, int what, int flag, dident module, type mod_tag, syntax_desc *sd, int depth),
		    _write_string(int idwrite, stream_id out, char *start, long int length, int depth),
		    _portray_term(int idwrite, stream_id out, value val, type tag, dident module, type mod_tag);

static void	_output_mode_string(char *s, int mask);

static pword	*_write_trafo(dident d, int flags, int *idwrite, value val, type tag, dident module, type mod_tag);


/*
 * EXTERNAL VARIABLE DECLARATIONS:
 */

extern pword		*transf_meta_out(value val, type tag, pword *top, dident mod, pword *presult);
extern pword		*p_meta_arity_;


/*
 * STATIC VARIABLE DEFINITIONS: 
 */

static dident		d_dollar_var,
			d_portray1,
			d_portray2,
			d_print_attributes,
                        d_var_name,
                        d_vname2;

static char	output_mode_chars[OUTPUT_MODES+1] = "OD.QvVPKmGMTCN_IU";

static int	output_mode_mask = QUOTED | PRINT_CALL | ATTRIBUTE;


 /*
  * FUNCTION DEFINITIONS: 
  */

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 */

void
write_init(int flags)
{
    d_portray1 = in_dict("portray", 1);
    d_portray2 = in_dict("portray", 2);
    d_dollar_var = in_dict("$VAR", 1);
    d_print_attributes = in_dict("print_attributes", 2);
    d_var_name = in_dict("var_name", 0);
    d_vname2 = in_dict("vname", 2);

    tag_desc[TINT].string_size = _num_string_size;
    tag_desc[TINT].to_string = _int_to_string;
    tag_desc[TDBL].string_size = _num_string_size;
    tag_desc[TDBL].to_string = _float_to_string;
    tag_desc[THANDLE].string_size = _handle_string_size;
    tag_desc[THANDLE].to_string = _handle_to_string;

    if (!(flags & INIT_SHARED))
	return;
    
    PrintDepth = 20;

    (void) exported_built_in(in_dict("write_", 2), p_write, B_SAFE);
    (void) exported_built_in(in_dict("writeq_", 2), p_writeq, B_SAFE);
    (void) exported_built_in(in_dict("print_", 2), p_print, B_SAFE);
    (void) exported_built_in(in_dict("write_canonical_", 2), p_write_canonical, B_SAFE);
    (void) exported_built_in(in_dict("print_", 3), p_print3, B_SAFE);
    (void) exported_built_in(in_dict("printf_", 8), p_printf5, B_SAFE);
    (void) exported_built_in(in_dict("write_", 3), p_write3, B_SAFE);
    (void) local_built_in(in_dict("writeln_body", 3), p_writeln, B_SAFE);
    (void) exported_built_in(in_dict("writeq_", 3), p_writeq3, B_SAFE);
    (void) exported_built_in(in_dict("write_canonical_", 3), p_write_canonical3, B_SAFE);
    (void) exported_built_in(in_dict("write_term", 6), p_write_term, B_SAFE);
    (void) built_in(in_dict("display", 2), p_display, B_SAFE);
    (void) local_built_in(in_dict("output_mode", 1), p_output_mode, B_UNSAFE|U_SIMPLE);
    (void) local_built_in(in_dict("output_mode_mask", 1), p_output_mode_mask, B_UNSAFE|U_SIMPLE);
}


/*
 * visible_d_procedure() is the same as visible_procedure() except that
 * it only returns something if there is a CODE_DEFINED (callable)
 * procedure. It also does not set global_bip_error.
*/
static pri *
visible_d_procedure(dident functor, dident module, type module_tag)
{
    pri *pd = visible_procedure(functor, module, module_tag, 0);
    if (!pd)
    {
	Set_Bip_Error(0);
	return 0;
    }
    return PriFlags(pd) & CODE_DEFINED ? pd : 0;
}


#define Check_Stream(out, res)				\
   if (out == NO_STREAM) { Bip_Error(res) }		\
   if (!(IsWriteStream(out))) { Bip_Error(STREAM_MODE) }

#define	Write_Infix(ww, s, d, flags, mod, mt, sd, arg, narg)		\
	status = _write_infix(ww, s, d, flags, mod, mt, sd, narg, depth);\
	if (status < 0)							\
	   return(status);

#define	Write_Postfix(ww, s, d, flags, mod, mt, sd)			\
	if((status = ec_outfc( s, ' ')) < 0 || 				\
	(status = _write_atom(ww, s, d, OPERATOR, flags, mod, mt, sd, depth)) < 0)	\
	return(status);

#define	Write_Prefix(ww, s, d, flags, mod, mt, sd)			\
	if((status = _write_atom(ww, s, d, OPERATOR, flags, mod, mt, sd, depth)) < 0 || \
	(status = ec_outfc( s, ' ')) < 0) return(status);

#define	Write_Atom(ww, s, d, what, flags, mod, mt, sd)			\
    if((status = _write_atom(ww, s, d, what, flags, mod, mt, sd, depth)) < 0)	\
	return(status);

#define Pwrite(ww, s, v, t, mp, d, mod, mt, sd, flags) 			\
    if((status = _pwrite1(ww, s, v, t, mp, d, mod, mt, sd, flags)) < 0)	\
	return(status);
	
#define Write_Char(s,c) if ((status = ec_outfc(s,c)) < 0) return(status);

#define Write_Str(s,str,l) if ((status = ec_outf(s,str,l)) < 0) return(status);

#define Write_Comma(s) \
	Write_Char(s, ','); \
	if (!(idwrite & WRITE_COMPACT)) { Write_Char(s, ' '); }

#define Next_Element(element, list, Return)			\
	{							\
	    if (list)						\
	    {							\
		element = list++;				\
		Dereference_(list)				\
		Dereference_(element)				\
		if (IsNil(list->tag))				\
		    list = 0;					\
		else if (!IsList(list->tag)) {			\
		    Return(TYPE_ERROR);				\
		}						\
		else {						\
		    list = list->val.ptr;			\
		}						\
	    }							\
	    else {						\
		Return(BAD_ARGUMENT_LIST);			\
	    }							\
	}

#define Get_Counter(start,ptr,c)				\
	c = 0;							\
	ptr = start;						\
	while (*(ptr) >= '0' && *(ptr) <= '9')			\
	    c = c * 10 + *(ptr)++ - '0';


#define MAXPREC		((sd->options & LIMIT_ARG_PRECEDENCE) ? 999 : 1200)


/*
	write_(Term, Module)
 	writes the Prolog term (tag,val) to the current output stream.
 	The term is written according to the current operator
 	declarations and spaces are inserted to separate operators
 	where necessary.
	Functors, atoms and strings are not quoted.
*/
static int
p_write(value val, type tag, value vm, type tm)
{
    int		res;
    Check_Module(tm, vm);
    Lock_Stream(current_output_);
    res = ec_pwrite(0, 0, current_output_, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(current_output_);
    return res;
}

/*
	writeq_(Term, Module)
	The Prolog term is written to the current output stream
	according to the current operator declarations.
	Functors, atoms and strings are quoted.
*/
static int
p_writeq(value val, type tag, value vm, type tm)
{
    int		res;
    Check_Module(tm, vm);
    Lock_Stream(current_output_);
    if (IsAtom(tag) && val.did == d_.eocl)
	res = ec_outf(current_output_, "'.'", 3);
    else
	res = ec_pwrite(0, QUOTED|FULLDEPTH|VAR_NUMBERS|STD_ATTR|NO_MACROS,
		    current_output_, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(current_output_);
    return res;
}


/*
	writeq_(Stream, Term, Module)
*/
int
p_writeq3(value vals, type tags, value val, type tag, value vm, type tm)
{
    int		res;
    stream_id	out = get_stream_id(vals, tags, SWRITE, &res);

    Check_Stream(out, res);
    Check_Module(tm, vm);
    Lock_Stream(out);
    if (IsAtom(tag) && val.did == d_.eocl)
     	res = ec_outf(out, "'.'", 3);
    else
	res = ec_pwrite(0, QUOTED|FULLDEPTH|VAR_NUMBERS|STD_ATTR|NO_MACROS,
		    out, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(out);
    return res;
}

/*
	write_canonical_(Term, Module)
*/
static int
p_write_canonical(value val, type tag, value vm, type tm)
{
    int		res;
    Check_Module(tm, vm);
    Lock_Stream(current_output_);
    if (IsAtom(tag) && val.did == d_.eocl)
	res = ec_outf(current_output_, "'.'", 3);
    else
	res = ec_pwrite(0, QUOTED|FULLDEPTH|VAR_NUMBERS|STD_ATTR|NO_MACROS|
		    CANONICAL|DOTLIST,
		    current_output_, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(current_output_);
    return res;
}

/*
	write_canonical_(Stream, Term, Module)
*/
static int
p_write_canonical3(value vals, type tags, value val, type tag, value vm, type tm)
{
    int		res;
    stream_id	out = get_stream_id(vals, tags, SWRITE, &res);

    if (IsAtom(tag) && val.did == d_.eocl)
    {
     	return(ec_outf(out, "'.'", 3));
    }
    Check_Stream(out, res);
    Check_Module(tm, vm);
    Lock_Stream(out);
    res = ec_pwrite(0, QUOTED|FULLDEPTH|VAR_NUMBERS|STD_ATTR|NO_MACROS|
		CANONICAL|DOTLIST,
		    out, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(out);
    return res;
}

/*
 	write_(Stream, Term, Module)
 	writes the Prolog term (tag,val) to the specified output stream.
 	The term is written according to the current operator
 	declarations and spaces are inserted to separate operators
 	where necessary.
*/
int
p_write3(value vals, type tags, value val, type tag, value vm, type tm)
{
    int		res;
    stream_id out = get_stream_id(vals, tags, SWRITE, &res);

    Check_Stream(out, res);
    Check_Module(tm, vm);
    Lock_Stream(out);
    res = ec_pwrite(0, 0, out, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(out);
    return res;
}


/* 
 * writeln is in C because we want it atomic and the correct flushing
 * behaviour (like nl)
 */
static int
p_writeln(value vals, type tags, value val, type tag, value vm, type tm)
{
    int		res;
    stream_id out = get_stream_id(vals, tags, SWRITE, &res);

    Check_Stream(out, res);
    Check_Module(tm, vm);
    Lock_Stream(out);
    res = ec_pwrite(0, 0, out, val, tag, 1200, 0, vm.did, tm);
    if (res == PSUCCEED)
	res = ec_newline(out);
    Unlock_Stream(out);
    return res;
}

/*
 	print_(Term, Module)
 	writes the Prolog term (tag,val) using portray/1,2 if it exists.
 	The term is written according to the current operator
 	declarations and spaces are inserted to separate operators
 	where necessary.
*/
static int
p_print(value val, type tag, value vm, type tm)
{
    int		res;

    Check_Module(tm, vm);
    Lock_Stream(current_output_);
    res = ec_pwrite(0, PRINT_CALL, current_output_, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(current_output_);
    return res;
}

/*
 	print_(Stream, Term, Module)
 	writes the Prolog term (tag,val) to the specified output stream,
	possibly using portray/1,2 to output it.
 	The term is written according to the current operator
 	declarations and spaces are inserted to separate operators
 	where necessary.
*/
static int
p_print3(value vals, type tags, value val, type tag, value vm, type tm)
{
    int		res;
    stream_id out = get_stream_id(vals, tags, SWRITE, &res);

    Check_Stream(out, res);
    Check_Module(tm, vm);
    Lock_Stream(out);
    res = ec_pwrite(0, PRINT_CALL, out, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(out);
    return res;
}


/*
 *	display(Stream, Term)
 *	The output is written (even for the operators) in functional form.
 *	Functors, atoms and strings are not quoted.
*/
static int
p_display(value vs, type ts, value val, type tag)
{
    int		res;
    stream_id out = get_stream_id(vs, ts, SWRITE, &res);

    Check_Stream(out, res);
    /* the module tag is not meaningful here				*/
    Lock_Stream(out);
    res = ec_pwrite(0, CANONICAL|DOTLIST,
		    out, val, tag, 1200, 0, d_.default_module, tdict);
    Unlock_Stream(out);
    return res;
}


/*
 * ec_pwrite() - write a Prolog term
 *
 * When writing any meta variables are marked (tag is modified) these marks
 * are trailed. This function is simply a wrapper round prwite1() which
 * does initialisation and finalisation, while pwrite() is recursive.
 */
int
ec_pwrite(int mode_clr, int mode_set, stream_id out, value val, type tag, int maxprec, int depth, dident module, type mod_tag)
{
    pword			**old_tt = TT, *old_tg = TG, *old_ld = LD;
    int				idwrite;
    int				result;

    /* Catch null stream here because some code within _pwrite1()
     * assumes the presence of a stream buffer! */
    if ((StreamMode(out) & STYPE) == SNULL)
	return PSUCCEED;
    	
    /*
     * Merge the stream's default output mode settings with the modes
     * for this particular call
     */
    idwrite = _merge_output_modes(StreamOutputMode(out), mode_clr, mode_set);

    /*
     * For backward compatibility, map obsolete syntax options to output modes
     */
    if (ModuleSyntax(module)->options & DOLLAR_VAR)
    	idwrite |= OUT_DOLLAR_VAR;
    /* not fully compatible:
    if (ModuleSyntax(module)->options & DENSE_OUTPUT)
    	idwrite |= WRITE_COMPACT;
    */

    /*
     * If 0, inherit print depth from stream or from global setting
     * (if the FULLDEPTH flag is set, this is irrelevant)
     */
    if (depth == 0)
    {
	depth = StreamPrintDepth(out);
	if (depth == 0)
	    depth = PrintDepth;
    }

    /*
     * If the module is locked we cannot call any print handlers
     * or look up the visible operators.
     * In principle, we should also not see the locked module's
     * syntax, but that may be unnecessarily restrictive.
     */
    if (UnauthorizedAccess(module, mod_tag))
    	idwrite = idwrite & ~(ATTRIBUTE|PRINT|PRINT1|PRINT_CALL)
			|NO_MACROS|CANONICAL;

    /*
     * If needed, do the expensive procedure lookups for portray/1,2
     * here and set PRINT and PRINT1 flags accordingly.
     */
    if (idwrite & PRINT_CALL)
    {
	if (visible_d_procedure(d_portray2, module, mod_tag))
	    idwrite |= PRINT;
	else if (visible_d_procedure(d_portray1, module, mod_tag))
	    idwrite |= PRINT | PRINT1;
    }

    result = _pwrite1(idwrite, out, val, tag, maxprec, depth,
			module, mod_tag, ModuleSyntax(module), ARGLAST);

    /*
     * Pop stuff that may have been left by write macros and
     * untrail all marking that has been done during printing.
     */
    Untrail_Variables(old_tt); TG = old_tg; LD = old_ld;
    return result;

}

/*
 * _pwrite1() - write a Prolog term
 *
 * idwrite: flags for the different write options (see io.h)
 *	CANONICAL	ignore operators
 *	FULLDEPTH	ignore depth
 *	DOTLIST		write lists in dot notation
 *	QUOTED		print quotes when needed
 *	VAR_NUMBERS	print var number only
 *	VAR_NAMENUM	print var name (if available) and number
 *	VAR_ANON	print var as _
 *	PRINT_CALL	print was called, use portray
 *	PORTRAY_VAR	call portray even for variables
 *	WRITE_GOAL	print with goal output macros
 *	ATTRIBUTE	print attributes of metaterms in user format
 *	STD_ATTR	print attributes of metaterms in standard format
 *	NO_MACROS	don't apply write macros
 *	PRINT		a portray predicate exists
 *	PRINT1		only portray/1 exists
 *	VARTERM		print variables as '_'(...)
 * flags: further context information for writeq
 *	ARGOP		immediate argument of any operator
 *	ARGYF		immediate argument of YF or YFX operator
 *	ARGLAST		last term, i.e. a delimiter follows
 *	ARGLIST		inside a bracketed list, used to handle
 *			bars that occur as atoms or operators
 *	ARGTERM		inside a structure argument, used to handle
 *			commas that are not argument separators
 *	ARGPREF		term _textually_ follows a prefix operator
 * maxprec: the maximum precedence that may be printed without brackets
 */

#define NumberNeedsBrackets(val) \
    ((idwrite & QUOTED) && (flags & ARGPREF) && (val < 0))

static int
_pwrite1(int idwrite, stream_id out, value val, type tag, int maxprec, int depth, dident module, type mod_tag, syntax_desc *sd, register int flags)
{
    register pword	*arg;
    register int	status, arity;
    register dident	d;
    opi			*d_opi_desc;
    int			res;

_pwrite_:
    if (UseDepth(idwrite) && depth <= 0)
	return (ec_outf(out, "...", 3));

    if (IsRef(tag))
	if ((idwrite & PRINT) && (idwrite & PORTRAY_VAR || IsMeta(tag)) &&
			_portray_term(idwrite, out, val, tag, module, mod_tag))
	    return PSUCCEED;
	else
	{
	    return _print_var(idwrite, val.ptr->val, val.ptr->tag, out, depth,
					module, mod_tag, sd);
	}
    else if (idwrite & PRINT && _portray_term(idwrite, out, val, tag, module, mod_tag))
	return PSUCCEED;

    switch (TagType(tag))
    {
    case TDICT:
	Handle_Type_Macro(TDICT)
	if (MacrosAllowed(idwrite) && DidMacro(val.did))
	{
	    pword *narg;
	    if ((narg = _write_trafo(val.did, GoalMacro(idwrite),
				&idwrite, val, tag, module, mod_tag)))
	    {
		val.all = narg->val.all;
		tag.all = narg->tag.all;
		idwrite &= ~(WRITE_GOAL|WRITE_CLAUSE);
		goto _pwrite_;		/* print the transformed term */
	    }
	}
	return _write_atom(idwrite,out,val.did,ATOM,flags,module,mod_tag, sd, depth);

    case TINT:
	Handle_Type_Macro(TINT)
	if NumberNeedsBrackets(val.nint)
	    return (p_fprintf(out, "(%ld)", val.nint));
	else
	    return (p_fprintf(out, "%ld", val.nint));

    case TDBL:
	Handle_Type_Macro(TDBL)
	{
	    char fbuf[32];
	    int size = _float_to_string(val, tag, fbuf, idwrite & QUOTED);
	    if (NumberNeedsBrackets(Dbl(val)))
	    {
		if ((status = ec_outfc(out, '(')) < 0 ||
		    (status = ec_outf(out, fbuf, size)) < 0 ||
		    (status = ec_outfc(out, ')')) < 0)
			return status;
		return status;
	    }
	    else
		return ec_outf(out, fbuf, size);
	}

    case TSTRG:
	Handle_Type_Macro(TSTRG)
	return  (idwrite & QUOTED)  ?
		_write_quoted(idwrite, out, StringStart(val), StringLength(val),
					(char) sd->current_sq_char, sd, depth) :
		_write_string(idwrite, out, StringStart(val),
				StringLength(val), depth);

    case TNIL:
	Handle_Type_Macro(TDICT)
	return (ec_outf(out, "[]", 2));

    case TEXTERN:	/* shouldn't occur */
        return p_fprintf(out, "EXTERN_%lx", val.nint);

    case TPTR:
        return p_fprintf(out, "PTR_%lx", val.ptr);

    case TSUSP:
	Handle_Type_Macro(TSUSP)
	if (!val.ptr)
	    return p_fprintf(out, "'SUSP-0-dead'");
	res = SuspDebugInvoc(val.ptr);
        status = p_fprintf(out, "'SUSP-%s%d-%s'",
		res ? "" : "_", res ? res : val.ptr - TG_ORIG, 
		SuspDead(val.ptr) ? "dead" : SuspScheduled(val.ptr) ? "sched" : "susp");
	if (status < 0)
	    return status;
#if 0
	if (SuspDead(val.ptr) || !(idwrite & QUOTED))
	    return PSUCCEED;
	arg = &val.ptr[SUSP_GOAL];	/* print: (Goal,Module) */
	arity = 2;
	goto _write_args_;		/* (arg,arity) */
#else
	return PSUCCEED;
#endif

    case THANDLE:
	Handle_Type_Macro(THANDLE)
	if (ExternalClass(val.ptr)->to_string && ExternalData(val.ptr))
	{
	    int bufsize = 1 + (ExternalClass(val.ptr)->string_size)(ExternalData(val.ptr), idwrite&QUOTED?1:0);
	    char *buf = (char *) hp_alloc_size(bufsize);
	    int len = (ExternalClass(val.ptr)->to_string)(ExternalData(val.ptr), buf, idwrite&QUOTED?1:0);
	    status = ec_outf(out, buf, len);
	    hp_free_size((generic_ptr) buf, bufsize);
	    return status;
	}
	else
	{
	    return p_fprintf(out, "'HANDLE'(16'%08x)", ExternalData(val.ptr));
	}

    case TPROC:		/* an atom goal in the compiler */
	return _write_atom(idwrite, out, PriDid((pri *) (val.ptr)),
		ATOM,flags,module,mod_tag, sd, depth);

    case TCOMP:
    case TGRS:		/* a ground structure in the compiler */
	if (val.ptr == 0) {	/* e.g. default WL */
	    return p_fprintf(out, "BAD_TERM_0x%lx_0x%lx", val.all, tag.all);
	}
	Handle_Type_Macro(TCOMP)
	if (SameTypeC(val.ptr->tag, TPROC))
	{
	    /* We are inside the compiler, change TPROC to TDICT */
	    d = PriDid((pri *) (val.ptr->val.ptr));
	}
	else
	    d = val.ptr->val.did;	/* did of the functor */
	arg = (val.ptr) + 1;
_write_structure_:			/* (d, arg) */
	arity = DidArity(d);
	if (!(idwrite & CANONICAL))
	{
	    dident hd = d;
	    if (d == d_.rulech2) {
		pword		*p = val.ptr + 1;
		Dereference_(p);
		if (IsAtom(p->tag))
		    hd = p->val.did;
		else if (IsStructure(p->tag))
		    hd = p->val.ptr->val.did;
	    }
	    if (MacrosAllowed(idwrite) && DidMacro(hd))	/* output macros */
	    {
		pword *narg;
		if ((narg = _write_trafo(hd, GoalMacro(idwrite),
				    &idwrite, val, tag, module, mod_tag)))
		{
		    val.all = narg->val.all;
		    tag.all = narg->tag.all;
		    idwrite &= ~(WRITE_GOAL|WRITE_CLAUSE);
		    goto _pwrite_;	/* print the transformed term */
		}
	    }
	    idwrite &= ~(WRITE_GOAL|WRITE_CLAUSE);

	    /*
	     * Check for all the functors that can have special syntax
	     */
	    if (d == d_dollar_var && (idwrite & OUT_DOLLAR_VAR)) /* '$VAR'/1 */
	    {
		pword *narg = arg;
		Dereference_(narg);
		switch (TagType(narg->tag))
		{
		case TINT:
		    if (narg->val.nint < 0)
			break;
		    if ((status = ec_outfc(out, 'A' + (char)(narg->val.nint % 26))) < 0)
			return (status);
		    if (narg->val.nint / 26)
			return p_fprintf(out, "%ld", narg->val.nint / 26);
		    return PSUCCEED;
		case TSTRG:
		    return ec_outf(out, StringStart(narg->val),
					(int) StringLength(narg->val));
		case TDICT:
		    return ec_outf(out, DidName(narg->val.did),
					(int) DidLength(narg->val.did));
		case TNIL:
		    return ec_outf(out, "[]", 2);
		}
		/* else print the structure normally */
	    }
	    else if (d == d_.nilcurbr1)	/* special case {}/1 */
	    {
		if ((status = ec_outfc(out, '{')) < 0)
		    return (status);
		Dereference_(arg);
		status = _pwrite1(idwrite, out, arg->val, arg->tag, MAXPREC, 
				 depth-1, module, mod_tag, sd, 0);
		if (status < 0 || (status = ec_outfc(out, '}')) < 0)
		    return (status);
		return (PSUCCEED);
	    }
 	    else if (d == d_.subscript  &&  !(sd->options & NO_ARRAY_SUBSCRIPTS	))
 	    {
		pword *arg1 = arg;
 		pword *arg2 = arg + 1;
 		Dereference_(arg1);
 		Dereference_(arg2);
 		if (IsList(arg2->tag) && (IsStructure(arg1->tag) ||
 		    IsRef(arg1->tag) && !IsMeta(arg1->tag) ||
 		    IsAtom(arg1->tag) && (sd->options & ATOM_SUBSCRIPTS)))
 		{
 		    Pwrite(idwrite|CANONICAL, out, arg1->val, arg1->tag, MAXPREC,
 			     depth, module, mod_tag, sd, flags);
 		    Pwrite(idwrite, out, arg2->val, arg2->tag, MAXPREC,
 			     depth, module, mod_tag, sd, flags);
 		    return (PSUCCEED);
 		}
 	    }
	    else if (d == d_.with_attributes2  &&  !(sd->options & NO_ATTRIBUTES))
	    {
		pword *arg1 = arg;
 		pword *arg2 = arg + 1;
 		Dereference_(arg1);
 		Dereference_(arg2);
		if ((IsRef(arg1->tag) && !IsMeta(arg1->tag)) && _is_proper_list(arg2))
		{
		    Pwrite(idwrite, out, arg1->val, arg1->tag, MAXPREC, 
			     depth, module, mod_tag, sd, ARGTERM | ARGLAST);
		    Write_Char(out, '{');
		    status = _write_args_from_list(idwrite, out, arg2, depth, module, mod_tag, sd, flags);
		    if (status < 0) return status;
		    Write_Char(out, '}');
		    return (PSUCCEED);
		}
	    }
	    else if (d == d_.apply2  &&  (sd->options & VAR_FUNCTOR_IS_APPLY))
	    {
		pword *arg1 = arg;
 		pword *arg2 = arg + 1;
 		Dereference_(arg1);
 		Dereference_(arg2);
		if ((IsRef(arg1->tag) && !IsMeta(arg1->tag)) && _is_proper_list(arg2))
		{
		    Pwrite(idwrite, out, arg1->val, arg1->tag, MAXPREC, 
			     depth, module, mod_tag, sd, ARGTERM | ARGLAST);
		    Write_Char(out, '(');
		    status = _write_args_from_list(idwrite, out, arg2, depth, module, mod_tag, sd, flags);
		    if (status < 0) return status;
		    Write_Char(out, ')');
		    return (PSUCCEED);
		}
	    }
	    else if (d == d_.with2  &&  !(sd->options & NO_CURLY_ARGUMENTS))
	    {
		pword *arg1 = arg;
 		pword *arg2 = arg + 1;
 		Dereference_(arg1);
 		Dereference_(arg2);
		if (IsAtom(arg1->tag) && (IsNil(arg2->tag) || _is_proper_list(arg2)))
		{
		    Write_Atom(idwrite, out, arg1->val.did, ATOM, flags & ARGLIST, module, mod_tag, sd);
		    Write_Char(out, '{');
		    status = _write_args_from_list(idwrite, out, arg2, depth, module, mod_tag, sd, flags);
		    if (status < 0) return status;
		    Write_Char(out, '}');
		    return (PSUCCEED);
		}
	    }

	    /*
	     * Check whether the functor is an operator
	     */
	    if ((d_opi_desc = visible_op(d, module, mod_tag, &res)))
	    {			/* val is an operator */
		int		prec;
		int		openpar = NO;
		long		assoc;
		opi		*post_infix = 0;
		pword		*narg;

		prec = GetOpiPreced(d_opi_desc);
		assoc = GetOpiAssoc(d_opi_desc);
	    	narg = arg + 1;
		if (IsPostfixAss(assoc))
		{
		    dident		atom = add_dict(d, 0);
		    post_infix = visible_infix_op(atom, module, mod_tag, &res);
		}
		if (  prec > maxprec 
		    || d == d_.comma && (flags & ARGTERM)
		    || flags & ARGYF && prec == maxprec &&
			(assoc == FY || assoc == XFY)
		    || post_infix && !(flags & ARGLAST)
		   )
		{
		    flags = flags  & ~(ARGTERM | ARGLIST | ARGPREF) | ARGLAST;
		    openpar = YES;
		    Write_Char(out, '(');
		}
		Dereference_(arg);
		if (arity == 1)
		{
		    switch (assoc)
		    {
		    case FX:
			prec -= 1;
		    case FY:
			if (d == d_.plus1  ||  d == d_.minus1)
			{
			    /* ignore operators to avoid confusion
			     * with signed numbers	*/
			    Write_Atom(idwrite, out, d, ATOM, flags & ARGLIST,
					 module, mod_tag, sd);
			    Write_Char(out, '(');
			    Pwrite(idwrite, out, arg->val, arg->tag,
				    MAXPREC, depth - 1, module, mod_tag,
				    sd, ARGTERM | ARGLAST);
			    Write_Char(out, ')');
			}
			else
			{
			    Write_Prefix(idwrite, out, d, flags & ARGLIST,
					 module, mod_tag, sd);
			    Pwrite(idwrite, out, arg->val, arg->tag,
				    prec, depth - 1, module, mod_tag,
				    sd, flags & (ARGTERM | ARGLIST | ARGLAST)
				    | ARGOP | ARGPREF);
			}
			break;

		    case YF:
			Pwrite(idwrite, out, arg->val, arg->tag,
				prec, depth - 1, module, mod_tag, sd,
				flags & ~ARGLAST & (ARGTERM | ARGLIST | ARGPREF)
				| ARGYF | ARGOP);
			Write_Postfix(idwrite, out, d, flags & ARGLIST,
				      module, mod_tag, sd);
			break;

		    case XF:
			Pwrite(idwrite, out, arg->val, arg->tag,
				prec - 1, depth - 1, module, mod_tag, sd,
				flags & ~ARGLAST & (ARGTERM | ARGLIST | ARGPREF)
				| ARGOP);
 			Write_Postfix(idwrite, out, d, flags & ARGLIST,
				      module, mod_tag, sd);
			break;
 		    }
		}
		else	/* arity = 2 */
		{
		    Dereference_(narg);
		    switch (assoc)
		    {
		    case XFX:
		    case XFY:
		    case YFX:
			Pwrite(idwrite, out, arg->val, arg->tag,
				assoc == YFX ? prec : prec - 1,
				depth - 1, module, mod_tag, sd,
				flags & ~ARGLAST & (ARGTERM | ARGLIST | ARGPREF)
				| ARGOP);
			Write_Infix(idwrite, out, d, flags & ARGLIST,
				    module, mod_tag, sd, arg, narg);
			Pwrite(idwrite, out, narg->val, narg->tag,
				assoc == XFY ? prec : prec - 1,
				depth - 1, module, mod_tag, sd,
				flags & (ARGTERM | ARGLIST | ARGLAST)
				| ARGOP);
			break;

		    case FXX:
		    case FXY:
			Write_Prefix(idwrite, out, d, flags & ARGLIST,
				     module, mod_tag, sd);
			Pwrite(idwrite, out, arg->val, arg->tag,
				prec - 1, depth - 1, module, mod_tag, sd,
				flags & ~ARGLAST & (ARGTERM | ARGLIST)
				| ARGOP | ARGPREF);
			Write_Char(out, ' ');
			Pwrite(idwrite, out, narg->val, narg->tag,
				assoc == FXY ? prec : prec - 1,
				depth - 1, module, mod_tag, sd,
				flags & (ARGTERM | ARGLIST | ARGLAST)
				| ARGOP);
			break;
		    }
		}
		if (openpar == YES)
		{
		    Write_Char(out, ')');
		}
		return (PSUCCEED);
	    }
	    /* else do as for a normal functor */
	}

	/* normal functor or we ignore operators */

	Write_Atom(idwrite, out, d, ATOM, flags & ARGLIST, module, mod_tag, sd);

_write_args_:				/* (arg,arity) */
	Write_Char(out, '(');
	if (UseDepth(idwrite) && depth <= 1)
	{
	    /* abbreviate even more: only one ... for all arguments */
	    if ((status = ec_outf(out, "...", 3)) < 0)
	    	return status;
	}
	else if (arity > 0)		/* should always be true */
	{
	    for(;;)
	    {
		pword *narg = arg + 1;
		Dereference_(arg);
		Pwrite(idwrite, out, arg->val, arg->tag, MAXPREC, 
				 depth-1, module, mod_tag, sd, ARGTERM | ARGLAST);
		if (--arity == 0)
		    break;
		Write_Comma(out);
		arg = narg;
	    }
	}
	Write_Char(out, ')');
	break;

    case TLIST:
    case TGRL:		/* a ground list in the compiler */
	Handle_Type_Macro(TCOMP)
	if (idwrite & DOTLIST)
	{
	    d = d_.list;		/* write list in ./2 notation */
	    arg = val.ptr;
	    goto _write_structure_;
	}
	else				/* write list in [ ] notation */
	{
	    pword *tail;
	    if (MacrosAllowed(idwrite) && DidMacro(d_.list))	/* output macros */
	    {
		pword *narg;
		if ((narg = _write_trafo(d_.list, GoalMacro(idwrite),
				    &idwrite, val, tag, module, mod_tag)))
		{
		    val.all = narg->val.all;
		    tag.all = narg->tag.all;
		    idwrite &= ~(WRITE_GOAL|WRITE_CLAUSE);
		    goto _pwrite_;	/* print the transformed term */
		}
	    }
	    idwrite &= ~(WRITE_GOAL|WRITE_CLAUSE);

	    if ((status = ec_outfc(out, '[')) < 0)
		return (status);
	    arg = val.ptr;
	    tail = arg + 1;
	    Dereference_(arg)
	    status = _pwrite1(idwrite, out, arg->val, arg->tag, MAXPREC, 
		     --depth, module, mod_tag, sd, ARGTERM | ARGLIST | ARGLAST);
	    if (status < 0)
		return (status);
	    while (!(UseDepth(idwrite) && depth <= 0))
	    {
		Dereference_(tail);
		switch (TagType(tail->tag))
		{
		case TNIL:
		    break;
		case TLIST:
		    Write_Comma(out);
		    tail = tail->val.ptr;
		    arg = tail++;
		    Dereference_(arg);
		    status = _pwrite1(idwrite, out, arg->val, arg->tag, MAXPREC, 
				    --depth, module, mod_tag, sd,
				    ARGTERM | ARGLIST | ARGLAST);
		    if (status < 0)
			return (status);
		    continue;
		default:
		    if ((status = ec_outfc(out, '|')) < 0)
			return (status);
		    status = _pwrite1(idwrite, out, tail->val, tail->tag, 
				    MAXPREC, --depth, module, mod_tag,
				    sd, ARGTERM | ARGLIST | ARGLAST);
		    if (status < 0)
			return (status);
		    break;
		}
		break;
	    }
	    return (ec_outfc(out, ']'));
	}


/***** EXTENSION SLOT WRITE *****/

    default:
	if (TagType(tag) >= 0 && TagType(tag) <= NTYPES)
	{
	    value		vmodule;
	    Handle_Type_Macro(TagType(tag))
	    vmodule.did = module;
	    return tag_desc[TagType(tag)].write(idwrite & QUOTED, out, val, tag);
	}
	else
	    p_fprintf(out, "BAD_TERM_0x%lx_0x%lx", val.all, tag.all);
	Succeed_
    }
    return (PSUCCEED);
}


static int
_is_proper_list(pword *list)
{
    if (!IsList(list->tag))
    	return 0;
    for(;;)
    {
	list = list->val.ptr + 1;
	Dereference_(list);
	if (!IsList(list->tag))
	    return IsNil(list->tag);
    }
}



/* CAUTION: this function assumes that list is a proper list! */
static int
_write_args_from_list(int idwrite, stream_id out, pword *list, int depth, dident module, type mod_tag, syntax_desc *sd, int flags)
{
    pword *arg;
    int status;
    if (IsNil(list->tag))
	return PSUCCEED;
    if (UseDepth(idwrite) && depth <= 1)
    {
	/* abbreviate even more: only one ... for all arguments */
	Write_Str(out, "...", 3);
    }
    for(;;)
    {
	list = list->val.ptr;
	arg = list++;
	Dereference_(arg);
	Pwrite(idwrite, out, arg->val, arg->tag, MAXPREC, 
		 depth-1, module, mod_tag, sd, ARGTERM | ARGLAST);
	Dereference_(list);
	if (IsList(list->tag))
	{
	    Write_Comma(out);
	    continue;
	}
	return PSUCCEED;
    }
}


static pword *
_write_trafo(dident d, int flags, int *idwrite, value val, type tag, dident module, type mod_tag)
{
    extern pword *trafo_term(dident tr_did, int flags, dident mv, type mt, int *tr_flags);
    extern int do_trafo(pword *term);
    int macroflags;
    register pword *result, *tr_goal;
    pword	*pw;

    if (d == D_UNKNOWN) {	/* meta attribute */
	pw = TG;
	TG += 3;
	Check_Gc;
	pw[0].val.did = d_print_attributes;
	pw[0].tag.kernel = TDICT;
	pw[1].val.all = val.all;
	pw[1].tag.kernel = tag.kernel;
	pw[2].tag.kernel = TREF;
	pw[2].val.ptr = pw + 2;
	tr_goal = pw;
	macroflags = 0;
	result = pw + 2;
    } else {
	tr_goal = trafo_term(d, TR_WRITE|TR_TOP|flags, module, mod_tag, &macroflags);
	if (tr_goal)
	{
	    TransfTermIn(tr_goal)->val.all = val.all;
	    TransfTermIn(tr_goal)->tag.kernel = tag.kernel;
	    result = TransfTermOut(tr_goal);
	} else
	    return (pword *) 0;
    }

    if (do_trafo(tr_goal) == PSUCCEED)
    {
	Dereference_(result);
	/* to avoid looping, check if something was actually transformed */
	if (result->val.all != val.all || result->tag.all != tag.all) {
	    if (macroflags & TR_PROTECT)
		*idwrite |= NO_MACROS;
	    return result;
	}
    }
    return (pword *) 0;
}


/*
 * Call portray/1,2 on a specified term. Returns 1 iff the call succeeded.
 */
static int
_portray_term(int idwrite, stream_id out, value val, type tag, dident module, type mod_tag)
{
    value		v1, v2;
    int			status;
    pword		goal[3];
    int			i = 1;

    v1.ptr = goal;
    goal[0].tag.kernel = TDICT;
    if (!(idwrite & PRINT1))
    {
	goal[0].val.did = d_portray2;
	goal[i].tag = tint;
	goal[i++].val.nint = StreamNr(out);
    }
    else
	goal[0].val.did = d_portray1;
    goal[i].tag = tag;
    goal[i].val = val;
    v2.did = module;
    Unlock_Stream(out);	/* release the stream lock while executing Prolog */
    status = query_emulc(v1, tcomp, v2, mod_tag);
    Lock_Stream(out);
    return (status == PSUCCEED) ? 1 : 0;
}

/*
 * Try to avoid space printing around some frequent infix operators.
 * Except for comma, make it symmetric.
 *
 * TODO: This should be done very differently. Rather than trying to
 * look ahead to the right hand side argument, we should remember the
 * last character of the operator and lazily insert a space if necessary
 * when we are about to print the first character of the next item.
 */
static int
_write_infix(int idwrite, stream_id out, dident d, register int flags, dident module, type mod_tag, syntax_desc *sd, pword *right, int depth)
{
    int		status;
    int		spaces = 0;

    if ((sd->options & DENSE_OUTPUT || idwrite & WRITE_COMPACT) &&  d != d_.comma)
    {
	int last_left, first_right;
	int first = sd->char_class[*DidName(d)];
	int last = sd->char_class[*(DidName(d) + DidLength(d) - 1)];
	last_left = sd->char_class[(unsigned char)StreamLastWritten(out)];
	if (IsNumber(right->tag))
	{
	    pword sign;
	    int res = tag_desc[TagType(right->tag)].arith_op[ARITH_SGN](right->val, &sign);
	    /* res can be ARITH_EXCEPTION for zero-spanning breals! */
	    if (res != PSUCCEED  ||  sign.val.nint < 0)
		first_right = sd->char_class['-'];
	    else
		first_right = N;
	}
	else if (IsAtom(right->tag))
	    first_right = sd->char_class[*(DidName(right->val.did))];
	else
	    first_right = -1;

	if (last_left == first || Alphanum(last_left) && Alphanum(first) ||
	    last == first_right || Alphanum(last) && Alphanum(first_right) ||
	    (!IsNumber(right->tag) && !IsAtom(right->tag) && !IsList(right->tag)))
	{
	    spaces = 1;
	}
    }
    else
    {
	spaces = 1;
    }
    if (spaces && d != d_.comma)
	if ((status = ec_outfc(out, ' ')) < 0)
	    return status;
    if ((status = _write_atom(idwrite, out, d, OPERATOR, flags,
						    module, mod_tag, sd, depth)) < 0)
	return status;
    if (spaces && (d != d_.comma || !(idwrite & WRITE_COMPACT)))
	if ((status = ec_outfc(out, ' ')) < 0)
	    return(status);
    return 0;
}


#define STRING_PLUS	10
/*ARGSUSED*/
static int
_write_string(int idwrite, stream_id out, char *start, long int length, int depth)
{
/* It is not obvious what is the best way to avoid long strings
    if (UseDepth(idwrite) && depth > 0 &&
	    length > PrintDepth - depth + STRING_PLUS) {
	length = PrintDepth - depth + STRING_PLUS;
	Write_Str(out, start, (int) length);
	return (ec_outf(out, "...", 3));
    } else
 */
	return ec_outf(out, start, (int) length);
}

/* module argument is meaningful only when ARGOP is set in flag &&
   QUOTED is set in idwrite						*/
static int
_write_atom(int idwrite, stream_id out, dident d, int what, int flag, dident module, type mod_tag, syntax_desc *sd, int depth)
{
    int	    status;
    long    length = DidLength(d);
    char    *name = DidName(d);

    if (DidArity(d) < 0)
    {
	return ec_outfs(out, DidArity(d) == UNUSED_DID_ARITY ?
			    "ILLEGAL_FREED_FUNCTOR" : "ILLEGAL_FUNCTOR");
    }

    if (idwrite & QUOTED)
    {
	dident  d0 = check_did(d, 0);
	int nq = ec_need_quotes(d, sd);

	if (nq == QIDENTIFIER ||
	    nq == COMMA && (what != OPERATOR) ||
	    nq == BAR && (flag & ARGLIST) ||
	    nq == EOCL && (what == OPERATOR || (flag & ARGOP)))
	{
		if ((flag & ARGOP)
		    && is_visible_op(d0, module, mod_tag))
		{
			if ( ((status = ec_outfc(out, '(')) < 0)
			   || ((status = _write_quoted(idwrite, out, name, length,
					(char) sd->current_aq_char, sd, depth)) < 0)
			   || ((status = ec_outfc(out, ')')) <0 ))
			{
				return (status);
			}
			else
			{
				return(PSUCCEED);
			}
		}
		else
		{
		    Set_Bip_Error(0); /* access checking allready done	*/
		    return _write_quoted(idwrite, out, name, length,
					(char) sd->current_aq_char, sd, depth);
		}
	}

	if ((flag & ARGOP)
	    && is_visible_op(d0, module, mod_tag))
	{
		if (((status = ec_outfc(out, '(')) <0)
		    || ((status = ec_outf(out, name, (int) length)) < 0)
		    || ((status = ec_outfc(out, ')')) < 0))
		{
			return(status);
		}
		else
		{
			return(PSUCCEED);
		}
	}
	else
	{
	    Set_Bip_Error(0); /* access checking allready done		*/
	    return(ec_outf(out, name, (int) length));
	}
   }
   else
   {
	if (!strcmp(name, "|") && (flag &ARGLIST))
	{
	    return _write_quoted(idwrite, out, name, length,
			(char)sd->current_aq_char, sd, depth);
	}
	else
	{
	    return _write_string(idwrite, out, name, length, depth);
	}
   }

}


/*
 *	write a quoted atom or string
 *
 *	If an escape character (usually backslash) is defined,
 *	non printable characters are printed as <escape> <letter>
 *	or (if no special notation exists) as <escape> <octal>.
 *	Moreover, the escape character itself and the current quote
 *	are escaped.
 *	If no escape character is defined, only the current quote is
 *	treated in a special way (doubled) to achieve Cprolog compatibility.
 */
/*ARGSUSED*/
static int
_write_quoted(int idwrite, stream_id out, char *name, register long int len, char quotechar, syntax_desc *sd, int depth)
{
    int			status;
    int			cut;
    register char	c;

/* It is not obvious what is the best way to avoid long strings
    if (UseDepth(idwrite) && depth > 0 && len > PrintDepth - depth + STRING_PLUS) {
	len = PrintDepth - depth + STRING_PLUS;
	cut = 1;
    } else
*/
	cut = 0;
    if ((status = ec_outfc(out, quotechar)))	/* write the left quote		*/
	return status;

    if (sd->current_escape >= 0)	/* there is an escape character */
    {
	while (len-- > 0)
	{
	    switch(c = *name++)
	    {
	    case '\b':
		c = 'b'; break;
	    case '\t':
		if (idwrite & DONT_QUOTE_NL)
		{
		    if ((status = ec_outfc(out, c)))
			return status;
		    continue;
		}
		c = 't'; break;
	    case '\n':
		if (idwrite & DONT_QUOTE_NL)
		{
		    if ((status = ec_outfc(out, c)))
			return status;
		    continue;
		}
		c = 'n';
		break;
	    case '\r':
		c = 'r'; break;
	    case '\f':
		c = 'f'; break;
	    default:
		if (c == (char) sd->current_escape  ||  c == quotechar)
		    break;
		else if(c < 32  ||  c >= 127)	/* write escaped octal	*/
		{
		    if ((status = ec_outfc(out, sd->current_escape)))
			return status;
		    if ((status = p_fprintf(out, "%03o", c & 0xff)))
			return status;
		}
		else			/* normal printable character	*/
		    if ((status = ec_outfc(out, c)))
			return status;
		continue;
	    }
	    				/* write escaped char	*/
	    if ((status = ec_outfc(out, sd->current_escape)))
		return status;
	    if ((status = ec_outfc(out, c)))
		return status;
	}
    }
    else				/* we have no escape character */
    {
	while (len-- > 0)
	{
	    c = *name++;
	    if (c == quotechar)		/* double an internal quote	*/
		if ((status = ec_outfc(out, c)))
		     return status;
	    if ((status = ec_outfc(out, c)))
		return status;
	}
    }
    if (cut) {
	Write_Str(out, "...", 3);
    }

    return ec_outfc(out, quotechar);	/* write the right quote	*/
}

/*
 * Print the variable.
 * The number is the distance in pwords from the stack origin.
 * The stack is pword-aligned.
 */
static int
_print_var(int idwrite, value v, type t, stream_id str, int depth, dident module, type mod_tag, syntax_desc *sd)
{
    int name_printed = 0;
    int slot;

    if (idwrite & VARTERM)
	(void) ec_outf(str, "'_'(\"", 5);

    if (idwrite & VAR_ANON)
    {
	(void) ec_outfc(str, (char) sd->current_ul_char);
    }
    else if (GlobalFlags & STRIP_VARIABLES) /* in the tests, all vars are the same */
    {
	if (IsMeta(t))
	    (void) ec_outf(str, "_m", 2);
	else
	    (void) ec_outf(str, "_g", 2);
	return PSUCCEED;
    }
    else
    {
	if (!(idwrite & VAR_NUMBERS))
	{
	    switch (TagType(t))
	    {
	    case TMETA:
		if ((slot = meta_index(d_var_name)))
		{
		    pword *t1, *t2;

		    t1 = (v.ptr + 1)->val.ptr + slot;
		    Dereference_(t1);
		    if (IsStructure(t1->tag))
		    {
			t1 = t1->val.ptr;
			if ((t1++)->val.did == d_vname2)
			{/* vname(basename, number) as in var_name.ecl */
			    t2 = t1 + 1;
			    Dereference_(t1);
			    Dereference_(t2);
			    if (IsString(t1->tag) && IsInteger(t2->tag)) 
			    {
				p_fprintf(str, "%s#%ld", StringStart(t1->val), t2->val.nint);
				name_printed = 2; 
			    }
			}
		    }
		}

	    case TNAME:		/* all the named variable types */
	    case TUNIV:
		if (IsNamed(t.kernel) && (name_printed != 2))
		{
		    p_fprintf(str, "%s", DidName(TagDid(t.kernel)));
		    name_printed = 1;
		}
	    }
	}

	if ((idwrite & (VAR_NUMBERS|VAR_NAMENUM) && name_printed != 2)
		|| !name_printed)
	{
	    (void) ec_outfc(str, (char) sd->current_ul_char);
	    switch (TagType(t))
	    {
	    case TVAR_TAG:
		if (B_ORIG < v.ptr && v.ptr <= SP_ORIG) /* local */
		    p_fprintf(str, "l%ld", SP_ORIG - v.ptr);
		else
	    case TNAME:
		if (TG_ORIG <= v.ptr && v.ptr < B_ORIG)	/* global */
		    p_fprintf(str, "%ld", v.ptr - TG_ORIG);
		else			/* heap */
		    p_fprintf(str, "h%ld", v.ptr - B_ORIG);
		break;

	    case TUNIV:
		p_fprintf(str, "%ld", v.ptr - TG_ORIG);
		break;

	    case TMETA:
		p_fprintf(str, "%ld", v.ptr - TG_ORIG);
		break;

	    default:
		p_fprintf(str, "BAD_VAR_0x%lx_0x%lx", v.all, t.all);
		break;
	    }
	}
    }

    /* if it's a non marked metavariable write the metaterm */
    if (IsMeta(t) && (idwrite & (STD_ATTR | ATTRIBUTE)) && !(t.kernel & HIDE_ATTR))
    {
	/* important to mark before printing meta term or
	 * could not write circular metaterms.
	 * mark by changing type to normal variable so that other occurrences
	 * will be printed normally
	 */
	Trail_Tag(v.ptr);

	if (idwrite & STD_ATTR) {
	    pword *pw, *r;
	    pword pw_out;
            (v.ptr)->tag.kernel  |= HIDE_ATTR;
	    (void) ec_outfc(str,'{');
	    pw = MetaTerm(v.ptr);
	    Dereference_(pw);
	    r = TG;
	    TG += ATTR_IO_TERM_SIZE;
	    Check_Gc;
	    TG = transf_meta_out(pw->val, pw->tag, r,
	    	(idwrite & CANONICAL ? D_UNKNOWN : module), &pw_out);
	    (void) _pwrite1(idwrite, str, pw_out.val, pw_out.tag, 1200, depth,
						module, mod_tag, sd, ARGLAST);
	    (void) ec_outfc(str,'}');
	} else {
	    pword *r = _write_trafo(D_UNKNOWN /*META*/, 0,
				&idwrite, v, t, module, mod_tag);
	    (v.ptr)->tag.kernel  |= HIDE_ATTR;
	    if (r) {
		(void) _pwrite1(idwrite, str, r->val, r->tag, 1200, depth,
			    module, mod_tag, sd, ARGLAST);
	    }
	}
    }

    if (idwrite & VARTERM)
	(void) ec_outf(str, "\")", 2);

    return PSUCCEED;
}


/*
 * Convert a float to a Prolog-readable representation.
 * The caller has to provide a large enough buffer.
 * The length of the printed representation is returned.
 * If the precise-flag is set, we make sure that reading back the
 * number will give exactly the same float as before.
 */

static int
_float_to_string(value v, type t, char *buf, int precise)
{
    char aux[32];
    char *s;
    char *bufp = buf;
    int dot_seen = 0;
    double f = Dbl(v);

    if (!GoodFloat(f))
    {
	s = "1.0NaN";		/* should not occur normally */
    }
    else if (!finite(f))
    {
	s = f < 0 ? "-1.0Inf" : "1.0Inf";
    }
    else if (f == 0.0)		/* not all sprintf's deal properly with -0.0 */
    {
	s = (1.0/f < 0.0 /* && precise */) ? "-0.0" : "0.0";
    }
    else
    {
	if (IsDouble(t))
	{
	    (void) sprintf(aux, "%.15g", f);  /* try with precise digits only */
	    if (precise && f != atof(aux))
		(void) sprintf(aux, "%.17g", f);/* not exact enough, use more */
	}
	else
	{
	    (void) sprintf(aux, "%.6g", f);   /* try with precise digits only */
	    if (precise && (float) f != (float) atof(aux))
		(void) sprintf(aux, "%.9g", f); /* not exact enough, use more */
	}
	s = aux;
	if (*s == '-')
	    *bufp++ = *s++;		/* copy sign */
	if (*s == '.')
	    *bufp = '0';		/* insert 0 in front of . */
	for (;;)
	{
	    switch (*s)
	    {
	    case 'e':
	    case 'E':
		dot_seen = 1;
		*bufp++ = *s++;
		if (*s == '+' || *s == '-')	/* copy sign if any */
		    *bufp++ = *s++;
		while (*s == '0')	/* remove leading zeros in exponent */
		    ++s;
		if (! *s)		/* but don't lose them all */
		    *bufp++ = '0';
		continue;
	    case '.':
		dot_seen = 1;
		break;
	    case 0:
		if (!dot_seen)
		{
		    *bufp++ = '.';	/* insert .0 */
		    *bufp++ = '0';
		}
		*bufp = 0;
		return bufp - buf;
	    }
	    *bufp++ = *s++;
	}
	/* NOTREACHED */
    }
    while ((*bufp++ = *s++))
	;				/* copy the rest */
    return (bufp - buf) - 1;
}

/*ARGSUSED*/
static int
_num_string_size(value v, type t, int quoted)
{
    /* enough space for an integer in base 2 + sign */
    return 8*SIZEOF_LONG + 1;
}

/*ARGSUSED*/
static int
_int_to_string(value v, type t, char *buf, int quoted_or_base)
{
    int base = quoted_or_base < 2 ? 10 : quoted_or_base;
    word number = v.nint;
    word aux = number;
    int	len, pos = 0;

    if (number < 0)
    {
	buf[pos++] = '-';
	number = -number;	/* may overflow, see below */
    }

    do	/* count digits */
    {
	++pos;
	aux /= base;
    } while(aux);
    len = pos;

    buf[pos--] = '\0';
    if (number < 0)		/* special case -2^(wordsize-1) */
    {
	int ch = (number-base) % base;
	buf[pos--] = (ch < 10) ? ch + '0' : ch + 'a' - 10;
	number = -(number/base);
    }
    do
    {
	int ch = number % base;
	buf[pos--] = (ch < 10) ? ch + '0' : ch + 'a' - 10;
	number /= base;
    } while(number);

    return len;
}


static int
_handle_string_size(value v, type t, int quoted_or_base)
{
    if (ExternalClass(v.ptr)->string_size && ExternalData(v.ptr))
	return (ExternalClass(v.ptr)->string_size)(ExternalData(v.ptr), quoted_or_base);
    else
	return 0;
}

static int
_handle_to_string(value v, type t, char *buf, int quoted_or_base)
{
    if (ExternalClass(v.ptr)->to_string && ExternalData(v.ptr))
	return (ExternalClass(v.ptr)->to_string)(ExternalData(v.ptr), buf, quoted_or_base);
    else
	return 0;
}


/*
 *
 * printf_(+Stream, +Format, +List, +Module, 0'%, -ErrFormat, -ErrList, -Res)
 *
 * ErrFormat and ErrList return the remaining data
 * when there was an error (Res != 0)
 */

/*
 * CAUTION: p_printf5() uses a special error return mechanism in order to
 * deal better with errors that occur halfway through the format string.
 * It always succeeds and returns:
 *	the return/error code in verr/terr
 *	the remaining format string in vse/tse
 *	the remaining argument list in vle/tle
 * Bip_Error() is therefore temporarily redefined during p_printf5()
 * and changed back later!!!
 */

#undef Bip_Error
#define Bip_Error(N) Printf_Error(N)
#define Printf_Error(N) { res = N; goto _return_res_; }

static int
p_printf5(value vs, type ts, value strval, type strtag, value lval, type ltag, value vm, type tm, value vfc, type tfc, value vse, type tse, value vle, type tle, value verr, type terr)
{
    char 	formstrt = vfc.nint;
    char 	*format, *cpar, *npar, par[32];
    int 	success_code = PSUCCEED;
    int 	res;
    stream_id 	nst = get_stream_id(vs, ts, SWRITE, &res);
    long 	asterisk, c, i;
    int		radix;
    pword	my_list[2];
    pword	*list;
    pword	*elem;
    char	*last_format = NULL;
    pword	*last_list;

    Get_Name(strval, strtag, format);
    if (nst == NO_STREAM) {
	Bip_Error(res)
    }
    Check_Stream(nst, res);
    Check_Module(tm, vm);
    Check_Integer(tfc);

    if ((StreamMode(nst) & STYPE) == SNULL)
	goto _return_succ_;

    if (IsNil(ltag))
	list = 0;
    else if (!IsList(ltag))
    {
	my_list[0].tag = ltag;
	my_list[0].val = lval;
	my_list[1].tag.kernel = TNIL;
	list = &my_list[0];
    }
    else
	list = lval.ptr;

    par[0] = '%';	/* here we build up the format string for C printf */
    cpar = &par[0];

    last_list = list;
    last_format = format;

    Lock_Stream(nst);	/* Be sure to unlock before returning !!! */

    for (; *format; last_format = ++format, last_list = list)
    {
	if (*format == formstrt)
	{                           /* within control sequence */
	    asterisk = 0;
	    while ((*(++cpar) = *(++format)))
	    {
		if (*cpar == formstrt)
		{
		    if (cpar != &par[1]) {
			/* something between two %'s */
			Printf_Error(BAD_FORMAT_STRING);
		    } else if ((res = ec_outfc(nst, formstrt)) < 0) {
			goto _return_res_;
                    }
		} else
		switch (*cpar)
		{
/* 
 * free : hjyz BFHJLSYZ
 */
		case ' ' :       /* flags and sizes */
		case '+' :
		case '-' :
		case '.' :
		case '#' :
		case '0' :
		case '1' :
		case '2' :
		case '3' :
		case '4' :
		case '5' :
		case '6' :
		case '7' :
		case '8' :
		case '9' :
		case 'l' :
		case 'm' :
		case 'v' :
		case 'C' :
		case 'D' :
		case 'G' :
		case 'I' :
		case 'K' :
		case 'M' :
		case 'N' :
		case 'O' :
		case 'P' :
		case 'Q' :
		case 'T' :
		case 'U' :
		case 'V' :
		case '_' :
		     continue;

		case '*' :
		    if (++asterisk > 2) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    continue;

		case 'd' :        /* integers  */
		case 'o' :
		case 'u' :
		case 'x' :
		case 'X' :
		    *(++cpar) = '\0';
		    res = _printf_asterisk(asterisk, &list, tint, nst, par);
		    if (res < 0) {
		        goto _return_res_;
		    }
		    break;

		case 'f' :        /*  floating numbers  */
		case 'e' :
		case 'E' :
		case 'g' :
		    *(++cpar) = '\0';
		    res = _printf_asterisk(asterisk, &list, tag_desc[TDBL].tag, nst, par);
		    if (res < 0) {
		        goto _return_res_;
		    }
		    break;

		case 'n' :		/* newline */
		case 't' :		/* tab */
		case 'c' :		/*  single char  */
		    if (asterisk > 1) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    else if (asterisk) {
			Next_Element(elem, list, Printf_Error)
			Check_Integer(elem->tag)
			i = elem->val.nint;	/* character count */
		    }
		    else {
			Get_Counter(par+1,npar,i);
			if (i==0) i=1;
		    }
		    switch (*cpar)
		    {
		    case 'c':
			Next_Element(elem, list, Printf_Error)
			Check_Integer(elem->tag)
			c = elem->val.nint;
			break;
		    case 'n':
			while (i)
			{
			    if ((res = ec_newline(nst)) < 0) {
				if (res == YIELD_ON_FLUSH_REQ)
				    success_code = res;
				else
				    goto _return_res_;
			    }
			    --i;
			}
			break;
		    case 't':
			c = '\t';
			break;
		    }
		    while(i--)
		    {
			if ((res = ec_outfc(nst, (char) c) < 0))
			    goto _return_res_;
		    }
		    break;

		case 's' : 	  /*  string	   */
		    if (cpar != &par[1])
		    {
			/* we don't have a simple %s, pass to C's printf ... */
			*(++cpar) = '\0';
			res = _printf_asterisk(asterisk, &list, tstrg, nst, par);
			if (res < 0) {
			    goto _return_res_;
			}
			break;
		    }
		    /* else fall through and treat %s like %a
		     * (because we cope better with long strings)
		     */

		case 'a' :	/* 'write' atom or string (may contain NUL) */
		case 'A' :	/* same but map to upper case */
		    Next_Element(elem, list, Printf_Error)
		    if (cpar != &par[1])
		    {
			 Printf_Error(BAD_FORMAT_STRING)
		    }
		    if (IsString(elem->tag)) {
			i = (int) StringLength(elem->val);
			npar = StringStart(elem->val);
		    } else if (IsAtom(elem->tag)) {
			i = (int) DidLength(elem->val.did);
			npar = DidName(elem->val.did);
		    } else if (IsNil(elem->tag)) {
			i = (int) DidLength(d_.nil);
			npar = DidName(d_.nil);
		    } else if (IsRef(elem->tag)) {
			Printf_Error(INSTANTIATION_FAULT);
		    } else {
			Printf_Error(TYPE_ERROR);
		    }
		    if (*cpar == 'A') {
			for (res=0; res==0 && i--; ++npar)
			    res = ec_outfc(nst, toupper(*npar));
		    } else {
			res = ec_outf(nst, npar, i);
		    }
		    if (res < 0) {
		       goto _return_res_;
                    }
		    break;

                case 'w' :        /* 'write' term (ignore stream defaults) */
                case 'W' :        /* 'write' term (use stream defaults) */
		{
		    char form_char = *cpar;
		    int mask_clr, mask_set;
		    if (asterisk > 1) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    else if (asterisk) {
			Next_Element(elem, list, Printf_Error)
			Check_Integer(elem->tag)
			i = elem->val.nint;	/* character count */
			npar = par+2;
		    }
		    else {
			Get_Counter(par+1,npar,i);
		    }
		    Next_Element(elem, list, Printf_Error)

		    *(cpar) = '\0';
		    res = _get_mode_mask(npar, &mask_clr, &mask_set);
		    if (res != PSUCCEED) {
			goto _return_res_;
		    }
		    if (form_char == 'w')
		    	mask_clr = StreamOutputMode(nst);

		    res = ec_pwrite(mask_clr, mask_set, nst, elem->val, elem->tag,
			1200, i, vm.did, tm);
		    if (res < 0) {
		       goto _return_res_;
                    }
		    break;
		}

                case 'p' :              /* 'print' term  */
		    if (cpar != &par[1])
		    {
			 Printf_Error(BAD_FORMAT_STRING)
                    }
		    Next_Element(elem, list, Printf_Error)
		    res = ec_pwrite(0, PRINT_CALL, nst, elem->val, elem->tag,
			1200, 0, vm.did, tm);
		    if (res < 0) {
			goto _return_res_;
                    }
		    break;

                case 'q' :              /* 'writeq' term  */
		    if (cpar != &par[1])
		    {
			 Printf_Error(BAD_FORMAT_STRING)
                    }
		    Next_Element(elem, list, Printf_Error)
		    res = ec_pwrite(0, QUOTED|FULLDEPTH|VAR_NUMBERS|STD_ATTR|
			NO_MACROS, nst, elem->val, elem->tag,
			1200, 0, vm.did, tm);
		    if (res < 0) {
			goto _return_res_;
                    }
		    break;

                case 'k' :              /* 'display' term  */
		    if (cpar != &par[1])
		    {
			 Printf_Error(BAD_FORMAT_STRING)
                    }
		    Next_Element(elem, list, Printf_Error)
		    res = ec_pwrite(0, CANONICAL|DOTLIST, nst, elem->val, elem->tag,
			1200, 0, vm.did, tm);
		    if (res < 0) {
		       goto _return_res_;
                    }
		    break;

                case 'i' :              /* skip term */
		    if (asterisk > 1) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    else if (asterisk)
		    {
			Next_Element(elem, list, Printf_Error)
			Check_Integer(elem->tag)
			i = elem->val.nint;
		    }
		    else
		    {
			Get_Counter(par+1,npar,i);
			if (i==0) i=1;
		    }
		    while (i--) {
			Next_Element(elem, list, Printf_Error)
		    }
		    break;

		case 'b':		/* flush buffer */
		    if (cpar != &par[1])
		    {
			 Printf_Error(BAD_FORMAT_STRING)
                    }
		    if ((res = ec_flush(nst)) < 0) {
			if (res == YIELD_ON_FLUSH_REQ)
			    success_code = res;
			else
			    goto _return_res_;
		    }
		    break;

		case 'R':
		case 'r':		/* radix printing */
		    if (asterisk > 1) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    else if (asterisk)
		    {
			Next_Element(elem, list, Printf_Error)
			Check_Integer(elem->tag)
			radix = elem->val.nint;
		    }
		    else if (cpar == par + 1)
			radix = 8;
		    else
		    {
			Get_Counter(par+1,npar,radix);
		    }
		    if (radix < 2 || radix > 'z' - 'a' + 11) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    Next_Element(elem, list, Printf_Error)
		    if (IsRef(elem->tag)) {
			Printf_Error(INSTANTIATION_FAULT)
		    } else if (IsInteger(elem->tag) || IsBignum(elem->tag)) {
			int bufsize = 1 + tag_desc[TagType(elem->tag)].string_size(elem->val, elem->tag, radix);
			char *buf = (char *) hp_alloc_size(bufsize);
			int len = tag_desc[TagType(elem->tag)].to_string(elem->val, elem->tag, buf, radix);
			if (*cpar == 'R') {
			    for (res=0,i=0; res==0 && i<len; ++i)
				res = ec_outfc(nst, toupper(buf[i]));
			} else {
			    res = ec_outf(nst, buf, len);
			}
			hp_free_size((generic_ptr) buf, bufsize);
			if (res < 0) {
			   goto _return_res_;
			}
		    } else {
			Printf_Error(TYPE_ERROR)
		    }
		    break;

		default:
		    Printf_Error(BAD_FORMAT_STRING);
		    break;
                } 
		cpar = &par[0];
		break;
            }
        }
        else
        {
	    if ((res = ec_outfc(nst, (char) *format)) < 0)
	    {
	        goto _return_res_;
	    }
	}
    }
    if (cpar != &par[0]) {
	/* % without a control character */
	Printf_Error(BAD_FORMAT_STRING)
    }
    if (list) {
	Printf_Error(BAD_ARGUMENT_LIST)
    }
    Unlock_Stream(nst);
_return_succ_:
    Return_Unify_Integer(verr, terr, success_code)

_return_res_:
    {
	value	fv;
	Prepare_Requests;

	if (last_format)
	{
	    /* stream was already locked, unlock it */
	    Unlock_Stream(nst);

	    /* compute the "remaining" format string and list */
	    Cstring_To_Prolog(last_format, fv);
	    Request_Unify_String(vse, tse, fv.ptr);
	    if (last_list == 0) {
		Request_Unify_Nil(vle, tle);
	    } else if (last_list == &my_list[0]) {
		Request_Unify_Pw(vle, tle, my_list[0].val, my_list[0].tag);
	    } else {
		Request_Unify_List(vle, tle, last_list);
	    }
	}
	else
	{
	    Request_Unify_Pw(vse, tse, strval, strtag);
	    Request_Unify_Pw(vle, tle, lval, ltag);
	}
	Request_Unify_Integer(verr, terr, -res)
	Return_Unify;
    }
}

/* define Bip_Error() back to Bip_Error_Fail() */
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

static int
_printf_asterisk(long int asterisk, pword **list, type arg_type, stream_id nst, char *par)
{
    pword	*elem;
    pword	*elem2;
    pword	*elem3;

    if (asterisk == 0)
    {
	Next_Element(elem, (*list), return)
	if (!(SameType(elem->tag, arg_type) ||
	    SameType(arg_type, tstrg) && (IsAtom(elem->tag)||IsNil(elem->tag))
	))
	    return(TYPE_ERROR);
	switch (TagType(elem->tag))
	{
	case TSTRG:
	    return p_fprintf(nst, par, StringStart(elem->val));
	case TDICT:
	    return p_fprintf(nst, par, DidName(elem->val.did));
	case TNIL:
	    return p_fprintf(nst, par, "[]");
	case TDBL:
	    return p_fprintf(nst, par, Dbl(elem->val));
	case TINT:
	    return p_fprintf(nst, par, elem->val.nint);
	}
    }
    else if (asterisk == 1)
    {
	Next_Element(elem, (*list), return)
	if (IsRef(elem->tag))
	    return INSTANTIATION_FAULT;
	else if (!IsInteger(elem->tag))
	    return TYPE_ERROR;
	Next_Element(elem2, (*list), return)
	if (!(SameType(elem2->tag, arg_type) ||
	    SameType(arg_type, tstrg) && (IsAtom(elem2->tag)||IsNil(elem2->tag))
	))
	    return(TYPE_ERROR);
	switch (TagType(elem2->tag))
	{
	case TSTRG:
	    return p_fprintf(nst, par, elem->val.nint, StringStart(elem2->val));
	case TDICT:
	    return p_fprintf(nst, par, elem->val.nint, DidName(elem2->val.did));
	case TNIL:
	    return p_fprintf(nst, par, elem->val.nint, "[]");
	case TDBL:
	    return p_fprintf(nst, par, elem->val.nint, Dbl(elem2->val));
	case TINT:
	    return p_fprintf(nst, par, elem->val.nint, elem2->val.nint);
	}
    }
    else if (asterisk == 2)
    {
	Next_Element(elem, (*list), return)
	if (IsRef(elem->tag))
	    return INSTANTIATION_FAULT;
	else if (!IsInteger(elem->tag))
	    return TYPE_ERROR;
	Next_Element(elem2, (*list), return)
	if (IsRef(elem2->tag))
	    return INSTANTIATION_FAULT;
	else if (!IsInteger(elem2->tag))
	    return TYPE_ERROR;
	Next_Element(elem3, (*list), return)
	if (!(SameType(elem3->tag, arg_type) ||
	    SameType(arg_type, tstrg) && (IsAtom(elem3->tag)||IsNil(elem3->tag))
	))
	    return(TYPE_ERROR);
	switch (TagType(elem3->tag))
	{
	case TSTRG:
	    return p_fprintf(nst, par,
		elem->val.nint, elem2->val.nint, StringStart(elem3->val));
	case TDICT:
	    return p_fprintf(nst, par,
		elem->val.nint, elem2->val.nint, DidName(elem3->val.did));
	case TNIL:
	    return p_fprintf(nst, par,
		elem->val.nint, elem2->val.nint, "[]");
	case TDBL:
	    return p_fprintf(nst, par,
		elem->val.nint, elem2->val.nint, Dbl(elem3->val));
	case TINT:
	    return p_fprintf(nst, par,
		elem->val.nint, elem2->val.nint, elem3->val.nint);
	}
    }

    return(BAD_FORMAT_STRING);
}


/*
 * get/set output_mode_mask (as integer)
 */
static int
p_output_mode_mask(value v, type t)
{
    if (IsRef(t)) {
	Return_Unify_Integer(v, t, output_mode_mask);
    } else {
	Check_Integer(t);
	if (v.nint & WRITE_GOAL) {	/* must not be set */
	    Bip_Error(RANGE_ERROR)
	}
	output_mode_mask = v.nint;
	Succeed_;
    }
}

/*
 * get/set output_mode_mask (as string)
 */
static int
p_output_mode(value val, type tag)
{
    if (IsRef(tag))
    {
	value	sv;
	char	s[OUTPUT_MODES+1];

	_output_mode_string(s, output_mode_mask);
	Cstring_To_Prolog(s, sv);
	Return_Unify_String(val, tag, sv.ptr);
    }
    else
    {
	char	*new_output_mode;
	int	mask, mask_clr;
	int	res;

	Get_Name(val, tag, new_output_mode);
	if ((res = _get_mode_mask(new_output_mode, &mask_clr, &mask)) != PSUCCEED) {
	    Bip_Error(res)
	}
	if (mask & WRITE_GOAL) {	/* must not be set */
	    Bip_Error(RANGE_ERROR)
	}
	output_mode_mask = mask;
	Succeed_;
    }
}

static void
_output_mode_string(char *s, int mask)
{
    int		i = 0, j;

    for (j=0; j<OUTPUT_MODES; j++)
    {
	if (mask & 1<<j)
	    s[i++] = output_mode_chars[j];
    }
    s[i] = '\0';
}


/*
 * _get_mode_mask() to decode a printf %w format string:
 *
 *	characters must be those in output_mode_chars[]
 *	options can be negated by prefixing a - sign
 *	returns one bit mask with bits to clear, and one with bits to set
 * 
 */

#define MoreThanOneBitSet(n) ((n) & ((n)-1))	/* cute 2's complement trick */

static int
_get_mode_mask(char *string, int *clr_mask, int *mask)
{
    char	c;
    char	*p;
    int		negative = 0;
    int		bit;

    *mask = *clr_mask = 0;
    for (; (c = *string); ++string)
    {
	if (c == '-')
	{
	    negative = 1;
	    continue;
	}
	if ((p = strchr(output_mode_chars, c)))
	    bit = 1 << (p - output_mode_chars);
	else
	    return(RANGE_ERROR);
	if (negative)
	{
	    negative = 0;
	    *clr_mask |= bit;
	}
	else
	{
	    *mask |= bit;
	}
    }

    /* Don't allow setting more than one of the mutually exclusive options */
    if (MoreThanOneBitSet(*mask & (VAR_NUMBERS|VAR_ANON|VAR_NAMENUM))
     || MoreThanOneBitSet(*mask & (STD_ATTR|ATTRIBUTE)))
    {
	return BAD_FORMAT_STRING;
    }
    return PSUCCEED;
}

static int
_merge_output_modes(int mask, int remove, int add)
{
    mask &= ~remove;
    /* if any of the one-of-several-bits options is added, clear bits first */
    if (add & (VAR_NUMBERS|VAR_ANON|VAR_NAMENUM))
    	mask &= ~(VAR_NUMBERS|VAR_ANON|VAR_NAMENUM);
    if (add & (STD_ATTR|ATTRIBUTE))
    	mask &= ~(STD_ATTR|ATTRIBUTE);
    return mask | add;
}


/* A Function to be used in the debugger */
void
writeq_term(uword val, uword tag)
{
    value	v;
    type	t;
    value	vm;

    v.all = val;
    t.kernel = tag;
    vm.did = d_.default_module;

    (void) p_writeq(v, t, vm, tdict);
    ec_flush(current_output_);
    (void) ec_newline(current_output_);
}


/*
 * write_term(+Stream, +Term, +ClrOptions, +SetOptions, ?Depth, +Module)
 *
 * Depth=0	use stream's/global default setting
 */
static int
p_write_term(value vs, type ts, value val, type tag, value vcm, type tcm, value vsm, type tsm, value vdepth, type tdepth, value vm, type tm)
{
    int		res;
    stream_id out = get_stream_id(vs, ts, SWRITE, &res);

    Check_Stream(out, res);
    Check_Integer(tcm);
    Check_Integer(tsm);
    Check_Integer(tdepth);
    Check_Module(tm, vm);
    Lock_Stream(out);
    res = ec_pwrite(vcm.nint, vsm.nint, out, val, tag, 1200, vdepth.nint, vm.did, tm);
    Unlock_Stream(out);
    return res;
}


/* CAUTION: Bip_Error() is redefined to Bip_Error_Fail() ! */

