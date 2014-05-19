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

/*----------------------------------------------------------------------
 * System:	ECLiPSe Constraint Logic Programming System
 * Version:	$Id: read.c,v 1.1 2008/06/30 17:43:58 jschimpf Exp $
 *
 * Content:	ECLiPSe parser
 * Author: 	Joachim Schimpf, IC-Parc
 *		Micha Meier, ECRC (some macro transformation code)
 *
 * History:
 *	This is a complete rewrite of the original Sepia parser (written
 *	by Dominique Henry de Villeneuve) and retains very little of the
 *	original code. The new code is structurally based on Richard O'Keefe's
 *	public domain Prolog parser (read.pl), however, it is completely
 *	deterministic and therefore has a few restrictions wrt the resolution
 *	of ambiguities.
 *
 * TODO:
 *	- reduce the overhead of operator lookups, and avoid multiple lookups
 *	- optionally parse {a,b} as {}(a,b) as in Mercury
 *	- parse (Term)(Args) as apply(Term, Args), similar to Mercury's
 *		(X^T)(A,B,C) as ''((X^T),A,B,C)
 *
 * Syntax extensions:
 *
 *	Parse:		as:				syntax_option:
 *
 *	X[Args]		subscript(X, [Args])		no_array_subscripts
 *	f(a)[Args]	subscript(X, [Args])		no_array_subscripts
 *	a[Args]		subscript(a, [Args])		atom_subscripts
 *	(...)[Args]	subscript(..., [Args])		general_subscripts
 *	[Xs][Args]	subscript([Xs], [Args])		general_subscripts
 *	Subscript[Args]	subscript(Subscript, [Args])	general_subscripts
 *
 *	X{Args}		X 'with attributes' [Args]	no_attributes
 *	a{Args}		a with [Args]			no_curly_arguments
 *	a{}		a with []			no_curly_arguments

 *	{Args}		{}(Args)			not curly_args_as_list
 *	{Args}		{}([Args])			curly_args_as_list
 *
 *	X(Args)		apply(X, [Args])		var_functor_is_apply
 *
 *	f(a){Args}	unused
 *	f(a)(Args)	unused
 *	123[Args]	unused
 *	123{Args}	unused
 *	123(Args)	unused
 *
 *	(the atom-bracket sequences are recognised only where the
 *	atom is not a prefix/infix).
 *
 * Call hierarchy of the parser:
 *
 *  ec_read_term
 *	_read_next_term			% top term or right arg of infix/prefix
 *	    _read_next_term	
 *	    _read_list			% reads a normal list in [] syntax
 *		_read_next_term
 *	    _read_struct		% reads structure in canonical syntax
 *		_read_next_term
 *	    _read_sequence_until	% reads bracketed comma-sequence
 *		_read_next_term
 *	    _read_after_term		% infix/postfix/subscript/delimiter
 *		_read_next_term	
 *		_read_after_term
 *		_read_list		% reads a [] subscript list
 *
 *
 *
 *
 * Annotated terms
 * ---------------
 * When invoked with the LAYOUT_PLEASE option, the parser returns an
 * annotated term instead of the plain parsed term. In an annotated
 * term, every subterm is wrapped into a annotated_term/4 structure:
 *
 *	:- export struct(annotated_term(
 *		term,			% var,atomic,compound
 *		type,			% term type (see below)
 *		from, to		% source position (integer)
 *	)).
 *
 * The type field describes the type of the parsed term and is one of
 * the following:
 *
 *	integer
 *	float
 *	rational
 *	breal
 *	atom
 *	string		term is a string or a char_code list
 *	anonymous	term is an anonymous variable
 *	var(NameAtom)	term is a variable with the given name
 *	compound	term is compound (with annotated subterms)
 *
 * In the case of atomic terms and variables, the term field simply
 * contains the plain parsed term. For compound terms, the term field
 * contains a structure whose functor is the functor of the plain term,
 * but whose arguments are annotated versions of the plain term arguments.
 * E.g. the source term
 * 	3
 *    is parsed as
 *	annotated_term(3, integer, ...)
 * 
 * The source term
 * 	foo(bar, X, _, 3)
 *    is parsed as
 *	annotated_term(foo(
 *		annotated_term(bar,
 *		    atom, ...),
 *		annotated_term(_,
 *		    anonymous, ...),
 *		annotated_term(3,
 *		    integer, ...)),
 *	    compound, ...)
 * The source term
 * 	[1,2]
 *    is parsed as
 *	annotated_term(.(
 *		annotated_term(1,
 *		    integer, ...),
 *		annotated_term(.(
 *			annotated_term(2,
 *			    integer, ...),
 *			annotated_term([],
 *			    atom, ...)),
 *		    compound, ...)),
 *	    compound, ...)
 *
 *
 * The from/to fields of an annotated term describe a "source position"
 * of the term. Every term/subterm is represented by one (sometimes two
 * consecutive) tokens in the source, defined as follows:
 *
 * - atoms, strings and unsigned numbers are represented by their
 *   corresponding IDENTIFIER, NUMBER or STRING token.
 * - signed numbers are represented by two consecutive tokens (sign+number)
 * - compound terms in canonical notation are represented by two consecutive
 *   tokens (functor and opening parenthesis)
 * - compound terms in operator syntax are represented by the operator's
 *   IDENTIFIER token
 * - lists: a proper list [a,b] has subterms
 * 	[a,b]	represented by the [ token,
 * 	[b]	represented by the , token,
 * 	[]	represented by the ] token,
 *	a	represented by itself,
 *	b	represented by itself.
 *   a general list [a,b|T] has subterms
 *   	[a,b|T]	represented by the [ token,
 *   	[b|T]	represented by the , token,
 *   	T	represented by itself,
 *   	a	represented by itself,
 *   	b	represented by itself.
 *   Note that the | and ] tokens do not represent any term.
 * - special notations:
 *   X[Args]
 *	subscript(X, [...]) represented by the [ token,
 *	X,Args	represented by itself,
 *   X{Args}
 *   	'with attributes'(X,[Args]) represented by { token,
 *		(alternatively: X{ tokens)
 *   	X,Args	represented by themselves
 *   a{Args}
 *	with(a,[Args])	represented by the { token
 *		(alternatively: a{ tokens)
 *   	a,Args	represented by themselves
 *   X(Args)
 *   	apply(X,[Args])	represented by the ( token
 *   	X,Args	represented by themselves
 *   In all these cases, the commas represent nothing.
 *
 * The source position of a term is the union of the source positions of
 * the representing tokens.
 *----------------------------------------------------------------------*/

#ifdef HAVE_STRING_H
#include <string.h>
#endif


#include	"config.h"
#include	"sepia.h"
#include	"types.h"
#include	"embed.h"
#include	"error.h"
#include	"mem.h"
#include	"dict.h"
#include	"lex.h"
#include	"emu_export.h"
#include 	"io.h"
#include 	"read.h"
#include	"module.h"
#include	"property.h"



/*
 * EXTERNALS
 */

extern pword	*p_meta_arity_;


/*
 *	TYPES
 */

typedef struct s_varword		/* variable stack */
{ 
    char  		*str;
    pword		*ptr;
    int			 lock;
    struct s_varword	*next;
}	vword;


typedef struct parse_desc {

    /* in: */
	stream_id	nst;		/* stream we are reading from	*/
	syntax_desc	*sd;		/* module syntax descriptor	*/
	dident		module;		/* caller module (for op,macro)	*/
	type		module_tag;	/* caller module tag		*/
	int		options;	/* parser options		*/
	int		max_arg_prec;	/* maximium argument precedence	*/

    /* internal: */
	token_desc	token,		/* current token		*/
			prev_token,	/* previous token		*/
			next_token;	/* next token			*/

	vword		*var_table;	/* hash table for variable names */
	int		var_table_size;	/* the table's size		*/
	long		counter;	/* its generation counter	*/

	int		macro;		/* term may contain a macro	*/
	pword		*var_list_tail;	/* tail of varlist (readvar)	*/

	temp_area	string_store;	/* temp store for strings	*/
} parse_desc;


/*
 * STATIC VARIABLES
 */

static dident	d_comma0_;
static dident	d_bar0_;
static dident	d_annotated_term_;
static dident	d_anonymous_;


/*
 * FUNCTIONS
 */

static parse_desc
	*_alloc_parse_env(int caller, stream_id nst, dident module, type mod_tag);

static vword *
	_var_table_entry(parse_desc *pd, char *varname, word lenght);

int
	do_trafo(pword *),
	p_read3(value vs, type ts, value v, type t, value vm, type tm);

static int
	_pread3(value v, type t, stream_id nst, value vm, type tm),
	p_read2(value v, type t, value vm, type tm),
	p_read_annotated_raw(value vs, type ts, value v, type t, value vf, type tf, value vm, type tm),
	p_readvar(value vs, type ts, value v, type t, value vv, type tv, value vm, type tm);

static unsigned long
	hashfunction(char *id);

static vword
	*_alloc_vword(register parse_desc *pd);

static int
	_transf_attribute(register pword *pw, pword *r, int def),
	_read_next_term(parse_desc *pd, int context_prec, int context_flags,
		pword *result),
	_read_after_term(parse_desc *pd, int context_prec, int context_flags,
		int term_prec, pword *result);


/*
 *	CONSTANTS OF THE PARSER
*/

/* size of the variable hash table (should be a prime) */
#define NUMBER_VAR 1009L



/*
 * Values for context_flags
 * The *_TERMINATES flags mean that COMMA/BAR terminate a term
 * unconditionally, i.e. overriding the normal precedence rules
 * (this is used when a subterm is a list or structure argument).
 * The SUBSCRIPTABLE flag means the term may be followed by a subscript.
 */

#define COMMA_TERMINATES	1	/* list elements or structure fields */
#define BAR_TERMINATES		2	/* list elements only */
#define SUBSCRIPTABLE		4	/* term can be followed by subscript */
#define PREBINFIRST		8	/* first argument of prefix binary op */
#define FZINC_SUBSCRIPTABLE	16	/* subscripts after atoms */
#define ZINC_SUBSCRIPTABLE	32	/* subscripts after almost everything */


/*
 * Interface with the lexer
 *
 * The current token is always cached in the parse_desc.
 * Normally, we advance to the next token by calling Next_Token().
 * When lookahead is needed, we use Lookahead_Next_Token() instead,
 * and later go back by calling Prev_Token().
 */

#define	Next_Token(pd) \
	if (pd->next_token.class == NO_TOKEN) { \
	    (void) lex_an(pd->nst, pd->sd, &pd->token); \
	} else { \
	    pd->token = pd->next_token; \
	    pd->next_token.class = NO_TOKEN; \
	}

#define	Lookahead_Next_Token(pd) \
	pd->prev_token = pd->token; \
	if (pd->token.string == (char*) StreamLexAux(pd->nst)) { \
	    pd->prev_token.string = TempAlloc(pd->string_store, pd->token.term.val.nint + 1); \
	    Copy_Bytes(pd->prev_token.string, pd->token.string, pd->token.term.val.nint + 1); \
	} \
	Next_Token(pd)

#define	Prev_Token(pd) \
	pd->next_token = pd->token; \
	if (pd->token.string == (char*) StreamLexAux(pd->nst)) { \
	    pd->next_token.string = TempAlloc(pd->string_store, pd->token.term.val.nint + 1); \
	    Copy_Bytes(pd->next_token.string, pd->token.string, pd->token.term.val.nint + 1); \
	} \
	pd->token = pd->prev_token;

#define IsClass(pd,cl)		((pd)->token.class == (cl))
#define IsChar(pd,char)		((pd)->token.term.val.nint == (char))
#define IsToken(pd,cl,char)	(IsClass(pd,cl) && IsChar(pd,char))

#define TokenString(pd) \
	(pd->token.string)

#define TokenStringLen(pd) \
	(pd->token.term.val.nint)

#define Save_Token_String(pd, s, l) \
	l = TokenStringLen(pd); \
	if (TokenString(pd) == (char*) StreamLexAux(pd->nst)) { \
	    s = TempAlloc(pd->string_store, l + 1); \
	    Copy_Bytes(s, TokenString(pd), l + 1); \
	} else { \
	    s = TokenString(pd); \
	}


/*
 * Macros (read/clause/goal-macros)
 * While parsing the term, we check whether we come across any items that
 * _may_ have a macro transformation defined (no matter which), and set a flag.
 * If any, we do a macro-expansion pass over the term after it has been parsed.
 * This is done in Prolog (by calling expand_macros_/3).
 */

#define Flag_Type_Macro(pd, type) \
	{ if (DidMacro(TransfDid(type))) pd->macro = 1; }
	
#define Flag_Did_Macro(pd, wdid) \
	{ if (DidMacro(wdid)) pd->macro = 1; }
	

/*
 * Term construction:
 * The Build_XXX macros/functions construct the ECLiPSe terms, wrapped
 * into a term descriptor, if requested.
 */

#define	TERM_TERM	1
#define	TERM_TYPE	2
#define	TERM_FILE	3
#define	TERM_LINE	4
#define	TERM_FROM	5
#define	TERM_TO		6
#define	TERM_ARITY	6


static source_pos_t no_pos_ = {D_UNKNOWN,0,0,0};

#define Merge_Source_Pos(p1,p2,paux)			\
	paux.file = p1.file;				\
	paux.line = p1.line;				\
	paux.from = p1.from;				\
	paux.to = p2.to;


/*
 * Term construction
 */

/*
 * Construct the annotated_term/4 descriptive wrapper, if requested
 *	annotated_term(Term, <dtype>, <pos.from>, <pos.to>)
 */

#define	Make_Term_Wrapper(pw, _pw, dtype, pos)		\
	if (!(pd->options & LAYOUT_PLEASE)) {		\
	    _pw = (pw);					\
	} else {					\
	    _pw = TG;					\
	    Make_Struct(pw, TG);			\
	    Push_Struct_Frame(d_annotated_term_);	\
	    Make_Atom(_pw+TERM_TYPE, dtype);		\
	    Make_Atom(_pw+TERM_FILE, (pos).file);	\
	    Make_Integer(_pw+TERM_LINE, (pos).line);	\
	    Make_Integer(_pw+TERM_FROM, (pos).from);	\
	    Make_Integer(_pw+TERM_TO, (pos).to);	\
	    _pw += TERM_TERM;				\
	}

/*
 * Construct a annotated_term/4 descriptive wrapper for a variable, if requested
 *	annotated_term(X, var('X'), <pos.from>, <pos.to>)
 */
#define	Make_Var_Wrapper(pw, pvar, pos)			\
	if (!(pd->options & LAYOUT_PLEASE)) {		\
	    pvar = (pw);				\
	} else {					\
	    pword *_pw = TG;				\
	    Make_Struct(pw, TG);			\
	    Push_Struct_Frame(d_annotated_term_);	\
	    pvar = _pw+TERM_TERM;			\
	    Make_Struct(_pw+TERM_TYPE, TG);		\
	    Make_Atom(_pw+TERM_FILE, (pos).file);	\
	    Make_Integer(_pw+TERM_LINE, (pos).line);	\
	    Make_Integer(_pw+TERM_FROM, (pos).from);	\
	    Make_Integer(_pw+TERM_TO, (pos).to);	\
	    _pw = TG;					\
	    Push_Struct_Frame(d_.var);			\
	    Make_Atom(_pw+1, enter_dict_n(TokenString(pd), TokenStringLen(pd), 0));\
	}

/*
 * This macro copies the pwords at 'from' to 'to', except that in the case of
 * self-reference it creates a new self-reference at 'to'. This assumes that
 * there are no references to the location 'from' that could become dangling!
 */
#define Move_Pword(from, to)					\
	(to)->tag.all = (from)->tag.all;			\
	if (IsRef((from)->tag) && (from)->val.ptr == (from)) {	\
	    (to)->val.ptr = (to);				\
	} else {						\
	    (to)->val.all = (from)->val.all;			\
	}

#define Build_List(pw,phead,pos) {			\
	pword *_pw;					\
	Make_Term_Wrapper(pw, _pw, d_.compound0, pos);	\
	phead = TG;					\
	Make_List(_pw, TG);				\
	Push_List_Frame();				\
    }

#define Build_Nil(pw,pos) {			\
	pword *_pw;					\
	Make_Term_Wrapper(pw, _pw, d_.atom0, pos);	\
	Make_Nil(_pw);					\
    }

#define Build_Integer(pw,n,pos) {			\
	pword *_pw;					\
	Make_Term_Wrapper(pw, _pw, d_.integer0, pos);	\
	Make_Integer(_pw, n);				\
    }

#define Build_Struct(pw, pfct, d,pos) {			\
	pword *_pw;					\
	Flag_Type_Macro(pd, TCOMP);			\
	Flag_Did_Macro(pd, d);				\
	Make_Term_Wrapper(pw, _pw, d_.compound0, pos);	\
	Make_Struct(_pw, TG);			\
	pfct = TG;					\
	Push_Struct_Frame(d);			\
    }

#define Build_Struct_Or_List(pw, pfct, d,pos) {		\
	pword *_pw;					\
	Flag_Type_Macro(pd, TCOMP);			\
	Flag_Did_Macro(pd, d);				\
	Make_Term_Wrapper(pw, _pw, d_.compound0, pos);	\
	if ((d) == d_.list) {				\
	    Make_List(_pw, TG);				\
	    pfct = TG-1;				\
	    Push_List_Frame();				\
	} else {					\
	    Make_Struct(_pw, TG);			\
	    pfct = TG;					\
	    Push_Struct_Frame(d);			\
	}						\
    }

#define	Build_Atom_Or_Nil(pw, d,pos) {			\
	pword *_pw;					\
	Flag_Type_Macro(pd, TDICT);			\
	Flag_Did_Macro(pd, d);				\
	Make_Term_Wrapper(pw, _pw, d_.atom0, pos);	\
	_pw->val.did = d;				\
	_pw->tag.kernel = ((d) == d_.nil) ? TNIL : TDICT;\
    }

#define	Build_Number_From_Token(pd, pw) {		\
	pword *_pw;					\
	Make_Term_Wrapper(pw, _pw,			\
		tag_desc[tag_desc[TagType(pd->token.term.tag)].super].type_name,\
		pd->token.pos);\
	Flag_Type_Macro(pd, TagType(pd->token.term.tag));	\
	if (IsInterval(pd->token.term.tag)) {		\
	    Unmark_Interval_Raw(pd->token.term.val.ptr);	\
	    if (!GoodInterval(pd->token.term.val.ptr))	\
	    	return BAD_NUMERIC_CONSTANT;		\
	}						\
	*_pw = pd->token.term;				\
    }

#define	Build_String_From_Token(pd, pw) {		\
	word len1 = TokenStringLen(pd) + 1;		\
	pword *_pw;					\
	Flag_Type_Macro(pd, TSTRG);			\
	Make_Term_Wrapper(pw, _pw, d_.string0, pd->token.pos);		\
	_pw->val.ptr = TG;				\
	_pw->tag.kernel = TSTRG;			\
	Push_Buffer(len1);				\
	Copy_Bytes(StringStart(_pw->val), TokenString(pd), len1);\
    }


static void
_build_list_from_token(parse_desc *pd, pword *pw)
{
    int i;
    Flag_Type_Macro(pd, TINT);
    Flag_Type_Macro(pd, TDICT);
    Flag_Did_Macro(pd, d_.nil);
    Flag_Type_Macro(pd, TCOMP);
    Flag_Did_Macro(pd, d_.list);
    for(i=0; i<TokenStringLen(pd); ++i)
    {
	pword *phead;
	Build_List(pw, phead, pd->token.pos);
	Build_Integer(phead, TokenString(pd)[i], pd->token.pos);
	pw = phead+1;
    }
    Build_Nil(pw, pd->token.pos);
}


static pword *
_make_variable_from_token(parse_desc *pd, pword *pvar)
{
    dident did0 = D_UNKNOWN;
    /*
     * Non-anonymous variables are always allocated separately and referenced
     * from all their occurrences. The self-reference is never moved, and
     * the references can simply be copied. This is somewhat suboptimal since
     * one occurrence could have the self-reference in-place, but then that
     * occurrence could not be moved easily because it would be referred by
     * other occurrences and by the name table (see usage of Move_Pword()).
     */
    Make_Ref(pvar, TG);
    pvar = TG++;
    Check_Gc;
    if (pd->options & VARNAMES_PLEASE) {
	did0 = enter_dict_n(TokenString(pd), TokenStringLen(pd), 0);
	Make_NamedVar(pvar, did0);
    } else {
	Make_Var(pvar);
    }
    if (pd->var_list_tail)		/* need list element for readvar */
    {
	pword *pw = TG;
	Make_List(pd->var_list_tail, TG);
	Push_List_Frame();		/* list element */
	Push_List_Frame();		/* ['Name'|Var] pair */
	Make_List(&pw[0], &pw[2]);
	pd->var_list_tail = &pw[1];
	if (did0 == D_UNKNOWN)
	    did0 = enter_dict_n(TokenString(pd), TokenStringLen(pd), 0);
	Make_Atom(&pw[2], did0);
	Make_Ref(&pw[3], pvar);
    }
    return pvar;
}


/*
 * Lookahead function:	_delimiter_follows()
 *
 * Succeeds if a delimiter follows, i.e. something that can definitely
 * not start a term. This is used to disambiguate between infix and postfix:
 * when a delimiter follows, only the postfix interpretation is possible.
 */

static int
_delimiter_follows(parse_desc *pd)
{
    int res;
    Lookahead_Next_Token(pd);
    switch(pd->token.class)
    {
    case EOI:
    case EOCL:
    case COMMA:
    case BAR:
    case CLOSING_SOLO:
	res = 1;
	break;

    default:
	res = 0;
	break;
    }
    Prev_Token(pd);
    return res;
}


/*
 * Lookahead function:	_cant_follow_prefix()
 *
 * This is one of the more tricky bits of the whole parser.  The
 * function returns true if the current token cannot possibly follow a
 * prefix operator with precedence oprec/rprec (in that case, we may
 * still be able to parse the term by ignoring the prefix-property and
 * interpreting the potential prefix as a plain atom).  If the current
 * token happens to be an identifier, we look ahead a second token to
 * get better ambiguity resolution.
 */

/*
 * Conservative check for tokens that can start but not follow a term
 */
#define CantFollowTerm(class) (\
	(class)==NUMBER || (class)==SPACE_NUMBER || (class)==REFERENCE \
	|| (class)==UREFERENCE || (class)==STRING || (class)==LIST \
	|| (class)==SPACE_SOLO)
 
#define IsDelimiter(class) (\
	(class)==EOI || (class)==EOCL || (class)==COMMA || (class)==BAR \
	|| (class)==CLOSING_SOLO)\

static int
_cant_follow_prefix(parse_desc *pd, int context_flags,
	int oprec, int rprec, int prefix_arity)
{
    opi *pre_op, *follow_op;
    dident did0;
    int status;

    switch(pd->token.class)
    {
    case EOCL:
    case EOI:
    case CLOSING_SOLO:
	return 1;

    case COMMA:
	if (context_flags & COMMA_TERMINATES)
	    return 1;
	did0 = d_comma0_;
	goto _check_precedence_;

    case BAR:
	if ((context_flags & BAR_TERMINATES) || (pd->sd->options & BAR_IS_NO_ATOM))
	    return 1;
	did0 = d_bar0_;
	goto _check_precedence_;

    case IDENTIFIER:
    case QIDENTIFIER:
	/*
	 * An atom which is not an operator CAN follow the prefix
	 */
	did0 = check_did_n(TokenString(pd), TokenStringLen(pd), 0);
	if (did0 == D_UNKNOWN || !DidIsOp(did0))
	    return 0;

_check_precedence_:			/* (did0) */
	/*
	 * A functor-term CAN follow the prefix
	 */
	Lookahead_Next_Token(pd);	/* Prev_Token(pd) must follow! */
	if (IsClass(pd, SOLO) && (IsChar(pd, '(') || IsChar(pd, '{')))
	    goto _return_0_;
	    
	/*
	 * A signed number CAN follow the prefix
	 */
	if ((did0 == d_.minus0 || did0 == d_.plus0) && IsClass(pd,NUMBER))
	    goto _return_0_;

	/*
	 * prefix with lower priority CAN follow first prefix
	 */
	pre_op = visible_prefix_op(did0, pd->module, pd->module_tag, &status);
	if (pre_op && (GetOpiPreced(pre_op) <= rprec))
	    goto _return_0_;

	if (prefix_arity == 1)
	{
	    /*
	     * An infix/postfix with higher precedence CAN'T follow the prefix,
	     * i.e. forces the prefix to be interpreted as an atom.
	     * Moreover, if we have a sequence
	     *        prefix infix NEXT
	     * and NEXT is a token that can't follow a complete term, this
	     * forces the prefix to be interpreted as an atom (otherwise it
	     * would be a syntax error anyway), eg
	     *        local / 2
	     */
	    if (((follow_op = visible_infix_op(did0, pd->module, pd->module_tag, &status))
		    && (oprec <= InfixLeftPrecedence(follow_op)
			    || CantFollowTerm(pd->token.class)))
	     || ((follow_op = visible_postfix_op(did0, pd->module, pd->module_tag, &status))
		    && oprec <= PostfixLeftPrecedence(follow_op))
	       )
	    {
		Prev_Token(pd);
		return 1;
	    }
	}
	else /* if (prefix_arity == 2) */
	{
	    /*
	     * A sequence
	     *        prefix2 infix NEXT
	     * is ambiguous (independent of NEXT, either prefix2 of infix can
	     * be the functor). We could disambiguate using the precedence,
	     * but my feeling is that in practice one always wants the infix
	     * in such a case, e.g.
	     *        some / 2
	     * In a sequence
	     *        prefix2 postfix delimiter
	     * there is no choice but to prefer the postfix, and for a general
	     *        prefix2 postfix NEXT
	     * we prefer the postfix only if it binds weaker than the prefix
	     * (analogous to prefix/infix and prefix/postfix disambiguation).
	     */
	    if (((follow_op = visible_infix_op(did0, pd->module, pd->module_tag, &status))
		    /* && (oprec <= InfixLeftPrecedence(follow_op)) */ )
	     || ((follow_op = visible_postfix_op(did0, pd->module, pd->module_tag, &status))
		    && (oprec <= PostfixLeftPrecedence(follow_op)
			|| IsDelimiter(pd->token.class)))
	       )
	    {
		Prev_Token(pd);
		return 1;
	    }
	}

_return_0_:
    	Prev_Token(pd);
	return 0;


    default:
	return 0;
    }
}


/*
 * Parse a standard list, i.e. everything following  [  up until  ]
 */

static int
_read_list(parse_desc *pd, pword *result, source_pos_t *ppos)
{
    source_pos_t pos = *ppos;

    for(;;)
    {
	int status;
	pword *pw = TG;

	Build_List(result, pw, pos);
	status = _read_next_term(pd, pd->max_arg_prec, COMMA_TERMINATES|BAR_TERMINATES, pw);
	Return_If_Error(status);
	result = &pw[1];

	switch(pd->token.class)
	{
	case BAR:
	    Next_Token(pd);
	    status = _read_next_term(pd, pd->max_arg_prec, COMMA_TERMINATES|BAR_TERMINATES, result);
	    Return_If_Error(status);
	    if (!IsToken(pd, CLOSING_SOLO, ']'))
		return PUNCTUATION;
	    Next_Token(pd);
	    Flag_Type_Macro(pd, TCOMP);
	    Flag_Did_Macro(pd, d_.list);
	    return PSUCCEED;

	case COMMA:
	    pos = pd->token.pos;
	    Next_Token(pd);
	    break;

	case CLOSING_SOLO:
	    if (IsChar(pd, ']'))
	    {
		Build_Nil(result, pd->token.pos);
		Next_Token(pd);
		Flag_Type_Macro(pd, TDICT);
		Flag_Did_Macro(pd, d_.nil);
		Flag_Type_Macro(pd, TCOMP);
		Flag_Did_Macro(pd, d_.list);
		return PSUCCEED;
	    }
	    return UNCLOSBR;

	case EOI:
	    return ENDOFFILE;

	case EOCL:
	    return ENDOFCLAUSE;

	default:
	    return LexError(pd->token.class) ? pd->token.class : UNEXPECTED;
	}
    }
}


/*
 * Parse a comma-separated sequence up until the solo-char terminator
 * (currently either a closing round or curly bracket) and return it as a list
 */

static int
_read_sequence_until(parse_desc *pd, pword *result, int terminator)
{
    for(;;)
    {
	int status;
	pword *pw = TG;

	Build_List(result, pw, no_pos_);
	status = _read_next_term(pd, pd->max_arg_prec, COMMA_TERMINATES, pw);
	Return_If_Error(status);
	result = &pw[1];

	switch(pd->token.class)
	{
	case COMMA:
	    Next_Token(pd);
	    break;

	case CLOSING_SOLO:
	    if (IsChar(pd, terminator))
	    {
		Build_Nil(result, no_pos_);
		Next_Token(pd);
		return PSUCCEED;
	    }
	    return UNCLOSBR;

	case EOI:
	    return ENDOFFILE;

	case EOCL:
	    return ENDOFCLAUSE;

	default:
	    return LexError(pd->token.class) ? pd->token.class : UNEXPECTED;
	}
    }
}


/*
 * Parse a standard structure, i.e. everything after  f(  up until  )
 */

static int
_read_struct(parse_desc *pd, char *name, uword length, pword *result,
	source_pos_t *ppos)
{
    int status;
    dident functor;
    pword all_args;
    pword *tail = &all_args;
    pword *pw;
    uword i;

    for(i=1; ; ++i)
    {
	pw = TG;
	Make_List(tail, pw);
	Push_List_Frame();
	tail = pw+1;

	status = _read_next_term(pd, pd->max_arg_prec, COMMA_TERMINATES, pw);
	Return_If_Error(status);

	switch(pd->token.class)
	{
	case COMMA:
	    Next_Token(pd);
	    continue;

	case CLOSING_SOLO:
	    if (IsChar(pd, ')'))
	    {
		Make_Nil(tail);
		Next_Token(pd);
		break;
	    }
	    return UNCLOSBR;

	case EOI:
	    return ENDOFFILE;

	case EOCL:
	    return ENDOFCLAUSE;

	default:
	    return LexError(pd->token.class) ? pd->token.class : UNEXPECTED;
	}
	break;
    }

    /* create the structure from the argument list (the list becomes garbage) */
    pw = TG;
    functor = enter_dict_n(name, length, i);
    Build_Struct_Or_List(result, pw, functor, *ppos);
    tail = all_args.val.ptr;
    do
    {
	/* We use Move_Pword() instead of a simple copy - this will move
	 * anonymous variables instead of creating a reference to them.
	 */
	++pw;
	Move_Pword(tail, pw);
	tail = tail[1].val.ptr;
    }
    while(--i);

    return PSUCCEED;
}


/*
 * Parse a toplevel term, a subterm, or a right argument of prefix/infix
 */

static int
_read_next_term(parse_desc *pd,
	int context_prec,
	int context_flags,	/* terminators only */
	pword *result)
{
    int		status;
    pword	term;
    char	*name;
    uword	length;
    dident	did0;
    opi		*pre_op;
    source_pos_t	pos;

    pos = pd->token.pos;
    switch(pd->token.class)
    {

    case BAR:
	if (pd->sd->options & BAR_IS_NO_ATOM)
	    return UNEXPECTED;
	/* fall through and treat like normal identifier */

    case IDENTIFIER:
    case QIDENTIFIER:
	Save_Token_String(pd, name, length);	/* don't make dident eagerly */
_treat_like_identifier_:		/* (name, length) */
	Next_Token(pd);
	switch(pd->token.class)
	{

	case SOLO:
	    if (IsChar(pd, '('))
	    {
_treat_as_functor_:
		/* a compound term in canonical functor notation */
		Merge_Source_Pos(pos, pd->token.pos, pos);
		Next_Token(pd);
		status = _read_struct(pd, name, length, &term, &pos);
		Return_If_Error(status);
		*result = term;
		return _read_after_term(pd, context_prec, context_flags|SUBSCRIPTABLE, 0, result);
	    }
	    else if (IsChar(pd, '{') && !(pd->sd->options & NO_CURLY_ARGUMENTS))
	    {
		/* a structure with arguments in curly braces */
		pword *pw;
		dident did0 = enter_dict_n(name, length, 0);
		Build_Struct(&term, pw, d_.with2, pd->token.pos);
		Build_Atom_Or_Nil(&pw[1], did0, pos);
		Next_Token(pd);
		if (IsToken(pd, CLOSING_SOLO, '}'))
		{
		    Build_Nil(&pw[2], no_pos_);
		    Next_Token(pd);
		}
		else
		{
		    status = _read_sequence_until(pd, &pw[2], '}');
		    Return_If_Error(status);
		}
		*result = term;
		return _read_after_term(pd, context_prec, context_flags, 0, result);
	    }
	    break;

	case SPACE_NUMBER:
	    if (!(pd->sd->options & BLANK_AFTER_SIGN))
	    	break;
	    /* fall through */

	case NUMBER:
	    if (length==1 && (*name=='+' || *name=='-'))
	    {
		/* +/- followed by number: treat as a sign */
		if (*name == '-')
		{
		    tag_desc[pd->token.term.tag.kernel].arith_op[ARITH_CHGSIGN]
			    (pd->token.term.val, &pd->token.term);
		}
		Merge_Source_Pos(pos, pd->token.pos, pos);
		goto _make_number_;
	    }
	    break;
	}

	/* none of the special cases above - check if prefix */
	did0 = enter_dict_n(name, length, 0);
	pre_op = visible_prefix_op(did0, pd->module, pd->module_tag, &status);
	if (pre_op)
	{
	    if (!IsPrefix2(pre_op))	/* unary prefix */
	    {
		int oprec, rprec;
		Get_Prefix_Prec(pre_op, oprec, rprec);
		if (oprec <= context_prec
		    && !_cant_follow_prefix(pd, context_flags, oprec, rprec, 1))
		{
		    /* treat as prefix operator */
		    pword *pw;
		    Build_Struct(&term, pw, OpiDid(pre_op), pos);
		    status = _read_next_term(pd, rprec, context_flags, &pw[1]);
		    Return_If_Error(status);
		    *result = term;
		    return _read_after_term(pd, context_prec, context_flags, oprec, result);
		}
	    }
	    else			/* binary prefix */
	    {
		int oprec, lprec, rprec;
		Get_Prefix2_Prec(pre_op, oprec, lprec, rprec);
		if (oprec <= context_prec
		    && !_cant_follow_prefix(pd, context_flags, oprec, lprec, 2))
		{
		    /* treat as binary prefix operator */
		    pword *pw;
		    Build_Struct_Or_List(&term, pw, OpiDid(pre_op), pos);
		    status = _read_next_term(pd, lprec, context_flags|PREBINFIRST, &pw[1]);
		    Return_If_Error(status);
		    status = _read_next_term(pd, rprec, context_flags, &pw[2]);
		    Return_If_Error(status);
		    *result = term;
		    return _read_after_term(pd, context_prec, context_flags, oprec, result);
		}
	    }
	}

	if (IsToken(pd, SPACE_SOLO, '('))
	{
	    /* compatibility: allow space between functor and (  */
	    if (pd->sd->options & NO_BLANKS)
		return BLANK;
	    goto _treat_as_functor_;
	}

	/* treat as a simple atom */
	Build_Atom_Or_Nil(&term, did0, pos);
	*result = term;
	return _read_after_term(pd, context_prec, context_flags|FZINC_SUBSCRIPTABLE|ZINC_SUBSCRIPTABLE, 0, result);


    case NUMBER:
    case SPACE_NUMBER:
_make_number_:
	Build_Number_From_Token(pd, &term);
	Next_Token(pd);
	*result = term;
	return _read_after_term(pd, context_prec, context_flags, 0, result);


    case STRING:			/* string-quoted string */
	Build_String_From_Token(pd, &term);
	Next_Token(pd);
	*result = term;
	return _read_after_term(pd, context_prec, context_flags, 0, result);


    case LIST:				/* list-quoted string */
    	_build_list_from_token(pd, &term);
	Next_Token(pd);
	*result = term;
	return _read_after_term(pd, context_prec, context_flags|ZINC_SUBSCRIPTABLE, 0, result);


    case REFERENCE:			/* general variable */
    {
	vword *vp = _var_table_entry(pd, TokenString(pd), TokenStringLen(pd));
	pword *pvar;
	Make_Var_Wrapper(result, pvar, pd->token.pos);
	if (!vp->ptr)
	    vp->ptr = _make_variable_from_token(pd, pvar);
	else
	{
	    Make_Ref(pvar, vp->ptr);
	}
	goto _after_variable_;
    }


    case UREFERENCE:			/* anonymous variable */
    {
	pword *pvar;
	/* Anonymous variables are created "in-place" and
	 * may be moved later by the Move_Pword() macro */
	Make_Term_Wrapper(result, pvar, d_anonymous_, pd->token.pos);
	Make_Var(pvar);
    }

_after_variable_:
	Next_Token(pd);
	if (IsClass(pd, SOLO))
	{
	    if (IsChar(pd, '{') && !(pd->sd->options & NO_ATTRIBUTES))
	    {
		/* Attribute follows */
		pword *pw;
		Build_Struct(&term, pw, d_.with_attributes2, pd->token.pos);
		Move_Pword(result, pw+1);
		Next_Token(pd);
		status = _read_sequence_until(pd, &pw[2], '}');
		Return_If_Error(status);
		*result = term;
		return _read_after_term(pd, context_prec, context_flags, 0, result);
	    }
	    else if (IsChar(pd, '(') && (pd->sd->options & VAR_FUNCTOR_IS_APPLY))
	    {
		/* Arguments follow */
		pword *pw;
		Build_Struct(&term, pw, d_.apply2, pd->token.pos);
		Move_Pword(result, pw+1);
		Next_Token(pd);
		status = _read_sequence_until(pd, &pw[2], ')');
		Return_If_Error(status);
		*result = term;
		return _read_after_term(pd, context_prec, context_flags|ZINC_SUBSCRIPTABLE, 0, result);
	    }
	}
	return _read_after_term(pd, context_prec, context_flags|SUBSCRIPTABLE, 0, result);


    case SOLO:				/* {[( */
    case SPACE_SOLO:			/* {( */
    	if (IsChar(pd, '['))
	{
	    Next_Token(pd);
	    if (IsToken(pd, CLOSING_SOLO, ']'))
	    {
		/* the atom or functor '[]' */
		name = "[]"; length = 2;
		Merge_Source_Pos(pos, pd->token.pos, pos);
		goto _treat_like_identifier_;	/* (name,length) */
	    }
	    else
	    {
		/* non-empty list in standard list notation */
		status = _read_list(pd, &term, &pos);
		Return_If_Error(status);
	    }
	    *result = term;
	    context_flags |= ZINC_SUBSCRIPTABLE;
	}
    	else if (IsChar(pd, '('))
	{
	    /* parenthesised subterm */
	    Next_Token(pd);
	    status = _read_next_term(pd, 1200, 0, &term);
	    Return_If_Error(status);
	    if (!IsClass(pd, CLOSING_SOLO))
	    	return UNEXPECTED;
	    if (!IsChar(pd, ')'))
	    	return UNCLOSBR;
	    Next_Token(pd);
	    Move_Pword(&term, result);	/* could be a self-ref! */
	    context_flags |= ZINC_SUBSCRIPTABLE;
	}
    	else if (IsChar(pd, '{'))
	{
	    Next_Token(pd);
	    if (IsToken(pd, CLOSING_SOLO, '}'))
	    {
		/* the atom or functor '{}' */
		name = "{}"; length = 2;
		Merge_Source_Pos(pos, pd->token.pos, pos);
		goto _treat_like_identifier_;	/* (name,length) */
	    }
	    else
	    {
		/* term in curly brackets: parse as {}/1 structure */
		pword *pw;
		Build_Struct(&term, pw, d_.nilcurbr1, pos);
		if (pd->sd->options & CURLY_ARGS_AS_LIST)
		{
		    /* {a,b,c} is read as {}([a,b,c]) */
		    status = _read_sequence_until(pd, &pw[1], '}');
		}
		else
		{
		    /* {a,b,c} is read as {}(','(a,','(b,c))) */
		    status = _read_next_term(pd, 1200, 0, &pw[1]);
		    Return_If_Error(status);
		    if (!IsClass(pd, CLOSING_SOLO))
			return UNEXPECTED;
		    if (!IsChar(pd, '}'))
			return UNCLOSBR;
		    Next_Token(pd);
		}
	    }
	    *result = term;
	}
	else
	{
	    return UNEXPECTED;
	}
	return _read_after_term(pd, context_prec, context_flags, 0, result);


    case CLOSING_SOLO:			/* }]) */
	return UNCLOSBR;

    case COMMA:
	return UNEXCOMMA;

    case EOI:
	return ENDOFFILE;

    case EOCL:	
	return ENDOFCLAUSE;

    default:
	return LexError(pd->token.class) ? pd->token.class : UNEXPECTED;
    }
}


/*
 * Parse what can follow a complete term, i.e.
 * delimiter, infix, postfix, or possibly subscript.
 */

static int
_read_after_term(parse_desc *pd, int context_prec,
	int context_flags,	/* terminators, allowed subscripts */
	int lterm_prec,
	pword *result)		/* in: lterm, out: result */
{
    int		status;
    pword	term;
    dident	did0;
    opi		*in_op;
    opi		*post_op;

    for(;;)				/* for removing tail recursion */
    {
	switch(pd->token.class)
	{

	case IDENTIFIER:
	case QIDENTIFIER:
	    did0 = enter_dict_n(TokenString(pd), TokenStringLen(pd), 0);
_treat_like_atom_:		/* (did0) - caution: may have wrong token in pd */
	    context_flags &= ~(SUBSCRIPTABLE|FZINC_SUBSCRIPTABLE|ZINC_SUBSCRIPTABLE);
	    in_op = visible_infix_op(did0, pd->module, pd->module_tag, &status);
	    post_op = visible_postfix_op(did0, pd->module, pd->module_tag, &status);
	    if (in_op && !(post_op && _delimiter_follows(pd)))
	    {
		/* treat as infix */
		pword *pw;
		int lprec, oprec, rprec;
		Get_Infix_Prec(in_op, lprec, oprec, rprec);
		if (oprec > context_prec)
		    return PSUCCEED;
		if (lterm_prec > lprec)
		    return context_flags & PREBINFIRST ? PSUCCEED : BRACKET;
		Build_Struct_Or_List(&term, pw, OpiDid(in_op), pd->token.pos);
		/* Use Move_Pword() to move possible self-refs in result
		 * (because we are going to reuse result!) */
		Move_Pword(result, pw+1);
		Next_Token(pd);
		status = _read_next_term(pd, rprec, context_flags, &pw[2]);
		Return_If_Error(status);
		/*return _read_after_term(pd, context_prec, context_flags, term, oprec, result);*/
		*result = term; lterm_prec = oprec;
		break;	/* tail recursion */
	    }
	    else if (post_op)
	    {
		/* treat as postfix */
		pword *pw;
		int lprec, oprec;
		Get_Postfix_Prec(post_op, lprec, oprec);
		if (oprec > context_prec)
		    return PSUCCEED;
		if (lterm_prec > lprec)
		    return context_flags & PREBINFIRST ? PSUCCEED : BRACKET;
		Build_Struct(&term, pw, OpiDid(post_op), pd->token.pos);
		/* Use Move_Pword() to move possible self-refs in result
		 * (because we are going to reuse result!) */
		Move_Pword(result, pw+1);
		Next_Token(pd);
		/*return _read_after_term(pd, context_prec, context_flags, term, oprec, result);*/
		*result = term; lterm_prec = oprec;
		break;	/* tail recursion */
	    }
	    return context_flags & PREBINFIRST ? PSUCCEED : POSTINF;


	case COMMA:
	    if ((context_flags & COMMA_TERMINATES))
	    {
		return PSUCCEED;
	    }
	    did0 = d_comma0_;
	    goto _treat_like_atom_;	/* (did0) */


	case BAR:
	    if ((context_flags & BAR_TERMINATES))
	    {
		return PSUCCEED;
	    }
	    /* Prolog compatibility: an (unquoted) bar in infix
	     * position is treated as if it were a semicolon */
	    did0 = pd->sd->options & BAR_IS_NO_ATOM ? d_.semi0 : d_bar0_;
	    goto _treat_like_atom_;	/* (did0) */


	case SOLO:
	    if (IsChar(pd, '[') && !(pd->sd->options & NO_ARRAY_SUBSCRIPTS)
	    	&& ((context_flags & SUBSCRIPTABLE) ||
		    (context_flags & FZINC_SUBSCRIPTABLE) && (pd->sd->options & ATOM_SUBSCRIPTS) ||
		    (context_flags & ZINC_SUBSCRIPTABLE) && (pd->sd->options & GENERAL_SUBSCRIPTS)))
	    {
		/* translate Term[Args] into subscript(Term, [Args]) */
		pword *pw;
		Build_Struct(&term, pw, d_.subscript, pd->token.pos);
		pw[1] = *result;
		Next_Token(pd);
		status = _read_sequence_until(pd, &pw[2], ']');
		Return_If_Error(status);
		context_flags &= ~(SUBSCRIPTABLE|FZINC_SUBSCRIPTABLE);
		/*return _read_after_term(pd, context_prec, context_flags, term, 0, result);*/
		*result = term; lterm_prec = 0;
		break;	/* tail recursion */
	    }
	    return UNEXPECTED;


	case EOI:
	case EOCL:
	case CLOSING_SOLO:
	    return PSUCCEED;

	default:
	    return LexError(pd->token.class) ? pd->token.class : context_flags & PREBINFIRST ? PSUCCEED : UNEXPECTED;
	}
    }
}


/*
 * The toplevel parser procedure. It reads one term from the given stream
 * and makes sure it is properly terminated.
 */

int
ec_read_term(
    	stream_id nst,		/* the stream to read from */
	int options,		/* options (VARNAMES_PLEASE etc) */
	pword *result,		/* where to store the parsed term */
	pword *varlist,		/* where to store the var list (or NULL) */
	int *has_macro,		/* flag that the term may contain (clause or
				* goal) macros that need to be expanded */
	value vm, type tm	/* context module */
    )
{
    int status;
    parse_desc *pd;
    pword *old_tg = TG;
    pword *pw;

    if (StreamMode(nst) & REPROMPT_ONLY)
	StreamMode(nst) |= DONT_PROMPT;

    pd = _alloc_parse_env(options, nst, vm.did, tm);
    pd->var_list_tail = varlist;

    Next_Token(pd);
    switch(pd->token.class)
    {
    case EOI:
	if (StreamMode(pd->nst) & MEOF)
	{
	    status = IsSoftEofStream(pd->nst) ? PEOF : READ_PAST_EOF;
	    goto _return_error_;
	}
	StreamMode(pd->nst) |= MEOF;
	status = PEOF;
	goto _return_error_;

    default:
	status = _read_next_term(pd, 1200, 0, result);
	if (status != PSUCCEED)
	    goto _return_error_;
	switch(pd->token.class)
	{
	case EOCL:
	case EOI:
	    break;
	default:
	    status = UNEXPECTED;
	    goto _return_error_;
	}
	break;
    }

    if (pd->var_list_tail)
    {
	Make_Nil(pd->var_list_tail);
    }

    /* expand (read) macros if there were any (and expansion is not disabled) */
    if (pd->macro && (GlobalFlags & MACROEXP) && !(StreamMode(pd->nst) & SNOMACROEXP)
    	&& !(options & LAYOUT_PLEASE))
    {
	pw = result;
	Dereference_(pw);
	if (!(IsRef(pw->tag) && pw == result))
	{
	    pw = TG;
	    Push_Struct_Frame(in_dict("expand_macros_",3));
	    pw[1] = *result;
	    Make_Var(&pw[2]);
	    pw[3].val.all = vm.all;
	    pw[3].tag.all = tm.all;
	    status = do_trafo(pw);
	    Return_If_Error(status);
	    *result = pw[2];
	}
    }
    if (has_macro)
    	*has_macro = pd->macro;

    return PSUCCEED;

_return_error_:
    TG = old_tg;	/* pop (possibly incomplete) constructed term */
    return status;
}



/*********************** THE PARSER RELATED BUILTINS ********************/

void
read_init(int flags)
{

    d_comma0_ = in_dict(",", 0);
    d_bar0_ = in_dict("|", 0);
    d_annotated_term_ = in_dict("annotated_term", TERM_ARITY);
    d_anonymous_ = in_dict("anonymous", 0);
    no_pos_.file = d_.empty;

    if (!(flags & INIT_SHARED))
	return;

    exported_built_in(in_dict("read_", 2), p_read2, B_UNSAFE|U_FRESH)
	-> mode = BoundArg(1, NONVAR);
    exported_built_in(in_dict("read_", 3), p_read3, B_UNSAFE|U_FRESH)
	-> mode = BoundArg(2, NONVAR);
    exported_built_in(in_dict("readvar", 4), p_readvar, B_UNSAFE|U_FRESH)
	-> mode = BoundArg(2, NONVAR) | BoundArg(3, NONVAR);
    exported_built_in(in_dict("read_annotated_raw", 4), p_read_annotated_raw, B_UNSAFE|U_FRESH)
	-> mode = BoundArg(2, NONVAR) | BoundArg(3, CONSTANT);
}



/*
 *	read_(Term, Module)
 *	reads a term from the current input
*/
static int
p_read2(value v, type t, value vm, type tm)
{
    int     status;

    Check_Module(tm, vm);
    status = _pread3(v, t, current_input_, vm, tm);
    if (status < 0)
    {
	Bip_Error(status)
    }
    return (status);
}

/*
 *	read_(Stream, Term, Module)
 *	reads a termfrom the current input and unifies it with its argument.
 *	The unification/dereferencing is done by the emulator on Request_unify
*/
int
p_read3(value vs, type ts, value v, type t, value vm, type tm)
{
    int     	status;
    stream_id	nst = get_stream_id(vs, ts, SREAD, &status);

    Check_Module(tm, vm);
    if (nst == NO_STREAM)
    {
	Bip_Error(status)
    }
    if(!(IsReadStream(nst)))
    {
	Bip_Error(STREAM_MODE);
    }

    status = _pread3(v, t, nst, vm, tm);
    if (status < 0)
    {
	Bip_Error(status)
    }
    return (status);
}


static int
_pread3(value v, type t, stream_id nst, value vm, type tm)
{
    int  	status;
    pword	*pw;
    pword	result;		/* be careful not to pass this pword to Prolog,
				 * e.g. when calling a macro transformation
				 * (cf. bug #560), or when returning (see below).
				 */
    status = ec_read_term(nst,
    		(GlobalFlags & VARIABLE_NAMES ? VARNAMES_PLEASE : 0),
		&result, 0, 0, vm, tm);

    if (status != PSUCCEED)
	return (status);

    pw = &result;
    Dereference_(pw);
    if (IsRef(pw->tag) && pw == &result)
    {
	Succeed_;	/* a free variable */
    }
    Return_Unify_Pw(v, t, pw->val, pw->tag)
}


/*
 *	readvar(Stream, Term, ListVar, Module)
 *	reads a term from the current input, unifies with with the
 *	first argument, unifies the second argument with the list of doublets
 *	[namevar|adrvar].
*/
static int
p_readvar(value vs, type ts, value v, type t, value vv, type tv, value vm, type tm)
{
    pword	*pw;
    pword	result;
    pword	vars;
    int		 status;
    stream_id	 nst = get_stream_id(vs, ts, SREAD, &status);
    Prepare_Requests

    if (nst == NO_STREAM)
    {
	Bip_Error(status)
    }

    Check_Ref(tv);
    Check_Module(tm, vm);
    if(!(IsReadStream(nst)))
    {
	Bip_Error(STREAM_MODE);
    }

    status = ec_read_term(nst,
    		(GlobalFlags & VARIABLE_NAMES ? VARNAMES_PLEASE : 0),
		&result, &vars, 0, vm, tm);

    if (status != PSUCCEED)
    {
	Bip_Error(status);
    }

    Request_Unify_Pw(vv, tv, vars.val, vars.tag);

    pw = &result;
    Dereference_(pw);
    if (!(IsRef(pw->tag) && pw == &result))
    {
	Request_Unify_Pw(v, t, pw->val, pw->tag);
    }
    Return_Unify;
}


static int
p_read_annotated_raw(value vs, type ts, value v, type t, value vf, type tf, value vm, type tm)
{
    pword	*pw;
    pword	result;
    int		 status;
    int		has_macro = 0;
    stream_id	 nst = get_stream_id(vs, ts, SREAD, &status);
    Prepare_Requests

    if (nst == NO_STREAM)
    {
	Bip_Error(status)
    }

    Check_Module(tm, vm);
    if(!(IsReadStream(nst)))
    {
	Bip_Error(STREAM_MODE);
    }

    status = ec_read_term(nst, LAYOUT_PLEASE |
    		(GlobalFlags & VARIABLE_NAMES ? VARNAMES_PLEASE : 0),
		&result, 0, &has_macro, vm, tm);

    if (status != PSUCCEED)
    {
	Bip_Error(status);
    }

    /* return flag indicating request for macro expansion */
    if (!(GlobalFlags & MACROEXP) || (StreamMode(nst) & SNOMACROEXP))
    	has_macro = 0;
    Request_Unify_Integer(vf, tf, has_macro)

    pw = &result;
    Dereference_(pw);
    if (!(IsRef(pw->tag) && pw == &result))
    {
	Request_Unify_Pw(v, t, pw->val, pw->tag)
    }
    Return_Unify
}



/*********************** PREPARING A PARSER CALL ************************/

/*
 * Allocate and initialise a parsing environment
 *
 * contents of the parsing environment: see type declaration
 *
 * Remaining Problem: when a read is aborted via an interrupt, the parsing
 * environment is not freed.
 */

static parse_desc *
_alloc_parse_env(int options, stream_id nst, dident module, type mod_tag)
{
    register parse_desc	*pd = (parse_desc *) PARSENV;

    if (pd)			/* reinit the existing parser environment */
    {
	if (NUMBER_VAR != pd->var_table_size)	/* table size changed */
	{
	    hp_free_size((generic_ptr) pd->var_table, pd->var_table_size*sizeof(vword));
	    pd->var_table_size = NUMBER_VAR;
	    pd->var_table = (vword *) hp_alloc_size((int)NUMBER_VAR * sizeof(vword));
	    pd->counter = 0;
	}
	Temp_Reset(pd->string_store);
    }
    else			/* allocate a new parsing environment */
    {
	pd = (parse_desc *) hp_alloc_size(sizeof(parse_desc));
	pd->var_table_size = NUMBER_VAR;
	pd->var_table = (vword *) hp_alloc_size((int)NUMBER_VAR * sizeof(vword));
	pd->counter = 0;
	Temp_Create(pd->string_store, 1024);
	PARSENV = (void_ptr) pd;	/* store it globally */
    }

    pd->nst = nst;
    pd->sd = ModuleSyntax(module);
    pd->module = module;
    pd->module_tag = mod_tag;
    pd->token.class = pd->prev_token.class = pd->next_token.class = NO_TOKEN;
    pd->macro = 0;
    pd->options = options;
    pd->max_arg_prec = (pd->sd->options & LIMIT_ARG_PRECEDENCE) ? 999 : 1200;

    if (pd->counter++ == 0)	/* (re)init the hash table	*/
    {
	register vword	*v = pd->var_table;
	register vword	*last = v + NUMBER_VAR;
	while (v < last)
	    (v++)->lock = 0;
    }
    return pd;
}


int
destroy_parser_env(void)			/* called when exiting emulators */
{
    register parse_desc	*pd = (parse_desc *) PARSENV;

    if (pd)				/* deallocate the parsing environment */
    {
	hp_free_size((generic_ptr) pd->var_table, pd->var_table_size*sizeof(vword));
	Temp_Destroy(pd->string_store);
	hp_free_size((generic_ptr) pd, sizeof(parse_desc));
	PARSENV = (void_ptr) 0;
    }
    return 0;
}


/************************ MACRO TRANSFORMATION SUPPORT **************************/

/*
 * Run transformation goal, catch aborts,
 * turn numeric exit_block tags into negative error return code.
 * Returns PSUCCEED, PFAIL or error code
*/
int
do_trafo(pword *term)	/* goal to call */
{
    pword	saved_a1;
    int		res;
    value	v1;
    value 	v2;
    type	t2;

    v1.ptr = term;
    v2.did = d_.kernel_sepia;
    t2.kernel = ModuleTag(d_.kernel_sepia);
    /* hack to preserve A[1] in case it gets overwritten by exit_block/1 */
    saved_a1 = A[1];
    res = sub_emulc_noexit(v1, tcomp, v2, t2);
    if (res == PTHROW)
    {
	pword ball = A[1];
	A[1] = saved_a1;
	if (IsInteger(ball.tag) && ball.val.nint > 0)
	    res = (int) -ball.val.nint;
	else
	    res = TRANS_ERROR;
    }
    return res;
}

/*
 * Create a transformation goal for the functor tr_did. Return 0 if
 * no transformation possible/necessary.
 */
pword *
trafo_term(dident tr_did,	/* the functor of the term to transform	*/
	int flags,		/* conditions for the macro		*/
	dident mv,		/* current module	*/
	type mt,		/* its tag		*/
	int *tr_flags)		/* flags of the macro	*/
{
    pword	*pw;
    pword	*prop;
    macro_desc	*md;
    int		err;
    int		propid;

    /* for input goal and clause macros we don't build the goal */
    if ((flags & TR_GOAL) && !(flags & TR_WRITE))
    {
	*tr_flags = TR_GOAL;
	return 0;
    }

    if (flags & TR_CLAUSE)
	propid = CLAUSE_TRANS_PROP;
    else if (flags & TR_GOAL)
	propid = GOAL_TRANS_PROP;
    else
	propid = TRANS_PROP;
    if (flags & TR_WRITE)
	propid++;
    prop = get_modular_property(tr_did, propid, mv, mt, VISIBLE_PROP, &err);
    if (!prop) {
	*tr_flags = 0;
	return 0;
    }

    md = (macro_desc *) prop->val.ptr;
    *tr_flags = md->flags;
    /* check if the type is ok */
    if ((md->flags & flags) != (md->flags & TR_TYPE))
	return 0;

    /* create a goal of the form:
     *	trans_term( <trans>(In,Out{,AnnIn,AnnOut}{,CurModule}), TrModule ) or
     *  AnnIn,AnnOut are always uninstantiated here
     */
    pw = Gbl_Tg;
    Gbl_Tg += DidArity(md->trans_function) + 4;
    Check_Gc;
    pw->tag.all		= TDICT;
    pw->val.did		= d_.trans_term;
    (pw+1)->tag.kernel	= TCOMP;
    (pw+1)->val.ptr	= pw+3;
    (pw+2)->tag.kernel	= ModuleTag(tr_did);
    (pw+2)->val.did	= md->module;
    (pw+3)->tag.kernel	= TDICT;
    (pw+3)->val.did	= md->trans_function;

    (pw+5)->tag.kernel	= TREF;
    (pw+5)->val.ptr	= (pw+5);
    switch (DidArity(md->trans_function))
    {
    case 2: /* <trans>(In, Out) */
	break;
    case 3: /* <trans>(In,Out,CurModule) */
	(pw+6)->tag.all	= mt.all;
	(pw+6)->val.did	= mv;
	break;
    case 5: /* <trans>(In,Out,AnnIn,AnnOut,CurModule) */
	(pw+8)->tag.all	= mt.all;
	(pw+8)->val.did	= mv;
 	/* falls through */
    case 4: /* <trans>(In,Out,AnnIn,AnnOut) */
	(pw+6)->tag.kernel = TREF;
	(pw+6)->val.ptr	= (pw+6);
	(pw+7)->tag.kernel = TREF;
	(pw+7)->val.ptr	= (pw+7);
	break;
    default:
	/* incorrect arity for <trans> */
	Gbl_Tg = Gbl_Tg - DidArity(md->trans_function) - 4;
	return 0;
    }

    return pw;
}


/*
 * Transform the metaterm attribute into the internal form.
 */
pword *
transf_meta_in(pword *pw, dident mod, int *err)
{
    int			arity = p_meta_arity_->val.nint;
    int			i;
    register pword	*r;

    r = TG;
    TG += 1 + arity;
    Check_Gc;
    r[0].val.did = in_dict("meta", arity);
    r[0].tag.kernel = TDICT;
    for (i = 1; i <= arity; i++) {
	r[i].val.ptr = r + i;
	r[i].tag.kernel = TREF;
    }
    i = meta_index(mod);
    i = _transf_attribute(pw, r, i);
    if (i != PSUCCEED) {
	*err = i;
	return 0;
    } else
	return r;
}

static int
_transf_attribute(register pword *pw, pword *r, int def)
{
    int		res;
    pword	*s;

    Dereference_(pw);
    if (IsStructure(pw->tag))
    {
	s = pw->val.ptr;
	if (s->val.did == d_.comma) {
	    if ((res = _transf_attribute(s + 1, r, def)) < 0)
		return res;
	    return _transf_attribute(s + 2, r, def);
	} else if (s->val.did == d_.colon) {
	    pw = s + 1;
	    Dereference_(pw);
	    if (IsAtom(pw->tag)) {
		def = meta_index(pw->val.did);
		pw = s + 2;
	    } else if (IsRef(pw->tag))
		return INSTANTIATION_FAULT;
	    else
		return TYPE_ERROR;
	}
    }
    if (!def)
	return UNDEF_ATTR;
    if (!(IsVar(r[def].tag)  &&  r[def].val.ptr == r + def))
	return TYPE_ERROR;
    r[def].val.ptr = pw->val.ptr;
    r[def].tag.kernel = pw->tag.kernel;
    return PSUCCEED;
}


/*
 * Transform the metaterm attribute into the external format.
 * Note that the caller has to allocate sufficient memory for
 * the constructed term (ATTR_IO_TERM_SIZE pwords at top).
 * The function returns the end of the memory actually used.
 */
pword *
transf_meta_out(value val,	/* attribute term to transform */
	type tag,
	pword *top,		/* where to build the the resulting term */
	dident mod,		/* context module (or D_UNKNOWN) */
	pword *presult)		/* where to store the result */
{
    /* by default, return the untransformed term */
    presult->val.all = val.all;
    presult->tag.all = tag.all;

    /* transform only if we have a proper meta/N structure */
    if (IsStructure(tag)  &&  check_did(val.ptr->val.did,0) == d_.meta0)
    {
	int i, first = 1;

	for (i = DidArity(val.ptr->val.did); i > 0; --i)
	{
	    dident wd = meta_name(i);

	    if (wd != D_UNKNOWN)
	    {
		pword attr;
		if (wd == mod) {
		    attr = val.ptr[i];		/* don't module-qualify */
		} else {
		    pword *pw = top;		/* construct name:AttrI */
		    top += 3;
		    Make_Struct(&attr, pw);
		    Make_Atom(&pw[0], d_.colon); /* should be Make_Functor() */
		    Make_Atom(&pw[1], wd);
		    pw[2] = val.ptr[i];
		}
		if (first) {
		    *presult = attr;		/* the only attribute so far*/
		    first = 0;
		} else {
		    pword *pw = top;		/* construct QAttrI,Others */
		    top += 3;
		    Make_Atom(&pw[0], d_.comma); /* should be Make_Functor() */
		    pw[1] = attr;
		    pw[2] = *presult;
		    Make_Struct(presult, pw);
		}
	    }
	}
    }
    return top;
}


/********************* VARIABLE NAME HASHING *******************************/

static unsigned long
hashfunction(char *id)
{
	register unsigned long	hash;
	register int		length, shift, ival;
	register char		*str;

	length = 0;
	hash = 0;
        for (str = id; *str; str++)        
        {
            ival = *str & 0x000000FF;    		/* get rid of sign extension */ 
            shift = length + 4 * (length & 3);	 /* add 0, 4, 8 or 12 */
            shift &= 0x0000000F;               	 /* keep important bits */
	    hash ^= (ival << (shift) | ival >> (16 - shift)); 
            hash &= 0x0000FFFF;
            length++;
        }

        return(hash);
}

static vword *
_alloc_vword(register parse_desc *pd)
{
    Temp_Align(pd->string_store, sizeof(int *));
    return (vword *) TempAlloc(pd->string_store, sizeof(vword));
}

static vword *
_var_table_entry(parse_desc *pd, char *varname, word length)
{
    vword *p, *q;
    p = &pd->var_table[hashfunction(varname) % NUMBER_VAR];

    if (p->lock == pd->counter)		/* there is a table entry */
    {
	while (p && strcmp(p->str, varname)) { /* search the chain */
	    q = p;
	    p = p->next;
	}
	if (p)
	{
	    return p;
	}
	q->next = p = _alloc_vword(pd);
    }

    /* it is a new variable, copy the string and make a table entry */
    p->str = TempAlloc(pd->string_store, length+1);
    Copy_Bytes(p->str, varname, length+1);
    p->lock = pd->counter;
    p->next = 0;
    p->ptr = 0;
    return p;
}
