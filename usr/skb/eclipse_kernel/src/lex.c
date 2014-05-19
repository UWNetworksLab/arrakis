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
 * VERSION	$Id: lex.c,v 1.1 2008/06/30 17:43:56 jschimpf Exp $
 */

/*
 * IDENTIFICATION		lex.c
 *
 *
 *
 * AUTHOR	VERSION	 DATE	REASON
 * Jorge Bocca
 * Pierre Dufresne
 * Micha Meier
 */

/*
 * INCLUDES:
 */

#include <math.h>

#include    "config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#else
extern char     *strcpy();
#endif

#include	"sepia.h"	/* to be able to have built-ins */
#include	"types.h"	/* to have the standard types (for BIP) */
#include	"embed.h"
#include	"mem.h"		/* to use in_dict and DidName */
#include	"error.h"	/* the BIP return values and standard errors */
#include	"dict.h"
#include	"lex.h"		/* the values returned lex_an */
#include	"io.h"
#include	"emu_export.h"
#include 	"module.h"
#include	"property.h"	/* for MODULE_PROP */
#include	"os_support.h"
#include	"rounding_control.h"



#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#else
extern double atof();
#endif

/*
 * DEFINES:
 */

/* The maximum numeric value of a character constant in a string.
 * This should eventually be changed to depend on the stream encoding,
 * e.g. 8-bit:255, utf8:2147483647
 */
#define MAX_CHAR_CODE	255

/*
 * FUNCTOR_COMPLETION causes trouble in connection with testing for
 * the .eco header in procedure.c. Also, it does not work on Windows.
 */
#undef FUNCTOR_COMPLETION

#ifdef FUNCTOR_COMPLETION
#if defined(HAVE_READLINE)
static char	**_complete_predicate(char *text, int start, int end);
static char	*_find_matching_predicate(char *string, int state);
#define Find_Matching_Atom(end, nst, pw, stop)
#else
static void	_find_matching_atom(unsigned char *end, stream_id nst, unsigned char **pw, unsigned char **stop);
#define Find_Matching_Atom(end, nst, pw, stop)	_find_matching_atom(end, nst, &pw, &stop)
#endif
#endif

#define Extend_Lex_Aux(nst, pw, stop)				\
		pw = _extend_lex_aux(nst);			\
		stop = StreamLexAux(nst) + StreamLexSize(nst);

/*
 * STATIC VARIABLE DEFINITIONS: 
 */
static int 	p_set_chtab(value v1, type t1, value v2, type t2, value vm, type tm), 
		p_get_chtab(value v1, type t1, value v2, type t2, value vm, type tm),
		p_get_syntax(value val1, type tag1, value val2, type tag2, value vm, type tm),
		p_set_syntax(value val1, type tag1, value val2, type tag2, value vm, type tm),
		p_copy_syntax(value vfrom, type tfrom, value vto, type tto),
		p_read_token_(value vs, type ts, value v, type t, value vc, type tc, value vm, type tm);

static unsigned char	*_extend_lex_aux(stream_id nst);
static int	_skip_blanks(stream_id nst, syntax_desc *sd, unsigned char **p_pligne, int *p_cc, int *p_ctype);

static dident	chname_[NBCH + 1];
static dident	tname_[NBTK + 1];
static dident	d_comma0_;
static int	completion_idx,
		completion_length,
		completion_start;
static dident	completion_dip;

static syntax_desc	default_syntax_desc = {
/* Here is the initial type distribution: */
{
/* nul soh stx etx eot enq ack bel  bs  ht  nl  vt  np  cr  so  si */
    BS, BS, BS, BS, BS, BS, BS, BS, DL, BS, NL, BS, BS, BS, BS, BS,
/* dle dc1 dc2 dc3 dc4 nak syn etb can  em sub esc  fs  gs  rs  us */
    BS, CQ, BS, BS, BS, KI, BS, BS, KI, BS, BS, BS, BS, BS, BS, BS,
/*  sp   !   "   #   $   %   &   '   (   )   *    +   ,   -   .   / */
    BS, SL, SQ, SY, SY, CM, SY, AQ, DS, DS, CM2, SY, DS, SY, SY, CM1,
/*   0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? */
     N,  N,  N,  N,  N,  N,  N,  N,  N,  N, SY, SL, SY, SY, SY, SY,
/*   @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O */
    SY, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,
/*   P   Q   R   S   T   U   V   W   X   Y   Z   [   \  ]   ^   _ */
    UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, DS, ES, DS, SY, UL,
/*   `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o */
    SY, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
/*   p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~ del */
    LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, DS, DS, DS, SY, DL,
/*  80	                       Latin-1                          8f */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,
/*  90	                                                        9f */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,
/*  a0	                                                        af */
    BS, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY,
/*  b0	                                                        bf */
    SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY, SY,
/*  c0	                                                        cf */
    UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,
/*  d0	                                                        df */
    UC, UC, UC, UC, UC, UC, UC, SY, UC, UC, UC, UC, UC, UC, UC, LC,
/*  e0	                                                        ef */
    LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
/*  f0	                                                        ff */
    LC, LC, LC, LC, LC, LC, LC, SY, LC, LC, LC, LC, LC, LC, LC, LC,
/*  EOI symbol */
    RE},
    NEWLINE_IN_QUOTES|NO_BLANKS,	/* options */
    '"',				/* sq	   */
    '\'',				/* aq	   */
    '_',				/* ul	   */
    '\\'				/* escape  */
};


static dident syntax_flags[SYNTAX_FLAGS]; /* the syntax flag names (dids) */

/*
 * EXTERNAL VARIABLE DECLARATIONS:
 */

/*
 * EXTERNAL VARIABLE DEFINITIONS: 
 */

syntax_desc	*default_syntax = &default_syntax_desc;

/*
 * FUNCTION DEFINITIONS: 
 */

void
lex_init(int flags)	/* initialization: setting the name of types */
{
    /*
     * syntax_flags, chname_ and tname_ are read-only data
     * and are replicated in every process
     */

    /* the array must correspond to the flag order in syntax */
    syntax_flags[0] =	in_dict("nl_in_quotes",0);
    syntax_flags[1] =	in_dict("limit_arg_precedence",0);
    syntax_flags[2] =	in_dict("no_blanks",0);
    syntax_flags[3] =	in_dict("bar_is_no_atom",0);
    syntax_flags[4] =	in_dict("blanks_in_nil",0);	/* obsolete, no effect */
    syntax_flags[5] =	in_dict("no_attributes",0);
    syntax_flags[6] =	in_dict("$VAR",0);		/* obsolete */
    syntax_flags[7] =	in_dict("nested_comments",0);
    syntax_flags[8] =	in_dict("based_bignums",0);
    syntax_flags[9] =	in_dict("dense_output",0);	/* obsolete */
    syntax_flags[10] =	in_dict("no_array_subscripts",0);
    syntax_flags[11] =	in_dict("doubled_quote_is_quote",0);
    syntax_flags[12] =	in_dict("iso_escapes",0);
    syntax_flags[13] =	in_dict("iso_base_prefix",0);
    syntax_flags[14] =	in_dict("read_floats_as_breals",0);
    syntax_flags[15] =	in_dict("no_curly_arguments",0);
    syntax_flags[16] =	in_dict("blanks_after_sign",0);
    syntax_flags[17] =	in_dict("var_functor_is_apply",0);
    syntax_flags[18] =	in_dict("atom_subscripts",0);
    syntax_flags[19] =	in_dict("general_subscripts",0);
    syntax_flags[20] =	in_dict("curly_args_as_list",0);

    default_syntax_desc.char_class[EOB_MARK] = RE;

    chname_[0] = in_dict("unused",0);
    chname_[UC] = in_dict("upper_case",0);
    chname_[UL] = in_dict("underline",0);
    chname_[LC] = in_dict("lower_case",0);
    chname_[N] = in_dict("digit",0);
    chname_[BS] = in_dict("blank_space",0);
    chname_[NL] = in_dict("end_of_line",0);
    chname_[AQ] = in_dict("atom_quote",0);
    chname_[SQ] = in_dict("string_quote",0);
    chname_[SL] = in_dict("solo",0);
    chname_[DS] = in_dict("special",0);
    chname_[CM] = in_dict("line_comment",0);
    chname_[LQ] = in_dict("list_quote",0);
    chname_[RA] = in_dict("radix",0);
    chname_[AS] = in_dict("ascii",0);
    chname_[TS] = in_dict("terminator",0);
    chname_[ES] = in_dict("escape",0);
    chname_[CM1] = in_dict("first_comment",0);
    chname_[CM2] = in_dict("second_comment",0);
    chname_[SY] = in_dict("symbol",0);
    chname_[NBCH] = in_dict("null",0);

    tname_[NO_TOKEN] =	d_.err;
    tname_[BLANK_SPACE] = d_.err;
    tname_[EOI] =	d_.eof;
    tname_[EOCL] =	in_dict("fullstop", 0);
    tname_[IDENTIFIER] = d_.atom0;
    tname_[QIDENTIFIER] = in_dict("quoted_atom", 0);
    tname_[COMMA] =	in_dict("comma", 0);
    tname_[BAR] =
    tname_[CLOSING_SOLO] =
    tname_[SOLO] =	in_dict("solo", 0);
    tname_[SPACE_NUMBER] = 
    tname_[NUMBER] =	in_dict("number", 0);
    tname_[STRING] =	d_.string0;
    tname_[REFERENCE] =	d_.var0;
    tname_[UREFERENCE] = in_dict("anonymous", 0);
    tname_[LIST] =	in_dict("list", 0);
    tname_[SPACE_SOLO] = in_dict("open_par", 0);

    d_comma0_ = in_dict(",", 0);
#if defined(FUNCTOR_COMPLETION) && defined(HAVE_READLINE)
    {
	extern char ** (*rl_attempted_completion_function)();

	rl_attempted_completion_function = _complete_predicate;
    }
#endif

    if (flags & INIT_SHARED)
    {
	(void) exported_built_in(in_dict("set_chtab_", 3), p_set_chtab, B_SAFE);
	(void) exported_built_in(in_dict("get_chtab_", 3), p_get_chtab, B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("set_syntax_", 3), p_set_syntax, B_SAFE);
	(void) b_built_in(in_dict("get_syntax_", 3), p_get_syntax, d_.kernel_sepia);
	exported_built_in(in_dict("read_token_", 4),	p_read_token_, B_UNSAFE|U_GROUND) -> mode = BoundArg(2, CONSTANT) | BoundArg(3, CONSTANT);
	(void) local_built_in(in_dict("copy_syntax", 2), p_copy_syntax, B_SAFE);
    }
}

#define Set_TokenString(s, l) \
	token->term.val.nint = (word)(l); \
	token->string = (char *) (s);


/* up to three characters of backup are needed, e.g. in "3e+a" */
/* at eoi we don't advance pligne, so don't backup */
#define Backup_(c,n) \
	pligne -= (n) - ((c)==EOI_SYMBOL? 1: 0);

#define EoB(nst) (StreamBuf(nst) + StreamCnt(nst))

#define Get_Chr(c) \
	c = *pligne++; \
	if ((c) == EOB_MARK  &&  pligne > EoB(nst)) { \
	    StreamPtr(nst) = pligne-1; \
	    if (fill_buffer(nst) != PSUCCEED) { \
		pligne = StreamPtr(nst); \
	    	c = EOI_SYMBOL; \
	    } else { \
		pligne = StreamPtr(nst); \
		c = *pligne++; \
	    } \
	}

#define Get_Ch_Class(c,t) \
	Get_Chr(c) \
	t = sd->char_class[c];

#ifdef FUNCTOR_COMPLETION
#define Get_Ch_Class_And_Complete(c,t) \
	c = *pligne++; \
	if ((c) == EOB_MARK  &&  pligne > EoB(nst)) { \
	    if (IsTty(nst) && StreamCnt(nst) != StreamSize(nst) && \
		*(pw - 1) != '\n') { \
		Find_Matching_Atom(pw, nst, pw, stop); \
	    } \
	    StreamPtr(nst) = pligne-1; \
	    if (fill_buffer(nst) != PSUCCEED) { \
		pligne = StreamPtr(nst); \
	    	c = EOI_SYMBOL; \
	    } else { \
		pligne = StreamPtr(nst); \
		c = *pligne++; \
	    } \
	} \
	t = sd->char_class[c];
#else
#define Get_Ch_Class_And_Complete(c,t) \
	Get_Ch_Class(c,t)
#endif


/*
 * Compute the file position of the next/last character read
 */
#define CurrentOffset(nst,pligne) \
	(StreamOffset(nst) + (pligne - StreamBuf(nst)))

#define PreviousOffset(nst,pligne,cc) \
	(CurrentOffset(nst,pligne) - BytesPerChar(cc))

#define BytesPerChar(cc) 1


int
lex_an(	stream_id nst,		/* in: stream to read from */
	syntax_desc *sd,	/* in: syntax descriptor */
	token_desc *token	/* out: token descriptor */
    )				/* returns: token or (negative) error code */
{
    unsigned char	*pligne = StreamPtr(nst);
    unsigned char 	*pw, *stop;
    int			tok = NO_TOKEN;
    int			cc;
    int			quote_char;
    int			ctype;

    token->string = (char *) 0;
    token->pos.file = StreamName(nst);
    token->pos.line = StreamLine(nst);
    token->pos.from = CurrentOffset(nst,pligne);

    if (!pligne) {
    	tok = EOI;
	goto _return_tok_;
    }

    Get_Ch_Class(cc, ctype);		/* read first character */

_start_:		/* cc/ctype: current char, pligne: ptr to next char */
    switch (ctype)
    {
    case RE:
	tok = EOI;
	break;

    case BS:
    case NL:
    case CM:
	tok = _skip_blanks(nst, sd, &pligne, &cc, &ctype);
    	if (LexError(tok))
	    break;
	token->pos.line = StreamLine(nst);
	token->pos.from = PreviousOffset(nst,pligne,cc);
	goto _start_;		/* tok maybe BLANK_SPACE */

    case CM1:
	tok = _skip_blanks(nst, sd, &pligne, &cc, &ctype);
    	if (LexError(tok))
	    break;
	else if (tok == BLANK_SPACE) {
	    token->pos.line = StreamLine(nst);
	    token->pos.from = PreviousOffset(nst,pligne,cc);
	    goto _start_;	/* it was really a comment */
	} else
	    goto _symbol_;	/* was no comment, treat CM1 as symbol */

    case AQ:
	tok = QIDENTIFIER;
	goto _quote_;
    case SQ:
	tok = STRING;
	goto _quote_;
    case LQ:
	tok = LIST;
_quote_:
	quote_char = cc;
	pw = StreamLexAux(nst);
	stop = pw + StreamLexSize(nst);
	Get_Ch_Class(cc, ctype);
	for(;;)
	{
	    int base, iresult, max_no, max_lc, max_uc;

	    switch (ctype)
	    {
	    case RE:
		Set_TokenString(StreamLexAux(nst), pw - StreamLexAux(nst));
		tok = ENDOFFILE;
		goto _return_tok_;

	    case AQ:
	    case LQ:
	    case SQ:
		if (cc != quote_char) {
		    *pw++ = cc;
		    break;		/* other quote within */
		}
		if (sd->options & DOUBLED_QUOTE_IS_QUOTE)
		{
		    Get_Chr(cc);
		    if (cc == quote_char)
		    {
			*pw++ = cc;
			break;
		    }
		    Backup_(cc, 1);
		}
		if (ctype != AQ)	/* check for consecutive strings */
		{
		    Get_Ch_Class(cc,ctype);
		    if (LexError(_skip_blanks(nst, sd, &pligne, &cc, &ctype))) {
			Set_TokenString(StreamLexAux(nst), pw - StreamLexAux(nst));
			tok = ENDOFFILE;
			goto _return_tok_;
		    }
		    if (cc == quote_char)
		    	break;		/* skip doubled quote */
		    Backup_(cc, 1);
		}
		*pw = 0;		/* end of quoted item */
		Set_TokenString(StreamLexAux(nst), pw - StreamLexAux(nst));
		goto _return_tok_;

	    case ES: 	/* escape character: interpret next */
		Get_Ch_Class(cc,ctype);
		switch (ctype)
		{
		case RE:
		    Set_TokenString(StreamLexAux(nst), pw - StreamLexAux(nst));
		    tok = ENDOFFILE;
		    goto _return_tok_;

		case BS:	 /* ignore escaped line end \r\n */
		    if (cc != '\r')
			goto _return_ill_quoted_;
		    Get_Ch_Class(cc, ctype);	/* next character */
		    if (ctype != NL)
			goto _return_ill_quoted_;
		    /* fall through */

		case NL:	/* ignore escaped line end \n */
		    StreamLine(nst)++;
		    /* don't call Get_Ch_Class_And_Complete() because
		     * nothing was put in the lex_aux buffer */
		    Get_Ch_Class(cc, ctype);	/* next character */
		    continue;

		case ES:
		case AQ:
		case LQ:
		case SQ:
		    *pw++ = cc;		/* just take cc as it is */
		    break;

		case LC:
		    switch (cc)
		    {
		    case 'a': *pw++ = 0007; break;	/* alert */
		    case 'b': *pw++ = '\b'; break;	/* backspace */
		    case 't': *pw++ = '\t'; break;	/* tab */
		    case 'n': *pw++ = '\n'; break;	/* newline */
		    case 'v': *pw++ = 0013; break;	/* vertical tab */
		    case 'f': *pw++ = '\f'; break;	/* form feed */
		    case 'r': *pw++ = '\r'; break;	/* return */
		    case 'e': *pw++ = 0033; break;	/* escape */
		    case 'd': *pw++ = 0177; break;	/* delete */

		    case 'c':		/* Quintus/Sicstus feature */
			do {
			    Get_Ch_Class(cc, ctype);
			} while (ctype == BS || ctype == NL);
			continue;

		    case 'x':
			base = 16; max_no = '9'; max_lc = 'f'; max_uc = 'F';
			Get_Ch_Class(cc, ctype)
_iso_numeric_escape_:
			if (!(cc>='0' && cc<=max_no || cc>='a' && cc<=max_lc || cc>='A' && cc<=max_uc))
			    goto _return_ill_quoted_;
			for (iresult=0;;) {
			    if (cc>='0' && cc<=max_no) cc -= '0';
			    else if (cc>='a' && cc<=max_lc) cc = cc - 'a' + 10;
			    else if (cc>='A' && cc<=max_uc) cc = cc - 'A' + 10;
			    else if (ctype == ES)
				break;
			    else
				goto _return_ill_quoted_;
			    if ((unsigned )iresult <= MAX_CHAR_CODE/base &&
				    ((unsigned ) (iresult * base) <= MAX_CHAR_CODE - cc))
				iresult = iresult * base + cc;
			    else goto _return_ill_quoted_;	/* overflow */
			    Get_Ch_Class(cc, ctype)
			}
			*pw++ = iresult;
			break;

		    default: goto _return_ill_quoted_;
		    }
		    break;

		case N:
		    if (sd->options & ISO_ESCAPES)
		    {
			/* variable length, require terminating \ */
			base = 8; max_no = '7'; max_lc = 0; max_uc = 0;
		    	goto _iso_numeric_escape_;
		    }
		    switch (cc)
		    {
		    case '0':
		    case '1':
		    case '2':
		    case '3':	/* check for 3 octal digits */
			{
			    int val = cc - '0';
			    Get_Chr(cc);			/* second */
			    if (!octal(cc)) {
				goto _return_ill_quoted_;
			    }
			    val = (val << 3) + (cc - '0');
			    Get_Chr(cc);			/* third */
			    if (!octal(cc)) {
				goto _return_ill_quoted_;
			    }
			    val = (val << 3) + (cc - '0');
			    *pw++ = val;
			    break;
			}
		    default: goto _return_ill_quoted_;
		    }
		    break;

		default:
		    goto _return_ill_quoted_;
		}
		break;

	    case NL:
		StreamLine(nst)++;
		if (!(sd->options & NEWLINE_IN_QUOTES))
		    goto _return_ill_quoted_;
		/* fall through */

	    default:
		*pw++ = cc;
		break;
	    }
	    Get_Ch_Class_And_Complete(cc, ctype);	/* next character */
	    if (pw == stop) {
		Extend_Lex_Aux(nst, pw, stop);
	    }
	} /* end for(;;) */


    case UC:	/* uppercase */
    	tok = REFERENCE;
	goto _name_;
    case UL:	/* special prefix for variables */
    	tok = UREFERENCE;
	goto _name_;
    case LC:	/* a lower case symbol */
    	tok = IDENTIFIER;
_name_:
	pw = StreamLexAux(nst);
	stop = pw + StreamLexSize(nst);
	*pw++ = cc;
	Get_Ch_Class(cc, ctype);
	for(;;)
	{
	    if (Alphanum(ctype))
	    {
		if (pw == stop) {
		    Extend_Lex_Aux(nst, pw, stop);
		}
		*pw++ = cc;
		Get_Ch_Class_And_Complete(cc, ctype);
	    } else {
	    	break;
	    }
	}
	Backup_(cc, 1);
	*pw = 0;
	Set_TokenString(StreamLexAux(nst), pw - StreamLexAux(nst));
	if (tok == UREFERENCE && (pw - StreamLexAux(nst)) > 1)
	    tok = REFERENCE;
	break;


    case SY:
    case ES:
    case CM2:
_symbol_:
	pw = StreamLexAux(nst);
	stop = pw + StreamLexSize(nst);
	*pw++ = cc;
	Get_Ch_Class(cc, ctype);
	for(;;)
	{
	    if (Symbol(ctype) && ctype != RE)
	    {
		if (pw == stop) {
		    Extend_Lex_Aux(nst, pw, stop);
		}
		*pw++ = cc;
		Get_Ch_Class_And_Complete(cc, ctype);
	    } else {
	    	break;
	    }
	}
	if ((pw - StreamLexAux(nst)) == 1  &&  *StreamLexAux(nst) == '.'
	    && (ctype == BS || ctype == NL || ctype == RE || ctype == CM))
	{
	    if (ctype == RE || ctype == CM)
	    {
		Backup_(cc, 1);
	    }
	    else if (ctype == NL)
	    {
		StreamLine(nst)++;
	    }
	    Make_Atom(&token->term, d_.eocl);
	    tok = EOCL;				/* full stop */
	} else {
	    Backup_(cc, 1);
	    *pw = 0;
	    Set_TokenString(StreamLexAux(nst), pw - StreamLexAux(nst));
	    tok = IDENTIFIER;
	}
	break;


    case AS:				/* ascii-quote */
	Get_Ch_Class(cc,ctype);
	if (ctype ==  RE)
	    goto _start_;
	if (ctype == NL)
	    StreamLine(nst)++;
	Make_Integer(&token->term, cc);
	tok = NUMBER;
	break;


    case N:
	pligne = string_to_number((char *) pligne - 1, &token->term, nst, sd->options);
	tok = token->term.tag.kernel == TEND ? BAD_NUMERIC_CONSTANT :
		tok == BLANK_SPACE ? SPACE_NUMBER :
		NUMBER;
	goto _return_tok_;


    case DS:
	switch(cc)
	{
	case  '(':
	case  '[':
	case  '{':
	    Make_Integer(&token->term, cc);
	    tok = (tok == BLANK_SPACE) ? SPACE_SOLO : SOLO;
	    break;

	case  ',':
	    Make_Atom(&token->term, d_comma0_);
	    tok = COMMA;
	    break;

	case  '|':
	    StreamLexAux(nst)[0] = cc;
	    StreamLexAux(nst)[1] = 0;
	    Set_TokenString(StreamLexAux(nst), 1);
	    tok = BAR;
	    break;

	case  ')':
	case  ']':
	case  '}':
	    Make_Integer(&token->term, cc);
	    tok = CLOSING_SOLO;
	    break;

	default:
	    Make_Integer(&token->term, cc);
	    tok = SOLO;
	    break;
	}
	break;


    case TS:		/* terminator character (non-Prolog extension) */
	Make_Atom(&token->term, d_.eocl);
	tok = EOCL;				/* full stop */
	break;

    case SL:		/* like SY, but every character is its own token */
	StreamLexAux(nst)[0] = cc;
	StreamLexAux(nst)[1] = 0;
	Set_TokenString(StreamLexAux(nst), 1);
	tok = IDENTIFIER;
	break;


    default:
	Make_Integer(&token->term, cc);
	tok = SOLO;
	break;

    } /* end switch */

_return_tok_:
    StreamPtr(nst) = pligne;
    token->pos.to = CurrentOffset(nst,pligne);
    token->class = tok;
    return tok;

_return_ill_quoted_:
    Set_TokenString(StreamLexAux(nst), pw - StreamLexAux(nst));
    tok = ILL_QUOTED;
    goto _return_tok_;
}


/*
 * Return the next non-blank and non-comment character.
 * pligne, cc, ctype are maintained as in lex_an()
 *
 * Return values:
 *	NO_TOKEN	no blank space was skipped
 *	BLANK_SPACE	some blank space was skipped
 *	ENDOFFILE	error (cc,ctype not updated)
 */
static int
_skip_blanks(stream_id nst, syntax_desc *sd, unsigned char **p_pligne, int *p_cc, int *p_ctype)
{
    unsigned char	*pligne = *p_pligne;
    int			ret = NO_TOKEN;
    int			cc = *p_cc;
    int			ctype = *p_ctype;
    int			cc2, ctype2, depth;

    for(;;)
    {
	switch (ctype)
	{
	case NL:
	    StreamLine(nst)++;
	    /* fall through */
	case BS:
	    ret = BLANK_SPACE;
	    break;

	case CM:			/* comment until end of line */
	    ret = BLANK_SPACE;
	    do {
		Get_Ch_Class(cc,ctype);
	    } while (ctype != NL  &&  ctype != RE);
	    continue;

	case CM1:			/* C-style comment */
	    Get_Ch_Class(cc2,ctype2);	/* lookahead */
	    if (ctype2 == CM2)	
	    {
		ret = BLANK_SPACE;		/* it's definitely a comment */
		Get_Ch_Class(cc,ctype);
		for (depth = 1; depth > 0; )
		{
		    switch (ctype)
		    {
		    case RE:		/* EOF within comment not allowed */
			*p_pligne = pligne;
			return ENDOFFILE;

		    case NL:		/* don't forget to count lines */
			StreamLine(nst)++;
			break;

		    case CM1:		/* possible nested comment */
			if (sd->options & NESTED_COMMENTS)
			{
			    Get_Ch_Class(cc,ctype);
			    if (ctype != CM2)
				continue;
			    depth++;
			}
			break;

		    case CM2:		/* possible end of comment */
			Get_Ch_Class(cc,ctype);
			if (ctype != CM1)
			    continue;
			depth--;
			break;
		    }
		    Get_Ch_Class(cc,ctype);
		}
		continue;		/* end of comment */
	    }
	    Backup_(cc2, 1);
	    /* no comment, fall through */

	default:
	    *p_cc = cc;
	    *p_ctype = ctype;
	    *p_pligne = pligne;
	    return ret;
	}
	Get_Ch_Class(cc,ctype);
    }
}


/*
 * Check if an atom needs to be quoted. This is called from the
 * write_atom() routine. Return values:
 *	IDENTIFIER - no quotes needed
 *	QIDENTIFIER - quotes needed
 *	BAR - may need quotes
 *	DOT - may need quotes
 *	COMMA - may need quotes
 */
int
ec_need_quotes(dident d, syntax_desc *sd)
{
    register unsigned char	*name = (unsigned char *) DidName(d);
    register int		rest = (int) DidLength(d);
    register int		c;

    if (rest-- == 0)
	return QIDENTIFIER;

    switch (sd->char_class[c = *name++])
    {
    case LC:			/* atoms starting with lower case	*/
	while (rest--)
	{
	    c = *name++;
	    if (!Alphanum(sd->char_class[c]))
		return QIDENTIFIER;
	}
	return IDENTIFIER;

    case SY:			/* symbol atoms: . may need quotes	*/
	if (c == '.' && rest == 0)
	    return EOCL;
	/* else fall through */
    case ES:
    case CM2:
	while (rest--)
	{
	    c = *name++;
_need_quotes1_:
	    switch (sd->char_class[c])
	    {
	    case CM1:
	    case CM2:
	    case ES:
	    case SY:
		break;
	    default:
		return QIDENTIFIER;
	    }
	}
	return IDENTIFIER;

    case CM1:			/* begin of comment must be quoted	*/
	if (rest--)
	{
	    c = *name++;
	    if (sd->char_class[c] == CM2)
		return QIDENTIFIER;
	    else
		goto _need_quotes1_;
	}
	else return IDENTIFIER;

    case DS:
	switch (c)
	{
	case '{':		/* {} needs no quotes	*/
	    if (rest == 1 && *name == '}')
		return IDENTIFIER;
	    else return QIDENTIFIER;
	case '[':		/* [] needs no quotes	*/
	    if (rest == 1 && *name == ']')
		return IDENTIFIER;
	    else return QIDENTIFIER;
	case ',':		/* ,/2 sometimes needs no quotes	*/
	    if (d == d_.comma)
		return COMMA;
	    else return QIDENTIFIER;
	case '|':		/* | needs quotes ony inside lists	*/
	    if (rest == 0  &&  !(sd->options & BAR_IS_NO_ATOM))
		return BAR;
	    else return QIDENTIFIER;
	default:
	    return QIDENTIFIER;
	}

    case SL:
	if (rest == 0)
	    return IDENTIFIER;	/* ! and ; don't need quotes	*/
	else return QIDENTIFIER;

    case TS:
	return QIDENTIFIER;
    }
    return QIDENTIFIER;
}


static unsigned char *
_extend_lex_aux(stream_id nst)
{
    register long		n = StreamLexSize(nst);

    StreamLexAux(nst) = (unsigned char *) hg_resize((generic_ptr) (StreamLexAux(nst)), (int)(n + n));
    StreamLexSize(nst) = n + n;
    return StreamLexAux(nst) + n;
}

#ifdef FUNCTOR_COMPLETION
#if defined(HAVE_READLINE)
/*ARGSUSED*/
static char **
_complete_predicate(char *text, int start, int end)
{
    char		**matches;
    register char	*s;
    int			i = 0;
    extern char		**completion_matches();
    extern char		*rl_line_buffer;

    matches = (char **)NULL;

    /* strip spaces so that we know if we match a predicate or not */
    for (s = rl_line_buffer; (*s == ' ' || *s == '\t'); s++)
	i++;
    if (i == start)
	completion_start = 0;
    else {
	completion_start = start;
	for (s = rl_line_buffer + start - 1; s >= rl_line_buffer; s--) {
	    if (*s == '[')
		return matches;		/* matching filenames */
	    else if (*s != ' ' && *s != '\t' && *s != '\'' && *s != '"')
		break;
	}
    }
    matches = completion_matches(text, _find_matching_predicate);
    return matches;
}

static char *
_find_matching_predicate(char *string, int state)
{
    char		*s1, *s2;
    extern char		*sprintf();
    int			search_arity;
    int			length;

    if (state == 0) {
	completion_idx = 0;
	completion_length = strlen(string);
    }
    if (string[completion_length - 1] == '/') {
	/* We have the whole name and search for arity only */
	search_arity = 1;
	length = completion_length - 1;
    } else {
	search_arity = 0;
	length = completion_length;
    }
    while (next_functor(&completion_idx, &completion_dip))
    {
	if (s1 = DidName(completion_dip))
	{
	    if (strncmp(string, s1, length) == 0 &&
		((!search_arity && completion_start) ||
		    visible_procedure(completion_dip,
			d_.default_module, tdict, PRI_DONTIMPORT)))
	    {
		if (search_arity) {
		    if (strlen(s1) == length) {
			s2 = (char *) hp_alloc(length + 4);
			(void) strcpy(s2, s1);
			s2[length] = '/';
			(void) sprintf(s2 + length + 1,
			    "%d", DidArity(completion_dip));
			return s2;
		    }
		} else {
		    s2 = (char *) hp_alloc(strlen(s1) + 1);
		    (void) strcpy(s2, s1);
		    return s2;
		}
	    }
	}
    }
    Set_Bip_Error(0);
    return (char *) 0;
}
#else

static int
_prefix_length(char *s1, char *s2)
{
    register char	*p = s1;

    while (*s1++ == *s2++)
	;
    return (s1 - p) - 1;
}

static int
_complete(unsigned char *string, unsigned char *end, char **out)
{
    int				length = 0;
    register unsigned char	*s1;
    register unsigned char	*s2;
    char			*found_string;
    int				match;
    int				idx = 0;
    dident			dip;

    while (next_functor(&idx, &dip))
    {
	if (s1 = (unsigned char *) DidName(dip))
	{
	    s2 = string;
	    while (s2 < end)
		if (*s2 != *s1++)
		    break;
		else
		    s2++;
	    if (s2 == end)		/* Found one */
	    {
		if (!length)
		{
		    found_string = DidName(dip);
		    length = strlen(found_string);
		}
		else if (DidName(dip) != found_string)
		{
		    match = _prefix_length(DidName(dip), found_string);
		    if (match < length)
			length = match;
		}
	    }
	}
    }
    if (length > end - string) {
	    *out = found_string + (end - string);
	    return length - (end - string);
    }
    else
	return 0;
}

/*ARGSUSED*/
static void
_find_matching_atom(
	unsigned char	*end,
	stream_id	nst,
	unsigned char	**pw,
	unsigned char	**stop)
{
    char	*p;
    int		sl;
    
    if (sl = _complete(StreamLexAux(nst), end, &p))
    {
#ifdef HAVE_PUSHBACK
	while (sl--)
	    pushback_char((int) (StreamUnit(nst)), p++);
#else
	(void) write((int) (StreamUnit(nst)), p, sl);
	if (*pw + sl >= *stop) {
	    Extend_Lex_Aux(nst, *pw, *stop);
	}
	while (sl--)
	    *(*pw)++ = *p++;
#endif
    }
    else
	(void) write((int) (StreamUnit(nst)), "\007", 1);
    StreamMode(nst) |= DONT_PROMPT;
}
#endif
#endif


syntax_desc *
copy_syntax_desc(syntax_desc *sd)
{
    syntax_desc		*newsd;

    newsd = (syntax_desc *) hg_alloc_size(sizeof(syntax_desc));
    *newsd = *sd;
    return newsd;
}

/*ARGSUSED*/
static int
p_copy_syntax(value vfrom, type tfrom, value vto, type tto)
{
    module_item		*from, *to;

    from = ModuleItem(vfrom.did);
    to = ModuleItem(vto.did);

    hg_free_size((generic_ptr) to->syntax, sizeof(syntax_desc));
    to->syntax = copy_syntax_desc(from->syntax);
    Succeed_;
}

/*
 * get_chtab_(+Character, ?CharacterClass, Module)
 */
static int
p_get_chtab(value v1, type t1, value v2, type t2, value vm, type tm)
{
    Check_Integer(t1);
    Check_Output_Atom(t2);
    Check_Module_And_Access(vm, tm)
    if (v1.nint < 0 || v1.nint > 255)
    {
	Bip_Error(RANGE_ERROR);
    }
    Return_Unify_Atom(v2,t2,(dident)chname_[ModuleSyntax(vm.did)->char_class[(unsigned char)v1.nint]]);
}

/*
 * set_chtab_(+Character, +CharacterClass, Module)
 */
static int
p_set_chtab(value v1, type t1, value v2, type t2, value vm, type tm)
{
    unsigned char	c;	/* to hold the concerned character */
    int			new_cc;
    syntax_desc		*sd;

    Check_Integer(t1);
    Check_Atom(t2);
    Check_Module_And_Access(vm, tm)
    if (v1.nint < 0 || v1.nint > 255)
    {
	Bip_Error(RANGE_ERROR);
    }
    c = (unsigned char) v1.nint;
    sd = ModuleSyntax(vm.did);

    /* Then try to find the character class among the known ones */
    for(new_cc = 1; new_cc <= NBCH && v2.did != chname_[new_cc]; new_cc++)
	;
    if (new_cc > NBCH) { Bip_Error(RANGE_ERROR) }	/* Not found */

    /* Check if we are redefining the current AQ, SQ or ES character.
     * For writing, we always need an AQ and SQ, hence they may only be
     * redefined if there is an alternative one that can be used instead.
     * Having no ES character is allowed.
     */
    if ((unsigned char)new_cc != sd->char_class[c]
     && (sd->current_sq_char == c || sd->current_aq_char == c ||
	 sd->current_escape == c  || sd->current_ul_char == c))
    {	int j;
	unsigned char cc = sd->char_class[c];

	for(j = 0; j <= 255; j++)	/* scan through all characters */
	{
	    if (sd->char_class[j] == cc  &&  (int)c != j)
	    {
		switch(cc)		/* found an alternative character j */
		{
		case AQ:	sd->current_aq_char = j; break;
		case SQ:	sd->current_sq_char = j; break;
		case UL:	sd->current_ul_char = j; break;
		case ES:	sd->current_escape = j; break;
		}
		break;
	    }
	}
	if (j > 255)
	    if (cc == ES)
		sd->current_escape = -1; /* no longer an ES character	*/
	    else
	    {
		Bip_Error(ONE_SQ_AQ)	/* these quotes are needed	*/
	    }
    }

    sd->char_class[c] = (unsigned char) new_cc; /* now redefine the character */
    switch(new_cc)			/* might be the new current_...	*/
    {
    case AQ:    sd->current_aq_char = c; break;
    case SQ:    sd->current_sq_char = c; break;
    case ES:    sd->current_escape = c; break;
    case UL:    sd->current_ul_char = c; break;
    }
    Succeed_;
}


/*
 * get_syntax_(?Flag, Remember, Module) - backtrack over syntax flags
 *
 * internal use only !
 */
/*ARGSUSED*/
static int
p_get_syntax(value val1, type tag1, value val2, type tag2, value vm, type tm)
{
    value	vi;
    int		syntax;

    /* no check on tag1 ! */
    /* Check_Integer(tag2); not needed */

    syntax = ModuleSyntax(vm.did)->options;
    vi.nint = val2.nint;
    while (vi.nint < SYNTAX_FLAGS)
    {
	if (syntax & (1 << vi.nint++))
	{
	    Remember(2, vi, tag2);
	    Return_Unify_Atom(val1, tag1, syntax_flags[vi.nint-1]);
	}
    }
    Cut_External;
    Fail_;
}

/*	read_token_(Stream, Token, Class, Module)	*/
/*ARGSUSED*/
static int
p_read_token_(value vs, type ts, value v, type t, value vc, type tc, value vm, type tm)
{
    int		res;
    char	*s;
    token_desc	token;
    stream_id	nst = get_stream_id(vs,ts, SREAD, &res);
    register long len;
    syntax_desc	*sd = ModuleSyntax(vm.did);
    dident	tname;
    Prepare_Requests;

    if (!IsRef(t) && IsCompound(t))
    {
	Bip_Error(TYPE_ERROR)
    }
    if (nst == NO_STREAM)
    {
	Bip_Error(res)
    }
    if (!IsReadStream(nst))
    {
	Bip_Error(STREAM_MODE);
    }
    Check_Module_And_Access(vm, tm)
    if (StreamMode(nst) & REPROMPT_ONLY)
	StreamMode(nst) |= DONT_PROMPT;

    (void) lex_an(nst, sd, &token);
    tname = LexError(token.class) ? d_.err : tname_[token.class];
    switch(token.class)
    {
	case COMMA:
	case EOCL:
	    break;

	case REFERENCE:
	case UREFERENCE:
	case STRING:
	case LIST:
	case BAR:
	default:		/* LexError() */
	    len = token.term.val.nint;
	    Make_Stack_String(len, token.term.val, s)
	    Copy_Bytes(s, token.string, len + 1);
 	    token.term.tag.kernel = TSTRG;
            break;

	case ENDOFFILE:		/* we don't have the string */
	    Make_Stack_String(0, token.term.val, s)
	    *s = 0;
 	    token.term.tag.kernel = TSTRG;
            break;

	case SOLO:
	case CLOSING_SOLO:
	case SPACE_SOLO:
	{
	    char c = (char) token.term.val.nint;
	    Make_Stack_String(1, token.term.val, s)
	    s[0] = c;
	    s[1] = 0;
 	    token.term.tag.kernel = TSTRG;
            break;
	}

	case NUMBER:
	case SPACE_NUMBER:
	    if (IsInterval(token.term.tag))
	    {
		Unmark_Interval_Raw(token.term.val.ptr);
	    }
	    tname = tag_desc[tag_desc[TagType(token.term.tag)].super].type_name;
	    break;

	case IDENTIFIER:
	case QIDENTIFIER:
	    token.term.val.did = enter_dict_n(token.string, token.term.val.nint, 0);
	    token.term.tag.kernel = token.term.val.did == d_.nil ? TNIL : TDICT;
	    break;

	case EOI:
	    if (StreamMode(nst) & MEOF ) {
		Bip_Error(IsSoftEofStream(nst) ? PEOF : READ_PAST_EOF);
	    }
	    else
		StreamMode(nst) |= MEOF;
	    Bip_Error(PEOF);
    }
    Request_Unify_Pw(v, t, token.term.val, token.term.tag);
    Request_Unify_Atom(vc, tc, tname);
    Return_Unify;
}


/*** the subsequent BIPs fail on error and set the global variable ***/

#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

/*
 * set_syntax(+flag, +val) - set or reset a syntax flag, fails on error
 *
 * internal use only !
 */
/*ARGSUSED*/
static int
p_set_syntax(value val1, type tag1, value val2, type tag2, value vm, type tm)
{
    int		i, flag;
    syntax_desc	*sd;

    Check_Atom(tag1);
    Check_Atom(tag2);
    sd = ModuleSyntax(vm.did);

    for (i=0, flag=1; i < SYNTAX_FLAGS; i++, flag <<= 1)
    {
	if (val1.did == syntax_flags[i])
	{
	    if (val2.did == d_.on)
		sd->options |= flag;
	    else if (val2.did == d_.off)
		sd->options &= ~flag;
	    else { Bip_Error(RANGE_ERROR); }
	    Succeed_;
	}
    }
    Bip_Error(RANGE_ERROR);
}


#ifdef HAVE_INFINITY
extern double infinity();
#else
#ifdef HUGE_VAL
#define infinity() HUGE_VAL
#else
#ifdef HUGE
#define infinity() HUGE
#else
#define infinity() 1.0e310
#endif
#endif
#endif

/*
 * char *string_to_number(start, result, nst)
 *
 * 	Auxiliary function used to convert a string (pointed to by start)
 *	to a number. The result is a prolog word in *result
 *	and is either a TINT, TBIG, TRAT, TDBL or TIVL.
 *	If the tag is TEND there has been a conversion error.
 *	The return value is the pointer to the next character after
 *	the number.
 *	The function can be used both for parsing from a stream (nst)
 *	or for parsing a string (when nst == NULL).
 *	StreamPtr is updated according to the return value.
 *	This function is independent of character classes.
 *	For better backward compatibility, based integers are 
 *	not parsed as bignums (otherwise 16'ffffffff would be a bignum),
 *	unless the based_bignums syntax option is active.
 */

#define Init_S2N() \
    if (nst) { \
	aux = StreamLexAux(nst); \
	stop = StreamLexAux(nst) + StreamLexSize(nst); \
    }

#define Reset_Start() \
	if (nst) aux = StreamLexAux(nst); \
	else start = (char *) t;

/* up to three characters of backup are needed, e.g. in "3e+a" */
#define Push_Back() \
	--t; \
	if (nst) { \
	    *(--aux) = 0; \
	}

#define Get_Ch(c) \
	c = *t++; \
	if (nst) { \
	    if (!c) { \
		StreamPtr(nst) = t-1; \
		(void) fill_buffer(nst); \
		t = StreamPtr(nst); \
		c = *t++; \
	    } \
	    if (aux == stop) { Extend_Lex_Aux(nst, aux, stop) } \
	    *aux++ = c; \
	}

#define NEG	1
#define BIG	2
#define FLOAT	4
#define IVL	8
#define PRECISE	16

char *
string_to_number(char *start, pword *result, stream_id nst, int syntax)
{
    unsigned register char *t;		/* next character to read */
    unsigned register char *aux;	/* next location in LexAux */
    unsigned char *stop;		/* end of LexAux */
    register int c;			/* current character */
    int	flags = 0;			/* to remember established facts */
    int base = 10;			/* radix for number reading */
    register word iresult = 0;		/* accumulator for integer value */
    double f, low_f;			/* the float result */
    int float_digits = 0;

    Init_S2N();
    t = (unsigned char *) start;

_start_:
    Get_Ch(c)
    if (c == '-') {		/* check for optional sign */
	flags |= NEG;
	Get_Ch(c)
    } else if (c == '+') {
	Get_Ch(c)
    }

    if (!isdigit(c))		/* read digits */
	goto return_err;	/* can't happen in the lexer */

    do {
	++float_digits;
	if (!(flags & BIG))
	{
	    c -= '0';
	    if (iresult <= MAX_S_WORD/10 && ((iresult *= 10) <= MAX_S_WORD - c))
		iresult += c;
	    else flags |= BIG;	/* 32 bit overflow */
	}
	Get_Ch(c)
    } while (isdigit(c));

    if (c == '\'') {			/* based integer */
	if ((flags & BIG) || iresult < 0 || iresult > 36)
	{
	    goto return_int;
	}
	base = iresult;
	if (base == 0)			/* ascii */
	{
	    Get_Ch(c)
	    result->val.nint = (long) c;
	    result->tag.kernel = TINT;
	    goto return_ok;
	}
	if (syntax & ISO_BASE_PREFIX)
	    goto return_int;	/* (flags, iresult) */

_based_number_:				/* (base,iresult) */
	{
	    int max_no = base < 10 ? '0'+base-1 : '9';
	    int max_lc = 'a' + base-11;
	    int max_uc = 'A' + base-11;
	    Reset_Start()
	    Get_Ch(c)
	    if (!(c>='0' && c<=max_no || c>='a' && c<=max_lc || c>='A' && c<=max_uc))
	    {
		Push_Back();		/* the bad digit */
		goto return_int;	/* (flags, iresult) */
	    }
	    for (iresult=0;;) {
		if (c>='0' && c<=max_no) c -= '0';
		else if (c>='a' && c<=max_lc) c = c - 'a' + 10;
		else if (c>='A' && c<=max_uc) c = c - 'A' + 10;
		else break;
		if (!(flags & BIG))
		{
		    if ((uword)iresult <= MAX_U_WORD/base &&
			    (((uword)iresult * base) <= MAX_U_WORD - c))
			iresult = iresult * base + c;
		    else flags |= BIG;	/* 32 bit overflow */
		}
		Get_Ch(c)
	    }
	    if (syntax & BASED_BIGNUMS)
	    {
		if (!(flags & BIG) && (uword)iresult > MAX_S_WORD)
		    flags |= BIG;
	    }
	    else if (flags & BIG) {
		Push_Back();		/* the delimiter */
		goto return_err;
	    }
	    goto return_int;
	}
    }
    else if(c == '.')			/* could be a float */
    {
	int first;
	Get_Ch(c)			/* first after point */
	if (!isdigit(c))
	{
	    Push_Back();		/* the non-digit */
            goto return_int;		/* it was no decimal point */
	}
	++float_digits;
	flags |= FLOAT;			/* definitely a float */
	first = c;
	Get_Ch(c)
	if (!isdigit(c)) {		/* only one fractional digit */
	    if (first == '0' || first == '5')
		flags |= PRECISE;
	} else {
	    do {
		++float_digits;
		Get_Ch(c)		/* read remaining digits */
	    } while (isdigit(c));
	}
	if (c == 'e' || c == 'E')	/* exponent is now optional */
	    flags &= ~PRECISE;		/* conservative assumption */
	else if (c == 'I')		/* check for Inf */
	{
	    Get_Ch(c)
	    if (c == 'n')
	    {
		Get_Ch(c)
		if (c == 'f') goto return_infinity;
		Push_Back();		/* the f position*/
	    }
	    Push_Back();		/* the n position*/
	    goto return_real;
	}
	else				/* no exponent */
	    goto return_real;
	/* go read exponent */ 
    }
    else if (c == 'e' || c == 'E')
    	;
    else if (c == '_')			/* could be a rational */
    {
	Get_Ch(c)
	if (!isdigit(c))
	{
	    Push_Back();		/* the non-digit */
	    goto return_int;		/* just an integer */
	}
	do {				/* definitely a rational */
	    Get_Ch(c)
	} while (isdigit(c));
	goto return_rat;
    }
    else if (syntax & ISO_BASE_PREFIX)
    {
	switch (c) {
	case 'b': base =  2; goto _based_number_; /* (base,iresult) */
	case 'o': base =  8; goto _based_number_; /* (base,iresult) */
	case 'x': base = 16; goto _based_number_; /* (base,iresult) */
	}
	goto return_int;		/* integer or bignum */
    }
    else
	goto return_int;		/* integer or bignum */

    Get_Ch(c)				/* read exponent */
    if (c == '-' || c == '+')		/* optional exponent sign */
    {
	Get_Ch(c)
	if (!isdigit(c))
	{
	    Push_Back();		/* the non-digit */
	    Push_Back();		/* the sign */
	    if (flags & FLOAT)
		goto return_real;
	    else
		goto return_int;
	}
    }
    else if (!isdigit(c))		/* one or more digits */
    {
	Push_Back();			/* the non-digit */
	if (flags & FLOAT)
	    goto return_real;
	else
	    goto return_int;
    }
    /* flags |= FLOAT;			definitely a float */
    do {
	Get_Ch(c)
    } while (isdigit(c));

return_real:				/* we have a valid real */
    Push_Back();			/* pushback the delimiter */
    if (nst) start = (char *) StreamLexAux(nst);
    f = atof(start);
#ifdef ATOF_NEGZERO_BUG
    /* some versions of atof() don't properly create negative zeros */
    if (f == 0.0  &&  1.0/f > 0.0  &&  flags & NEG) f = -f;
#endif

return_f:				/* f */

    if (flags & IVL)			/* second half of interval? */
    {
	if (!GoodFloat(f) || !GoodFloat(low_f))
	    goto return_err;
	/*
	 * When called from the lexer we allow to return an illformed (raw)
	 * interval (lwb > upb) because we don't see a possibly leading
	 * minus sign!
	 */
	if (!nst && low_f > f)
	    goto return_err;
	Make_Interval(result, low_f, f);
	if (nst)
	{
	    /* this flag is used in _ivl_chgsign() and
	     * reset in the parser or in read_token */
	    Mark_Interval_Raw(result->val.ptr);
	}
	goto return_ok;
    }

    Get_Ch(c)				/* check for float interval separator */
    if (c == '_')
    {
	Get_Ch(c)
	if (c == '_')
	{
	    low_f = f;
	    Reset_Start()
	    flags = IVL;
	    goto _start_;		/* go read the second float */
	}
	Push_Back();			/* the non-underscore */
    }
    Push_Back();

    if (syntax & FLOATS_AS_BREALS)
    {
	if (!GoodFloat(f))
	{
	    goto return_err;
	}
	if (!(flags & PRECISE) || float_digits > 15)
	{
	    low_f = ec_ieee_down(f);
	    f = ec_ieee_up(f);
	}
	else
	{
	    low_f = f;
	}
	Make_Interval(result, low_f, f);
    }
    else
    {
	/* conversion error in atof() ? */
	if (!GoodFloat(f))
	{
	    goto return_err;
	}
	Make_Double(result, f)
    }
    
    goto return_ok;

return_infinity:			/* we have an infinity */
    f = flags & NEG ? -infinity() : infinity();
    goto return_f;

return_rat:				/* (start, base) */
    Push_Back();			/* pushback the delimiter */
    if (flags & IVL) goto return_err;
    if (nst) start = (char *) StreamLexAux(nst);
    if (tag_desc[TRAT].from_string(start, result, base) != PSUCCEED)
	goto return_err;
    goto return_ok;

return_int:				/* (flags, iresult, start, base) */
    Push_Back();			/* pushback the delimiter */
    if (flags & IVL) goto return_err;
    if (flags & BIG)
    {
	if (nst) start = (char *) StreamLexAux(nst);
	if (tag_desc[TBIG].from_string(start, result, base) != PSUCCEED)
	    goto return_err;
    }
    else	/* integer */
    {
	result->val.nint = flags & NEG ? -iresult : iresult;
	result->tag.kernel = TINT;
    }

return_ok:
    if (nst) StreamPtr(nst) = t;
    return (char *) t;
return_err:
    result->tag.kernel = TEND;
    if (nst)
	result->val.nint = aux - StreamLexAux(nst);
    if (nst) StreamPtr(nst) = t;
    return (char *) t;
}

/* CAUTION: Bip_Error() is redefined to Bip_Error_Fail() */
