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
 * VERSION	$Id: bip_strings.c,v 1.1 2008/06/30 17:43:52 jschimpf Exp $
 */

/*
 * IDENTIFICATION: 		bip_strings.c
 *
 * DESCRIPTION:			SEPIA Built-in Predicates: Strings
 *
 * CONTENTS:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * P.Dufresne     0.0           File Created.   
 * E.Falvey       0.1    890221 Added ICL standards.   
 * J.Schimpf		 02/90	New string format
 */

#include	"config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "dict.h"
#include 	"emu_export.h"
#include        "error.h"

 
pword     	*empty_string;

static dident	d_sha_;

static int	_concat_string(value v1, type t1, value vsep, pword **conc);

static int	p_string_list(value vs, type ts, value vl, type tl),
		p_utf8_list(value vs, type ts, value vl, type tl),
		p_concat_atoms(value v1, type t1, value v2, type t2, value vconc, type tconc),
		p_concat_atom(value v1, type t1, value vconc, type tconc),
		p_concat_string(value v1, type t1, value vconc, type tconc),
		p_concat_strings(value v1, type t1, value v2, type t2, value vconc, type tconc),
		p_first_substring(value vstr, type tstr, value vpos, type tpos, value vlen, type tlen, value vsub, type tsub),
		p_hash_secure(value v1, type t1, value vhash, type thash, value vmethod, type tmethod),
		p_join_string(value v1, type t1, value vsep, type tsep, value vconc, type tconc),
		p_string_length(value sval, type stag, value nval, type ntag),
		p_atom_length(value aval, type atag, value nval, type ntag),
		p_split_string(value vstr, type tstr, value vsep, type tsep, value vpad, type tpad, value v, type t),
		p_string_code(value vs, type ts, value vi, type ti, value vc, type tc),
		p_substring(value val1, type tag1, value val2, type tag2, value valp, type tagp),
		p_string_print_length(value v1, type t1, value vs, type ts, value ve, type te, value vl, type tl),
		p_char_int(value chval, type chtag, value ival, type itag);



/*
 * FUNCTION NAME:       bip_strings_init() 
 *
 * PARAMETERS:          NONE. 
 *
 * DESCRIPTION:         links the 'C' functions in this file with SEPIA 
 *                      built-in predicates.    
 */
void
bip_strings_init(int flags)
{
    if (flags & INIT_PRIVATE)
    {
	empty_string = enter_string_n("", 0L, DICT_PERMANENT);
	d_sha_ = in_dict("sha", 0);
    }

    if (flags & INIT_SHARED)
    {
	built_in(in_dict("string_list", 2),    p_string_list, B_UNSAFE|U_GROUND|PROC_DEMON)
	    -> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
	built_in(in_dict("utf8_list", 2),    p_utf8_list, B_UNSAFE|U_GROUND|PROC_DEMON)
	    -> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
	(void) built_in(in_dict("hash_secure", 3), 	p_hash_secure,	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("string_length", 2), 	p_string_length,B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("string_code", 3), 	p_string_code,	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("substring", 3), 	p_substring,	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("atom_length", 2), 	p_atom_length,	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("concat_atoms", 3), 	p_concat_atoms,	B_UNSAFE|U_SIMPLE|PROC_DEMON);
	(void) built_in(in_dict("concat_atom", 2), 	p_concat_atom,	B_UNSAFE|U_SIMPLE|PROC_DEMON);
	(void) built_in(in_dict("concat_strings", 3), 	p_concat_strings,B_UNSAFE|U_SIMPLE|PROC_DEMON);
	(void) built_in(in_dict("concat_string", 2), 	p_concat_string,B_UNSAFE|U_SIMPLE|PROC_DEMON);
	(void) built_in(in_dict("join_string", 3), 	p_join_string,	B_UNSAFE|U_SIMPLE|PROC_DEMON);
	built_in(in_dict("split_string", 4), 		p_split_string,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(4, GROUND);
	built_in(in_dict("char_int", 2), 	p_char_int,	B_UNSAFE|U_SIMPLE)
	    -> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
	(void) exported_built_in(in_dict("first_substring", 4),
						p_first_substring, B_UNSAFE|U_SIMPLE);
	exported_built_in(in_dict("string_print_length", 4),
						p_string_print_length, B_UNSAFE|U_SIMPLE) -> mode = BoundArg(3, CONSTANT);
    }
}



/*
 * FUNCTION NAME:       p_string_list(vs, ts, vl, tl) - logical
 *
 * PARAMETERS: 		vs, ts - a string or a variable.
 *                      vl, tl - a list or a variable.
 *  
 * DESCRIPTION:         Used to convert a string to a list whose elements are 
 *                      the ascii codes for the characters of the string.
 *                      Also used to convert a list (whose elements are ascii 
 *                      codes - i.e. integers in the range 0 to 255) to a  
 *                      string.
 *			If both arguments are instantiated, we chose the
 *			string->list direction. This is necessary since the
 *			argument list may be partly instantiated.
 *			In this case the list is currently no type checked!
 */

static int
p_string_list(value vs, type ts, value vl, type tl)
{
    register pword	*pw, *list;
    register char	*s;
    register int	len;
    pword		*old_tg = Gbl_Tg;

    if (IsRef(ts))			/* no string given	*/
    {
	if (IsRef(tl))			/* we need at least one	*/
	{
	    Bip_Error(PDELAY_1_2);
	}
	else if (IsList(tl))		/* make a string from a list	*/
	{
	    list = vl.ptr;		/* space for the string header	*/
	    Push_Buffer(1);		/* make minimum buffer		*/
	    s = (char *) BufferStart(old_tg);	/* start of the new string */
	    for(;;)			/* loop through the list	*/
	    {
		pw = list++;
		Dereference_(pw);		/* get the list element	*/
		if (IsRef(pw->tag))		/* check it		*/
		{
		    Gbl_Tg = old_tg;
		    Push_var_delay(vs.ptr, ts.all);
		    Push_var_delay(pw, pw->tag.all);
		    Bip_Error(PDELAY);
		}
		else if (!IsInteger(pw->tag))
		{
		    Gbl_Tg = old_tg;
		    Bip_Error(TYPE_ERROR);
		}
		else if (pw->val.nint < 0  ||  pw->val.nint > 255)
		{
		    Gbl_Tg = old_tg;
		    Bip_Error(RANGE_ERROR);
		}
		*s++ = pw->val.nint;
		if (s == (char *) Gbl_Tg)	/* we need another pword */
		{
		    Gbl_Tg += 1;
		    Check_Gc;
		}
		Dereference_(list);		/* get the list tail	*/
		if (IsRef(list->tag))
		{
		    Gbl_Tg = old_tg;
		    Push_var_delay(vs.ptr, ts.all);
		    Push_var_delay(list, list->tag.all);
		    Bip_Error(PDELAY);
		}
		else if (IsList(list->tag))
		    list = list->val.ptr;
		else if (IsNil(list->tag))
		    break;			/* end of the list	*/
		else
		{
		    Gbl_Tg = old_tg;
		    Bip_Error(TYPE_ERROR);
		}
	    }
	    *s = '\0';			/* terminate the string		*/
	    Set_Buffer_Size(old_tg, s - (char *)(old_tg + 1) + 1);
	    Kill_DE;
	    Return_Unify_String(vs, ts, old_tg);
	}
	else if (IsNil(tl))
	{
	    Kill_DE;
	    Return_Unify_String(vs, ts, empty_string);
	}
	else
	{
	    Bip_Error(TYPE_ERROR);
	}
    }
    else if (IsString(ts))
    {
	Kill_DE;
	Check_Output_List(tl);
	s = StringStart(vs);		/* get a pointer to the string	*/
	len = StringLength(vs);
	if (len == 0)
	{
	    Return_Unify_Nil(vl, tl);
	}
	/* Additional a-priori overflow check because adding to TG may
	 * may wrap around the address space and break Check_Gc below
	 */
	Check_Available_Pwords(2*len);
	pw = Gbl_Tg;			/* reserve space for the list	*/
	Gbl_Tg += 2*len;
	Check_Gc;
	pw->val.nint = *s++ & 0xFFL;	/* construct the list	*/
	pw++->tag.kernel = TINT;
	while (--len > 0)
	{
	    pw->val.ptr = pw + 1;
	    pw++->tag.kernel = TLIST;
	    pw->val.nint = *s++ & 0xFFL;
	    pw++->tag.kernel = TINT;
	}
	pw->tag.kernel = TNIL;
	Return_Unify_List(vl, tl, old_tg);
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }
}



/*
 * FUNCTION NAME:       p_substring(val1, tag1, val2, tag2, valp, tagp) 
 *
 * PARAMETERS:          val1 - string1->val 
 *                      tag1 - string1->tag, where string1 is the string
 *                             containing string2. 
 *                      string1 must be a string. 
 *                       
 *                      val2 - string2->val 
 *                      tag2 - string2->tag, where string2 is a substring
 *                             of string1. 
 *                      string2 must be a string. 
 *                       
 *                      valp - posn->val 
 *                      tagp - posn->tag, where posn is the position 
 *                             in string1 where string2 first occurs.  
 *                      posn must be an integer or a variable.  
 *                       
 * DESCRIPTION:         Used to test that string2 is a substring of string1  
 *                      beginning at position posn. In this case, string1 
 *                      and string2 are strings and posn is an integer. 
 *                      Also used to find the position in string1 that its 
 *                      substring string2 begins. In this case, string1 and 
 *                      string2 are strings and posn is a variable. 
 */

static int
p_substring(value val1, type tag1, value val2, type tag2, value valp, type tagp)
{
	char	*p1, *p2;
	word	length1, length2;
	word	i, j;

        /* string1 and string2 must be strings; posn an integer/variable. */ 

	Check_Output_Integer(tagp);
        Check_Output_String(tag1);
        Check_String(tag2);
	Error_If_Ref(tag1);

	length1 = StringLength(val1);
	length2 = StringLength(val2);

	if (!IsRef(tagp))
	{
		if (valp.nint <= 0 || valp.nint > length1 + 1)
		{
		    Bip_Error(RANGE_ERROR);
		}
		if (valp.nint > length1 - length2 + 1)
		{
		    Fail_;	/* string 2 is too long to match */
		}

		p1 = StringStart(val1) + valp.nint - 1;
		p2 = StringStart(val2);
		for(j = 0; j < length2; ++j)
		{
		    if (p1[j] != p2[j])
		    {
			Fail_;
		    }
		}
		Succeed_;
	}
	else
	{
		p1 = StringStart(val1);
		p2 = StringStart(val2);
		for (i = 1; i <= length1 - length2 + 1; i++)
		{
			/*
	         	 * search through p (i.e. string1) 'length2' characters
		 	 * at a time for val2.str (i.e. string2), till the end
		 	 * of string1. 
			 */ 
			for(j = 0; j < length2; ++j)
			{
			    if (p1[j] != p2[j])
				break;
			}
			if (j == length2)
			{
			    Return_Unify_Integer(valp, tagp, i);
			}
			p1++;
		}
		/* if not found, fail. */	
		Fail_;
	}	
}



/*
 * FUNCTION NAME:       p_string_length(sval, stag, nval, ntag) - logical
 *
 * PARAMETERS:          sval - string1->val
 *                      stag - string1->tag, where string1 is the string passed.
 *                      string1 must be a string. 
 *                      
 *                      nval - length1->val
 *                      ntag - length1->tag
 *                      length1 must be an integer/variable. 
 *
 * DESCRIPTION:         Used to measure the length of a string. In this case,  
 *                      string1 is a string and length1 is a variable. 
 *                      Also used to test whether length1 matches string1's 
 *                      length. In this case, string1 is a string and length1  
 *                      is an integer. 
 */

static int
p_string_length(value sval, type stag, value nval, type ntag)
{
        Check_Output_Integer(ntag);
	if (IsRef(stag))
	    { Bip_Error(PDELAY_1); }
	else if (!IsString(stag))
	    { Bip_Error(TYPE_ERROR); }

	Return_Unify_Integer(nval, ntag, StringLength(sval));
}



/*
 * FUNCTION NAME: 	p_atom_length(aval, atag, nval, ntag) - logical
 *
 * PARAMETERS: 		value aval - atom1->val   
 * 			type atag  - atom1->tag where atom1 is the atom passed. 
 *                      atom1 must be an atom. 
 *   
 *			value nval - length1->val 
 * 			type ntag  - length1->tag where length1 is the length of
 *                                   the atom passed. 
 *                      length1 must be an integer or a variable. 
 *  
 * DESCRIPTION:  	Used to find the length of the atom passed to it as a 
 * 			parameter. In this case, the atom is passed to the 
 * 			function as 'aval' and 'atag', and as the length of  
 *			the atom is uninstantiated, 'nval' and 'ntag' refer to
 *			a variable.
 *			Also used to match the integer 'nval.int' to the length 
 * 			of the atom. In this case, the atom is passed as   
 *			before, and the 'nval' and 'ntag' are also passed 
 *			instantiated. The success or failure of the matching 
 *			is returned. 
 */

static int
p_atom_length(value aval, type atag, value nval, type ntag)
{
        Check_Output_Integer(ntag);
	if (IsRef(atag))
	    { Bip_Error(PDELAY_1); }
	Check_Output_Atom_Or_Nil(aval, atag);
	Return_Unify_Integer(nval, ntag, DidLength(aval.did));
}



/*
 * FUNCTION NAME:       p_char_int(chval, chtag, ival, itag) - logical
 *                        
 * PARAMETERS:          chval, chtag - a single character string or a variable
 *                      ival,itag - an integer (0..255) or a variable
 *                        
 * DESCRIPTION:         Used to find the ascii code for a character passed.
 *			The character is represented by a single-element
 *			string. Character codes are in the range 0..255.
 *			This is a BSI predicate of questionable usefulness.
 */

static
p_char_int(value chval, type chtag, value ival, type itag)
{

        /* Case of: converting an integer to a character. */ 	

	if (IsRef(chtag))
	{
	    value		v;
	    register char	*s;

	    if (IsRef(itag))
		{ Bip_Error(PDELAY_1_2); }
	    else if (!IsInteger(itag))
		{ Bip_Error(TYPE_ERROR); }
	    if ((ival.nint < 0) || (ival.nint > 255)) 
	    {
		Bip_Error(RANGE_ERROR)
	    }
	    Make_Stack_String(1, v, s);
	    *s++ = ival.nint;
	    *s = '\0';
	    Return_Unify_String(chval, chtag, v.ptr);
	}
	else if (IsString(chtag) && StringLength(chval) == 1)
	{	
	    /* 
	     * Case of: converting a character to an integer / testing 	
	     *          whether character and integer match. 
	     */	

	    Check_Output_Integer(itag);
	    Return_Unify_Integer(ival, itag, (*(StringStart(chval)) & 0xFFL));
	}

	Bip_Error(TYPE_ERROR)
}



/*
 * FUNCTION NAME:       p_concat_atoms(v1, t1, v2, t2, vconc, tconc) - logical
 *                        
 * PARAMETERS:          v1    - atom1->val
 *                      t1    - atom1->tag, where atom1 is the leftmost part   
 *                              of the resultant atom atomconc. 
 *                      atom1 must be an atom.  
 *                      v2    - atom2->val  
 *                      t2    - atom2->tag, where atom2 is the rightmost part  
 *                              of the resultant atom atomconc. 
 *                      atom2 must be an atom.  
 *                      vconc - atomconc->val  
 *                      tconc - atomconc->tag, where atomconc is the concaten-  
 *                              ation of atom1 and atom2. 
 *                      atomconc must be an atom or a variable.  
 *
 * DESCRIPTION:         Used to concatenate atom1 with atom2 to form the atom 
 *                      atomconc. In this case, atom1 and atom2 are atoms and  
 *                      atomconc is a variable.  
 *                      Also used to test if atomconc is the concatenation of  
 *                      atom1 and atom2. In this case, all args are atoms.  
 */

static int
p_concat_atoms(value v1, type t1, value v2, type t2, value vconc, type tconc)
{
	dident		cdid;
	register char	*s, *t;
	value		v;
	register long	l1, l2;
	pword		*old_tg = Gbl_Tg;

        Check_Output_Atom_Or_Nil(vconc, tconc);	
	Check_Output_Atom_Or_Nil(v1, t1);
	Check_Output_Atom_Or_Nil(v2, t2);
	if (IsRef(t1))
	    { Bip_Error(PDELAY_1); }
	if (IsRef(t2))
	    { Bip_Error(PDELAY_2); }
	Kill_DE;

	l1 = DidLength(v1.did);
	l2 = DidLength(v2.did);
	Make_Stack_String(l1+l2, v, s)
	t = DidName(v1.did);			/* copy the strings	*/
	while (l1--)
	    *s++ = *t++;
	t = DidName(v2.did);
	while (l2--)
	    *s++ = *t++;
	*s = '\0';

	cdid = enter_dict_n(StringStart(v), StringLength(v), 0);
	Gbl_Tg = old_tg;
	Return_Unify_Atom(vconc, tconc, cdid);
}


/*
 * FUNCTION NAME:       p_concat_string(v1, t1, vconc, tconc) 
 *                        
 * PARAMETERS:          - a list of constants
 *                      - a string or variable
 *
 * DESCRIPTION:         Used to concatenate constants in the given list
 *			to yield a string.
 */

static int
p_concat_string(value v1, type t1, value vconc, type tconc)
{
    value	v, vsep;
    int		status;

    Check_Output_List(t1);
    Check_Output_String(tconc);
    if (IsRef(t1))
	{ Bip_Error(PDELAY_1); }
    vsep.ptr = empty_string;
    if ((status = _concat_string(v1, t1, vsep, &v.ptr)) != PSUCCEED)
    {
	return status;
    }
    Kill_DE;
    Return_Unify_String(vconc, tconc, v.ptr);

}

static int
p_join_string(value v1, type t1, value vsep, type tsep, value vconc, type tconc)
{
    value	v;
    int		status;

    if (IsRef(t1))
	{ Bip_Error(PDELAY_1); }
    if (IsRef(tsep))
	{ Bip_Error(PDELAY_2); }
    Check_Output_String(tconc);
    Check_List(t1);
    if (IsString(tsep)) ;
    else if (IsAtom(tsep)) vsep.ptr = DidString(vsep.did);
    else if (IsNil(tsep)) vsep.ptr = DidString(d_.nil);
    else { Bip_Error(TYPE_ERROR); }
    if ((status = _concat_string(v1, t1, vsep, &v.ptr)) != PSUCCEED)
    {
	return status;
    }
    Kill_DE;
    Return_Unify_String(vconc, tconc, v.ptr);

}


/*
 * FUNCTION NAME:       p_concat_atom(v1, t1, vconc, tconc) 
 *                        
 * PARAMETERS:          - a list of constants
 *                      - an atom or variable
 *
 * DESCRIPTION:         Used to concatenate constants in the given list
 *			to yield an atom.
 */

static int
p_concat_atom(value v1, type t1, value vconc, type tconc)
{
    pword	*old_tg = Gbl_Tg;
    value	v, vsep;
    dident	cdid;
    int		status;

    Check_Output_List(t1);
    Check_Output_Atom_Or_Nil(vconc, tconc);
    if (IsRef(t1))
	{ Bip_Error(PDELAY_1); }
    vsep.ptr = empty_string;
    if ((status = _concat_string(v1, t1, vsep, &v.ptr)) != PSUCCEED)
    {
	return status;
    }
    Kill_DE;
    cdid = enter_dict_n(StringStart(v), StringLength(v), 0);
    Gbl_Tg = old_tg;	/* the string can be discarded now */
    Return_Unify_Atom(vconc, tconc, cdid);
}


/*
 * auxiliary function for concat_atom/2 and concat_string/2
 * CAUTION: it may push something on SV and return PDELAY
 */

static int
_concat_string(value v1, type t1, value vsep, pword **conc)
{
	pword		*p;
	pword		*cst;
	char		*pa;
	char		*pc;
	long		length = 0;
	value		v;
	int		parts = 0;
	long		cst_tag;

	if (IsNil(t1))
	{
	    *conc = empty_string;
	    Succeed_;
	}

	/* First check all arguments and obtain a conservative
	 * estimate for the length of the concatenated atom.
	 */
	p = v1.ptr;
	for (;;)
	{
	    ++parts;
	    cst = p++;
	    Dereference_(cst);
	    cst_tag = TagType(cst->tag);
	    if (IsRef(cst->tag))
	    {
		Push_var_delay(cst, cst->tag.all);
		Bip_Error(PDELAY);
	    }
	    switch(cst_tag)
	    {
	    case TDICT:
		length += DidLength(cst->val.did);
		break;
	    case TSTRG:
		length += StringLength(cst->val);
		break;
	    case TNIL:
		length += 2;
		break;
	    default:	/* handles all the numeric types */
		if (IsNumber(cst->tag))
		    length += tag_desc[cst_tag].string_size(cst->val, cst->tag, 0);
		else
		    { Bip_Error(TYPE_ERROR); }
		break;
	    }

	    Dereference_(p);
	    if (IsRef(p->tag))
	    {
		Push_var_delay(p, p->tag.all);
		Bip_Error(PDELAY);
	    }
	    else if (IsNil(p->tag))
		break;
	    else if (IsList(p->tag))
		p = p->val.ptr;
	    else
	    {
		Bip_Error(TYPE_ERROR);
	    }
	}
	length += (parts-1) * StringLength(vsep);
	Make_Stack_String(length, v, pa);	/* may be too long */
	/*
	 * Then copy the strings to the buffer.
	 */
	p = v1.ptr;
	for (;;)
	{
	    cst = p++;
	    Dereference_(cst);
	    cst_tag = TagType(cst->tag);
	    switch(cst_tag)
	    {
	    case TDICT:
		pc = DidName(cst->val.did);
		length = DidLength(cst->val.did);
		while (length--) *pa++ = *pc++;
		break;
	    case TSTRG:
		pc = StringStart(cst->val);
		length = StringLength(cst->val);
		while (length--) *pa++ = *pc++;
		break;
	    case TNIL:
		*pa++ = '['; *pa++ = ']';
		break;
	    default:	/* handles all the numeric types */
		pa += tag_desc[cst_tag].to_string(cst->val, cst->tag, pa, 0);
		break;
	    }

	    Dereference_(p);
	    if (IsNil(p->tag))
		break;

	    length = StringLength(vsep);	/* add separator */
	    pc = StringStart(vsep);
	    while (length--)
		*pa++ = *pc++;

	    p = p->val.ptr;
	}
	*pa++ = 0;	/* NUL terminator */

	Trim_Buffer(v.ptr, (pa-StringStart(v)));
	*conc = v.ptr;
	Succeed_;
}


/*
 * split_string(+String, +SepChars, +PadChars, -List)
 *
 * Break up a string at the given separator characters.
 * Padding characters are removed around separators.
 * The remaining substrings are returned in List.
 * Characters occuring both in SepChars and PadChars are multi-separators,
 * ie. a sequence of them is treated as a single separator. If they
 * occur at the beginning or end of the input string, they are treated
 * like padding.
 */

#define S_START	0	/* in initial padding */
#define S_PRE	1	/* in padding after separator (pre-data) */
#define S_FIRST	2	/* just after first data char */
#define S_DATA	3	/* in data field */
#define S_POST	4	/* padding within or after data */
#define S_SEP	5	/* just after separator */
#define S_MSEP	6	/* in multi-separator */
#define S_STOP	7	/* end of string */
#define S_SIZE	7
#define P	0x10	/* output action */

#define C_DATA	0	/* input character classes */
#define C_PAD	1
#define C_SEP	2
#define C_MSEP	(C_PAD|C_SEP)
#define C_STOP	4
#define C_SIZE	5

static int transitions[S_SIZE][C_SIZE] =
{
/*		    C_DATA	C_PAD	    C_SEP	C_MSEP	    C_STOP */

/* S_START  */	    S_FIRST,	S_START,    P|S_SEP,	S_START,    P|S_STOP,
/* S_PRE    */	    S_FIRST,	S_PRE,	    P|S_SEP,	S_MSEP,     P|S_STOP,
/* S_FIRST  */	    S_DATA,	S_POST,     P|S_SEP,	P|S_MSEP,   P|S_STOP,
/* S_DATA   */	    S_DATA,	S_POST,     P|S_SEP,	P|S_MSEP,   P|S_STOP,
/* S_POST   */	    S_DATA,	S_POST,     P|S_SEP,	P|S_MSEP,   P|S_STOP,
/* S_SEP    */	    S_FIRST,	S_PRE,	    P|S_SEP,	P|S_MSEP,   P|S_STOP,
/* S_MSEP   */	    S_FIRST,	S_PRE,	    P|S_SEP,	S_MSEP,     S_STOP
};

static int
p_split_string(value vstr, type tstr, value vsep, type tsep, value vpad, type tpad, value v, type t)
{
    pword	result;
    pword	*tail = &result;
    char	*first, *last;
    char	*s, *stop;
    int		state, cc;

    Check_String(tstr);	
    Check_String(tsep);	
    Check_String(tpad);	
    Check_Output_List(t);	

    last = s = StringStart(vstr);
    stop = s-- + StringLength(vstr);
    first = last+1;

    for (state = S_START; ; state = transitions[state][cc])
    {
	if (state & P)
	{
	    char *ss;
	    Make_List(tail, TG);	/* create list element with substring */
	    tail = TG;
	    Push_List_Frame();
	    tail->val.ptr = TG;
	    tail++->tag.kernel = TSTRG;
	    ss = (char *) BufferStart(TG);
	    Push_Buffer(last-first+2);
	    while (first <= last)
		*ss++ = *first++;
	    *ss = 0;
	    first = last + 1;
	    state &= ~P;
	}
	switch (state)
	{
	case S_FIRST:
	    first = s;
	case S_DATA:
	    last = s;
	    break;
	case S_STOP:
	    Make_Nil(tail);
	    Return_Unify_Pw(v, t, result.val, result.tag);
	}
	if (++s == stop)		/* get next character class */
	    cc = C_STOP;
	else
	{
	    int i;
	    char c = *s;
	    cc = C_DATA;
	    for (i=0; i<StringLength(vpad); ++i)
	    	if (c == StringStart(vpad)[i]) { cc |= C_PAD; break; }
	    for (i=0; i<StringLength(vsep); ++i)
	    	if (c == StringStart(vsep)[i]) { cc |= C_SEP; break; }
	}
    }
}


/*
 * FUNCTION NAME:       p_concat_strings(v1, t1, v2, t2, vconc, tconc) logical
 *
 * PARAMETERS:          v1, t1 - the left string
 *			v2, t2 - the right string
 *			vconc, tconc - a variable or a string
 *				it is unified with the concatenation
 *				of the other two strings
 *                       
 * DESCRIPTION:		concat_strings(+String1, +String2, ?String3)
 *
 *			Used to concatenate string1 with string2 to form the 
 *                      string string3.
 */

static int
p_concat_strings(value v1, type t1, value v2, type t2, value vconc, type tconc)
{
    value		v;
    register char	*s, *t;
    register int	l1, l2;

    Check_Output_String(tconc);	
    Check_Output_String(t1);	
    Check_Output_String(t2);	
    if (IsRef(t1))
	{ Bip_Error(PDELAY_1); }
    if (IsRef(t2))
	{ Bip_Error(PDELAY_2); }
    Kill_DE;

    l1 = StringLength(v1);
    l2 = StringLength(v2);

    Make_Stack_String(l1 + l2, v, s);

    t = StringStart(v1);			/* copy the strings	*/
    while (l1--)
	*s++ = *t++;
    t = StringStart(v2);
    while (l2--)
	*s++ = *t++;
    *s = '\0';

    Return_Unify_String(vconc, tconc, v.ptr);
}



/*
 * first_substring(+String, +Position, +Length, ?SubString)
 * deterministic substring extraction
 */

static int
p_first_substring(value vstr, type tstr, value vpos, type tpos, value vlen, type tlen, value vsub, type tsub)
{
    char	*s;
    value	v;

    Check_String(tstr);
    Check_Integer(tpos);
    Check_Integer(tlen);
    Check_Output_String(tsub);
    if (vpos.nint + vlen.nint > StringLength(vstr) + 1)
	{ Fail_ }

    Make_Stack_String(vlen.nint, v, s);
    Copy_Bytes(s, StringStart(vstr) + vpos.nint - 1, vlen.nint);
    s[vlen.nint] = '\0';
    Return_Unify_String(vsub, tsub, v.ptr);
}

/*
 * Find out the print length of a given string up to a given
 * character, taken into account
 * tabs and backspaces and a starting position
 * string_print_length(+String, +Start, +CharPos, -Length)
*/
#define TAB_LENGTH	8
static int
p_string_print_length(value v1, type t1, value vs, type ts, value ve, type te, value vl, type tl)
{
    register char	*p;
    register int	size;
    int			pl;
    int			tabs;
    char		c;

    Check_String(t1)
    Check_Integer(ts)
    Check_Integer(te)
    Check_Output_Integer(tl)
    p = StringStart(v1);
    size = StringLength(v1);
    if (ve.nint < size && ve.nint >= 0)
	size = ve.nint;
    /* the number of spaces to make up to the next tab stop */
    tabs = TAB_LENGTH - vs.nint % TAB_LENGTH;
    pl = vs.nint/TAB_LENGTH*TAB_LENGTH;
    while (size--) {
	if ((c = *p++) == '\t') {
	    pl += tabs;
	    tabs = TAB_LENGTH;
	}
	else if (c == '\b') {
	    pl--;
	    tabs++;
	    if (tabs > TAB_LENGTH)
		tabs = 1;
	}
	else {
	    pl++;
	    tabs--;
	    if (tabs == 0)
		tabs = TAB_LENGTH;
	}
    }
    Return_Unify_Integer(vl, tl, pl);
}


static int
p_utf8_list(value vs, type ts, value vl, type tl)
{
    register pword	*pw, *list;
    register char	*s;
    register int	len;
    pword		*old_tg = TG;

    if (IsRef(ts))			/* no string given	*/
    {
	if (IsRef(tl))			/* we need at least one	*/
	{
	    Bip_Error(PDELAY_1_2);
	}
	else if (IsList(tl))		/* make a string from a list	*/
	{
	    list = vl.ptr;		/* space for the string header	*/
	    Push_Buffer(1);		/* make minimum buffer		*/
	    s = (char *) BufferStart(old_tg);	/* start of the new string */
	    for(;;)			/* loop through the list	*/
	    {
		uint32 ch;
		pw = list++;
		Dereference_(pw);		/* get the list element	*/
		if (IsRef(pw->tag))		/* check it		*/
		{
		    TG = old_tg;
		    Push_var_delay(vs.ptr, ts.all);
		    Push_var_delay(pw, pw->tag.all);
		    Bip_Error(PDELAY);
		}
		else if (!IsInteger(pw->tag))
		{
		    TG = old_tg;
		    Bip_Error(TYPE_ERROR);
		}

		if (s + 6 >= (char*) TG)
		{
		    TG += 1;
		    Check_Gc;
		}
		ch = pw->val.nint;
		if (ch < 0x80) {
		    *s++ = ch;
		} else if (ch < 0x800) {
		    s[1] = ch & 0xBF | 0x80; ch >>= 6;
		    s[0] = ch | 0xC0;
		    s += 2;
		} else if (ch < 0x10000) {
		    s[2] = ch & 0xBF | 0x80; ch >>= 6;
		    s[1] = ch & 0xBF | 0x80; ch >>= 6;
		    s[0] = ch | 0xE0;
		    s += 3;
		} else if (ch < 0x200000) {
		    s[3] = ch & 0xBF | 0x80; ch >>= 6;
		    s[2] = ch & 0xBF | 0x80; ch >>= 6;
		    s[1] = ch & 0xBF | 0x80; ch >>= 6;
		    s[0] = ch | 0xF0;
		    s += 4;
		} else if (ch < 0x4000000) {
		    s[4] = ch & 0xBF | 0x80; ch >>= 6;
		    s[3] = ch & 0xBF | 0x80; ch >>= 6;
		    s[2] = ch & 0xBF | 0x80; ch >>= 6;
		    s[1] = ch & 0xBF | 0x80; ch >>= 6;
		    s[0] = ch | 0xF8;
		    s += 5;
		} else {
		    s[5] = ch & 0xBF | 0x80; ch >>= 6;
		    s[4] = ch & 0xBF | 0x80; ch >>= 6;
		    s[3] = ch & 0xBF | 0x80; ch >>= 6;
		    s[2] = ch & 0xBF | 0x80; ch >>= 6;
		    s[1] = ch & 0xBF | 0x80; ch >>= 6;
		    s[0] = ch | 0xFC;
		    s += 6;
		}

		Dereference_(list);		/* get the list tail	*/
		if (IsRef(list->tag))
		{
		    TG = old_tg;
		    Push_var_delay(vs.ptr, ts.all);
		    Push_var_delay(list, list->tag.all);
		    Bip_Error(PDELAY);
		}
		else if (IsList(list->tag))
		    list = list->val.ptr;
		else if (IsNil(list->tag))
		    break;			/* end of the list	*/
		else
		{
		    TG = old_tg;
		    Bip_Error(TYPE_ERROR);
		}
	    }
	    *s = '\0';			/* terminate the string		*/
	    Trim_Buffer(old_tg, s - (char *)(old_tg + 1) + 1);
	    Kill_DE;
	    Return_Unify_String(vs, ts, old_tg);
	}
	else if (IsNil(tl))
	{
	    Kill_DE;
	    Return_Unify_String(vs, ts, empty_string);
	}
	else
	{
	    Bip_Error(TYPE_ERROR);
	}
    }
    else if (IsString(ts))
    {
	pword result;

	Kill_DE;
	Check_Output_List(tl);
	s = StringStart(vs);		/* get a pointer to the string	*/
	len = StringLength(vs);
	/* Additional a-priori overflow check because adding to TG may
	 * may wrap around the address space and break Check_Gc below
	 */
	Check_Available_Pwords(2*len);
	pw = TG;			/* reserve space for the list	*/
	TG += 2*len;
	Check_Gc;
	list = &result;
	while (len > 0)
	{
	    int c, upper_shift;
	    uint8 first = *s++;
	    --len;

	    if (first < 0xc0)
	    {
	    	c = first;
	    }
	    else
	    {
		upper_shift = -1;
		c = 0;
		while ((first <<= 1) & 0x80)
		{
		    upper_shift += 5;
		    c = (c<<6) + (*s++ & 0x3F);
		    --len;
		}
		c += first << upper_shift;
	    }
	    Make_List(list, pw);
	    Make_Integer(pw, c);
	    list = pw + 1;
	    pw += 2;
	}
	if (len < 0)
	{
	    TG = old_tg;
	    Bip_Error(BAD_FORMAT_STRING);
	}
	Make_Nil(list);
	Return_Unify_Pw(vl, tl, result.val, result.tag);
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }
}


static int
p_string_code(value vs, type ts, value vi, type ti, value vc, type tc)
{
    word i = vi.nint;
    Check_Integer(ti);
    Check_String(ts);
    Check_Output_Integer(tc);
    if (i > 0)
    {
	i -= 1;
	if (i >= StringLength(vs)) { Bip_Error(RANGE_ERROR); }
    }
#ifdef ALLOW_NEGATIVE_STRING_INDICES
    else if (i < 0)
    {
    	i += StringLength(vs);
	if (i < 0) { Bip_Error(RANGE_ERROR); }
    }
#endif
    else { Bip_Error(RANGE_ERROR); }
    Return_Unify_Integer(vc, tc, StringStart(vs)[i]);
}



/*
 * hash_secure(+String, -Hash, +Method)
 *
 * Computes a secure hash value for String.
 * The only method currently implemented is 'sha'.
 * The hash value is returned in Hash as a bignum.
 *
 * We use a free implementation by Jim Gillogly (sha.c)
 */

#undef A
#undef B
#undef E
#undef S
#ifdef WORDS_BIGENDIAN
#undef LITTLE_ENDIAN
#else
#ifndef LITTLE_ENDIAN
#define LITTLE_ENDIAN
#endif
#endif
#define ONT_WRAP
#define MEMORY_ONLY
#include "sha.c"

static int
p_hash_secure(value v, type t, value vhash, type thash, value vmethod, type tmethod)
{
    Check_Atom(tmethod);

    if (vmethod.did == d_sha_)
    {
	pword result;
	unsigned long hash[5];

	if (IsString(t))
	{
	    sha_memory(StringStart(v), StringLength(v), hash);
	}
	else
	{
	    pword pw;
	    value vstring;
	    extern pword *term_to_dbformat(pword *, dident);

	    pw.val.all = v.all;
	    pw.tag.all = t.all;
	    vstring.ptr = term_to_dbformat(&pw, D_UNKNOWN);
	    sha_memory(StringStart(vstring), StringLength(vstring), hash);
	}

	ec_array_to_big((const void *) hash, 5, 1, sizeof(long),
#ifdef WORDS_BIGENDIAN
		1,
#else
		0,
#endif
#if (SIZEOF_LONG == 8)
		32,
#else
		0,
#endif
		&result);
	Return_Unify_Pw(vhash, thash, result.val, result.tag);
    }
    else
    {
	Bip_Error(RANGE_ERROR);
    }
}

