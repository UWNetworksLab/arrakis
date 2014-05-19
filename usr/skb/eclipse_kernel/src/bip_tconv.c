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
 * VERSION	$Id: bip_tconv.c,v 1.1 2008/06/30 17:43:52 jschimpf Exp $
 */

/*
 * IDENTIFICATION:       bip_tconv.c
 *
 * DESCRIPTION:          SEPIA Built-in Predicates: Type testing and conversion.
 *
 * CONTENTS:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * Micha Meier    0.0    880906 Created file.   
 * E.Falvey       0.1    890302 Added ICL standards, corrected 3 bugs. 
 * E.Falvey       0.2    890629 Rewrote "univ" in C.
 */

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include 	"dict.h"
#include	"emu_export.h"
#include        "io.h"

 
#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

static int	p_atom_string(value va, type ta, value vs, type ts),  
		p_char_code(value v1, type t1, value v2, type t2), 
		p_functor(value vt, type t, value vf, type tf, value va, type ta), 
		p_integer_atom(value vn, type tn, value vs, type ts), 
		p_number_string(value vn, type tn, value vs, type ts),
		p_term_hash(value vterm, type tterm, value vdepth, type tdepth, value vrange, type trange, value vhash, type thash),
		p_canonical_copy(value v, type t, value vi, type ti),
		p_setarg(value vn, type tn, value vt, type tt, value va, type ta),
		p_type_of(value vterm, type term, value votype, type ttype),
		p_get_var_type(value vvar, type tvar, value vvtype, type ttype),
		p_get_var_name(value vvar, type tvar, value vname, type tname),
		p_univ(value tv, type tt, value lv, type lt);

/*
 * FUNCTION NAME:	bip_tconv_init() 
 *
 * PARAMETERS:		NONE. 
 *
 * DESCRIPTION:		links the 'C' functions in this file with SEPIA 
 * 			built-in predicates.   
 */
void
bip_tconv_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	/* functor/3 is U_UNIFY because the bound argument is not known */
	built_in(in_dict("functor", 3), 	p_functor, B_UNSAFE|U_UNIFY|PROC_DEMON)
	    -> mode = BoundArg(1, NONVAR) | BoundArg(2, CONSTANT) |
		BoundArg(3, CONSTANT);
	built_in(in_dict("char_code", 2), p_char_code, B_UNSAFE|U_GROUND)
		-> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
	built_in(in_dict("atom_string", 2), p_atom_string, B_UNSAFE|U_GROUND)
		-> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
	built_in(in_dict("integer_atom", 2), p_integer_atom, B_UNSAFE|U_GROUND)
		-> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
	(void) built_in(in_dict("type_of", 2), p_type_of, B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("get_var_type", 2),
			      p_get_var_type, B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("get_var_name", 2),
			      p_get_var_name, B_UNSAFE|U_SIMPLE);
	built_in(in_dict("=..", 2), p_univ, B_UNSAFE|U_UNIFY|PROC_DEMON)
	    -> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
	built_in(in_dict("number_string",2), p_number_string, B_UNSAFE|U_GROUND)
		-> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);

	(void) built_in(in_dict("term_hash", 4), p_term_hash, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("canonical_copy", 2), p_canonical_copy, B_UNSAFE|U_GROUND);
	(void) built_in(in_dict("setarg", 3), p_setarg, B_UNSAFE);
    }
}

/*
 * FUNCTION NAME: 	p_functor(vt, t, vf, tf, va, ta) - logical
 *
 * PARAMETERS:		vt - term1->val  
 *			t  - term1->tag, where term1 is the compound term
 *			     or list passed.
 *                      term1 must be a compound term or a list or a variable.
 *			 
 *			vf - functor1->val 
 *			tf - functor1->tag, where functor1 is the functor passed. 
 *			functor1 must be a functor (incl. an atom) or a variable. 
 *			 
 *			va - arity1->val 
 *			ta - arity1->tag, where arity1 is the arity passed. 
 *			arity1 must be an integer or a variable. 
 *			 
 * DESCRIPTION:		Used to instantaite variable(s) to either the functor 
 *		 	and / or the arity of the compound term term1. In this 
 *			case, term1 is instantiated to a compound term (i.e. a 
 *			structure or a list) and either functor1 is not instant-
 *			iated (to an atom) or arity1 is not instantiated (to an 
 *			integer) (or neither). 
 *			 
 *			Also used to test that functor1 is the functor and 
 *			arity1 is the arity of the compound term term1. In this 
 *			case, all arguments of functor/3 are instantiated. 
 *			 
 *			Also used to build the compound term term1 from the 
 *			functor functor1 and the arity arity1. In this case,  
 *			term1 is a variable, functor1 is an atom and arity1 is 
 *			an integer. 
 *			 
 *			Also used to instantiate the variable functor1 to the
 *			atomic term1 (or vice versa). In this case, arity1 is 0.
 */
static int
p_functor(value vt, type t, value vf, type tf, value va, type ta)
{
    int             i;
    register long	arity;
    pword           *p;

    if (IsRef(t))
    {	
	/*
	 * Case of: term1 uninstantiated.
	 * Thus functor1 must be instantiated to an atomic,
	 * arity1 must be instantiated to an integer.
	 */

	if (IsRef(ta))
	{
	    if (IsCompound(tf))
	    {
		Bip_Error(TYPE_ERROR)
	    }
	    Bip_Error(PDELAY_1_3)
	}
	else if (!IsInteger(ta))
	{
	    if (IsBignum(ta)) { Bip_Error(RANGE_ERROR) };
	    Bip_Error(TYPE_ERROR)
	}
	else
	{
	    /* arity must be a positive integer */	
	    if ((arity = va.nint) < 0)
	    {		
		Bip_Error(RANGE_ERROR);
	    }	
	    if (IsRef(tf))
	    {
		Bip_Error(PDELAY_1_2);
	    }
	}

	/* if arity = 0, term1 is unified with (atomic) functor1. */
	if (arity == 0)	
	{
	    if (IsCompound(tf))
	    {
		Bip_Error(TYPE_ERROR)
	    }
	    Kill_DE;
	    Return_Bind_Var(vt, t, vf.all, tf.kernel); 
	}
	Kill_DE;

	if (!IsAtom(tf) && !IsNil(tf))  
	{	
	    Bip_Error(TYPE_ERROR)
	}

	if (vf.did == d_.eocl && arity == 2)	/* a list functor */
	{
	    /*	
	     * This is for the case of a list functor: functor1 is 
	     * '.', and arity1 is 2. This is a special case; 
	     * functor1 = '.' and arity1 != 2 is treated normally.
	     */
	    p = Gbl_Tg;
	    Gbl_Tg += 2;
	    Check_Gc;
	    Bind_Var(vt, t, p, TLIST);
	}
	else
	{    
	    /* 
	     * this is for the case of a structure defined by
	     * functor1 and arity1. Thus, term1's arguments are 
	     * variables.  
	     */

	    register dident	d;

	    p = Gbl_Tg;
	    /* Additional a-priori overflow check because adding arity to TG
	     * may may wrap around the address space and break Check_Gc below
	     */
	    Check_Available_Pwords(arity+1);
	    Gbl_Tg += arity + 1;
	    Check_Gc;

	    /* create the structure functor */ 
	    if (IsNil(tf))  
		d = d_.nil;
	    else
		d = vf.did;
	    Add_Dict(d, (int) arity);
	    Bind_Var(vt, t, p, TCOMP);
	    p->val.did = (dident) d;
	    (p++)->tag.kernel = TDICT;
	}
	for (i = 0; i < arity; i++)
	{
	    p->val.ptr = p;
	    (p++)->tag.kernel = TREF;
	}
	Succeed_;
    }

    /* Case of: term1 instantiated. */	

    Kill_DE;
    if (IsCompound(tf))
    {
	Bip_Error(TYPE_ERROR);
    }
    else if (IsRef(tf) && IsRef(ta) && vf.ptr == va.ptr)
    {
	/* catch functor(Term,F,F) - only call BindVar once! */
	if (!(IsInteger(t) && vt.nint == 0))
	{
	    Fail_;
	}
	Bind_Var(va, ta, 0, TINT);
	Succeed_;
    }

    if (IsStructure(t))
    {
	/*
	 * term1 is a compound term. Its value, accessed by 
	 * vt.ptr, points to the functor1 (which is accessed by 
	 * val.did), followed by (next addresses) the arguments,
	 * though the latter are not required. 
	 * Thus, one can get the functor1, then use DidArity
	 * to get the arity1. 
	 * 
	 * Since term1 is instantiated, functor1's DID is 
	 * in the dictionary, so DidArity and DidName work. 
	 */

	register dident	d;

	d =  vt.ptr->val.did;
	arity = DidArity(d);
	Add_Dict(d, 0);
	if (IsRef(tf))
	{
	    Bind_Var(vf, tf, d, (d == d_.nil ? TNIL : TDICT));
	}
	else if (!(d == d_.nil ? IsNil(tf) : IsAtom(tf) && vf.did == d))
	{
	    Fail_;
	}
    }
    else if (IsList(t))
    {
	/*
	 * This is the case where term1 is the functor list.
	 * Thus, functor is '.' and arity is 2.
	 */
	arity = 2;
	if (IsRef(tf))
	{
	    Bind_Var(vf, tf, d_.eocl, TDICT);
	}
	else if (!IsAtom(tf) || vf.did != d_.eocl)
	{
	    Fail_;
	}
    }
    else
    {
	int res = Unify_Pw(vf, tf, vt, t);
	Return_If_Not_Success(res);
	arity = 0;
    }

    /* arity1 must be a variable or a positive integer */
    if (!IsRef(ta))
    {
	if (!IsInteger(ta))
	{
	    if (IsBignum(ta)) { Fail_; }
	    Bip_Error(TYPE_ERROR);
	}
	if (va.nint != arity)
	    Fail_;
    }
    else
    {
	Bind_Var(va, ta, arity, TINT);
    }
    Succeed_;
}


/*
 * FUNCTION NAME:	p_type_of(vterm, term, votype,ttype) - logical
 *
 * PARAMETERS:		vterm  - term1->val 
 *		 	term   - term1->tag, where term1 is the expression
 *			         whose type is to be evaluated / tested.
 *			term1 can be of any type.
 *
 *		 	votype - type1->val  	
 *		 	ttype  - type1->tag, where type1 is one of the atoms
 *			         in the set {atom, var, integer, string, real, 
 * 			         compound}. 
 *
 * DESCRIPTION:		Used to find the data type of an expression. In this 
 *		 	case, Expression is instantiated and Type is a variable.
 *		 	  	
 *		 	Also used to test whether Type is the data type of  	
 *		 	Expression. In this case, Expression is instantiated 
 *		 	and Type is an atom that is in the above set.  	
 */
/*ARGSUSED*/
static int
p_type_of(value vterm, type term, value votype, type ttype)
{
	dident          dtype;

	/* atom1 should be an atom or a variable. */

	Check_Output_Atom_Or_Nil(votype, ttype);

	if (IsRef(term))
	{
		dtype = d_.var0;
	}
	else if (TagType(term) >= 0 && TagType(term) <= NTYPES)
	{
		dtype = tag_desc[tag_desc[TagType(term)].super].type_name;
	}
	else
	    { Bip_Error(UNIFY_OVNI); }

	/* unify (the assigned) dtype with the passed argument type1. */

	Return_Unify_Atom(votype, ttype, dtype);
}


/*
 * FUNCTION NAME:	p_atom_string(va, ta, vs, ts) - logical
 *
 * PARAMETERS:		va - atom1->val 
 *			ta - atom1->tag, where atom1 is the atom corresponding  
 *			     to the string string1. 
 *			atom1 must be an atom or a variable.  
 *			 
 *			vs - string1->val  
 *			ts - string1->tag, where string1 is the string 
 *			     corresponding to the atom atom1.  
 *			string1 must be a string or a variable. 
 *			 
 * DESCRIPTION:		Used to convert an atom to its string form. In this 
 *			case, atom1 is an atom and string1 is a variable.  
 *			 
 *		  	Also used to convert a string to its string form. In 
 *			this case, aom1 is a variable and string1 is a string. 
 *			 
 *			Also used to check whether string1 is the string form  
 *			of atom1. In this case, atom1 is an atom and string1 is 
 *			a string. 
 */
static int
p_atom_string(value va, type ta, value vs, type ts)
{
	if (IsRef(ts))
	{
		if (IsRef(ta))
		{
		    Bip_Error(PDELAY_1_2);
		}
		Check_Output_Atom_Or_Nil(va, ta);
		Return_Unify_String(vs, ts, DidString(va.did));
	}
	else if IsString(ts)
	{
		if (IsRef(ta))
		{
			/*
			 * if only string1 is instantiated, unify its DID
			 * with atom1.
			 */
			dident wdid = enter_dict_n(StringStart(vs),
							StringLength(vs), 0);
			if (wdid == d_.nil)	/* necessary !!! */
			{
				Return_Unify_Nil(va, ta);
			}
			else
			{  
				Return_Unify_Atom(va, ta, wdid);
			}
		}
		else if (IsAtom(ta))
		{
			/* both arguments are instantiated. */	

			value v1;
			v1.ptr = DidString(va.did);
			Succeed_If(!compare_strings(vs, v1));
		}
		else if (IsNil(ta))
		{
			/* as before, IsAtom([]) fails, so deal with it now. */	

			Succeed_If(!strcmp(StringStart(vs), DidName(d_.nil)))
		}
	}

	/* any other types => type error. */	

	Bip_Error(TYPE_ERROR);
}


/*
 * FUNCTION NAME: 	p_integer_atom(vn, tn, vs, ts)
 *
 * PARAMETERS: 		vn, tn	variable or integer
 * 			vs, ts	variable or atom
 * 		 	  	 
 * DESCRIPTION:		Used to convert integer to string and vice versa.
 *			Fails if this is not possible.
 *			Mainly for backward compatibility, superseded
 *			now by number_string/2.
 */

static int
p_integer_atom(value vn, type tn, value vs, type ts)
{
    pword result;

    if (IsRef(ts))
    {
	if (IsRef(tn))
	    { Bip_Error(PDELAY_1_2); }
	else				/* integer to atom */
	{
	    char *s;
	    dident wdid;
	    pword *old_tg = TG;

	    if (IsInteger(tn) || IsBignum(tn))
	    {
		int len = tag_desc[TagType(tn)].string_size(vn, tn, 1);
		Make_Stack_String(len, result.val, s);	/* maybe too long */
		len = tag_desc[TagType(tn)].to_string(vn, tn, s, 1);
		wdid = enter_dict_n(s, len, 0);
	    }
	    else
		{ Bip_Error(TYPE_ERROR); }

	    TG = old_tg;	/* pop the temporary string */
	    Return_Unify_Atom(vs, ts, wdid);
	}
    }
    else if (IsRef(tn) || IsInteger(tn) || IsBignum(tn))
    {
	Check_Atom_Or_Nil(vs, ts);	/* atom to integer */
	if (string_to_number(DidName(vs.did), &result, (stream_id) 0, 0) ==
		DidName(vs.did) + DidLength(vs.did)
	    && (IsInteger(result.tag) || IsBignum(result.tag)))
	{
	    Return_Unify_Pw(vn, tn, result.val, result.tag);
	}
	else { Fail_; }
    }
    else { Bip_Error(TYPE_ERROR); }
}



/*
 * FUNCTION NAME: 	p_number_string(vn, tn, vs, ts)
 *
 * PARAMETERS: 		vn, tn	variable or number
 * 			vs, ts	variable or string
 * 		 	  	 
 * DESCRIPTION:		Used to convert a string to an integer or real,
 *			and vice versa. Fails if this is not possible.
 */

static int
p_number_string(value vn, type tn, value vs, type ts)
{
    pword result;

    if (IsRef(ts))
	if (IsRef(tn))
	    { Bip_Error(PDELAY_1_2); }
	else if (!IsNumber(tn))
	    { Bip_Error(TYPE_ERROR); }
	else				/* number to string */
	{
	    char *s;
	    int len = tag_desc[TagType(tn)].string_size(vn, tn, 1);
	    Make_Stack_String(len, result.val, s);	/* maybe too long */
	    len = tag_desc[TagType(tn)].to_string(vn, tn, s, 1);
	    Trim_Buffer(result.val.ptr, len+1);		/* adjust length */
	    Return_Unify_String(vs, ts, result.val.ptr);
	}
    else if (IsString(ts)		/* string to number */
	&& (IsRef(tn) || IsNumber(tn)))
    {
	if (string_to_number(StringStart(vs), &result, (stream_id) 0, 0) ==
		StringStart(vs) + StringLength(vs)
	    && !IsTag(result.tag.kernel, TEND))
	{
	    Return_Unify_Pw(vn, tn, result.val, result.tag);
	}
	else { Fail_; }
    }
    else { Bip_Error(TYPE_ERROR); }
}


/*
 * FUNCTION NAME:       p_char_code(tv, tt, lv, lt) - logical
 *
 */

static int
p_char_code(value v1, type t1, value v2, type t2)
{
    int len;
    char *s;

    if (IsRef(t1)) {
	if (IsRef(t2)) {			/* char_code(-,-) */
	    Bip_Error(PDELAY_1_2);
	} else if (IsInteger(t2)) {		/* char_code(-Char, +Code) */
	    char buf[2];
	    if (v2.nint < 0 || v2.nint > 255) {
		Bip_Error(RANGE_ERROR);
	    }
	    buf[0] = (char) v2.nint;
	    buf[1] = 0;
	    Return_Unify_Atom(v1, t1, enter_dict_n(buf,1,0));
	} else {
	    Bip_Error(TYPE_ERROR);
	}
    } else {					/* char_code(+Char, ?Code) */
	if (IsAtom(t1)) {
	    len = DidLength(v1.did);
	    s = DidName(v1.did);
	} else if (IsString(t1)) {
	    len = StringLength(v1);
	    s = StringStart(v1);
	} else {
	    Bip_Error(TYPE_ERROR)
	}
	if (len != 1) {
	    Bip_Error(TYPE_ERROR)
	}
	if (IsRef(t2)) {
	} else if (IsInteger(t2)) {
	    if (v2.nint < 0 || v2.nint > 255) {
		Bip_Error(RANGE_ERROR);
	    }
	} else {
	    Bip_Error(TYPE_ERROR)
	}
	Return_Unify_Integer(v2, t2, *s);
    }
}

/*
 * FUNCTION NAME:       p_univ(tv, tt, lv, lt) - logical
 *
 * PARAMETERS:          tv - Term->val
 *                      tt - Term->tag, where Term is the term passed
 *                      lv - List->val
 *                      lt - List->tag, where List is the list passed.
 *
 * DESCRIPTION:         Pronounced "univ".
 *
 * If Term is atomic and/or List is a single-element list, unifies this
 * element with Term.
 *
 * Otherwise, either Term is instantiated to a compound term, or List
 * is instantiated to a list, or both. In which case, "univ" unifies Term
 * with functor(Arg1, Arg2, ..., ArgN), and List with
 * [Functor', Arg1', Arg2', .., argN'], where functor is unified with
 * Functor', Arg1 is unified with Arg1', etc.
 * functor must be an atom, and it must be possible to determine the length
 * of List from either Term or List.
 *
 * NOTE: The structure arguments are simply copied to the list elements
 * and vice versa. We assume that this it is always possible to copy
 * a pword from the global stack to the global stack if it occurs inside
 * a compound term (ie no nonstandard variables/mutable objects inside)
 */


static int
p_univ(value tv, type tt, value lv, type lt)
{
long     arity, i;
pword   *tail, *head, *newel, *tvptr, *elem;
dident  fd;

        tvptr = tv.ptr;

        if (IsRef(tt))  
        {
        /* case of: converting List to Term. */

		if (IsRef(lt)) { Bip_Error(PDELAY_1_2); }
                Check_Output_Pair(lt);                 

		elem = lv.ptr;
		tail = elem + 1;
		Dereference_(tail)
	       	if (IsRef(tail->tag))
		{
                                    /* partial list -> error 4. */
		    Push_var_delay(tv.ptr, tt.all);
		    Push_var_delay(tail, tail->tag.all);
		    Bip_Error(PDELAY)
                }
		else if (IsList(tail->tag))
 		{
		    /* converting List to Compound Term. */

		    Dereference_(elem)
		    if (IsRef(elem->tag))
		    {
			/* no functor given */
			Push_var_delay(tv.ptr, tt.all);
			Push_var_delay(elem, elem->tag.all);
			Bip_Error(PDELAY)
		    }
		    Check_Output_Atom_Or_Nil(elem->val,elem->tag);

		    fd = elem->val.did;

		    head = Gbl_Tg++;
		    head->val.did = fd;
		    head->tag.kernel = TDICT;

		    for (i = 0; !IsRef(tail->tag) && IsList(tail->tag); i++)
		    {
			    elem = tail->val.ptr;
			    head = Gbl_Tg++;
			    Check_Gc;
			    *head = *elem;
			    tail = elem + 1;
			    Dereference_(tail)
		    }	

		    if (IsRef(tail->tag))
		    {
					/* partial list -> error 4. */
			    Gbl_Tg = head - i;
			    Push_var_delay(tv.ptr, tt.all);
			    Push_var_delay(tail, tail->tag.all);
			    Bip_Error(PDELAY)
		    }
		    else if (!IsNil(tail->tag))
		    {
					/* bad list -> error 5. */
			    Gbl_Tg = head - i;
			    Bip_Error(TYPE_ERROR)
		    }

		    /* go back to write functor with now known arity i. */

		    Kill_DE;
		    if (fd == d_.eocl && i == 2)
		    {
			    head--;         /* ignore the functor */
			    Return_Unify_List(tv, tt, head);
		    }
		    else
		    {
			    head -= i;
			    head->val.did = add_dict(fd, (int) i);
			    Return_Unify_Structure(tv, tt, head);
		    }
		}
                else if (IsNil(tail->tag))
                {
					/* single element list	*/
			Dereference_(elem)
			if (IsRef(elem->tag))
			{
			    Push_var_delay(tv.ptr, tt.all);
			    Push_var_delay(elem, elem->tag.all);
			    Bip_Error(PDELAY)
			}
			Kill_DE;
                        if (!IsCompound(elem->tag))
                        {
                                Return_Unify_Pw(tv, tt, elem->val, elem->tag);
			}
			else
			{
			    Bip_Error(TYPE_ERROR);
			}
                }
                else
                {
                                    /* bad list -> error 5. */
                        Bip_Error(TYPE_ERROR)
                }
        }

        /** case of: converting Term to List. **/

        else if (IsCompound(tt))
        {
                /* converting Compound Term to List. */

		Kill_DE;
                if (!IsRef(lt) && !IsList(lt))
                {
                        Bip_Error(TYPE_ERROR);
                }

                newel = Gbl_Tg;
                Gbl_Tg += 2;

                if (IsList(tt))
		{
                        arity = 2;
                        newel->tag.kernel = TDICT;
                        (newel++)->val.did = d_.eocl;
                        tvptr--;
                }
                else
                {
                        arity = DidArity(tvptr->val.did);
                        fd = add_dict(tvptr->val.did, 0);
                        if (fd == d_.nil)
                            (newel++)->tag.kernel = TNIL;
                        else
                        {
                            newel->tag.kernel = TDICT;
                            (newel++)->val.did = fd;
                        }
                }

		/* Additional a-priori overflow check because adding arity to TG
		 * may may wrap around the address space and break Check_Gc below
		 */
		Check_Available_Pwords(2*arity);
                Gbl_Tg += 2*arity;
                Check_Gc
                for (i = 0; i < arity; i++)
                {
                        newel->val.ptr = newel + 1;
                        (newel++)->tag.kernel = TLIST;
                        *newel++ = *(++tvptr);
                }
                newel->tag.kernel = TNIL;

                newel -= (2*arity + 1);

                Return_Unify_List(lv, lt, newel);
        }
        else
        {
                /* the rare case of atomic term -> 1-element list. */

		Kill_DE;
                if (!IsRef(lt) && !IsList(lt))                        
                {
                        Bip_Error(TYPE_ERROR);
                }

                newel = Gbl_Tg;
                Gbl_Tg += 2;
                newel->val = tv;
                (newel++)->tag = tt;
                (newel--)->tag.kernel = TNIL;
                Check_Gc
                Return_Unify_List(lv, lt, newel);
        }
}


pword *
ec_chase_arg(value vn, type tn, value vt, type tt, int *perr)
{
    pword *pw1;
    word argi, arity;
    if (IsInteger(tn))
    {
	argi = vn.nint;
	if (IsStructure(tt))
	{
	    pw1 = vt.ptr;
	    arity = DidArity(pw1->val.did);
	}
	else if IsList(tt)
	{
	    pw1 = vt.ptr-1;
	    arity = 2;
	}
	else
	{
	    *perr = IsRef(tt) ? INSTANTIATION_FAULT : TYPE_ERROR;
	    return 0;
	}
	if (argi < 1 || argi > arity)
	{
	    *perr = RANGE_ERROR;
	    return 0;
	}
	return pw1 + argi;	/* not dereferenced! (for setarg) */
    }
    else if (IsList(tn))
    {
	pword *plist = vn.ptr;
	for(;;)
	{
	    pword *car = plist++;
	    Dereference_(car);
	    if (IsInteger(car->tag))	/* list element must be integer */
	    {
		argi = car->val.nint;
		if (IsStructure(tt))
		{
		    pw1 = vt.ptr;
		    arity = DidArity(pw1->val.did);
		}
		else if IsList(tt)
		{
		    pw1 = vt.ptr-1;
		    arity = 2;
		}
		else
		{
		    *perr = IsRef(tt) ? INSTANTIATION_FAULT : TYPE_ERROR;
		    return 0;
		}
		if (argi < 1 || argi > arity)
		{
		    *perr = RANGE_ERROR;
		    return 0;
		}
		pw1 += argi;		/* get argument */
		Dereference_(plist);
		if (IsNil(plist->tag))
		{
		    return pw1;		/* not dereferenced! (for setarg) */
		}
		else if (!IsList(plist->tag))
		{
		    *perr = IsRef(plist->tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
		    return 0;
		}
		plist = plist->val.ptr;
		Dereference_(pw1);
		vt.all = pw1->val.all;
		tt.all = pw1->tag.all;
	    }
	    else
	    {
		*perr = IsRef(car->tag) ? INSTANTIATION_FAULT :
			IsBignum(car->tag) ? RANGE_ERROR :
			tag_desc[TagType(car->tag)].numeric ? TYPE_ERROR :
			ARITH_TYPE_ERROR;
		return 0;
	    }
	}
    }
    else
    {
	*perr = IsRef(tn) ? INSTANTIATION_FAULT :
		IsBignum(tn) ? RANGE_ERROR :
		TYPE_ERROR;
	return 0;
    }
}



/*
 * FUNCTION NAME:       p_setarg(vn, tn, vt, tt, va, ta)
 *
 * PARAMETERS:          setarg(+N, +Term, ?NewArg)
 *
 * DESCRIPTION:         Destructively replaces the Nth argument of Term.
 *			This is undone on backtracking.
 */

static int
p_setarg(value vn, type tn, value vt, type tt, value va, type ta)
{
    pword	*argp;
    word	arity;
    int		err;

    if (IsInteger(tn))
    {
	if (IsRef(tt))
	{
	    Bip_Error(INSTANTIATION_FAULT)
	}
	else if (IsStructure(tt))
	{
	    argp = vt.ptr;
	    arity = DidArity(argp->val.did);
	}
	else if (IsList(tt))
	{
	    argp = vt.ptr - 1;
	    arity = 2;
	}
	else if (SameTypeC(tt, THANDLE))
	{
	    pword pw;
	    pw.val = va;
	    pw.tag = ta;
	    Check_Type(vt.ptr->tag, TEXTERN);
	    if (!ExternalData(vt.ptr))
		{ Bip_Error(STALE_HANDLE); }
	    if (!ExternalClass(vt.ptr)->set)
		{ Bip_Error(UNIMPLEMENTED); }
	    return ExternalClass(vt.ptr)->set(ExternalData(vt.ptr), vn.nint, pw);
	}
	else
	{
	    Bip_Error(TYPE_ERROR)	/* no compound term	*/
	}
	if (vn.nint < 1 || vn.nint > arity)
	{
	    Bip_Error(RANGE_ERROR);
	}
	argp += vn.nint;
    }
    else	/* deal with IsList(tn) and errors */
    {
	argp = ec_chase_arg(vn, tn, vt, tt, &err);
	if (!argp)
	{
	    Bip_Error(err);
	}
    }
#if 0
    /* this is a sensible restriction, but not imposed for compatibility */
    if (IsRef(argp->tag)  &&  argp == argp->val.ptr)
    {
	Bip_Error(INSTANTIATION_FAULT);	/* trying to destroy a variable! */
    }
#endif
    if (argp < TG_ORIG || TG <= argp)
    {
	Bip_Error(GROUND_CONST_MODIFY);	/* trying to modify a heap term! */
    }
    return ec_assign(argp, va, ta);	/* succeeds */
}


/*
 * term_hash(+Term, +Depth, +Range, -Hash)
 *
 * Hash is not instantiated when the Term is not sufficiently
 * instantiated (ie. up to Depth)
 */

/* compute hash value of a string of given length */
#if 0
#define Hashl(id, hash, n) {						\
	register char *str = (id);					\
	register int length = (n);					\
        for (hash = 0; length > 0; str++, --length)			\
            hash += (hash<<3) + *(unsigned char *)str;			\
}

#else

/*
 * This hash function is the same as the simple one above as long as
 * the string is shorter than MAX_SAMPLED_CHARS. If it is longer, we
 * look only at every incr'th character, where incr is chosen such
 * that we look at no more than MAX_SAMPLED_CHARS characters to compute
 * the hash value. The code is a bit tricky because we want to make sure
 * that we always consider the last character. We achieve that by making
 * one possibly smaller step (< incr) in the middle of the string.
 */

#define MAX_SAMPLED_CHARS 32
#define Hashl(id, hash, n) {						\
	unsigned char *str = (unsigned char *) (id);			\
	int incr = 1 + (n)/MAX_SAMPLED_CHARS;				\
	int _i, _j;							\
	hash = 0;							\
	for (_i= 0, _j=(n)-1; _i < _j; _i+=incr, _j-=incr)		\
	    hash += (hash<<3) + str[_i];				\
	if (_j < _i) _j+=incr;						\
	for (; _j < (n); _j+=incr)					\
	    hash += (hash<<3) + str[_j];				\
}
#endif

uword
ec_term_hash(value vterm,
	type tterm,
	uword maxdepth,			/* > 0 */
	int *pres)
{
    uword h;
    int arity;
    dident d;
    pword *arg_i;

    switch(TagType(tterm))
    {
    case TVAR_TAG:
    case TNAME:
    case TMETA:
    case TUNIV:
	*pres = INSTANTIATION_FAULT;
	return 0;

    case TINT:
	return vterm.nint;

    case TDBL:
#ifdef UNBOXED_DOUBLES
	Hashl((char*) &vterm.all, h, SIZEOF_DOUBLE);
#else
	Hashl(StringStart(vterm), h, SIZEOF_DOUBLE);
#endif
	return h;

    case TSTRG:
	Hashl(StringStart(vterm), h, StringLength(vterm));
	return h;

    case TDICT:
	Hashl(DidName(vterm.did), h, DidLength(vterm.did));
	return h;

    case TCOMP:
	d = (vterm.ptr++)->val.did;
	Hashl(DidName(d), h, DidLength(d));
	arity = DidArity(d);
	break;

    case TLIST:
	h = 0;
	arity = 2;
	break;

    default:
	if (ISPointer(tterm.kernel) && IsTag(vterm.ptr->tag.kernel, TBUFFER))
	{
	    Hashl(StringStart(vterm), h, StringLength(vterm)+1);
	    return h;
	}
	return 0;
    }

    if (--maxdepth == 0)
    	return h;

    for(;arity > 0; arity--)
    {
	pword *pvar;
	arg_i = vterm.ptr++;
	Dereference_(arg_i);
	h += (h<<3) + ec_term_hash(arg_i->val, arg_i->tag, maxdepth, pres);
    }
    return h;
}

static int
p_term_hash(value vterm, type tterm, value vdepth, type tdepth, value vrange, type trange, value vhash, type thash)
{
    uword h;
    int res = PSUCCEED;

    Check_Integer(tdepth);
    Check_Integer(trange);
    if (vrange.nint <= 0) { Bip_Error(RANGE_ERROR); }
    if (vdepth.nint < -1) { Bip_Error(RANGE_ERROR); }

    h = vdepth.nint ? ec_term_hash(vterm, tterm, (uword)vdepth.nint, &res) : 0;
    if (res == INSTANTIATION_FAULT)
    {
	Succeed_;	/* don't bind the hash value if variable */
    }
    h = (h % vrange.nint);
    Return_Unify_Integer(vhash, thash, h);
}


static int
p_canonical_copy(value v, type t, value vi, type ti)
{
    pword pw;
    int res = ec_constant_table_enter(v, t, &pw);
    if (res != PSUCCEED)
    	return res;
    Return_Unify_Pw(vi, ti, pw.val, pw.tag);
}


/* The following builtins use the global error variable ! */
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

/*
  get_var_type(Var, Type) unify the type of the free variable Var with Type.
  Fails if Var is nonvar.
*/
/*ARGSUSED*/
static int
p_get_var_type(value vvar, type tvar, value vvtype, type ttype)
{
    dident	dtype;

    Check_Output_Atom_Or_Nil(vvtype, ttype);

    if (IsRef(tvar))
    {
	switch (TagType(tvar))
	{
	case TNAME:
	case TVAR_TAG:
	    dtype = d_.free;
	    break;
	case TUNIV:
	    dtype = d_.universally_quantified;
	    break;
	case TMETA:
	    dtype = d_.meta0;
	    break;
	    
	default:
	    Bip_Error(UNIFY_OVNI);
	}
	Return_Unify_Atom(vvtype, ttype, dtype);
    }
    else
    {
	Set_Bip_Error(0);
	Fail_;
    }
}

/*ARGSUSED*/
static int
p_get_var_name(value vvar, type tvar, value vname, type tname)
{
    dident      dname;

    Check_Output_Atom_Or_Nil(vname, tname);
    
    if (IsRef(tvar) && IsNamed(tvar.kernel))
    {
	dname = TagDid(tvar.kernel);
	Return_Unify_Atom(vname, tname, dname);
    }
    else
    {
	Set_Bip_Error(0);
	Fail_;
    }
}

