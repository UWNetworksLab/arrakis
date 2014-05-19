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
 * Contributor(s): ECRC GmbH and IC-Parc.
 * 
 * END LICENSE BLOCK */

/*
 * SEPIA INCLUDE FILE
 *
 * $Id: sepia.h,v 1.4 2008/09/01 11:44:54 jschimpf Exp $
 *	
 * IDENTIFICATION		sepia.h
 *
 * DESCRIPTION :		defines Type Tags, and creates macros for	
 *				tesing types; returning to SEPIA;  	
 *				dictionary macros; backtracking macros.  	
 *	
 */

#include "ec_public.h"

/*****************************************************************/
/*								 */
/*		P R O L O G   T A G S   			 */
/*								 */
/*****************************************************************/

/* The order is important for the grouping:
 *	TLIST		pointer		compound
 *	TCOMP		pointer		compound
 *	TSUSP		pointer		opaque
 *	THANDLE		pointer		opaque
 *	TSTRG		pointer		atomic
 *	TBIG		pointer		atomic
 *	TIVL		pointer		atomic
 *	TRAT		pointer		atomic
 *	TDBL		pointer/simple	atomic	(see UNBOXED_DOUBLES)
 *	TNIL		simple		atomic
 *	TINT		simple		atomic
 *	TDICT		simple		atomic
 *	TPTR		simple		atomic
 */

#define TLIST		0	/* list (Ptr to global stack or heap) */
#define TCOMP		1	/* structure (Ptr to global stack or heap) */
#define TSUSP		2	/* suspension (Ptr to global stack) */
#define THANDLE		3	/* handle (Ptr to global stack anchor) */
#define TSTRG		4	/* string (Ptr to global stack or heap) */
#define TBIG		5	/* bignum (Ptr to global stack or heap) */
#define TIVL		6	/* breal (Ptr to global stack or heap) */
#define TRAT		7	/* rational (Ptr to global stack or heap) */
#define TDBL		8	/* double (or ptr to global/heap on 32 bit) */
#define TNIL		9	/* nil, value field does not matter */
#define TINT		10	/* integer */
#define TDICT		11 	/* atom / functor */
#define TPTR   		12     	/* pointer - used for objects, arrays and */
				/* other highly illogical stuff */

/* Keep this definition in ec_public.h up-to-date: */
/* #define NTYPES	13 	 * no. of types + var (for codegen) */

/* internal tags, not unifiable */

#define TPROC		13 	/* goal tag, val is a (pri *)		*/
#define TEND		14	/* to mark the first unused argument	*/

/* tags for identifying special global stack structures */

#define TDE     	15	/* delay environment			*/
#define TGRS		16	/* ground body structure (codegen)	*/
#define TGRL		17	/* ground body list (codegen)		*/
#define TEXTERN		18	/* a stack anchor for external data,	*/
				/* referenced by THANDLE pwords.      	*/
				/* Value part is pointer to type desc.	*/
				/* Next pword is TPTR | &data.		*/
#define TBUFFER		19	/* a buffer on the global stack, used	*/
				/* for strings, bignums, doubles etc.	*/
				/* The value part gives the number of	*/
				/* bytes - 1 that follow		*/
#define TVARNUM		20	/* for temporarily numbering variables	*/

/* If modifying tags, update tag names in printam.c */


/*
 * variables and references:
 * - they all have the TREFBIT set and a variable type in Tag(t)
 * - if not a self reference, the Tag(t) part is irrelevant
 * - if a self reference, the Tag(t) part type gives the variable type
 */

#define TREFBIT		SIGN_BIT

#define TVAR_TAG	-1	/* simple variable			*/
#define TNAME		-2	/* named variable			*/
#define TMETA		-3	/* attributed variable (metaterm)	*/
#define TUNIV		-4	/* explictly quantified variable	*/
#define TSTAMP		-5	/* time stamp (never self reference)	*/

#define TFORWARD	-6	/* used internally for heap copying	*/
#define TFORWARD2	-7	/* used internally for heap copying	*/


/* some aliases */

#define TCUT    TINT            /* no special tag necessary */


/*
 * The next 2 bits are used by the garbage collector. They must be zero
 * when a garbage collection is invoked and they are guaranteed to be zero
 * after a collection.
 * Under certain conditions it is possible to use them otherwise, e.g.
 * locally in a builtin, if it is made sure that they are reset to zero
 * after use, and that a GC will never be invoked while the bits are set.
 */
#define MARK		(SIGN_BIT >> 1)
#define LINK		(SIGN_BIT >> 2)


/*
 * The PERSISTENT bit can be set in the following pointer tags:
 * TLIST, TCOMP, TSTRG, TBIG, TIVL, TRAT, TDBL
 * It indicates that the pointer points to a non-volatile, shareable heap
 * copy of the object (usually an entry in the table of ground constants).
 * Losing the bit will disable sharing-based optimizations, but should
 * otherwise be non-fatal. However, care must be taken not to copy this
 * bit (along with the rest of the tag) to non-persistent pointer values:
 * Use  new_tag = Tag(old_tag)  to copy any (nonvar) pointer tag!
 */
#define PERSISTENT	(SIGN_BIT >> 3)


/*
 * Bits that can be set in TBUFFER tags:
 *	IN_DICT		a corresponding atom exists (only in TSTRG buffers)
 *	BIGSIGN		sign of a bignum (only in TBIG buffers)
 *	RAW_IVL		lexer breal, unnormalised (only in TIVL buffers)
 */
#define IN_DICT		(SIGN_BIT >> 3)
#define BIGSIGN		(SIGN_BIT >> 3)
#define RAW_IVL		(SIGN_BIT >> 3)

/*
 * Bit that can be set in a TPROC (event handler property) tag
 */
#define EVENT_DEFERS	(SIGN_BIT >> 3)

/*
 * Bit that can be set in TMETA tag:
 * 	HIDE_ATTR	suppresses printing of the attribute 
 */
#define HIDE_ATTR	(SIGN_BIT >> 3)

/*
 * Variable names are stored as 19-bit field in variable tags
 */
#define TAG_NAME_MASK 0x07ffff00


/*****************************************************************/
/*								 */
/*		TAG  CONSTRUCTION				 */
/*								 */
/*****************************************************************/

/*
 * construct a non-variable tag from another tag of the same type
 */
#define Tag(t)			((t) & 0xFF)

/*
 * construct a variable tag:  tagtype is TVAR_TAG,TNAME,TMETA etc
 */
#define RefTag(tagtype)		(SIGN_BIT|Tag(tagtype))
#define TREF			RefTag(TVAR_TAG) /* simple var/ref tag */

/*
 * the same for a named variable
 */
#define DidTag(tagtype, vdid)	(RefTag(tagtype)|DidBitField(vdid)<<8)


/*****************************************************************/
/*								 */
/*		T A G  T E S T I N G    macros			 */
/*								 */
/*****************************************************************/

#define TagNameField(t)		((t) & TAG_NAME_MASK)
#define IsNamed(t)		TagNameField(t)
#define TagDid(t)	((dident) bitfield_did((long)(TagNameField(t)>>8)))

#define TagTypeC(t)		((int8) (t))
#define TagType(item_tag)	TagTypeC((item_tag).kernel)
#define SameType(item_tag1,item_tag2)\
				(TagType(item_tag1) == TagType(item_tag2))
#define SameTypeC(item_tag,c)	(TagType(item_tag) == TagTypeC(c))
#define DifferType(item_tag1,item_tag2)\
				(TagType(item_tag1) != TagType(item_tag2))
#define DifferTypeC(item_tag,c)	(TagType(item_tag) != TagTypeC(c))

	/* test for reference, including self reference (all variables) */
#define IsRef(item_tag)         (item_tag.kernel < 0)
#define ISRef(t)		((t) < 0)

	/* test for simple variable */
#define IsVar(item_tag)		SameTypeC(item_tag, Tag(TVAR_TAG))

#define IsForward(item_tag)	SameTypeC(item_tag, Tag(TFORWARD))
#define IsPersistent(item_tag)	((item_tag).kernel & PERSISTENT)
#define IsCompound(d)		(IsList(d) || IsStructure(d))
#define IsNumber(d)		(!IsRef(d) && tag_desc[TagType(d)].numeric)
#define ISAtomic(tag)		(TagTypeC(tag) >= TagTypeC(TSTRG))

#define IsUniv(item)            SameTypeC(item, Tag(TUNIV))
#define IsMeta(item)          	SameTypeC(item, Tag(TMETA))
#define IsName(item)     	SameTypeC(item, Tag(TNAME))
#define IsSusp(item)		SameTypeC(item, TSUSP)
#define IsList(item)		SameTypeC(item, TLIST)
#define IsStructure(item)	SameTypeC(item, TCOMP)
#define IsString(item)		SameTypeC(item, TSTRG)
#define IsBignum(item)		SameTypeC(item, TBIG)
#define IsRational(item)	SameTypeC(item, TRAT)
#define IsDouble(item)		SameTypeC(item, TDBL)
#define IsInterval(item)	SameTypeC(item, TIVL)
#define IsNil(item)		SameTypeC(item, TNIL)
#define IsInteger(item)		SameTypeC(item, TINT)
#define IsAtom(item)		SameTypeC(item, TDICT)
#define IsProc(item)		SameTypeC(item, TPROC)
#define IsHandle(item)		SameTypeC(item, THANDLE)


/*
 * These tag testing macros are similar to the ones above
 * but they take a 'ktype' as argument rather than a 'type'.
 */

#define IsTag(t,c)		(TagTypeC(t) == TagTypeC(Tag(c)))
#define EqTag(t1,t2)		(TagTypeC(t1) == TagTypeC(t2))


/* Compare two simple values. For TNIL the value does not matter.
 * Note that if the value is a float, we still compare it as if it
 * was an integer, which lets us distinguish -0.0 from 0.0.
 */

#define SimpleEq(t,v1,v2)       \
        ((v1).all == (v2).all   \
         || IsTag(t,TNIL))

#ifdef UNBOXED_DOUBLES
#define ISSimple(t)		(TagTypeC(t) >= TagTypeC(TDBL))
#else
#define ISSimple(t)		(TagTypeC(t) > TagTypeC(TDBL))
#endif

#define IsSimple(item_tag)	ISSimple((item_tag).kernel)


/*
 * Used by th GC. It yields true for all pwords whose value part might
 * be a pointer into the global stack (ie. references and compounds)
 */
#ifdef UNBOXED_DOUBLES
#define ISPointer(tag)          (TagTypeC(tag) < TagTypeC(TDBL))
#else
#define ISPointer(tag)          (TagTypeC(tag) <= TagTypeC(TDBL))
#endif


/*
 * This one is needed by th GC. A "special" tag occurs only inside
 * the global stack and is never handled by prolog.
 */
#define ISSpecial(tag)          (TagTypeC(tag) > TagTypeC(TEND))


/*****************************************************************/
/*								 */
/*		E R R O R     macros      			 */
/*								 */
/*****************************************************************/

#define Error_If_Ref(tag)	if (IsRef(tag)) {\
					Bip_Error(INSTANTIATION_FAULT)\
				}

#define Bip_Error(errcode)	return(errcode);

#define Set_Errno { \
	ec_os_errgrp_ = 0 /*ERRNO_UNIX*/ ; \
	ec_os_errno_ = errno; \
	errno = 0; }

#define Bip_Throw(val, tag)	return return_throw(val, tag);

#define Exit_Block(val, tag)	longjmp_throw(val, tag);


/*****************************************************************/
/*								 */
/*		S U C C E E D / F A I L  macros		         */
/*								 */
/*****************************************************************/

#define Succeed_		return(PSUCCEED);
#define Fail_			return(PFAIL);
#define Succeed_If(cond)	return((cond) ? PSUCCEED : PFAIL);
#define Succeed_Last		{ Cut_External; Succeed_; }


/*****************************************************************/
/*								 */
/*		C H E C K   T Y P E     macros			 */
/*								 */
/*****************************************************************/

#define Check_Type(tag, type)			\
	if (DifferTypeC(tag, type)) {		\
		Error_If_Ref(tag);		\
		Bip_Error(TYPE_ERROR)		\
	}
#define Check_Ref(item)				\
	if (!IsRef(item))			\
	{					\
		Bip_Error(TYPE_ERROR)		\
	}
#define Check_List(item)			\
	if ((!IsList(item)) && (!IsNil(item)))\
	{					\
		Error_If_Ref(item);		\
		Bip_Error(TYPE_ERROR)		\
	}
#define Check_Pair(item)	Check_Type(item, TLIST)
#define Check_Structure(item)	Check_Type(item, TCOMP)
#define Check_String(item)	Check_Type(item, TSTRG)
#define Check_Nil(item)		Check_Type(item, TNIL)
#define Check_Atom(item)	Check_Type(item, TDICT)
#define Check_Double(item)	Check_Type(item, TDBL)
#define Check_Interval(item)	Check_Type(item, TIVL)
/*
 * The following macros cope with the special representation of [] in Sepia.
 * It is like Check_Atom except that is succeeds even for nil and sets
 * the val to the nil DID. It is the responsibility of the caller to
 * make sure that it will pass to Prolog the TNIL and not (TDICT, d_.nil);
 */
#define Check_Output_Atom_Or_Nil(val, tag)	\
	if (IsNil(tag))				\
	    val.did = d_.nil;			\
	else if (!IsRef(tag) && !IsAtom(tag)) {	\
	    Bip_Error(TYPE_ERROR)		\
	}
#define Check_Atom_Or_Nil(val, tag)		\
	if (IsNil(tag))				\
		val.did = d_.nil;		\
	else if (!IsAtom(tag))			\
	{					\
		Error_If_Ref(tag);		\
		Bip_Error(TYPE_ERROR)		\
	}

/* check for short integers, make range error for bignums */

#define Check_Integer(tag) \
	if (DifferTypeC(tag,TINT)) { \
	    Error_If_Ref(tag); \
	    if (SameTypeC(tag, TBIG)) \
		{ Bip_Error(RANGE_ERROR); } \
	    else if (IsNumber(tag)) \
		{ Bip_Error(TYPE_ERROR); } \
	    else { Bip_Error(ARITH_TYPE_ERROR); } \
	}

/* check for any kind of integer, including bignums */

#define Check_Integer_Or_Bignum(tag) \
	if (DifferTypeC(tag,TINT)) { \
	    if (DifferTypeC(tag, TBIG)) { \
		Error_If_Ref(tag); \
		if (IsNumber(tag)) \
		    { Bip_Error(TYPE_ERROR); } \
		else { Bip_Error(ARITH_TYPE_ERROR); } \
	    } \
	}

/* check for float or double */

#define Check_Float(tag) \
	if (DifferTypeC(tag,TDBL)) { \
	    Error_If_Ref(tag); \
	    if (IsNumber(tag)) \
		{ Bip_Error(TYPE_ERROR); } \
	    else { Bip_Error(ARITH_TYPE_ERROR); } \
	}

/* check for any numeric type */

#define Check_Number(tag) \
	if (!IsNumber(tag))			\
	{					\
		Error_If_Ref(tag);		\
		Bip_Error(ARITH_TYPE_ERROR)	\
	}
	
/* check for potential goal types */

#define Check_Goal(tag) 					\
	if (!(IsCompound(tag) || IsNil(tag) || IsAtom(tag)))	\
	{							\
		Error_If_Ref(tag);				\
		Bip_Error(TYPE_ERROR)				\
	}
	
/*****************************************************************/
/*								 */
/*	    C H E C K   O U T P U T   T Y P E     macros	 */
/*								 */
/*****************************************************************/

#define Check_Output_Type(tag, type)		\
	if (!IsRef(tag) && !SameTypeC(tag, type)) {	\
		Bip_Error(TYPE_ERROR)		\
	}
#define Check_Output_List(item)			\
	if (!IsRef(item) && (!IsList(item)) && (!IsNil(item)))\
	{					\
		Bip_Error(TYPE_ERROR)		\
	}
#define Check_Output_Pair(item)		Check_Output_Type(item, TLIST)
#define Check_Output_Structure(item)	Check_Output_Type(item, TCOMP)
#define Check_Output_String(item)	Check_Output_Type(item, TSTRG)
#define Check_Output_Nil(item)		Check_Output_Type(item, TNIL)
#define Check_Output_Atom(item)		Check_Output_Type(item, TDICT)

#define Check_Output_Integer(tag) \
	if (!IsRef(tag) && !SameTypeC(tag, TINT)) { \
	    if (SameTypeC(tag, TBIG)) \
		{ Fail_; } \
	    else if (IsNumber(tag)) \
		{ Bip_Error(TYPE_ERROR); } \
	    else { Bip_Error(ARITH_TYPE_ERROR); } \
	}

#define Check_Output_Integer_Or_Bignum(tag) \
	if (!IsRef(tag) && !SameTypeC(tag, TINT) && !SameTypeC(tag, TBIG)) { \
	    if (IsNumber(tag)) \
		{ Bip_Error(TYPE_ERROR); } \
	    else { Bip_Error(ARITH_TYPE_ERROR); } \
	}

#define Check_Output_Float(tag) \
	if (!IsRef(tag) && DifferTypeC(tag,TDBL)) { \
	    if (IsNumber(tag)) \
		{ Bip_Error(TYPE_ERROR); } \
	    else { Bip_Error(ARITH_TYPE_ERROR); } \
	}

#define Check_Output_Interval(tag) \
	if (!IsRef(tag) && DifferTypeC(tag,TIVL)) { \
	    if (IsNumber(tag)) \
		{ Bip_Error(TYPE_ERROR); } \
	    else { Bip_Error(ARITH_TYPE_ERROR); } \
	}

#define Check_Output_Number(tag) \
	if (!IsRef(tag) && !IsNumber(tag)) { \
	    Bip_Error(ARITH_TYPE_ERROR); \
	}

/******************************************************************/
/* 								  */
/*		U N I F I C A T I O N    macros			  */
/*								  */
/******************************************************************/

#define Unify_Pw(vx,tx,vy,ty)		ec_unify_(vx,tx,vy,ty,&MU)

#define Prepare_Requests		int uNiFy_result = PSUCCEED;

#define Request_Unify_Pw(vx,tx,vy,ty)		\
	uNiFy_result = uNiFy_result == PFAIL ? PFAIL : ec_unify_(vx,tx,vy,ty,&MU);

#define Request_Unify_Type(vx,tx,valytype,v,t)	\
	{					\
	    pword py;				\
	    py.tag.kernel = (t);		\
	    py.val.valytype = (v);		\
	    Request_Unify_Pw(vx,tx,py.val,py.tag)\
	}


#define Request_Unify_Integer(vx,tx,vy)	   Request_Unify_Type(vx,tx,nint,vy,TINT)
#define Request_Unify_Atom(vx,tx,vy)	   \
		Request_Unify_Type(vx,tx,did,vy,((vy) == d_.nil ? TNIL : TDICT))
#define Request_Unify_String(vx,tx,vy)     Request_Unify_Type(vx,tx,ptr,vy,TSTRG)
#define Request_Unify_List(vx,tx,vy)	   Request_Unify_Type(vx,tx,ptr,vy,TLIST)
#define Request_Unify_Structure(vx,tx,vy)     Request_Unify_Type(vx,tx,ptr,vy,TCOMP)
#define Request_Unify_Nil(vx,tx)	   Request_Unify_Type(vx,tx,nint,0,TNIL)

#define Request_Unify_Float(vx,tx,dbl) { \
	    pword result_pw; \
	    Make_Checked_Float(&result_pw, dbl); \
	    Request_Unify_Pw(vx, tx, result_pw.val, result_pw.tag) \
	}

#define Request_Unify_Double(vx,tx,dbl) { \
	    pword result_pw; \
	    Make_Checked_Double(&result_pw, dbl); \
	    Request_Unify_Pw(vx, tx, result_pw.val, result_pw.tag) \
	}

#define Request_Unify_Interval(vx,tx,from,to) { \
	    pword result_pw; \
	    Make_Checked_Interval(&result_pw, from, to); \
	    Request_Unify_Pw(vx, tx, result_pw.val, result_pw.tag) \
	}


#define Return_If_Failure		if (uNiFy_result == PFAIL) return PFAIL;
#define Return_If_Not_Success(_err)	{ if ((_err) != PSUCCEED) return (_err); }
#define Return_If_Error(_err)		{ if ((_err) < 0) return (_err); }
#define Return_Unify			return uNiFy_result;

#define Return_Unify_Pw(vx,tx,vy,ty)	return ec_unify_(vx,tx,vy,ty,&MU);

#define Return_Unify_Type(vx,tx,valytype,v,t)	\
	{					\
	    pword py;				\
	    py.tag.kernel = (t);		\
	    py.val.valytype = (v);		\
	    Return_Unify_Pw(vx,tx,py.val,py.tag)\
	}

#define Return_Unify_Integer(vx,tx,vy)	   Return_Unify_Type(vx,tx,nint,vy,TINT)
#define Return_Unify_Atom(vx,tx,vy)	   \
		Return_Unify_Type(vx,tx,did,vy,((vy) == d_.nil ? TNIL : TDICT))
#define Return_Unify_String(vx,tx,vy)      Return_Unify_Type(vx,tx,ptr,vy,TSTRG)
#define Return_Unify_List(vx,tx,vy)	   Return_Unify_Type(vx,tx,ptr,vy,TLIST)
#define Return_Unify_Structure(vx,tx,vy)      Return_Unify_Type(vx,tx,ptr,vy,TCOMP)
#define Return_Unify_Nil(vx,tx)		Return_Unify_Type(vx,tx,nint,0,TNIL)

#define Return_Unify_Float(vx,tx,dbl) { \
	    pword result_pw; \
	    Make_Checked_Float(&result_pw, dbl); \
	    Return_Unify_Pw(vx, tx, result_pw.val, result_pw.tag) \
	}

#define Return_Unify_Double(vx,tx,dbl) { \
	    pword result_pw; \
	    Make_Checked_Double(&result_pw, dbl); \
	    Return_Unify_Pw(vx, tx, result_pw.val, result_pw.tag) \
	}

#define Return_Unify_Interval(vx,tx,from,to) { \
	    pword result_pw; \
	    Make_Checked_Interval(&result_pw, from, to); \
	    Return_Unify_Pw(vx, tx, result_pw.val, result_pw.tag) \
	}


/******************************************************************/
/* 								  */
/*		D E R E F E N C I N G    macro			  */
/*								  */
/******************************************************************/

#define Dereference_(ref)					\
	while (IsRef(ref->tag) && ref != ref->val.ptr)		\
		ref = ref->val.ptr;


#define IsSelfRef(ref)	((ref)->val.ptr == (ref))


/****************************************************************/
/*	Backtracking Externals					*/
/****************************************************************/

#define Remember(n, v, t)				\
		{					\
			int	code = ec_remember(n, v, t);\
			if (code != PSUCCEED)		\
			{				\
				Bip_Error(code);	\
			}				\
		}

#define Cut_External	cut_external();


/******************************************************************/
/*		Overflow Checks    		       		  */
/******************************************************************/

#define Check_Trail_Ov  if (TT <= TT_LIM) trail_ov();
#define Check_Gc        if (TG >= TG_LIM) global_ov();
#define GlobalStackOverflow	(TG >= TG_LIM && final_overflow())
#define Check_Available_Pwords(n) \
	if ((uword)(n) > (uword)((pword*) TT - TG)) { \
	    pword exit_tag; \
	    Make_Atom(&exit_tag, d_.global_trail_overflow); \
	    Bip_Throw(exit_tag.val, exit_tag.tag); \
	}


/******************************************************************/
/*		Term construction    		       		  */
/******************************************************************/

#define Make_Nil(pw) \
	(pw)->tag.kernel = TNIL;

#define Make_Atom(pw, wdid) \
	(pw)->tag.kernel = TDICT; \
	(pw)->val.did = wdid;

#define Make_Integer(pw, /* long */ n) \
	(pw)->tag.kernel = TINT; \
	(pw)->val.nint = (long) n;

#define Make_Double(pw, /* double */ dbl) \
	(pw)->tag.kernel = TDBL; \
	Make_Double_Val((pw)->val, (dbl))

#define Make_Checked_Double(pw, /* double */ dbl) \
	(pw)->tag.kernel = TDBL; \
	Make_Checked_Double_Val((pw)->val, dbl)

#define Make_Float(pw, /* double */ dbl) \
	Make_Double(pw, dbl)

#define Make_Checked_Float(pw, /*  double */ dbl) \
	Make_Checked_Double(pw, dbl)

#define Make_Interval(pw,from,to) {		\
	(pw)->tag.kernel = TIVL;		\
	Push_Interval((pw)->val.ptr,from,to)	\
}

#define Make_Checked_Interval(pw,from,to) {		\
	(pw)->tag.kernel = TIVL;			\
	Push_Checked_Interval((pw)->val.ptr,from,to)	\
}

#define Make_String(pw, /* char * */ s) \
	(pw)->tag.kernel = TSTRG; \
	Cstring_To_Prolog(s, (pw)->val)

#define Make_List(pw, plist) \
	(pw)->tag.kernel = TLIST; \
	(pw)->val.ptr = (plist)

#define Push_List_Frame() \
	TG += 2; \
	Check_Gc;

#define Make_Struct(pw, pstruct) \
	(pw)->tag.kernel = TCOMP; \
	(pw)->val.ptr = (pstruct)

#define Push_Struct_Frame(wdid) { \
	register pword *_pstruct = TG; \
	TG += DidArity(wdid) + 1; \
	Check_Gc; \
	_pstruct->val.did = (wdid); \
	_pstruct->tag.kernel = TDICT; }

#define Make_Ref(pw, p) \
	(pw)->tag.kernel = TREF; \
	(pw)->val.ptr = (p);

#define Make_Var(pw) \
	(pw)->tag.kernel = TREF; \
	(pw)->val.ptr = (pw);

#define Make_NamedVar(pw, namedid) \
	(pw)->tag.kernel = DidTag(TNAME, namedid); \
	(pw)->val.ptr = (pw);

#define Push_Var() \
	++TG; \
	Make_Var(TG-1); \
	Check_Gc;

#define Push_NamedVar(namedid) \
	++TG; \
	Make_NamedVar(TG-1, namedid); \
	Check_Gc;
/*
 * Global Stack Buffers, used for strings, bignums, rationals, doubles, ...
 */

#define Push_Buffer(size_bytes) { \
	register pword *_pstruct = TG; \
	TG += BufferSizePwords(size_bytes); \
	Check_Gc; \
	Set_Buffer_Size(_pstruct, size_bytes); \
	_pstruct->tag.kernel = TBUFFER; }

#define Trim_Buffer(pw, size_bytes) { \
	Set_Buffer_Size(pw, size_bytes); \
	TG = (pw) + BufferSizePwords(size_bytes); }

#define Set_Buffer_Size(pw, size_bytes) (pw)->val.nint = (size_bytes)-1L;
#define BufferSize(pw) ((int) (pw)->val.nint + 1)
#define BufferStart(pw) ((pw) + 1)
#define BufferPwords(pw) ((int) (pw)->val.nint / sizeof(pword) + 2)
#define BufferSizePwords(size_bytes) (((size_bytes) - 1) / sizeof(pword) + 2)

#define BigNegative(pw) ((pw)->tag.kernel & BIGSIGN)

/*
 *	S T R I N G S
 *
 *				+----------+
 * SEPIA strings look		|	   |
 * like this:			| "....\0" |
 *				|	   |
 *	+---------+		+----------+
 *	|  TSTRG  |		| TBUFFER  |
 *	+---------+		+----------+
 *	|    ----------------->	|  length  |
 *	+---------+		+----------+
 *
 * The string itself may be in the global stack or in the heap.
 * If it is a non-volatile heap string its tag is TBUFFER|IN_DICT.
 * Although we have the explicit length information, the strings are
 * always terminated with a zero character to keep C compatibility.
 * The length field is the string length in bytes without the terminator.
 */

/* get the string length from its value part	*/

#define StringLength(v)	((v).ptr->val.nint)


/* get a pointer to the string characters from the value part	*/

#define StringStart(v)	((char *) BufferStart((v).ptr))


#define StringInDictionary(v)	((v).ptr->tag.kernel & IN_DICT)


/* Create an uninitialised string buffer on the global stack.
 * Assign the value to <v> and the buffer pointer to <start>.
 * The buffer can hold <len> bytes plus a zero terminator.
 */
#define Make_Stack_String(len, v, start) \
	(v).ptr = TG;\
	Push_Buffer((len)+1);\
	(start) = StringStart(v);


/* Build a prolog string from an existing C string (zero terminated).
 * The C string is copied to the global stack and a length field added.
 */
#define Cstring_To_Prolog(cstring, v) \
	{   char *neww, *old = (cstring);\
	    (v).ptr = TG;\
	    Push_Buffer(1);\
	    neww = StringStart(v);\
	    while((*neww++ = *old++))\
		if (neww == (char *) TG) {\
		    TG++; Check_Gc;\
		}\
	    Set_Buffer_Size((v).ptr, neww - StringStart(v));\
	}


#define Copy_Bytes(dest, source, len) \
	{   register char *dp = dest;\
	    register char *sp = source;\
	    register long ctr = len;\
	    while (ctr-- > 0) *dp++ = *sp++;\
	}

/*
 *	D O U B L E S
 */

#ifdef UNBOXED_DOUBLES

#define Dbl(v)	((v).dbl)

#define	Make_Double_Val(v, /* double */ dexpr) \
	(v).dbl = (dexpr);

#define Make_Checked_Double_Val(v, dexpr) { \
	double _d = (dexpr); \
	Check_Float_Exception(_d); \
	(v).dbl = _d; \
    }

#else

#define Dbl(v)	(*((double *) (((v).ptr)+1)))

/* CAUTION: read input before storing output - may be same location */
#define	Make_Double_Val(v, /* double */ dexpr) {		\
	double _d = (dexpr);					\
	(v).ptr = TG;						\
	Push_Buffer(sizeof(double));				\
	*((double *) BufferStart((v).ptr)) = _d;		\
    }

#define Make_Checked_Double_Val(v, dexpr) {			\
	double _d = (dexpr);					\
	Check_Float_Exception(_d);				\
	(v).ptr = TG;						\
	Push_Buffer(sizeof(double));				\
	*((double *) BufferStart((v).ptr)) = _d;		\
    }

#endif

 
/*
 * Check a float/double and raise an exception for NaN.
 * CAUTION: argument may be expanded twice!
 */
#ifdef HAVE_ISNAN
#  ifdef _WIN32
#    define GoodFloat(x)	(!_isnan(x))
#  else
#    define GoodFloat(x)	(!isnan(x))
#  endif
#else
#  define GoodFloat(x)		((x)==(x))	/* fails for NaN */
#endif

#define Check_Float_Exception(x) \
	{ if (!GoodFloat(x)) {Bip_Error(ARITH_EXCEPTION);} }


/*
 * Portable check for floating-point finite-ness
 */

#ifndef HAVE_FINITE
#  ifdef HAVE_ISINF
#    define finite(f)	(!isinf(f))
#  else
#    define finite(f)	((f)==0.0 || (f)+(f)!=(f)) /* arg multiply expanded! */
#  endif
#endif


/*
 * Macros for comparing doubles while distinguishing
 * negative and positive zeros. C's == doesn't do that!
 */

#define PedanticEq(d1,d2) \
 	((d1) == (d2) && ((d1) != 0.0 || PedanticZeroEq(d1,d2)))

#define PedanticZeroEq(d1,d2) \
 	(1.0/(d1) == 1.0/(d2))

#define PedanticLess(d1,d2) \
 	((d1) < (d2) ||  ((d1) == (d2) && PedanticZeroLess(d1,d2)))

#define PedanticGreater(d1,d2) \
	PedanticLess(d2,d1)

#define PedanticZeroLess(d1,d2) \
 	(1.0/(d1) < 1.0/(d2))


/*
 * on Solaris, atan2() gives incorrect results with negative zeros
 */
#ifdef __sun__
#define Atan2(y,x) ((x)==0.0 && (y)==0.0 ? atan2(y,1/(x)) : atan2(y,x))
#else
#define Atan2(y,x) atan2(y,x)
#endif


/*
 * Double Intervals
 */

#define IvlLwb(pw)	(*((double *) ((pw)+1)))
#define IvlUpb(pw)	(*((double *) ((pw)+1) + 1))

#define GoodInterval(pw) (!PedanticGreater(IvlLwb(pw), IvlUpb(pw)))
#define RawInterval(pw) ((pw)->tag.kernel & RAW_IVL)

#define Check_Interval_Exception(x) \
	{ if (!GoodInterval(x)) { Bip_Error(ARITH_EXCEPTION);} }

#define Push_Interval(pw,from,to) {		\
	(pw) = TG;				\
	Push_Buffer(2*sizeof(double));		\
	IvlLwb(pw) = from;			\
	IvlUpb(pw) = to;			\
}

#define Push_Checked_Interval(pw,from,to) {	\
	Push_Interval(pw, from, to);		\
	Check_Interval_Exception(pw);		\
}

#define Mark_Interval_Raw(pw)			\
	(pw)->tag.kernel |= RAW_IVL;

#define Unmark_Interval_Raw(pw)			\
	(pw)->tag.kernel &= ~RAW_IVL;


/****************************************************************/
/*	Handles to external data				*/
/****************************************************************/

#define Check_Typed_Object_Handle(v, t, expected_class) {	\
	Check_Type((t), THANDLE);				\
	Check_Type((v).ptr->tag, TEXTERN);			\
	if (ExternalClass((v).ptr) != (expected_class))		\
	    { Bip_Error(TYPE_ERROR); }				\
}

#define Get_Typed_Object(v, t, expected_class, obj) {		\
	Check_Typed_Object_Handle(v, t, expected_class);	\
	(obj) = ExternalData((v).ptr);				\
	if (!(obj)) { Bip_Error(STALE_HANDLE); }		\
}

#define ExternalClass(h)	((t_ext_type*) (h)[0].val.ptr)
#define ExternalData(h)		((generic_ptr) (h)[1].val.ptr)

#define HANDLE_ANCHOR_SIZE	2


/****************************************************************/
/*	Timestamps to optimize trailing				*/
/****************************************************************/

#define Init_Stamp(p) \
    (p)->val.ptr = TG_ORIG ; (p)->tag.kernel = TREF;

#define Make_Stamp(p) \
    (p)->val.ptr = GB ; (p)->tag.kernel = TREF;

#define OldStamp(p) \
    ((p)->val.ptr < GB)

#define Trail_Needed(p) \
    ((pword *)(p) < GB)


/* The context in which an undo function is being called */

#define UNDO_FAIL		0	/* untrail during fail */
#define UNDO_GC			1	/* untrail during gc */

/* Type of trailed data */

#define TRAILED_PWORD		0x0
#define TRAILED_REF		0x4
#define TRAILED_WORD32		0x8
#define TRAILED_COMP		0xc


/******************************************************************/
/* 								  */
/*		D I C T I O N A R Y      macros/function	  */
/*								  */
/******************************************************************/

#define Did(string, arity)	ec_did(string, arity)
#define DidString(d)		(((dident)(d))->string)
#define	DidLength(D)		DidString(D)->val.nint

/*
 * Get the name of v into name.
 * Must be instantiated to an atom or string otherwise
 * appropriate error is returned.
 *	value	v;
 *	type	t;
 *	char	*name;
 */
#define Get_Name(v,t,name)				\
	if (IsString(t))				\
		name = StringStart(v);			\
	else if (IsAtom(t))				\
		name = DidName(v.did);			\
	else if (IsNil(t))				\
		name = DidName(d_.nil);			\
	else						\
	{						\
		Error_If_Ref(t)				\
		Bip_Error(TYPE_ERROR)			\
	}


/*
 * This macro converts the procedure ID in the form Name/Arity
 * to its DID. It makes all tests.
 *	value	v;
 *	type	t;
 *	dident	wdid;
 */
#define Get_Proc_Did(v, t, wdid) 			\
	if (IsStructure(t) && v.ptr->val.did == d_.quotient)\
	{						\
		pword * pw;				\
		pw = v.ptr + 1;				\
		Dereference_(pw)			\
		Check_Atom_Or_Nil(pw->val, pw->tag)	\
		wdid = pw->val.did;			\
		pw = v.ptr + 2;				\
		Dereference_(pw)			\
		Check_Integer(pw->tag)			\
		if (pw->val.nint < 0 || pw->val.nint > MAXARITY) \
		    { Bip_Error(RANGE_ERROR) }          \
		wdid = add_dict(wdid, (int) pw->val.nint);\
	}						\
	else						\
	{						\
		Error_If_Ref(t);			\
		Bip_Error(TYPE_ERROR);			\
	}


/*
 * Get the did of a term Name/Arity or Name (== Name/0)
 */

#define Get_Functor_Did(v, t, wdid) 			\
	if (IsStructure(t) && v.ptr->val.did == d_.quotient)\
	{						\
		pword * pw;				\
		pw = v.ptr + 1;				\
		Dereference_(pw)			\
		Check_Atom_Or_Nil(pw->val, pw->tag)	\
		wdid = pw->val.did;			\
		pw = v.ptr + 2;				\
		Dereference_(pw)			\
		Check_Integer(pw->tag)			\
		wdid = add_dict(wdid, (int) pw->val.nint);\
	}						\
	else if (IsAtom(t))				\
		wdid = v.did;				\
	else if (IsNil(t))				\
		wdid = d_.nil;				\
	else						\
	{						\
		Error_If_Ref(t);			\
		Bip_Error(TYPE_ERROR);			\
	}


/*
 * Get the did of an atom or structure, else type error
 */

#define Get_Key_Did(key,v,t)		       	\
	Error_If_Ref(t)				\
	if(IsAtom(t)) key = (v).did;		\
	else if(IsStructure(t)) key = (v).ptr->val.did;\
	else if(IsNil(t)) key = d_.nil;		\
	else if(IsList(t)) key = d_.list;	\
	else { Bip_Error(TYPE_ERROR) }


/*
 * Get the DID from the name and arity, error if wrong type
 *	value	vname, varity;
 *	type	tname, tarity;
 *	dident	did;
 */
#define Get_Did(vname, tname, varity, tarity, d)		\
	if (IsRef(tname)) {					\
		Bip_Error(INSTANTIATION_FAULT)			\
	}							\
	Check_Integer(tarity)					\
	if (IsNil(tname))					\
	    vname.did = d_.nil;					\
	else if (!IsAtom(tname)) {				\
		Bip_Error(TYPE_ERROR)				\
	}							\
	d = add_dict(vname.did, (int) varity.nint);


/******************************************************************/
/* 								  */
/*	Protecting code sequences on C level			  */
/*								  */
/******************************************************************/

/*
 * Disable interrupts (now in shared_mem.h)
 */

/*
 * Protect code against being aborted via exit_block/1, i.e. allow
 * interrupt handling, but don't allow the handler to abort the
 * interrupted execution.
 */

#define Disable_Exit()	VM_FLAGS |= NO_EXIT;

#define Enable_Exit() \
	{ if (VM_FLAGS & WAS_EXIT) delayed_exit(); else VM_FLAGS &= ~NO_EXIT; }


/****************************************************************/
/*	I / O							*/
/****************************************************************/

#define Current_Input		ec_stream_id(0)	/* current_input_ */
#define Current_Output		ec_stream_id(1)	/* current_output_ */
#define Current_Error		ec_stream_id(2)	/* current_err_ */
#define Current_Null		ec_stream_id(3)	/* null_ */


/****************************************************************/
/*	Global Shared Data             				*/
/****************************************************************/
/* The funny casts like    * (type *) &object
 * are needed to convince the C compiler that you can use
 * these things as a left hand side of an assignment.
 */

#define SharedDataLock		(shared_data->general_lock)
#define ModuleLock		(shared_data->mod_desc_lock)
#define PropertyLock		(shared_data->prop_desc_lock)
#define PropListLock		(shared_data->prop_list_lock)
#define ProcedureLock		(shared_data->proc_desc_lock)
#define ProcListLock		(shared_data->proc_list_lock)
#define ProcChainLock		(shared_data->proc_chain_lock)
#define AssertRetractLock	(shared_data->assert_retract_lock)
#define GlobalFlags		(shared_data->global_flags)
#define PrintDepth		(shared_data->print_depth)
#define LoadReleaseDelay	(shared_data->load_release_delay)
#define PublishingParam		(shared_data->publishing_param)
#define OutputModeMask		(shared_data->output_mode_mask)
#define CompileId		(shared_data->compile_id)
#define CodeHeapUsed		(shared_data->code_heap_used)
#define GlobalVarIndex		(shared_data->global_var_index)
#define SymbolTableVersion	(shared_data->symbol_table_version)
#define DynGlobalClock		(shared_data->dyn_global_clock)
#define DynKilledCodeSize	(shared_data->dyn_killed_code_size)
#define DynNumOfKills		(shared_data->dyn_num_of_kills)
#define AbolishedDynProcedures	(*(proc_duet **) &shared_data->abolished_dynamic_procedures)
#define AbolishedProcedures	(*(proc_duet **) &shared_data->abolished_procedures)
#define DynamicProcedures	(*(proc_duet **) &shared_data->dynamic_procedures)
#define GlobalProcedures	(*(proc_duet **) &shared_data->global_procedures)
#define CompiledStructures	(*(proc_duet **) &shared_data->compiled_structures)
#define NbStreams		(shared_data->nbstreams)
#define StreamDescriptors	(*(stream_id **) &shared_data->stream_descriptors)
#define ErrorHandler		(*(pri ***) &shared_data->error_handler)
#define DefaultErrorHandler	(*(pri ***) &shared_data->default_error_handler)
#define InterruptHandler	(*(pri ***) &shared_data->interrupt_handler)
#define InterruptHandlerFlags	(*(int **) &shared_data->interrupt_handler_flags)
#define InterruptName		(*(dident **) &shared_data->interrupt_name)
#define UserError		(shared_data->user_error)
#define ErrorMessage		(*(char ***) &shared_data->error_message)
#define MaxErrors		(shared_data->max_errors)


/****************************************************************/
/*      Global references                                       */
/****************************************************************/

/* If GLOBALREFS_ARE_ECREFS is defined, global references are implemented
 * with ec_refs.  They are on the heap and there is no limit on their
 * number.  This does not work with the parallel system because of
 * heap->stack pointers!
 */
#define GLOBALREFS_ARE_ECREFS

/* Otherwise, the global references are stored in the GLOBVAR array,
	accessed by the index. The unused items are linked using
	the index, the start of this list is stored in the last
	array element. List end is marked by TNIL.
*/
#ifdef GLOBALREFS_ARE_ECREFS
#define GLOBAL_VARS_NO		10	/* Size of the global variables array */
#else
#define GLOBAL_VARS_NO		100	/* Size of the global variables array */
#endif
#define GLOBAL_VARS_LAST	(GLOBAL_VARS_NO-1)
#define GlobalVarFree		GLOBVAR[GLOBAL_VARS_NO-1]


/****************************************************************/
/*      Message passing                                         */
/****************************************************************/

#define HALT1_APORT_NUMBER     0
#define HALT2_APORT_NUMBER     1
#define WM_APORT_NUMBER        2
#define NUM_STD_PORTS          3

#define SCH_APORT_NUMBER        (NUM_STD_PORTS + 0)
#define ENG_APORT_NUMBER        (NUM_STD_PORTS + 1)
#define IO_APORT_NUMBER         (NUM_STD_PORTS + 2)
#define IO_REPLY_APORT_NUMBER   (NUM_STD_PORTS + 3)

#define TOTAL_APORT_NUMBER      (NUM_STD_PORTS + 4)


/****************************************************************/
/*	Abstract machine registers				*/
/****************************************************************/

#define	SP	g_emu_.sp
#define	TT	g_emu_.tt
#define	TG	g_emu_.tg
#define	E	g_emu_.e
#define	EB	g_emu_.eb
#define	GB	g_emu_.gb
#define	S	g_emu_.s
#define	B	g_emu_.b
#define	PPB	g_emu_.ppb
#define	PB	g_emu_.pb
#define	ORA	g_emu_.oracle
#define	NTRY	g_emu_.ntry
#define	LEAF	g_emu_.leaf
#define	LOAD	g_emu_.load
#define	GCTG	g_emu_.gctg
#define	ORC	g_emu_.oracle
#define	PP	g_emu_.pp

#define	TD		g_emu_.trace_data.debug_top.val.ptr
#define	TAGGED_TD	g_emu_.trace_data.debug_top
#define	NINVOC		g_emu_.trace_data.next_invoc
#define	RLEVEL		g_emu_.trace_data.redo_level
#define	FDROP		g_emu_.trace_data.fail_drop
#define	FCULPRIT	g_emu_.trace_data.fail_culprit
#define	JMININVOC	g_emu_.trace_data.min_invoc
#define	JMAXINVOC	g_emu_.trace_data.max_invoc
#define	JMINLEVEL	g_emu_.trace_data.min_level
#define	JMAXLEVEL	g_emu_.trace_data.max_level
#define	PORTFILTER	g_emu_.trace_data.port_filter
#define	FTRACE		g_emu_.trace_data.fail_trace
#define	TRACEMODE	g_emu_.trace_data.trace_mode
#define	DBG_PRI		g_emu_.trace_data.call_proc
#define	DBG_PORT	g_emu_.trace_data.call_port
#define	DBG_INVOC	g_emu_.trace_data.call_invoc
#define	DBG_DELAY_INVOC	g_emu_.trace_data.first_delay_invoc
#define DBG_SRCPOS	g_emu_.trace_data.source_pos
#define DBG_PATH        g_emu_.trace_data.source_pos.file
#define DBG_LINE        g_emu_.trace_data.source_pos.line
#define DBG_FROM        g_emu_.trace_data.source_pos.from
#define DBG_TO          g_emu_.trace_data.source_pos.to

#define	LCA	g_emu_.lca
#define	VM_FLAGS	g_emu_.vm_flags
#define	EVENT_FLAGS	g_emu_.event_flags
#define	DE	g_emu_.de
#define	LD	g_emu_.ld
#define	MU	g_emu_.mu
#define	SV	g_emu_.sv
#define WP	g_emu_.wp
#define WP_STAMP	g_emu_.wp_stamp
#define WL	g_emu_.wl.val.ptr
#define TAGGED_WL	g_emu_.wl
#define TO	g_emu_.oracle
#define FO	g_emu_.followed_oracle
#define PO	g_emu_.pending_oracle
#define	OCB	g_emu_.occur_check_boundary
#define	TCS	g_emu_.top_constructed_structure
#define	TG_SL	g_emu_.tg_soft_lim
#define	TG_SLS	g_emu_.tg_soft_lim_shadow
#define	IFOFLAG	g_emu_.irq_faked_overflow
#define	TG_SEG	g_emu_.segment_size
#define	TG_LIM	g_emu_.tg_limit
#define	TT_LIM	g_emu_.tt_limit
#define	TG_ORIG	((pword *) g_emu_.global_trail[0].start)
#define	TT_ORIG	((pword **) g_emu_.global_trail[1].start)
#define	B_ORIG	((pword *) g_emu_.control_local[0].start)
#define	SP_ORIG	((pword *) g_emu_.control_local[1].start)
#define	IT_BUF	g_emu_.it_buf
#define	PARSENV	g_emu_.parser_env
#define POSTED  g_emu_.posted
#define POSTED_LAST  g_emu_.posted_last
#define	GLOBVAR	(g_emu_.global_variable+1)
#define	A	g_emu_.emu_args
#define PostponedList	g_emu_.postponed_list


/*
 * The following are obsolete, to be phased out (use macros above instead)
 */

#define Gbl_Tg	g_emu_.tg
#define Gbl_Tt	g_emu_.tt


/****************************************************************/
/* The bits in GlobalFlags (shared memory flags)		*/
/* CAUTION: These values also occur in environment.pl		*/
/****************************************************************/

#define BREAL_EXCEPTIONS 0x00000001 /* undecidable breal comparisons	*/
#define PREFER_RATIONALS 0x00000002 /* use rationals where possible	*/
#define HEAP_READY	0X00000004 /* for synchronising worker booting	*/
#define SCH_TRACE_FLAG	0x00000008 /* parallel scheduler trace		*/
#define ENG_TRACE_FLAG	0X00000020 /* parallel engine trace		*/
#define GC_ENABLED	0X00000010 /* the garbage collector is switched on */
#define GC_VERBOSE	0X00000040 /* garbage collections are reported	*/
#define GC_NO_CHP	0X00000200 /* don't allow the gc to make chps 	*/
#define GC_ADAPTIVE	0X00004000 /* automatically adjust gc intervals	*/
#define DBGCOMP		0X00000080 /* compiler generates debug instrs	*/
#define CORTN		0X00000100 /* built-ins delay			*/
#define MACROEXP        0X00000400 /* macro transformations enabled	*/
#define GOALEXPAND	0X00000800 /* goal transformation enabled	*/
#define FULL_COPY	0X00001000 /* disable incremental stack copying	*/
#define CHECK_COPY      0X00002000 /* stack copying debug facility	*/
#define SCH_SYNC_ONLY	0X00008000 /* no async scheduler msg handling	*/
#define DFID_COMPILE	0X01000000 /* depth-first iterative deepening	*/
#define OCCUR_CHECK	0X02000000 /* occurs check enabled		*/
#define VARIABLE_NAMES	0X04000000 /* keep variable names		*/
#define SINGLETON_CHECK	0X08000000 /* compiler warns on singletons	*/
#define STRIP_VARIABLES	0X10000000 /* print all variables as _g		*/


/****************************************************************/
/* The bits in EVENT_FLAGS (per engine)				*/
/* EVENT_FLAGS may be changed by signal handlers and must	*/
/* only be updated inside interrupt-protected regions		*/
/****************************************************************/

#define SCH_MSG_PENDING	0X00000001 /* scheduler message pending		*/
#define ENG_MSG_PENDING	0X00000004 /* engine message pending		*/
#define SLEEP_REQUEST	0X00000002 /* engine should go to sleep		*/
#define COUNT_DOWN	0X00000008 /* countdown running			*/
#define EVENT_POSTED	0X00000010 /* events in posted_events-queue	*/
#define DEL_IRQ_POSTED	0X00000020 /* maybe delayed irq among events	*/
#define SYNC_MSG_PENDING (SCH_MSG_PENDING|ENG_MSG_PENDING)


/****************************************************************/
/* The bits in VM_FLAGS (per engine)				*/
/****************************************************************/

#define EVENTS_DEFERRED	0X00000001 /* sync event handling is suppressed	*/
#define GLOBAL_NO_IT	0X00000002 /* interrupts are disabled at Prolog level */
				   /* (only set together with it_disabled_ !) */
#define TRACE		0X00000008 /* we are tracing VM instructions	*/
#define ORACLES_ENABLED	0X00000010 /* record oracles during execution	*/
#define STATISTICS	0X00000020 /* we are counting VM instructions	*/
#define STAT_PAIRS	0X00000800 /* we are counting pairs of VM instr. */
#define PROFILING	0x00001000
#define NO_EXIT		0X04000000 /* exit_block is forbidden		*/
#define WAS_EXIT	0X08000000 /* an exit_block has been delayed	*/
#define FP_EXCEPTION	0X10000000 /* floating point exception		*/
#define EXPORTED	0X40000000 /* registers have been globalized	*/
#define DET		0X80000000 /* no choicepoint			*/

#define INT_SAFE_BITS	0	/* mask to be saved/restored on interrupts */


/****************************************************************/
/* Values for the interrrupt_handler_flags_ array		*/
/****************************************************************/

#define IH_UNCHANGED	0	/* (fail) */
#define IH_SYSTEM_DFL	1	/* default/0 */
#define IH_IGNORE	2	/* true/0 */
#define IH_ECLIPSE_DFL	3	/* internal/0 */
#define IH_POST_EVENT	4	/* event/1 */
#define IH_THROW	5	/* throw/1 */
#define IH_ABORT	6	/* abort/0 */
#define IH_HALT		7	/* halt/0 */
#define IH_HANDLE_ASYNC	8	/* other */


/****************************************************************/
/*	struct tag_descriptor related definitions		*/
/****************************************************************/

#define tdict	tag_desc[TDICT].tag
#define tlist	tag_desc[TLIST].tag
#define tcomp	tag_desc[TCOMP].tag
#define tstrg	tag_desc[TSTRG].tag
#define tint	tag_desc[TINT].tag

#define ARITH_PLUS		0
#define ARITH_NEG		1
#define ARITH_ABS		2
#define ARITH_ADD		3
#define ARITH_SUB		4
#define ARITH_MUL		5
#define ARITH_DIV		6
#define ARITH_IDIV		7
#define ARITH_MOD		8
#define ARITH_POW		9
#define ARITH_MIN		10
#define ARITH_MAX		11
#define ARITH_FLOOR		12
#define ARITH_FIX		13
#define ARITH_FLOAT		14
#define ARITH_ROUND		15
#define ARITH_COM		16
#define ARITH_AND		17
#define ARITH_OR		18
#define ARITH_XOR		19
#define ARITH_SHR		20
#define ARITH_SHL		21
#define ARITH_SIN		22
#define ARITH_COS		23
#define ARITH_TAN		24
#define ARITH_ASIN		25
#define ARITH_ACOS		26
#define ARITH_ATAN		27
#define ARITH_EXP		28
#define ARITH_LN		29
#define ARITH_SQRT		30
#define ARITH_NUM		31
#define ARITH_DEN		32
#define ARITH_SGN		33
#define ARITH_CEIL		34
#define ARITH_SETBIT		35
#define ARITH_CLRBIT		36
#define ARITH_GETBIT		37
#define ARITH_CHGSIGN		38
#define ARITH_BOXLONGLONG	39
#define ARITH_TOCLONGLONG	40
#define ARITH_NICERAT		41
#define ARITH_GCD		42
#define ARITH_LCM		43
#define ARITH_POWM		44
#define ARITH_NEXT		45
#define ARITH_PREV		46
#define ARITH_FLOORDIV		47
#define ARITH_FLOORREM		48
#define ARITH_ATAN2		49
#define ARITH_TRUNCATE		50
#define ARITH_INT		51
#define ARITH_GCD_EXT		52
/* Keep this definition in ec_public.h up-to-date: */
/* #define ARITH_OPERATIONS	53 */


/****************************************************************/
/*	Shorthands/aliases for global data structures		*/
/****************************************************************/

#define tag_desc		(ec_.td)
#define d_			(ec_.d)
#define shared_data		(ec_.shared)
#define g_emu_			(ec_.m)

/****************************************************************/
/*		Static / Dynamic event queue limits		*/
/****************************************************************/

#define MAX_STATIC_EVENT_SLOTS 		32
#define MIN_DYNAMIC_EVENT_SLOTS 	32
#define DYNAMIC_EVENT_Q_SHRINK_FACTOR	2
