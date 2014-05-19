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
 * Copyright (C) 2001-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Warwick Harvey, IC-Parc
 * 
 * END LICENSE BLOCK */
/*--------------------------------------------------------------------
**
** IC low-level C functions.
**
** System:       ECLiPSe Constraint Logic Programming System
** Author/s:     Warwick Harvey, IC-Parc
** 
** $Id: ic.c,v 1.2 2008/06/20 13:41:14 jschimpf Exp $
**
** This file provides low-level primitives in support of the ic_kernel and
** ic_constraints ECLiPSe modules.  This is almost entirely for efficiency
** reasons; there is very little here that couldn't have been done in
** ECLiPSe.
**
**-------------------------------------------------------------------*/

/*
** TODO:
**  - Do something about the dereferences?  E.g. assume nobody else is
**    tampering with the structures, etc.?
**  - Implement more special cases/optimisations.
**  - Probably lots of stuff.  :)
*/


#if 0
#define IC_DEBUG
#include <stdio.h>
#endif

#undef USE_BOUND_SET_SHORTCUT

/*----------------------------------------------------------------------
**
** Load some needed header files.
**
*/
#include <math.h>
#include <string.h>

#include "external.h"
#include "error.h"
#include "bitmap.h"
#include "intervals.h"
#include "rounding_control.h"
#include "emu_export.h"


/*----------------------------------------------------------------------
**
** Define some useful constants and macros.
**
*/

#define min(x,y) ((x) < (y) ? (x) : (y))
#define max(x,y) ((x) > (y) ? (x) : (y))

    /*
    ** This is for Windows' benefit, but should be fine for all other
    ** architectures as well.
    */
#define NEG_ZERO    (-1 * 0.0)

    /*
    ** Return the preferred waking priority for the given constraint.
    */
#define ConstraintPriority(con)		((con)->term_count < 3 ? 3 : 4)

/*
** Offsets (in pwords) of the various fields in the ic var structure.
**
** Assumes the ECLiPSe type looks like this:
**
**	ic(
**	    var_type,	% atom: 'integer' or 'float' [or 'expression'?]
**	    lo,		% float: lower bound
**	    hi,		% float: upper bound
**	    bitmap,	% 'undefined' or bitmap of integer domain
**	%   subst,	% substitution
**	%   subst_occ,	% list of substitution occurrences?
** XXX	    variable,	% the variable this attribute belongs to
**	    min,	% suspensions: wake on update of lo
**	    max,	% suspensions: wake on update of hi
**	    hole,	% suspensions: wake on new hole in domain
**	%   any		% suspensions: wake on any domain change
**	%   subst	% suspensions: wake on substitution
**	    type	% suspensions: wake on type change (float -> int)
**	)
**
** Note that the variable field is not needed if the constraints don't
** retain references to variable attributes.
*/

#define	OFF_TYPE	1
#define	OFF_LO		2
#define	OFF_HI		3
#define	OFF_BITMAP	4
#ifdef IC_VARS_AS_ATTRS
#define	OFF_VARIABLE	5
#define	OFF_WAKE_LISTS	6
#else
#define	OFF_WAKE_LISTS	5
#endif
#define	OFF_WAKE_LO	OFF_WAKE_LISTS
#define	OFF_WAKE_HI	(OFF_WAKE_LISTS + 1)
#define	OFF_WAKE_HOLE	(OFF_WAKE_LISTS + 2)
#define	OFF_WAKE_TYPE	(OFF_WAKE_LISTS + 3)
#define	ATTR_ARITY	OFF_WAKE_TYPE

#ifdef IC_VARS_AS_ATTRS
#define Fill_Var_Field(pw, var)	Make_Ref(&pw[OFF_VARIABLE], var)
#else
#define Fill_Var_Field(pw, var)
#endif


/* Offset of 'constrained' list in 'suspend' attribute, from emu_c_env.c. */
#define CONSTRAINED_OFF		2


/*
** Create an IC attribute except the reference to the variable.
*/
#define Create_Default_IC_Attr(attr) \
	    create_ic_attr(&attr, -HUGE_VAL, HUGE_VAL, 0)


/*
** Retrieve the IC attribute of a variable, assuming the variable has
** attributes.  'var' must be dereferenced and 'IsMeta'.
*/   
#define IC_Var_Get_Attr(var, attr) {				\
	    (attr) = MetaTerm(var);				\
	    Dereference(attr);					\
	    (attr) = (attr)->val.ptr + ic_domain_slot;		\
	    Dereference(attr);					\
	}

/*
** Retrieve the "contents" of the IC attribute of a variable (i.e. the
** vector containing the data, rather than the TCOMP cell pointing to the
** vector).  'var' must be dereferenced and have a proper IC attribute.
*/   
#define IC_Var_Get_Attr_Vec(var, attr) {	\
	    IC_Var_Get_Attr(var, attr)		\
	    (attr) = (attr)->val.ptr;		\
	    Dereference(attr);			\
	}

/*
** Retrieve the "contents" of the IC attribute of a variable (i.e. the
** vector containing the data, rather than the TCOMP cell pointing to the
** vector), creating the IC attribute if it doesn't exist.  'var' must be
** dereferenced and 'IsRef'.
*/   
#define IC_Var_Attr_Vec(var, attr) {	\
	    (attr) = make_ic_var_attr(var);	\
	    (attr) = (attr)->val.ptr;	\
	    Dereference(attr);		\
	}

/*
** Macros for the IC constraint structure.
**
** Assumes the ECLiPSe type looks like this:
**
**	ic_con(
**	    data,	% flags, RHS constant, etc. in buffer struct
**	    boolean,	% reification boolean
**	    lo_vector,	% vector containing lower bounds of term coefficients
**	    hi_vector,	% vector containing upper bounds of term coefficients
**	    var_vector,	% vector containing term variables
**	    susp	% the constraint suspension
**	)
*/

#define	CON_OFF_DATA		1
#define	CON_OFF_BOOLEAN		2
#define	CON_OFF_LO_VEC		3
#define	CON_OFF_HI_VEC		4
#define	CON_OFF_VAR_VEC		5
#define	CON_OFF_SUSP		6
#define CON_ARITY		6


/*
** Macros for dealing with buffer structure used to store "constant" data
** for a constraint.  We could store the flags and (optional) term count in
** the same word, but since the buffer will take up an even number of pwords
** anyway, we might as well store them separately.
**
**	|-------------------------------|
**	|                               |
**	| - - - - - - - - - - - - - - - |
**	| RHS upper bound (double)      |
**	|-------------------------------|
**	|                               |
**	| - - - - - - - - - - - - - - - |
**	| RHS lower bound (double)      |
**	|-------------------------------|
**	| Term count                    |
**	|-------------------------------|
**	| Flags                         |
**	|-------------------------------|
**	| TBUFFER                       |
**	|-------------------------------|
**	| Buffer size                   |
**	|-------------------------------|
*/

    /* Offsets from the start of the buffer, in words. */
#define	CON_DATA_OFF_FLAGS	0
#define	CON_DATA_OFF_COUNT	1
#define	CON_DATA_OFF_RHS_LO	2
#define	CON_DATA_OFF_RHS_HI	4
#define	CON_DATA_SIZE		6
#define	CON_DATA_SIZE_BYTES	(CON_DATA_SIZE * sizeof(pword))

    /*
    ** Values for the flags.
    ** Note that the 3 low bit flags must match op_to_flags in
    ** ic_constraints.ecl.  Note also that we prefer the boolean toggle to
    ** be in the low bit.
    */
#define CON_LOWER		0x1
#define CON_UPPER		0x2
#define	CON_INTEGRAL		0x4
#define	CON_INTEGRALITY_PROP	0x8

#define	CON_OP_MASK		(CON_LOWER + CON_UPPER)

    /*
    ** Convert a constraint's flags plus a boolean representing whether the
    ** constraint must be true or false into a single number representing
    ** the constraint operator.
    */
#define FlagsAndBoolToOp(flags, bool)	\
	(((flags) & CON_OP_MASK) ^ (CON_OP_MASK * !(bool)))

#define LOWER_BOUND		CON_LOWER
#define UPPER_BOUND		CON_UPPER

#define	OpIsLowerBound(op)	(((op) & LOWER_BOUND) == LOWER_BOUND)
#define	OpIsUpperBound(op)	(((op) & UPPER_BOUND) == UPPER_BOUND)

#define	OpIsEqual(op)		((op) == LOWER_BOUND + UPPER_BOUND)
#define	OpIsNotEqual(op)	((op) == 0)
#define	OpIsLessEqual(op)	((op) == UPPER_BOUND)
#define	OpIsGreater(op)		((op) == LOWER_BOUND)


    /* Returns a pointer to the data buffer entry at the given word offset. */
#define ConBufEntry(buf, offset)	(&((word *)BufferStart(buf))[offset])

    /*
    ** Fill in the data in a constraint data buffer.
    */
#define Set_Con_Data_Buf(con_info) {					\
	word	*tmp;							\
	tmp = ConBufEntry((con_info)->data, 0);				\
	tmp[CON_DATA_OFF_FLAGS] = (con_info)->flags;			\
	tmp[CON_DATA_OFF_COUNT] = (con_info)->term_count;		\
	*((double *) (tmp + CON_DATA_OFF_RHS_LO)) = (con_info)->c.l;	\
	*((double *) (tmp + CON_DATA_OFF_RHS_HI)) = (con_info)->c.u;	\
    }

#define Allocate_Con_Data_Buf(con_info) {	\
	(con_info)->data = TG;			\
	Push_Buffer(CON_DATA_SIZE_BYTES);	\
    }

    /*
    ** Update the constraint data buffer based on the contents of the
    ** con_info structure, if the data has changed (currently defined to be
    ** if there are now fewer terms than there used to be).  If the buffer
    ** needs to be trailed, create a new buffer for the new data, trail it,
    ** etc.  If it doesn't need to be trailed, just overwrite the contents.
    */
#define Update_Con_Data_Buf_If_Needed(con_info)				\
	if ((con_info)->term_count < (con_info)->old_term_count) {	\
	    int result;							\
	    result = update_con_data_buf(con_info);			\
	    Return_If_Not_Success(result)				\
	}

    /* Returns a pointer to the first coefficient in the coef vector. */
#define CoefVec(pw)	((double *)BufferStart(pw))

    /* Returns a pointer to the first variable in the variable vector. */
#define VarVec(pw)	((pw) + 1)


    /* Codes returned when evaluating propagation short-cuts. */
#define	SHORT_ERROR	-1	/* should never be returned on success */
#define	SHORT_NONE 	0	/* no short-cut */
#define	SHORT_LWB	1	/* set everything to their lower bound */
#define	SHORT_UPB	2	/* set everything to their upper bound */
#define	SHORT_ENTAILED	3	/* constraint is entailed */

/*
** Useful C data structures for managing variables, constraints, etc.
*/

    /* A bounds pair, without type. */
typedef struct bounds {
	double	l, u;
} bounds;

    /* A bounds pair, with type. */
typedef struct typed_bounds {
	bounds  b;
	int	i;
} typed_bounds;

    /* Information about a constraint. */
typedef struct con_info {
	pword	*con;
	pword	*data;
	pword	*bool;	/* So we have access to it during setup. */
			/* May not be valid if not a reified constraint. */
	double	*lo_vec;
	double	*hi_vec;
	pword	*var_vec;
	pword	*susp;	/* Not worthwhile? */
	/* orig? */
	bounds	c;
	int	flags;
	int	term_count, old_term_count;
	int	op;
	int	reified;
} con_info;

    /* Information about a variable. */
typedef struct var_info {
	pword	*var;
	pword	*attr;
	pword	*bitmap;    /* XXX Store "uword" bitmap rather than "pword"? */
	typed_bounds	tb;
	int	prop_int;	/* Prop integrality to the var if appropriate */
	/* Track what's changed, so can update variable / wake just once? */
} var_info;

    /* Information used when propagating a constraint. */
typedef struct prop_info {
	bounds	sum;	/* lwb == neg_e, upb == f */
	word	inf_f_idx, inf_e_idx;
	double	inf_f_bound, inf_e_bound;	/* ? */
	int	num_non_int_vars, non_int_idx;
	int	no_prop;	/* No propagation/entailment will occur. */
				/* Implies all other fields may be invalid. */
} prop_info;

    /*
    ** Note that we leave some fields undefined if it is safe to do so (e.g.
    ** non_int_idx won't be looked at with num_non_int_vars set to 0).
    */
#define Init_Prop_Info(prop)		\
	(prop)->sum.l = NEG_ZERO;	\
	(prop)->sum.u = 0.0;		\
	(prop)->inf_f_idx = -1;		\
	(prop)->inf_e_idx = -1;		\
	(prop)->num_non_int_vars = 0;	\
	(prop)->no_prop = 0;


    /*
    ** We allow bitmaps over a pretty large range, but for convenience we
    ** wish to limit them to numbers which can be accurately represented
    ** both as (machine) integers and doubles.  On 32-bit machines, this
    ** means we're limited by the integers, while on 64-bit machines we're
    ** limited by doubles.
    ** 
    ** Note that we stop a little short of the real limits, in particular so
    ** that "+1" and the like are always safe.
    */
#ifdef DOUBLE_INT_LIMIT

#define	MIN_BITMAP_RANGE	(-DOUBLE_INT_LIMIT >> 1)
#define	MAX_BITMAP_RANGE	((DOUBLE_INT_LIMIT >> 1) - 1)

#else

#define	MIN_BITMAP_RANGE	(-1 << (8 * SIZEOF_WORD - 2))
#define	MAX_BITMAP_RANGE	((1 << (8 * SIZEOF_WORD - 2)) - 1)

#endif


    /* Macro for filling an empty space so it can be garbage collected. */
    /* Fills a gap of n pwords. */
#define Pad(pw, n) 					\
	if ((n) > 1) {					\
	    (pw)->tag.kernel = TBUFFER;			\
	    Set_Buffer_Size(pw, ((n)-1) * sizeof(pword));	\
	} else if ((n) == 1) {				\
	    Make_Nil(pw)				\
	}


/*
** Miscellaneous macros.
*/

#define	Type_Is_Int(type)	((type) == d_.integer0)


/* Round up/down if the type is integer. */
#define	maybe_round_up(integral, val)	\
	if (integral) { val = ceil(val); } else
#define	maybe_round_down(integral, val)	\
	if (integral) { val = floor(val); } else


/*----------------------------------------------------------------------
**
** Auxilliary "functions", implemented as macros.
**
*/

    /*
    ** Return an integer.
    **
    ** NOTE: should only be used when v/t are "fresh" - won't work with old
    ** vars.
    */
#define Return_Integer(v, t, i)		\
	{				\
	    v.ptr->val.nint = i;	\
	    v.ptr->tag.kernel = TINT;	\
	}


    /*
    ** Convert constants to bounds.
    */

#define Constant_To_Lower_Bound(vc, tc, bound)			\
	if (IsDouble(tc)) {					\
	    /* If it's a double here, assume it's accurate. */	\
	    bound = Dbl(vc);					\
	} else {						\
	    value	ivl;					\
	    tag_desc[TagType(tc)].coerce_to[TIVL](vc, &ivl);		\
	    bound = IvlLwb(ivl.ptr);				\
	}

#define Constant_To_Upper_Bound(vc, tc, bound)			\
	if (IsDouble(tc)) {					\
	    /* If it's a double here, assume it's accurate. */	\
	    bound = Dbl(vc);					\
	} else {						\
	    value	ivl;					\
	    tag_desc[TagType(tc)].coerce_to[TIVL](vc, &ivl);		\
	    bound = IvlUpb(ivl.ptr);				\
	}

#define Constant_To_Bounds(vc, tc, lwb, upb, integral)		\
	if (IsDouble(tc)) {					\
	    /* If it's a double here, assume it's accurate. */	\
	    lwb = upb = Dbl(vc);				\
	    integral = 0;					\
	} else {						\
	    value	ivl;					\
	    tag_desc[TagType(tc)].coerce_to[TIVL](vc, &ivl);		\
	    lwb = IvlLwb(ivl.ptr);				\
	    upb = IvlUpb(ivl.ptr);				\
	    integral = (IsInteger(tc) || IsBignum(tc));		\
	}

#define Constant_To_Typed_Bounds(vc, tc, a)		\
	if (IsDouble(tc)) {					\
	    /* If it's a double here, assume it's accurate. */	\
	    a.b.l = a.b.u = Dbl(vc);				\
	    a.i = 0;					\
	} else {						\
	    value	ivl;					\
	    tag_desc[TagType(tc)].coerce_to[TIVL](vc, &ivl);		\
	    a.b.l = IvlLwb(ivl.ptr);				\
	    a.b.u = IvlUpb(ivl.ptr);				\
	    a.i = (IsInteger(tc) || IsBignum(tc));		\
	}


    /*
    ** Macros for conveniently accessing the low-level arithmetic operations
    ** provided by RIA.
    */

#define Add(a, b, z)	\
	ec_i_add((a).l, (a).u, (b).l, (b).u, &(z).l, &(z).u)

#define Sub(a, b, z)	\
	ec_i_sub((a).l, (a).u, (b).l, (b).u, &(z).l, &(z).u)

#define Mul(a, b, z)	\
	ec_i_mul((a).l, (a).u, (b).l, (b).u, &(z).l, &(z).u)

#define Div(a, b, z)	\
	ec_i_div((a).l, (a).u, (b).l, (b).u, &(z).l, &(z).u)

    /*
    ** "Undo" the subtraction of b from a.  Like Add, but the upper and
    ** lower bounds of b have been reversed, which quite conveniently does
    ** what we want...
    */
#define Undo_Sub(a, b, z)	\
	ec_i_add((a).l, (a).u, (b).u, (b).l, &(z).l, &(z).u)


    /*
    ** IC_Var_Info(attr, lwb, upb, ic_type, bitmap)
    **      Convenience macro for extracting the most common information from
    **      an IC attribute.
    */
#define	IC_Var_Info(attr, lwb, upb, ic_type, bitmap)	\
	{					\
	    pword	*tmp;			\
						\
	    tmp = attr + OFF_LO;			\
	    Dereference(tmp);			\
	    lwb = Dbl(tmp->val);		\
						\
	    tmp = attr + OFF_HI;			\
	    Dereference(tmp);			\
	    upb = Dbl(tmp->val);		\
						\
	    tmp = attr + OFF_TYPE;		\
	    Dereference(tmp);			\
	    ic_type = tmp->val.did;		\
						\
	    bitmap = attr + OFF_BITMAP;		\
	    Dereference(bitmap);		\
	}


#define Update_Bitmap(var_info, new_bitmap) {				\
	/* Check whether we need to update the bitmap pointer. */	\
	/* Note the test catches both changed and new bitmaps. */	\
	if ((var_info)->bitmap->val.wptr != (new_bitmap)) {		\
	    value	val;						\
	    type	tag;						\
	    val.wptr = (new_bitmap);					\
	    tag.kernel = TBITMAP;					\
	    ec_assign((var_info)->attr + OFF_BITMAP, val, tag);		\
	    (var_info)->bitmap = (var_info)->attr + OFF_BITMAP;		\
	    Dereference((var_info)->bitmap);				\
	}								\
    }


    /*
    ** Mark a constraint as entailed; i.e. make sure it doesn't get updated
    ** any further or propagated again.
    ** XXX - Assumes the constraint does actually have a suspension set up
    ** which isn't already dead.
    */
#define Mark_Constraint_Entailed(con_info) {				\
	/* Don't bother updating the constraint data structures. */	\
	(con_info)->old_term_count = 0;					\
	/* Kill the constraint. */					\
	Set_Susp_Dead((con_info)->susp->val.ptr);			\
    }

#define Unify_Boolean(bool, value, result) {				\
	pword pw;							\
	pw.tag.kernel = TINT;						\
	pw.val.nint = (value);						\
	(result) = Unify_Pw((bool)->val, (bool)->tag, pw.val, pw.tag);	\
    }


    /* Check for constraints which can be turned into unifications. */
#define Check_Con_Is_Unification(con)					\
	if (OpIsEqual((con)->op) && !(con)->reified &&			\
		(con)->term_count == 2 &&				\
		(con)->c.l == 0.0 && (con)->c.u == 0.0 &&		\
		(con)->lo_vec[0] == (con)->hi_vec[0] &&			\
		(con)->lo_vec[1] == (con)->hi_vec[1] &&			\
		(con)->lo_vec[0] == -(con)->lo_vec[1] &&		\
		(con)->lo_vec[0] != HUGE_VAL &&				\
		(con)->lo_vec[0] != -HUGE_VAL) {			\
	    Mark_Constraint_Entailed(con);				\
	    return Unify_Pw((con)->var_vec[0].val, (con)->var_vec[0].tag, \
		    (con)->var_vec[1].val, (con)->var_vec[1].tag);	\
	}


    /* XXX - this can be used for any value, not just a bound? */
#define Set_Var_To_Value(var_info, bound) {				\
	int	result;							\
	pword	res;							\
									\
	if ((var_info)->prop_int && (bound) == floor(bound)) {		\
	    /* Make it integral before binding. */			\
	    (var_info)->tb.i = 1;					\
	    (var_info)->prop_int = 0;					\
	}								\
	if ((var_info)->tb.i) {						\
	    /* Bind var to integer/bignum. */				\
	    result = ec_double_to_int_or_bignum(bound, &res);		\
	    Return_If_Not_Success(result);				\
	} else {							\
	    /* Bind var to bounded real. */				\
	    Make_Interval(&res, bound, bound);				\
	}								\
	result = Unify_Pw((var_info)->var->val, (var_info)->var->tag, res.val, res.tag);	\
	Return_If_Not_Success(result);					\
	(var_info)->tb.b.l = (bound);					\
	(var_info)->tb.b.u = (bound);					\
    }


#if 0
#define	DoublesIdentical(a, b)	(memcmp(&(a), &(b), sizeof(double)) == 0)
#else
#if SIZEOF_DOUBLE == SIZEOF_WORD
#define	DoublesIdentical(a, b)	(*((word*)&(a)) == *((word*)&(b)))
#elif SIZEOF_DOUBLE == 2*SIZEOF_WORD
#define	DoublesIdentical(a, b)	(*((word*)&(a)) == *((word*)&(b)) && \
				*(((word*)&(a))+1) == *(((word*)&(b))+1))
#else
PROBLEM: Cannot deal with word size SIZEOF_WORD
#endif
#endif

    /*
    ** Note we want positive and negative zeros to compare different for
    ** non-integral constants.
    */
#define Bounds_To_Constant(lo, hi, res) {			\
	if (((lo) == (hi))					\
		&& ((lo) != -HUGE_VAL) && ((lo) != HUGE_VAL)	\
		&& ((lo) == ceil(lo))) {			\
	    /* Looks like an integer, so treat it as one. */	\
	    int result;						\
	    result = ec_double_to_int_or_bignum(lo, res);	\
	    Return_If_Not_Success(result);			\
	} else if (DoublesIdentical(lo, hi)) {			\
	    Make_Double(res, lo);				\
	} else {						\
	    Make_Interval(res, lo, hi);				\
	}							\
    }


    /*
    ** A couple of macros that abstract iterating over an ECLiPSe list of
    ** linear terms.
    **
    ** XXX - Don't bother with some of the type checking if it's not the
    ** first pass over the constraint?
    */
#define BeginIterateLinList(plin, pterm, idx, coef, var)		\
	idx = -1;							\
	while (!IsNil(plin->tag)) {					\
	    Check_Pair(plin->tag);					\
	    idx++;							\
	    pterm = plin->val.ptr;					\
	    Dereference(pterm);						\
	    Check_Structure(pterm->tag);				\
	    /* Ignore the functor. */					\
	    /* Extract the coefficient - use var as temporary. */	\
	    var = pterm->val.ptr + 1;					\
	    Dereference(var);						\
	    Constant_To_Typed_Bounds(var->val, var->tag, coef);		\
	    /* Extract the variable. */					\
	    var = pterm->val.ptr + 2;					\
	    Dereference(var);

#define EndIterateLinList(plin)			\
	    /* Move on to next list cell. */	\
	    plin = plin->val.ptr + 1;		\
	    Dereference(plin);			\
	}


    /*
    ** Global variables for caching values needed for accessing or
    ** creating IC attributes.
    */
static int ic_domain_slot;		/* IC attribute slot */
static int suspend_slot;		/* Suspend attribute slot */
static dident d_ic_attr_functor;	/* IC attribute functor dict entry */
static dident d_ic_con;			/* ic_con/5 dict entry */
static dident d_ic_real;		/* real/0 dict entry */
static dident d_ic_integer;		/* integer/0 dict entry */
static dident d_ic_undefined;		/* undefined/0 dict entry */
static dident d_exclude;		/* exclude/2 dict entry */
static dident d_exclude_range;		/* exclude_range/3 dict entry */
static dident d_prop_ic_con;		/* prop_ic_con/1 dict entry */
static void *proc_exclude;		/* exclude/2 procedure */
static void *proc_exclude_range;	/* exclude_range/3 procedure */
static void *proc_prop_ic_con;		/* prop_ic_con/1 procedure */
static void *proc_infq;			/* =</2 procedure */
static void *proc_supq;			/* >=/2 procedure */


/*----------------------------------------------------------------------
**
** Propagation threshold.
**
** The propagation threshold is used to limit the amount of propagation
** performed when bounds changes are very small.  Basically, for non-integer
** variables, bounds are only changed if the absolute and relative changes
** of the bound exceed the threshold.  See ic_kernel.ecl and ic_design for
** more details and discussion.
**
** We do this in C rather than ECLiPSe since we want convenient access to it
** from C.
*/

    /* The threshold. */
static double threshold = 1e-8;


    /*
    ** get_threshold(?Threshold)
    **      Unify Threshold with the propagation threshold.
    */
int
p_get_threshold(value vthreshold, type tthreshold)
{
	Return_Unify_Double(vthreshold, tthreshold, threshold);
}

    /*
    ** set_threshold(++Threshold)
    **      Set the propagation threshold to Threshold.
    */
int
p_set_threshold(value vthreshold, type tthreshold)
{
	Check_Double(tthreshold);
	threshold = Dbl(vthreshold);

	Succeed
}


/*----------------------------------------------------------------------
**
** Primitives for operating on variables/attributes.
**
*/

    /*
    ** Use the given bounds and type to create an IC attribute except the
    ** reference to the variable.
    */   
void
create_ic_attr(pword **attr, double lwb, double upb, int integral)
{
	pword	*tmp;

	tmp = TG;
	Push_Struct_Frame(d_ic_attr_functor);
	Make_Atom(tmp + OFF_TYPE, integral ? d_ic_integer : d_ic_real);
	Make_Double(tmp + OFF_LO, lwb);
	Make_Double(tmp + OFF_HI, upb);
	Make_Atom(tmp + OFF_BITMAP, d_ic_undefined);
	Make_Nil(tmp + OFF_WAKE_LO);
	Make_Nil(tmp + OFF_WAKE_HI);
	Make_Nil(tmp + OFF_WAKE_HOLE);
	Make_Nil(tmp + OFF_WAKE_TYPE);

	*attr = tmp;
}


/*
** Retrieve the IC attribute of a variable, creating one if it doesn't
** exist.  'var' must be dereferenced and 'IsRef'.
*/   
pword *
make_ic_var_attr(pword *var)
{
	pword	*attr;

	if (IsMeta(var->tag)) {
	    IC_Var_Get_Attr(var, attr);
	    if (IsRef(attr->tag)) {
		pword *pattr;
		Create_Default_IC_Attr(pattr);
		Fill_Var_Field(pw, var);
		Bind_Var(attr->val, attr->tag, pattr, TCOMP);
		notify_constrained(var);
	    }
	} else {
	    pword *pw, *pattr;
	    Create_Default_IC_Attr(pattr);
	    pw = (pword *)add_attribute(var->tag.all,
		    pattr, TCOMP, ic_domain_slot);
	    Fill_Var_Field(pw, var);
	    Bind_Var(var->val, var->tag, pw, TREF);
	    var = pw; /* Implicit Deref(var) */
	    IC_Var_Get_Attr(var, attr);
	}

	return attr;
}


    /*
    ** Use this one if you know it already has an attribute.
    */
void
get_var_info(pword *x, var_info *vi)
{
	pword	*attr;
	dident	ic_type;
	vi->var = x;
	IC_Var_Get_Attr(x, attr);
	attr = attr->val.ptr;
	Dereference(attr);
	vi->attr = attr;
	IC_Var_Info(vi->attr, vi->tb.b.l, vi->tb.b.u, ic_type, vi->bitmap);
	vi->tb.i = Type_Is_Int(ic_type);
	vi->prop_int = 0;
}

    /*
    ** Use this one if it might not have an attribute (it will give it one).
    */
void
make_var_info(pword *x, var_info *vi)
{
	dident	ic_type;
	vi->var = x;
	IC_Var_Attr_Vec(x, vi->attr);
	IC_Var_Info(vi->attr, vi->tb.b.l, vi->tb.b.u, ic_type, vi->bitmap);
	vi->tb.i = Type_Is_Int(ic_type);
	vi->prop_int = 0;
}

void
get_var_info_from_attr(pword *x, pword *attr, var_info *vi)
{
	dident	ic_type;
	vi->var = x;
	vi->attr = attr->val.ptr;
	IC_Var_Info(vi->attr, vi->tb.b.l, vi->tb.b.u, ic_type, vi->bitmap);
	vi->tb.i = Type_Is_Int(ic_type);
	vi->prop_int = 0;
}


    /*
    ** set_type_integral(var_info)
    ** 	    Set the given variable to be integral.  Note that this assumes
    ** 	    that the variable is not already integral.  Note also that the
    ** 	    variable's bounds are not updated, even if they now need
    ** 	    rounding.  However, the constrained and type change wake lists
    ** 	    are notified.
    ** 	    cf. set_integral(), which also updates the bounds.
    */
int
set_type_integral(var_info *vi)
{
	value	val;
	type	tag;
	int	result;

	/* Update the type. */
	val.did = d_.integer0;
	tag.kernel = TDICT;
	ec_assign(vi->attr + OFF_TYPE, val, tag);
	vi->tb.i = 1;

	/* Notify constrained / type change. */
	result = notify_constrained(vi->var);
	Return_If_Not_Success(result)
	return ec_schedule_susps(vi->attr + OFF_WAKE_TYPE);
}

    /*
    ** ic_lwb(var_info, bound)
    **      Given info about a variable (var_info), impose the given lower
    **      bound.
    */
int
ic_lwb(var_info *vi, double bound)
{
	int	result;
	double	abs_delta;
	value	val;
	type	tag;

	if (bound <= vi->tb.b.l) {
	    Succeed
	}

	if (bound > vi->tb.b.u) {
	    Fail
	}

	/*
	** Note that the following rounding will not change the results of
	** the bounds tests made above (since the variable bounds will be
	** integers if rounding occurs here).
	*/
	maybe_round_up(vi->tb.i, bound);

	if (bound == vi->tb.b.u) {
	    Set_Var_To_Value(vi, bound)
	    /* No more to do. */
	    Succeed
	} else /* bound > lwb */ {
	    if (!vi->tb.i) {
		abs_delta = bound - vi->tb.b.l;
		if (abs_delta != HUGE_VAL && (abs_delta <= threshold ||
			abs_delta <= threshold * fabs(vi->tb.b.l))) {
		    /* Bound change is below the threshold, so ignore it. */
		    /*
		    ** XXX - instead of not modifying the bound (so that no
		    ** further propagation occurs), should we instead update
		    ** the bound, and just not wake anything?
		    */
		    /*
		    bound = vi->tb.b.l;
		    status = IC_SLACK;
		    */
		    Succeed
		}
	    } else {
		if (!IsAtom(vi->bitmap->tag)) {
		    word	int_bound;
		    word	new_int_bound;

		    /*
		    ** Note that due to the existence of a bitmap, we can
		    ** assume it's safe to convert between ints and doubles.
		    */

		    /* Check for holes. */
		    int_bound = (word) bound;
		    result = next_greater_member(vi->bitmap->val.wptr,
			    int_bound - 1, &new_int_bound);

		    if (Result_Is_Empty(result)) {
			/* No entry in bitmap. */
			Fail
		    }

		    if (Result_Is_Slack(result)) {
			/* There was a hole at bound. */
			bound = (double) new_int_bound;

			if (bound == vi->tb.b.u) {
			    Set_Var_To_Value(vi, bound)
			    /* No more to do. */
			    Succeed
			}
		    }
		}
	    }
	}

	/* If we get here, then the bound needs to be updated. */

	Make_Checked_Double_Val(val, bound);
	tag.kernel = TDBL;
	ec_assign(vi->attr + OFF_LO, val, tag);
	vi->tb.b.l = bound;

	if (!IsAtom(vi->bitmap->tag)) {
	    /* Need to update the bitmap. */
	    uword	*new_bitmap;

	    result = set_bitmap_lwb(vi->bitmap->val.wptr, (word) bound,
			    &new_bitmap);

	    if (Result_Is_Empty(result)) {
		/* Bound should not be bitmap empty. */
		Bip_Error(EC_EXTERNAL_ERROR);
	    }

	    /* Update the bitmap. */
	    Update_Bitmap(vi, new_bitmap)
	}

	/* Notify constrained. */
	result = notify_constrained(vi->var);
	Return_If_Not_Success(result)

	/* Schedule suspensions (lo). */
	return ec_schedule_susps(vi->attr + OFF_WAKE_LO);
}


    /*
    ** ic_impose_min(?Var, ++Bound)
    **      Applies the lower bound `Bound' to the variable `Var'.
    **      Note that the `Var' must not be ground.
    */
int
p_ic_impose_min(value vvar, type tvar, value vc, type tc)
{
	var_info	vi;
	double	bound;

	Check_Ref(tvar);
	Constant_To_Lower_Bound(vc, tc, bound);

	make_var_info(vvar.ptr, &vi);

	return ic_lwb(&vi, bound);
}


    /*
    ** ic_upb(var_info, bound)
    **      Given info about a variable (var_info), impose the given upper
    **      bound.
    */
int
ic_upb(var_info *vi, double bound)
{
	int	result;
	double	abs_delta;
	value	val;
	type	tag;

	if (bound >= vi->tb.b.u) {
	    Succeed
	}

	if (bound < vi->tb.b.l) {
	    Fail
	}

	/*
	** Note that the following rounding will not change the results of
	** the bounds tests made above (since the variable bounds will be
	** integers if rounding occurs here).
	*/
	maybe_round_down(vi->tb.i, bound);

	if (bound == vi->tb.b.l) {
	    Set_Var_To_Value(vi, bound)
	    /* No more to do. */
	    Succeed
	} else /* bound < upb */ {
	    if (!vi->tb.i) {
		abs_delta = vi->tb.b.u - bound;
		if (abs_delta != HUGE_VAL && (abs_delta <= threshold ||
			abs_delta <= threshold * fabs(vi->tb.b.u))) {
		    /* Bound change is below the threshold, so ignore it. */
		    /*
		    ** XXX - instead of not modifying the bound (so that no
		    ** further propagation occurs), should we instead update
		    ** the bound, and just not wake anything?
		    */
		    /*
		    bound = vi->tb.b.u;
		    status = IC_SLACK;
		    */
		    Succeed
		}
	    } else {
		if (!IsAtom(vi->bitmap->tag)) {
		    word	int_bound;
		    word	new_int_bound;

		    /*
		    ** Note that due to the existence of a bitmap, we can
		    ** assume it's safe to convert between ints and doubles.
		    */

		    /* Check for holes. */
		    int_bound = (word) bound;
		    result = next_smaller_member(vi->bitmap->val.wptr,
			    int_bound + 1, &new_int_bound);

		    if (Result_Is_Empty(result)) {
			/* No entry in bitmap. */
			Fail
		    }

		    if (Result_Is_Slack(result)) {
			/* There was a hole at bound. */
			bound = (double) new_int_bound;

			if (bound == vi->tb.b.l) {
			    Set_Var_To_Value(vi, bound)
			    /* No more to do. */
			    Succeed
			}
		    }
		}
	    }
	}

	/* If we get here, then the bound needs to be updated. */

	Make_Checked_Double_Val(val, bound);
	tag.kernel = TDBL;
	ec_assign(vi->attr + OFF_HI, val, tag);
	vi->tb.b.u = bound;

	if (!IsAtom(vi->bitmap->tag)) {
	    /* Need to update the bitmap. */
	    uword	*new_bitmap;

	    result = set_bitmap_upb(vi->bitmap->val.wptr, (word) bound,
			    &new_bitmap);

	    if (Result_Is_Empty(result)) {
		/* Bound should not be empty. */
		Bip_Error(EC_EXTERNAL_ERROR);
	    }

	    /* Update the bitmap. */
	    Update_Bitmap(vi, new_bitmap)
	}

	/* Notify constrained. */
	result = notify_constrained(vi->var);
	Return_If_Not_Success(result)

	/* Schedule suspensions (hi). */
	return ec_schedule_susps(vi->attr + OFF_WAKE_HI);
}


    /*
    ** ic_impose_max(?Var, ++Bound)
    **      Applies the upper bound `Bound' to the variable `Var'.
    **      Note that the `Var' must not be ground.
    */
int
p_ic_impose_max(value vvar, type tvar, value vc, type tc)
{
	var_info	vi;
	double	bound;

	Check_Ref(tvar);
	Constant_To_Upper_Bound(vc, tc, bound);

	make_var_info(vvar.ptr, &vi);

	return ic_upb(&vi, bound);
}


    /*
    ** ic_impose_bounds(?Var, ++Lwb, ++Upb)
    **      Applies the lower bound `Lwb' and upper bound `Upb' to the
    **      variable `Var'.  Note that the `Var' must not be ground.
    */
int
p_ic_impose_bounds(value vvar, type tvar, value vlwb, type tlwb, value vupb, type tupb)
{
	var_info	vi;
	double	new_lwb, new_upb;
	int	result;

	Check_Ref(tvar);
	Constant_To_Lower_Bound(vlwb, tlwb, new_lwb);
	Constant_To_Upper_Bound(vupb, tupb, new_upb);

	make_var_info(vvar.ptr, &vi);

	result = ic_lwb(&vi, new_lwb);
	Return_If_Not_Success(result);
	return ic_upb(&vi, new_upb);
}


    /*
    ** Create an IC attribute for a variable with the given parameters, or
    ** constrain it if it already exists.  'var' must be dereferenced and
    ** 'IsRef'; the function returns a suitably dereferenced copy when done.
    */   
int
make_constrained_ic_var(pword **pvar, double lwb, double upb, int integral)
{
	pword	*var = *pvar;
	int	result;

	if (IsMeta(var->tag)) {
	    pword *attr;
	    IC_Var_Get_Attr(var, attr);
	    if (IsRef(attr->tag)) {
		pword *pw;
		create_ic_attr(&pw, lwb, upb, integral);
		Fill_Var_Field(pw, var);
		Bind_Var(attr->val, attr->tag, pw, TCOMP);
		notify_constrained(var);
	    } else {
		/* Attribute already exists, so must constrain it. */
		var_info    vi;
		get_var_info_from_attr(var, attr, &vi);
		if (integral && !vi.tb.i) {
		    result = set_type_integral(&vi);
		    Return_If_Not_Success(result)
		    /* Make sure bounds are not left unrounded. */
		    if (lwb <= vi.tb.b.l)
			lwb = ceil(vi.tb.b.l);
		    if (upb >= vi.tb.b.u)
			upb = floor(vi.tb.b.u);
		}
		result = ic_lwb(&vi, lwb);
		Return_If_Not_Success(result)
		result = ic_upb(&vi, upb);
		Return_If_Not_Success(result)

		/* In case now ground. */
		Dereference(var);
		/* var = var->val.ptr; */
	    }
	} else {
	    pword *pw, *attr;
	    create_ic_attr(&attr, lwb, upb, integral);
	    pw = (pword *)add_attribute(var->tag.all,
		    attr, TCOMP, ic_domain_slot);
	    Fill_Var_Field(pw, var);
	    Bind_Var(var->val, var->tag, pw, TREF);
	    var = pw; /* Implicit Deref(var) */
	}

	*pvar = var;
	Succeed
}


    /*
    ** Constrain a variable to be a boolean.
    */
int
p_make_bool(value vbool, type tbool)
{
	if (!IsRef(tbool)) {
	    Check_Integer(tbool);
	    if (vbool.nint < 0 || vbool.nint > 1) {
		Fail
	    }
	} else {
	    /* Make sure it's an IC boolean. */
	    return make_constrained_ic_var(&vbool.ptr, 0.0, 1.0, 1);
	}

	Succeed
}


    /*
    ** set_integral(var_info)
    ** 	    Set the given variable to be integral.  Note that this assumes
    ** 	    that the variable is not already integral.  The variable's
    ** 	    bounds are updated and waking lists woken as appropriate.
    ** 	    cf. set_type_integral(), which does not update the bounds.
    */
int
set_integral(var_info *vi)
{
	double	bnd;
	int	result;

	result = set_type_integral(vi);
	Return_If_Not_Success(result)
	bnd = ceil(vi->tb.b.l);
	if (bnd > vi->tb.b.l) {
	    result = ic_lwb(vi, bnd);
	    Return_If_Not_Success(result)
	}
	bnd = floor(vi->tb.b.u);
	if (bnd < vi->tb.b.u) {
	    result = ic_upb(vi, bnd);
	    Return_If_Not_Success(result)
	}

	Succeed
}


    /*
    ** set_var_integer(?Var)
    **      Constrains the variable `Var' to be integral.  Note that the
    **      variable must not be ground.
    */
int
p_set_var_integer(value vvar, type tvar)
{
	var_info	vi;

	Check_Ref(tvar);

	make_var_info(vvar.ptr, &vi);

	if (!vi.tb.i) {
	    return set_integral(&vi);
	}

	Succeed
}


int
set_up_exclude_delayed_goal(var_info *vi, word exclude)
{
	pword	goal, susp, *pw;
	int	result;
	int	delayed = 0;

	pw = TG;
	Push_Struct_Frame(d_exclude);
	pw[1].val.ptr = vi->var;
	pw[1].tag.kernel = TREF;
	pw[2].val.nint = exclude;
	pw[2].tag.kernel = TINT;
	Make_Struct(&goal, pw);

	result = ec_make_suspension(goal, 3, proc_exclude, &susp);
	if (result == DEBUG_SUSP_EVENT) {
	    delayed = 1;
	    result = PSUCCEED;
	}
	Return_If_Not_Success(result)

	/* Wake on lower/upper bound changes. */
	result = ec_enter_suspension(vi->attr + OFF_WAKE_LO, susp.val.ptr);
	Return_If_Not_Success(result)
	result = ec_enter_suspension(vi->attr + OFF_WAKE_HI, susp.val.ptr);
	Return_If_Not_Success(result)

	return delayed ? DEBUG_SUSP_EVENT : PSUCCEED;
}


    /*
    ** ic_exclude(var_info, exclude)
    **      Excludes the element `exclude' from the domain of the variable.
    **      Note that the variable must be integral and must not be ground.
    **
    **      If the value cannot be excluded (because it would result in a
    **      bitmap which was considered too large, or the bounds are just
    **      too large) then a delayed goal is set up to deal with it later.
    */
int
ic_exclude(var_info *vi, word exclude)
{
	word	int_lwb;
	word	int_upb;
	uword	*bitmap;
	uword	*new_bitmap;
	int	result;
	int	out_of_range = 0;

	if (!vi->tb.i) {
	    Bip_Error(TYPE_ERROR)
	}

	if (vi->tb.b.l >= MIN_BITMAP_RANGE && vi->tb.b.l <= MAX_BITMAP_RANGE) {
	    int_lwb = (word) vi->tb.b.l;
	    if (exclude < int_lwb) {
		Succeed
	    }
	    if (exclude == int_lwb) {
		return ic_lwb(vi, (double) (exclude+1));
	    }
	} else {
	    out_of_range = 1;
	}

	if (vi->tb.b.u >= MIN_BITMAP_RANGE && vi->tb.b.u <= MAX_BITMAP_RANGE) {
	    int_upb = (word) vi->tb.b.u;
	    if (exclude > int_upb) {
		Succeed
	    }
	    if (exclude == int_upb) {
		return ic_upb(vi, (double) (exclude-1));
	    }
	} else {
	    out_of_range = 1;
	}

	if (out_of_range) {
	    return set_up_exclude_delayed_goal(vi, exclude);
	}

	/* int_lwb < exclude < int_upb */

	if (IsAtom(vi->bitmap->tag)) {
	    /* Allocate a new bitmap. */
	    result = create_bitmap(int_lwb, int_upb, &bitmap);
	    Return_If_Not_Success(result)
	} else {
	    bitmap = vi->bitmap->val.wptr;
	}

	result = remove_bitmap_element(bitmap, exclude, &new_bitmap);

	if (Result_Is_Empty(result)) {
	    /* Bitmap is empty, so no solution. */
	    Fail
	}

	if (Result_Is_Change(result)) {
	    Update_Bitmap(vi, new_bitmap)

	    /* Notify constrained. */
	    result = notify_constrained(vi->var);
	    Return_If_Not_Success(result)

	    /* Schedule suspensions (hole). */
	    result = ec_schedule_susps(vi->attr + OFF_WAKE_HOLE);
	    Return_If_Not_Success(result)
	}

	Succeed
}


    /*
    ** ic_exclude(?Var, ++Excl)
    **      Excludes the element `Excl' from the domain of the variable
    **      `Var'.  Note that the variable must be integral and must not be
    **      ground.
    */
int
p_ic_exclude(value vvar, type tvar, value vc, type tc)
{
	var_info	vi;

	Check_Ref(tvar);
	Check_Integer(tc);

	make_var_info(vvar.ptr, &vi);

	return ic_exclude(&vi, vc.nint);
}


int
set_up_exclude_range_delayed_goal(var_info *vi, word int_lwb, word int_upb)
{
	pword	goal, susp, *pw;
	int	result;
	int	delayed = 0;

	pw = TG;
	Push_Struct_Frame(d_exclude_range);
	pw[1].val.ptr = vi->var;
	pw[1].tag.kernel = TREF;
	pw[2].val.nint = int_lwb;
	pw[2].tag.kernel = TINT;
	pw[3].val.nint = int_upb;
	pw[3].tag.kernel = TINT;
	Make_Struct(&goal, pw);

	result = ec_make_suspension(goal, 3, proc_exclude_range, &susp);
	if (result == DEBUG_SUSP_EVENT) {
	    delayed = 1;
	    result = PSUCCEED;
	}
	Return_If_Not_Success(result)

	/* Wake on lower/upper bound changes. */
	result = ec_enter_suspension(vi->attr + OFF_WAKE_LO, susp.val.ptr);
	Return_If_Not_Success(result)
	result = ec_enter_suspension(vi->attr + OFF_WAKE_HI, susp.val.ptr);
	Return_If_Not_Success(result)

	return delayed ? DEBUG_SUSP_EVENT : PSUCCEED;
}


    /*
    ** ic_exclude_range(var_info, int_lwb, int_upb)
    **      Excludes all the elements from `int_lwb' to `int_upb',
    **      inclusive, from the domain of the variable `var_info'.  Note
    **      that the variable must be integral and must not be ground.
    **
    **      If the range cannot be excluded (because it would result in a
    **      bitmap which was considered too large, or the variable's bounds
    **      are just too large) then a delayed goal is set up to deal with
    **      it later.
    */
int
ic_exclude_range(var_info *vi, word int_lwb0, word int_upb0)
{
	word	int_lwb;
	word	int_upb;
	uword	*bitmap;
	uword	*new_bitmap;
	int	result;
	int	out_of_range = 0;

	if (!vi->tb.i) {
	    Bip_Error(TYPE_ERROR)
	}

	if (vi->tb.b.l >= MIN_BITMAP_RANGE && vi->tb.b.l <= MAX_BITMAP_RANGE) {
	    int_lwb = (word) vi->tb.b.l;
	    if (int_upb0 < int_lwb) {
		Succeed
	    }
	    if (int_upb0 > MAX_BITMAP_RANGE) {
		Bip_Error(RANGE_ERROR)
	    }
	    if (int_lwb0 <= int_lwb) {
		return ic_lwb(vi, (double) (int_upb0 + 1));
	    }
	} else {
	    out_of_range = 1;
	}

	if (vi->tb.b.u >= MIN_BITMAP_RANGE && vi->tb.b.u <= MAX_BITMAP_RANGE) {
	    int_upb = (word) vi->tb.b.u;
	    if (int_lwb0 > int_upb) {
		Succeed
	    }
	    if (int_lwb0 < MIN_BITMAP_RANGE) {
		Bip_Error(RANGE_ERROR)
	    }
	    if (int_upb0 >= int_upb) {
		return ic_upb(vi, (double) (int_lwb0 - 1));
	    }
	} else {
	    out_of_range = 1;
	}

	if (out_of_range) {
	    return set_up_exclude_range_delayed_goal(vi, int_lwb0, int_upb0);
	}

	/* int_lwb < int_lwb0 <= int_upb0 < int_upb */

	if (IsAtom(vi->bitmap->tag)) {
	    /* Allocate a new bitmap. */
	    result = create_bitmap(int_lwb, int_upb, &bitmap);
	    Return_If_Not_Success(result)
	} else {
	    bitmap = vi->bitmap->val.wptr;
	}

	result = remove_bitmap_range(bitmap, int_lwb0, int_upb0, &new_bitmap);

	if (Result_Is_Empty(result)) {
	    /* Bitmap is empty, so no solution. */
	    Fail
	}

	if (Result_Is_Change(result)) {
	    Update_Bitmap(vi, new_bitmap)

	    /* Notify constrained. */
	    result = notify_constrained(vi->var);
	    Return_If_Not_Success(result)

	    /* Schedule suspensions (hole). */
	    result = ec_schedule_susps(vi->attr + OFF_WAKE_HOLE);
	    Return_If_Not_Success(result)
	}

	Succeed
}


    /*
    ** ic_exclude_range(?Var, ++Lwb, ++Upb)
    **      Excludes all the elements from `Lwb' to `Upb', inclusive, from
    **      the domain of the variable `Var'.  Note that the variable must
    **      be integral and must not be ground.
    */
int
p_ic_exclude_range(value vvar, type tvar, value vlo, type tlo, value vhi, type thi)
{
	var_info	vi;

	Check_Ref(tvar);
	Check_Integer(tlo);
	Check_Integer(thi);

	make_var_info(vvar.ptr, &vi);

	return ic_exclude_range(&vi, vlo.nint, vhi.nint);
}


    /* Solves  a * x (op) c  */
    /* op: 1 for lower bound, 2 for upper bound, 3 for both. */
int
impose_coef_bounds(int op, int integer_strict, bounds *a, var_info *vi,
	bounds *c)
{
	bounds	res;
	int	neg_coef;
	int	result;

	neg_coef = ((a->u) <= 0.0);
	/* Only propagate if coefficient doesn't span zero. */
	if (neg_coef || ((a->l) >= 0.0)) {
	    Div(*c, *a, res);

	    if (OpIsEqual(op)) {
		result = ic_lwb(vi, res.l);
		Return_If_Not_Success(result)
		result = ic_upb(vi, res.u);
		Return_If_Not_Success(result)
	    } else
	    /* If coef is negative, swap which bounds to impose. */
	    if ((op) == neg_coef + 1) {
		/* Impose lower bound. */
		if ((integer_strict) && (op) == 1 &&
			vi->tb.i && res.l == ceil(res.l)) {
		    set_round_down();
		    res.l += 1;
		    restore_round_mode();
		}
		result = ic_lwb(vi, res.l);
		Return_If_Not_Success(result)
	    } else {
		/* Impose upper bound. */
		if ((integer_strict) && (op) == 1 &&
			vi->tb.i && res.u == floor(res.u)) {
		    set_round_up();
		    res.u -= 1;
		    restore_round_mode();
		}
		result = ic_upb(vi, res.u);
		Return_If_Not_Success(result)
	    }
	}

	Succeed
}

    /*
    ** Solves  a * x = c
    ** Just binds the variable, if possible & appropriate.
    ** solved is a boolean which will be set to zero if the constraint is
    ** not fully resolved (i.e. a delayed goal should be left behind).
    */
int
solve_equal(bounds *a, var_info *vi, bounds *c, int *solved)
{
	bounds	res;
	pword	tmp;
	int	result;

	*solved = 0;
	Div(*c, *a, res);

	if (vi->prop_int && res.l == res.u && res.l == ceil(res.l)) {
	    result = ec_double_to_int_or_bignum(res.l, &tmp);
	    Return_If_Not_Success(result);
	    result = Unify_Pw(vi->var->val, vi->var->tag, tmp.val, tmp.tag);
	    Return_If_Not_Success(result);
	    *solved = 1;
	} else if (vi->tb.i) {
	    if (res.l == res.u) {
		*solved = 1;
	    }
	    res.l = ceil(res.l);
	    res.u = floor(res.u);
	    if (res.l == res.u) {
		result = ec_double_to_int_or_bignum(res.l, &tmp);
		Return_If_Not_Success(result);
		result = Unify_Pw(vi->var->val, vi->var->tag, tmp.val, tmp.tag);
		Return_If_Not_Success(result);
	    } else if (res.l > res.u) {
		Fail
	    } else {
		/* Should avoid rounding bounds again? */
		result = ic_lwb(vi, res.l);
		Return_If_Not_Success(result)
		result = ic_upb(vi, res.u);
		Return_If_Not_Success(result)
	    }
	} else {
	    Make_Interval(&tmp, res.l, res.u);
	    result = Unify_Pw(vi->var->val, vi->var->tag, tmp.val, tmp.tag);
	    Return_If_Not_Success(result);
	    *solved = 1;
	}

	Succeed
}


    /*
    ** If it's an integer variable, see if we can just
    ** exclude a single element from its domain to make it
    ** entailed.
    ** Note does not check for "out of bounds" entailment.
    */
int
solve_not_equal(bounds *a, var_info *vi, bounds *c, int *solved)
{
	bounds	res;
	int	result;

	*solved = 0;

	if (c->l == c->u && a->l == a->u && vi->tb.i) {
	    Div(*c, *a, res);

	    if ((res.l == res.u) && (ceil(res.l) == res.l)) {
		/* Exclude the value. */
		if (res.l < (double) MIN_S_WORD ||
			res.l >= ((double) MAX_S_WORD + 1.0)) {
		    /* XXX - update to handle bignums. */
		    Bip_Error(RANGE_ERROR)
		}
		result = ic_exclude(vi, (word) res.l);
		Return_If_Not_Success(result);
		*solved = 1;
	    } else if (res.u < ceil(res.l) && res.l > floor(res.u)) {
		/* Value falls between integers. */
		*solved = 1;
	    }
	} else {
	    Mul(*a, vi->tb.b, res);

	    if (c->l > res.u || c->u < res.l) {
		/* Entailed. */
		*solved = 1;
	    }
	}

	Succeed
}


/*----------------------------------------------------------------------
**
** Functions for working with constraint data structures.
**
*/

    /*
    ** Update the constraint data buffer based on the contents of the
    ** con_info structure.  If the buffer needs to be trailed, create a new
    ** buffer for the new data, trail it, etc.  If it doesn't need to be
    ** trailed, just overwrite the contents.
    **
    ** Also updates the suspension priority of the constraint, in case the
    ** priority should have changed (the constraint has gotten shorter).
    */
int
update_con_data_buf(con_info *con_info)
{
	pword	*susp;
	pword	prio;

	if (Trail_Needed(con_info->data)) {
	    pword *tmp;
	    Allocate_Con_Data_Buf(con_info);
	    tmp = con_info->con + CON_OFF_DATA;
	    Trail_Pword(tmp);
	    (tmp)->val.ptr = (con_info->data);
	    (tmp)->tag.kernel = TSTRG;
	}
	Set_Con_Data_Buf(con_info);

	/* Update the priority of the suspension in case it's changed. */
	susp = con_info->con + CON_OFF_SUSP;
	Dereference(susp);
	prio.val.nint = ConstraintPriority(con_info);
	prio.tag.kernel = TINT;
	return p_set_suspension_priority(susp->val, susp->tag, prio.val, prio.tag);
}


    /*
    ** Extract the data from the argument vector an ECLiPSe constraint
    ** structure, and put it in a con_info structure.
    */
void
con_struct_vec_to_con_info(pword *con_ecl_struct_vec, con_info* con)
{
	pword	*tmp;
	int	status;

	con->con = con_ecl_struct_vec;

	tmp = con_ecl_struct_vec + CON_OFF_DATA;
	Dereference(tmp);
	con->data = tmp->val.ptr;
	Dereference(con->data);	/* Not necessary? */

	con->flags = *ConBufEntry(con->data, CON_DATA_OFF_FLAGS);
	con->term_count = *ConBufEntry(con->data, CON_DATA_OFF_COUNT);
	con->old_term_count = con->term_count;
	con->c.l = *((double *) ConBufEntry(con->data, CON_DATA_OFF_RHS_LO));
	con->c.u = *((double *) ConBufEntry(con->data, CON_DATA_OFF_RHS_HI));

	if (con->term_count > 0) {
	    tmp = con_ecl_struct_vec + CON_OFF_LO_VEC;
	    Dereference(tmp);
	    tmp = tmp->val.ptr;
	    Dereference(tmp);	/* Not necessary? */
	    con->lo_vec = CoefVec(tmp);

	    tmp = con_ecl_struct_vec + CON_OFF_HI_VEC;
	    Dereference(tmp);
	    tmp = tmp->val.ptr;
	    Dereference(tmp);	/* Not necessary? */
	    con->hi_vec = CoefVec(tmp);

	    tmp = con_ecl_struct_vec + CON_OFF_VAR_VEC;
	    Dereference(tmp);
	    tmp = tmp->val.ptr;
	    Dereference(tmp);	/* Not necessary? */
	    con->var_vec = VarVec(tmp);
#ifdef IC_DEBUG
	} else {
	    con->lo_vec = 0;
	    con->hi_vec = 0;
	    con->var_vec = 0;
#endif
	}

	tmp = con_ecl_struct_vec + CON_OFF_SUSP;
	Dereference(tmp);
	con->susp = tmp;

	tmp = con_ecl_struct_vec + CON_OFF_BOOLEAN;
	Dereference(tmp);
	con->bool = tmp;

	/*
	** Set status to the effective value of the boolean (treat it as
	** true if the boolean is still a variable).
	*/
	if (IsRef(con->bool->tag)) {
	    status = 1;
	    con->reified = 1;
	} else {
	    status = con->bool->val.nint;
	    con->reified = 0;
	}
	con->op = FlagsAndBoolToOp(con->flags, status);

	/*
	(((((flags) & CON_EQUATION) + 1) * (2 * (bool) - 1) + 3) / 2)
	bool=1: (((flags & CON_EQUATION) >> 1) + 2)
	bool=0: (1 - ((flags & CON_EQUATION) >> 1))
	*/
}


#if 0
This not used yet (but probably should be?).  The idea was to have the start
and end of the constraint set-up close together in the code...

Note that this copy of the code is now a little out-of-date.

void
start_setting_up_con_struct(con_info *con, int flags, value vbool, type tbool,
		value vlin, type tlin)
{
	int	status;

	/*
	** Set status to the effective value of the boolean (treat it as
	** true if the boolean is still a variable).
	*/
	if (IsRef(tbool)) {
	    status = 1;
	} else {
	    status = vbool.nint;
	}

	con->c.l = NEG_ZERO;
	con->c.u = 0.0;
	con->flags = flags;
	con->term_count = 0;
	con->op = FlagsAndBoolToOp(flags, status);

	/* Create the ic_con structure. */
	con->con = TG;
	Push_Struct_Frame(d_ic_con);

	con->con[CON_OFF_BOOLEAN].val.all = vbool.all;
	con->con[CON_OFF_BOOLEAN].tag.all = tbool.all;
#if 0
#if 1
	con->con[CON_OFF_SUSP].val.all = vsusp.all;
	con->con[CON_OFF_SUSP].tag.all = tsusp.all;
#else
	con->con[CON_OFF_SUSP].val.ptr = con->con + CON_OFF_SUSP;
	con->con[CON_OFF_SUSP].tag.kernel = TREF;
	result = Unify_Pw(con->con[CON_OFF_SUSP].val, con->con[CON_OFF_SUSP].tag, vsusp, tsusp);
	Return_If_Not_Success(result);
#endif
#endif
}
#endif


    /*
    ** finish_setting_up_con_struct
    **	Uses the information provided to finish setting up the ECLiPSe
    **	constraint structure.
    */
void
finish_setting_up_con_struct(con_info *con, pword *lo_buf, pword *hi_buf,
		pword *var_buf, int count)
{
	dident	did;

#if 0
	/* Check whether the coefficient vectors are identical. */
	/* XXX - Turning this on breaks some things for some reason. */
	if (count > 0 && !memcmp(con->lo_vec, con->hi_vec,
			    count * sizeof(double))) {
	    /*
	    ** Make hi_buf/hi_vec the same as lo_buf/lo_vec.  Note that we
	    ** can't reset the stack to pop hi_buf, since other things may
	    ** have been added since; however, the buffer will be garbage
	    ** collected, since nothing refers to it.
	    */
	    hi_buf = lo_buf;
	    con->hi_vec = con->lo_vec;
	}
#endif

	if (count < con->term_count) {
#ifdef IC_DEBUG
	    printf("term count: %d -> %d.\n", con->term_count, count);
#endif
	    /*
	    ** We didn't get as many terms as expected, presumably because
	    ** something became ground due to bound rounding for an integer
	    ** var.  Pad things to match.
	    */
	    if (count > 0) {
		int	n;
		pword	*gap_start, *gap_end;

		did = ec_did("[]", count);
		var_buf->val.did = did;
		n = con->term_count - count;
		Pad(lo_buf - n, n);
		gap_end = lo_buf + BufferPwords(lo_buf);
		Set_Buffer_Size(lo_buf, count * sizeof(double));
		gap_start = lo_buf + BufferPwords(lo_buf);
		Pad(gap_start, gap_end-gap_start);
		if (hi_buf != lo_buf) {
		    gap_end = hi_buf + BufferPwords(hi_buf);
		    Set_Buffer_Size(hi_buf, count * sizeof(double));
		    gap_start = hi_buf + BufferPwords(hi_buf);
		    Pad(gap_start, gap_end-gap_start);
		}
	    } else {
		/* XXX - do we want to arrange that this cannot occur? */
		/* Or rather, have some short-cut code to call. */
		/*
		** Note that we clobber the allocated var struct to make
		** sure the contents are not interpreted by the garbage
		** collector; there is no need to do this for the coef
		** buffers since their contents are not interpreted anyway.
		*/
		Pad(var_buf, con->term_count + 1);
#ifdef	IC_DEBUG
		var_buf = 0;
		lo_buf = 0;
		hi_buf = 0;
		con->var_vec = 0;
		con->lo_vec = 0;
		con->hi_vec = 0;
#endif
	    }

	    con->term_count = count;
	}

	/* Create and fill in the constraint data buffer. */
	Allocate_Con_Data_Buf(con);
	Set_Con_Data_Buf(con);

	/*
	** Fill in the constraint struct.
	** Pretend all the buffer structures are strings since we don't have
	** any better tag to use.
	*/
	con->con[CON_OFF_DATA].val.ptr = con->data;
	con->con[CON_OFF_DATA].tag.kernel = TSTRG;
	if (count > 0) {
	    con->con[CON_OFF_LO_VEC].val.ptr = lo_buf;
	    con->con[CON_OFF_LO_VEC].tag.kernel = TSTRG;
	    con->con[CON_OFF_HI_VEC].val.ptr = hi_buf;
	    con->con[CON_OFF_HI_VEC].tag.kernel = TSTRG;
	    Make_Struct(con->con + CON_OFF_VAR_VEC, var_buf);
	} else {
	    Make_Nil(con->con + CON_OFF_LO_VEC);
	    Make_Nil(con->con + CON_OFF_HI_VEC);
	    Make_Nil(con->con + CON_OFF_VAR_VEC);
	    /* XXX - For reproducible bugs...  :)  Should remove later. */
	    con->con[CON_OFF_LO_VEC].val.ptr = 0;
	    con->con[CON_OFF_HI_VEC].val.ptr = 0;
	    con->con[CON_OFF_VAR_VEC].val.ptr = 0;
	}
}

    /*
    ** The constraint entry at position idx is ground, and should be swapped
    ** with one at the end of the constraint.
    */
void
swap_entries(int idx, con_info *con, bounds *a)
{
	bounds		b;
	typed_bounds	ytb;
	pword	*tmp;

	while (--con->term_count > idx) {
	    /* Scan backwards to find a variable to swap with. */
	    tmp = con->var_vec + con->term_count;
	    Dereference(tmp);
	    if (IsRef(tmp->tag)) {
		break;
	    }
	    Constant_To_Typed_Bounds(tmp->val, tmp->tag, ytb);
	    if ((con->flags & CON_INTEGRALITY_PROP) && !ytb.i) {
		/* No integrality to propagate. */
		con->flags &= ~CON_INTEGRALITY_PROP;
	    }
	    b.l = con->lo_vec[con->term_count];
	    b.u = con->hi_vec[con->term_count];
	    Mul(b, ytb.b, ytb.b);
	    Sub(con->c, ytb.b, con->c);
	}

	if (con->term_count > idx) {
	    value   v;
	    double  d;
	    int	    tc = con->term_count;
	    /*
	    ** Note that we have to use the raw (not dereferenced) values
	    ** when swapping variables since we're not restoring on
	    ** backtracking.
	    **
	    ** XXX - Do we need to swap the tags for the variables as well?
	    ** E.g. TREF vs. TMETA...  Probably all start as TMETA, and stay
	    ** that way?
	    */
	    v = con->var_vec[idx].val;
	    con->var_vec[idx].val = con->var_vec[tc].val;
	    con->var_vec[tc].val = v;
	    d = con->lo_vec[tc];
	    con->lo_vec[tc] = a->l;
	    a->l = con->lo_vec[idx] = d;
	    if (con->lo_vec != con->hi_vec) {
		d = con->hi_vec[tc];
		con->hi_vec[tc] = a->u;
		a->u = con->hi_vec[idx] = d;
	    }
	}
}



/*----------------------------------------------------------------------
**
** Functions for helping propagate/evaluate a linear constraint.
**
*/

    /*
    ** Updates E/F based on the term a * x.
    */
void
update_ef(prop_info *prop, con_info *con, bounds *a, bounds *x, int idx)
{
	bounds	res;

#ifdef IC_DEBUG
	fprintf(stderr, "   al = %f, au = %f.\n", a->l, a->u);
	fprintf(stderr, "   xl = %f, xu = %f.\n", x->l, x->u);
#endif

	Mul(*a, *x, res);

	if (res.l == -HUGE_VAL) {
	    /*
	    ** If F is being used for entailment, no entailment
	    ** possible.  If F is being used for propagation and we've
	    ** already seen an infinite bound, no propagation is
	    ** possible.
	    */
	    if (OpIsGreater(con->op) || prop->inf_f_idx >= 0) {
#if 0
		/* Little hack for 1-var constraints. */
		if (prop->inf_f_idx < MAX_S_WORD) {
		    res.l = min(0.0, res.u);
		}
#endif

		prop->inf_f_idx = MAX_S_WORD;
#ifdef IC_DEBUG
		fprintf(stderr, "    inf_f_idx = %d.\n", prop->inf_f_idx);
#endif

		if (prop->inf_e_idx >= con->term_count) {
		    /*
		    ** No E-based bound updates / (dis)entailment
		    ** possible either.  Don't worry about eliminating
		    ** any remaining ground terms, which is the only
		    ** useful work we could do by continuing.
		    */
#ifdef IC_DEBUG
		    fprintf(stderr, "    No propagation can occur.\n");
#endif
		    prop->no_prop = 1;
		    return;
		}
	    } else {
		prop->inf_f_idx = idx;
		prop->inf_f_bound = res.u;
		res.l = min(0.0, res.u);
#ifdef IC_DEBUG
		fprintf(stderr, "    inf_f_idx = %d, inf_f_bound = %f.\n",
			prop->inf_f_idx, prop->inf_f_bound);
#endif
	    }
	}

	if (res.u == HUGE_VAL) {
	    /*
	    ** If E is being used for entailment, no entailment
	    ** possible.  If E is being used for propagation and we've
	    ** already seen an infinite bound, no propagation is
	    ** possible.
	    */
	    if (OpIsLessEqual(con->op) || prop->inf_e_idx >= 0) {
#if 0
		/* Little hack for 1-var constraints. */
		if (prop->inf_e_idx < MAX_S_WORD) {
		    res.u = max(0.0, res.l);
		}
#endif

		prop->inf_e_idx = MAX_S_WORD;
#ifdef IC_DEBUG
		fprintf(stderr, "    inf_e_idx = %d.\n", prop->inf_e_idx);
#endif
		if (prop->inf_f_idx >= con->term_count) {
		    /*
		    ** No F-based bound updates / (dis)entailment
		    ** possible either.  Don't worry about eliminating
		    ** any remaining ground terms, which is the only
		    ** useful work we could do by continuing.
		    */
#ifdef IC_DEBUG
		    fprintf(stderr, "    No propagation can occur.\n");
#endif
		    prop->no_prop = 1;
		    return;
		}
	    } else {
		prop->inf_e_idx = idx;
		prop->inf_e_bound = res.l; /* XXX might have been mod by inf_f */
		res.u = max(0.0, res.l);
#ifdef IC_DEBUG
		fprintf(stderr, "    inf_e_idx = %d, inf_e_bound = %f.\n",
			prop->inf_e_idx, prop->inf_e_bound);
#endif
	    }
	}

	Sub(prop->sum, res, prop->sum);

#ifdef IC_DEBUG
	fprintf(stderr, "   term lo = %f, hi = %f.\n", res.l, res.u);
	fprintf(stderr, "   neg_e = %f, f = %f.\n", prop->sum.l, prop->sum.u);
#endif
}


    /*
    ** Check whether a linear constraint (con) is entailed or disentailed,
    ** and if so, unify the boolean from the constraint with the appropriate
    ** value and kill the suspension.
    */
int
evaluate_reified(con_info *con, prop_info *prop, int *solved)
{
	int	result, inequality;

	/* Adjust for any infinities left out of sums. */
	if (prop->inf_f_idx >= 0) {
	    prop->sum.u = HUGE_VAL;
	}
	if (prop->inf_e_idx >= 0) {
	    prop->sum.l = -HUGE_VAL;
	}

	/* Note that op is either = or =< */
	inequality = OpIsLessEqual(con->op) || OpIsGreater(con->op);
	if (prop->sum.u < 0.0 || (!inequality && prop->sum.l > 0.0)) {
	    /*
	    ** F < 0, or it's an equation and E < 0 (-E > 0).
	    ** Result: Constraint is disentailed (false).
	    */
	    Unify_Boolean(con->bool, !OpIsUpperBound(con->op), result);
	    Return_If_Not_Success(result);
	    con->reified = 0;
	    *solved = 1;
	    Succeed
	} else if (prop->sum.l >= 0.0 && (inequality || prop->sum.u <= 0.0)) {
	    /*
	    ** E <= 0 (-E >= 0), and if it's an equation F <= 0 (actually,
	    ** E = F = 0 in this case).
	    ** Result: Constraint is entailed (true).
	    */
	    Unify_Boolean(con->bool, !!OpIsUpperBound(con->op), result);
	    Return_If_Not_Success(result);
	    con->reified = 0;
	    *solved = 1;
	    Succeed
	} /* else unknown status */

	*solved = 0;
	Succeed
}


    /*
    ** Determine the status of a 0-variable constraint.
    */
int
check_ic_0v_con(con_info *con)
{
	switch (con->op) {
	    case UPPER_BOUND + LOWER_BOUND:
		/* Equation. */
		if (0.0 <= con->c.l && 0.0 >= con->c.u) {
		    /* Entailed. */
		    Mark_Constraint_Entailed(con)
		    Succeed
		} else if (0.0 > con->c.u || 0.0 < con->c.l) {
		    /* Failure. */
		    Fail
		}
		break;

	    case UPPER_BOUND:
		/* Less than or equal constraint. */
		if (0.0 <= con->c.l) {
		    /* Entailed. */
		    Mark_Constraint_Entailed(con)
		    Succeed
		} else if (0.0 > con->c.u) {
		    /* Failure. */
		    Fail
		}
		break;

	    case LOWER_BOUND:
		/* Greater than constraint. */
		if (0.0 > con->c.u) {
		    /* Entailed. */
		    Mark_Constraint_Entailed(con)
		    Succeed
		} else if (0.0 <= con->c.l) {
		    /* Failure. */
		    Fail
		}
		break;

	    default:
		Bip_Error(EC_EXTERNAL_ERROR);
	}

	Update_Con_Data_Buf_If_Needed(con);

	Succeed
}


    /*
    ** Propagate a constraint with only 1 variable.
    */
int
prop_ic_1v_con(con_info *con, prop_info *prop)
{
	int	result, solved;
	pword	*tmp;
	bounds	a, res;
	var_info	vi;

#ifdef IC_DEBUG
	fprintf(stderr, "Entering prop_ic_1v_con.\n");
#endif

#ifdef IC_DEBUG
	fprintf(stderr, " flags = %x, term_count = %d, cl = %f, cu = %f.\n",
		con->flags, con->term_count, con->c.l, con->c.u);
	fprintf(stderr, " reified = %d, op = %x.\n", con->reified, con->op);
#endif

	a.l = con->lo_vec[0];
	a.u = con->hi_vec[0];

#ifdef IC_DEBUG
	fprintf(stderr, " al = %f, au = %f.\n", a.l, a.u);
#endif

	tmp = con->var_vec;
	Dereference(tmp);

	if (IsRef(tmp->tag)) {
	    get_var_info(tmp, &vi);
	    vi.prop_int = con->flags & CON_INTEGRALITY_PROP;

#ifdef IC_DEBUG
	    fprintf(stderr, " xl = %f, xu = %f, xi = %d.\n", vi.tb.b.l, vi.tb.b.u,
		    vi.tb.i);
#endif

	    if (OpIsEqual(con->op)) {
		result = solve_equal(&a, &vi, &con->c, &solved);
		Return_If_Not_Success(result);
		if (solved) {
		    Mark_Constraint_Entailed(con);
		    Succeed
		}
	    } else {
		result = impose_coef_bounds(con->op, OpIsGreater(con->op),
		&a, &vi, &con->c);
		Return_If_Not_Success(result);

		Mul(a, vi.tb.b, res);

		if (OpIsGreater(con->op) && res.u <= con->c.l) {
		    Fail
		}
		if (OpIsLessEqual(con->op) ? res.u <= con->c.l : res.l > con->c.u) {
		    Mark_Constraint_Entailed(con)
		    Succeed
		}
	    }
	} else {
	    con->term_count = 0;

	    Constant_To_Typed_Bounds(tmp->val, tmp->tag, vi.tb);

#ifdef IC_DEBUG
	    fprintf(stderr, " Ground x: xl = %f, xu = %f, xi = %d.\n",
		    vi.tb.b.l, vi.tb.b.u, vi.tb.i);
#endif

	    Mul(a, vi.tb.b, res);
	    Sub(con->c, res, con->c);

	    if (OpIsEqual(con->op)) {
		if (con->c.l > 0.0 || con->c.u < 0.0) {
		    Fail
		}
		if (con->c.l == 0.0 && con->c.u == 0.0) {
		    Mark_Constraint_Entailed(con)
		    Succeed
		}
	    } else if (OpIsLessEqual(con->op)) {
		if (con->c.u < 0.0) {
		    Fail
		}
		if (con->c.l >= 0.0) {
		    Mark_Constraint_Entailed(con)
		    Succeed
		}
	    } else {
		if (con->c.l >= 0.0) {
		    Fail
		}
		if (con->c.u < 0.0) {
		    Mark_Constraint_Entailed(con)
		    Succeed
		}
	    }
	}

	Update_Con_Data_Buf_If_Needed(con);

	Succeed
}


    /*
    ** Do the first pass of the two-pass propagation algorithm.
    */
int
prop_pass_1(con_info *con, prop_info *prop)
{
	var_info	vi;
	bounds	a, res;
	pword	*tmp;
	int	idx, result;

#ifdef IC_DEBUG
	fprintf(stderr, "Entering prop_pass_1.\n");
#endif

	if (OpIsNotEqual(con->op) && !con->reified) {
	    Bip_Error(EC_EXTERNAL_ERROR);
	}

#ifdef IC_DEBUG
	fprintf(stderr, " flags = %x, term_count = %d, cl = %f, cu = %f.\n",
		con->flags, con->term_count, con->c.l, con->c.u);
	fprintf(stderr, " op = %x, reified = %d.\n",
		con->op, con->reified);
#endif

	Init_Prop_Info(prop);

#ifdef IC_DEBUG
	fprintf(stderr, " Commencing e/f computation.\n");
#endif

	for (idx = 0; idx < con->term_count; idx++) {
	    a.l = con->lo_vec[idx];
	    a.u = con->hi_vec[idx];
	    tmp = con->var_vec + idx;
	    Dereference(tmp);

#ifdef IC_DEBUG
	    fprintf(stderr, "  Term %d.\n", idx);
#endif

	    if (!IsRef(tmp->tag)) {
		/* X is ground. */
		Constant_To_Typed_Bounds(tmp->val, tmp->tag, vi.tb);

		if ((con->flags & CON_INTEGRALITY_PROP) && !vi.tb.i) {
		    /*
		    ** No integrality to propagate.
		    ** Note that since at least one ground term has been
		    ** eliminated, modifying the constraint, this change
		    ** will be written to the constraint later with the
		    ** other changes.
		    */
		    con->flags &= ~CON_INTEGRALITY_PROP;
		}

		/* Adjust RHS constant. */
		Mul(a, vi.tb.b, res);
		Sub(con->c, res, con->c);

		/* Swap X with a yet-to-be-processed variable. */
		swap_entries(idx, con, &a);

#ifdef IC_DEBUG
		fprintf(stderr, "   Adjusted c: cl = %f, cu = %f.\n", con->c.l, con->c.u);
#endif

		/* If we're at the end now (no more variables), stop. */
		if (idx >= con->term_count) {
		    break;
		}

		/* We now have a variable at position idx. */
		tmp = con->var_vec + idx;
		Dereference(tmp);
	    }

	    get_var_info(tmp, &vi);

	    if (!vi.tb.i) {
		prop->num_non_int_vars++;
		prop->non_int_idx = idx;
	    }

	    update_ef(prop, con, &a, &vi.tb.b, idx);
	    if (prop->no_prop) {
		break;
	    }
	}

	/* Don't move this to the start of the loop!  con->c changes... */
	Add(prop->sum, con->c, prop->sum);

#ifdef IC_DEBUG
	fprintf(stderr, " Final neg_e = %f, f = %f.\n", prop->sum.l, prop->sum.u);
#endif

	/*
	** Remaining code in this function does not apply to constraints
	** which are still reified.
	*/
	if (con->reified) {
	    Succeed
	}

	if (con->flags & CON_INTEGRALITY_PROP) {
	    if (prop->num_non_int_vars == 0) {
		/*
		** Clearing the flag here isn't necessary, which is lucky
		** since it will only be updated in the structure if the
		** constraint has otherwise been modified.
		*/
		con->flags &= ~CON_INTEGRALITY_PROP;
		prop->non_int_idx = -1;		/* Just to be safe. */
	    } else if (prop->num_non_int_vars == 1 && !con->reified) {
		a.l = con->lo_vec[prop->non_int_idx];
		a.u = con->hi_vec[prop->non_int_idx];
		if ((a.l == 1.0 && a.u == 1.0) || (a.l == -1.0 && a.u == -1.0)) {
		    tmp = con->var_vec + prop->non_int_idx;
		    Dereference(tmp);
		    get_var_info(tmp, &vi);
		    result = set_integral(&vi);
		    Return_If_Not_Success(result);
		    /* Integrality propagation now completely dealt with. */
		    con->flags &= ~CON_INTEGRALITY_PROP;
		    prop->num_non_int_vars = 0;
		    prop->non_int_idx = -1;
		}
	    } else {
		prop->non_int_idx = -1;
	    }
	} else {
	    prop->non_int_idx = -1;
	}
	/*
	** Now non_int_idx >= 0 iff we're supposed to be propagating
	** integrality to that variable if we ground it to an integral
	** value.
	*/

	Succeed
}


    /*
    ** Propagate a linear constraint (ic_con).
    ** We assume bool is a ground integer.
    */
int
prop_pass_2(con_info *con, prop_info *prop)
{
	pword	*tmp;
	var_info    vi;
	bounds	a, res;
	int	pseudo_op, idx, result, solved;

#ifdef IC_DEBUG
	fprintf(stderr, "Entering prop_pass_2.\n");
#endif

	/*
	** XXX - Do we want to check for short (1 & 2 var) constraints here?
	** Probably not --- by this point just as efficient to use general
	** algorithm, right?
	** Either way, we must check for term_count == 1 && non_int_idx == 0.
	*/
	if (con->term_count == 1 && OpIsEqual(con->op)) {
	    a.l = con->lo_vec[0];
	    a.u = con->hi_vec[0];
    
	    tmp = con->var_vec;
	    Dereference(tmp);
	    get_var_info(tmp, &vi);
	    vi.prop_int = (prop->non_int_idx == 0);

	    result = solve_equal(&a, &vi, &con->c, &solved);
	    Return_If_Not_Success(result);
	    if (solved) {
		Mark_Constraint_Entailed(con);
	    } else {
		Update_Con_Data_Buf_If_Needed(con);
	    }

	    Succeed
	}

	/* Short-cuts and special processing for infinities. */

#if 0
	no_f_bounds = OpIsGreater(op) || inf_f_idx >= term_count;
	/* XXX - can't just do this --- stuffs up entailment checking? */
	if (no_f_bounds) f = neg_e;
	no_e_bounds = OpIsLessEqual(op) || inf_e_idx >= term_count;
	/* XXX - can't just do this --- stuffs up entailment checking? */
	if (no_e_bounds) neg_e = f;

	prop_bounds = LOWER_BOUND * !no_e_bounds + UPPER_BOUND * !no_f_bounds;
#endif

	pseudo_op = con->op;
	if (prop->inf_f_idx >= con->term_count) {
	    pseudo_op &= ~UPPER_BOUND;
	    /* f = neg_e; */
	}
	if (prop->inf_e_idx >= con->term_count) {
	    pseudo_op &= ~LOWER_BOUND;
	    /* neg_e = f; */
	}

#ifdef IC_DEBUG
	fprintf(stderr, " pseudo_op = %d.\n", pseudo_op);
#endif

	/*
	** XXX - update to handle single infinities as described in
	** ic_design.html.
	*/

	if (OpIsUpperBound(pseudo_op) && prop->inf_f_idx >= 0) {
	    /* Upper bound update. */
	    /* a * x =< f */
#if 0
	    if (inf_f_bound <= f) {
		/* Bound already tight enough. */
	    } else
#endif
	    {
#ifdef IC_DEBUG
		fprintf(stderr, " Upper bound update with single infinity.\n");
#endif

		a.l = con->lo_vec[prop->inf_f_idx];
		a.u = con->hi_vec[prop->inf_f_idx];

		tmp = con->var_vec + prop->inf_f_idx;
		Dereference(tmp);
		get_var_info(tmp, &vi);

#ifdef IC_DEBUG
		fprintf(stderr, "  Term %d.\n", prop->inf_f_idx);
		fprintf(stderr, "   al = %f, au = %f.\n", a.l, a.u);
		fprintf(stderr, "   xl = %f, xu = %f, xi = %d.\n",
		vi.tb.b.l, vi.tb.b.u, vi.tb.i);
#endif

		Mul(a, vi.tb.b, res);

		/*
		** Specialised infinity adjustment based on the fact that we
		** already know res.l must be -inf.
		*/
		res.l = min(0.0, res.u);
		if (res.u == HUGE_VAL) {
		    res.u = 0.0;
		}

		Undo_Sub(prop->sum, res, res);

#ifdef IC_DEBUG
		fprintf(stderr, "   lo = %f, hi = %f.\n", res.l, res.u);
#endif

		result = impose_coef_bounds(UPPER_BOUND, 0, &a, &vi, &res);
		Return_If_Not_Success(result);
		/*
		** Note that the above cannot make X ground, even with
		** integer rounding, because the other bound is still
		** infinite.
		*/

#ifdef IC_DEBUG
		fprintf(stderr, "   new xl = %f, new xu = %f.\n", vi.tb.b.l,
		vi.tb.b.u);
#endif
	    }
	    pseudo_op &= ~UPPER_BOUND;
	}

	if (OpIsLowerBound(pseudo_op) && prop->inf_e_idx >= 0) {
	    /* Lower bound update. */
	    /* a * x >= neg_e */
#if 0
	    if (inf_e_bound > neg_e) {
		/* Bound already tight enough. */
	    } else
#endif
	    {
#ifdef IC_DEBUG
		fprintf(stderr, " Lower bound update with single infinity.\n");
#endif

		a.l = con->lo_vec[prop->inf_e_idx];
		a.u = con->hi_vec[prop->inf_e_idx];

		tmp = con->var_vec + prop->inf_e_idx;
		Dereference(tmp);
		get_var_info(tmp, &vi);

#ifdef IC_DEBUG
		fprintf(stderr, "  Term %d.\n", prop->inf_e_idx);
		fprintf(stderr, "   al = %f, au = %f.\n", a.l, a.u);
		fprintf(stderr, "   xl = %f, xu = %f, xi = %d.\n",
		vi.tb.b.l, vi.tb.b.u, vi.tb.i);
#endif

		Mul(a, vi.tb.b, res);

		/*
		** Specialised infinity adjustment based on the fact that we
		** already know res.u must be +inf.
		*/
		res.u = max(0.0, res.l);
		if (res.l == -HUGE_VAL) {
		    res.l = 0.0;
		}

		Undo_Sub(prop->sum, res, res);

#ifdef IC_DEBUG
		fprintf(stderr, "   lo = %f, hi = %f.\n", res.l, res.u);
#endif

		result = impose_coef_bounds(LOWER_BOUND,
			OpIsGreater(con->op), &a, &vi, &res);
		Return_If_Not_Success(result);
		/*
		** Note that the above cannot make X ground, even with
		** integer rounding, because the other bound is still
		** infinite.
		*/

#ifdef IC_DEBUG
		fprintf(stderr, "   new xl = %f, new xu = %f.\n", vi.tb.b.l,
		vi.tb.b.u);
#endif
	    }
	    pseudo_op &= ~LOWER_BOUND;
	}

	if (pseudo_op) {
	    /* At least one bound requires the second pass. */

#ifdef IC_DEBUG
	    fprintf(stderr, " Commencing general propagation.\n");
#endif

	    for (idx = 0; idx < con->term_count; idx++) {
		a.l = con->lo_vec[idx];
		a.u = con->hi_vec[idx];

		tmp = con->var_vec + idx;
		Dereference(tmp);
		get_var_info(tmp, &vi);
		/* Can't rely on index of non-int var remaining stable... :( */
		vi.prop_int = (con->flags & CON_INTEGRALITY_PROP) &&
			prop->num_non_int_vars == 1 && !vi.tb.i;

#ifdef IC_DEBUG
		fprintf(stderr, "  Term %d.\n", idx);
		fprintf(stderr, "   al = %f, au = %f.\n", a.l, a.u);
		fprintf(stderr, "   xl = %f, xu = %f, xi = %d.\n",
		vi.tb.b.l, vi.tb.b.u, vi.tb.i);
#endif

		Mul(a, vi.tb.b, res);

		if (res.l == -HUGE_VAL) {
		    res.l = min(0.0, res.u);
		}
		if (res.u == HUGE_VAL) {
		    res.u = max(0.0, res.l);
		}

		Undo_Sub(prop->sum, res, res);

#ifdef IC_DEBUG
		fprintf(stderr, "   lo = %f, hi = %f.\n", res.l, res.u);
#endif

		result = impose_coef_bounds(pseudo_op, OpIsGreater(con->op),
			&a, &vi, &res);
		Return_If_Not_Success(result);

#ifdef IC_DEBUG
		fprintf(stderr, "   new xl = %f, new xu = %f.\n", vi.tb.b.l,
		vi.tb.b.u);
#endif

		/*
		** If X is integral, rounding might have caused it to become
		** ground.  Note that xl and xu contain the current bounds
		** of X.
		*/
		if (vi.tb.b.l == vi.tb.b.u) {
		    /* X is ground. */
		    /* Adjust RHS constant. */
		    Mul(a, vi.tb.b, res);
		    Sub(con->c, res, con->c);

		    /* Swap X with a yet-to-be-processed variable. */
		    swap_entries(idx, con, &a);

#ifdef USE_BOUND_SET_SHORTCUT
		    /* Assume X is integral, so don't update flags. */
#else
		    if ((con->flags & CON_INTEGRALITY_PROP) && !vi.tb.i) {
			/*
			** No integrality to propagate.
			** Note that since at least one ground term has been
			** eliminated, modifying the constraint, this change
			** will be written to the constraint later with the
			** other changes.
			*/
			con->flags &= ~CON_INTEGRALITY_PROP;
		    }
#endif

		    /*
		    ** We now have a variable at position idx.
		    ** Update idx so that next iter does this position
		    ** again.  (Of course, idx may now be beyond term_count
		    ** anyway.)
		    */
		    idx--;
		}
	    }
	}

	/*
	** Check for the constraint now being entailed...
	** E.g. it's now a 0-var equation or perhaps a 1-var int inequality.
	** What kinds of things can be entailed here that weren't detected
	** earlier?  Is it only 0-var equations or 0,1 var inequalities?
	** Nope, with integer rounding an inequality with an arbitrary
	** number of variables can become entailed.
	**
	** Actually, can only happen with integer rounding (otherwise a "set
	** to bounds" short-cut would have been used) or with what was a
	** 1-var equality (dealt with above?).
	**
	** Umm, 1-var inequalities are not caught earlier...?
	**
	** Easy to catch if we keep F,E up-to-date (useful for equation
	** propagation).
	*/

	/* Be lazy for now, and just catch 0-var constraints. */
	if (con->term_count == 0) {
	    if (OpIsLessEqual(con->op)) {
		if (0.0 <= con->c.l) {
		    /* Entailed. */
		    Mark_Constraint_Entailed(con)
		    Succeed
		}
	    } else if (OpIsGreater(con->op)) {
		if (0.0 > con->c.u) {
		    /* Entailed. */
		    Mark_Constraint_Entailed(con)
		    Succeed
		}
	    } else {
		/* op is equality */
		if (con->c.u == 0.0 && con->c.l == 0.0) {
		    /* Entailed. */
		    Mark_Constraint_Entailed(con)
		    Succeed
		}
	    }
	}

	/* Check for constraints which can be turned into unifications. */
	Check_Con_Is_Unification(con)

	Update_Con_Data_Buf_If_Needed(con);

	Succeed
}


    /*
    ** Propagate a linear disequation (ic_con).
    */
int
prop_ic_neq_con(con_info *con)
{
	pword	*tmp;
	bounds	a, res;
	var_info	vi;
	int	idx, result, solved;

	/*
	** If it's a two-variable constraint, guess that we're going to be
	** able to solve it completely now and try to short-circuit its
	** processing.
	*/
	if (con->term_count == 2) {
	    bounds  c;

	    tmp = con->var_vec;
	    Dereference(tmp);

	    if (!IsRef(tmp->tag)) {
		/* First variable is ground. */
		Constant_To_Typed_Bounds(tmp->val, tmp->tag, vi.tb);
		a.l = con->lo_vec[0];
		a.u = con->hi_vec[0];

		Mul(a, vi.tb.b, res);
		Sub(con->c, res, c);

		tmp = con->var_vec + 1;
		Dereference(tmp);

		a.l = con->lo_vec[1];
		a.u = con->hi_vec[1];

		if (!IsRef(tmp->tag)) {
		    /* Second variable is ground too. */
		    Constant_To_Typed_Bounds(tmp->val, tmp->tag, vi.tb);

		    Mul(a, vi.tb.b, res);
		    Sub(c, res, con->c);

		    if (con->c.l > 0.0 || con->c.u < 0.0) {
			/* Entailed. */
			Mark_Constraint_Entailed(con)
			Succeed
		    }
		    if (con->c.l == 0.0 && con->c.u == 0.0) {
			Fail
		    }
		    /*
		    ** If we get here, we guessed wrong, but the work we did
		    ** was not wasted.
		    */
		    con->term_count = 0;
		} else {
		    /* Second variable is still a variable. */
		    get_var_info(tmp, &vi);

		    result = solve_not_equal(&a, &vi, &c, &solved);
		    Return_If_Not_Success(result);
		    if (solved) {
			Mark_Constraint_Entailed(con)
			Succeed
		    }
		    /*
		    ** If we get here, we guessed wrong.  XXX - Try to
		    ** exploit the work we did?
		    */
		}
	    } else {
		/* First variable is still a variable. */
		get_var_info(tmp, &vi);

		tmp = con->var_vec + 1;
		Dereference(tmp);

		if (!IsRef(tmp->tag)) {
		    /* Second variable is ground. */
		    typed_bounds tb;

		    Constant_To_Typed_Bounds(tmp->val, tmp->tag, tb);
		    a.l = con->lo_vec[1];
		    a.u = con->hi_vec[1];

		    Mul(a, tb.b, res);
		    Sub(con->c, res, c);

		    a.l = con->lo_vec[0];
		    a.u = con->hi_vec[0];

		    result = solve_not_equal(&a, &vi, &c, &solved);
		    Return_If_Not_Success(result);
		    if (solved) {
			Mark_Constraint_Entailed(con)
			Succeed
		    }
		    /*
		    ** If we get here, we guessed wrong.  XXX - Try to
		    ** exploit the work we did?
		    */
		} else {
		    /* Both variables are still variables: nothing to do. */
		    Succeed
		}
	    }
	}

	for (idx = 0; idx < con->term_count; idx++) {
	    if (idx >= 2) break;

	    tmp = con->var_vec + idx;
	    Dereference(tmp);

	    if (!IsRef(tmp->tag)) {
		/* X is ground. */
		Constant_To_Typed_Bounds(tmp->val, tmp->tag, vi.tb);
		a.l = con->lo_vec[idx];
		a.u = con->hi_vec[idx];

		/* Adjust RHS constant. */
		Mul(a, vi.tb.b, res);
		Sub(con->c, res, con->c);

		/* Swap X with a yet-to-be-processed variable. */
		swap_entries(idx, con, &a);
	    }
	}

	if (con->term_count == 0) {
	    if (con->c.l > 0.0 || con->c.u < 0.0) {
		/* Entailed. */
		Mark_Constraint_Entailed(con)
		Succeed
	    }
	    if (con->c.l == 0.0 && con->c.u == 0.0) {
		Fail
	    }
	} else if (con->term_count == 1) {
	    a.l = con->lo_vec[0];
	    a.u = con->hi_vec[0];
	    tmp = con->var_vec;
	    Dereference(tmp);

	    get_var_info(tmp, &vi);

	    result = solve_not_equal(&a, &vi, &con->c, &solved);
	    Return_If_Not_Success(result);
	    if (solved) {
		Mark_Constraint_Entailed(con)
		Succeed
	    }
	}

	Update_Con_Data_Buf_If_Needed(con);

	Succeed
}


#ifdef USE_BOUND_SET_SHORTCUT
int
set_vars_to_lwb_list(pword *plin, con_info *con, prop_info *prop)
{
	pword	*pterm, *tmp;
	int	idx, result;
	typed_bounds	a;
	bounds	res, new_sum;

	new_sum = con->c;

	BeginIterateLinList(plin, pterm, idx, a, tmp)
	    if (!IsRef(tmp->tag)) {
		/* Variable (X) is ground. */
		Constant_To_Typed_Bounds(tmp->val, tmp->tag, vi.tb);

		/* Update sum. */
		Mul(a.b, vi.tb.b, res);
		Sub(new_sum, res, new_sum);
	    } else {
		/* Variable is still a variable, so set term to lower bound. */
		get_var_info(tmp, &vi);
		if (a.tb.b.l >= 0.0) {
		    which_var_bound = 0;	/* lower */
		    bound_value = vi.tb.b.l;
		} else if (a.tb.b.u <= 0.0) {
		    which_var_bound = 1;	/* upper */
		    bound_value = vi.tb.b.u;
		} else {
		    /*
		    ** Coefficient spans zero.
		    ** Try to determine which variable bound is the right
		    ** one.
		    */
		    ...
		}
	    }

	    if (vi.tb.i) {
		pword	bnd;
		result = ec_double_to_int_or_bignum(bound_value, &bnd);
		Return_If_Not_Success(result);
		result = Unify_Pw(tmp->val, tmp->tag, bnd.val, bnd.tag);
		Return_If_Not_Success(result);
	    } else {
		pword	*bnd;
		bnd = var->attr + (which_var_bound ? OFF_HI : OFF_LO);
		result = Unify_Pw(tmp->val, tmp->tag, bnd->val, bnd->tag);
		Return_If_Not_Success(result);
	    }

	    res.l = bound_value;
	    res.u = bound_value;
	    Mul(a.b, res, res);
	    Sub(new_sum, res, new_sum);
	EndIterateLinList(plin)

	/* Check entailment... */
	...

	Succeed
}
#endif

/* XXX - prop integrality when last non-int var and have unit coef. */
int
short_cuts(con_info *con, prop_info *prop, int *short_cut)
{
	*short_cut = SHORT_ERROR;

	/* Check to see whether we can short-cut the default processing. */
	/* See ic_design.html for more explanation of the logic here. */

	/* Short-cuts based on F and E (if not infinite). */
	if (prop->inf_f_idx < 0) {
	    if (prop->sum.u < 0.0) {
		/*
		** We have failure (= or =<) or entailment (> or \=).
		*/
		if (OpIsUpperBound(con->op)) {
		    Fail
		}
		*short_cut = SHORT_ENTAILED;
		Succeed
	    }
	    if (prop->sum.u == 0.0) {
		if (prop->inf_e_idx < 0 && prop->sum.l == 0.0) {
		    /* Ground constraint with LHS == RHS. */
		    if (OpIsUpperBound(con->op)) {
			*short_cut = SHORT_ENTAILED;
			Succeed
		    } else {
			Fail
		    }
		}
#ifdef USE_BOUND_SET_SHORTCUT
		if (OpIsUpperBound(con->op)) {
		    /* Set everything to lower bounds. */
		    *short_cut = SHORT_LWB;
		    Succeed
		}
#endif
	    }
	}

	if (prop->inf_e_idx < 0) {
	    if (prop->sum.l > 0.0) {
		/*
		** We have failure (= or >) or entailment (=< or \=).
		*/
		if (OpIsLowerBound(con->op)) {
		    Fail
		}
		*short_cut = SHORT_ENTAILED;
		Succeed
	    }
	    if (prop->sum.l == 0.0) {
#ifdef USE_BOUND_SET_SHORTCUT
		if (OpIsEqual(con->op)) {
		    /* Set everything to upper bounds. */
		    *short_cut = SHORT_UPB;
		    Succeed
		}
#endif
		if (OpIsLessEqual(con->op)) {
		    /* Entailment. */
		    *short_cut = SHORT_ENTAILED;
		    Succeed
		}
		if (OpIsGreater(con->op)) {
		    Fail
		}
		/* No need to check \=, since already covered. */
	    }
	}

	*short_cut = SHORT_NONE;
	Succeed
}


/*
** Updates an attribute with a new bitmap, updating bounds and waking lists,
** etc. if indicated by result (Result_Is_Slack(), etc.).
*/
int
sync_attr_with_new_bitmap(var_info *vi, void *bitmap, int result)
{
	double	lwb, upb;
	word	min, max;
	value	val;
	type	tag;

	if (Result_Is_Empty(result)) {
	    /* New bitmap is empty, so no solution. */
	    Fail
	}
	if (!Result_Is_Change(result)) {
	    /* No change - nothing to do. */
	    Succeed
	}
	if (Result_Is_Slack(result)) {
	    /* Intersection resulted in bound change(s). */
	    bitmap_range(bitmap, &min, &max);
	    if (min == max) {
		/* Variable is now ground. */
		val.nint = min;
		tag.kernel = TINT;
		return Unify_Pw(vi->var->val, vi->var->tag, val, tag);
	    }
	    lwb = (double) min;
	    if (lwb > vi->tb.b.l) {
		/* Lower bound has changed. */
		Make_Checked_Double_Val(val, lwb);
		tag.kernel = TDBL;
		ec_assign(vi->attr + OFF_LO, val, tag);
		vi->tb.b.l = lwb;
		result = ec_schedule_susps(vi->attr + OFF_WAKE_LO);
		Return_If_Not_Success(result)
	    }
	    upb = (double) max;
	    if (upb < vi->tb.b.u) {
		/* Upper bound has changed. */
		Make_Checked_Double_Val(val, upb);
		tag.kernel = TDBL;
		ec_assign(vi->attr + OFF_HI, val, tag);
		vi->tb.b.u = upb;
		result = ec_schedule_susps(vi->attr + OFF_WAKE_HI);
		Return_If_Not_Success(result)
	    }
	}
	/* Update the attribute if the bitmap has moved. */
	Update_Bitmap(vi, bitmap);
	/* Wake things suspended on new holes. */
	/* XXX - maybe there are no new holes? */
	result = ec_schedule_susps(vi->attr + OFF_WAKE_HOLE);
	Return_If_Not_Success(result)
	/* Notify constrained. */
	return notify_constrained(vi->var);
}


/*
** Enforces initial consistency for ac_eq(X, Y, C).
*/
int
p_ac_eq_init(value vx, type tx, value vy, type ty, value vc, type tc)
{
	var_info	vix, viy;
	word	c;
	uword	*bitmap_x, *bitmap_y;
	double	lo, hi;
	int	result;

	Check_Integer(tc);
	Check_Ref(tx);
	Check_Ref(ty);

	c = vc.nint;
	if (c < MIN_BITMAP_RANGE || c > MAX_BITMAP_RANGE) {
	    Bip_Error(RANGE_ERROR)
	}

	get_var_info(vx.ptr, &vix);
	get_var_info(vy.ptr, &viy);

	/* Check that between them the variables have "reasonable" bounds. */
	if (c < 0) {
	    if (vix.tb.b.l < MIN_BITMAP_RANGE
		    && viy.tb.b.l < MIN_BITMAP_RANGE - c) {
		/* Lower bounds too low. */
		Bip_Error(RANGE_ERROR);
	    }
	    if (vix.tb.b.u > MAX_BITMAP_RANGE + c
		    && viy.tb.b.u > MAX_BITMAP_RANGE) {
		/* Upper bounds too high. */
		Bip_Error(RANGE_ERROR);
	    }
	} else {
	    if (vix.tb.b.l < MIN_BITMAP_RANGE + c
		    && viy.tb.b.l < MIN_BITMAP_RANGE) {
		/* Lower bounds too low. */
		Bip_Error(RANGE_ERROR);
	    }
	    if (vix.tb.b.u > MAX_BITMAP_RANGE
		    && viy.tb.b.u > MAX_BITMAP_RANGE - c) {
		/* Upper bounds too high. */
		Bip_Error(RANGE_ERROR);
	    }
	}

	/* Make sure both variables have bitmaps. */
	if (IsAtom(vix.bitmap->tag)) {
	    if (IsAtom(viy.bitmap->tag)) {
		/*
		** Neither variable has a bitmap; impose bounds and create
		** bitmaps.
		*/
		/* X */
		lo = max(vix.tb.b.l, viy.tb.b.l + c);
		hi = min(vix.tb.b.u, viy.tb.b.u + c);
		result = ic_lwb(&vix, lo);
		Return_If_Not_Success(result)
		result = ic_upb(&vix, hi);
		Return_If_Not_Success(result)
		result = create_bitmap((word) lo, (word) hi, &bitmap_x);
		Return_If_Not_Success(result)
		Update_Bitmap(&vix, bitmap_x);
		/* Y */
		lo = max(viy.tb.b.l, vix.tb.b.l - c);
		hi = min(viy.tb.b.u, vix.tb.b.u - c);
		result = ic_lwb(&viy, lo);
		Return_If_Not_Success(result)
		result = ic_upb(&viy, hi);
		Return_If_Not_Success(result)
		result = create_bitmap((word) lo, (word) hi, &bitmap_y);
		Return_If_Not_Success(result)
		Update_Bitmap(&viy, bitmap_y);
	    } else {
		/*
		** Y has a bitmap but X doesn't; impose bounds on Y (in case
		** the bitmap causes them to be tightened), then impose
		** bounds on X and copy Y's bitmap (actually, copy Y's
		** bitmap and extract bounds for X).
		*/
		/* Y */
		lo = max(viy.tb.b.l, vix.tb.b.l - c);
		hi = min(viy.tb.b.u, vix.tb.b.u - c);
		result = ic_lwb(&viy, lo);
		Return_If_Not_Success(result)
		result = ic_upb(&viy, hi);
		Return_If_Not_Success(result)
		/* X */
		if (viy.tb.b.l == viy.tb.b.u) {
		    /* Y is ground, so just ground X. */
		    Set_Var_To_Value(&vix, viy.tb.b.l + c);
		} else {
		    copy_bitmap_shifted(viy.bitmap->val.wptr, c, &bitmap_x);
		    result = sync_attr_with_new_bitmap(&vix, bitmap_x,
			    RES_CHANGED | RES_SLACK);
		    Return_If_Not_Success(result);
		}
	    }
	} else {
	    if (IsAtom(viy.bitmap->tag)) {
		/*
		** X has a bitmap but Y doesn't; impose bounds on X (in case
		** the bitmap causes them to be tightened), then impose
		** bounds on Y and copy X's bitmap (actually, copy X's
		** bitmap and extract bounds for Y).
		*/
		/* X */
		lo = max(vix.tb.b.l, viy.tb.b.l + c);
		hi = min(vix.tb.b.u, viy.tb.b.u + c);
		result = ic_lwb(&vix, lo);
		Return_If_Not_Success(result)
		result = ic_upb(&vix, hi);
		Return_If_Not_Success(result)
		/* Y */
		if (vix.tb.b.l == vix.tb.b.u) {
		    /* X is ground, so just ground Y. */
		    Set_Var_To_Value(&viy, vix.tb.b.l - c);
		} else {
		    copy_bitmap_shifted(vix.bitmap->val.wptr, -c, &bitmap_y);
		    result = sync_attr_with_new_bitmap(&viy, bitmap_y,
			    RES_CHANGED | RES_SLACK);
		    Return_If_Not_Success(result);
		}
	    } else {
		/* They both have bitmaps: intersect into each other. */
		result = bitmap_shifted_intersect_into(vix.bitmap->val.wptr,
			viy.bitmap->val.wptr, c, &bitmap_x);
		result = sync_attr_with_new_bitmap(&vix, bitmap_x, result);
		Return_If_Not_Success(result);
		result = bitmap_shifted_intersect_into(viy.bitmap->val.wptr,
			bitmap_x, -c, &bitmap_y);
		result = sync_attr_with_new_bitmap(&viy, bitmap_y, result);
		Return_If_Not_Success(result);
	    }
	}

	Succeed
}


/*
** Propagates ac_eq(X, Y, C).
*/
int
p_ac_eq_prop(value vx, type tx, value vy, type ty, value vc, type tc,
	value vsusp, type tsusp)
{
	var_info	vix, viy;
	word	c;
	uword	*bitmap_x, *bitmap_y;
	value	val;
	type	tag;
	int	result;

	c = vc.nint;

	if (!IsRef(tx)) {
	    val.nint = vx.nint - c;
	    tag.kernel = TINT;
	    result = Unify_Pw(vy, ty, val, tag);
	    Return_If_Not_Success(result)
	    Set_Susp_Dead(vsusp.ptr);
	    Succeed
	}

	if (!IsRef(ty)) {
	    val.nint = vy.nint + c;
	    tag.kernel = TINT;
	    result = Unify_Pw(vx, tx, val, tag);
	    Return_If_Not_Success(result)
	    Set_Susp_Dead(vsusp.ptr);
	    Succeed
	}

	/* X and Y are both still variables. */

	get_var_info(vx.ptr, &vix);
	get_var_info(vy.ptr, &viy);

	result = bitmap_shifted_intersect_into(vix.bitmap->val.wptr,
		viy.bitmap->val.wptr, c, &bitmap_x);
	result = sync_attr_with_new_bitmap(&vix, bitmap_x, result);
	Return_If_Not_Success(result);
	result = bitmap_shifted_intersect_into(viy.bitmap->val.wptr,
		bitmap_x, -c, &bitmap_y);
	result = sync_attr_with_new_bitmap(&viy, bitmap_y, result);
	Return_If_Not_Success(result);

	Succeed
}


int
p_prop_ic_con(value vcon, type tcon)
{
	con_info	con;
	prop_info	prop;
	int	result, short_cut, solved;

	Check_Structure(tcon);

	con_struct_vec_to_con_info(vcon.ptr, &con);

	if (OpIsNotEqual(con.op) && !con.reified) {
	    /* Disequality constraint. */
	    return prop_ic_neq_con(&con);
	}

	/* XXX - Should do first pass of propagation here, for use by all? */
	/* XXX - Not for short constraints? */
	result = prop_pass_1(&con, &prop);
	Return_If_Not_Success(result);

#ifdef IC_DEBUG
	fprintf(stderr, "prop_pass_1 completed successfully.\n");
#endif

	/* Check for constraints which can be turned into unifications. */
	Check_Con_Is_Unification(&con)

	if (prop.no_prop) {
	    Update_Con_Data_Buf_If_Needed(&con);
	    Succeed
	}

	if (con.reified) {
	    /* No propagation, just check entailment/disentailment. */
	    result = evaluate_reified(&con, &prop, &solved);
	    Return_If_Not_Success(result);
	    if (solved) {
		Mark_Constraint_Entailed(&con)
	    } else {
		Update_Con_Data_Buf_If_Needed(&con);
	    }
	    Succeed
	}

	/* Check short-cuts. */
	result = short_cuts(&con, &prop, &short_cut);
	Return_If_Not_Success(result);

	switch(short_cut) {
	    case SHORT_NONE:
		/* Continue execution. */
		break;
#ifdef USE_BOUND_SET_SHORTCUT
	    case SHORT_LWB:
		...
		break;
	    case SHORT_UPB:
		...
		break;
#endif
	    case SHORT_ENTAILED:
		/* Nothing more to do. */
		Mark_Constraint_Entailed(&con);
		Succeed
		break;
	    default:
		Bip_Error(EC_EXTERNAL_ERROR);
		break;
	}

	/*
	** Should switch on (say) term_count, but for now just call the
	** general propagation routine.
	*/
	switch (con.term_count) {
	    case 0:
		return check_ic_0v_con(&con);
		break;
	    case 1:
		return prop_ic_1v_con(&con, &prop);
		break;
/*
	    case 2:
		break;
*/
	    default:
		return prop_pass_2(&con, &prop);
		break;
	}
}



/*----------------------------------------------------------------------
**
** Functions for processing an ECLiPSe linear constraint.
**
*/


    /*
    ** type_check_lin_terms
    **	Check a list of linear terms for type errors (expected to be
    **	called when we've detected failure, but want to make sure there
    **	aren't any errors we should report instead).
    */
int
type_check_lin_terms(pword *plin, int flags)
{
	pword	*pterm, *tmp;
	int	idx;
	typed_bounds	a;

	BeginIterateLinList(plin, pterm, idx, a, tmp)
	    if ((flags & CON_INTEGRAL) && !a.i) {
		Bip_Error(TYPE_ERROR)
	    }

	    if (!IsRef(tmp->tag)) {
		if (flags & CON_INTEGRAL) {
		    Check_Integer_Or_Bignum(tmp->tag);
		} else {
		    Check_Number(tmp->tag);
		}
	    }
	EndIterateLinList(plin)

	Succeed
}


    /*
    ** setup_pass_1
    **	Check a list of linear terms for type errors, and constrain the
    **	variables to be of the appropriate type, while also performing the
    **	first pass of the two-pass linear constraint propagation algorithm
    **	and counting the number of non-ground terms.  :)
    */
int
setup_pass_1(pword *plin, con_info *con, prop_info *prop)
{
	pword	*pterm, *tmp;
	int	idx, maybe_prop_int;
	var_info	vi;
	typed_bounds	a;
	bounds	res;
	int	result;

#ifdef IC_DEBUG
	fprintf(stderr, "Entering setup_pass_1.\n");
#endif

#ifdef IC_DEBUG
	fprintf(stderr, " flags = %x, term_count = %d, cl = %f, cu = %f.\n",
		con->flags, con->term_count, con->c.l, con->c.u);
	fprintf(stderr, " op = %x, reified = %d.\n",
		con->op, con->reified);
#endif

	Init_Prop_Info(prop);

	/*
	** We want to consider propagating integrality only if the
	** constraint is a non-integer equation with reified boolean not 0.
	*/
	maybe_prop_int = (OpIsEqual(con->op) ||
			    (OpIsNotEqual(con->op) && con->reified))
			&& !(con->flags & CON_INTEGRAL);

#ifdef IC_DEBUG
	fprintf(stderr, " maybe_prop_int = %d.\n", maybe_prop_int);
	fprintf(stderr, " Commencing scan and e/f computation.\n");
#endif

	BeginIterateLinList(plin, pterm, idx, a, tmp)
#ifdef IC_DEBUG
	    fprintf(stderr, "  Term %d.\n", idx);
#endif

#ifdef IC_DEBUG
	    fprintf(stderr, "   al = %f, au = %f, ai = %d.\n", a.b.l, a.b.u, a.i);
#endif

	    if ((con->flags & CON_INTEGRAL) && !a.i) {
		Bip_Error(TYPE_ERROR)
	    }
	    maybe_prop_int &= a.i;

	    if (IsRef(tmp->tag)) {
#ifdef IC_DEBUG
		fprintf(stderr, "   Initial constraining of var.\n");
#endif
		/*
		** Make sure it's an IC var, and constrain it to be integral
		** if necessary.
		*/
		result = make_constrained_ic_var(&tmp, -HUGE_VAL, HUGE_VAL, con->flags & CON_INTEGRAL);
		Return_If_Not_Success(result)
	    }

	    if (IsRef(tmp->tag)) {
		/* Variable is still a variable. */
		con->term_count++;

		/* Don't bother updating E/F if there's no point. */
		if (!prop->no_prop) {
		    get_var_info(tmp, &vi);

#ifdef IC_DEBUG
		    fprintf(stderr, "   xl = %f, xu = %f, xi = %d.\n",
		    vi.tb.b.l, vi.tb.b.u, vi.tb.i);
#endif

		    if (!vi.tb.i) {
			prop->num_non_int_vars++;
			prop->non_int_idx = idx;
		    }

		    update_ef(prop, con, &a.b, &vi.tb.b, idx);
		}
	    } else {
		/* Variable is ground. */
#if 1
		/*
		** Don't count it as part of idx or infinity tracking won't
		** work properly (an inf idx is assumed to be less than
		** term_count).
		*/
		idx--;
#endif

		if (con->flags & CON_INTEGRAL) {
		    Check_Integer_Or_Bignum(tmp->tag);
		} else {
		    Check_Number(tmp->tag);
		}

		/* Don't bother updating E/F if there's no point. */
		if (!prop->no_prop) {
		    Constant_To_Typed_Bounds(tmp->val, tmp->tag, vi.tb);

#ifdef IC_DEBUG
		    fprintf(stderr, "   xl = %f, xu = %f, xi = %d.\n",
		    vi.tb.b.l, vi.tb.b.u, vi.tb.i);
		    fprintf(stderr, "   Adjusting e/f for ground term.\n");
#endif

		    maybe_prop_int &= vi.tb.i;

		    Mul(a.b, vi.tb.b, res);
		    Sub(prop->sum, res, prop->sum);

#ifdef IC_DEBUG
		    fprintf(stderr, "   term lo = %f, hi = %f.\n", res.l, res.u);
		    fprintf(stderr, "   neg_e = %f, f = %f.\n", prop->sum.l, prop->sum.u);
#endif
		}
	    }
	EndIterateLinList(plin)

#ifdef IC_DEBUG
	fprintf(stderr, " Final neg_e = %f, f = %f.\n", prop->sum.l, prop->sum.u);
#endif

	/*
	** Don't bother trying to propagate integrality later if all the
	** variables are already integral.
	*/
	if (maybe_prop_int && prop->num_non_int_vars >= 1) {
	    con->flags |= CON_INTEGRALITY_PROP;

#ifdef IC_DEBUG
	    fprintf(stderr, " Adjusted flags = %d.\n", con->flags);
#endif
	}

	Succeed
}


    /*
    ** process_lin_terms_to_vectors_and_prop
    **	Set up & fill in the coefficient and variable vectors, imposing the
    **	relevant propagation bounds in the process.  Also set up the
    **	constraint data buffer.
    */
int
process_lin_terms_to_vectors_and_prop(pword *plin, con_info *con,
		prop_info *prop)
{
	pword	*pterm, *tmp;
	pword	goal;
	pword	*lo_buf, *hi_buf, *var_buf;
	dident	did;
	int	count = 0;
	typed_bounds	a;
	var_info	vi;
	bounds	res;
	int	strict;
	int	idx, result;
	int	delayed = 0;
	int	pseudo_op, pseudo_op0;
	int	off_pos, off_neg;

#ifdef IC_DEBUG
	fprintf(stderr, "Entering process_lin_terms_to_vectors_and_prop.\n");
#endif

#ifdef IC_DEBUG
	fprintf(stderr, " flags = %x, term_count = %d, cl = %f, cu = %f.\n",
		con->flags, con->term_count, con->c.l, con->c.u);
	fprintf(stderr, " op = %x, reified = %d.\n",
		con->op, con->reified);
#endif

	/*
	** We allocate the variable vector first, so that if we discover the
	** coefficient vectors are identical, we can easily throw one away.
	**
	** Note that we can't automatically assume the vectors are identical
	** for integer constraints, since they may contain bignums not
	** exactly representable as doubles.  :(
	**
	** Don't change the order of these allocations without updating the
	** rest of the code.
	*/

	if (con->term_count > 0) {
	    var_buf = TG;
	    con->var_vec = VarVec(var_buf);
	    did = ec_did("[]", con->term_count);
	    Push_Struct_Frame(did);

	    lo_buf = TG;
	    con->lo_vec = CoefVec(lo_buf);
	    Push_Buffer(con->term_count * sizeof(double));

	    hi_buf = TG;
	    con->hi_vec = CoefVec(hi_buf);
	    Push_Buffer(con->term_count * sizeof(double));
	} else {
	    /* XXX - do we want to arrange that this cannot occur? */
	    /*
	    Make_Nil(TG);
	    TG->val.ptr = 0;	* For reproducible bugs...  :) *
	    var_vec = TG;
	    lo_vec = TG;
	    hi_vec = TG;
	    TG++;
	    */
#ifdef IC_DEBUG
	    var_buf = 0;
	    lo_buf = 0;
	    hi_buf = 0;
	    con->var_vec = 0;
	    con->lo_vec = 0;
	    con->hi_vec = 0;
#endif
	}

	/* Create the suspension. */
	Make_Struct(&goal, TG);
	Push_Struct_Frame(d_prop_ic_con);
	goal.val.ptr[1].val.ptr = con->con;
	goal.val.ptr[1].tag.kernel = TCOMP;
	result = ec_make_suspension(goal, ConstraintPriority(con),
			proc_prop_ic_con, con->con + CON_OFF_SUSP);
	if (result == DEBUG_SUSP_EVENT) {
	    delayed = 1;
	    result = PSUCCEED;
	}
	Return_If_Not_Success(result);
	con->susp = con->con + CON_OFF_SUSP;
	Dereference(con->susp);

	con->c.l = NEG_ZERO;
	con->c.u = 0.0;

	/*
	** Compute the default "pseudo" op; i.e. leaving out propagating
	** bounds involving at least one infinity.
	*/
	pseudo_op0 = con->op;
	if (prop->inf_f_idx >= 0) {
	    pseudo_op0 &= ~UPPER_BOUND;
	}
	if (prop->inf_e_idx >= 0) {
	    pseudo_op0 &= ~LOWER_BOUND;
	}

#ifdef IC_DEBUG
	fprintf(stderr, " pseudo_op0 = %d.\n", pseudo_op0);
#endif

	if ((!(con->flags & CON_INTEGRALITY_PROP)) || prop->num_non_int_vars != 1) {
	    prop->non_int_idx = -1;
	}

	strict = OpIsGreater(con->op);

	/*
	** If it's an inequality, pre-compute which bounds to wake on for
	** positive & negative coefficients.
	*/
	if (OpIsGreater(con->op)) {
	    off_pos = OFF_WAKE_HI;
	    off_neg = OFF_WAKE_LO;
	} else if (OpIsLessEqual(con->op)) {
	    off_pos = OFF_WAKE_LO;
	    off_neg = OFF_WAKE_HI;
	}

#ifdef IC_DEBUG
	fprintf(stderr, " Commencing setup and propagation pass.\n");
#endif

	BeginIterateLinList(plin, pterm, idx, a, tmp)
#ifdef IC_DEBUG
	    fprintf(stderr, "  Term %d.\n", idx);
	    fprintf(stderr, "   al = %f, au = %f.\n", a.b.l, a.b.u);
#endif

	    if (!IsRef(tmp->tag)) {
		/* Variable (X) is ground. */
#if 1
		/*
		** Don't count it as part of idx or infinity tracking won't
		** work properly (an inf idx is assumed to be less than
		** term_count).
		*/
		idx--;
#endif

		Constant_To_Typed_Bounds(tmp->val, tmp->tag, vi.tb);

#ifdef IC_DEBUG
		fprintf(stderr, "   xl = %f, xu = %f, xi = %d (ground).\n",
		vi.tb.b.l, vi.tb.b.u, vi.tb.i);
#endif

		/* Subtract A * X from RHS. */
		Mul(a.b, vi.tb.b, res);
		Sub(con->c, res, con->c);

#ifdef IC_DEBUG
		fprintf(stderr, "   Adjusted c: cl = %f, cu = %f.\n", con->c.l, con->c.u);
#endif
	    } else {
		/* Variable is still a variable, so propagate, and put */
		/* term in vector. */
		get_var_info(tmp, &vi);

#ifdef IC_DEBUG
		fprintf(stderr, "   xl = %f, xu = %f, xi = %d.\n",
		vi.tb.b.l, vi.tb.b.u, vi.tb.i);
#endif

		if (con->reified) {
		    /*
		    ** Reified constraint.
		    ** Wake on either bound change.
		    */
		    result = ec_enter_suspension(vi.attr + OFF_WAKE_LO,
			    con->susp->val.ptr);
		    Return_If_Not_Success(result)
		    result = ec_enter_suspension(vi.attr + OFF_WAKE_HI,
			    con->susp->val.ptr);
		    Return_If_Not_Success(result)

		    Make_Ref(con->var_vec + count, tmp);
		    con->lo_vec[count] = a.b.l;
		    con->hi_vec[count] = a.b.u;
		    count++;
		} else {
		    switch(con->op) {
			case LOWER_BOUND:
			case UPPER_BOUND:
			    /*
			    ** We have a (non-reified) inequality.
			    ** Wake on appropriate bound only.
			    */
			    if (a.b.l > 0.0) {
				result = ec_enter_suspension(vi.attr + off_pos,
					con->susp->val.ptr);
				Return_If_Not_Success(result)
				break;
			    } else if (a.b.u < 0.0) {
				result = ec_enter_suspension(vi.attr + off_neg,
					con->susp->val.ptr);
				Return_If_Not_Success(result)
				break;
			    }
			    /* Coefficient spans zero: wake on either bound. */

			    /* === Fall through to equation case === */

			case LOWER_BOUND + UPPER_BOUND:
			    /*
			    ** We have an equation.
			    ** Wake on either bound change.
			    */
			    result = ec_enter_suspension(vi.attr + OFF_WAKE_LO,
				    con->susp->val.ptr);
			    Return_If_Not_Success(result)
			    result = ec_enter_suspension(vi.attr + OFF_WAKE_HI,
				    con->susp->val.ptr);
			    Return_If_Not_Success(result)
			    break;

			case 0:
			    /*
			    ** We have a (non-reified) disequality.
			    ** Wake on instantiation only.
			    */
			    result = insert_suspension(vi.var, 1,
				    con->susp->val.ptr, suspend_slot);
			    Return_If_Not_Success(result)
			    break;

			default:
			    Bip_Error(EC_EXTERNAL_ERROR);
		    }

		    /*
		    ** If we might want to make this variable integral, mark
		    ** it so.
		    */
		    vi.prop_int = (idx == prop->non_int_idx);

		    if (!prop->no_prop) {
			Mul(a.b, vi.tb.b, res);

			/* Adjust any infinite bounds. */
			/*
			** XXX - should go in macro to make sure done
			** exactly the same way everywhere.
			** XXX - except then can't interleave the pseudo_op
			** stuff.  :)
			*/
			pseudo_op = pseudo_op0;
			if (res.l == -HUGE_VAL) {
			    res.l = min(0.0, res.u);
			    /* If appropriate, propagate the bound for this term. */
			    if (idx == prop->inf_f_idx) {
				pseudo_op |= (con->op & UPPER_BOUND);
			    }
			}
			if (res.u == HUGE_VAL) {
			    res.u = max(0.0, res.l);
			    /* If appropriate, propagate the bound for this term. */
			    if (idx == prop->inf_e_idx) {
				pseudo_op |= (con->op & LOWER_BOUND);
			    }
			}

			/* Don't do any more work if there's nothing to do. */
			/* (Besides, impose_coef_bounds requires op != 0.) */
			if (pseudo_op) {
			    Undo_Sub(prop->sum, res, res);

#ifdef IC_DEBUG
			    fprintf(stderr, "   lo = %f, hi = %f.\n", res.l, res.u);
#endif

			    result = impose_coef_bounds(pseudo_op, strict,
			    &a.b, &vi, &res);
			    Return_If_Not_Success(result);
			}
		    }

		    /*
		    ** If we might want to propagate integrality to this
		    ** variable, and it has a unit coefficient, then
		    ** constrain it to be integral.  Note that we can't do
		    ** this before imposing the bounds above, since the
		    ** integrality may alter the variable's bounds, which
		    ** would mess up the propagation algorithm.
		    */
		    if (vi.prop_int && (a.b.l == -1.0 || a.b.l == 1.0)) {
#ifdef IC_DEBUG
			fprintf(stderr, "   Propagating integrality.\n");
#endif
			result = set_integral(&vi);
			Return_If_Not_Success(result);
			con->flags -= CON_INTEGRALITY_PROP;
		    }

#ifdef IC_DEBUG
		    fprintf(stderr, "   new xl = %f, new xu = %f.\n",
		    vi.tb.b.l, vi.tb.b.u);
#endif

		    /*
		    ** Propagation and/or integer rounding may have caused X
		    ** to become ground...
		    */
		    if (vi.tb.b.l == vi.tb.b.u) {
			/* X is ground. */
#ifdef USE_BOUND_SET_SHORTCUT
			/* Assume X is integral, so don't update flags. */
#else
			if ((con->flags & CON_INTEGRALITY_PROP) && !vi.tb.i) {
			    /* No integrality to propagate. */
			    con->flags &= ~CON_INTEGRALITY_PROP;
			}
#endif

			/* Adjust RHS constant. */
			Mul(a.b, vi.tb.b, res);
			Sub(con->c, res, con->c);
		    } else {
			Make_Ref(con->var_vec + count, tmp);
			con->lo_vec[count] = a.b.l;
			con->hi_vec[count] = a.b.u;
			count++;
		    }
		}
	    }
	EndIterateLinList(plin)

#ifdef IC_DEBUG
	fprintf(stderr, " flags = %x, term_count = %d, cl = %f, cu = %f.\n",
		con->flags, con->term_count, con->c.l, con->c.u);
	fprintf(stderr, " op = %x, reified = %d.\n",
		con->op, con->reified);
#endif

	/*
	** Catch some annoying cases where the RHS constant is an infinity
	** and clean them up.
	*/
	if (con->c.l == HUGE_VAL) {
	    if (OpIsLessEqual(con->op)) {
		/* Assume the constraint is entailed. */
		if (con->reified) {
		    Unify_Boolean(con->bool, 1, result);
		    Return_If_Not_Success(result);
		    con->reified = 0;
		}
		Mark_Constraint_Entailed(con)
	    } else if (OpIsGreater(con->op)) {
		/* Assume the constraint fails. */
		if (con->reified) {
		    Unify_Boolean(con->bool, 0, result);
		    Return_If_Not_Success(result);
		    con->reified = 0;
		    Mark_Constraint_Entailed(con)
		} else {
		    Fail
		}
	    }
	}

	if (con->reified) {
	    /* Suspend on boolean instantiation. */
	    result = insert_suspension(con->bool, 1, con->susp->val.ptr,
		    suspend_slot);
	    Return_If_Not_Success(result)
	}

	finish_setting_up_con_struct(con, lo_buf, hi_buf, var_buf, count);

	/* Check for constraints which can be turned into unifications. */
	Check_Con_Is_Unification(con)

	return delayed ? DEBUG_SUSP_EVENT : PSUCCEED;
}


#if 0
    /*
    ** If the first setup pass discovered there were no variables, determine
    ** whether the constraint succeeds/fails, or requires a delayed goal.
    ** XXX - should really set up the delayed goal here, 
    */
int
setup_0v_con(con_info *con, prop_info *prop, int *solved)
{
	*solved = 1;

	switch (con->op) {
	    case UPPER_BOUND + LOWER_BOUND:
		/* Equation. */
		if (0.0 <= con->c.l && 0.0 >= con->c.u) {
		    /* Entailed. */
		    Succeed
		} else if (0.0 > con->c.u || 0.0 < con->c.l) {
		    /* Failure. */
		    Fail
		}
		break;

	    case UPPER_BOUND:
		/* Less than or equal constraint. */
		if (0.0 <= con->c.l) {
		    /* Entailed. */
		    Succeed
		} else if (0.0 > con->c.u) {
		    /* Failure. */
		    Fail
		}
		break;

	    case LOWER_BOUND:
		/* Greater than constraint. */
		if (0.0 > con->c.u) {
		    /* Entailed. */
		    Succeed
		} else if (0.0 <= con->c.l) {
		    /* Failure. */
		    Fail
		}
		break;

	    case 0:
		/* Disequality constraint. */
		if (0.0 < con->c.l || 0.0 > con->c.u) {
		    /* Entailed. */
		    Succeed
		} else if (0.0 == con->c.l && 0.0 == con->c.u) {
		    /* Failure. */
		    Fail
		}
		break;
	}

	*solved = 0;
	Succeed
}
#endif


int
setup_1v_con(pword *plin, con_info *con, prop_info *prop, int *solved)
{
	int	idx, result;
	pword	*tmp, *pterm;
	typed_bounds	a;
	bounds	res, sum;
	var_info	vi;

#ifdef IC_DEBUG
	fprintf(stderr, "Entering setup_1v_con.\n");
#endif

	*solved = 0;

#ifdef IC_DEBUG
	fprintf(stderr, " flags = %x, term_count = %d, cl = %f, cu = %f.\n",
		con->flags, con->term_count, con->c.l, con->c.u);
	fprintf(stderr, " reified = %d, op = %x.\n", con->reified, con->op);
#endif

	sum = con->c;	/* Probably zero? */

	BeginIterateLinList(plin, pterm, idx, a, tmp)
	    if (!IsRef(tmp->tag)) {
		Constant_To_Typed_Bounds(tmp->val, tmp->tag, vi.tb);

		/* Subtract A * X from sum. */
		Mul(a.b, vi.tb.b, res);
		Sub(sum, res, sum);
	    } else {
		get_var_info(tmp, &vi);
		vi.prop_int = (con->flags & CON_INTEGRALITY_PROP);

#ifdef IC_DEBUG
		fprintf(stderr, " xl = %f, xu = %f, xi = %d.\n", vi.tb.b.l,
		vi.tb.b.u,
			vi.tb.i);
#endif

		Mul(a.b, vi.tb.b, res);

		/* XXX - Why aren't we using inf_f_idx/inf_e_idx for this? */
		if (res.l == -HUGE_VAL) {
		    res.l = min(0.0, res.u);
		}
		if (res.u == HUGE_VAL) {
		    res.u = max(0.0, res.l);
		}

#if 0
		if (prop->inf_f_idx != idx) {
		    prop->sum.u = HUGE_VAL;
		}
		if (prop->inf_e_idx != idx) {
		    prop->sum.l = -HUGE_VAL;
		}
#endif

#ifdef IC_DEBUG
		fprintf(stderr, "   prop.sum.l = %f, prop.sum.u = %f.\n", prop->sum.l, prop->sum.u);
		fprintf(stderr, "   res.l = %f, res.u = %f.\n", res.l, res.u);
#endif

		Undo_Sub(prop->sum, res, res);

#ifdef IC_DEBUG
		fprintf(stderr, "   lo = %f, hi = %f.\n", res.l, res.u);
#endif

		if (OpIsEqual(con->op)) {
		    return solve_equal(&a.b, &vi, &res, solved);
		} else if (OpIsNotEqual(con->op)) {
		    return solve_not_equal(&a.b, &vi, &res, solved);
		} else {
		    result = impose_coef_bounds(con->op,
		    OpIsGreater(con->op), &a.b, &vi, &res);
		    Return_If_Not_Success(result);
		}

		/* Subtract A * X from sum. */
		Mul(a.b, vi.tb.b, res);
		Sub(sum, res, sum);
	    }
	EndIterateLinList(plin)

#ifdef IC_DEBUG
	fprintf(stderr, " sum.l = %f, sum.u = %f.\n", sum.l, sum.u);
#endif

	/* To get here, it must be an inequality. */
	if (OpIsGreater(con->op) && 0.0 <= sum.l) {
	    Fail
	}
	if (OpIsLessEqual(con->op) ? 0.0 <= sum.l : 0.0 > sum.u) {
	    *solved = 1;
	}

	Succeed
}


    /*
    ** set_up_ic_con(++Flags, ?Bool, +Lin)
    **	Sets up a linear constraint, Lin Op 0, where Op is encoded by Flags
    **	and Bool gives the reification status of the constraint.
    */
int
p_set_up_ic_con(value vflags, type tflags, value vbool, type tbool, value vlin, type tlin)
{
	int	    result, result2;
	con_info    con;
	prop_info   prop;
	pword	    lin;
	int	    status, delayed, short_cut, solved;

	Check_Integer(tflags);

	lin.val = vlin;
	lin.tag = tlin;

	/* Make sure bool is a boolean. */
	result = p_make_bool(vbool, tbool);
	Return_If_Error(result);
	if (result != PSUCCEED) {
	    /* Don't fail until we've finished type checking. */
	    result2 = type_check_lin_terms(&lin, vflags.nint);
	    Return_If_Not_Success(result2);
	    return result;
	}

	/* Make sure bool is fully dereferenced again. */
	if (IsRef(tbool)) {
	    pword   *bool = vbool.ptr;
	    Dereference(bool);
	    vbool.all = bool->val.all;
	    tbool.all = bool->tag.all;
	}

	/*
	** Set status to the effective value of the boolean (treat it as
	** true if the boolean is still a variable).
	*/
	if (IsRef(tbool)) {
	    status = 1;
	    con.bool = vbool.ptr;
	    con.reified = 1;
	} else {
	    status = vbool.nint;
	    con.bool = 0;
	    con.reified = 0;
	}

	con.c.l = NEG_ZERO;
	con.c.u = 0.0;
	con.flags = vflags.nint;
	con.term_count = 0;
	con.op = FlagsAndBoolToOp(con.flags, status);
	/* Don't try to access these yet... */
	con.con = 0;
	con.data = 0;
	con.lo_vec = 0;
	con.hi_vec = 0;
	con.var_vec = 0;
	con.susp = 0;

	/* Just do a type check and count the terms for now. */
	result = setup_pass_1(&lin, &con, &prop);
	Return_If_Not_Success(result);

	if (con.reified) {
	    /* No propagation, just check entailment/disentailment. */
	    /* Already know the answer if no_prop has been set. */
	    if (!prop.no_prop) {
		result = evaluate_reified(&con, &prop, &solved);
		Return_If_Not_Success(result);
		if (solved) {
		    Succeed
		}
		/*
		** Make sure we don't try to propagate later during
		** constraint set up.
		*/
		prop.no_prop = 1;
	    }
	} else {
	    /* Check short-cuts. */
	    result = short_cuts(&con, &prop, &short_cut);
	    Return_If_Not_Success(result);

	    switch(short_cut) {
		case SHORT_NONE:
		    /* Continue execution. */
		    break;
#ifdef USE_BOUND_SET_SHORTCUT
		case SHORT_LWB:
		    ...
		    break;
		case SHORT_UPB:
		    ...
		    break;
#endif
		case SHORT_ENTAILED:
		    /* Nothing more to do. */
		    Succeed
		    break;
		default:
		    Bip_Error(EC_EXTERNAL_ERROR);
		    break;
	    }
	}

	/*
	** XXX - check for zero-variable constraints?
	** "Solved" ones should already by short-cutted by the above, but
	** delayed ones could be set up simpler/faster than using the
	** general approach below.
	*/
#if 0
	if (con.term_count == 0) {
	    result = setup_0v_con(&con, &prop, &solved);
	    Return_If_Not_Success(result)
	    if (solved) {
		Succeed
	    }
	} else
#endif

	/*
	** XXX - check for one-variable constraints (likely to be entailed
	** after propagation if all constants are zero-width).
	*/
	if (!prop.no_prop && !con.reified && con.term_count == 1) {
	    result = setup_1v_con(&lin, &con, &prop, &solved);
	    Return_If_Not_Success(result)
	    if (solved) {
		Succeed
	    }
	    /*
	    ** We've already propagated, and doing so again during
	    ** constraint set up will give the wrong answer if infinities
	    ** are involved, so make sure we don't.
	    */
	    prop.no_prop = 1;
	}

	/* XXX - check for two-variable equations to make unifications. */

	/* Create the ic_con structure. */
	con.con = TG;
	Push_Struct_Frame(d_ic_con);

	con.con[CON_OFF_BOOLEAN].val.all = vbool.all;
	con.con[CON_OFF_BOOLEAN].tag.all = tbool.all;

	delayed = 0;
	result = process_lin_terms_to_vectors_and_prop(&lin, &con, &prop);
	if (result == DEBUG_SUSP_EVENT) {
	    delayed = 1;
	    result = PSUCCEED;
	}
	if (result != PSUCCEED) {
	    /* Restore global stack to remove any garbage we added. */
	    TG = con.con;
	    return result;
	}

	return delayed ? DEBUG_SUSP_EVENT : PSUCCEED;
}

int
p_get_print_info(value vcon, type tcon, value vflags, type tflags,
		value vbool, type tbool, value vcoefs, type tcoefs,
		value vvars, type tvars, value vc, type tc)
{
	con_info	con;
	pword	*dest;
	int	result, i;
	bounds	a;
	value	val;
	type	tag;
	dident	did;
	pword	res;

	/* XXX - check that it really is an IC constraint structure. */
	Check_Structure(tcon);

	con_struct_vec_to_con_info(vcon.ptr, &con);

	result = Unify_Pw(vbool, tbool,
		con.con[CON_OFF_BOOLEAN].val, con.con[CON_OFF_BOOLEAN].tag);
	Return_If_Not_Success(result);

	val.nint = con.flags;
	tag.kernel = TINT;
	result = Unify_Pw(vflags, tflags, val, tag);
	Return_If_Not_Success(result);

	Bounds_To_Constant(con.c.l, con.c.u, &res);
	result = Unify_Pw(vc, tc, res.val, res.tag);
	Return_If_Not_Success(result);

	if (con.term_count > 0) {
	    did = ec_did("[]", con.term_count);
	    val.ptr = TG;
	    tag.kernel = TCOMP;
	    Push_Struct_Frame(did);

	    for (i = 0; i < con.term_count; i++) {
		a.l = con.lo_vec[i];
		a.u = con.hi_vec[i];
		dest = val.ptr + i + 1;

		Bounds_To_Constant(a.l, a.u, dest);
	    }
	    result = Unify_Pw(vcoefs, tcoefs, val, tag);
	    Return_If_Not_Success(result);

	    val.ptr = TG;
	    Push_Struct_Frame(did);
	    /* Copy the first term_count elements from the variable array. */
	    memcpy(val.ptr + 1, con.var_vec, con.term_count * sizeof(pword));
	    result = Unify_Pw(vvars, tvars, val, tag);
	    Return_If_Not_Success(result);
	} else {
	    val.ptr = 0;
	    tag.kernel = TNIL;

	    result = Unify_Pw(vcoefs, tcoefs, val, tag);
	    Return_If_Not_Success(result);
	    result = Unify_Pw(vvars, tvars, val, tag);
	    Return_If_Not_Success(result);
	}

	Succeed
}


#if 0
int
prop_0v(int flags, pword *bool, double cl, double cu, value vsusp, type tsusp, ...)
{
	if (flags & CON_EQUATION) {
	    /* Equation or disequation. */
	    ...
	} else {
	    /* Inequality. */
	    ...
	}

	...
}
#endif


/*----------------------------------------------------------------------
**
** Support for ic_kernel.ecl.
**
** The functions in this section are mainly in support of the ic_kernel
** module.
**
*/

    /*
    ** ic_init()
    **      Caches IC attribute slot and auxiliary dictionary
    **      entries needed required for attribute
    **      extraction/creation from/of an IC variable.
    */
int
p_ic_init()
{
	pword	module;
	int	result;

	ic_domain_slot = meta_index(ec_did("ic", 0));
	suspend_slot = meta_index(ec_did("suspend", 0));
	d_ic_attr_functor = ec_did("ic", ATTR_ARITY);
	d_ic_con = ec_did("ic_con", CON_ARITY);
	d_ic_real = ec_did("real", 0);
	d_ic_integer = ec_did("integer", 0);
	d_ic_undefined = ec_did("undefined", 0);

	module.val.did = ec_did("ic_kernel", 0);
	module.tag.kernel = TDICT;

/*
	d_prop_ic_con = ec_did("prop_ic_con", 1);
	result = ec_visible_procedure(d_prop_ic_con, module,
		&proc_prop_ic_con);
	Return_If_Not_Success(result);
*/

	d_exclude = ec_did("exclude", 2);
	result = ec_visible_procedure(d_exclude, module, &proc_exclude);
	Return_If_Not_Success(result);

	d_exclude_range = ec_did("exclude_range", 3);
	result = ec_visible_procedure(d_exclude_range, module,
		&proc_exclude_range);
	Return_If_Not_Success(result);

	result = ec_visible_procedure(d_.infq, module, &proc_infq);
	Return_If_Not_Success(result);
	result = ec_visible_procedure(d_.supq, module, &proc_supq);
	Return_If_Not_Success(result);

	Succeed;
}

    /*
    ** ic_constraints_init()
    **      Initialisation for ic_constraints.
    */
int
p_ic_constraints_init()
{
	pword	module;
	int	result;

	module.val.did = ec_did("ic_constraints", 0);
	module.tag.kernel = TDICT;

	d_prop_ic_con = ec_did("prop_ic_con", 1);
	result = ec_visible_procedure(d_prop_ic_con, module,
		&proc_prop_ic_con);
	Return_If_Not_Success(result);

	Succeed;
}


    /*
    ** get_ic_attr(?Var, -Attr)
    **	    Returns the IC attribute of a variable.  If the variable does
    **	    not have an IC attribute, it is given one first.  Attr must be a
    **	    fresh variable.
    */   
int
p_get_ic_attr(value vvar, type tvar, value vattr, type tattr)
{
	pword *res;

	if (!IsRef(tvar)) {
	    Fail
	}

	res = make_ic_var_attr(vvar.ptr);
	Return_Bind_Var(vattr, tattr, res->val.all, res->tag.kernel);
}


    /*
    ** Return the given bounds as integers if possible, or doubles if not
    ** (i.e. because they're infinite).
    ** Assumes the lower bound cannot be +inf or the upper bound -inf.
    */
int
unify_integer_bounds(double lwb, double upb, value vlo, type tlo, value vhi, type thi)
{
	value	val;
	type	tag;
	int	result;

	tag.kernel = TDBL;

	/* Lower bound. */
	Make_Double_Val(val, lwb);
	if (lwb != -HUGE_VAL) {
	    /* Return an integer. */
	    result = unary_arith_op(val, tag, vlo, tlo, ARITH_FIX, TINT);
	} else {
	    result = Unify_Pw(vlo, tlo, val, tag);
	}
	Return_If_Not_Success(result);

	/* Upper bound. */
	Make_Double_Val(val, upb);
	if (upb != HUGE_VAL) {
	    /* Return an integer. */
	    result = unary_arith_op(val, tag, vhi, thi, ARITH_FIX,
		    TINT);
	} else {
	    result = Unify_Pw(vhi, thi, val, tag);
	}

	return result;
}

    /*
    ** get_bounds(?Var, -Lo, -Hi)
    **	    Unifies Lo and Hi with the lower and upper (resp.) IC bounds of
    **	    the variable Var.  If Var is an integer variable then the bounds
    **	    returned will be integer (unless they're infinite).  Also works
    **	    if Var is a ground number.
    */
int
p_get_bounds(value vvar, type tvar, value vlo, type tlo, value vhi, type thi)
{
	value	val;
	type	tag;
	int	result;

	tag.kernel = TDBL;

	if (IsRef(tvar)) {
	    var_info    vi;
	    /* For variables, extract the bounds and return them. */
	    make_var_info(vvar.ptr, &vi);
	    if (vi.tb.i) {
		return unify_integer_bounds(vi.tb.b.l, vi.tb.b.u, vlo, tlo, vhi, thi);
	    } else {
		Make_Double_Val(val, vi.tb.b.l);
		result = Unify_Pw(vlo, tlo, val, tag);
		Return_If_Not_Success(result);
		Make_Double_Val(val, vi.tb.b.u);
		return Unify_Pw(vhi, thi, val, tag);
	    }
	} else if (IsInteger(tvar) || IsBignum(tvar) || IsDouble(tvar)) {
	    /* For integers and floats, the bounds are equal to the input. */
	    result = Unify_Pw(vlo, tlo, vvar, tvar);
	    Return_If_Not_Success(result);
	    return Unify_Pw(vhi, thi, vvar, tvar);
	} else if (IsNumber(tvar)) {
	    /* For other numbers, coerce to bounded real and return bounds. */
	    value breal;
	    result = tag_desc[TagType(tvar)].coerce_to[TIVL](vvar, &breal);
	    Return_If_Not_Success(result);
	    Make_Double_Val(val, IvlLwb(breal.ptr));
	    result = Unify_Pw(vlo, tlo, val, tag);
	    Return_If_Not_Success(result);
	    Make_Double_Val(val, IvlUpb(breal.ptr));
	    return Unify_Pw(vhi, thi, val, tag);
	} else {
	    Bip_Error(TYPE_ERROR)
	}
	/* Not reached. */
}


    /*
    ** get_integer_bounds1(?Var, ++Finite, -Lo, -Hi, -Wake)
    **	    Unifies Lo and Hi with the lower and upper (resp.) IC bounds of
    **	    the variable X.  Var is constrained to be an integer IC
    **	    variable.  If Finite == 1 then Var is also constrained to be
    **	    finite (-10000000..10000000 by default).  Wake must be a fresh
    **	    variable, and is set to 1 if something has changed which
    **	    warrants a call to wake/0 (otherwise it's set to 0).
    **	    The bounds returned will be integer unless they're infinite.
    **	    Var must be a variable.
    */
int
p_get_integer_bounds1(value vvar, type tvar, value vfinite, type tfinite,
	value vlo, type tlo, value vhi, type thi, value vwake, type twake)
{
	var_info	vi;
	int	wake = 0;
	int	result;

	Check_Ref(tvar);
	Check_Integer(tfinite);

	make_var_info(vvar.ptr, &vi);

	/* Make sure the variable is integral. */
	if (!vi.tb.i) {
	    /* Update the type. */
	    result = set_integral(&vi);
	    Return_If_Not_Success(result);
	    wake = 1;
	}

	/* Make sure the bounds are finite if that's required. */
	if (vfinite.nint) {
	    if (vi.tb.b.l == -HUGE_VAL) {
		result = ic_lwb(&vi, -8000000000.0);
		Return_If_Not_Success(result);
		wake = 1;
	    }
	    if (vi.tb.b.u == HUGE_VAL) {
		result = ic_upb(&vi, 8000000000.0);
		Return_If_Not_Success(result);
		wake = 1;
	    }
	}

	/* Return the bounds. */
	result = unify_integer_bounds(vi.tb.b.l, vi.tb.b.u, vlo, tlo, vhi, thi);
	Return_If_Not_Success(result);

	Return_Integer(vwake, twake, wake);

	Succeed
}


    /*
    ** get_domain_size(?Var, -Size)
    **	    Returns the number of integer elements in the domain of Var.
    **	    Returns 1 if Var is a number.
    **	    Throws a range error if Var is a real variable.
    */   
int
p_get_domain_size(value vvar, type tvar, value vsize, type tsize)
{
	pword	*attr;
	double	lwb, upb;
	dident	ic_type;
	pword	*bitmap;
	pword   res;
	int	result;

	if (IsNumber(tvar)) {
	    /* Unify Size with 1. */
	    res.val.nint = 1;
	    res.tag.kernel = TINT;
	    Return_Unify_Pw(vsize, tsize, res.val, res.tag);
	}
	if (!IsMeta(tvar)) {
	    /* Not a variable or no attributes. */
	    Bip_Error(TYPE_ERROR)
	}
	IC_Var_Get_Attr(vvar.ptr, attr);
	if (IsRef(attr->tag)) {
	    /* No IC attribute. */
	    Bip_Error(TYPE_ERROR)
	}
	/* Get the attribute's vector. */
	attr = attr->val.ptr;
	Dereference(attr);

	IC_Var_Info(attr, lwb, upb, ic_type, bitmap);

	if (!Type_Is_Int(ic_type)) {
	    /* Not an integer IC variable. */
	    Bip_Error(RANGE_ERROR)
	}

	if (IsAtom(bitmap->tag)) {
	    /*
	    ** No bitmap: size is just one more than the difference between
	    ** the bounds.
	    ** XXX - Claim that we don't care if it's inaccurate for domains
	    ** which extend beyond the float-representable integers, since
	    ** the domain can't represent them all anyway.  Hence don't
	    ** bother being careful with the float operations...
	    */
	    double  fsize;

	    fsize = upb - lwb + 1;
	    if (fsize == HUGE_VAL) {
		/* For compatibility with old version, not unreasonable... */
		Make_Double_Val(res.val, fsize);
		res.tag.kernel = TDBL;
	    } else {
		result = ec_double_to_int_or_bignum(fsize, &res);
		Return_If_Not_Success(result);
	    }
	} else {
	    res.val.nint = bitmap_size(bitmap->val.wptr);
	    res.tag.kernel = TINT;
	}

	Return_Unify_Pw(vsize, tsize, res.val, res.tag);
}


/*----------------------------------------------------------------------
**
** Unification helper predicate.
**
*/

    /*
    ** Check that the number represented by vnum/tnum is in the domain
    ** represented by attr2 and wake any appropriate suspension lists.
    */
int
unify_number_ic(value vnum, type tnum, pword *attr2vec)
{
	double	cl, cu;
	int	ci;
	double	lwb, upb;
	dident	ic_type;
	pword	*bitmap;
	pword	*pw;
	int	delayed = 0;
	int	result;

	/*
	** XXX - should we try to do something better with integers which
	** can't be represented exactly as doubles?
	*/

	Constant_To_Bounds(vnum, tnum, cl, cu, ci);

	IC_Var_Info(attr2vec, lwb, upb, ic_type, bitmap);

	if (cu < lwb || cl > upb) {
	    /* Value completely outside domain. */
	    Fail
	}

	if (Type_Is_Int(ic_type)) {
	    if (!ci) {
		/* Don't allow binding an integer var to a non-integer value. */
		Fail
	    }

	    if (!IsAtom(bitmap->tag)) {
		/*
		** Since there's a bitmap, we can assume anything falling
		** within lwb..upb (which num does if we got here) can
		** safely be cast to a word.  This also means we can assume
		** num is a TINT (any integer representable in a word is
		** TINT rather than TBIG).  Using vnum.nint means we don't
		** have to worry about any fuzziness in cl/cu if we're on a
		** 64-bit architecture and num is beyond the range exactly
		** representable as a double.
		*/
		result = bitmap_contains(bitmap->val.wptr, vnum.nint);
		Return_If_Not_Success(result)
	    }
	}

	/* Check if num implies a type change for attr2. */
	if (ci && !Type_Is_Int(ic_type)) {
	    result = ec_schedule_susps(attr2vec + OFF_WAKE_TYPE);
	    Return_If_Not_Success(result)
	}

	/* Check if num implies bound changes for attr2. */
	if (cl > lwb) {
	    /* Bound change. */
	    result = ec_schedule_susps(attr2vec + OFF_WAKE_LO);
	    Return_If_Not_Success(result)
	} else {
	    /* No bound change --- postpone the suspensions. */
	    result = p_schedule_postponed(attr2vec[OFF_WAKE_LO].val,
		    attr2vec[OFF_WAKE_LO].tag);
	    Return_If_Not_Success(result)
	    if (cl < lwb) {
		/* Overlap --- set up delayed goal: Lwb =< Num. */
		pword	goal, dummy_susp;
		pw = TG;
		Push_Struct_Frame(d_.infq);
		pw[1].val = attr2vec[OFF_LO].val;
		pw[1].tag = attr2vec[OFF_LO].tag;
		pw[2].val = vnum;
		pw[2].tag = tnum;
		Make_Struct(&goal, pw);
		result = ec_make_suspension(goal, 0, proc_infq, &dummy_susp);
		if (result == DEBUG_SUSP_EVENT) {
		    delayed = 1;
		    result = PSUCCEED;
		}
		Return_If_Not_Success(result)
	    }
	}
	if (cu < upb) {
	    /* Bound change. */
	    result = ec_schedule_susps(attr2vec + OFF_WAKE_HI);
	    Return_If_Not_Success(result)
	} else {
	    /* No bound change --- postpone the suspensions. */
	    result = p_schedule_postponed(attr2vec[OFF_WAKE_HI].val,
		    attr2vec[OFF_WAKE_HI].tag);
	    Return_If_Not_Success(result)
	    if (cu > upb) {
		/* Overlap --- set up delayed goal: Upb >= Num. */
		pword	goal, dummy_susp;
		pw = TG;
		Push_Struct_Frame(d_.supq);
		pw[1].val = attr2vec[OFF_HI].val;
		pw[1].tag = attr2vec[OFF_HI].tag;
		pw[2].val = vnum;
		pw[2].tag = tnum;
		Make_Struct(&goal, pw);
		result = ec_make_suspension(goal, 0, proc_supq, &dummy_susp);
		if (result == DEBUG_SUSP_EVENT) {
		    delayed = 1;
		    result = PSUCCEED;
		}
		Return_If_Not_Success(result)
	    }
	}

	return delayed ? DEBUG_SUSP_EVENT : PSUCCEED;
}

/*
Var1	Var2	Actions
real	real	Take max of lower bounds, min of upper bounds, check non-empty.
		Impose bounds on var1.
		Wake either var slack bounds.
real	int	Round bounds of var1.
		Take max of lower bounds, min of upper bounds, check non-empty.
		Impose bounds on var1.
		Wake var1 type change and either var slack bounds.
real	bitmap	Round bounds of var1.
		Impose var1 bounds on var2 bitmap, check non-empty.
		Extract (only if slack?) bounds from new bitmap.
		Impose bounds on var1 and give it new bitmap.
		Wake var1 type change and hole and either var slack bounds.
int	real	Round bounds of var2.
		Take max of lower bounds, min of upper bounds, check non-empty.
		Impose bounds on var1.
		Wake var2 type change and either var slack bounds.
int	int	Take max of lower bounds, min of upper bounds, check non-empty.
		Impose bounds on var1.
		Wake either var slack bounds.
int	bitmap	Impose var1 bounds on var2 bitmap, check non-empty.
		Extract (only if slack?) bounds from new bitmap.
		Impose bounds on var1 and give it new bitmap.
		Wake var1 hole and either var slack bounds.
bitmap	real	Round bounds of var2.
		Impose var2 bounds on var1 bitmap, check non-empty.
		Extract (only if slack?) bounds from new bitmap.
		Impose bounds on var1 and give it new bitmap (if required).
		Wake var2 type change and hole and either var slack bounds.
bitmap	int	Impose var2 bounds on var1 bitmap, check non-empty.
		Extract (only if slack?) bounds from new bitmap.
		Impose bounds on var1 and give it new bitmap (if required).
		Wake var2 hole and either var slack bounds.
bitmap	bitmap	Compare bitmaps to check for subsumption of one by the other.
		...
		Intersect var2 bitmap into var1 bitmap, check non-empty.
		Extract (only if slack?) bounds from new bitmap.
		Impose bounds on var1 and give it new bitmap (if required).
*/


    /*
    ** Intersect the domain represented by attr2 into that of attr1, merging
    ** and waking any appropriate suspension lists.
    */
int
unify_ic_ic(pword *attr1vec, pword *attr2vec, value vvar, type tvar, value vsusp_attr, type tsusp_attr)
{
	double	lwb, upb;
	word	min, max;
	double	lwb1, upb1;
	double	lwb2, upb2;
	dident	ic_type, ic_type1, ic_type2;
	pword	*bitmap1, *bitmap2;
	uword	*bitmap_ptr;
	value	val, a1val, a2val;
	type	tag, atag;
	int	integral;
	int	res;
	int	result;
	int	constrained1, constrained2;

	IC_Var_Info(attr1vec, lwb1, upb1, ic_type1, bitmap1);
	IC_Var_Info(attr2vec, lwb2, upb2, ic_type2, bitmap2);

	constrained1 = 0;
	constrained2 = 0;

#if 1
	lwb = lwb1 > lwb2 ? lwb1 : lwb2;
	upb = upb1 < upb2 ? upb1 : upb2;

	if (upb < lwb) {
	    Fail
	}

	if (Type_Is_Int(ic_type1)) {
	    integral = 1;
	    if (!Type_Is_Int(ic_type2)) {
		/* ic_type2 is real, unified type is integer. */
		lwb = ceil(lwb);	/* In case it was lwb2. */
		upb = floor(upb);	/* In case it was upb2. */
		if (upb < lwb) {
		    Fail
		}
		result = ec_schedule_susps(attr2vec + OFF_WAKE_TYPE);
		Return_If_Not_Success(result)
		constrained2 = 1;
	    }
	} else {
	    if (Type_Is_Int(ic_type2)) {
		integral = 1;
		/* ic_type1 is real, unified type is integer. */
		lwb = ceil(lwb);	/* In case it was upb1. */
		upb = floor(upb);	/* In case it was upb1. */
		if (upb < lwb) {
		    Fail
		}
		result = ec_schedule_susps(attr1vec + OFF_WAKE_TYPE);
		Return_If_Not_Success(result)
		constrained1 = 1;
		/* Update the type field. */
		val.did = d_.integer0;
		tag.kernel = TDICT;
		ec_assign(attr1vec + OFF_TYPE, val, tag);
	    } else {
		integral = 0;
	    }
	}
#else
	Unfortunately this version modifies lwb/upb[12], which makes it
	harder to check which suspension lists to wake.

	if (Type_Is_Int(ic_type1) || Type_Is_Int(ic_type2)) {
	    ic_type = d_.integer0;
	} else {
	    ic_type = ic_type1;
	}

	if (ic_type1 != ic_type) {
	    /* ic_type1 is real, ic_type is integer. */
	    lwb1 = ceil(lwb1);
	    upb1 = floor(upb1);
	    result = ec_schedule_susps(attr1vec + OFF_WAKE_TYPE);
	    Return_If_Not_Success(result)
	} else if (ic_type2 != ic_type) {
	    /* ic_type2 is real, ic_type is integer. */
	    lwb2 = ceil(lwb2);
	    upb2 = floor(upb2);
	    result = ec_schedule_susps(attr2vec + OFF_WAKE_TYPE);
	    Return_If_Not_Success(result)
	}

	lwb = max(lwb1, lwb2);
	upb = min(upb1, upb2);

	if (upb < lwb) {
	    Fail
	}
#endif

	if (!IsAtom(bitmap1->tag)) {
	    if (!IsAtom(bitmap2->tag)) {
		/*
		** Both vars have bitmaps; compare them before intersecting
		** them, in case the intersection is trivial.
		*/
		result = compare_bitmaps(bitmap1->val.wptr,
			bitmap2->val.wptr, (word*)&res);
		/*
		** XXX - No convenient/cheap way to know whether internal
		** elements would be removed from bitmap1 or bitmap2 (or
		** even find out whether they were, after an intersection is
		** actually done).
		*/
		if (result == PSUCCEED) {
		    if (res < 0) {
			/* Bitmap1 is contained in bitmap2. */
			/* Wake things suspended on new holes in second var. */
			result = ec_schedule_susps(attr2vec + OFF_WAKE_HOLE);
			Return_If_Not_Success(result)
			constrained2 = 1;
		    } else if (res > 0) {
			/* Bitmap1 contains bitmap2. */
			/* Update attr1 to point at bitmap2. */
			ec_assign(attr1vec + OFF_BITMAP, bitmap2->val,
				bitmap2->tag);
			/* Wake things suspended on new holes in first var. */
			result = ec_schedule_susps(attr1vec + OFF_WAKE_HOLE);
			Return_If_Not_Success(result)
			constrained1 = 1;
		    } else {
			/* Bitmaps are equal. */
		    }
		    /* Note that the bitmap's bounds don't need updating. */
		} else {
		    /* Neither bitmap contains the other. */
		    /* Intersect the bitmaps. */
		    res = bitmap_intersect_into(bitmap1->val.wptr,
			    bitmap2->val.wptr, 0, &bitmap_ptr);
		    if (Result_Is_Empty(res)) {
			Fail
		    }
		    if (Result_Is_Slack(res)) {
			/* Intersection resulted in bound change(s). */
			bitmap_range(bitmap_ptr, &min, &max);
			lwb = (double) min;
			upb = (double) max;
		    }
		    /* Update the pointer in attr1 if the bitmap has moved. */
		    if (bitmap_ptr != bitmap1->val.wptr) {
			val.wptr = bitmap_ptr;
			tag.kernel = TBITMAP;
			ec_assign(attr1vec + OFF_BITMAP, val, tag);
		    }
		    /* Wake things suspended on new holes. */
		    result = ec_schedule_susps(attr1vec + OFF_WAKE_HOLE);
		    Return_If_Not_Success(result)
		    result = ec_schedule_susps(attr2vec + OFF_WAKE_HOLE);
		    Return_If_Not_Success(result)
		    constrained1 = 1;
		    constrained2 = 1;
		}
	    } else {
		/*
		** First var has a bitmap, but second does not: trim
		** bitmap1 to match the new bounds and update the pointer in
		** attr1 if required.
		*/
		bitmap_ptr = bitmap1->val.wptr;
		min = (word) lwb;
		res = set_bitmap_lwb(bitmap_ptr, min, &bitmap_ptr);
		if (Result_Is_Empty(res)) {
		    Fail
		}
		max = (word) upb;
		res |= set_bitmap_upb(bitmap_ptr, max, &bitmap_ptr);
		if (Result_Is_Empty(res)) {
		    Fail
		}
		if (Result_Is_Slack(res)) {
		    /* Bitmap resulted in trimming one of the bounds. */
		    bitmap_range(bitmap_ptr, &min, &max);
		    lwb = (double) min;
		    upb = (double) max;
		}
		/* Update the pointer in attr1 if the bitmap has moved. */
		if (bitmap_ptr != bitmap1->val.wptr) {
		    val.wptr = bitmap_ptr;
		    tag.kernel = TBITMAP;
		    ec_assign(attr1vec + OFF_BITMAP, val, tag);
		}
		/* Wake things suspended on new holes in the second var. */
		result = ec_schedule_susps(attr2vec + OFF_WAKE_HOLE);
		Return_If_Not_Success(result)
		constrained2 = 1;
	    }
	} else if (!IsAtom(bitmap2->tag)) {
	    /*
	    ** Second var has a bitmap, but first does not: trim bitmap2 to
	    ** match the new bounds and update attr1 to point to it.
	    */
	    bitmap_ptr = bitmap2->val.wptr;
	    min = (word) lwb;
	    res = set_bitmap_lwb(bitmap_ptr, min, &bitmap_ptr);
	    if (Result_Is_Empty(res)) {
		Fail
	    }
	    max = (word) upb;
	    res |= set_bitmap_upb(bitmap_ptr, max, &bitmap_ptr);
	    if (Result_Is_Empty(res)) {
		Fail
	    }
	    if (Result_Is_Slack(res)) {
		/* Bitmap resulted in trimming one of the bounds. */
		bitmap_range(bitmap_ptr, &min, &max);
		lwb = (double) min;
		upb = (double) max;
	    }
	    /* Add bitmap to attr1. */
	    val.wptr = bitmap_ptr;
	    tag.kernel = TBITMAP;
	    ec_assign(attr1vec + OFF_BITMAP, val, tag);
	    /* Wake things suspended on new holes in the first var. */
	    result = ec_schedule_susps(attr1vec + OFF_WAKE_HOLE);
	    Return_If_Not_Success(result)
	    constrained1 = 1;
	}

	if (lwb == upb) {
	    pword   num;

	    if (integral) {
		/* Bind var to integer/bignum. */
		result = ec_double_to_int_or_bignum(lwb, &num);
		Return_If_Not_Success(result);
	    } else {
		/* Bind var to bounded real. */
		Make_Interval(&num, lwb, lwb);
	    }
	    result = Unify_Pw(vvar, tvar, num.val, num.tag);
	    Return_If_Not_Success(result);
	}

	/* Update bounds if needed, and wake things suspended on bounds. */
	if (lwb > lwb1) {
	    /* XXX - Should we avoid creating a new double if lwb == lwb2? */
	    Make_Checked_Double_Val(val, lwb);
	    tag.kernel = TDBL;
	    ec_assign(attr1vec + OFF_LO, val, tag);
	    result = ec_schedule_susps(attr1vec + OFF_WAKE_LO);
	    Return_If_Not_Success(result)
	    constrained1 = 1;
	}
	if (upb < upb1) {
	    /* XXX - Should we avoid creating a new double if upb == upb2? */
	    Make_Checked_Double_Val(val, upb);
	    tag.kernel = TDBL;
	    ec_assign(attr1vec + OFF_HI, val, tag);
	    result = ec_schedule_susps(attr1vec + OFF_WAKE_HI);
	    Return_If_Not_Success(result)
	    constrained1 = 1;
	}
	if (lwb > lwb2) {
	    result = ec_schedule_susps(attr2vec + OFF_WAKE_LO);
	    Return_If_Not_Success(result)
	    constrained2 = 1;
	}
	if (upb < upb2) {
	    result = ec_schedule_susps(attr2vec + OFF_WAKE_HI);
	    Return_If_Not_Success(result)
	    constrained2 = 1;
	}

	if (constrained1) {
	    notify_constrained(vvar.ptr);
	}
	if (constrained2 && IsStructure(tsusp_attr)) {
	    ec_schedule_susps(vsusp_attr.ptr + CONSTRAINED_OFF);
	}

	/* Merge the suspension lists. */
	tag.kernel = TINT;
	val.nint = OFF_WAKE_LO;
	atag.kernel = TCOMP;
	a1val.ptr = attr1vec;
	a2val.ptr = attr2vec;
	result = p_merge_suspension_lists(
		val, tag, a2val, atag,
		val, tag, a1val, atag);
	Return_If_Not_Success(result)
	val.nint = OFF_WAKE_HI;
	result = p_merge_suspension_lists(
		val, tag, a2val, atag,
		val, tag, a1val, atag);
	Return_If_Not_Success(result)
	val.nint = OFF_WAKE_HOLE;
	result = p_merge_suspension_lists(
		val, tag, a2val, atag,
		val, tag, a1val, atag);
	Return_If_Not_Success(result)
	val.nint = OFF_WAKE_TYPE;
	result = p_merge_suspension_lists(
		val, tag, a2val, atag,
		val, tag, a1val, atag);
	Return_If_Not_Success(result)

	Succeed
}


    /*
    ** unify_ic(?Term, ?Attr, ?SuspAttr)
    **	Unification handler for IC variables.
    */
int
p_unify_ic(value vterm, type tterm, value vattr, type tattr, value vsusp_attr, type tsusp_attr)
{
	pword	*attr1;
	pword	*attr1vec, *attr2vec;

	if (IsRef(tattr)) {
	    /*
	    ** Second variable has no IC attribute - only thing to do is
	    ** wake the second variable's constrained list (if it has one)
	    ** if the first term is an IC variable (other cases covered
	    ** elsewhere, right?).
	    */
	    if (IsStructure(tsusp_attr) && IsRef(tterm)) {
		IC_Var_Get_Attr(vterm.ptr, attr1);
		if (!IsRef(attr1->tag)) {
		    ec_schedule_susps(vsusp_attr.ptr + CONSTRAINED_OFF);
		}
	    }
	    Succeed
	}
	if (!IsStructure(tattr)) {
	    /* IC attribute of second var is not a structure! */
	    Fail
	}
	attr2vec = vattr.ptr;
	if (attr2vec->val.did != d_ic_attr_functor) {
	    /* IC attribute of second var is not correct structure! */
	    Fail
	}

	/* We now have a validated IC attribute for the second variable. */

	if (IsNumber(tterm)) {
	    /* First var is a ground number. */
	    return unify_number_ic(vterm, tterm, attr2vec);
	} else if (IsRef(tterm)) {
	    /* First var is a variable (and we're promised it's meta). */
	    IC_Var_Get_Attr(vterm.ptr, attr1);
	    if (IsRef(attr1->tag)) {
		/*
		** First var has no IC attribute, so just give it the
		** one from the second variable.
		*/
		Bind_Var(attr1->val, attr1->tag, attr2vec, TCOMP);
		notify_constrained(vterm.ptr);
	    } else {
		if (!IsStructure(attr1->tag)) {
		    /* IC attribute of first var is not a structure! */
		    Fail
		}
		attr1vec = attr1->val.ptr;
		if (attr1vec->val.did != d_ic_attr_functor) {
		    /* IC attribute of first var is not correct structure! */
		    Fail
		}

		/* Have to merge attributes from both vars. */
		return unify_ic_ic(attr1vec, attr2vec, vterm, tterm,
			vsusp_attr, tsusp_attr);
	    }
	} else {
	    /* First var is neither a number or a variable. */
	    Fail
	}

	Succeed
}


/*----------------------------------------------------------------------
**
** indomain/1
**
*/

    /*
    ** Give range error unless bounds are in the floating point range where
    ** integers are exactly representable _and_ the bounds can be
    ** represented as normal integers (rather than bignums).
    ** (Actually, for simplicity we restrict it to the valid range for
    ** bitmaps.)
    */
int
p_indomain_init(value vx, type tx, value vlo, type tlo)
{
	pword	*attr;
	pword	*tmp;
	word	lo;

	/* If it's already a ground integer, nothing to do. */
	if (IsInteger(tx)) {
	    /* Return_Unify_Integer(vlo, tlo, vx.nint) */
	    Succeed
	}

	if (!IsMeta(tx)) {
	    /* Not a variable or no attributes. */
	    Bip_Error(TYPE_ERROR)
	}
	IC_Var_Get_Attr(vx.ptr, attr);
	if (IsRef(attr->tag)) {
	    /* No IC attribute. */
	    Bip_Error(TYPE_ERROR)
	}
	/* Get the attribute's vector. */
	attr = attr->val.ptr;
	Dereference(attr);

	tmp = attr + OFF_TYPE;
	Dereference(tmp);
	if (!Type_Is_Int(tmp->val.did)) {
	    /* Not an integer IC variable. */
	    Bip_Error(TYPE_ERROR)
	}

	/*
	** Check for whether the lower bound is too small (or large) to make
	** integer labelling sensible.  We don't worry about the upper bound
	** being too large, so that indomain/1 can be called for variables
	** with (for example) an infinite upper bound.
	*/
	tmp = attr + OFF_LO;
	Dereference(tmp);
	if (Dbl(tmp->val) < MIN_BITMAP_RANGE
		|| Dbl(tmp->val) > MAX_BITMAP_RANGE) {
	    Bip_Error(RANGE_ERROR)
	}
	lo = (word) Dbl(tmp->val);

	Return_Unify_Integer(vlo, tlo, lo)
}


    /* Assumes X has already been vetted by p_indomain_check (but may */
    /* have become ground since). */
    /* Try gives the next suggested integer to try. */
int
p_indomain_try(value vx, type tx, value vtry, type ttry)
{
	var_info	vi;
	word	try;
	word	xl;
	word	result;

	if (!IsRef(tx)) {
	    /* If it's ground already, nothing to do. */
	    Cut_External
	    Succeed
	}

	get_var_info(vx.ptr, &vi);

	try = vtry.nint;

	if (try > MAX_BITMAP_RANGE) {
	    Bip_Error(RANGE_ERROR)
	}

	if (!IsAtom(vi.bitmap->tag)) {
	    /* If we have a bitmap, use that to choose the next value to try. */
	    result = next_greater_member(vi.bitmap->val.wptr, try - 1, &try);
	    if (Result_Is_Empty(result)) {
		/* No remaining entry in bitmap. */
		Cut_External
		Fail
	    }
	} else {
	    /* Make sure we don't try anything smaller than the lower bound. */
	    xl = (word) vi.tb.b.l;
	    if (xl > try) {
		try = xl;
	    }
	}

	/* Don't leave choice point if this is the last value. */
	/* Careful: upper bound may be too large to safely cast to a word. */
	if (vi.tb.b.u <= MAX_BITMAP_RANGE && try == (word) vi.tb.b.u) {
	    Cut_External
	} else {
	    value v;
	    v.nint = try+1;
	    Remember(2, v, ttry)
	}

	Return_Unify_Integer(vx, tx, try)
}


