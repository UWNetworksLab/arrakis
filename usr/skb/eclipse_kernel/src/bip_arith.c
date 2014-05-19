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
 * VERSION	$Id: bip_arith.c,v 1.2.2.1 2009/01/03 07:46:08 jschimpf Exp $
 */

/*
 * Overview of the arithmetic system
 * ----------------------------------
 * 
 * The externals corresponding to the arithmetic built-in predicates are
 * either here or (for the more critical ones) in the emulator. E.g.
 * BIAdd in the emulator is the implementation of +/3, and p_sin() here
 * is the implementation of sin/1.
 * 
 * Although the external functions sometimes handle TINT and TDBL arguments 
 * directly, the general mechanism is to go through tag-indexed function
 * tables that handle type coercion, i.e.
 * 
 *     tag_desc[<type>].coerce_to[<othertype>]
 *		functions in this table are called _<type>_<othertype>
 * 
 * or dispatch to the routine that implements a particular arithmetic
 * operation for a particular type:
 * 
 *     tag_desc[<type>].arith_op[<operation>]
 *		functions in this table are called _<type>_<operation>
 *
 * Other table functions that must be implemented for each numeric type are
 *
 *	tag_desc[<type>].arith_compare
 *		functions in this table are called _arith_compare_<type>
 *	tag_desc[<type>].compare
 *		functions in this table are called _compare_<type>
 *	tag_desc[<type>].equal
 *		functions in this table are called _equal_<type>
 * 
 * All table functions for TINT and TDBL are here, the functions for TBIG
 * and TRAT are in bigrat.c, and the ones for breals (TIVL) in intervals.c.
 * In principle, it should be possible to build eclipse without bigrat.c
 * and intervals.c (the corresponding functions then remain initialised
 * to dummies).
 *
 * Sufficiently regular arithmetic externals share code by calling
 * unary_arith_op() or binary_arith_op(). These do the following:
 *
 * 1. check for instantiation fault/delay
 * 2. make argument types equal, and/or lift argument type(s) if necessary
 * 3. invoke the actual operation via arith_op table on that type
 * 4. unify the result
 * 
 */

/*
 * To make delaying and error reporting consistent in all combinations
 * of inline-expanded, interpreted and coroutining arithmetic, the
 * policy is as follows. Note that we differ from the usual rule
 * of checking for type errors first.
 *
 * 1. evaluate sub-expressions
 *	This is needed for compatibility with inline expansion
 * 2. check delay conditions
 *	In non-coroutine mode we simply report an instantiation
 *	fault instead of delaying. Thus the same code can be used
 *	for delaying and non-delaying builtins.
 * 3. check for type errors
 * 4. compute the result
 */

#include	<math.h>

#include 	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "error.h"
#include        "mem.h"
#include	"dict.h"
#include        "emu_export.h"
#include	"rounding_control.h"	/* for init_rounding_modes() */

/* range checks for functions */

#define OneMOne(x)	 ( (x) >= -1.0 && (x) <= 1.0 )
#define Positive(x)	 ( (x) > 0.0 )
#define NonNegative(x)	 ( (x) >= 0.0 )

#define NonIntNum(t) (IsDouble(t) || IsRational(t) || IsInterval(t))

#define BITS_PER_WORD (8*SIZEOF_LONG)

static int
	_reverse_times(long int x, long int y, value zval, type ztag);

#if defined(i386) && defined(__GNUC__)
#define Pow (*pow_ptr_to_avoid_buggy_inlining)
static double (*pow_ptr_to_avoid_buggy_inlining)(double,double) = pow;
#else
#define Pow pow
#endif


/*------------------------------------------------------------------------
 * The multi-directional builtins succ/2, plus/3 and times/3
 * Some don't work with bignums yet! Maybe write them in Prolog?
 *-----------------------------------------------------------------------*/

static int
p_succ(value x, type tx, value y, type ty)
{
    pword result;

    if (!(IsRef(tx) || IsNumber(tx))) { Bip_Error(ARITH_TYPE_ERROR) }
    if (NonIntNum(tx)) { Bip_Error(TYPE_ERROR) }
    if (!(IsRef(ty) || IsNumber(ty))) { Bip_Error(ARITH_TYPE_ERROR) }
    if (NonIntNum(ty)) { Bip_Error(TYPE_ERROR) }

    result.tag.kernel = TINT;
    if (IsInteger(tx))
    {
	result.val.nint = x.nint + 1;
	if (result.val.nint <= 0) {
	    if (result.val.nint == MIN_S_WORD) {
		Bip_Error(INTEGER_OVERFLOW);
	    }
	    Fail_;
	}
	Return_Numeric(y, ty, result);
    }
    else if (IsRef(tx))
    {
	if (IsInteger(ty)) {
	    if (y.nint <= 0) {
		Fail_
	    }
	    result.val.nint = y.nint - 1;
	    Return_Numeric(x, tx, result);
	} else if (IsRef(ty)) {
	    return PDELAY_1_2;
	}
	return unary_arith_op(y, ty, x, tx, ARITH_PREV, TINT);
    }
    else
	return unary_arith_op(x, tx, y, ty, ARITH_NEXT, TINT);
}


static int
p_plus(value x, type tx, value y, type ty, value z, type tz)
{
    if (IsRef(tx))
    {
	if (IsRef(ty))
	{
	    return PDELAY_1_2;
	}
	else if (IsRef(tz))
	{
	    return PDELAY_1_3;
	}
	else if (IsInteger(ty) && IsInteger(tz))
	{
	    Kill_DE;
	    Return_Unify_Integer(x, tx, z.nint - y.nint);
	}
    }
    else if (IsRef(ty))
    {
	if (IsRef(tz))
	{
	    return PDELAY_2_3;
	}
	else if (IsInteger(tx) && IsInteger(tz))
	{
	    Kill_DE;
	    Return_Unify_Integer(y, ty, z.nint - x.nint);
	}
    }
    else if (IsInteger(tx) && IsInteger(ty) && (IsRef(tz) || IsInteger(tz)))
    {
	Kill_DE;
	Return_Unify_Integer(z, tz, x.nint + y.nint);
    }

    if (NonIntNum(tx) || NonIntNum(ty) || NonIntNum(tz))
	{Bip_Error(TYPE_ERROR);}
    else if (!IsNumber(tx) || !IsNumber(ty) || !IsNumber(tz))
	{Bip_Error(ARITH_TYPE_ERROR);}
    else if (IsBignum(tx) || !IsBignum(ty) || !IsBignum(tz))
	{Bip_Error(RANGE_ERROR);}
    else
	{Bip_Error(TYPE_ERROR);}
}


static int
p_times(value x, type tx, value y, type ty, value z, type tz)
{
    if ((IsRef(tx) || IsInteger(tx)) &&
	(IsRef(ty) || IsInteger(ty)) &&
	(IsRef(tz) || IsInteger(tz)))
    {
	if (IsRef(tx))
	{
	    if (IsRef(ty))
	    {
		if (x.ptr == y.ptr && IsInteger(tz) && z.nint == 0) {
		    Kill_DE;
		    Return_Unify_Integer(x, tx, 0)
		} else {
		    return PDELAY_1_2;
		}
	    }
	    else if (y.nint == 0)
	    {
		Kill_DE;
		Return_Unify_Integer(z, tz, 0);
	    }
	    else if (y.nint == 1)
	    {
		Kill_DE;
		Return_Unify_Pw(z, tz, x, tx)
	    }
	    else if (IsRef(tz))
	    {
		return PDELAY_1_3;
	    }
	    else
		return _reverse_times(z.nint, y.nint, x, tx);
	}
	else if (x.nint == 0)
	{
	    Kill_DE;
	    Return_Unify_Integer(z, tz, 0);
	}
	else if (x.nint == 1)
	{
	    Kill_DE;
	    Return_Unify_Pw(z, tz, y, ty)
	}
	else if (IsRef(ty))
	{
	    if (IsRef(tz))
	    {
		if (y.ptr == z.ptr) {
		    if (x.nint != 1) {
			Kill_DE;
			Return_Unify_Integer(z, tz, 0)
		    }
		}
		return PDELAY_2_3;
	    }
	    else
		return _reverse_times(z.nint, x.nint, y, ty);
	}
	else
	{
	    Kill_DE;
	    Return_Unify_Integer(z, tz, x.nint * y.nint);
	}
    }
    if (NonIntNum(tx) || NonIntNum(ty) || NonIntNum(tz))
	{Bip_Error(TYPE_ERROR);}
    else if (!IsNumber(tx) || !IsNumber(ty) || !IsNumber(tz))
	{Bip_Error(ARITH_TYPE_ERROR);}
    else if (IsBignum(tx) || !IsBignum(ty) || !IsBignum(tz))
	{Bip_Error(RANGE_ERROR);}
    else
	{Bip_Error(TYPE_ERROR);}
}


/* 
 * _reverse_times is an auxiliary function for times
 * used when times(INT,VAR,INT) or times(VAR,INT,INT) are called.
 * receives two integers x,y 
 * returns_unifies z= x/y if this is an integer otherwise fails
 * for times(X, 0, 0) we delay since this may be true (X=0) or false
 */

static int
_reverse_times(long int x, long int y, value zval, type ztag)
{
    if (y == 0) 
        if (x == 0) 
	{
	    Push_var_delay(zval.ptr, ztag.all);
	    return PDELAY;
	} 
	else
	{
	    Fail_
	}
    else if (x % y != 0) 
    {
	Fail_;
    }
    else
    {
	Kill_DE;
	Return_Unify_Integer(zval,ztag, x/y);
    } 
}
  

/*------------------------------------------------------------------------
 * Other arithmetic-related built-ins
 *------------------------------------------------------------------------ */

/*
 * between(Min, Max, Step, Index)
 */
static int
p_between(value vmi, type tmi, value vma, type tma, value vs, type ts, value vi, type ti)
{
    value	v;

    Check_Integer(tmi)
    Check_Integer(tma)
    Check_Integer(ts)
    if (vs.nint == 0) {
	Bip_Error(RANGE_ERROR)
    }
    if (IsInteger(ti)) {
	Cut_External
	if (vs.nint > 0) {
	    Succeed_If(
		vi.nint >= vmi.nint
		&& vi.nint <= vma.nint
		&& (vi.nint - vmi.nint) % vs.nint == 0
	    )
	} else {
	    Succeed_If(
		vi.nint <= vmi.nint
		&& vi.nint >= vma.nint
		&& (vmi.nint - vi.nint) % -vs.nint == 0
	    )
	}
    } else {
	Check_Output_Integer(ti)
    }
    if (vs.nint > 0) {
	if (vmi.nint >= vma.nint) {
	    Cut_External
	    if (vmi.nint > vma.nint) {
		Fail_
	    }
	}
    } else {
	if (vmi.nint <= vma.nint) {
	    Cut_External
	    if (vmi.nint < vma.nint) {
		Fail_
	    }
	}
    }
    v.nint = vmi.nint + vs.nint;
    Remember(1, v, tmi)
    Return_Unify_Integer(vi, ti, vmi.nint)
}


/*
 * is_zero/1 and collect/3
 *	support externals for lib(r) - largely obsolete
 */

static int
p_is_zero(value v, type t)
{
    pword result;
    Succeed_If(tag_desc[TagType(t)].arith_op[ARITH_SGN](v, &result) == PSUCCEED
	    && result.val.nint == 0L);
}


#define OFF_C	0
#define OFF_V	1

static int
p_collect(value vin, type tin, value vout, type tout, value vzero, type tzero)
{
    register pword *curr_var, *curr_tail, *zero_tail, *new_tail;
    register pword *pcoeff, *pvar, *pw;
    pword	in_list, out_list, zero_list, new_coeff;
    int		err;
    Prepare_Requests;

    Check_List(tin);
    Check_Output_List(tout);
    Check_Output_List(tzero);

    in_list.val = vin;
    in_list.tag = tin;
    curr_tail = &in_list;
    new_tail = &out_list;
    zero_tail = &zero_list;

    new_coeff.tag.kernel = TINT;
    new_coeff.val.nint = 0L;
    curr_var = 0;			/* 0 stands for constant sequence */

    while (IsList(curr_tail->tag))
    {
	pw = curr_tail->val.ptr;
	curr_tail = &pw[1];
	Dereference_(curr_tail);
	Dereference_(pw);
	pw = pw->val.ptr;
	pcoeff = &pw[OFF_C];
	Dereference_(pcoeff);
	pvar = &pw[OFF_V];
	Dereference_(pvar);
	if (IsRef(pvar->tag))			/* mono(..., Var) */
	{
	    if (pvar == curr_var)			/* inside a sequence */
	    {
		err = bin_arith_op(pcoeff->val, pcoeff->tag,
			new_coeff.val, new_coeff.tag, &new_coeff, ARITH_ADD);
		if (err != PSUCCEED) goto _error_;
	    }
	    else				/* end of a sequence */
	    {
		if (p_is_zero(new_coeff.val, new_coeff.tag) == PSUCCEED)
		{
		    if (curr_var)
		    {
			pw = TG;
			TG += 4;
			Check_Gc;
			Make_List(zero_tail, pw);
			Make_List(&pw[0], &pw[2]);
			zero_tail = &pw[1];
			pw = pw + 2;
			pw[OFF_C].val.nint = 0;
			pw[OFF_C].tag.kernel = TINT;
			pw[OFF_V].val.ptr = curr_var;
			pw[OFF_V].tag.kernel = TREF;
		    }
		}
		else
		{
		    pw = TG;
		    TG += 4;
		    Check_Gc;
		    Make_List(new_tail, pw);
		    Make_List(&pw[0], &pw[2]);
		    new_tail = &pw[1];
		    pw = pw + 2;
		    pw[OFF_C] = new_coeff;
		    if (curr_var)
		    {
			pw[OFF_V].val.ptr = curr_var;
			pw[OFF_V].tag.kernel = TREF;
		    }
		    else		/* build new constant mono */
		    {
			Make_Integer(&pw[OFF_V], 1);
		    }
		}
		curr_var = pvar;
		new_coeff = *pcoeff;
	    }
	}
	else					/* in the constant part */
	{
	    pword product;
	    err = bin_arith_op(pcoeff->val, pcoeff->tag,
			pvar->val, pvar->tag, &product, ARITH_MUL);
	    if (err != PSUCCEED) goto _error_;
	    err = bin_arith_op(product.val, product.tag,
			new_coeff.val, new_coeff.tag, &new_coeff, ARITH_ADD);
	    if (err != PSUCCEED) goto _error_;
	}
    }

    /* end of last sequence */
    if (p_is_zero(new_coeff.val, new_coeff.tag) == PSUCCEED)
    {
	if (curr_var)
	{
	    pw = TG;
	    TG += 4;
	    Check_Gc;
	    Make_List(zero_tail, pw);
	    Make_List(&pw[0], &pw[2]);
	    zero_tail = &pw[1];
	    pw = pw + 2;
	    pw[OFF_C].val.nint = 0;
	    pw[OFF_C].tag.kernel = TINT;
	    pw[OFF_V].val.ptr = curr_var;
	    pw[OFF_V].tag.kernel = TREF;
	}
    }
    else
    {
	pw = TG;
	TG += 4;
	Check_Gc;
	Make_List(new_tail, pw);
	Make_List(&pw[0], &pw[2]);
	new_tail = &pw[1];
	pw = pw + 2;
	pw[OFF_C] = new_coeff;
	if (curr_var)
	{
	    pw[OFF_V].val.ptr = curr_var;
	    pw[OFF_V].tag.kernel = TREF;
	}
	else		/* build new constant mono */
	{
	    Make_Integer(&pw[OFF_V], 1);
	}
    }

    if (IsNil(curr_tail->tag))
    {
	Make_Nil(new_tail);
	Make_Nil(zero_tail);
	Request_Unify_Pw(vout, tout, out_list.val, out_list.tag);
	Request_Unify_Pw(vzero, tzero, zero_list.val, zero_list.tag);
	Return_Unify
    }

    err = TYPE_ERROR;
_error_:
    /* may have to pop incomplete junk on the stack */
    Bip_Error(err);
}


/*------------------------------------------------------------------------
 * Standard arithmetic bips
 * More frequently used ones are in the emulator
 *-----------------------------------------------------------------------*/

int
p_sgn(value v1, type t1, value v, type t)
{
    int err;
    pword result;
    if (IsRef(t1)) { Bip_Error(PDELAY_1) }
    result.tag.kernel = TINT;
    err = tag_desc[TagType(t1)].arith_op[ARITH_SGN](v1, &result);
    if (err != PSUCCEED) return err;
    Return_Numeric(v, t, result)
}

static int
p_min(value v1, type t1, value v2, type t2, value v, type t)
{
    return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_MIN);
}

static int
p_max(value v1, type t1, value v2, type t2, value v, type t)
{
    return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_MAX);
}

static int
p_gcd(value v1, type t1, value v2, type t2, value v, type t)
{
    return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_GCD);
}

static int
p_gcd_ext(value v1, type t1, value v2, type t2, value s, type ts, value t, type tt, value g, type tg)
{
    pword res1,res2,res3;
    int err;
    Prepare_Requests;
    if (IsRef(t1)) { Bip_Error(PDELAY_1) }
    if (IsRef(t2)) { Bip_Error(PDELAY_2) }
    Check_Integer_Or_Bignum(t1)
    Check_Integer_Or_Bignum(t2)
    Check_Output_Integer_Or_Bignum(ts)
    Check_Output_Integer_Or_Bignum(tt)
    Check_Output_Integer_Or_Bignum(tg)
    /* we don't have a TINT implementation, always compute via bignums */
    err = tag_desc[TagType(t1)].coerce_to[TBIG](v1, &v1);
    if (err != PSUCCEED) return(err);
    err = tag_desc[TagType(t2)].coerce_to[TBIG](v2, &v2);
    if (err != PSUCCEED) return(err);
    err = tag_desc[TBIG].arith_op[ARITH_GCD_EXT](v1, v2, &res1, &res2, &res3);
    if (err != PSUCCEED) return err;
    Kill_DE;	/* in case it's a demon */
    Request_Unify_Pw(s, ts, res1.val, res1.tag);
    Request_Unify_Pw(t, tt, res2.val, res2.tag);
    Request_Unify_Pw(g, tg, res3.val, res3.tag);
    Return_Unify
}

static int
p_lcm(value v1, type t1, value v2, type t2, value v, type t)
{
    return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_LCM);
}

static int
p_uplus(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_PLUS, TINT);
}

static int
p_abs(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_ABS, TINT);
}

static int
p_sin(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_SIN, TDBL);
}

static int
p_cos(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_COS, TDBL);
}

static int
p_tan(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_TAN, TDBL);
}

static int
p_asin(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_ASIN, TDBL);
}

static int
p_acos(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_ACOS, TDBL);
}

static int
p_atan(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_ATAN, TDBL);
}

static int
p_atan2(value v1, type t1, value v2, type t2, value v, type t)
{
    return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_ATAN2);
}

static int
p_exp(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_EXP, TDBL);
}

static int
p_ln(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_LN, TDBL);
}

static int
p_sqrt(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_SQRT, TDBL);
}

static int
p_round(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_ROUND, TINT);
}

static int
p_floor(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_FLOOR, TINT);
}

static int
p_ceiling(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_CEIL, TINT);
}

static int
p_truncate(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_TRUNCATE, TINT);
}

static int
p_fix(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_FIX, TINT);
}

static int
p_integer2(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_INT, TINT);
}

static int
p_numerator(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_NUM, TRAT);
}

static int
p_denominator(value v1, type t1, value v, type t)
{
    return unary_arith_op(v1, t1, v, t, ARITH_DEN, TRAT);
}

static int
p_pi(value v, type t)
{
    pword result;
    Make_Float(&result, 3.1415926535897932)
    Return_Numeric(v, t, result)
}

static int
p_e(value v, type t)
{
    pword result;
    Make_Float(&result, 2.7182818284590455)
    Return_Numeric(v, t, result)
}

/*------------------------------------------------------------------------
 * Irregular arithmetic Bips
 * They have special rules concerning their argument and result types
 *-----------------------------------------------------------------------*/

static int
p_rational2(value v1, type t1, value v, type t)
{
    int err;
    pword result;
    if (IsRef(t1)) { Bip_Error(PDELAY_1) }
    result.tag.kernel = TRAT;
    err = tag_desc[TagType(t1)].coerce_to[TRAT](v1, &result.val);
    if (err != PSUCCEED) return err;
    Return_Numeric(v, t, result)
}

static int
p_rationalize(value v1, type t1, value v, type t)
{
    int err;
    pword result;
    if (IsRef(t1)) { Bip_Error(PDELAY_1) }
    result.tag.kernel = TRAT;
    err = tag_desc[TagType(t1)].arith_op[ARITH_NICERAT](v1, &result);
    if (err != PSUCCEED) return err;
    Return_Numeric(v, t, result)
}

static int
p_bignum2(value v1, type t1, value v, type t)
{
    int err;
    pword result;
    if (IsRef(t1)) { Bip_Error(PDELAY_1) }
    result.tag.kernel = TBIG;
    err = tag_desc[TagType(t1)].coerce_to[TBIG](v1, &result.val);
    /* We fail here instead of making a type error. Thus bignum/2 can be
     * used more easily to test whether bignums are available or not */
    if (err != PSUCCEED) { Fail_; }
    Return_Numeric(v, t, result)
}

static int
p_breal2(value v1, type t1, value v, type t)
{
    int err;
    pword result;
    if (IsRef(t1)) { Bip_Error(PDELAY_1) }
    result.tag.kernel = TIVL;
    err = tag_desc[TagType(t1)].coerce_to[TIVL](v1, &result.val);
    if (err != PSUCCEED) return err;
    Return_Numeric(v, t, result)
}

static int
p_float2(value v1, type t1, value v, type t)
{
    pword result;
    int err;
    if (IsRef(t1)) { Bip_Error(PDELAY_1) }
    result.tag.kernel = TDBL;
    err = tag_desc[TagType(t1)].coerce_to[TagType(result.tag)](v1, &result.val);
    if (err != PSUCCEED) return(err);
    Return_Numeric(v, t, result)
}

/* auxiliary function for power operation */

static int
_int_pow(word x,
	word y,		/* y >= 0 */
	word *r)
{
    word result = 1;
    int	neg = 0;
    word rs;

    if (x < 0)
    {
	if (y & 1)
	    neg = 1;
	x = -x;
    }
    if (x > 0xffff || (y > BITS_PER_WORD && x > 1))
	return INTEGER_OVERFLOW;
    while (y-- > 0)
    {
	if ((rs = ((result >> 16) & 0xffff) * x) > 0xffff ||
	    (result = (rs << 16) + (result & 0xffff) * x) < 0)
	    return INTEGER_OVERFLOW;
    }
    if (neg)
	result = -result;
    *r = result;
    return 0;
}

static int
p_power(value v1, type t1, value v2, type t2, value v, type t)
{
    pword result;
    int err;

    if(IsInteger(t2))
    {
	if (IsInteger(t1))
	{
	    if (v2.nint < 0)
	    {
		if (GlobalFlags & PREFER_RATIONALS)
		{
		    /* this will force bignum ^ int -> rational */
		    Bip_Error(INTEGER_OVERFLOW)
		}
		else
		{
		    Make_Checked_Float(&result,
				Pow((double)(v1.nint),(double)v2.nint));
		}
	    }
	    else if(v1.nint == 0 && v2.nint == 0)
		{ Bip_Error(ARITH_EXCEPTION) }
	    else
	    {
		result.tag.kernel = TINT;
		err = _int_pow(v1.nint, v2.nint, &result.val.nint);
		if (err) { Bip_Error(err); }
	    }
	}
	else if (IsBignum(t1))
	{
	    result.tag.kernel = TBIG;
	    err = tag_desc[TBIG].arith_op[ARITH_POW](v1, v2, &result);
	    if (err) { Bip_Error(err); }
	}
	else if (IsRational(t1))
	{
	    result.tag.kernel = TRAT;
	    err = tag_desc[TRAT].arith_op[ARITH_POW](v1, v2, &result);
	    if (err) { Bip_Error(err); }
	}
	else 
	{
	    return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_POW);
	}
    }
    else if(IsBignum(t2))
    {
	Bip_Error(RANGE_ERROR)
    }
    else if (IsRational(t2) && !IsDouble(t1))
    {
	Bip_Error(TYPE_ERROR)
    }
    else /* default also handles delay */
    {
	return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_POW);
    }
    Kill_DE;
    Return_Numeric(v, t, result)
}


static int
p_powm(value v1, type t1, value v2, type t2, value v3, type t3, value v, type t)
{
    pword result;
    int err;
    if (IsRef(t1)) { Bip_Error(PDELAY_1) }
    if (IsRef(t2)) { Bip_Error(PDELAY_2) }
    if (IsRef(t3)) { Bip_Error(PDELAY_3) }
    err = tag_desc[TagType(t1)].coerce_to[TBIG](v1, &v1);
    if (err != PSUCCEED) return(err);
    err = tag_desc[TagType(t2)].coerce_to[TBIG](v2, &v2);
    if (err != PSUCCEED) return(err);
    err = tag_desc[TagType(t3)].coerce_to[TBIG](v3, &v3);
    if (err != PSUCCEED) return(err);
    err = tag_desc[TBIG].arith_op[ARITH_POWM](v1, v2, v3, &result);
    if (err != PSUCCEED) return(err);
    Return_Unify_Pw(v, t, result.val, result.tag);
}


/*
 * Shifts counts that exceed the word length are undefined in C.
 * If we shift left, it's an overflow, otherwise undefined (change this).
 * In the other cases, overflow is checked by shifting back and checking
 * if we obtain the original value.
 */

static int
p_lshift(value v1, type t1, value v2, type t2, value v, type t)
{
    pword result;
    int err;

    if (IsInteger(t2))
    {
	if (IsInteger(t1))
	{
	    if (v2.nint > 0)		/* shift left */
	    {
		if (v1.nint && v2.nint >= BITS_PER_WORD)
		    { Bip_Error(INTEGER_OVERFLOW); }
		result.val.nint = v1.nint << v2.nint;
		if (result.val.nint >> v2.nint != v1.nint)
		    { Bip_Error(INTEGER_OVERFLOW); }
	    }
	    else			/* shift right */
	    {
		if (-v2.nint >= BITS_PER_WORD)
		    result.val.nint = v1.nint >> BITS_PER_WORD-1;
		else
		    result.val.nint = v1.nint >> -v2.nint;
	    }
	    result.tag.kernel = TINT;
	}
	else if (IsBignum(t1))
	{
	    result.tag.kernel = TBIG;
	    err = tag_desc[TBIG].arith_op[ARITH_SHL](v1, v2, &result);
	    if (err != PSUCCEED) { Bip_Error(err); }
	}
	else	/* error */
	    return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_SHL);
    }
    else if (IsBignum(t2))
	{ Bip_Error(RANGE_ERROR); }
    else
	return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_SHL);

    Kill_DE;
    Return_Numeric(v, t, result)
}

static int
p_rshift(value v1, type t1, value v2, type t2, value v, type t)
{
    pword result;
    int err;

    if (IsInteger(t2))
    {
	if (IsInteger(t1))
	{
	    if (v2.nint >= 0)	/* shift right */
	    {
		if (v2.nint >= BITS_PER_WORD)
		    result.val.nint = v1.nint >> BITS_PER_WORD-1;
		else
		    result.val.nint = v1.nint >> v2.nint;
	    }
	    else		/* shift left */
	    {
		if (v1.nint && -v2.nint >= BITS_PER_WORD)
		    { Bip_Error(INTEGER_OVERFLOW); }
		result.val.nint = v1.nint << -v2.nint;
		if (result.val.nint >> -v2.nint != v1.nint)
		    { Bip_Error(INTEGER_OVERFLOW); }
	    }
	    result.tag.kernel = TINT;
	}
	else if (IsBignum(t1))
	{
	    result.tag.kernel = TBIG;
	    err = tag_desc[TBIG].arith_op[ARITH_SHR](v1, v2, &result);
	    if (err != PSUCCEED) { Bip_Error(err); }
	}
	else
	    return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_SHR);
    }
    else if (IsBignum(t2))
	{ Bip_Error(RANGE_ERROR); }
    else
	return binary_arith_op(v1, t1, v2, t2, v, t, ARITH_SHR);

    Kill_DE;
    Return_Numeric(v, t, result)
}

static int
p_minint(value v, type t)
{
    pword result;
    Make_Integer(&result, MIN_S_WORD);
    Return_Numeric(v, t, result)
}

static int
p_maxint(value v, type t)
{
    pword result;
    Make_Integer(&result, MAX_S_WORD);
    Return_Numeric(v, t, result)
}


static int
p_setbit(value vi, type ti, value vn, type tn, value v, type t)	/* argument order because of overflow handler */
{
    int err;
    pword result;

    Check_Integer(tn);
    if (vn.nint < 0)
    {
	Bip_Error(RANGE_ERROR);
    }
    if (IsInteger(ti))
    {
    	if (vn.nint < BITS_PER_WORD-1)
	{
	    Make_Integer(&result, vi.nint | ((word)1 << vn.nint));
	}
	else if (vi.nint < 0)
	{
	    Make_Integer(&result, vi.nint);
	}
	else
	{
	    Bip_Error(INTEGER_OVERFLOW);
	}
    }
    else if (IsBignum(ti) || IsString(ti))
    {
	result.tag.kernel = Tag(ti.kernel);
	err = tag_desc[TagType(result.tag)].arith_op[ARITH_SETBIT](vi, vn, &result);
	if (err != PSUCCEED) return err;
    }
    else
	return binary_arith_op(vi, ti, vn, tn, v, t, ARITH_SETBIT);

    Kill_DE;
    Return_Numeric(v, t, result)
}

static int
p_clrbit(value vi, type ti, value vn, type tn, value v, type t)
{
    int err;
    pword result;

    Check_Integer(tn);
    if (vn.nint < 0)
    {
	Bip_Error(RANGE_ERROR);
    }
    if (IsInteger(ti))
    {
    	if (vn.nint < BITS_PER_WORD-1)
	{
	    Make_Integer(&result, vi.nint & ~((word)1 << vn.nint));
	}
	else if (vi.nint >= 0)
	{
	    Make_Integer(&result, vi.nint);
	}
	else
	{
	    Bip_Error(INTEGER_OVERFLOW);
	}
    }
    else if (IsBignum(ti) || IsString(ti))
    {
	result.tag.kernel = Tag(ti.kernel);
	err = tag_desc[TagType(result.tag)].arith_op[ARITH_CLRBIT](vi, vn, &result);
	if (err != PSUCCEED) return err;
    }
    else
	return binary_arith_op(vi, ti, vn, tn, v, t, ARITH_CLRBIT);

    Kill_DE;
    Return_Numeric(v, t, result)
}

static int
p_getbit(value vi, type ti, value vn, type tn, value v, type t)
{
    int err;
    pword result;

    Check_Integer(tn);
    if (vn.nint < 0)
    {
	Bip_Error(RANGE_ERROR);
    }
    result.tag.kernel = TINT;
    if (IsInteger(ti))
    {
	result.val.nint = vn.nint < BITS_PER_WORD ?
		((uword) vi.nint >> vn.nint) & 1 :
		vi.nint < 0 ? 1 : 0;
    }
    else if (IsBignum(ti) || IsString(ti))
    {
	err = tag_desc[TagType(ti)].arith_op[ARITH_GETBIT](vi, vn, &result);
	if (err != PSUCCEED) return err;
    }
    else
	return binary_arith_op(vi, ti, vn, tn, v, t, ARITH_GETBIT);

    Kill_DE;
    Return_Numeric(v, t, result)
}

#if 0
static int
_strg_setbit(value v1, value v2, pword *pres)	/* string x int -> string */
{
    int words_old = 2*(BufferPwords(v1.ptr)-1);
    int words_offset = v2.nint / BITS_PER_WORD;
    int words_new = (words_old > words_offset+1) ? words_old : words_offset+1;
    word *from, *to;
    int i;

    pres->val.ptr = TG;
    Push_Buffer(words_new * SIZEOF_LONG);
    to = (word *) BufferStart(pres->val.ptr);
    from = (word *) BufferStart(v1.ptr);
    for (i = 0; i < words_old; i++)
    	to[i] = from[i];
    for (; i < words_offset; i++)
    	to[i] = 0L;
    to[words_offset] |= 1 << (v2.nint % BITS_PER_WORD);
    Succeed_;
}
#endif


/*
 * integer_list(+Integer, +ChunkSizeInBits, -ListOfChunks)
 *
 * Takes a (big) integer and splits it up into a list of small integers of
 * ChunkSizeInBits each. The first list element contains the least
 * significant bits.  E.g.
 *	?- X is 8'1234567, sepia_kernel:integer_list(X,3,L).
 *	X = 342391
 *	L = [7, 6, 5, 4, 3, 2, 1]
 * ChunkSizeInBits must not exceed the wordsize.
 */

p_integer_list(value vi, type ti, value vsz, type tsz, value v, type t)
{
    int err;
    pword result;

    Check_Integer_Or_Bignum(ti)
    Check_Integer(tsz)
    err = tag_desc[TagType(ti)].coerce_to[TBIG](vi, &vi);
    if (err != PSUCCEED) return(err);
    err = ec_big_to_chunks(vi.ptr, vsz.nint, &result);
    Return_If_Error(err);
    Return_Unify_Pw(v, t, result.val, result.tag);
}


/*------------------------------------------------------------------------
 * Generic auxiliary functions
 *-----------------------------------------------------------------------*/

int
un_arith_op(
    	value v1, type t1,	/* input */
	pword *result,		/* output */
	int op,			/* operation */
	int top)		/* the 'minimal' type for the result */
{
    int err;

    if (tag_desc[TagType(t1)].numeric < tag_desc[top].numeric)
    {
	if (!IsNumber(t1))
	    { Bip_Error(ARITH_TYPE_ERROR); }
	err = tag_desc[TagType(t1)].coerce_to[top](v1, &v1);
	if (err != PSUCCEED) return err;
	result->tag.kernel = top;
    }
    else
	/* CAUTION: must strip extra tag bits, e.g. PERSISTENT */
	result->tag.kernel = Tag(t1.kernel);
    return tag_desc[TagType(result->tag)].arith_op[op](v1, result);
}

int
unary_arith_op(
    	value v1, type t1,	/* input */
	value v, type t,	/* output */
	int op,			/* operation */
	int top)		/* the 'minimal' type for the result */
{
    pword result;
    int err;
    if (IsRef(t1)) { Bip_Error(PDELAY_1) }
    err = un_arith_op(v1, t1, &result, op, top);
    if (err != PSUCCEED) return err;
    Return_Numeric(v, t, result)
}

int
bin_arith_op(value v1, type t1, value v2, type t2, pword *pres, int op)
{
    int err;

    if (!SameType(t1,t2))
    {
	if (tag_desc[TagType(t1)].numeric > tag_desc[TagType(t2)].numeric)
	{
	    if (!IsNumber(t2))
		{ Bip_Error(ARITH_TYPE_ERROR); }
	    err = tag_desc[TagType(t2)].coerce_to[TagType(t1)](v2, &v2);
	    pres->tag.kernel = Tag(t1.kernel);
	}
	else
	{
	    if (!IsNumber(t1))
		{ Bip_Error(ARITH_TYPE_ERROR); }
	    err = tag_desc[TagType(t1)].coerce_to[TagType(t2)](v1, &v1);
	    pres->tag.kernel = Tag(t2.kernel);
	}
	if (err != PSUCCEED) return err;
    }
    else
	/* CAUTION: must strip extra tag bits, e.g. PERSISTENT */
	pres->tag.kernel = Tag(t1.kernel);

    return tag_desc[TagType(pres->tag)].arith_op[op](v1, v2, pres);
}

int
binary_arith_op(value v1, type t1, value v2, type t2, value v, type t, int op)
{
    pword result;
    int err;
    if (IsRef(t1)) { Bip_Error(PDELAY_1) }
    if (IsRef(t2)) { Bip_Error(PDELAY_2) }
    err = bin_arith_op(v1, t1, v2, t2, &result, op);
    if (err != PSUCCEED) return err;
    Kill_DE;	/* in case it's a demon */
    Return_Numeric(v, t, result)
}

int
arith_compare(value v1, type t1, value v2, type t2, int *res)
{
    int err;
    pword *old_tg = TG; /* coercion may create temporaries */
    if (!SameType(t1,t2))
    {
	if (tag_desc[TagType(t1)].numeric > tag_desc[TagType(t2)].numeric)
	{
	    if (!IsNumber(t2))
		{ Bip_Error(ARITH_TYPE_ERROR); }
	    err = tag_desc[TagType(t2)].coerce_to[TagType(t1)](v2, &v2);
	    t2.kernel = Tag(t1.kernel);
	}
	else
	{
	    if (!IsNumber(t1))
		{ Bip_Error(ARITH_TYPE_ERROR); }
	    err = tag_desc[TagType(t1)].coerce_to[TagType(t2)](v1, &v1);
	    t1.kernel = Tag(t2.kernel);
	}
	if (err != PSUCCEED) return err;
    }
    err = tag_desc[TagType(t1)].arith_compare(v1, v2, res);
    TG = old_tg;      /* coercion may have created temporaries */
    return err;
}

static int
_nop(value v1, pword *pres)
{
    pres->val.all = v1.all;
    Succeed_;
}

static int
_noc(value in, value *out)
{
    *out = in;
    Succeed_;
}

/*------------------------------------------------------------------------
 * Integer operations (most of them are expanded)
 *-----------------------------------------------------------------------*/

static int
_compare_int(value v1, value v2)
{
    return (v1.nint > v2.nint ? 1 : v1.nint < v2.nint ? -1 : 0);
}

static int
_arith_compare_int(value v1, value v2, int *res)
{
    *res = (v1.nint > v2.nint ? 1 : v1.nint < v2.nint ? -1 : 0);
    Succeed_;
}

/* CAUTION: The code for int_add and int_mul is duplicated in the emulator */

static int
_int_add(value v1, value v2, pword *pres)	/* int x int -> int/big */
{
    register long   n1 = v1.nint;
    register long   n2 = v2.nint;

    pres->val.nint = n1 + n2;
    if (((n1 >= 0) == (n2 >= 0)) && (n1 >= 0) != (pres->val.nint >= 0))
    {
	Bip_Error(INTEGER_OVERFLOW)
    }
    Succeed_;
}

static int
_int_sub(value v1, value v2, pword *pres)	/* int x int -> int/big */
{
    register long   n1 = v1.nint;
    register long   n2 = v2.nint;

    pres->val.nint = n1 - n2;
    if (((n1 >= 0) != (n2 >= 0)) && (n1 >= 0) != (pres->val.nint >= 0))
    {
	Bip_Error(INTEGER_OVERFLOW)
    }
    Succeed_;
}

static int
_int_mul(value v1, value v2, pword *pres)	/* int x int -> int/big */
{
    register long   n1 = v1.nint;
    register long   n2 = v2.nint;
    register long   n3;

    if (n1 == 0) {
    	pres->val.nint = 0;
	Succeed_
    }
    if (n2 == MIN_S_WORD) {
	/* Not true if n1 == 1, but who cares... */
    	Bip_Error(INTEGER_OVERFLOW)
    }
    n3 = n1 * n2;
    if (n3 / n1 != n2) {
    	Bip_Error(INTEGER_OVERFLOW)
    }
    pres->val.nint = n3;
    Succeed_;
}

static int
_int_neg(value v1, pword *pres)	/* needed in the parser to evaluate signs */
{
    if ((pres->val.nint = -v1.nint) == MIN_S_WORD)
	{ Bip_Error(INTEGER_OVERFLOW); }
    Succeed_;
}

static int
_int_sgn(value v1, pword *pres)
{
    pres->val.nint = (long) (v1.nint > 0 ? 1 : v1.nint < 0 ? -1: 0);
    Succeed_;
}

static int
_int_min(value v1, value v2, pword *pres)
{
    pres->val.nint = v1.nint > v2.nint ? v2.nint : v1.nint;
    Succeed_;
}

static int
_int_max(value v1, value v2, pword *pres)
{
    pres->val.nint = v1.nint < v2.nint ? v2.nint : v1.nint;
    Succeed_;
}

static int
_int_abs(value v1, pword *pres)
{
    if (v1.nint < 0)
	return _int_neg(v1, pres);
    pres->val.nint = v1.nint;
    Succeed_;
}

static int
_int_gcd(value v1, value v2, pword *pres)
{
    /* No special TINT implementation, we default to the TBIG one */
    int err;
    err = tag_desc[TINT].coerce_to[TBIG](v1, &v1);
    if (err != PSUCCEED) return err;
    err = tag_desc[TINT].coerce_to[TBIG](v2, &v2);
    if (err != PSUCCEED) return err;
    return tag_desc[TBIG].arith_op[ARITH_GCD](v1, v2, pres);
}

static int
_int_lcm(value v1, value v2, pword *pres)
{
    /* No special TINT implementation, we default to the TBIG one */
    int err;
    err = tag_desc[TINT].coerce_to[TBIG](v1, &v1);
    if (err != PSUCCEED) return err;
    err = tag_desc[TINT].coerce_to[TBIG](v2, &v2);
    if (err != PSUCCEED) return err;
    return tag_desc[TBIG].arith_op[ARITH_LCM](v1, v2, pres);
}

static int
_int_atan2(value v1, value v2, pword *pres)
{
    Make_Double(pres, Atan2((double)v1.nint, (double)v2.nint));
    Succeed_;
}


/*----------------------------------------------------------------------------
 * Doubles
 *----------------------------------------------------------------------------*/

static int
_compare_dbl(value v1, value v2)
{
    return Dbl(v1) > Dbl(v2) ? 1
	: Dbl(v1) < Dbl(v2) ? -1
	: Dbl(v1) != 0.0 ? 0
	: PedanticZeroLess(Dbl(v1),Dbl(v2)) ? -1
	: PedanticZeroLess(Dbl(v2),Dbl(v1)) ? 1
	: 0;
}

static int
_arith_compare_dbl(value v1, value v2, int *res)
{
    *res = (Dbl(v1) > Dbl(v2) ? 1
	    : Dbl(v1) < Dbl(v2) ? -1 : 0);
    Succeed_;
}

#ifndef UNBOXED_DOUBLES
static int
_equal_dbl(pword *pw1, pword *pw2)
{
    /* compare the doubles bitwise (as integers) in order to be
     * able to distinguish negative and positive zeros */
#if SIZEOF_DOUBLE == SIZEOF_WORD
    return BufferStart(pw1)->val.all == BufferStart(pw2)->val.all;
#elif SIZEOF_DOUBLE == 2*SIZEOF_WORD
    return BufferStart(pw1)->val.all == BufferStart(pw2)->val.all
    	&& BufferStart(pw1)->tag.all == BufferStart(pw2)->tag.all;
#else
    PROBLEM: Cannot deal with word size SIZEOF_WORD.
#endif
}
#endif


/*
 * arithmetic operations on doubles
 */

static int
_dbl_neg(value v1, pword *pres)	/* needed in the parser to evaluate signs */
{
    Make_Double_Val(pres->val, -Dbl(v1))
    Succeed_;
}

static int
_dbl_sgn(value v1, pword *pres)
{
    pres->val.nint = (long)
	(Dbl(v1) == 0.0 ? 0 : Dbl(v1) > 0.0 ? 1: -1);
    Succeed_;
}

static int
_dbl_min(value v1, value v2, pword *pres)
{
    pres->val.all = Dbl(v1) > Dbl(v2) ? v2.all : v1.all;
    Succeed_;
}

static int
_dbl_max(value v1, value v2, pword *pres)
{
    pres->val.all = Dbl(v1) > Dbl(v2) ? v1.all : v2.all;
    Succeed_;
}

static int
_dbl_add(value v1, value v2, pword *pres)
{
    Make_Checked_Double_Val(pres->val, Dbl(v1) + Dbl(v2))
    Succeed_;
}

static int
_dbl_sub(value v1, value v2, pword *pres)
{
    Make_Checked_Double_Val(pres->val, Dbl(v1) - Dbl(v2))
    Succeed_;
}

static int
_dbl_mul(value v1, value v2, pword *pres)
{
    Make_Checked_Double_Val(pres->val, Dbl(v1) * Dbl(v2))
    Succeed_;
}

static int
_dbl_div(value v1, value v2, pword *pres)
{
    Make_Checked_Double_Val(pres->val, Dbl(v1) / Dbl(v2))
    Succeed_;
}

static int
_dbl_abs(value v1, pword *pres)
{
    if (Dbl(v1) < 0.0)
    {
	Make_Double_Val(pres->val, -Dbl(v1))
    }
    else if (Dbl(v1) == 0.0)	/* for negative zero */
    {
	Make_Double_Val(pres->val, 0.0)
    }
    else
	pres->val.all = v1.all;
    Succeed_;
}

static int
_dbl_sin(value v1, pword *pres)
{
    Make_Checked_Double_Val(pres->val, sin(Dbl(v1)))
    Succeed_;
}

static int
_dbl_cos(value v1, pword *pres)
{
    Make_Checked_Double_Val(pres->val, cos(Dbl(v1)))
    Succeed_;
}

static int
_dbl_tan(value v1, pword *pres)
{
    Make_Checked_Double_Val(pres->val, tan(Dbl(v1)))
    Succeed_;
}

static int
_dbl_asin(value v1, pword *pres)
{
    double y = Dbl(v1);
    if (!OneMOne(y))
	{ Bip_Error(ARITH_EXCEPTION) }
    Make_Checked_Double_Val(pres->val, asin(y))
    Succeed_;
}

static int
_dbl_acos(value v1, pword *pres)
{
    double y = Dbl(v1);
    if (!OneMOne(y))
	{ Bip_Error(ARITH_EXCEPTION) }
    Make_Checked_Double_Val(pres->val, acos(y))
    Succeed_;
}

static int
_dbl_atan(value v1, pword *pres)
{
    Make_Checked_Double_Val(pres->val, atan(Dbl(v1)))
    Succeed_;
}

static int
_dbl_atan2(value v1, value v2, pword *pres)
{
    Make_Checked_Double_Val(pres->val, Atan2(Dbl(v1), Dbl(v2)))
    Succeed_;
}

static int
_dbl_exp(value v1, pword *pres)
{
    double d = Dbl(v1);
    /* Catch the special cases of raising 'e' to +Inf and -Inf as some
       platforms give incorrect results w.r.t. IEEE754 specs */
    if ( d == -HUGE_VAL ) {
	d = 0.0;
    } else if ( d == HUGE_VAL ) {
	/* Do nothing as e^(+Inf) = +Inf */
    } else {
	d = exp(d);
    }
    Make_Double_Val(pres->val, d)
    Succeed_;
}

static int
_dbl_ln(value v1, pword *pres)
{
    double y = Dbl(v1);
    if (!NonNegative(y))
	{ Bip_Error(ARITH_EXCEPTION); }
    Make_Double_Val(pres->val, log(y))
    Succeed_;
}

static int
_dbl_sqrt(value v1, pword *pres)
{
    double y = Dbl(v1);
    if (!NonNegative(y))
	{ Bip_Error(ARITH_EXCEPTION); }
    Make_Checked_Double_Val(pres->val, sqrt(y))
    Succeed_;
}

static int
_dbl_pow(value v1, value v2, pword *pres)
{
    Make_Checked_Double_Val(pres->val, Pow(Dbl(v1), Dbl(v2)))
    Succeed_;
}

static int
_dbl_round(value v1, pword *pres)
{
    double x;
#if defined(HAVE_RINT) && !defined(HP_RINT)
    x = rint(Dbl(v1));
#else
    /*
     * Round to even number we are exactly in the middle.
     * Make sure we round to -0.0 if between -0.5 and -0.0
     */
    x = Ceil(Dbl(v1));
    if (x - Dbl(v1) > 0.5 ||
        (x - Dbl(v1) == 0.5 && ((long)x & 1)))
	    x -= 1.0;
#endif /* rint */
    Make_Checked_Double_Val(pres->val, x)
    Succeed_;
}

static int
_dbl_floor(value v1, pword *pres)
{
    Make_Checked_Double_Val(pres->val, floor(Dbl(v1)))
    Succeed_;
}

static int
_dbl_ceil(value v1, pword *pres)
{
    Make_Checked_Double_Val(pres->val, Ceil(Dbl(v1)))
    Succeed_;
}

static int
_dbl_truncate(value v1, pword *pres)
{
#ifdef HAVE_TRUNC
    Make_Checked_Double_Val(pres->val, trunc(Dbl(v1)))
#else
    double f = Dbl(v1);
    Make_Checked_Double_Val(pres->val, f < 0 ? Ceil(f) : floor(f));
#endif
    Succeed_;
}


/* This _dbl_fix() is a simplified version of the one in bigrat.c */

static int
_dbl_fix(value v1, pword *pres)
{
    double f = Dbl(v1);
    if (MIN_S_WORD_DBL <= f && f < MAX_S_WORD_1_DBL)	/* fits in word? */
    {
	pres->val.nint = (word) f;
	pres->tag.kernel = TINT;
    }
    else if (finite(f))
    {
	Bip_Error(INTEGER_OVERFLOW);
    }
    else
    {
	Bip_Error(ARITH_EXCEPTION);
    }
    Succeed_;
}

static int
_dbl_int2(value v1, pword *pres)
{
    double f = Dbl(v1);
    if (MIN_S_WORD_DBL <= f && f < MAX_S_WORD_1_DBL)	/* fits in word? */
    {
	double ipart;
	if (modf(f, &ipart) == 0.0)
	{
	    pres->val.nint = (word) ipart;
	    pres->tag.kernel = TINT;
	}
	else
	{
	    Bip_Error(ARITH_EXCEPTION);
	}
    }
    else if (finite(f))
    {
	Bip_Error(INTEGER_OVERFLOW);
    }
    else
    {
	Bip_Error(ARITH_EXCEPTION);
    }
    Succeed_;
}

/*--------------------------------------------------------------------------
 * type coercion functions
 *--------------------------------------------------------------------------*/

/*ARGSUSED*/
static int
_arith_type_err(value in, value *out)	/* CAUTION: we allow out == &in */
{
    return ARITH_TYPE_ERROR;
}

/*ARGSUSED*/
static int
_type_err(value in, value *out)		/* CAUTION: we allow out == &in */
{
    return TYPE_ERROR;
}

static int
_unimp_err(value in, value *out)        /* CAUTION: we allow out == &in */
{
    return UNIMPLEMENTED;
}

static int
_int_dbl(value in, value *out)        	/* CAUTION: we allow out == &in */
{
    Make_Double_Val(*out, (double) in.nint)
    Succeed_;
}


/*--------------------------------------------------------------------------
 * Initialize the arithmetic system (tables and bips)
 *--------------------------------------------------------------------------*/

void
bip_arith_init(int flags)
{
    int i, j;
    extern void bigrat_init(void);

    /* Initialise rounding mode information for interval arithmetic. */
    init_rounding_modes();

    for(i=0; i <= NTYPES; i++)
	tag_desc[i].numeric = 0;

    tag_desc[TINT].numeric = 1;		/* mark and order the numeric types */
    tag_desc[TBIG].numeric = 2;
    tag_desc[TRAT].numeric = 3;
    tag_desc[TDBL].numeric = 4;
    tag_desc[TIVL].numeric = 5;

    for(i=0; i <= NTYPES; i++)
    {
	for(j=0; j <= NTYPES; j++)
	    tag_desc[i].coerce_to[j] =
		tag_desc[i].numeric ? _type_err : _arith_type_err;
	for(j=0; j <= ARITH_OPERATIONS; j++)
	    tag_desc[i].arith_op[j] =
		tag_desc[i].numeric ? _type_err : _arith_type_err;
	if (tag_desc[i].numeric)
	{
	    tag_desc[i].coerce_to[TBIG] = _unimp_err;
	    tag_desc[i].coerce_to[TRAT] = _unimp_err;
	}
	tag_desc[i].coerce_to[i] = _noc;
    }

    tag_desc[TINT].compare = _compare_int;
    tag_desc[TINT].arith_compare = _arith_compare_int;
    tag_desc[TINT].coerce_to[TDBL] = _int_dbl;
    tag_desc[TINT].arith_op[ARITH_PLUS] = _nop;
    tag_desc[TINT].arith_op[ARITH_CHGSIGN] =
    tag_desc[TINT].arith_op[ARITH_NEG] = _int_neg;
    tag_desc[TINT].arith_op[ARITH_ADD] = _int_add;
    tag_desc[TINT].arith_op[ARITH_SUB] = _int_sub;
    tag_desc[TINT].arith_op[ARITH_MUL] = _int_mul;
    tag_desc[TINT].arith_op[ARITH_MIN] = _int_min;
    tag_desc[TINT].arith_op[ARITH_MAX] = _int_max;
    tag_desc[TINT].arith_op[ARITH_GCD] = _int_gcd;
    tag_desc[TINT].arith_op[ARITH_LCM] = _int_lcm;
    tag_desc[TINT].arith_op[ARITH_ABS] = _int_abs;
    tag_desc[TINT].arith_op[ARITH_ROUND] = _nop;
    tag_desc[TINT].arith_op[ARITH_FLOOR] = _nop;
    tag_desc[TINT].arith_op[ARITH_CEIL] = _nop;
    tag_desc[TINT].arith_op[ARITH_TRUNCATE] = _nop;
    tag_desc[TINT].arith_op[ARITH_FIX] = _nop;
    tag_desc[TINT].arith_op[ARITH_INT] = _nop;
    tag_desc[TINT].arith_op[ARITH_SGN] = _int_sgn;
    tag_desc[TINT].arith_op[ARITH_ATAN2] = _int_atan2;

    tag_desc[TBIG].arith_op[ARITH_PLUS] = _nop;
    tag_desc[TBIG].arith_op[ARITH_FLOAT] = _nop;
    tag_desc[TBIG].arith_op[ARITH_ROUND] = _nop;
    tag_desc[TBIG].arith_op[ARITH_FLOOR] = _nop;
    tag_desc[TBIG].arith_op[ARITH_CEIL] = _nop;
    tag_desc[TBIG].arith_op[ARITH_TRUNCATE] = _nop;
    tag_desc[TBIG].arith_op[ARITH_FIX] = _nop;
    tag_desc[TBIG].arith_op[ARITH_INT] = _nop;

    tag_desc[TRAT].arith_op[ARITH_PLUS] = _nop;
    tag_desc[TRAT].arith_op[ARITH_FLOAT] = _nop;
    tag_desc[TRAT].arith_op[ARITH_NICERAT] = _nop;

    tag_desc[TDBL].compare = _compare_dbl;
    tag_desc[TDBL].arith_compare = _arith_compare_dbl;
#ifndef UNBOXED_DOUBLES
    tag_desc[TDBL].equal = _equal_dbl;
#endif
    tag_desc[TDBL].arith_op[ARITH_PLUS] = _nop;
    tag_desc[TDBL].arith_op[ARITH_CHGSIGN] =
    tag_desc[TDBL].arith_op[ARITH_NEG] = _dbl_neg;
    tag_desc[TDBL].arith_op[ARITH_ADD] = _dbl_add;
    tag_desc[TDBL].arith_op[ARITH_SUB] = _dbl_sub;
    tag_desc[TDBL].arith_op[ARITH_MUL] = _dbl_mul;
    tag_desc[TDBL].arith_op[ARITH_DIV] = _dbl_div;
    tag_desc[TDBL].arith_op[ARITH_MIN] = _dbl_min;
    tag_desc[TDBL].arith_op[ARITH_MAX] = _dbl_max;
    tag_desc[TDBL].arith_op[ARITH_ABS] = _dbl_abs;
    tag_desc[TDBL].arith_op[ARITH_SIN] = _dbl_sin;
    tag_desc[TDBL].arith_op[ARITH_COS] = _dbl_cos;
    tag_desc[TDBL].arith_op[ARITH_TAN] = _dbl_tan;
    tag_desc[TDBL].arith_op[ARITH_ASIN] = _dbl_asin;
    tag_desc[TDBL].arith_op[ARITH_ACOS] = _dbl_acos;
    tag_desc[TDBL].arith_op[ARITH_ATAN] = _dbl_atan;
    tag_desc[TDBL].arith_op[ARITH_ATAN2] = _dbl_atan2;
    tag_desc[TDBL].arith_op[ARITH_EXP] = _dbl_exp;
    tag_desc[TDBL].arith_op[ARITH_LN] = _dbl_ln;
    tag_desc[TDBL].arith_op[ARITH_SQRT] = _dbl_sqrt;
    tag_desc[TDBL].arith_op[ARITH_POW] = _dbl_pow;
    tag_desc[TDBL].arith_op[ARITH_ROUND] = _dbl_round;
    tag_desc[TDBL].arith_op[ARITH_FLOOR] = _dbl_floor;
    tag_desc[TDBL].arith_op[ARITH_FIX] = _dbl_fix;
    tag_desc[TDBL].arith_op[ARITH_INT] = _dbl_int2;
    tag_desc[TDBL].arith_op[ARITH_CEIL] = _dbl_ceil;
    tag_desc[TDBL].arith_op[ARITH_TRUNCATE] = _dbl_truncate;
    tag_desc[TDBL].arith_op[ARITH_SGN] = _dbl_sgn;

    bigrat_init();		/* implementation of bignums and rationals */
    ec_intervals_init();	/* implementation of float intervals */

    if (!(flags & INIT_SHARED))
	return;

    /* plus/3 and times/3 have NONVAR because the bound argument is not known */
    built_in(in_dict("succ", 2), p_succ, B_UNSAFE|U_SIMPLE)
	-> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
    built_in(in_dict("plus", 3), p_plus, B_UNSAFE|U_SIMPLE|PROC_DEMON)
	-> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR) |
		BoundArg(3, NONVAR);
    built_in(in_dict("times", 3), p_times, B_UNSAFE|U_SIMPLE|PROC_DEMON)
	-> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR) |
		BoundArg(3, NONVAR);

    (void) exported_built_in(in_dict("collect", 3), p_collect,	B_UNSAFE);
    (void) b_built_in(in_dict("between", 4), p_between, d_.kernel_sepia);

    (void) built_in(in_dict("+", 2),	p_uplus, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("abs", 2),	p_abs,	B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("^", 3),	p_power, B_UNSAFE|U_SIMPLE|PROC_DEMON);
    (void) built_in(in_dict("<<", 3),	p_lshift, B_UNSAFE|U_SIMPLE|PROC_DEMON);
    (void) built_in(in_dict(">>", 3),	p_rshift, B_UNSAFE|U_SIMPLE|PROC_DEMON);
    (void) built_in(in_dict("min", 3),	p_min,	B_UNSAFE|U_SIMPLE|PROC_DEMON);
    (void) built_in(in_dict("max", 3),	p_max,	B_UNSAFE|U_SIMPLE|PROC_DEMON);
    (void) built_in(in_dict("gcd", 3),	p_gcd,	B_UNSAFE|U_SIMPLE|PROC_DEMON);
    (void) built_in(in_dict("gcd", 5),	p_gcd_ext,	B_UNSAFE|U_GROUND|PROC_DEMON);
    (void) built_in(in_dict("lcm", 3),	p_lcm,	B_UNSAFE|U_SIMPLE|PROC_DEMON);
    (void) built_in(in_dict("setbit", 3), p_setbit, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("getbit", 3), p_getbit, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("clrbit", 3), p_clrbit, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("sin", 2),	p_sin,	B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("cos", 2),	p_cos,	B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("tan", 2),	p_tan,	B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("asin", 2), p_asin, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("acos", 2), p_acos, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("atan", 2), p_atan, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("atan", 3), p_atan2, B_UNSAFE|U_SIMPLE|PROC_DEMON);
    (void) built_in(in_dict("exp", 2),	p_exp,	B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("ln", 2),	p_ln,	B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("sqrt", 2), p_sqrt, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("round", 2), p_round, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("floor", 2), p_floor, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("ceiling", 2), p_ceiling, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("truncate", 2), p_truncate, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("numerator", 2), p_numerator, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("denominator", 2), p_denominator,B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("sgn", 2), p_sgn,	B_UNSAFE|U_SIMPLE);
    (void) local_built_in(in_dict("pi", 1), p_pi, B_UNSAFE|U_SIMPLE);
    (void) local_built_in(in_dict("e", 1), p_e, B_UNSAFE|U_SIMPLE);

    (void) built_in(in_dict("fix", 2),	p_fix,	B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("integer", 2), p_integer2,	B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("float", 2), p_float2, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("rational", 2), p_rational2, B_UNSAFE|U_SIMPLE);
    (void) built_in(in_dict("rationalize", 2), p_rationalize, B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("minint", 1), p_minint, B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("maxint", 1), p_maxint, B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("bignum", 2), p_bignum2,B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("breal", 2), p_breal2,B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("is_zero", 1), p_is_zero,B_SAFE);
    (void) exported_built_in(in_dict("integer_list", 3), p_integer_list,B_UNSAFE|U_SIMPLE);

    (void) exported_built_in(in_dict("powm", 4), p_powm, B_UNSAFE|U_SIMPLE|PROC_DEMON);
}


/* At least on SUNs, defining matherr() returning non-zero will
 * suppress error messages being printed by math library routines.
 */
#ifdef HAVE_MATHERR
/*ARGSUSED*/
int
matherr(struct exception *ex)
{
    return 1;
}
#endif
