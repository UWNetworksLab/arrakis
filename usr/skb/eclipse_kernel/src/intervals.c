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
 * Copyright (C) 1996-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*----------------------------------------------------------------------
* System:	ECLiPSe Constraint Logic Programming System
* Version:	$Id: intervals.c,v 1.2 2008/07/16 23:58:32 kish_shen Exp $
*

Supported operations:

+ E            unary plus
- E            unary minus
abs(E)         absolute value
floor(E)       round down to integral value
ceiling(E)     round up to integral value
truncate(E)    round towards zero to integral value
round(E)       round to nearest integral value
sgn(E)         sign value (exception if zero-crossing)

E1 + E2        addition
E1 - E2        subtraction
E1 * E2        multiplication
E1 / E2        division
E1 ^ E2        power operation
min(E1,E2)     minimum of 2 values
max(E1,E2)     maximum of 2 values

sin(E)         trigonometric function
cos(E)         trigonometric function
tan(E)         trigonometric function
asin(E)        trigonometric function
acos(E)        trigonometric function
atan(E)        trigonometric function
atan(Y,X)      trigonometric function
exp(E)         exponential function e^x
ln(E)          natural logarithm
sqrt(E)        square root

Conversions:

integer(E)     convert to integer, value must be exact
float(E)       return "middle" value (logsplit)
breal(E)	convert to interval
		rounds out floats
breal_from_bounds(L, U)  create from bounds
breal_min(E)	return lower bound of an interval
breal_max(E)	return upper bound of an interval

breal_bounds(I, L, U)
		returns upper and lower bounds of an interval I

    Extra predicates for intervals:
sqr(E)
+-(E,Res)		not a function, can be done with -/1 and union
rsqr(E,Res)		+- sqrt, not a function
pow(E,Int,Res)		
pow(E,Int,Res)		Z=pow(X,Y), reverse of X=pow(Z,N), N odd
rpow_even(X,Y,Z,Res)	Reverse of Z^N=X, N even.
			Computes Res = intersect( X^Y, Z), Y=1/N

accuracy(E,-Width:double)
interval_union(X,Y,Z)
linsplit(X,MinWidth,Ratio,-Split:double)
logsplit(X,MinWidth,Ratio,-Split:double)
mindeltas(+Xold, +Xnew, -LwdDelta:double, -UpbDelta:double)

*----------------------------------------------------------------------*/
#include <math.h>

#include	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "error.h"
#include        "mem.h"
#include        "dict.h"
#include	"emu_export.h"
#include	"opcode.h"
#include	"intervals.h"
#include	"rounding_control.h"

/*
 * Contents:	Floating-Point Interval Constraint Solver
 *		(C part: primitives for interval arithmetic)
 *
 * Author:	Joachim Schimpf, IC-Parc
 *
 *
 * The following code relies heavily on IEEE-conformant floating-point
 * arithmetic for rounding, infinity handling and undefined values (NaN).
 *
 * General hints about programming with IEEE-floats:
 * Great care has to be taken when intermediate results are undefined,
 * eg. after computations involving infinities (inf-inf etc). The result
 * is then an IEEE-NaN. NaNs propagate through further computations
 * and comparisons involving them do always fail. The latter means that
 * (A>=B) and (!(A<B)) are not semantically equivalent!!!
 * Note that we never return NaNs to the Prolog level (well, at least
 * not intentionally...).
 * Negative Zeros: A test like x < 0.0 does not succeed for -0.0!
 *               : The Test x == 0.0 will succeed for both 0.0 AND -0.0.
 *               : In order to assign a value of -0.0, one must use the
 *                 expression -1*(0.0) as MSVC incorrectly handles -0.0
 *                 in source code.
 *
 * About safe rounding:
 * We use two approaches to ensure safe rounding.  For basic operations
 * (+, -, *, /) where we are sure it will work correctly, we use processor
 * rounding modes to guarantee the safety of the results (see
 * rounding_control.h).  In other cases, where it's less clear how the
 * rounding modes will affect the operations (sin, cos and others), we
 * always round the results one step outwards.
 */


/* Declarations of global variables used by the macros in rounding_control.h */

Declare_Rounding_Control_State
//asq:
;

#define Return_Double(v, t, d) 	{	\
    value dval;				\
    Make_Double_Val(dval, d);		\
    Bind_Var(v, t, dval.all, TDBL);	\
}

#ifdef IEEE_INEXACT
int result_inexact;
#endif

#define min(x,y) ((x) < (y) ? (x) : (y))
#define max(x,y) ((x) > (y) ? (x) : (y))


#if defined(i386) && defined(__GNUC__)
#define Pow (*pow_ptr_to_avoid_buggy_inlining)
static double (*pow_ptr_to_avoid_buggy_inlining)(double,double) = pow;
#else
#define Pow pow
#endif

/* ----------------------------------------------------------------------
 *  Forward declarations
 * ---------------------------------------------------------------------- */
static int
linsplit(double minwidth, double ratio, double lwb, double upb, int upper, double *split);
static int
logsplit(double minwidth, double ratio, double orig_lwb, double orig_upb, int upper, double *split);

static dident d_undecidable;

/* ----------------------------------------------------------------------
 *  Auxiliary functions
 * ---------------------------------------------------------------------- */

typedef union {
	double	as_dbl;
#if (SIZEOF_LONG == 8)
	unsigned long as_int;
#else
	struct ieee_parts {
#ifdef WORDS_BIGENDIAN 
		uint32 mant1;
		uint32 mant0;
#else
		uint32 mant0;
		uint32 mant1;
#endif
	} as_struct;
#endif
} ieee_double;


static int
samesign(double x, double y)
{
    ieee_double dx, dy;
    dx.as_dbl = x;
    dy.as_dbl = y;
#if (SIZEOF_LONG == 8)
    return (dx.as_int & SIGN_BIT) == (dy.as_int & SIGN_BIT);
#else
    return (dx.as_struct.mant1 & SIGN_BIT) == (dy.as_struct.mant1 & SIGN_BIT);
#endif
}

#if (SIZEOF_LONG == 8)

double
ec_ieee_up(double x)
{
    ieee_double res;
    res.as_dbl = x;
    if ((res.as_int & ~SIGN_BIT) == 0)			/* x == +-0 */
    {
	return MINDOUBLE;
    }
    else if ((long) res.as_int >= 0)			/* x > 0 */
    {
	if (res.as_int < 0x7ff0000000000000UL)		/* x < Inf */
	{
	    res.as_int += 1;
	    return res.as_dbl;
	}
    }
    else if (res.as_int <= 0xfff0000000000000UL)	/* -Inf <= x < 0 */
    {
	res.as_int -= 1;
	return res.as_dbl;
    }
    return x;						/* NaN */
}


double
ec_ieee_down(double x)
{
    ieee_double res;
    res.as_dbl = x;
    if ((res.as_int & ~SIGN_BIT) == 0)			/* x == +-0 */
    {
	return -MINDOUBLE;
    }
    else if ((long) res.as_int > 0)			/* x > 0 */
    {
	if (res.as_int <= 0x7ff0000000000000UL)		/* x <= Inf */
	{
	    res.as_int -= 1;
	    return res.as_dbl;
	}
    }
    else if (res.as_int < 0xfff0000000000000UL)		/* -Inf < x < 0 */
    {
	res.as_int += 1;
	return res.as_dbl;
    }
    return x;						/* NaN */
}

#else

#define Inc64(h,l) { \
	if ((l += 1) == 0) \
	    h += 1; \
    }

#define Dec64(h,l) { \
	if (l == 0) { \
	    l -= 1; \
	    h -= 1; \
	} else { \
	    l -= 1; \
	} \
    }

double
ec_ieee_up(double x)
{
    ieee_double res;
    unsigned m1, m0;
    res.as_dbl = x;
    m1 = res.as_struct.mant1;
    m0 = res.as_struct.mant0;
    if ((m1 & ~SIGN_BIT | m0) == 0)			/* x == +-0 */
    {
	return MINDOUBLE;
    }
    else if (((long) m1) >= 0)				/* x > 0 */
    {
	if (m1 < 0x7ff00000)				/* x < Inf */
	{
	    Inc64(m1,m0);
	    res.as_struct.mant1 = m1;
	    res.as_struct.mant0 = m0;
	    return res.as_dbl;
	}
    }
    else if (m1 < 0xfff00000
        || m1 == 0xfff00000 && m0 == 0)			/* -Inf <= x < 0 */
    {
	Dec64(m1,m0);
	res.as_struct.mant1 = m1;
	res.as_struct.mant0 = m0;
	return res.as_dbl;
    }
    return x;						/* NaN */
}


double
ec_ieee_down(double x)
{
    ieee_double res;
    unsigned m1, m0;
    res.as_dbl = x;
    m1 = res.as_struct.mant1;
    m0 = res.as_struct.mant0;
    if ((m1 & ~SIGN_BIT | m0) == 0)			/* x == +-0 */
    {
	return -MINDOUBLE;
    }
    else if ((long) m1 > 0 || m1 == 0 && m0 > 0)	/* x > 0 */
    {
	if (m1 < 0x7ff00000
	    || m1 == 0x7ff00000 && m0 == 0)		/* x <= Inf */
	{
	    Dec64(m1,m0);
	    res.as_struct.mant1 = m1;
	    res.as_struct.mant0 = m0;
	    return res.as_dbl;
	}
    }
    else if (m1 < 0xfff00000)				/* -Inf < x < 0 */
    {
	Inc64(m1,m0);
	res.as_struct.mant1 = m1;
	res.as_struct.mant0 = m0;
	return res.as_dbl;
    }
    return x;						/* NaN */
}

#endif

extern void
ec_i_add(
    	volatile double xl,	/* Make sure the additions don't get reordered. */
	volatile double xu,
	double yl, double yu,
	double *lwb, double *upb)
{
    volatile double tmp_upb;
    volatile double tmp_lwb;

    set_round_up();
    tmp_upb = xu + yu;
    set_round_down();
    tmp_lwb = xl + yl;
    restore_round_mode();

    if (!GoodFloat(tmp_lwb) || !GoodFloat(tmp_upb)) {
	/* One or other of the resulting bounds is a NaN, so the "true" */
	/* value could be anything */
	*lwb = -HUGE_VAL;
	*upb = HUGE_VAL;
	return;
    }

    /* For any breal with one bound set to zero, the sign of the zero
       should be the same as the sign of the other bound*/
    if (tmp_lwb == 0.0 && tmp_upb > 0.0) {
	/* ensure lwb is positive zero */
	*lwb = 0.0;
    } else {
	*lwb = tmp_lwb;
    }
    if (tmp_upb == 0.0 && tmp_lwb < 0.0) {
	/* ensure upb is negative zero */
	*upb = -1*(0.0);
    } else {
	*upb = tmp_upb;
    }
}

extern void
ec_i_sub(double xl, double xu, double yl, double yu, double *lwb, double *upb)
{
    ec_i_add(xl, xu, -yu, -yl, lwb, upb);
}

/* Multiplies a bounded real by another bounded real. */

/*
** The usual way to multiply two bounded numbers of unknown sign is to
** compute the product of the 4 relevant bound pairs and select the smallest
** and largest as the resulting bounds.  If we're using processor rounding
** modes we probably need to do the calculations twice: once rounding up and
** once rounding down.
**
** Unfortunately, since zeros compare equal, it is hard to ensure that -0.0
** is selected over 0.0 for a lower bound, and vice-versa for the upper
** bound.  As a result, we do something more complicated, based on the
** following table.  The table gives, for each possible combination of signs
** of the input variables, the signs of each of the 4 products, and which
** products will give the lower and upper bounds of the results:
**
**	z = x * y
**
**	  sign of   |   sign of   | min/max
**	  inputs    |   products  | products
**	------------+-------------+-------------
**	xl xu yl yu | ll lu ul uu | zl    zu
**	------------+-------------+-------------
**	-  -  -  -  | +  +  +  +  | uu    ll
**	-  -  -  +  | +  -  +  -  | lu    ll
**	-  -  +  +  | -  -  -  -  | lu    ul
**	-  +  -  -  | +  +  -  -  | ul    ll
**	-  +  -  +  | +  -  -  +  | lu/ul ll/uu
**	-  +  +  +  | -  -  +  +  | lu    uu
**	+  +  -  -  | -  -  -  -  | ul    lu
**	+  +  -  +  | -  +  -  +  | ul    uu
**	+  +  +  +  | +  +  +  +  | ll    uu
**
** For computing the upper bound, we first determine whether xl and yl have
** the same sign.  Extracting the relevant subtables from the above we have:
**
** Case samesign(xl, yl):
**
**	  sign of   |   sign of   | max
**	  inputs    |   products  | product
**	------------+-------------+---------
**	xl xu yl yu | ll lu ul uu | zu
**	------------+-------------+---------
**	-  -  -  -  | +  +  +  +  | ll
**	-  -  -  +  | +  -  +  -  | ll
**	-  +  -  -  | +  +  -  -  | ll
**	-  +  -  +  | +  -  -  +  | ll/uu
**	+  +  +  +  | +  +  +  +  | uu
**
**	In this case the upper bound can only be ll or uu, and since ll
**	always has positive sign, it is safe wrt zeros to choose ll over uu
**	if they compare equal (if they're zero, it will yield the positive
**	one).
**
** Case !samesign(xl, yl):
**
**	  sign of   |   sign of   | max
**	  inputs    |   products  | product
**	------------+-------------+---------
**	xl xu yl yu | ll lu ul uu | zu
**	------------+-------------+---------
**	-  -  +  +  | -  -  -  -  | ul
**	-  +  +  +  | -  -  +  +  | uu
**	+  +  -  -  | -  -  -  -  | lu
**	+  +  -  +  | -  +  -  +  | uu
**
**	In this case we don't have to consider ll, and when products compare
**	equal it is safe to choose uu (if any of the other products have
**	positive sign, uu will too).
**
** For the lower bounds we can obtain similarly useful distinctions by
** comparing the signs for xl and yu:
**
** Case samesign(xl, yu):
**
**	  sign of   |   sign of   | min
**	  inputs    |   products  | product
**	------------+-------------+---------
**	xl xu yl yu | ll lu ul uu | zl
**	------------+-------------+---------
**	-  -  -  -  | +  +  +  +  | uu
**	-  +  -  -  | +  +  -  -  | ul
**	+  +  -  +  | -  +  -  +  | ul
**	+  +  +  +  | +  +  +  +  | ll
**
**	In this case we don't have to consider lu, and when products compare
**	equal it is safe to choose ul (if any of the other products have
**	negative sign, ul will too).
**
** Case !samesign(xl, yu):
**
**	  sign of   |   sign of   | min
**	  inputs    |   products  | product
**	------------+-------------+---------
**	xl xu yl yu | ll lu ul uu | zl
**	------------+-------------+---------
**	-  -  -  +  | +  -  +  -  | lu
**	-  -  +  +  | -  -  -  -  | lu
**	-  +  -  +  | +  -  -  +  | lu/ul
**	-  +  +  +  | -  -  +  +  | lu
**	+  +  -  -  | -  -  -  -  | ul
**	
**	In this case the lower bound can only be lu or ul, and since lu
**	always has negative sign, it is safe wrt zeros to choose lu over ul
**	if they compare equal (if they're zero, it will yield the negative
**	one).
*/

extern void
ec_i_mul(
	volatile double xl, /* Make sure multiplications don't get cached. */
	volatile double xu,
	double yl, double yu,
	double *lwb, double *upb)
{
    double ll;
    double lu;
    double ul;
    double uu;
    double ll_lu, ul_uu;

    /*
    ** Note that we catch NaNs in ll and uu when computing the upper bound
    ** and NaNs in lu and ul when computing the lower bound --- we assume
    ** the rounding mode does not affect a NaN result.
    */

    /* Compute the upper bound. */

    set_round_up();

    ll = xl * yl;
    if (!GoodFloat(ll)) goto I_MUL_BAD_FLOAT;
    uu = xu * yu;
    if (!GoodFloat(uu)) goto I_MUL_BAD_FLOAT;

    if (samesign(xl, yl)) {
	/* Upper bound is either ll or uu. */
	/* When equal, choosing ll over uu is safe wrt signed zeros. */
	*upb = ll >= uu ? ll : uu;
    } else {
	/* Upper bound can't be ll. */
	/* When equal, choosing uu is safe wrt signed zeros. */
	ul = xu * yl;
	lu = xl * yu;
	ul_uu = uu >= ul ? uu : ul;
	*upb = ul_uu >= lu ? ul_uu : lu;
    }

    /* Compute the lower bound. */

    set_round_down();

    lu = xl * yu;
    if (!GoodFloat(lu)) goto I_MUL_BAD_FLOAT;
    ul = xu * yl;
    if (!GoodFloat(ul)) goto I_MUL_BAD_FLOAT;

    if (samesign(xl, yu)) {
	/* Lower bound can't be lu. */
	/* When equal, choosing ul is safe wrt signed zeros. */
	ll = xl * yl;
	uu = xu * yu;
	ul_uu = ul <= uu ? ul : uu;
	*lwb = ul_uu <= ll ? ul_uu : ll;
    } else {
	/* Lower bound is either lu or ul. */
	/* When equal, choosing lu over ul is safe wrt signed zeros. */
	*lwb = lu <= ul ? lu : ul;
    }

    restore_round_mode();
    return;

I_MUL_BAD_FLOAT:
    /* We got a NaN, so just return an infinite interval. */
    restore_round_mode();
    *lwb = -HUGE_VAL;
    *upb = HUGE_VAL;
    return;
}

/* Divides a bounded real by another bounded real. */
/* See the notes for multiplication above. */
/* Note that it might be possible to exploit the fact that yl and yu will */
/* have the same sign in the main body of the code. */
extern void
ec_i_div(
	volatile double xl,	/* Make sure divisions don't get cached. */
	volatile double xu,
	double yl, double yu,
	double *lwb, double *upb)
{
    double ll;
    double lu;
    double ul;
    double uu;
    double ll_lu, ul_uu;

    /* Correctly handle division by negative zero */
    if (yl == 0.0) {
	/* This will ensure that if yl == 0.0 or yl == -0.0 then
	   it will be set to 0.0 */
	yl = 0.0;
    }

    if (yu == 0.0) {
	/* This will ensure that if yu == 0.0 or yu == -0.0 then
	   it will be set to -0.0 */
	yu = -1*(0.0);
    }

    /* If divisor spans zero, nothing to be done */
    if (!samesign(yl, yu)) {
	*lwb = -HUGE_VAL;
	*upb = HUGE_VAL;
	return;
    }

    /*
    ** Note that we catch NaNs in ll and uu when computing the upper bound
    ** and NaNs in lu and ul when computing the lower bound --- we assume
    ** the rounding mode does not affect a NaN result.
    */

    /* Compute the upper bound. */

    set_round_up();

    ll = xl / yu;
    if (!GoodFloat(ll)) goto I_DIV_BAD_FLOAT;
    uu = xu / yl;
    if (!GoodFloat(uu)) goto I_DIV_BAD_FLOAT;

    if (samesign(xl, yu)) {
	/* Upper bound is either ll or uu. */
	/* When equal, choosing ll over uu is safe wrt signed zeros. */
	*upb = ll >= uu ? ll : uu;
    } else {
	/* Upper bound can't be ll. */
	/* When equal, choosing uu is safe wrt signed zeros. */
	ul = xu / yu;
	lu = xl / yl;
	ul_uu = uu >= ul ? uu : ul;
	*upb = ul_uu >= lu ? ul_uu : lu;
    }

    /* Compute the lower bound. */

    set_round_down();

    lu = xl / yl;
    if (!GoodFloat(lu)) goto I_DIV_BAD_FLOAT;
    ul = xu / yu;
    if (!GoodFloat(ul)) goto I_DIV_BAD_FLOAT;

    if (samesign(xl, yl)) {
	/* Lower bound can't be lu. */
	/* When equal, choosing ul is safe wrt signed zeros. */
	ll = xl / yu;
	uu = xu / yl;
	ul_uu = ul <= uu ? ul : uu;
	*lwb = ul_uu <= ll ? ul_uu : ll;
    } else {
	/* Lower bound is either lu or ul. */
	/* When equal, choosing lu over ul is safe wrt signed zeros. */
	*lwb = lu <= ul ? lu : ul;
    }

    restore_round_mode();
    return;

I_DIV_BAD_FLOAT:
    /* We got a NaN, so just return an infinite interval. */
    restore_round_mode();
    *lwb = -HUGE_VAL;
    *upb = HUGE_VAL;
    return;
}


static void
i_exp(double xl, double xu, double *lwb, double *upb)
{
   /* Catch the special cases of raising 'e' to +Inf and -Inf as some
       platforms give incorrect results w.r.t. IEEE754 specs */
    if ( xl == -HUGE_VAL ) {
	*lwb = 0.0;
    } else if ( xl == HUGE_VAL ) {
        *lwb = HUGE_VAL;
    } else {
	*lwb = down(exp(xl));
    }
    if ( xu == -HUGE_VAL ) {
	*upb = 0.0;
    } else if ( xu == HUGE_VAL ) {
        *upb = HUGE_VAL;
    } else {
	*upb = up(exp(xu));
    }
}

static void
i_sin(double xl, double xu, double *lwb, double *upb, int cosflag)
{
    double width;
    volatile double xl1 = xl;	/* Hack to stop reordering */

    set_round_up();
    width = xu-xl1;
    restore_round_mode();

    if (width >= 2*M_PI)
    {
	*lwb = -1.0; *upb = 1.0;
    }
    else
    {
	double ls, lc, us, uc;

	if (cosflag) {
	    sincos(xl, &lc, &ls);
	    sincos(xu, &uc, &us);
	    lc = -lc; uc = -uc;
	} else {
	    sincos(xl, &ls, &lc);
	    sincos(xu, &us, &uc);
	}

	if (lc >= 0) {
	    if (uc >= 0) {
		if (width >= M_PI) {
		    *lwb = -1.0; *upb = 1.0;
		} else {
		    *lwb = down(ls); *upb = up(us);
		}
	    } else {
		*lwb = down(min(ls, us)); *upb = 1.0;
	    }
	} else {
	    if (uc >= 0) {
		*lwb = -1.0; *upb = up(max(ls, us));
	    } else {
		if (width >= M_PI) {
		    *lwb = -1.0; *upb = 1.0;
		} else {
		    *lwb = down(us); *upb = up(ls);
		}
	    }
	}
    }
}


static double
ipow(double x, int n, int roundup)	/* currently n > 0 */
{
    double res;
    int neg = 0;
    if (x < 0) {
	x = -x;
	if (n&1) {
	    neg = !neg;
	    roundup = !roundup;
	}
    }
    if (n < 5) {
	/* faster, but less precise... */
	volatile double x1 = x;	/* Hack to stop reordering */
	if (roundup) {
	    set_round_up();
	} else {
	    set_round_down();
	}
	res = x;
	while(--n > 0) res *= x1;
	restore_round_mode();
    } else {
	res = roundup ? up(Pow(x,(double)n)) : down(Pow(x,(double)n));
    }
    return neg ? -res : res;
}


static int
linsplit(double minwidth, double ratio, double lwb, double upb, int upper, double *split)
{
	/*
	 * Splits an interval given by lwb - upb, at 1/Ratio if
	 * upper == FALSE or 1/1-Ratio if upper = TRUE giving a
	 * new split point *split.
	 * We fail if the input interval is already narrower than MinWidth.
	 * Or if the piece removed is smaller than minwidth and the new
	 * interval is greater than minwidth.
	 */

	double halfwidth,halfmin;	/* never a NaN */
	double halfchunk;
	double scale;

	/* Quick check which ensures (among other things) that the lower */
	/* bound is not +inf and the upper bound is not -inf. */
	if (lwb == upb) {
	    if (minwidth == 0.0) {
		*split = lwb;
		return 0;
	    } else {
		/* Input interval is too small. */
		return -1;
	    }
	}

	if (!finite(upb)) {
		upb = MAXDOUBLE;
	}
	if (!finite(lwb)) {
		lwb = -MAXDOUBLE;
	}

	scale = fabs(upper ? upb : lwb);
	if (scale > 1.0) minwidth = minwidth * scale;

	halfmin = minwidth/2;

	halfwidth = upb/2.0 - lwb/2.0;	/* avoiding inf */
	if (halfwidth < halfmin) {
	    return -1;
	}

	halfchunk = ratio * halfwidth; 
	if (halfchunk < halfmin  &&  halfwidth-halfchunk > halfmin) {
	    return -1;
	}
	/* PROBLEM: we might cut off very small bits here... */
	if (upper) {
	    *split = upb - 2*halfchunk;
	} else {
	    *split = lwb + 2*halfchunk;
	}

	/*
	** In extreme cases (such as the user specifying a precision too
	** small), it might be possible for the split to lie outside the
	** bounds; in such cases we round it inwards.
	*/
	if (*split > upb) {
	    *split = upb;
	} else if (*split < lwb) {
	    *split = lwb;
	}

	return 0;
}


static int
logsplit(double minwidth, double ratio, double orig_lwb, double orig_upb, int upper, double *split)
{
	/*
	** Splits the interval in such a way that between -1 and 1 the split
	** is linear, and outside that it's logarithmic.  The rationale
	** behind this choice of splitting is that between -1 and 1 the
	** absolute error is smaller than the relative error (and hence
	** should be used for deciding when to stop splitting), while
	** outside that region the reverse is true.
	**
	** The splitting is done by mapping values x > 1 to 1 + log(x) and
	** values x < -1 to -1 - log(-x), and then calling the linear
	** splitting routine.  The result is then transformed back using the
	** reverse function: values y > 1 are mapped to exp(y - 1) and
	** values y < -1 to -exp(-y - 1).
	*/

#define convert_bound_log(x)	\
	if (x > 1) {		\
	    x = 1 + log(x);	\
	} else if (x < -1) {	\
	    x = -1 - log(-x);	\
	}

#define unconvert_bound_log(y)	\
	if (y > 1) {		\
	    y = exp(y - 1);	\
	} else if (y < -1) {	\
	    y = -exp(-y - 1);	\
	}

	int result;
	double lwb = orig_lwb, upb = orig_upb;

	/* Quick check which ensures (among other things) that the lower */
	/* bound is not +inf and the upper bound is not -inf. */
	if (lwb == upb) {
	    if (minwidth == 0.0) {
		*split = lwb;
		return 0;
	    } else {
		/* Input interval is too small. */
		return -1;
	    }
	}

	if (!finite(upb)) {
	    upb = MAXDOUBLE;
	}
	if (!finite(lwb)) {
	    lwb = -MAXDOUBLE;
	}

	convert_bound_log(lwb);
	convert_bound_log(upb);

	result = linsplit(minwidth, ratio, lwb, upb, upper, split);
	if (result != 0) {
	    return result;
	}

	unconvert_bound_log(*split);

	/*
	** In extreme cases (such as the user specifying a precision too
	** small), the split may not lie between the bounds; in such cases
	** we round it inwards.
	*/
	if (*split > orig_upb) {
	    *split = orig_upb;
	} else if (*split < orig_lwb) {
	    *split = orig_lwb;
	}

	return 0;
}


extern
int
ec_ria_unop(int op, double xl, double xu, double *zl_ptr, double *zu_ptr)
{
    double lwb, upb;

    switch (op)
    {
    case RIA_UN_SQR:				/* sqr */
	{
	    volatile double xl1 = xl, xu1 = xu; /* hack to stop reordering */
	    if (xl >= 0.0) {
		set_round_down();
		lwb = xl1 * xl1;
		set_round_up();
		upb = xu1 * xu1;
	    } else if (xu < 0.0) {
		set_round_down();
		lwb = xu1 * xu1;
		set_round_up();
		upb = xl1 * xl1;
	    } else {
		set_round_up();
		lwb = 0.0;
		upb = -xl > xu ? xl1 * xl1 : xu1 * xu1;
	    }
	    restore_round_mode();
	}
	break;
    case RIA_UN_SQRT:				/* sqrt */
	if (xu >= 0.0) {
	    upb = up(sqrt(xu));
	    lwb = xl < 0.0 ? 0.0 : down(sqrt(xl));
	} else {
	    Fail_;
	}
	break;
    case RIA_UN_SIN:				/* sin */
	i_sin(xl, xu, &lwb, &upb, 0);
	break;
    case RIA_UN_COS:				/* cos */
	i_sin(xl, xu, &lwb, &upb, 1);
	break;
    case RIA_UN_EXP:				/* exp */
        i_exp(xl, xu, &lwb, &upb);
	break;
    case RIA_UN_LN:				/* ln */
	if (xu >= 0.0) {
	    lwb = xl > 0.0 ? down(log(xl)) : -HUGE_VAL;
	    upb = up(log(xu));
	} else {
	    Fail_;
	}
	break;
    case RIA_UN_ATAN:				/* atan */
	lwb = down(atan(xl));
	upb = up(atan(xu));
	break;
    case RIA_UN_PI:				/* pi */
	/* argument is dummy */
	lwb = 3.141592653589793;
	upb = 3.1415926535897935;
	break;
    case RIA_UN_ABS:				/* abs */
	if (xl >= 0.0) {
	    lwb = xl; upb = xu;
	} else if (xu < 0.0) {
	    lwb = -xu; upb = -xl;
	} else {
	    lwb = 0.0; upb = max(xu, -xl);
	}
	break;
    case RIA_UN_ROUNDOUT:			/* roundout */
	lwb = down(xl);
	upb = up(xu);
	break;
    case RIA_UN_NEG:				/* - */
	lwb = -xu;
	upb = -xl;
	break;
    case RIA_UN_WIDTH:				/* width */
	lwb = upb = xu-xl;
	break;
    default:
	Bip_Error(RANGE_ERROR);
    }

    *zl_ptr = lwb;
    *zu_ptr = upb;
    
    Succeed_;
}


extern
int
ec_ria_binop(int op, double xl, double xu, double yl, double yu, double *zl_ptr, double *zu_ptr)
{
    double lwb, upb;
    int result;

    switch (op)
    {
    case RIA_BIN_ADD:				/* + */
        ec_i_add(xl, xu, yl, yu, &lwb, &upb);
	break;
    case RIA_BIN_SUB:				/* - */
        ec_i_sub(xl, xu, yl, yu, &lwb, &upb);
	break;
    case RIA_BIN_MULT:				/* * */
	ec_i_mul(xl, xu, yl, yu, &lwb, &upb);
	break;
    case RIA_BIN_DIV:				/* / */
	ec_i_div(xl, xu, yl, yu, &lwb, &upb);
	break;
    case RIA_BIN_RSQR:				/* rsqr i.e. +-sqrt */
    {
	/* second argument is used to check result range */
	double rmin, rmax;
	if (xu >= 0.0) {
	    rmax = up(sqrt(xu));
	    rmin = xl <= 0.0 ? 0.0 : down(sqrt(xl));
	    if (yl > -rmin) {
		upb = rmax; lwb = rmin;
	    } else if (yu < rmin) {
		upb = -rmin; lwb = -rmax;
	    } else {
		upb = rmax; lwb = -rmax;
	    }
	} else {
	    Fail_;
	}
	break;
    }
    case RIA_BIN_POW_INT:			/* pow(X,int) */
    {
	int n = (long) yl;
	if (n > 0)
	{
	    if (n&1) {				/* odd */
		lwb = ipow(xl,n,DOWN);
		upb = ipow(xu,n,UP);
	    } else {				/* even */
		if (xl >= 0.0) {
		    lwb = ipow(xl,n,DOWN);
		    upb = ipow(xu,n,UP);
		} else if (xu < 0.0) {
		    lwb = ipow(xu,n,DOWN);
		    upb = ipow(xl,n,UP);
		} else {	/* 0-crossing */
		    lwb = 0.0;
		    upb = xu > -xl ? ipow(xu,n,UP)
					: ipow(xl,n,UP);
		}
	    }
	}
	else if (n < 0)
	{
	    Bip_Error(RANGE_ERROR);
	}
	else
	{
	    if (xl <= 0.0 && xu >= 0.0) {
		lwb = -HUGE_VAL; upb = HUGE_VAL;
	    } else {
		lwb = 1.0; upb = 1.0;
	    }
	}
	break;
    }
    case RIA_BIN_RPOW_ODD:	/* Z=pow(X,Y), reverse of X=pow(Z,N), N odd */
	lwb = xl < 0.0 ?
		-up(Pow(-xl, -xl>1? yu: yl)) :
		down(Pow(xl, xl>1? yl: yu));
	upb = xu < 0.0 ?
		-down(Pow(-xu, -xu>1? yl: yu)) :
		up(Pow(xu, xu>1? yu: yl));
	break;
#if 0
    case 7:					/* pow(X,Y) */
	if (xu < 0.0) {
	    Fail_;
	} else {
	    if (yl > 0.0) {
		lwb = xl <= 0.0 ? 0.0 :
		    down(pow(xl, xl>1? yl: yu));
		upb = up(pow(xu, xu>1? yu: yl));
	    } else if (yu < 0.0) {
		lwb = down(pow(xu, xu>1? yl: yu));
		upb = xl <= 0.0 ? HUGE_VAL :
		    up(pow(xl, xl>1? yu: yl));
	    } else if (xl <= 0.0) {	/* X and Y-range include 0 */
		lwb = 0.0; upb = HUGE_VAL;
	    } else {				/* Y-range includes 0 */
		double ll = pow(xl,yl);
		double lu = pow(xl,yu);
		double ul = pow(xu,yl);
		double uu = pow(xu,yu);
		lwb = down(min(min(ll,lu),min(ul,uu)));
		upb = up(max(max(ll,lu),min(ul,uu)));
	    }
	}
	break;
    case RIA_BIN_RELAX:				/* relax(X,const) */
	lwb = down(xl-yu);
	upb = up(xu+yu);
	break;
#endif
    case RIA_BIN_MIN:				/* min(X,Y) */
	lwb = min(xl,yl);
	upb = min(xu,yu);
	break;
    case RIA_BIN_MAX:				/* max(X,Y) */
	lwb = max(xl,yl);
	upb = max(xu,yu);
	break;
    case RIA_BIN_LOGSPLIT:		/* logsplit(X,MinWidth,Ratio) */
	if (0 != logsplit(yl, yu, xl, xu, 0, &lwb))
		Fail_;
	break;
    case RIA_BIN_PLUSMINUS:			/* +- */
    {
	/* second argument is used to check result range */
	double absmin, absmax;
	lwb = xl;
	upb = xu;
	if (lwb >= 0.0) {
	    absmin = lwb; absmax = upb;
	} else if (upb <= 0.0) {
	    absmin = -upb; absmax = -lwb;
	} else {
	   absmin = 0.0; absmax = max(-lwb, upb);
	}
	if (yl > -absmin) {
	    lwb = absmin; upb = absmax;
	} else if (yu < absmin) {
	    lwb = -absmax; upb = -absmin;
	} else {
	    lwb = -absmax; upb = absmax;
	}
	break;
    }
    case RIA_BIN_MIN_DELTA:	/* compute min of relative and absolute delta */
    {
	double lwb_delta = yl-xl;	/* absolute delta */
	double upb_delta = xu-yu;	/* absolute delta */

#ifdef _WIN32
	/* Hack for Windows, where inf/inf is not a NAN, but a small float */
	/* (meaning an infinite bound changes return a negligible delta). */
	if (lwb_delta == HUGE_VAL) {
	    lwb = lwb_delta;
	} else
#endif
	if (lwb_delta > 0.0) {			/* lwb_delta may be NaN! */
	    lwb = lwb_delta/fabs(xl);	/* relative delta */
	    if (!(lwb < lwb_delta))		/* lwb mab be NaN! */
		lwb = lwb_delta;
	} else  {
	    lwb = 0.0;				/* no change */
	}
#ifdef _WIN32
	/* Hack for Windows, where inf/inf is not a NAN, but a small float */
	/* (meaning an infinite bound changes return a negligible delta). */
	if (upb_delta == HUGE_VAL) {
	    upb = upb_delta;
	} else
#endif
	if (upb_delta > 0.0) {			/* upb_delta may be NaN */
	    upb = upb_delta/fabs(xu);	/* relative delta */
	    if (!(upb < upb_delta))		/* upb may be NaN */
		upb = upb_delta;
	} else  {
	    upb = 0.0;				/* no change */
	}
	break;
    }
    case RIA_BIN_LINSPLIT:		/* linsplit(X,MinWidth,Ratio) */
	if (0 != linsplit(yl, yu, xl, xu, 0, &lwb))
		Fail_;
	break;
    case RIA_BIN_LINSPLIT_UPPER:	/* linsplit(X,MinWidth,Ratio) */
	if (0 != linsplit(yl, yu, xl, xu, 1, &lwb))
		Fail_;
	break;
    case RIA_BIN_LOGSPLIT_UPPER:	/* logsplit(X,MinWidth,Ratio) */
	if (0 != logsplit(yl, yu, xl, xu, 1, &lwb))
		Fail_;
	break;
#if 0
    case 17:			/* round(XL,XH,Precision) */
	lwb = xl - remainder(xl,yl);
	upb = xu - remainder(xu,yu);
	break;
#endif
    default:
	Bip_Error(RANGE_ERROR);
    }

    *zl_ptr = lwb;
    *zu_ptr = upb;
    
    Succeed_;
}


extern
int
ec_ria_ternop(int op, double xl, double xu, double yl, double yu, double zl, double zu, double *rl_ptr, double *ru_ptr)
{
    double lwb, upb;
    int result;
    switch (op)
    {
    case RIA_TERN_RPOW_EVEN:			/* rpow_even */
	/* Reverse of Z^N=X, N even. Computes R = intersect( X^Y, Z), Y=1/N */
	if (xu >= 0.0) {
	    lwb = xl <= 0.0 ? 0.0 : down(Pow(xl, yl));
	    upb = up(Pow(xu, yu));
	    if (zl > -lwb) {
		;			/* only positive solution */
	    } else if (zu < lwb) {
		double aux = upb;	/* only negative solution */
		upb = -lwb;
		lwb = -aux;
	    } else {
		lwb = -upb;		/* both solutions */
	    }
	} else {
	    Fail_;
	}
	break;
    case RIA_TERN_UNION:			/* union(X,Y,Z) */
	if (xu < yl) {		/* disjoint, X below Y */
	    lwb = xu < zl ? yl : xl;
	    upb = zu < yl ? xu : yu;
	    if (lwb > upb) { Fail_; }
	} else if (yu < xl) {	/* disjoint, Y below X */
	    lwb = yu < zl ? xl : yl;
	    upb = zu < xl ? yu : xu;
	    if (lwb > upb) { Fail_; }
	} else {
	    lwb = min(xl,yl);
	    upb = max(xu,yu);
	}
	break;
    case RIA_TERN_DIV:				/* / */
	if (!samesign(yl, yu)) {
	    volatile double xl1 = xl, xu1 = xu; /* hack to stop reordering */
	    /*
	    ** Want to work out the union of the intervals:
	    **   X / yu .. +inf
	    **   -inf .. X / yl
	    ** and match them against the interval for Z.
	    */
	    set_round_down();
	    lwb = xl1 / yu;
	    set_round_up();
	    upb = xu1 / yl;
	    if (zl > upb) {
		/* Lower computed interval falls outside Z's range, */
		/* so return upper computed interval. */
		upb = HUGE_VAL;
	    } else if (zu < lwb) {
		/* Upper computed interval falls outside Z's range, */
		/* so return lower computed interval. */
		lwb = -HUGE_VAL;
	    } else {
		/* Z overlaps both intervals, so can't choose one yet. */
		lwb = -HUGE_VAL;
		upb = HUGE_VAL;
	    }
	    restore_round_mode();
	} else {
            ec_i_div(xl, xu, yl, yu, &lwb, &upb);
	}
	break;
    default:
	Bip_Error(RANGE_ERROR);
    }

    *rl_ptr = lwb;
    *ru_ptr = upb;
    
    Succeed_;
}


/* ----------------------------------------------------------------------
 *  Prolog Interface
 * ---------------------------------------------------------------------- */

int
p_breal_from_bounds(value vl, type tl, value vu, type tu, value vx, type tx)
{
    double lo, hi;
    value ivl;
    int err;

    Check_Number(tl);
    Check_Number(tu);
    Check_Output_Type(tx, TIVL);

    /* Get the lower bound to use, coercing it (safely) if required. */
    if (IsDouble(tl)) {
	lo = Dbl(vl);
    } else {
	if (IsInterval(tl)) {
	    ivl = vl;
	} else {
	    err = tag_desc[TagType(tl)].coerce_to[TIVL](vl, &ivl);
	    if (err != PSUCCEED) return err;
	}
	lo = IvlLwb(ivl.ptr);
    }

    /* Get the upper bound to use, coercing it (safely) if required. */
    if (IsDouble(tu)) {
	hi = Dbl(vu);
    } else {
	value ivl;
	if (IsInterval(tu)) {
	    ivl = vu;
	} else {
	    err = tag_desc[TagType(tu)].coerce_to[TIVL](vu, &ivl);
	    if (err != PSUCCEED) return err;
	}
	hi = IvlUpb(ivl.ptr);
    }

    Return_Unify_Interval(vx, tx, lo, hi);
}


int
p_breal_min(value vx, type tx, value vmin, type tmin)
{
    double  min;

    Check_Output_Float(tmin);

    if (IsDouble(tx)) {
	Return_Unify_Pw(vmin, tmin, vx, tx);
    } else if (IsInterval(tx)) {
	Return_Unify_Double(vmin, tmin, IvlLwb(vx.ptr));
    } else {
	value   ivl;
	int	result;

	Check_Number(tx);
	result = tag_desc[TagType(tx)].coerce_to[TIVL](vx, &ivl);
	Return_If_Not_Success(result);

	Return_Unify_Double(vmin, tmin, IvlLwb(ivl.ptr));
    }
}


int
p_breal_max(value vx, type tx, value vmax, type tmax)
{
    Check_Output_Float(tmax);

    if (IsDouble(tx)) {
	Return_Unify_Pw(vmax, tmax, vx, tx);
    } else if (IsInterval(tx)) {
	Return_Unify_Double(vmax, tmax, IvlUpb(vx.ptr));
    } else {
	value   ivl;
	int	result;

	Check_Number(tx);
	result = tag_desc[TagType(tx)].coerce_to[TIVL](vx, &ivl);
	Return_If_Not_Success(result);

	Return_Unify_Double(vmax, tmax, IvlUpb(ivl.ptr));
    }
}


int
p_breal_bounds(value vx, type tx, value vmin, type tmin, value vmax, type tmax)
{
    Prepare_Requests

    Check_Output_Float(tmin);
    Check_Output_Float(tmax);

    if (IsDouble(tx)) {
	Request_Unify_Pw(vmin, tmin, vx, tx);
	Request_Unify_Pw(vmax, tmax, vx, tx);
    } else if (IsInterval(tx)) {
	Request_Unify_Double(vmin, tmin, IvlLwb(vx.ptr));
	Request_Unify_Double(vmax, tmax, IvlUpb(vx.ptr));
    } else {
	value   ivl;
	int	result;

	Check_Number(tx);
	result = tag_desc[TagType(tx)].coerce_to[TIVL](vx, &ivl);
	Return_If_Not_Success(result);

	Request_Unify_Double(vmin, tmin, IvlLwb(ivl.ptr));
	Request_Unify_Double(vmax, tmax, IvlUpb(ivl.ptr));
    }

    Return_Unify
}


/*
** NOTE:
** The following predicates (ec_ria_unop, ec_ria_binop, ec_ria_ternop) are
** intended to only be called with fresh new variables for the result
** bounds (zl/zu for unop/binop, rl/ru for ternop).  Using old variables
** will likely do the wrong thing.
*/

int
p_ria_unop(value vop, type top, value v_xl, type t_xl, value v_xu, type t_xu, value v_zl, type t_zl, value v_zu, type t_zu)
{
    double lwb, upb;
    int result;

    Check_Double(t_xl); Check_Double(t_xu);
    Check_Ref(t_zl);    Check_Ref(t_zu);

    result = ec_ria_unop(vop.nint, Dbl(v_xl), Dbl(v_xu), &lwb, &upb);
    Return_If_Not_Success(result);

    Return_Double(v_zl, t_zl, lwb);
    Return_Double(v_zu, t_zu, upb);
    Succeed_;
}


int
p_ria_binop(value vop, type top, value v_xl, type t_xl, value v_xu, type t_xu, value v_yl, type t_yl, value v_yu, type t_yu, value v_zl, type t_zl, value v_zu, type t_zu)
{
    double lwb, upb;
    int result;

    Check_Double(t_xl); Check_Double(t_xu);
    Check_Double(t_yl); Check_Double(t_yu);
    Check_Ref(t_zl);    Check_Ref(t_zu);

    result = ec_ria_binop(vop.nint, Dbl(v_xl), Dbl(v_xu), Dbl(v_yl), Dbl(v_yu),
		    &lwb, &upb);
    Return_If_Not_Success(result);

    Return_Double(v_zl, t_zl, lwb);
    Return_Double(v_zu, t_zu, upb);
    Succeed_;
}


int
p_ria_ternop(value vop, type top, value v_xl, type t_xl, value v_xu, type t_xu, value v_yl, type t_yl, value v_yu, type t_yu, value v_zl, type t_zl, value v_zu, type t_zu, value v_rl, type t_rl, value v_ru, type t_ru)
{
    double lwb, upb;
    int result;

    Check_Double(t_xl); Check_Double(t_xu);
    Check_Double(t_yl); Check_Double(t_yu);
    Check_Double(t_zl); Check_Double(t_zu);
    Check_Ref(t_rl);    Check_Ref(t_ru);

    result = ec_ria_ternop(vop.nint, Dbl(v_xl), Dbl(v_xu), Dbl(v_yl), Dbl(v_yu),
		    Dbl(v_zl), Dbl(v_zu), &lwb, &upb);
    Return_If_Not_Success(result);

    Return_Double(v_rl, t_rl, lwb);
    Return_Double(v_ru, t_ru, upb);
    Succeed_;
}


/*--------------------------------------------------------------------------
 * Methods
 *--------------------------------------------------------------------------*/

#define IVL_STRING_SIZE	64


static int
_int_ivl(value in, value *out)	/* CAUTION: we allow out == &in */
{
    pword *pw;

#ifdef DOUBLE_INT_LIMIT
    /* We're on a machine where an integer may not be exactly representable
     * as a double.
     */
    if (in.nint > DOUBLE_INT_LIMIT || in.nint < -DOUBLE_INT_LIMIT) {
	/* The integer is not exactly representable, so we need to widen. */
	Push_Interval(pw, down((double) in.nint), up((double) in.nint));
    } else
#endif
	Push_Interval(pw, (double) in.nint, (double) in.nint);

    out->ptr = pw;
    Succeed_;
}


static int
_dbl_ivl(value in, value *out)	/* CAUTION: we allow out == &in */
{
    pword *pw;
    double in_dbl = Dbl(in) ;
    Push_Interval(pw, in_dbl, in_dbl);
    out->ptr = pw;
    Succeed_;
}


static int
_big_ivl(value in, value *out)	/* CAUTION: we allow out == &in */
{
    pword *pw;
    value dval;
    double d;
    int err;
    err = tag_desc[TBIG].coerce_to[TDBL](in, &dval);
    if (err != PSUCCEED) return(err);
    d = Dbl(dval);
    if (d >= DOUBLE_INT_LIMIT_AS_DOUBLE || d <= -DOUBLE_INT_LIMIT_AS_DOUBLE) {
	/* The integer is not exactly representable, so we need to widen. */
	Push_Interval(pw, down(d), up(d));
    } else {
	Push_Interval(pw, d, d);
    }
    out->ptr = pw;
    Succeed_;
}


    /* This is too conservative: we need to round out only
     * if the float cannot not represent the rational exactly
     */
static int
_rat_ivl(value in, value *out)	/* CAUTION: we allow out == &in */
{
    pword *pw;
    value dval;
    int err;
    err = tag_desc[TRAT].coerce_to[TDBL](in, &dval);
    if (err != PSUCCEED) return(err);
    Push_Interval(pw, down(Dbl(dval)), up(Dbl(dval)));
    out->ptr = pw;
    Succeed_;
}


/*ARGSUSED*/
static int
_ivl_dbl(value in, value *out)	/* CAUTION: we allow out == &in */
{
    double d;
    if (logsplit(0.0, 0.5, IvlLwb(in.ptr), IvlUpb(in.ptr), 0, &d) != 0)
    {
	Bip_Error(RANGE_ERROR);
    }
    Make_Double_Val(*out, d);
    Succeed_;
}


/*ARGSUSED*/
static int
_ivl_string_size(value v, type t, int quoted)
{
    return IVL_STRING_SIZE;
}


/*ARGSUSED*/
static int
_ivl_to_string(value v, type t, char *buf, int quoted)
{
    int len;
    type tdbl;
    value dval;
    tdbl.kernel = TDBL;
#ifdef UNBOXED_DOUBLES
    Make_Double_Val(dval, IvlLwb(v.ptr));
    len = tag_desc[TDBL].to_string(dval, tdbl, buf, quoted);
    buf[len++] = '_';
    buf[len++] = '_';
    Make_Double_Val(dval, IvlUpb(v.ptr));
    len += tag_desc[TDBL].to_string(dval, tdbl, buf+len, quoted);
#else
    dval.ptr = v.ptr;		/* dirty */
    len = tag_desc[TDBL].to_string(dval, tdbl, buf, quoted);
    buf[len++] = '_';
    buf[len++] = '_';
    dval.ptr = (pword*)((double*)(v.ptr)+1);	/* dirty */
    len += tag_desc[TDBL].to_string(dval, tdbl, buf+len, quoted);
#endif
    return len;
}


/*ARGSUSED*/
static int
_write_ivl(int quoted, stream_id stream, value v, type t)
{
    char buf[IVL_STRING_SIZE];
    int len = _ivl_to_string(v, t, buf, quoted);
    return ec_outf(stream, buf, len);
}


static int
_ivl_from_string(char *s, pword *result, int base)
{
    (void) string_to_number(s, result, (stream_id) 0, 0);
    if (IsTag(result->tag.kernel, TEND))
	{ Bip_Error(BAD_FORMAT_STRING) }
    Succeed_;
}

static int
_ivl_add(value v1, value v2, pword *pres)
{
    double lwb, upb;
    double v1_lwb, v1_upb, v2_lwb, v2_upb ;
    v1_lwb = IvlLwb(v1.ptr);
    v1_upb = IvlUpb(v1.ptr);
    v2_lwb = IvlLwb(v2.ptr);
    v2_upb = IvlUpb(v2.ptr);
    ec_i_add(v1_lwb, v1_upb, v2_lwb, v2_upb, &lwb, &upb);
    Make_Interval(pres, lwb, upb);
    Succeed_;
}

 
static int
_ivl_sub(value v1, value v2, pword *pres)
{
    double lwb, upb;
    double v1_lwb, v1_upb, v2_lwb, v2_upb ;
    v1_lwb = IvlLwb(v1.ptr);
    v1_upb = IvlUpb(v1.ptr);
    v2_lwb = IvlLwb(v2.ptr);
    v2_upb = IvlUpb(v2.ptr);
    ec_i_sub(v1_lwb, v1_upb, v2_lwb, v2_upb, &lwb, &upb );
    Make_Interval(pres, lwb, upb);
    Succeed_;
}

 
static int
_ivl_mul(value v1, value v2, pword *pres)
{
    double lwb, upb;
    double v1_lwb, v1_upb, v2_lwb, v2_upb ;
    v1_lwb = IvlLwb(v1.ptr);
    v1_upb = IvlUpb(v1.ptr);
    v2_lwb = IvlLwb(v2.ptr);
    v2_upb = IvlUpb(v2.ptr);
    ec_i_mul(v1_lwb, v1_upb, v2_lwb, v2_upb, &lwb, &upb);
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_div(value v1, value v2, pword *pres)
{
    double lwb, upb;
    double v1_lwb, v1_upb, v2_lwb, v2_upb ;
    v1_lwb = IvlLwb(v1.ptr);
    v1_upb = IvlUpb(v1.ptr);
    v2_lwb = IvlLwb(v2.ptr);
    v2_upb = IvlUpb(v2.ptr);
    ec_i_div(v1_lwb, v1_upb, v2_lwb, v2_upb, &lwb, &upb);
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_min(value v1, value v2, pword *pres)
{
    double t1, t2;
    double lwb, upb;
    t1 = IvlLwb(v1.ptr);
    t2 = IvlLwb(v2.ptr);
    lwb = min(t1,t2);
    t1 = IvlUpb(v1.ptr);
    t2 = IvlUpb(v2.ptr);
    upb = min(t1,t2);
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_max(value v1, value v2, pword *pres)
{
    double t1, t2;
    double lwb, upb;
    t1 = IvlLwb(v1.ptr);
    t2 = IvlLwb(v2.ptr);
    lwb = max(t1,t2);
    t1 = IvlUpb(v1.ptr);
    t2 = IvlUpb(v2.ptr);
    upb = max(t1,t2);
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_neg(value v1, pword *pres)
{
    Make_Interval(pres, -IvlUpb(v1.ptr), -IvlLwb(v1.ptr));
    Succeed_;
}


/*
 * This function is only called from the parser and gets passed
 * intervals that have been constructed by the lexer. These might
 * be proper intervals, in which case they are normally negated.
 *
 * Otherwise they are "raw intervals". This happens because the lexer
 * returns "-1.1__-0.9" as two separate tokens, "-" and "1.1__-0.9",
 * The latter is then a "raw interval" which doesn't satisfy lwb=<upb.
 * The parser then calls this function to combine the sign and the raw
 * interval into a proper interval. This is done by just negating the
 * lower bound. The raw-flag is reset in the parser or read_token.
 */ 
static int
_ivl_chgsign(value v1, pword *pres)
{
    if (!RawInterval(v1.ptr))
    	return _ivl_neg(v1, pres);

    Make_Interval(pres, -IvlLwb(v1.ptr), IvlUpb(v1.ptr));
    Succeed_;
}


static int
_ivl_abs(value v1, pword *pres)
{
    double lwb, upb;
    if (IvlLwb(v1.ptr) >= 0.0) {
	lwb = IvlLwb(v1.ptr); upb = IvlUpb(v1.ptr);
    } else if (IvlUpb(v1.ptr) < 0.0) {
	lwb = -IvlUpb(v1.ptr); upb = -IvlLwb(v1.ptr);
    } else {
	double t1 = IvlUpb(v1.ptr);
	double t2 = -IvlLwb(v1.ptr);
	lwb = 0.0; upb = max(t1, t2);
    }
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_sqrt(value v1, pword *pres)
{
    double lwb, upb;
    if (IvlUpb(v1.ptr) >= 0.0) {
	upb = up(sqrt(IvlUpb(v1.ptr)));
	lwb = IvlLwb(v1.ptr) < 0.0 ? 0.0 : down(sqrt(IvlLwb(v1.ptr)));
    } else {
	Bip_Error(ARITH_EXCEPTION);
    }
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_sin(value v1, pword *pres)
{
    double lwb, upb;
    i_sin(IvlLwb(v1.ptr), IvlUpb(v1.ptr), &lwb, &upb, 0);
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_cos(value v1, pword *pres)
{
    double lwb, upb;
    i_sin(IvlLwb(v1.ptr), IvlUpb(v1.ptr), &lwb, &upb, 1);
    Make_Interval(pres, lwb, upb);
    Succeed_;
}

static int
_ivl_exp(value v1, pword *pres)
{
    double lwb, upb;
    i_exp(IvlLwb(v1.ptr), IvlUpb(v1.ptr), &lwb, &upb);
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_ln(value v1, pword *pres)
{
    double lwb, upb;
    if (IvlUpb(v1.ptr) >= 0.0) {
	lwb = IvlLwb(v1.ptr) > 0.0 ? down(log(IvlLwb(v1.ptr))) : -HUGE_VAL;
	upb = up(log(IvlUpb(v1.ptr)));
    } else {
	Bip_Error(ARITH_EXCEPTION);
    }
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_atan(value v1, pword *pres)
{
    double lwb, upb;
    lwb = down(atan(IvlLwb(v1.ptr)));
    upb = up(atan(IvlUpb(v1.ptr)));
    Make_Interval(pres, lwb, upb);
    Succeed_;
}

static int
_ivl_atan2(value vy, value vx, pword *pres)
{
    double lwb, upb;
    double xl, xu, yl, yu;
    yl = IvlLwb(vy.ptr);
    yu = IvlUpb(vy.ptr);
    xl = IvlLwb(vx.ptr);
    xu = IvlUpb(vx.ptr);
    if (samesign(xl,-1.0) && !samesign(yl, yu)) {
	lwb = -3.1415926535897935;
	upb = 3.1415926535897935;
    } else {
	double t1, t2;
	double ll = Atan2(yl,xl);
	double lu = Atan2(yl,xu);
	double ul = Atan2(yu,xl);
	double uu = Atan2(yu,xu);
	t1 = min(ll, lu);
	t2 = min(ul, uu);
	lwb = down(min(t1, t2));
	t1 = max(ll, lu);
	t2 = max(ul, uu);
	upb = up(max(t1, t2));
    }
    Make_Interval(pres, lwb, upb);
    Succeed_;
}

static int
_ivl_pow(value v1, value v2, pword *pres)
{
    double	xl, xu, yl, yu;
    double	lwb, upb;

    xl = IvlLwb(v1.ptr);
    xu = IvlUpb(v1.ptr);
    yl = IvlLwb(v2.ptr);
    yu = IvlUpb(v2.ptr);

    if (xu < 0.0) {
        Bip_Error(RANGE_ERROR);
    } else {
	if ( xl < 0.0 ) {
	    /* If the base lower bound is negative the result could be
               complex*/
	    Bip_Error(ARITH_EXCEPTION);
	}
	if (yl > 0.0) {
            lwb = (xl == 0.0)?0.0:down(Pow(xl, xl>1? yl: yu));
            upb = up(Pow(xu, xu>1? yu: yl));
        } else if (yu < 0.0) {
            lwb = down(Pow(xu, xu>1? yl: yu));
            upb = up(Pow(xl, xl>1? yu: yl));
        } else if (xl == 0.0) {	/* X and Y-range include 0 */
            lwb = 0.0; upb = HUGE_VAL;
        } else {				/* Y-range includes 0 */
	    double t1, t2;
            double ll = Pow(xl,yl);
            double lu = Pow(xl,yu);
            double ul = Pow(xu,yl);
            double uu = Pow(xu,yu);
	    t1 = min(ll, lu);
	    t2 = min(ul, uu);
            lwb = down(min(t1, t2));
	    t1 = max(ll, lu);
	    t2 = max(ul, uu);
            upb = up(max(t1, t2));
        }
    }
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_floor(value v1, pword *pres)
{
    double lwb, upb;
    lwb = floor(IvlLwb(v1.ptr));	/* integers, no inaccuracy */
    upb = floor(IvlUpb(v1.ptr));
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_ceil(value v1, pword *pres)
{
    double lwb, upb;
    lwb = Ceil(IvlLwb(v1.ptr));		/* integers, no inaccuracy */
    upb = Ceil(IvlUpb(v1.ptr));
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_truncate(value v1, pword *pres)
{
    double lwb, upb;
#ifdef HAVE_TRUNC
    lwb = trunc(IvlLwb(v1.ptr));		/* integers, no inaccuracy */
    upb = trunc(IvlUpb(v1.ptr));
#else
    lwb = IvlLwb(v1.ptr);
    lwb = lwb < 0 ? Ceil(lwb) : floor(lwb);
    upb = IvlUpb(v1.ptr);
    upb = upb < 0 ? Ceil(upb) : floor(upb);
#endif
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_sgn(value v1, pword *pres)
{
    if (IvlLwb(v1.ptr) > 0.0)
    	pres->val.nint = 1L;
    else if (IvlUpb(v1.ptr) < 0.0)
    	pres->val.nint = -1L;
    else if (IvlLwb(v1.ptr) == 0.0  &&  IvlUpb(v1.ptr) == 0.0)
    	pres->val.nint = 0L;
    else { Bip_Error(ARITH_EXCEPTION); }
    Succeed_;
}


static int
_ivl_round(value v1, pword *pres)
{
    double lwb, upb;
#if defined(HAVE_RINT) && !defined(HP_RINT)
    lwb = rint(IvlLwb(v1.ptr));
    upb = rint(IvlUpb(v1.ptr));
#else
    /*
     * Round to even number if we are exactly in the middle.
     * Make sure we round to -0.0 if between -0.5 and -0.0
     */
    lwb = Ceil(IvlLwb(v1.ptr));
    if (lwb - IvlLwb(v1.ptr) > 0.5 || (lwb - IvlLwb(v1.ptr) == 0.5 && ((long)lwb & 1)))
	lwb -= 1.0;
    upb = Ceil(IvlUpb(v1.ptr));
    if (upb - IvlUpb(v1.ptr) > 0.5 || (upb - IvlUpb(v1.ptr) == 0.5 && ((long)upb & 1)))
	upb -= 1.0;
#endif /* rint */
    Make_Interval(pres, lwb, upb);
    Succeed_;
}


static int
_ivl_int2(value v1, pword *pres)
{
    double ipart;
    if (IvlLwb(v1.ptr) == IvlUpb(v1.ptr) && (modf(IvlLwb(v1.ptr), &ipart) == 0.0))
    {
	value dval;
	Make_Double_Val(dval, ipart);
	return tag_desc[TDBL].arith_op[ARITH_FIX](dval, pres);
    }
    else
    {
	Bip_Error(ARITH_EXCEPTION);
    }
}


/*
 * This is term comparison, which must be totally defined.
 * Much of this ordering is arbitrary, but the following invariants hold:
 *
 * Where ==/2 succeeds
 *	term compare must give = (compatible with _equal_ivl())
 * Where arith compare yields < or >
 *	term compare should give the same.
 * Where arith compare yields =
 *	term compare will give =
 *	except for different zeros, where it can give <, > or =
 * Where arith compare delays (or ==/2 is undecidable)
 *	term compare can give >, < or =
 */

static int
_compare_ivl(value v1, value v2)
{
    double lwb1, upb1;
    double lwb2, upb2;

    if (v1.ptr == v2.ptr)
    	return 0;

    if ((lwb1 = IvlLwb(v1.ptr)) > (upb2 = IvlUpb(v2.ptr)))
	return 1;				/* unambiguous */

    if ((upb1 = IvlUpb(v1.ptr)) < (lwb2 = IvlLwb(v2.ptr)))
	return -1;				/* unambiguous */

    /* overlap, but possibly just at -0.0,0.0 */
    return PedanticGreater(lwb1,lwb2) ?  1	/* arbitrary (overlap) */
	 : PedanticLess(lwb1,lwb2)    ? -1	/* arbitrary (overlap) */
	 : PedanticGreater(upb1,upb2) ?  1	/* arbitrary (overlap) */
	 : PedanticLess(upb1,upb2)    ? -1	/* arbitrary (overlap) */
    /* both lower bounds and both upper bounds are exactly equal */
	 : !(GlobalFlags & BREAL_EXCEPTIONS) ? 0
	 : lwb1 == upb2 ? 0			/* zero width */
	 : v1.ptr > v2.ptr ? 1 : -1;		/* arbitrary (same bounds) */
}


/*
 * This is arithmetic comparison, which may delay if it is not decidable.
 */

static int
_arith_compare_ivl(value v1, value v2, int *relation)
{
    /* We want identical terms to compare equal regardless of anything else. */
    if (v1.ptr == v2.ptr) {
    	*relation = 0;
    	return PSUCCEED;
    }

    /*
     * Note that in some of the cases below, we might return 1 or -1 when
     * the values could actually be equal.  This works out OK though because
     * the test in the calling code will give the right answer.
     */
    switch(*relation) {
    	case BILe:
	    if (IvlUpb(v1.ptr) <= IvlLwb(v2.ptr)) {
		*relation = -1;	/* Means -1 or 0 */
	    	return PSUCCEED;
	    } else if (IvlLwb(v1.ptr) > IvlUpb(v2.ptr)) {
	    	*relation = 1;
		return PSUCCEED;
	    } else {
	    	*relation = 0;
		return PDELAY;
	    }
	    break;
    	case BILt:
	    if (IvlUpb(v1.ptr) < IvlLwb(v2.ptr)) {
		*relation = -1;
	    	return PSUCCEED;
	    } else if (IvlLwb(v1.ptr) >= IvlUpb(v2.ptr)) {
	    	*relation = 1;	/* Means 1 or 0 */
		return PSUCCEED;
	    } else {
	    	*relation = 0;
		return PDELAY;
	    }
	    break;
    	case BIGe:
	    if (IvlLwb(v1.ptr) >= IvlUpb(v2.ptr)) {
		*relation = 1;	/* Means 1 or 0 */
	    	return PSUCCEED;
	    } else if (IvlUpb(v1.ptr) < IvlLwb(v2.ptr)) {
	    	*relation = -1;
		return PSUCCEED;
	    } else {
	    	*relation = 0;
		return PDELAY;
	    }
	    break;
    	case BIGt:
	    if (IvlLwb(v1.ptr) > IvlUpb(v2.ptr)) {
		*relation = 1;
	    	return PSUCCEED;
	    } else if (IvlUpb(v1.ptr) <= IvlLwb(v2.ptr)) {
	    	*relation = -1;	/* Means -1 or 0 */
		return PSUCCEED;
	    } else {
	    	*relation = 0;
		return PDELAY;
	    }
	    break;
	case BIEq:
	case BINe:
	    if (IvlUpb(v1.ptr) < IvlLwb(v2.ptr)) {
	    	*relation = -1;
		return PSUCCEED;
	    } else if (IvlLwb(v1.ptr) > IvlUpb(v2.ptr)) {
	    	*relation = 1;
		return PSUCCEED;
	    } else if (IvlLwb(v1.ptr) == IvlUpb(v2.ptr) &&
	    		IvlUpb(v1.ptr) == IvlLwb(v2.ptr)) {
	    	*relation = 0;
		return PSUCCEED;
	    } else {
	    	*relation = 0;
		return PDELAY;
	    }
	    break;
    }

    return PERROR;
}


static int
_equal_ivl(pword *pw1, pword *pw2)
{
    if (pw1 == pw2)
	return 1;	/* pointers identical */

    if (GlobalFlags & BREAL_EXCEPTIONS)
    {
	if (IvlUpb(pw1) < IvlLwb(pw2) || IvlUpb(pw2) < IvlLwb(pw1))
	    return 0;	/* no overlap */

	/* overlap: check for zero width */
	if (IvlLwb(pw1) == IvlUpb(pw2) && IvlUpb(pw1) == IvlLwb(pw2))
	{
	    if (IvlLwb(pw1) != 0)
		return 1;		/* nonzero and zero width: identical */

	    if (PedanticZeroEq(IvlLwb(pw1), IvlLwb(pw2))
	     && PedanticZeroEq(IvlUpb(pw1), IvlUpb(pw2)))
		return 1;		/* identical zeros */
	}

	/* nonzero width and overlap: undecidable */
	{
	    value v;
	    v.did = d_undecidable;
	    Exit_Block(v, tdict);
	}
    }
    else
    {
#if SIZEOF_DOUBLE < SIZEOF_WORD  ||  SIZEOF_DOUBLE > 2*SIZEOF_WORD
	PROBLEM: Cannot deal with word size SIZEOF_WORD.
#else
	return ((uword*)BufferStart(pw1))[0] == ((uword*)BufferStart(pw2))[0]
	    && ((uword*)BufferStart(pw1))[1] == ((uword*)BufferStart(pw2))[1]
#if SIZEOF_DOUBLE > SIZEOF_WORD
	    && ((uword*)BufferStart(pw1))[2] == ((uword*)BufferStart(pw2))[2]
	    && ((uword*)BufferStart(pw1))[3] == ((uword*)BufferStart(pw2))[3]
#endif
#endif
        ;
    }
}


/*ARGSUSED*/
static int
_unimp_err(void)
{
    return UNIMPLEMENTED;
}


/*--------------------------------------------------------------------------
 * Initialize intervals
 *--------------------------------------------------------------------------*/

void
ec_intervals_init(void)
{

#ifdef SAFE_ROUNDING
#ifdef IEEE_ROUND_DOWN
    char *res;
    ieee_flags("set", "direction", "negative", &res);
#endif
#ifdef IEEE_INEXACT
    ieee_handler("set", "inexact", inexact_handler);
#endif
#endif

    tag_desc[TIVL].tag_name = in_dict("breal", 0);
    tag_desc[TIVL].type_name = in_dict("breal", 0);

    tag_desc[TINT].coerce_to[TIVL] = _int_ivl;	/* coerce to interval */
    tag_desc[TDBL].coerce_to[TIVL] = _dbl_ivl;
    tag_desc[TBIG].coerce_to[TIVL] = _big_ivl;
    tag_desc[TRAT].coerce_to[TIVL] = _rat_ivl;

    tag_desc[TIVL].coerce_to[TDBL] = _ivl_dbl;	/* coerce from interval */

    tag_desc[TIVL].string_size = _ivl_string_size;
    tag_desc[TIVL].to_string = _ivl_to_string;
    tag_desc[TIVL].write = _write_ivl;
    tag_desc[TIVL].compare = _compare_ivl;
    tag_desc[TIVL].arith_compare = _arith_compare_ivl;
    tag_desc[TIVL].equal = _equal_ivl;

    tag_desc[TIVL].arith_op[ARITH_PLUS] = tag_desc[TDBL].arith_op[ARITH_PLUS];
    tag_desc[TIVL].arith_op[ARITH_NEG] = _ivl_neg;
    tag_desc[TIVL].arith_op[ARITH_ABS] = _ivl_abs;
    tag_desc[TIVL].arith_op[ARITH_ADD] = _ivl_add;
    tag_desc[TIVL].arith_op[ARITH_SUB] = _ivl_sub;
    tag_desc[TIVL].arith_op[ARITH_MUL] = _ivl_mul;
    tag_desc[TIVL].arith_op[ARITH_DIV] = _ivl_div;
    tag_desc[TIVL].arith_op[ARITH_MIN] = _ivl_min;
    tag_desc[TIVL].arith_op[ARITH_MAX] = _ivl_max;
    tag_desc[TIVL].arith_op[ARITH_SIN] = _ivl_sin;
    tag_desc[TIVL].arith_op[ARITH_COS] = _ivl_cos;
    tag_desc[TIVL].arith_op[ARITH_TAN] = _unimp_err;
    tag_desc[TIVL].arith_op[ARITH_EXP] = _ivl_exp;
    tag_desc[TIVL].arith_op[ARITH_LN] = _ivl_ln;
    tag_desc[TIVL].arith_op[ARITH_ASIN] = _unimp_err;
    tag_desc[TIVL].arith_op[ARITH_ACOS] = _unimp_err;
    tag_desc[TIVL].arith_op[ARITH_ATAN] = _ivl_atan;
    tag_desc[TIVL].arith_op[ARITH_ATAN2] = _ivl_atan2;
    tag_desc[TIVL].arith_op[ARITH_SQRT] = _ivl_sqrt;
    tag_desc[TIVL].arith_op[ARITH_POW] = _ivl_pow;
    tag_desc[TIVL].arith_op[ARITH_FLOOR] = _ivl_floor;
    tag_desc[TIVL].arith_op[ARITH_CEIL] = _ivl_ceil;
    tag_desc[TIVL].arith_op[ARITH_TRUNCATE] = _ivl_truncate;
    tag_desc[TIVL].arith_op[ARITH_ROUND] = _ivl_round;
    tag_desc[TIVL].arith_op[ARITH_SGN] = _ivl_sgn;
    tag_desc[TIVL].arith_op[ARITH_NICERAT] = _unimp_err;
    tag_desc[TIVL].arith_op[ARITH_INT] = _ivl_int2;

    tag_desc[TIVL].arith_op[ARITH_CHGSIGN] = _ivl_chgsign;

    tag_desc[TIVL].from_string = _ivl_from_string;

    built_in(in_dict("ria_unop", 5),	p_ria_unop, B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(4, CONSTANT) | BoundArg(5, CONSTANT);
    built_in(in_dict("ria_binop", 7),	p_ria_binop, B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(6, CONSTANT) | BoundArg(7, CONSTANT);
    (void) built_in(in_dict("ria_ternop", 9),	p_ria_ternop, B_UNSAFE|U_GROUND);
	/* no space in mode mask to store this information (arity too high): */
    	/* -> mode = BoundArg(8, CONSTANT) | BoundArg(9, CONSTANT);	*/
    (void) exported_built_in(in_dict("breal_from_bounds", 3), p_breal_from_bounds, B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("breal_min", 2), p_breal_min, B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("breal_max", 2), p_breal_max, B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("breal_bounds", 3), p_breal_bounds, B_UNSAFE|U_SIMPLE);

    d_undecidable = in_dict("undecidable comparison of bounded reals", 0);
}
