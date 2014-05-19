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
 * ECLiPSe INCLUDE FILE
 *
 * $Id: ec_public.h,v 1.2 2008/07/10 00:33:05 jschimpf Exp $
 *
 * Macro definitions needed for the ECLiPSe embedding interface.
 *
 */

/* To allow to compile external include files under C++ */

#if defined(__cplusplus)
#  define Dots	...
#  define Extern extern "C"
#  define ARGS(x) x
#  ifdef const
#    undef const
#  endif
#else
#  define Dots
#  define Extern extern
#  ifdef __STDC__
#    define ARGS(x) x
#  else
#    define ARGS(x) ()
#  endif
#endif

/******************************************************************/
/*		Machine-dependent definitions			 */
/******************************************************************/

#if (SIZEOF_CHAR_P != SIZEOF_LONG)
PROBLEM: Code assumes that SIZEOF_CHAR_P == SIZEOF_LONG
#endif

/* A "word" is a pointer-sized integer.
 * The following size/min/max definitions are all about "words",
 * even when they say "int".
 */
#define SIZEOF_WORD	SIZEOF_CHAR_P

/* Important:  SIGN_BIT is unsigned! */
#if (SIZEOF_WORD == 8)
#define SIGN_BIT	((unsigned long) 0x8000000000000000L)
#define MAX_NUMBER	"9223372036854775807"
#else
#if (SIZEOF_WORD == 4)
#define SIGN_BIT	((unsigned) 0x80000000L)
#define MAX_NUMBER	"2147483647"
#else
PROBLEM: Cannot deal with word size SIZEOF_WORD.
#endif
#endif

#define MAX_U_WORD	((unsigned long) -1)
#define MAX_S_WORD	((long) ~SIGN_BIT)
#define MIN_S_WORD      ((long) SIGN_BIT)

/*
 * Min/max words as doubles.
 * These definitions are guaranteed to work fine regardless of word size as
 * as long as MIN_S_WORD is a power of 2 and MAX_S_WORD is one less than a
 * power of two --- assuming round-to-nearest during type conversion.
 */

#define MIN_S_WORD_DBL		((double)MIN_S_WORD)
#define MAX_S_WORD_1_DBL	((double)MAX_S_WORD+1.0)


/*
 * If the word size is big enough, doubles are unboxed
 */

#define SIZEOF_DOUBLE	8

#if SIZEOF_WORD >= SIZEOF_DOUBLE
#define UNBOXED_DOUBLES
#else
#undef UNBOXED_DOUBLES
#endif


/*
 * Sometimes it's useful to know how large an integer we can fit in a
 * double.
 * The following probably should be autoconf'ed.
 * An IEEE double precision floating point number has a 52 bit significand,
 * which coupled with an implicit 1, means we have 53 bits to play with.
 * This means that 2^53 - 1 is the largest integer representable with all
 * its bits stored explicitly, 2^53 works (the bit that drops off the end is
 * a 0, which gets assumed), and 2^53 + 1 is the first one to really fail.
 * Of course, these integers cannot be represented on a 32-bit machine, so
 * we provide the same limit as a double, but in this case we don't know
 * whether 2^53 really was 2^53 before conversion, or whether it's 2^53 + 1
 * rounded down.
 */
#define DOUBLE_SIGNIFICANT_BITS	53
#if (SIZEOF_WORD >= SIZEOF_DOUBLE)
#define DOUBLE_INT_LIMIT	(1L << DOUBLE_SIGNIFICANT_BITS)
#endif
#define DOUBLE_INT_LIMIT_AS_DOUBLE	9007199254740992.0

/*
 * Global array sizes
 */

#define MAXARITY		255	/* Max arity of a regular predicate */
#define MAXSIMPLEARITY		8	/* Max arity of a simple predicate */
#define NARGREGS		(1/*A0*/ + MAXARITY + MAXSIMPLEARITY + 2/*SPARE*/)
#define NTYPES			13	/* Number of types */
#define ARITH_OPERATIONS	53	/* Number of arithmetic operations */


/*
 * The most common return values from C externals
 * and from resumed Eclipse executions
 */

#define PSUCCEED		0	/* success			*/
#define PFAIL			1	/* failure			*/
#define PTHROW			2	/* exit_block, ball in A1	*/
#define PYIELD			4	/* Eclipse engine suspended	*/
#define PRUNNING		5	/* Eclipse engine running	*/
#define PWAITIO			6	/* waiting for queue input	*/
#define PFLUSHIO		7	/* request queue output		*/

#define INSTANTIATION_FAULT	-4	/* variable instead of constant */
#define TYPE_ERROR		-5	/* wrong type */
#define RANGE_ERROR		-6	/* out of range */


/*
 * Initialisation
 *
 * init_flags indicates what parts of the system need to be
 * initialised (bit-significant flags):
 *
 *	INIT_SHARED	shared/saveable heap
 *	REINIT_SHARED	heap was restored, some info must be updated
 *	INIT_PRIVATE	C variables, private heap
 *	INIT_ENGINE	abstract machine
 *	INIT_PROCESS	do initialisations that are needed once
 *
 * Initialisation is done in different situations:
 *
 * raw boot		INIT_SHARED|INIT_PRIVATE|INIT_ENGINE|INIT_PROCESS
 * after -r		REINIT_SHARED|INIT_PROCESS|INIT_PRIVATE [|INIT_ENGINE]
 * after -c		INIT_PROCESS|INIT_PRIVATE
 * after restore/1	REINIT_SHARED|INIT_PRIVATE [|INIT_ENGINE]
 * after reset	0 (maybe INIT_PRIVATE)
 */

#define	INIT_SHARED	1
#define	INIT_PRIVATE	2
#define	INIT_ENGINE	4
#define	INIT_PROCESS	8
#define	REINIT_SHARED	16


