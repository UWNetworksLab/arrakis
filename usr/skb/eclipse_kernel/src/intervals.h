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


/* Codes for the various ria operations. */
/* If you change these, be sure to update ria.pl as well. */

#define	RIA_UN_SQR		0
#define	RIA_UN_SQRT		1
#define	RIA_UN_SIN		2
#define	RIA_UN_COS		3
#define	RIA_UN_EXP		4
#define	RIA_UN_LN		5
#define	RIA_UN_ATAN		6
#define	RIA_UN_PI		7
#define	RIA_UN_ABS		8
#define	RIA_UN_ROUNDOUT		10
#define	RIA_UN_NEG		11
#define	RIA_UN_WIDTH		12

#define	RIA_BIN_ADD		0
#define	RIA_BIN_SUB		1
#define	RIA_BIN_MULT		2
#define	RIA_BIN_DIV		3
#define	RIA_BIN_RSQR		4
#define	RIA_BIN_POW_INT		5
#define	RIA_BIN_RPOW_ODD	6
#define	RIA_BIN_RELAX		8
#define	RIA_BIN_MIN		9
#define	RIA_BIN_MAX		10
#define	RIA_BIN_LOGSPLIT	11
#define	RIA_BIN_PLUSMINUS	12
#define	RIA_BIN_MIN_DELTA	13
#define	RIA_BIN_LINSPLIT	14
#define	RIA_BIN_LINSPLIT_UPPER	15
#define	RIA_BIN_LOGSPLIT_UPPER	16

#define	RIA_TERN_RPOW_EVEN	0
#define	RIA_TERN_UNION		1
#define	RIA_TERN_DIV		2


#include <math.h>	/* for ieee_flags(), infinity(), nextafter() */
#if defined(_WIN32) || defined(__APPLE__)
#include <float.h>
#define MINDOUBLE DBL_MIN
#define MAXDOUBLE DBL_MAX
#else
#include <values.h>	/* for MINDOUBLE/MAXDOUBLE */
#endif

#ifndef M_PI
#define M_PI	3.14159265358979323846
#endif

/* #define HAVE_SINCOS */
/* #define HAVE_NEXTAFTER */
/* #define RIA_DEBUG */

#ifndef HAVE_SINCOS
#define sincos(x,s,c)	{ *(s) = sin(x); *(c) = cos(x); }
#else
/* not all math.h seem to define it */
void sincos(double,double*,double*);
#endif

#ifndef DLLEXP
#ifdef _WIN32
#ifndef EC_EXTERNAL
#define DLLEXP __declspec(dllexport)
#else
#define DLLEXP __declspec(dllimport)
#endif
#else	/* UNIX */
#define DLLEXP
#endif
#endif

Extern DLLEXP void ec_i_add ARGS((double,double,double,double,double*,double*));
Extern DLLEXP void ec_i_sub ARGS((double,double,double,double,double*,double*));
Extern DLLEXP void ec_i_mul ARGS((double,double,double,double,double*,double*));
Extern DLLEXP void ec_i_div ARGS((double,double,double,double,double*,double*));
