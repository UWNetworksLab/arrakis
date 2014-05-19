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
 * Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, Kish Shen and Andrew Eremin, IC-Parc
 * 
 * END LICENSE BLOCK */
/*
 * ECLiPSe / CPLEX interface
 *
 * System:	ECLiPSe Constraint Logic Programming System
 * Author/s:	Joachim Schimpf, IC-Parc
 *              Kish Shen,       IC-Parc
 * Version:	$Id: seplex.c,v 1.13 2008/10/25 03:41:10 kish_shen Exp $
 *
 */

/*#define LOG_CALLS*/
#undef LOG_CALLS
#ifdef LOG_CALLS
int log_ctr = 0;
#endif

#ifndef __STDC__
typedef unsigned size_t;
char *getenv();
#else
#include <stdlib.h>
#endif
#include <string.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

# include <stdio.h>

#ifdef WIN32
# include <process.h>
# define unlink(File) _unlink(File)
# define getpid()   _getpid()
# define gethostid()  0
# define putenv(s) _putenv(s)
# define PATH_SEPARATOR ";"
#else
# define PATH_SEPARATOR ":"
#endif


/* Kish 2008-08-13:
   Cannot define __eprintf() here for PPC Mac OS X -- at least version 10.4
   we have access to. It is defined in stdc++, and Mac OS X does not allow 
   multiple definitions of symbols during linking (for flat_namespace). 
   versions, 

   Kish 2008-07-24: define __eprintf() for all cases -- Cisco lab Solaris
   has older libraries that does not have __eprintf() defined

   code modified from koders.com's definition of __eprintf(), which uses
   fiprintf(), which is also undefined for Intel MacOSX. Here eprintf()
   is redefined to just the abort. This should be OK, as it is used in 
   assert.h, which should only be used when debugging

-- Kish Shen 2007-11-22

   This is an implementation of the __eprintf function which is
   compatible with the assert.h which is distributed with gcc.

   This function is provided because in some cases libgcc.a will not
   provide __eprintf.  This will happen if inhibit_libc is defined,
   which is done because at the time that libgcc2.c is compiled, the
   correct <stdio.h> may not be available.  newlib provides its own
   copy of assert.h, which calls __assert, not __eprintf.  However, in
   some cases you may accidentally wind up compiling with the gcc
   assert.h.  In such a case, this __eprintf will be used if there
   does not happen to be one in libgcc2.c.  */

#if !(defined(__APPLE__) && defined(__ppc__))

void
__eprintf (format, file, line, expression)
     const char *format;
     const char *file;
     unsigned int line;
     const char *expression;
{
/* (void) fiprintf (stderr, format, file, line, expression);*/
  abort ();
 }

#endif

#ifdef XPRESS
# include "xprs.h"
# define XP_PROBNAME_MAX 200  /* maximum length of problem name */
# define HAS_MIQP
/* copying a problem with zeroed quad. coeff. can lead to core dumps */
# define HAS_MIQP_FIXEDCOPYBUG 
# define HAS_INTLB_BUG /* LB lost when converting col to int type */
# if XPRESS <= 14
#  define HAS_MIQP_CALLBACKBUG /* callback to get MIQP solution core dumps */
#  define HAS_SAMEBOUNDSBUG /* sol. value = 0 if both bounds set to same */
# endif
# define SOLVER_HAS_LOCAL_PARAMETERS
# define SOLVE_MIP_COPY
# define SOLVER_SHORT_NAME XPRS
# if XPRESS >= 14      /* need to constrain integer range...*/
#  define HAS_NARROW_INT_RANGE
#  ifndef XPRS_MAXINT  /* in case we are using an old xprs.h file */
#   define XPRS_MAXINT         2147483647
#  endif
# endif
# if XPRESS >= 15
#  define HAS_POSTSOLVE
/* XPRSpostsolve is not documented and not declared in xprs.h */
int XPRS_CC XPRSpostsolve(XPRSprob prob);
# endif
#endif

#if defined(LOG_CALLS) 
# define USE_PROBLEM_ARRAY
#endif

#ifdef SOLVE_MIP_COPY

# define XP_COPYOFF     0    /* do not make copy of problem */
# define XP_COPYINVALID 1    /* copy of problem not valid */
# define XP_COPYCURRENT 2    /* copy of problem is current */
# define Mark_Copy_As_Modified(lpd)   \
      if (lpd->copystatus != XP_COPYOFF) lpd->copystatus = XP_COPYINVALID
#else

# define Mark_Copy_As_Modified(lpd) {}

#endif

#ifdef CPLEX

#ifndef CPLEXMINOR
#define CPLEXMINOR 0
#endif

#define CPX_MODERN

#include "cplex.h"		/* CPLEX declarations */

#define SOLVER_SHORT_NAME CPX

#ifndef CPXPUBLIC
#define CPXPUBLIC
#endif

# define CPX_COL_AT_LOWER   CPX_AT_LOWER
# define CPX_COL_AT_UPPER   CPX_AT_UPPER
# define CPX_COL_BASIC      CPX_BASIC
# define CPX_COL_FREE_SUPER CPX_FREE_SUPER

#define SUPPORT_IIS

#if CPLEX >= 10
/* CPLEX 10+ has more generic error for no solution state and the more general conflict set 
   rather than IIS (which is for LP only) for infeasible analyses
*/
# define CPXERR_NO_INT_SOLN CPXERR_NO_SOLN 

/* CPXcopysos() args changed in 10: removed sospri and added sosname */
# define CPXcopysos_(E,L,A1,A2,A3,A4,A5,A6) CPXcopysos(E,L,A1,A2,A3,A4,A5,A6,NULL)

# define HAS_GENERAL_CONFLICT_REFINER

# define Find_Conflict(Res, L, NRows, NCols)  Res = CPXrefineconflict(cpx_env, L, &(NRows), &(NCols))

# define Get_Conflict(L, Status, RowIdxs, RowStat, NRows_p, ColIdxs, ColStat, NCols_p) \
	CPXgetconflict(cpx_env, L, &(Status), RowIdxs, RowStat, NRows_p, ColIdxs, ColStat, NCols_p)

#else

# define CPXcopysos_(E,L,A1,A2,A3,A4,A5,A6) CPXcopysos(E,L,A1,A2,A3,NULL,A4,A5,A6)

/* mapping the calls to find a conflict set to the older and less general routines (LP only) to get 
   the IIS 
*/
# define Find_Conflict(Res, L, NRows, NCols)    Res = CPXfindiis(cpx_env, L, &(NRows), &(NCols))

# define Get_Conflict(L, Status, RowIdxs, RowStat, NRows_p, ColIdxs, ColStat, NCols_p) \
	CPXgetiis(cpx_env, L, &(Status), RowIdxs, RowStat, NRows_p, ColIdxs, ColStat, NCols_p)

# define CPX_CONFLICT_LB           CPXIIS_AT_LOWER
# define CPX_CONFLICT_UB           CPXIIS_AT_UPPER
# define CPX_STAT_CONFLICT_MINIMAL CPXIIS_COMPLETE

#endif 

#if CPLEX >= 7
# define CPX_HAS_LPOPT
#endif

#if CPLEX < 9
/* CPLEX 8 and older has no valid bestobjval for an optimal MIP solution! 
   cutoff
*/
# define NO_MIPBESTBOUND_IF_OPTIMAL
#endif

#if (CPLEX >= 8)

/* uniform treatment of solution status (independent of primal/dual) */
# define UNIFORM_SOL_STAT       
# define HAS_SIFT
# define CPX_HAS_DEFAULTALG /* has way of specifying `default' method without
			       naming method, for all LP/MIP/QP */
# define HAS_MIQP

# define SetCPXAlg(cpx_env, method)  { \
    CPXsetintparam(cpx_env, CPX_PARAM_LPMETHOD, method);\
    CPXsetintparam(cpx_env, CPX_PARAM_STARTALG, method);\
    if (method != CPX_ALG_SIFTING) \
        CPXsetintparam(cpx_env, CPX_PARAM_QPMETHOD, method);\
    else \
        CPXsetintparam(cpx_env, CPX_PARAM_QPMETHOD, CPX_ALG_AUTOMATIC);\
}

# define SetSiftAlg(cpx_env, meth) {  \
    switch (meth)  \
    { \
    case METHOD_PRIMAL: \
	CPXsetintparam(cpx_env, CPX_PARAM_SIFTALG, CPX_ALG_PRIMAL); \
	break; \
    case METHOD_DUAL: \
	CPXsetintparam(cpx_env, CPX_PARAM_SIFTALG, CPX_ALG_DUAL); \
	break; \
    case METHOD_BAR: \
	CPXsetintparam(cpx_env, CPX_PARAM_SIFTALG, CPX_ALG_BARRIER); \
	break; \
    case METHOD_NET: \
	CPXsetintparam(cpx_env, CPX_PARAM_SIFTALG, CPX_ALG_NET); \
	break; \
    default: \
	/* use error stream as warning stream unavailable */ \
	Fprintf(Current_Error, "Eplex warning: aux. method chosen for" \
		" sifting is unavailable. Using default instead\n"); \
	ec_flush(Current_Error); \
        meth = METHOD_DEFAULT; \
    case METHOD_DEFAULT: \
	CPXsetintparam(cpx_env, CPX_PARAM_SIFTALG, CPX_ALG_AUTOMATIC); \
	break; \
    } \
}

#else /* CPLX < 8 */

/* pre-CPLEX 8 names */
# define CPXPROB_MILP			  CPXPROB_MIP
# define CPXPROB_FIXEDMILP		  CPXPROB_FIXED

# define CPX_STAT_OPTIMAL                 CPX_OPTIMAL
# define CPX_STAT_INFEASIBLE              CPX_INFEASIBLE
# define CPX_STAT_OPTIMAL_INFEAS          CPX_OPTIMAL_INFEAS
# define CPX_STAT_UNBOUNDED               CPX_UNBOUNDED
# define CPX_STAT_INForUNBD               CPX_INForUNBD
# define CPX_STAT_OPTIMAL_FACE_UNBOUNDED  CPX_OPTIMAL_FACE_UNBOUNDED

# define CPXqpopt(A,B) CPXbaropt(A,B) /* no CPXqpopt() */

/* DualMethod(lpd,m,am) should be true for methods that require different
 * interpretation of the optimization result code */

# define DualMethod(lpd,m,am) \
     (((lpd)->prob_type == PROBLEM_LP || (lpd)->prob_type == PROBLEM_RELAXEDL) && \
 	((m) == METHOD_DUAL || ((m) == METHOD_NET && (am) == METHOD_DUAL)) )

# define CPX_HAS_RELAXEDLP

# if (CPLEX >= 7)  /* CPLEX 7 only */

#  define SetCPXAlg(cpx_env, method)  { \
    CPXsetintparam(cpx_env, CPX_PARAM_LPMETHOD, method);\
    CPXsetintparam(cpx_env, CPX_PARAM_STARTALG, method);\
}

# elif (CPLEX >= 6)  /* CPLEX 6 only */

#  define CPX_ALG_NET                     CPX_NODEALG_HYBNETOPT

#  define HAS_NO_BARCROSSOVER  /* does not have `no crossover' for barrier */

# define SetCPXAlg(cpx_env, method)  { \
    CPXsetintparam(cpx_env, CPX_PARAM_STARTALG, method);\
}

# endif

#endif /* CPLEX >= 8 */


#define SuccessState(d) ( \
	(d)->sol_state == CPX_STAT_OPTIMAL || \
	(d)->sol_state == CPX_STAT_OPTIMAL_INFEAS || \
        (d)->sol_state == CPX_STAT_OPTIMAL_FACE_UNBOUNDED || \
        MIPSuccessState(d))

#define MIPSuccessState(d) ( \
	(d)->sol_state == CPXMIP_OPTIMAL || \
	(d)->sol_state == CPXMIP_OPTIMAL_TOL || \
	(d)->sol_state == CPXMIP_OPTIMAL_INFEAS)

#define FailState(d) ( \
	(d)->sol_state == CPX_STAT_INFEASIBLE || \
	(d)->sol_state == CPXMIP_INFEASIBLE)

#if CPLEX >= 8
#define UnboundedState(d) ( \
	(d)->sol_state == CPX_STAT_UNBOUNDED || \
        (d)->sol_state == CPXMIP_UNBOUNDED)

#define MaybeFailState(d) ( \
	(d)->sol_state == CPX_STAT_INForUNBD || \
        (d)->sol_state == CPXMIP_INForUNBD || \
	(d)->sol_state == CPXERR_PRESLV_INForUNBD )

#define LPAbortedState(d) ( \
	(d)->sol_state == CPX_STAT_ABORT_IT_LIM || \
	(d)->sol_state == CPX_STAT_ABORT_TIME_LIM || \
	(d)->sol_state == CPX_STAT_ABORT_OBJ_LIM || \
	(d)->sol_state == CPX_STAT_ABORT_USER || \
        (d)->sol_state == CPX_STAT_NUM_BEST || \
	(d)->sol_state == CPX_STAT_ABORT_PRIM_OBJ_LIM || \
	(d)->sol_state == CPX_STAT_ABORT_DUAL_OBJ_LIM )

#else

/* aborted with no feasible (primal/dual) solution */
#define LPAbortedNoSolState(d) ( \
	(d)->sol_state == CPX_IT_LIM_INFEAS || \
	(d)->sol_state == CPX_TIME_LIM_INFEAS || \
	(d)->sol_state == CPX_NUM_BEST_INFEAS || \
	(d)->sol_state == CPX_ABORT_INFEAS  )

/* aborted with feasible solution */
#define LPAbortedSolState(d) ( \
	(d)->sol_state == CPX_OBJ_LIM || \
	(d)->sol_state == CPX_IT_LIM_FEAS || \
	(d)->sol_state == CPX_TIME_LIM_FEAS || \
	(d)->sol_state == CPX_NUM_BEST_FEAS || \
	(d)->sol_state == CPX_NUM_BEST_PRIM_DUAL_FEAS || \
	(d)->sol_state == CPX_ABORT_FEAS )

#define LPAbortedState(d) ( \
        LPAbortedSolState(d) || LPAbortedNoSolState(d) )

#define UnboundedState(d) ( \
	(d)->sol_state == CPX_STAT_UNBOUNDED )

#if CPLEX > 6 || ( CPLEX == 6 && CPLEXMINOR > 0 )
#define MaybeFailState(d) ( \
	(d)->sol_state == CPX_STAT_INForUNBD || \
	(d)->sol_state == CPXERR_PRESLV_INForUNBD )
#else
#define MaybeFailState(d) 0
#endif

#endif

#if CPLEX >= 7
# define MIPSemiFailState(d) ( \
	(d)->sol_state == CPXMIP_NODE_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_TIME_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_FAIL_INFEAS || \
	(d)->sol_state == CPXMIP_MEM_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_ABORT_INFEAS || \
	(d)->sol_state == CPXMIP_FAIL_INFEAS_NO_TREE)

# define MIPSemiSuccessState(d) ( \
	(d)->sol_state == CPXMIP_SOL_LIM || \
	(d)->sol_state == CPXMIP_NODE_LIM_FEAS || \
	(d)->sol_state == CPXMIP_TIME_LIM_FEAS || \
	(d)->sol_state == CPXMIP_FAIL_FEAS || \
	(d)->sol_state == CPXMIP_MEM_LIM_FEAS || \
	(d)->sol_state == CPXMIP_ABORT_FEAS || \
	(d)->sol_state == CPXMIP_FAIL_FEAS_NO_TREE)
#else
# define MIPSemiFailState(d) ( \
	(d)->sol_state == CPXMIP_NODE_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_TIME_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_FAIL_INFEAS || \
	(d)->sol_state == CPXMIP_MEM_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_ABORT_INFEAS || \
	(d)->sol_state == CPXMIP_FAIL_INFEAS_NO_TREE || \
	(d)->sol_state == CPXMIP_NODE_FILE_LIM_INFEAS)

# define MIPSemiSuccessState(d) ( \
	(d)->sol_state == CPXMIP_SOL_LIM || \
	(d)->sol_state == CPXMIP_NODE_LIM_FEAS || \
	(d)->sol_state == CPXMIP_TIME_LIM_FEAS || \
	(d)->sol_state == CPXMIP_FAIL_FEAS || \
	(d)->sol_state == CPXMIP_MEM_LIM_FEAS || \
	(d)->sol_state == CPXMIP_ABORT_FEAS || \
	(d)->sol_state == CPXMIP_FAIL_FEAS_NO_TREE || \
	(d)->sol_state == CPXMIP_NODE_FILE_LIM_FEAS)
#endif


#define SetPreSolve(state) \
{\
   Log1(CPXsetintparam(cpx_env, CPX_PARAM_PREIND, %d), state); \
   CPXsetintparam(cpx_env, CPX_PARAM_PREIND, state); \
}

#define Get_Feasibility_Tolerance(E,L,T) CPXgetdblparam(E,CPX_PARAM_EPRHS,T)

#define Get_Int_Param(E,L,A1,A2) 	CPXgetintparam(E,A1,A2)
#define Get_Dbl_Param(E,L,A1,A2)	CPXgetdblparam(E,A1,A2)
#define Set_Int_Param(E,L,A1,A2) 	CPXsetintparam(E,A1,A2)
#define Set_Dbl_Param(E,L,A1,A2)	CPXsetdblparam(E,A1,A2)

# define Get_LP_Objval(A1,A2)		CPXgetobjval(cpx_env,(A1)->lp,A2)
# define Get_Best_Objbound(A1, A2)      CPXgetbestobjval(cpx_env,A1,A2)

#define Get_MIPCutOff(d, v) \
       ((d)->sense == SENSE_MIN ? CPXgetdblparam(cpx_env, CPX_PARAM_CUTUP, v) : CPXgetdblparam(cpx_env, CPX_PARAM_CUTLO, v))

#define UsingBarrierNoCrossOver(d) (CPXgetmethod(cpx_env, (d)) == CPX_ALG_BARRIER)
#define Get_Bar_Primal_Obj(d, obj) CPXgetdblquality(cpx_env, (d), obj, CPX_PRIMAL_OBJ)
#define Get_Bar_Dual_Obj(d, obj) CPXgetdblquality(cpx_env, (d), obj, CPX_DUAL_OBJ)

#define HAS_QUADRATIC
#define SOLVER_MAT_BASE   0
#define SOLVER_MAT_OFFSET 1

#endif	/* CPLEX */

#ifdef XPRESS

# ifndef XPRESSMINOR
#  define XPRESSMINOR 0
# endif

# ifdef __STDC__
#  define __ANSIC_	/* used in xpresso.h */
# endif

# define XP_GLSTAT_OFFSET 0x10


# define CPX_INFBOUND			XPRS_PLUSINFINITY
# define CPX_AT_LOWER                   0
# define CPX_BASIC                      1
# define CPX_AT_UPPER                   2
# define CPX_FREE_SUPER                 0
# define CPX_COL_AT_LOWER               CPX_AT_LOWER
# define CPX_COL_AT_UPPER               CPX_AT_UPPER
# define CPX_COL_BASIC                  CPX_BASIC
/* XPRESS has no separate code for super-basic variables */
# define CPX_COL_FREE_SUPER             CPX_FREE_SUPER
# define CPXgetrhs(E,A1,A2,A3,A4)	XPRSgetrhs(A1,A2,A3,A4)
# define CPXgetsense(E,A1,A2,A3,A4)	XPRSgetrowtype(A1,A2,A3,A4)
# define CPXgetlb(E,A1,A2,A3,A4)	XPRSgetlb(A1,A2,A3,A4)
# define CPXgetub(E,A1,A2,A3,A4)	XPRSgetub(A1,A2,A3,A4)
# define CPXgetctype(E,A1,A2,A3,A4)	XPRSgetcoltype(A1,A2,A3,A4)
# define CPXchgctype(E,A1,A2,A3,A4)	XPRSchgcoltype(A1,A2,A3,A4)
# define CPXchgbds(E,A1,A2,A3,A4,A5)	XPRSchgbounds(A1,A2,A3,A4,A5)
# define CPXcopybase(E,A1,A2,A3)	XPRSloadbasis(A1,A3,A2) /* args swapped! */
# define CPXgetbase(E,A1,A2,A3)		XPRSgetbasis(A1,A3,A2) /* args swapped! */
# define Get_LP_Objval(A1,A2)		XPRSgetdblattrib((A1)->lpcopy,XPRS_LPOBJVAL,A2)
# define Get_Best_Objbound(A1, A2)      XPRSgetdblattrib(A1,XPRS_BESTBOUND,A2)
# define CPXgetmipobjval(E,A1,A2)	XPRSgetdblattrib(A1,XPRS_MIPOBJVAL,A2)
# define CPXgetobj(E,A1,A2,A3,A4)	XPRSgetobj(A1,A2,A3,A4)
# define CPXchgobj(E,A1,A2,A3,A4)	XPRSchgobj(A1,A2,A3,A4)
# define CPXcopyorder(E,A1,A2,A3,A4,A5)	XPRSloaddirs(A1,A2,A3,A4,A5,NULL,NULL)
# define CPXchgqpcoef(E,A1,A2,A3,A4)	XPRSchgqobj(A1,A2,A3,A4)
# define CPXchgrhs(E,A1,A2,A3,A4)       XPRSchgrhs(A1,A2,A3,A4)
# define CPXcloseCPLEX(E)		XPRSfree()
# define CPXaddcols(E,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) \
          XPRSaddcols(A1,A2,A3,A4,A5,A6,A7,A8,A9) /* diff args! */
# define CPXaddrows(E,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11) \
          XPRSaddrows(A1,A3,A4,A6,A5,NULL,A7,A8,A9) /* diff args! */
# define CPXgetrows(E,A1,A2,A3,A4,A5,A6,A7,A8,A9) \
          XPRSgetrows(A1,A3,A4,A5,A6,A2,A8,A9) /* diff args! */
# define CPXchgobjsen(E,A1,A2) 		
# define CPXchgprobtype(A1, A2, A3)     0 /* 0 for success return code */

# define CPXLPptr			XPRSprob /* prob. pointer 13+ only! */

# define SUPPORT_IIS
# define Find_Conflict(Err, L, NRows, NCols) { \
	Err = XPRSiis(L, ""); \
        if (!Err) Err = XPRSgetiis(L, &(NCols), &(NRows), NULL, NULL); \
}

# define Get_Conflict(L, Status, RowIdxs, RowStat, Nrows_p, ColIdxs, ColStat, Ncols_p)  \
	Status = XPRSgetiis(L, Ncols_p, Nrows_p,  ColIdxs, RowIdxs)

# define Get_Feasibility_Tolerance(E,L,T) XPRSgetdblcontrol((L)->lp, XPRS_FEASTOL, T)

# define Get_Int_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSgetintcontrol(E,A1,A2) : XPRSgetintcontrol((L)->lp,A1,A2))
# define Get_Dbl_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSgetdblcontrol(E,A1,A2) : XPRSgetdblcontrol((L)->lp,A1,A2))
# define Set_Int_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSsetintcontrol(E,A1,A2) : XPRSsetintcontrol((L)->lp,A1,A2))
# define Set_Dbl_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSsetdblcontrol(E,A1,A2) : XPRSsetdblcontrol((L)->lp,A1,A2))
# define Set_Str_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSsetstrcontrol(E,A1,A2) : XPRSsetstrcontrol((L)->lp,A1,A2))
# define Get_MIPCutOff(d, v) XPRSgetdblcontrol((d)->lpcopy, XPRS_MIPABSCUTOFF, v)
# define Get_Dual_Infeas(lp, v) XPRSgetintattrib(lp, XPRS_DUALINFEAS, v)
# define Get_Primal_Infeas(lp, v) XPRSgetintattrib(lp, XPRS_PRIMALINFEAS, v)

# define SetPreSolve(state) 

#ifdef XPRESS_OEM_ICPARC_2002
# include "xprsoem.h"
#endif


# define Get_Xp_Stat(lpd) \
	if (IsMIPProb(lpd->prob_type)) { \
	    (void) XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &lpd->sol_state); \
	    if (lpd->sol_state == XPRS_MIP_LP_NOT_OPTIMAL || \
               lpd->sol_state == XPRS_MIP_LP_OPTIMAL) \
		(void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
	    else \
		lpd->sol_state += XP_GLSTAT_OFFSET; \
	} else if (lpd->prob_type == PROBLEM_FIXEDL || lpd->prob_type == PROBLEM_FIXEDQ) { \
        /* fixglobal only performed if MIP was optimal */ \
            (void) XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &lpd->sol_state); \
            if (lpd->sol_state == XPRS_MIP_OPTIMAL) \
               (void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
            else \
            { \
	       if (lpd->sol_state == XPRS_MIP_LP_NOT_OPTIMAL || \
                  lpd->sol_state == XPRS_MIP_LP_OPTIMAL) \
		  (void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
	       else \
		  lpd->sol_state += XP_GLSTAT_OFFSET; \
            } \
        } else if (lpd->prob_type == PROBLEM_RELAXEDL || lpd->prob_type == PROBLEM_RELAXEDQ) \
        { \
	    (void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
        } \
	else \
        { \
	    (void) XPRSgetintattrib(lpd->lp, XPRS_LPSTATUS, &lpd->sol_state); \
	}



# define SuccessState(d)	( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_OPTIMAL || \
	(d)->sol_state == XPRS_LP_OPTIMAL )
/* the LP_CUTOFF* LPSTATUS happens only with MIP search, and as we access
   LPSTATUS for MIP search only if the MIP search is stopped at the root,
   we know that these states means that a cutoff occurred at the root node,
   and that the MIP optimal solution cannot be better than the cutoff, and
   this is considered to be a failure state
*/
# define FailState(d) ( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_INFEAS || \
	(d)->sol_state == XPRS_LP_INFEAS || \
	(d)->sol_state == XPRS_LP_CUTOFF || \
	(d)->sol_state == XPRS_LP_CUTOFF_IN_DUAL)
/* catches the MIP cases only */
# define MIPSemiFailState(d) ( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_NO_SOL_FOUND)
/* catches the MIP cases only */
# define MIPSemiSuccessState(d) ( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_SOLUTION)
/* An aborted LP can be either semi-fail or semi-success */
# define LPAbortedState(d) ( \
        (d)->sol_state == XPRS_LP_UNFINISHED )
# define MaybeFailState(d) (0)
# define UnboundedState(d) ( \
	(d)->sol_state == XPRS_LP_UNBOUNDED )

# define DualMethod(lpd,m,am) 0

# define UsingBarrierNoCrossOver(d) (meth == METHOD_BAR && auxmeth == METHOD_NONE) 
# define Get_Bar_Primal_Obj(d, obj) XPRSgetdblattrib((d), XPRS_BARPRIMALOBJ, obj)
# define Get_Bar_Dual_Obj(d, obj) XPRSgetdblattrib((d), XPRS_BARDUALOBJ, obj)
# define Bar_Is_Primal_Feasible(lpd) \
	(XPRSgetdblattrib(lpd->lp, XPRS_BARPRIMALINF, &infeas), infeas < 1e-6)
# define Bar_Is_Dual_Feasible(lpd) \
	(XPRSgetdblattrib(lpd->lp, XPRS_BARDUALINF, &infeas), infeas < 1e-6)


# define HAS_QUADRATIC

# define SOLVER_MAT_BASE   0
# define SOLVER_MAT_OFFSET 1

#endif	/* XPRESS */

#ifdef COIN /* COIN based solvers */

/*  these void are for void pointers, as the type is COIN solver specific
    and defined in coinplex.cpp only
*/
# define OsiXxxSolverInterface 		void
# define COINprob 			void

# define CPXgetobjsen(E,A1)		coin_get_objsen(A1)
# define CPXgetnumcols(E,A1)		coin_get_numcols(A1)
# define CPXgetnumrows(E,A1)		coin_get_numrows(A1)
# define CPXgetprobtype(E,A1)		coin_get_probtype(A1)
# define CPXgetrhs(E,A1,A2,A3,A4)	coin_getrhs(A1,A2,A3,A4)
# define CPXgetsense(E,A1,A2,A3,A4)	coin_getrowsense(A1,A2,A3,A4)
# define CPXgetlb(E,A1,A2,A3,A4)	coin_getlb(A1,A2,A3,A4)
# define CPXgetub(E,A1,A2,A3,A4)	coin_getub(A1,A2,A3,A4)
# define CPXgetctype(E,A1,A2,A3,A4)	coin_getcoltype(A1,A2,A3,A4)
# define CPXchgctype(E,A1,A2,A3,A4)	coin_chgcoltype(A1,A2,A3,A4)
# define CPXchgbds(E,A1,A2,A3,A4,A5)	coin_chgbds(A1,A2,A3,A4,A5)
# define CPXcopybase(E,A1,A2,A3)	coin_loadbasis(A1,A2,A3) 
# define CPXgetbase(E,A1,A2,A3)		coin_getbasis(A1,A2,A3) 
# define Get_LP_Objval(A1,A2)		coin_get_lpobjval(A1,A2)
# define Get_Best_Objbound(A1, A2)      coin_get_bestmipbound(A1,A2)
# define CPXgetmipobjval(E,A1,A2)	coin_get_mipobjval(A1,A2)
# define CPXgetobj(E,A1,A2,A3,A4)	coin_get_objcoeffs(A1,A2,A3,A4)
# define CPXchgobj(E,A1,A2,A3,A4)	coin_chg_objcoeffs(A1,A2,A3,A4)
# define CPXcopyorder(E,A1,A2,A3,A4,A5)	coin_get_order(A1,A2,A3,A4,A5,NULL,NULL)
# define CPXchgqpcoef(E,A1,A2,A3,A4)	coin_chgqobj(A1,A2,A3,A4)
# define CPXchgrhs(E,A1,A2,A3,A4)       coin_chgrhs(A1,A2,A3,A4)
# define CPXcopylp(E,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14) \
          coin_loadprob(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13)
# define CPXcopyctype(E,A1,A2)          coin_setcoltype(A1,A2)
# define CPXaddcols(E,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) \
          coin_addcols(A1,A2,A3,A4,A5,A6,A7,A8,A9) /* diff args! */
# define CPXaddrows(E,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11) \
          coin_addrows(A1,A3,A4,A5,A6,A7,A8,A9) /* diff args! */
# define CPXchgobjsen(E,A1,A2) coin_chgobjsen(A1,A2)
# define CPXchgprobtype(A1, A2, A3)     0 /* 0 for success return code */
# define CPXcopysos_(E,A1,A2,A3,A4,A5,A6,A7) -1 /* -1 for error */
# define CPXgetrows(E,A1,A2,A3,A4,A5,A6,A7,A8,A9) \
          coin_get_row(A1,A2,A4,A5,A8) /* gets one row only! */
# define CPXwriteprob(E,A1,A2,A3) coin_writeprob(A1,A2,A3)
# define CPXreadcopyprob(E,A1,A2,A3) coin_readprob(A1,A2,A3)
# define CPXgetnumnz(E,A1) coin_getnumnz(A1)
# define CPXgetnumint(E,A1) coin_getnumint(A1) /* include binaries */
# define CPXchgname(E,A1,A2,A3,A4) coin_set_name(A1,A2,A3,A4)
# define CPXgetnumbin(E,A1) 0
# define CPXgetnumqpnz(E,A1) 0 /* for now */
# define CPXpreslvwrite(E,A1,A2,A3) -1 
# define CPXmbasewrite(E,A1,A2) -1
# define CPXtreewrite(E,A1,A2) -1
# define CPXembwrite(E,A1,A2) -1
# define XPRSdelrows(A1,A2,A3) coin_delrows(A1,A2,A3)
# define XPRSdelcols(A1,A2,A3) coin_delcols(A1,A2,A3)

# define UsingBarrierNoCrossOver(d) 0
# define Get_Bar_Primal_Obj(d, obj) coin_get_bar_primal_objval(d, obj)
# define Get_Bar_Dual_Obj(d, obj) coin_get_bar_dual_objval(d, obj)
# define Bar_Is_Primal_Feasible(lpd) coin_bar_is_primal_feas(lpd->lp)
# define Bar_Is_Dual_Feasible(d) coin_bar_is_dual_feas(lpd->lp)

# define SuccessState(d) (coin_get_result_state(d) == state_success)
# define FailState(d) (coin_get_result_state(d) == state_fail)
# define MIPSemiSuccessState(d) (coin_get_result_state(d) == state_mipsemisucc)
# define MIPSemiFailState(d) (coin_get_result_state(d) == state_mipsemifail)
# define LPAbortedState(d) (coin_get_result_state(d) == state_lpaborted)
# define UnboundedState(d) (coin_get_result_state(d) == state_unbounded)
# define MaybeFailState(d) (coin_get_result_state(d) == state_unknown)

# define Get_MIPCutOff(d, v) coin_get_mipcutoff(d, v)
# define DualMethod(lpd,m,am) 0

# define Get_Dual_Infeas(lp, v) coin_get_dual_infeas(lp, v)
# define Get_Primal_Infeas(lp, v) coin_get_primal_infeas(lp, v)

# define SetPreSolve(state)

# define Get_Feasibility_Tolerance(E,L,T) coin_getdblparam((L)->lp, OsiPrimalTolerance, T)

# define Get_Int_Param(E,L,A1,A2) \
   coin_getintparam(((L) == NULL ? E : (L)->lp),A1,A2)
# define Get_Dbl_Param(E,L,A1,A2) \
   coin_getdblparam(((L) == NULL ? E : (L)->lp),A1,A2)
# define Set_Int_Param(E,L,A1,A2) \
   coin_setintparam(((L) == NULL ? E : (L)->lp),A1,A2)
# define Set_Dbl_Param(E,L,A1,A2) \
   coin_setdblparam(((L) == NULL ? E : (L)->lp),A1,A2)

/*
# define Set_Str_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSsetstrcontrol(E,A1,A2) : XPRSsetstrcontrol((L)->lp,A1,A2))
*/

# define SOLVER_SHORT_NAME OSI

# define NONLICENCED /* not license based */
# define SOLVER_MAT_BASE 0
# define SOLVER_MAT_OFFSET 1

#define HAS_QUADRATIC /* CLP has quadratic */

# define CPX_AT_LOWER                   3
# define CPX_BASIC                      1
# define CPX_AT_UPPER                   2
# define CPX_FREE_SUPER                 0
# define CPX_COL_AT_LOWER               CPX_AT_LOWER
# define CPX_COL_AT_UPPER               CPX_AT_UPPER
# define CPX_COL_BASIC                  CPX_BASIC
/* COIN has no separate code for super-basic variables */
# define CPX_COL_FREE_SUPER             CPX_FREE_SUPER
# define CPX_INFBOUND coin_infinity(cpx_env) /* use the default cpx_env */

# define CPXPROB_MILP	PROBLEM_MIP
# define CPXPROB_QP	PROBLEM_QP
# define CPXPROB_MIQP	PROBLEM_MIQP 
# define CPXPROB_LP	PROBLEM_LP

# define CPX_MIN	SENSE_MIN
# define CPX_MAX	SENSE_MAX

#define SOLVER_HAS_LOCAL_PARAMETERS

#endif /* COIN */

#ifndef HUGE_VAL
# include <math.h>
#endif

/*
 * Macros to make code more readable 
 */

/* this extra step is needed to allow Call itself to be transformed */
#define Transform_Quoted(Item) Add_Quotes(Item)
#define Add_Quotes(Item) #Item

/* Call logging macros:
     Call(Ret, Call)   Log and call Call if LOG_CALLS, assign return value to Ret 
     CallN(Call)       Log and call Call if LOG_CALLS, return value is lost

Log only macros (these should be accompanied by an actual call to the
logged call!)

     Log1(Call, A1)...Log6(Call, A1,A2,A3,A4,A5,A6) 
                       Log Call if LOG_CALLS. Call should be in printf
                       form, with appropriate % arguments for the arguments
*/   
#ifdef LOG_CALLS 
# define Call(Err, C) { \
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";");\
    ec_flush(log_output_); \
    Err = C;\
}

# define CallN(C) { \
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";");\
    ec_flush(log_output_); \
    C;\
}

# define Log0(C) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";");\
    ec_flush(log_output_);\
}

# define Log1(C,A1) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1);\
    ec_flush(log_output_);\
}

# define Log2(C,A1,A2) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1,A2);\
    ec_flush(log_output_);\
}

# define Log3(C,A1,A2,A3) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1,A2,A3);\
    ec_flush(log_output_);\
}

# define Log4(C,A1,A2,A3,A4) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1,A2,A3,A4); \
    ec_flush(log_output_); \
}

# define Log5(C,A1,A2,A3,A4,A5) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1,A2,A3,A4,A5); \
    ec_flush(log_output_); \
}

# define Log6(C,A1,A2,A3,A4,A5,A6) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1,A2,A3,A4,A5,A6); \
    ec_flush(log_output_); \
}

#else
# define Call(Err, C) {Err = C;}
# define CallN(C) C
# define Log0(C)
# define Log1(C,A1)
# define Log2(C,A1,A2)
# define Log3(C,A1,A2,A3)
# define Log4(C,A1,A2,A3,A4)
# define Log5(C,A1,A2,A3,A4,A5)
# define Log6(C,A1,A2,A3,A4,A5,A6)

#endif

/*
 * ECLiPSe declarations
 */

#include "external.h"

#ifdef WIN32
/* must be after include of external.h to avoid redefining log_output_ there */
# define log_output_ ec_stream_id(ec_stream_nr(log_output_))
#endif

/* should be used only if v,t is a number */
#define DoubleVal(v, t) ( IsInteger(t) ? (double) (v).nint : \
			  IsDouble(t) ? Dbl(v) : coerce_to_double(v,t) )

#define Check_Constant_Range(x) \
	{if ((x) < -CPX_INFBOUND || (x) > CPX_INFBOUND) {Bip_Error(RANGE_ERROR);}}


/*
 * LpDescOnly can be used if we only want to access
 * fields within lp_desc, not in the external solver
 * this made a difference in old Xpress, but now there
 * is a difference only when logging calls
 */
#define LpDescOnly(vlp, tlp, lpd) Get_Typed_Object(vlp, tlp, &lp_handle_tid, lpd)

#ifdef LOG_CALLS
static int next_matno = 0, current_matno = -1;

# define LpDesc(vlp, tlp, lpd) {                        \
    Get_Typed_Object(vlp, tlp, &lp_handle_tid, (lpd));  \
    if ((lpd)->matno != current_matno)                  \
    {                                                   \
	Fprintf(log_output_, "\n\
	lpd = (lp_desc *) lpdmat[%d];", (lpd)->matno);  \
	ec_flush(log_output_);                          \
	current_matno = (lpd)->matno;                   \
    }                                                   \
}

#else
# define LpDesc(vlp, tlp, lpd) LpDescOnly(vlp, tlp, lpd)
#endif


#define Check_Array(t) Check_String(t)
#define IArrayStart(pw) ((int *)BufferStart(pw))
#define DArrayStart(pw) ((double *)BufferStart(pw))
#define CArrayStart(pw) ((char *)BufferStart(pw))
#define Return_Unify_Array(v,t,a) Return_Unify_String(v,t,a)
#define DArraySize(pbuf) ((BufferSize(pbuf) - 1) / sizeof(double))
#define IArraySize(pbuf) ((BufferSize(pbuf) - 1) / sizeof(int))

static pword * _create_carray();
static pword * _create_darray();
static pword * _create_iarray();


/*
 * Constants
 */

#define HANDLE_CPH	1	/* argument indices in Prolog prob-handle */
#define HANDLE_STAMP	2	/* timestamp for prob-handle */

#define COL_STAMP       1       /* timestamp for a column (in attribute) */


#define DESCR_EMPTY		0	/* problem descriptor state */
#define DESCR_LOADED		1
#define DESCR_SOLVED_SOL	2
#define DESCR_SOLVED_NOSOL	3
#define DESCR_ABORTED_SOL	4
#define DESCR_ABORTED_NOSOL	5
#define DESCR_UNBOUNDED_NOSOL	6
#define DESCR_UNKNOWN_NOSOL	7

#define CSTR_TYPE_NORM          0 /* correspond to constraint_type_code/2 */
#define CSTR_TYPE_PERMCP        1
#define CSTR_TYPE_CONDCP        2

#define CSTR_STATE_NOTADDED    -1
#define CSTR_STATE_VIOLATED    -1
#define CSTR_STATE_SAT         -2
#define CSTR_STATE_BINDING     -3
#define CSTR_STATE_INVALID     -4
#define CSTR_STATE_INACTIVE    -5


#define CP_ACTIVE       1       /* correspond to cp_cond_code/2  */
#define CP_ADDINIT      2

#define NEWROW_INCR	60	/* default sizes for addrow arrays */
#define NEWNZ_INCR	510
#define NEWBD_INCR	510	/* arrays needed for changing bounds */
#define NEWCOL_INCR	1022	/* macsz arrays growth increment */
#define NEWSOS_INCR	32
#define CUTPOOL_INCR    10      /* number of cutpools increment */

#define RoundTo(n,unit) ((n) - ((n) - 1) % (unit) -1 + (unit))
#define Max(x,y)	((x)>(y)?(x):(y))

/* minimum number of words that Type would fit in */
#define NumberOfWords(Type) (1+(sizeof(Type)-1)/sizeof(word))

#include "seplex.h"

/* 
 * Problem handle
 */

/* Methods for lp_handle_tid */
static void _free_lp_handle(lp_desc *lpd);
static int _strsz_lp_handle(lp_desc *lpd, int quoted);
static int _tostr_lp_handle(lp_desc *lpd, char *buf, int quoted);

/* 2*sizeof(void *) for max. size for a printed address */
#define STRSZ_LP_HANDLE 2*sizeof(void *)+20
 
static int
_strsz_lp_handle(lp_desc *lpd, int quoted)
{
    return STRSZ_LP_HANDLE;
}

static int
_tostr_lp_handle(lp_desc *lpd, char *buf, int quoted)
{
    sprintf(buf, "'EPLEX_"Transform_Quoted(SOLVER_SHORT_NAME)"'(16'%x)", (word) lpd);
    return strlen(buf); /* size of actual string */
}

t_ext_type lp_handle_tid = {
    (void (*)(t_ext_ptr)) _free_lp_handle,  /* free */
    NULL,  /* copy */
    NULL,  /* mark_dids */
    (int (*)(t_ext_ptr,int)) _strsz_lp_handle,  /* string_size */
    (int (*)(t_ext_ptr,char *,int)) _tostr_lp_handle,  /* to_string */
    NULL,  /* equal */
    NULL,  /* remote_copy */
    NULL,  /* get */
    NULL   /* set */
};


typedef struct {
  int              oldmar, oldmac;
} untrail_data;
      
typedef struct {
  int              idx;
  char             ctype;
} untrail_ctype;

typedef struct {
    double bds[2];     /* bounds: lower, upper */
    int    idx;       /* index of column */
} untrail_bound;

typedef struct {
    int old_ptype;   /* old problem type */
} untrail_ptype;

/*
 * Global data
 */

struct param_desc {
    char *	name;
    int	 	num;
    int		type;
};


#include "eplex_params.h"


#ifdef CPLEX

static CPXENVptr	cpx_env = (CPXENVptr) 0; /* != 0 when initialized */
static CPXCHANNELptr	cpxresults = (CPXCHANNELptr) 0;
static CPXCHANNELptr	cpxwarning = (CPXCHANNELptr) 0;
static CPXCHANNELptr	cpxerror = (CPXCHANNELptr) 0;
static CPXCHANNELptr	cpxlog = (CPXCHANNELptr) 0;

# if CPLEX >= 8
static void CPXPUBLIC eclipse_out ARGS((void *nst, const char*msg));
# else
static void CPXPUBLIC eclipse_out ARGS((void *nst, char*msg));
# endif
#endif /* CPLEX */


#ifdef XPRESS

struct lp_sol {
    double *sols;
    double *slacks;
    double *pis;
    double *djs;
    int    *cbase;
    int    *rbase;
    int    mac;
};
 


/* Type of XPRESS library */
#define XP_OEM_UNKNOWN	-1
#define XP_OEM_NO	0
#define XP_OEM_YES	1
static int oem_xpress = XP_OEM_UNKNOWN;

static stream_id solver_streams[4];

static XPRSprob         cpx_env = 0;  /* != 0 when initialized */

static void XPRS_CC eclipse_out ARGS((XPRSprob prob, void *obj, const char *msg, int len, int msgtype));


# ifdef LOG_CALLS
extern stream_id log_output_;
# endif

#endif	/* XPRESS */

#ifdef COIN

static stream_id solver_streams[4];

static COINprob * cpx_env = NULL;

#endif

static double 
coerce_to_double(value vval, type tval)
{
    /* tval MUST be a number type */
    value buffer;

    tag_desc[TagType(tval)].coerce_to[TDBL](vval, &buffer);
    return Dbl(buffer);
}


static void _grow_cb_arrays ARGS((lp_desc *, int));


#if 0
void *
Malloc(size_t size)
{
    void *p;
    p = malloc(size);
    Fprintf(Current_Error, "%8x malloc(%d)\n", p, size);
    ec_flush(Current_Error);
    return p;
}

void *
Realloc(void *p, size_t size)
{
    Fprintf(Current_Error, "%8x realloc(%d)\n", p, size);
    ec_flush(Current_Error);
    return realloc(p, size);
}

void
Free(void *p)
{
    Fprintf(Current_Error, "%8x free\n", p);
    ec_flush(Current_Error);
    free(p);
}
#else
#if 1
#define Malloc(size) malloc(size)
#define Realloc(p, size) realloc(p, size)
#define Free(p) free(p)
#else
/* Eclipse's hp_alloc() can cause problems because of private heap limit */
#define Malloc(size) hp_alloc(size)
#define Realloc(p, size) hp_resize(p, size)
#define Free(p) hp_free(p)
#endif
#endif

/* free *p if it is pointing at something */
#define TryFree(p)  if (p) { CallN(Free(p)); p = NULL; } 

int
p_cpx_cleanup(value vlp, type tlp)
{
    pword handle;
    handle.val.all = vlp.all;
    handle.tag.all = tlp.all;
    return ec_free_handle(handle, &lp_handle_tid);
}

static void
_free_lp_handle(lp_desc *lpd)
{
    int i;
#ifdef XPRESS
    char name[128];
#endif
    if (lpd->descr_state != DESCR_EMPTY)
    {
#ifdef CPLEX
	if (lpd->lp)
	    CallN(CPXfreeprob(cpx_env, &lpd->lp));
#endif
#ifdef XPRESS
	strcpy(name, lpd->probname);
	strcat(name, ".glb"); 
	unlink(name);
	if (lpd->lp) CallN(XPRSdestroyprob(lpd->lp));
	if (lpd->lpcopy && lpd->lpcopy != lpd->lp) CallN(XPRSdestroyprob(lpd->lpcopy));
	Mark_Copy_As_Modified(lpd);
    	TryFree(lpd->qgtype);
    	TryFree(lpd->mgcols);
        TryFree(lpd->probname);
#endif /* XPRESS */
#ifdef COIN
	CallN(coin_free_prob(lpd->lp));
	/* lpd->lp allocated with new, and freed with delete by coin_free_prob()
	   so no need to free here
	*/
	lpd->lp = NULL;
#endif /* COIN */

	TryFree(lpd->rhsx);
	TryFree(lpd->senx);
	TryFree(lpd->matbeg);
	TryFree(lpd->matcnt);
	TryFree(lpd->matind);
	TryFree(lpd->matval);
	TryFree(lpd->bdl);
	TryFree(lpd->bdu);
	TryFree(lpd->objx);
	TryFree(lpd->ctype);
	if (lpd->cb_sz)
	{
	    CallN(Free(lpd->cb_index));
	    TryFree(lpd->cb_index2);
	    CallN(Free(lpd->cb_value));
	}
	if (lpd->nsos)
	{
	    CallN(Free(lpd->sostype));
	    CallN(Free(lpd->sosbeg));
	    CallN(Free(lpd->sosind));
	    CallN(Free(lpd->sosref));
	}
	TryFree(lpd->rngval);
	TryFree(lpd->cname);
	TryFree(lpd->cstore);
	TryFree(lpd->rname);
	TryFree(lpd->rstore);
	TryFree(lpd->numbers);
	TryFree(lpd->zeroes);
	TryFree(lpd->dzeroes);
	if (lpd->nr_sz)
	{
	    CallN(Free(lpd->rmatbeg));
	}
	if (lpd->nnz_sz)
	{
	    CallN(Free(lpd->rmatind));
	    CallN(Free(lpd->rmatval));
	}
/*
	if (lpd->cp_nr_sz)
	{
	    CallN(Free(lpd->cp_rmatbeg));
	    CallN(Free(lpd->cp_rhsx));
	    CallN(Free(lpd->cp_senx));
	}
	if (lpd->cp_nz_sz)
	{
	    CallN(Free(lpd->cp_rmatind));
	    CallN(Free(lpd->cp_rmatval));
	}
*/
	if (lpd->cp_nr_sz2)
	{
	    CallN(Free(lpd->cp_rmatbeg2));
	    CallN(Free(lpd->cp_rhsx2));
	    CallN(Free(lpd->cp_senx2));
	    CallN(Free(lpd->cp_active2));
	    CallN(Free(lpd->cp_initial_add2));
	    if (lpd->cp_nz_sz2)
	    {
		CallN(Free(lpd->cp_rmatind2));
		CallN(Free(lpd->cp_rmatval2));
	    }
	}

	for (i=0; i < lpd->cp_npools2; i++) { TryFree(lpd->cp_pools2[i]); }
	TryFree(lpd->cp_pools2);
	TryFree(lpd->cp_pools_max2);
	TryFree(lpd->cp_pools_sz2);
    }
#ifdef CPLEX
    CPXflushchannel (cpx_env, cpxresults);
    CPXflushchannel (cpx_env, cpxlog);
    CPXflushchannel (cpx_env, cpxerror);
    CPXflushchannel (cpx_env, cpxwarning);
#endif
#if defined(XPRESS) || defined(COIN)
    (void) ec_flush(solver_streams[LogType]);
    (void) ec_flush(solver_streams[ResType]);
    (void) ec_flush(solver_streams[WrnType]);
    (void) ec_flush(solver_streams[ErrType]);
    
#endif
    CallN(Free(lpd));
}




/*
 * Get a licence if necessary, print message and fail if impossible.
 * If ok, do some low-level initialization
 *
 * As an indication that initialization was successful,
 * cpx_env is set not non-NULL, even for XPRESS.
 * All functions that do not use a problem handle must make sure that
 * the system is initialised by checking whether cpx_env is non-NULL!
 */

#ifdef NONLICENCED
/* mostly NoOps -- No licence needed */

int p_cpx_init(value vlicloc, type tlicloc, 
	       value vserialnum, type tserialnum, 
	       value vsubdir, type tsubdir)
{
    int i;

    /* use cpx_env to store the `default problem' */
    CallN(coin_create_prob(&cpx_env, NULL));

    for (i=0; i<4; i++)
    {
	solver_streams[i] = Current_Null;
    }

    Succeed;
}

int
p_cpx_challenge(value v, type t)
{
    Succeed;
}

int
p_cpx_exit()
{

    CallN(coin_free_prob(cpx_env));
    Succeed;
}

#else

int
p_cpx_init(value vlicloc, type tlicloc, 
	   value vserialnum, type tserialnum, 
	   value vsubdir, type tsubdir)
{
    char *licloc, /* licence location (XPRESS) or environment string (CPLEX) */
	 *subdir; /* solver/platform/version-specific stuff (XPRESS 15) */

    Check_Integer(tserialnum);
    Get_Name(vlicloc, tlicloc, licloc);
    Get_Name(vsubdir, tsubdir, subdir);
    if (*licloc == '\0') licloc = NULL;

    if (!cpx_env)
    {
# if defined(CPLEX)
	char errmsg[512];
	int status, dev_status;
#  if CPLEX >= 7
	if (licloc)
	{
	    /* We have a CPLEX runtime key, call CPXRegisterLicense().
	     * CAUTION: when this call fails, the process may be in a funny
	     * state. With Cplex 7/8, the thread bindings are changed and
	     * the process cannot use virtual timers any longer (bug 243).
	     */
	    Log1(CPXRegisterLicense(0, %d), (int) vserialnum.nint);
	    if (CPXRegisterLicense(licloc, (int) vserialnum.nint))
	    {
		Fprintf(Current_Error, "Invalid CPLEX runtime key.\n");
		(void) ec_flush(Current_Error);
		Fail;
	    }
	}
	/* Note CPLEX prints a banner to stderr in CPXopenCPLEX! */
	CallN(cpx_env = CPXopenCPLEX(&dev_status));
	if (dev_status)
	{
	    CPXgeterrorstring(cpx_env, dev_status, errmsg);
	    Fprintf(Current_Error, "%s", errmsg);
	    (void) ec_flush(Current_Error);
	    Fail;
	}
#   if CPLEX >= 8
	/* no dual reduction as suggested by manual to get firm infeasible 
           conclusion for MIP
	*/
	CallN(CPXsetintparam(cpx_env, CPX_PARAM_REDUCE, 1));
#   endif
#  else
	int rt_status = 0;
	CallN(cpx_env = CPXopenCPLEXdevelop(&dev_status));

	if (dev_status == 32027)	/* out of licences */
	{
	    CPXgeterrorstring(cpx_env, dev_status, errmsg);
	    Fprintf(Current_Error, "%s", errmsg);
	    (void) ec_flush(Current_Error);
	    Fail;
	}
	if (dev_status != 0)		/* other problem, try runtime licence */
	{
	    char *serialnumstring = getenv("ECLIPSECPLEXSERIALNUM");
	    int serialnum = serialnumstring ? strtol(serialnumstring, NULL, 0) : vserialnum.nint;
	    if (serialnum)
	    {
		cpx_env = CPXopenCPLEXruntime(&rt_status, serialnum, licloc);
	    }
	    else
	    {
		Fprintf(Current_Error, "Couldn't find CPLEX development licence: check setting of CPLEXLICENCE,\nCPLEXLICDIR, CPLEXLICTYPE or set ECLIPSECPLEXSERIALNUM to use a runtime licence.\n", 0);
		(void) ec_flush(Current_Error);
		Fail;
	    }
	}
	if (dev_status && rt_status)	/* no licence could be opened */
	{
	    CPXgeterrorstring(cpx_env, dev_status, errmsg);
	    Fprintf(Current_Error, "DEV: %s", errmsg);
	    CPXgeterrorstring(cpx_env, rt_status, errmsg);
	    Fprintf(Current_Error, "RT: %s", errmsg);
	    (void) ec_flush(Current_Error);
	    Fail;
	}
#  endif /* CPLEX < 7 */
	status = CPXgetchannels(cpx_env, &cpxresults, &cpxwarning, &cpxerror, &cpxlog);
	if (status)
	{
	    CPXgeterrorstring(cpx_env, status, errmsg);
	    Fprintf(Current_Error, "%s", errmsg);
	    (void) ec_flush(Current_Error);
	    Bip_Error(EC_EXTERNAL_ERROR);
	}

# endif /* CPLEX */
# ifdef XPRESS
	int i, err;
	char slicmsg[256], banner[256];

	for (i=0; i<4; i++)
	{
	    solver_streams[i] = Current_Null;
	}

	/* Embedded OEM licence handling */
	if (oem_xpress == XP_OEM_YES)
	{
	    i = (int) vserialnum.nint;
	    err = XPRSlicense(&i, slicmsg);	/* second call */
	}
#  ifdef XPRESS_OEM_ICPARC_2002
	Handle_OEM_ICPARC
#  endif
	{
	    int ndays;
#  if (XPRESS == 15) 
	    {/* Xpress 15 requires the PATH environment variable to be set
                to where the license manager lmgrd is, as it execs an
                unqualified lmgrd from within XPRSinit()!
	     */
		const char * curpaths;
		char * newpaths;
		curpaths = getenv("PATH");
		newpaths = Malloc(strlen("PATH=") + strlen(curpaths)
			+ strlen(PATH_SEPARATOR) + strlen(subdir) + 1);
		strcpy(newpaths, "PATH=");
		strcat(newpaths, subdir);
		strcat(newpaths, PATH_SEPARATOR);
		strcat(newpaths, curpaths);
		putenv(newpaths);
	    }
#  endif
		
	    err = XPRSinit(licloc);
	    /* no banner printed in XPRESS 13, print it now as it may contain
	       extra error information
	    */
	    XPRSgetbanner(banner);
	    Fprintf(Current_Output, "%s\n", banner);
	    XPRSgetdaysleft(&ndays);
	    /* ndays == 0 if license is not time-limited */
	    if (ndays > 0)
	    {
		Fprintf(Current_Output, "This XPRESS license will expire in %d days.\n\n", ndays);
	    }
	    else if (ndays < 0)
	    {
		Fprintf(Current_Error, "This XPRESS license has expired\n\n");
		ec_flush(Current_Error);
	    }
	    ec_flush(Current_Output);
	}
	/* use cpx_env to store the `default problem' */
	CallN(XPRScreateprob(&cpx_env));
	if (err != 0  &&  err != 32 /* Student mode */)
	{
	    if (err != 8 || oem_xpress == XP_OEM_YES)	/* suppress message for OEM library */
	    {

		Fprintf(Current_Error, "XPRESS error (probably licencing problem)\n");
		(void) ec_flush(Current_Error);
	    }
	    Fail;
	}
	if (oem_xpress == XP_OEM_YES)		/* print the OEM message */
	{
	    Fprintf(Current_Output, slicmsg);
	    (void) ec_newline(Current_Output);
	}
# endif /* XPRESS */
    }
# ifdef LOG_CALLS
    Fprintf(log_output_, "\nvoid step_%d() {\n", log_ctr++);
    ec_flush(log_output_);
# endif
    Succeed;
}


int
p_cpx_challenge(value v, type t)
{
# ifdef CPLEX
    Fail;
# endif
# ifdef XPRESS
#  if defined(WIN32) 
    int nvalue;
    char slicmsg[256];
    /* Caution: calling optlicence() twice crashes on some non-oem XPRESSes */
    if (oem_xpress != XP_OEM_NO)
    {
	if (XPRSlicense(&nvalue, slicmsg) != 8)
	{
	    oem_xpress = XP_OEM_YES;
	    Return_Unify_Integer(v, t, (long) nvalue);
	}
    }
#  endif
    oem_xpress = XP_OEM_NO;
    Fail;
# endif
}


int
p_cpx_exit()
{
    if (cpx_env)
    {
	(void) CPXcloseCPLEX(&cpx_env);
	cpx_env = 0;
    }
    Succeed;
}

#endif /* !NONLICENCED */

int
p_cpx_prob_init(value vpre, type tpre, 
		value vcpy, type tcpy, 
		value vrow, type trow, 
		value vcol, type tcol, 
		value vnz, type tnz,
		value vdir, type tdir, 
		value vsense, type tsense, 
		value vhandle, type thandle)
{
    int	i;
    lp_desc *lpd;

    Check_Integer(tpre);
    Check_Integer(tcpy);
    Check_Integer(trow);
    Check_Integer(tcol);
    Check_Integer(tnz);
    Check_Structure(thandle);

    CallN(lpd = (lp_desc *) Malloc(sizeof(lp_desc)));
    /*CallN(_clr_lp_desc(lpd));*/
    CallN(memset(lpd, 0, sizeof(lp_desc)));


#ifdef USE_PROBLEM_ARRAY
    Log1(lpdmat[%d] = lpd, next_matno);
    current_matno = next_matno;
    lpd->matno = next_matno++;
#endif

#ifdef XPRESS
    lpd->copystatus = (vcpy.nint == 1 ? XP_COPYINVALID : XP_COPYOFF);

    {/* need unique name so that  file names created by XPRESS are unique
        dir is a directory path (but may need to be have the directory
        separator character (/ or \) added
     */
	int dirlen;

#ifdef WIN32
# define BASE_PROB_NAME \\eclipse
#else
# define BASE_PROB_NAME /eclipse
#endif 
	Check_String(tdir);
	dirlen = strlen(StringStart(vdir));
    
	lpd->probname = (char *) Malloc((50+dirlen) * sizeof(char));
	sprintf(lpd->probname, "%s"Transform_Quoted(BASE_PROB_NAME)"%u-%u", 
		StringStart(vdir), gethostid(), getpid()); 
	if (strlen(lpd->probname) > XP_PROBNAME_MAX)
	{
	    Fprintf(Current_Error, "Eplex error: the problem name for Xpress is too long.\n" 
            "Change tmp_dir to a directory with shorter path length.\n");
	    ec_flush(Current_Error);
	    Bip_Error(RANGE_ERROR);
	}
    }
#endif
    lpd->prob_type = PROBLEM_LP;
    lpd->presolve = vpre.nint;
    lpd->sense = vsense.nint;
    lpd->mac = vcol.nint;
    lpd->macsz = vcol.nint;
    lpd->macadded = 0;
    lpd->marsz = vrow.nint;	       /* max number of rows */
    lpd->mar = vrow.nint;
    lpd->matnz = vnz.nint;	       /* number of nonzero coefficients */

    /* if vcol/vrow/vnz is 0, malloc arrays of at least size 1. This avoid
       calling malloc with size 0, and does create an array (Xpress crashes
       if NULL is given in place of an array address in some cases)
       This increment should only be done after assigning their original
       values to the lpd fields!

    */
    if (vcol.nint == 0) vcol.nint++; 
    if (vrow.nint == 0) vrow.nint++; 
    if (vnz.nint == 0) vnz.nint++;
    lpd->rhsx = (double *) Malloc(vrow.nint * sizeof(double));
    lpd->senx = (char *) Malloc(vrow.nint * sizeof(char));

    /* one extra element for matbeg, because some representations of the 
       matrix (e.g. COIN) needs the `matbeg' for vcol+1 to be specified,
       so that the end of the last column can be determined without matcnt
    */
    lpd->matbeg = (int *) Malloc((1+vcol.nint) * sizeof(int));
    lpd->matcnt = (int *) Malloc(vcol.nint * sizeof(int));
    lpd->matind = (int *) Malloc(vnz.nint * sizeof(int));
    lpd->matval = (double *) Malloc(vnz.nint * sizeof(double));

    lpd->bdl = (double *) Malloc(vcol.nint * sizeof(double));
    lpd->bdu = (double *) Malloc(vcol.nint * sizeof(double));
    lpd->dirtybdflag = 0;
    lpd->objx = (double *) Malloc(vcol.nint * sizeof(double));
    lpd->ctype = (char *) Malloc(vcol.nint * sizeof(char));
    for (i = 0; i < vcol.nint; i++)
    {
	lpd->bdl[i] = -CPX_INFBOUND;
	lpd->bdu[i] = CPX_INFBOUND;
	lpd->objx[i] = 0.0;
	lpd->ctype[i] = 'C';
    }

    /* the cutpools fields in lpd should all be zero/NULL at this point, so 
       they do not need to be initialised
    */

    lpd->descr_state = DESCR_LOADED;

    {/* Return the cplex descriptor in argument HANDLE_CPH of the handle structure. */
	vhandle.ptr[HANDLE_CPH] = ec_handle(&lp_handle_tid, lpd);
	Make_Stamp(vhandle.ptr+HANDLE_STAMP); /* needed for other trail undos */
    }
    Succeed;
}


int
p_cpx_get_prob_param(value vlp, type tlp, value vp, type tp, value vval, type tval)
{
    lp_desc *lpd; 
    int i;
    Check_Integer(tp);
    LpDesc(vlp, tlp, lpd);
    switch(vp.nint)
    {
    case 0:	i = lpd->mar; break;
    case 1:	i = lpd->mac; break;
    case 3:	i = lpd->sense; break;
    case 4:	i = lpd->prob_type; break;
    case 5:	i = lpd->optimum_ctr; break;
    case 6:	i = lpd->infeas_ctr; break;
    case 7:	i = lpd->abort_ctr; break;
    case 8:	i = lpd->sol_itcnt; break;
    case 9:	i = lpd->sol_nodnum; break;
    case 10:	i = lpd->descr_state; break;
    case 11:	i = lpd->sol_state; break;
#ifdef XPRESS
    case 12:    XPRSgetintattrib(lpd->lp, XPRS_ELEMS, &i); break;
    case 13:    XPRSgetintattrib(lpd->lp, XPRS_MIPENTS, &i); break;
    case 14:    XPRSgetintattrib(lpd->lp, XPRS_QELEMS, &i); break;
#endif
#if defined(CPLEX) || defined(COIN)
    case 12:    i = CPXgetnumnz(cpx_env, lpd->lp); break;
    case 13:    
	i = IsMIPProb(lpd->prob_type) ? 
	    CPXgetnumint(cpx_env, lpd->lp) + CPXgetnumbin(cpx_env, lpd->lp) : 0;
	break;
    case 14:    
	i = IsQPProb(lpd->prob_type) ? CPXgetnumqpnz(cpx_env, lpd->lp) : 0;
	break;
#endif
    case 15: i = lpd->start_mac; break;
/*    case 16: i = lpd->cp_nr; break;*/
    case 17: i = lpd->cp_nr2; break;
    case 18: i = lpd->cp_nact2; break;
    default:
	Bip_Error(RANGE_ERROR);
    }
    Return_Unify_Integer(vval, tval, i);
}


int
p_cpx_get_param(value vlp, type tlp, value vp, type tp, value vval, type tval)
{
    double dres;
    int i, ires;
    lp_desc *lpd; 

#ifndef COIN
    if (!cpx_env)
    {
	Bip_Error(EC_LICENSE_ERROR);
    }
#endif
    /* lpd is NULL for global/default param */
    if (IsHandle(tlp))  { LpDesc(vlp, tlp, lpd); } else lpd = NULL;

    if (IsAtom(tp))
    {
	for(i=0; i<NUMPARAMS+NUMALIASES; ++i)	/* lookup the parameter name */
	{
	    if (strcmp(params[i].name, DidName(vp.did)) == 0)
	    	break;
	}
	if (i==NUMPARAMS+NUMALIASES)
	{
	    Bip_Error(RANGE_ERROR);
	}
	if (params[i].type == 0  &&
		Get_Int_Param(cpx_env, lpd, params[i].num, &ires)
	    == 0)
	{
	    Return_Unify_Integer(vval, tval, ires);
	}
	if (params[i].type == 1  &&
		Get_Dbl_Param(cpx_env, lpd, params[i].num, &dres)
	    == 0)
	{
	    Return_Unify_Float(vval, tval, dres);
	}
#ifdef COIN
	if (params[i].type == 3 && 
	        coin_get_solver_intparam((lpd==NULL ? cpx_env : lpd->lp), 
					  params[i].num, &ires)
	    == 0)
	{
	    Return_Unify_Integer(vval, tval, ires);
	}
	if (params[i].type == 4  &&
		coin_get_solver_dblparam((lpd==NULL ? cpx_env : lpd->lp),
					 params[i].num, &dres)
	    == 0)
	{
	    Return_Unify_Float(vval, tval, dres);
	}
#endif
	    
	Bip_Error(TYPE_ERROR);	/* occurs only if params[i].type is wrong */
    }
    Check_Integer(tp);
    switch (vp.nint)
    {
    case -1:			/* get optimizer code */
#ifdef CPLEX
	Return_Unify_Integer(vval, tval, 0);
#endif
#ifdef XPRESS
	Return_Unify_Integer(vval, tval, 1);
#endif
#ifdef COIN
	Return_Unify_Integer(vval, tval, 2);
#endif
    case -2:			/* get optimizer version */
#ifdef CPLEX
	Return_Unify_Integer(vval, tval, 10*CPLEX+CPLEXMINOR);
#endif
#ifdef XPRESS
	Return_Unify_Integer(vval, tval, XPRESS);
#endif
#ifdef COIN
      {
	  char * ver = Malloc(32*sizeof(char));
	  pword pw;

	  coin_get_solver_info(ver);
	  Make_String(&pw, ver);
	  Free(ver);

	  Return_Unify_Pw(vval, tval, pw.val, pw.tag);
      }
#endif
    default:
	Bip_Error(RANGE_ERROR);
    }
}


int
p_cpx_set_param(value vlpd, type tlpd, value vp, type tp, value vval, type tval)
/* fails if parameter unknown */
{
    int i;
    int err = 1;
    lp_desc *lpd; 

    Check_Atom(tp);
#ifndef COIN
    if (!cpx_env)
    {
	Bip_Error(EC_LICENSE_ERROR);
    }
#endif
    /* lpd is NULL for global/default param */
    if (IsHandle(tlpd))  { LpDesc(vlpd, tlpd, lpd); } else lpd = NULL;

#ifndef SOLVER_HAS_LOCAL_PARAMETERS
    if (lpd != NULL) 
    {
	Bip_Error(UNIMPLEMENTED);
    }
#endif
    for(i=0; i<NUMPARAMS+NUMALIASES; ++i)	/* lookup the parameter name */
    {
	if (strcmp(params[i].name, DidName(vp.did)) == 0)
	    break;
    }
    if (i==NUMPARAMS+NUMALIASES)
    {
	Fail;
    }
    if (params[i].type == 0 && IsInteger(tval))
    {
	/* Log4 because Set_Int_Param expands into 4 arg for Xpress */
	Log4(Set_Int_Param(cpx_env, lpd, %d, %d), params[i].num, vval.nint,
			   params[i].num, vval.nint);
	err = Set_Int_Param(cpx_env, lpd, params[i].num, vval.nint);
    }
    else if (params[i].type == 1 && IsDouble(tval))
    {
	Log4(Set_Dbl_Param(cpx_env, lpd, %d, %f), params[i].num, Dbl(vval),
             params[i].num, Dbl(vval));
        err = Set_Dbl_Param(cpx_env, lpd, params[i].num, Dbl(vval));
    }
#ifdef XPRESS
    else if (params[i].type == 2 && (IsAtom(tval) || IsString(tval)))
    {
	char *s = IsAtom(tval)? DidName(vval.did): StringStart(vval);

	Call(err, XPRSsetstrcontrol(lpd->lp, params[i].num, s));
    }
# ifdef WIN32
    else if (params[i].type == 3 && (IsAtom(tval) || IsString(tval)))
    {
	char *s = IsAtom(tval)? DidName(vval.did): StringStart(vval);
	err = XPRSsetlogfile(lpd->lp, s);
    }
# endif
#endif
#ifdef COIN
    /* Solver dependent parameters */
    else if (params[i].type == 3 && IsInteger(tval))
    {
	err = coin_set_solver_intparam((lpd == NULL ? cpx_env : lpd->lp), params[i].num, vval.nint);
    }
    else if (params[i].type == 4 && IsDouble(tval))
    {
        err = coin_set_solver_dblparam((lpd == NULL ? cpx_env : lpd->lp), params[i].num, Dbl(vval));
    }
#endif
    if (err) {
	Bip_Error(TYPE_ERROR);
    }
    Succeed_;
}


/*----------------------------------------------------------------------*
 * Message and error output
 *----------------------------------------------------------------------*/

#ifdef CPLEX

static void CPXPUBLIC
# if CPLEX >= 8
eclipse_out(void * nst, const char * msg)
# else 
eclipse_out(void * nst, char * msg)
# endif
{
    (void) ec_outf((stream_id) nst, msg, strlen(msg));
    (void) ec_flush((stream_id) nst);
}


int
p_cpx_output_stream(value vc, type tc, value vwhat, type twhat, value vs, type ts)
{
    stream_id nst;
    CPXCHANNELptr ch;

    if (!cpx_env)
    {
	Bip_Error(EC_LICENSE_ERROR);
    }
    Check_Integer(ts);
    nst = ec_stream_id(vs.nint);
    Check_Integer(tc);
    switch (vc.nint) {
    case 0: ch = cpxresults; break;
    case 1: ch = cpxerror; break;
    case 2: ch = cpxwarning; break;
    case 3: ch = cpxlog; break;
    default: Bip_Error(RANGE_ERROR);
    }
    Check_Integer(twhat);
    if (vwhat.nint == 0)
    {
	CPXdelfuncdest(cpx_env, ch, (void *) nst, eclipse_out);
    } else {
	/* raise error only if adding a stream */
	if (CPXaddfuncdest(cpx_env, ch, (void *) nst, eclipse_out))
	    { Bip_Error(EC_EXTERNAL_ERROR); }
    }
    Succeed_;
}

#endif
#if defined(XPRESS) || defined(COIN)

# ifdef XPRESS
static void XPRS_CC
eclipse_out(XPRSprob lp, void * obj, const char * msg, int len, int msgtype)
{
    if (msgtype >= 1 && msgtype <= 4)
    {
	/* filter out and not print unwanted messages  */
	if (msgtype == 3)
	{
	    /* it seems that there are no other way of filtering out
               warning messages than to check the msg string
	    */
	    if (strncmp(msg, "?140 ", 5) == 0) return; /* basis lost */
	    if (strncmp(msg, "?359 ", 5) == 0) return; /* illegal int bounds */
	}
	/* Fprintf(solver_streams[msgtype-1], "*%d*", msgtype); */
	(void) ec_outf(solver_streams[msgtype-1], (char*) msg, len);
	(void) ec_newline(solver_streams[msgtype-1]);
    }
    /* flushing is done in cleanup */
}

# endif
# ifdef COIN

void
eclipse_out(int msgtype, const char* message)
{
    
    (void) ec_outf(solver_streams[msgtype], message, strlen(message));
    (void) ec_newline(solver_streams[msgtype]);

}

# endif

int
p_cpx_output_stream(value vc, type tc, value vwhat, type twhat, value vs, type ts)
{
    stream_id nst;
    stream_id *solver_stream;

# ifdef XPRESS
    if (!cpx_env)
    {
	Bip_Error(EC_LICENSE_ERROR);
    }
# endif
    Check_Integer(ts);
    nst = ec_stream_id(vs.nint);
    Check_Integer(tc);
    Check_Integer(twhat);
    switch (vc.nint) {
	case 0: solver_stream = &solver_streams[ResType]; break;
	case 1: solver_stream = &solver_streams[ErrType]; break;
	case 2: solver_stream = &solver_streams[WrnType]; break;
	case 3: solver_stream = &solver_streams[LogType]; break;
	default: Bip_Error(RANGE_ERROR);
    }
    if (vwhat.nint == 1)
	*solver_stream = nst;
    else if (*solver_stream == nst)
	*solver_stream = Current_Null;
    Succeed_;
}

#endif

/*----------------------------------------------------------------------*
 * Initial setup
 *----------------------------------------------------------------------*/

int
p_cpx_get_rhs(value vlp, type tlp, value vpool, type tpool, value vi, type ti, 
	      value vsense, type tsense, value vval, type tval)
{
    lp_desc *lpd; 
    Prepare_Requests
    double rhs[1];
    char sen[1];

    LpDesc(vlp, tlp, lpd);
    switch (vpool.nint)
    {
    case CSTR_TYPE_NORM:
	SetPreSolve(lpd->presolve);
	if (CPXgetrhs(cpx_env, lpd->lp, rhs, (int) vi.nint, (int) vi.nint))
	   { Bip_Error(EC_EXTERNAL_ERROR); }
	if (CPXgetsense(cpx_env, lpd->lp, sen, (int) vi.nint, (int) vi.nint))
	   { Bip_Error(EC_EXTERNAL_ERROR); }
	break;
/*
    case CSTR_TYPE_PERMCP:
	sen[0] = lpd->cp_senx[vi.nint];
	rhs[0] = lpd->cp_rhsx[vi.nint];
	break;
*/
    case CSTR_TYPE_CONDCP:
	sen[0] = lpd->cp_senx2[vi.nint];
	rhs[0] = lpd->cp_rhsx2[vi.nint];
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }

    Request_Unify_Integer(vsense, tsense, (long) sen[0]);
    Request_Unify_Float(vval, tval, rhs[0]);

    Return_Unify;
}

int
p_cpx_set_rhs_coeff(value vlp, type tlp, value vi, type ti, value vsense, type tsense, value vval, type tval)
{
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(ti);
    Check_Integer(tsense);
    Check_Number(tval);
    if (vi.nint >= lpd->marsz) { Bip_Error(RANGE_ERROR); }
    lpd->senx[vi.nint] = (char) vsense.nint;
    lpd->rhsx[vi.nint] = DoubleVal(vval, tval);
    Check_Constant_Range(lpd->rhsx[vi.nint]);
    Succeed;
}

int
p_cpx_set_obj_coeff(value vlp, type tlp, value vj, type tj, value vval, type tval)
{
    lp_desc *lpd;  
    int j;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tj);
    Check_Number(tval);
    j = vj.nint;

    if (j >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    if (j >= lpd->macadded) j -= lpd->macadded; /* added col */

    lpd->objx[j] = DoubleVal(vval, tval);
    Check_Constant_Range(lpd->objx[j]);
    Succeed;
}

int
p_cpx_get_obj_coeff(value vlp, type tlp, value vj, type tj, value vval, type tval)
{
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tj);
    if (vj.nint >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    Return_Unify_Float(vval, tval, lpd->objx[vj.nint]);
}

int
p_cpx_set_qobj_coeff(value vlp, type tlp, value vi, type ti, value vj, type tj, value vval, type tval)
{
    lp_desc *lpd; 
    double coef;
    int i;
    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(ti);
    Check_Integer(tj);
    Check_Number(tval);
    if (IsInteger(tval)) {
	coef = (double) vval.nint;
    } else {
	Check_Float(tval);
	coef = DoubleVal(vval, tval);
	Check_Constant_Range(coef);
    }
    if (vj.nint >= lpd->mac || vi.nint >= lpd->mac) { Bip_Error(RANGE_ERROR); }

    if (lpd->cb_cnt == 0)		/* first quadratic coefficient */
    {
	/* change the problem type to quadratic if linear */
	switch (lpd->prob_type)
	{
	case PROBLEM_LP:
	    lpd->prob_type = PROBLEM_QP;
	    break;
	case PROBLEM_MIP:
	    lpd->prob_type = PROBLEM_MIQP;
	    break;
	case PROBLEM_QP:
	case PROBLEM_MIQP:
	    /* nothing to be done if already quadratic */
	    break;
	default:
	    Fprintf(Current_Error, 
	       "Eplex error: quadratic objective coefficients cannot be added to problem type %d\n.", 
	    lpd->prob_type);
	    Bip_Error(RANGE_ERROR);
	}
    }
    if (lpd->cb_cnt >= lpd->cb_sz)	/* grow arrays if necessary */
	_grow_cb_arrays(lpd, 1);
    i = lpd->cb_cnt++;
    lpd->cb_index[i] = vi.nint;
    lpd->cb_index2[i] = vj.nint;
    lpd->cb_value[i] = vi.nint==vj.nint ? 2*coef : coef;
    Succeed;
}


int
p_cpx_set_var_name(value vlp, type tlp, value vj, type tj, value vname, type tname)
{
    lp_desc *lpd; 


#ifdef XPRESS
      int maxlen = 128;
      size_t namelen;
      char buffer[128];
#endif

    Check_Integer(tj);
    Check_String(tname);
    LpDesc(vlp, tlp, lpd);
    if (vj.nint >= lpd->mac) { Bip_Error(RANGE_ERROR); }

#ifdef XPRESS
    /* need to use temp. buffer in XPRESS as string is length limited */
    namelen = strlen(StringStart(vname));
    if (maxlen >= (int) namelen) {
    /* just in case the call fails, assign some maxlen in that case */
    if (XPRSgetintcontrol(lpd->lp, XPRS_MPSNAMELENGTH, &maxlen) != 0) maxlen = 8;
      strcpy(buffer, StringStart(vname));
    } else { 
      strncpy(buffer, StringStart(vname), (size_t) maxlen);
      buffer[maxlen] = '\0';
    }
    Log5({
	char buffer[%d];
	strncpy(buffer, "%s", %d);
	XPRSaddnames(lpd->lp, 2, buffer, %d, %d);
    }, maxlen, buffer, maxlen, vj.nint, vj.nint);
    XPRSaddnames(lpd->lp, 2, buffer, vj.nint, vj.nint);

#endif
#if defined(CPLEX) || defined(COIN)
    /* this assumes that vname.str will be copied upto first \0 */
    /* we use CPXchgname() as this is in version 4 */
    Log2(CPXchgname(cpx_env, lpd->lp, 'c', %d, %s), vj.nint, StringStart(vname));
    CPXchgname(cpx_env, lpd->lp, 'c', vj.nint, StringStart(vname));
#endif
    Succeed;
}


/*----------------------------------------------------------------------*
 * Changing rhs 
 * cplex_change_rhs(++CPH, ++Size, ++RowIdxs, ++RhsCoeffs) 
 *   Changes the rhs coefficients. RowIdxs and RhsCoeffs are lists of length
 *   Size. There is no provision for backtracking: the changes should be
 *   undone with another call to cplex_change_rhs
 */
int
p_cpx_change_rhs(value vlp, type tlp, value vsize, type tsize, 
	      value vidxs, type tidxs, value vvals, type tvals)
{
    lp_desc *lpd;
    int i, err;

    LpDesc(vlp, tlp, lpd);
    Check_Integer(tsize);

    if (vsize.nint > 0)
    {
	double *rhs = (double *) Malloc(vsize.nint * sizeof(double));
	int *idxs = (int *) Malloc(vsize.nint * sizeof(int));

	for (i=0; i<vsize.nint; ++i)
	{
	    if (IsList(tidxs) && IsList(tvals))
	    {
		pword *ihead = vidxs.ptr;
		pword *itail = ihead + 1;
		pword *vhead = vvals.ptr;
		pword *vtail = vhead + 1;

		Dereference_(ihead);
		Dereference_(vhead);
		if (!IsInteger(ihead->tag) || !IsNumber(vhead->tag))
		{
		    Free(idxs);
		    Free(rhs);
		    Bip_Error(TYPE_ERROR); 
		}
		idxs[i] = ihead->val.nint;
		rhs[i] = DoubleVal(vhead->val, vhead->tag);
		/* check that the row index and rhs value are in range... */
		if (idxs[i] < 0 || idxs[i] >= lpd->mar || 
		    rhs[i] < -CPX_INFBOUND || rhs[i] > CPX_INFBOUND)
		{ 
		    Free(idxs);
		    Free(rhs);
		    Bip_Error(RANGE_ERROR); 
		}

		Dereference_(itail);
		Dereference_(vtail);
		tidxs = itail->tag;
		vidxs = itail->val;
		tvals = vtail->tag;
		vvals = vtail->val;
	    } 
	    else 
	    { 
		Free(idxs);
		Free(rhs);
		Bip_Error(TYPE_ERROR); 
	    }
	}

	Call(err, CPXchgrhs(cpx_env, lpd->lp, vsize.nint, idxs, rhs));

	Free(idxs);
	Free(rhs);
	if (err) { Bip_Error(EC_EXTERNAL_ERROR); }
    }

    Succeed_;
}

/*----------------------------------------------------------------------*
 * Changing column bounds 
 * cplex_change_cols_bounds(++CPH, ++Size, ++Idxs, ++Los, ++His) 
 *   Changes the lower and upper bounds of columns. Idxs, Los, His are
 *   lists of length Size. There is no provision for backtracking: the 
 *   changes should be  undone with another call to cplex_change_cols_bounds
 *
 */
int
p_cpx_change_cols_bounds(value vlp, type tlp, value vsize, type tsize, 
      value vidxs, type tidxs, value vlos, type tlos, value vhis, type this)
{
    lp_desc *lpd;
    int i, err, size;

    LpDesc(vlp, tlp, lpd);
    Check_Integer(tsize);

    size = vsize.nint*2;
    if (size > 0)
    {
	double *bds = (double *) Malloc(size * sizeof(double));
	int *idxs = (int *) Malloc(size * sizeof(double));
	char *types = (char *) Malloc(size * sizeof(char));

	for (i=0; i<size; )
	{
	    if (IsList(tidxs) && IsList(tlos) && IsList(this))
	    {
		pword *ihead = vidxs.ptr;
		pword *itail = ihead + 1;
		pword *lohead = vlos.ptr;
		pword *lotail = lohead + 1;
		pword *uphead = vhis.ptr;
		pword *uptail = uphead + 1;

		Dereference_(ihead);
		Dereference_(lohead);
		Dereference_(uphead);
		if (!IsInteger(ihead->tag) || !IsNumber(lohead->tag) ||
		    !IsNumber(uphead->tag))
		{
		    Free(bds);
		    Free(idxs);
		    Free(types);
		    Bip_Error(TYPE_ERROR); 
		}
		idxs[i] = ihead->val.nint;
		if (idxs[i] < 0 || idxs[i] >= lpd->mac) 
		{ 
		    Free(bds);
		    Free(idxs);
		    Free(types);
		    Bip_Error(RANGE_ERROR); 
		}
		bds[i] = DoubleVal(lohead->val, lohead->tag);
		if (bds[i] <= -CPX_INFBOUND) bds[i] = -CPX_INFBOUND;
		types[i++] = 'L';
		
		idxs[i] = ihead->val.nint;
		bds[i] = DoubleVal(uphead->val, uphead->tag);
		if (bds[i] >= CPX_INFBOUND) bds[i] = CPX_INFBOUND;
		if (bds[i] < bds[i-1]) 
		{ 
		    Free(bds);
		    Free(idxs);
		    Free(types);
		    Fail; 
		}
		types[i++] = 'U';

		Dereference_(itail);
		Dereference_(lotail);
		Dereference_(uptail);
		tidxs = itail->tag;
		vidxs = itail->val;
		tlos = lotail->tag;
		vlos = lotail->val;
		this = uptail->tag;
		vhis = uptail->val;
	    } else 
	    { 
		Free(bds);
		Free(idxs);
		Free(types);
		Bip_Error(TYPE_ERROR); 
	    }
	}

	Call(err, CPXchgbds(cpx_env, lpd->lp, size, idxs, types, bds));
	Free(bds);
	Free(idxs);
	Free(types);

	if (err) { Bip_Error(EC_EXTERNAL_ERROR); }
    }

    Succeed_;
}

int
p_cpx_lo_hi(value vlo, type tlo, value vhi, type thi)
{
    Prepare_Requests;
    Request_Unify_Float(vlo, tlo, -CPX_INFBOUND);
    Request_Unify_Float(vhi, thi, CPX_INFBOUND);
    Return_Unify;
}

/*----------------------------------------------------------------------*
 * Changing problem type
 */

static void _cpx_reset_probtype ARGS((pword*,word*,int,int));

static void
_cpx_reset_probtype(pword * pw, word * pdata, int size, int flags)
{
    int err;
    lp_desc *lpd = ExternalData(pw[HANDLE_CPH].val.ptr); 

    if (!lpd)
	return; /* stale handle */

    if (lpd->descr_state != DESCR_EMPTY)
    {
#ifdef CPLEX
	switch  (((untrail_ptype*) pdata)->old_ptype)
	{
	case PROBLEM_LP:
	    Call(err, CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_LP));
	    break;
	case PROBLEM_QP:
	    Call(err, CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_QP));
	    break;
	default:
	    Fprintf(Current_Error, "Eplex problem: Trying to reset an Eplex problem to a unsupported type: %i.\n", 
		    ((untrail_ptype*) pdata)->old_ptype);
	    ec_flush(Current_Error);
	    return;

	}
#endif
	lpd->prob_type = ((untrail_ptype*) pdata)->old_ptype;
#ifdef SOLVE_MIP_COPY
	if (lpd->copystatus != XP_COPYOFF && lpd->lp != lpd->lpcopy)
	{
	    CallN(XPRSdestroyprob(lpd->lpcopy));
	    CallN(lpd->lpcopy = lpd->lp);
	    Mark_Copy_As_Modified(lpd);
	}
#endif
    }
}

int
p_cpx_change_lp_to_mip(value vhandle, type thandle)
{
    lp_desc *lpd;
    int err;
    untrail_ptype pdata;

    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);
    pdata.old_ptype = lpd->prob_type;

    /* all columns are assumed to be type 'C' by changing to MIP. Any
       integer columns will be set explicitly later
    */
    switch (pdata.old_ptype)
    {
    case PROBLEM_LP:
	lpd->prob_type = PROBLEM_MIP;
	Call(err, CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_MILP));
	break;
    case PROBLEM_QP:
#ifdef HAS_MIQP
	lpd->prob_type = PROBLEM_MIQP;
	Call(err, CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_MIQP));
#else
	Fprintf(Current_Error, "Eplex error: this solver does not support solving of quadratic MIP problems.\n");
	ec_flush(Current_Error);
	Bip_Error(UNIMPLEMENTED);
#endif
	break;
    case PROBLEM_MIQP:
    case PROBLEM_MIP:
	return PSUCCEED;
    default:
	Fprintf(Current_Error, "Eplex error: trying to change problem to mixed integer from an unexpected state.\n");
	ec_flush(Current_Error);
	Bip_Error(RANGE_ERROR);
	break;
    }
	
    if (err) { Bip_Error(EC_EXTERNAL_ERROR); }

#ifdef SOLVE_MIP_COPY
    if (lpd->copystatus != XP_COPYOFF)  
    {
	Call(err, XPRScreateprob(&lpd->lpcopy));
	if (err) { Bip_Error(EC_EXTERNAL_ERROR); }
	Mark_Copy_As_Modified(lpd);
    }
#endif
    ec_trail_undo(_cpx_reset_probtype, vhandle.ptr, NULL, (word*)&pdata, NumberOfWords(untrail_ptype), TRAILED_WORD32);
    return PSUCCEED;
}

/* cplex_set_problem_type(CPH, ProbType, SetSolverType)

   changes the problem type to ProbType. Used to set and reset the problem 
   type during probing, when the problem type might be changed temporarily.
   SetSolverType applies to external solvers which has its own problem type
   (currently CPLEX): it specifies if the external solver's problem type
   should be set as well (1 = yes, 0 = no). CPLEX's problem type cannot 
   always be changed when setting a problem type before a probe, e.g.
   CPXPROB_FIXEDMILP/MIQP can only be set if there is already a MILP/MIQP
   solution. Therefore, in these cases, the solver's problem type is only 
   changed during the solving of the problem in p_cpx_optimise().
*/
int 
p_cpx_set_problem_type(value vlp, type tlp, value vtype, type ttype, 
		       value vsetsolver, type tsetsolver)
{
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(ttype);
    Check_Integer(tsetsolver);
#ifdef CPLEX
    if (vsetsolver.nint == 1)
    {
	int err;
	switch (vtype.nint)
	{
	case PROBLEM_LP: 
	    Call(err,CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_LP)); 
	    break;
	case PROBLEM_MIP:
# ifndef HAS_RELAXEDLP
	    if (CPXgetprobtype(cpx_env, lpd->lp) == CPXPROB_LP &&
		lpd->ctype != NULL)
	    {
		/* this assumes that we have copied the ctype information
                   of original MIP problem to lpd->ctype 
		*/
		Call(err, CPXcopyctype(cpx_env, lpd->lp, lpd->ctype));
		TryFree(lpd->ctype);
	    }
	    else
# endif
	    {
		Call(err,CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_MILP));
	    }
	    break;
	case PROBLEM_QP:
	    Call(err,CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_QP));
	    break;
# ifdef HAS_MIQP
	case PROBLEM_MIQP:
	    if (CPXgetprobtype(cpx_env, lpd->lp) == CPXPROB_QP &&
		lpd->ctype != NULL)
	    {
		/* this assumes that we have copied the ctype information
                   of original MIQP problem to lpd->ctype 
		*/
		Call(err, CPXcopyctype(cpx_env, lpd->lp, lpd->ctype));
		TryFree(lpd->ctype);
	    }
	    else
	    {
		Call(err,CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_MILP));
	    }
	    break;
	case PROBLEM_FIXEDQ:
	    Call(err, CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_FIXEDMIQP));
	    break;
# endif
	case PROBLEM_FIXEDL:
	    Call(err,CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_FIXEDMILP));
	    break;
	default:
	    Bip_Error(RANGE_ERROR);
	}
	if (err) Bip_Error(EC_EXTERNAL_ERROR);
    }
#endif

    lpd->prob_type = vtype.nint;
    Succeed;
}

/*----------------------------------------------------------------------*
 * Changing column type 
 */

#define Get_Col_Bounds(j, lo0,hi0) {                    \
        if (CPXgetlb(cpx_env, lpd->lp, &lo0, j,  j))	\
	    { Bip_Error(EC_EXTERNAL_ERROR); }		\
        if (CPXgetub(cpx_env, lpd->lp, &hi0, j, j))	\
            { Bip_Error(EC_EXTERNAL_ERROR); }		\
}

#define Change_Col_Bound(j, Which, oldlo, oldhi, newbd, Stamp, changed) {\
    untrail_bound udata;					       \
    udata.bds[0] = oldlo;                                              \
    udata.bds[1] = oldhi;                                              \
    udata.idx = j;                                                     \
                                                                       \
    Log3(                                                              \
	 {\n\
            int myj      = %d;\n\
	    double bd  = %.15e;\n\
	    CPXchgbds(cpx_env, lpd->lp, 1, &myj, "%s", &bd);\n\
         }, j, newbd, Which                                            \
    );                                                                 \
                                                                       \
    CPXchgbds(cpx_env, lpd->lp, 1, &j, Which, &newbd);                 \
    changed = 1;                                                       \
    ec_trail_undo(_cpx_restore_bounds, vhandle.ptr,                    \
                  Stamp, (word *) &udata,                              \
		  NumberOfWords(untrail_bound), TRAILED_WORD32);       \
}


static void _cpx_restore_bounds ARGS((pword*,word*,int,int));

static void _cpx_reset_col_type ARGS((pword*,word*,int,int));

int
p_cpx_change_col_type(value vhandle, type thandle, 
		      value vj, type tj, 
		      value vtype, type ttype)
{
    int idx[1], res;
    char ctype[1];
    untrail_ctype udata;
    lp_desc *lpd;
#if defined(HAS_NARROW_INT_RANGE) || defined(HAS_INTLB_BUG)
    double lo0, hi0;
#endif

    Check_Structure(thandle);
    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);
    Check_Integer(tj);

    if (vj.nint >= lpd->mac || vj.nint < 0) { Bip_Error(RANGE_ERROR); }

    SetPreSolve(lpd->presolve);
    Mark_Copy_As_Modified(lpd);
    idx[0] = vj.nint;
    res = CPXgetctype(cpx_env, lpd->lp, ctype, vj.nint, vj.nint);

    if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }

    if ((char) vtype.nint != ctype[0]) {
	/* only need to change if new column type different */
	udata.idx = vj.nint;
	udata.ctype = ctype[0];

	/* if change is from B->I, just ignore it (user could have posted
           extra integer constraints)
	*/
	if (!((char) vtype.nint == 'I' && (char) ctype[0] == 'B'))
	{
	    ctype[0] = (char) vtype.nint;
#if defined(HAS_NARROW_INT_RANGE) || defined(HAS_INTLB_BUG)
            {/* Xpress 14 likes its integers to have smaller bounds */
		Get_Col_Bounds(idx[0], lo0, hi0);
# ifdef HAS_NARROW_INT_RANGE
                /* no timestamp on these column bound changes: they will always
		   be untrailed. This is because the type changes may be 
                   applied to multiple merged columns, and we need to
                   ensure that the bound change is always undone
		*/
		if (lo0 < -XPRS_MAXINT) 
		{
		    double intbound = -XPRS_MAXINT;
		    int changed; /* dummy here for the macro */
		    Change_Col_Bound(idx[0], "L", lo0, hi0,
				     intbound, NULL, changed);
		    lo0 = intbound; /* for HAS_INTLB_BUG below */
		}
		if (hi0 > XPRS_MAXINT)
		{
		    double intbound = XPRS_MAXINT;
		    int changed; /* dummy here for the macro */
		    Change_Col_Bound(idx[0], "U", lo0, hi0,
				     intbound, NULL, changed);
		}
# endif
	    }
#endif
	    res = CPXchgctype(cpx_env, lpd->lp, 1, idx, ctype);
	    Log2(
	    {int  idx[1];
	    char ctype[1];
	    idx[0] = %d;
	    ctype[0] = '%c';
	    CPXchgctype(cpx_env,lpd->lp,1, idx, ctype);
	    }, idx[0], ctype[0]);
#ifdef HAS_INTLB_BUG 
	    /* After changing a column type to integer, the lower bound is 
               lost (set to 0) if it was negative (Xpress 13-15)
	       Reported to Dash 2004-10-19
	    */
	    { 
		char btype[1];
		double lo1[1];

		btype[0] = 'L';
		lo1[0] = lo0;

		XPRSchgbounds(lpd->lp, 1, idx, btype, lo1);
	    }
	    Log2({\n\
		int myj = %d;\n\
		double bd = %.15e;\n\
		XPRSchgbounds(lpd->lp, 1, &myj, "L", &bd);\n\
	    }, idx[0], lo0);
#endif

	    if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }	
	    ec_trail_undo(_cpx_reset_col_type, vhandle.ptr, NULL, (word*)&udata, NumberOfWords(untrail_ctype), TRAILED_WORD32);
	}
    }
    Succeed_;
}


static void _cpx_reset_col_type(pword * phandle, word * udata, int size, int flags)

{
    int idx[1];
    char octype[1];

    lp_desc *lpd = ExternalData(phandle[HANDLE_CPH].val.ptr); 

    if (!lpd)
	return; /* stale handle */

    idx[0] = ((untrail_ctype*) udata)->idx; 
        octype[0] = ((untrail_ctype*) udata)->ctype;

#if 0
    Fprintf(Current_Error, "Resetting col %d to %c, in gc:%d\n",
	idx[0], octype[0], ec_.m.vm_flags & NO_EXIT);
    ec_flush(Current_Error);
#endif

    if (lpd->descr_state != DESCR_EMPTY) 
    {
	Log2(
	{int  idx[1];\n\
	char octype[1];\n\
	idx[0] = %d;\n\
	octype[0] = '%c';\n\
	CPXchgctype(cpx_env,lpd->lp,1, idx, octype);\n\
	}, idx[0], octype[0]);

	if (CPXchgctype(cpx_env, lpd->lp, 1, idx, octype))
	{
	    Fprintf(Current_Error, "Error in Changing column %d to type %c\n",
		    idx[0], octype[0]);
	    ec_flush(Current_Error);
	    return;
	}
      Mark_Copy_As_Modified(lpd);
    }
}


/*----------------------------------------------------------------------*
 * Adding constraints
 *----------------------------------------------------------------------*/
/*
 * We first collect the new row/col data in (growable) arrays using
 * p_cpx_set_matbeg(), p_cpx_set_matval() and p_cpx_set_obj_coeff() [cols]
 * p_cpx_new_row() and p_cpx_add_coeff() [rows].
 * Then, the information is transferred to the solver (and trailed)
 * by calling p_cpx_flush_new_rowcols().
 * On failure, the constraints get removed by _cpx_del_rowcols().
 *
 * added by AE 25/10/02
 * this is for adding rows whose index in the external
 * solver we want to know for sure - when we get duals
 * in colgen we really have to know we are getting the right
 * ones associated with the sp cost function vars
 * this requires all variables to have their index already in the attribute
 */

#define New_Row(nrs, nr_sz, senx, rhsx, rmatbeg, nnz, sense, vrhs, trhs, ExtraAlloc) {\
    if (nrs+1 >= nr_sz)	/* allocate/grow arrays */\
    {\
	CallN(nr_sz += NEWROW_INCR);\
	CallN(senx = (char *) Realloc(senx, nr_sz*sizeof(char)));\
	CallN(rhsx = (double *) Realloc(rhsx, nr_sz*sizeof(double)));\
	CallN(rmatbeg = (int *) Realloc(rmatbeg, nr_sz*sizeof(int)));\
        ExtraAlloc; \
    }\
    senx[nrs] = (char) sense;\
    rhsx[nrs] = DoubleVal(vrhs, trhs);\
    Check_Constant_Range(rhsx[nrs]);\
    rmatbeg[nrs] = nnz;\
    ++nrs;\
}

int
p_cpx_new_row(value vlp, type tlp, value vsense, type tsense, 
	      value vrhs, type trhs, value vgtype, type tgtype, 
	      value vidx, type tidx)
{
    lp_desc *lpd; 
    int    idx; 

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tsense);
    Check_Number(trhs);
    Check_Integer(tgtype);
    
    switch (vgtype.nint)
    {
    case CSTR_TYPE_NORM:
	idx = lpd->mar+lpd->nr;
	New_Row(lpd->nr, lpd->nr_sz, lpd->senx, lpd->rhsx, lpd->rmatbeg, 
		lpd->nnz, vsense.nint, vrhs, trhs, {});
	break;
/*
    case CSTR_TYPE_PERMCP:
	idx = lpd->cp_nr;
	New_Row(lpd->cp_nr, lpd->cp_nr_sz, lpd->cp_senx, lpd->cp_rhsx, 
		lpd->cp_rmatbeg, lpd->cp_nnz, vsense.nint, vrhs, trhs, {});
	break;
*/
    case CSTR_TYPE_CONDCP:
	idx = lpd->cp_nr2;
	New_Row(lpd->cp_nr2, lpd->cp_nr_sz2, lpd->cp_senx2, lpd->cp_rhsx2, 
		lpd->cp_rmatbeg2, lpd->cp_nnz2, vsense.nint, vrhs, trhs, 
		{CallN(lpd->cp_active2 = (char *) Realloc(lpd->cp_active2, lpd->cp_nr_sz2*sizeof(char))); 
		 CallN(lpd->cp_initial_add2 = (char *) Realloc(lpd->cp_initial_add2, lpd->cp_nr_sz2*sizeof(char)));
		});
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }
    Return_Unify_Integer(vidx, tidx, idx);
}
    	


#define Add_Row_Coeff(nnz_sz, nnzs, rmatind, rmatval, idxj, val, tag) {\
    if (nnzs >= nnz_sz)	/* allocate/grow arrays */\
    {\
        CallN(nnz_sz += NEWNZ_INCR);\
	CallN(rmatind = (int *) Realloc(rmatind, nnz_sz*sizeof(int)));\
	CallN(rmatval = (double *) Realloc(rmatval, nnz_sz*sizeof(double)));\
    }\
    if (idxj >= lpd->mac) { Bip_Error(RANGE_ERROR); }\
    rmatind[nnzs] = idxj;\
    rmatval[nnzs] = DoubleVal(val, tag);\
    Check_Constant_Range(rmatval[nnzs]);\
    ++nnzs;\
}

int
p_cpx_add_coeff(value vlp, type tlp, value vj, type tj, value v, type t, value vpool, type tpool)
{
    lp_desc *lpd;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tj);
    Check_Number(t);
    Check_Integer(tpool);

    switch (vpool.nint)
    {
    case CSTR_TYPE_NORM:
	Add_Row_Coeff(lpd->nnz_sz, lpd->nnz, lpd->rmatind, lpd->rmatval, vj.nint, v, t);
	break;
/*
    case CSTR_TYPE_PERMCP:
	Add_Row_Coeff(lpd->cp_nz_sz, lpd->cp_nnz, lpd->cp_rmatind, lpd->cp_rmatval, vj.nint, v, t);
	break;
*/
    case CSTR_TYPE_CONDCP:
	Add_Row_Coeff(lpd->cp_nz_sz2, lpd->cp_nnz2, lpd->cp_rmatind2, lpd->cp_rmatval2, vj.nint, v, t);
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }
    Succeed_;
}

static void
_grow_numbers_array(lp_desc * lpd, int m)   /* make sure array contains 0..m-1 */
{
    if (m > lpd->numsz)	/* grow auxiliary array if necessary */
    {
	int i = lpd->numsz;
	m = Max(m, i+NEWCOL_INCR);
	lpd->numsz = m;
# ifdef LOG_CALLS
	if (i == 0)
	    Fprintf(log_output_, "\n\
	    	lpd->numbers = (int *) malloc(%d*sizeof(int));\n\
	    	lpd->zeroes = (int *) malloc(%d*sizeof(int));\n\
	        lpd->dzeroes = (double *) malloc(%d*sizeof(double));", m, m, m);
	else
	    Fprintf(log_output_, "\n\
		lpd->numbers = (int *) realloc(lpd->numbers, %d*sizeof(int));\n\
		lpd->zeroes = (int *) realloc(lpd->zeroes, %d*sizeof(int));\n\
	        lpd->dzeroes = (int *) realloc(lpd->dzeroes, %d*sizeof(double));", m, m, m);
	Fprintf(log_output_, "\n\
	    { int i; for (i=%d; i<%d; i++)\n\
		{ lpd->numbers[i] = i; lpd->zeroes[i] = 0; lpd->dzeroes[i] = 0.0; } }", i, m);
# endif
	if (i == 0) {
	    lpd->numbers = (int *) Malloc(m*sizeof(int));
	    lpd->zeroes = (int *) Malloc(m*sizeof(int));
	    lpd->dzeroes = (double *) Malloc(m*sizeof(double));
	} else {
	    lpd->numbers = (int *) Realloc(lpd->numbers, m*sizeof(int));
	    lpd->zeroes = (int *) Realloc(lpd->zeroes, m*sizeof(int));
	    lpd->dzeroes = (double *) Realloc(lpd->dzeroes, m*sizeof(double));
	}
	for (; i < m; i++) {
	    lpd->numbers[i] = i;
	    lpd->zeroes[i] = 0;
	    lpd->dzeroes[i] = 0.0;
	}
    }
}


static void _cpx_restore_bounds(pword * phandle, word * udata, int size, int undo_context)
{
    lp_desc *lpd = ExternalData(phandle[HANDLE_CPH].val.ptr); 

    if (!lpd)
	return; /* stale handle */

    if (lpd->descr_state != DESCR_EMPTY)
    {
	/* lp has not been cleaned up */

	int idx[2];
	int res;
	untrail_bound adata; /* needed to ensure proper alignment */

	memcpy(&adata, udata, sizeof(untrail_bound));

	idx[0] = adata.idx;
	idx[1] = adata.idx;

	Log4(
	      { 
               int idx[2];
	       double bds[2];
               idx[0] = %d;
               idx[1] = %d;
	       bds[0] = %.15e;
	       bds[1] = %.15e;
	       CPXchgbds(cpx_env, lpd->lp, 2, idx, "LU", bds);
              }, idx[0], idx[1], adata.bds[0], adata.bds[1] 
	);

	res = CPXchgbds(cpx_env, lpd->lp, 2, idx, "LU", adata.bds);
	if (res != 0) 
	{
	    Fprintf(Current_Error, "Eplex external solver error while trying to restore bounds.to column %d\n", idx[0]);
	    ec_flush(Current_Error);

	}
    }
}

static void
reset_rowcols(lp_desc * lpd, int oldmar, int oldmac)
{
#ifdef CPLEX
	if (lpd->mar > oldmar)
	{
	    Log2(CPXdelrows(cpx_env, lpd->lp, %d, %d), oldmar, lpd->mar-1);
	    if (CPXdelrows(cpx_env, lpd->lp, oldmar, lpd->mar-1))
	    {
		Fprintf(Current_Error, "Error in CPXdelrows(%d..%d)\n",
			oldmar, lpd->mar-1);
		ec_flush(Current_Error);
	    }
	    lpd->mar = oldmar;
	}
	if (lpd->macadded > oldmac)
	{
	    Log2(CPXdelcols(cpx_env, lpd->lp, %d, %d), oldmac, lpd->macadded-1);
	    if (CPXdelcols(cpx_env, lpd->lp, oldmac, lpd->macadded-1))
	    {
		Fprintf(Current_Error, "Error in CPXdelcols(%d..%d)\n",
		    oldmac, lpd->macadded-1);
		ec_flush(Current_Error);
	    }
	    lpd->macadded = oldmac;
	}
#endif
#if defined(XPRESS) || defined(COIN)
	int ndr = lpd->mar - oldmar;
	int ndc = lpd->macadded - oldmac;
	int m = Max(lpd->mar,lpd->macadded);

	_grow_numbers_array(lpd, m);	/* if necessary */

	if (ndr > 0)
	{
	    Log2(XPRSdelrows(lpd->lp,%d, &lpd->numbers[%d]), ndr,oldmar);
	    if (XPRSdelrows(lpd->lp, ndr, &lpd->numbers[oldmar]))
	    {
		Fprintf(Current_Error, "Error in deleting rows %d..%d\n",
			oldmar, oldmar+ndr-1);
		ec_flush(Current_Error);
	    }
	    lpd->mar = oldmar;
	}
	if (ndc > 0)
	{
	    Log2(XPRSdelcols(lpd->lp,%d, &lpd->numbers[%d]), ndc,oldmac);
	    if (XPRSdelcols(lpd->lp, ndc, &lpd->numbers[oldmac]))
	    {
		Fprintf(Current_Error, "Error in deleting cols %d..%d\n",
		    oldmac, oldmac+ndc-1);
		ec_flush(Current_Error);
	    }
	    lpd->macadded = oldmac;
	}
#endif
	lpd->mac = lpd->macadded;
	Mark_Copy_As_Modified(lpd);
}

static void _cpx_del_rowcols ARGS((pword*,word*,int,int));

static void _cpx_del_rowcols(pword * phandle,word * udata, int size, int flags)

{
    lp_desc *lpd = ExternalData(phandle[HANDLE_CPH].val.ptr); 

    int oldmar = ((untrail_data*) udata)->oldmar, 
        oldmac = ((untrail_data*) udata)->oldmac;

    if (!lpd)
	return; /* stale handle */

#if 0
    Fprintf(Current_Error, "Removing rows %d..%d, cols %d..%d, in gc:%d\n",
	oldmar, lpd->mar-1, oldmac, lpd->mac-1,
	ec_.m.vm_flags & NO_EXIT);
    ec_flush(Current_Error);
#endif

    if (lpd->descr_state != DESCR_EMPTY)
    {
	reset_rowcols(lpd, oldmar, oldmac);
    }
}


/* newcolobjs == 1  if non-zero objective coeffs are to be added */
int
p_cpx_flush_new_rowcols(value vhandle, type thandle, value vnewcolobjs, type tnewcolobjs)
{
    lp_desc *lpd; 
    int res, coladded, rowadded, nzadded;

    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);
    Check_Structure(thandle);
    Check_Integer(tnewcolobjs);
    coladded = lpd->mac - lpd->macadded;
    rowadded = lpd->nr;
    nzadded = lpd->nnz;

    /***
    Fprintf(Current_Error, "Adding rows %d..%d, cols %d..%d\n",
    	lpd->mar, lpd->mar+rowadded-1,
    	lpd->mac, lpd->mac+coladded-1);
    ec_flush(Current_Error);
    ***/
    SetPreSolve(lpd->presolve);
    Mark_Copy_As_Modified(lpd);

#ifdef LOG_CALLS
    {
	int i;
	Fprintf(log_output_, "\n\
		lpd->nr = %d;", lpd->nr);
	Fprintf(log_output_, "\n\
                lpd->matnz = %d;", lpd->matnz);
	/* needed by OSI */
	Fprintf(log_output_, "\n\
                lpd->mac = %d;", lpd->mac);
	if (vnewcolobjs.nint)
	{
	    for (i=0; i<coladded; ++i)
	    {
		Fprintf(log_output_, "\n\
                lpd->objx[%d] = %.15e;", i, lpd->objx[i]);
	    }
	}
	for (i=0; i<coladded; ++i)
	{
	    Fprintf(log_output_, "\n\
                lpd->bdl[%d] = %.15e;\n\
                lpd->bdu[%d] = %.15e;", i, lpd->bdl[i], i, lpd->bdu[i]);
	}
	for (i=0; i<lpd->matnz; ++i)
	{
	    Fprintf(log_output_, "\n\
                lpd->matind[%d] = %d;\n\
                lpd->matval[%d] = %.15e;", i, lpd->matind[i], i, lpd->matval[i]);
	}
	if (lpd->matnz > 0)
	{
	    for (i=0; i<coladded; ++i)
	    {
		Fprintf(log_output_, "\n\
                lpd->matbeg[%d] = %d;", i, lpd->matbeg[i]);
	    }
	}

	for (i=0; i<lpd->nr; ++i)
	{
	    Fprintf(log_output_, "\n\
		lpd->senx[%d] = '%c';\n\
		lpd->rhsx[%d] = %.15e;\n\
		lpd->rmatbeg[%d] = %d;",
	    	i, lpd->senx[i], i, lpd->rhsx[i], i, lpd->rmatbeg[i]);
	}
	Fprintf(log_output_, "\n\
		lpd->nnz = %d;", lpd->nnz);
	for (i=0; i<lpd->nnz; ++i)
	{
	    Fprintf(log_output_, "\n\
		lpd->rmatind[%d] = %d;\n\
		lpd->rmatval[%d] = %.15e;",
	    	i, lpd->rmatind[i], i, lpd->rmatval[i]);
	}
    }
#endif

    if (coladded)
    {
	_grow_numbers_array(lpd, coladded);	/* for zeroes[] */
	Log2(CPXaddcols(cpx_env, lpd->lp, %d, lpd->matnz, 
			%s, (lpd->matnz ? lpd->matbeg : lpd->zeroes),
		       lpd->matind, lpd->matval, lpd->bdl, lpd->bdu, NULL),
	               coladded, (vnewcolobjs.nint ? "lpd->objx" : "lpd->dzeroes"));

	res = CPXaddcols(cpx_env, lpd->lp, coladded, lpd->matnz, 
			 (vnewcolobjs.nint ? lpd->objx : lpd->dzeroes),
			 (lpd->matnz ? lpd->matbeg : lpd->zeroes),
			 lpd->matind, lpd->matval, lpd->bdl, lpd->bdu, NULL);
	TryFree(lpd->objx);
	TryFree(lpd->matbeg);
	TryFree(lpd->matind);
	TryFree(lpd->matval);
	lpd->matnz = 0;
	if (lpd->dirtybdflag & 3)
	{
	    if (lpd->dirtybdflag & 1) TryFree(lpd->bdl);
	    if (lpd->dirtybdflag & 2) TryFree(lpd->bdu);
	    lpd->dirtybdflag = 0;
	}
			     
	if (IsMIPProb(lpd->prob_type) && res == 0)
	{
	    int i, colidx;
	    if (lpd->qgtype) CallN(Free(lpd->qgtype));
	    lpd->qgtype = NULL;
	    if (lpd->mgcols) CallN(Free(lpd->mgcols));
	    lpd->mgcols = Malloc(coladded*sizeof(int));
	    Log1(lpd->mgcols = (int *) malloc(%d*sizeof(int)), coladded);
	    /* we reuse mgcols for the index of the added columns. We set the
               type for all columns, as Dash does not specify what types
               columns are set to in XPRSaddcols().
	    */
	    for ((i=0, colidx=lpd->macadded); i < coladded; (i++, colidx++))
	    {
		lpd->mgcols[i] = colidx;
		Log2(lpd->mgcols[%d] = %d, i, colidx);
		Log2(lpd->ctype[%d] = %d, i, lpd->ctype[i]);
	    }
	    Log1(CPXchgctype(cpx_env, lpd->lp, %d, lpd->mgcols, lpd->ctype),
		 coladded);
	    res = CPXchgctype(cpx_env, lpd->lp, coladded,
				  lpd->mgcols, lpd->ctype);
	}

	if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
    }
    if (rowadded)
    {
	Log2(CPXaddrows(cpx_env, lpd->lp, 0, %d, %d, lpd->rhsx, lpd->senx, 
			lpd->rmatbeg, lpd->rmatind, lpd->rmatval, NULL, NULL),
	     rowadded, nzadded);
	res = CPXaddrows(cpx_env, lpd->lp, 0, rowadded, nzadded,
			 lpd->rhsx, lpd->senx, lpd->rmatbeg, lpd->rmatind, 
			 lpd->rmatval, NULL, NULL);
	if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
    }

    if (coladded || rowadded)
    {
      untrail_data udata;
      udata.oldmac = lpd->macadded;
      udata.oldmar = lpd->mar;
      ec_trail_undo(_cpx_del_rowcols, vhandle.ptr, vhandle.ptr+HANDLE_STAMP, (word*) &udata, NumberOfWords(untrail_data), TRAILED_WORD32); 
    }

    lpd->macadded = lpd->mac;	/* remember what we added */
    lpd->mar += rowadded;

    lpd->nr = lpd->nnz = 0;	/* maybe shrink arrays here */
    Succeed_;
}


/*----------------------------------------------------------------------*
 * Updating variable bounds
 *----------------------------------------------------------------------*/

/* the *impose* procedures are for columns that have been added to the
   external solver already.
*/
int 
p_cpx_impose_col_lwb(value vhandle, type thandle, 
		     value vatt, type tatt, 
		     value vj, type tj, 
		     value vlo, type tlo, 
		     value vchanged, type tchanged)
{
    lp_desc *lpd; 
    double lo0, hi0, newlo;
    int j, changed = 0;
    Check_Integer(tj);
    Check_Float(tlo);
    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);

    if (lpd->descr_state == DESCR_EMPTY) 
    {
	Fprintf(Current_Error, "Eplex error: empty handle\n");
	(void) ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    j = (int) vj.nint;
    if (j >= lpd->macadded) { Bip_Error(RANGE_ERROR); } 
    if ((newlo = Dbl(vlo)) < -CPX_INFBOUND) newlo = -CPX_INFBOUND; 

    Get_Col_Bounds(j, lo0, hi0);    
    if (newlo > hi0) 
    { 
	double ftol;
	
	Get_Feasibility_Tolerance(cpx_env, lpd, &ftol);
	if (newlo <= hi0 + ftol) newlo = hi0;
	else { Fail; }
    }

    if (lo0 < newlo) 
    {
	Change_Col_Bound(j, "L", lo0, hi0, newlo, vatt.ptr+COL_STAMP, changed);
    }

    Return_Unify_Integer(vchanged, tchanged, changed);
}

int 
p_cpx_impose_col_upb(value vhandle, type thandle, 
		     value vatt, type tatt, 
		     value vj, type tj, 
		     value vhi, type thi, 
		     value vchanged, type tchanged)
{
    lp_desc *lpd; 
    double lo0, hi0, newhi;
    int j, changed = 0;

    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);
    Check_Integer(tj);
    Check_Float(thi);

    if (lpd->descr_state == DESCR_EMPTY) 
    {
	Fprintf(Current_Error, "Eplex error: empty handle\n");
	(void) ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    j = (int) vj.nint;
    if (j >= lpd->macadded) { Bip_Error(RANGE_ERROR); } 
    if ((newhi = Dbl(vhi)) > CPX_INFBOUND) newhi = CPX_INFBOUND;

    Get_Col_Bounds(j, lo0, hi0);
    if (newhi < lo0) 
    { 
	double ftol;
	
	Get_Feasibility_Tolerance(cpx_env, lpd, &ftol);
	if (newhi >= lo0 - ftol) newhi = lo0;
	else { Fail; }
    }

    if (hi0 > newhi) 
    {
	Change_Col_Bound(j, "U", lo0, hi0, newhi, vatt.ptr+COL_STAMP, changed);
    }

    Return_Unify_Integer(vchanged, tchanged, changed);
}

int
p_cpx_impose_col_bounds(value vhandle, type thandle, 
			value vatt, type tatt, 
			value vj, type tj, 
			value vflag, type tflag, 
			value vlo, type tlo, 
			value vhi, type thi, 
			value vchanged, type tchanged)
{
    lp_desc *lpd; 
    double  lo0, hi0, newlo, newhi;
    int     res, j, flag, changed = 0;
    char    ctype[1];

    Check_Integer(tj);
    Check_Integer(tflag);
    Check_Float(thi);
    Check_Float(tlo);
    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);

    if (lpd->descr_state == DESCR_EMPTY) 
    {
	Fprintf(Current_Error, "Eplex error: empty handle\n");
	(void) ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    j = (int) vj.nint;
    if (j >= lpd->macadded) { Bip_Error(RANGE_ERROR); } 
    if ((newhi = Dbl(vhi)) > CPX_INFBOUND) newhi = CPX_INFBOUND;
    if ((newlo = Dbl(vlo)) < -CPX_INFBOUND) newlo = -CPX_INFBOUND;

    flag = (int) vflag.nint;
    if (flag == 0) {
	/* flag == 0 ==> we can widen the bound. Check to make sure that the new 
           bounds are not too wide, i.e. invalid for the column type
	*/
        switch (lpd->prob_type)
	{
	case PROBLEM_MIP:
	case PROBLEM_MIQP:
	    res = CPXgetctype(cpx_env, lpd->lp, ctype, vj.nint, vj.nint);
	    if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
	    
	    if (ctype[0] == 'B' && (newhi > 1 || newhi < 0 || newlo > 1 || newlo < 0))
	        { Bip_Error(RANGE_ERROR); }
#ifdef HAS_NARROW_INT_RANGE
	    if (ctype[0] == 'I' && (newhi > XPRS_MAXINT || newhi < -XPRS_MAXINT || newlo > XPRS_MAXINT || newlo < -XPRS_MAXINT))
	        { Bip_Error(RANGE_ERROR); }
#endif
	default:
	    break;
	}
    }

    Get_Col_Bounds(j, lo0, hi0);
    if (newhi < newlo) { Fail; } 
    if (flag && (newhi < lo0)) 
    {
	double ftol;
	
	/* if new bound outside but within tolerance of old bound, ignore change */
	Get_Feasibility_Tolerance(cpx_env, lpd, &ftol);
	if (newhi >= lo0 - ftol) 
	{
	    newhi = lo0;
	    if (newlo > newhi) newlo = lo0; /* make sure other bound is consistent! */
	}
	else 
	{ 
	    Fail; 
	}
    }

    if (flag && (newlo > hi0)) 
    { 
	double ftol;
	
	Get_Feasibility_Tolerance(cpx_env, lpd, &ftol);
	if (newlo <= hi0 + ftol) 
	{
	    newlo = hi0;
	    if (newhi < newlo) newhi = hi0;
	} else 
	{ 
	    Fail; 
	}
    }
    
    if (newhi == newlo) 
    {
	if (newhi != hi0 || newhi != lo0)
	    Change_Col_Bound(j, "B", lo0, hi0, newlo, vatt.ptr+COL_STAMP,
			     changed);
    }
    else
    {
	if (newhi < hi0 || (!flag && newhi > hi0)) Change_Col_Bound(j, "U", lo0, hi0, newhi,
								    vatt.ptr+COL_STAMP, changed);
	if (newlo > lo0 || (!flag && newlo < lo0)) Change_Col_Bound(j, "L", lo0, hi0, newlo,
								    vatt.ptr+COL_STAMP, changed);
    }

    Return_Unify_Integer(vchanged, tchanged, changed);
}

int
p_cpx_get_col_bounds(value vlp, type tlp, 
		     value vj, type tj, 
		     value vlo, type tlo, 
		     value vhi, type thi)
{
    Prepare_Requests
    lp_desc *lpd; 
    double lo, hi;
    int j;

    LpDesc(vlp, tlp, lpd);
    Check_Integer(tj);
    j = vj.nint;

    if (lpd->descr_state == DESCR_EMPTY) 
    {
	Fprintf(Current_Error, "Eplex error: empty handle\n");
	(void) ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    if (lpd->lp == NULL)
    {/* solver not created for problem yet, get bounds from arrays */
	Request_Unify_Float(vhi, thi, lpd->bdu[j]);
	Request_Unify_Float(vlo, tlo, lpd->bdl[j]);
	Return_Unify;
    }
    else if (j >= lpd->macadded)
    {/* column not yet added to solver, get bounds from arrays */
	j -= lpd->macadded; /* arrays are for new added columns only */
	Request_Unify_Float(vhi, thi, lpd->bdu[j]);
	Request_Unify_Float(vlo, tlo, lpd->bdl[j]);
	Return_Unify;
    }
    else
    {
	if (CPXgetub(cpx_env, lpd->lp, &hi, (int) vj.nint, (int) vj.nint))
	{
	    Bip_Error(EC_EXTERNAL_ERROR);
	}
	if (CPXgetlb(cpx_env, lpd->lp, &lo, (int) vj.nint, (int) vj.nint))
	{
	    Bip_Error(EC_EXTERNAL_ERROR);
	}
	Request_Unify_Float(vhi, thi, hi);
	Request_Unify_Float(vlo, tlo, lo);
	Return_Unify;
    }
}

int
p_cpx_set_new_cols(value vlp, type tlp, value vadded, type tadded, value vobjs, type tobjs, value vlos, type tlos, value vhis, type this, value vnzs, type tnzs)
{
    /* the column `buffer arrays' are needed by CPLEX and Xpress's interface
       to pass information for the columns. Except for ctype
       and possibly bdl, bdu, we simpy pass
       default values to the solver on setup/adding columns
    */
    lp_desc *lpd; 
    int i;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tadded);
    Check_Integer(tnzs);
    if (vadded.nint == 0) { Succeed_; } /* no added columns, return now! */

    lpd->mac += vadded.nint;
    lpd->matnz = vnzs.nint;
    TryFree(lpd->matind);
    TryFree(lpd->matval);
    if (vnzs.nint > 0)
    {
	lpd->matind = (int *) Malloc((size_t) vnzs.nint*sizeof(int));    
	lpd->matval = (double *) Malloc((size_t) vnzs.nint*sizeof(double));
	Log1(lpd->matind = (int *) malloc((size_t) %d*sizeof(int)), vnzs.nint);    
	Log1(lpd->matval = (double *) Malloc((size_t) %d*sizeof(double)), vnzs.nint);
    }
    TryFree(lpd->ctype);
    TryFree(lpd->matbeg);
    TryFree(lpd->matcnt);
    lpd->ctype = (char *) Malloc((size_t) vadded.nint*sizeof(char));
    /* +1 for matbeg required by COIN */
    lpd->matbeg = (int *) Malloc((size_t) (vadded.nint+1)*sizeof(int));
    lpd->matcnt = (int *) Malloc((size_t) vadded.nint*sizeof(int));
    Log3({\n\
	lpd->ctype = (char *) Malloc((size_t) %d*sizeof(char));\n\
	lpd->matbeg = (int *) Malloc((size_t) %d*sizeof(int));\n\
	lpd->matcnt = (int *) Malloc((size_t) %d*sizeof(int));\n\
    }, vadded.nint, vadded.nint, vadded.nint);

    for (i=0; i<vadded.nint; i++) lpd->ctype[i] = 'C';

    /* treatment of bounds arrays lpd->bdl, lpd->bdu:

       if columns with non-default bounds were added immediately previously
       the bounds arrays will have been freed in p_cpx_flush_new_rowcols so
       any existing bounds arrays contain default values and are length lpd->macsz

       1) if vadded.nint > lpd->macsz any existing arrays are too small:
          a) if we have non-default bounds to apply we free any existing array
	     and Malloc a new one of correct size vadded.nint since Realloc may
	     have to copy the contents of the existing array and free it and we
	     need to overwrite the entries again anyway
	  b) if we have default bounds to apply we just expand the existing array
	     with Realloc and initialize the new positions
       2) otherwise  any existing arrays are big enough:
          a) if we have non-default bounds to apply we overwrite the necessary entries
	  b) if we have default bounds to apply we have nothing to do

       if we have non-default bounds we set the appropriate bit of the lpd->dirtybdflag
       so that the arrays can be freed in p_cpx_flush_new_rowcols
    */
    if (vadded.nint > lpd->macsz) /* any existing bound arrays are too small */
    {
        if (IsList(tlos)) /* non-default lower bounds */
	{
	    /* since Realloc may copy and free and we need to overwrite entries
	       anyway it is probably better to free and Malloc */
	    TryFree(lpd->bdl);
	    lpd->bdl = (double *) Malloc((size_t) vadded.nint*sizeof(double));
	    Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), vadded.nint);
	    if (IsList(this)) /* non-default upper bounds */
	    {
	        /* both bounds arrays are non-default and will be freed
		   immediately after flushing: no need to increase lpd->macsz since
		   we will need to Malloc new bounds arrays of correct size next
		   time around anyway */
	        TryFree(lpd->bdu);
		lpd->bdu = (double *) Malloc((size_t) vadded.nint*sizeof(double));
		Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), vadded.nint);
		/* fill the bounds arrays with explicit bounds */
		for (i = 0; (IsList(tlos) && IsList(this)); ++i)
		{
		    pword *lhead = vlos.ptr;
		    pword *ltail = lhead + 1;
		    pword *hhead = vhis.ptr;
		    pword *htail = hhead + 1;
		    double lo, hi;
		    
		    Dereference_(lhead);
		    if (IsInteger(lhead->tag)) {
		      lo = (double) lhead->val.nint;
		    } else {
		      Check_Float(lhead->tag);
		      lo = Dbl(lhead->val);
		    }
		    lpd->bdl[i] = (lo < -CPX_INFBOUND ? -CPX_INFBOUND : lo);
		    Dereference_(hhead);
		    if (IsInteger(hhead->tag)) {
		      hi = (double) hhead->val.nint;
		    } else {
		      Check_Float(hhead->tag);
		      hi = Dbl(hhead->val);
		    }
		    lpd->bdu[i] = (hi > CPX_INFBOUND ? CPX_INFBOUND : hi);
		    Dereference_(ltail);
		    tlos = ltail->tag;
		    vlos = ltail->val;
		    Dereference_(htail);
		    this = htail->tag;
		    vhis = htail->val;
		}
		/* check that there are the right number of bds */
		if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
		/* mark both arrays as "dirty" */
		lpd->dirtybdflag = (lpd->dirtybdflag & 3);
	    }
	    else /* default upper bounds */
	    {
	        int newcsz;

		newcsz = Max(vadded.nint, lpd->macsz+NEWCOL_INCR);
		if (lpd->bdu == NULL) /* upper bounds array freed */
		{
		    lpd->bdu = (double *) Malloc((size_t) newcsz*sizeof(double));
		    Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), newcsz);

		    for (i=0; i<newcsz; i++)
		    {
		      lpd->bdu[i]  = CPX_INFBOUND;
		    }
		}
		else
		{
		    lpd->bdu = (double *) Realloc(lpd->bdu, (size_t) newcsz*sizeof(double));
		    Log1(lpd->bdu = (double *) realloc(%d*sizeof(double)), newcsz);
		    for (i=lpd->macsz; i<newcsz; i++)
		    {
		      lpd->bdu[i]  = CPX_INFBOUND;
		    }
		}
		lpd->macsz = newcsz;
		/* fill the lower bounds array with explicit bounds */
		for (i = 0; (IsList(tlos)); ++i)
		{
		    pword *lhead = vlos.ptr;
		    pword *ltail = lhead + 1;
		    double lo;
		    
		    Dereference_(lhead);
		    if (IsInteger(lhead->tag)) {
		      lo = (double) lhead->val.nint;
		    } else {
		      Check_Float(lhead->tag);
		      lo = Dbl(lhead->val);
		    }
		    lpd->bdl[i] = (lo < -CPX_INFBOUND ? -CPX_INFBOUND : lo);
		    Dereference_(ltail);
		    tlos = ltail->tag;
		    vlos = ltail->val;
		}
		/* check that there are the right number of bds */
		if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
		/* mark lower bounds array as "dirty" */
		lpd->dirtybdflag = (lpd->dirtybdflag & 2);
	    }
	}
	else if (IsList(this)) /* default lower bounds, non-default upper bounds */
	{
	    int newcsz;
	    
	    newcsz = Max(vadded.nint, lpd->macsz+NEWCOL_INCR);
	    if (lpd->bdl == NULL) /* lower bounds array freed */
	    {
	        lpd->bdl = (double *) Malloc((size_t) newcsz*sizeof(double));
		Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), newcsz);
		for (i=0; i<newcsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    else
	    {
	        lpd->bdl = (double *) Realloc(lpd->bdl, (size_t) newcsz*sizeof(double));
		Log1(lpd->bdl = (double *) realloc(%d*sizeof(double)), newcsz);
		for (i=lpd->macsz; i<newcsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    lpd->macsz = newcsz;
	    TryFree(lpd->bdu);
	    lpd->bdu = (double *) Malloc((size_t) vadded.nint*sizeof(double));
	    Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), vadded.nint);
	    /* fill the upper bounds array with explicit bounds */
	    for (i = 0; (IsList(this)); ++i)
	    {
	        pword *hhead = vhis.ptr;
		pword *htail = hhead + 1;
		double hi;
		
		Dereference_(hhead);
		if (IsInteger(hhead->tag)) {
		  hi = (double) hhead->val.nint;
		} else {
		  Check_Float(hhead->tag);
		  hi = Dbl(hhead->val);
		}
		lpd->bdu[i] = (hi > CPX_INFBOUND ? CPX_INFBOUND : hi);
		Dereference_(htail);
		this = htail->tag;
		vhis = htail->val;
	    }
	    /* check that there are the right number of bds */
	    if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
	    /* mark upper bounds array as "dirty" */
	    lpd->dirtybdflag = (lpd->dirtybdflag & 1);
	}
	else /* default bounds */
	{
	    int newcsz;
	    
	    newcsz = Max(vadded.nint, lpd->macsz+NEWCOL_INCR);
	    if (lpd->bdl == NULL) /* lower bounds array freed */
	    {
	        lpd->bdl = (double *) Malloc((size_t) newcsz*sizeof(double));
		Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), newcsz);
		for (i=0; i<newcsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    else
	    {
	        lpd->bdl = (double *) Realloc(lpd->bdl, (size_t) newcsz*sizeof(double));
		Log1(lpd->bdl = (double *) realloc(%d*sizeof(double)), newcsz);
		for (i=lpd->macsz; i<newcsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    if (lpd->bdu == NULL) /* upper bounds array freed */
	    {
	        lpd->bdu = (double *) Malloc((size_t) newcsz*sizeof(double));
		Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), newcsz);
		for (i=0; i<newcsz; i++)
		{
		  lpd->bdu[i]  = CPX_INFBOUND;
		}
	    }
	    else
	    {
	        lpd->bdu = (double *) Realloc(lpd->bdu, (size_t) newcsz*sizeof(double));
		Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), newcsz);
		for (i=lpd->macsz; i<newcsz; i++)
		{
		  lpd->bdu[i]  = CPX_INFBOUND;
		}
	    }
	    lpd->macsz = newcsz;
	}
    }
    else /* any existing bound arrays are big enough */
    {
        if (IsList(tlos)) /* non-default lower bounds */
	{
	    if (lpd->bdl == NULL) 
	    {
		lpd->bdl = (double *) Malloc((size_t) vadded.nint*sizeof(double));
		Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), vadded.nint);
	    }
	    if (IsList(this)) /* non-default upper bounds */
	    {
	        if (lpd->bdu == NULL) 
                {
		    lpd->bdu = (double *) Malloc((size_t) vadded.nint*sizeof(double));
		    Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), vadded.nint);
		}
		/* fill the bounds arrays with explicit bounds */
		for (i = 0; (IsList(tlos) && IsList(this)); ++i)
		{
		    pword *lhead = vlos.ptr;
		    pword *ltail = lhead + 1;
		    pword *hhead = vhis.ptr;
		    pword *htail = hhead + 1;
		    double lo, hi;
		    
		    Dereference_(lhead);
		    if (IsInteger(lhead->tag)) {
		      lo = (double) lhead->val.nint;
		    } else {
		      Check_Float(lhead->tag);
		      lo = Dbl(lhead->val);
		    }
		    lpd->bdl[i] = (lo < -CPX_INFBOUND ? -CPX_INFBOUND : lo);
		    Dereference_(hhead);
		    if (IsInteger(hhead->tag)) {
		      hi = (double) hhead->val.nint;
		    } else {
		      Check_Float(hhead->tag);
		      hi = Dbl(hhead->val);
		    }
		    lpd->bdu[i] = (hi > CPX_INFBOUND ? CPX_INFBOUND : hi);
		    Dereference_(ltail);
		    tlos = ltail->tag;
		    vlos = ltail->val;
		    Dereference_(htail);
		    this = htail->tag;
		    vhis = htail->val;
		}
		/* check that there are the right number of bds */
		if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
		/* mark both arrays as "dirty" */
		lpd->dirtybdflag = (lpd->dirtybdflag & 3);
	    }
	    else /* default upper bounds */
	    {
	        if (lpd->bdu == NULL) /* upper bounds array freed */
		{
		    lpd->bdu = (double *) Malloc((size_t) lpd->macsz*sizeof(double));
		    Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), lpd->macsz);
		    for (i=0; i<lpd->macsz; i++)
		    {
		      lpd->bdu[i]  = CPX_INFBOUND;
		    }
		}
		/* fill the lower bounds array with explicit bounds */
		for (i = 0; (IsList(tlos)); ++i)
		{
		    pword *lhead = vlos.ptr;
		    pword *ltail = lhead + 1;
		    double lo;
		    
		    Dereference_(lhead);
		    if (IsInteger(lhead->tag)) {
		      lo = (double) lhead->val.nint;
		    } else {
		      Check_Float(lhead->tag);
		      lo = Dbl(lhead->val);
		    }
		    lpd->bdl[i] = (lo < -CPX_INFBOUND ? -CPX_INFBOUND : lo);
		    Dereference_(ltail);
		    tlos = ltail->tag;
		    vlos = ltail->val;
		}
		/* check that there are the right number of bds */
		if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
		/* mark lower bounds array as "dirty" */
		lpd->dirtybdflag = (lpd->dirtybdflag & 2);
	    }
	}
	else if (IsList(this)) /* default lower bounds, non-default upper bounds */
	{
	    if (lpd->bdl == NULL) /* lower bounds array freed */
	    {
	        lpd->bdl = (double *) Malloc((size_t) lpd->macsz*sizeof(double));
		Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), lpd->macsz);
		for (i=0; i<lpd->macsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    /* fill the upper bounds array with explicit bounds */
	    for (i = 0; (IsList(this)); ++i)
	    {
	        pword *hhead = vhis.ptr;
		pword *htail = hhead + 1;
		double hi;
		
		Dereference_(hhead);
		if (IsInteger(hhead->tag)) {
		  hi = (double) hhead->val.nint;
		} else {
		  Check_Float(hhead->tag);
		  hi = Dbl(hhead->val);
		}
		lpd->bdu[i] = (hi > CPX_INFBOUND ? CPX_INFBOUND : hi);
		Dereference_(htail);
		this = htail->tag;
		vhis = htail->val;
	    }
	    /* check that there are the right number of bds */
	    if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
	    /* mark upper bounds array as "dirty" */
	    lpd->dirtybdflag = (lpd->dirtybdflag & 1);
	}
	else /* default bounds */
	{
	    if (lpd->bdl == NULL) /* lower bounds array freed */
	    {
	        lpd->bdl = (double *) Malloc((size_t) lpd->macsz*sizeof(double));
		Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), lpd->macsz);
		for (i=0; i<lpd->macsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    if (lpd->bdu == NULL) /* upper bounds array freed */
	    {
	        lpd->bdu = (double *) Malloc((size_t) lpd->macsz*sizeof(double));
		Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), lpd->macsz);
		for (i=0; i<lpd->macsz; i++)
		{
		  lpd->bdu[i]  = CPX_INFBOUND;
		}
	    }
	}
    }

    /* fill the objective coefficients array if specified */
    if (IsList(tobjs)) 
    {

	if (IsList(tobjs)) /* only if there are objective coefficients */
	{
	    TryFree(lpd->objx);
	    Log1(lpd->objx = (double *) malloc((size_t) %d*sizeof(double)), vadded.nint);
	    lpd->objx = (double *) Malloc((size_t) vadded.nint*sizeof(double));
	}

	for (i = 0; IsList(tobjs); ++i)
	{
	    pword *head = vobjs.ptr;
	    pword *tail = head + 1;
	    double coeff;

	    Dereference_(head);
	    if (IsInteger(head->tag)) {
		coeff = (double) head->val.nint;
	    } else {
		Check_Float(head->tag);
		coeff = Dbl(head->val);
		Check_Constant_Range(coeff);
	    }
	    lpd->objx[i] = coeff;
	    Dereference_(tail);
	    tobjs = tail->tag;
	    vobjs = tail->val;
	}
	/* check that there are the right number of objs */
	if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
    }

    Succeed_;
}


int
p_cpx_set_type(value vlp, type tlp, value vj, type tj, value vtype, type ttype)
{
    lp_desc *lpd; 
    int j;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tj);
    j = vj.nint;

    if (j >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    if (j >= lpd->macadded) j -= lpd->macadded; /* added col */
    lpd->ctype[j] = (char) vtype.nint;
    if (vtype.nint != 'C')
    {
	switch (lpd->prob_type)
	{
	case PROBLEM_LP:
	    lpd->prob_type = PROBLEM_MIP;
	    break;
#ifdef HAS_MIQP
	case PROBLEM_QP:
	    lpd->prob_type = PROBLEM_MIQP;
	    break;
	case PROBLEM_MIQP:
#endif
	case PROBLEM_MIP:
	    break;
	default:
	    Fprintf(Current_Error, "Eplex error: this solver does not support solving of quadratic MIP problems.\n");
	    ec_flush(Current_Error);
	    Bip_Error(EC_EXTERNAL_ERROR);
	    break;
	}

#ifdef XPRESS
    	++lpd->ngents;
#endif
    }
    Succeed_;
}

/*----------------------------------------------------------------------*
 * Retrieving variable type and bounds
 *----------------------------------------------------------------------*/


int
p_cpx_get_col_type(value vlp, type tlp, value vj, type tj, value vtype, type ttype)
{
    Prepare_Requests
    lp_desc *lpd; 
    char ctype[1];

    LpDesc(vlp, tlp, lpd);
    Check_Integer(tj);
    if (vj.nint >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    SetPreSolve(lpd->presolve);
    if (IsMIPProb(lpd->prob_type))
    {
	if (CPXgetctype(cpx_env, lpd->lp, ctype, (int) vj.nint, (int) vj.nint))
	{
	    Bip_Error(EC_EXTERNAL_ERROR);
	}
    }
    else
    {
	ctype[0] = 'C';
    }
    Request_Unify_Integer(vtype, ttype, (int) ctype[0]);
    Return_Unify;
}

/*----------------------------------------------------------------------*
 * Updating variable bounds
 *----------------------------------------------------------------------*/

static void
_grow_cb_arrays(lp_desc * lpd, int with_index2)
{
    if (lpd->cb_sz == 0)
    {
#ifdef LOG_CALLS
    Fprintf(log_output_, "\n\
	lpd->cb_sz = %d;\n\
	lpd->cb_index = (int *) malloc(lpd->cb_sz*sizeof(int));\n\
	lpd->cb_value = (double *) malloc(lpd->cb_sz*sizeof(double));",
	    NEWBD_INCR);
    if (with_index2)
	Fprintf(log_output_, "\n\
	    lpd->cb_index2 = (int *) malloc(lpd->cb_sz*sizeof(int));");
    ec_flush(log_output_);
#endif
	lpd->cb_sz = NEWBD_INCR;
	lpd->cb_index = (int *) Malloc(NEWBD_INCR*sizeof(int));
	if (with_index2)
	    lpd->cb_index2 = (int *) Malloc(NEWBD_INCR*sizeof(int));
	lpd->cb_value = (double *) Malloc(NEWBD_INCR*sizeof(double));
    }
    else
    {
#ifdef LOG_CALLS
    Fprintf(log_output_, "\n\
	lpd->cb_sz += %d;\n\
	lpd->cb_index = (int *) realloc(lpd->cb_index, lpd->cb_sz*sizeof(int));\n\
	lpd->cb_value = (double *) realloc(lpd->cb_value, lpd->cb_sz*sizeof(double));",
	    NEWBD_INCR);
    if (with_index2)
    {
	Fprintf(log_output_, "\n\
	    lpd->cb_index2 = lpd->cb_index2\n\
	    	? (int *) realloc(lpd->cb_index2, lpd->cb_sz*sizeof(int))\n\
		: (int *) malloc(lpd->cb_sz*sizeof(int));");
    }
    else if (lpd->cb_index2)
    {
	Fprintf(log_output_, "\n\
		free(lpd->cb_index2);\n\
		lpd->cb_index2 = 0;");
    }
    ec_flush(log_output_);
#endif
	lpd->cb_sz += NEWBD_INCR;
	lpd->cb_index = (int *) Realloc(lpd->cb_index, lpd->cb_sz*sizeof(int));
	lpd->cb_value = (double *) Realloc(lpd->cb_value, lpd->cb_sz*sizeof(double));
	if (with_index2)
	{
	    lpd->cb_index2 = lpd->cb_index2
	    	? (int *) Realloc(lpd->cb_index2, lpd->cb_sz*sizeof(int))
		: (int *) Malloc(lpd->cb_sz*sizeof(int));
	}
	else if (lpd->cb_index2)
	{
	    Free(lpd->cb_index2);
	    lpd->cb_index2 = 0;
	}
    }
}

/* cplex_init_new_col_bounds(CPH, J, Lo, Hi)
   initialises the bounds of a new column J that is to be added to the
    matrix to lower bound Lo and upper bound Hi.
*/
int
p_cpx_init_new_col_bounds(value vlp, type tlp, 
			  value vj, type tj, 
			  value vlo, type tlo, 
			  value vhi, type thi)
{
    lp_desc *lpd; 
    double bds[2];
    int j;

    Check_Integer(tj);
    Check_Float(thi);
    Check_Float(tlo);
    LpDescOnly(vlp, tlp, lpd);
    j = vj.nint;

    if (j >= lpd->mac) { Bip_Error(RANGE_ERROR); }

    bds[0] = Dbl(vlo);
    bds[1] = Dbl(vhi);
    if (bds[0] > bds[1]) { Fail; }
    if (bds[1] > CPX_INFBOUND)  bds[1] =  CPX_INFBOUND;
    if (bds[0] < -CPX_INFBOUND) bds[0] = -CPX_INFBOUND;

    if (bds[0] == bds[1]) 
    {
	Log2({
	    int j = %d;\n\
	    double bds = %.15e;\n\
	    CPXchgbds(cpx_env, lpd->lp, 1, &j, "B", &bds);
	}, j, bds[0]);

	CPXchgbds(cpx_env, lpd->lp, 1, &j, "B", bds);
    }
    else
    {
	int idx[2];
	idx[0] = j;
	idx[1] = j;
	Log4({
	    int idx[2];\n\
	    double bds[2];\n\
            idx[0] = %d;\n\
	    idx[1] = %d;\n\
            bds[0] = %.15e;\n\
            bds[1] = %.15e;\n\
	    CPXchgbds(cpx_env, lpd->lp, 2, idx, "LU", bds);
	}, j, j, bds[0], bds[1]);

	CPXchgbds(cpx_env, lpd->lp, 2, idx, "LU", bds);
    }
    Succeed;
}

/* cplex_new_col_bound(CPH, J, Bound, Which) 
   updates the Which bound (-ve = lower, 0 = both, +ve = upper) of an
   existing column J in the solver matrix
*/
int
p_cpx_new_col_bound(value vlp, type tlp, 
		    value vj, type tj, 
		    value vbd, type tbd, 
		    value vwhich, type twhich)
{
    lp_desc *lpd; 
    double newbd, lo0, hi0;
    int j;
    char which[1]; 

    Check_Integer(tj);
    Check_Integer(twhich);
    Check_Float(tbd);
    LpDesc(vlp, tlp, lpd);

    which[0] =  (char) 0;
    j = vj.nint;

    if (j >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    newbd = Dbl(vbd);

    Get_Col_Bounds(j, lo0, hi0);
    if (vwhich.nint <= 0) /* lower bound */
    {
	if (newbd > hi0) { Fail;}
	if (newbd > lo0) which[0] = 'L';
    }
    if (vwhich.nint >= 0) /* upper bound */
    {
	if (newbd < lo0) { Fail; }
	if (newbd < hi0) which[0] = (which[0] == 'L' ? 'B' : 'U');
    }
    if (which[0] != 0) 
    {
	Log3({
	    int j = %d;\n\
	    double newbd = %.15e;\n\
	    CPXchgbds(cpx_env, lpd->lp, 1, &j, "%c", &newbd);
	}, j, newbd, which[0]);

	CPXchgbds(cpx_env, lpd->lp, 1, &j, which, &newbd);
#ifdef HAS_SAMEBOUNDSBUG
	/* workaround for Xpress 14 bug: if both bounds are set to the same
           value, then the solution value can be returned incorrectly as 0.0.
           Workaround: create a small interval around the actual bound so
           that the two bounds are not the same
	*/
	if (which[0] == 'B')
	{/* 1e-8 seems small enough and works */
	    newbd += 1e-8;
	    CPXchgbds(cpx_env, lpd->lp, 1, &j, "U", &newbd);
	    newbd -= 2e-8;
	    CPXchgbds(cpx_env, lpd->lp, 1, &j, "L", &newbd);
	}
#endif	    
    }

    Succeed;
}


int
p_cpx_new_obj_coeff(value vlp, type tlp, value vj, type tj, value vcoeff, type tcoeff)
{
    lp_desc *lpd; 
    double coeff;
    int i;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tj);
    if (vj.nint >= lpd->mac)
       { Bip_Error(RANGE_ERROR); }

    if (IsInteger(tcoeff)) {
	coeff = (double) vcoeff.nint;
    } else {
	Check_Float(tcoeff);
	coeff = Dbl(vcoeff);
	Check_Constant_Range(coeff);
    }
    if (lpd->cb_cnt >= lpd->cb_sz)	/* grow arrays if necessary */
    {
	_grow_cb_arrays(lpd, 0);
    }
    i = lpd->cb_cnt++;
    lpd->cb_index[i] = vj.nint;
    lpd->cb_value[i] = coeff;
    Succeed;
}

int
p_cpx_flush_obj(value vlp, type tlp)
{
    lp_desc *lpd; 
    LpDesc(vlp, tlp, lpd);
    if (lpd->cb_cnt == 0)
    {
    	Succeed;
    }
    SetPreSolve(lpd->presolve);
    Mark_Copy_As_Modified(lpd);
#ifdef LOG_CALLS
    {
	int i;
	for (i=0; i<lpd->cb_cnt; ++i)
	{
	Fprintf(log_output_, "\n\
	    lpd->cb_index[%d] = %d;\n\
	    lpd->cb_value[%d] = %.15e;",
	    	i, lpd->cb_index[i], i, lpd->cb_value[i]);
	}
	Log1(CPXchgobj(cpx_env, lpd->lp, %d, lpd->cb_index, lpd->cb_value), lpd->cb_cnt);
    }
#endif
    if (CPXchgobj(cpx_env, lpd->lp, lpd->cb_cnt, lpd->cb_index, lpd->cb_value))
    {
	Bip_Error(EC_EXTERNAL_ERROR);
    }
    lpd->cb_cnt = 0;
    Succeed;
}

int
p_cpx_new_qobj_coeff(value vlp, type tlp, 
		     value vi, type ti, 
		     value vj, type tj, 
		     value vcoeff, type tcoeff)
{
    lp_desc *lpd; 
    double coeff;

    Check_Integer(ti);
    Check_Integer(tj);
    LpDesc(vlp, tlp, lpd);
    if (vj.nint >= lpd->mac || vi.nint >= lpd->mac)
       { Bip_Error(RANGE_ERROR); }

    if (IsInteger(tcoeff)) {
	coeff = (double) vcoeff.nint;
    } else {
	Check_Float(tcoeff);
	coeff = Dbl(vcoeff);
	Check_Constant_Range(coeff);
    }
    if (vi.nint==vj.nint)
    	coeff *= 2;
    SetPreSolve(lpd->presolve);
    Log3(CPXchgqpcoef(cpx_env, lpd->lp, %d,%d,%.15e), vi.nint, vj.nint, coeff);   
    if (CPXchgqpcoef(cpx_env, lpd->lp, vi.nint, vj.nint, coeff))
    {
	Bip_Error(EC_EXTERNAL_ERROR);
    }
    Succeed;
}


int
p_cpx_change_obj_sense(value vlp, type tlp, value vsense, type tsense)
{
    lp_desc *lpd; 

    Check_Integer(tsense);
    LpDesc(vlp, tlp, lpd);

    SetPreSolve(lpd->presolve);
    lpd->sense = vsense.nint;
    CPXchgobjsen(cpx_env, lpd->lp, vsense.nint);
#ifdef SOLVE_MIP_COPY
    if (lpd->copystatus != XP_COPYOFF) Mark_Copy_As_Modified(lpd);
#endif
    Succeed;
}

/*----------------------------------------------------------------------*
 * Initial matrix setup
 *----------------------------------------------------------------------*/

int
p_cpx_set_matbeg(value vlp, type tlp, 
		 value vj, type tj, 
		 value vk, type tk, 
		 value vk1, type tk1)
{
    lp_desc *lpd; 
    int j;

    Check_Integer(tk);
    Check_Integer(tk1);
    Check_Integer(tj);
    j = vj.nint;
    LpDescOnly(vlp, tlp, lpd);

    if (j >= lpd->mac || j < 0) { Bip_Error(RANGE_ERROR); }
    if (j >= lpd->macadded) j -= lpd->macadded; /* added col */
    lpd->matbeg[j] = vk.nint;
    lpd->matcnt[j] = vk1.nint - vk.nint;
    Succeed;
}

int
p_cpx_set_matval(value vlp, type tlp, 
		 value vk, type tk, 
		 value vi, type ti, 
		 value vval, type tval)
{
    lp_desc *lpd; 
    Check_Integer(tk);
    Check_Integer(ti);
    Check_Number(tval);
    LpDescOnly(vlp, tlp, lpd);

    if (vk.nint >= lpd->matnz || vk.nint < 0 ||
	vi.nint >= lpd->mar || vi.nint < SOLVER_MAT_BASE)
    	{ Bip_Error(RANGE_ERROR); }
    lpd->matind[vk.nint] = vi.nint;
    lpd->matval[vk.nint] = DoubleVal(vval, tval);
    Check_Constant_Range(lpd->matval[vk.nint]);
    Succeed;
}

int
p_cpx_loadprob(value vlp, type tlp)
{
    int err;
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);

    SetPreSolve(lpd->presolve);
    lpd->start_mac = lpd->mac; 

#ifndef HAS_MIQP
    if (lpd->prob_type == PROBLEM_MIQP)
    {
	Fprintf(Current_Error, "Eplex error: this solver does not support solving of quadratic MIP problems.\n");
	ec_flush(Current_Error);
	Bip_Error(UNIMPLEMENTED);
    }
#endif
#ifdef LOG_CALLS
    {
    int i;
# ifndef DUMPMAT
#  ifdef XPRESS
    Fprintf(log_output_, "\n\
	lpd->probname = (char *) malloc(16*sizeof(char));\n\
	strcpy(lpd->probname, \"eclipse\");"
    );
#  endif
#  ifdef CPLEX
    Log1(lpd->sense = %d, lpd->sense);
#  endif
    Fprintf(log_output_, "\n\
	lpd->sense = %d;\n\
    	lpd->macsz = %d;\n\
    	lpd->marsz = %d;\n\
    	lpd->matnz = %d;\n\
    	lpd->mac = %d;\n\
    	lpd->mar = %d;\n\
	lpd->rhsx = (double *) malloc(lpd->marsz * sizeof(double));\n\
	lpd->senx = (char *) malloc(lpd->marsz * sizeof(char));\n\
	lpd->matbeg = (int *) malloc((lpd->macsz+1) * sizeof(int));\n\
	lpd->matcnt = (int *) malloc(lpd->macsz * sizeof(int));\n\
	lpd->matind = (int *) malloc(lpd->matnz * sizeof(int));\n\
	lpd->matval = (double *) malloc(lpd->matnz * sizeof(double));\n\
	lpd->bdl = (double *) malloc(lpd->macsz * sizeof(double));\n\
	lpd->bdu = (double *) malloc(lpd->macsz * sizeof(double));\n\
	lpd->objx = (double *) malloc(lpd->macsz * sizeof(double));\n\
	lpd->ctype = (char *) malloc(lpd->macsz * sizeof(char));",
	lpd->sense,(lpd->macsz ? lpd->macsz : 1), (lpd->marsz ? lpd->marsz: 1), (lpd->matnz? lpd->matnz : 1),
	lpd->mac, lpd->mar);

    for (i=0; i<lpd->mac; ++i)
    {
	Fprintf(log_output_, "\n\tlpd->objx[%d] = %.15e;", i, lpd->objx[i]);
	Fprintf(log_output_, "\n\tlpd->bdl[%d] = %.15e;", i, lpd->bdl[i]);
	Fprintf(log_output_, "\n\tlpd->bdu[%d] = %.15e;", i, lpd->bdu[i]);
	Fprintf(log_output_, "\n\tlpd->matbeg[%d] = %d;", i, lpd->matbeg[i]);
	Fprintf(log_output_, "\n\tlpd->matcnt[%d] = %d;", i, lpd->matcnt[i]);
    }
    for (i=0; i<lpd->mar; ++i)
    {
	Fprintf(log_output_, "\n\tlpd->rhsx[%d] = %.15e;", i, lpd->rhsx[i]);
	Fprintf(log_output_, "\n\tlpd->senx[%d] = '%c';", i, lpd->senx[i]);
    }
    for (i=0; i<lpd->matnz; ++i)
    {
	Fprintf(log_output_, "\n\tlpd->matind[%d] = %d;", i, lpd->matind[i]);
	Fprintf(log_output_, "\n\tlpd->matval[%d] = %.15e;", i, lpd->matval[i]);
    }
# else  /* DUMPMAT */
    dump_problem(lpd);
# endif
    }
#endif /* LOG_CALLS */


#ifdef CPLEX
    lpd->lp = NULL;

    CallN(lpd->lp = CPXcreateprob(cpx_env, &err, "eclipse"));

    if (lpd->lp == NULL)
    { 
	if (err == CPXERR_NO_ENVIRONMENT) {
	    Fprintf(Current_Error, "Unable to create problem in CPLEX: licensing problem?\n");
	    ec_flush(Current_Error);
	}
	Bip_Error(EC_EXTERNAL_ERROR); 
    }
#endif
#ifdef COIN
    CallN(coin_create_prob(&(lpd->lp), cpx_env));
#endif
#if defined(CPLEX) || defined(COIN)
    CallN(lpd->lpcopy = lpd->lp);  /* no need for a copy in CPLEX */
    Call(err, CPXcopylp(cpx_env, lpd->lp, lpd->mac, lpd->mar,
    		lpd->sense, lpd->objx, lpd->rhsx, lpd->senx,
		lpd->matbeg, lpd->matcnt, lpd->matind, lpd->matval,
		lpd->bdl, lpd->bdu, NULL));
    if (err)
	{ Bip_Error(EC_EXTERNAL_ERROR); }

    if (IsMIPProb(lpd->prob_type))
    {
# if defined(LOG_CALLS) 
/* no need to log for XPRESS as ctype array not used directly */
	{ int i;
	    for (i=0; i<lpd->mac; ++i)
	    {
		Fprintf(log_output_, "\n\tlpd->ctype[%d] = '%c';", i, lpd->ctype[i]);
	    }
	}
# endif
	Call(err, CPXcopyctype(cpx_env, lpd->lp, lpd->ctype));
	if (err)
	    { Bip_Error(EC_EXTERNAL_ERROR); }
    }

    if (lpd->nsos)
    {
	/* macro mapping to version depending CPXcopysos() */
	if (CPXcopysos_(cpx_env, lpd->lp, lpd->nsos, lpd->nsosnz, lpd->sostype,
		lpd->sosbeg, lpd->sosind, lpd->sosref))
	    { Bip_Error(EC_EXTERNAL_ERROR); }
    }
    if IsQPProb(lpd->prob_type)
    {
# ifdef HAS_QUADRATIC
	int i;
#  ifdef HAS_MIQP
	int ptype = (lpd->prob_type == PROBLEM_QP ? CPXPROB_QP : CPXPROB_MIQP);
#  else
	int ptype = CPXPROB_QP;
#  endif
	if (CPXgetprobtype(cpx_env, lpd->lp) != ptype)
	{
	    Call(err, CPXchgprobtype(cpx_env, lpd->lp, ptype));
	    if (err != 0)
	    { Bip_Error(EC_EXTERNAL_ERROR); }
	}
#  ifdef CPLEX
	for (i=0; i<lpd->cb_cnt; ++i)
	{
	    Log3(CPXchgqpcoef(cpx_env, lpd->lp, %d, %d, %f),
		lpd->cb_index[i], lpd->cb_index2[i], lpd->cb_value[i]);
		
	    if (CPXchgqpcoef(cpx_env, lpd->lp, lpd->cb_index[i],
				    lpd->cb_index2[i], lpd->cb_value[i]))
		{ Bip_Error(EC_EXTERNAL_ERROR); }
	}
	lpd->cb_cnt = 0;
#  elif defined(COIN)
	coin_set_qobj(lpd->lp, lpd->mac, lpd->cb_cnt, lpd->cb_index, lpd->cb_index2, lpd->cb_value);
#  endif

# else /* !HAS_QUADRATIC */
	Fprintf(Current_Error, "Eplex error: Quadratic problems not supported for this solver!\n");
	ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
# endif
    }
    
#endif /* CPLEX || COIN */
#ifdef XPRESS

   Call(err, XPRScreateprob(&lpd->lp));
   if (lpd->copystatus != XP_COPYOFF)
   {
       Mark_Copy_As_Modified(lpd);
       if (IsMIPProb(lpd->prob_type))
       {
	   if (err == 0) { Call(err, XPRScreateprob(&lpd->lpcopy)); }
       }
       else
	   CallN(lpd->lpcopy = lpd->lp);
   }
   else
       CallN(lpd->lpcopy = lpd->lp);

    if (err && (err != 32/*student version*/)) 
    { 
	char errmsg[256];
        XPRSgetlasterror(lpd->lp, errmsg);
	Fprintf(Current_Error, "Eplex error: %s\n", errmsg);
	ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR); 
    }

    CallN(XPRScopycontrols(lpd->lp, cpx_env));
    if (lpd->presolve == 1) 
    {
	CallN(XPRSsetintcontrol(lpd->lp, XPRS_PRESOLVE, 1));
	/* 3 for XPRS_MIPPRESOLVE as recommended by Robert Ashford @ Dash */
	CallN(XPRSsetintcontrol(lpd->lp, XPRS_MIPPRESOLVE, 3));
    } 
    else 
    {
	CallN(XPRSsetintcontrol(lpd->lp, XPRS_PRESOLVE, 0));
	CallN(XPRSsetintcontrol(lpd->lp, XPRS_MIPPRESOLVE, 0));
    }
    /* this call back was done globally before version 13 */
    XPRSsetcbmessage(lpd->lp, eclipse_out, NULL);

    /* the problem is now always loaded with XPRSloadglobal() 
       as suggested by David Nielsen @ Dash 2004-09-28 
       For a quadratic problem, the quadratic terms are then added
     */
    if (lpd->ngents)    /* has integers */
    {
	int i,j;
	/* don't know whether these arrays can be temporary */
	Log1({
        lpd->ngents = %i;\n\
	lpd->nsos = 0;\n\
	lpd->sostype = NULL;\n\
	lpd->sosbeg = NULL;\n\
	lpd->sosind = NULL;\n\
	lpd->sosref = NULL;\n\
	}, lpd->ngents);
	CallN(lpd->qgtype = (char *)Malloc(lpd->ngents*sizeof(char)));
	CallN(lpd->mgcols = (int *)Malloc(lpd->ngents*sizeof(int)));
	for (i=0,j=0; i < lpd->mac; i++)
	{
	    if (lpd->ctype[i] != 'C')
	    {
		Log4({\n\
		lpd->qgtype[%i] = '%c';\n\
		lpd->mgcols[%i] = %i;\n\
		}, j, lpd->ctype[i], j, i);

		lpd->qgtype[j] = lpd->ctype[i];	/* 'B' or 'I' */
		lpd->mgcols[j++] = i;
	    }
	}
	/* correct the count, in case there were duplicates
	 * in the integer list (yes it happened...) */
	lpd->ngents = j;
	Log1(lpd->ngents = %i, j);
    }
    else
    {
	lpd->qgtype = NULL;
	lpd->mgcols = NULL;
    }

    Call(err, XPRSloadglobal(lpd->lp, lpd->probname,
	             lpd->mac, lpd->mar, lpd->senx, lpd->rhsx, NULL, lpd->objx,
		     lpd->matbeg, lpd->matcnt, lpd->matind, lpd->matval,
		     lpd->bdl, lpd->bdu,
		     lpd->ngents, lpd->nsos, lpd->qgtype, lpd->mgcols, NULL,
		     lpd->sostype, lpd->sosbeg, lpd->sosind, lpd->sosref));

    if (err) { Bip_Error(EC_EXTERNAL_ERROR); }

    if (lpd->cb_cnt) /* has quadratic objective terms */
    {
# ifdef LOG_CALLS
	int i;
	Fprintf(log_output_, "\n\tlpd->cb_cnt = %d;", lpd->cb_cnt);
	for(i=0; i< lpd->cb_cnt; ++i)
	{
	    Fprintf(log_output_, "\n\tlpd->cb_index[%d] = %d;", i, lpd->cb_index[i]);
	    Fprintf(log_output_, "\n\tlpd->cb_index2[%d] = %d;", i, lpd->cb_index2[i]);
	    Fprintf(log_output_, "\n\tlpd->cb_value[%d] = %.15e;", i, lpd->cb_value[i]);
	}
# endif
	Call(err, XPRSchgmqobj(lpd->lp, lpd->cb_cnt, 
	    lpd->cb_index, lpd->cb_index2, lpd->cb_value));

	lpd->cb_cnt = 0;
	if (err) { Bip_Error(EC_EXTERNAL_ERROR); }
    }



#endif /* XPRESS */

    /* free our copy of the problem */
    Free(lpd->rhsx); lpd->rhsx = NULL;
    Free(lpd->senx); lpd->senx = NULL;
    Free(lpd->matbeg); lpd->matbeg = NULL;
    Free(lpd->matcnt); lpd->matcnt = NULL;
    Free(lpd->matind); lpd->matind = NULL;
    Free(lpd->matval); lpd->matval = NULL;
    Free(lpd->bdl); lpd->bdl = NULL;
    Free(lpd->bdu); lpd->bdu = NULL;
    Free(lpd->ctype); lpd->ctype = NULL;
    Free(lpd->objx); lpd->objx = NULL;
    lpd->matnz = 0;
    lpd->macsz = 0;

    if (lpd->nsos)
    {
	Free(lpd->sosbeg); lpd->sosbeg = NULL;
	Free(lpd->sosind); lpd->sosind = NULL;
	Free(lpd->sosref); lpd->sosref = NULL;
	lpd->nsos = lpd->nsosnz = 0;
    }

    lpd->macadded = lpd->mac;
    Succeed;
}

/* add initial cutpool constraints to problem */
static int 
_setup_initial_cp_constraints(lp_desc * lpd, int add_all, int * unadded_cntp, 
			      int * cp_unadded, int * cp_map2)
{
    double * rhs, * rmatval;
    int * rmatbeg, * rmatind, i, offset, first = -1, 
	cp_rcnt2 = 0, rcnt = 0;
    char * senx;

                        
    rmatbeg = (int *)Malloc((lpd->cp_nr2+1)*sizeof(int));

    for (i=0; i < lpd->cp_nr2; i++) 
    {
	if (lpd->cp_active2[i] == 1) 
	{
	    if (lpd->cp_initial_add2[i] == 1 || add_all)
	    {/* active, added initially (or add all active constraints) */
		if (first == -1) 
		{
		    first = i;
		    offset = lpd->cp_rmatbeg2[first];
		}
		/* rmatbeg need to be offset from the start of array */
		rmatbeg[cp_rcnt2] = lpd->cp_rmatbeg2[i] - offset;
		cp_map2[i] = cp_rcnt2++;
		rcnt++;
		continue;
	    } else 
	    { /* active, but not added initially */
		cp_map2[i] = CSTR_STATE_NOTADDED; /* not added yet */
		cp_unadded[(*unadded_cntp)++] = i;
	    }
	} else
	{/* not active */
	    cp_map2[i] = CSTR_STATE_INACTIVE; /* not active */
	}

	if (rcnt > 0) 
	{/* there are some rows to add... */
	    rhs = &lpd->cp_rhsx2[first];
	    rmatind = &lpd->cp_rmatind2[offset];
	    rmatval = &lpd->cp_rmatval2[offset];
	    senx   =  &lpd->cp_senx2[first];
	    CPXaddrows(cpx_env, lpd->lp, 0, rcnt, 
		       (lpd->cp_rmatbeg2[i] - offset), 
		       rhs, senx, rmatbeg, rmatind, rmatval, NULL, NULL);
	    rcnt = 0;
	    first = -1;
	}

    }

    if (rcnt > 0) 
    {/* there are some rows to add... */
	rhs = &lpd->cp_rhsx2[first];
	rmatind = &lpd->cp_rmatind2[offset];
	rmatval = &lpd->cp_rmatval2[offset];
	senx   =  &lpd->cp_senx2[first];
	CPXaddrows(cpx_env, lpd->lp, 0, rcnt, 
		   (lpd->cp_nnz2 - offset), 
		   rhs, senx, rmatbeg, rmatind, rmatval, NULL, NULL);
	rcnt = 0;
	first = -1;
    }

    lpd->mar += cp_rcnt2;
    lpd->cp_nact2 = cp_rcnt2;
    if (cp_rcnt2 > 0) { Mark_Copy_As_Modified(lpd); }
    Free(rmatbeg);

    return 0;
}

/*----------------------------------------------------------------------*
 * Read/write
 *----------------------------------------------------------------------*/

static int
_lpwrite(lp_desc * lpd, int format, char * file)
{
    int res;

    switch (format)
    {
#if defined(CPLEX) || defined(COIN)
    case 0:
	Log1(CPXwriteprob(cpx_env, lpd->lp, "%s", "LP"), file);
	res = CPXwriteprob(cpx_env, lpd->lp, file, "LP");
	break;
    case 1:
	Log1(CPXwriteprob(cpx_env, lpd->lp, "%s", "MPS"), file);
	res = CPXwriteprob(cpx_env, lpd->lp, file, "MPS");
	break;
    case 2:
    case 8: /* compatibility with Xpress */
	Log1(CPXwriteprob(cpx_env, lpd->lp, "%s", "SAV"), file);
	res = CPXwriteprob(cpx_env, lpd->lp, file, "SAV");
	break;
    case 4:
    {
	double objoff;
	res = CPXpreslvwrite(cpx_env, lpd->lp, file, &objoff);
	break;
    }
    case 3:
	res = CPXmbasewrite(cpx_env, lpd->lp, file);
	break;
# if defined(CPLEX) && CPLEX < 10
    /* removed in CPLEX 10 */
    case 5:
	res = CPXtreewrite(cpx_env, lpd->lp, file);
	break;
# endif
    case 6:
	res = CPXembwrite(cpx_env, lpd->lp, file);
	break;
#endif
#ifdef XPRESS
    case 0:
    case 1:
    {
	char *flag = format  == 0 ? "lp" : "p";

	if (lpd->copystatus == XP_COPYCURRENT) 
	{
	    Log2(XPRSwriteprob(lpd->lpcopy, "%s", "%s"), file, flag);
	    res = XPRSwriteprob(lpd->lpcopy, file, flag);
	} else
	{
	    Log2(XPRSwriteprob(lpd->lp, "%s", "%s"), file, flag);
	    res = XPRSwriteprob(lpd->lp, file, flag);
	}
	if (res == 352)		/* not authorised in this version */
	    res = 0;		/* ignore */
	break;
    }
    case 8:
    case 2: /* compatibility with CPLEX */
	if (strcmp(file, "") != 0) 
	{/* give a warning if a file name was given */
	    Fprintf(Current_Error, "Eplex warning: filename %s ignored, as"
		    " filenames cannot be given for Xpress MP's .svf format. "
		    "The problem name %s is used instead.\n",
		    file, lpd->probname);
	    ec_flush(Current_Error);
	}

	if (lpd->copystatus == XP_COPYCURRENT) 
	{
	    Call(res, XPRSsave(lpd->lpcopy));
	} else
	{
	    Call(res, XPRSsave(lpd->lp));
	}
	break;
#endif
#ifdef COIN
#endif
    default:
	return -1;
    }

    return res;
}


int
p_cpx_lpwrite(value vfile, type tfile, value vformat, type tformat, 
	      value vlp, type tlp)
{
    lp_desc *lpd; 
    char has_cp = 0, *file;
    int oldmar, res;
    Check_Integer(tformat);
    Get_Name(vfile, tfile, file);
    LpDesc(vlp, tlp, lpd);

    oldmar = lpd->mar;

    SetPreSolve(lpd->presolve);
    if (lpd->cp_nr2 > 0)
    {
	int unadded_cnt = 0, * cp_unadded, * cp_map2;

	cp_unadded = (int *)Malloc(lpd->cp_nr2*sizeof(int));
	cp_map2 = (int *)Malloc(lpd->cp_nr2*sizeof(int));
	if (_setup_initial_cp_constraints(lpd, 1, &unadded_cnt, cp_unadded, cp_map2) == -1) 
	{ 
	    reset_rowcols(lpd, oldmar, lpd->mac);
	    Bip_Error(RANGE_ERROR);
	}
    }
    res = _lpwrite(lpd, vformat.nint, file);
    reset_rowcols(lpd, oldmar, lpd->mac);
    if (res == 0) 
    {
	Succeed;
    } else 
    {
	Bip_Error(EC_EXTERNAL_ERROR);
    }
}

int
p_cpx_lpread(value vfile, type tfile, 
	     value vformat, type tformat, 
	     value vhandle, type thandle)
{
    lp_desc *lpd;
    char *file;
    int res;
#if defined(CPLEX) || defined(XPRESS)
    if (!cpx_env)
    {
	Bip_Error(EC_LICENSE_ERROR);
    }
#endif

    Check_Integer(tformat);
    Check_Structure(thandle);
    Get_Name(vfile, tfile, file);

    CallN((lpd =  (lp_desc *) Malloc(sizeof(lp_desc))));
    /*CallN(_clr_lp_desc(lpd));*/
    CallN(memset(lpd, 0, sizeof(lp_desc)));
    /* the logged code needs to be hand-adjusted to put file in scope */
    Log1({char *file = "%s";}, file); 

#ifdef USE_PROBLEM_ARRAY
    Log1(lpdmat[%d] = lpd, next_matno);
    current_matno = next_matno;
    lpd->matno = next_matno++;
#endif


#if defined(CPLEX) || defined(COIN)
# ifdef CPLEX
    lpd->lp = CPXcreateprob(cpx_env, &res, "eclipse");
    if (lpd->lp == NULL)
	{ Bip_Error(EC_EXTERNAL_ERROR); }
# endif
# ifdef COIN
    Log0(coin_create_prob(&lpd->lp, cpx_env));
    if (coin_create_prob(&(lpd->lp), cpx_env)) { Bip_Error(EC_EXTERNAL_ERROR); }
# endif

    CallN(lpd->lpcopy = lpd->lp);  /* no need for a copy in CPLEX */

    switch (vformat.nint)
    {
    case 0:
	res = CPXreadcopyprob(cpx_env, lpd->lp, file, "LP");
	break;
    case 1:
	res = CPXreadcopyprob(cpx_env, lpd->lp, file, "MPS");
	break;
    case 2:
    case 8: /* compatibility with Xpress */
	res = CPXreadcopyprob(cpx_env, lpd->lp, file, "SAV");
	break;
    default:
	res = CPXreadcopyprob(cpx_env, lpd->lp, file, NULL);
	break;
    }
    if (res) { Bip_Error(EC_EXTERNAL_ERROR); }

    lpd->sense = CPXgetobjsen(cpx_env, lpd->lp) == CPX_MIN ? SENSE_MIN : SENSE_MAX;
    lpd->macadded = lpd->mac = CPXgetnumcols(cpx_env, lpd->lp);
    lpd->mar = CPXgetnumrows(cpx_env, lpd->lp);

    switch(CPXgetprobtype(cpx_env, lpd->lp))
    {
    case CPXPROB_MILP:
	lpd->prob_type = PROBLEM_MIP;
	break;
    case CPXPROB_QP:
	lpd->prob_type = PROBLEM_QP;
	break;
# ifdef HAS_MIQP
    case CPXPROB_MIQP:
	lpd->prob_type = PROBLEM_MIQP;
	break;
# endif
    default:
    	lpd->prob_type = PROBLEM_LP;
	break;
    }
#endif /* CPLEX || COIN */
#ifdef XPRESS
    if (strlen(file) > XP_PROBNAME_MAX)
    {
	Fprintf(Current_Error, "Eplex error: filename for problem is too"
		" long for Xpress MP.\n");
	ec_flush(Current_Error);
	Bip_Error(RANGE_ERROR);
    }
	
    Call(res, XPRScreateprob(&lpd->lp));
    if (res) { Bip_Error(EC_EXTERNAL_ERROR); }
    /* set the defaults *before* reading in the problem */
    CallN(XPRScopycontrols(lpd->lp, cpx_env)); 

    XPRSsetcbmessage(lpd->lp, eclipse_out, NULL);

    switch (vformat.nint)
    {
    case 2: /* compatibility with CPLEX */
    case 8:
	Call(res, XPRSrestore(lpd->lp, file));
	break;
    case 0:
	Call(res, XPRSreadprob(lpd->lp, file, "l"));
	break;
    case 1:
    default:
	Call(res, XPRSreadprob(lpd->lp, file, ""));
	break;
    }
    /* need to check for possible errors as XPress returns 0 for the
       read functions even if a problem occurred.
       This was suggested by Oliver Bastert @ DASH, 2004-12-08
    */
    if (res == 0) { XPRSgetintattrib(lpd->lp, XPRS_ERRORCODE, &res); }
    if (res) { Bip_Error(EC_EXTERNAL_ERROR); }

    lpd->sense = SENSE_MIN; /* Xpress ignores the sense! */
    CallN(XPRSgetintattrib(lpd->lp, XPRS_COLS, &(lpd->mac)));
    CallN(XPRSgetintattrib(lpd->lp, XPRS_ROWS, &(lpd->mar)));
    CallN(lpd->probname = (char *) Malloc((strlen(file) + 1) * sizeof(char)));
    CallN(strcpy(lpd->probname, file));
    {
	int nmips, nq;

	XPRSgetintattrib(lpd->lp, XPRS_MIPENTS, &nmips);
	XPRSgetintattrib(lpd->lp, XPRS_QELEMS, &nq);
	if (nq > 0)
	{
	    lpd->prob_type = (nmips == 0 ? PROBLEM_QP : PROBLEM_MIQP);
	}
	else
	{
	    lpd->prob_type = (nmips == 0 ? PROBLEM_LP : PROBLEM_MIP);
	}
    }

    lpd->copystatus = XP_COPYINVALID;

    if (lpd->copystatus != XP_COPYOFF)
    {
	Mark_Copy_As_Modified(lpd);
	if (IsMIPProb(lpd->prob_type))
	{
	    Call(res, XPRScreateprob(&lpd->lpcopy));
	    if (res) { Bip_Error(EC_EXTERNAL_ERROR); }
	}
	else
	    CallN(lpd->lpcopy = lpd->lp);
    }
    else
	CallN(lpd->lpcopy = lpd->lp);

#endif /* XPRESS */
    lpd->descr_state = DESCR_LOADED;

    {/* Return the cplex descriptor in argument HANDLE_CPH of the handle structure. */
	vhandle.ptr[HANDLE_CPH] = ec_handle(&lp_handle_tid, lpd);
	Make_Stamp(vhandle.ptr+HANDLE_STAMP); /* needed for other trail undos */
    }

    lpd->macadded = lpd->mac;
    Succeed;
}

pword *
_create_result_carray(value vhandle, int pos, int size)
{
    pword *argp = &vhandle.ptr[pos];

    Dereference_(argp);

    if (IsRef(argp->tag))
	return NULL;
    else
    {
	return _create_carray(size);
    }
}

pword *
_create_result_darray(value vhandle, int pos, int size)
{
    pword *argp = &vhandle.ptr[pos];

    Dereference_(argp);

    if (IsRef(argp->tag))
	return NULL;
    else
    {
	return _create_darray(size);
    }
}

pword *
_create_result_iarray(value vhandle, int pos, int size)
{
    pword *argp = &vhandle.ptr[pos];

    Dereference_(argp);

    if (IsRef(argp->tag))
	return NULL;
    else
    {
	return _create_iarray(size);
    }
}

/*----------------------------------------------------------------------*
 * Accessing Infeasible information
 *----------------------------------------------------------------------*/

#ifdef SUPPORT_IIS 
static int
_get_iis(lp_desc * lpd, int * nrowsp, int * ncolsp, int * rowidxs, int * colidxs, char * colstats)
{
    int status;
    int i;
# ifdef CPLEX
    int * rowstatbuf, * colstatbuf;

    rowstatbuf = Malloc(sizeof(int) * *nrowsp);
    colstatbuf = Malloc(sizeof(int) * *ncolsp);

# endif

    Get_Conflict(lpd->lp, status, rowidxs, rowstatbuf, nrowsp, colidxs, colstatbuf, ncolsp);

# ifdef CPLEX
    switch (status)
    {
    case CPX_STAT_CONFLICT_MINIMAL:
	for(i=0;i<*ncolsp;i++)
	{
	    switch (colstatbuf[i])
	    {
# ifdef HAS_GENERAL_CONFLICT_REFINER
	    case CPX_CONFLICT_MEMBER:
		colstats[i] = 'b';
		break;
#  endif
	    case CPX_CONFLICT_LB:
		colstats[i] = 'l';
		break;
	    case CPX_CONFLICT_UB:
		colstats[i] = 'u';
		break;
	    default:
		colstats[i] = 'x';
		break;
	    }
	}
	Free(rowstatbuf);
	Free(colstatbuf);
	break;
    default:
	Free(rowstatbuf);
	Free(colstatbuf);
	return -1;
	break;
#  ifdef HAS_GENERAL_CONFLICT_REFINER
    case CPX_STAT_CONFLICT_FEASIBLE:
	/* An infeaible problem can return CONFLICT_FEASIBLE, with no conflict set, probably because
	   problem is near feasible.
	*/
	*nrowsp = 0;
	*ncolsp = 0;
	Free(rowstatbuf);
	Free(colstatbuf);
	return 1;
	break;
#  endif
    }
# endif
# ifdef XPRESS
    if (!status) {
	for(i=0;i<*ncolsp;i++) colstats[i] = 'x';
    } else {
	return -1;
    }
# endif

    return 0;
}

#endif /* SUPPORT_IIS */

int
p_cpx_infeas_info(value vlp, type tlp, 
		  value vnrows, type tnrows, 
		  value vncols, type tncols, 
		  value vrowidx, type trowidx, 
		  value vcolidx, type tcolidx, 
		  value vcolstat, type tcolstat)
{
#ifdef SUPPORT_IIS
    int cnrows, cncols, status;
    pword * rowidxbuf, * colidxbuf, * colstatbuf;
    lp_desc *lpd;
    int res;
    pword * old_tg;
# ifdef CPLEX
    int * rowstat, * colstat;
# endif
    LpDescOnly(vlp, tlp, lpd);
    Prepare_Requests

    if (lpd->descr_state == DESCR_SOLVED_NOSOL) 
    {
	/* it seems that for CPLEX 10, if the find conflct call is made for a sp;ved problem,
           the function can succeed without error but does not set the size of the rows and columns!
	   So we only call find conflict for infeasible problems
	*/
	Find_Conflict(res, lpd->lp, cnrows, cncols);
	if (res) { Bip_Error(EC_EXTERNAL_ERROR); }

	old_tg = TG;
	rowidxbuf  = _create_iarray(cnrows);
	colidxbuf =  _create_iarray(cncols); 
	colstatbuf = _create_carray(cncols);
	res = _get_iis(lpd, &cnrows, &cncols, IArrayStart(rowidxbuf), IArrayStart(colidxbuf), CArrayStart(colstatbuf));

	switch (res)
	{
	case 1:
	    /* no conflict set, reset TG and create zero length arrays instead */
	    TG = old_tg;
	    rowidxbuf = _create_iarray(0);
	    colidxbuf = _create_iarray(0);
	    colstatbuf = _create_carray(0);
	    break;
	case -1:
	    /* error in trying to obtain IIS */
	    Fail;
	    break;
	}

    } else
    {/* lpd->desc_state != DESCR_SOLVED_NOSOL */
	cnrows = 0;
	cncols = 0;
	rowidxbuf  = _create_iarray(cnrows);
	colidxbuf =  _create_iarray(cncols); 
	colstatbuf = _create_carray(cncols);
    }

    Request_Unify_Integer(vnrows, tnrows, cnrows);
    Request_Unify_Integer(vncols, tncols, cncols);
    Request_Unify_String(vrowidx, trowidx, rowidxbuf);
    Request_Unify_String(vcolidx, tcolidx, colidxbuf);
    Request_Unify_String(vcolstat, tcolstat, colstatbuf);

    Return_Unify;

#else

    Fprintf(Current_Error, "Eplex error: this solver does not support infeasibilty analysis.\n");
    ec_flush(Current_Error);
    Bip_Error(UNIMPLEMENTED);

#endif

}

/*----------------------------------------------------------------------*
 * Solve
 *----------------------------------------------------------------------*/

#ifdef XPRESS
int _mip_opt(lp_desc * lpd, char * meth_string, char * node_meth_string,
	     void * solution)
{
    int res;
    struct lp_sol *sol = (struct lp_sol *) solution;

    /* can use same probname as we are not going to solve using the original */
    if (lpd->copystatus != XP_COPYOFF)
    {
	/* XPRScopyprob() should cleanup any existing problem 
	   According to Michael Perregaard@DASH, XPRScopycontrols() is
	   no longer needed after an XPRScopyprob() in Xpress 14+, as
	   controls are copied by the copyprob
	*/
	CallN(XPRScopyprob(lpd->lpcopy, lpd->lp, lpd->probname));
	CallN(XPRScopycontrols(lpd->lpcopy, lpd->lp));
	CallN(XPRScopycallbacks(lpd->lpcopy, lpd->lp));
	lpd->copystatus = XP_COPYCURRENT;
    }
    else
    {/* this turns off operations which might delete columns, to avoid
	the problem of bounds being sent to the wrong column
     */       
	XPRSsetintcontrol(lpd->lp, XPRS_PRESOLVEOPS,86);
    }


    /* the global optimisation is broken down into getting the initial
       root LP relaxation and subsequent MIP so that the basis for 
       the root node can be obtained: this should be better than a solution
    */
    CallN(XPRSscale(lpd->lp, NULL, NULL));
    if (lpd->sense == SENSE_MIN) 
    {

	Log1(XPRSminim(lpd->lpcopy, "%s"), meth_string);
	res = XPRSminim(lpd->lpcopy, meth_string);
	if (res == 0)
	{
	    /* proceed with the global solve only if there is an optimal LP:
	       otherwise the XPRS_MIPSTATUS loses its XPRS_MIP_LP_NOT_OPTIMAL
               status when XPRSminim is called again
	    */
	    XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &lpd->sol_state);
	    if (lpd->sol_state != XPRS_MIP_LP_NOT_OPTIMAL)
	    {
		if (sol->cbase != NULL) 
		    XPRSgetbasis(lpd->lpcopy, sol->rbase, sol->cbase);
		Log1(XPRSminim(lpd->lpcopy, "%s"), node_meth_string);
		res = XPRSminim(lpd->lpcopy, node_meth_string);
	    }
	}
    }
    else
    {
	Log1(XPRSmaxim(lpd->lpcopy, "%s"), meth_string);
	res = XPRSmaxim(lpd->lpcopy, meth_string);
	if (res == 0)
	{
	    XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &lpd->sol_state);
	    if (lpd->sol_state != XPRS_MIP_LP_NOT_OPTIMAL)
	    {
		if (sol->cbase != NULL) 
		    XPRSgetbasis(lpd->lpcopy, sol->rbase, sol->cbase);
		Log1(XPRSmaxim(lpd->lpcopy, "%s"), node_meth_string);
		res = XPRSmaxim(lpd->lpcopy, node_meth_string);
	    }
	}
    }
    return res;
}
#endif /* XPRESS */

#ifdef CPLEX
int _lp_opt(lp_desc * lpd, int * meth, int * auxmeth)
{
    int res;
    /* meth/auxmeth needs to be updated to actual methods used */
    switch (*meth)
    {
    default:
	Fprintf(Current_Error, "Eplex warning: Specified LP method unavilable for"
		" solver. Using default instead.\n");
	ec_flush(Current_Error);
	*meth = METHOD_DEFAULT;
    case METHOD_DEFAULT:
#  ifdef CPX_HAS_LPOPT
    case METHOD_PRIMAL:
    case METHOD_DUAL:
	/* method specified by parameters set earlier */
	Call(res, CPXlpopt(cpx_env, lpd->lp)); break;
#  else
    case METHOD_DUAL:	Call(res, CPXdualopt(cpx_env, lpd->lp)); break;
    case METHOD_PRIMAL:	Call(res, CPXprimopt(cpx_env, lpd->lp)); break;
#  endif
    case METHOD_NET:
	switch (*auxmeth)
	{
	case METHOD_PRIMAL:	Call(res, CPXhybnetopt(cpx_env, lpd->lp, CPX_ALG_PRIMAL)); break;
	default:
	    Fprintf(Current_Error, "Eplex warning: Invalid Simplex"
		    " method selected for Network-Simplex.\nUsing default.\n");
	    *auxmeth = METHOD_DEFAULT;
	case METHOD_DEFAULT:
	    /* use CPXlpopt() if it exists, otherwise assume that the
	       default is dual Simplex
	    */
#ifdef CPX_HAS_LPOPT
	    Call(res, CPXlpopt(cpx_env, lpd->lp));
	    break;
#endif
	case METHOD_DUAL:	
	    Call(res, CPXhybnetopt(cpx_env, lpd->lp, CPX_ALG_DUAL));
	    break;
	}
	break;

    case METHOD_BAR:
	switch (*auxmeth)
	{
#ifndef HAS_NO_BARCROSSOVER
	case METHOD_NONE:
	    Call(res, CPXhybbaropt(cpx_env, lpd->lp, CPX_ALG_NONE));
	    break;
#endif
	case METHOD_PRIMAL:
	    Call(res, CPXhybbaropt(cpx_env, lpd->lp, CPX_ALG_PRIMAL));
	    break;
	case METHOD_DUAL:	
	    Call(res, CPXhybbaropt(cpx_env, lpd->lp, CPX_ALG_DUAL));
	    break;
	default:
	    Fprintf(Current_Error, "Eplex warning: Selected crossover"
		    " method for Barrier method unavailable.\nUsing default crossover.\n");
	    ec_flush(Current_Error);
	    *auxmeth = METHOD_DEFAULT;
	case METHOD_DEFAULT:
#ifdef CPX_HAS_LPOPT
	    Call(res, CPXlpopt(cpx_env, lpd->lp));
#else
	    Call(res, CPXbaropt(cpx_env, lpd->lp));
#endif
	    break;
	}
	break; 

#ifdef HAS_SIFT
    case METHOD_SIFT:
	Call(res, CPXlpopt(cpx_env, lpd->lp));
	break;
#endif
    }
    return res;
} 

#define TryMIPBeforeFixing(res) { \
    /* check if there is already a current optimal MI(Q)P solution */\
    lpd->sol_state = CPXgetstat(cpx_env, lpd->lp);   \
    if (!MIPSuccessState(lpd)) \
    { \
        /* solve the MIP problem first */\
	Call(res, CPXmipopt(cpx_env, lpd->lp)); \
	lpd->sol_itcnt = lpd->sol_nodnum = 0; \
	lpd->sol_itcnt = CPXgetmipitcnt(cpx_env, lpd->lp); \
	lpd->sol_nodnum = CPXgetnodecnt(cpx_env, lpd->lp); \
    } else { \
        res = 0; \
    } \
}

#endif /* CPLEX */

#ifdef XPRESS
void XPRS_CC _get_xpress_sol(XPRSprob lp, void *solution);

void XPRS_CC
_get_xpress_sol(XPRSprob lp, void * solution)
{
    struct lp_sol *sol = (struct lp_sol *) solution;

    XPRSgetsol(lp, sol->sols, sol->slacks, sol->pis, sol->djs);
}
#endif


static int 
_cstr_state(lp_desc * lpd, int row, char add_cp_cstr, double * sols)
{
    int lastarg, argpos;
    double lhs = 0.0, slack, tol;

    /* add_cp_cstr == 2 if unbounded result -- simply add all constraints
       in this case by returning violated state
    */
    if (add_cp_cstr == 2) return CSTR_STATE_VIOLATED;
    lastarg = (row < lpd->cp_nr2 - 1 ? lpd->cp_rmatbeg2[row+1] : lpd->cp_nnz2);
    for (argpos = lpd->cp_rmatbeg2[row] ; argpos < lastarg ; argpos++)
    {
	lhs += sols[lpd->cp_rmatind2[argpos]] * lpd->cp_rmatval2[argpos];
    }
    /* definition of slack for all row types except ranged rows, which we
       don't use
    */
    slack = lpd->cp_rhsx2[row] - lhs;
#ifdef CPLEX
    CPXgetdblparam(cpx_env, CPX_PARAM_EPRHS, &tol);
#endif
#ifdef XPRESS
    XPRSgetdblcontrol(lpd->lp, XPRS_FEASTOL, &tol);
#endif
#ifdef COIN
    coin_getdblparam(lpd->lp, OsiPrimalTolerance, &tol);
#endif
    switch (lpd->cp_senx2[row])
    {
    case 'L':
	return (slack<-tol ? CSTR_STATE_VIOLATED 
		: (slack<=tol ? CSTR_STATE_BINDING : CSTR_STATE_SAT));
	break;
    case 'G':
	return  (slack > tol ? CSTR_STATE_VIOLATED
		 : (slack >= -tol ? CSTR_STATE_BINDING : CSTR_STATE_SAT));
	break;
    case 'E':
	return (slack <= tol && slack >= -tol ? CSTR_STATE_BINDING
		: CSTR_STATE_VIOLATED);
	break;
    default:
	/* constraint type out of range */
	return CSTR_STATE_INVALID;
	break;
    }
}


/* cplex_optimise(Handle, SolveMethods, TimeOut, SetNext, OutputPos, 
                  OptResult, OptStatus, WorstBound, BestBound)

   optimises problem in Handle. Handle is needed to access the result
   arrays located in Handle by the OutputPos arguments.
   OptResult is the resulting status after the optimisation, OptStatus is
   the optimiser-dependent status returned by the optimiser. Worst and
   Best bounds are the bounds on the optimal solution determined by the
   solver.

   Any solution state must be extracted from the optimiser in this procedure,
   as it modifies the problem by first adding the cutpool constraints before 
   calling the optimiser and then removing them before exiting.
*/

int
p_cpx_optimise(value vhandle, type thandle, value vmeths, type tmeths, 
         value vtimeout, type ttimeout, value vdump, type tdump, 
	 value vout, type tout, value vres, type tres, value vstat, type tstat,
	 value vworst, type tworst, value vbest, type tbest) 
{
    lp_desc *lpd; 
    int res, oldmar;
    int meth, node_meth, auxmeth, node_auxmeth;
    int solspos, pispos, slackspos, djspos, cbasepos, rbasepos, cpcondmappos;
    int iis_rowspos, iis_colspos, iis_colstatspos;
    pword * pw, outsols, outpis, outslacks, outdjs, outcbase, outrbase;
    /* outdjs: when adding a solver, check to make sure that the reduced
       cost is of the same sign as what we defined (and what CPLEX and
       XPress uses). Reverse the signs before returning to ECLiPSe if 
       rquired!
    */
#ifdef XPRESS
    char *meth_string, *node_meth_string;
    struct lp_sol sol;
#endif
    char has_cp = 0; /* has cutpool constraints added */
    char add_cp_cstr = 0;
    char * file;
    double bestbound, worstbound;
    int * cp_unadded, last_violated_idx, violated_cnt, unadded_cnt = 0;
    int * cp_map2;
    int format;
    pword * old_tg;

    /*********************************************************************
     *     Type Checking and Initialisation                              *
     *********************************************************************/

    Prepare_Requests

    Check_Structure(thandle);
    Check_Structure(tmeths);
    Check_Structure(tout);
    Check_Number(ttimeout);

    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);

    if (lpd->descr_state == DESCR_EMPTY)
    {
	Fprintf(Current_Error, "Eplex optimise: empty handle\n");
	(void) ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    /* m(Method,AuxMeth,NodeMeth,NodeAuxMeth) */
    pw = &vmeths.ptr[1];
    Dereference_(pw);
    Check_Integer(pw->tag);
    meth = pw->val.nint;
    pw = &vmeths.ptr[2];
    Dereference_(pw);
    Check_Integer(pw->tag);
    auxmeth = pw->val.nint;
    pw = &vmeths.ptr[3];
    Dereference_(pw);
    Check_Integer(pw->tag);
    node_meth = pw->val.nint;
    pw = &vmeths.ptr[4];
    Dereference_(pw);
    Check_Integer(pw->tag);
    node_auxmeth = pw->val.nint;

    /* out(sols,pis,slacks,djs,cbase,rbase,cp_cond_map,iis_rows,iis_cols,iis_colstats) 
       must correspond to the out structure in the call to cplex_optimise in ECLiPSe!
    */
    pw = &vout.ptr[1];
    Dereference_(pw);
    Check_Integer(pw->tag);
    solspos = pw->val.nint;
    pw = &vout.ptr[2];
    Dereference_(pw);
    Check_Integer(pw->tag);
    pispos = pw->val.nint;
    pw = &vout.ptr[3];
    Dereference_(pw);
    Check_Integer(pw->tag);
    slackspos = pw->val.nint;
    pw = &vout.ptr[4];
    Dereference_(pw);
    Check_Integer(pw->tag);
    djspos = pw->val.nint;
    pw = &vout.ptr[5];
    Dereference_(pw);
    Check_Integer(pw->tag);
    cbasepos = pw->val.nint;
    pw = &vout.ptr[6];
    Dereference_(pw);
    Check_Integer(pw->tag);
    rbasepos = pw->val.nint;
    pw = &vout.ptr[7];
    Dereference_(pw);
    Check_Integer(pw->tag);
    cpcondmappos = pw->val.nint;
    pw = &vout.ptr[8];
    Dereference_(pw);
    Check_Integer(pw->tag);
    iis_rowspos = pw->val.nint;
    pw = &vout.ptr[9];
    Dereference_(pw);
    Check_Integer(pw->tag);
    iis_colspos = pw->val.nint;
    pw = &vout.ptr[10];
    Dereference_(pw);
    Check_Integer(pw->tag);
    iis_colstatspos = pw->val.nint;

    
    if (IsStructure(tdump)) 
    { /* write_before_solve(Format,File) */
	pw = &vdump.ptr[1];
	Dereference_(pw);
	Check_Integer(pw->tag);
	format = pw->val.nint;
	pw = &vdump.ptr[2];
	Dereference_(pw);
	Get_Name(pw->val, pw->tag, file);
    }

    SetPreSolve(lpd->presolve);

    oldmar = lpd->mar;
    if (lpd->cp_nr2 > 0)
    {

	pword map;

	map.val.ptr = _create_result_iarray(vhandle, cpcondmappos, lpd->cp_nr2);
	map.tag.kernel = TSTRG;

	cp_unadded = (int *)Malloc(lpd->cp_nr2*sizeof(int));
	cp_map2 = IArrayStart(map.val.ptr);

	if (_setup_initial_cp_constraints(lpd, 0, &unadded_cnt, cp_unadded, cp_map2) == -1) 
	{ 
	    reset_rowcols(lpd, oldmar, lpd->mac);
	    Bip_Error(RANGE_ERROR);
	}
	ec_assign(vhandle.ptr+cpcondmappos, map.val, map.tag);
	has_cp = 1;
    }


    outsols.tag.kernel = TSTRG;
    outdjs.tag.kernel = TSTRG;
    outcbase.tag.kernel = TSTRG;
    outrbase.tag.kernel = TSTRG;
    outpis.tag.kernel = TSTRG;
    outslacks.tag.kernel = TSTRG;

    outsols.val.ptr = _create_result_darray(vhandle, solspos,   lpd->mac);
#ifdef CPLEX
    if (!IsMIPProb(lpd->prob_type))
#endif
    {/* djs, basis, pis are available for non-MIP problems only for CPLEX;
        for XPRESS, the returned values are for the optimal LP node
     */
	outdjs.val.ptr = _create_result_darray(vhandle,   djspos, lpd->mac);
	outcbase.val.ptr = _create_result_iarray(vhandle, cbasepos, lpd->mac);
    }
    old_tg = TG; 
    /* allocate the row-wise arrays later as these may need to be expanded
       with the addition cutpool constraints
    */
    outslacks.val.ptr = _create_result_darray(vhandle, slackspos, lpd->mar);
#ifdef CPLEX
    if (!IsMIPProb(lpd->prob_type))
#endif
    {
	outrbase.val.ptr = _create_result_iarray(vhandle, rbasepos, lpd->mar);
	outpis.val.ptr = _create_result_darray(vhandle,   pispos, lpd->mar);
    }
#ifdef CPLEX
    else
    {/* not MIP, CPLEX */
	outpis.val.ptr = outdjs.val.ptr = outcbase.val.ptr = outrbase.val.ptr = NULL;
    }
#endif

#ifdef XPRESS 

    sol.sols = (outsols.val.ptr == NULL ? NULL : DArrayStart(outsols.val.ptr));
    sol.pis = (outpis.val.ptr == NULL ? NULL : DArrayStart(outpis.val.ptr));
    sol.slacks  = (outslacks.val.ptr == NULL ? NULL : DArrayStart(outslacks.val.ptr));
    sol.djs = (outdjs.val.ptr == NULL ? NULL : DArrayStart(outdjs.val.ptr));
    sol.cbase = (outcbase.val.ptr == NULL ? NULL : IArrayStart(outcbase.val.ptr));
    sol.rbase = (outrbase.val.ptr == NULL ? NULL : IArrayStart(outrbase.val.ptr));
    sol.mac = lpd->mac;

    Log6({
	sol.sols   = (double *) malloc(sizeof(double) * %d);\n\
	sol.pis    = (double *) malloc(sizeof(double) * %d);\n\
	sol.slacks = (double *) malloc(sizeof(double) * %d);\n\
	sol.djs    = (double *) malloc(sizeof(double) * %d);\n\
	sol.cbase   = (int *) malloc(sizeof(int) * %d);
	sol.rbase   = (int *) malloc(sizeof(int) * %d);
    }, lpd->mac, lpd->mar, lpd->mar, lpd->mac, lpd->mac, lpd->mar);

    /* set up call-back to get solution state at each integer solution */
# ifdef HAS_MIQP_CALLBACKBUG
    /* too much trouble to support MIQP with older XPRESS with this bug;
       the MIQP method is `not recommended' for use in these versions
       by DASH in anycase
    */
    switch (lpd->prob_type)
    {
    case PROBLEM_MIQP:
    case PROBLEM_FIXEDQ:
    case PROBLEM_RELAXEDQ:
	Fprintf(Current_Error, "Eplex error: quadratic MIP not support for this solver because it is unstable.\n");
	ec_flush(Current_Error);
	Bip_Error(UNIMPLEMENTED);
	break;
    }
# endif
    CallN(XPRSsetintcontrol(lpd->lp, XPRS_SOLUTIONFILE, 0));

    if (IsMIPProb(lpd->prob_type))
    {
	CallN(XPRSsetcbintsol(lpd->lp, _get_xpress_sol, (void *)&sol));
    }

    if (IsInteger(ttimeout) && vtimeout.nint != 0)
    {/* set timeout if non-zero */
	Log1(XPRSsetintcontrol(lpd->lp, XPRS_MAXTIME, %d), vtimeout.nint);
	XPRSsetintcontrol(lpd->lp, XPRS_MAXTIME, vtimeout.nint);
    }
    /* meth, auxmeth, node_meth are kept upto-date as they may be required
       to determine the actual method used to solve the problem
    */
    switch (meth)
    {
    case METHOD_DEFAULT:        meth_string = "";  break;
    case METHOD_PRIMAL:		meth_string = "p"; break;
    case METHOD_DUAL:		
	if (IsQPProb(lpd->prob_type))
	{
	    Fprintf(Current_Error, 
		    "Eplex warning: Dual Simplex method not available to solve a quadratic"
		    " problem for Xpress MP. Using Primal instead.\n");
	    ec_flush(Current_Error);
	    meth_string = "p";
	    meth = METHOD_PRIMAL;
	} else	meth_string = "d"; 
	break;
    case METHOD_BAR:
        meth_string = "b";
	switch (auxmeth)
	{
	default:
	case METHOD_DUAL:
	    Fprintf(Current_Error, 
		    "Eplex warning: Specified crossover method unavilable for"
		    " barrier method. Using Primal instead.\n");
	    ec_flush(Current_Error);
	case METHOD_DEFAULT:
	    auxmeth = METHOD_PRIMAL;
	case METHOD_PRIMAL:
	    /* Richard Laundy says primal Simplex is used to do crossover */
	    XPRSsetintcontrol(lpd->lp, XPRS_CROSSOVER, 1);
	    break;
	case METHOD_NONE:
	    switch (lpd->prob_type)
	    {
	    default:
		XPRSsetintcontrol(lpd->lp, XPRS_CROSSOVER, 0);
		break;
	    case PROBLEM_MIP:
	    case PROBLEM_FIXEDL:
	    case PROBLEM_MIQP:
	    case PROBLEM_FIXEDQ:
		/* according to Richard Laundy @ DASH, 2004-03-26,
                   turning off CROSSOVER for MIP case would lead to problems
		*/
		Fprintf(Current_Error, 
			"Eplex warning: For MIP problems, crossover must be "
                        "enabled for barrier method. Using Primal crossover.\n");
		ec_flush(Current_Error);
		XPRSsetintcontrol(lpd->lp, XPRS_CROSSOVER, 1);
		auxmeth = METHOD_PRIMAL;
		break;
	    }
	    break;
	}
	break;
    default:
	Fprintf(Current_Error, "Eplex warning: Specified method unavilable for"
		    " solver. Using default instead.\n");
	ec_flush(Current_Error);
	meth_string = "";
	meth = METHOD_DEFAULT;
	break;
    }

    /* ignore node_auxmeth */
    switch (node_meth)
    {
    case METHOD_DEFAULT:        node_meth_string = "g";  break;
    case METHOD_PRIMAL:		node_meth_string = "gp"; break;
    case METHOD_DUAL:		
	if (IsQPProb(lpd->prob_type))
	{
	    Fprintf(Current_Error, 
		    "Eplex warning: Dual Simplex method not available to solve a quadratic"
		    " problem for Xpress MP. Using Primal instead.\n");
	    ec_flush(Current_Error);
	    meth_string = "gp";
	    node_meth = METHOD_PRIMAL;
	} else	meth_string = "gd"; 
	break;
    case METHOD_BAR:            node_meth_string = "gb"; break;
    default:
	Fprintf(Current_Error, "Eplex warning: Specified node method unavilable for"
		    " solver. Using default instead.\n");
	ec_flush(Current_Error);
	node_meth_string = "";
	node_meth = METHOD_DEFAULT;
	break;
    }
#endif /* XPRESS */ 
#ifdef CPLEX
# if !defined(CPX_HAS_DEFAULTALG)
    /* the default method is dual Simplex in CPLEX 6+7, and we
       assume it here to avoid another switch statement 
    */
    if (meth == METHOD_DEFAULT) meth = METHOD_DUAL;
# endif
	
    /* set timeout. If no timeout was set, vtimeout is integer 0 */
    if (IsDouble(ttimeout) && Dbl(vtimeout) > 0.0)
    {
	Log1(CPXsetdblparam(cpx_env, CPX_PARAM_TILIM, %f), Dbl(vtimeout));
	CPXsetdblparam(cpx_env, CPX_PARAM_TILIM,  Dbl(vtimeout));
    } else
    {/* no timeouts, make sure it is set to the default no timeout value */
	CallN(CPXsetdblparam(cpx_env, CPX_PARAM_TILIM, 1e75));
    }
    switch (meth)
    {
    case METHOD_PRIMAL:
	SetCPXAlg(cpx_env, CPX_ALG_PRIMAL);
	break;
    case METHOD_DUAL:
	SetCPXAlg(cpx_env, CPX_ALG_DUAL);
	break;
    case METHOD_NET:
	SetCPXAlg(cpx_env, CPX_ALG_NET);
	break;
    case METHOD_BAR:
	SetCPXAlg(cpx_env, CPX_ALG_BARRIER);
	break;
# ifdef HAS_SIFT
    case METHOD_SIFT:
	SetCPXAlg(cpx_env, CPX_ALG_SIFTING);

	/* this assumes that the CPX_PARAM_SIFTALG can use the CPX_ALG_*
	   macros. This is not explicitly stated in the manual, even though
	   the macro values corresponds
	*/
	SetSiftAlg(cpx_env, auxmeth);
	break;
# endif
# ifdef CPX_HAS_DEFAULTALG
    default:
	Fprintf(Current_Error, "Eplex warning: Specified method unavilable for"
		    " solver. Using default instead.\n");
	ec_flush(Current_Error);
        meth = METHOD_DEFAULT;
    case METHOD_DEFAULT:
	SetCPXAlg(cpx_env, CPX_ALG_AUTOMATIC);
	break;
# else
    default:
	Fprintf(Current_Error, "Eplex warning: Specified method unavilable for"
		    " solver. Using default instead.\n");
	ec_flush(Current_Error);
	SetCPXAlg(cpx_env, CPX_ALG_DUAL);
        meth = METHOD_DUAL;
	break;
# endif
    }

    switch (node_meth)
    {
    case METHOD_PRIMAL:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_PRIMAL);
	break;
    case METHOD_DUAL:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_DUAL);
	break;
    case METHOD_NET:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_NET);
	break;
    case METHOD_BAR:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_BARRIER);
	break;
# ifdef HAS_SIFT
    case METHOD_SIFT:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_SIFTING);
	break;
# endif
    default:
	Fprintf(Current_Error, "Eplex warning: Specified node method unavilable for"
		    " solver. Using default instead.\n");
	ec_flush(Current_Error);
	node_meth = METHOD_DEFAULT;
    case METHOD_DEFAULT:
	/* no `automatic' default, use the specified default dual instead */
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_DUAL);
	break;
    }
#endif /* CPLEX */

#ifdef COIN
    /* set timeout. If no timeout was set, vtimeout is integer 0 */
    if (IsDouble(ttimeout) && Dbl(vtimeout) > 0.0)
    {
	Log1(coin_set_timeout(lpd->lp, %f), Dbl(vtimeout));
	coin_set_timeout(lpd->lp, Dbl(vtimeout));
    } 
/*    coin_set_solve_methods(lpd, method, auxmethod, node_meth, node_auxmeth);*/
#endif

    /* if solution values are unavailable, and there are unadded cutpool
       constraints, abort with RANGE_ERROR as we can't check for violations
    */
    if (unadded_cnt > 0 && outsols.val.ptr == NULL)
    {
	reset_rowcols(lpd, oldmar, lpd->mac);
	Bip_Error(RANGE_ERROR);
    }

    /*********************************************************************
     *     Solve Problem with the External Solver                        *
     *        depending on problem type, call the appropirate routine    *
     *        may solve multiple times with cutpool constraints          *
     *********************************************************************/

    do
    {
	int i;

	violated_cnt = 0;
	if (IsStructure(tdump))
	{
	    _lpwrite(lpd, format, file); /* ignore any errors here */
	}
#ifdef XPRESS
	switch (lpd->prob_type)
	{

	    case PROBLEM_FIXEDL:
	    case PROBLEM_FIXEDQ:
	    {
		int status;
		char c[1];
		double * oldsols;

		if (lpd->copystatus == XP_COPYOFF) 
		{ 
		    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		    Bip_Error(RANGE_ERROR); 
		}
		if (lpd->copystatus == XP_COPYCURRENT)
		{
		    XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &status);
		    if (status == XPRS_MIP_OPTIMAL)
		    {
			pword * argp = &vhandle.ptr[solspos];
	        
			Dereference_(argp);

			oldsols = DArrayStart(argp->val.ptr);
		    }
		}
		else
		    status = XPRS_MIP_NOT_LOADED;
		if (status != XPRS_MIP_OPTIMAL)
		{/* solve the MIP first (no cutpool cstrs) */
		    CallN(XPRSsetcbintsol(lpd->lp, _get_xpress_sol, (void *)&sol));
		    if (lpd->lpcopy == lpd->lp) CallN(XPRScreateprob(&lpd->lpcopy));
		    res = _mip_opt(lpd, meth_string, node_meth_string, (void *)&sol);
		    oldsols = sol.sols;
		    XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &status);
		}
	
		/* solve the fixed LP if we have an optimal MIP */
		if (status == XPRS_MIP_OPTIMAL)
		{
# ifdef HAS_MIQP_FIXEDCOPYBUG
	       /* According to Richard Laundy@Dash, 2004-10-01, copying a prob.
                  that originally has quadratic coeffs which has been zeroed
                  (in our case because of a probe's linear objective) lead
                  to the crash when trying to fix the columns. 
                  To work round this, we always put in a dummy 
                  quadratic term before copying, and remove it afterwards. 
                  This is only needed for changing quad. obj -> linear obj,
                  but we can't determine this easily.
	       */
		    if (lpd->prob_type == PROBLEM_FIXEDL) XPRSchgqobj(lpd->lp, 0, 0, 1.0);
# endif
		    CallN(XPRScopyprob(lpd->lpcopy, lpd->lp, lpd->probname));
		    CallN(XPRScopycontrols(lpd->lpcopy, lpd->lp));
		    CallN(XPRScopycallbacks(lpd->lpcopy, lpd->lp));
# ifdef HAS_MIQP_FIXEDCOPYBUG
		    /* now remove the dummy quadratic term */
		    if (lpd->prob_type == PROBLEM_FIXEDL)
		    {
			XPRSchgqobj(lpd->lpcopy, 0, 0, 0.0);
			XPRSchgqobj(lpd->lp, 0, 0, 0.0);
		    }
# endif
		    lpd->copystatus = XP_COPYCURRENT;

		    for(i=SOLVER_MAT_BASE; i<lpd->mac; i++)
		    {
			XPRSgetcoltype(lpd->lpcopy, c, i, i);
			if (c[0] == 'I' || c[0] == 'B') 
			{
			    Log2(
			    {\n\
			    int i = %d;\n\
			    double oldsols = %f;\n\
			    XPRSchgbounds(lpd->lpcopy, 1, &i, "B", &oldsols);\n\
			    }, i, oldsols[i]
				);
			    XPRSchgbounds(lpd->lpcopy, 1, &i, "B", &(oldsols[i]));
			}
		    }

		    CallN(XPRSscale(lpd->lp, NULL, NULL));
		    if (lpd->sense == SENSE_MIN)
		    {
			Log1(XPRSminim(lpd->lpcopy, "%s"), meth_string);
			res = XPRSminim(lpd->lpcopy, meth_string);
		    }
		    else
		    {
			Log1(XPRSmaxim(lpd->lpcopy, "%s"), meth_string);
			res = XPRSmaxim(lpd->lpcopy, meth_string);
		    }
		}
		break;
	    }

	    case PROBLEM_RELAXEDL:
	    case PROBLEM_RELAXEDQ:
		/* must solve a copy, as Xpress considers the MIP as started with
		   the solving of the root node, and problem cannot be modified
		*/
		if (lpd->copystatus == XP_COPYOFF) 
		{ 
		    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		    Bip_Error(RANGE_ERROR); 
		}
		if (lpd->lpcopy != lpd->lp)
		{
		    CallN(XPRScopyprob(lpd->lpcopy, lpd->lp, lpd->probname));
		    CallN(XPRScopycontrols(lpd->lpcopy, lpd->lp));
		    CallN(XPRScopycallbacks(lpd->lpcopy, lpd->lp));
		    lpd->copystatus = XP_COPYCURRENT;
		}
		CallN(XPRSscale(lpd->lp, NULL, NULL));
		if (lpd->sense == SENSE_MIN) 
		{
		    Log1(XPRSminim(lpd->lpcopy,"%s"), meth_string);
		    res = XPRSminim(lpd->lpcopy, meth_string);
		}
		else
		{
		    Log1(XPRSmaxim(lpd->lpcopy,"%s"), meth_string);
		    res = XPRSmaxim(lpd->lpcopy, meth_string);
		}
		break;

	    case PROBLEM_LP:
		CallN(XPRSscale(lpd->lp, NULL, NULL));
		if (lpd->sense == SENSE_MIN) 
		{ 
		    Log1(XPRSminim(lpd->lp,"%s"), meth_string);
		    res = XPRSminim(lpd->lp, meth_string);
		}
		else
		{
		    Log1(XPRSmaxim(lpd->lp,"%s"), meth_string);
		    res = XPRSmaxim(lpd->lp, meth_string);
		}
		break;

	    case PROBLEM_QP:
		CallN(XPRSscale(lpd->lp, NULL, NULL));
		if (lpd->sense == SENSE_MIN)
		{
		    Call(res, XPRSminim(lpd->lp, meth_string));
		}
		else
		{
		    Call(res, XPRSmaxim(lpd->lp, meth_string));
		}
		break;

	    case PROBLEM_MIP:
	    case PROBLEM_MIQP:
		res = _mip_opt(lpd, meth_string, node_meth_string, (void *)&sol);
		break;

	    default:
		if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		Bip_Error(RANGE_ERROR);
	}
#endif /* XPRESS */
#ifdef CPLEX

	switch (lpd->prob_type)
	{

	    case PROBLEM_RELAXEDL:
# ifdef HAS_RELAXEDLP
		CallN(CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_RELAXED));
# else
		TryFree(lpd->ctype);
		lpd->ctype = (char *) Malloc(lpd->mac * sizeof(char));
		/* mac > 0 because we have a MIP problem */
		CPXgetctype(cpx_env, lpd->lp, lpd->ctype, 0, (lpd->mac - 1));
		CallN(CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_LP));
# endif
		res = _lp_opt(lpd, &meth, &auxmeth);
		break;

	    case PROBLEM_RELAXEDQ:
		TryFree(lpd->ctype);
		lpd->ctype = (char *) Malloc(lpd->mac * sizeof(char));
		/* mac > 0 because we have a MIP problem */
		CPXgetctype(cpx_env, lpd->lp, lpd->ctype, 0, (lpd->mac - 1));
		CallN(CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_QP));
		Call(res, CPXqpopt(cpx_env, lpd->lp));
		break;

	    case PROBLEM_FIXEDL:
		TryMIPBeforeFixing(res);
		if (res != 0) break; 
		Call(res,CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_FIXEDMILP)); 
		if (res != 0) { 
		    /* reset res if no integer solution so the solution status for 
		       the previous CPXmipopt() will be obtained */ 
		    if (res == CPXERR_NO_INT_SOLN) res = 0; 
		    break; 
		} 
		/* if no error, falls through and solve as (fixed) LP  */ 

	    case PROBLEM_LP:
		res = _lp_opt(lpd, &meth, &auxmeth);
		break; /* PROBLEM_LP */

# ifdef HAS_MIQP

	    case PROBLEM_FIXEDQ:
		TryMIPBeforeFixing(res);
		if (res != 0) break; 
		Call(res,CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_FIXEDMIQP)); 
		if (res != 0) { 
		    /* reset res if no integer solution so the solution status for 
		       the previous CPXmipopt() will be obtained */ 
		    if (res == CPXERR_NO_INT_SOLN) res = 0; 
		    break; 
		} 
		/* if no error, falls through and solve as (fixed) QP  */ 

# endif
	    case PROBLEM_QP:
		Call(res, CPXqpopt(cpx_env, lpd->lp));
		break;


	    case PROBLEM_MIP:
	    case PROBLEM_MIQP:
# ifdef HAS_SIFT
		if (node_meth == METHOD_SIFT)
		{
		    if (meth != METHOD_SIFT) 
		    {
			SetSiftAlg(cpx_env, node_auxmeth);
		    }
		    else if (node_auxmeth != METHOD_DEFAULT && 
			     node_auxmeth != auxmeth)
		    {
			Fprintf(Current_Error, "Eplex warning: Sifting methods must "
				"be the same for root and nodes for a"
				" MIP.\n Node sifting method ignored\n");
			ec_flush(Current_Error);
			node_auxmeth = auxmeth;
		    }
		}
# endif
		Call(res, CPXmipopt(cpx_env, lpd->lp));
		break;

	    default:
		if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		Bip_Error(RANGE_ERROR);
	} /* switch lpd->prob_type */

#endif /* CPLEX */
#ifdef COIN
	Log2({lpd->prob_type = %d; lpd->presolve = %d;}, lpd->prob_type, lpd->presolve);
	Log4(coin_solve_problem(lpd, %d, %d, %d, %d), meth, auxmeth, node_meth, node_auxmeth);
	if (coin_solve_problem(lpd, meth, auxmeth, node_meth, node_auxmeth) == -1)
	    Bip_Error(UNIMPLEMENTED);
#endif /* COIN */

	/*********************************************************************
	 *     Get State Information from External Solver                    *
	 *********************************************************************/

#ifdef LOG_CALLS
	Fprintf(log_output_, "\n}\nvoid step_%d() {\n", log_ctr++);
	ec_flush(log_output_);
	current_matno = -1; /* no current mat, exited from procedure */
#endif

#ifdef CPLEX
	/* sol_state is given the solution status unless an error has occurred
	   (res != 0), in which case the error code is return unless the error
	   is CPXERR_SUBPROB_SOLVE, where the MIP was aborted and there is a
	   valid solution status
	*/
	lpd->sol_state = res ? 
	    (res  == CPXERR_SUBPROB_SOLVE ? CPXgetstat(cpx_env, lpd->lp) : res) :
	    CPXgetstat(cpx_env, lpd->lp); 

	if ((lpd->prob_type == PROBLEM_FIXEDL && 
	     CPXgetprobtype(cpx_env, lpd->lp) == CPXPROB_FIXEDMILP)
# ifdef HAS_MIQP
	    || (lpd->prob_type == PROBLEM_FIXEDQ && 
		CPXgetprobtype(cpx_env, lpd->lp) == CPXPROB_FIXEDMIQP)
# endif
	    )
	{/* add in the counts for the fixedmilp solve */ 
	    lpd->sol_itcnt += CPXgetitcnt(cpx_env, lpd->lp);
	}
	else
	{
	    lpd->sol_itcnt = IsMIPProb(lpd->prob_type) ?
		CPXgetmipitcnt(cpx_env, lpd->lp) : CPXgetitcnt(cpx_env, lpd->lp);
	    lpd->sol_nodnum = IsMIPProb(lpd->prob_type) ?
		CPXgetnodecnt(cpx_env, lpd->lp) : 0;
	}
#endif /* CPLEX */
#ifdef XPRESS
	/* here lpcopy should be the problem that has been solved, either:
	     a) It is the copy that is solved (MIP)
	     b) It is the same as lp (non-MIP or if XP_COPYOFF)
	   so always obtain solution information from lpcopy
	*/
	if (res) 
	{ 
	    int err;
	    char errmsg[256];

	    XPRSgetintattrib(lpd->lpcopy, XPRS_ERRORCODE,&err); 
	    Fprintf(Current_Error, "XPRESS problem: %d\n", err);
	    XPRSgetlasterror(lpd->lpcopy, errmsg);
	    Fprintf(Current_Error, "problem: %s\n", errmsg);
	    ec_flush(Current_Error);
	}
	Get_Xp_Stat(lpd);
	XPRSgetintattrib(lpd->lpcopy, XPRS_SIMPLEXITER, &lpd->sol_itcnt);
	XPRSgetintattrib(lpd->lpcopy, XPRS_NODES, &lpd->sol_nodnum); 
#endif
#ifdef COIN
	CallN(coin_get_stats(lpd));
#endif

/* Here we test for various states of the solution. The following macro tests
   are defined for all the solvers:
      SuccessState: solution is `optimal' (may be optimal within tolerance)
      FailState: problem is proven infeasible or no MIP solution is better 
                 than cutoff.
      MIPSemiSuccessState: solution exist, but may be suboptimal (MIP only)
      MIPSemiFailState: no solution found yet, but problem not proven 
                        infeasible (MIP only)
      LPAbortedState: LP solve was aborted 
                      (for LP, or  root node LP solve for MIP (not CPLEX)) 
      UnboundedState: problem is unbounded
      MayBeFailState: problem is infeasible or unbounded, but solver cannot 
                      determine which
*/
      
	if (SuccessState(lpd)) {
	    lpd->descr_state = DESCR_SOLVED_SOL;
	    lpd->optimum_ctr++;
	    if (IsMIPProb(lpd->prob_type))
	    {
		CallN(CPXgetmipobjval(cpx_env, lpd->lpcopy, &worstbound));
		/* bestbound may be different from objval because of tolerance */
		lpd->objval = worstbound;
#ifdef NO_MIPBESTBOUND_IF_OPTIMAL
		if (lpd->sol_state == CPXMIP_OPTIMAL_TOL)
		{ /* bestobjval only available if nodes have been cutoff! */
		    CallN(Get_Best_Objbound(lpd->lpcopy, &bestbound));
		} else
		{
		    CallN(CPXgetmipobjval(cpx_env, lpd->lpcopy, &bestbound));
		}
#else
		CallN(Get_Best_Objbound(lpd->lpcopy, &bestbound));
#endif
	    } else
	    {
		CallN(Get_LP_Objval(lpd, &lpd->objval));
		if (UsingBarrierNoCrossOver(lpd->lp))
		{ 
		    CallN(Get_Bar_Primal_Obj(lpd->lp, &worstbound));
		    CallN(Get_Bar_Dual_Obj(lpd->lp, &bestbound));
		} else 
		{ 
		    CallN(Get_LP_Objval(lpd, &bestbound));
		    worstbound = bestbound;
		}
	    }
 
	} else if (FailState(lpd)) {
	    /* for MIP problems, the MIP search may have nodes that were not
	       explored further because of cutoff -- i.e. they cannot lead to
	       solutions better than the cutoff. If no solution is found, the
	       problem is considered infeasible, but strictly it means there is
	       no solution better than cutoff. Unfortunately, it is not easy to
	       know if cutoff had occurred in a MIP search, so bestbound is set
	       to cutoff unless we know otherwise
	    */
#ifdef UNIFORM_SOL_STAT
	    lpd->descr_state = DESCR_SOLVED_NOSOL;
	    worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    if (IsMIPProb(lpd->prob_type))
	    {
		Get_MIPCutOff(lpd, &bestbound);
	    } else
		bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
#else
	    if (DualMethod(lpd, meth, auxmeth)) {
		lpd->descr_state = DESCR_UNKNOWN_NOSOL;
		/* dual infeasible ==> primal infeasible or unbounded
		   no information: full interval */
		worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
		bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    } else {
		lpd->descr_state = DESCR_SOLVED_NOSOL;
		worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
# ifdef XPRESS
		/* In Xpress, we can get more information as we can tell if 
		   cutoff occurred at the root MIP node 
		*/
		if (IsMIPProb(lpd->prob_type))
		{
		    if (lpd->sol_state == XPRS_LP_CUTOFF) /* at root node */
		    { /* primal or barrier - have a feasible root LP worse than
			 cutoff */
			XPRSgetdblattrib(lpd->lpcopy, XPRS_MIPOBJVAL, &bestbound);
		    } else if (lpd->sol_state == XPRS_LP_INFEAS)
		    {/* root is infesible => MIP is infeasible  */
			bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
		    } else
		    {/* safe option of returning cutoff as best bound */
			Get_MIPCutOff(lpd, &bestbound);
		    } 
		} else /* infeasible LP problem  */
# endif
		    /* infeasible LP for Xpress, infeasible LP/MIP for COIN */
		    bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
# ifdef CPLEX
		if (IsMIPProb(lpd->prob_type))
		{
		    Get_MIPCutOff(lpd, &bestbound);
		} else
		    bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
# endif

	    }
#endif /* !UNIFORM_SOL_STAT */
	    lpd->objval = worstbound;
	    lpd->infeas_ctr++;
	} else if (MIPSemiSuccessState(lpd)) {
#ifdef CPLEX
	    /* For the MIPSemiSuccessState to test correctly, we need the MIP
	       solution status. However, if the MIP solve aborted in a LP
	       node, the solution status for that LP solve is more informative.
	       res must still be the return code from the optimisation call.
	    */
	    if (res == CPXERR_SUBPROB_SOLVE) lpd->sol_state = CPXgetsubstat(cpx_env, lpd->lp); 
#endif
	    lpd->descr_state = DESCR_ABORTED_SOL;
	    lpd->abort_ctr++;

	    CallN(Get_Best_Objbound(lpd->lpcopy, &bestbound));
	    CallN(CPXgetmipobjval(cpx_env, lpd->lpcopy, &worstbound));
	    lpd->objval = worstbound;

	} else if (MIPSemiFailState(lpd)) {
#ifdef CPLEX
	/* similar to MIPSemiSuccessState, but need to check if we have
           aborted at the root node solve, in which case there is no valid 
           bestobjval  */
	    if (res == CPXERR_SUBPROB_SOLVE) 
	    {
		lpd->sol_state = CPXgetsubstat(cpx_env, lpd->lp); 
		if (lpd->sol_nodnum > 0)
		{/* did not abort at root node */
		    CallN(Get_Best_Objbound(lpd->lpcopy, &bestbound));
		} else /* no valid bestobjval... */
		    bestbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    } else
	    {
		CallN(Get_Best_Objbound(lpd->lpcopy, &bestbound));
	    }
#endif
#if defined(XPRESS) || defined(COIN)
	/* For Xpress and COIN, the MIPSemiFailState does not include 
	   aborting/stopping at the root node solve */
	    CallN(Get_Best_Objbound(lpd->lpcopy, &bestbound)); 
#endif
	    worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);

	    lpd->objval = worstbound;
	    lpd->descr_state = DESCR_ABORTED_NOSOL;
	    lpd->abort_ctr++;
  
	} else if (UnboundedState(lpd)) {
#ifdef UNIFORM_SOL_STAT
# if defined(CPLEX) && CPLEX >= 8
	    int meth, type, isprimf, isdualf;

	    /* CPLEX 8+ redefined the `unbounded' state to mean an unbounded
	       ray is detected, i.e. that solution will be unbounded *if* a
	       feasible solution exists, but feasiblity may be unproven
	    */
	    CPXsolninfo(cpx_env, lpd->lp, &meth, &type, &isprimf, &isdualf);
	    if (isprimf) {
		/* is primary feasible, i.e. has feasible solution and is 
		   unbounded
		*/
		lpd->descr_state = DESCR_UNBOUNDED_NOSOL;
		lpd->abort_ctr++;
		bestbound = worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
	    } else {
		/* not proven to be primary feasible */
		lpd->descr_state = DESCR_UNKNOWN_NOSOL;
		lpd->infeas_ctr++;
		/* no information on bounds */
		worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
		bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
		lpd->objval = worstbound;
	    }

# else
	    lpd->descr_state = DESCR_UNBOUNDED_NOSOL;
	    lpd->abort_ctr++;
	    bestbound = worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
# endif

#else
	    if (DualMethod(lpd, meth, auxmeth)) {
		lpd->descr_state = DESCR_SOLVED_NOSOL;
		lpd->infeas_ctr++;
		bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
		worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
	    } else {
		lpd->descr_state = DESCR_UNBOUNDED_NOSOL;
		lpd->abort_ctr++;
		bestbound = worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
	    }
#endif
	    lpd->objval = worstbound;

	} else if (MaybeFailState(lpd)) {
	    lpd->descr_state = DESCR_UNKNOWN_NOSOL;
	    lpd->infeas_ctr++;
	    /* no information on bounds */
	    worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
	    bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    lpd->objval = worstbound;
	} else if (LPAbortedState(lpd)) {
	    /* The exact status of an aborted LP case depends on the solving 
	       method used */
#ifdef CPLEX
# if CPLEX >= 8
	    int meth, type, isprimf, isdualf;

	    CPXsolninfo(cpx_env, lpd->lp, &meth, &type, &isprimf, &isdualf);

	    /* initial worstbound and objval, may be revised */
	    lpd->objval = worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
	    if (isprimf)
	    {
		lpd->descr_state = DESCR_ABORTED_SOL;
		if (meth == CPX_ALG_BARRIER)
		    CPXgetdblquality(cpx_env, lpd->lp, &worstbound, CPX_PRIMAL_OBJ);
		else 
		    Get_LP_Objval(lpd, &worstbound);
		lpd->objval = worstbound; 

	    } else
	    {
		lpd->descr_state = DESCR_ABORTED_NOSOL;
	    }
	    if (isdualf)
	    {
		if (meth == CPX_ALG_BARRIER)
		    CPXgetdblquality(cpx_env, lpd->lp, &bestbound, CPX_DUAL_OBJ);
		else
		    Get_LP_Objval(lpd, &bestbound);
	    } else
	    {
		bestbound = (lpd->sense == SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
	    }

	    lpd->abort_ctr++;

# else /* CPLEX < 8 */

	    if (LPAbortedNoSolState(lpd))
	    { /* no feasible solution, no information on bounds */
		lpd->descr_state = DESCR_ABORTED_NOSOL;
		lpd->objval = worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
		bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    } else
	    {
		switch (meth)
		{
		case METHOD_DEFAULT: /* default assumed to be dual simplex! */
		case METHOD_DUAL: /* in phase II, with feasible dual sol. */
		    Get_LP_Objval(lpd, &bestbound);
		    lpd->objval = worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
		    lpd->descr_state = DESCR_ABORTED_NOSOL;
		    break;
		case METHOD_PRIMAL: /* in phase II, with feasible primal sol.*/
		case METHOD_BAR: /* has feasible primal sol */
		    Get_LP_Objval(lpd, &worstbound);
		    lpd->objval = worstbound;
		    bestbound = (lpd->sense == SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
		    lpd->descr_state = DESCR_ABORTED_SOL;
		    break;
		default:
		    /* just assume there is no valid information */
		    lpd->descr_state = DESCR_ABORTED_NOSOL;
		    lpd->objval = worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
		    bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
		    break;
		}
	    }

	    lpd->abort_ctr++;

# endif
#endif /* CPLEX */
#if defined(XPRESS) || defined(COIN)
/*      if (LPAbortedState(lpd))  */
	/* Xpress can reach the LPAbortedState either when an LP
           solving was aborted for an LP problem, or if the root LP solve
           was aborted for a MIP problem
	*/
	    int attr; /* variable for integer attribute */

	    /* meth is used for LP and root MIP LP */

	    if (meth == METHOD_DEFAULT) 
#ifdef COIN
		meth = METHOD_DUAL;
#endif
#ifdef XPRESS
	    {/* turn `default' method into actual method used 
		CAUTION: may need revising if Xpress changes the default
		method mapping! [they have no call to get actual method used]
	     */
		int method;

		XPRSgetintcontrol(lpd->lpcopy, XPRS_DEFAULTALG, &method);
		switch (method)
		{
		case 1: 
		    meth = IsQPProb(lpd->prob_type) ? METHOD_BAR : METHOD_DUAL; 
		    break;
		case 2: meth = METHOD_DUAL; break;
		case 3: meth = METHOD_PRIMAL; break;
		case 4: meth = METHOD_BAR; break;
		}
	    }
#endif 
	    if (IsMIPProb(lpd->prob_type))
	    { /* MIP search aborted in root LP solve */
		lpd->descr_state = DESCR_ABORTED_NOSOL;
		lpd->objval = worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
		CallN(Get_Best_Objbound(lpd->lpcopy, &bestbound));
		if (meth == METHOD_DUAL)
		{
		    Get_Dual_Infeas(lpd->lpcopy, &attr);
		    if (attr == 0)
		    {
			/* attr == 0 ==> we have a feasible dual `solution' for 
			   root node. This is superoptimal, and so can form the
			   best bound for the MIP problem
			*/
			Get_LP_Objval(lpd, &bestbound);
		    }
		}
	    } else
	    {   /* !IsMIPProb */
		switch (meth)
		{
		case METHOD_DUAL:
		    lpd->descr_state = DESCR_ABORTED_NOSOL;
		    Get_Dual_Infeas(lpd->lp, &attr);
		    /* attr == 0 ==> we have a feasible dual `solution' (i.e.
		       we are in phase II of the Simplex). This
		       is superoptimal for the original problem */
		    if (attr == 0)
			Get_LP_Objval(lpd, &bestbound);
		    else /* no information on bestbound */
			bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
		    lpd->objval = worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
		    break;
		case METHOD_BAR:
		/* In XPRESS, cross-over may be performed after aborting
                   from a barrier solve. So we may arrive here if the 
                   cross-over (using primal simplex) was also aborted. We
                   check if this is the case with SIMPLEXITER -- if non-zero,
                   we fall through and treat like METHOD_PRIMAL
		*/
		    lpd->descr_state = DESCR_ABORTED_NOSOL;
#if defined(XPRESS) || defined(COIN)
# ifdef XPRESS
		    XPRSgetintattrib(lpd->lpcopy, XPRS_SIMPLEXITER, &attr);
		    if (attr == 0)
		    {
			double infeas;
			/* Oliver Bastert @ DASH, 2005-06 states that if the
			   BARPRIMAL/DUAL infeasibilities are small enough,
			   the solution can be considered primal and dual feasible
			   respectively. Otherwise, the BARPRIMAL/DUAL objective
			   is usually bounds on the optimal value, but this may
			   be wrong in degenerate cases, so we don't use them
			   here unless they are feasible
			*/
			lpd->descr_state = DESCR_ABORTED_NOSOL;
			bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
			lpd->objval = worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
# endif
			if (Bar_Is_Primal_Feasible(lpd))
			{/* primal feasible */
			    Get_Bar_Primal_Obj(lpd->lp, &worstbound);
			    lpd->objval = worstbound;
			    lpd->descr_state = DESCR_ABORTED_SOL;
			}
			if (Bar_Is_Dual_Feasible(lpd))
			{/* dual feasible */
			    Get_Bar_Dual_Obj(lpd->lp, &bestbound);
			}
			break;
# ifdef XPRESS
		    }
		    /* if the crossover simplex is performed, fall through and
		       treat as primal simplex
		    */
# endif
#endif
		case METHOD_PRIMAL:
		    Get_Primal_Infeas(lpd->lp, &attr);
		    /* attr == 0 ==> we have a feasible primal solution */
		    if (attr == 0)
		    {
			Get_LP_Objval(lpd, &worstbound);
			lpd->objval = worstbound;
			bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
			lpd->descr_state = DESCR_ABORTED_SOL;
		    } else
		    {
			lpd->descr_state = DESCR_ABORTED_NOSOL;
			bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
			lpd->objval = worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
		    }
		    break;
		default:
		    /* this should not happen! */
		    Fprintf(Current_Error, "Eplex error: Unexpected method case while classifying results. Aborting.\n");
		    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		    Bip_Error(EC_EXTERNAL_ERROR);
		}

	    } /* !IsMIPProb() */

	    lpd->abort_ctr++;

#endif /* XPRESS/COIN */

	} else { 
	    /* fall back case where we don't have any information */
	    lpd->descr_state = DESCR_ABORTED_NOSOL;
	    bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
	    lpd->objval = worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
	}

	/*********************************************************************
	 *     Get Result from External Solver                               *
	 *       Get the result for the optimisation from the external       *
	 *       solver if there is one                                      *
	 *********************************************************************/

	switch (lpd->descr_state)
	{
	case DESCR_SOLVED_SOL:
	case DESCR_ABORTED_SOL:
	    add_cp_cstr = 1;
#ifdef XPRESS
	    /* if MIP callback is used, then the MIP solution state is already
	       gathered by the callbacks into the appropriate arrays for
	       the MIP case (and basis was obtained already from the root LP)
	    */
	    if (!IsMIPProb(lpd->prob_type))
	    {
		/* using lpcopy because same as lp for LP and QP, but point at
		   actual problem for FIXED
		*/
		res = XPRSgetsol(lpd->lpcopy, sol.sols, sol.slacks, sol.pis, sol.djs);
		Log4(XPRSgetsol(lpd->lpcopy, (double *)malloc(%d*sizeof(double)), 
				(double *)malloc(%d*sizeof(double)),
				(double *)malloc(%d*sizeof(double)),
				(double *)malloc(%d*sizeof(double))),
		     lpd->mac, lpd->mar, lpd->mar, lpd->mac); 

# ifdef HAS_POSTSOLVE
		if (lpd->descr_state == DESCR_ABORTED_SOL)
		    CallN(XPRSpostsolve(lpd->lp)); /* post-solve problem if possible */
# endif
		if (sol.rbase != NULL && res == 0)
		    res = XPRSgetbasis(lpd->lpcopy, sol.rbase, sol.cbase);
	    }
	    if (res != 0) 
	    { 
		if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		Bip_Error(EC_EXTERNAL_ERROR); 
	    }

#endif /* XPRESS */
#ifdef CPLEX
	    if (outsols.val.ptr != NULL && lpd->mac > 0) 
	    {
		res = IsMIPProb(lpd->prob_type)
		    ? CPXgetmipx(cpx_env, lpd->lp, DArrayStart(outsols.val.ptr), 0, lpd->mac-1)
		    : CPXgetx(cpx_env, lpd->lp, DArrayStart(outsols.val.ptr), 0, lpd->mac-1);

		if (res != 0) 
		{ 
		    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		    Bip_Error(EC_EXTERNAL_ERROR); 
		}
	    }
	    if (outpis.val.ptr != NULL && lpd->mar > 0) 
	    {
		res = CPXgetpi(cpx_env, lpd->lp, DArrayStart(outpis.val.ptr), 0, lpd->mar-1);
		if (res != 0) 
		{ 
		    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		    Bip_Error(EC_EXTERNAL_ERROR); 
		}
	    }
	    if (outslacks.val.ptr != NULL && lpd->mar > 0)
	    {
		res = IsMIPProb(lpd->prob_type)
		    ? CPXgetmipslack(cpx_env, lpd->lp, DArrayStart(outslacks.val.ptr), 0, lpd->mar-1)
		    : CPXgetslack(cpx_env, lpd->lp, DArrayStart(outslacks.val.ptr), 0, lpd->mar-1);
		if (res != 0) 
		{ 
		    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		    Bip_Error(EC_EXTERNAL_ERROR); 
		}
	    }
	    if (outdjs.val.ptr != NULL && lpd->mac > 0)
	    {
		CPXgetdj(cpx_env, lpd->lp, DArrayStart(outdjs.val.ptr), 0, lpd->mac-1);
		if (res != 0) 
		{
		    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		    Bip_Error(EC_EXTERNAL_ERROR); 
		}
	    }

	    if (outcbase.val.ptr != NULL)
	    {
		res = CPXgetbase(cpx_env, lpd->lp, IArrayStart(outcbase.val.ptr), 
				 IArrayStart(outrbase.val.ptr));
		if (res != 0) 
		{ 
		    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		    Bip_Error(EC_EXTERNAL_ERROR); 
		}
	    }
#endif
#ifdef COIN
	    coin_get_soln_state(lpd, 
		outsols.val.ptr == NULL ? NULL : DArrayStart(outsols.val.ptr),
		outpis.val.ptr == NULL ? NULL : DArrayStart(outpis.val.ptr),
		outslacks.val.ptr == NULL ? NULL : DArrayStart(outslacks.val.ptr),
		outdjs.val.ptr == NULL ? NULL : DArrayStart(outdjs.val.ptr),
		outcbase.val.ptr == NULL ? NULL : IArrayStart(outcbase.val.ptr),
	        outrbase.val.ptr == NULL ? NULL : IArrayStart(outrbase.val.ptr));

#endif
	    break; 
	case DESCR_SOLVED_NOSOL:
	    add_cp_cstr = 0;
	    /* no solution; state always fail */
#ifdef SUPPORT_IIS
	    {
		pword *argp = &vhandle.ptr[iis_rowspos];

		Dereference_(argp);

		if (!IsRef(argp->tag))  
		{
		    int iis_nrows, iis_ncols;
		    int err;
		    pword * old_tg1;

		    pword iis_rowidxs, iis_colidxs, iis_colstats;

		    Find_Conflict(err, lpd->lp, iis_nrows, iis_ncols);
		    if (err) 
		    {/* we can't simply abort here if an error occurs, just create dummy arrays
                        and do not proceed to try to get the IIS */
			iis_nrows = 0;
			iis_ncols = 0;
		    }
		    old_tg1 = TG;
		    iis_rowidxs.val.ptr = _create_iarray(iis_nrows);
		    iis_rowidxs.tag.kernel = TSTRG;
		    iis_colidxs.val.ptr = _create_iarray(iis_ncols);
		    iis_colidxs.tag.kernel = TSTRG;
		    iis_colstats.val.ptr = _create_carray(iis_ncols);
		    iis_colstats.tag.kernel = TSTRG;


		    if (!err && (_get_iis(lpd, &iis_nrows, &iis_ncols, 
				     IArrayStart(iis_rowidxs.val.ptr), IArrayStart(iis_colidxs.val.ptr), 
					  CArrayStart(iis_colstats.val.ptr)) 
			    != 0)
			)
		    {
			/* something went wrong; reallocate iis arrays with 0 size */
			TG = old_tg1;
			iis_nrows = 0;
			iis_ncols = 0;
			iis_rowidxs.val.ptr = _create_iarray(0);
			iis_colidxs.val.ptr = _create_iarray(0);
			iis_colstats.val.ptr = _create_carray(0);
		    }

		    ec_assign(vhandle.ptr+iis_rowspos, iis_rowidxs.val, iis_rowidxs.tag);
		    ec_assign(vhandle.ptr+iis_colspos, iis_colidxs.val, iis_colidxs.tag);
		    ec_assign(vhandle.ptr+iis_colstatspos, iis_colstats.val, iis_colstats.tag);
			
		}
	    }
#endif
	    break;
	default:
	{
#ifdef HAS_POSTSOLVE
	    int presolve; /* postsolve prob. if it is left in a presolved state */

	    if (XPRSgetintattrib(lpd->lp, XPRS_PRESOLVESTATE, &presolve))
	    {
		if (presolve & 2) /* is in a presolve state */
		    CallN(XPRSpostsolve(lpd->lp)); /* post-solve problem if possible */
	    }
#endif
	    if (lpd->descr_state == DESCR_UNBOUNDED_NOSOL || 
		lpd->descr_state == DESCR_UNKNOWN_NOSOL)
	    {/* no result this time, but add all cutpool constraints 
                and resolve may give a solution
	     */
		add_cp_cstr = 2;
	    } else
	    {/* no results, and adding more constraints will not improve the 
		situation */
		add_cp_cstr = 0;
	    }
	}}
#ifdef COIN
	coin_reset_prob(lpd);
#endif

	if (add_cp_cstr)
	{
	    int zerobeg = 0, offset, nzcount, j;

	    i = 0;
	    last_violated_idx = -1;
	    while (i < unadded_cnt)
	    {
		if ((j = cp_unadded[i]) >= 0)
		{
		    switch (cp_map2[j] = _cstr_state(lpd,j,add_cp_cstr,DArrayStart(outsols.val.ptr)))
		    {
		    case CSTR_STATE_VIOLATED:
			violated_cnt++;
			offset = lpd->cp_rmatbeg2[j];
			nzcount = ( j <  lpd->cp_nr2-1 ? lpd->cp_rmatbeg2[j+1] - offset : lpd->cp_nnz2 - offset);

			CPXaddrows(cpx_env, lpd->lp, 0, 1, nzcount,
				   &(lpd->cp_rhsx2[j]), &(lpd->cp_senx2[j]),
				   &zerobeg, /* only one row */ 
				   &(lpd->cp_rmatind2[offset]),
				   &(lpd->cp_rmatval2[offset]), NULL, NULL);
			lpd->mar++;
			cp_map2[j] = lpd->cp_nact2++;
			/* set last_violated_idx if it is not valid */
			if (last_violated_idx < 0) last_violated_idx = i;
			break;
		    case CSTR_STATE_SAT: /* satisfied, but not binding */
		    case CSTR_STATE_BINDING: /* satisfied and binding */
			if (last_violated_idx >= 0)
			{
			    cp_unadded[last_violated_idx] = last_violated_idx - i;
			    last_violated_idx = -1;
			}
			break;
		    case CSTR_STATE_INVALID: /* error */
			Bip_Error(RANGE_ERROR);
			break;
		    }
		    i++;
		} else
		{/* j < 0 : j is -displacement to unadded cstr */
		    i -= j;
		}
	    }
	    if (last_violated_idx >= 0) cp_unadded[last_violated_idx] = last_violated_idx- i;
	    if (violated_cnt > 0) 
	    {
		Mark_Copy_As_Modified(lpd);
		TG = old_tg; /* reallocate row-wise result arrays */
		if (outslacks.val.ptr != NULL)
		{
		    outslacks.val.ptr = _create_result_darray(vhandle, slackspos, lpd->mar);
#ifdef XPRESS
		    sol.slacks = DArrayStart(outslacks.val.ptr);
#endif
		}
		if (outrbase.val.ptr != NULL)
		{
		    outrbase.val.ptr = _create_result_iarray(vhandle, rbasepos, lpd->mar);
#ifdef XPRESS
		    sol.rbase = IArrayStart(outrbase.val.ptr);
#endif
		}
		if (outpis.val.ptr != NULL)
		{
		    outpis.val.ptr = _create_result_darray(vhandle,   pispos, lpd->mar);
#ifdef XPRESS
		    sol.pis = DArrayStart(outpis.val.ptr);
#endif
		}
	    }
	} /* if (add_cp_cstr) */
	
    } while (violated_cnt > 0); /* do ... */

    Request_Unify_Integer(vres, tres, lpd->descr_state);
    Request_Unify_Integer(vstat, tstat, lpd->sol_state);

    /* (-)HUGE_VAL is used for the maximum best/worst bound instead of 
       (-)CPX_INFBOUND because:
          1) The objective value can exceed CPX_INFBOUND
          2) We use 1.0Inf at the ECLiPSe level for unbounded objective
             value
       Note worst and best bounds are unified even for failure case 
       (for use in any future failure handler)
    */
    Request_Unify_Float(vworst, tworst, worstbound);
    Request_Unify_Float(vbest, tbest, bestbound);

    if (add_cp_cstr == 1)
    {/* have results */
	if (outsols.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+solspos, outsols.val, outsols.tag);
	if (outpis.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+pispos,  outpis.val, outpis.tag);
	if (outslacks.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+slackspos, outslacks.val, outslacks.tag);
	if (outdjs.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+djspos, outdjs.val, outdjs.tag);
	if (outcbase.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+cbasepos, outcbase.val, outcbase.tag);
	if (outrbase.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+rbasepos, outrbase.val, outrbase.tag);
    }
    else 
    {
	pword pw;
	/* no solution; reset arrays as these states might not fail */
	Make_Nil(&pw);
	if (outsols.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+solspos, pw.val, pw.tag);
	if (outpis.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+pispos, pw.val, pw.tag);
	if (outslacks.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+slackspos, pw.val, pw.tag);
	if (outdjs.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+djspos, pw.val, pw.tag);
	if (outcbase.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+cbasepos, pw.val, pw.tag);
	if (outrbase.val.ptr != NULL) 
	    ec_assign(vhandle.ptr+rbasepos, pw.val, pw.tag);
    }

    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);

    Return_Unify;
}


int
p_cpx_loadbase(value vlp, type tlp, value vcarr, type tcarr, value vrarr, type trarr)
{
    lp_desc *lpd; 
    int res;
    Check_Array(tcarr);
    Check_Array(trarr);
    LpDesc(vlp, tlp, lpd);
    SetPreSolve(lpd->presolve);
    if (lpd->mac == IArraySize(vcarr.ptr) && lpd->mar == IArraySize(vrarr.ptr)) {
	/* Finx b58: only load basis if current row/col == array sizes */
#ifdef LOG_CALLS
	Fprintf(log_output_, "\niloadbasis(...);");
#endif
	res = CPXcopybase(cpx_env, lpd->lp, IArrayStart(vcarr.ptr), IArrayStart(vrarr.ptr));
	if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
    }
    Succeed;
}

#ifdef COIN
int
p_cpx_loadorder(value vlp, type tlp, value vn, type tn, value vl, type tl)
{
    Succeed;
}
#else

int
p_cpx_loadorder(value vlp, type tlp, value vn, type tn, value vl, type tl)
{
    lp_desc *lpd; 
    int *idx, *prio;
#ifdef CPLEX
    int *bdir;
#endif
#ifdef XPRESS
    char *bdir;
#endif
    int i, res;
    pword *buf = TG;

    Check_Integer(tn);
    if (vn.nint <= 0) Succeed_; /* no need to load anything */

    LpDesc(vlp, tlp, lpd);
    Push_Buffer(vn.nint*3*sizeof(int));
    idx = (int*) BufferStart(buf);
    prio = (int*) (BufferStart(buf) + vn.nint*sizeof(int));
#ifdef CPLEX
    bdir = (int*) (BufferStart(buf) + vn.nint*2*sizeof(int));
#endif
#ifdef XPRESS
    bdir = (char*) (BufferStart(buf) + vn.nint*2*sizeof(int));
#endif
    i = 0;
    while (IsList(tl))
    {
	pword *car = vl.ptr;
	pword *cdr = car + 1;
	pword *pw;

	Dereference_(car);
	Check_Structure(car->tag);
	if (DidArity(car->val.ptr->val.did) != 3)
	    { Bip_Error(RANGE_ERROR); }

	pw = car->val.ptr + 1;		/* colindex */
	Dereference_(pw);
	idx[i] = pw->val.nint;
	pw = car->val.ptr + 2;		/* priority */
	Dereference_(pw);
	prio[i] = pw->val.nint;
	pw = car->val.ptr + 3;		/* direction */
	Dereference_(pw);
#ifdef CPLEX
	bdir[i] = pw->val.nint;
#endif
#ifdef XPRESS
	bdir[i] = pw->val.nint == 1 ? 'D' : pw->val.nint == 2 ? 'U' : 'N';
#endif
	++i;
	
	Dereference_(cdr);
	tl = cdr->tag;
	vl = cdr->val;
    }
    Check_List(tl);
    if (i != vn.nint)
	{ Bip_Error(RANGE_ERROR); }

#ifdef LOG_CALLS
    Fprintf(log_output_, "\nloaddir(...);"); ec_flush(log_output_);
#endif
    SetPreSolve(lpd->presolve);
    res = CPXcopyorder(cpx_env, lpd->lp, i, idx, prio, bdir);
    TG = buf;				/* pop aux arrays */
    if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
    Succeed;
}

#endif

int
p_cpx_loadsos(value vlp, type tlp, 
	      value vsostype, type tsostype, 
	      value vn, type tn, 
	      value vl, type tl)
{
    lp_desc *lpd; 
    double weight;
    int i;

    Check_Integer(tsostype);
    Check_Integer(tn);

    LpDescOnly(vlp, tlp, lpd);
    if (vn.nint <= 0) Succeed_; /* return immediately if sos set empty */
    /* allocate enough space for the new sos */
    /* CAUTION: in this array must have lpd->nsos+1 elements !!! */
    ++lpd->nsos;
    if (lpd->nsos >= lpd->sossz)
    {
	if (lpd->nsos == 0)
	{
	    lpd->sossz = NEWSOS_INCR;
	    lpd->sosbeg = (int *) Malloc(NEWSOS_INCR*sizeof(int));
	}
	else
	{
	    lpd->sossz += NEWSOS_INCR;
	    lpd->sosbeg = (int *) Realloc(lpd->sosbeg, lpd->sossz*sizeof(int));
	}
    }
    lpd->sosbeg[lpd->nsos-1] = lpd->nsosnz;
    lpd->sosbeg[lpd->nsos] = lpd->nsosnz + vn.nint;

    /* allocate enough space for the sos members */
    i = lpd->nsosnz;
    lpd->nsosnz += vn.nint;
    if (lpd->nsosnz > lpd->sosnzsz)
    {
	if (lpd->sosnzsz == 0)
	{
	    lpd->sosnzsz = RoundTo(lpd->nsosnz, 512);
	    lpd->sostype = (char *) Malloc(lpd->sosnzsz*sizeof(char));
	    lpd->sosind = (int *) Malloc(lpd->sosnzsz*sizeof(int));
	    lpd->sosref = (double *) Malloc(lpd->sosnzsz*sizeof(double));
	}
	else
	{
	    lpd->sosnzsz = RoundTo(lpd->nsosnz, 512);
	    lpd->sostype = (char *) Realloc(lpd->sostype, lpd->sosnzsz*sizeof(char));
	    lpd->sosind = (int *) Realloc(lpd->sosind, lpd->sosnzsz*sizeof(int));
	    lpd->sosref = (double *) Realloc(lpd->sosref, lpd->sosnzsz*sizeof(double));
	}
    }

    for (weight = 1.0; IsList(tl); weight += 1.0, ++i)
    {
	pword *car = vl.ptr;
	pword *cdr = car + 1;

	Dereference_(car);
	if (!IsInteger(car->tag))
	    { Bip_Error(TYPE_ERROR); }

	lpd->sostype[i] = (char) vsostype.nint;
	lpd->sosind[i] = (int) car->val.nint;
	lpd->sosref[i] = weight;

	Dereference_(cdr);
	tl = cdr->tag;
	vl = cdr->val;
    }
    Check_List(tl);
    if (i != lpd->nsosnz)
	{ Bip_Error(RANGE_ERROR); }

    if (lpd->prob_type == PROBLEM_QP) 
	lpd->prob_type = PROBLEM_MIQP;
    else 
	lpd->prob_type = PROBLEM_MIP;
    Succeed;
}


int
p_cpx_get_objval(value vlp, type tlp, value v, type t)
{
    lp_desc *lpd; 

    LpDescOnly(vlp, tlp, lpd);
    Return_Unify_Float(v, t, lpd->objval);
}


#if 0 /* not used */
int
p_cpx_get_coef(value vlp, type tlp, 
	       value vi, type ti, 
	       value vj, type tj, 
	       value vc, type tc)
{
#ifdef CPLEX
    lp_desc *lpd; 
    double d;
    LpDesc(vlp, tlp, lpd);
    SetPreSolve(lpd->presolve);
    Check_Integer(ti);
    Check_Integer(tj);
    if (vi.nint >= lpd->mar || vj.nint >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    if (CPXgetcoef(cpx_env, lpd->lp, vi.nint, vj.nint, &d) != 0)
	{ Bip_Error(EC_EXTERNAL_ERROR); }
    Return_Unify_Float(vc, tc, d);
#else
    Bip_Error(UNIMPLEMENTED);
#endif
}

#endif

/*
 * Retrieve the matrix coefficients:
 * - First call cplex_get_row(+CPH, +CType, +I, -Base) which 
 *   prepares for retrieval of row i of constraint type CType:
 *       normal constraints:
 *          retrieves the coefficients of row I into the rmatind/rmatval 
 *          arrays 
 *       cutpool constraints:
 *          setup lpd->nnz to the non-zero size for row i, and set Base
 *          to the offset to coefficients for row i in the appropriate
 *          rmatind/rmatval  arrays
 * - Then call cplex_get_col_coef(+CPH, +CType, +Base, -J, -C) which returns 
 *   one nonzero column J and the corresponding coefficient C. Successive
 *   calls return the other nonzero columns in decreasing order. When no
 *   nonzero column is left, cplex_get_col_coef/3 fails. The row number is
 *   the one given in the preceding cpx_get_row/2 call.  */

int
p_cpx_get_row(value vlp, type tlp, value vpool, type tpool,  
	      value vi, type ti, value vbase, type tbase)
{
    lp_desc *lpd;
    int base;
    LpDesc(vlp, tlp, lpd);

    Check_Integer(ti);

    switch (vpool.nint)
    {
    case CSTR_TYPE_NORM:
    {
	int ncols; 
	int rmatbeg[2];
#ifdef CPLEX
	int surplus;
#endif
	base = 0; /* read one constraint only */
	ncols = lpd->mac;
	if (ncols > lpd->nnz_sz)	/* allocate/grow arrays */
	{
	    if (lpd->nnz_sz == 0)
	    {
		lpd->nnz_sz = ncols;
		lpd->rmatind = (int *) Malloc(ncols*sizeof(int));
		lpd->rmatval = (double *) Malloc(ncols*sizeof(double));
	    }
	    else
	    {
		lpd->nnz_sz = ncols;
		lpd->rmatind = (int *) Realloc(lpd->rmatind, ncols*sizeof(int));
		lpd->rmatval = (double *) Realloc(lpd->rmatval, ncols*sizeof(double));
	    }
	}
	SetPreSolve(lpd->presolve);
	/* note that for COIN, CPXgetrows maps to coin_getrow, which gets
	   one row only
	*/
	if (
	    CPXgetrows(cpx_env, lpd->lp, &lpd->nnz, rmatbeg, lpd->rmatind,
	        lpd->rmatval, lpd->nnz_sz, &surplus, vi.nint, vi.nint)
/*
#ifdef XPRESS
	    XPRSgetrows(lpd->lp, rmatbeg, lpd->rmatind, lpd->rmatval, lpd->nnz_sz, &lpd->nnz,
	        vi.nint, vi.nint)
#endif
*/
	    )
	{ Bip_Error(EC_EXTERNAL_ERROR); }
	break;
    }
/*
    case CSTR_TYPE_PERMCP:
	base = lpd->cp_rmatbeg[vi.nint];
	lpd->nnz = (vi.nint == lpd->cp_nr-1 ? lpd->cp_nnz-base : lpd->cp_rmatbeg[vi.nint+1]-base);
	break;
*/
    case CSTR_TYPE_CONDCP:
	base = lpd->cp_rmatbeg2[vi.nint];
	lpd->nnz = (vi.nint == lpd->cp_nr2-1 ? lpd->cp_nnz2-base : lpd->cp_rmatbeg2[vi.nint+1]-base);
	
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }
    
    Return_Unify_Integer(vbase, tbase, base);
}


/* returns the coeff vc for vj'th argument of the Prolog variable array
   NOTE: assumes variable array created from a list in reverse column order
*/
int
p_cpx_get_col_coef(value vlp, type tlp, value vpool, type tpool, 
		   value vbase, type tbase, 
		   value vj, type tj, value vc, type tc)
{
    int i;
    lp_desc *lpd; 
    Prepare_Requests
    LpDescOnly(vlp, tlp, lpd);
    if (lpd->nnz == 0)
	{ Fail; }
    --lpd->nnz;
    i = vbase.nint + lpd->nnz;
    switch (vpool.nint)
    {
/*
    case CSTR_TYPE_PERMCP:
	Request_Unify_Integer(vj, tj, (lpd->mac+SOLVER_MAT_BASE - lpd->cp_rmatind[i]));
	Request_Unify_Float(vc, tc, lpd->cp_rmatval2[i]);
	break;
*/
    case CSTR_TYPE_CONDCP:
	Request_Unify_Integer(vj, tj, (lpd->mac+SOLVER_MAT_BASE - lpd->cp_rmatind2[i]));
	Request_Unify_Float(vc, tc, lpd->cp_rmatval2[i]);
	break;
    case CSTR_TYPE_NORM:
	Request_Unify_Integer(vj, tj, (lpd->mac+SOLVER_MAT_BASE - lpd->rmatind[i]));
	Request_Unify_Float(vc, tc, lpd->rmatval[i]);
	break;
    default:
	Bip_Error(RANGE_ERROR);
    }
    Return_Unify;
}

int
p_cpx_get_obj_coef(value vlp, type tlp, value vj, type tj, value vc, type tc)
{
    lp_desc *lpd; 
    double d[1];
    Check_Integer(tj);
    LpDesc(vlp, tlp, lpd);
    if (vj.nint >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    SetPreSolve(lpd->presolve);
    if (CPXgetobj(cpx_env, lpd->lp, d, (int) vj.nint, (int) vj.nint) != 0)
	{ Bip_Error(EC_EXTERNAL_ERROR); }
    Return_Unify_Float(vc, tc, d[0]);
}


/*----------------------------------------------------------------------*
 * Global stack arrays (used for answer vectors)
 *----------------------------------------------------------------------*/


int
p_create_darray(value vi, type ti, value varr, type tarr)
{
    pword *pbuf;
    Check_Integer(ti);
    pbuf = _create_darray(vi.nint);
    Return_Unify_String(varr, tarr, pbuf);
}

static pword *
_create_carray(int i)
{
    pword *pbuf = TG;
    Push_Buffer(i*sizeof(char) + 1);
    return pbuf;
}

static pword *
_create_darray(int i)
{
    pword *pbuf = TG;
    Push_Buffer(i*sizeof(double) + 1);
    return pbuf;
}

static pword *
_create_iarray(int i)
{
    pword *pbuf = TG;
    Push_Buffer(i*sizeof(int) + 1);
    return pbuf;
}

int
p_darray_size(value varr, type tarr, value vi, type ti)
{
    Check_Array(tarr);
    Return_Unify_Integer(vi, ti, DArraySize(varr.ptr));
}

int
p_get_darray_element(value varr, type tarr, value vi, type ti, value vel, type tel)
{
    double f;
    Check_Array(tarr);
    Check_Integer(ti);
    /* RANGE_ERROR if vi.nint is negative -- as it is a large unsigned */
    if ((unsigned) vi.nint >= DArraySize(varr.ptr))
	{ Bip_Error(RANGE_ERROR); }
    f = ((double *) BufferStart(varr.ptr))[vi.nint];
    Return_Unify_Float(vel, tel, f);
}

int
p_set_darray_element(value varr, type tarr, value vi, type ti, value vel, type tel)
{
    Check_Array(tarr);
    Check_Integer(ti);
    Check_Float(tel);
    Check_Number(tel);

    if ((unsigned) vi.nint >= DArraySize(varr.ptr))
	{ Bip_Error(RANGE_ERROR); }
    if (GB <= varr.ptr  &&  varr.ptr < TG)
    {
	((double *) BufferStart(varr.ptr))[vi.nint] = DoubleVal(vel, tel);
	Succeed;
    }
    else	/* nondeterministic */
    {
    	Bip_Error(UNIMPLEMENTED);
    }
}

int
p_darray_list(value varr, type tarr, value vmr, type tmr, value vlst, type tlst)
{
    pword	list;
    pword	*car;
    pword	*cdr = &list;
    unsigned	i;

    Check_Array(tarr);
    Check_Integer(tmr);
    if (vmr.nint > DArraySize(varr.ptr)) Bip_Error(RANGE_ERROR);
    for (i = 0; i < vmr.nint; ++i)
    {
	car = TG;
	Push_List_Frame();
	Make_List(cdr, car);
	Make_Float(car, ((double *) BufferStart(varr.ptr))[i]);
	cdr = car + 1;
    }
    Make_Nil(cdr);
    Return_Unify_Pw(vlst, tlst, list.val, list.tag);
}

/* returns the base (start) of the solver matrix (0 or 1) */
int
p_cpx_matrix_base(value vbase, type tbase)
{
    Return_Unify_Integer(vbase, tbase, SOLVER_MAT_BASE);
}

int
p_cpx_matrix_offset(value voff, type toff)
{
    Return_Unify_Integer(voff, toff, SOLVER_MAT_OFFSET);
}


/*----------------------------------------------------------------------*
 * CutPools
 *----------------------------------------------------------------------*/

/* cutpools implemented using the rowwise representation of a problem
   used by CPLEX and Xpress. These are then added to the problem before
   optimisation. There two types of cutpools: unconditional and
   conditional, each representing by its own data structures in the handle.
   Rows in the conditional cutpool have a `name' associated with them, which
   group the rows into different virtual pools. These virtual pools correspond
   conceptually to the different cutpools in a MP solver
*/

int
p_cpx_get_cutpool_size(value vlp, type tlp,  value vnr, type tnr, value vnnz, type tnnz)
{
    lp_desc *lpd; 
    Prepare_Requests
    LpDescOnly(vlp, tlp, lpd);

    Request_Unify_Integer(vnr, tnr, lpd->cp_nr2);
    Request_Unify_Integer(vnnz, tnnz, lpd->cp_nnz2);
    Return_Unify;
}

int
p_cpx_reset_cutpool_size(value vlp, type tlp, 
     value vnr, type tnr, value vnnz, type tnnz)
{
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(tnr);
    Check_Integer(tnnz);

    if (vnr.nint > lpd->cp_nr2 || vnr.nint < 0) {Bip_Error(RANGE_ERROR);}
    lpd->cp_nr2 = vnr.nint;
    if (vnnz.nint > lpd->cp_nnz2 || vnnz.nint < 0) {Bip_Error(RANGE_ERROR);}
    lpd->cp_nnz2 = vnnz.nint;
    Succeed;
}

int
p_cpx_set_cpcstr_cond(value vlp, type tlp, value vidx, type tidx,
		      value vtype, type ttype, value vc, type tc)
{
    int i; 
    lp_desc *lpd;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tidx);
    Check_Integer(ttype);
    Check_Integer(tc);

    if (vidx.nint < 0 || vidx.nint >= lpd->cp_nr2) { Bip_Error(RANGE_ERROR); }
    switch (vtype.nint)
    {
    case CP_ACTIVE: /* active state */
	if (vc.nint != 0 && vc.nint != 1) { Bip_Error(RANGE_ERROR); } 
	lpd->cp_active2[vidx.nint] = (char) vc.nint;
	break;
    case CP_ADDINIT: /* add initially */
	if (vc.nint != 0 && vc.nint != 1) { Bip_Error(RANGE_ERROR); } 
	lpd->cp_initial_add2[vidx.nint] = (char) vc.nint;
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }

    Succeed;
}

int
p_cpx_init_cpcstr(value vlp, type tlp, value vidx, type tidx, value vgrp, type tgrp,
		   value vact, type tact, value vinit_add, type tinit_add)
{
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(tidx);
    Check_Integer(tgrp);
    Check_Integer(tact);
    Check_Integer(tinit_add);

    lpd->cp_initial_add2[vidx.nint] = (char) vinit_add.nint;
    lpd->cp_active2[vidx.nint] = (char) vact.nint;
    if (lpd->cp_pools_max2[vgrp.nint] >= lpd->cp_pools_sz2[vgrp.nint])
    {
	lpd->cp_pools_sz2[vgrp.nint] += NEWROW_INCR;
	lpd->cp_pools2[vgrp.nint] = (int *) Realloc(lpd->cp_pools2[vgrp.nint],lpd->cp_pools_sz2[vgrp.nint]*sizeof(int));
    }
    lpd->cp_pools2[vgrp.nint][lpd->cp_pools_max2[vgrp.nint]++] = vidx.nint;

    return PSUCCEED;
}

#define Expand_Named_CutPool_Arrays(lpd, n) { \
    int sz = lpd->cp_npools_sz2 += CUTPOOL_INCR; \
    /* expand the arrays */\
    lpd->cp_pools2 = (int **) Realloc(lpd->cp_pools2,sz*sizeof(int *));\
    lpd->cp_pools_max2 = (int *) Realloc(lpd->cp_pools_max2,sz*sizeof(int));\
    lpd->cp_pools_sz2 = (int *) Realloc(lpd->cp_pools_sz2,sz*sizeof(int));\
    lpd->cp_names2 = (char **) Realloc(lpd->cp_names2,sz*sizeof(char *));\
    for (i = n+1; i < sz; i++) \
    { /* initialise the new elements (except n - which will be filled \
         by Create_New_CutPool) */\
	lpd->cp_pools2[i] = NULL;\
        lpd->cp_names2[i] = NULL; \
	lpd->cp_pools_max2[i] = 0;\
	lpd->cp_pools_sz2[i] = 0;\
    }\
}

#define Create_New_CutPool(lpd, n, name) {                      \
    lpd->cp_pools_sz2[n] = NEWROW_INCR;                        \
    lpd->cp_pools_max2[n] = 0;                                  \
    lpd->cp_pools2[n] = Malloc(NEWROW_INCR*sizeof(int));        \
    lpd->cp_names2[n] = Malloc((strlen(name)+1)*sizeof(char));  \
    strcpy(lpd->cp_names2[n], name);                            \
}


int
p_cpx_get_named_cp_index(value vlp, type tlp, value vname, type tname,
		         value vnew, type tnew, value vidx, type tidx)
{
    int i, n;
    lp_desc *lpd;
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(tnew);

    if (lpd->cp_npools_sz2 == 0)
    {/* create the default group the first time we use the named groups */
	lpd->cp_npools2 = 1;
	Expand_Named_CutPool_Arrays(lpd, 0);
	Create_New_CutPool(lpd, 0, "[]");
    }
    if (IsNil(tname)) i = 0; /* default group */
    else
    {/* user defined named group */
	Check_Atom(tname);
	for(i=1; i < lpd->cp_npools2; ++i)  
	{
	    if (strcmp(lpd->cp_names2[i], DidName(vname.did)) == 0) break;
	}
	if (i == lpd->cp_npools2) 
	{/* name was not found */	    
	    if (vnew.nint == 0) { Fail; } /* don't create new group */
	    else if ((n = lpd->cp_npools2++) >= lpd->cp_npools_sz2)
	    { 
		Expand_Named_CutPool_Arrays(lpd, n);
	    }
	    Create_New_CutPool(lpd, n, DidName(vname.did));
	}
		
    }

    Return_Unify_Integer(vidx, tidx, i);
}


int
p_cpx_get_cpcstr_info(value vlp, type tlp, value vidx, type tidx, 
		      value vitype, type titype, value vval, type tval)
{
    int i, val;

    lp_desc *lpd;
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(titype);
    Check_Integer(tidx);

    if (vidx.nint < 0 || vidx.nint >= lpd->cp_nr2) { Bip_Error(RANGE_ERROR);}

    switch (vitype.nint)
    {
    case CP_ACTIVE: /* active state */
	val = (int) lpd->cp_active2[vidx.nint];
	break;
    case CP_ADDINIT:
	val = (int) lpd->cp_initial_add2[vidx.nint];
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
     }

    Return_Unify_Integer(vval, tval, val);
}

int
p_cpx_get_named_cpcstr_indices(value vlp, type tlp, value vpidx, type tpidx,
				value vilst, type tilst)
{
    int i;
    pword list;
    pword * head, * next = &list;

    lp_desc *lpd;
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(tpidx);

    if (vpidx.nint < 0 || vpidx.nint >= lpd->cp_npools2) Bip_Error(RANGE_ERROR); 
    for (i=0; i < lpd->cp_pools_max2[vpidx.nint]; i++)
    {
	head = TG;
	Push_List_Frame();
	Make_List(next, head);
	Make_Integer(head, lpd->cp_pools2[vpidx.nint][i]);
	next = head + 1;
    }
    Make_Nil(next);
    Return_Unify_Pw(vilst, tilst, list.val, list.tag);
}


/*----------------------------------------------------------------------*
 * Extending basis matrices
 *----------------------------------------------------------------------*/


int
p_create_extended_iarray(value varr, type tarr, value vi, type ti, value vxarr, type txarr)
{
    pword *pbuf;

    Check_Integer(ti);
    Check_Array(tarr);
    pbuf = _create_iarray(vi.nint + IArraySize(varr.ptr));
    Return_Unify_String(vxarr, txarr, pbuf);
}

int
p_create_extended_darray(value varr, type tarr, value vi, type ti, value vxarr, type txarr)
{
    pword *pbuf;

    Check_Integer(ti);
    Check_Array(tarr);
    pbuf = _create_darray(vi.nint + DArraySize(varr.ptr));
    Return_Unify_String(vxarr, txarr, pbuf);
}

int
p_copy_extended_column_basis(value varr, type tarr, value vxarr, type txarr)
{
    unsigned i;
    int      *v;
    int      *vx;

    Check_Array(tarr);
    Check_Array(txarr);

    v = IArrayStart(varr.ptr);
    vx = IArrayStart(vxarr.ptr);

    /*
     * Note that this assumes the basis status array contains
     * the status of the problem variables (columns) only
     * and not the status of the slack variables (rows).
     * We want to add status for the new columns so we must:
     * 1) copy the existing variables' status
     * 2) add the CPX_COL_FREE_SUPER status for the new columns
     */
    for (i = 0; i < IArraySize(varr.ptr); ++i)
    {
      vx[i] = v[i];
    }
    for (i = IArraySize(varr.ptr); i < IArraySize(vxarr.ptr); ++i)
    {
      vx[i] = CPX_COL_FREE_SUPER;
    }
    Succeed;
}

int
p_copy_extended_arrays(value vbarr, type tbarr, value vsarr, type tsarr, value vdarr, type tdarr, value vxbarr, type txbarr, value vxsarr, type txsarr, value vxdarr, type txdarr)
{
    int     i;
    int     *vb;
    int     *vxb;
    double  *vs;
    double  *vxs;
    double  *vd;
    double  *vxd;

    Check_Array(tbarr);
    Check_Array(tsarr);
    Check_Array(tdarr);
    Check_Array(txbarr);
    Check_Array(txsarr);
    Check_Array(txdarr);

    vb = IArrayStart(vbarr.ptr);
    vxb = IArrayStart(vxbarr.ptr);
    vs = DArrayStart(vsarr.ptr);
    vxs = DArrayStart(vxsarr.ptr);
    vd = DArrayStart(vdarr.ptr);
    vxd = DArrayStart(vxdarr.ptr);

    /*
     * Note that this assumes the basis status array contains
     * the status of the problem variables (columns) only
     * and not the status of the slack variables (rows).
     * We want to add status for the new columns so we must:
     * 1) copy the existing variables' status, solution value
     * 2) add the CPX_COL_FREE_SUPER status, 0.0 solution value
     *    and 0.0 reduced cost for the new columns
     */
    for (i = 0; i < IArraySize(vbarr.ptr); ++i)
    {
      vxb[i] = vb[i];
      vxs[i] = vs[i];
      vxd[i] = vd[i];
    }
    for (i = IArraySize(vbarr.ptr); i < IArraySize(vxbarr.ptr); ++i)
    {
      vxb[i] = CPX_COL_FREE_SUPER;
      vxs[i] = 0.0;
      vxd[i] = 0.0;
    }
    Succeed;
}


/*----------------------------------------------------------------------*
 * Accessing iarrays
 *----------------------------------------------------------------------*/


int
p_create_iarray(value vi, type ti, value varr, type tarr)
{
    pword *pbuf;
    Check_Integer(ti);
    pbuf = _create_iarray(vi.nint);
    Return_Unify_String(varr, tarr, pbuf);
}

int
p_iarray_size(value varr, type tarr, value vi, type ti)
{
    Check_Array(tarr);
    Return_Unify_Integer(vi, ti, IArraySize(varr.ptr));
}

int
p_get_iarray_element(value varr, type tarr, value vi, type ti, value vel, type tel)
{
    int i;
    Check_Array(tarr);
    Check_Integer(ti);
    if ((unsigned) vi.nint >= IArraySize(varr.ptr))
	{ Bip_Error(RANGE_ERROR); }
    i = ((int *) BufferStart(varr.ptr))[vi.nint];
    Return_Unify_Integer(vel, tel, i);
}

int
p_set_iarray_element(value varr, type tarr, value vi, type ti, value vel, type tel)
{
    Check_Array(tarr);
    Check_Integer(ti);
    Check_Integer(tel);
    if ((unsigned) vi.nint >= IArraySize(varr.ptr))
	{ Bip_Error(RANGE_ERROR); }
    if (GB <= varr.ptr  &&  varr.ptr < TG)
    {
	((int *) BufferStart(varr.ptr))[vi.nint] = vel.nint;
	Succeed;
    }
    else	/* nondeterministic */
    {
    	Bip_Error(UNIMPLEMENTED);
    }
}

int
p_iarray_list(value varr, type tarr, value vlst, type tlst)
{
    pword      list;
    pword      *car;
    pword      *cdr = &list;
    unsigned   i;

    Check_Array(tarr);
    for (i = 0; i < IArraySize(varr.ptr); ++i)
      {
	car = TG;
	Push_List_Frame();
	Make_List(cdr, car);
	Make_Integer(car, ((int *) BufferStart(varr.ptr))[i]);
	cdr = car + 1;
      }
    Make_Nil(cdr);
    Return_Unify_Pw(vlst, tlst, list.val, list.tag);
}

#ifdef DUMPMAT

int
dump_problem(lp_desc * lpd)
{
    int i;
    Fprintf(log_output_, "\n\
    	lpd->macsz = %d;\n\
    	lpd->marsz = %d;\n\
    	lpd->matnz = %d;\n\
    	lpd->mac = %d;\n\
    	lpd->mar = %d;\n\
	lpd->rhsx = a_rhsx;\n\
	lpd->senx = a_senx;\n\
	lpd->matbeg = a_matbeg;\n\
	lpd->matcnt = a_matcnt;\n\
	lpd->matind = a_matind;\n\
	lpd->matval = a_matval;\n\
	lpd->bdl = a_bdl;\n\
	lpd->bdu = a_bdu;\n\
	lpd->objx = a_objx;\n\
	lpd->ctype = a_ctype;",
      lpd->macsz, lpd->marsz, lpd->matnz,
      lpd->mac, lpd->mar);

    Fprintf(log_output_, "\n\
	/*\n\
	 * Problem data\n\
	 */\n\
	 ");
    Fprintf(log_output_, "double a_objx[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->objx[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "double a_bdl[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->bdl[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "double a_bdu[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->bdu[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "int a_matbeg[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "%d,\n", lpd->matbeg[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "int a_matcnt[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "%d,\n", lpd->matcnt[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "char a_ctype[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "'%c',\n", lpd->ctype[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "double a_rhsx[%d] ={\n", lpd->mar);
    for (i=0; i<lpd->mar; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->rhsx[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "char a_senx[%d] ={\n", lpd->mar);
    for (i=0; i<lpd->mar; ++i)
	Fprintf(log_output_, "'%c',\n", lpd->senx[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "int a_matind[%d] ={\n", lpd->matnz);
    for (i=0; i<lpd->matnz; ++i)
	Fprintf(log_output_, "%d,\n", lpd->matind[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "double a_matval[%d] ={\n", lpd->matnz);
    for (i=0; i<lpd->matnz; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->matval[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "\n\
	/*\n\
	 * End data\n\
	 */\n\
	 ");

# if 0
    Fprintf(log_output_, "\n\
    	lpd->cb_sz = %d;\n\
	lpd->cb_index = a_cb_index;\n\
	lpd->cb_index2 = a_cb_index2;\n\
	lpd->cb_value = a_cb_value;\n\
    	lpd->cb_cnt = %d;",
	lpd->cb_cnt, lpd->cb_cnt);
    Fprintf(log_output_, "int a_cb_index[%d] ={\n", lpd->cb_cnt);
    for (i=0; i<lpd->cb_cnt; ++i)
	Fprintf(log_output_, "%d,\n", lpd->cb_index[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "int a_cb_index2[%d] ={\n", lpd->cb_cnt);
    for (i=0; i<lpd->cb_cnt; ++i)
	Fprintf(log_output_, "%d,\n", lpd->cb_index2[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "double a_cb_value[%d] ={\n", lpd->cb_cnt);
    for (i=0; i<lpd->cb_cnt; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->cb_value[i]);
    Fprintf(log_output_, "};\n\n");
# endif
}
#endif
