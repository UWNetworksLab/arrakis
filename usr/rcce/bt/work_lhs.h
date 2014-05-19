//---------------------------------------------------------------------
//---------------------------------------------------------------------
//
//  work_lhs.h
//
//---------------------------------------------------------------------
//---------------------------------------------------------------------
#ifndef __WORK_LHS_H
#define __WORK_LHS_H

#define fjac(m,n,i) fjac[(m-1)+5*((n-1)+5*(i+2))]
#define njac(m,n,i) njac[(m-1)+5*((n-1)+5*(i+2))]
#define lhsa(m,n,i) lhsa[(m-1)+5*((n-1)+5*(i+1))]
#define lhsb(m,n,i) lhsb[(m-1)+5*((n-1)+5*(i+1))]

#ifdef G_MAIN
      double fjac[5*5*(MAX_CELL_DIM+4)],
                       njac[5*5*(MAX_CELL_DIM+4)],
                       lhsa[5*5*(MAX_CELL_DIM+2)],
                       lhsb[5*5*(MAX_CELL_DIM+2)],
                       tmp1, tmp2, tmp3;
//      common /work_lhs/ fjac, njac, lhsa, lhsb, tmp1, tmp2, tmp3;
#else
extern double fjac[5*5*(MAX_CELL_DIM+4)],
                       njac[5*5*(MAX_CELL_DIM+4)],
                       lhsa[5*5*(MAX_CELL_DIM+2)],
                       lhsb[5*5*(MAX_CELL_DIM+2)],
                       tmp1, tmp2, tmp3;
#endif /*G_MAIN*/
#ifdef _OPENMP
#pragma omp threadprivate (fjac, njac, lhsa, lhsb, tmp1, tmp2, tmp3)
#endif
#endif
