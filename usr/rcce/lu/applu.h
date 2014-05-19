#ifdef _OPENMP
#include <omp.h>
#endif
#include "RCCE.h"
#include "npbparams.h"
#include "applu_protos.h"

//c---------------------------------------------------------------------
//c   npbparams.h defines parameters that depend on the class and 
//c   number of nodes
//c---------------------------------------------------------------------

      int ipr_default = 1;
      double omega_default = 1.2;
      double tolrsd1_def=1.0e-08, 
             tolrsd2_def=1.0e-08, tolrsd3_def=1.0e-08, 
             tolrsd4_def=1.0e-08, tolrsd5_def=1.0e-08;

      double c1 = 1.4, c2 = 0.40, c3 = 0.1, c4 = 1.0, c5 = 1.4;

//c---------------------------------------------------------------------
//c   grid
//c---------------------------------------------------------------------
      int nx, ny, nz;
      int nx0, ny0, nz0;
      int ipt, ist, iend;
      int jpt, jst, jend;
      int ii1, ii2;
      int ji1, ji2;
      int ki1, ki2;
      double  dxi, deta, dzeta;
      double  tx1, tx2, tx3;
      double  ty1, ty2, ty3;
      double  tz1, tz2, tz3;

//c---------------------------------------------------------------------
//c   dissipation
//c---------------------------------------------------------------------
      double dx1, dx2, dx3, dx4, dx5;
      double dy1, dy2, dy3, dy4, dy5;
      double dz1, dz2, dz3, dz4, dz5;
      double dssp;
//c---------------------------------------------------------------------
//c   field variables and residuals
//c---------------------------------------------------------------------
      double u[5*(isiz1+4)*(isiz2+4)*isiz3],
             rsd[5*(isiz1+4)*(isiz2+4)*isiz3],
             frct[5*(isiz1+4)*(isiz2+4)*isiz3],
             flux[5*(isiz1+2)*(isiz2+2)*isiz3];

//c---------------------------------------------------------------------
//c   output control parameters
//c---------------------------------------------------------------------
      int ipr, inorm;

//#pragma omp threadprivate(ipr, inorm)

//c---------------------------------------------------------------------
//c   newton-raphson iteration control parameters
//c---------------------------------------------------------------------
      int itmax, invert;
      double  dt, omega, tolrsd[5], rsdnm[5], errnm[5], frc, ttotal;

      double a[5*5*isiz1*isiz2],
             b[5*5*isiz1*isiz2],
             c[5*5*isiz1*isiz2],
             d[5*5*isiz1*isiz2];


//c---------------------------------------------------------------------
//c   coefficients of the exact solution
//c---------------------------------------------------------------------
      double ce[5*13];

//#pragma omp threadprivate(ce)

//c---------------------------------------------------------------------
//c   multi-processor common blocks
//c---------------------------------------------------------------------
      int id, ndim, num, xdim, ydim, row, col;

      int north,south,east,west;

      int npmax=isiz01+isiz02;

//      double buf[5*2*isiz2*isiz3], buf1[5*2*isiz2*isiz3];

      double maxtime;

//c---------------------------------------------------------------------
//c   coordination flags
//c---------------------------------------------------------------------
      RCCE_FLAG flagsent[4], flagready[4];
      double *buf1_exch_1;

#ifdef _OPENMP
#pragma omp threadprivate (nx, ny, nz, nx0, ny0, nz0, \
                     ipt, ist, iend, jpt, jst, jend, \
                     ii1, ii2, ji1, ji2, ki1, ki2, \
                     dxi, deta, dzeta, \
                     tx1, tx2, tx3, ty1, ty2, ty3, tz1, tz2, tz3)
#pragma omp threadprivate (dx1, dx2, dx3, dx4, dx5, \
                     dy1, dy2, dy3, dy4, dy5, \
                     dz1, dz2, dz3, dz4, dz5, \
                     dssp)
#pragma omp threadprivate(u, rsd, frct, flux)

#pragma omp threadprivate(itmax, invert, \
                    dt, omega, tolrsd, rsdnm, errnm, frc, ttotal, \
                    a, b, c, d)
#pragma omp threadprivate (id, ndim, num, xdim, ydim, row, col, \
                     north,south,east,west, flagsent, flagready, \
                     buf1_exch_1, npmax, maxtime)
#endif

//c---------------------------------------------------------------------
//c   end of include file
//c---------------------------------------------------------------------
