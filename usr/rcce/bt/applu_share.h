#include "npbparams.h"
#include "applu_protos.h"
#include "RCCE.h"

extern double u[5*(isiz1+4)*(isiz2+4)*isiz3],
              rsd[5*(isiz1+4)*(isiz2+4)*isiz3],
              frct[5*(isiz1+4)*(isiz2+4)*isiz3],
              flux[5*(isiz1+2)*(isiz2+2)*isiz3];
extern double a[5*5*isiz1*isiz2],
              b[5*5*isiz1*isiz2],
              c[5*5*isiz1*isiz2],
              d[5*5*isiz1*isiz2];

extern double dt, omega, tolrsd[5], rsdnm[5], errnm[5], frc, ttotal;
extern double tolrsd1_def, tolrsd2_def, tolrsd3_def, tolrsd4_def, tolrsd5_def,
              omega_default;
extern double ce[5*13];

extern int ndim, id, num, xdim, ydim, row, col;
extern int ii1, ii2, ji1, ji2, ki1, ki2;
extern int itmax, invert; 
extern int ipr, ipr_default, inorm;
extern int north,south,east,west;
extern int nx0, ny0, nz0;
extern int nx, ny, nz;
extern int ist, iend, jst, jend, ipt, jpt;
extern int dp_type;
extern double tx1, ty1, tz1, 
              dx1, dy1, dz1, 
              tx2, ty2, tz2, 
              dx2, dy2, dz2, 
              tx3, ty3, tz3, 
              dx3, dy3, dz3, 
              dx4, dy4, dz4, 
              dx5, dy5, dz5, 
              dssp, c1,  c2,  
              c3,  c4,  c5;
extern double dxi, deta, dzeta;
extern double npmax, maxtime;
extern double *buf1_exch_1;

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
#pragma omp threadprivate(ipr, inorm)
#pragma omp threadprivate(itmax, invert, \
                    dt, omega, tolrsd, rsdnm, errnm, frc, ttotal, \
                    a, b, c, d)
#pragma omp threadprivate(ce)
#pragma omp threadprivate (id, ndim, num, xdim, ydim, row, col, \
                     north,south,east,west, buf1_exch_1, npmax, maxtime)
#endif
