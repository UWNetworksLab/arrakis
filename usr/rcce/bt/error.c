//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include <math.h>
#include "header.h"
#include "mpinpb.h"
#include "applu_macros.h"

#define u_exact(m) u_exact[m-1]
#define rms(m) rms[m-1]
#define rms_work(m) rms_work[m-1]

void error_norm(double rms[]) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     this function computes the norm of the difference between the
//     computed solution and the exact solution
//---------------------------------------------------------------------

      int c, i, j, k, m, ii, jj, kk, d, error;
      double xi, eta, zeta, u_exact[5], rms_work[5],
           add;

      for (m = 1; m <= 5; m++) {
         rms_work(m) = 0.0e0;
      }

      for (c = 1; c <= ncells; c++) {
         kk = 0;
         for (k = cell_low(3,c); k <= cell_high(3,c); k++) {
            zeta = (double)(k) * dnzm1;
            jj = 0;
            for (j = cell_low(2,c); j <= cell_high(2,c); j++) {
               eta = (double)(j) * dnym1;
               ii = 0;
               for (i = cell_low(1,c); i <= cell_high(1,c); i++) {
                  xi = (double)(i) * dnxm1;
                  exact_solution(xi, eta, zeta, u_exact);

                  for (m = 1; m <= 5; m++) {
                     add = u(m,ii,jj,kk,c)-u_exact(m);
                     rms_work(m) = rms_work(m) + add*add;
                  }
                  ii = ii + 1;
               }
               jj = jj + 1;
            }
            kk = kk + 1;
         }
      }

      RCCE_allreduce((char*)rms_work, (char*)rms, 5, RCCE_DOUBLE, RCCE_SUM, RCCE_COMM_WORLD);

      for (m = 1; m <= 5; m++) {
         for (d = 1; d <= 3; d++) {
            rms(m) = rms(m) / (double)(grid_points(d)-2);
         }
         rms(m) = sqrt(rms(m));
      }

      return;
}


//---------------------------------------------------------------------
//---------------------------------------------------------------------

void rhs_norm(double rms[]) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

      int c, i, j, k, d, m, error;
      double rms_work[5], add;

      for (m = 1; m <= 5; m++) {
         rms_work(m) = 0.0e0;
      }

      for (c = 1; c <= ncells; c++) {
         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     add = rhs(m,i,j,k,c);
                     rms_work(m) = rms_work(m) + add*add;
                  }
               }
            }
         }
      }

      RCCE_allreduce((char*)rms_work, (char*)rms, 5, RCCE_DOUBLE, RCCE_SUM, RCCE_COMM_WORLD);

      for (m = 1; m <= 5; m++) {
         for (d = 1; d <= 3; d++) {
            rms(m) = rms(m) / (double)(grid_points(d)-2);
         }
         rms(m) = sqrt(rms(m));
      }

      return;
}

