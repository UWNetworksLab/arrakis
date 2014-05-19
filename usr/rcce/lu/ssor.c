#include <stdio.h>
#include <stdlib.h>
#include "RCCE.h"
#include "applu_share.h"
#include "applu_macros.h"

void ssor(int niter) {

//c---------------------------------------------------------------------
//c  local variables
//c---------------------------------------------------------------------
      int i, j, k, m, mm, nn;
      int istep, one=1;
      double  tmp;
      double tv[5*isiz1*isiz2];
      double wtime;

//c---------------------------------------------------------------------
//c   begin pseudo-time stepping iterations
//c---------------------------------------------------------------------
      tmp = 1.0 / ( omega * ( 2.0 - omega ) );

//c---------------------------------------------------------------------
//c   initialize a,b,c,d to zero (guarantees that page tables have been
//c   formed, if applicable on given architecture, before timestepping).
//c---------------------------------------------------------------------
      for (m=1; m<=isiz2; m++) {
         for (k=1; k<=isiz1; k++) {
            for (j=1; j<=5; j++) {
               for (i=1; i<=5; i++) {
                  a(i,j,k,m) = 0.0;
                  b(i,j,k,m) = 0.0;
                  c(i,j,k,m) = 0.0;
                  d(i,j,k,m) = 0.0;
               }
            }
         }
      }

//c---------------------------------------------------------------------
//c   compute the steady-state residuals
//c---------------------------------------------------------------------
       rhs();

      timer_clear(&one);
      timer_start(&one);

//c---------------------------------------------------------------------
//c   the timestep loop
//c---------------------------------------------------------------------
      for (istep = 1; istep<= niter; istep++) {

         rhs();
         if (id == 0) {
            if ( istep%20 == 0 || istep == itmax || istep == 1) {
               if (niter > 1) printf(" Time step %4d\n", istep);
            }
         }
 
//c---------------------------------------------------------------------
//c   perform SSOR iteration
//c---------------------------------------------------------------------
         for (k = 2; k<= nz - 1; k++) {
            for (j = jst; j<= jend; j++) {
               for (i = ist; i<= iend; i++) {
                  for (m = 1; m<= 5; m++) {
                     rsd(m,i,j,k) = dt * rsd(m,i,j,k);
                  }
               }
            }
         }

         for (k = 2; k<= nz -1 ; k++) {
//c---------------------------------------------------------------------
//c   form the lower triangular part of the jacobian matrix
//c---------------------------------------------------------------------

            jacld(k);

//c---------------------------------------------------------------------
//c   perform the lower triangular solution
//c---------------------------------------------------------------------
            blts(k);

          }

          for (k = nz - 1; k>= 2; k--) {
//c---------------------------------------------------------------------
//c   form the strictly upper triangular part of the jacobian matrix
//c---------------------------------------------------------------------
            jacu(k);

//c---------------------------------------------------------------------
//c   perform the upper triangular solution
//c---------------------------------------------------------------------
            buts(k, tv);
          }

//c---------------------------------------------------------------------
//c   update the variables
//c---------------------------------------------------------------------
 
         for (k = 2; k<= nz-1; k++) {
            for (j = jst; j<= jend; j++) {
               for (i = ist; i<= iend; i++) {
                  for (m = 1; m<= 5; m++) {
                     u( m, i, j, k ) = u( m, i, j, k )
                          + tmp * rsd( m, i, j, k );
                  }
               }
            }
         }
 
//c---------------------------------------------------------------------
//c   compute the steady-state residuals
//c---------------------------------------------------------------------
         rhs();
 
//c---------------------------------------------------------------------
//c   compute the max-norms of newton iteration residuals
//c---------------------------------------------------------------------

         if (istep == itmax) {
            l2norm( isiz1, isiz2, isiz3, rsd, rsdnm );            
         }

      }

      timer_stop(&one);
      wtime = timer_read(&one);
 
      RCCE_allreduce((char *)(&wtime), (char *)&maxtime, 1, RCCE_DOUBLE, RCCE_MAX, RCCE_COMM_WORLD);
 
      return;
}
      
