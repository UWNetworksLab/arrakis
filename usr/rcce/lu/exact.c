#include "applu_share.h"

#define ce(m,n) ce[m-1+5*(n-1)]
#define u000ijk(m) u000ijk[m-1]

void exact( int i, int j, int k, double *u000ijk ) {

//   compute the exact solution at (i,j,k)

      int m;
      double xi, eta, zeta;

      xi   = ( double ) (i - 1) / (nx0 - 1);
      eta  = ( double ) (j - 1) / (ny0 - 1);
      zeta = ( double ) (k - 1) / (nz0 - 1);

      for (m=1; m<=5; m++)
         u000ijk(m) =  ce(m,1)
             + ce(m,2) * xi
             + ce(m,3) * eta
             + ce(m,4) * zeta
             + ce(m,5) * xi * xi
             + ce(m,6) * eta * eta
             + ce(m,7) * zeta * zeta
             + ce(m,8) * xi * xi * xi
             + ce(m,9) * eta * eta * eta
             + ce(m,10) * zeta * zeta * zeta
             + ce(m,11) * xi * xi * xi * xi
             + ce(m,12) * eta * eta * eta * eta
             + ce(m,13) * zeta * zeta * zeta * zeta;

      return;
}

