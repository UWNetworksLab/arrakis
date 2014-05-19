#include <math.h>
#include "RCCE.h"
#include "applu_share.h"
#include "applu_macros.h"
#include "applu_protos.h"

#define   v(m,i,j,k)   v[m-1+5*((i+1)+(ldx+4)*((j+1)+(ldy+4)*(k-1)))]
#define   sum(m)       sum[m-1]
#define   dummy(m)     dummy[m-1]

void l2norm(int ldx, int ldy, int ldz, double *v, double *sum ) {

//c   to compute the l2-norm of vector v.

      int i, j, k, m;
      double dummy[5];

      for (m=1; m<=5; m++) dummy(m) = 0.0;
   
      for (k=2; k<=nz0-1; k++)
      for (j=jst; j<=jend; j++) 
         for (i=ist; i<=iend; i++) 
            for (m=1; m<=5; m++) 
                  dummy(m)+= v(m,i,j,k) * v(m,i,j,k);

//c   compute the global sum of individual contributions to dot product.

      RCCE_allreduce( (char*)dummy, (char*)sum, 5, RCCE_DOUBLE, RCCE_SUM, RCCE_COMM_WORLD);

      for (m=1; m<=5; m++)
         sum(m) = sqrt ( sum(m) / ( (nx0-2)*(ny0-2)*(nz0-2) ) );

      return;
}

