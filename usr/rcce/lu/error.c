#include "RCCE.h"
#include <math.h>
#include <stdio.h>
#include "applu_share.h"
#include "applu_macros.h"
#include "applu_protos.h"

#define u000ijk(m) u000ijk[m-1]
#define errnm(m)   errnm[m-1]
#define dummy(m)   dummy[m-1]

void error() {

//  local variables

      int i, j, k, m,  iglob, jglob;
      double  tmp, u000ijk[5], dummy[5];

      for (m=1; m<=5; m++) {
         errnm(m) = 0.0;
         dummy(m) = 0.0;
      }

      for (k=2; k<=(nz)-1; k++)
         for (j=jst; j<=(jend); j++) {
            jglob = jpt + j;
            for (i=ist;i<=(iend); i++) {
               iglob = ipt + i;
               exact(iglob, jglob, k, u000ijk);
               for (m=1; m<=5; m++) {
                  tmp = ( u000ijk(m) - u(m,i,j,k) );
                  dummy(m) = dummy(m) + tmp * tmp;
               }
            }
         }

//   compute the global sum of individual contributions to dot product.

     RCCE_allreduce((char*)dummy, (char*)errnm, 5, RCCE_DOUBLE, RCCE_SUM, RCCE_COMM_WORLD);

     for (m=1; m<=5; m++) {
         errnm(m) = sqrt ( errnm(m) / ( (nx0-2)*(ny0-2)*(nz0-2) ) );
     }
     return;
}
 
