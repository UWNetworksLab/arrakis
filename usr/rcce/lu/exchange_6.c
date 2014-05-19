#include "RCCE.h"
#include <stdlib.h>
#include <stdio.h>
#include "directions.h"
#include "applu_share.h"
#include "applu_macros.h"

#define g(i,j) g[i+(isiz2+2)*(j)]
#define COMM_SIZE 1024

extern int rcce_curphase;

void exchange_6(double *g, int jbeg, int jfin1) {

      int k;
      size_t chunk;
      double bufin[COMM_SIZE], bufout[COMM_SIZE];

      rcce_curphase = 1;

//c---------------------------------------------------------------------
//c   communicate in the east and west directions
//c---------------------------------------------------------------------

//c---------------------------------------------------------------------
//c   receive from east
//c---------------------------------------------------------------------
      if (jfin1 == ny) {
        RCCE_recv((char*)bufin, nz*sizeof(double), east);

        for (k=1; k<=nz; k++) {
          g(ny+1,k) = bufin[k-1];
        }

      }

//c---------------------------------------------------------------------
//c   send west
//c---------------------------------------------------------------------
      if (jbeg == 1) {
        for (k=1; k<=nz; k++) bufout[k-1] = g(1,k);

        RCCE_send((char*)bufout, nz*sizeof(double), west);
      }

      return;
}
