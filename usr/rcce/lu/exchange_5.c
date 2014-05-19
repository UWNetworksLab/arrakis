#include "RCCE.h"
#include <stdlib.h>
#include <stdio.h>
#include "directions.h"
#include "applu_share.h"
#include "applu_macros.h"

#define g(i,j) g[i+(isiz2+2)*(j)]
#define COMM_SIZE 1024

extern int rcce_curphase;

void exchange_5(double *g, int ibeg, int ifin1) {


      int k;
      size_t chunk;
      double bufin[COMM_SIZE], bufout[COMM_SIZE];

      rcce_curphase = 0;

//c---------------------------------------------------------------------
//c   communicate in the south and north directions
//c---------------------------------------------------------------------

//c---------------------------------------------------------------------
//c   receive from south
//c---------------------------------------------------------------------
      if (ifin1 == nx) {
        RCCE_recv((char*)bufin, nz*sizeof(double), south);

        for (k=1; k<=nz; k++) g(nx+1,k) = bufin[k-1];
      }

//c---------------------------------------------------------------------
//c   send north
//c---------------------------------------------------------------------
      if (ibeg == 1) {
        for (k=1; k<=nz; k++) bufout[k-1] = g(1,k);

        RCCE_send((char*)bufout, nz*sizeof(double), north);
      }

      return;
}

