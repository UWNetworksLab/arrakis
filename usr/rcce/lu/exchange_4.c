#include "RCCE.h"
#include <stdlib.h>
#include <stdio.h>
#include "directions.h"
#include "applu_share.h"
#include "applu_macros.h"

#define g(i,j) g[i+(isiz2+2)*(j)]
#define h(i,j) h[i+(isiz2+2)*(j)]
#define COMM_SIZE 1024

extern int rcce_curphase;

void exchange_4(double *g, double *h, 
                int ibeg, int ifin1, int jbeg, int jfin1) {

      int i, j;
      size_t chunk;
      double bufin[COMM_SIZE], bufout[COMM_SIZE];
      int ny2 = ny + 2;

//c---------------------------------------------------------------------
//c   communicate in the east and west directions
//c---------------------------------------------------------------------

      rcce_curphase = 1;

//c---------------------------------------------------------------------
//c   receive from east
//c---------------------------------------------------------------------
      if (jfin1 == ny) {
        RCCE_recv((char*)bufin, 2*nx*sizeof(double), east);

        for (int ib=0, i=1; i<=nx; i++) {
          g(i,ny+1) = bufin[ib++];
          h(i,ny+1) = bufin[ib++];
        }

      }

//c---------------------------------------------------------------------
//c   send west
//c---------------------------------------------------------------------
      if (jbeg == 1) {
        for (int ib=0,i=1; i<=nx; i++) {
          bufout[ib++] = g(i,1);
          bufout[ib++] = h(i,1);
        }

        RCCE_send((char*)bufout, 2*nx*sizeof(double), west);
      }

//c---------------------------------------------------------------------
//c   communicate in the south and north directions
//c---------------------------------------------------------------------

      rcce_curphase = 0;

//c---------------------------------------------------------------------
//c   receive from south
//c---------------------------------------------------------------------
      if (ifin1 == nx) {
        RCCE_recv((char*)bufin, 2*ny2*sizeof(double), south);

        for (int ib=0,j=0; j<=ny+1; j++) {
          g(nx+1,j) = bufin[ib++];
          h(nx+1,j) = bufin[ib++];
        }

      }

//c---------------------------------------------------------------------
//c   send north
//c---------------------------------------------------------------------
      if (ibeg == 1) {
        for (int ib=0,j=0; j<=ny+1; j++) {
          bufout[ib++] = g(1,j);
          bufout[ib++] = h(1,j);
        }
        RCCE_send((char*)bufout, 2*ny2*sizeof(double), north);
      }

      return;
}

