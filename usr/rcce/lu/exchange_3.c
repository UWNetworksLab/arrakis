#include "RCCE.h"
#include <stdlib.h>
#include <stdio.h>
#include "directions.h"
#include "applu_share.h"
#include "applu_macros.h"

#define g(m,i,j,k) g[m-1+5*((i+1)+(isiz1+4)*((j+1)+(isiz2+4)*(k-1)))]

extern int rcce_curphase;

void exchange_3(double *g,int iex) {

      int i, j, k;
      size_t chunk;
      double bufin[5*2*isiz2*isiz3], bufout[5*2*isiz2*isiz3];

      rcce_curphase = iex == 0 ? 0 : 1;

      if (iex == 0) {

//c---------------------------------------------------------------------
//c   communicate in the south and north directions
//c---------------------------------------------------------------------

//c---------------------------------------------------------------------
//c   send south
//c---------------------------------------------------------------------
      if (south != -1) {
        for (int ib=0,k=1; k<=nz; k++) {
          for (j=1; j<=ny; j++) {
            bufout[ib++] = g(1,nx-1,j,k) ;
            bufout[ib++] = g(2,nx-1,j,k) ;
            bufout[ib++] = g(3,nx-1,j,k) ;
            bufout[ib++] = g(4,nx-1,j,k) ;
            bufout[ib++] = g(5,nx-1,j,k) ;
            bufout[ib++] = g(1,nx,j,k);
            bufout[ib++] = g(2,nx,j,k);
            bufout[ib++] = g(3,nx,j,k);
            bufout[ib++] = g(4,nx,j,k);
            bufout[ib++] = g(5,nx,j,k);
          }
        }

        RCCE_send((char*)bufout, 10*ny*nz*sizeof(double), south);
     }

//c---------------------------------------------------------------------
//c   receive from north
//c---------------------------------------------------------------------
      if (north != -1) {
        RCCE_recv((char*)bufin, 10*ny*nz*sizeof(double), north);

        for (int ib=0,k=1; k<=nz; k++) {
          for (j=1; j<=ny; j++) {
            g(1,-1,j,k) = bufin[ib++];
            g(2,-1,j,k) = bufin[ib++];
            g(3,-1,j,k) = bufin[ib++];
            g(4,-1,j,k) = bufin[ib++];
            g(5,-1,j,k) = bufin[ib++];
            g(1,0,j,k) = bufin[ib++];
            g(2,0,j,k) = bufin[ib++];
            g(3,0,j,k) = bufin[ib++];
            g(4,0,j,k) = bufin[ib++];
            g(5,0,j,k) = bufin[ib++];
          }
        }
       }

//c---------------------------------------------------------------------
//c   send north
//c---------------------------------------------------------------------
      if (north != -1) {
        for (int ib=0,k=1; k<=nz; k++) {
          for (j=1; j<=ny; j++) {
            bufout[ib++] = g(1,2,j,k);
            bufout[ib++] = g(2,2,j,k);
            bufout[ib++] = g(3,2,j,k);
            bufout[ib++] = g(4,2,j,k);
            bufout[ib++] = g(5,2,j,k);
            bufout[ib++] = g(1,1,j,k);
            bufout[ib++] = g(2,1,j,k);
            bufout[ib++] = g(3,1,j,k);
            bufout[ib++] = g(4,1,j,k);
            bufout[ib++] = g(5,1,j,k);
          }
        }
        RCCE_send((char*)bufout, 10*ny*nz*sizeof(double), north);
      }

//c---------------------------------------------------------------------
//c   receive from south
//c---------------------------------------------------------------------
      if (south != -1) {
        RCCE_recv((char*)bufin, 10*ny*nz*sizeof(double), south);

        for (int ib=0,k=1; k<=nz; k++) {
          for (j=1; j<=ny; j++) {
            g(1,nx+2,j,k) = bufin[ib++];
            g(2,nx+2,j,k) = bufin[ib++];
            g(3,nx+2,j,k) = bufin[ib++];
            g(4,nx+2,j,k) = bufin[ib++];
            g(5,nx+2,j,k) = bufin[ib++];
            g(1,nx+1,j,k) = bufin[ib++];
            g(2,nx+1,j,k) = bufin[ib++];
            g(3,nx+1,j,k) = bufin[ib++];
            g(4,nx+1,j,k) = bufin[ib++];
            g(5,nx+1,j,k) = bufin[ib++];
          }
        }
      }

    }

    else {

//c---------------------------------------------------------------------
//c   communicate in the east and west directions
//c---------------------------------------------------------------------
//c---------------------------------------------------------------------
//c   send east
//c---------------------------------------------------------------------
      if (east != -1) {
        for (int ib=0,k=1; k<=nz; k++) {
          for (i=1; i<=nx; i++) {
            bufout[ib++] = g(1,i,ny-1,k);
            bufout[ib++] = g(2,i,ny-1,k);
            bufout[ib++] = g(3,i,ny-1,k);
            bufout[ib++] = g(4,i,ny-1,k);
            bufout[ib++] = g(5,i,ny-1,k);
            bufout[ib++] = g(1,i,ny,k);
            bufout[ib++] = g(2,i,ny,k);
            bufout[ib++] = g(3,i,ny,k);
            bufout[ib++] = g(4,i,ny,k);
            bufout[ib++] = g(5,i,ny,k);
          }
        }
        RCCE_send((char*)bufout, 10*nx*nz*sizeof(double), east);
    }
      
//c---------------------------------------------------------------------
//c   receive from west
//c---------------------------------------------------------------------
      if (west != -1) {
        RCCE_recv((char*)bufin, 10*nx*nz*sizeof(double), west);

        for (int ib=0,k=1; k<=nz; k++) {
          for (i=1; i<=nx; i++) {
            g(1,i,-1,k) = bufin[ib++];
            g(2,i,-1,k) = bufin[ib++];
            g(3,i,-1,k) = bufin[ib++];
            g(4,i,-1,k) = bufin[ib++];
            g(5,i,-1,k) = bufin[ib++];
            g(1,i,0,k) = bufin[ib++];
            g(2,i,0,k) = bufin[ib++];
            g(3,i,0,k) = bufin[ib++];
            g(4,i,0,k) = bufin[ib++];
            g(5,i,0,k) = bufin[ib++];
          }
        }

      }

//c---------------------------------------------------------------------
//c   send west
//c---------------------------------------------------------------------
      if (west != -1) {
          for (int ib=0,k=1; k<=nz; k++) {
            for (i=1; i<=nx; i++) {
              bufout[ib++] = g(1,i,2,k);
              bufout[ib++] = g(2,i,2,k);
              bufout[ib++] = g(3,i,2,k);
              bufout[ib++] = g(4,i,2,k);
              bufout[ib++] = g(5,i,2,k);
              bufout[ib++] = g(1,i,1,k);
              bufout[ib++] = g(2,i,1,k);
              bufout[ib++] = g(3,i,1,k);
              bufout[ib++] = g(4,i,1,k);
              bufout[ib++] = g(5,i,1,k);
            }
          }
        RCCE_send((char*)bufout, 10*nx*nz*sizeof(double), west);
      }

//c---------------------------------------------------------------------
//c   receive from east
//c---------------------------------------------------------------------
      if (east != -1) {
        RCCE_recv((char*)bufin, 10*nx*nz*sizeof(double), east);

        for (int ib=0,k=1; k<=nz; k++) {
          for (i=1; i<=nx; i++) {
            g(1,i,ny+2,k) = bufin[ib++];
            g(2,i,ny+2,k) = bufin[ib++];
            g(3,i,ny+2,k) = bufin[ib++];
            g(4,i,ny+2,k) = bufin[ib++];
            g(5,i,ny+2,k) = bufin[ib++];
            g(1,i,ny+1,k) = bufin[ib++];
            g(2,i,ny+1,k) = bufin[ib++];
            g(3,i,ny+1,k) = bufin[ib++];
            g(4,i,ny+1,k) = bufin[ib++];
            g(5,i,ny+1,k) = bufin[ib++];
          }
        }

      }

    }

    return;
}
