#include "RCCE.h"
#include <stdlib.h>
#include "directions.h"
#include "applu_share.h"
#include "applu_macros.h"

#define g(m,i,j,k) g[m-1+5*((i+1)+(isiz1+4)*((j+1)+(isiz2+4)*(k-1)))]

extern int rcce_curphase;

void exchange_1(double *g, int k, int iex) {

  int i, j;
  size_t chunk;
  int error, len;
  char msg[200];
  double bufin[5*(isiz1+isiz2)], bufout[5*(isiz1+isiz2)];

  /* rcce_curphase = iex == 0 ? 0 : 1; */
  if(iex == 1 || iex == 2) {
      rcce_curphase = 2;
  } else {
      rcce_curphase = 3;
  }

  if( iex == 0 ) {

     if( north != -1 ) {
        RCCE_recv((char*)bufin, 5*(jend-jst+1)*sizeof(double), north);
       
       for (int ib=0,j=jst; j<=jend; j++) {
          g(1,0,j,k) = bufin[ib++];
          g(2,0,j,k) = bufin[ib++];
          g(3,0,j,k) = bufin[ib++];
          g(4,0,j,k) = bufin[ib++];
          g(5,0,j,k) = bufin[ib++];
       }
     }

     if( west != -1 ) {
        RCCE_recv((char*)bufin, 5*(iend-ist+1)*sizeof(double), west);

       for (int ib=0,i=ist; i<=iend; i++) {
          g(1,i,0,k) = bufin[ib++];
          g(2,i,0,k) = bufin[ib++];
          g(3,i,0,k) = bufin[ib++];
          g(4,i,0,k) = bufin[ib++];
          g(5,i,0,k) = bufin[ib++];
       }
     }
  }

  else if( iex == 1 ) {

     if( south != -1 ) {
        RCCE_recv((char*)bufin, 5*(jend-jst+1)*sizeof(double), south);

       for (int ib=0,j=jst; j<=jend; j++) {
          g(1,nx+1,j,k) = bufin[ib++];
          g(2,nx+1,j,k) = bufin[ib++];
          g(3,nx+1,j,k) = bufin[ib++];
          g(4,nx+1,j,k) = bufin[ib++];
          g(5,nx+1,j,k) = bufin[ib++];
       }
     }

     if( east != -1 ) {
        RCCE_recv((char*)bufin, 5*(iend-ist+1)*sizeof(double), east);

       for (int ib=0,i=ist; i<=iend; i++) {
          g(1,i,ny+1,k) = bufin[ib++];
          g(2,i,ny+1,k) = bufin[ib++];
          g(3,i,ny+1,k) = bufin[ib++];
          g(4,i,ny+1,k) = bufin[ib++];
          g(5,i,ny+1,k) = bufin[ib++];
       }
     }
  }
  else if( iex == 2 ) {

     if( south != -1 ) {
       for (int ib=0,j=jst; j<=jend; j++) {
          bufout[ib++] = g(1,nx,j,k);
          bufout[ib++] = g(2,nx,j,k);
          bufout[ib++] = g(3,nx,j,k);
          bufout[ib++] = g(4,nx,j,k);
          bufout[ib++] = g(5,nx,j,k);
       }
        RCCE_send((char*)bufout, 5*(jend-jst+1)*sizeof(double), south);
     }

     if( east != -1 ) {
        for (int ib=0,i=ist; i<=iend; i++) {
           bufout[ib++] = g(1,i,ny,k);
           bufout[ib++] = g(2,i,ny,k);
           bufout[ib++] = g(3,i,ny,k);
           bufout[ib++] = g(4,i,ny,k);
           bufout[ib++] = g(5,i,ny,k);
        } 

        RCCE_send((char*)bufout, 5*(iend-ist+1)*sizeof(double), east);
     }
  }
  else {

     if( north != -1 ) {
       for (int ib=0,j=jst; j<=jend; j++) {
          bufout[ib++] = g(1,1,j,k);
          bufout[ib++] = g(2,1,j,k);
          bufout[ib++] = g(3,1,j,k);
          bufout[ib++] = g(4,1,j,k);
          bufout[ib++] = g(5,1,j,k);
       }

        RCCE_send((char*)bufout, 5*(jend-jst+1)*sizeof(double), north);
     }

     if( west != -1 ) {
       for (int ib=0,i=ist; i<=iend; i++) {
          bufout[ib++] = g(1,i,1,k);
          bufout[ib++] = g(2,i,1,k);
          bufout[ib++] = g(3,i,1,k);
          bufout[ib++] = g(4,i,1,k);
          bufout[ib++] = g(5,i,1,k);
       }

        RCCE_send((char*)bufout, 5*(iend-ist+1)*sizeof(double), west);
     }

  }

  return;
}
