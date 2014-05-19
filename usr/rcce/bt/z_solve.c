//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include "header.h"
#include "mpinpb.h"
#include "work_lhs.h"

extern void z_sendrecv_solve(int c, int cprev);
extern void z_sendrecv_back(int c, int cprev);
extern void z_backsubstitute(int first, int last, int c);
extern void z_solve_cell(int first, int last, int c);

void z_solve() {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     Performs line solves in Z direction by first factoring
//     the block-tridiagonal matrix into an upper triangular matrix, 
//     and then performing back substitution to solve for the unknow
//     vectors of each line.  
//     
//     Make sure we treat elements zero to cell_size in the direction
//     of the sweep.
//---------------------------------------------------------------------

      int  c, cprev, stage, first, last, error;

//---------------------------------------------------------------------
//     in our terminology stage is the number of the cell in the y-direction
//     i.e. stage = 1 means the start of the line stage=ncells means end
//---------------------------------------------------------------------
      for (stage = 1; stage <= ncells; stage++) {
         c = slice(3,stage);
//---------------------------------------------------------------------
//     set last-cell flag
//---------------------------------------------------------------------
         first = (stage == 1);
         last =  (stage == ncells);

        if (stage >1) {
           cprev = slice(3,stage-1);
           z_sendrecv_solve(c, cprev);
        }
        z_solve_cell(first,last,c);
      }

//---------------------------------------------------------------------
//     now perform backsubstitution in reverse direction
//---------------------------------------------------------------------
      for (stage = ncells; stage >= 1; stage--) {
         c = slice(3,stage);
         first = (stage == 1);
         last =  (stage == ncells);

         if (stage <ncells) {
            cprev = slice(3,stage+1);
            z_sendrecv_back(c, cprev);
         }

         z_backsubstitute(first,last,c);
      }

      return;
}
      
//---------------------------------------------------------------------
//---------------------------------------------------------------------
      
void z_unpack_solve_info(int c) {
//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     unpack C'(-1) and rhs'(-1) for
//     all i and j
//---------------------------------------------------------------------

      int i,j,m,n,ptr,kstart ;

      kstart = 0;
      ptr = 0;
      for (j = 0; j <= JMAX-1; j++) {
         for (i = 0; i <= IMAX-1; i++) {
            for (m = 1; m <= BLOCK_SIZE; m++) {
               for (n = 1; n <= BLOCK_SIZE; n++) {
                  lhsc(m,n,i,j,kstart-1,c) = out_buffer(ptr+n);
               }
               ptr = ptr+BLOCK_SIZE;
            }
            for (n = 1; n <= BLOCK_SIZE; n++) {
               rhs(n,i,j,kstart-1,c) = out_buffer(ptr+n);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

      return;
}

      
void z_sendrecv_solve(int c, int cprev) {

//---------------------------------------------------------------------
//     pack up and send C'(kend) and rhs'(kend) for
//     all i and j
//---------------------------------------------------------------------

      int i,j,m,n,ksize,ptr,kstart;
      int phase;
      int error,buffer_size;

      ksize = cell_size(3,cprev)-1;
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*
           (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE);

//---------------------------------------------------------------------
//     pack up buffer
//---------------------------------------------------------------------
      ptr = 0;
      for (j = 0; j <= JMAX-1; j++) {
         for (i = 0; i <= IMAX-1; i++) {
            for (m = 1; m <= BLOCK_SIZE; m++) {
               for (n = 1; n <= BLOCK_SIZE; n++) {
                  in_buffer(ptr+n) = lhsc(m,n,i,j,ksize,cprev);
               }
               ptr = ptr+BLOCK_SIZE;
            }
            for (n = 1; n <= BLOCK_SIZE; n++) {
               in_buffer(ptr+n) = rhs(n,i,j,ksize,cprev);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

//---------------------------------------------------------------------
//     send and receive buffer 
//---------------------------------------------------------------------

      for (phase = 0; phase < 3; phase++) {

        if (send_color[TOPDIR]==phase) 
          RCCE_send((char*)in_buffer, buffer_size*sizeof(double), successor(3));
        if (recv_color[TOPDIR]==phase) 
          RCCE_recv((char*)out_buffer, buffer_size*sizeof(double), predecessor(3));
      }

//---------------------------------------------------------------------
//     unpack buffer
//---------------------------------------------------------------------
      kstart = 0;
      ptr = 0;
      for (j = 0; j <= JMAX-1; j++) {
         for (i = 0; i <= IMAX-1; i++) {
            for (m = 1; m <= BLOCK_SIZE; m++) {
               for (n = 1; n <= BLOCK_SIZE; n++) {
                  lhsc(m,n,i,j,kstart-1,c) = out_buffer(ptr+n);
               }
               ptr = ptr+BLOCK_SIZE;
            }
            for (n = 1; n <= BLOCK_SIZE; n++) {
               rhs(n,i,j,kstart-1,c) = out_buffer(ptr+n);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

      return;
}

//---------------------------------------------------------------------
//---------------------------------------------------------------------

void z_sendrecv_back(int c, int cprev) {

//---------------------------------------------------------------------
//     pack up and send U(jstart) for all i and j
//---------------------------------------------------------------------

      int i,j,n,ptr,kstart;
      int phase;
      int error,buffer_size;

//---------------------------------------------------------------------
//     Send element 0 to previous processor
//---------------------------------------------------------------------
      kstart = 0;
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*BLOCK_SIZE;
      ptr = 0;
      for (j = 0; j <= JMAX-1; j++) {
         for (i = 0; i <= IMAX-1; i++) {
            for (n = 1; n <= BLOCK_SIZE; n++) {
               in_buffer(ptr+n) = rhs(n,i,j,kstart,cprev);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

//---------------------------------------------------------------------
//     send and receive buffer 
//---------------------------------------------------------------------

      for (phase = 0; phase < 3; phase++) {

        if (send_color[BOTTOMDIR]==phase) 
          RCCE_send((char*)in_buffer, buffer_size*sizeof(double), predecessor(3));
        if (recv_color[BOTTOMDIR]==phase) 
          RCCE_recv((char*)out_buffer, buffer_size*sizeof(double), successor(3));
      }

//---------------------------------------------------------------------
//     unpack U(ksize) for all i and j
//---------------------------------------------------------------------

      ptr = 0;
      for (j = 0; j <= JMAX-1; j++) {
         for (i = 0; i <= IMAX-1; i++) {
            for (n = 1; n <= BLOCK_SIZE; n++) {
               backsub_info(n,i,j,c) = out_buffer(ptr+n);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

      return;
}


//---------------------------------------------------------------------
//---------------------------------------------------------------------

void z_backsubstitute(int first, int last, int c) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     back solve: if last cell, then generate U(ksize)=rhs(ksize)
//     else assume U(ksize) is loaded in un pack backsub_info
//     so just use it
//     after call u(kstart) will be sent to next cell
//---------------------------------------------------------------------

      int i, k;
      int m,n,j,jsize,isize,ksize,kstart;
      
      kstart = 0;
      isize = cell_size(1,c)-end(1,c)-1      ;
      jsize = cell_size(2,c)-end(2,c)-1;
      ksize = cell_size(3,c)-1;
      if (last == 0) {
         for (j = start(2,c); j <= jsize; j++) {
            for (i = start(1,c); i <= isize; i++) {
//---------------------------------------------------------------------
//     U(jsize) uses info from previous cell if not last cell
//---------------------------------------------------------------------
               for (m = 1; m <= BLOCK_SIZE; m++) {
                  for (n = 1; n <= BLOCK_SIZE; n++) {
                     rhs(m,i,j,ksize,c) = rhs(m,i,j,ksize,c) 
                          - lhsc(m,n,i,j,ksize,c)*
                          backsub_info(n,i,j,c);
                  }
               }
            }
         }
      }
      for (k = ksize-1; k >= kstart; k--) {
         for (j = start(2,c); j <= jsize; j++) {
            for (i = start(1,c); i <= isize; i++) {
               for (m = 1; m <= BLOCK_SIZE; m++) {
                  for (n = 1; n <= BLOCK_SIZE; n++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) 
                          - lhsc(m,n,i,j,k,c)*rhs(n,i,j,k+1,c);
                  }
               }
            }
         }
      }

      return;
}

//---------------------------------------------------------------------
//---------------------------------------------------------------------

void z_solve_cell(int first,int last,int c) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     performs guaussian elimination on this cell.
//     
//     assumes that unpacking routines for non-first cells 
//     preload C' and rhs' from previous cell.
//     
//     assumed send happens outside this routine, but that
//     c'(KMAX) and rhs'(KMAX) will be sent to next cell.
//---------------------------------------------------------------------

      int i,j,k,isize,ksize,jsize,kstart;
      double utmp[6*(KMAX+4)];
#define utmp(m,i) utmp[(m-1)+6*(i+2)]

      kstart = 0;
      isize = cell_size(1,c)-end(1,c)-1;
      jsize = cell_size(2,c)-end(2,c)-1;
      ksize = cell_size(3,c)-1;

      lhsabinit(lhsa, lhsb, ksize);

      for (j = start(2,c); j <= jsize; j++) {
         for (i = start(1,c); i <= isize; i++) {

//---------------------------------------------------------------------
//     This function computes the left hand side for the three z-factors 
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     Compute the indices for storing the block-diagonal matrix;
//     determine c (labeled f) and s jacobians for cell c
//---------------------------------------------------------------------
            for (k = start(3,c)-1; k <= cell_size(3,c)-end(3,c); k++) {
               utmp(1,k) = 1.0e0 / u(1,i,j,k,c);
               utmp(2,k) = u(2,i,j,k,c);
               utmp(3,k) = u(3,i,j,k,c);
               utmp(4,k) = u(4,i,j,k,c);
               utmp(5,k) = u(5,i,j,k,c);
               utmp(6,k) = qs(i,j,k,c);
            }

            for (k = start(3,c)-1; k <= cell_size(3,c)-end(3,c); k++) {

               tmp1 = utmp(1,k);
               tmp2 = tmp1 * tmp1;
               tmp3 = tmp1 * tmp2;

               fjac(1,1,k) = 0.0e+00;
               fjac(1,2,k) = 0.0e+00;
               fjac(1,3,k) = 0.0e+00;
               fjac(1,4,k) = 1.0e+00;
               fjac(1,5,k) = 0.0e+00;

               fjac(2,1,k) = - ( utmp(2,k)*utmp(4,k) ) 
                    * tmp2 ;
               fjac(2,2,k) = utmp(4,k) * tmp1;
               fjac(2,3,k) = 0.0e+00;
               fjac(2,4,k) = utmp(2,k) * tmp1;
               fjac(2,5,k) = 0.0e+00;

               fjac(3,1,k) = - ( utmp(3,k)*utmp(4,k) )
                    * tmp2 ;
               fjac(3,2,k) = 0.0e+00;
               fjac(3,3,k) = utmp(4,k) * tmp1;
               fjac(3,4,k) = utmp(3,k) * tmp1;
               fjac(3,5,k) = 0.0e+00;

               fjac(4,1,k) = - (utmp(4,k)*utmp(4,k) * tmp2 ) 
                    + c2 * utmp(6,k);
               fjac(4,2,k) = - c2 *  utmp(2,k) * tmp1 ;
               fjac(4,3,k) = - c2 *  utmp(3,k) * tmp1;
               fjac(4,4,k) = ( 2.0e+00 - c2 )
                    *  utmp(4,k) * tmp1 ;
               fjac(4,5,k) = c2;

               fjac(5,1,k) = ( c2 * 2.0e0 * utmp(6,k)
                    - c1 * ( utmp(5,k) * tmp1 ) )
                    * ( utmp(4,k) * tmp1 );
               fjac(5,2,k) = - c2 * ( utmp(2,k)*utmp(4,k) )
                    * tmp2 ;
               fjac(5,3,k) = - c2 * ( utmp(3,k)*utmp(4,k) )
                    * tmp2;
               fjac(5,4,k) = c1 * ( utmp(5,k) * tmp1 )
                    - c2 * ( utmp(6,k)
                    + utmp(4,k)*utmp(4,k) * tmp2 );
               fjac(5,5,k) = c1 * utmp(4,k) * tmp1;

               njac(1,1,k) = 0.0e+00;
               njac(1,2,k) = 0.0e+00;
               njac(1,3,k) = 0.0e+00;
               njac(1,4,k) = 0.0e+00;
               njac(1,5,k) = 0.0e+00;

               njac(2,1,k) = - c3c4 * tmp2 * utmp(2,k);
               njac(2,2,k) =   c3c4 * tmp1;
               njac(2,3,k) =   0.0e+00;
               njac(2,4,k) =   0.0e+00;
               njac(2,5,k) =   0.0e+00;

               njac(3,1,k) = - c3c4 * tmp2 * utmp(3,k);
               njac(3,2,k) =   0.0e+00;
               njac(3,3,k) =   c3c4 * tmp1;
               njac(3,4,k) =   0.0e+00;
               njac(3,5,k) =   0.0e+00;

               njac(4,1,k) = - con43 * c3c4 * tmp2 * utmp(4,k);
               njac(4,2,k) =   0.0e+00;
               njac(4,3,k) =   0.0e+00;
               njac(4,4,k) =   con43 * c3 * c4 * tmp1;
               njac(4,5,k) =   0.0e+00;

               njac(5,1,k) = - (  c3c4
                    - c1345 ) * tmp3 * SQR(utmp(2,k))
                    - ( c3c4 - c1345 ) * tmp3 * SQR(utmp(3,k))
                    - ( con43 * c3c4
                    - c1345 ) * tmp3 * SQR(utmp(4,k))
                    - c1345 * tmp2 * utmp(5,k);

               njac(5,2,k) = (  c3c4 - c1345 ) * tmp2 * utmp(2,k);
               njac(5,3,k) = (  c3c4 - c1345 ) * tmp2 * utmp(3,k);
               njac(5,4,k) = ( con43 * c3c4
                    - c1345 ) * tmp2 * utmp(4,k);
               njac(5,5,k) = ( c1345 )* tmp1;


            }

//---------------------------------------------------------------------
//     now joacobians set, so form left hand side in z direction
//---------------------------------------------------------------------
            for (k = start(3,c); k <= ksize-end(3,c); k++) {

               tmp1 = dt * tz1;
               tmp2 = dt * tz2;

               lhsa(1,1,k) = - tmp2 * fjac(1,1,k-1)
                    - tmp1 * njac(1,1,k-1)
                    - tmp1 * dz1 ;
               lhsa(1,2,k) = - tmp2 * fjac(1,2,k-1)
                    - tmp1 * njac(1,2,k-1);
               lhsa(1,3,k) = - tmp2 * fjac(1,3,k-1)
                    - tmp1 * njac(1,3,k-1);
               lhsa(1,4,k) = - tmp2 * fjac(1,4,k-1)
                    - tmp1 * njac(1,4,k-1);
               lhsa(1,5,k) = - tmp2 * fjac(1,5,k-1)
                    - tmp1 * njac(1,5,k-1);

               lhsa(2,1,k) = - tmp2 * fjac(2,1,k-1)
                    - tmp1 * njac(2,1,k-1);
               lhsa(2,2,k) = - tmp2 * fjac(2,2,k-1)
                    - tmp1 * njac(2,2,k-1)
                    - tmp1 * dz2;
               lhsa(2,3,k) = - tmp2 * fjac(2,3,k-1)
                    - tmp1 * njac(2,3,k-1);
               lhsa(2,4,k) = - tmp2 * fjac(2,4,k-1)
                    - tmp1 * njac(2,4,k-1);
               lhsa(2,5,k) = - tmp2 * fjac(2,5,k-1)
                    - tmp1 * njac(2,5,k-1);

               lhsa(3,1,k) = - tmp2 * fjac(3,1,k-1)
                    - tmp1 * njac(3,1,k-1);
               lhsa(3,2,k) = - tmp2 * fjac(3,2,k-1)
                    - tmp1 * njac(3,2,k-1);
               lhsa(3,3,k) = - tmp2 * fjac(3,3,k-1)
                    - tmp1 * njac(3,3,k-1)
                    - tmp1 * dz3 ;
               lhsa(3,4,k) = - tmp2 * fjac(3,4,k-1)
                    - tmp1 * njac(3,4,k-1);
               lhsa(3,5,k) = - tmp2 * fjac(3,5,k-1)
                    - tmp1 * njac(3,5,k-1);

               lhsa(4,1,k) = - tmp2 * fjac(4,1,k-1)
                    - tmp1 * njac(4,1,k-1);
               lhsa(4,2,k) = - tmp2 * fjac(4,2,k-1)
                    - tmp1 * njac(4,2,k-1);
               lhsa(4,3,k) = - tmp2 * fjac(4,3,k-1)
                    - tmp1 * njac(4,3,k-1);
               lhsa(4,4,k) = - tmp2 * fjac(4,4,k-1)
                    - tmp1 * njac(4,4,k-1)
                    - tmp1 * dz4;
               lhsa(4,5,k) = - tmp2 * fjac(4,5,k-1)
                    - tmp1 * njac(4,5,k-1);

               lhsa(5,1,k) = - tmp2 * fjac(5,1,k-1)
                    - tmp1 * njac(5,1,k-1);
               lhsa(5,2,k) = - tmp2 * fjac(5,2,k-1)
                    - tmp1 * njac(5,2,k-1);
               lhsa(5,3,k) = - tmp2 * fjac(5,3,k-1)
                    - tmp1 * njac(5,3,k-1);
               lhsa(5,4,k) = - tmp2 * fjac(5,4,k-1)
                    - tmp1 * njac(5,4,k-1);
               lhsa(5,5,k) = - tmp2 * fjac(5,5,k-1)
                    - tmp1 * njac(5,5,k-1)
                    - tmp1 * dz5;

               lhsb(1,1,k) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(1,1,k)
                    + tmp1 * 2.0e+00 * dz1;
               lhsb(1,2,k) = tmp1 * 2.0e+00 * njac(1,2,k);
               lhsb(1,3,k) = tmp1 * 2.0e+00 * njac(1,3,k);
               lhsb(1,4,k) = tmp1 * 2.0e+00 * njac(1,4,k);
               lhsb(1,5,k) = tmp1 * 2.0e+00 * njac(1,5,k);

               lhsb(2,1,k) = tmp1 * 2.0e+00 * njac(2,1,k);
               lhsb(2,2,k) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(2,2,k)
                    + tmp1 * 2.0e+00 * dz2;
               lhsb(2,3,k) = tmp1 * 2.0e+00 * njac(2,3,k);
               lhsb(2,4,k) = tmp1 * 2.0e+00 * njac(2,4,k);
               lhsb(2,5,k) = tmp1 * 2.0e+00 * njac(2,5,k);

               lhsb(3,1,k) = tmp1 * 2.0e+00 * njac(3,1,k);
               lhsb(3,2,k) = tmp1 * 2.0e+00 * njac(3,2,k);
               lhsb(3,3,k) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(3,3,k)
                    + tmp1 * 2.0e+00 * dz3;
               lhsb(3,4,k) = tmp1 * 2.0e+00 * njac(3,4,k);
               lhsb(3,5,k) = tmp1 * 2.0e+00 * njac(3,5,k);

               lhsb(4,1,k) = tmp1 * 2.0e+00 * njac(4,1,k);
               lhsb(4,2,k) = tmp1 * 2.0e+00 * njac(4,2,k);
               lhsb(4,3,k) = tmp1 * 2.0e+00 * njac(4,3,k);
               lhsb(4,4,k) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(4,4,k)
                    + tmp1 * 2.0e+00 * dz4;
               lhsb(4,5,k) = tmp1 * 2.0e+00 * njac(4,5,k);

               lhsb(5,1,k) = tmp1 * 2.0e+00 * njac(5,1,k);
               lhsb(5,2,k) = tmp1 * 2.0e+00 * njac(5,2,k);
               lhsb(5,3,k) = tmp1 * 2.0e+00 * njac(5,3,k);
               lhsb(5,4,k) = tmp1 * 2.0e+00 * njac(5,4,k);
               lhsb(5,5,k) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(5,5,k) 
                    + tmp1 * 2.0e+00 * dz5;

               lhsc(1,1,i,j,k,c) =  tmp2 * fjac(1,1,k+1)
                    - tmp1 * njac(1,1,k+1)
                    - tmp1 * dz1;
               lhsc(1,2,i,j,k,c) =  tmp2 * fjac(1,2,k+1)
                    - tmp1 * njac(1,2,k+1);
               lhsc(1,3,i,j,k,c) =  tmp2 * fjac(1,3,k+1)
                    - tmp1 * njac(1,3,k+1);
               lhsc(1,4,i,j,k,c) =  tmp2 * fjac(1,4,k+1)
                    - tmp1 * njac(1,4,k+1);
               lhsc(1,5,i,j,k,c) =  tmp2 * fjac(1,5,k+1)
                    - tmp1 * njac(1,5,k+1);

               lhsc(2,1,i,j,k,c) =  tmp2 * fjac(2,1,k+1)
                    - tmp1 * njac(2,1,k+1);
               lhsc(2,2,i,j,k,c) =  tmp2 * fjac(2,2,k+1)
                    - tmp1 * njac(2,2,k+1)
                    - tmp1 * dz2;
               lhsc(2,3,i,j,k,c) =  tmp2 * fjac(2,3,k+1)
                    - tmp1 * njac(2,3,k+1);
               lhsc(2,4,i,j,k,c) =  tmp2 * fjac(2,4,k+1)
                    - tmp1 * njac(2,4,k+1);
               lhsc(2,5,i,j,k,c) =  tmp2 * fjac(2,5,k+1)
                    - tmp1 * njac(2,5,k+1);

               lhsc(3,1,i,j,k,c) =  tmp2 * fjac(3,1,k+1)
                    - tmp1 * njac(3,1,k+1);
               lhsc(3,2,i,j,k,c) =  tmp2 * fjac(3,2,k+1)
                    - tmp1 * njac(3,2,k+1);
               lhsc(3,3,i,j,k,c) =  tmp2 * fjac(3,3,k+1)
                    - tmp1 * njac(3,3,k+1)
                    - tmp1 * dz3;
               lhsc(3,4,i,j,k,c) =  tmp2 * fjac(3,4,k+1)
                    - tmp1 * njac(3,4,k+1);
               lhsc(3,5,i,j,k,c) =  tmp2 * fjac(3,5,k+1)
                    - tmp1 * njac(3,5,k+1);

               lhsc(4,1,i,j,k,c) =  tmp2 * fjac(4,1,k+1)
                    - tmp1 * njac(4,1,k+1);
               lhsc(4,2,i,j,k,c) =  tmp2 * fjac(4,2,k+1)
                    - tmp1 * njac(4,2,k+1);
               lhsc(4,3,i,j,k,c) =  tmp2 * fjac(4,3,k+1)
                    - tmp1 * njac(4,3,k+1);
               lhsc(4,4,i,j,k,c) =  tmp2 * fjac(4,4,k+1)
                    - tmp1 * njac(4,4,k+1)
                    - tmp1 * dz4;
               lhsc(4,5,i,j,k,c) =  tmp2 * fjac(4,5,k+1)
                    - tmp1 * njac(4,5,k+1);

               lhsc(5,1,i,j,k,c) =  tmp2 * fjac(5,1,k+1)
                    - tmp1 * njac(5,1,k+1);
               lhsc(5,2,i,j,k,c) =  tmp2 * fjac(5,2,k+1)
                    - tmp1 * njac(5,2,k+1);
               lhsc(5,3,i,j,k,c) =  tmp2 * fjac(5,3,k+1)
                    - tmp1 * njac(5,3,k+1);
               lhsc(5,4,i,j,k,c) =  tmp2 * fjac(5,4,k+1)
                    - tmp1 * njac(5,4,k+1);
               lhsc(5,5,i,j,k,c) =  tmp2 * fjac(5,5,k+1)
                    - tmp1 * njac(5,5,k+1)
                    - tmp1 * dz5;

            }


//---------------------------------------------------------------------
//     outer most do loops - sweeping in i direction
//---------------------------------------------------------------------
            if (first == 1) {

//---------------------------------------------------------------------
//     multiply c(i,j,kstart) by b_inverse and copy back to c
//     multiply rhs(kstart) by b_inverse(kstart) and copy to rhs
//---------------------------------------------------------------------
               binvcrhs( &lhsb(1,1,kstart),
                              &lhsc(1,1,i,j,kstart,c),
                              &rhs(1,i,j,kstart,c) );

            }

//---------------------------------------------------------------------
//     begin inner most do loop
//     do all the elements of the cell unless last 
//---------------------------------------------------------------------
            for (k = kstart+first; k <= ksize-last; k++) {

//---------------------------------------------------------------------
//     subtract A*lhs_vector(k-1) from lhs_vector(k)
//     
//     rhs(k) = rhs(k) - A*rhs(k-1)
//---------------------------------------------------------------------
               matvec_sub(&lhsa(1,1,k),
                               &rhs(1,i,j,k-1,c),&rhs(1,i,j,k,c));

//---------------------------------------------------------------------
//     B(k) = B(k) - C(k-1)*A(k)
//     call matmul_sub(aa,i,j,k,c,cc,i,j,k-1,c,bb,i,j,k,c)
//---------------------------------------------------------------------
               matmul_sub(&lhsa(1,1,k),
                               &lhsc(1,1,i,j,k-1,c),
                               &lhsb(1,1,k));

//---------------------------------------------------------------------
//     multiply c(i,j,k) by b_inverse and copy back to c
//     multiply rhs(i,j,1) by b_inverse(i,j,1) and copy to rhs
//---------------------------------------------------------------------
               binvcrhs( &lhsb(1,1,k),
                              &lhsc(1,1,i,j,k,c),
                              &rhs(1,i,j,k,c) );

            }

//---------------------------------------------------------------------
//     Now finish up special cases for last cell
//---------------------------------------------------------------------
            if (last == 1) {

//---------------------------------------------------------------------
//     rhs(ksize) = rhs(ksize) - A*rhs(ksize-1)
//---------------------------------------------------------------------
               matvec_sub(&lhsa(1,1,ksize),
                               &rhs(1,i,j,ksize-1,c),&rhs(1,i,j,ksize,c));

//---------------------------------------------------------------------
//     B(ksize) = B(ksize) - C(ksize-1)*A(ksize)
//     call matmul_sub(aa,i,j,ksize,c,
//     $              cc,i,j,ksize-1,c,bb,i,j,ksize,c)
//---------------------------------------------------------------------
               matmul_sub(&lhsa(1,1,ksize),
                               &lhsc(1,1,i,j,ksize-1,c),
                               &lhsb(1,1,ksize));

//---------------------------------------------------------------------
//     multiply rhs(ksize) by b_inverse(ksize) and copy to rhs
//---------------------------------------------------------------------
               binvrhs( &lhsb(1,1,ksize),
                             &rhs(1,i,j,ksize,c) );

            }
         }
      }


      return;
}
      





