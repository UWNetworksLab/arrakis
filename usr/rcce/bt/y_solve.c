//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include "header.h"
#include "mpinpb.h"
#include "work_lhs.h"

extern void y_sendrecv_solve(int c, int cprev);
extern void y_sendrecv_back(int c, int cprev);
extern void y_backsubstitute(int first, int last, int c);
extern void y_solve_cell(int first, int last, int c);

void y_solve() {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     Performs line solves in Y direction by first factoring
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
         c = slice(2,stage);
//---------------------------------------------------------------------
//     set last-cell flag
//---------------------------------------------------------------------
         first = (stage == 1);
         last =  (stage == ncells);

        if (stage >1) {
           cprev = slice(2,stage-1);
           y_sendrecv_solve(c, cprev);
        }
        y_solve_cell(first,last,c);
      }

//---------------------------------------------------------------------
//     now perform backsubstitution in reverse direction
//---------------------------------------------------------------------
      for (stage = ncells; stage >= 1; stage--) {
         c = slice(2,stage);
         first = (stage == 1);
         last =  (stage == ncells);

         if (stage <ncells) {
            cprev = slice(2,stage+1);
            y_sendrecv_back(c, cprev);
         }

         y_backsubstitute(first,last,c);
      }

      return;
}
      
      
void y_sendrecv_solve(int c, int cprev) {

//---------------------------------------------------------------------
//     pack up and send C'(jend) and rhs'(jend) for
//     all i and k
//---------------------------------------------------------------------

      int i,k,m,n,jsize,ptr,jstart;
      int phase;
      int error,buffer_size;

      jsize = cell_size(2,cprev)-1;
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*
           (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE);

//---------------------------------------------------------------------
//     pack up buffer
//---------------------------------------------------------------------
      ptr = 0;
      for (k = 0; k <= KMAX-1; k++) {
         for (i = 0; i <= IMAX-1; i++) {
            for (m = 1; m <= BLOCK_SIZE; m++) {
               for (n = 1; n <= BLOCK_SIZE; n++) {
                  in_buffer(ptr+n) = lhsc(m,n,i,jsize,k,cprev);
               }
               ptr = ptr+BLOCK_SIZE;
            }
            for (n = 1; n <= BLOCK_SIZE; n++) {
               in_buffer(ptr+n) = rhs(n,i,jsize,k,cprev);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

//---------------------------------------------------------------------
//     send and receive buffer 
//---------------------------------------------------------------------

      for (phase = 0; phase < 3; phase++) {

        if (send_color[NORTHDIR]==phase) 
          RCCE_send((char*)in_buffer, buffer_size*sizeof(double), successor(2));
        if (recv_color[NORTHDIR]==phase) 
          RCCE_recv((char*)out_buffer, buffer_size*sizeof(double), predecessor(2));
      }

//---------------------------------------------------------------------
//     unpack buffer
//---------------------------------------------------------------------
      jstart = 0;
      ptr = 0;
      for (k = 0; k <= KMAX-1; k++) {
         for (i = 0; i <= IMAX-1; i++) {
            for (m = 1; m <= BLOCK_SIZE; m++) {
               for (n = 1; n <= BLOCK_SIZE; n++) {
                  lhsc(m,n,i,jstart-1,k,c) = out_buffer(ptr+n);
               }
               ptr = ptr+BLOCK_SIZE;
            }
            for (n = 1; n <= BLOCK_SIZE; n++) {
               rhs(n,i,jstart-1,k,c) = out_buffer(ptr+n);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

      return;
}

//---------------------------------------------------------------------
//---------------------------------------------------------------------

void y_sendrecv_back(int c, int cprev) {

//---------------------------------------------------------------------
//     pack up and send U(jstart) for all i and k
//---------------------------------------------------------------------

      int i,k,n,ptr,jstart;
      int phase;
      int error,buffer_size;

//---------------------------------------------------------------------
//     Send element 0 to previous processor
//---------------------------------------------------------------------
      jstart = 0;
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*BLOCK_SIZE;
      ptr = 0;
      for (k = 0; k <= KMAX-1; k++) {
         for (i = 0; i <= IMAX-1; i++) {
            for (n = 1; n <= BLOCK_SIZE; n++) {
               in_buffer(ptr+n) = rhs(n,i,jstart,k,cprev);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

//---------------------------------------------------------------------
//     send and receive buffer 
//---------------------------------------------------------------------

      for (phase = 0; phase < 3; phase++) {

        if (send_color[SOUTHDIR]==phase) 
          RCCE_send((char*)in_buffer, buffer_size*sizeof(double), predecessor(2));
        if (recv_color[SOUTHDIR]==phase) 
          RCCE_recv((char*)out_buffer, buffer_size*sizeof(double), successor(2));
      }

//---------------------------------------------------------------------
//     unpack U(jsize) for all i and k
//---------------------------------------------------------------------

      ptr = 0;
      for (k = 0; k <= KMAX-1; k++) {
         for (i = 0; i <= IMAX-1; i++) {
            for (n = 1; n <= BLOCK_SIZE; n++) {
               backsub_info(n,i,k,c) = out_buffer(ptr+n);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

      return;
}

void y_backsubstitute(int first, int last, int c) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     back solve: if last cell, then generate U(jsize)=rhs(jsize)
//     else assume U(jsize) is loaded in un pack backsub_info
//     so just use it
//     after call u(jstart) will be sent to next cell
//---------------------------------------------------------------------

      int i, k;
      int m,n,j,jsize,isize,ksize,jstart;
      
      jstart = 0;
      isize = cell_size(1,c)-end(1,c)-1      ;
      jsize = cell_size(2,c)-1;
      ksize = cell_size(3,c)-end(3,c)-1;
      if (last == 0) {
         for (k = start(3,c); k <= ksize; k++) {
            for (i = start(1,c); i <= isize; i++) {
//---------------------------------------------------------------------
//     U(jsize) uses info from previous cell if not last cell
//---------------------------------------------------------------------
               for (m = 1; m <= BLOCK_SIZE; m++) {
                  for (n = 1; n <= BLOCK_SIZE; n++) {
                     rhs(m,i,jsize,k,c) = rhs(m,i,jsize,k,c) 
                          - lhsc(m,n,i,jsize,k,c)*
                          backsub_info(n,i,k,c);
                  }
               }
            }
         }
      }
      for (k = start(3,c); k <= ksize; k++) {
         for (j = jsize-1; j >= jstart; j--) {
            for (i = start(1,c); i <= isize; i++) {
               for (m = 1; m <= BLOCK_SIZE; m++) {
                  for (n = 1; n <= BLOCK_SIZE; n++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) 
                          - lhsc(m,n,i,j,k,c)*rhs(n,i,j+1,k,c);
                  }
               }
            }
         }
      }

      return;
}

//---------------------------------------------------------------------
//---------------------------------------------------------------------

void y_solve_cell(int first,int last,int c) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     performs guaussian elimination on this cell.
//     
//     assumes that unpacking routines for non-first cells 
//     preload C' and rhs' from previous cell.
//     
//     assumed send happens outside this routine, but that
//     c'(JMAX) and rhs'(JMAX) will be sent to next cell
//---------------------------------------------------------------------

      int i,j,k,isize,ksize,jsize,jstart;
      double utmp[6*(JMAX+4)];
#define utmp(m,i) utmp[(m-1)+6*(i+2)]

      jstart = 0;
      isize = cell_size(1,c)-end(1,c)-1;
      jsize = cell_size(2,c)-1;
      ksize = cell_size(3,c)-end(3,c)-1;

      lhsabinit(lhsa, lhsb, jsize);

      for (k = start(3,c); k <= ksize; k++) {
         for (i = start(1,c); i <= isize; i++) {

//---------------------------------------------------------------------
//     This function computes the left hand side for the three y-factors 
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     Compute the indices for storing the tri-diagonal matrix;
//     determine a (labeled f) and n jacobians for cell c
//---------------------------------------------------------------------
            for (j = start(2,c)-1; j <= cell_size(2,c)-end(2,c); j++) {
               utmp(1,j) = 1.0e0 / u(1,i,j,k,c);
               utmp(2,j) = u(2,i,j,k,c);
               utmp(3,j) = u(3,i,j,k,c);
               utmp(4,j) = u(4,i,j,k,c);
               utmp(5,j) = u(5,i,j,k,c);
               utmp(6,j) = qs(i,j,k,c);
            }

            for (j = start(2,c)-1; j <= cell_size(2,c)-end(2,c); j++) {

               tmp1 = utmp(1,j);
               tmp2 = tmp1 * tmp1;
               tmp3 = tmp1 * tmp2;

               fjac(1,1,j) = 0.0e+00;
               fjac(1,2,j) = 0.0e+00;
               fjac(1,3,j) = 1.0e+00;
               fjac(1,4,j) = 0.0e+00;
               fjac(1,5,j) = 0.0e+00;

               fjac(2,1,j) = - ( utmp(2,j)*utmp(3,j) )
                    * tmp2;
               fjac(2,2,j) = utmp(3,j) * tmp1;
               fjac(2,3,j) = utmp(2,j) * tmp1;
               fjac(2,4,j) = 0.0e+00;
               fjac(2,5,j) = 0.0e+00;

               fjac(3,1,j) = - ( utmp(3,j)*utmp(3,j)*tmp2)
                    + c2 * utmp(6,j);
               fjac(3,2,j) = - c2 *  utmp(2,j) * tmp1;
               fjac(3,3,j) = ( 2.0e+00 - c2 )
                    *  utmp(3,j) * tmp1 ;
               fjac(3,4,j) = - c2 * utmp(4,j) * tmp1 ;
               fjac(3,5,j) = c2;

               fjac(4,1,j) = - ( utmp(3,j)*utmp(4,j) )
                    * tmp2;
               fjac(4,2,j) = 0.0e+00;
               fjac(4,3,j) = utmp(4,j) * tmp1;
               fjac(4,4,j) = utmp(3,j) * tmp1;
               fjac(4,5,j) = 0.0e+00;

               fjac(5,1,j) = ( c2 * 2.0e0 * utmp(6,j)
                    - c1 * utmp(5,j) * tmp1 ) 
                    * utmp(3,j) * tmp1 ;
               fjac(5,2,j) = - c2 * utmp(2,j)*utmp(3,j) 
                    * tmp2;
               fjac(5,3,j) = c1 * utmp(5,j) * tmp1 
                    - c2 * ( utmp(6,j)
                    + utmp(3,j)*utmp(3,j) * tmp2 );
               fjac(5,4,j) = - c2 * ( utmp(3,j)*utmp(4,j) )
                    * tmp2;
               fjac(5,5,j) = c1 * utmp(3,j) * tmp1 ;

               njac(1,1,j) = 0.0e+00;
               njac(1,2,j) = 0.0e+00;
               njac(1,3,j) = 0.0e+00;
               njac(1,4,j) = 0.0e+00;
               njac(1,5,j) = 0.0e+00;

               njac(2,1,j) = - c3c4 * tmp2 * utmp(2,j);
               njac(2,2,j) =   c3c4 * tmp1;
               njac(2,3,j) =   0.0e+00;
               njac(2,4,j) =   0.0e+00;
               njac(2,5,j) =   0.0e+00;

               njac(3,1,j) = - con43 * c3c4 * tmp2 * utmp(3,j);
               njac(3,2,j) =   0.0e+00;
               njac(3,3,j) =   con43 * c3c4 * tmp1;
               njac(3,4,j) =   0.0e+00;
               njac(3,5,j) =   0.0e+00;

               njac(4,1,j) = - c3c4 * tmp2 * utmp(4,j);
               njac(4,2,j) =   0.0e+00;
               njac(4,3,j) =   0.0e+00;
               njac(4,4,j) =   c3c4 * tmp1;
               njac(4,5,j) =   0.0e+00;

               njac(5,1,j) = - (  c3c4
                    - c1345 ) * tmp3 * SQR(utmp(2,j))
                    - ( con43 * c3c4
                    - c1345 ) * tmp3 * SQR(utmp(3,j))
                    - ( c3c4 - c1345 ) * tmp3 * SQR(utmp(4,j))
                    - c1345 * tmp2 * utmp(5,j);

               njac(5,2,j) = (  c3c4 - c1345 ) * tmp2 * utmp(2,j);
               njac(5,3,j) = ( con43 * c3c4
                    - c1345 ) * tmp2 * utmp(3,j);
               njac(5,4,j) = ( c3c4 - c1345 ) * tmp2 * utmp(4,j);
               njac(5,5,j) = ( c1345 ) * tmp1;

            }

//---------------------------------------------------------------------
//     now joacobians set, so form left hand side in y direction
//---------------------------------------------------------------------
            for (j = start(2,c); j <= jsize-end(2,c); j++) {

               tmp1 = dt * ty1;
               tmp2 = dt * ty2;

               lhsa(1,1,j) = - tmp2 * fjac(1,1,j-1)
                    - tmp1 * njac(1,1,j-1)
                    - tmp1 * dy1 ;
               lhsa(1,2,j) = - tmp2 * fjac(1,2,j-1)
                    - tmp1 * njac(1,2,j-1);
               lhsa(1,3,j) = - tmp2 * fjac(1,3,j-1)
                    - tmp1 * njac(1,3,j-1);
               lhsa(1,4,j) = - tmp2 * fjac(1,4,j-1)
                    - tmp1 * njac(1,4,j-1);
               lhsa(1,5,j) = - tmp2 * fjac(1,5,j-1)
                    - tmp1 * njac(1,5,j-1);

               lhsa(2,1,j) = - tmp2 * fjac(2,1,j-1)
                    - tmp1 * njac(2,1,j-1);
               lhsa(2,2,j) = - tmp2 * fjac(2,2,j-1)
                    - tmp1 * njac(2,2,j-1)
                    - tmp1 * dy2;
               lhsa(2,3,j) = - tmp2 * fjac(2,3,j-1)
                    - tmp1 * njac(2,3,j-1);
               lhsa(2,4,j) = - tmp2 * fjac(2,4,j-1)
                    - tmp1 * njac(2,4,j-1);
               lhsa(2,5,j) = - tmp2 * fjac(2,5,j-1)
                    - tmp1 * njac(2,5,j-1);

               lhsa(3,1,j) = - tmp2 * fjac(3,1,j-1)
                    - tmp1 * njac(3,1,j-1);
               lhsa(3,2,j) = - tmp2 * fjac(3,2,j-1)
                    - tmp1 * njac(3,2,j-1);
               lhsa(3,3,j) = - tmp2 * fjac(3,3,j-1)
                    - tmp1 * njac(3,3,j-1)
                    - tmp1 * dy3 ;
               lhsa(3,4,j) = - tmp2 * fjac(3,4,j-1)
                    - tmp1 * njac(3,4,j-1);
               lhsa(3,5,j) = - tmp2 * fjac(3,5,j-1)
                    - tmp1 * njac(3,5,j-1);

               lhsa(4,1,j) = - tmp2 * fjac(4,1,j-1)
                    - tmp1 * njac(4,1,j-1);
               lhsa(4,2,j) = - tmp2 * fjac(4,2,j-1)
                    - tmp1 * njac(4,2,j-1);
               lhsa(4,3,j) = - tmp2 * fjac(4,3,j-1)
                    - tmp1 * njac(4,3,j-1);
               lhsa(4,4,j) = - tmp2 * fjac(4,4,j-1)
                    - tmp1 * njac(4,4,j-1)
                    - tmp1 * dy4;
               lhsa(4,5,j) = - tmp2 * fjac(4,5,j-1)
                    - tmp1 * njac(4,5,j-1);

               lhsa(5,1,j) = - tmp2 * fjac(5,1,j-1)
                    - tmp1 * njac(5,1,j-1);
               lhsa(5,2,j) = - tmp2 * fjac(5,2,j-1)
                    - tmp1 * njac(5,2,j-1);
               lhsa(5,3,j) = - tmp2 * fjac(5,3,j-1)
                    - tmp1 * njac(5,3,j-1);
               lhsa(5,4,j) = - tmp2 * fjac(5,4,j-1)
                    - tmp1 * njac(5,4,j-1);
               lhsa(5,5,j) = - tmp2 * fjac(5,5,j-1)
                    - tmp1 * njac(5,5,j-1)
                    - tmp1 * dy5;

               lhsb(1,1,j) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(1,1,j)
                    + tmp1 * 2.0e+00 * dy1;
               lhsb(1,2,j) = tmp1 * 2.0e+00 * njac(1,2,j);
               lhsb(1,3,j) = tmp1 * 2.0e+00 * njac(1,3,j);
               lhsb(1,4,j) = tmp1 * 2.0e+00 * njac(1,4,j);
               lhsb(1,5,j) = tmp1 * 2.0e+00 * njac(1,5,j);

               lhsb(2,1,j) = tmp1 * 2.0e+00 * njac(2,1,j);
               lhsb(2,2,j) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(2,2,j)
                    + tmp1 * 2.0e+00 * dy2;
               lhsb(2,3,j) = tmp1 * 2.0e+00 * njac(2,3,j);
               lhsb(2,4,j) = tmp1 * 2.0e+00 * njac(2,4,j);
               lhsb(2,5,j) = tmp1 * 2.0e+00 * njac(2,5,j);

               lhsb(3,1,j) = tmp1 * 2.0e+00 * njac(3,1,j);
               lhsb(3,2,j) = tmp1 * 2.0e+00 * njac(3,2,j);
               lhsb(3,3,j) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(3,3,j)
                    + tmp1 * 2.0e+00 * dy3;
               lhsb(3,4,j) = tmp1 * 2.0e+00 * njac(3,4,j);
               lhsb(3,5,j) = tmp1 * 2.0e+00 * njac(3,5,j);

               lhsb(4,1,j) = tmp1 * 2.0e+00 * njac(4,1,j);
               lhsb(4,2,j) = tmp1 * 2.0e+00 * njac(4,2,j);
               lhsb(4,3,j) = tmp1 * 2.0e+00 * njac(4,3,j);
               lhsb(4,4,j) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(4,4,j)
                    + tmp1 * 2.0e+00 * dy4;
               lhsb(4,5,j) = tmp1 * 2.0e+00 * njac(4,5,j);

               lhsb(5,1,j) = tmp1 * 2.0e+00 * njac(5,1,j);
               lhsb(5,2,j) = tmp1 * 2.0e+00 * njac(5,2,j);
               lhsb(5,3,j) = tmp1 * 2.0e+00 * njac(5,3,j);
               lhsb(5,4,j) = tmp1 * 2.0e+00 * njac(5,4,j);
               lhsb(5,5,j) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(5,5,j) 
                    + tmp1 * 2.0e+00 * dy5;

               lhsc(1,1,i,j,k,c) =  tmp2 * fjac(1,1,j+1)
                    - tmp1 * njac(1,1,j+1)
                    - tmp1 * dy1;
               lhsc(1,2,i,j,k,c) =  tmp2 * fjac(1,2,j+1)
                    - tmp1 * njac(1,2,j+1);
               lhsc(1,3,i,j,k,c) =  tmp2 * fjac(1,3,j+1)
                    - tmp1 * njac(1,3,j+1);
               lhsc(1,4,i,j,k,c) =  tmp2 * fjac(1,4,j+1)
                    - tmp1 * njac(1,4,j+1);
               lhsc(1,5,i,j,k,c) =  tmp2 * fjac(1,5,j+1)
                    - tmp1 * njac(1,5,j+1);

               lhsc(2,1,i,j,k,c) =  tmp2 * fjac(2,1,j+1)
                    - tmp1 * njac(2,1,j+1);
               lhsc(2,2,i,j,k,c) =  tmp2 * fjac(2,2,j+1)
                    - tmp1 * njac(2,2,j+1)
                    - tmp1 * dy2;
               lhsc(2,3,i,j,k,c) =  tmp2 * fjac(2,3,j+1)
                    - tmp1 * njac(2,3,j+1);
               lhsc(2,4,i,j,k,c) =  tmp2 * fjac(2,4,j+1)
                    - tmp1 * njac(2,4,j+1);
               lhsc(2,5,i,j,k,c) =  tmp2 * fjac(2,5,j+1)
                    - tmp1 * njac(2,5,j+1);

               lhsc(3,1,i,j,k,c) =  tmp2 * fjac(3,1,j+1)
                    - tmp1 * njac(3,1,j+1);
               lhsc(3,2,i,j,k,c) =  tmp2 * fjac(3,2,j+1)
                    - tmp1 * njac(3,2,j+1);
               lhsc(3,3,i,j,k,c) =  tmp2 * fjac(3,3,j+1)
                    - tmp1 * njac(3,3,j+1)
                    - tmp1 * dy3;
               lhsc(3,4,i,j,k,c) =  tmp2 * fjac(3,4,j+1)
                    - tmp1 * njac(3,4,j+1);
               lhsc(3,5,i,j,k,c) =  tmp2 * fjac(3,5,j+1)
                    - tmp1 * njac(3,5,j+1);

               lhsc(4,1,i,j,k,c) =  tmp2 * fjac(4,1,j+1)
                    - tmp1 * njac(4,1,j+1);
               lhsc(4,2,i,j,k,c) =  tmp2 * fjac(4,2,j+1)
                    - tmp1 * njac(4,2,j+1);
               lhsc(4,3,i,j,k,c) =  tmp2 * fjac(4,3,j+1)
                    - tmp1 * njac(4,3,j+1);
               lhsc(4,4,i,j,k,c) =  tmp2 * fjac(4,4,j+1)
                    - tmp1 * njac(4,4,j+1)
                    - tmp1 * dy4;
               lhsc(4,5,i,j,k,c) =  tmp2 * fjac(4,5,j+1)
                    - tmp1 * njac(4,5,j+1);

               lhsc(5,1,i,j,k,c) =  tmp2 * fjac(5,1,j+1)
                    - tmp1 * njac(5,1,j+1);
               lhsc(5,2,i,j,k,c) =  tmp2 * fjac(5,2,j+1)
                    - tmp1 * njac(5,2,j+1);
               lhsc(5,3,i,j,k,c) =  tmp2 * fjac(5,3,j+1)
                    - tmp1 * njac(5,3,j+1);
               lhsc(5,4,i,j,k,c) =  tmp2 * fjac(5,4,j+1)
                    - tmp1 * njac(5,4,j+1);
               lhsc(5,5,i,j,k,c) =  tmp2 * fjac(5,5,j+1)
                    - tmp1 * njac(5,5,j+1)
                    - tmp1 * dy5;

            }


//---------------------------------------------------------------------
//     outer most do loops - sweeping in i direction
//---------------------------------------------------------------------
            if (first == 1) {

//---------------------------------------------------------------------
//     multiply c(i,jstart,k) by b_inverse and copy back to c
//     multiply rhs(jstart) by b_inverse(jstart) and copy to rhs
//---------------------------------------------------------------------
               binvcrhs( &lhsb(1,1,jstart),
                              &lhsc(1,1,i,jstart,k,c),
                              &rhs(1,i,jstart,k,c) );

            }

//---------------------------------------------------------------------
//     begin inner most do loop
//     do all the elements of the cell unless last 
//---------------------------------------------------------------------
            for (j = jstart+first; j <= jsize-last; j++) {

//---------------------------------------------------------------------
//     subtract A*lhs_vector(j-1) from lhs_vector(j)
//     
//     rhs(j) = rhs(j) - A*rhs(j-1)
//---------------------------------------------------------------------
               matvec_sub(&lhsa(1,1,j),
                               &rhs(1,i,j-1,k,c),&rhs(1,i,j,k,c));

//---------------------------------------------------------------------
//     B(j) = B(j) - C(j-1)*A(j)
//---------------------------------------------------------------------
               matmul_sub(&lhsa(1,1,j),
                               &lhsc(1,1,i,j-1,k,c),
                               &lhsb(1,1,j));

//---------------------------------------------------------------------
//     multiply c(i,j,k) by b_inverse and copy back to c
//     multiply rhs(i,1,k) by b_inverse(i,1,k) and copy to rhs
//---------------------------------------------------------------------
               binvcrhs( &lhsb(1,1,j),
                              &lhsc(1,1,i,j,k,c),
                              &rhs(1,i,j,k,c) );

            }

//---------------------------------------------------------------------
//     Now finish up special cases for last cell
//---------------------------------------------------------------------
            if (last == 1) {

//---------------------------------------------------------------------
//     rhs(jsize) = rhs(jsize) - A*rhs(jsize-1)
//---------------------------------------------------------------------
               matvec_sub(&lhsa(1,1,jsize),
                               &rhs(1,i,jsize-1,k,c),&rhs(1,i,jsize,k,c));

//---------------------------------------------------------------------
//     B(jsize) = B(jsize) - C(jsize-1)*A(jsize)
//     call matmul_sub(aa,i,jsize,k,c,
//     $              cc,i,jsize-1,k,c,bb,i,jsize,k,c)
//---------------------------------------------------------------------
               matmul_sub(&lhsa(1,1,jsize),
                               &lhsc(1,1,i,jsize-1,k,c),
                               &lhsb(1,1,jsize));

//---------------------------------------------------------------------
//     multiply rhs(jsize) by b_inverse(jsize) and copy to rhs
//---------------------------------------------------------------------
               binvrhs( &lhsb(1,1,jsize),
                             &rhs(1,i,jsize,k,c) );

            }
         }
      }


      return;
}
      


