//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include "header.h"
#include "mpinpb.h"
#define G_MAIN
#include "work_lhs.h"
#undef G_MAIN

extern void x_sendrecv_solve(int c, int cprev);
extern void x_sendrecv_back(int c, int cprev);
extern void x_backsubstitute(int first, int last, int c);
extern void x_solve_cell(int first, int last, int c);

void x_solve() {
//      printf("entered x_solve()...\n");fflush(0);

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     Performs line solves in X direction by first factoring
//     the block-tridiagonal matrix into an upper triangular matrix, 
//     and then performing back substitution to solve for the unknown
//     vectors of each line.  
//     
//     Make sure we treat elements zero to cell_size in the direction
//     of the sweep.
//---------------------------------------------------------------------

      int  c, cprev, stage, first, last, error;

//---------------------------------------------------------------------
//     in our terminology stage is the number of the cell in the x-direction
//     i.e. stage = 1 means the start of the line stage=ncells means end
//---------------------------------------------------------------------
      for (stage = 1; stage <= ncells; stage++) {
         c = slice(1,stage);
//      printf("passed slice.1(stage %0d)...\n", stage);fflush(0);
//---------------------------------------------------------------------
//     set first/last-cell flags
//---------------------------------------------------------------------
         first = (stage == 1);
         last =  (stage == ncells);

        if (stage >1) {
           cprev = slice(1,stage-1);
//           printf("passed slice.2(stage %0d)...\n", stage);fflush(0);
           x_sendrecv_solve(c, cprev);
//           printf("passed x_sendrecv_solve(stage %0d)...\n", stage);fflush(0);
        }
        x_solve_cell(first,last,c);
//        printf("passed x_solve_cell(stage %0d)...\n", stage);fflush(0);
      }
//      printf("passed \"set first/last-cell flags\"...\n");fflush(0);

//---------------------------------------------------------------------
//     now perform backsubstitution in reverse direction
//---------------------------------------------------------------------
      for (stage = ncells; stage >= 1; stage--) {
         c = slice(1,stage);
         first = (stage == 1);
         last =  (stage == ncells);

         if (stage <ncells) {
            cprev = slice(1,stage+1);
            x_sendrecv_back(c, cprev);
         }

         x_backsubstitute(first,last,c);
      }

//      printf("passed \"perform backsubstitution in reverse direction\"...\n");fflush(0);
      return;
}
      
      
void x_sendrecv_solve(int c, int cprev) {

//---------------------------------------------------------------------
//     pack up and send C'(iend) and rhs'(iend) for
//     all j and k of previous cell
//---------------------------------------------------------------------

      int j,k,m,n,isize,ptr, istart;
      int phase;
      int error, buffer_size;
//      printf("Entered x_sendrecv_solve()...\n");fflush(0);

      isize = cell_size(1,cprev)-1;
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*
           (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE);

//---------------------------------------------------------------------
//     pack up buffer
//---------------------------------------------------------------------
      ptr = 0;
      for (k = 0; k <= KMAX-1; k++) {
         for (j = 0; j <= JMAX-1; j++) {
            for (m = 1; m <= BLOCK_SIZE; m++) {
               for (n = 1; n <= BLOCK_SIZE; n++) {
                  in_buffer(ptr+n) = lhsc(m,n,isize,j,k,cprev);
               }
               ptr = ptr+BLOCK_SIZE;
            }
            for (n = 1; n <= BLOCK_SIZE; n++) {
               in_buffer(ptr+n) = rhs(n,isize,j,k,cprev);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }
//      printf("Passed \"pack up buffer\"...\n");fflush(0);

//---------------------------------------------------------------------
//     send and receive buffer 
//---------------------------------------------------------------------

      for (phase = 0; phase < 3; phase++) {

        if (send_color[EASTDIR]==phase) 
//          printf("before RCCE_send (in_buffer=%08x, size=%08d)\n", in_buffer, buffer_size*sizeof(double));fflush(0);
          RCCE_send((char*)in_buffer, buffer_size*sizeof(double), successor(1));
//          printf("after RCCE_send");fflush(0);
        if (recv_color[EASTDIR]==phase) 
//          printf("before RCCE_recv");fflush(0);
          RCCE_recv((char*)out_buffer, buffer_size*sizeof(double), predecessor(1));
//          printf("after RCCE_recv");fflush(0);
      }
//      printf("Passed \"send and receive buffer\"...\n");fflush(0);

//---------------------------------------------------------------------
//     unpack buffer
//---------------------------------------------------------------------
      istart = 0;
      ptr = 0;
      for (k = 0; k <= KMAX-1; k++) {
         for (j = 0; j <= JMAX-1; j++) {
            for (m = 1; m <= BLOCK_SIZE; m++) {
               for (n = 1; n <= BLOCK_SIZE; n++) {
                  lhsc(m,n,istart-1,j,k,c) = out_buffer(ptr+n);
               }
               ptr = ptr+BLOCK_SIZE;
            }
            for (n = 1; n <= BLOCK_SIZE; n++) {
               rhs(n,istart-1,j,k,c) = out_buffer(ptr+n);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }
//      printf("Passed \"unpack buffer\"...\n");fflush(0);

      return;
}

//---------------------------------------------------------------------
//---------------------------------------------------------------------

void x_sendrecv_back(int c, int cprev) {

//---------------------------------------------------------------------
//     pack up and send U(istart) for all j and k
//---------------------------------------------------------------------

      int j,k,n,ptr,istart,jp,kp;
      int phase;
      int error, buffer_size;

//---------------------------------------------------------------------
//     Send element 0 to previous processor
//---------------------------------------------------------------------
      istart = 0;
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*BLOCK_SIZE;
      ptr = 0;
      for (k = 0; k <= KMAX-1; k++) {
         for (j = 0; j <= JMAX-1; j++) {
            for (n = 1; n <= BLOCK_SIZE; n++) {
               in_buffer(ptr+n) = rhs(n,istart,j,k,cprev);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

//---------------------------------------------------------------------
//     send and receive buffer 
//---------------------------------------------------------------------

      for (phase = 0; phase < 3; phase++) {

        if (send_color[WESTDIR]==phase) 
          RCCE_send((char*)in_buffer, buffer_size*sizeof(double), predecessor(1));
        if (recv_color[WESTDIR]==phase) 
          RCCE_recv((char*)out_buffer, buffer_size*sizeof(double), successor(1));
      }

//---------------------------------------------------------------------
//     unpack U(isize) for all j and k
//---------------------------------------------------------------------

      ptr = 0;
      for (k = 0; k <= KMAX-1; k++) {
         for (j = 0; j <= JMAX-1; j++) {
            for (n = 1; n <= BLOCK_SIZE; n++) {
               backsub_info(n,j,k,c) = out_buffer(ptr+n);
            }
            ptr = ptr+BLOCK_SIZE;
         }
      }

      return;
}
      
void x_backsubstitute(int first, int last, int c) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     back solve: if last cell, then generate U(isize)=rhs(isize)
//     else assume U(isize) is loaded in un pack backsub_info
//     so just use it
//     after call u(istart) will be sent to next cell
//---------------------------------------------------------------------

      int i, j, k;
      int m,n,isize,jsize,ksize,istart;
      
      istart = 0;
      isize = cell_size(1,c)-1;
      jsize = cell_size(2,c)-end(2,c)-1      ;
      ksize = cell_size(3,c)-end(3,c)-1;
      if (last == 0) {
         for (k = start(3,c); k <= ksize; k++) {
            for (j = start(2,c); j <= jsize; j++) {
//---------------------------------------------------------------------
//     U(isize) uses info from previous cell if not last cell
//---------------------------------------------------------------------
               for (m = 1; m <= BLOCK_SIZE; m++) {
                  for (n = 1; n <= BLOCK_SIZE; n++) {
                     rhs(m,isize,j,k,c) = rhs(m,isize,j,k,c) 
                          - lhsc(m,n,isize,j,k,c)*
                          backsub_info(n,j,k,c);
                  }
               }
            }
         }
      }
      for (k = start(3,c); k <= ksize; k++) {
         for (j = start(2,c); j <= jsize; j++) {
            for (i = isize-1; i >= istart; i--) {
               for (m = 1; m <= BLOCK_SIZE; m++) {
                  for (n = 1; n <= BLOCK_SIZE; n++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) 
                          - lhsc(m,n,i,j,k,c)*rhs(n,i+1,j,k,c);
                  }
               }
            }
         }
      }

      return;
}


//---------------------------------------------------------------------
//---------------------------------------------------------------------

void x_solve_cell(int first, int last, int c) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     performs guaussian elimination on this cell.
//     
//     assumes that unpacking routines for non-first cells 
//     preload C' and rhs' from previous cell.
//     
//     assumed send happens outside this routine, but that
//     c'(IMAX) and rhs'(IMAX) will be sent to next cell
//---------------------------------------------------------------------

      int i,j,k,isize,ksize,jsize,istart;

      istart = 0;
      isize = cell_size(1,c)-1;
      jsize = cell_size(2,c)-end(2,c)-1;
      ksize = cell_size(3,c)-end(3,c)-1;

      lhsabinit(lhsa, lhsb, isize);

      for (k = start(3,c); k <= ksize; k++) {
         for (j = start(2,c); j <= jsize; j++) {

//---------------------------------------------------------------------
//     This function computes the left hand side in the xi-direction
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     determine a (labeled f) and n jacobians for cell c
//---------------------------------------------------------------------
            for (i = start(1,c)-1; i <= cell_size(1,c) - end(1,c); i++) {

               tmp1 = rho_i(i,j,k,c);
               tmp2 = tmp1 * tmp1;
               tmp3 = tmp1 * tmp2;
//---------------------------------------------------------------------
//     
//---------------------------------------------------------------------
               fjac(1,1,i) = 0.0e+00;
               fjac(1,2,i) = 1.0e+00;
               fjac(1,3,i) = 0.0e+00;
               fjac(1,4,i) = 0.0e+00;
               fjac(1,5,i) = 0.0e+00;

               fjac(2,1,i) = -(u(2,i,j,k,c) * tmp2 * 
                    u(2,i,j,k,c))
                    + c2 * qs(i,j,k,c);
               fjac(2,2,i) = ( 2.0e+00 - c2 )
                    * ( u(2,i,j,k,c) * tmp1 );
               fjac(2,3,i) = - c2 * ( u(3,i,j,k,c) * tmp1 );
               fjac(2,4,i) = - c2 * ( u(4,i,j,k,c) * tmp1 );
               fjac(2,5,i) = c2;

               fjac(3,1,i) = - ( u(2,i,j,k,c)*u(3,i,j,k,c) ) * tmp2;
               fjac(3,2,i) = u(3,i,j,k,c) * tmp1;
               fjac(3,3,i) = u(2,i,j,k,c) * tmp1;
               fjac(3,4,i) = 0.0e+00;
               fjac(3,5,i) = 0.0e+00;

               fjac(4,1,i) = - ( u(2,i,j,k,c)*u(4,i,j,k,c) ) * tmp2;
               fjac(4,2,i) = u(4,i,j,k,c) * tmp1;
               fjac(4,3,i) = 0.0e+00;
               fjac(4,4,i) = u(2,i,j,k,c) * tmp1;
               fjac(4,5,i) = 0.0e+00;

               fjac(5,1,i) = ( c2 * 2.0e0 * qs(i,j,k,c)
                    - c1 * ( u(5,i,j,k,c) * tmp1 ) )
                    * ( u(2,i,j,k,c) * tmp1 );
               fjac(5,2,i) = c1 *  u(5,i,j,k,c) * tmp1 
                    - c2
                    * ( u(2,i,j,k,c)*u(2,i,j,k,c) * tmp2
                    + qs(i,j,k,c) );
               fjac(5,3,i) = - c2 * ( u(3,i,j,k,c)*u(2,i,j,k,c) )
                    * tmp2;
               fjac(5,4,i) = - c2 * ( u(4,i,j,k,c)*u(2,i,j,k,c) )
                    * tmp2;
               fjac(5,5,i) = c1 * ( u(2,i,j,k,c) * tmp1 );

               njac(1,1,i) = 0.0e+00;
               njac(1,2,i) = 0.0e+00;
               njac(1,3,i) = 0.0e+00;
               njac(1,4,i) = 0.0e+00;
               njac(1,5,i) = 0.0e+00;

               njac(2,1,i) = - con43 * c3c4 * tmp2 * u(2,i,j,k,c);
               njac(2,2,i) =   con43 * c3c4 * tmp1;
               njac(2,3,i) =   0.0e+00;
               njac(2,4,i) =   0.0e+00;
               njac(2,5,i) =   0.0e+00;

               njac(3,1,i) = - c3c4 * tmp2 * u(3,i,j,k,c);
               njac(3,2,i) =   0.0e+00;
               njac(3,3,i) =   c3c4 * tmp1;
               njac(3,4,i) =   0.0e+00;
               njac(3,5,i) =   0.0e+00;

               njac(4,1,i) = - c3c4 * tmp2 * u(4,i,j,k,c);
               njac(4,2,i) =   0.0e+00 ;
               njac(4,3,i) =   0.0e+00;
               njac(4,4,i) =   c3c4 * tmp1;
               njac(4,5,i) =   0.0e+00;

               njac(5,1,i) = - ( con43 * c3c4
                    - c1345 ) * tmp3 * SQR(u(2,i,j,k,c))
                    - ( c3c4 - c1345 ) * tmp3 * SQR(u(3,i,j,k,c))
                    - ( c3c4 - c1345 ) * tmp3 * SQR(u(4,i,j,k,c))
                    - c1345 * tmp2 * u(5,i,j,k,c);

               njac(5,2,i) = ( con43 * c3c4
                    - c1345 ) * tmp2 * u(2,i,j,k,c);
               njac(5,3,i) = ( c3c4 - c1345 ) * tmp2 * u(3,i,j,k,c);
               njac(5,4,i) = ( c3c4 - c1345 ) * tmp2 * u(4,i,j,k,c);
               njac(5,5,i) = ( c1345 ) * tmp1;

            }
//---------------------------------------------------------------------
//     now jacobians set, so form left hand side in x direction
//---------------------------------------------------------------------
            for (i = start(1,c); i <= isize - end(1,c); i++) {

               tmp1 = dt * tx1;
               tmp2 = dt * tx2;

               lhsa(1,1,i) = - tmp2 * fjac(1,1,i-1)
                    - tmp1 * njac(1,1,i-1)
                    - tmp1 * dx1 ;
               lhsa(1,2,i) = - tmp2 * fjac(1,2,i-1)
                    - tmp1 * njac(1,2,i-1);
               lhsa(1,3,i) = - tmp2 * fjac(1,3,i-1)
                    - tmp1 * njac(1,3,i-1);
               lhsa(1,4,i) = - tmp2 * fjac(1,4,i-1)
                    - tmp1 * njac(1,4,i-1);
               lhsa(1,5,i) = - tmp2 * fjac(1,5,i-1)
                    - tmp1 * njac(1,5,i-1);

               lhsa(2,1,i) = - tmp2 * fjac(2,1,i-1)
                    - tmp1 * njac(2,1,i-1);
               lhsa(2,2,i) = - tmp2 * fjac(2,2,i-1)
                    - tmp1 * njac(2,2,i-1)
                    - tmp1 * dx2;
               lhsa(2,3,i) = - tmp2 * fjac(2,3,i-1)
                    - tmp1 * njac(2,3,i-1);
               lhsa(2,4,i) = - tmp2 * fjac(2,4,i-1)
                    - tmp1 * njac(2,4,i-1);
               lhsa(2,5,i) = - tmp2 * fjac(2,5,i-1)
                    - tmp1 * njac(2,5,i-1);

               lhsa(3,1,i) = - tmp2 * fjac(3,1,i-1)
                    - tmp1 * njac(3,1,i-1);
               lhsa(3,2,i) = - tmp2 * fjac(3,2,i-1)
                    - tmp1 * njac(3,2,i-1);
               lhsa(3,3,i) = - tmp2 * fjac(3,3,i-1)
                    - tmp1 * njac(3,3,i-1)
                    - tmp1 * dx3 ;
               lhsa(3,4,i) = - tmp2 * fjac(3,4,i-1)
                    - tmp1 * njac(3,4,i-1);
               lhsa(3,5,i) = - tmp2 * fjac(3,5,i-1)
                    - tmp1 * njac(3,5,i-1);

               lhsa(4,1,i) = - tmp2 * fjac(4,1,i-1)
                    - tmp1 * njac(4,1,i-1);
               lhsa(4,2,i) = - tmp2 * fjac(4,2,i-1)
                    - tmp1 * njac(4,2,i-1);
               lhsa(4,3,i) = - tmp2 * fjac(4,3,i-1)
                    - tmp1 * njac(4,3,i-1);
               lhsa(4,4,i) = - tmp2 * fjac(4,4,i-1)
                    - tmp1 * njac(4,4,i-1)
                    - tmp1 * dx4;
               lhsa(4,5,i) = - tmp2 * fjac(4,5,i-1)
                    - tmp1 * njac(4,5,i-1);

               lhsa(5,1,i) = - tmp2 * fjac(5,1,i-1)
                    - tmp1 * njac(5,1,i-1);
               lhsa(5,2,i) = - tmp2 * fjac(5,2,i-1)
                    - tmp1 * njac(5,2,i-1);
               lhsa(5,3,i) = - tmp2 * fjac(5,3,i-1)
                    - tmp1 * njac(5,3,i-1);
               lhsa(5,4,i) = - tmp2 * fjac(5,4,i-1)
                    - tmp1 * njac(5,4,i-1);
               lhsa(5,5,i) = - tmp2 * fjac(5,5,i-1)
                    - tmp1 * njac(5,5,i-1)
                    - tmp1 * dx5;

               lhsb(1,1,i) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(1,1,i)
                    + tmp1 * 2.0e+00 * dx1;
               lhsb(1,2,i) = tmp1 * 2.0e+00 * njac(1,2,i);
               lhsb(1,3,i) = tmp1 * 2.0e+00 * njac(1,3,i);
               lhsb(1,4,i) = tmp1 * 2.0e+00 * njac(1,4,i);
               lhsb(1,5,i) = tmp1 * 2.0e+00 * njac(1,5,i);

               lhsb(2,1,i) = tmp1 * 2.0e+00 * njac(2,1,i);
               lhsb(2,2,i) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(2,2,i)
                    + tmp1 * 2.0e+00 * dx2;
               lhsb(2,3,i) = tmp1 * 2.0e+00 * njac(2,3,i);
               lhsb(2,4,i) = tmp1 * 2.0e+00 * njac(2,4,i);
               lhsb(2,5,i) = tmp1 * 2.0e+00 * njac(2,5,i);

               lhsb(3,1,i) = tmp1 * 2.0e+00 * njac(3,1,i);
               lhsb(3,2,i) = tmp1 * 2.0e+00 * njac(3,2,i);
               lhsb(3,3,i) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(3,3,i)
                    + tmp1 * 2.0e+00 * dx3;
               lhsb(3,4,i) = tmp1 * 2.0e+00 * njac(3,4,i);
               lhsb(3,5,i) = tmp1 * 2.0e+00 * njac(3,5,i);

               lhsb(4,1,i) = tmp1 * 2.0e+00 * njac(4,1,i);
               lhsb(4,2,i) = tmp1 * 2.0e+00 * njac(4,2,i);
               lhsb(4,3,i) = tmp1 * 2.0e+00 * njac(4,3,i);
               lhsb(4,4,i) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(4,4,i)
                    + tmp1 * 2.0e+00 * dx4;
               lhsb(4,5,i) = tmp1 * 2.0e+00 * njac(4,5,i);

               lhsb(5,1,i) = tmp1 * 2.0e+00 * njac(5,1,i);
               lhsb(5,2,i) = tmp1 * 2.0e+00 * njac(5,2,i);
               lhsb(5,3,i) = tmp1 * 2.0e+00 * njac(5,3,i);
               lhsb(5,4,i) = tmp1 * 2.0e+00 * njac(5,4,i);
               lhsb(5,5,i) = 1.0e+00
                    + tmp1 * 2.0e+00 * njac(5,5,i)
                    + tmp1 * 2.0e+00 * dx5;

               lhsc(1,1,i,j,k,c) =  tmp2 * fjac(1,1,i+1)
                    - tmp1 * njac(1,1,i+1)
                    - tmp1 * dx1;
               lhsc(1,2,i,j,k,c) =  tmp2 * fjac(1,2,i+1)
                    - tmp1 * njac(1,2,i+1);
               lhsc(1,3,i,j,k,c) =  tmp2 * fjac(1,3,i+1)
                    - tmp1 * njac(1,3,i+1);
               lhsc(1,4,i,j,k,c) =  tmp2 * fjac(1,4,i+1)
                    - tmp1 * njac(1,4,i+1);
               lhsc(1,5,i,j,k,c) =  tmp2 * fjac(1,5,i+1)
                    - tmp1 * njac(1,5,i+1);

               lhsc(2,1,i,j,k,c) =  tmp2 * fjac(2,1,i+1)
                    - tmp1 * njac(2,1,i+1);
               lhsc(2,2,i,j,k,c) =  tmp2 * fjac(2,2,i+1)
                    - tmp1 * njac(2,2,i+1)
                    - tmp1 * dx2;
               lhsc(2,3,i,j,k,c) =  tmp2 * fjac(2,3,i+1)
                    - tmp1 * njac(2,3,i+1);
               lhsc(2,4,i,j,k,c) =  tmp2 * fjac(2,4,i+1)
                    - tmp1 * njac(2,4,i+1);
               lhsc(2,5,i,j,k,c) =  tmp2 * fjac(2,5,i+1)
                    - tmp1 * njac(2,5,i+1);

               lhsc(3,1,i,j,k,c) =  tmp2 * fjac(3,1,i+1)
                    - tmp1 * njac(3,1,i+1);
               lhsc(3,2,i,j,k,c) =  tmp2 * fjac(3,2,i+1)
                    - tmp1 * njac(3,2,i+1);
               lhsc(3,3,i,j,k,c) =  tmp2 * fjac(3,3,i+1)
                    - tmp1 * njac(3,3,i+1)
                    - tmp1 * dx3;
               lhsc(3,4,i,j,k,c) =  tmp2 * fjac(3,4,i+1)
                    - tmp1 * njac(3,4,i+1);
               lhsc(3,5,i,j,k,c) =  tmp2 * fjac(3,5,i+1)
                    - tmp1 * njac(3,5,i+1);

               lhsc(4,1,i,j,k,c) =  tmp2 * fjac(4,1,i+1)
                    - tmp1 * njac(4,1,i+1);
               lhsc(4,2,i,j,k,c) =  tmp2 * fjac(4,2,i+1)
                    - tmp1 * njac(4,2,i+1);
               lhsc(4,3,i,j,k,c) =  tmp2 * fjac(4,3,i+1)
                    - tmp1 * njac(4,3,i+1);
               lhsc(4,4,i,j,k,c) =  tmp2 * fjac(4,4,i+1)
                    - tmp1 * njac(4,4,i+1)
                    - tmp1 * dx4;
               lhsc(4,5,i,j,k,c) =  tmp2 * fjac(4,5,i+1)
                    - tmp1 * njac(4,5,i+1);

               lhsc(5,1,i,j,k,c) =  tmp2 * fjac(5,1,i+1)
                    - tmp1 * njac(5,1,i+1);
               lhsc(5,2,i,j,k,c) =  tmp2 * fjac(5,2,i+1)
                    - tmp1 * njac(5,2,i+1);
               lhsc(5,3,i,j,k,c) =  tmp2 * fjac(5,3,i+1)
                    - tmp1 * njac(5,3,i+1);
               lhsc(5,4,i,j,k,c) =  tmp2 * fjac(5,4,i+1)
                    - tmp1 * njac(5,4,i+1);
               lhsc(5,5,i,j,k,c) =  tmp2 * fjac(5,5,i+1)
                    - tmp1 * njac(5,5,i+1)
                    - tmp1 * dx5;

            }


//---------------------------------------------------------------------
//     outer most do loops - sweeping in i direction
//---------------------------------------------------------------------
            if (first == 1) {

//---------------------------------------------------------------------
//     multiply c(istart,j,k) by b_inverse and copy back to c
//     multiply rhs(istart) by b_inverse(istart) and copy to rhs
//---------------------------------------------------------------------
               binvcrhs( &lhsb(1,1,istart),
                              &lhsc(1,1,istart,j,k,c),
                              &rhs(1,istart,j,k,c) );

            }

//---------------------------------------------------------------------
//     begin inner most do loop
//     do all the elements of the cell unless last 
//---------------------------------------------------------------------
            for (i = istart+first; i <= isize-last; i++) {

//---------------------------------------------------------------------
//     rhs(i) = rhs(i) - A*rhs(i-1)
//---------------------------------------------------------------------
               matvec_sub(&lhsa(1,1,i),
                               &rhs(1,i-1,j,k,c),&rhs(1,i,j,k,c));

//---------------------------------------------------------------------
//     B(i) = B(i) - C(i-1)*A(i)
//---------------------------------------------------------------------
               matmul_sub(&lhsa(1,1,i),
                               &lhsc(1,1,i-1,j,k,c),
                               &lhsb(1,1,i));


//---------------------------------------------------------------------
//     multiply c(i,j,k) by b_inverse and copy back to c
//     multiply rhs(1,j,k) by b_inverse(1,j,k) and copy to rhs
//---------------------------------------------------------------------
               binvcrhs( &lhsb(1,1,i),
                              &lhsc(1,1,i,j,k,c),
                              &rhs(1,i,j,k,c) );

            }

//---------------------------------------------------------------------
//     Now finish up special cases for last cell
//---------------------------------------------------------------------
            if (last == 1) {

//---------------------------------------------------------------------
//     rhs(isize) = rhs(isize) - A*rhs(isize-1)
//---------------------------------------------------------------------
               matvec_sub(&lhsa(1,1,isize),
                               &rhs(1,isize-1,j,k,c),&rhs(1,isize,j,k,c));

//---------------------------------------------------------------------
//     B(isize) = B(isize) - C(isize-1)*A(isize)
//---------------------------------------------------------------------
               matmul_sub(&lhsa(1,1,isize),
                               &lhsc(1,1,isize-1,j,k,c),
                               &lhsb(1,1,isize));

//---------------------------------------------------------------------
//     multiply rhs() by b_inverse() and copy to rhs
//---------------------------------------------------------------------
               binvrhs( &lhsb(1,1,isize),
                             &rhs(1,isize,j,k,c) );

            }
         }
      }


      return;
}
      
