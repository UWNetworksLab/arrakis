//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include "header.h"

void  initialize() {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     This subroutine initializes the field variable u using 
//     tri-linear transfinite interpolation of the boundary values     
//---------------------------------------------------------------------
      
      int c, i, j, k, m, ii, jj, kk, ix, iy, iz;
      double xi, eta, zeta, Pface[5*3*2], Pxi, Peta, 
           Pzeta, temp[5];
#define Pface(m,n,i) Pface[(m-1)+5*((n-1)+3*(i-1))]
#define temp(m) temp[m-1]

//---------------------------------------------------------------------
//  Later (in compute_rhs) we compute 1/u for every element. A few of 
//  the corner elements are not used, but it convenient (and faster) 
//  to compute the whole thing with a simple loop. Make sure those 
//  values are nonzero by initializing the whole thing here. 
//---------------------------------------------------------------------
      for (c = 1; c <= ncells; c++) {
         for (kk = -1; kk <= KMAX; kk++) {
            for (jj = -1; jj <= JMAX; jj++) {
               for (ii = -1; ii <= IMAX; ii++) {
                  for (m = 1; m <= 5; m++) {
                     u(m, ii, jj, kk, c) = 1.0;
                  }
               }
            }
         }
      }
//---------------------------------------------------------------------



//---------------------------------------------------------------------
//     first store the "interpolated" values everywhere on the grid    
//---------------------------------------------------------------------
      for (c = 1; c <= ncells; c++) {
         kk = 0;
         for (k = cell_low(3,c); k <= cell_high(3,c); k++) {
            zeta = (double)(k) * dnzm1;
            jj = 0;
            for (j = cell_low(2,c); j <= cell_high(2,c); j++) {
               eta = (double)(j) * dnym1;
               ii = 0;
               for (i = cell_low(1,c); i <= cell_high(1,c); i++) {
                  xi = (double)(i) * dnxm1;
                  
                  for (ix = 1; ix <= 2; ix++) {
                     exact_solution((double)(ix-1), eta, zeta, 
                          &Pface(1,1,ix));
                  }

                  for (iy = 1; iy <= 2; iy++) {
                     exact_solution(xi, (double)(iy-1) , zeta, 
                          &Pface(1,2,iy));
                  }

                  for (iz = 1; iz <= 2; iz++) {
                     exact_solution(xi, eta, (double)(iz-1),   
                          &Pface(1,3,iz));
                  }

                  for (m = 1; m <= 5; m++) {
                     Pxi   = xi   * Pface(m,1,2) + 
                          (1.0e0-xi)   * Pface(m,1,1);
                     Peta  = eta  * Pface(m,2,2) + 
                          (1.0e0-eta)  * Pface(m,2,1);
                     Pzeta = zeta * Pface(m,3,2) + 
                          (1.0e0-zeta) * Pface(m,3,1);
                     
                     u(m,ii,jj,kk,c) = Pxi + Peta + Pzeta - 
                          Pxi*Peta - Pxi*Pzeta - Peta*Pzeta + 
                          Pxi*Peta*Pzeta;

                  }
                  ii = ii + 1;
               }
               jj = jj + 1;
            }
            kk = kk+1;
         }
      }

//---------------------------------------------------------------------
//     now store the exact values on the boundaries        
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     west face                                                  
//---------------------------------------------------------------------
      c = slice(1,1);
      ii = 0;
      xi = 0.0e0;
      kk = 0;
      for (k = cell_low(3,c); k <= cell_high(3,c); k++) {
         zeta = (double)(k) * dnzm1;
         jj = 0;
         for (j = cell_low(2,c); j <= cell_high(2,c); j++) {
            eta = (double)(j) * dnym1;
            exact_solution(xi, eta, zeta, temp);
            for (m = 1; m <= 5; m++) {
               u(m,ii,jj,kk,c) = temp(m);
            }
            jj = jj + 1;
         }
         kk = kk + 1;
      }

//---------------------------------------------------------------------
//     east face                                                      
//---------------------------------------------------------------------
      c  = slice(1,ncells);
      ii = cell_size(1,c)-1;
      xi = 1.0e0;
      kk = 0;
      for (k = cell_low(3,c); k <= cell_high(3,c); k++) {
         zeta = (double)(k) * dnzm1;
         jj = 0;
         for (j = cell_low(2,c); j <= cell_high(2,c); j++) {
            eta = (double)(j) * dnym1;
            exact_solution(xi, eta, zeta, temp);
            for (m = 1; m <= 5; m++) {
               u(m,ii,jj,kk,c) = temp(m);
            }
            jj = jj + 1;
         }
         kk = kk + 1;
      }

//---------------------------------------------------------------------
//     south face                                                 
//---------------------------------------------------------------------
      c = slice(2,1);
      jj = 0;
      eta = 0.0e0;
      kk = 0;
      for (k = cell_low(3,c); k <= cell_high(3,c); k++) {
         zeta = (double)(k) * dnzm1;
         ii = 0;
         for (i = cell_low(1,c); i <= cell_high(1,c); i++) {
            xi = (double)(i) * dnxm1;
            exact_solution(xi, eta, zeta, temp);
            for (m = 1; m <= 5; m++) {
               u(m,ii,jj,kk,c) = temp(m);
            }
            ii = ii + 1;
         }
         kk = kk + 1;
      }


//---------------------------------------------------------------------
//     north face                                    
//---------------------------------------------------------------------
      c = slice(2,ncells);
      jj = cell_size(2,c)-1;
      eta = 1.0e0;
      kk = 0;
      for (k = cell_low(3,c); k <= cell_high(3,c); k++) {
         zeta = (double)(k) * dnzm1;
         ii = 0;
         for (i = cell_low(1,c); i <= cell_high(1,c); i++) {
            xi = (double)(i) * dnxm1;
            exact_solution(xi, eta, zeta, temp);
            for (m = 1; m <= 5; m++) {
               u(m,ii,jj,kk,c) = temp(m);
            }
            ii = ii + 1;
         }
         kk = kk + 1;
      }

//---------------------------------------------------------------------
//     bottom face                                       
//---------------------------------------------------------------------
      c = slice(3,1);
      kk = 0;
      zeta = 0.0e0;
      jj = 0;
      for (j = cell_low(2,c); j <= cell_high(2,c); j++) {
         eta = (double)(j) * dnym1;
         ii = 0;
         for (i = cell_low(1,c); i <= cell_high(1,c); i++) {
            xi = (double)(i) *dnxm1;
            exact_solution(xi, eta, zeta, temp);
            for (m = 1; m <= 5; m++) {
               u(m,ii,jj,kk,c) = temp(m);
            }
            ii = ii + 1;
         }
         jj = jj + 1;
      }

//---------------------------------------------------------------------
//     top face     
//---------------------------------------------------------------------
      c = slice(3,ncells);
      kk = cell_size(3,c)-1;
      zeta = 1.0e0;
      jj = 0;
      for (j = cell_low(2,c); j <= cell_high(2,c); j++) {
         eta = (double)(j) * dnym1;
         ii = 0;
         for (i = cell_low(1,c); i <= cell_high(1,c); i++) {
            xi = (double)(i) * dnxm1;
            exact_solution(xi, eta, zeta, temp);
            for (m = 1; m <= 5; m++) {
               u(m,ii,jj,kk,c) = temp(m);
            }
            ii = ii + 1;
         }
         jj = jj + 1;
      }

      return;
}


//---------------------------------------------------------------------
//---------------------------------------------------------------------

void lhsinit() {

//---------------------------------------------------------------------
//---------------------------------------------------------------------
      
      int i, j, k, d, c, m, n;

//---------------------------------------------------------------------
//     loop over all cells                                       
//---------------------------------------------------------------------
      for (c = 1; c <= ncells; c++) {

//---------------------------------------------------------------------
//     first, initialize the start and end arrays
//---------------------------------------------------------------------
         for (d = 1; d <= 3; d++) {
            if (cell_coord(d,c) == 1) {
               start(d,c) = 1;
            } else {
               start(d,c) = 0;
            }
            if (cell_coord(d,c) == ncells) {
               end(d,c) = 1;
            } else {
               end(d,c) = 0;
            }
         }

//---------------------------------------------------------------------
//     zero the whole left hand side for starters
//---------------------------------------------------------------------
         for (k = 0; k <= cell_size(3,c)-1; k++) {
            for (j = 0; j <= cell_size(2,c)-1; j++) {
               for (i = 0; i <= cell_size(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     for (n = 1; n <= 5; n++) {
                        lhsc(m,n,i,j,k,c) = 0.0e0;
                     }
                  }
               }
            }
         }

      }

      return;
}


//---------------------------------------------------------------------
//---------------------------------------------------------------------

void lhsabinit(double lhsa[], double lhsb[], int size) {

#define lhsa(m,n,i) lhsa[(m-1)+5*((n-1)+5*(i+1))]
#define lhsb(m,n,i) lhsb[(m-1)+5*((n-1)+5*(i+1))]

      int i, m, n;

//---------------------------------------------------------------------
//     next, set all diagonal values to 1. This is overkill, but convenient
//---------------------------------------------------------------------
      for (i = 0; i <= size; i++) {
         for (m = 1; m <= 5; m++) {
            for (n = 1; n <= 5; n++) {
               lhsa(m,n,i) = 0.0e0;
               lhsb(m,n,i) = 0.0e0;
            }
            lhsb(m,m,i) = 1.0e0;
         }
      }

      return;
}



