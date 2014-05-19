
//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include "header.h"

void exact_rhs() {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     compute the right hand side based on exact solution
//---------------------------------------------------------------------

      double dtemp[5], xi, eta, zeta, dtpp;
      int          c, m, i, j, k, ip1, im1, jp1, 
           jm1, km1, kp1;
#define dtemp(m) dtemp[m-1]


//---------------------------------------------------------------------
//     loop over all cells owned by this node                   
//---------------------------------------------------------------------
      for (c = 1; c <= ncells; c++) {

//---------------------------------------------------------------------
//     initialize                                  
//---------------------------------------------------------------------
         for (k = 0; k <= cell_size(3,c)-1; k++) {
            for (j = 0; j <= cell_size(2,c)-1; j++) {
               for (i = 0; i <= cell_size(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     forcing(m,i,j,k,c) = 0.0e0;
                  }
               }
            }
         }

//---------------------------------------------------------------------
//     xi-direction flux differences                      
//---------------------------------------------------------------------
         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            zeta = (double)(k+cell_low(3,c)) * dnzm1;
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               eta = (double)(j+cell_low(2,c)) * dnym1;

               for (i = -2*(1-start(1,c)); i <= cell_size(1,c)+1-2*end(1,c); i++) {
                  xi = (double)(i+cell_low(1,c)) * dnxm1;

                  exact_solution(xi, eta, zeta, dtemp);
                  for (m = 1; m <= 5; m++) {
                     ue(i,m) = dtemp(m);
                  }

                  dtpp = 1.0e0 / dtemp(1);

                  for (m = 2; m <= 5; m++) {
                     buf(i,m) = dtpp * dtemp(m);
                  }

                  cuf(i)   = buf(i,2) * buf(i,2);
                  buf(i,1) = cuf(i) + buf(i,3) * buf(i,3) + 
                       buf(i,4) * buf(i,4) ;
                  q(i) = 0.5e0*(buf(i,2)*ue(i,2) + buf(i,3)*ue(i,3) +
                       buf(i,4)*ue(i,4));

               }
               
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  im1 = i-1;
                  ip1 = i+1;

                  forcing(1,i,j,k,c) = forcing(1,i,j,k,c) -
                       tx2*( ue(ip1,2)-ue(im1,2) )+
                       dx1tx1*(ue(ip1,1)-2.0e0*ue(i,1)+ue(im1,1));

                  forcing(2,i,j,k,c) = forcing(2,i,j,k,c) - tx2 * (
                       (ue(ip1,2)*buf(ip1,2)+c2*(ue(ip1,5)-q(ip1)))-
                       (ue(im1,2)*buf(im1,2)+c2*(ue(im1,5)-q(im1))))+
                       xxcon1*(buf(ip1,2)-2.0e0*buf(i,2)+buf(im1,2))+
                       dx2tx1*( ue(ip1,2)-2.0e0* ue(i,2)+ue(im1,2));

                  forcing(3,i,j,k,c) = forcing(3,i,j,k,c) - tx2 * (
                       ue(ip1,3)*buf(ip1,2)-ue(im1,3)*buf(im1,2))+
                       xxcon2*(buf(ip1,3)-2.0e0*buf(i,3)+buf(im1,3))+
                       dx3tx1*( ue(ip1,3)-2.0e0*ue(i,3) +ue(im1,3));
                  
                  forcing(4,i,j,k,c) = forcing(4,i,j,k,c) - tx2*(
                       ue(ip1,4)*buf(ip1,2)-ue(im1,4)*buf(im1,2))+
                       xxcon2*(buf(ip1,4)-2.0e0*buf(i,4)+buf(im1,4))+
                       dx4tx1*( ue(ip1,4)-2.0e0* ue(i,4)+ ue(im1,4));

                  forcing(5,i,j,k,c) = forcing(5,i,j,k,c) - tx2*(
                       buf(ip1,2)*(c1*ue(ip1,5)-c2*q(ip1))-
                       buf(im1,2)*(c1*ue(im1,5)-c2*q(im1)))+
                       0.5e0*xxcon3*(buf(ip1,1)-2.0e0*buf(i,1)+
                       buf(im1,1))+
                       xxcon4*(cuf(ip1)-2.0e0*cuf(i)+cuf(im1))+
                       xxcon5*(buf(ip1,5)-2.0e0*buf(i,5)+buf(im1,5))+
                       dx5tx1*( ue(ip1,5)-2.0e0* ue(i,5)+ ue(im1,5));
               }

//---------------------------------------------------------------------
//     Fourth-order dissipation                         
//---------------------------------------------------------------------
               if (start(1,c) > 0) {
                  for (m = 1; m <= 5; m++) {
                     i = 1;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (5.0e0*ue(i,m) - 4.0e0*ue(i+1,m) +ue(i+2,m));
                     i = 2;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (-4.0e0*ue(i-1,m) + 6.0e0*ue(i,m) -
                          4.0e0*ue(i+1,m) +       ue(i+2,m));
                  }
               }

               for (i = start(1,c)*3; i <= cell_size(1,c)-3*end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp*
                          (ue(i-2,m) - 4.0e0*ue(i-1,m) +
                          6.0e0*ue(i,m) - 4.0e0*ue(i+1,m) + ue(i+2,m));
                  }
               }

               if (end(1,c) > 0) {
                  for (m = 1; m <= 5; m++) {
                     i = cell_size(1,c)-3;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (ue(i-2,m) - 4.0e0*ue(i-1,m) +
                          6.0e0*ue(i,m) - 4.0e0*ue(i+1,m));
                     i = cell_size(1,c)-2;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (ue(i-2,m) - 4.0e0*ue(i-1,m) + 5.0e0*ue(i,m));
                  }
               }

            }
         }

//---------------------------------------------------------------------
//     eta-direction flux differences             
//---------------------------------------------------------------------
         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            zeta = (double)(k+cell_low(3,c)) * dnzm1;
            for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
               xi = (double)(i+cell_low(1,c)) * dnxm1;

               for (j = -2*(1-start(2,c)); j <= cell_size(2,c)+1-2*end(2,c); j++) {
                  eta = (double)(j+cell_low(2,c)) * dnym1;

                  exact_solution(xi, eta, zeta, dtemp);
                  for (m = 1; m <= 5; m++) {
                     ue(j,m) = dtemp(m);
                  }
                  
                  dtpp = 1.0e0/dtemp(1);

                  for (m = 2; m <= 5; m++) {
                     buf(j,m) = dtpp * dtemp(m);
                  }

                  cuf(j)   = buf(j,3) * buf(j,3);
                  buf(j,1) = cuf(j) + buf(j,2) * buf(j,2) + 
                       buf(j,4) * buf(j,4);
                  q(j) = 0.5e0*(buf(j,2)*ue(j,2) + buf(j,3)*ue(j,3) +
                       buf(j,4)*ue(j,4));
               }

               for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
                  jm1 = j-1;
                  jp1 = j+1;
                  
                  forcing(1,i,j,k,c) = forcing(1,i,j,k,c) -
                       ty2*( ue(jp1,3)-ue(jm1,3) )+
                       dy1ty1*(ue(jp1,1)-2.0e0*ue(j,1)+ue(jm1,1));

                  forcing(2,i,j,k,c) = forcing(2,i,j,k,c) - ty2*(
                       ue(jp1,2)*buf(jp1,3)-ue(jm1,2)*buf(jm1,3))+
                       yycon2*(buf(jp1,2)-2.0e0*buf(j,2)+buf(jm1,2))+
                       dy2ty1*( ue(jp1,2)-2.0* ue(j,2)+ ue(jm1,2));

                  forcing(3,i,j,k,c) = forcing(3,i,j,k,c) - ty2*(
                       (ue(jp1,3)*buf(jp1,3)+c2*(ue(jp1,5)-q(jp1)))-
                       (ue(jm1,3)*buf(jm1,3)+c2*(ue(jm1,5)-q(jm1))))+
                       yycon1*(buf(jp1,3)-2.0e0*buf(j,3)+buf(jm1,3))+
                       dy3ty1*( ue(jp1,3)-2.0e0*ue(j,3) +ue(jm1,3));

                  forcing(4,i,j,k,c) = forcing(4,i,j,k,c) - ty2*(
                       ue(jp1,4)*buf(jp1,3)-ue(jm1,4)*buf(jm1,3))+
                       yycon2*(buf(jp1,4)-2.0e0*buf(j,4)+buf(jm1,4))+
                       dy4ty1*( ue(jp1,4)-2.0e0*ue(j,4)+ ue(jm1,4));

                  forcing(5,i,j,k,c) = forcing(5,i,j,k,c) - ty2*(
                       buf(jp1,3)*(c1*ue(jp1,5)-c2*q(jp1))-
                       buf(jm1,3)*(c1*ue(jm1,5)-c2*q(jm1)))+
                       0.5e0*yycon3*(buf(jp1,1)-2.0e0*buf(j,1)+
                       buf(jm1,1))+
                       yycon4*(cuf(jp1)-2.0e0*cuf(j)+cuf(jm1))+
                       yycon5*(buf(jp1,5)-2.0e0*buf(j,5)+buf(jm1,5))+
                       dy5ty1*(ue(jp1,5)-2.0e0*ue(j,5)+ue(jm1,5));
               }

//---------------------------------------------------------------------
//     Fourth-order dissipation                      
//---------------------------------------------------------------------
               if (start(2,c) > 0) {
                  for (m = 1; m <= 5; m++) {
                     j = 1;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (5.0e0*ue(j,m) - 4.0e0*ue(j+1,m) +ue(j+2,m));
                     j = 2;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (-4.0e0*ue(j-1,m) + 6.0e0*ue(j,m) -
                          4.0e0*ue(j+1,m) +       ue(j+2,m));
                  }
               }

               for (j = start(2,c)*3; j <= cell_size(2,c)-3*end(2,c)-1; j++) {
                  for (m = 1; m <= 5; m++) {
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp*
                          (ue(j-2,m) - 4.0e0*ue(j-1,m) +
                          6.0e0*ue(j,m) - 4.0e0*ue(j+1,m) + ue(j+2,m));
                  }
               }

               if (end(2,c) > 0) {
                  for (m = 1; m <= 5; m++) {
                     j = cell_size(2,c)-3;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (ue(j-2,m) - 4.0e0*ue(j-1,m) +
                          6.0e0*ue(j,m) - 4.0e0*ue(j+1,m));
                     j = cell_size(2,c)-2;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (ue(j-2,m) - 4.0e0*ue(j-1,m) + 5.0e0*ue(j,m));

                  }
               }

            }
         }

//---------------------------------------------------------------------
//     zeta-direction flux differences                      
//---------------------------------------------------------------------
         for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
            eta = (double)(j+cell_low(2,c)) * dnym1;
            for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
               xi = (double)(i+cell_low(1,c)) * dnxm1;

               for (k = -2*(1-start(3,c)); k <= cell_size(3,c)+1-2*end(3,c); k++) {
                  zeta = (double)(k+cell_low(3,c)) * dnzm1;

                  exact_solution(xi, eta, zeta, dtemp);
                  for (m = 1; m <= 5; m++) {
                     ue(k,m) = dtemp(m);
                  }

                  dtpp = 1.0e0/dtemp(1);

                  for (m = 2; m <= 5; m++) {
                     buf(k,m) = dtpp * dtemp(m);
                  }

                  cuf(k)   = buf(k,4) * buf(k,4);
                  buf(k,1) = cuf(k) + buf(k,2) * buf(k,2) + 
                       buf(k,3) * buf(k,3);
                  q(k) = 0.5e0*(buf(k,2)*ue(k,2) + buf(k,3)*ue(k,3) +
                       buf(k,4)*ue(k,4));
               }

               for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
                  km1 = k-1;
                  kp1 = k+1;
                  
                  forcing(1,i,j,k,c) = forcing(1,i,j,k,c) -
                       tz2*( ue(kp1,4)-ue(km1,4) )+
                       dz1tz1*(ue(kp1,1)-2.0e0*ue(k,1)+ue(km1,1));

                  forcing(2,i,j,k,c) = forcing(2,i,j,k,c) - tz2 * (
                       ue(kp1,2)*buf(kp1,4)-ue(km1,2)*buf(km1,4))+
                       zzcon2*(buf(kp1,2)-2.0e0*buf(k,2)+buf(km1,2))+
                       dz2tz1*( ue(kp1,2)-2.0e0* ue(k,2)+ ue(km1,2));

                  forcing(3,i,j,k,c) = forcing(3,i,j,k,c) - tz2 * (
                       ue(kp1,3)*buf(kp1,4)-ue(km1,3)*buf(km1,4))+
                       zzcon2*(buf(kp1,3)-2.0e0*buf(k,3)+buf(km1,3))+
                       dz3tz1*(ue(kp1,3)-2.0e0*ue(k,3)+ue(km1,3));

                  forcing(4,i,j,k,c) = forcing(4,i,j,k,c) - tz2 * (
                       (ue(kp1,4)*buf(kp1,4)+c2*(ue(kp1,5)-q(kp1)))-
                       (ue(km1,4)*buf(km1,4)+c2*(ue(km1,5)-q(km1))))+
                       zzcon1*(buf(kp1,4)-2.0e0*buf(k,4)+buf(km1,4))+
                       dz4tz1*( ue(kp1,4)-2.0e0*ue(k,4) +ue(km1,4));

                  forcing(5,i,j,k,c) = forcing(5,i,j,k,c) - tz2 * (
                       buf(kp1,4)*(c1*ue(kp1,5)-c2*q(kp1))-
                       buf(km1,4)*(c1*ue(km1,5)-c2*q(km1)))+
                       0.5e0*zzcon3*(buf(kp1,1)-2.0e0*buf(k,1)
                       +buf(km1,1))+
                       zzcon4*(cuf(kp1)-2.0e0*cuf(k)+cuf(km1))+
                       zzcon5*(buf(kp1,5)-2.0e0*buf(k,5)+buf(km1,5))+
                       dz5tz1*( ue(kp1,5)-2.0e0*ue(k,5)+ ue(km1,5));
               }

//---------------------------------------------------------------------
//     Fourth-order dissipation                        
//---------------------------------------------------------------------
               if (start(3,c) > 0) {
                  for (m = 1; m <= 5; m++) {
                     k = 1;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (5.0e0*ue(k,m) - 4.0e0*ue(k+1,m) +ue(k+2,m));
                     k = 2;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (-4.0e0*ue(k-1,m) + 6.0e0*ue(k,m) -
                          4.0e0*ue(k+1,m) +       ue(k+2,m));
                  }
               }

               for (k = start(3,c)*3; k <= cell_size(3,c)-3*end(3,c)-1; k++) {
                  for (m = 1; m <= 5; m++) {
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp*
                          (ue(k-2,m) - 4.0e0*ue(k-1,m) +
                          6.0e0*ue(k,m) - 4.0e0*ue(k+1,m) + ue(k+2,m));
                  }
               }

               if (end(3,c) > 0) {
                  for (m = 1; m <= 5; m++) {
                     k = cell_size(3,c)-3;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (ue(k-2,m) - 4.0e0*ue(k-1,m) +
                          6.0e0*ue(k,m) - 4.0e0*ue(k+1,m));
                     k = cell_size(3,c)-2;
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *
                          (ue(k-2,m) - 4.0e0*ue(k-1,m) + 5.0e0*ue(k,m));
                  }
               }

            }
         }

//---------------------------------------------------------------------
//     now change the sign of the forcing function, 
//---------------------------------------------------------------------
         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     forcing(m,i,j,k,c) = -1.e0 * forcing(m,i,j,k,c);
                  }
               }
            }
         }

      }

      return;
}
