//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include "header.h"

void compute_rhs() {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

      int c, i, j, k, m;
      double rho_inv, uijk, up1, um1, vijk, vp1, vm1,
           wijk, wp1, wm1;


//---------------------------------------------------------------------
//     loop over all cells owned by this node                           
//---------------------------------------------------------------------
      for (c = 1; c <= ncells; c++) {

//---------------------------------------------------------------------
//     compute the reciprocal of density, and the kinetic energy, 
//     and the speed of sound.
//---------------------------------------------------------------------
         for (k = -1; k <= cell_size(3,c); k++) {
            for (j = -1; j <= cell_size(2,c); j++) {
               for (i = -1; i <= cell_size(1,c); i++) {
                  rho_inv = 1.0e0/u(1,i,j,k,c);
                  rho_i(i,j,k,c) = rho_inv;
                  us(i,j,k,c) = u(2,i,j,k,c) * rho_inv;
                  vs(i,j,k,c) = u(3,i,j,k,c) * rho_inv;
                  ws(i,j,k,c) = u(4,i,j,k,c) * rho_inv;
                  square(i,j,k,c)     = 0.5e0* (
                       u(2,i,j,k,c)*u(2,i,j,k,c) + 
                       u(3,i,j,k,c)*u(3,i,j,k,c) +
                       u(4,i,j,k,c)*u(4,i,j,k,c) ) * rho_inv;
                  qs(i,j,k,c) = square(i,j,k,c) * rho_inv;
               }
            }
         }

//---------------------------------------------------------------------
// copy the exact forcing term to the right hand side;  because 
// this forcing term is known, we can store it on the whole of every 
// cell,  including the boundary                   
//---------------------------------------------------------------------

         for (k = 0; k <= cell_size(3,c)-1; k++) {
            for (j = 0; j <= cell_size(2,c)-1; j++) {
               for (i = 0; i <= cell_size(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = forcing(m,i,j,k,c);
                  }
               }
            }
         }


//---------------------------------------------------------------------
//     compute xi-direction fluxes 
//---------------------------------------------------------------------
         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  uijk = us(i,j,k,c);
                  up1  = us(i+1,j,k,c);
                  um1  = us(i-1,j,k,c);

                  rhs(1,i,j,k,c) = rhs(1,i,j,k,c) + dx1tx1 * 
                       (u(1,i+1,j,k,c) - 2.0e0*u(1,i,j,k,c) + 
                       u(1,i-1,j,k,c)) -
                       tx2 * (u(2,i+1,j,k,c) - u(2,i-1,j,k,c));

                  rhs(2,i,j,k,c) = rhs(2,i,j,k,c) + dx2tx1 * 
                       (u(2,i+1,j,k,c) - 2.0e0*u(2,i,j,k,c) + 
                       u(2,i-1,j,k,c)) +
                       xxcon2*con43 * (up1 - 2.0e0*uijk + um1) -
                       tx2 * (u(2,i+1,j,k,c)*up1 - 
                       u(2,i-1,j,k,c)*um1 +
                       (u(5,i+1,j,k,c)- square(i+1,j,k,c)-
                       u(5,i-1,j,k,c)+ square(i-1,j,k,c))*
                       c2);

                  rhs(3,i,j,k,c) = rhs(3,i,j,k,c) + dx3tx1 * 
                       (u(3,i+1,j,k,c) - 2.0e0*u(3,i,j,k,c) +
                       u(3,i-1,j,k,c)) +
                       xxcon2 * (vs(i+1,j,k,c) - 2.0e0*vs(i,j,k,c) +
                       vs(i-1,j,k,c)) -
                       tx2 * (u(3,i+1,j,k,c)*up1 - 
                       u(3,i-1,j,k,c)*um1);

                  rhs(4,i,j,k,c) = rhs(4,i,j,k,c) + dx4tx1 * 
                       (u(4,i+1,j,k,c) - 2.0e0*u(4,i,j,k,c) +
                       u(4,i-1,j,k,c)) +
                       xxcon2 * (ws(i+1,j,k,c) - 2.0e0*ws(i,j,k,c) +
                       ws(i-1,j,k,c)) -
                       tx2 * (u(4,i+1,j,k,c)*up1 - 
                       u(4,i-1,j,k,c)*um1);

                  rhs(5,i,j,k,c) = rhs(5,i,j,k,c) + dx5tx1 * 
                       (u(5,i+1,j,k,c) - 2.0e0*u(5,i,j,k,c) +
                       u(5,i-1,j,k,c)) +
                       xxcon3 * (qs(i+1,j,k,c) - 2.0e0*qs(i,j,k,c) +
                       qs(i-1,j,k,c)) +
                       xxcon4 * (up1*up1 -       2.0e0*uijk*uijk + 
                       um1*um1) +
                       xxcon5 * (u(5,i+1,j,k,c)*rho_i(i+1,j,k,c) - 
                       2.0e0*u(5,i,j,k,c)*rho_i(i,j,k,c) +
                       u(5,i-1,j,k,c)*rho_i(i-1,j,k,c)) -
                       tx2 * ( (c1*u(5,i+1,j,k,c) - 
                       c2*square(i+1,j,k,c))*up1 -
                       (c1*u(5,i-1,j,k,c) - 
                       c2*square(i-1,j,k,c))*um1 );
               }
            }
         }

//---------------------------------------------------------------------
//     add fourth order xi-direction dissipation               
//---------------------------------------------------------------------
         if (start(1,c) > 0) {
            for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
               for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
                  i = 1;
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c)- dssp * 
                          ( 5.0e0*u(m,i,j,k,c) - 4.0e0*u(m,i+1,j,k,c) +
                          u(m,i+2,j,k,c));
                  }

                  i = 2;
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp * 
                          (-4.0e0*u(m,i-1,j,k,c) + 6.0e0*u(m,i,j,k,c) -
                          4.0e0*u(m,i+1,j,k,c) + u(m,i+2,j,k,c));
                  }
               }
            }
         }

         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = 3*start(1,c); i <= cell_size(1,c)-3*end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp * 
                          (  u(m,i-2,j,k,c) - 4.0e0*u(m,i-1,j,k,c) + 
                          6.0*u(m,i,j,k,c) - 4.0e0*u(m,i+1,j,k,c) + 
                          u(m,i+2,j,k,c) );
                  }
               }
            }
         }
         

         if (end(1,c) > 0) {
            for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
               for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
                  i = cell_size(1,c)-3;
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *
                          ( u(m,i-2,j,k,c) - 4.0e0*u(m,i-1,j,k,c) + 
                          6.0e0*u(m,i,j,k,c) - 4.0e0*u(m,i+1,j,k,c) );
                  }

                  i = cell_size(1,c)-2;
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *
                          ( u(m,i-2,j,k,c) - 4.e0*u(m,i-1,j,k,c) +
                          5.e0*u(m,i,j,k,c) );
                  }
               }
            }
         }

//---------------------------------------------------------------------
//     compute eta-direction fluxes 
//---------------------------------------------------------------------
         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  vijk = vs(i,j,k,c);
                  vp1  = vs(i,j+1,k,c);
                  vm1  = vs(i,j-1,k,c);
                  rhs(1,i,j,k,c) = rhs(1,i,j,k,c) + dy1ty1 * 
                       (u(1,i,j+1,k,c) - 2.0e0*u(1,i,j,k,c) + 
                       u(1,i,j-1,k,c)) -
                       ty2 * (u(3,i,j+1,k,c) - u(3,i,j-1,k,c));
                  rhs(2,i,j,k,c) = rhs(2,i,j,k,c) + dy2ty1 * 
                       (u(2,i,j+1,k,c) - 2.0e0*u(2,i,j,k,c) + 
                       u(2,i,j-1,k,c)) +
                       yycon2 * (us(i,j+1,k,c) - 2.0e0*us(i,j,k,c) + 
                       us(i,j-1,k,c)) -
                       ty2 * (u(2,i,j+1,k,c)*vp1 - 
                       u(2,i,j-1,k,c)*vm1);
                  rhs(3,i,j,k,c) = rhs(3,i,j,k,c) + dy3ty1 * 
                       (u(3,i,j+1,k,c) - 2.0e0*u(3,i,j,k,c) + 
                       u(3,i,j-1,k,c)) +
                       yycon2*con43 * (vp1 - 2.0e0*vijk + vm1) -
                       ty2 * (u(3,i,j+1,k,c)*vp1 - 
                       u(3,i,j-1,k,c)*vm1 +
                       (u(5,i,j+1,k,c) - square(i,j+1,k,c) - 
                       u(5,i,j-1,k,c) + square(i,j-1,k,c))
                       *c2);
                  rhs(4,i,j,k,c) = rhs(4,i,j,k,c) + dy4ty1 * 
                       (u(4,i,j+1,k,c) - 2.0e0*u(4,i,j,k,c) + 
                       u(4,i,j-1,k,c)) +
                       yycon2 * (ws(i,j+1,k,c) - 2.0e0*ws(i,j,k,c) + 
                       ws(i,j-1,k,c)) -
                       ty2 * (u(4,i,j+1,k,c)*vp1 - 
                       u(4,i,j-1,k,c)*vm1);
                  rhs(5,i,j,k,c) = rhs(5,i,j,k,c) + dy5ty1 * 
                       (u(5,i,j+1,k,c) - 2.0e0*u(5,i,j,k,c) + 
                       u(5,i,j-1,k,c)) +
                       yycon3 * (qs(i,j+1,k,c) - 2.0e0*qs(i,j,k,c) + 
                       qs(i,j-1,k,c)) +
                       yycon4 * (vp1*vp1       - 2.0e0*vijk*vijk + 
                       vm1*vm1) +
                       yycon5 * (u(5,i,j+1,k,c)*rho_i(i,j+1,k,c) - 
                       2.0e0*u(5,i,j,k,c)*rho_i(i,j,k,c) +
                       u(5,i,j-1,k,c)*rho_i(i,j-1,k,c)) -
                       ty2 * ((c1*u(5,i,j+1,k,c) - 
                       c2*square(i,j+1,k,c)) * vp1 -
                       (c1*u(5,i,j-1,k,c) - 
                       c2*square(i,j-1,k,c)) * vm1);
               }
            }
         }

//---------------------------------------------------------------------
//     add fourth order eta-direction dissipation         
//---------------------------------------------------------------------
         if (start(2,c) > 0) {
            for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
               j = 1;
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c)- dssp * 
                          ( 5.0e0*u(m,i,j,k,c) - 4.0e0*u(m,i,j+1,k,c) +
                          u(m,i,j+2,k,c));
                  }
               }

               j = 2;
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp * 
                          (-4.0e0*u(m,i,j-1,k,c) + 6.0e0*u(m,i,j,k,c) -
                          4.0e0*u(m,i,j+1,k,c) + u(m,i,j+2,k,c));
                  }
               }
            }
         }

         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            for (j = 3*start(2,c); j <= cell_size(2,c)-3*end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp * 
                          (  u(m,i,j-2,k,c) - 4.0e0*u(m,i,j-1,k,c) + 
                          6.0*u(m,i,j,k,c) - 4.0e0*u(m,i,j+1,k,c) + 
                          u(m,i,j+2,k,c) );
                  }
               }
            }
         }
         
         if (end(2,c) > 0) {
            for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
               j = cell_size(2,c)-3;
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *
                          ( u(m,i,j-2,k,c) - 4.0e0*u(m,i,j-1,k,c) + 
                          6.0e0*u(m,i,j,k,c) - 4.0e0*u(m,i,j+1,k,c) );
                  }
               }

               j = cell_size(2,c)-2;
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *
                          ( u(m,i,j-2,k,c) - 4.e0*u(m,i,j-1,k,c) +
                          5.e0*u(m,i,j,k,c) );
                  }
               }
            }
         }

//---------------------------------------------------------------------
//     compute zeta-direction fluxes 
//---------------------------------------------------------------------
         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  wijk = ws(i,j,k,c);
                  wp1  = ws(i,j,k+1,c);
                  wm1  = ws(i,j,k-1,c);

                  rhs(1,i,j,k,c) = rhs(1,i,j,k,c) + dz1tz1 * 
                       (u(1,i,j,k+1,c) - 2.0e0*u(1,i,j,k,c) + 
                       u(1,i,j,k-1,c)) -
                       tz2 * (u(4,i,j,k+1,c) - u(4,i,j,k-1,c));
                  rhs(2,i,j,k,c) = rhs(2,i,j,k,c) + dz2tz1 * 
                       (u(2,i,j,k+1,c) - 2.0e0*u(2,i,j,k,c) + 
                       u(2,i,j,k-1,c)) +
                       zzcon2 * (us(i,j,k+1,c) - 2.0e0*us(i,j,k,c) + 
                       us(i,j,k-1,c)) -
                       tz2 * (u(2,i,j,k+1,c)*wp1 - 
                       u(2,i,j,k-1,c)*wm1);
                  rhs(3,i,j,k,c) = rhs(3,i,j,k,c) + dz3tz1 * 
                       (u(3,i,j,k+1,c) - 2.0e0*u(3,i,j,k,c) + 
                       u(3,i,j,k-1,c)) +
                       zzcon2 * (vs(i,j,k+1,c) - 2.0e0*vs(i,j,k,c) + 
                       vs(i,j,k-1,c)) -
                       tz2 * (u(3,i,j,k+1,c)*wp1 - 
                       u(3,i,j,k-1,c)*wm1);
                  rhs(4,i,j,k,c) = rhs(4,i,j,k,c) + dz4tz1 * 
                       (u(4,i,j,k+1,c) - 2.0e0*u(4,i,j,k,c) + 
                       u(4,i,j,k-1,c)) +
                       zzcon2*con43 * (wp1 - 2.0e0*wijk + wm1) -
                       tz2 * (u(4,i,j,k+1,c)*wp1 - 
                       u(4,i,j,k-1,c)*wm1 +
                       (u(5,i,j,k+1,c) - square(i,j,k+1,c) - 
                       u(5,i,j,k-1,c) + square(i,j,k-1,c))
                       *c2);
                  rhs(5,i,j,k,c) = rhs(5,i,j,k,c) + dz5tz1 * 
                       (u(5,i,j,k+1,c) - 2.0e0*u(5,i,j,k,c) + 
                       u(5,i,j,k-1,c)) +
                       zzcon3 * (qs(i,j,k+1,c) - 2.0e0*qs(i,j,k,c) + 
                       qs(i,j,k-1,c)) +
                       zzcon4 * (wp1*wp1 - 2.0e0*wijk*wijk + 
                       wm1*wm1) +
                       zzcon5 * (u(5,i,j,k+1,c)*rho_i(i,j,k+1,c) - 
                       2.0e0*u(5,i,j,k,c)*rho_i(i,j,k,c) +
                       u(5,i,j,k-1,c)*rho_i(i,j,k-1,c)) -
                       tz2 * ( (c1*u(5,i,j,k+1,c) - 
                       c2*square(i,j,k+1,c))*wp1 -
                       (c1*u(5,i,j,k-1,c) - 
                       c2*square(i,j,k-1,c))*wm1);
               }
            }
         }

//---------------------------------------------------------------------
//     add fourth order zeta-direction dissipation                
//---------------------------------------------------------------------
         if (start(3,c) > 0) {
            k = 1;
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c)- dssp * 
                          ( 5.0e0*u(m,i,j,k,c) - 4.0e0*u(m,i,j,k+1,c) +
                          u(m,i,j,k+2,c));
                  }
               }
            }

            k = 2;
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp * 
                          (-4.0e0*u(m,i,j,k-1,c) + 6.0e0*u(m,i,j,k,c) -
                          4.0e0*u(m,i,j,k+1,c) + u(m,i,j,k+2,c));
                  }
               }
            }
         }

         for (k = 3*start(3,c); k <= cell_size(3,c)-3*end(3,c)-1; k++) {
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp * 
                          (  u(m,i,j,k-2,c) - 4.0e0*u(m,i,j,k-1,c) + 
                          6.0*u(m,i,j,k,c) - 4.0e0*u(m,i,j,k+1,c) + 
                          u(m,i,j,k+2,c) );
                  }
               }
            }
         }
         
         if (end(3,c) > 0) {
            k = cell_size(3,c)-3;
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *
                          ( u(m,i,j,k-2,c) - 4.0e0*u(m,i,j,k-1,c) + 
                          6.0e0*u(m,i,j,k,c) - 4.0e0*u(m,i,j,k+1,c) );
                  }
               }
            }

            k = cell_size(3,c)-2;
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *
                          ( u(m,i,j,k-2,c) - 4.e0*u(m,i,j,k-1,c) +
                          5.e0*u(m,i,j,k,c) );
                  }
               }
            }
         }

         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) * dt;
                  }
               }
            }
         }

      }
      
      return;
}




