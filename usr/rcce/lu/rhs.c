#include "applu_share.h"
#include "applu_macros.h"

void rhs(){


//c   compute the right hand sides

//c---------------------------------------------------------------------
//c  local variables
//c---------------------------------------------------------------------
      int i, j, k, m;
      int iex;
      int L1, L2;
      int ist1, iend1;
      int jst1, jend1;
      double  q;
      double  u21, u31, u41;
      double  tmp;
      double  u21i, u31i, u41i, u51i;
      double  u21j, u31j, u41j, u51j;
      double  u21k, u31k, u41k, u51k;
      double  u21im1, u31im1, u41im1, u51im1;
      double  u21jm1, u31jm1, u41jm1, u51jm1;
      double  u21km1, u31km1, u41km1, u51km1;

      for (k=1; k<=nz; k++)
         for (j=1; j<=ny; j++)
            for (i=1; i<=nx; i++)
               for (m=1; m<=5; m++)
                  rsd(m,i,j,k) = - frct(m,i,j,k);

//c---------------------------------------------------------------------
//c   xi-direction flux differences
//c---------------------------------------------------------------------
//c
//c---------------------------------------------------------------------
//c   iex = flag : iex = 0  north/south communication
//c              : iex = 1  east/west communication
//c---------------------------------------------------------------------

      iex   = 0;

//c---------------------------------------------------------------------
//c   communicate and receive/send two rows of data
//c---------------------------------------------------------------------
      exchange_3(u,iex);

      L1 = 0;
      if (north == -1) L1 = 1;
      L2 = nx + 1;
      if (south == -1) L2 = nx;

      ist1 = 1;
      iend1 = nx;
      if (north == -1) ist1 = 4;
      if (south == -1) iend1 = nx - 3;

      for (k=2; k<=nz-1; k++) {
         for (j=jst; j<=jend; j++) {
            for (i=L1; i<=L2; i++) {
               flux(1,i,j,k) = u(2,i,j,k);
               u21 = u(2,i,j,k) / u(1,i,j,k);

               q = 0.50 * (  u(2,i,j,k) * u(2,i,j,k)
                               + u(3,i,j,k) * u(3,i,j,k)
                               + u(4,i,j,k) * u(4,i,j,k) )
                            / u(1,i,j,k);

               flux(2,i,j,k) = u(2,i,j,k) * u21 + c2 * 
                              ( u(5,i,j,k) - q );
               flux(3,i,j,k) = u(3,i,j,k) * u21;
               flux(4,i,j,k) = u(4,i,j,k) * u21;
               flux(5,i,j,k) = ( c1 * u(5,i,j,k) - c2 * q ) * u21;
            }

            for (i=ist; i<=iend; i++) {
               for (m=1; m<=5; m++) {
                  rsd(m,i,j,k) =  rsd(m,i,j,k)
                       - tx2 * ( flux(m,i+1,j,k) - flux(m,i-1,j,k) );
               }
            }

            for (i=ist; i<=L2; i++) {
               tmp = 1.0 / u(1,i,j,k);

               u21i = tmp * u(2,i,j,k);
               u31i = tmp * u(3,i,j,k);
               u41i = tmp * u(4,i,j,k);
               u51i = tmp * u(5,i,j,k);

               tmp = 1.0 / u(1,i-1,j,k);

               u21im1 = tmp * u(2,i-1,j,k);
               u31im1 = tmp * u(3,i-1,j,k);
               u41im1 = tmp * u(4,i-1,j,k);
               u51im1 = tmp * u(5,i-1,j,k);

               flux(2,i,j,k) = (4.0/3.0) * tx3 * (u21i-u21im1);
               flux(3,i,j,k) = tx3 * ( u31i - u31im1 );
               flux(4,i,j,k) = tx3 * ( u41i - u41im1 );
               flux(5,i,j,k) = 0.50 * ( 1.0 - c1*c5 )
                    * tx3 * ( ( u21i  *u21i + u31i  *u31i + u41i  *u41i )
                            - ( u21im1*u21im1 + u31im1*u31im1 + u41im1*u41im1 ) )
                    + (1.0/6.0)
                    * tx3 * ( u21i*u21i - u21im1*u21im1 )
                    + c1 * c5 * tx3 * ( u51i - u51im1 );
            }

            for (i=ist; i<=iend; i++) {
               rsd(1,i,j,k) = rsd(1,i,j,k)
                    + dx1 * tx1 * (            u(1,i-1,j,k)
                                   - 2.0 * u(1,i,j,k)
                                   +           u(1,i+1,j,k) );
               rsd(2,i,j,k) = rsd(2,i,j,k)
                + tx3 * c3 * c4 * ( flux(2,i+1,j,k) - flux(2,i,j,k) )
                    + dx2 * tx1 * (            u(2,i-1,j,k)
                                   - 2.0 * u(2,i,j,k)
                                   +           u(2,i+1,j,k) );
               rsd(3,i,j,k) = rsd(3,i,j,k)
                + tx3 * c3 * c4 * ( flux(3,i+1,j,k) - flux(3,i,j,k) )
                    + dx3 * tx1 * (            u(3,i-1,j,k)
                                   - 2.0 * u(3,i,j,k)
                                   +           u(3,i+1,j,k) );
               rsd(4,i,j,k) = rsd(4,i,j,k)
                + tx3 * c3 * c4 * ( flux(4,i+1,j,k) - flux(4,i,j,k) )
                    + dx4 * tx1 * (            u(4,i-1,j,k)
                                   - 2.0 * u(4,i,j,k)
                                   +           u(4,i+1,j,k) );
               rsd(5,i,j,k) = rsd(5,i,j,k)
                + tx3 * c3 * c4 * ( flux(5,i+1,j,k) - flux(5,i,j,k) )
                    + dx5 * tx1 * (            u(5,i-1,j,k)
                                   - 2.0 * u(5,i,j,k)
                                   +           u(5,i+1,j,k) );
            }

//c---------------------------------------------------------------------
//c   Fourth-order dissipation
//c---------------------------------------------------------------------
            if (north == -1) {
             for (m=1; m<=5; m++) {
               rsd(m,2,j,k) = rsd(m,2,j,k)
                 - dssp * ( + 5.0 * u(m,2,j,k)
                            - 4.0 * u(m,3,j,k)
                            +           u(m,4,j,k) );
               rsd(m,3,j,k) = rsd(m,3,j,k)
                 - dssp * ( - 4.0 * u(m,2,j,k)
                            + 6.0 * u(m,3,j,k)
                            - 4.0 * u(m,4,j,k)
                            +           u(m,5,j,k) );
             }
            }

            for (i=ist1; i<=iend1; i++) {
               for (m=1; m<=5; m++) {
                  rsd(m,i,j,k) = rsd(m,i,j,k)
                    - dssp * (            u(m,i-2,j,k)
                              - 4.0 * u(m,i-1,j,k)
                              + 6.0 * u(m,i,j,k)
                              - 4.0 * u(m,i+1,j,k)
                              +           u(m,i+2,j,k) );
               }
            }

            if (south == -1) {
             for (m=1; m<=5; m++) {
               rsd(m,nx-2,j,k) = rsd(m,nx-2,j,k)
                 - dssp * (             u(m,nx-4,j,k)
                            - 4.0 * u(m,nx-3,j,k)
                            + 6.0 * u(m,nx-2,j,k)
                            - 4.0 * u(m,nx-1,j,k)  );
               rsd(m,nx-1,j,k) = rsd(m,nx-1,j,k)
                 - dssp * (             u(m,nx-3,j,k)
                            - 4.0 * u(m,nx-2,j,k)
                            + 5.0 * u(m,nx-1,j,k) );
             }
            }
         }
         
      } 

//c---------------------------------------------------------------------
//c   eta-direction flux differences
//c---------------------------------------------------------------------
//
//c---------------------------------------------------------------------
//c   iex = flag : iex = 0  north/south communication
//c---------------------------------------------------------------------
      iex   = 1;

//c---------------------------------------------------------------------
//c   communicate and receive/send two rows of data
//c---------------------------------------------------------------------
      exchange_3(u,iex);

      L1 = 0;
      if (west == -1) L1 = 1;
      L2 = ny + 1;
      if (east == -1) L2 = ny;

      jst1 = 1;
      jend1 = ny;
      if (west == -1) jst1 = 4;
      if (east == -1) jend1 = ny - 3;

      for (k=2; k<=nz-1; k++) {
         for (j=L1; j<=L2; j++) {
            for (i=ist; i<=iend; i++) {
               flux(1,i,j,k) = u(3,i,j,k);
               u31 = u(3,i,j,k) / u(1,i,j,k);

               q = 0.50 * (  u(2,i,j,k) * u(2,i,j,k)
                               + u(3,i,j,k) * u(3,i,j,k)
                               + u(4,i,j,k) * u(4,i,j,k) )
                            / u(1,i,j,k);

               flux(2,i,j,k) = u(2,i,j,k) * u31 ;
               flux(3,i,j,k) = u(3,i,j,k) * u31 + c2 * (u(5,i,j,k)-q);
               flux(4,i,j,k) = u(4,i,j,k) * u31;
               flux(5,i,j,k) = ( c1 * u(5,i,j,k) - c2 * q ) * u31;
            }
         }

         for (j=jst; j<=jend; j++) {
            for (i=ist; i<=iend; i++) {
               for (m=1; m<=5; m++) {
                  rsd(m,i,j,k) =  rsd(m,i,j,k)
                         - ty2 * ( flux(m,i,j+1,k) - flux(m,i,j-1,k) );
               }
            }
         }

         for (j=jst; j<=L2; j++) {
            for (i=ist; i<=iend; i++) {
               tmp = 1.0 / u(1,i,j,k);

               u21j = tmp * u(2,i,j,k);
               u31j = tmp * u(3,i,j,k);
               u41j = tmp * u(4,i,j,k);
               u51j = tmp * u(5,i,j,k);

               tmp = 1.0 / u(1,i,j-1,k);
               u21jm1 = tmp * u(2,i,j-1,k);
               u31jm1 = tmp * u(3,i,j-1,k);
               u41jm1 = tmp * u(4,i,j-1,k);
               u51jm1 = tmp * u(5,i,j-1,k);

               flux(2,i,j,k) = ty3 * ( u21j - u21jm1 );
               flux(3,i,j,k) = (4.0/3.0) * ty3 * (u31j-u31jm1);
               flux(4,i,j,k) = ty3 * ( u41j - u41jm1 );
               flux(5,i,j,k) = 0.50 * ( 1.0 - c1*c5 )
                    * ty3 * ( ( u21j  *u21j + u31j  *u31j + u41j  *u41j )
                            - ( u21jm1*u21jm1 + u31jm1*u31jm1 + u41jm1*u41jm1 ) )
                    + (1.0/6.0)
                    * ty3 * ( u31j*u31j - u31jm1*u31jm1 )
                    + c1 * c5 * ty3 * ( u51j - u51jm1 );
            }
         }

         for (j=jst; j<=jend; j++) {
            for (i=ist; i<=iend; i++) {

               rsd(1,i,j,k) = rsd(1,i,j,k)
                    + dy1 * ty1 * (            u(1,i,j-1,k)
                                   - 2.0 * u(1,i,j,k)
                                   +           u(1,i,j+1,k) );

               rsd(2,i,j,k) = rsd(2,i,j,k)
                + ty3 * c3 * c4 * ( flux(2,i,j+1,k) - flux(2,i,j,k) )
                    + dy2 * ty1 * (            u(2,i,j-1,k)
                                   - 2.0 * u(2,i,j,k)
                                   +           u(2,i,j+1,k) );

               rsd(3,i,j,k) = rsd(3,i,j,k)
                + ty3 * c3 * c4 * ( flux(3,i,j+1,k) - flux(3,i,j,k) )
                    + dy3 * ty1 * (            u(3,i,j-1,k)
                                   - 2.0 * u(3,i,j,k)
                                   +           u(3,i,j+1,k) );

               rsd(4,i,j,k) = rsd(4,i,j,k)
                + ty3 * c3 * c4 * ( flux(4,i,j+1,k) - flux(4,i,j,k) )
                    + dy4 * ty1 * (            u(4,i,j-1,k)
                                   - 2.0 * u(4,i,j,k)
                                   +           u(4,i,j+1,k) );

               rsd(5,i,j,k) = rsd(5,i,j,k)
                + ty3 * c3 * c4 * ( flux(5,i,j+1,k) - flux(5,i,j,k) )
                    + dy5 * ty1 * (            u(5,i,j-1,k)
                                   - 2.0 * u(5,i,j,k)
                                   +           u(5,i,j+1,k) );

           }
         }

//c---------------------------------------------------------------------
//c   fourth-order dissipation
//c---------------------------------------------------------------------
         if (west == -1) {
            for (i=ist; i<=iend; i++) {
             for (m=1; m<=5; m++) {
               rsd(m,i,2,k) = rsd(m,i,2,k)
                 - dssp * ( + 5.0 * u(m,i,2,k)
                            - 4.0 * u(m,i,3,k)
                            +           u(m,i,4,k) );
               rsd(m,i,3,k) = rsd(m,i,3,k)
                 - dssp * ( - 4.0 * u(m,i,2,k)
                            + 6.0 * u(m,i,3,k)
                            - 4.0 * u(m,i,4,k)
                            +           u(m,i,5,k) );
             }
            }
         }

         for (j=jst1; j<=jend1; j++) {
            for (i=ist; i<=iend; i++) {
               for (m=1; m<=5; m++) {
                  rsd(m,i,j,k) = rsd(m,i,j,k)
                    - dssp * (            u(m,i,j-2,k)
                              - 4.0 * u(m,i,j-1,k)
                              + 6.0 * u(m,i,j,k)
                              - 4.0 * u(m,i,j+1,k)
                              +           u(m,i,j+2,k) );
               }
            }
         }

         if (east == -1) {
            for (i=ist; i<=iend; i++) {
             for (m=1; m<=5; m++) {
               rsd(m,i,ny-2,k) = rsd(m,i,ny-2,k)
                 - dssp * (             u(m,i,ny-4,k)
                            - 4.0 * u(m,i,ny-3,k)
                            + 6.0 * u(m,i,ny-2,k)
                            - 4.0 * u(m,i,ny-1,k)  );
               rsd(m,i,ny-1,k) = rsd(m,i,ny-1,k)
                 - dssp * (             u(m,i,ny-3,k)
                            - 4.0 * u(m,i,ny-2,k)
                            + 5.0 * u(m,i,ny-1,k) );
             }
            }
         }

      }

//c---------------------------------------------------------------------
//c   zeta-direction flux differences
//c---------------------------------------------------------------------
      for (k=1; k<=nz; k++) {
         for (j=jst; j<=jend; j++) {
            for (i=ist; i<=iend; i++) {
               flux(1,i,j,k) = u(4,i,j,k);
               u41 = u(4,i,j,k) / u(1,i,j,k);

               q = 0.50 * (  u(2,i,j,k) * u(2,i,j,k)
                               + u(3,i,j,k) * u(3,i,j,k)
                               + u(4,i,j,k) * u(4,i,j,k) )
                            / u(1,i,j,k);

               flux(2,i,j,k) = u(2,i,j,k) * u41 ;
               flux(3,i,j,k) = u(3,i,j,k) * u41 ;
               flux(4,i,j,k) = u(4,i,j,k) * u41 + c2 * (u(5,i,j,k)-q);
               flux(5,i,j,k) = ( c1 * u(5,i,j,k) - c2 * q ) * u41;
            }
         }
      }

      for (k=2; k<=nz-1; k++) {
         for (j=jst; j<=jend; j++) {
            for (i=ist; i<=iend; i++) {
               for (m=1; m<=5; m++) {
                  rsd(m,i,j,k) =  rsd(m,i,j,k)
                      - tz2 * ( flux(m,i,j,k+1) - flux(m,i,j,k-1) );
               }
            }
         }
      }

      for (k=2; k<=nz; k++) {
         for (j=jst; j<=jend; j++) {
            for (i=ist; i<=iend; i++) {
               tmp = 1.0 / u(1,i,j,k);

               u21k = tmp * u(2,i,j,k);
               u31k = tmp * u(3,i,j,k);
               u41k = tmp * u(4,i,j,k);
               u51k = tmp * u(5,i,j,k);

               tmp = 1.0 / u(1,i,j,k-1);

               u21km1 = tmp * u(2,i,j,k-1);
               u31km1 = tmp * u(3,i,j,k-1);
               u41km1 = tmp * u(4,i,j,k-1);
               u51km1 = tmp * u(5,i,j,k-1);

               flux(2,i,j,k) = tz3 * ( u21k - u21km1 );
               flux(3,i,j,k) = tz3 * ( u31k - u31km1 );
               flux(4,i,j,k) = (4.0/3.0) * tz3 * (u41k-u41km1);
               flux(5,i,j,k) = 0.50 * ( 1.0 - c1*c5 )
                    * tz3 * ( ( u21k  *u21k + u31k  *u31k + u41k  *u41k )
                            - ( u21km1*u21km1 + u31km1*u31km1 + u41km1*u41km1 ) )
                    + (1.0/6.0)
                    * tz3 * ( u41k*u41k - u41km1*u41km1 )
                    + c1 * c5 * tz3 * ( u51k - u51km1 );
            }
         }
      }

      for (k=2; k<=nz-1; k++) {
         for (j=jst; j<=jend; j++) {
            for (i=ist; i<=iend; i++) {
               rsd(1,i,j,k) = rsd(1,i,j,k)
                    + dz1 * tz1 * (            u(1,i,j,k-1)
                                   - 2.0 * u(1,i,j,k)
                                   +           u(1,i,j,k+1) );
               rsd(2,i,j,k) = rsd(2,i,j,k)
                + tz3 * c3 * c4 * ( flux(2,i,j,k+1) - flux(2,i,j,k) )
                    + dz2 * tz1 * (            u(2,i,j,k-1)
                                   - 2.0 * u(2,i,j,k)
                                   +           u(2,i,j,k+1) );
               rsd(3,i,j,k) = rsd(3,i,j,k)
                + tz3 * c3 * c4 * ( flux(3,i,j,k+1) - flux(3,i,j,k) )
                    + dz3 * tz1 * (            u(3,i,j,k-1)
                                   - 2.0 * u(3,i,j,k)
                                   +           u(3,i,j,k+1) );
               rsd(4,i,j,k) = rsd(4,i,j,k)
                + tz3 * c3 * c4 * ( flux(4,i,j,k+1) - flux(4,i,j,k) )
                    + dz4 * tz1 * (            u(4,i,j,k-1)
                                   - 2.0 * u(4,i,j,k)
                                   +           u(4,i,j,k+1) );
               rsd(5,i,j,k) = rsd(5,i,j,k)
                + tz3 * c3 * c4 * ( flux(5,i,j,k+1) - flux(5,i,j,k) )
                    + dz5 * tz1 * (            u(5,i,j,k-1)
                                   - 2.0 * u(5,i,j,k)
                                   +           u(5,i,j,k+1) );
            }
         }
      }

//c---------------------------------------------------------------------
//c   fourth-order dissipation
//c---------------------------------------------------------------------
      for (j=jst; j<=jend; j++) {
         for (i=ist; i<=iend; i++) {
            for (m=1; m<=5; m++) {
               rsd(m,i,j,2) = rsd(m,i,j,2)
                 - dssp * ( + 5.0 * u(m,i,j,2)
                            - 4.0 * u(m,i,j,3)
                            +           u(m,i,j,4) );
               rsd(m,i,j,3) = rsd(m,i,j,3)
                 - dssp * ( - 4.0 * u(m,i,j,2)
                            + 6.0 * u(m,i,j,3)
                            - 4.0 * u(m,i,j,4)
                            +           u(m,i,j,5) );
            }
         }
      }

      for (k=4; k<=nz-3; k++) {
         for (j=jst; j<=jend; j++) {
            for (i=ist; i<=iend; i++) {
               for (m=1; m<=5; m++) {
                  rsd(m,i,j,k) = rsd(m,i,j,k)
                    - dssp * (            u(m,i,j,k-2)
                              - 4.0 * u(m,i,j,k-1)
                              + 6.0 * u(m,i,j,k)
                              - 4.0 * u(m,i,j,k+1)
                              +           u(m,i,j,k+2) );
               }
            }
         }
      }

      for (j=jst; j<=jend; j++) {
         for (i=ist; i<=iend; i++) {
            for (m=1; m<=5; m++) {
               rsd(m,i,j,nz-2) = rsd(m,i,j,nz-2)
                 - dssp * (             u(m,i,j,nz-4)
                            - 4.0 * u(m,i,j,nz-3)
                            + 6.0 * u(m,i,j,nz-2)
                            - 4.0 * u(m,i,j,nz-1)  );
               rsd(m,i,j,nz-1) = rsd(m,i,j,nz-1)
                 - dssp * (             u(m,i,j,nz-3)
                            - 4.0 * u(m,i,j,nz-2)
                            + 5.0 * u(m,i,j,nz-1) );
            }
         }
      }

      return;
}

