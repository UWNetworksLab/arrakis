#include "applu_share.h"
#include "applu_macros.h"

void buts(int k, double *tv) {

//c   compute the regular-sparse, block upper triangular solution:


//c  local variables

      int i, j, m;
      int iex;
      double tmp, tmp1;
      double tmat[5*5];

//c   receive data from south and east

      iex = 1;
      exchange_1(rsd, k, iex);

      for (j=jend; j>=jst; j--) {
         for (i=iend; i>=ist; i--) {
            for (m=1; m<=5; m++) {
                  tv( m, i, j ) = 
            omega * (  c( m, 1, i, j ) * rsd( 1, i, j, k+1 )
                     + c( m, 2, i, j ) * rsd( 2, i, j, k+1 )
                     + c( m, 3, i, j ) * rsd( 3, i, j, k+1 )
                     + c( m, 4, i, j ) * rsd( 4, i, j, k+1 )
                     + c( m, 5, i, j ) * rsd( 5, i, j, k+1 ) );
            }
         }
      }

      for (j=jend; j>=jst; j--) {
        for (i=iend; i>=ist; i--) {

            for (m=1; m<=5; m++) {
                  tv( m, i, j ) = tv( m, i, j )
       + omega * ( b( m, 1, i, j ) * rsd( 1, i, j+1, k )
                 + a( m, 1, i, j ) * rsd( 1, i+1, j, k )
                 + b( m, 2, i, j ) * rsd( 2, i, j+1, k )
                 + a( m, 2, i, j ) * rsd( 2, i+1, j, k )
                 + b( m, 3, i, j ) * rsd( 3, i, j+1, k )
                 + a( m, 3, i, j ) * rsd( 3, i+1, j, k )
                 + b( m, 4, i, j ) * rsd( 4, i, j+1, k )
                 + a( m, 4, i, j ) * rsd( 4, i+1, j, k )
                 + b( m, 5, i, j ) * rsd( 5, i, j+1, k )
                 + a( m, 5, i, j ) * rsd( 5, i+1, j, k ) );
            }

//c---------------------------------------------------------------------
//c   diagonal block inversion
//c---------------------------------------------------------------------
            for (m=1; m<=5; m++) {
               tmat( m, 1 ) = d( m, 1, i, j );
               tmat( m, 2 ) = d( m, 2, i, j );
               tmat( m, 3 ) = d( m, 3, i, j );
               tmat( m, 4 ) = d( m, 4, i, j );
               tmat( m, 5 ) = d( m, 5, i, j );
            }

            tmp1 = 1.0 / tmat( 1, 1 );
            tmp = tmp1 * tmat( 2, 1 );
            tmat( 2, 2 ) =  tmat( 2, 2 )
                 - tmp * tmat( 1, 2 );
            tmat( 2, 3 ) =  tmat( 2, 3 )
                 - tmp * tmat( 1, 3 );
            tmat( 2, 4 ) =  tmat( 2, 4 )
                 - tmp * tmat( 1, 4 );
            tmat( 2, 5 ) =  tmat( 2, 5 )
                 - tmp * tmat( 1, 5 );
            tv( 2, i, j ) = tv( 2, i, j )
              - tv( 1, i, j ) * tmp;

            tmp = tmp1 * tmat( 3, 1 );
            tmat( 3, 2 ) =  tmat( 3, 2 )
                 - tmp * tmat( 1, 2 );
            tmat( 3, 3 ) =  tmat( 3, 3 )
                 - tmp * tmat( 1, 3 );
            tmat( 3, 4 ) =  tmat( 3, 4 )
                 - tmp * tmat( 1, 4 );
            tmat( 3, 5 ) =  tmat( 3, 5 )
                 - tmp * tmat( 1, 5 );
            tv( 3, i, j ) = tv( 3, i, j )
              - tv( 1, i, j ) * tmp;

            tmp = tmp1 * tmat( 4, 1 );
            tmat( 4, 2 ) =  tmat( 4, 2 )
                 - tmp * tmat( 1, 2 );
            tmat( 4, 3 ) =  tmat( 4, 3 )
                 - tmp * tmat( 1, 3 );
            tmat( 4, 4 ) =  tmat( 4, 4 )
                 - tmp * tmat( 1, 4 );
            tmat( 4, 5 ) =  tmat( 4, 5 )
                 - tmp * tmat( 1, 5 );
            tv( 4, i, j ) = tv( 4, i, j )
              - tv( 1, i, j ) * tmp;

            tmp = tmp1 * tmat( 5, 1 );
            tmat( 5, 2 ) =  tmat( 5, 2 )
                 - tmp * tmat( 1, 2 );
            tmat( 5, 3 ) =  tmat( 5, 3 )
                 - tmp * tmat( 1, 3 );
            tmat( 5, 4 ) =  tmat( 5, 4 )
                 - tmp * tmat( 1, 4 );
            tmat( 5, 5 ) =  tmat( 5, 5 )
                 - tmp * tmat( 1, 5 );
            tv( 5, i, j ) = tv( 5, i, j )
              - tv( 1, i, j ) * tmp;

            tmp1 = 1.0 / tmat( 2, 2 );
            tmp = tmp1 * tmat( 3, 2 );
            tmat( 3, 3 ) =  tmat( 3, 3 )
                 - tmp * tmat( 2, 3 );
            tmat( 3, 4 ) =  tmat( 3, 4 )
                 - tmp * tmat( 2, 4 );
            tmat( 3, 5 ) =  tmat( 3, 5 )
                 - tmp * tmat( 2, 5 );
            tv( 3, i, j ) = tv( 3, i, j )
              - tv( 2, i, j ) * tmp;

            tmp = tmp1 * tmat( 4, 2 );
            tmat( 4, 3 ) =  tmat( 4, 3 )
                 - tmp * tmat( 2, 3 );
            tmat( 4, 4 ) =  tmat( 4, 4 )
                 - tmp * tmat( 2, 4 );
            tmat( 4, 5 ) =  tmat( 4, 5 )
                 - tmp * tmat( 2, 5 );
            tv( 4, i, j ) = tv( 4, i, j )
              - tv( 2, i, j ) * tmp;

            tmp = tmp1 * tmat( 5, 2 );
            tmat( 5, 3 ) =  tmat( 5, 3 )
                 - tmp * tmat( 2, 3 );
            tmat( 5, 4 ) =  tmat( 5, 4 )
                 - tmp * tmat( 2, 4 );
            tmat( 5, 5 ) =  tmat( 5, 5 )
                 - tmp * tmat( 2, 5 );
            tv( 5, i, j ) = tv( 5, i, j )
              - tv( 2, i, j ) * tmp;

            tmp1 = 1.0 / tmat( 3, 3 );
            tmp = tmp1 * tmat( 4, 3 );
            tmat( 4, 4 ) =  tmat( 4, 4 )
                 - tmp * tmat( 3, 4 );
            tmat( 4, 5 ) =  tmat( 4, 5 )
                 - tmp * tmat( 3, 5 );
            tv( 4, i, j ) = tv( 4, i, j )
              - tv( 3, i, j ) * tmp;

            tmp = tmp1 * tmat( 5, 3 );
            tmat( 5, 4 ) =  tmat( 5, 4 )
                 - tmp * tmat( 3, 4 );
            tmat( 5, 5 ) =  tmat( 5, 5 )
                 - tmp * tmat( 3, 5 );
            tv( 5, i, j ) = tv( 5, i, j )
              - tv( 3, i, j ) * tmp;

            tmp1 = 1.0 / tmat( 4, 4 );
            tmp = tmp1 * tmat( 5, 4 );
            tmat( 5, 5 ) =  tmat( 5, 5 )
                 - tmp * tmat( 4, 5 );
            tv( 5, i, j ) = tv( 5, i, j )
              - tv( 4, i, j ) * tmp;

//c---------------------------------------------------------------------
//c   back substitution
//c---------------------------------------------------------------------
            tv( 5, i, j ) = tv( 5, i, j )
                            / tmat( 5, 5 );

            tv( 4, i, j ) = tv( 4, i, j )
                 - tmat( 4, 5 ) * tv( 5, i, j );
            tv( 4, i, j ) = tv( 4, i, j )
                            / tmat( 4, 4 );

            tv( 3, i, j ) = tv( 3, i, j )
                 - tmat( 3, 4 ) * tv( 4, i, j )
                 - tmat( 3, 5 ) * tv( 5, i, j );
            tv( 3, i, j ) = tv( 3, i, j )
                            / tmat( 3, 3 );

            tv( 2, i, j ) = tv( 2, i, j )
                 - tmat( 2, 3 ) * tv( 3, i, j )
                 - tmat( 2, 4 ) * tv( 4, i, j )
                 - tmat( 2, 5 ) * tv( 5, i, j );
            tv( 2, i, j ) = tv( 2, i, j )
                            / tmat( 2, 2 );

            tv( 1, i, j ) = tv( 1, i, j )
                 - tmat( 1, 2 ) * tv( 2, i, j )
                 - tmat( 1, 3 ) * tv( 3, i, j )
                 - tmat( 1, 4 ) * tv( 4, i, j )
                 - tmat( 1, 5 ) * tv( 5, i, j );
            tv( 1, i, j ) = tv( 1, i, j )
                            / tmat( 1, 1 );

            rsd( 1, i, j, k ) = rsd( 1, i, j, k ) - tv( 1, i, j );
            rsd( 2, i, j, k ) = rsd( 2, i, j, k ) - tv( 2, i, j );
            rsd( 3, i, j, k ) = rsd( 3, i, j, k ) - tv( 3, i, j );
            rsd( 4, i, j, k ) = rsd( 4, i, j, k ) - tv( 4, i, j );
            rsd( 5, i, j, k ) = rsd( 5, i, j, k ) - tv( 5, i, j );

          }
      }

//c---------------------------------------------------------------------
//c   send data to north and west
//c---------------------------------------------------------------------
      iex = 3;
      exchange_1(rsd, k, iex);
 
      return;
}
