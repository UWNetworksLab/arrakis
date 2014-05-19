#include "applu_share.h"
#include "applu_macros.h"

void blts(int k) {

//c   compute the regular-sparse, block lower triangular solution:


//c  local variables

      int i, j, m;
      int iex;
      double  tmp, tmp1;
      double  tmat[5*5];

//c   receive data from north and west

      iex = 0;
      exchange_1(rsd, k, iex);

      for (j=jst; j<=jend; j++) {
         for (i=ist; i<=iend; i++) {
            for (m=1; m<=5; m++) {

                  rsd( m, i, j, k ) =  rsd( m, i, j, k )
          - omega * (  a( m, 1, i, j ) * rsd( 1, i, j, k-1 )
                     + a( m, 2, i, j ) * rsd( 2, i, j, k-1 )
                     + a( m, 3, i, j ) * rsd( 3, i, j, k-1 )
                     + a( m, 4, i, j ) * rsd( 4, i, j, k-1 )
                     + a( m, 5, i, j ) * rsd( 5, i, j, k-1 )  );
            }
         }
      }

      for (j=jst; j<=jend; j++) {
         for (i=ist; i<=iend; i++) {
            for (m=1; m<=5; m++) {

                  rsd( m, i, j, k ) =  rsd( m, i, j, k )
       - omega * ( b( m, 1, i, j ) * rsd( 1, i, j-1, k )
                 + c( m, 1, i, j ) * rsd( 1, i-1, j, k )
                 + b( m, 2, i, j ) * rsd( 2, i, j-1, k )
                 + c( m, 2, i, j ) * rsd( 2, i-1, j, k )
                 + b( m, 3, i, j ) * rsd( 3, i, j-1, k )
                 + c( m, 3, i, j ) * rsd( 3, i-1, j, k )
                 + b( m, 4, i, j ) * rsd( 4, i, j-1, k )
                 + c( m, 4, i, j ) * rsd( 4, i-1, j, k )
                 + b( m, 5, i, j ) * rsd( 5, i, j-1, k )
                 + c( m, 5, i, j ) * rsd( 5, i-1, j, k ) );
            }

//c   diagonal block inversion
//c
//c   forward elimination

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
            rsd( 2, i, j, k ) = rsd( 2, i, j, k )
              - rsd( 1, i, j, k ) * tmp;

            tmp = tmp1 * tmat( 3, 1 );
            tmat( 3, 2 ) =  tmat( 3, 2 )
                 - tmp * tmat( 1, 2 );
            tmat( 3, 3 ) =  tmat( 3, 3 )
                 - tmp * tmat( 1, 3 );
            tmat( 3, 4 ) =  tmat( 3, 4 )
                 - tmp * tmat( 1, 4 );
            tmat( 3, 5 ) =  tmat( 3, 5 )
                 - tmp * tmat( 1, 5 );
            rsd( 3, i, j, k ) = rsd( 3, i, j, k )
              - rsd( 1, i, j, k ) * tmp;

            tmp = tmp1 * tmat( 4, 1 );
            tmat( 4, 2 ) =  tmat( 4, 2 )
                 - tmp * tmat( 1, 2 );
            tmat( 4, 3 ) =  tmat( 4, 3 )
                 - tmp * tmat( 1, 3 );
            tmat( 4, 4 ) =  tmat( 4, 4 )
                 - tmp * tmat( 1, 4 );
            tmat( 4, 5 ) =  tmat( 4, 5 )
                 - tmp * tmat( 1, 5 );
            rsd( 4, i, j, k ) = rsd( 4, i, j, k )
              - rsd( 1, i, j, k ) * tmp;

            tmp = tmp1 * tmat( 5, 1 );
            tmat( 5, 2 ) =  tmat( 5, 2 )
                 - tmp * tmat( 1, 2 );
            tmat( 5, 3 ) =  tmat( 5, 3 )
                 - tmp * tmat( 1, 3 );
            tmat( 5, 4 ) =  tmat( 5, 4 )
                 - tmp * tmat( 1, 4 );
            tmat( 5, 5 ) =  tmat( 5, 5 )
                 - tmp * tmat( 1, 5 );
            rsd( 5, i, j, k ) = rsd( 5, i, j, k )
              - rsd( 1, i, j, k ) * tmp;

            tmp1 = 1.0 / tmat( 2, 2 );
            tmp = tmp1 * tmat( 3, 2 );
            tmat( 3, 3 ) =  tmat( 3, 3 )
                 - tmp * tmat( 2, 3 );
            tmat( 3, 4 ) =  tmat( 3, 4 )
                 - tmp * tmat( 2, 4 );
            tmat( 3, 5 ) =  tmat( 3, 5 )
                 - tmp * tmat( 2, 5 );
            rsd( 3, i, j, k ) = rsd( 3, i, j, k )
              - rsd( 2, i, j, k ) * tmp;

            tmp = tmp1 * tmat( 4, 2 );
            tmat( 4, 3 ) =  tmat( 4, 3 )
                 - tmp * tmat( 2, 3 );
            tmat( 4, 4 ) =  tmat( 4, 4 )
                 - tmp * tmat( 2, 4 );
            tmat( 4, 5 ) =  tmat( 4, 5 )
                 - tmp * tmat( 2, 5 );
            rsd( 4, i, j, k ) = rsd( 4, i, j, k )
              - rsd( 2, i, j, k ) * tmp;

            tmp = tmp1 * tmat( 5, 2 );
            tmat( 5, 3 ) =  tmat( 5, 3 )
                 - tmp * tmat( 2, 3 );
            tmat( 5, 4 ) =  tmat( 5, 4 )
                 - tmp * tmat( 2, 4 );
            tmat( 5, 5 ) =  tmat( 5, 5 )
                 - tmp * tmat( 2, 5 );
            rsd( 5, i, j, k ) = rsd( 5, i, j, k )
              - rsd( 2, i, j, k ) * tmp;

            tmp1 = 1.0 / tmat( 3, 3 );
            tmp = tmp1 * tmat( 4, 3 );
            tmat( 4, 4 ) =  tmat( 4, 4 )
                 - tmp * tmat( 3, 4 );
            tmat( 4, 5 ) =  tmat( 4, 5 )
                 - tmp * tmat( 3, 5 );
            rsd( 4, i, j, k ) = rsd( 4, i, j, k )
              - rsd( 3, i, j, k ) * tmp;

            tmp = tmp1 * tmat( 5, 3 );
            tmat( 5, 4 ) =  tmat( 5, 4 )
                 - tmp * tmat( 3, 4 );
            tmat( 5, 5 ) =  tmat( 5, 5 )
                 - tmp * tmat( 3, 5 );
            rsd( 5, i, j, k ) = rsd( 5, i, j, k )
              - rsd( 3, i, j, k ) * tmp;

            tmp1 = 1.0 / tmat( 4, 4 );
            tmp = tmp1 * tmat( 5, 4 );
            tmat( 5, 5 ) =  tmat( 5, 5 )
                 - tmp * tmat( 4, 5 );
            rsd( 5, i, j, k ) = rsd( 5, i, j, k )
              - rsd( 4, i, j, k ) * tmp;

//c   back substitution

            rsd( 5, i, j, k ) = rsd( 5, i, j, k )
                            / tmat( 5, 5 );

            rsd( 4, i, j, k ) = rsd( 4, i, j, k )
                 - tmat( 4, 5 ) * rsd( 5, i, j, k );
            rsd( 4, i, j, k ) = rsd( 4, i, j, k )
                            / tmat( 4, 4 );

            rsd( 3, i, j, k ) = rsd( 3, i, j, k )
                 - tmat( 3, 4 ) * rsd( 4, i, j, k )
                 - tmat( 3, 5 ) * rsd( 5, i, j, k );
            rsd( 3, i, j, k ) = rsd( 3, i, j, k )
                            / tmat( 3, 3 );

            rsd( 2, i, j, k ) = rsd( 2, i, j, k )
                 - tmat( 2, 3 ) * rsd( 3, i, j, k )
                 - tmat( 2, 4 ) * rsd( 4, i, j, k )
                 - tmat( 2, 5 ) * rsd( 5, i, j, k );
            rsd( 2, i, j, k ) = rsd( 2, i, j, k )
                            / tmat( 2, 2 );

            rsd( 1, i, j, k ) = rsd( 1, i, j, k )
                 - tmat( 1, 2 ) * rsd( 2, i, j, k )
                 - tmat( 1, 3 ) * rsd( 3, i, j, k )
                 - tmat( 1, 4 ) * rsd( 4, i, j, k )
                 - tmat( 1, 5 ) * rsd( 5, i, j, k );
            rsd( 1, i, j, k ) = rsd( 1, i, j, k )
                            / tmat( 1, 1 );

         }
      }

//c   send data to east and south

      iex = 2;
      exchange_1(rsd, k, iex);

      return;
}



