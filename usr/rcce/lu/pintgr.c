#include "RCCE.h"
#include <stdio.h>
#include <stdlib.h>
#include "applu_share.h"
#include "applu_macros.h"

void pintgr() {

      int i, j, k, ind1, ind2;
      int ibeg, ifin, ifin1, jbeg, jfin, jfin1;
      int iglob, iglob1, iglob2, jglob, jglob1, jglob2;
      double phi1[(isiz2+2)*(isiz3+2)], phi2[(isiz2+2)*(isiz3+2)], 
             frc1, frc2, frc3, dummy;

//c---------------------------------------------------------------------
//c   set up the sub-domains for integeration in each processor
//c---------------------------------------------------------------------
      ibeg   = nx + 1;
      ifin   = 0;
      iglob1 = ipt + 1;
      iglob2 = ipt + nx;
      if ((iglob1 >= ii1)   && (iglob2 < ii2+nx)) ibeg = 1;
      if ((iglob1 > ii1-nx) && (iglob2 <= ii2)  ) ifin = nx;
      if ((ii1 >= iglob1)   && (ii1 <= iglob2)  ) ibeg = ii1 - ipt;
      if ((ii2 >= iglob1)   && (ii2 <= iglob2)  ) ifin = ii2 - ipt;
      jbeg = ny + 1;
      jfin = 0;
      jglob1 = jpt + 1;
      jglob2 = jpt + ny;
      if ((jglob1 >= ji1)   && (jglob2 < ji2+ny)) jbeg = 1;
      if ((jglob1 > ji1-ny) && (jglob2 <= ji2)  ) jfin = ny;
      if ((ji1 >= jglob1)   && (ji1 <= jglob2)  ) jbeg = ji1 - jpt;
      if ((ji2 >= jglob1)   && (ji2 <= jglob2)  ) jfin = ji2 - jpt;
      ifin1 = ifin;
      jfin1 = jfin;
      if (ipt + ifin1 == ii2) ifin1 = ifin -1;
      if (jpt + jfin1 == ji2) jfin1 = jfin -1;

//c---------------------------------------------------------------------
//c   initialize
//c---------------------------------------------------------------------
      for (i=0; i<=isiz2+1; i++) {
        for (k=0; k<=isiz3+1; k++) {
          phi1(i,k) = 0.0;
          phi2(i,k) = 0.0;
        }
      }

      for (j= jbeg; j<=jfin; j++) {
         jglob = jpt + j;
         for (i=ibeg; i<=ifin; i++) {
            iglob = ipt + i;

            k = ki1;

            phi1(i,j) = c2*(  u(5,i,j,k)
                 - 0.50 * (  u(2,i,j,k) * u(2,i,j,k)
                               + u(3,i,j,k) * u(3,i,j,k)
                               + u(4,i,j,k) * u(4,i,j,k) )
                              / u(1,i,j,k) );

            k = ki2;

            phi2(i,j) = c2*(  u(5,i,j,k)
                 - 0.50 * (  u(2,i,j,k) * u(2,i,j,k) 
                               + u(3,i,j,k) * u(3,i,j,k)
                               + u(4,i,j,k) * u(4,i,j,k) )
                              / u(1,i,j,k) );
         }
      }

//c---------------------------------------------------------------------
//c  communicate in i and j directions
//c---------------------------------------------------------------------
      exchange_4(phi1, phi2, ibeg, ifin1, jbeg, jfin1);

      frc1 = 0.0;

      for (j= jbeg; j<=jfin1; j++) {
         for (i=ibeg; i<= ifin1; i++) {
            frc1 = frc1 + (  phi1(i,j)
                           + phi1(i+1,j)
                           + phi1(i,j+1)
                           + phi1(i+1,j+1)
                           + phi2(i,j)
                           + phi2(i+1,j)
                           + phi2(i,j+1)
                           + phi2(i+1,j+1) );
         }
      }

//c---------------------------------------------------------------------
//c  compute the global sum of individual contributions to frc1
//c---------------------------------------------------------------------
      dummy = frc1;
      RCCE_allreduce((char*)(&dummy), (char*)(&frc1), 1, RCCE_DOUBLE, RCCE_SUM, RCCE_COMM_WORLD);

      frc1 = dxi * deta * frc1;

//c---------------------------------------------------------------------
//c   initialize
//c---------------------------------------------------------------------
      for (i=0; i<=isiz2+1; i++) {
        for (k= 0; k<=isiz3+1; k++) {
          phi1(i,k) = 0.0;
          phi2(i,k) = 0.0;
        }
      }
      jglob = jpt + jbeg;
      ind1 = 0;
      if (jglob == ji1) {
        ind1 = 1;
        for (k= ki1; k<= ki2; k++) {
           for (i=ibeg; i<= ifin; i++) {
              iglob = ipt + i;
              phi1(i,k) = c2*(  u(5,i,jbeg,k)
                   - 0.50 * (  u(2,i,jbeg,k) * u(2,i,jbeg,k)
                                 + u(3,i,jbeg,k) * u(3,i,jbeg,k)
                                 + u(4,i,jbeg,k) *u(4,i,jbeg,k) )
                                / u(1,i,jbeg,k) );
           }
        }
      }

      jglob = jpt + jfin;
      ind2 = 0;
      if (jglob == ji2) {
        ind2 = 1;
        for (k= ki1; k<= ki2; k++) {
           for (i=ibeg; i<= ifin; i++) {
              iglob = ipt + i;
              phi2(i,k) = c2*(  u(5,i,jfin,k)
                   - 0.50 * (  u(2,i,jfin,k) * u(2,i,jfin,k)
                                 + u(3,i,jfin,k) * u(3,i,jfin,k)
                                 + u(4,i,jfin,k) * u(4,i,jfin,k) )
                                / u(1,i,jfin,k) );
           }
        }
      }

//c---------------------------------------------------------------------
//c  communicate in i direction
//c---------------------------------------------------------------------
      if (ind1 == 1) exchange_5(phi1, ibeg, ifin1);
      if (ind2 == 1) exchange_5(phi2, ibeg, ifin1);

      frc2 = 0.0;
      for (k= ki1; k<= ki2-1; k++) {
         for (i=ibeg; i<= ifin1; i++) {
            frc2 = frc2 + (  phi1(i,k)
                           + phi1(i+1,k)
                           + phi1(i,k+1)
                           + phi1(i+1,k+1)
                           + phi2(i,k)
                           + phi2(i+1,k)
                           + phi2(i,k+1)
                           + phi2(i+1,k+1) );
         }
      }

//c---------------------------------------------------------------------
//c  compute the global sum of individual contributions to frc2
//c---------------------------------------------------------------------
      dummy = frc2;
      RCCE_allreduce((char*)(&dummy), (char*)(&frc2), 1, RCCE_DOUBLE, RCCE_SUM, RCCE_COMM_WORLD);

      frc2 = dxi * dzeta * frc2;

//c---------------------------------------------------------------------
//c   initialize
//c---------------------------------------------------------------------
      for (i=0; i<=isiz2+1; i++) {
        for (k= 0; k<=isiz3+1; k++) {
          phi1(i,k) = 0.0;
          phi2(i,k) = 0.0;
        }
      }
      iglob = ipt + ibeg;
      ind1 = 0;
      if (iglob == ii1) {
        ind1 = 1;
        for (k= ki1; k<= ki2; k++) {
           for (j= jbeg; j<= jfin; j++) {
              jglob = jpt + j;
              phi1(j,k) = c2*(  u(5,ibeg,j,k)
                   - 0.50 * (  u(2,ibeg,j,k) * u(2,ibeg,j,k)
                                 + u(3,ibeg,j,k) * u(3,ibeg,j,k)
                                 + u(4,ibeg,j,k) * u(4,ibeg,j,k) )
                                / u(1,ibeg,j,k) );
           }
        }
      }

      iglob = ipt + ifin;
      ind2 = 0;
      if (iglob == ii2) {
        ind2 = 1;
        for (k= ki1; k<= ki2; k++) {
           for (j= jbeg; j<= jfin; j++) {
              jglob = jpt + j;
              phi2(j,k) = c2*(  u(5,ifin,j,k)
                   - 0.50 * (  u(2,ifin,j,k) * u(2,ifin,j,k)
                                 + u(3,ifin,j,k) * u(3,ifin,j,k)
                                 + u(4,ifin,j,k) * u(4,ifin,j,k) )
                                / u(1,ifin,j,k) );
           }
        }
      }

//c---------------------------------------------------------------------
//c  communicate in j direction
//c---------------------------------------------------------------------
      if (ind1 == 1) exchange_6(phi1, jbeg, jfin1);
      if (ind2 == 1) exchange_6(phi2, jbeg, jfin1);

      frc3 = 0.0;

      for (k= ki1; k<= ki2-1; k++) {
         for (j= jbeg; j<= jfin1; j++) {
            frc3 = frc3 + (  phi1(j,k)
                           + phi1(j+1,k)
                           + phi1(j,k+1)
                           + phi1(j+1,k+1)
                           + phi2(j,k)
                           + phi2(j+1,k)
                           + phi2(j,k+1)
                           + phi2(j+1,k+1) );
         }
      }

//c---------------------------------------------------------------------
//c  compute the global sum of individual contributions to frc3
//c---------------------------------------------------------------------
      dummy = frc3;
      RCCE_allreduce((char*)(&dummy), (char*)(&frc3), 1, RCCE_DOUBLE, RCCE_SUM, RCCE_COMM_WORLD);

      frc3 = deta * dzeta * frc3;
      frc = 0.25 * ( frc1 + frc2 + frc3 );

      return;
}
