#include "applu_share.h"
#include "applu_macros.h"

#define ue_1jk(m)   ue_1jk[m-1]
#define ue_nx0jk(m) ue_nx0jk[m-1]
#define ue_i1k(m)   ue_i1k[m-1]
#define ue_iny0k(m) ue_iny0k[m-1]
#define ue_ij1(m)   ue_ij1[m-1]
#define ue_ijnz(m)  ue_ijnz[m-1]

void setiv() {

//c---------------------------------------------------------------------
//c
//c   set the initial values of independent variables based on tri-linear
//c   interpolation of boundary values in the computational space.
//c
//c---------------------------------------------------------------------

//c---------------------------------------------------------------------
//c  local variables
//c---------------------------------------------------------------------
      int i, j, k, m;
      int iglob, jglob;
      double xi, eta, zeta;
      double pxi, peta, pzeta;
      double ue_1jk[5],ue_nx0jk[5],ue_i1k[5],
             ue_iny0k[5],ue_ij1[5],ue_ijnz[5];
      int one = 1;

      for ( k = 2;  k <= nz - 1;  k++) {
         zeta = ( (double) (k-1) ) / (nz-1);
         for ( j = 1;  j <= ny;  j++) {
          jglob = jpt + j;
          if (jglob != 1 && jglob != ny0) {
            eta = ( (double) (jglob-1) ) / (ny0-1);
            for ( i = 1;  i <= nx;  i++) {
              iglob = ipt + i;
              if (iglob != 1 && iglob != nx0) {
               xi = ( (double) (iglob-1) ) / (nx0-1);
               exact(one,jglob,k,ue_1jk);
               exact(nx0,jglob,k,ue_nx0jk);
               exact(iglob,one,k,ue_i1k);
               exact(iglob,ny0,k,ue_iny0k);
               exact(iglob,jglob,one,ue_ij1);
               exact(iglob,jglob,nz,ue_ijnz);
               for ( m = 1;  m <= 5;  m++) {
                  pxi =   (1.0 - xi  ) * ue_1jk(m) + xi   * ue_nx0jk(m);
                  peta =  (1.0 - eta ) * ue_i1k(m) + eta  * ue_iny0k(m);
                  pzeta = (1.0 - zeta) * ue_ij1(m) + zeta * ue_ijnz(m);

                  u( m, i, j, k ) = pxi + peta + pzeta
                       - pxi * peta - peta * pzeta - pzeta * pxi
                       + pxi * peta * pzeta;

               }
              }
            }
          }
         }
      }

//      for ( k = 1;  k <= nz;  k++) {
//         for ( j = -1;  j <= ny+2;  j++) {
//            for ( i = -1;  i <= nx+2;  i++) {
//               for ( m = 1;  m <= 5;  m++) {
//                  u( m, i, j, k ) = (double) (id*100 + m);
//               }
//            }
//         }
//      }

      return;
}

