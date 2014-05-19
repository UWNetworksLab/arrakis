
//---------------------------------------------------------------------
//---------------------------------------------------------------------
#define ablock(m,n) ablock[(m-1)+5*(n-1)]
#define bblock(m,n) bblock[(m-1)+5*(n-1)]
#define cblock(m,n) cblock[(m-1)+5*(n-1)]
#define avec(m) avec[m-1]
#define bvec(m) bvec[m-1]
#define lhs(m,n) lhs[(m-1)+5*(n-1)]
#define c(m,n) c[(m-1)+5*(n-1)]
#define r(m) r[m-1]

void matvec_sub(double ablock[],double avec[],double bvec[]) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     subtracts bvec=bvec - ablock*avec
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//            rhs(i,ic,jc,kc,ccell) = rhs(i,ic,jc,kc,ccell) 
//     $           - lhs(i,1,ablock,ia,ja,ka,acell)*
//---------------------------------------------------------------------
         bvec(1) = bvec(1) - ablock(1,1)*avec(1)
                           - ablock(1,2)*avec(2)
                           - ablock(1,3)*avec(3)
                           - ablock(1,4)*avec(4)
                           - ablock(1,5)*avec(5);
         bvec(2) = bvec(2) - ablock(2,1)*avec(1)
                           - ablock(2,2)*avec(2)
                           - ablock(2,3)*avec(3)
                           - ablock(2,4)*avec(4)
                           - ablock(2,5)*avec(5);
         bvec(3) = bvec(3) - ablock(3,1)*avec(1)
                           - ablock(3,2)*avec(2)
                           - ablock(3,3)*avec(3)
                           - ablock(3,4)*avec(4)
                           - ablock(3,5)*avec(5);
         bvec(4) = bvec(4) - ablock(4,1)*avec(1)
                           - ablock(4,2)*avec(2)
                           - ablock(4,3)*avec(3)
                           - ablock(4,4)*avec(4)
                           - ablock(4,5)*avec(5);
         bvec(5) = bvec(5) - ablock(5,1)*avec(1)
                           - ablock(5,2)*avec(2)
                           - ablock(5,3)*avec(3)
                           - ablock(5,4)*avec(4)
                           - ablock(5,5)*avec(5);


      return;
}

//---------------------------------------------------------------------
//---------------------------------------------------------------------

void matmul_sub(double ablock[], double bblock[], double cblock[]) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     subtracts a(i,j,k) X b(i,j,k) from c(i,j,k)
//---------------------------------------------------------------------


         cblock(1,1) = cblock(1,1) - ablock(1,1)*bblock(1,1)
                                   - ablock(1,2)*bblock(2,1)
                                   - ablock(1,3)*bblock(3,1)
                                   - ablock(1,4)*bblock(4,1)
                                   - ablock(1,5)*bblock(5,1);
         cblock(2,1) = cblock(2,1) - ablock(2,1)*bblock(1,1)
                                   - ablock(2,2)*bblock(2,1)
                                   - ablock(2,3)*bblock(3,1)
                                   - ablock(2,4)*bblock(4,1)
                                   - ablock(2,5)*bblock(5,1);
         cblock(3,1) = cblock(3,1) - ablock(3,1)*bblock(1,1)
                                   - ablock(3,2)*bblock(2,1)
                                   - ablock(3,3)*bblock(3,1)
                                   - ablock(3,4)*bblock(4,1)
                                   - ablock(3,5)*bblock(5,1);
         cblock(4,1) = cblock(4,1) - ablock(4,1)*bblock(1,1)
                                   - ablock(4,2)*bblock(2,1)
                                   - ablock(4,3)*bblock(3,1)
                                   - ablock(4,4)*bblock(4,1)
                                   - ablock(4,5)*bblock(5,1);
         cblock(5,1) = cblock(5,1) - ablock(5,1)*bblock(1,1)
                                   - ablock(5,2)*bblock(2,1)
                                   - ablock(5,3)*bblock(3,1)
                                   - ablock(5,4)*bblock(4,1)
                                   - ablock(5,5)*bblock(5,1);
         cblock(1,2) = cblock(1,2) - ablock(1,1)*bblock(1,2)
                                   - ablock(1,2)*bblock(2,2)
                                   - ablock(1,3)*bblock(3,2)
                                   - ablock(1,4)*bblock(4,2)
                                   - ablock(1,5)*bblock(5,2);
         cblock(2,2) = cblock(2,2) - ablock(2,1)*bblock(1,2)
                                   - ablock(2,2)*bblock(2,2)
                                   - ablock(2,3)*bblock(3,2)
                                   - ablock(2,4)*bblock(4,2)
                                   - ablock(2,5)*bblock(5,2);
         cblock(3,2) = cblock(3,2) - ablock(3,1)*bblock(1,2)
                                   - ablock(3,2)*bblock(2,2)
                                   - ablock(3,3)*bblock(3,2)
                                   - ablock(3,4)*bblock(4,2)
                                   - ablock(3,5)*bblock(5,2);
         cblock(4,2) = cblock(4,2) - ablock(4,1)*bblock(1,2)
                                   - ablock(4,2)*bblock(2,2)
                                   - ablock(4,3)*bblock(3,2)
                                   - ablock(4,4)*bblock(4,2)
                                   - ablock(4,5)*bblock(5,2);
         cblock(5,2) = cblock(5,2) - ablock(5,1)*bblock(1,2)
                                   - ablock(5,2)*bblock(2,2)
                                   - ablock(5,3)*bblock(3,2)
                                   - ablock(5,4)*bblock(4,2)
                                   - ablock(5,5)*bblock(5,2);
         cblock(1,3) = cblock(1,3) - ablock(1,1)*bblock(1,3)
                                   - ablock(1,2)*bblock(2,3)
                                   - ablock(1,3)*bblock(3,3)
                                   - ablock(1,4)*bblock(4,3)
                                   - ablock(1,5)*bblock(5,3);
         cblock(2,3) = cblock(2,3) - ablock(2,1)*bblock(1,3)
                                   - ablock(2,2)*bblock(2,3)
                                   - ablock(2,3)*bblock(3,3)
                                   - ablock(2,4)*bblock(4,3)
                                   - ablock(2,5)*bblock(5,3);
         cblock(3,3) = cblock(3,3) - ablock(3,1)*bblock(1,3)
                                   - ablock(3,2)*bblock(2,3)
                                   - ablock(3,3)*bblock(3,3)
                                   - ablock(3,4)*bblock(4,3)
                                   - ablock(3,5)*bblock(5,3);
         cblock(4,3) = cblock(4,3) - ablock(4,1)*bblock(1,3)
                                   - ablock(4,2)*bblock(2,3)
                                   - ablock(4,3)*bblock(3,3)
                                   - ablock(4,4)*bblock(4,3)
                                   - ablock(4,5)*bblock(5,3);
         cblock(5,3) = cblock(5,3) - ablock(5,1)*bblock(1,3)
                                   - ablock(5,2)*bblock(2,3)
                                   - ablock(5,3)*bblock(3,3)
                                   - ablock(5,4)*bblock(4,3)
                                   - ablock(5,5)*bblock(5,3);
         cblock(1,4) = cblock(1,4) - ablock(1,1)*bblock(1,4)
                                   - ablock(1,2)*bblock(2,4)
                                   - ablock(1,3)*bblock(3,4)
                                   - ablock(1,4)*bblock(4,4)
                                   - ablock(1,5)*bblock(5,4);
         cblock(2,4) = cblock(2,4) - ablock(2,1)*bblock(1,4)
                                   - ablock(2,2)*bblock(2,4)
                                   - ablock(2,3)*bblock(3,4)
                                   - ablock(2,4)*bblock(4,4)
                                   - ablock(2,5)*bblock(5,4);
         cblock(3,4) = cblock(3,4) - ablock(3,1)*bblock(1,4)
                                   - ablock(3,2)*bblock(2,4)
                                   - ablock(3,3)*bblock(3,4)
                                   - ablock(3,4)*bblock(4,4)
                                   - ablock(3,5)*bblock(5,4);
         cblock(4,4) = cblock(4,4) - ablock(4,1)*bblock(1,4)
                                   - ablock(4,2)*bblock(2,4)
                                   - ablock(4,3)*bblock(3,4)
                                   - ablock(4,4)*bblock(4,4)
                                   - ablock(4,5)*bblock(5,4);
         cblock(5,4) = cblock(5,4) - ablock(5,1)*bblock(1,4)
                                   - ablock(5,2)*bblock(2,4)
                                   - ablock(5,3)*bblock(3,4)
                                   - ablock(5,4)*bblock(4,4)
                                   - ablock(5,5)*bblock(5,4);
         cblock(1,5) = cblock(1,5) - ablock(1,1)*bblock(1,5)
                                   - ablock(1,2)*bblock(2,5)
                                   - ablock(1,3)*bblock(3,5)
                                   - ablock(1,4)*bblock(4,5)
                                   - ablock(1,5)*bblock(5,5);
         cblock(2,5) = cblock(2,5) - ablock(2,1)*bblock(1,5)
                                   - ablock(2,2)*bblock(2,5)
                                   - ablock(2,3)*bblock(3,5)
                                   - ablock(2,4)*bblock(4,5)
                                   - ablock(2,5)*bblock(5,5);
         cblock(3,5) = cblock(3,5) - ablock(3,1)*bblock(1,5)
                                   - ablock(3,2)*bblock(2,5)
                                   - ablock(3,3)*bblock(3,5)
                                   - ablock(3,4)*bblock(4,5)
                                   - ablock(3,5)*bblock(5,5);
         cblock(4,5) = cblock(4,5) - ablock(4,1)*bblock(1,5)
                                   - ablock(4,2)*bblock(2,5)
                                   - ablock(4,3)*bblock(3,5)
                                   - ablock(4,4)*bblock(4,5)
                                   - ablock(4,5)*bblock(5,5);
         cblock(5,5) = cblock(5,5) - ablock(5,1)*bblock(1,5)
                                   - ablock(5,2)*bblock(2,5)
                                   - ablock(5,3)*bblock(3,5)
                                   - ablock(5,4)*bblock(4,5)
                                   - ablock(5,5)*bblock(5,5);

              
      return;
}



//---------------------------------------------------------------------
//---------------------------------------------------------------------

void binvcrhs( double lhs[],double c[],double r[] ) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     
//---------------------------------------------------------------------

      double pivot, coeff;

//---------------------------------------------------------------------
//     
//---------------------------------------------------------------------

      pivot = 1.00e0/lhs(1,1);
      lhs(1,2) = lhs(1,2)*pivot;
      lhs(1,3) = lhs(1,3)*pivot;
      lhs(1,4) = lhs(1,4)*pivot;
      lhs(1,5) = lhs(1,5)*pivot;
      c(1,1) = c(1,1)*pivot;
      c(1,2) = c(1,2)*pivot;
      c(1,3) = c(1,3)*pivot;
      c(1,4) = c(1,4)*pivot;
      c(1,5) = c(1,5)*pivot;
      r(1)   = r(1)  *pivot;

      coeff = lhs(2,1);
      lhs(2,2)= lhs(2,2) - coeff*lhs(1,2);
      lhs(2,3)= lhs(2,3) - coeff*lhs(1,3);
      lhs(2,4)= lhs(2,4) - coeff*lhs(1,4);
      lhs(2,5)= lhs(2,5) - coeff*lhs(1,5);
      c(2,1) = c(2,1) - coeff*c(1,1);
      c(2,2) = c(2,2) - coeff*c(1,2);
      c(2,3) = c(2,3) - coeff*c(1,3);
      c(2,4) = c(2,4) - coeff*c(1,4);
      c(2,5) = c(2,5) - coeff*c(1,5);
      r(2)   = r(2)   - coeff*r(1);

      coeff = lhs(3,1);
      lhs(3,2)= lhs(3,2) - coeff*lhs(1,2);
      lhs(3,3)= lhs(3,3) - coeff*lhs(1,3);
      lhs(3,4)= lhs(3,4) - coeff*lhs(1,4);
      lhs(3,5)= lhs(3,5) - coeff*lhs(1,5);
      c(3,1) = c(3,1) - coeff*c(1,1);
      c(3,2) = c(3,2) - coeff*c(1,2);
      c(3,3) = c(3,3) - coeff*c(1,3);
      c(3,4) = c(3,4) - coeff*c(1,4);
      c(3,5) = c(3,5) - coeff*c(1,5);
      r(3)   = r(3)   - coeff*r(1);

      coeff = lhs(4,1);
      lhs(4,2)= lhs(4,2) - coeff*lhs(1,2);
      lhs(4,3)= lhs(4,3) - coeff*lhs(1,3);
      lhs(4,4)= lhs(4,4) - coeff*lhs(1,4);
      lhs(4,5)= lhs(4,5) - coeff*lhs(1,5);
      c(4,1) = c(4,1) - coeff*c(1,1);
      c(4,2) = c(4,2) - coeff*c(1,2);
      c(4,3) = c(4,3) - coeff*c(1,3);
      c(4,4) = c(4,4) - coeff*c(1,4);
      c(4,5) = c(4,5) - coeff*c(1,5);
      r(4)   = r(4)   - coeff*r(1);

      coeff = lhs(5,1);
      lhs(5,2)= lhs(5,2) - coeff*lhs(1,2);
      lhs(5,3)= lhs(5,3) - coeff*lhs(1,3);
      lhs(5,4)= lhs(5,4) - coeff*lhs(1,4);
      lhs(5,5)= lhs(5,5) - coeff*lhs(1,5);
      c(5,1) = c(5,1) - coeff*c(1,1);
      c(5,2) = c(5,2) - coeff*c(1,2);
      c(5,3) = c(5,3) - coeff*c(1,3);
      c(5,4) = c(5,4) - coeff*c(1,4);
      c(5,5) = c(5,5) - coeff*c(1,5);
      r(5)   = r(5)   - coeff*r(1);


      pivot = 1.00e0/lhs(2,2);
      lhs(2,3) = lhs(2,3)*pivot;
      lhs(2,4) = lhs(2,4)*pivot;
      lhs(2,5) = lhs(2,5)*pivot;
      c(2,1) = c(2,1)*pivot;
      c(2,2) = c(2,2)*pivot;
      c(2,3) = c(2,3)*pivot;
      c(2,4) = c(2,4)*pivot;
      c(2,5) = c(2,5)*pivot;
      r(2)   = r(2)  *pivot;

      coeff = lhs(1,2);
      lhs(1,3)= lhs(1,3) - coeff*lhs(2,3);
      lhs(1,4)= lhs(1,4) - coeff*lhs(2,4);
      lhs(1,5)= lhs(1,5) - coeff*lhs(2,5);
      c(1,1) = c(1,1) - coeff*c(2,1);
      c(1,2) = c(1,2) - coeff*c(2,2);
      c(1,3) = c(1,3) - coeff*c(2,3);
      c(1,4) = c(1,4) - coeff*c(2,4);
      c(1,5) = c(1,5) - coeff*c(2,5);
      r(1)   = r(1)   - coeff*r(2);

      coeff = lhs(3,2);
      lhs(3,3)= lhs(3,3) - coeff*lhs(2,3);
      lhs(3,4)= lhs(3,4) - coeff*lhs(2,4);
      lhs(3,5)= lhs(3,5) - coeff*lhs(2,5);
      c(3,1) = c(3,1) - coeff*c(2,1);
      c(3,2) = c(3,2) - coeff*c(2,2);
      c(3,3) = c(3,3) - coeff*c(2,3);
      c(3,4) = c(3,4) - coeff*c(2,4);
      c(3,5) = c(3,5) - coeff*c(2,5);
      r(3)   = r(3)   - coeff*r(2);

      coeff = lhs(4,2);
      lhs(4,3)= lhs(4,3) - coeff*lhs(2,3);
      lhs(4,4)= lhs(4,4) - coeff*lhs(2,4);
      lhs(4,5)= lhs(4,5) - coeff*lhs(2,5);
      c(4,1) = c(4,1) - coeff*c(2,1);
      c(4,2) = c(4,2) - coeff*c(2,2);
      c(4,3) = c(4,3) - coeff*c(2,3);
      c(4,4) = c(4,4) - coeff*c(2,4);
      c(4,5) = c(4,5) - coeff*c(2,5);
      r(4)   = r(4)   - coeff*r(2);

      coeff = lhs(5,2);
      lhs(5,3)= lhs(5,3) - coeff*lhs(2,3);
      lhs(5,4)= lhs(5,4) - coeff*lhs(2,4);
      lhs(5,5)= lhs(5,5) - coeff*lhs(2,5);
      c(5,1) = c(5,1) - coeff*c(2,1);
      c(5,2) = c(5,2) - coeff*c(2,2);
      c(5,3) = c(5,3) - coeff*c(2,3);
      c(5,4) = c(5,4) - coeff*c(2,4);
      c(5,5) = c(5,5) - coeff*c(2,5);
      r(5)   = r(5)   - coeff*r(2);


      pivot = 1.00e0/lhs(3,3);
      lhs(3,4) = lhs(3,4)*pivot;
      lhs(3,5) = lhs(3,5)*pivot;
      c(3,1) = c(3,1)*pivot;
      c(3,2) = c(3,2)*pivot;
      c(3,3) = c(3,3)*pivot;
      c(3,4) = c(3,4)*pivot;
      c(3,5) = c(3,5)*pivot;
      r(3)   = r(3)  *pivot;

      coeff = lhs(1,3);
      lhs(1,4)= lhs(1,4) - coeff*lhs(3,4);
      lhs(1,5)= lhs(1,5) - coeff*lhs(3,5);
      c(1,1) = c(1,1) - coeff*c(3,1);
      c(1,2) = c(1,2) - coeff*c(3,2);
      c(1,3) = c(1,3) - coeff*c(3,3);
      c(1,4) = c(1,4) - coeff*c(3,4);
      c(1,5) = c(1,5) - coeff*c(3,5);
      r(1)   = r(1)   - coeff*r(3);

      coeff = lhs(2,3);
      lhs(2,4)= lhs(2,4) - coeff*lhs(3,4);
      lhs(2,5)= lhs(2,5) - coeff*lhs(3,5);
      c(2,1) = c(2,1) - coeff*c(3,1);
      c(2,2) = c(2,2) - coeff*c(3,2);
      c(2,3) = c(2,3) - coeff*c(3,3);
      c(2,4) = c(2,4) - coeff*c(3,4);
      c(2,5) = c(2,5) - coeff*c(3,5);
      r(2)   = r(2)   - coeff*r(3);

      coeff = lhs(4,3);
      lhs(4,4)= lhs(4,4) - coeff*lhs(3,4);
      lhs(4,5)= lhs(4,5) - coeff*lhs(3,5);
      c(4,1) = c(4,1) - coeff*c(3,1);
      c(4,2) = c(4,2) - coeff*c(3,2);
      c(4,3) = c(4,3) - coeff*c(3,3);
      c(4,4) = c(4,4) - coeff*c(3,4);
      c(4,5) = c(4,5) - coeff*c(3,5);
      r(4)   = r(4)   - coeff*r(3);

      coeff = lhs(5,3);
      lhs(5,4)= lhs(5,4) - coeff*lhs(3,4);
      lhs(5,5)= lhs(5,5) - coeff*lhs(3,5);
      c(5,1) = c(5,1) - coeff*c(3,1);
      c(5,2) = c(5,2) - coeff*c(3,2);
      c(5,3) = c(5,3) - coeff*c(3,3);
      c(5,4) = c(5,4) - coeff*c(3,4);
      c(5,5) = c(5,5) - coeff*c(3,5);
      r(5)   = r(5)   - coeff*r(3);


      pivot = 1.00e0/lhs(4,4);
      lhs(4,5) = lhs(4,5)*pivot;
      c(4,1) = c(4,1)*pivot;
      c(4,2) = c(4,2)*pivot;
      c(4,3) = c(4,3)*pivot;
      c(4,4) = c(4,4)*pivot;
      c(4,5) = c(4,5)*pivot;
      r(4)   = r(4)  *pivot;

      coeff = lhs(1,4);
      lhs(1,5)= lhs(1,5) - coeff*lhs(4,5);
      c(1,1) = c(1,1) - coeff*c(4,1);
      c(1,2) = c(1,2) - coeff*c(4,2);
      c(1,3) = c(1,3) - coeff*c(4,3);
      c(1,4) = c(1,4) - coeff*c(4,4);
      c(1,5) = c(1,5) - coeff*c(4,5);
      r(1)   = r(1)   - coeff*r(4);

      coeff = lhs(2,4);
      lhs(2,5)= lhs(2,5) - coeff*lhs(4,5);
      c(2,1) = c(2,1) - coeff*c(4,1);
      c(2,2) = c(2,2) - coeff*c(4,2);
      c(2,3) = c(2,3) - coeff*c(4,3);
      c(2,4) = c(2,4) - coeff*c(4,4);
      c(2,5) = c(2,5) - coeff*c(4,5);
      r(2)   = r(2)   - coeff*r(4);

      coeff = lhs(3,4);
      lhs(3,5)= lhs(3,5) - coeff*lhs(4,5);
      c(3,1) = c(3,1) - coeff*c(4,1);
      c(3,2) = c(3,2) - coeff*c(4,2);
      c(3,3) = c(3,3) - coeff*c(4,3);
      c(3,4) = c(3,4) - coeff*c(4,4);
      c(3,5) = c(3,5) - coeff*c(4,5);
      r(3)   = r(3)   - coeff*r(4);

      coeff = lhs(5,4);
      lhs(5,5)= lhs(5,5) - coeff*lhs(4,5);
      c(5,1) = c(5,1) - coeff*c(4,1);
      c(5,2) = c(5,2) - coeff*c(4,2);
      c(5,3) = c(5,3) - coeff*c(4,3);
      c(5,4) = c(5,4) - coeff*c(4,4);
      c(5,5) = c(5,5) - coeff*c(4,5);
      r(5)   = r(5)   - coeff*r(4);


      pivot = 1.00e0/lhs(5,5);
      c(5,1) = c(5,1)*pivot;
      c(5,2) = c(5,2)*pivot;
      c(5,3) = c(5,3)*pivot;
      c(5,4) = c(5,4)*pivot;
      c(5,5) = c(5,5)*pivot;
      r(5)   = r(5)  *pivot;

      coeff = lhs(1,5);
      c(1,1) = c(1,1) - coeff*c(5,1);
      c(1,2) = c(1,2) - coeff*c(5,2);
      c(1,3) = c(1,3) - coeff*c(5,3);
      c(1,4) = c(1,4) - coeff*c(5,4);
      c(1,5) = c(1,5) - coeff*c(5,5);
      r(1)   = r(1)   - coeff*r(5);

      coeff = lhs(2,5);
      c(2,1) = c(2,1) - coeff*c(5,1);
      c(2,2) = c(2,2) - coeff*c(5,2);
      c(2,3) = c(2,3) - coeff*c(5,3);
      c(2,4) = c(2,4) - coeff*c(5,4);
      c(2,5) = c(2,5) - coeff*c(5,5);
      r(2)   = r(2)   - coeff*r(5);

      coeff = lhs(3,5);
      c(3,1) = c(3,1) - coeff*c(5,1);
      c(3,2) = c(3,2) - coeff*c(5,2);
      c(3,3) = c(3,3) - coeff*c(5,3);
      c(3,4) = c(3,4) - coeff*c(5,4);
      c(3,5) = c(3,5) - coeff*c(5,5);
      r(3)   = r(3)   - coeff*r(5);

      coeff = lhs(4,5);
      c(4,1) = c(4,1) - coeff*c(5,1);
      c(4,2) = c(4,2) - coeff*c(5,2);
      c(4,3) = c(4,3) - coeff*c(5,3);
      c(4,4) = c(4,4) - coeff*c(5,4);
      c(4,5) = c(4,5) - coeff*c(5,5);
      r(4)   = r(4)   - coeff*r(5);


      return;
}



//---------------------------------------------------------------------
//---------------------------------------------------------------------

void binvrhs( double lhs[],double r[] ) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     
//---------------------------------------------------------------------

      double pivot, coeff;

//---------------------------------------------------------------------
//     
//---------------------------------------------------------------------


      pivot = 1.00e0/lhs(1,1);
      lhs(1,2) = lhs(1,2)*pivot;
      lhs(1,3) = lhs(1,3)*pivot;
      lhs(1,4) = lhs(1,4)*pivot;
      lhs(1,5) = lhs(1,5)*pivot;
      r(1)   = r(1)  *pivot;

      coeff = lhs(2,1);
      lhs(2,2)= lhs(2,2) - coeff*lhs(1,2);
      lhs(2,3)= lhs(2,3) - coeff*lhs(1,3);
      lhs(2,4)= lhs(2,4) - coeff*lhs(1,4);
      lhs(2,5)= lhs(2,5) - coeff*lhs(1,5);
      r(2)   = r(2)   - coeff*r(1);

      coeff = lhs(3,1);
      lhs(3,2)= lhs(3,2) - coeff*lhs(1,2);
      lhs(3,3)= lhs(3,3) - coeff*lhs(1,3);
      lhs(3,4)= lhs(3,4) - coeff*lhs(1,4);
      lhs(3,5)= lhs(3,5) - coeff*lhs(1,5);
      r(3)   = r(3)   - coeff*r(1);

      coeff = lhs(4,1);
      lhs(4,2)= lhs(4,2) - coeff*lhs(1,2);
      lhs(4,3)= lhs(4,3) - coeff*lhs(1,3);
      lhs(4,4)= lhs(4,4) - coeff*lhs(1,4);
      lhs(4,5)= lhs(4,5) - coeff*lhs(1,5);
      r(4)   = r(4)   - coeff*r(1);

      coeff = lhs(5,1);
      lhs(5,2)= lhs(5,2) - coeff*lhs(1,2);
      lhs(5,3)= lhs(5,3) - coeff*lhs(1,3);
      lhs(5,4)= lhs(5,4) - coeff*lhs(1,4);
      lhs(5,5)= lhs(5,5) - coeff*lhs(1,5);
      r(5)   = r(5)   - coeff*r(1);


      pivot = 1.00e0/lhs(2,2);
      lhs(2,3) = lhs(2,3)*pivot;
      lhs(2,4) = lhs(2,4)*pivot;
      lhs(2,5) = lhs(2,5)*pivot;
      r(2)   = r(2)  *pivot;

      coeff = lhs(1,2);
      lhs(1,3)= lhs(1,3) - coeff*lhs(2,3);
      lhs(1,4)= lhs(1,4) - coeff*lhs(2,4);
      lhs(1,5)= lhs(1,5) - coeff*lhs(2,5);
      r(1)   = r(1)   - coeff*r(2);

      coeff = lhs(3,2);
      lhs(3,3)= lhs(3,3) - coeff*lhs(2,3);
      lhs(3,4)= lhs(3,4) - coeff*lhs(2,4);
      lhs(3,5)= lhs(3,5) - coeff*lhs(2,5);
      r(3)   = r(3)   - coeff*r(2);

      coeff = lhs(4,2);
      lhs(4,3)= lhs(4,3) - coeff*lhs(2,3);
      lhs(4,4)= lhs(4,4) - coeff*lhs(2,4);
      lhs(4,5)= lhs(4,5) - coeff*lhs(2,5);
      r(4)   = r(4)   - coeff*r(2);

      coeff = lhs(5,2);
      lhs(5,3)= lhs(5,3) - coeff*lhs(2,3);
      lhs(5,4)= lhs(5,4) - coeff*lhs(2,4);
      lhs(5,5)= lhs(5,5) - coeff*lhs(2,5);
      r(5)   = r(5)   - coeff*r(2);


      pivot = 1.00e0/lhs(3,3);
      lhs(3,4) = lhs(3,4)*pivot;
      lhs(3,5) = lhs(3,5)*pivot;
      r(3)   = r(3)  *pivot;

      coeff = lhs(1,3);
      lhs(1,4)= lhs(1,4) - coeff*lhs(3,4);
      lhs(1,5)= lhs(1,5) - coeff*lhs(3,5);
      r(1)   = r(1)   - coeff*r(3);

      coeff = lhs(2,3);
      lhs(2,4)= lhs(2,4) - coeff*lhs(3,4);
      lhs(2,5)= lhs(2,5) - coeff*lhs(3,5);
      r(2)   = r(2)   - coeff*r(3);

      coeff = lhs(4,3);
      lhs(4,4)= lhs(4,4) - coeff*lhs(3,4);
      lhs(4,5)= lhs(4,5) - coeff*lhs(3,5);
      r(4)   = r(4)   - coeff*r(3);

      coeff = lhs(5,3);
      lhs(5,4)= lhs(5,4) - coeff*lhs(3,4);
      lhs(5,5)= lhs(5,5) - coeff*lhs(3,5);
      r(5)   = r(5)   - coeff*r(3);


      pivot = 1.00e0/lhs(4,4);
      lhs(4,5) = lhs(4,5)*pivot;
      r(4)   = r(4)  *pivot;

      coeff = lhs(1,4);
      lhs(1,5)= lhs(1,5) - coeff*lhs(4,5);
      r(1)   = r(1)   - coeff*r(4);

      coeff = lhs(2,4);
      lhs(2,5)= lhs(2,5) - coeff*lhs(4,5);
      r(2)   = r(2)   - coeff*r(4);

      coeff = lhs(3,4);
      lhs(3,5)= lhs(3,5) - coeff*lhs(4,5);
      r(3)   = r(3)   - coeff*r(4);

      coeff = lhs(5,4);
      lhs(5,5)= lhs(5,5) - coeff*lhs(4,5);
      r(5)   = r(5)   - coeff*r(4);


      pivot = 1.00e0/lhs(5,5);
      r(5)   = r(5)  *pivot;

      coeff = lhs(1,5);
      r(1)   = r(1)   - coeff*r(5);

      coeff = lhs(2,5);
      r(2)   = r(2)   - coeff*r(5);

      coeff = lhs(3,5);
      r(3)   = r(3)   - coeff*r(5);

      coeff = lhs(4,5);
      r(4)   = r(4)   - coeff*r(5);


      return;
}



