//we introduce the next macro to avoid confusing the compiler, which
//sometimes thinks the variable class is a reserved word
#define class _class_
#define a(m,n,j,k)       a[m-1+5*(n-1+5*(j-1+isiz1*(k-1)))]
#define b(m,n,j,k)       b[m-1+5*(n-1+5*(j-1+isiz1*(k-1)))]
#define c(m,n,j,k)       c[m-1+5*(n-1+5*(j-1+isiz1*(k-1)))]
#define d(m,n,j,k)       d[m-1+5*(n-1+5*(j-1+isiz1*(k-1)))]
#define flux(m,i,j,k) flux[m-1+5*((i+0)+(isiz1+2)*((j+0)+(isiz2+2)*(k-1)))]
#define frct(m,i,j,k) frct[m-1+5*((i+1)+(isiz1+4)*((j+1)+(isiz2+4)*(k-1)))]
#define rsd(m,i,j,k)   rsd[m-1+5*((i+1)+(isiz1+4)*((j+1)+(isiz2+4)*(k-1)))]
#define u(m,i,j,k)       u[m-1+5*((i+1)+(isiz1+4)*((j+1)+(isiz2+4)*(k-1)))]

#define tv(m,i,j)     tv[m-1+5*((i-1)+isiz1*(j-1))]

#define phi1(i,k)     phi1[i+(isiz2+2)*(k)] 
#define phi2(i,k)     phi2[i+(isiz2+2)*(k)] 
#define ce(m,n)       ce[m-1+5*(n-1)]
#define tmat(m,n)     tmat[m-1+5*(n-1)]

#define rsdnm(m)      rsdnm[m-1]
#define tolrsd(m)     tolrsd[m-1]
#define errnm(m)      errnm[m-1]

/* PAD32byte is used to compute a cacheline padded length of n (input) bytes */
#define  PAD32byte(n) ((n)%32==0 ? (n) : (n) + 32 - (n)%32)
/* PAD32dbl is used to compute a cacheline padded length of n (input) doubles */
#define  PAD32dbl(n)  ((n)%(32/sizeof(double))==0 ? (n) : (n) + (32/sizeof(double)) \
                      - (n)%(32/sizeof(double)))

#define max(x,y)      ((x)>(y)? (x) : (y))
#define min(x,y)      ((x)<(y)? (x) : (y))
