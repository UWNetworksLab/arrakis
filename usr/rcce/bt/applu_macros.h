/* PAD32byte is used to compute a cacheline padded length of n (input) bytes */
#define  PAD32byte(n) ((n)%32==0 ? (n) : (n) + 32 - (n)%32)
/* PAD32dbl is used to compute a cacheline padded length of n (input) doubles */
#define  PAD32dbl(n)  ((n)%(32/sizeof(double))==0 ? (n) : (n) + (32/sizeof(double)) \
                      - (n)%(32/sizeof(double)))

#define max(x,y)      ((x)>(y)? (x) : (y))
#define min(x,y)      ((x)<(y)? (x) : (y))
