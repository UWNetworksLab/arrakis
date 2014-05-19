#ifndef __COMMON_H
#define __COMMON_H

extern void timer_clear(int t);
extern void timer_start(int t);
extern void timer_stop(int t);
extern double timer_read(int t);
extern void c_print_results( char   *name,
                      char   class,
                      int    n1, 
                      int    n2,
                      int    n3,
                      int    niter,
                      int    nprocs_compiled,
                      int    nprocs_total,
                      double t,
                      double mops,
		      char   *optype,
                      int    passed_verification,
                      char   *npbversion,
                      char   *compiletime,
                      char   *mpicc,
                      char   *clink,
                      char   *cmpi_lib,
                      char   *cmpi_inc,
                      char   *cflags,
                      char   *clinkflags,
                      char   *randf );

extern double randlc(double *x, double a);
extern void vranlc(int n, double *x, double a, double y[]);

#endif
