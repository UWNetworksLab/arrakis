#include "RCCE.h"
#ifdef _OPENMP
#include "omp.h"
#endif
#include "timers.h"
#define elapsed(n) elapsed[n-1]
#define start(n)   start[n-1]
      
void timer_clear(int *np){
      
      int n = *np;
      elapsed(n) = 0.0;
      return;
}


void timer_start(int *np) {

      int n = *np;

      start(n) = RCCE_wtime();

      return;
}

void timer_stop(int *np) {

      int n = *np;

      double t, now;
      now = RCCE_wtime();
      t = now - start(n);
      elapsed(n) = elapsed(n) + t;

      return;
}


double timer_read(int *np) {

      int n = *np;      
      return( elapsed(n));
}

