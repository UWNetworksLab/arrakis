#include <stdio.h>
#include "applu_share.h"

void read_input() {

  int  root;

//c---------------------------------------------------------------------
//c    only root does initializtion, it uses defaults
//c       ipr = 1 for detailed progress output
//c       inorm = how often the norm is printed (once every inorm iterations)
//c       itmax = number of pseudo time steps
//c       dt = time step
//c       omega 1 over-relaxation factor for SSOR
//c       tolrsd = steady state residual tolerance levels
//c       nx, ny, nz = number of grid points in x, y, z directions
//c---------------------------------------------------------------------
   root = 0;


         ipr = ipr_default;
         inorm = inorm_default;
         itmax = itmax_default;
         dt = dt_default;
         omega = omega_default;
         tolrsd[0] = tolrsd1_def;
         tolrsd[1] = tolrsd2_def;
         tolrsd[2] = tolrsd3_def;
         tolrsd[3] = tolrsd4_def;
         tolrsd[4] = tolrsd5_def;
         nx0 = isiz01;
         ny0 = isiz02;
         nz0 = isiz03;
      if (id == root) { 
//c---------------------------------------------------------------------
//c   check problem size
//c---------------------------------------------------------------------

         if (num != nnodes_compiled) {
            printf("Warning: program is running on %d processors ", num);
            printf("but was compiled for %d\n", nnodes_compiled);
         }

         if ( nx0 < 4 || ny0 < 4 || nz0 < 4 ) {
            printf("PROBLEM SIZE IS TOO SMALL\n");
         }

         if (nx0 > isiz01 || ny0 > isiz02 || nz0 > isiz03 ) {
            printf("PROBLEM SIZE IS TOO LARGE\n");
         }

         printf("\nNAS Parallel Benchmarks 3.3 -- LU Benchmark\n");
         printf("Size: %d %d %d\n", nx0, ny0, nz0);
         printf("Iterations: %d\n", itmax);
         printf("Number of processes: %d\n", num);
         
      }

      return;
}



