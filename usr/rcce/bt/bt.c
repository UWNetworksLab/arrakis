//-------------------------------------------------------------------------!
//                                                                         !
//        N  A  S     P A R A L L E L     B E N C H M A R K S  3.3         !
//                                                                         !
//                                   B T                                   !
//                                                                         !
//-------------------------------------------------------------------------!
//                                                                         !
//    This benchmark is part of the NAS Parallel Benchmark 3.3 suite.      !
//    It is described in NAS Technical Reports 95-020 and 02-007.          !
//                                                                         !
//    Permission to use, copy, distribute and modify this software         !
//    for any purpose with or without fee is hereby granted.  We           !
//    request, however, that all derived work reference the NAS            !
//    Parallel Benchmarks 3.3. This software is provided "as is"           !
//    without express or implied warranty.                                 !
//                                                                         !
//    Information on NPB 3.3, including the technical report, the          !
//    original specifications, source code, results and information        !
//    on how to submit new results, is available at:                       !
//                                                                         !
//           http://www.nas.nasa.gov/Software/NPB/                         !
//                                                                         !
//    Send comments or suggestions to  npb@nas.nasa.gov                    !
//                                                                         !
//          NAS Parallel Benchmarks Group                                  !
//          NASA Ames Research Center                                      !
//          Mail Stop: T27A-1                                              !
//          Moffett Field, CA   94035-1000                                 !
//                                                                         !
//          E-mail:  npb@nas.nasa.gov                                      !
//          Fax:     (650) 604-3957                                        !
//                                                                         !
//-------------------------------------------------------------------------!

//---------------------------------------------------------------------
//
// Authors: R. F. Van der Wijngaart
//          T. Harris
//          M. Yarrow
//
//---------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "RCCE.h"
#include "applu_macros.h"
#define G_MAIN
#include "header.h"
#include "mpinpb.h"

#define BSIZE 132
void make_color(void);
void print_results(char*, char, int, int, int, int, int, int, double,
                   double, char*, int, char*, char*, char*, char*, 
                   char*, char*, char*, char*);

//---------------------------------------------------------------------
//      program MPBT;
//---------------------------------------------------------------------
int RCCE_APP(int argc, char **argv) {

       int N = 1000, nothing;
       int i, niter, step, c, error, fstatus;
       double navg, mflops, mbytes, n3;
       /* RCCE_COMM aux[N]; */

       double t, tmax, tiominv, tpc;
       int verified;
       char class;
       size_t chunk;

       char cbuf[BSIZE];

       if (setup_mpi(&argc, &argv)) {
       RCCE_finalize();
       return 0;
       }

//       RCCE_debug_set(RCCE_DEBUG_ALL);

//---------------------------------------------------------------------
//      Root node reads input file (if it exists) else takes
//      defaults from parameters
//---------------------------------------------------------------------
       if (node == root) {
          
          printf("\n\n NAS Parallel Benchmarks 3.3 -- BT Benchmark\n");

       }
          niter = NITER_DEFAULT;
          dt    = dt_default;
          grid_points(1) = PROBLEM_SIZE;
          grid_points(2) = PROBLEM_SIZE;
          grid_points(3) = PROBLEM_SIZE;

       if (node == root) {
          printf(" Size: %4dx%4dx%4d\n", 
                 grid_points(1), grid_points(2), grid_points(3));
          printf(" Iterations: %4d    dt: %11.7f\n", niter, dt);
          if (no_nodes != total_nodes)
              printf(" Total number of processes: %5d\n", total_nodes);
          if (no_nodes != MAXCELLS*MAXCELLS) 
              printf(" WARNING: compiled for %5d processes\n",
                     MAXCELLS*MAXCELLS);
          printf(" Number of active processes: %5d\n\n", no_nodes);

       }

       make_set();
       make_color();


       for (c = 1; c <= MAXCELLS; c++) {
          if ( (cell_size(1,c) > IMAX) ||
               (cell_size(2,c) > JMAX) ||
               (cell_size(3,c) > KMAX) ) {
             printf(" %d %d %d %d %d\n", node, c, cell_size(1,c),
                     cell_size(2,c), cell_size(3,c));
             printf(" Problem size too big for compiled array sizes\n");
          }
       }

       set_constants();

       initialize();

       lhsinit();

       exact_rhs();

       compute_buffer_size(5);

//---------------------------------------------------------------------
//      do one time step to touch all code, and reinitialize
//---------------------------------------------------------------------
       adi();

       initialize();

       timer_clear(2);

//---------------------------------------------------------------------
//      Synchronize before placing time stamp
//---------------------------------------------------------------------
       RCCE_barrier(&RCCE_COMM_WORLD);

       timer_clear(1);
       timer_start(1);

       for (step = 1; step <= niter; step++) {

          if (node == root) {
             if ((step%20) == 0 || step == niter ||
                 step == 1) {
                printf(" Time step %4d\n", step);
             }
          }
          adi();
       }

       timer_stop(1);
       t = timer_read(1);
       
       verify(niter, &class, &verified);

       RCCE_reduce((char*)(&t), (char*)(&tmax), 1, RCCE_DOUBLE, RCCE_MAX, root, RCCE_COMM_WORLD);

       if( node == root ) {
          n3 = 1.0e0*grid_points(1)*grid_points(2)*grid_points(3);
          navg = (grid_points(1)+grid_points(2)+grid_points(3))/3.0;
          if( tmax != 0. ) {
             mflops = 1.0e-6*(double)(niter)*
               (3478.8*(double)n3-17655.7*navg*navg+28023.7*navg)
               / tmax;
          } else {
             mflops = 0.0;
          }

         print_results("BT", class, grid_points[0], 
           grid_points[1], grid_points[2], niter, MAXCELLS*MAXCELLS, 
           total_nodes, tmax, mflops, "          floating point", 
           verified, NPBVERSION,COMPILETIME, CS1, CS2, CS3, CS4, CS5, 
           CS6);


//         FILE *perf_file;
//         char name[50] = "/shared/DEMOS/RCCE/NPB_BT/perf."; 
//         char postfix[50]; 
//         sprintf(postfix, "%d", total_nodes); 
//         strcat(name, postfix); 
//         perf_file = fopen(name,"w"); 
//         fprintf(perf_file, "%d", (int)mflops); 
//         fclose(perf_file); 
       }


       RCCE_barrier(&RCCE_COMM_WORLD);
       RCCE_finalize();

       return 0;

}

