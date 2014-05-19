#include "applu.h"
#include <string.h>
#include "applu_macros.h"
#include <barrelfish/barrelfish.h>

double test_u();
double test_rsd();

//!-------------------------------------------------------------------------!
//!                                                                         !
//!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.3         !
//!                                                                         !
//!                                   L U                                   !
//!                                                                         !
//!-------------------------------------------------------------------------!
//!                                                                         !
//!    This benchmark is part of the NAS Parallel Benchmark 3.3 suite.      !
//!    It is described in NAS Technical Reports 95-020 and 02-007           !
//!                                                                         !
//!    Permission to use, copy, distribute and modify this software         !
//!    for any purpose with or without fee is hereby granted.  We           !
//!    request, however, that all derived work reference the NAS            !
//!    Parallel Benchmarks 3.3. This software is provided "as is"           !
//!    without express or implied warranty.                                 !
//!                                                                         !
//!    Information on NPB 3.3, including the technical report, the          !
//!    original specifications, source code, results and information        !
//!    on how to submit new results, is available at:                       !
//!                                                                         !
//!           http://www.nas.nasa.gov/Software/NPB/                         !
//!                                                                         !
//!    Send comments or suggestions to  npb@nas.nasa.gov                    !
//!                                                                         !
//!          NAS Parallel Benchmarks Group                                  !
//!          NASA Ames Research Center                                      !
//!          Mail Stop: T27A-1                                              !
//!          Moffett Field, CA   94035-1000                                 !
//!                                                                         !
//!          E-mail:  npb@nas.nasa.gov                                      !
//!          Fax:     (650) 604-3957                                        !
//!                                                                         !
//!-------------------------------------------------------------------------!

//c---------------------------------------------------------------------
//c
//c Authors: S. Weeratunga
//c          V. Venkatakrishnan
//c          E. Barszcz
//c          M. Yarrow
//c C-version: Rob Van der Wijngaart, Intel Corporation
//c
//c---------------------------------------------------------------------


int LU_main(int argc, char **argv){

//c---------------------------------------------------------------------
//c
//c   driver for the performance evaluation of the solver for
//c   five coupled parabolic/elliptic partial differential equations.
//c
//c---------------------------------------------------------------------



      char class;
      double mflops;
      int ierr, i, j, k, mm, iverified = 1;

//c---------------------------------------------------------------------
//c   initialize communications
//c---------------------------------------------------------------------
       init_comm(&argc, &argv);
//       RCCE_debug_set(RCCE_DEBUG_SYNCH);
//c---------------------------------------------------------------------
//c   read input data
//c---------------------------------------------------------------------
       read_input();

//c---------------------------------------------------------------------
//c   set up processor grid
//c---------------------------------------------------------------------
       proc_grid();

//c---------------------------------------------------------------------
//c   determine the neighbors
//c---------------------------------------------------------------------
       neighbors();

//c---------------------------------------------------------------------
//c   set up sub-domain sizes
//c---------------------------------------------------------------------
       subdomain();

//c---------------------------------------------------------------------
//c   set up coefficients
//c---------------------------------------------------------------------
       setcoeff();

//c---------------------------------------------------------------------
//c   set the boundary values for dependent variables
//c---------------------------------------------------------------------

       setbv();

//c---------------------------------------------------------------------
//c   set the initial values for dependent variables
//c---------------------------------------------------------------------

       setiv();

//c---------------------------------------------------------------------
//c   compute the forcing term based on prescribed exact solution
//c---------------------------------------------------------------------
       erhs();

////c---------------------------------------------------------------------
////c   perform one SSOR iteration to touch all data and program pages 
////c---------------------------------------------------------------------
       ssor(1);

//
////c---------------------------------------------------------------------
////c   reset the boundary and initial values
////c---------------------------------------------------------------------
       setbv();
       setiv();
//
////c---------------------------------------------------------------------
////c   perform the SSOR iterations
////c---------------------------------------------------------------------
       ssor(itmax);

////c---------------------------------------------------------------------
////c   compute the solution error
////c---------------------------------------------------------------------
        error();

////c---------------------------------------------------------------------
////c   compute the surface integral
////c---------------------------------------------------------------------
      pintgr();
//
////c---------------------------------------------------------------------
////c   verification test
////c---------------------------------------------------------------------

      if (id ==0) {
        verify( rsdnm, errnm, &frc, &class );
         mflops = (double)(itmax)*(1984.77*(double)( nx0 )
              *(double)( ny0 )
              *(double)( nz0 )
              -10923.3*((double)( nx0+ny0+nz0 )/3.)*((double)( nx0+ny0+nz0 )/3.)
              +27770.9* (double)( nx0+ny0+nz0 )/3.
              -144010.)
              / (maxtime*1000000.);

          print_results("LU", &class, &nx0,
           &ny0, &nz0, &itmax, &nnodes_compiled,
           &num, &maxtime, &mflops, "          floating point", &iverified, 
           NPBVERSION, COMPILETIME, CS1, CS2, CS3, CS4, CS5, CS6);

//         FILE *perf_file;
//         char name[50] = "/shared/DEMOS/RCCE/NPB_LU/perf."; 
//         char postfix[50]; 
//         sprintf(postfix, "%d", nnodes_compiled); 
//         strcat(name, postfix); 
//         perf_file = fopen(name,"w"); 
//         fprintf(perf_file, "%d", (int)mflops); 
//         fclose(perf_file); 
      }      

      RCCE_finalize();
      return(0);
}

/// LU program stack size
#define LU_STACK_SIZE   (1 * 1024 * 1024)

struct args {
    int argc;
    char **argv;
};

static int lu_bootstrap(void *arg)
{
    errval_t err = thread_detach(thread_self());
    assert(err_is_ok(err));
    struct args *a = arg;
    return LU_main(a->argc, a->argv);
}

int main(int argc, char **argv)
{
    static struct args a;

    a.argc = argc;
    a.argv = argv;

    thread_create_varstack(lu_bootstrap, &a, LU_STACK_SIZE);
    thread_exit();
}
