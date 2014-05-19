
//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include <stdio.h>
#include "header.h"
#include "mpinpb.h"

#define dabs(x) ((x)<0? -(x):(x))

void verify(int no_time_steps, char *class_r, int *verified_r) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//  verification routine                         
//---------------------------------------------------------------------

        double xcrref[5],xceref[5],xcrdif[5],xcedif[5], 
               epsilon, xce[5], xcr[5], dtref;
        int m;
        char class;
        int verified;
#define xcrref(m) xcrref[m-1]
#define xceref(m) xceref[m-1]
#define xcrdif(m) xcrdif[m-1]
#define xcedif(m) xcedif[m-1]
#define xce(m) xce[m-1]
#define xcr(m) xcr[m-1]

//---------------------------------------------------------------------
//   tolerance level
//---------------------------------------------------------------------
        epsilon = 1.0e-08;
        verified = 1;

//---------------------------------------------------------------------
//   compute the error norm and the residual norm, and exit if not printing
//---------------------------------------------------------------------

        error_norm(xce);

        copy_faces();

        rhs_norm(xcr);

        for (m = 1; m <= 5; m++) {
           xcr(m) = xcr(m) / dt;
        }

        if (node != root) return;

        class = 'U';

        for (m = 1; m <= 5; m++) {
           xcrref(m) = 1.0;
           xceref(m) = 1.0;
        }

//---------------------------------------------------------------------
//    reference data for 12X12X12 grids after 60 time steps, with DT = 1.0e-02
//---------------------------------------------------------------------
        if ( (grid_points(1)  == 12     ) && 
             (grid_points(2)  == 12     ) &&
             (grid_points(3)  == 12     ) &&
             (no_time_steps   == 60    )) {

           class = 'S';
           dtref = 1.0e-2;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of residual.
//---------------------------------------------------------------------
         xcrref(1) = 1.7034283709541311e-01;
         xcrref(2) = 1.2975252070034097e-02;
         xcrref(3) = 3.2527926989486055e-02;
         xcrref(4) = 2.6436421275166801e-02;
         xcrref(5) = 1.9211784131744430e-01;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of solution error.
//---------------------------------------------------------------------

           xceref(1) = 4.9976913345811579e-04;
           xceref(2) = 4.5195666782961927e-05;
           xceref(3) = 7.3973765172921357e-05;
           xceref(4) = 7.3821238632439731e-05;
           xceref(5) = 8.9269630987491446e-04;

//---------------------------------------------------------------------
//    reference data for 24X24X24 grids after 200 time steps, with DT = 0.8e-3
//---------------------------------------------------------------------
        } else if ( (grid_points(1) == 24) && 
                 (grid_points(2) == 24) &&
                 (grid_points(3) == 24) &&
                 (no_time_steps == 200) ) {

           class = 'W';
           dtref = 0.8e-3;
//---------------------------------------------------------------------
//  Reference values of RMS-norms of residual.
//---------------------------------------------------------------------
           xcrref(1) = 0.1125590409344e+03;
           xcrref(2) = 0.1180007595731e+02;
           xcrref(3) = 0.2710329767846e+02;
           xcrref(4) = 0.2469174937669e+02;
           xcrref(5) = 0.2638427874317e+03;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of solution error.
//---------------------------------------------------------------------

           xceref(1) = 0.4419655736008e+01;
           xceref(2) = 0.4638531260002e+00;
           xceref(3) = 0.1011551749967e+01;
           xceref(4) = 0.9235878729944e+00;
           xceref(5) = 0.1018045837718e+02;

//---------------------------------------------------------------------
//    reference data for 64X64X64 grids after 200 time steps, with DT = 0.8e-3
//---------------------------------------------------------------------
        } else if ( (grid_points(1) == 64) && 
                 (grid_points(2) == 64) &&
                 (grid_points(3) == 64) &&
                 (no_time_steps == 200) ) {

           class = 'A';
           dtref = 0.8e-3;
//---------------------------------------------------------------------
//  Reference values of RMS-norms of residual.
//---------------------------------------------------------------------
         xcrref(1) = 1.0806346714637264e+02;
         xcrref(2) = 1.1319730901220813e+01;
         xcrref(3) = 2.5974354511582465e+01;
         xcrref(4) = 2.3665622544678910e+01;
         xcrref(5) = 2.5278963211748344e+02;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of solution error.
//---------------------------------------------------------------------

           xceref(1) = 4.2348416040525025e+00;
           xceref(2) = 4.4390282496995698e-01;
           xceref(3) = 9.6692480136345650e-01;
           xceref(4) = 8.8302063039765474e-01;
           xceref(5) = 9.7379901770829278e+00;

//---------------------------------------------------------------------
//    reference data for 102X102X102 grids after 200 time steps,
//    with DT = 3.0e-04
//---------------------------------------------------------------------
        } else if ( (grid_points(1) == 102) && 
                 (grid_points(2) == 102) &&
                 (grid_points(3) == 102) &&
                 (no_time_steps == 200) ) {

           class = 'B';
           dtref = 3.0e-4;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of residual.
//---------------------------------------------------------------------
         xcrref(1) = 1.4233597229287254e+03;
         xcrref(2) = 9.9330522590150238e+01;
         xcrref(3) = 3.5646025644535285e+02;
         xcrref(4) = 3.2485447959084092e+02;
         xcrref(5) = 3.2707541254659363e+03;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of solution error.
//---------------------------------------------------------------------

           xceref(1) = 5.2969847140936856e+01;
           xceref(2) = 4.4632896115670668e+00;
           xceref(3) = 1.3122573342210174e+01;
           xceref(4) = 1.2006925323559144e+01;
           xceref(5) = 1.2459576151035986e+02;

//---------------------------------------------------------------------
//    reference data for 162X162X162 grids after 200 time steps,
//    with DT = 1.0e-04
//---------------------------------------------------------------------
        } else if ( (grid_points(1) == 162) && 
                 (grid_points(2) == 162) &&
                 (grid_points(3) == 162) &&
                 (no_time_steps == 200) ) {

           class = 'C';
           dtref = 1.0e-4;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of residual.
//---------------------------------------------------------------------
         xcrref(1) = 0.62398116551764615e+04;
         xcrref(2) = 0.50793239190423964e+03;
         xcrref(3) = 0.15423530093013596e+04;
         xcrref(4) = 0.13302387929291190e+04;
         xcrref(5) = 0.11604087428436455e+05;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of solution error.
//---------------------------------------------------------------------

           xceref(1) = 0.16462008369091265e+03;
           xceref(2) = 0.11497107903824313e+02;
           xceref(3) = 0.41207446207461508e+02;
           xceref(4) = 0.37087651059694167e+02;
           xceref(5) = 0.36211053051841265e+03;

//---------------------------------------------------------------------
//    reference data for 408x408x408 grids after 250 time steps,
//    with DT = 0.2e-04
//---------------------------------------------------------------------
        } else if ( (grid_points(1) == 408) && 
                 (grid_points(2) == 408) &&
                 (grid_points(3) == 408) &&
                 (no_time_steps == 250) ) {

           class = 'D';
           dtref = 0.2e-4;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of residual.
//---------------------------------------------------------------------
         xcrref(1) = 0.2533188551738e+05;
         xcrref(2) = 0.2346393716980e+04;
         xcrref(3) = 0.6294554366904e+04;
         xcrref(4) = 0.5352565376030e+04;
         xcrref(5) = 0.3905864038618e+05;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of solution error.
//---------------------------------------------------------------------

           xceref(1) = 0.3100009377557e+03;
           xceref(2) = 0.2424086324913e+02;
           xceref(3) = 0.7782212022645e+02;
           xceref(4) = 0.6835623860116e+02;
           xceref(5) = 0.6065737200368e+03;

//---------------------------------------------------------------------
//    reference data for 1020x1020x1020 grids after 250 time steps,
//    with DT = 0.4e-05
//---------------------------------------------------------------------
        } else if ( (grid_points(1) == 1020) && 
                 (grid_points(2) == 1020) &&
                 (grid_points(3) == 1020) &&
                 (no_time_steps == 250) ) {

           class = 'E';
           dtref = 0.4e-5;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of residual.
//---------------------------------------------------------------------
         xcrref(1) = 0.9795372484517e+05;
         xcrref(2) = 0.9739814511521e+04;
         xcrref(3) = 0.2467606342965e+05;
         xcrref(4) = 0.2092419572860e+05;
         xcrref(5) = 0.1392138856939e+06;

//---------------------------------------------------------------------
//  Reference values of RMS-norms of solution error.
//---------------------------------------------------------------------

           xceref(1) = 0.4327562208414e+03;
           xceref(2) = 0.3699051964887e+02;
           xceref(3) = 0.1089845040954e+03;
           xceref(4) = 0.9462517622043e+02;
           xceref(5) = 0.7765512765309e+03;

        } else {
           verified = 0;
        }

//---------------------------------------------------------------------
//    verification test for residuals if gridsize is one of 
//    the defined grid sizes above (class != 'U')
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//    Compute the difference of solution values and the known reference 
//    values.
//---------------------------------------------------------------------
        for (m = 1; m <= 5; m++) {
           
           xcrdif(m) = dabs((xcr(m)-xcrref(m))/xcrref(m)) ;
           xcedif(m) = dabs((xce(m)-xceref(m))/xceref(m));
           
        }

//---------------------------------------------------------------------
//    Output the comparison of computed results to known cases.
//---------------------------------------------------------------------

        if (class != 'U') {
           printf(" Verification being performed for class %c\n", class);
           printf(" accuracy setting for epsilon = %20.13e\n", epsilon);
           verified = (dabs(dt-dtref) <= epsilon);
           if (!verified) {
              class = 'U';
              printf(" DT does not match the reference value of %15.8e\n", 
                       dtref);
           }
        } else {
           printf(" Unknown class\n");
        }


        if (class != 'U') {
           printf(" Comparison of RMS-norms of residual\n");
        } else {
           printf(" RMS-norms of residual\n");
        }

        for (m = 1; m <= 5; m++) {
           if (class == 'U') {
              printf("          %2d %20.13e\n",
                      m, xcr(m));
           } else if (xcrdif(m) <= epsilon) {
              printf("          %2d %20.13e %20.13e %20.13e\n",
                      m,xcr(m),xcrref(m),xcrdif(m));
           } else {
              verified = 0;
              printf(" FAILURE: %2d %20.13e %20.13e %20.13e\n",
                      m,xcr(m),xcrref(m),xcrdif(m));
           }
        }

        if (class != 'U') {
           printf(" Comparison of RMS-norms of solution error\n");
        } else {
           printf(" RMS-norms of solution error\n");
        }
        
        for (m = 1; m <= 5; m++) {
           if (class == 'U') {
              printf("          %2d %20.13e\n",
                      m, xce(m));
           } else if (xcedif(m) <= epsilon) {
              printf("          %2d %20.13e %20.13e %20.13e\n",
                      m,xce(m),xceref(m),xcedif(m));
           } else {
              verified = 0;
              printf(" FAILURE: %2d %20.13e %20.13e %20.13e\n",
                      m,xce(m),xceref(m),xcedif(m));
           }
        }
        
        
        if (class == 'U') {
           printf(" No reference values provided\n");
           printf(" No verification performed\n");
        } else if (verified) {
           printf(" Verification Successful\n");
        } else {
           printf(" Verification failed\n");
        }

        *class_r = class;
        *verified_r = verified;

        return;


}
