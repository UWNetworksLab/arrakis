
//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include <math.h>
#include "mpinpb.h"
#include "npbparams.h"
#include "RCCE.h"

int setup_mpi(int *argc, char **argv[]) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
// set up MPI stuff
//---------------------------------------------------------------------
      int error, color, nc;

      if (error = RCCE_init(argc, argv)) return(error);

      total_nodes = RCCE_num_ues();
      node = RCCE_ue();      

//---------------------------------------------------------------------
//     compute square root; add small number to allow for roundoff
//---------------------------------------------------------------------
      nc = (int)(sqrt((double)(total_nodes) + 0.00001e0));

//---------------------------------------------------------------------
// We handle a non-square number of nodes by making the excess nodes
// inactive. However, we can never handle more cells than were compiled
// in. 
//---------------------------------------------------------------------

      if (nc > MAXCELLS) nc = MAXCELLS;
      no_nodes = nc*nc;      
      
//---------------------------------------------------------------------
//     let node 0 be the root for the group (there is only one)
//---------------------------------------------------------------------
      root = 0;

      return(0);
}

