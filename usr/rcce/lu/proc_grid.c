#include <math.h>
#include "applu_share.h"

void proc_grid() {

//c   set up a two-d grid for processors: column-major ordering of unknowns
//c   NOTE: assumes a power-of-two number of processors

      xdim   = pow(2,(ndim/2));
      if (ndim%2 == 1) xdim = xdim + xdim;
      ydim   = num/xdim;

      row    = id%xdim + 1;
      col    = id/xdim + 1;

      return;
}



