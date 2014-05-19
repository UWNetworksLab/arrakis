#include "applu_share.h"
#include "RCCE.h"

void neighbors() {

//c---------------------------------------------------------------------
//c     figure out the neighbors and their wrap numbers for each processor
//c---------------------------------------------------------------------

      south = -1;
      east  = -1;
      north = -1;
      west  = -1;

      if (row  >1)    north = id -1;
      else            north = -1;

      if (row < xdim) south = id + 1;
      else            south = -1;

      if (col > 1)    west = id- xdim;
      else            west = -1;

      if (col < ydim) east = id + xdim;
      else            east = -1;

      return;
}
