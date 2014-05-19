#include <math.h>
#include "applu_share.h"

int nodedim() {

  int dim;
  double fnum;

  fnum = (double) num;
  dim = (int)(log(fnum)/log(2.0) + 0.00001);
  return(dim);
}



