#include "RCCE.h"
#include "applu_share.h"

void init_comm(int *argc, char ***argv) {

      if(RCCE_init(argc, argv)) {
         RCCE_finalize();
         exit(0);
      }
      id = RCCE_ue();
      num = RCCE_num_ues();
      ndim    = nodedim();

      return;
}
