//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include "header.h"
#include "mpinpb.h"

void copy_faces() {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     
// This function copies the face values of a variable defined on a set 
// of cells to the overlap locations of the adjacent sets of cells. 
// Because a set of cells interfaces in each direction with exactly one 
// other set, we only need to fill six different buffers. We could try to
// overlap communication with computation, by computing
// some internal values while communicating boundary values, but this
// adds so much overhead that it's not clearly useful. 
//---------------------------------------------------------------------

      int i, j, k, c, m, p0, p1, phase,
           p2, p3, p4, p5, b_size[6], ss[6], 
           sr[6], error;

#define b_size(m) b_size[m]
#define ss(m) ss[m]
#define sr(m) sr[m]

//---------------------------------------------------------------------
//     exit immediately if there are no faces to be copied           
//---------------------------------------------------------------------
      if (no_nodes == 1) {
         compute_rhs();
         return;
      }

      ss(0) = start_send_east;
      ss(1) = start_send_west;
      ss(2) = start_send_north;
      ss(3) = start_send_south;
      ss(4) = start_send_top;
      ss(5) = start_send_bottom;

      sr(0) = start_recv_east;
      sr(1) = start_recv_west;
      sr(2) = start_recv_north;
      sr(3) = start_recv_south;
      sr(4) = start_recv_top;
      sr(5) = start_recv_bottom;

      b_size(0) = east_size   ;
      b_size(1) = west_size   ;
      b_size(2) = north_size  ;
      b_size(3) = south_size  ;
      b_size(4) = top_size    ;
      b_size(5) = bottom_size ;

//---------------------------------------------------------------------
//     because the difference stencil for the diagonalized scheme is 
//     orthogonal, we do not have to perform the staged copying of faces,
//     but can send all face information simultaneously to the neighboring
//     cells in all directions          
//---------------------------------------------------------------------
      p0 = 0;
      p1 = 0;
      p2 = 0;
      p3 = 0;
      p4 = 0;
      p5 = 0;

      for (c = 1; c <= ncells; c++) {

//---------------------------------------------------------------------
//     fill the buffer to be sent to eastern neighbors (i-dir)
//---------------------------------------------------------------------
         if (cell_coord(1,c) != ncells) {
            for (k = 0; k <= cell_size(3,c)-1; k++) {
               for (j = 0; j <= cell_size(2,c)-1; j++) {
                  for (i = cell_size(1,c)-2; i <= cell_size(1,c)-1; i++) {
                     for (m = 1; m <= 5; m++) {
                        out_buffer(ss(0)+p0) = u(m,i,j,k,c);
                        p0 = p0 + 1;
                     }
                  }
               }
            }
         }

//---------------------------------------------------------------------
//     fill the buffer to be sent to western neighbors 
//---------------------------------------------------------------------
         if (cell_coord(1,c) != 1) {
            for (k = 0; k <= cell_size(3,c)-1; k++) {
               for (j = 0; j <= cell_size(2,c)-1; j++) {
                  for (i = 0; i <= 1; i++) {
                     for (m = 1; m <= 5; m++) {
                        out_buffer(ss(1)+p1) = u(m,i,j,k,c);
                        p1 = p1 + 1;
                     }
                  }
               }
            }

         }

//---------------------------------------------------------------------
//     fill the buffer to be sent to northern neighbors (j_dir)
//---------------------------------------------------------------------
         if (cell_coord(2,c) != ncells) {
            for (k = 0; k <= cell_size(3,c)-1; k++) {
               for (j = cell_size(2,c)-2; j <= cell_size(2,c)-1; j++) {
                  for (i = 0; i <= cell_size(1,c)-1; i++) {
                     for (m = 1; m <= 5; m++) {
                        out_buffer(ss(2)+p2) = u(m,i,j,k,c);
                        p2 = p2 + 1;
                     }
                  }
               }
            }
         }

//---------------------------------------------------------------------
//     fill the buffer to be sent to southern neighbors 
//---------------------------------------------------------------------
         if (cell_coord(2,c)!= 1) {
            for (k = 0; k <= cell_size(3,c)-1; k++) {
               for (j = 0; j <= 1; j++) {
                  for (i = 0; i <= cell_size(1,c)-1; i++) {
                     for (m = 1; m <= 5; m++) {
                        out_buffer(ss(3)+p3) = u(m,i,j,k,c);
                        p3 = p3 + 1;
                     }
                  }
               }
            }
         }

//---------------------------------------------------------------------
//     fill the buffer to be sent to top neighbors (k-dir)
//---------------------------------------------------------------------
         if (cell_coord(3,c) != ncells) {
            for (k = cell_size(3,c)-2; k <= cell_size(3,c)-1; k++) {
               for (j = 0; j <= cell_size(2,c)-1; j++) {
                  for (i = 0; i <= cell_size(1,c)-1; i++) {
                     for (m = 1; m <= 5; m++) {
                        out_buffer(ss(4)+p4) = u(m,i,j,k,c);
                        p4 = p4 + 1;
                     }
                  }
               }
            }
         }

//---------------------------------------------------------------------
//     fill the buffer to be sent to bottom neighbors
//---------------------------------------------------------------------
         if (cell_coord(3,c)!= 1) {
            for (k = 0; k <= 1; k++) {
               for (j = 0; j <= cell_size(2,c)-1; j++) {
                  for (i = 0; i <= cell_size(1,c)-1; i++) {
                     for (m = 1; m <= 5; m++) {
                        out_buffer(ss(5)+p5) = u(m,i,j,k,c);
                        p5 = p5 + 1;
                     }
                  }
               }
            }
         }

//---------------------------------------------------------------------
//     cell loop
//---------------------------------------------------------------------
      }

      for (phase = 0; phase < 3; phase++) {

      if (send_color[WESTDIR]==phase)  {
        RCCE_send((char*)(&out_buffer(ss(1))), b_size(1)*sizeof(double), predecessor(1));
      }
      if (recv_color[WESTDIR]==phase)  {
        RCCE_recv((char*)(&in_buffer(sr(0))),  b_size(0)*sizeof(double), successor(1));
      }

      if (send_color[EASTDIR]==phase)  {
        RCCE_send((char*)(&out_buffer(ss(0))), b_size(0)*sizeof(double), successor(1));
      }
      if (recv_color[EASTDIR]==phase)  {
        RCCE_recv((char*)(&in_buffer(sr(1))),  b_size(1)*sizeof(double), predecessor(1));
      }

      if (send_color[SOUTHDIR]==phase)  {
        RCCE_send((char*)(&out_buffer(ss(3))), b_size(3)*sizeof(double), predecessor(2));
      }
      if (recv_color[SOUTHDIR]==phase)  {
        RCCE_recv((char*)(&in_buffer(sr(2))),  b_size(2)*sizeof(double), successor(2));
      }

      if (send_color[NORTHDIR]==phase)  {
        RCCE_send((char*)(&out_buffer(ss(2))), b_size(2)*sizeof(double),successor(2));
      }
      if (recv_color[NORTHDIR]==phase)  {
        RCCE_recv((char*)(&in_buffer(sr(3))),  b_size(3)*sizeof(double), predecessor(2));
      }

      if (send_color[BOTTOMDIR]==phase)  {
        RCCE_send((char*)(&out_buffer(ss(5))), b_size(5)*sizeof(double),predecessor(3));
      }
      if (recv_color[BOTTOMDIR]==phase)  {
        RCCE_recv((char*)(&in_buffer(sr(4))),  b_size(4)*sizeof(double), successor(3));
      }

      if (send_color[TOPDIR]==phase)  {
        RCCE_send((char*)(&out_buffer(ss(4))), b_size(4)*sizeof(double),successor(3));
      }
      if (recv_color[TOPDIR]==phase)  {
        RCCE_recv((char*)(&in_buffer(sr(5))),  b_size(5)*sizeof(double), predecessor(3));
      }
   }      

//---------------------------------------------------------------------
//     unpack the data that has just been received;             
//---------------------------------------------------------------------
      p0 = 0;
      p1 = 0;
      p2 = 0;
      p3 = 0;
      p4 = 0;
      p5 = 0;

      for (c = 1; c <= ncells; c++) {

         if (cell_coord(1,c) != 1) {
            for (k = 0; k <= cell_size(3,c)-1; k++) {
               for (j = 0; j <= cell_size(2,c)-1; j++) {
                  for (i = -2; i <= -1; i++) {
                     for (m = 1; m <= 5; m++) {
                        u(m,i,j,k,c) = in_buffer(sr(1)+p0);
                        p0 = p0 + 1;
                     }
                  }
               }
            }
         }

         if (cell_coord(1,c) != ncells) {
            for (k = 0; k <= cell_size(3,c)-1; k++) {
               for (j = 0; j <= cell_size(2,c)-1; j++) {
                  for (i = cell_size(1,c); i <= cell_size(1,c)+1; i++) {
                     for (m = 1; m <= 5; m++) {
                        u(m,i,j,k,c) = in_buffer(sr(0)+p1);
                        p1 = p1 + 1;
                     }
                  }
               }
            }
         }
            
         if (cell_coord(2,c) != 1) {
            for (k = 0; k <= cell_size(3,c)-1; k++) {
               for (j = -2; j <= -1; j++) {
                  for (i = 0; i <= cell_size(1,c)-1; i++) {
                     for (m = 1; m <= 5; m++) {
                        u(m,i,j,k,c) = in_buffer(sr(3)+p2);
                        p2 = p2 + 1;
                     }
                  }
               }
            }

         }
            
         if (cell_coord(2,c) != ncells) {
            for (k = 0; k <= cell_size(3,c)-1; k++) {
               for (j = cell_size(2,c); j <= cell_size(2,c)+1; j++) {
                  for (i = 0; i <= cell_size(1,c)-1; i++) {
                     for (m = 1; m <= 5; m++) {
                        u(m,i,j,k,c) = in_buffer(sr(2)+p3);
                        p3 = p3 + 1;
                     }
                  }
               }
            }
         }

         if (cell_coord(3,c) != 1) {
            for (k = -2; k <= -1; k++) {
               for (j = 0; j <= cell_size(2,c)-1; j++) {
                  for (i = 0; i <= cell_size(1,c)-1; i++) {
                     for (m = 1; m <= 5; m++) {
                        u(m,i,j,k,c) = in_buffer(sr(5)+p4);
                        p4 = p4 + 1;
                     }
                  }
               }
            }
         }

         if (cell_coord(3,c) != ncells) {
            for (k = cell_size(3,c); k <= cell_size(3,c)+1; k++) {
               for (j = 0; j <= cell_size(2,c)-1; j++) {
                  for (i = 0; i <= cell_size(1,c)-1; i++) {
                     for (m = 1; m <= 5; m++) {
                        u(m,i,j,k,c) = in_buffer(sr(4)+p5);
                        p5 = p5 + 1;
                     }
                  }
               }
            }
         }

//---------------------------------------------------------------------
//     cells loop
//---------------------------------------------------------------------
      }

//---------------------------------------------------------------------
//     do the rest of the rhs that uses the copied face values          
//---------------------------------------------------------------------
      compute_rhs();

      return;
}
