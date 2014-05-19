//---------------------------------------------------------------------
//---------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "header.h"
#include "mpinpb.h"

#define mod(p,q) ((p)%(q))
#define max(x,y)      ((x)>(y)? (x) : (y))
#define min(x,y)      ((x)<(y)? (x) : (y))

void make_set() {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     This function allocates space for a set of cells and fills the set
//     such that communication between cells on different nodes is only
//     nearest neighbor
//---------------------------------------------------------------------


      int p, i, j, c, dir, size, excess, ierr,ierrcode;

//---------------------------------------------------------------------
//     compute square root; add small number to allow for roundoff
//     (note: this is computed in setup_mpi.f also, but prefer to do
//     it twice because of some include file problems).
//---------------------------------------------------------------------
      ncells = (int)(sqrt((double)(no_nodes) + 0.00001e0));

//---------------------------------------------------------------------
//     this makes coding easier
//---------------------------------------------------------------------
      p = ncells;
      
//---------------------------------------------------------------------
//     determine the location of the cell at the bottom of the 3D 
//     array of cells
//---------------------------------------------------------------------
      cell_coord(1,1) = mod(node,p) ;
      cell_coord(2,1) = node/p ;
      cell_coord(3,1) = 0;

//---------------------------------------------------------------------
//     set the cell_coords for cells in the rest of the z-layers; 
//     this comes down to a simple linear numbering in the z-direct-
//     ion, and to the doubly-cyclic numbering in the other dirs     
//---------------------------------------------------------------------
      for (c = 2; c <= p; c++) {
         cell_coord(1,c) = mod(cell_coord(1,c-1)+1,p) ;
         cell_coord(2,c) = mod(cell_coord(2,c-1)-1+p,p) ;
         cell_coord(3,c) = c-1;
      }

//---------------------------------------------------------------------
//     offset all the coordinates by 1 to adjust for Fortran arrays
//---------------------------------------------------------------------
      for (dir = 1; dir <= 3; dir++) {
         for (c = 1; c <= p; c++) {
            cell_coord(dir,c) = cell_coord(dir,c) + 1;
         }
      }
      
//---------------------------------------------------------------------
//     slice(dir,n) contains the sequence number of the cell that is in
//     coordinate plane n in the dir direction
//---------------------------------------------------------------------
      for (dir = 1; dir <= 3; dir++) {
         for (c = 1; c <= p; c++) {
            slice(dir,cell_coord(dir,c)) = c;
         }
      }


//---------------------------------------------------------------------
//     fill the predecessor and successor entries, using the indices 
//     of the bottom cells (they are the same at each level of k 
//     anyway) acting as if full periodicity pertains; note that p is
//     added to those arguments to the mod functions that might
//     otherwise return wrong values when using the modulo function
//---------------------------------------------------------------------
      i = cell_coord(1,1)-1;
      j = cell_coord(2,1)-1;

      predecessor(1) = mod(i-1+p,p) + p*j;
      predecessor(2) = i + p*mod(j-1+p,p);
      predecessor(3) = mod(i+1,p) + p*mod(j-1+p,p);
      successor(1)   = mod(i+1,p) + p*j;
      successor(2)   = i + p*mod(j+1,p);
      successor(3)   = mod(i-1+p,p) + p*mod(j+1,p);

      int id = RCCE_ue();

//---------------------------------------------------------------------
//     now compute the sizes of the cells                                
//---------------------------------------------------------------------
      for (dir = 1; dir <= 3; dir++) {
//---------------------------------------------------------------------
//     set cell_coord range for each direction                           
//---------------------------------------------------------------------
         size   = grid_points(dir)/p;
         excess = mod(grid_points(dir),p);
         for (c = 1; c <= ncells; c++) {
            if (cell_coord(dir,c) <= excess) {
               cell_size(dir,c) = size+1;
               cell_low(dir,c) = (cell_coord(dir,c)-1)*(size+1);
               cell_high(dir,c) = cell_low(dir,c)+size;
            } else {
               cell_size(dir,c) = size;
               cell_low(dir,c)  = excess*(size+1)+
                    (cell_coord(dir,c)-excess-1)*size;
               cell_high(dir,c) = cell_low(dir,c)+size-1;
            }
            if (cell_size(dir, c) <= 2) {
               printf(" Error: Cell size too small. Min size is 3\n");
               ierrcode = 1;
               exit(1);
            }
         }
      }

      return;
}

//---------------------------------------------------------------------
//---------------------------------------------------------------------


void make_color() {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     This function determines cycles in the communication graphs in
//     the six coordinate directions, and colors the ranks so they know
//     how to construct deadlock-free blocking communication schedules
//---------------------------------------------------------------------

      int p, i, j, dir, node_loc, comm_color, node_min, length, start_found;

//---------------------------------------------------------------------
//     compute square root; add small number to allow for roundoff
//     (note: this is computed in setup_mpi.f also, but prefer to do
//     it twice because of some include file problems).
//---------------------------------------------------------------------
      ncells = (int)(sqrt((double)(no_nodes) + 0.00001e0));

//---------------------------------------------------------------------
//     this makes coding easier
//---------------------------------------------------------------------
      p = ncells;

      for (dir = 0; dir<6; dir++) {

        node_loc = node_min = node; length = 1; start_found = 0;
        while (!start_found) {
          i = mod(node_loc,p) ;
          j = node_loc/p ;

          switch (dir) {
            case (WESTDIR):   node_loc = mod(i-1+p,p) + p*j;          break;
            case (EASTDIR):   node_loc = mod(i+1,p) + p*j;            break;
            case (SOUTHDIR):  node_loc = i + p*mod(j-1+p,p);          break;
            case (NORTHDIR):  node_loc = i + p*mod(j+1,p);            break;
            case (BOTTOMDIR): node_loc = mod(i+1,p) + p*mod(j-1+p,p); break;
            case (TOPDIR):    node_loc = mod(i-1+p,p) + p*mod(j+1,p); break;
          }

          // the next block ensures that the node with the lowest rank
          // in this cycle is colored WHITE (=0), and that nodes an even
          // number of jumps removed from that lowest-ranked member
          // are also white. The others are RED (1).
          if (node_loc <= node_min) {
            node_min = node_loc;
            comm_color = 0;
          } else comm_color = !comm_color;
          if (node_loc == node) start_found = 1;
          else length++;
        }
        send_color[dir] = comm_color;
        recv_color[dir] = !send_color[dir];
        // if the number of nodes in this cycle is odd, we need to treat the 
        // last node before the "start" of the cycle differently
        if (length%2) {
          if (node == node_min) recv_color[dir] = 2;
          i = mod(node,p) ;
          j = node/p ;
          switch (dir) {
            case (WESTDIR):   node_loc = mod(i-1+p,p) + p*j;          break;
            case (EASTDIR):   node_loc = mod(i+1,p) + p*j;            break;
            case (SOUTHDIR):  node_loc = i + p*mod(j-1+p,p);          break;
            case (NORTHDIR):  node_loc = i + p*mod(j+1,p);            break;
            case (BOTTOMDIR): node_loc = mod(i+1,p) + p*mod(j-1+p,p); break;
            case (TOPDIR):    node_loc = mod(i-1+p,p) + p*mod(j+1,p); break;
          }      
          if (node_loc == node_min) send_color[dir] = 2;
        }
     }
     return;
}

//---------------------------------------------------------------------
//---------------------------------------------------------------------


