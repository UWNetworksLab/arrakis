
//---------------------------------------------------------------------
//---------------------------------------------------------------------
#ifndef __MPINPB_H
#define __MPINPB_H

#ifdef G_MAIN
       int           node, no_nodes, total_nodes, root;
       int           active;
#else
extern int           node, no_nodes, total_nodes, root;
extern int           active;

#endif
#ifdef _OPENMP
#pragma omp threadprivate (node, no_nodes, total_nodes, root, active)
#endif
#endif

