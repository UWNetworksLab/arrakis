//---------------------------------------------------------------------
//---------------------------------------------------------------------
//
//  header.h
//
//---------------------------------------------------------------------
//---------------------------------------------------------------------
#ifndef __HEADER_H
#define __HEADER_H

//---------------------------------------------------------------------
// The following include file is generated automatically by the
// "setparams" utility. It defines 
//      maxcells:      the square root of the maximum number of processors
//      problem_size:  12, 64, 102, 162 (for class T, A, B, C)
//      dt_default:    default time step for this problem size if no
//                     config file
//      niter_default: default number of iterations for this problem size
//---------------------------------------------------------------------

#include "npbparams.h"
#include "RCCE.h"
//we introduce the next definition to avoid confusing the compiler, which
//sometimes thinks the variable class is a reserved word
#define class _class_
#include "common.h"

#define AA 0
#define BB 1
#define CC 2
#define BLOCK_SIZE 5

#define EAST   2000
#define WEST   3000
#define NORTH  4000
#define SOUTH  5000
#define BOTTOM 6000
#define TOP    7000

#define WESTDIR   0
#define EASTDIR   1
#define SOUTHDIR  2
#define NORTHDIR  3
#define BOTTOMDIR 4
#define TOPDIR    5

#define MAX_CELL_DIM ((PROBLEM_SIZE/MAXCELLS)+1)
#define IMAX MAX_CELL_DIM
#define JMAX MAX_CELL_DIM
#define KMAX MAX_CELL_DIM

#define BUF_SIZE (MAX_CELL_DIM*MAX_CELL_DIM*(MAXCELLS-1)*60+1)

#define SQR(x) (x)*(x)

#define grid_points(m) grid_points[m-1]
#define ce(m,n) ce[(m-1)+5*(n-1)]
#define cell_coord(m,n) cell_coord[(m-1)+3*(n-1)]
#define cell_low(m,n) cell_low[(m-1)+3*(n-1)]
#define cell_high(m,n) cell_high[(m-1)+3*(n-1)]
#define cell_size(m,n) cell_size[(m-1)+3*(n-1)]
#define predecessor(m) predecessor[m-1]
#define slice(m,n) slice[(m-1)+3*(n-1)]
#define grid_size(m) grid_size[m-1]
#define successor(m) successor[m-1]
#define start(m,n) start[(m-1)+3*(n-1)]
#define end(m,n) end[(m-1)+3*(n-1)]
#define us(i,j,k,c) us[(i+1)+(IMAX+2)*((j+1)+(JMAX+2)*((k+1)+(KMAX+2)*(c-1)))]
#define vs(i,j,k,c) vs[(i+1)+(IMAX+2)*((j+1)+(JMAX+2)*((k+1)+(KMAX+2)*(c-1)))]
#define ws(i,j,k,c) ws[(i+1)+(IMAX+2)*((j+1)+(JMAX+2)*((k+1)+(KMAX+2)*(c-1)))]
#define qs(i,j,k,c) qs[(i+1)+(IMAX+2)*((j+1)+(JMAX+2)*((k+1)+(KMAX+2)*(c-1)))]
#define rho_i(i,j,k,c) rho_i[(i+1)+(IMAX+2)*((j+1)+(JMAX+2)*((k+1)+(KMAX+2)*(c-1)))]
#define square(i,j,k,c) square[(i+1)+(IMAX+2)*((j+1)+(JMAX+2)*((k+1)+(KMAX+2)*(c-1)))]
#define forcing(m,i,j,k,c) forcing[(m-1)+5*(i+IMAX*(j+JMAX*(k+KMAX*(c-1))))]
#define u(m,i,j,k,c) u[(m-1)+5*((i+2)+(IMAX+4)*((j+2)+(JMAX+4)*((k+2)+(KMAX+4)*(c-1))))]
#define rhs(m,i,j,k,c) rhs[(m-1)+5*((i+1)+(IMAX+1)*((j+1)+(JMAX+1)*((k+1)+(KMAX+1)*(c-1))))]
#define lhsc(m,n,i,j,k,c) lhsc[(m-1)+5*((n-1)+5*((i+1)+(IMAX+1)*((j+1)+(JMAX+1)*((k+1)+(KMAX+1)*(c-1)))))]
#define backsub_info(m,i,j,c) backsub_info[(m-1)+5*((i)+(IMAX+1)*((j)+(JMAX+1)*(c-1)))]
#define in_buffer(i) in_buffer[i-1]
#define out_buffer(i) out_buffer[i-1]
#define cv(m) cv[m+2]
#define rhon(m) rhon[m+2]
#define rhos(m) rhos[m+2]
#define rhoq(m) rhoq[m+2]
#define cuf(m) cuf[m+2]
#define q(m) q[m+2]
#define ue(m,n) ue[(m+2)+(MAX_CELL_DIM+4)*(n-1)]
#define buf(m,n) buf[(m+2)+(MAX_CELL_DIM+4)*(n-1)]
#define sum(m) sum[m-1]
#define xce_sub(m) xce_sub[m-1]


#ifdef G_MAIN
      int     ncells, grid_points[3];
      double  elapsed_time;

      double  tx1, tx2, tx3, ty1, ty2, ty3, tz1, tz2, tz3, 
                        dx1, dx2, dx3, dx4, dx5, dy1, dy2, dy3, dy4, 
                        dy5, dz1, dz2, dz3, dz4, dz5, dssp, dt, 
                        ce[5*13], dxmax, dymax, dzmax, xxcon1, xxcon2, 
                        xxcon3, xxcon4, xxcon5, dx1tx1, dx2tx1, dx3tx1,
                        dx4tx1, dx5tx1, yycon1, yycon2, yycon3, yycon4,
                        yycon5, dy1ty1, dy2ty1, dy3ty1, dy4ty1, dy5ty1,
                        zzcon1, zzcon2, zzcon3, zzcon4, zzcon5, dz1tz1, 
                        dz2tz1, dz3tz1, dz4tz1, dz5tz1, dnxm1, dnym1, 
                        dnzm1, c1c2, c1c5, c3c4, c1345, conz1, c1, c2, 
                        c3, c4, c5, c4dssp, c5dssp, dtdssp, dttx1, bt,
                        dttx2, dtty1, dtty2, dttz1, dttz2, c2dttx1, 
                        c2dtty1, c2dttz1, comz1, comz4, comz5, comz6, 
                        c3c4tx3, c3c4ty3, c3c4tz3, c2iv, con43, con16;

      int     cell_coord[MAXCELLS*3], cell_low[MAXCELLS*3], 
              cell_high[MAXCELLS*3],  cell_size[MAXCELLS*3],
              predecessor[3],         slice[MAXCELLS*3],
              grid_size[3],           successor[3],
              start[MAXCELLS*3],      end[MAXCELLS*3];

      double 
         us      [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         vs      [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         ws      [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         qs      [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         rho_i   [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         square  [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         forcing [5*IMAX*JMAX*KMAX*MAXCELLS],
         u       [5*(IMAX+4)*(JMAX+4)*(KMAX+4)*MAXCELLS],
         rhs     [5*(IMAX+1)*(JMAX+1)*(KMAX+1)*MAXCELLS],
         lhsc    [5*5*(IMAX+1)*(JMAX+1)*(KMAX+1)*MAXCELLS],
         backsub_info [5*(MAX_CELL_DIM+1)*(MAX_CELL_DIM+1)*MAXCELLS],
         in_buffer[BUF_SIZE], out_buffer[BUF_SIZE];

      double cv[MAX_CELL_DIM+4],   rhon[MAX_CELL_DIM+4],
             rhos[MAX_CELL_DIM+4], rhoq[MAX_CELL_DIM+4],
             cuf[MAX_CELL_DIM+4],  q[MAX_CELL_DIM+4],
             ue[(MAX_CELL_DIM+4)*5], buf[(MAX_CELL_DIM+4)*5];

      int  west_size, east_size, bottom_size, top_size,
               north_size, south_size, start_send_west, 
               start_send_east, start_send_south, start_send_north,
               start_send_bottom, start_send_top, start_recv_west,
               start_recv_east, start_recv_south, start_recv_north,
               start_recv_bottom, start_recv_top;
//
//     These are used by btio
//
      int collbuf_nodes, collbuf_size, iosize,
              idump, record_length,
              idump_sub, rd_interval;
      double sum[NITER_DEFAULT], xce_sub[5];
      long int iseek;
      int    send_color[6], recv_color[6];
#else
extern int     ncells, grid_points[3];
extern double  elapsed_time;

extern double  tx1, tx2, tx3, ty1, ty2, ty3, tz1, tz2, tz3, 
                        dx1, dx2, dx3, dx4, dx5, dy1, dy2, dy3, dy4, 
                        dy5, dz1, dz2, dz3, dz4, dz5, dssp, dt, 
                        ce[5*13], dxmax, dymax, dzmax, xxcon1, xxcon2, 
                        xxcon3, xxcon4, xxcon5, dx1tx1, dx2tx1, dx3tx1,
                        dx4tx1, dx5tx1, yycon1, yycon2, yycon3, yycon4,
                        yycon5, dy1ty1, dy2ty1, dy3ty1, dy4ty1, dy5ty1,
                        zzcon1, zzcon2, zzcon3, zzcon4, zzcon5, dz1tz1, 
                        dz2tz1, dz3tz1, dz4tz1, dz5tz1, dnxm1, dnym1, 
                        dnzm1, c1c2, c1c5, c3c4, c1345, conz1, c1, c2, 
                        c3, c4, c5, c4dssp, c5dssp, dtdssp, dttx1, bt,
                        dttx2, dtty1, dtty2, dttz1, dttz2, c2dttx1, 
                        c2dtty1, c2dttz1, comz1, comz4, comz5, comz6, 
                        c3c4tx3, c3c4ty3, c3c4tz3, c2iv, con43, con16;

extern int    cell_coord[MAXCELLS*3], cell_low[MAXCELLS*3], 
              cell_high[MAXCELLS*3],  cell_size[MAXCELLS*3],
              predecessor[3],         slice[MAXCELLS*3],
              grid_size[3],           successor[3],
              start[MAXCELLS*3],      end[MAXCELLS*3];

extern double 
         us      [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         vs      [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         ws      [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         qs      [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         rho_i   [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         square  [(IMAX+2)*(JMAX+2)*(KMAX+2)*MAXCELLS],
         forcing [5*IMAX*JMAX*KMAX*MAXCELLS],
         u       [5*(IMAX+4)*(JMAX+4)*(KMAX+4)*MAXCELLS],
         rhs     [5*(IMAX+1)*(JMAX+1)*(KMAX+1)*MAXCELLS],
         lhsc    [5*5*(IMAX+1)*(JMAX+1)*(KMAX+1)*MAXCELLS],
         backsub_info [5*(MAX_CELL_DIM+1)*(MAX_CELL_DIM+1)*MAXCELLS],
         in_buffer[BUF_SIZE], out_buffer[BUF_SIZE];

extern double cv[MAX_CELL_DIM+4],   rhon[MAX_CELL_DIM+4],
             rhos[MAX_CELL_DIM+4], rhoq[MAX_CELL_DIM+4],
             cuf[MAX_CELL_DIM+4],  q[MAX_CELL_DIM+4],
             ue[(MAX_CELL_DIM+4)*5], buf[(MAX_CELL_DIM+4)*5];

extern int  west_size, east_size, bottom_size, top_size,
               north_size, south_size, start_send_west, 
               start_send_east, start_send_south, start_send_north,
               start_send_bottom, start_send_top, start_recv_west,
               start_recv_east, start_recv_south, start_recv_north,
               start_recv_bottom, start_recv_top;

//
//     These are used by btio
//
extern int collbuf_nodes, collbuf_size, iosize,
              idump, record_length,
              idump_sub, rd_interval;
extern double sum[NITER_DEFAULT], xce_sub[5];
extern long int iseek;
extern int    send_color[6], recv_color[6];

#endif /*G_MAIN*/

extern void matvec_sub(double ablock[], double avec[], double bvec[]);
extern void matmul_sub(double ablock[], double bblock[], double cblock[]);
extern void binvcrhs( double lhs[], double c[], double r[] );
extern void binvrhs( double lhs[], double r[] );
extern void exact_solution(double xi,double eta,double zeta,double dtemp[]);

extern int setup_mpi(int *argc, char ***argv);
extern void make_set(void);
extern void set_constants(void);
extern void lhsinit(void);
extern void lhsabinit(double lhsa[], double lhsb[], int size);
extern void initialize(void);
extern void exact_rhs(void);
extern void compute_buffer_size(int c);
extern void adi(void);
extern void compute_rhs(void);
extern void copy_faces(void);
extern void x_solve(void);
extern void y_solve(void);
extern void z_solve(void);
extern void add(void);
extern void verify(int niter, char *class, int *verified);
extern void error_norm(double rms[]);
extern void rhs_norm(double rms[]);

extern void setup_btio(void);
extern void output_timestep(void);
extern void btio_cleanup(void);
extern void btio_verify(int *verified);
extern void accumulate_norms(double xce[]);
extern void clear_timestep(void);

#endif

#ifdef _OPENMP
#pragma omp threadprivate (cell_coord, cell_low, cell_high,  cell_size)
#pragma omp threadprivate (predecessor, slice, grid_size, successor)
#pragma omp threadprivate (start, end)

#pragma omp threadprivate (ncells, grid_points, elapsed_time)
#pragma omp threadprivate (tx1, tx2, tx3, ty1, ty2, ty3, tz1, tz2, tz3, \
                           dx1, dx2, dx3, dx4, dx5, dy1, dy2, dy3, dy4, \
                           dy5, dz1, dz2, dz3, dz4, dz5, dssp, dt, \
                           ce, dxmax, dymax, dzmax, xxcon1, xxcon2, \
                           xxcon3, xxcon4, xxcon5, dx1tx1, dx2tx1, dx3tx1, \
                           dx4tx1, dx5tx1, yycon1, yycon2, yycon3, yycon4, \
                           yycon5, dy1ty1, dy2ty1, dy3ty1, dy4ty1, dy5ty1, \
                           zzcon1, zzcon2, zzcon3, zzcon4, zzcon5, dz1tz1, \
                           dz2tz1, dz3tz1, dz4tz1, dz5tz1, dnxm1, dnym1, \
                           dnzm1, c1c2, c1c5, c3c4, c1345, conz1, c1, c2, \
                           c3, c4, c5, c4dssp, c5dssp, dtdssp, dttx1, bt, \
                           dttx2, dtty1, dtty2, dttz1, dttz2, c2dttx1, \
                           c2dtty1, c2dttz1, comz1, comz4, comz5, comz6, \
                           c3c4tx3, c3c4ty3, c3c4tz3, c2iv, con43, con16)

#pragma omp threadprivate (us, vs, ws, qs, rho_i, square, forcing, \
                           u, rhs, lhsc, backsub_info, in_buffer, out_buffer)

#pragma omp threadprivate (cv, rhon, rhos, rhoq, cuf, q, ue, buf)

#pragma omp threadprivate (west_size, east_size, bottom_size, top_size, \
                           north_size, south_size, start_send_west, \
                           start_send_east, start_send_south, start_send_north, \
                           start_send_bottom, start_send_top, start_recv_west, \
                           start_recv_east, start_recv_south, start_recv_north, \
                           start_recv_bottom, start_recv_top, send_color, recv_color)
//
//     These are used by btio
//
#pragma omp threadprivate (collbuf_nodes, collbuf_size, iosize, idump,\
                           record_length, idump_sub, rd_interval, \
                           sum, xce_sub, iseek)
#endif
