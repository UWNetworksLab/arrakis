/**
 * \file
 * \brief Computes a matrix multiplication. Tries to do that in a cache-aware way.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <stdlib.h>
#include <skb/skb.h>


int A[5][5] = {{1,2,3,4},
             {5,6,7,8},
             {9,10,11,12},
             {13,14,15,16}};

int B[7][9] = {{1,2,3,4},
             {1,2,3,4},
             {1,2,3,4},
             {1,2,3,4}};

int C[4][4];

int A2[5][2] = {{1,2}, //matrix size 4x2
                {3,4},
                {5,6},
                {7,8}};
int B2[3][4] = {{1,2,3,4}, //matrix size 2x4
                {1,2,3,4}};
int C2[9][9]; //the matrix size will be 4x4



int A3[16 * 512][4];
int B3[4][16 * 512];
int C3[16 * 512][16 * 512];


int A4[16 * 512][4];
int B4[4][16 * 512 + 16];
int C4[16 * 512][16 * 512];


//r = #rows in A, c = #columns in A (the real rows and columns)
//ar, ac && br, bc && cr, cc are the sizes of the respective arrays
static inline void matrix_mul(int r, int c,
                              int ar, int ac,
                              int A[ar][ac],
                              int br, int bc,
                              int B[br][bc],
                              int cr, int cc,
                              int C[cr][cc])
{
    for (int i = 0; i < r; i++) {
        for (int j = 0; j < r; j++) {
            register int tmp = 0;
            for (int k = 0; k < c; k++) {
                tmp = tmp + A[i][k] * B[k][j];
            }
            C[i][j] = tmp;
        }
    }
}

static inline void matrix_print(int r, int c, int mr, int mc, int M[mr][mc])
{
    for (int i = 0; i < r; i++) {
        printf("\n");
        for (int j = 0; j < c; j++) {
            printf("%02d ", M[i][j]);
        }
    }
}



int main(int argc, char **argv)
{
    printf("matrix mul: connecting to the SKB...\n");
    skb_client_connect();
    printf("matrix mul: connected.\n");

    skb_create_buffer();

    matrix_print(4, 4, 5, 5, A);
    matrix_print(4, 4, 7, 9, B);
    uint64_t start = rdtsc(); //rdtscp();
    matrix_mul(4, 4, 5, 5 , A, 7, 9, B, 4, 4, C);
    uint64_t end = rdtsc(); //rdtscp();
    matrix_print(4, 4, 4, 4, C);
    printf("\n%lu cycles for multiplying one matrix\n", end - start);


    matrix_print(4, 2, 5, 2, A2);
    matrix_print(2, 4, 3, 4, B2);
    start = rdtsc(); //rdtscp();
    matrix_mul(4, 2, 5, 2 , A2, 3, 4, B2, 9, 9, C2);
    end = rdtsc(); //rdtscp();
    matrix_print(4, 4, 9, 9, C2);
    printf("\n%lu cycles for multiplying one matrix\n", end - start);


    start = rdtsc(); //rdtscp();
    matrix_mul(16 * 512, 4, 16 * 512, 4 , A3, 4, 16 * 512, B3, 16 * 512, 16 * 512, C3);
    end = rdtsc(); //rdtscp();
    printf("\n%lu cycles for multiplying one matrix\n", end - start);


    start = rdtsc(); //rdtscp();
    matrix_mul(16 * 512, 4, 16 * 512, 4 , A4, 4, 16 * 512 + 16, B4,
               16 * 512, 16 * 512, C4);
    end = rdtsc(); //rdtscp();
    printf("\n%lu cycles for multiplying one matrix\n", end - start);


    return 0;
}
