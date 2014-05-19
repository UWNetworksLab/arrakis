// 
// Copyright 2010 Intel Corporation
// 
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
// 
//        http://www.apache.org/licenses/LICENSE-2.0
// 
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
// 
#ifndef RCCE_H
#define RCCE_H

#include <stdlib.h>
#include <stdio.h>

#define _RCCE "1.0.7 release"
// little trick to allow the application to be called "RCCE_APP" under
// OpenMP, and "main" otherwise 
#ifndef _OPENMP
  #define RCCE_APP main
#endif

// modify next line for BareMetal, which supports stdout, but not stdferr 
#define STDERR                             stdout

#define LOG2_LINE_SIZE                     5
#define RCCE_LINE_SIZE                     (1<<LOG2_LINE_SIZE)
// RCCE_BUFF_SIZE_MAX is space per UE, which is half of the space per tile 
#define RCCE_BUFF_SIZE_MAX                 (1<<13)
#define RCCE_MAXNP                         48
#define RCCE_SUCCESS                       0
#define RCCE_ERROR_BASE                    1234321
#define RCCE_ERROR_TARGET                  (RCCE_ERROR_BASE +  1)
#define RCCE_ERROR_SOURCE                  (RCCE_ERROR_BASE +  2)
#define RCCE_ERROR_ID                      (RCCE_ERROR_BASE +  3)
#define RCCE_ERROR_MESSAGE_LENGTH          (RCCE_ERROR_BASE +  4)
#define RCCE_ERROR_FLAG_UNDEFINED          (RCCE_ERROR_BASE +  5)
#define RCCE_ERROR_NUM_UES                 (RCCE_ERROR_BASE +  6)
#define RCCE_ERROR_DATA_OVERLAP            (RCCE_ERROR_BASE +  7)
#define RCCE_ERROR_ALIGNMENT               (RCCE_ERROR_BASE +  8)
#define RCCE_ERROR_DEBUG_FLAG              (RCCE_ERROR_BASE +  9)
#define RCCE_ERROR_FLAG_NOT_IN_COMM_BUFFER (RCCE_ERROR_BASE + 10)
#define RCCE_ERROR_FLAG_STATUS_UNDEFINED   (RCCE_ERROR_BASE + 11)
#define RCCE_ERROR_FLAG_NOT_ALLOCATED      (RCCE_ERROR_BASE + 12)
#define RCCE_ERROR_VAL_UNDEFINED           (RCCE_ERROR_BASE + 13)
#define RCCE_ERROR_INVALID_ERROR_CODE      (RCCE_ERROR_BASE + 14)
#define RCCE_ERROR_RPC_NOT_ALLOCATED       (RCCE_ERROR_BASE + 15)
#define RCCE_ERROR_RPC_INTERNAL            (RCCE_ERROR_BASE + 16)
#define RCCE_ERROR_MULTIPLE_RPC_REQUESTS   (RCCE_ERROR_BASE + 17)
#define RCCE_ERROR_FDIVIDER                (RCCE_ERROR_BASE + 18)
#define RCCE_ERROR_FREQUENCY_EXCEEDED      (RCCE_ERROR_BASE + 19)
#define RCCE_ERROR_NO_ACTIVE_RPC_REQUEST   (RCCE_ERROR_BASE + 20)
#define RCCE_ERROR_STALE_RPC_REQUEST       (RCCE_ERROR_BASE + 21)
#define RCCE_ERROR_COMM_UNDEFINED          (RCCE_ERROR_BASE + 22)
#define RCCE_ERROR_ILLEGAL_OP              (RCCE_ERROR_BASE + 23)
#define RCCE_ERROR_ILLEGAL_TYPE            (RCCE_ERROR_BASE + 24)
#define RCCE_ERROR_MALLOC                  (RCCE_ERROR_BASE + 25)
#define RCCE_ERROR_COMM_INITIALIZED        (RCCE_ERROR_BASE + 26)
#define RCCE_ERROR_CORE_NOT_IN_HOSTFILE    (RCCE_ERROR_BASE + 27)
#define RCCE_MAX_ERROR_STRING              45

#define RCCE_DEBUG_ALL                     111111
#define RCCE_DEBUG_SYNCH                   111444
#define RCCE_DEBUG_COMM                    111555
#define RCCE_DEBUG_RPC                     111666
#define RCCE_DEBUG_DEBUG                   111888

#define RCCE_FLAG_SET                      1
#define RCCE_FLAG_UNSET                    0

#define RCCE_NUM_OPS                       4
#define RCCE_OP_BASE                       23232323
#define RCCE_SUM                           (RCCE_OP_BASE)
#define RCCE_MIN                           (RCCE_OP_BASE+1)
#define RCCE_MAX                           (RCCE_OP_BASE+2)
#define RCCE_PROD                          (RCCE_OP_BASE+3)

#define RCCE_TYPE_BASE                     63636363
#define RCCE_INT                           (RCCE_TYPE_BASE)
#define RCCE_LONG                          (RCCE_TYPE_BASE+1)
#define RCCE_FLOAT                         (RCCE_TYPE_BASE+2)
#define RCCE_DOUBLE                        (RCCE_TYPE_BASE+3)

// MPB pointer type
typedef volatile unsigned char* t_vcharp;

#ifdef SINGLEBITFLAGS
typedef struct {
   int  location;      /* location of bit within line (0-255)  */
   t_vcharp line_address; /* start of cache line containing flag  */
}  RCCE_FLAG;
#else
typedef volatile int *RCCE_FLAG;
#endif

typedef int RCCE_FLAG_STATUS;
typedef struct {
   int size;
   int my_rank;
   int initialized;
   int member[RCCE_MAXNP];
   RCCE_FLAG gather;
   RCCE_FLAG release;
} RCCE_COMM;

#ifdef RC_POWER_MANAGEMENT
typedef struct{
    int release;
    int old_voltage_level;
    int new_voltage_level;
    int old_frequency_divider;
    int new_frequency_divider;
    long long start_cycle;
  } RCCE_REQUEST;
int RCCE_power_domain(void);
int RCCE_iset_power(int, RCCE_REQUEST *, int *, int *);
int RCCE_wait_power(RCCE_REQUEST *);
int RCCE_set_frequency_divider(int, int *);
int RCCE_power_domain_master(void);
int RCCE_power_domain_size(void);
#endif  

int    RCCE_init(int *, char***);
int    RCCE_finalize(void);
double RCCE_wtime(void);
int    RCCE_ue(void);
int    RCCE_num_ues(void);
#ifdef GORY
t_vcharp RCCE_malloc(size_t);
t_vcharp RCCE_malloc_request(size_t, size_t *);
void   RCCE_free(t_vcharp);
int    RCCE_put(t_vcharp, t_vcharp, int, int);
int    RCCE_get(t_vcharp, t_vcharp, int, int);
int    RCCE_wait_until(RCCE_FLAG, RCCE_FLAG_STATUS);
int    RCCE_flag_alloc(RCCE_FLAG *);
int    RCCE_flag_free(RCCE_FLAG *);
int    RCCE_flag_write(RCCE_FLAG *, RCCE_FLAG_STATUS, int);
int    RCCE_flag_read(RCCE_FLAG, RCCE_FLAG_STATUS *, int);
int    RCCE_send(char *, t_vcharp, size_t, RCCE_FLAG *, RCCE_FLAG *, size_t, int);
int    RCCE_recv(char *, t_vcharp, size_t, RCCE_FLAG *, RCCE_FLAG *, size_t, int);
int    RCCE_recv_test(char *, t_vcharp, size_t, RCCE_FLAG *, RCCE_FLAG *, 
                      size_t, int, int *);
#else
int    RCCE_send(char *, size_t, int);
int    RCCE_recv(char *, size_t, int);
int    RCCE_recv_test(char *, size_t, int, int *);
int    RCCE_allreduce(char *, char *, int, int, int, RCCE_COMM);
int    RCCE_reduce(char *, char *, int, int, int, int, RCCE_COMM);
int    RCCE_bcast(char *, size_t, int, RCCE_COMM);
#endif
int    RCCE_comm_split(int (*)(int, void *), void *, RCCE_COMM *);
int    RCCE_comm_free(RCCE_COMM *);
int    RCCE_comm_size(RCCE_COMM, int *);
int    RCCE_comm_rank(RCCE_COMM, int *);
void   RCCE_fence(void);
int    RCCE_barrier(RCCE_COMM *);
int    RCCE_error_string(int, char *, int *);
int    RCCE_debug_set(int);
int    RCCE_debug_unset(int);

extern RCCE_COMM    RCCE_COMM_WORLD;
#ifdef RC_POWER_MANAGEMENT
extern RCCE_COMM    RCCE_P_COMM;
#define RCCE_POWER_DEFAULT -99999
#endif

#ifdef _OPENMP
#pragma omp threadprivate (RCCE_COMM_WORLD)
#ifdef RC_POWER_MANAGEMENT
#pragma omp threadprivate (RCCE_P_COMM)
#endif
#endif

#endif

