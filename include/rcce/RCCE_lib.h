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
#ifndef RCCE_LIB_H
#define RCCE_LIB_H
#include "RCCE.h"
#ifdef _OPENMP
  #include <omp.h>
#endif
#include <string.h>

/* PAD32byte is used to compute a cacheline padded length of n (input) bytes */
#define PAD32byte(n) ((n)%32==0 ? (n) : (n) + 32 - (n)%32)

//#define BITSPERCHAR                     8

#define BOTH_IN_COMM_BUFFER             12
#define SOURCE_IN_PRIVATE_MEMORY        34
#define TARGET_IN_PRIVATE_MEMORY        56

#define RCCE_SUM_INT                       (RCCE_SUM+(RCCE_NUM_OPS)*(RCCE_INT))
#define RCCE_SUM_LONG                      (RCCE_SUM+(RCCE_NUM_OPS)*(RCCE_LONG))
#define RCCE_SUM_FLOAT                     (RCCE_SUM+(RCCE_NUM_OPS)*(RCCE_FLOAT))
#define RCCE_SUM_DOUBLE                    (RCCE_SUM+(RCCE_NUM_OPS)*(RCCE_DOUBLE))
#define RCCE_MAX_INT                       (RCCE_MAX+(RCCE_NUM_OPS)*(RCCE_INT))
#define RCCE_MAX_LONG                      (RCCE_MAX+(RCCE_NUM_OPS)*(RCCE_LONG))
#define RCCE_MAX_FLOAT                     (RCCE_MAX+(RCCE_NUM_OPS)*(RCCE_FLOAT))
#define RCCE_MAX_DOUBLE                    (RCCE_MAX+(RCCE_NUM_OPS)*(RCCE_DOUBLE))
#define RCCE_MIN_INT                       (RCCE_MIN+(RCCE_NUM_OPS)*(RCCE_INT))
#define RCCE_MIN_LONG                      (RCCE_MIN+(RCCE_NUM_OPS)*(RCCE_LONG))
#define RCCE_MIN_FLOAT                     (RCCE_MIN+(RCCE_NUM_OPS)*(RCCE_FLOAT))
#define RCCE_MIN_DOUBLE                    (RCCE_MIN+(RCCE_NUM_OPS)*(RCCE_DOUBLE))
#define RCCE_PROD_INT                      (RCCE_PROD+(RCCE_NUM_OPS)*(RCCE_INT))
#define RCCE_PROD_LONG                     (RCCE_PROD+(RCCE_NUM_OPS)*(RCCE_LONG))
#define RCCE_PROD_FLOAT                    (RCCE_PROD+(RCCE_NUM_OPS)*(RCCE_FLOAT))
#define RCCE_PROD_DOUBLE                   (RCCE_PROD+(RCCE_NUM_OPS)*(RCCE_DOUBLE))

#define RCCE_COMM_INITIALIZED              45328976
#define RCCE_COMM_NOT_INITIALIZED          -45328976

// auxiliary MPB pointer type
typedef volatile unsigned int*  t_vintp;
// Also need dereferenced types
typedef volatile unsigned char t_vchar;
typedef volatile unsigned int  t_vint;

typedef struct rcce_block {
  t_vcharp space;          // pointer to space for data in block             
  size_t free_size;        // actual free space in block (0 or whole block)  
  struct rcce_block *next; // pointer to next block in circular linked list 
} RCCE_BLOCK;

#ifdef SINGLEBITFLAGS
typedef struct rcce_flag_line {
  char flag[RCCE_LINE_SIZE];
  t_vcharp line_address;
  int  members;
  struct rcce_flag_line *next;
} RCCE_FLAG_LINE;
#endif

typedef struct  {
  RCCE_BLOCK *tail;     // "last" block in linked list of blocks           
} RCCE_BLOCK_S;

#ifndef GORY
  extern RCCE_FLAG    RCCE_sent_flag[RCCE_MAXNP];
  extern RCCE_FLAG    RCCE_ready_flag[RCCE_MAXNP];
  extern t_vcharp     RCCE_buff_ptr;
  extern size_t       RCCE_chunk;
  extern t_vcharp     RCCE_flags_start; 
#endif

extern t_vcharp     RCCE_comm_buffer[RCCE_MAXNP];
extern int          RCCE_NP;
extern int          RCCE_BUFF_SIZE;
#ifndef COPPERRIDGE
  extern omp_lock_t RCCE_corelock[RCCE_MAXNP];
  extern t_vchar    RC_comm_buffer[RCCE_MAXNP*RCCE_BUFF_SIZE_MAX];
#endif
extern int          RC_MY_COREID;
extern int          RC_COREID[RCCE_MAXNP];
extern double       RC_REFCLOCKGHZ;
extern int          RCCE_IAM;
extern int          RCCE_debug_synch;
extern int          RCCE_debug_comm;
extern int          RCCE_debug_debug;
extern int          RCCE_debug_RPC;
#ifdef SINGLEBITFLAGS
  extern RCCE_FLAG_LINE RCCE_flags;
  extern int            WORDSIZE;
  extern int            LEFTMOSTBIT;
  RCCE_FLAG_STATUS RCCE_bit_value(t_vcharp, int);
  RCCE_FLAG_STATUS RCCE_flip_bit_value(t_vcharp, int);
  int RCCE_write_bit_value(t_vcharp, int, RCCE_FLAG_STATUS);
#endif

extern int          RCCE_comm_init_val;

void     RCCE_malloc_init(t_vcharp, size_t);
int      RCCE_qsort(char *, size_t, size_t, int (*)(const void*, const void*));
int      id_compare(const void *, const void *);
int      RCCE_probe(RCCE_FLAG);
int      RCCE_error_return(int, int);
void     RC_cache_invalidate(void);
int      RCCE_acquire_lock(int);
int      RCCE_release_lock(int);
int      RCCE_global_color(int, void *);
t_vcharp RC_COMM_BUFFER_START(int);

#ifndef GORY
  t_vcharp RCCE_malloc(size_t);
  t_vcharp RCCE_malloc_request(size_t, size_t *);
  void     RCCE_free(t_vcharp);
  int      RCCE_put(t_vcharp, t_vcharp, int, int);
  int      RCCE_get(t_vcharp, t_vcharp, int, int);
  int      RCCE_wait_until(RCCE_FLAG, RCCE_FLAG_STATUS);
  int      RCCE_flag_alloc(RCCE_FLAG *);
  int      RCCE_flag_free(RCCE_FLAG *);
  int      RCCE_flag_write(RCCE_FLAG *, RCCE_FLAG_STATUS, int); 
  int      RCCE_flag_read(RCCE_FLAG, RCCE_FLAG_STATUS *, int);
#endif

#ifdef _OPENMP
  #pragma omp threadprivate (RC_COREID, RC_MY_COREID, RC_REFCLOCKGHZ)
  #pragma omp threadprivate (RCCE_comm_buffer)
  #pragma omp threadprivate (RCCE_BUFF_SIZE)
  #pragma omp threadprivate (RCCE_IAM, RCCE_NP)
  #pragma omp threadprivate (RCCE_debug_synch, RCCE_debug_comm, RCCE_debug_debug)
  #ifdef SINGLEBITFLAGS
    #pragma omp threadprivate (RCCE_flags, WORDSIZE, LEFTMOSTBIT)
  #endif
  #ifndef GORY
    #pragma omp threadprivate (RCCE_sent_flag, RCCE_ready_flag)
    #pragma omp threadprivate (RCCE_buff_ptr, RCCE_chunk)
    #pragma omp threadprivate (RCCE_flags_start)
  #endif
#endif

#endif
