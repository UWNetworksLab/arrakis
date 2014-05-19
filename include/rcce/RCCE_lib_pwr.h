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
#define RC_GLOBAL_CLOCK_MHZ          1600
#define RPC_ROOT                     0
#define RC_NUM_VOLTAGE_DOMAINS       6
#define RC_MIN_VOLTAGE_LEVEL         0
#define RC_MAX_VOLTAGE_LEVEL         6
#define RC_MAX_FREQUENCY_DIVIDER     16  // maximum divider value, so lowest F
#define RC_MIN_FREQUENCY_DIVIDER     2   // minimum divider value, so highest F
#define RC_NUM_FREQUENCY_DIVIDERS    (RC_MAX_FREQUENCY_DIVIDER+1)
#define RC_NUM_VOLTAGE_LEVELS        7
#define RPC_PHYSICAL_ADDRESS         0xFB000000
#ifndef COPPERRIDGE
/* real latency is probably closer to 8 800 000 */
#define RC_WAIT_CYCLES               1000000
#endif
#define TILEDIVIDER                  0x00000080
#define RC_DEFAULT_VOLTAGE_LEVEL     4
#define RC_DEFAULT_FREQUENCY_DIVIDER 3 // corresponds to tile clock of 1600/3


typedef struct {
float volt; 
int   VID;
int   MHz_cap; 
} triple;

int RCCE_set_frequency(int);
static int RC_wait_voltage(RCCE_REQUEST *);
static int RC_set_frequency_divider(int, int);
long long RC_global_clock(void);
unsigned int FID_word(int, int);
unsigned int VID_word(int, int);

#ifdef RC_POWER_MANAGEMENT
#ifndef COPPERRIDGE
  typedef struct {
    volatile int queue[RC_NUM_VOLTAGE_DOMAINS];
    volatile long long start_time;
  } RCCE_RPC_REGULATOR;
#define REGULATOR_LENGTH (PAD32byte(sizeof(RCCE_RPC_REGULATOR)))
  extern RCCE_RPC_REGULATOR *RCCE_RPC_regulator;
  extern long long    RC_time_at_birth;
#endif
  extern RCCE_COMM    RCCE_V_COMM;
  extern RCCE_COMM    RCCE_F_COMM;
  extern RCCE_COMM    RCCE_P_COMM;
  extern int          RCCE_ue_F_masters[4];
  extern int          RCCE_set_power_active;
  extern int          RC_current_voltage_level;
  extern int          RC_current_frequency_divider;
  
  int RCCE_init_RPC(int *, int, int);
#endif

#ifdef _OPENMP
  #pragma omp threadprivate (RCCE_ue_F_masters)
  #pragma omp threadprivate (RCCE_V_COMM, RCCE_F_COMM, RCCE_P_COMM)
  #pragma omp threadprivate (RCCE_set_power_active)
  #pragma omp threadprivate (RC_current_voltage_level, RC_current_frequency_divider)
#ifndef COPPERRIDGE
  #pragma omp threadprivate (RC_time_at_birth)
  #pragma omp threadprivate (RCCE_RPC_regulator)
#endif
#endif

