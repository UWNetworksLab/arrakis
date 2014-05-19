#include <string.h>
#include <stdio.h>
#include <rcce/RCCE.h>
#include <assert.h>
#include <inttypes.h>

//#define CHECKBUF
#define ENDLESS
#define NO_FLOAT

#ifdef NO_FLOAT
#       include <barrelfish/barrelfish.h>
#endif

#define BUFSIZE  32
#define MAXROUND 100000
#define ROUNDS_PER_SLICE 5

#ifdef __scc__
extern uint32_t yield_timeslices;
#endif

int RCCE_APP(int argc, char **argv){
  int YOU, ME, round;
#ifndef NO_FLOAT
  double timer;
#else
  uint64_t timer = 0, sum = 0;
#endif
  char buffer[BUFSIZE+1] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

  RCCE_init(&argc, &argv);

  RCCE_debug_set(RCCE_DEBUG_ALL);
  ME = RCCE_ue();
  printf("Core %d passed RCCE_init\n", ME);
  if (RCCE_num_ues() != 2) {
    if (!ME) printf("Ping pong needs exactly two UEs; try again\n");
    return(1);
  }
  YOU = !ME;

  // synchronize before starting the timer
  RCCE_barrier(&RCCE_COMM_WORLD);
#ifdef ENDLESS
  for(;;) {
#endif
#ifndef NO_FLOAT
  timer = RCCE_wtime();
#else
#endif
  for (round=0; round <MAXROUND; round++) {
    if (ME)  {
#ifdef CHECKBUF
        char oldcnt = buffer[0];
#endif

      RCCE_send(buffer, BUFSIZE, YOU);
      RCCE_recv(buffer, BUFSIZE, YOU);

#ifdef CHECKBUF
      if((++oldcnt) != buffer[0]) {
          printf("wrong value %d received, asserted %d would be there.\n",
                 buffer[0], oldcnt);
          for(int i = 0; i < BUFSIZE; i++) {
              printf("%d ", buffer[i]);
          }
          printf("\n");
          abort();
      }
      for(int i = 1; i <= 16; i++) {
          assert(buffer[i] == i);
      }
#endif
    } 
    else {
        timer = rdtsc();

      RCCE_recv(buffer, BUFSIZE, YOU);
#ifdef CHECKBUF
      buffer[0]++;
#endif
      RCCE_send(buffer, BUFSIZE, YOU);

      sum += rdtsc() - timer;

#if defined(ROUNDS_PER_SLICE) && defined(__scc__)
        if(round % ROUNDS_PER_SLICE == 0) {
            yield_timeslices = 1;
            thread_yield();
            yield_timeslices = 0;
        }
#endif
    }
  }
#ifndef NO_FLOAT
  timer = RCCE_wtime()-timer;

  if (!ME) printf("Round trip latency for %d bytes = %1.9lf\n", BUFSIZE, timer/MAXROUND);
#else

  if (!ME) printf("RTL %d b = %" PRIu64 "\n", BUFSIZE, sum/MAXROUND);
  sum = 0;
#endif

#ifdef ENDLESS
  }
#endif

  return(0);
}

