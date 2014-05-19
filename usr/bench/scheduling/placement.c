/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <rcce/RCCE.h>
#include <assert.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>

#define BUFSIZE  (64 * 1024)
#define MAXROUND 10000

static char buffer[BUFSIZE+1] = { 0 };

int RCCE_APP(int argc, char **argv)
{
  int YOU, ME, round;
  uint64_t timer = 0, sum = 0;

  RCCE_init(&argc, &argv);

  ME = RCCE_ue();
  if (RCCE_num_ues() != 2) {
    if (!ME) printf("Ping pong needs exactly two UEs; try again\n");
    return(1);
  }
  YOU = !ME;

  // synchronize before starting the timer
  for(;;) {
      RCCE_barrier(&RCCE_COMM_WORLD);
      for (round=0; round < MAXROUND; round++) {
          if (ME)  {
              RCCE_send(buffer, BUFSIZE, YOU);
              RCCE_recv(buffer, BUFSIZE, YOU);
          } else {
              timer = rdtsc();
              RCCE_recv(buffer, BUFSIZE, YOU);
              RCCE_send(buffer, BUFSIZE, YOU);
              sum += rdtsc() - timer;
          }
      }
      if (!ME) printf("RTL %d bytes = %" PRIu64 "\n", BUFSIZE, sum/MAXROUND);
      sum = 0;
  }

  return 0;
}
