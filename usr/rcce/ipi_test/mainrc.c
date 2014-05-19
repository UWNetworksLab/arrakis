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
#include <barrelfish/barrelfish.h>
#include <barrelfish/lmp_endpoints.h>
#include <if/rcce_defs.h>

#define MAXROUND 1000000
#define ROUNDS_PER_SLICE 50

extern uint32_t yield_timeslices;
extern struct rcce_binding *barray[MAX_CPUS];

static bool req_done = false;

static void rck_ping_handler(void *arg)
{
    req_done = true;
}

int RCCE_APP(int argc, char **argv){
  int YOU, ME, round;
  uint64_t timer = 0, sum = 0;

  int core1 = atoi(argv[3]);
  int core2 = atoi(argv[4]);

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

  struct rcce_ump_ipi_binding *ob;

  if(disp_get_core_id() == core1) {
      ob = (struct rcce_ump_ipi_binding *)barray[core2];
  } else {
      ob = (struct rcce_ump_ipi_binding *)barray[core1];
  }
  errval_t err = lmp_endpoint_deregister(ob->ipi_notify.iep);

  struct event_closure cl = {
      .handler = rck_ping_handler,
      .arg = NULL
  };

  for(;;) {
  for (round=0; round <MAXROUND; round++) {
    if (ME)  {
        ipi_notify_raise(&ob->ipi_notify);
      /* RCCE_send(buffer, BUFSIZE, YOU); */

        err = ipi_notify_register(&ob->ipi_notify, get_default_waitset(), cl);
        assert(err_is_ok(err));
        req_done = false;
        while(!req_done) {
            messages_wait_and_handle_next();
        }
      /* RCCE_recv(buffer, BUFSIZE, YOU); */

    } else {
        timer = rdtsc();

        err = ipi_notify_register(&ob->ipi_notify, get_default_waitset(), cl);
        assert(err_is_ok(err));
        req_done = false;
        while(!req_done) {
            messages_wait_and_handle_next();
        }
      /* RCCE_recv(buffer, BUFSIZE, YOU); */

        ipi_notify_raise(&ob->ipi_notify);
      /* RCCE_send(buffer, BUFSIZE, YOU); */

      sum += rdtsc() - timer;

        if(round % ROUNDS_PER_SLICE == 0) {
            yield_timeslices = 10;
            thread_yield();
            yield_timeslices = 0;
        }
    }
  }
  if (!ME) printf("RTL = %llu\n", sum/MAXROUND);
  sum = 0;
  }

  return(0);
}

