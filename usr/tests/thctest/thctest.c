// This is a simple test of the THC runtime system.  

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include "thc/thc.h"

static void foo(int x) {
  for (int i = 0; i < 5; i++) {
    fprintf(stderr, "x=%d\n", x);
    x++;
    THCYield();
    fprintf(stderr, "x=%d\n", x);
  }
}

static int caller(int a, ...)
{
  // Test do..finish and async
  int x = 42;
  fprintf(stderr, "Before finish\n");
  DO_FINISH({
      fprintf(stderr, "Start of finish\n");
      for (int i = 0; i < 4; i ++) {
        ASYNC({foo(100);});
        ASYNC({foo(200);});
        ASYNC({ 
            int my_i = i;
            THCYield(); 
            THCYield();
            printf("my_i=%d i=%d x=%d\n", my_i, i, x);
            x++;
          });
      }
      fprintf(stderr, "End of finish\n");
    });
  fprintf(stderr, "After finish\n");
  
  // Test basic cancellation
  thc_sem_t sem;
  thc_sem_init(&sem, 0);
  for (int i = 0; i < 2; i++) {
    fprintf(stderr, "\nFirst case: cancellation does not fire\n");
    fprintf(stderr, "Entering cancel block at outer level\n");
    DO_FINISH_(cb1, {
        ASYNC({
            fprintf(stderr, "  Blocking on semaphore at inner level\n");
            int canceled = thc_sem_p_x(&sem);
            fprintf(stderr, "  Resumed after blocking on semaphore at inner level %s\n",
                    canceled ? "CANCELED" : "OK");
          });
        fprintf(stderr, "Signaling semaphore at outer level\n");
        thc_sem_v(&sem);
        fprintf(stderr, "Triggering cancellation at outer level\n");
        CANCEL(cb1);
      });
    fprintf(stderr, "Finished cancellation at outer level\n");
    
    fprintf(stderr, "\nSecond case: cancellation fires\n");
    fprintf(stderr, "Entering cancel block at outer level\n");
    DO_FINISH_(cb2, {
        ASYNC({
            fprintf(stderr, "  Blocking on semaphore at inner level\n");
            int canceled = thc_sem_p_x(&sem);
            fprintf(stderr, "  Resumed after blocking on semaphore at inner level %s\n",
                    canceled ? "CANCELED" : "OK");
          });
        fprintf(stderr, "Triggering cancellation at outer level\n");
        CANCEL(cb2);
      });
    fprintf(stderr, "Finished cancellation at outer level\n");
  }


  // Test cancellation using CVs
  thc_lock_t l;
  thc_condvar_t cv;
  thc_lock_init(&l);
  thc_condvar_init(&cv);
  for (int i = 0; i < 2; i++) {
    fprintf(stderr, "\nFirst case: cancellation does not fire\n");
    fprintf(stderr, "Entering cancel block at outer level\n");
    DO_FINISH_(cb3, {
        ASYNC({
            fprintf(stderr, "  Blocking on condvar at inner level\n");
            thc_lock_acquire(&l);
            int canceled = thc_condvar_wait_x(&cv, &l);
            thc_lock_release(&l);
            fprintf(stderr, "  Resumed after blocking on condvar at inner level %s\n",
                    canceled ? "CANCELED" : "OK");
          });
        fprintf(stderr, "Signaling condition variable at outer level\n");
        thc_lock_acquire(&l);
        thc_condvar_signal(&cv);
        thc_lock_release(&l);
        fprintf(stderr, "Triggering cancellation at outer level\n");
        CANCEL(cb3);
      });
    fprintf(stderr, "Finished cancellation at outer level\n");
    
    fprintf(stderr, "\nSecond case: cancellation fires\n");
    fprintf(stderr, "Entering cancel block at outer level\n");
    DO_FINISH_(cb4, {
        ASYNC({
            fprintf(stderr, "  Blocking on condvar at inner level\n");
            thc_lock_acquire(&l);
            int canceled = thc_condvar_wait_x(&cv, &l);
            thc_lock_release(&l);
            fprintf(stderr, "  Resumed after blocking on condition variable at inner level %s\n",
                    canceled ? "CANCELED" : "OK");
          });
        fprintf(stderr, "Triggering cancellation at outer level\n");
        CANCEL(cb4);
      });
    fprintf(stderr, "Finished cancellation at outer level\n");
  }

  fprintf(stderr, "Done!\n");
  return 0;
}


int main(void) {
    return caller(0, 1, 2, 3);
}
