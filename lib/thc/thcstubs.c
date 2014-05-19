/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "barrelfish/barrelfish.h"
#include "thc/thc.h"
#include "thc/thcstubs.h"

#define DEBUGPRINTF debug_printf

#define DEBUG_STUBS(XX)
//#define DEBUG_STUBS(XX) do{ XX; } while (0)
#define DEBUG_STUBS_PREFIX         "         stubs:   "

//......................................................................
//
// Initialization

void thc_init_per_binding_state(struct thc_per_binding_state_t *thc) {
  thc_lock_init(&thc->thc_binding_lock);
  thc_sem_init(&thc->thc_next_sender, 1);
  thc->send_possible_event_requested = 0;
  thc->waiting_sender = NULL;
  thc->waiting_complete_sender = NULL;
}

void thc_init_per_recv_state(struct thc_per_recv_t *recv) {
  recv->r = NULL;
  thc_condvar_init(&recv->cv_bh);
  recv->num_bh = 0;
  recv->num_discard = 0;
  thc_queue_init(&recv->fifo_rpc_q);
  thc_lock_init(&recv->fifo_rpc_lock);
  recv->fifo_rpc_next_recv = 0;
}

//......................................................................
//
// Bottom-half receive functions
//
// The bottom-half stubs work as follows:
//
//   1. Call thc_start_bh/thc_start_demuxable_bh.  This checks whether
//      the call has been abandoned (no receive entry present), whether
//      the call is in progress (receive entry present, not yet waiting),
//      or whether the top-half is ready.  
//
//      NULL => abandoned
//
//      Otherwise, this returns with the receiver, and with thc_binding_lock
//      still held.
//
//   2. The Flounder-generated code deposits the parameters in the locations
//      identified by the receiver
//
//   3. Call thc_end_bh to release thc_binding_lock.

struct thc_receiver_info *thc_start_demuxable_bh(struct thc_per_binding_state_t *thc,
                                                 void *common_binding,
                                                 struct thc_per_recv_t *recv,
                                                 uint64_t demux) {
  assert(demux != NO_DEMUX);
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > start_demux_bh\n"));
  thc_lock_acquire(&thc->thc_binding_lock);

  bool found;
  struct thc_receiver_info *rxi;
  do {
    found = false;
    rxi = recv->r;
    while (rxi != NULL) {
      assert(rxi->demux != NO_DEMUX);
      if (rxi->demux == demux) {
        // Wait until the receiver is ready (the response to an OOO RPC
        // may come back after they've done the send but before they
        // block in receive).
        while (rxi->waiter == NULL) {
          recv->num_bh++;
          thc_condvar_wait(&recv->cv_bh, &thc->thc_binding_lock);
          recv->num_bh--;
        }
        assert(rxi->waiter != NULL);
        found = true;
        break;
      }
      rxi = rxi->next;
    }
    // No receiver entry present: they were canceled
    if (!found) {
      thc_lock_release(&thc->thc_binding_lock);
      DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < start_demux_bh\n"));
      return NULL;
    }
  } while (!found);

  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < start_demux_bh\n"));
  return rxi;
}


//......................................................................
//
// Top-half receive functions

// Simple receive: add rxi as the sole thc_receiver_info for this
// message, and wait until a bottom-half wakes us via rxi->waiter.

static void thc_receive0(void *s) {
  thc_lock_t *l = (thc_lock_t*)s;
  thc_lock_release(l);
}

errval_t thc_receive(struct thc_per_binding_state_t *thc,
                     struct thc_per_recv_t *recv,
                     struct thc_receiver_info *rxi) 
{
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_receive\n"));
  thc_lock_acquire(&thc->thc_binding_lock);

  assert(recv->r == NULL && "thc_receiver found existing receiver");

  // Install us as the receiver for this message
  rxi->next = NULL;
  rxi->demux = NO_DEMUX;
  recv->r = rxi;

  // Wake any bottom-half functions that are present
  if (recv->num_bh > 0) {
    thc_condvar_broadcast(&recv->cv_bh);
  }

  // Wait until a bottom-half provides a message
  //
  // We release the binding lock before blocking.  It is passed back to us
  // by the bottom-half receive function.
  THCSuspendThen(&rxi->waiter, thc_receive0, (void*) &thc->thc_binding_lock);

  // Remove us as the receiver
  assert(recv->r == rxi);  
  recv->r = NULL;

  thc_lock_release(&thc->thc_binding_lock);

  THCIncRecvCount();

  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_receive\n"));
  return 0;
}

struct thc_receive_cancel_info {
  struct thc_per_binding_state_t *thc;
  struct thc_receiver_info       *rxi;
  int                             was_canceled;
};

static void thc_receive_x_cancel_fn(void *c) {
  struct thc_receive_cancel_info *cinf = (struct thc_receive_cancel_info *)c;
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_receive_x_cancel_fn\n"));
  thc_lock_acquire(&cinf->thc->thc_binding_lock);
  assert(!cinf->was_canceled);
  if (cinf->rxi->waiter == NULL) {
    // We lost a race with an incoming message
    thc_lock_release(&cinf->thc->thc_binding_lock);
  } else {
    // Cancellation got the thc_binding_lock before an
    // incoming message arrived.
    cinf->was_canceled = 1;
    awe_t *awe = cinf->rxi->waiter;
    cinf->rxi->waiter = NULL;
    THCYieldTo(awe); // thc->thc_binding_lock passed to top-half function
  }
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_receive_x_cancel_fn\n"));
}

errval_t thc_receive_x(struct thc_per_binding_state_t *thc,
                       struct thc_per_recv_t *recv,
                       struct thc_receiver_info *rxi) 
{
  struct thc_receive_cancel_info cinf;
  cancel_item_t ci;
  int canceled = 0;

  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_receive_x\n"));

  thc_lock_acquire(&thc->thc_binding_lock);

  // Return THC_CANCELED if already requested
  if (THCIsCancelRequested()) {
    canceled = 1;
    goto done;
  }

  assert(recv->r == NULL && "thc_receiver found existing receiver");

  // Install us as the receiver for this message
  rxi->next = NULL;
  rxi->demux = NO_DEMUX;
  recv->r = rxi;

  // Wake any bottom-half functions that are present
  if (recv->num_bh > 0) {
    thc_condvar_broadcast(&recv->cv_bh);
  }

  // Wait until a bottom-half provides a message
  //
  // We release the binding lock before blocking.  It is passed back to us
  // by the bottom-half receive function or the cancelation function.
  cinf.thc = thc;
  cinf.rxi = rxi;
  cinf.was_canceled = 0;
  THCAddCancelItem(&ci, &thc_receive_x_cancel_fn, (void*)&cinf);
  THCSuspendThen(&rxi->waiter, thc_receive0, (void*) &thc->thc_binding_lock);
  canceled = cinf.was_canceled;
  if (!canceled) {
    // Remove cancel item if it did not run
    if (!THCCancelItemRan(&ci)) {
      THCRemoveCancelItem(&ci);
    }
  }

  // Remove us as the receiver
  assert(recv->r == rxi);  
  recv->r = NULL;
  
 done:
  thc_lock_release(&thc->thc_binding_lock);

  if (!canceled) THCIncRecvCount();

  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_receive_x\n"));
  return canceled ? THC_CANCELED : SYS_ERR_OK;
}

// Cause the next "n" messages to be discarded on receipt
void thc_discard(struct thc_per_binding_state_t *thc,
                 struct thc_per_recv_t *recv,
                 uint64_t n) 
{
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_discard\n"));
  thc_lock_acquire(&thc->thc_binding_lock);
  recv->num_discard += n;
  if (recv->num_bh > 0) {
    thc_condvar_broadcast(&recv->cv_bh);
  }
  thc_lock_release(&thc->thc_binding_lock);
    DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_discard\n"));
}

// Demux receive: add rxi to a list of receivers, and wait until
// a bottom-half wakes us.

void thc_start_receive_demux(struct thc_per_binding_state_t *thc,
                             struct thc_per_recv_t *recv,
                             struct thc_receiver_info *rxi) {
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_start_receive_demux\n"));
  thc_lock_acquire(&thc->thc_binding_lock);

  // Install us on the list of receivers
  assert(rxi->demux != NO_DEMUX);
  assert(rxi->waiter == NULL);
  rxi->next = recv->r;
  recv->r = rxi;

  thc_lock_release(&thc->thc_binding_lock);
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_start_recieve_demux\n"));
}

errval_t thc_receive_demux(struct thc_per_binding_state_t *thc,
                           struct thc_per_recv_t *recv,
                           struct thc_receiver_info *rxi) {
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_receive_demux\n"));
  thc_lock_acquire(&thc->thc_binding_lock);

  // Wake any bottom-half functions that are present
  if (recv->num_bh > 0) {
    thc_condvar_broadcast(&recv->cv_bh);
  }

  // Wait until a bottom-half provides a message
  //
  // We release the binding lock before blocking.  It is passed back to us
  // by the bottom-half receive function.

  THCSuspendThen(&rxi->waiter, thc_receive0, (void*) &thc->thc_binding_lock);

  // Remove us from the list of receivers
  struct thc_receiver_info **rxip = (struct thc_receiver_info **) &(recv->r);
  bool found = false;
  while (*rxip != NULL) {
    if (*rxip == rxi) {
      *rxip = rxi -> next;
      found = true;
      break;
    }
    rxip = &((*rxip)->next);
  }
  assert(found);

  thc_lock_release(&thc->thc_binding_lock);

  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_receive_demux\n"));
  return SYS_ERR_OK;
}

errval_t thc_receive_demux_x(struct thc_per_binding_state_t *thc,
                             struct thc_per_recv_t *recv,
                             struct thc_receiver_info *rxi) {
  struct thc_receiver_info **rxip;
  struct thc_receive_cancel_info cinf;
  cancel_item_t ci;
  int canceled = 0;

  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_receive_demux_x\n"));
  thc_lock_acquire(&thc->thc_binding_lock);

  // Return THC_CANCELED if already requested
  if (THCIsCancelRequested()) {
    canceled = 1;
    goto done;
  }

  // Wake any bottom-half functions that are present
  if (recv->num_bh > 0) {
    thc_condvar_broadcast(&recv->cv_bh);
  }

  // Wait until a bottom-half provides a message
  //
  // We release the binding lock before blocking.  It is passed back to us
  // by the bottom-half receive function.
  cinf.thc = thc;
  cinf.rxi = rxi;
  cinf.was_canceled = 0;
  THCAddCancelItem(&ci, &thc_receive_x_cancel_fn, (void*)&cinf);
  THCSuspendThen(&rxi->waiter, thc_receive0, (void*) &thc->thc_binding_lock);
  canceled = cinf.was_canceled;
  if (!canceled) {
    // Remove cancel item if it did not run
    if (!THCCancelItemRan(&ci)) {
      THCRemoveCancelItem(&ci);
    }
  }

 done:
  // Remove us from the list of receivers
  rxip = (struct thc_receiver_info **) &(recv->r);
  bool found = false;
  while (*rxip != NULL) {
    if (*rxip == rxi) {
      *rxip = rxi -> next;
      found = true;
      break;
    }
    rxip = &((*rxip)->next);
  }
  assert(found);

  thc_lock_release(&thc->thc_binding_lock);

  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_receive_demux_x\n"));
  return canceled ? THC_CANCELED : SYS_ERR_OK;
}

errval_t thc_cancel_receive_demux(struct thc_per_binding_state_t *thc,
                                  struct thc_per_recv_t *recv,
                                  struct thc_receiver_info *rxi) {
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_cancel_receive_demux\n"));
  thc_lock_acquire(&thc->thc_binding_lock);

  // Remove us from the list of receivers
  struct thc_receiver_info **rxip = (struct thc_receiver_info **) &(recv->r);
  bool found = false;
  while (*rxip != NULL) {
    if (*rxip == rxi) {
      *rxip = rxi -> next;
      found = true;
      break;
    }
    rxip = &((*rxip)->next);
  }
  assert(found);

  thc_lock_release(&thc->thc_binding_lock);

  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_cancel_receive_demux\n"));
  return THC_CANCELED;
}

//......................................................................

// A receive-any works as a series of steps:
//
//  1. Acquire the thc_binding_lock
//
//  2. Add ourselves as the sole receiver for each message we are
//     interested in
//
//  3. Block on rxi->cv
//
//  4. Remove ourselves from the messages we are interested in
//
//  5. Release the thc_binding_lock
//
// The flounder-generated code constructs this series of calls,
// doing steps 2 and 4 for different messages.

void thc_start_receive_any(struct thc_per_binding_state_t *thc) {
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_start_receive_any\n"));
  thc_lock_acquire(&thc->thc_binding_lock);
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_start_receive_any\n"));
}

void thc_start_receiving(struct thc_per_binding_state_t *thc,
                         struct thc_per_recv_t *recv,
                         struct thc_receiver_info *rxi) {
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_start_receiving\n"));
  // Install ourselves to receive the message
  assert(recv->r == NULL && 
         "receive_any attempted for message with pending receive");
  recv->r = rxi;
  rxi->next = NULL;
  rxi->demux = NO_DEMUX;

  // Wake any bottom-half functions present for this message
  if (recv->num_bh > 0) {
    thc_condvar_signal(&recv->cv_bh);
  }
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_start_receiving\n"));
}

static void thc_wait_receive_any0(void *s) {
  thc_lock_t *l = (thc_lock_t*)s;
  thc_lock_release(l);
}

void thc_wait_receive_any(struct thc_per_binding_state_t *thc,
                          struct thc_receiver_info *rxi) {
  // We release the binding lock before blocking.  It is passed back to us
  // by the bottom-half receive function.
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_wait_receive_any\n"));
  THCSuspendThen(&rxi->waiter, 
                 thc_wait_receive_any0, 
                 (void*)&thc->thc_binding_lock);
  THCIncRecvCount();
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_wait_receive_any\n"));
}

errval_t thc_wait_receive_any_x(struct thc_per_binding_state_t *thc,
                                struct thc_receiver_info *rxi) {
  struct thc_receive_cancel_info cinf;
  cancel_item_t ci;
  int canceled = 0;
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_wait_receive_any_x\n"));
  // We release the binding lock before blocking.  It is passed back to us
  // by the bottom-half receive function, or by the cancel action.
  cinf.thc = thc;
  cinf.rxi = rxi;
  cinf.was_canceled = 0;
  THCAddCancelItem(&ci, &thc_receive_x_cancel_fn, (void*)&cinf);
  THCSuspendThen(&rxi->waiter, 
                 thc_wait_receive_any0, 
                 (void*)&thc->thc_binding_lock);
  canceled = cinf.was_canceled;
  if (!canceled) {
    // Remove cancel item if it did not run
    if (!THCCancelItemRan(&ci)) {
      THCRemoveCancelItem(&ci);
    }
  }
  if (!canceled) THCIncRecvCount();
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_wait_receive_any\n"));
  return canceled ? THC_CANCELED : SYS_ERR_OK;
}

void thc_stop_receiving(struct thc_per_binding_state_t *thc,

                        struct thc_per_recv_t *recv,
                        struct thc_receiver_info *rxi) {
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_stop_receiving\n"));
  assert(recv->r == rxi);
  assert(rxi->next == NULL);
  recv->r = NULL;
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_stop_receiving\n"));
}

void thc_end_receive_any(struct thc_per_binding_state_t *thc) {
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_end_receive_any\n"));
  thc_lock_release(&thc->thc_binding_lock);
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_end_receive_any\n"));
}

//......................................................................
//
// Synchronization between senders

struct thc_await_send_cancel_info {
  struct thc_per_binding_state_t *thc;
  int                             was_canceled;
};

static void thc_send_possible_event(void *arg) {
  struct common_binding *b = (struct common_binding*) arg;
  struct thc_per_binding_state_t *thc;
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_send_possible_event\n"));
  thc = (struct thc_per_binding_state_t *)(b->st);
  thc_lock_acquire(&thc->thc_binding_lock);
  thc->send_possible_event_requested = 0;
  awe_t *awe = thc->waiting_sender;
  if (awe != NULL) {
    // Sender waiting
    thc->waiting_sender = NULL;
    THCSchedule(awe); // thc_binding_lock passed to sender
  } else {
    // No sender waiting (because they were canceled)
    thc_lock_release(&thc->thc_binding_lock);
  }
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_send_possible_event\n"));
}

static void thc_await_send_x_cancel_fn(void *c) {
  struct thc_await_send_cancel_info *cinf = (struct thc_await_send_cancel_info*)c;
  struct thc_per_binding_state_t *thc = cinf->thc;
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_await_send_x_cancel_fn\n"));
  thc_lock_acquire(&thc->thc_binding_lock);
  if (thc->waiting_sender == NULL) {
    // We lost a race with an incoming send_possible event
    thc_lock_release(&thc->thc_binding_lock);
  } else {
    // Cancellation got the thc_binding_lock before an
    // incoming send_possible event
    assert(!cinf->was_canceled);
    cinf->was_canceled = 1;
    awe_t *awe = cinf->thc->waiting_sender;
    cinf->thc->waiting_sender = NULL;
    THCYieldTo(awe); // thc->thc_binding_lock passed to await_send_x;
  }
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_await_send_x_cancel_fn\n"));
}

static void thc_await_send0(void *s) {
  thc_lock_t *l = (thc_lock_t*)s;
  thc_lock_release(l);
}


void thc_await_send(struct thc_per_binding_state_t *thc,
                    void *f) {
  struct common_binding *c = (struct common_binding *)f;
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_await_send\n"));
  // Synchronize with thc_send_possible_event callback
  thc_lock_acquire(&thc->thc_binding_lock);

  // Request an event when sending is possible
  if (!thc->send_possible_event_requested) {
    errval_t err = c->register_send(c, 
                                    get_default_waitset(), 
                                    MKCONT(thc_send_possible_event, c));
    if (err == FLOUNDER_ERR_TX_BUSY) {
      goto done;
    }

    assert(err_is_ok(err));
    thc->send_possible_event_requested = 1;
  }
  
  // Wait
  //
  // We release the binding lock before blocking.  It is passed back to us
  // by the notification

  THCSuspendThen(&thc->waiting_sender, 
                 thc_await_send0, 
                 (void*) &thc->thc_binding_lock);

 done:
  thc_lock_release(&thc->thc_binding_lock);
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_await_send\n"));
}

errval_t thc_await_send_x(struct thc_per_binding_state_t *thc,
                          void *f) {
  struct thc_await_send_cancel_info cinf;
  cancel_item_t ci;
  int canceled = 0;
  struct common_binding *c = (struct common_binding *)f;
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_await_send_x\n"));

  // Synchronize with thc_send_possible_event callback
  thc_lock_acquire(&thc->thc_binding_lock);

  // Return THC_CANCELED if already requested
  if (THCIsCancelRequested()) {
    canceled = 1;
    goto done;
  }

  // Request an event when sending is possible
  if (!thc->send_possible_event_requested) {
    errval_t err = c->register_send(c, 
                                    get_default_waitset(), 
                                    MKCONT(thc_send_possible_event, c));
    if (err == FLOUNDER_ERR_TX_BUSY) {
      goto done;
    }

    assert(err_is_ok(err));
    thc->send_possible_event_requested = 1;
  }
  
  // Wait
  //
  // We release the binding lock before blocking.  It is passed back to us
  // by the notification
  
  cinf.thc = thc;
  cinf.was_canceled = 0;
  THCAddCancelItem(&ci, &thc_await_send_x_cancel_fn, (void*)&cinf);
  THCSuspendThen(&thc->waiting_sender, 
                 thc_await_send0, 
                 (void*) &thc->thc_binding_lock);
  canceled = cinf.was_canceled;
  if (!canceled) {
    // Remove cancel item if it did not run
    if (!THCCancelItemRan(&ci)) {
      THCRemoveCancelItem(&ci);
    }
  }

 done:
  thc_lock_release(&thc->thc_binding_lock);

  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_await_send\n"));
  return canceled ? THC_CANCELED : SYS_ERR_OK;
}

// Send-completion callback.  This synchronizes with thc_complete_send
// so that a THC synchronous send does not return until the 
// message has actually been sent.

void thc_complete_send_cb(void *f) {
  struct common_binding *c = (struct common_binding *)f;
  struct thc_per_binding_state_t *thc;
  thc = (struct thc_per_binding_state_t *)(c->st);
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_complete_send_cb\n"));

  thc_lock_acquire(&thc->thc_binding_lock);

  if (thc->waiting_complete_sender) {
    awe_t *awe = thc->waiting_complete_sender;
    thc->waiting_complete_sender = NULL;
    THCSchedule(awe); // thc->thc_binding_lock passed to thc_complete_send
  } else {
    thc->thc_send_complete = 1;
    thc_lock_release(&thc->thc_binding_lock);
  }
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_complete_send_cb\n"));
}

static void thc_complete_send0(void *s) {
  thc_lock_t *l = (thc_lock_t*)s;
  thc_lock_release(l);
}

void thc_complete_send(struct thc_per_binding_state_t *thc,
                       void *f) {
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " > thc_complete_send\n"));
  // There is at most one sender, so if we are waiting to complete then
  // nobody else should be waiting for a can-send callback
  assert(thc->waiting_sender == NULL);

  thc_lock_acquire(&thc->thc_binding_lock);

  if (!thc->thc_send_complete) {
    // Send completion callback has not yet executed: wait
    THCSuspendThen(&thc->waiting_complete_sender,
                   thc_complete_send0,
                   (void*) &thc->thc_binding_lock);
  }

  thc_lock_release(&thc->thc_binding_lock);
  DEBUG_STUBS(DEBUGPRINTF(DEBUG_STUBS_PREFIX " < thc_complete_send\n"));
}




