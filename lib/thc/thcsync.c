//
// Synchronization primitives
//

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#ifdef BARRELFISH
#include <thc/thc.h>
#elif defined(_MSC_VER)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <thc/thc.h>
#else
#include "thc.h"
#endif

#define NOT_REACHED assert(0 && "Not reached")
#define DEBUGPRINTF debug_printf

#define DEBUG_SYNC(XX)
//#define DEBUG_SYNC(XX) do{ XX; } while (0)
#define DEBUG_SYNC_PREFIX         "         sync:    "

//......................................................................
//
// Latches

void thc_latch_init(struct thc_latch *l) {
  l->c = 0;
}

//......................................................................
//
// Semaphores

void thc_sem_init(thc_sem_t *s, int val) {
  thc_latch_init(&s->l);
  s->val = val;
  s->q = NULL;
}

static void thc_sem_p0(void *s) {
  thc_sem_t *sem = (thc_sem_t*)s;
  thc_latch_release(&sem->l);
}

void thc_sem_p(thc_sem_t *s) {
  thc_latch_acquire(&s->l);
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_sem_p(%p) %d\n", s, s->val));
  if (s->val == 0) {
    struct thc_waiter w;
    w.next = s->q;
    s->q = &w;
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_sem_p sleeping %p\n", &w));
    THCSuspendThen(&w.waiter, thc_sem_p0, (void*)s); // Sleep ......
    thc_latch_acquire(&s->l);
  } else {
    s->val --;
  }
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_sem_p(%p) done\n", s));
  thc_latch_release(&s->l);
}

// Information passed to a semaphore wait cancel function: we need a
// reference to the semaphore (so we can walk down its list to remove
// the waiter) and a reference to the waiter itself (to identify the
// item to remove, and to identify the AWE to resume).  
//
// NB: we distinguish woken/canceled by an explicit flag in this 
// structure to avoid races between (i) getting the lock in the
// cancelation action, and (ii) being woken from a V operation.  We
// cannot just look at whether or not the cancelation action ran.
struct thc_sem_cancel_info {
  struct thc_waiter *waiter_info;
  thc_sem_t         *sem;
  int                was_canceled;
};

static void thc_sem_p_x_cancel_fn(void *c) {
  struct thc_sem_cancel_info *cinf = (struct thc_sem_cancel_info *)c;
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Woken cinf=%p\n", cinf));

  // When we start waiting for the latch, we know that the thread
  // that invoked thc_sem_p_x has reached the end of a cancel block
  // and is processing cancel items.  We know that the cancel 
  // item for the thc_sem_p_x has not been removed.
  thc_latch_acquire(&cinf->sem->l);
  // When we finish waiting for the latch, it is possible that 
  // another thread has executed a V operation that has unblocked
  // the thc_sem_p_x.

  if (cinf->waiter_info->waiter == NULL) {
    // Cancelation lost wake/cancel race: leave was_canceled false,
    // leave waiter on the thread's run-queue, release the lock and
    // return.
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Cancelation lost wake/cancel race\n"));
  } else {
    // Cancelation won any wake/cancel race: remove the waiter so
    // that it cannot receive a subsequent V.
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Looking for wait-queue entry %p\n", cinf->waiter_info));
    cinf -> was_canceled = 1;
    struct thc_waiter **qptr = &(cinf->sem->q);
    while ((*qptr) != NULL && *qptr != cinf->waiter_info) {
      qptr = &((*qptr)->next);
    }
    assert((*qptr) != NULL && "Could not find waiter entry on cancel");
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Removed wait-queue entry (now %p -> %p)\n", qptr, (*qptr)->next));
    *qptr = (*qptr)->next;

    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Waking p_x operation\n"));
    THCSchedule(cinf->waiter_info->waiter);
  }

  thc_latch_release(&cinf->sem->l);
}

errval_t thc_sem_p_x(thc_sem_t *s) {
  // Return THC_CANCELED if already requested
  if (THCIsCancelRequested()) {
    return THC_CANCELED;
  }

  int canceled = 0;
  thc_latch_acquire(&s->l);
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_sem_p_x(%p) %d\n", s, s->val));
  if (s->val == 0) {
    struct thc_sem_cancel_info cinf;
    cancel_item_t ci;
    struct thc_waiter w;
    cinf.waiter_info = &w;
    cinf.sem = s;
    cinf.was_canceled = 0;
    w.next = s->q;
    s->q = &w;
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_sem_p_x sleeping %p\n", &w));
    
    THCAddCancelItem(&ci, &thc_sem_p_x_cancel_fn, (void*)&cinf);
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Adding cancel item\n"));
    THCSuspendThen(&w.waiter, thc_sem_p0, (void*)s); // Sleep ......
    canceled = cinf.was_canceled;
    if (!canceled) {
      if (!THCCancelItemRan(&ci)) {
        DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Removing cancel item\n"));
        THCRemoveCancelItem(&ci);
      }
    }
    thc_latch_acquire(&s->l);
  } else {
    s->val --;
  }
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_sem_p_x(%p) done\n", s));
  thc_latch_release(&s->l);
  return canceled ? THC_CANCELED : SYS_ERR_OK;
}

void thc_sem_v(thc_sem_t *s) {
  thc_latch_acquire(&s->l);
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_sem_v(%p)\n", s));
  if (s->q != NULL) {
    struct thc_waiter *w = s->q;
    s->q = w->next;
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_sem_v waking %p\n", w));
    // NB: unlink the waiter structure before scheduling: they may
    // run immediately on a concurrent thread, and de-allcoate the
    // waiter structure.
    awe_t *to_wake = w->waiter;
    // NULL out waiter; signals to the cancelation function
    // that they lost a wake-up/cancel race
    w->waiter = NULL; 
    THCSchedule(to_wake);
  } else {
    s->val ++;
  }
  thc_latch_release(&s->l);
}

//......................................................................
//
// Locks

#ifndef MUTEX_IS_LATCH
void thc_lock_init(thc_lock_t *l) {
  thc_sem_init(&(l->sem), 1);
}

void thc_lock_acquire(thc_lock_t *l) {
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_lock_acquire %p\n", l));
  thc_sem_p(&(l->sem));
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_lock_acquire done\n"));
}

void thc_lock_release(thc_lock_t *l) {
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_lock_release %p\n", l));
  thc_sem_v(&(l->sem));
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_lock_release done\n"));
}
#endif

//......................................................................
//
// Condition variables

void thc_condvar_init(thc_condvar_t *cv) {
  thc_latch_init(&cv->l);
  cv->q = NULL;
}

typedef struct {
  thc_condvar_t *cv;
  thc_lock_t *lock;
} thc_condvar_wait0_info_t;

static void thc_condvar_wait0(void *v) {
  thc_condvar_wait0_info_t *info = (thc_condvar_wait0_info_t *) v;
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_condvar_wait releasing lock %p\n", info->lock));
  thc_latch_release(&info->cv->l);
  thc_lock_release(info->lock);
}

void thc_condvar_wait(thc_condvar_t *cv, thc_lock_t *lock) {
  struct thc_waiter w;
  thc_condvar_wait0_info_t info;
  thc_latch_acquire(&cv->l);
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_condvar_wait(%p,%p)\n", cv, lock));
  w.next = cv->q;
  cv->q = &w;
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_condvar_wait sleeping %p\n", &w));
  info.cv = cv;
  info.lock = lock;
  THCSuspendThen(&w.waiter, thc_condvar_wait0, (void*)&info); // Sleep ......
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_condvar_wait retaking lock %p\n", lock));
  thc_latch_acquire(&cv->l);
  thc_lock_acquire(lock);
  thc_latch_release(&cv->l);
}

// Information passed to a condvar wait cancel function: we need a
// reference to the condvar (so we can walk down its list to remove
// the waiter) and a reference to the waiter itself (to identify the
// item to remove, and to identify the AWE to resume).  
//
// NB: we distinguish woken/canceled by an explicit flag in this 
// structure to avoid races between (i) getting the lock in the
// cancelation action, and (ii) being woken from a notify operation.  We
// cannot just look at whether or not the cancelation action ran.
struct thc_cv_cancel_info {
  struct thc_waiter *waiter_info;
  thc_condvar_t     *cv;
  int                was_canceled;
};

static void thc_condvar_wait_x_cancel_fn(void *c) {
  struct thc_cv_cancel_info *cinf = (struct thc_cv_cancel_info *)c;
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Woken cinf=%p\n", cinf));

  thc_latch_acquire(&cinf->cv->l);

  if (cinf->waiter_info->waiter == NULL) {
    // Cancelation lost wake/cancel race: leave was_canceled false,
    // leave waiter on the thread's run-queue, release the lock and
    // return.
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Cancelation lost wake/cancel race\n"));
  } else {
    // Cancelation won any wake/cancel race: remove the waiter so
    // that it cannot receive a subsequent notify.
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Looking for wait-queue entry %p\n", cinf->waiter_info));
    cinf -> was_canceled = 1;
    struct thc_waiter **qptr = &(cinf->cv->q);
    while ((*qptr) != NULL && *qptr != cinf->waiter_info) {
      qptr = &((*qptr)->next);
    }
    assert((*qptr) != NULL && "Could not find waiter entry on cancel");
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Removed wait-queue entry (now %p -> %p)\n", qptr, (*qptr)->next));
    *qptr = (*qptr)->next;

    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Waking condvar_wait_x operation\n"));
    THCSchedule(cinf->waiter_info->waiter);
  }

  thc_latch_release(&cinf->cv->l);
}

errval_t thc_condvar_wait_x(thc_condvar_t *cv, thc_lock_t *lock) {
  // Return THC_CANCELED if already requested
  if (THCIsCancelRequested()) {
    return THC_CANCELED;
  }

  int canceled = 0;
  struct thc_cv_cancel_info cinf;
  cancel_item_t ci;
  struct thc_waiter w;
  thc_condvar_wait0_info_t info;
  thc_latch_acquire(&cv->l);
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_condvar_wait(%p,%p)\n", cv, lock));
  cinf.waiter_info = &w;
  cinf.cv = cv;
  cinf.was_canceled = 0;
  w.next = cv->q;
  cv->q = &w;
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_condvar_wait sleeping %p\n", &w));
  info.cv = cv;
  info.lock = lock;
  THCAddCancelItem(&ci, &thc_condvar_wait_x_cancel_fn, (void*)&cinf);
  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Adding cancel item\n"));
  THCSuspendThen(&w.waiter, thc_condvar_wait0, (void*)&info); // Sleep ......
  canceled = cinf.was_canceled;
  if (!canceled) {
    if (!THCCancelItemRan(&ci)) {
      DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "Removing cancel item\n"));
      THCRemoveCancelItem(&ci);
    }
  }

  DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_condvar_wait retaking lock %p\n", lock));
  thc_latch_acquire(&cv->l);
  thc_lock_acquire(lock);
  thc_latch_release(&cv->l);

  return canceled ? THC_CANCELED : SYS_ERR_OK;
}

void thc_condvar_signal(thc_condvar_t *cv) {
  thc_latch_acquire(&cv->l);
  if (cv->q != NULL) {
    struct thc_waiter *w = cv->q;
    awe_t *to_wake = w->waiter;
    // NULL out waiter; signals to the cancelation function
    // that they lost a wake-up/cancel race
    w->waiter = NULL; 
    cv->q = w->next;
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_condvar_signal waking %p\n", w));
    THCSchedule(to_wake);
  }
  thc_latch_release(&cv->l);
}

void thc_condvar_broadcast(thc_condvar_t *cv) {
  thc_latch_acquire(&cv->l);
  struct thc_waiter *w = cv->q;
  while (w != NULL) {
    awe_t *to_wake = w->waiter;
    // NULL out waiter; signals to the cancelation function
    // that they lost a wake-up/cancel race
    w->waiter = NULL; 
    DEBUG_SYNC(DEBUGPRINTF(DEBUG_SYNC_PREFIX "thc_condvar_signal waking %p\n", w));
    THCSchedule(to_wake);
    w=w->next;
  }
  cv->q = NULL;
  thc_latch_release(&cv->l);
}

//......................................................................
//
// FIFO queue

void thc_queue_init(thc_queue_t *tq) {
  thc_lock_init(&tq->l);
  thc_condvar_init(&tq->cv);
  tq->start.bailed = tq->end.bailed = 0;
  tq->start.served = tq->end.served = 0;
  tq->start.prev = tq->end.next = NULL;
  tq->start.next = &(tq->end);
  tq->end.prev = &(tq->start);
}

void thc_queue_enter(thc_queue_t *tq,
                     thc_queue_entry_t *te) {
  thc_lock_acquire(&tq->l);
  te->bailed = 0;
  te->served = 0;
  te->prev = tq->end.prev;
  te->next = &(tq->end);
  tq->end.prev->next = te;
  tq->end.prev = te;
  thc_lock_release(&tq->l);
}

void thc_queue_await_turn(thc_queue_t *tq,
                          thc_queue_entry_t *te) {
  thc_lock_acquire(&tq->l);
  while (tq->start.next != te) {
    thc_condvar_wait(&tq->cv, &tq->l);
  }
  te->served = 1;
  thc_lock_release(&tq->l);
}

errval_t thc_queue_await_turn_x(thc_queue_t *tq,
                                thc_queue_entry_t *te) {
  errval_t result = SYS_ERR_OK;
  thc_lock_acquire(&tq->l);
  while (tq->start.next != te && 
         result == SYS_ERR_OK) {
    result = thc_condvar_wait_x(&tq->cv, &tq->l);
  }
  if (result == SYS_ERR_OK) {
    te->served = 1;
  }
  thc_lock_release(&tq->l);
  return result;
}

int thc_queue_leave(thc_queue_t *tq,
                    thc_queue_entry_t *te) {
  int result;

  thc_lock_acquire(&tq->l);

  // If we were not served then add ourself into the list
  // of threads that bailed without service
  if (!te->served) {
    te->bailed += 1;
  }

  if (tq->start.next == te) {
    // We are at the head of the queue
    result = te->bailed;
    // Wake the next waiter (if any)
    if (te->next != &(tq->end)) {
      thc_condvar_broadcast(&tq->cv);
    }
  } else {
    // We are not at the head of the queue.
    result = 0;
    te->prev->bailed += te->bailed;
  }

  // Remove self from queue
  te->prev->next = te->next;
  te->next->prev = te->prev;

  thc_lock_release(&tq->l);
  return result;
}


//......................................................................
//
// Event counts

void thc_ec_init(thc_ec_t *ec) {
  thc_latch_init(&ec->l);
  ec->n = 0;
  ec->waiters = NULL;
}

// Return current value
uint64_t thc_ec_read(thc_ec_t *ec) {
  uint64_t result;

  thc_latch_acquire(&ec->l);
  result = ec->n;
  thc_latch_release(&ec->l);
  return result;
}

static void thc_ec_await0(void *e) {
  thc_ec_t *ec = (thc_ec_t*)e;
  thc_latch_release(&ec->l);
}

// Wait until the count reaches or exceeds v
void thc_ec_await(thc_ec_t *ec, uint64_t v) {
  thc_latch_acquire(&ec->l);
  if (ec->n < v) {
    // We need to actually wait
    thc_ec_wait_list_t w;
    w.v = v;
    w.next = ec->waiters;
    ec->waiters = &w;

    THCSuspendThen(&w.waiter, thc_ec_await0, (void*)ec); // Sleep ....
    thc_latch_acquire(&ec->l);
  }

  thc_latch_release(&ec->l);
}

// Advance the count by n
void thc_ec_advance(thc_ec_t *ec, uint64_t n) {
  thc_ec_wait_list_t **ptr_w;

  thc_latch_acquire(&ec->l);
  ec->n += n;
  ptr_w = &ec->waiters;
  while (*ptr_w != NULL) {
    thc_ec_wait_list_t *w = *ptr_w;
    if (w->v <= ec->n) {
      // NB: unlink the waiter structure before scheduling: they may
      // run immediately on a concurrent thread, and de-allcoate the
      // waiter structure.
      *ptr_w = w->next;
      THCSchedule(w->waiter);
    } else {
      ptr_w = &(w->next);
    }
  }
  thc_latch_release(&ec->l);
}

//......................................................................

// Sequencers

void thc_seq_init(thc_seq_t *seq) {
  seq->n = 0;
}

uint64_t thc_seq_read(thc_seq_t *seq) {
  return seq->n;
}

uint64_t thc_seq_ticket(thc_seq_t *seq) {
#ifdef _MSC_VER
    C_ASSERT(sizeof(LONGLONG) == sizeof(seq->n));
    return InterlockedIncrement64(reinterpret_cast<volatile LONGLONG*>(&seq->n)) - 1;
#else
  uint64_t result;
  do {
    result = seq->n;
    if (__sync_bool_compare_and_swap(&seq->n, result, result+1)) {
      break;
    }
  } while(1);
  assert((result >= 0) && "URK!  Sequencer wrapped");
  return result;
#endif
}
