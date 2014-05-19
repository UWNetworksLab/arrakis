#ifndef _THCSYNC_H_
#define _THCSYNC_H_

#ifdef BARRELFISH
#include <barrelfish/barrelfish.h>
#endif
#include <assert.h>

// Configuration options:

// Synchronization primitives for use with THC
//
// All of the public abstractions (1) are thread-safe: they can be
// called from code running concurrently on multiple threads, (2)
// interact correctly with THC, in that if the calling AWE blocks then
// the thread will switch to any other AWE it has available.

// Internal types used by the synchronization primitives:
//
// Lists of waiters

struct thc_waiter {
  awe_t             *waiter;
  struct thc_waiter *next;
};

// Latch for short-term mutual exclusion:

struct thc_latch {
  volatile int c;
};

void thc_latch_init(struct thc_latch *l);

static inline void thc_latch_acquire(struct thc_latch *l) {
#ifdef _MSC_VER
    C_ASSERT(sizeof(l->c) == sizeof(LONG));
    while (InterlockedCompareExchange(reinterpret_cast<volatile long*>(&l->c), 1L, 0L) == 1) {
    }
    ;
#else
  do {
    if (__sync_bool_compare_and_swap(&l->c, 0, 1)) {
      break;
    }
  } while (1);
#endif
}

static inline void thc_latch_release(struct thc_latch *l) {
  assert(l->c == 1);
  l->c = 0;
}

// Public type provided by the synchronization primitives:
//
// Counting semaphores.

typedef struct {
  struct thc_latch   l;
  volatile int       val;
  struct thc_waiter *q;
} thc_sem_t;

void thc_sem_init(thc_sem_t *s, int val);
void thc_sem_p(thc_sem_t *s);       // Wait until val > 0.  val--.
errval_t thc_sem_p_x(thc_sem_t *s); // Wait until val > 0.  val--. (Cancelable)
void thc_sem_v(thc_sem_t *s);       // val++;

// Non-reentrant mutual exclusion locks.

typedef struct {
  thc_sem_t sem;
}  thc_lock_t;

void thc_lock_init(thc_lock_t *l);
void thc_lock_acquire(thc_lock_t *l);
void thc_lock_release(thc_lock_t *l);

// Condition variables.  No spurious wake-ups.  No guarantee of
// FIFO.  Signaller must hold lock held by waiter.

typedef struct {
  struct thc_latch   l;
  struct thc_waiter *q;
} thc_condvar_t;

void thc_condvar_init(thc_condvar_t *cv);
void thc_condvar_wait(thc_condvar_t *cv, thc_lock_t *lock);
errval_t thc_condvar_wait_x(thc_condvar_t *cv, thc_lock_t *lock);
void thc_condvar_signal(thc_condvar_t *cv);
void thc_condvar_broadcast(thc_condvar_t *cv);

// FIFO queues.  
//
// These are modeled on event-counts and sequencers, but are 
// designed to allow waiters to abandon their place in the queue.
// The design is motivated by requiring only storage space 
// in temporary stack-allocated queue-entry data structures used
// by the threads actually waiting.
//
// The caller passes in a thc_queue_entry_t which is used to 
// implement a doubly-link-list of waiters. 
//
// The thc_queue_await_turn... functions wait for the given queue
// entry to be the oldest that has entered and not left.  (The
// queue itself is not modified; a single caller may wait multiple
// times if necessary -- e.g., if interrupted each time).
//
// The thc_queue_leave function indicates that a given
// queue entry can be discarded.  The return value is 0 unless the
// queue entry is the oldest that has entered and not left.
// Otherwise, the thc_queue_leave function returns a count of the 
// number of queue entries that have entered and left without service,
// counting up (but not including) until the next queue entry (if any)
// that has entered and not left.  
//
// e.g.: a thread enters, waits in await_turn, and calls leave == 0 
//       a thread enters, is interrupted in await_turn, and calls leave == 1
//       thread T1 enters, thread T2 enters, T2 leaves=0, T1 leaves=2

typedef struct thc_queue_entry thc_queue_entry_t;

struct thc_queue_entry {
  int                bailed;
  int                served;
  thc_queue_entry_t *prev;
  thc_queue_entry_t *next;
};

typedef struct {
  thc_lock_t        l;
  thc_condvar_t     cv;
  thc_queue_entry_t start;
  thc_queue_entry_t end;
} thc_queue_t;

void thc_queue_init(thc_queue_t *tq);
void thc_queue_enter(thc_queue_t *tq,
                     thc_queue_entry_t *te);
void thc_queue_await_turn(thc_queue_t *tq,
                          thc_queue_entry_t *te);
errval_t thc_queue_await_turn_x(thc_queue_t *tq,
                                thc_queue_entry_t *te);
int thc_queue_leave(thc_queue_t *tq,
                    thc_queue_entry_t *te);

// Event counts.

typedef struct thc_ec_wait_list thc_ec_wait_list_t;

struct thc_ec_wait_list {
  awe_t              *waiter;
  volatile uint64_t   v;
  thc_ec_wait_list_t *next;
};

typedef struct {
  struct thc_latch    l;
  volatile uint64_t   n;
  thc_ec_wait_list_t *waiters;
} thc_ec_t;

void thc_ec_init(thc_ec_t *ec);
uint64_t thc_ec_read(thc_ec_t *ec);
void thc_ec_await(thc_ec_t *ec, uint64_t v);   // Until n>=v
void thc_ec_advance(thc_ec_t *ec, uint64_t d); // n+=d

// Sequencers

typedef struct {
  volatile long n;
} thc_seq_t;

void thc_seq_init(thc_seq_t *seq);
uint64_t thc_seq_read(thc_seq_t *seq);
uint64_t thc_seq_ticket(thc_seq_t *seq);  // n++

#endif // _THCSYNC_H_
