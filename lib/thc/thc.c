// THC runtime system
//
// Naming conventions:
//
//  THCStudlyCaps    - Functions for use from user code.
//
//  _thc_...         - Intrinsic functions, called from compiler-generated
//                     code.  Their prototypes must match Intrinsics.td
//
//  thc_lower_case   - Internal functions used in this library.
//
//  thc_lower_case_0 - Arch-OS specific functions (implemented at the
//                     bottom of this file).

#define FB_KIND_FINISH        0
#define FB_KIND_TOP_FINISH    1

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#ifdef BARRELFISH
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/waitset.h>
#include <thc/thc.h>
#else
#include "thc.h"
#endif

#ifdef linux
#include <pthread.h>
#endif

#define NOT_REACHED assert(0 && "Not reached")

/* It is necessary to set the esp of a lazy awe some way into it's lazy */
/* allocated stack, so that it can pass arguments below its current esp */
/* This value defines the size of the buffer (should be more than size  */
/* of arguments passed to any function call).                            */
#define LAZY_STACK_BUFFER 512

#ifdef BARRELFISH
#define DEBUGPRINTF debug_printf
#else
#define DEBUGPRINTF printf
#endif

//#define DEBUG_STATS(XX)
#define DEBUG_STATS(XX) do{ XX; } while (0)
#define DEBUG_STATS_PREFIX        "         stats:    "

//#define VERBOSE_DEBUG

#ifdef VERBOSE_DEBUG
#define DEBUG_YIELD(XX) do{ XX; } while (0)
#define DEBUG_STACK(XX) do{ XX; } while (0)
#define DEBUG_AWE(XX) do{ XX; } while (0)
#define DEBUG_FINISH(XX) do{ XX; } while (0)
#define DEBUG_CANCEL(XX) do{ XX; } while (0)
#define DEBUG_INIT(XX) do{ XX; } while (0)
#define DEBUG_DISPATCH(XX) do{ XX; } while (0)
#else
#define DEBUG_YIELD(XX)
#define DEBUG_STACK(XX)
#define DEBUG_AWE(XX)
#define DEBUG_FINISH(XX) 
#define DEBUG_CANCEL(XX)
#define DEBUG_INIT(XX)
#define DEBUG_DISPATCH(XX)
#endif

#define DEBUG_YIELD_PREFIX        "         yield:    "
#define DEBUG_STACK_PREFIX        "         stack:    "
#define DEBUG_AWE_PREFIX          "         awe:      "
#define DEBUG_FINISH_PREFIX       "         finish:   "
#define DEBUG_CANCEL_PREFIX       "         cancel:   "
#define DEBUG_INIT_PREFIX         "         init:     "
#define DEBUG_DISPATCH_PREFIX     "         dispatch: "

/***********************************************************************/

// Prototypes
// 
// NB: those marked as "extern" are actually defined in this same file,
// but the entire function (including label, prolog, epilogue, etc) is
// in inline-asm, and so the definition is not visible to the compiler.

static void thc_awe_init(awe_t *awe, void *eip, void *ebp, void *esp);
static void thc_dispatch(PTState_t *pts);

extern void thc_awe_execute_0(awe_t *awe);
extern void thc_on_alt_stack_0(void *stacktop, void *fn, void *args);
static void *thc_alloc_new_stack_0(void);

static PTState_t *thc_get_pts_0(void);
static void thc_set_pts_0(PTState_t *pts);

static inline void thc_schedule_local(awe_t *awe);

/***********************************************************************/

// Per-thread state

static PTState_t *PTS(void) {
  PTState_t *pts = thc_get_pts_0();
#ifndef NDEBUG
  if (pts!=NULL) {
    pts->getTls++;
  }
#endif
  return pts;
}

static void InitPTS(void) {
  PTState_t *pts = malloc(sizeof(PTState_t));
  memset(pts, 0, sizeof(PTState_t));
  thc_latch_init(&pts->latch);
  assert((PTS() == NULL) && "PTS already initialized");
  thc_set_pts_0(pts);
}

static void thc_pts_lock(PTState_t *t) {
#ifndef NDEBUG
  t->lock++;
#endif
  thc_latch_acquire(&t->latch);
}

static void thc_pts_unlock(PTState_t *t) {
  thc_latch_release(&t->latch);
}


#ifdef NDEBUG
static void thc_print_pts_stats(PTState_t *t, int clear) { }
#else
static struct thc_latch debug_latch = {0};
static void thc_print_pts_stats(PTState_t *t, int clear) {
  thc_latch_acquire(&debug_latch);

  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "----------------------------------------\n"));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "  %c stacks         %8d %8d\n",
                          (t->stacksAllocated == t->stacksDeallocated) ? ' ' : '*',
                          t->stacksAllocated, t->stacksDeallocated));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "  %c stack memories %8d %8d\n",
                          (t->stackMemoriesAllocated == t->stackMemoriesDeallocated) ? ' ' : '*',
                          t->stackMemoriesAllocated, t->stackMemoriesDeallocated));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "  %c finish blocks  %8d %8d\n",
                          (t->finishBlocksStarted == t->finishBlocksEnded) ? ' ' : '*',
                          t->finishBlocksStarted, t->finishBlocksEnded));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "  %c async calls    %8d %8d\n",
                          (t->asyncCallsStarted == t->asyncCallsEnded) ? ' ' : '*',
                          t->asyncCallsStarted, t->asyncCallsEnded));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "  %c awe            %8d %8d\n",
                          (t->aweCreated == t->aweResumed) ? ' ' : '*',
                          t->aweCreated, t->aweResumed));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "  %c idle           %8d %8d\n",
                          (t->idleStarted == t->idleComplete) ? ' ' : '*',
                          t->idleStarted, t->idleComplete));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "    get-tls        %8d\n",
                          t->getTls));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "    lock           %8d\n",
                          t->lock));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "    cancels        %8d\n",
                          t->cancelsRequested));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "  %c actions        %8d %8d %8d\n",
                          (t->cancelsAdded == (t->cancelsRun + t->cancelsRemoved)) ? ' ' : '*',
                          t->cancelsAdded, t->cancelsRun, t->cancelsRemoved));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "    message send   %8d\n",
                          t->sendCount));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "    message recv   %8d\n",
                          t->recvCount));
  DEBUG_STATS(DEBUGPRINTF(DEBUG_STATS_PREFIX "----------------------------------------\n"));

  if (clear) {
    t->stacksAllocated -= t->stacksDeallocated;
    t->stacksDeallocated = 0;
    t->stackMemoriesAllocated -= t->stackMemoriesDeallocated;
    t->stackMemoriesDeallocated = 0;
    t->finishBlocksStarted -= t->finishBlocksEnded;
    t->finishBlocksEnded = 0;
    t->asyncCallsStarted -= t->asyncCallsEnded;
    t->asyncCallsEnded = 0;
    t->aweCreated -= t->aweResumed;
    t->aweResumed = 0;
    t->idleStarted -= t->idleComplete;
    t->idleComplete = 0;
    t->getTls = 0;
    t->lock = 0;
    t->cancelsRequested = 0;
    t->cancelsAdded = 0;
    t->cancelsRun = 0;
    t->cancelsRemoved = 0;
  }

  thc_latch_release(&debug_latch);
}

#endif

/***********************************************************************/

// Stack management

// An value of type stack_t represents a stack which has been allocated
// but which is not currently in use.  It is placed at the top of the
// memory reserved for the stack.

#define STACK_COMMIT_BYTES (16*4096)
#define STACK_GUARD_BYTES  (1*4096)

// Allocate a new stack, returning an address just above the top of
// the committed region.  The stack comprises STACK_COMMIT_BYTES
// followed by an inaccessible STACK_GUARD_BYTES.
//
// There is currently no support for extending a stack, or allowing it
// to be discontiguous

void *_thc_allocstack(void) {
  PTState_t *pts = PTS();
  void *result = NULL;
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "> AllocStack\n"));
  if (pts->free_stacks != NULL) {
    // Re-use previously freed stack
    DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "  Re-using free stack\n"));
    stack_t *r = pts->free_stacks;
    pts->free_stacks = pts->free_stacks->next;
    result = ((void*)r) + sizeof(stack_t);
  } else {
    result = (void*)thc_alloc_new_stack_0();
#ifndef NDEBUG
    pts->stackMemoriesAllocated ++;
#endif
  }
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "< AllocStack = %p\n", result));
#ifndef NDEBUG
  pts->stacksAllocated ++;
#endif
  return result;
}

// De-allocate a stack back to THC's pool of free stacks

void _thc_freestack(void *s) {
  PTState_t *pts = PTS();
  stack_t *stack = (stack_t*)(s - sizeof(stack_t));
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "> FreeStack(%p)\n", stack));
  stack->next = pts->free_stacks;
  pts->free_stacks = stack;
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "< FreeStack\n"));
#ifndef NDEBUG
  pts->stacksDeallocated ++;
#endif
}

static void thc_pendingfree(PTState_t * pts) {
  if (pts->pendingFree) {
    DEBUG_DISPATCH(DEBUGPRINTF(DEBUG_DISPATCH_PREFIX
                               "  pending free of stack %p\n",
                               PTS()->pendingFree));
    _thc_freestack(pts->pendingFree);
    pts->pendingFree = NULL;
  }
}

void _thc_pendingfree(void) {
  thc_pendingfree(PTS());
}

#ifdef CONFIG_LAZY_THC

// This checks whether the awe's lazy stack is finished with (according to 
// the provided esp, and puts it on pending free list if so.

static void check_lazy_stack_finished (PTState_t *pts, void *esp) {
  assert(pts->curr_lazy_stack);
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX
			  "> CheckLazyStackFinished(s=%p,esp+buf=%p)\n",
			  pts->curr_lazy_stack, esp + LAZY_STACK_BUFFER));
  if ((esp + LAZY_STACK_BUFFER) == pts->curr_lazy_stack) {
    // nothing on lazy stack, we can safely free it
    DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "  freeing lazy stack %p\n", 
			    pts->curr_lazy_stack));
    assert(pts->pendingFree == NULL);
    pts->pendingFree = pts->curr_lazy_stack;
  }
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "< CheckLazyStackFinished()\n"));
}

// Allocate a lazy stack for this awe's continuation to execute on.

static void alloc_lazy_stack (awe_t *awe) {
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "> AllocLazyStack(awe=%p)\n", 
			  awe));
  assert(awe->status == LAZY_AWE && !awe->lazy_stack);
  awe->lazy_stack = _thc_allocstack();
  void * new_esp =  awe->lazy_stack - LAZY_STACK_BUFFER;
  *((void **) new_esp) = awe->esp;
  awe->esp = new_esp;
  awe->status = ALLOCATED_LAZY_STACK;
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "< AllocLazyStack(awe=%p,s=%p)\n",
			  awe, awe->lazy_stack));
}

#else // EAGER_THC
static inline void alloc_lazy_stack (awe_t *awe) {
    /* Shouldn't be called in eager version */
    NOT_REACHED;
}
static inline void check_lazy_stack_finished (PTState_t *pts, awe_t *awe) {
    /* Nothing to do here in eager version */
}
#endif // CONFIG_LAZY_THC

// Execute "fn(args)" on the stack growing down from "stacktop".  This is
// just a wrapper around the arch-os specific function.

void _thc_onaltstack(void *stacktop, void *fn, void *args) {
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "> OnAltStack(%p, %p, %p)\n",
                          stacktop, fn, args));

  thc_on_alt_stack_0(stacktop, fn, args);

  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "< OnAltStack\n"));
}

/***********************************************************************/

static void thc_run_idle_fn(void) {
  PTState_t *pts = PTS();
  void *s = pts->idle_stack;
#ifndef NDEBUG
  pts->idleStarted++;
#endif
  DEBUG_DISPATCH(DEBUGPRINTF(DEBUG_DISPATCH_PREFIX "  calling idle fn\n"));
  pts->idle_fn(pts->idle_args);
  DEBUG_DISPATCH(DEBUGPRINTF(DEBUG_DISPATCH_PREFIX "  returned from idle fn\n"));
#ifndef NDEBUG
  pts->idleComplete++;
#endif
  pts->pendingFree = s;
  thc_dispatch(pts);
  NOT_REACHED;
}

__attribute__ ((unused))
static void re_init_dispatch_awe(void *a, void *arg) {
  PTState_t *pts = PTS();
  awe_t *awe = (awe_t *)a;
  pts->dispatch_awe = *awe;
  assert(awe->status == EAGER_AWE && !pts->curr_lazy_stack);
#ifndef NDEBUG
  // Do not count dispatch AWE in the debugging stats (it is created 
  // once and then resumed once per dispatch-loop entry, so it obscures
  // mis-match between normal 1-shot AWEs)
  pts->aweCreated--;
#endif
  thc_dispatch(pts);
}

// Dispatch loop
//
// Currently, this maintains a doubly-linked list of runnable AWEs.
// New AWEs are added to the tail of the list.  Execution proceeds from
// the head.
//
// dispatch_awe is initialized to refer to the entry point for the
// "dispatch loop" function.

static void thc_dispatch_loop(void) {
  PTState_t *pts = PTS();

  // Re-initialize pts->dispatch_awe to this point, just after we have
  // read PTS.  This will save the per-thread-state access on future
  // executions of the function.
  CALL_CONT((unsigned char*)&re_init_dispatch_awe, NULL);

  DEBUG_DISPATCH(DEBUGPRINTF(DEBUG_DISPATCH_PREFIX "> dispatch_loop\n"));

  thc_pendingfree(pts);
  
  // Pick up work passed to us from other threads
  if (pts->aweRemoteHead.next != &pts->aweRemoteTail) {
    awe_t *tmp = pts->aweHead.next;
    thc_pts_lock(pts);
    // Move remote list into our local list
    pts->aweHead.next = pts->aweRemoteHead.next;
    pts->aweRemoteHead.next->prev = &pts->aweHead;
    pts->aweRemoteTail.prev->next = tmp;
    tmp->prev = pts->aweRemoteTail.prev;
    // Clear remote list
    pts->aweRemoteHead.next = &pts->aweRemoteTail;
    pts->aweRemoteTail.prev = &pts->aweRemoteHead;
    thc_pts_unlock(pts);
  } 
  
  if (pts->aweHead.next == &pts->aweTail) {
    DEBUG_DISPATCH(DEBUGPRINTF(DEBUG_DISPATCH_PREFIX "  queue empty\n"));
    assert(pts->idle_fn != NULL && "Dispatch loop idle, and no idle_fn work");
    void *idle_stack = _thc_allocstack();
    awe_t idle_awe;
    // Set start of stack-frame marker
    *((void**)(idle_stack - LAZY_STACK_BUFFER + __WORD_SIZE)) = NULL;
    thc_awe_init(&idle_awe, &thc_run_idle_fn, idle_stack-LAZY_STACK_BUFFER,
                 idle_stack-LAZY_STACK_BUFFER);
#ifndef NDEBUG
    pts->aweCreated++;
#endif
    pts->idle_stack = idle_stack;
    pts->current_fb = NULL;
#ifndef NDEBUG
    pts->aweResumed++;
#endif
    pts->curr_lazy_stack = NULL;
    DEBUG_DISPATCH(DEBUGPRINTF(DEBUG_DISPATCH_PREFIX "  executing idle function\n"));
    thc_awe_execute_0(&idle_awe);
    NOT_REACHED;
  }

  awe_t *awe = pts->aweHead.next;

  DEBUG_DISPATCH(DEBUGPRINTF(DEBUG_DISPATCH_PREFIX "  got AWE %p "
			     "(ip=%p, sp=%p, fp=%p)\n",
			     awe, awe->eip, awe->esp, awe->ebp));
  pts->aweHead.next = awe->next;
  pts->current_fb = awe->current_fb;
  pts->curr_lazy_stack = awe->lazy_stack;
  awe->next->prev = &(pts->aweHead);
#ifndef NDEBUG
  pts->aweResumed ++;
#endif
  thc_awe_execute_0(awe);
}

static void thc_init_dispatch_loop(void) {
  PTState_t *pts = PTS();
  pts->dispatchStack = _thc_allocstack();
  // Set start of stack-frame marker
  *((void**)(pts->dispatchStack - LAZY_STACK_BUFFER + __WORD_SIZE)) = NULL;
  thc_awe_init(&pts->dispatch_awe, &thc_dispatch_loop, 
               pts->dispatchStack-LAZY_STACK_BUFFER,
               pts->dispatchStack-LAZY_STACK_BUFFER);
  pts->aweHead.next = &(pts->aweTail);
  pts->aweTail.prev = &(pts->aweHead);
  pts->aweRemoteHead.next = &(pts->aweRemoteTail);
  pts->aweRemoteTail.prev = &(pts->aweRemoteHead);

  DEBUG_INIT(DEBUGPRINTF(DEBUG_INIT_PREFIX
                         "  initialized dispatch awe %p\n",
                         &pts->dispatch_awe));
  DEBUG_INIT(DEBUGPRINTF(DEBUG_INIT_PREFIX
                         "  (%p, %p, %p)\n",
                         pts->dispatch_awe.eip,
                         pts->dispatch_awe.ebp,
                         pts->dispatch_awe.esp));
}

static void thc_exit_dispatch_loop(void) {
  PTState_t *pts = PTS();
  assert(!pts->shouldExit);
  pts->shouldExit = 1;
  // Wait for idle loop to finish
  while (pts->aweHead.next != &(pts->aweTail)) {
    THCYield();
  }
  // Exit
  thc_pts_lock(pts);
  assert((pts->aweHead.next == &(pts->aweTail)) && 
         "Dispatch queue not empty at exit");
  DEBUG_INIT(DEBUGPRINTF(DEBUG_INIT_PREFIX
                         "  NULLing out dispatch AWE\n"));
  thc_awe_init(&pts->dispatch_awe, NULL, NULL, NULL);
  _thc_freestack(pts->dispatchStack);
}

// Enter the dispatch function via dispatch_awe.
//
// (Hence the dispatch loop will run on its own stack, rather than
// the caller's)

static void thc_dispatch(PTState_t *pts) {
  assert(pts && pts->doneInit && "Not initialized RTS");
  thc_awe_execute_0(&pts->dispatch_awe);
}

static void thc_start_rts(void) {
  InitPTS();
  assert(PTS() && (!PTS()->doneInit) && "Already initialized RTS");
  DEBUG_INIT(DEBUGPRINTF(DEBUG_INIT_PREFIX "> Starting\n"));
  thc_init_dispatch_loop();
  PTS()->doneInit = 1;
  DEBUG_INIT(DEBUGPRINTF(DEBUG_INIT_PREFIX "< Starting\n"));
}

static void thc_end_rts(void) {
  PTState_t *pts = PTS();
  assert(pts->doneInit && "Not initialized RTS");
  DEBUG_INIT(DEBUGPRINTF(DEBUG_INIT_PREFIX "> Ending\n"));
  thc_exit_dispatch_loop();

  // Count up the stacks that we have left.  This is merely for
  // book-keeping: once the dispatch loop is done, then the
  // number of stacks on our free list should equal the number
  // allocated from the OS.
  while (pts->free_stacks != NULL) {
    pts->free_stacks = pts->free_stacks->next;
#ifndef NDEBUG
    pts->stackMemoriesDeallocated ++;
#endif
  }

  // Done
  thc_print_pts_stats(PTS(), 0);
  PTS()->doneInit = 0;
  DEBUG_INIT(DEBUGPRINTF(DEBUG_INIT_PREFIX "< Ending\n"));
}

/***********************************************************************/

// AWE management

static void thc_awe_init(awe_t *awe, void *eip, void *ebp, void *esp) {
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "> AWEInit(%p, %p, %p, %p)\n",
                        awe, eip, ebp, esp));
  PTState_t *pts = PTS();
  awe->eip = eip;
  awe->ebp = ebp;
  awe->esp = esp;
  awe->pts = pts;
  awe->status = EAGER_AWE;
  awe->lazy_stack = NULL;
  awe->current_fb = NULL;
  awe->next = NULL;
  awe->prev = NULL;
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "< AWEInit\n"));
}

// This function is not meant to be used externally, but its only use
// here is from within the inline assembly language functions.  The
// C "used" attribute is not currently maintained through Clang & LLVM
// with the C backend, so we cannot rely on that.
extern void _thc_schedulecont_c(awe_t *awe);
void _thc_schedulecont_c(awe_t *awe) {
  PTState_t *pts = PTS();
  awe->pts = pts;
#ifndef NDEBUG
  pts->aweCreated ++;
#endif
  thc_schedule_local(awe);
}

// This function is not meant to be used externally, but its only use
// here is from within the inline assembly language functions.  The
// C "used" attribute is not currently maintained through Clang & LLVM
// with the C backend, so we cannot rely on that.
extern void _thc_callcont_c(awe_t *awe, THCContFn_t fn, void *args);
void _thc_callcont_c(awe_t *awe,
                     THCContFn_t fn,
                     void *args) {
  PTState_t *pts = PTS();
  awe->pts = pts;
  awe->current_fb = pts->current_fb;
#ifndef NDEBUG
  pts->aweCreated ++;
#endif
  fn(awe, args);
}

#ifdef CONFIG_LAZY_THC

static void init_lazy_awe (void ** lazy_awe_fp) {

  // Get the saved awe
  awe_t *awe = THC_LAZY_FRAME_AWE(lazy_awe_fp);
  
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX " found lazy awe %p @ frameptr %p",
			awe, lazy_awe_fp));

  // Scrub nested return, lazy awe will now return through dispatch loop
  THC_LAZY_FRAME_RET(lazy_awe_fp) = NULL;
  
  assert(awe->status == LAZY_AWE);
  // Allocate a new stack for this awe
  alloc_lazy_stack(awe);
  // lazily start async block
  _thc_startasync(awe->current_fb, awe->lazy_stack);
  // schedule lazy awe
  _thc_schedulecont_c(awe); 
}

// Check for all lazy awe on the stack - initalizing and scheduling any if 
// they are found.

static void check_for_lazy_awe (void * ebp) {
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "> CheckForLazyAWE (ebp=%p)\n", ebp));
  void **frame_ptr  = (void **) ebp;
  void *ret_addr    = THC_LAZY_FRAME_RET(frame_ptr);
  while (frame_ptr != NULL && ret_addr != NULL) {
    if (ret_addr == &_thc_lazy_awe_marker) {
      init_lazy_awe(frame_ptr);
    }
    frame_ptr = (void **) THC_LAZY_FRAME_PREV(frame_ptr);
    ret_addr   = THC_LAZY_FRAME_RET(frame_ptr);
  } 
 
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "< CheckForLazyAWE\n"));
}

#else  // CONFIG_EAGER_THC
static inline void check_for_lazy_awe (void * ebp) { }
#endif // CONFIG_LAZY_THC

/***********************************************************************/

// Implementation of finish blocks
//
// The implementation of finish blocks is straightforward:
// _thc_endfinishblock yields back to the dispatch loop if it finds
// the count non-zero, and stashes away a continuation in
// fb->finish_awe which will be resumed when the final async
// call finsihes.  _thc_endasync picks this up.

void _thc_startfinishblock(finish_t *fb, int fb_kind) {
  PTState_t *pts = PTS();
  finish_t *current_fb = pts->current_fb;
  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "> StartFinishBlock (%p,%s)\n",
                           fb,
                           (fb_kind == 0) ? "FINISH" : "TOP-FINISH"));
  assert(PTS() && (PTS()->doneInit) && "Not initialized RTS");
  fb -> count = 0;
  fb -> finish_awe = NULL;
  fb->cancel_item = NULL;
  fb->cancel_requested = 0;
  fb->start_node.fb = fb;
  fb->end_node.fb = fb;
  fb->enclosing_lazy_stack = PTS()->curr_lazy_stack;
  fb->enclosing_fb = current_fb;
  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  FB %p nested within %p\n",
                           fb, current_fb));
  pts->current_fb = fb;

  // Initialize cancel status
  fb->fb_kind = fb_kind;
  if (fb_kind != FB_KIND_TOP_FINISH &&
      current_fb != NULL &&
      current_fb->cancel_requested) {
    DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  Propagating cancel flag on init\n"));
    fb->cancel_requested = 1;
  }

  // Link into finish list
  //
  // Before:
  //  [current_fb.end->prev] <-> [current_fb.end]
  //
  // After:
  //  [current_fb.end->prev] <-> [fb->start] <-> [fb->end] <-> [current_fb.end]

  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  Connecting own [%p]<->[%p]\n",
                           &(fb->start_node), &(fb->end_node)));
  
  fb->start_node.next = &(fb->end_node);
  fb->end_node.prev = &(fb->start_node);
  if (current_fb != NULL) {
    DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  Splicing between [%p]<->[%p]\n",
                             (current_fb->end_node.prev), &(current_fb->end_node)));
    assert(current_fb->end_node.prev->next = &(current_fb->end_node));
    assert(current_fb->start_node.next->prev = &(current_fb->start_node));
    current_fb->end_node.prev->next = &(fb->start_node);
    fb->start_node.prev = current_fb->end_node.prev;
    fb->end_node.next = &(current_fb->end_node);
    current_fb->end_node.prev = &(fb->end_node);
  } else {
    fb->start_node.prev = NULL;
    fb->end_node.next = NULL;
  }

  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "< StartFinishBlock\n"));
#ifndef NDEBUG
  PTS()->finishBlocksStarted ++;
#endif
}

__attribute__ ((unused))
static void _thc_endfinishblock0(void *a, void *f) {
  finish_t *fb = (finish_t*)f;
  awe_t *awe = (awe_t*)a;

  awe->lazy_stack = awe->pts->curr_lazy_stack;
  
  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  Waiting f=%p awe=%p\n",
                           fb, a));
  assert(fb->finish_awe == NULL);
  fb->finish_awe = a;
  thc_dispatch(awe->pts);
  NOT_REACHED;
}

static void thc_run_cancel_actions(PTState_t *pts, finish_t *fb) {
  cancel_item_t *ci = fb->cancel_item;
  fb->cancel_item = NULL;
  if (ci == NULL) {
    DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  No cancel actions on %p\n", fb));
  } else {
    while (ci != NULL) {
      cancel_item_t *nci = ci->next;
      DEBUG_CANCEL(DEBUGPRINTF(DEBUG_CANCEL_PREFIX "  Running cancellation action %p\n",
                               ci));
#ifndef NDEBUG
      pts->cancelsRun ++;
#endif
      assert(ci->was_run == 0);
      ci->was_run = 1;
      (*ci->fn)(ci->arg);
      ci = nci;
    }
  }
}

void _thc_do_cancel_request(finish_t *fb) {
  PTState_t *pts = PTS();

  // Set own cancellation request flag
  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  Setting cancel request flag\n"));
  fb->cancel_requested = 1;
#ifndef NDEBUG
  pts->cancelsRequested++;
#endif
  
  // Handle nested cancel blocks
  finish_list_t *fl = fb->start_node.next;
  while (fl->fb != fb) {
    DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  Looking at nested FB %p kind %d\n",
                             fl->fb, (int)fl->fb->fb_kind));
    assert(fl->prev == NULL || fl->prev->next == fl);
    assert(fl->next == NULL || fl->next->prev == fl);
    if (fl->fb->fb_kind == FB_KIND_TOP_FINISH) {
      // We have found a non-nested cancel block.  This occurs when there is an
      // intervening non-cancelable function between (i) the block we are currently
      // cancelling, and (ii) the block that we have just encountered.  Skip
      // past the non-nested block.
      DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  Skipping FB_KIND_TOP_FINISH to %p\n",
                               fl->fb->end_node.next));
      fl = fl->fb->end_node.next;
    } else {
      fl->fb->cancel_requested = 1;
      if (fl == &(fl->fb->end_node)) {
        thc_run_cancel_actions(pts, fl->fb);
      }
      fl = fl->next;
    }
  }
  
  // Run our own cancellation actions
  thc_run_cancel_actions(pts, fb);
}

void _thc_endfinishblock(finish_t *fb, void *stack) {
  PTState_t *pts = PTS();
  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "> EndFinishBlock(%p)\n",
                           fb));
  assert((pts->doneInit) && "Not initialized RTS");
  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  count=%d\n",
                           (int)fb->count));

  if (fb->count == 0) {
    // Zero first time.  Check there's not an AWE waiting.
    assert(fb->finish_awe == NULL);
  } else {
    // Non-zero first time, add ourselves as the waiting AWE.
    CALL_CONT_LAZY((unsigned char*)&_thc_endfinishblock0, fb);
  }

  assert(fb->count == 0);
  assert(fb->cancel_item == NULL);
  assert(fb->start_node.next == &(fb->end_node));
  assert(fb->end_node.prev == &(fb->start_node));
  if (fb->start_node.prev == NULL) {
    // No enclosing finish block
    DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  No enclosing FB\n"));
    assert(fb->end_node.next == NULL);
  } else {
    // Remove from enclosing finish block's list
    DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  Removing from between [%p]<->[%p]\n",
                             fb->start_node.prev, fb->end_node.next));
    fb->start_node.prev->next = fb->end_node.next;
    fb->end_node.next->prev = fb->start_node.prev;
  }

  if (pts->curr_lazy_stack && 
      (fb->enclosing_fb == NULL || stack != fb->enclosing_fb->old_sp) &&
      pts->curr_lazy_stack != fb->enclosing_lazy_stack) {
      check_lazy_stack_finished(pts, stack);
  }

  PTS()->curr_lazy_stack = fb->enclosing_lazy_stack;
  pts->current_fb = fb->enclosing_fb;

  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "< EndFinishBlock\n"));
#ifndef NDEBUG
  pts->finishBlocksEnded ++;
#endif
}

void _thc_startasync(void *f, void *stack) {
  finish_t *fb = (finish_t*)f;
  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "> StartAsync(%p,%p)\n",
                           fb, stack));
  fb->count ++;
  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "< StartAsync count now %d\n",
                           (int)fb->count));
#ifndef NDEBUG
  PTS()->asyncCallsStarted ++;
#endif
}

void _thc_endasync(void *f, void *s) {
  finish_t *fb = (finish_t*)f;
  PTState_t *pts = PTS();
#ifndef NDEBUG
  pts->asyncCallsEnded ++;
#endif
  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "> EndAsync(%p,%p)\n",
                           fb, s));
  assert(fb->count > 0);
  fb->count --;
  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  count now %d\n",
                           (int)fb->count));
  assert(pts->pendingFree == NULL);

#ifdef CONFIG_LAZY_THC
  assert(__builtin_return_address(1) == NULL); /* Should have been nulled */
  /* Check whether we are running on a lazy stack, and can dispose of it */
  if (pts->curr_lazy_stack && s != fb->old_sp) {
      check_lazy_stack_finished(pts, s);
  }
#else // Eager AWE
  pts->pendingFree = s;
#endif // CONFIG_LAZY_THC

  if (fb->count == 0) {
    if (fb -> finish_awe) {
      DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "  waiting AWE %p\n",
                               fb->finish_awe));
      thc_schedule_local(fb -> finish_awe);
      fb -> finish_awe = NULL;
    }
  }

  DEBUG_FINISH(DEBUGPRINTF(DEBUG_FINISH_PREFIX "< EndAsync\n"));
  thc_dispatch(pts);
  NOT_REACHED;
}

/***********************************************************************/

// Operations for use by application code

void THCDumpStats(int clear_stats) {
  thc_print_pts_stats(PTS(), clear_stats);
}

void THCIncSendCount(void) {
#ifndef NDEBUG
  if (PTS() != NULL) {
    PTS()->sendCount++;
  }
#endif
}

void THCIncRecvCount(void) {
#ifndef NDEBUG
  if (PTS() != NULL) {
    PTS()->recvCount++;
  }
#endif
}

__attribute__ ((unused))
static void thc_yield_with_cont(void *a, void *arg) {
  DEBUG_YIELD(DEBUGPRINTF(DEBUG_YIELD_PREFIX "! %p (%p,%p,%p) yield\n",
                          a,
                          ((awe_t*)a)->eip,
                          ((awe_t*)a)->ebp,
                          ((awe_t*)a)->esp));
  awe_t *awe = (awe_t*)a; 
  awe->lazy_stack = awe->pts->curr_lazy_stack;
  // check if we have yielded within a lazy awe
  check_for_lazy_awe(awe->ebp);
  THCScheduleBack(awe);
  thc_dispatch(awe->pts);
}

void THCYield(void) {
  CALL_CONT_LAZY((void*)&thc_yield_with_cont, NULL);
}

__attribute__ ((unused))
static void thc_yieldto_with_cont(void *a, void *arg) {
  DEBUG_YIELD(DEBUGPRINTF(DEBUG_YIELD_PREFIX "! %p (%p,%p,%p) yield\n",
                          a,
                          ((awe_t*)a)->eip,
                          ((awe_t*)a)->ebp,
                          ((awe_t*)a)->esp));
  awe_t *last_awe = (awe_t*)a; 

  last_awe->lazy_stack = last_awe->pts->curr_lazy_stack;
  // check if we have yielded within a lazy awe
  check_for_lazy_awe(last_awe->ebp);

  THCScheduleBack(last_awe);
  awe_t *awe = (awe_t *)arg;
#ifndef NDEBUG
  PTS()->aweResumed++;
#endif

  awe->pts->curr_lazy_stack = awe->lazy_stack;
  awe->pts->current_fb = awe->current_fb;

  thc_awe_execute_0(awe);
}

void THCYieldTo(awe_t *awe_ptr) {
  if (PTS() == awe_ptr->pts) {
    CALL_CONT_LAZY((void*)&thc_yieldto_with_cont, (void*)awe_ptr);
  } else {
    THCSchedule(awe_ptr);
  }
}

void THCFinish(void) {
  thc_dispatch(PTS());
}

__attribute__ ((unused))
static void thc_suspend_with_cont(void *a, void *arg) {
  DEBUG_YIELD(DEBUGPRINTF(DEBUG_YIELD_PREFIX "! %p (%p,%p,%p) wait\n",
                          a,
                          ((awe_t*)a)->eip,
                          ((awe_t*)a)->ebp,
                          ((awe_t*)a)->esp));
  *(void**)arg = a;  awe_t *awe = (awe_t*)a; 
  awe->lazy_stack = awe->pts->curr_lazy_stack;
  // check if we have yielded within a lazy awe
  check_for_lazy_awe(awe->ebp);
  thc_dispatch(awe->pts);
}

void THCSuspend(awe_t **awe_ptr_ptr) {
  CALL_CONT_LAZY(&thc_suspend_with_cont, awe_ptr_ptr);
}

typedef struct {
  awe_t       **awe_addr;
  THCThenFn_t   then_fn;
  void         *then_arg;
} then_args_t;

__attribute__ ((unused))
static void thc_suspendthen_with_cont(void *a, void *arg) {
  then_args_t *ta = (then_args_t*)arg;

  DEBUG_YIELD(DEBUGPRINTF(DEBUG_YIELD_PREFIX "! %p (%p,%p,%p) waitthen\n",
                          a,
                          ((awe_t*)a)->eip,
                          ((awe_t*)a)->ebp,
                          ((awe_t*)a)->esp));
  *(void**)(ta->awe_addr) = a;
  ta->then_fn(ta->then_arg);

  awe_t *awe = (awe_t*)a; 
  awe->lazy_stack = awe->pts->curr_lazy_stack;
  // check if we have yielded within a lazy awe
  check_for_lazy_awe(awe->ebp);
  thc_dispatch(awe->pts);
}

void THCSuspendThen(awe_t **awe_ptr_ptr, THCThenFn_t fn, void *arg) {
  then_args_t t;
  t.awe_addr = awe_ptr_ptr;
  t.then_fn = fn;
  t.then_arg = arg;
  CALL_CONT_LAZY((void*)&thc_suspendthen_with_cont, &t);
}

// Add the supplied AWE to the dispatch queue
//
// By default we add to the head.  This means that in the implementation
// of "X ; async { Y } ; Z" we will run X;Y;Z in sequence (assuming that
// Y does not block).  This relies on Z being put at the head of the
// queue.

static inline void thc_schedule_local(awe_t *awe) {
  PTState_t *awe_pts;
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "> THCSchedule(%p)\n",
                        awe));
  awe_pts = awe->pts;
  awe->prev = &(awe_pts->aweHead);
  awe->next = awe_pts->aweHead.next;
  awe_pts->aweHead.next->prev = awe;
  awe_pts->aweHead.next = awe;
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "  added AWE between %p %p\n",
                        awe->prev, awe->next));
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "< THCSchedule\n"));
}

void THCSchedule(awe_t *awe) {
  PTState_t *awe_pts;
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "> THCSchedule(%p)\n",
                        awe));
  awe_pts = awe->pts;
  if (awe_pts == PTS()) {
    // Work is for us
    awe->prev = &(awe_pts->aweHead);
    awe->next = awe_pts->aweHead.next;
    awe_pts->aweHead.next->prev = awe;
    awe_pts->aweHead.next = awe;
  } else {
    // Work is remote
    thc_pts_lock(awe_pts);
    awe->prev = &(awe_pts->aweRemoteHead);
    awe->next = awe_pts->aweRemoteHead.next;
    awe_pts->aweRemoteHead.next->prev = awe;
    awe_pts->aweRemoteHead.next = awe;
    thc_pts_unlock(awe_pts);
  }
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "  added AWE between %p %p\n",
                        awe->prev, awe->next));
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "< THCSchedule\n"));
}

// Add the supplied AWE to the tail of the dispatch queue (for THCYield)

void THCScheduleBack(awe_t *awe) {
  PTState_t *awe_pts = awe->pts;
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "> THCSchedule(%p)\n",
                        awe));
  assert(awe_pts == PTS());
  awe_pts = awe->pts;
  awe->prev = awe_pts->aweTail.prev;
  awe->next = &(awe_pts->aweTail);
  awe_pts->aweTail.prev->next = awe;
  awe_pts->aweTail.prev = awe;
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "  added AWE between %p %p\n",
                        awe->prev, awe->next));
  DEBUG_AWE(DEBUGPRINTF(DEBUG_AWE_PREFIX "< THCSchedule\n"));
}

void THCAddCancelItem(cancel_item_t *ci, THCCancelFn_t fn, void *arg) {
  PTState_t *pts = PTS();
  DEBUG_CANCEL(DEBUGPRINTF(DEBUG_CANCEL_PREFIX "> THCAddCancelItem(%p)\n", ci));
  ci->fn = fn;
  ci->arg = arg;
  ci->was_run = 0;
  finish_t *fb = pts->current_fb;
  assert(fb != NULL && "Current fb NULL");
  DEBUG_CANCEL(DEBUGPRINTF(DEBUG_CANCEL_PREFIX "  FB %p\n", fb));
  ci->next = fb->cancel_item;
  fb->cancel_item = ci;
#ifndef NDEBUG
  PTS()->cancelsAdded ++;
#endif
  DEBUG_CANCEL(DEBUGPRINTF(DEBUG_CANCEL_PREFIX "< THCAddCancelItem(%p)\n", ci));
}

void THCRemoveCancelItem(cancel_item_t *ci) {
  PTState_t *pts = PTS();
  finish_t *fb = pts->current_fb;
  DEBUG_CANCEL(DEBUGPRINTF(DEBUG_CANCEL_PREFIX "> THCRemoveCancelItem(%p) from FB %p\n", 
                           ci, fb));
  assert(fb != NULL && "Current fb NULL");
  assert(!ci->was_run);
  cancel_item_t **cip = &(fb->cancel_item);
  while (*cip != NULL && *cip != ci) {
    cip = &((*cip)->next);
  }
  assert(*cip != NULL && "Cancel-item not found during remove");
  *cip = ci->next;
#ifndef NDEBUG
  PTS()->cancelsRemoved ++;
#endif
  DEBUG_CANCEL(DEBUGPRINTF(DEBUG_CANCEL_PREFIX "< THCRemoveCancelItem(%p)\n", ci));
}

int THCCancelItemRan(cancel_item_t *ci) {
  DEBUG_CANCEL(DEBUGPRINTF(DEBUG_CANCEL_PREFIX "> THCCancelItemRan(%p) = %d\n",
                           ci, ci->was_run));
  return ci->was_run;
}

int THCIsCancelRequested(void) {
  PTState_t *pts = PTS();
  finish_t *fb = pts->current_fb;
  DEBUG_CANCEL(DEBUGPRINTF(DEBUG_CANCEL_PREFIX "> THCIsCancelRequested()\n"));
  DEBUG_CANCEL(DEBUGPRINTF(DEBUG_CANCEL_PREFIX "  FB %p\n", fb));
  int result = fb->cancel_requested;
  DEBUG_CANCEL(DEBUGPRINTF(DEBUG_CANCEL_PREFIX "< THCIsCancelRequested()=%d\n", result));
  return result;
}

#if 0
int THCRun(THCFn_t fn,
           void *args,
           THCIdleFn_t idle_fn,
           void *idle_args) {
  thc_start_rts();
  PTS()->idle_fn = idle_fn;
  PTS()->idle_args = idle_args;
  PTS()->idle_stack = NULL;
  int r = fn(args);
  thc_end_rts();
  return r;
}
#endif

/**********************************************************************/

// Start-of-day code for Barrelfish, where we initialize THC before
// entry to main.

static int idle_ct = 0;

static void IdleFn(void *arg) {
  int me = ++idle_ct;
  struct waitset *ws = get_default_waitset();
  PTState_t *pts = PTS();

  while (!pts->shouldExit) {
    // Block for the next event to occur
    errval_t err = event_dispatch(ws);
    if (err_is_fail(err)) {
      assert(0 && "event_dispatch failed in THC idle function");
      abort();
    }

    // Exit if a new idle loop has started (this will happen
    // if the handler called from event_dispatch blocks, e.g.,
    // in the bottom-half of a THC receive function)
    if (me != idle_ct) {
      break;
    } 

    // Yield while some real work is now available
    while (pts->aweHead.next != &pts->aweTail &&
           !pts->shouldExit) {
      THCYield();
    }
  }
}

__attribute__((constructor))
static void thc_init(void) {
  thc_start_rts();
  PTS()->idle_fn = IdleFn;
  PTS()->idle_args = NULL;
  PTS()->idle_stack = NULL;
}

__attribute__((destructor))
static void thc_done(void) {
  thc_end_rts();
}

//struct run_args {
//  int argc;
//  char **argv;
//};
//
//static int thcmain_wrapper(void *st) {
//  struct run_args *ra = (struct run_args *) st;
//  return thcmain(ra->argc, ra->argv);
//}
//
//int main(int argc, char *argv[])
//{
//  struct run_args ra;
//  ra.argc = argc;
//  ra.argv = argv;
//  return THCRun(thcmain_wrapper, &ra, IdleFn, NULL);
//}


/**********************************************************************/

// Arch-OS specific code

// 1. Stack allocation

#if defined(WINDOWS) || defined(__CYGWIN__)
#include <Windows.h>
static void error_exit(LPTSTR lpszFunction)
{
    // Retrieve the system error message for the last-error code
    LPVOID lpMsgBuf;
    LPVOID lpDisplayBuf;
    DWORD dw = GetLastError();

    FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER |
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        dw,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPTSTR) &lpMsgBuf,
        0, NULL );

    // Display the error message and exit the process
    printf("%s failed with error %ld:%s", lpszFunction, dw, lpMsgBuf);
    ExitProcess(dw);
}

static void *thc_alloc_new_stack_0(void) {
  void *res = VirtualAlloc(NULL,
                           STACK_COMMIT_BYTES + STACK_GUARD_BYTES,
                           MEM_RESERVE,
                           PAGE_NOACCESS);
  if (!res) {
    error_exit(TEXT("VirtualAlloc(MEM_RESERVE)"));
  }
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "  Reserved %p..%p\n",
                          res, res+STACK_COMMIT_BYTES+STACK_GUARD_BYTES));
  void *com = VirtualAlloc(res + STACK_GUARD_BYTES,
                           STACK_COMMIT_BYTES,
                           MEM_COMMIT,
                           PAGE_READWRITE);
  if (!com) {
    error_exit(TEXT("VirtualAlloc(MEM_COMMIT)"));
  }
  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "  Committed %p..%p\n",
                          com, com+STACK_COMMIT_BYTES));
  void *result = com + STACK_COMMIT_BYTES;
  return result;
}
#elif defined(linux)
#include <sys/mman.h>
#include <errno.h>

static void *thc_alloc_new_stack_0(void) {
  void *res = mmap(NULL,
                   STACK_COMMIT_BYTES + STACK_GUARD_BYTES,
                   PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS,
                   0, 0);
  if (!res) {
    fprintf(stderr, "URK: mmap returned errno=%d\n", errno);
    exit(-1);
  }

  DEBUG_STACK(DEBUGPRINTF(DEBUG_STACK_PREFIX "  mmap %p..%p\n",
                          res, res+STACK_COMMIT_BYTES+STACK_GUARD_BYTES));

  int r = mprotect(res, STACK_GUARD_BYTES, PROT_NONE);
  if (r) {
    fprintf(stderr, "URK: mprotect returned errno=%d\n", errno);
    exit(-1);
  }

  res += STACK_GUARD_BYTES + STACK_COMMIT_BYTES;
  return res;
}
#elif defined(BARRELFISH)
static void *thc_alloc_new_stack_0(void) {
  char *res = malloc(STACK_COMMIT_BYTES+STACK_GUARD_BYTES);
  if (!res) {
    printf("URK: malloc failed\n");
    exit(-1);
  }

  for (int i = 0; i < STACK_GUARD_BYTES; i ++) {
    res[i] = 0xff;
  }

  //printf("Warning: stack %p..%p has no guard page\n",
  //       res + STACK_GUARD_BYTES, res + STACK_GUARD_BYTES + STACK_COMMIT_BYTES);

  return res + STACK_GUARD_BYTES + STACK_COMMIT_BYTES;
}
#else
#error No definition for _thc_alloc_new_stack_0
#endif

/***********************************************************************/

// 2. Execution on an alternative stack

#if (defined(__x86_64__) && (defined(linux) || defined(BARRELFISH)))
// Callee invoked via Linux x64 conventions (args in EDI)

/*
         static void thc_on_alt_stack_0(void *stack,   // rdi
                                        void *fn,      // rsi
                                        void *args)    // rdx
*/
__asm__ ("      .text \n\t"
         "      .align  16                  \n\t"
         "thc_on_alt_stack_0:               \n\t"
         " sub $8, %rdi                     \n\t"
         " mov %rsp, (%rdi)                 \n\t" // Save old ESP on new stack
         " mov %rdi, %rsp                   \n\t" // Set up new stack pointer
         " mov %rdx, %rdi                   \n\t" // Move args into rdi
         " call *%rsi                       \n\t" // Call callee (args in rdi)
         " pop %rsp                         \n\t" // Restore old ESP
         " ret                              \n\t");

#elif (defined(__i386__) && (defined(linux) || defined(BARRELFISH)))
// Callee invoked via stdcall (args on stack, removed by callee)

/*
         static void thc_on_alt_stack_0(void *stack,   //  4
                                       void *fn,       //  8
                                       void *args) {   // 12
*/
__asm__ ("      .text \n\t"
         "      .align  16                  \n\t"
         "thc_on_alt_stack_0:               \n\t"
         " mov 4(%esp), %edx                \n\t" // New stack
         " mov 8(%esp), %eax                \n\t" // Callee
         " mov 12(%esp), %ecx               \n\t" // Args
         " subl $4, %edx                    \n\t"
         " mov %esp, (%edx)                 \n\t" // Save old ESP on new stack
         " mov %edx, %esp                   \n\t" // Set up new stack pointer
         " push %ecx                        \n\t" // Push args on new stack
         " call *%eax                       \n\t" // Call callee (it pops args)
         " pop %esp                         \n\t" // Restore old ESP
         " ret \n\t");

#elif (defined(__i386__) && (defined(WINDOWS) || defined(__CYGWIN__)))
// Callee invoked via stdcall (args on stack, removed by callee)

/*
         static void thc_on_alt_stack_0(void *stack,   //  4
                                       void *fn,       //  8
                                       void *args) {   // 12
*/
__asm__ ("      .text \n\t"
         "      .align  16                  \n\t"
         "      .globl  _thc_on_alt_stack_0 \n\t"
         "_thc_on_alt_stack_0:              \n\t"
         " mov 4(%esp), %edx                \n\t" // New stack
         " mov 8(%esp), %eax                \n\t" // Callee
         " mov 12(%esp), %ecx               \n\t" // Args
         " subl $4, %edx                    \n\t"
         " mov %esp, (%edx)                 \n\t" // Save old ESP on new stack
         " mov %edx, %esp                   \n\t" // Set up new stack pointer
         " push %ecx                        \n\t" // Push args on new stack
         " call *%eax                       \n\t" // Call callee (it pops args)
         " pop %esp                         \n\t" // Restore old ESP
         " ret \n\t");
#else
void thc_on_alt_stack_0(void *stack,   
                        void *fn,   
                        void *args) {
  assert(0 && "thc_on_alt_stack_0 not implemented for this architecture");
}
#endif

/***********************************************************************/

// 3. AWE execution
//
// These functions are particularly delicate:
//
// (a) The _thc_schedulecont and _thc_callcont functions are called
//     with a pointer to an awe_t which has been alloca'd on the
//     caller's stack frame.  Aside from the stack/frame-pointers,
//     the caller is responsible for saving any registers that may
//     be live at the point of the call (including those which are
//     conventionally callee-save).  The _thc_schedulecont and
//     _thc_callcont functions initialize the AWE with the
//     stack/frame-pointer values for when the call returns, and
//     initializing the saved EIP with the instruction immediately
//     after that call.
//
// (b) A call to _thc_schedulecont returns normally with 0.
//
// (c) When an AWE is executed, the stack/frame-pointers are restored
//     and the register used for return values (e.g., EAX) is
//     initialized to non-0.

#if (defined(__x86_64__) && (defined(linux) || defined(BARRELFISH)))
/*
            static void thc_awe_execute_0(awe_t *awe)    // rdi
*/
__asm__ ("      .text \n\t"
         "      .align  16                 \n\t"
         "thc_awe_execute_0:               \n\t"
         " mov 8(%rdi), %rbp               \n\t"
         " mov 16(%rdi), %rsp              \n\t"
         " jmp *0(%rdi)                    \n\t");

/*
           int _thc_schedulecont(awe_t *awe)   // rdi
*/

__asm__ ("      .text \n\t"
         "      .align  16           \n\t"
         "      .globl  _thc_schedulecont \n\t"
         "      .type   _thc_schedulecont, @function \n\t"
         "_thc_schedulecont:         \n\t"
         " mov  0(%rsp), %rsi        \n\t"
         " mov  %rsi,  0(%rdi)       \n\t" // EIP   (our return address)
         " mov  %rbp,  8(%rdi)       \n\t" // EBP
         " mov  %rsp, 16(%rdi)       \n\t" // ESP+8 (after return)
         " addq $8,   16(%rdi)       \n\t"
         // AWE now initialized.  Call C function for scheduling.
         // It will return normally to us.  The AWE will resume
         // directly in our caller.
         " call _thc_schedulecont_c  \n\t"  // AWE still in rdi
         " movq $0, %rax             \n\t"
         " ret                       \n\t");

/*
           void _thc_callcont(awe_t *awe,   // rdi
                   THCContFn_t fn,          // rsi
                   void *args) {            // rdx
*/

__asm__ ("      .text \n\t"
         "      .align  16           \n\t"
         "      .globl  _thc_callcont \n\t"
         "      .type   _thc_callcont, @function \n\t"
         "_thc_callcont:             \n\t"
         " mov  0(%rsp), %rax        \n\t"
         " mov  %rax,  0(%rdi)       \n\t" // EIP (our return address)
         " mov  %rbp,  8(%rdi)       \n\t" // EBP
         " mov  %rsp, 16(%rdi)       \n\t" // ESP+8 (after return)
         " addq $8,   16(%rdi)       \n\t"
         // AWE now initialized.  Call into C for the rest.
         // rdi : AWE , rsi : fn , rdx : args
         " call _thc_callcont_c      \n\t"
         " int3\n\t");

/*
            static void _thc_lazy_awe_marker()   
*/

__asm__ ("      .text \n\t"
         "      .align  16            \n\t"
         "      .globl  _thc_lazy_awe \n\t"
         "      .globl  _thc_lazy_awe_marker \n\t"
	 " _thc_lazy_awe:            \n\t" /* This is for debugging so we get */
         " nop                       \n\t" /* a sensible call stack           */
	 " _thc_lazy_awe_marker:     \n\t"
	 " int3                      \n\t" /* should never be called */
	 );

#elif (defined(__i386__) && (defined(linux) || defined(BARRELFISH)))

/*
            static void thc_awe_execute_0(awe_t *awe)    // 4
*/

__asm__ ("      .text                     \n\t"
         "      .align  16                \n\t"
         "      .globl  thc_awe_execute_0 \n\t"
         "thc_awe_execute_0:              \n\t"
         " mov 4(%esp), %eax              \n\t"
         " mov 4(%eax), %ebp              \n\t"
         " mov 8(%eax), %esp              \n\t"
         " jmp *0(%eax)                   \n\t");

/*
           int _thc_schedulecont(awe_t *awe)   // 4
*/

__asm__ ("      .text                     \n\t"
         "      .align  16           \n\t"
         "      .globl  _thc_schedulecont \n\t"
         "_thc_schedulecont:         \n\t"
         " movl 4(%esp), %eax        \n\t"
         " movl 0(%esp), %esi        \n\t"
         " movl %esi,  0(%eax)       \n\t" // EIP   (our return address)
         " movl %ebp,  4(%eax)       \n\t" // EBP
         " movl %esp,  8(%eax)       \n\t" // ESP+4 (after return)
         " addl $4,    8(%eax)       \n\t"
         // AWE now initialized.  Call C function for scheduling.
         // It will return normally to us.  The AWE will resume
         // directly in our caller.
         " pushl %eax                \n\t"
         " call _thc_schedulecont_c  \n\t"
         " popl %eax                 \n\t"
         " movl $0, %eax             \n\t"
         " ret                       \n\t");

/*
           void _thc_callcont(awe_t *awe,   // 4
                   THCContFn_t fn,          // 8
                   void *args) {            // 12
*/

__asm__ ("      .text                     \n\t"
         "      .align  16           \n\t"
         "      .globl  _thc_callcont \n\t"
         "_thc_callcont:             \n\t"
         " movl 4(%esp), %eax        \n\t"
         " movl 0(%esp), %esi        \n\t"
         " movl %esi, 0(%eax)        \n\t" // EIP (our return address)
         " movl %ebp, 4(%eax)        \n\t" // EBP
         " movl %esp, 8(%eax)        \n\t" // ESP
         " addl $4, 8(%eax)          \n\t"
         // AWE now initialized.  Call into C for the rest.
         " movl 8(%esp), %edi        \n\t" // fn
         " movl 12(%esp), %esi       \n\t" // args
         // Set up stack frame for callee:
         " pushl %esi                \n\t"
         " pushl %edi                \n\t"
         " pushl %eax                \n\t"
         " call _thc_callcont_c      \n\t"
         " int3\n\t");

/*
            static void _thc_lazy_awe_marker()   
*/

__asm__ ("      .text \n\t"
         "      .align  16            \n\t"
         "      .globl  _thc_lazy_awe \n\t"
         "      .globl  _thc_lazy_awe_marker \n\t"
	 " _thc_lazy_awe:            \n\t" /* This is for debugging so we get */
         " nop                       \n\t" /* a sensible call stack           */
	 " _thc_lazy_awe_marker:     \n\t"
	 " int3                      \n\t" /* should never be called */
	 );

#elif (defined(__i386__) && (defined(WINDOWS) || defined(__CYGWIN__)))

/*
            static void thc_awe_execute_0(awe_t *awe)    // 4
*/

__asm__ ("      .text                     \n\t"
         "      .align  16                 \n\t"
         "      .globl  _thc_awe_execute_0 \n\t"
         "_thc_awe_execute_0:              \n\t"
         " mov 4(%esp), %eax               \n\t"
         " mov 4(%eax), %ebp               \n\t"
         " mov 8(%eax), %esp               \n\t"
         " jmp *0(%eax)                    \n\t");

/*
           int _thc_schedulecont(awe_t *awe)   // 4
*/

__asm__ ("      .text                     \n\t"
         "      .align  16           \n\t"
         "      .globl  __thc_schedulecont \n\t"
         "__thc_schedulecont:        \n\t"
         " movl 4(%esp), %eax        \n\t"
         " movl 0(%esp), %esi        \n\t"
         " movl %esi,  0(%eax)       \n\t" // EIP   (our return address)
         " movl %ebp,  4(%eax)       \n\t" // EBP
         " movl %esp,  8(%eax)       \n\t" // ESP+4 (after return)
         " addl $4,    8(%eax)       \n\t"
         // AWE now initialized.  Call C function for scheduling.
         // It will return normally to us.  The AWE will resume
         // directly in our caller.
         " pushl %eax                \n\t"
         " call __thc_schedulecont_c \n\t"
         " popl %eax                 \n\t"
         " movl $0, %eax             \n\t"
         " ret                       \n\t");

/*
           void _thc_callcont(awe_t *awe,   // 4
                   THCContFn_t fn,          // 8
                   void *args) {            // 12
*/

__asm__ ("      .text                     \n\t"
         "      .align  16           \n\t"
         "      .globl  __thc_callcont \n\t"
         "__thc_callcont:            \n\t"
         " movl 4(%esp), %eax        \n\t"
         " movl 0(%esp), %esi        \n\t"
         " movl %esi, 0(%eax)        \n\t" // EIP (our return address)
         " movl %ebp, 4(%eax)        \n\t" // EBP
         " movl %esp, 8(%eax)        \n\t" // ESP
         " addl $4, 8(%eax)          \n\t"
         // AWE now initialized.  Call into C for the rest.
         " movl 8(%esp), %edi        \n\t" // fn
         " movl 12(%esp), %esi       \n\t" // args
         // Set up stack frame for callee:
         " pushl %esi                \n\t"
         " pushl %edi                \n\t"
         " pushl %eax                \n\t"
         " call __thc_callcont_c     \n\t"
         " int3\n\t");

/*
            static void _thc_lazy_awe_marker()   
*/

__asm__ ("      .text \n\t"
         "      .align  16            \n\t"
         "      .globl  _thc_lazy_awe \n\t"
         "      .globl  _thc_lazy_awe_marker \n\t"
	 " _thc_lazy_awe:            \n\t" /* This is for debugging so we get */
         " nop                       \n\t" /* a sensible call stack           */
	 " _thc_lazy_awe_marker:     \n\t"
	 " int3                      \n\t" /* should never be called */
	 );

#elif (defined(__arm__) && (defined(linux) || defined(BARRELFISH)))
// NOTES:
//  - not sure about alignment (.align)

/*
            static void thc_awe_execute_0(awe_t *awe)    // r0
*/

__asm__ (" .text              \n\t"
         " .align  2          \n\t"
         "thc_awe_execute_0:  \n\t"
         " ldr sp, [r0, #8]   \n\t" // sp = awe->esp (stack pointer)
         " ldr fp, [r0, #4]   \n\t" // fp = awe->ebp (frame pointer)
         " ldr pc, [r0, #0]   \n\t" // pc = awe->eip (jump / pc)
);

/*
           int _thc_schedulecont(awe_t *awe)   // r0
*/

__asm__ (" .text                    \n\t"
         " .align  2                \n\t"
         " .globl _thc_schedulecont \n\t"
         " .type _thc_schedulecont, %function \n\t"
         "_thc_schedulecont:  \n\t"
         // save fp, sp, lr in stack (similarly to what gcc does)
         // from ARM Architecutre Reference Manual ARMv7-A and ARMv7-R
         // PUSH (A8-248):
         // "The SP and PC can be in the list in ARM code, but not in Thumb
         //  code. However, ARM instructions that include the SP or the PC in
         //  the list are deprecated, and if the SP is in the list, the value
         //  the instruction stores for the SP is UNKNOWN."
         " mov ip, sp         \n\t"
         " push {fp, ip, lr}  \n\t"
         // set awe
         " str lr, [r0, #0]   \n\t" // awe->eip = lr (return address)
         " str fp, [r0, #4]   \n\t" // awe->ebp = fp (frame pointer)
         " str sp, [r0, #8]   \n\t" // awe->esp = sp (stack pointer)
         // Call C function void _thc_schedulecont_c(awe_t *awe)
         // awe still in r0
         " bl _thc_schedulecont_c \n\t"
         // return 0
         "mov r0, #0 \n\t"
         // restore saved state. We return by restoring lr in the pc
         " ldm sp, {fp, sp, pc} \n\t"
);

/*
           __attribute__((returns_twice)) void
           void _thc_callcont(awe_t *awe,   // r0
                   THCContFn_t fn,          // r1
                   void *args) {            // r2
*/

__asm__ (" .text                          \n\t"
         " .align  2                      \n\t"
         " .globl _thc_callcont           \n\t"
         " .type _thc_callcont, %function \n\t"
         "_thc_callcont:                  \n\t"
         // set  awe
         " str lr, [r0, #0]   \n\t" // awe->eip = lr (return address)
         " str fp, [r0, #4]   \n\t" // awe->ebp = fp (frame pointer)
         " str sp, [r0, #8]   \n\t" // awe->esp = sp (stack pointer)
         // AWE now initialized.  Call into C for the rest.
         // r0 : AWE , r1 : fn , r2 : args
         " bl _thc_callcont_c\n\t"
         // hopefully a fault (x86 does int3)
         " mov r0, #0xffffffff \n\t"
         " ldr r0, [r0] \n\t"
);

#else
void thc_awe_execute_0(awe_t *awe) {
  assert(0 && "_thc_awe_execute_0 not implemented for this architecture");
}

int _thc_schedulecont(void *cont) {
  assert(0 && "_thc_schedulecont not implemented for this architecture");
  return 0;
}

void _thc_callcont(void *awe,   
                   void *fn,         
                   void *args) {            
  assert(0 && "_thc_callcont not implemented for this architecture");
}
void  _thc_lazy_awe_marker() {            
  assert(0 && "_thc_lazy_awe_marker not implemented for this architecture");
}
#endif

/***********************************************************************/

// 4. Per-thread state

#if defined(WINDOWS) || defined(__CYGWIN__)
volatile int TlsInitLatch = 0;
volatile DWORD TlsIndex = 0;

static PTState_t *thc_get_pts_0(void) {
  if (!TlsIndex) {
    do {
      if (__sync_bool_compare_and_swap(&TlsInitLatch, 0, 1)) {
        break;
      }
    } while (1);
    if (!TlsIndex) {
      TlsIndex = TlsAlloc();
      if (TlsIndex == TLS_OUT_OF_INDEXES) {
        error_exit("TlsAlloc failed");
      }
    }
    TlsInitLatch = 0;
  }

  return (PTState_t *) (TlsGetValue(TlsIndex));
}

static void thc_set_pts_0(PTState_t *st) {
  if (!TlsIndex) {
    DWORD index = TlsAlloc();
    if (index == TLS_OUT_OF_INDEXES) {
      error_exit("TlsAlloc failed");
    }
  }

  if (!TlsSetValue(TlsIndex, st)) {
    error_exit("TlsSetValue failed");
  }
}
#elif defined(BARRELFISH)
static PTState_t *thc_get_pts_0(void) {
  return (PTState_t*)thread_get_tls();
}

static void thc_set_pts_0(PTState_t *st) {
  thread_set_tls((void*)st);
}
#elif defined(linux)
volatile int TlsInitLatch = 0;
volatile int TlsDoneInit = 0;
pthread_key_t TlsKey = 0;

static PTState_t *thc_get_pts_0(void) {
  if (!TlsDoneInit) {
    do {
      if (__sync_bool_compare_and_swap(&TlsInitLatch, 0, 1)) {
        break;
      }
    } while (1);
    if (!TlsDoneInit) {
      int r = pthread_key_create(&TlsKey, NULL);
      assert((!r) && "pthread_key_create failed");
      TlsDoneInit = 1;
    }
    TlsInitLatch = 0;
  }

  return (PTState_t *) (pthread_getspecific(TlsKey));
}

static void thc_set_pts_0(PTState_t *st) {
  assert(TlsDoneInit);
  pthread_setspecific(TlsKey, (void*)st);
}
#else
#error No definition for thc_get_pts_0
#endif


/**********************************************************************/

