/* Barrelfish THC language extensions */

#ifndef _THC_H_
#define _THC_H_

#include <stdint.h>
#include <stdlib.h>

#ifndef BARRELFISH
typedef int errval_t;
#define SYS_ERR_OK   0
#define THC_CANCELED 1
#endif

// The implementation of do..finish relies on shadowing so that 
// _fb_info always refers to the closest enclosing do..finish block.
#pragma GCC diagnostic ignored "-Wshadow"

/*......................................................................*/

// Macros for use from user code
//
// Cancel.  _TAG is the name of an enclosig DO_FINISH block.

#define CANCEL(_TAG)                                                    \
  do {                                                                  \
    _thc_do_cancel_request(_fb_info_ ## _TAG);                          \
  }  while (0)

// DO_FINISH block. Non-cancellable:

#define DO_FINISH_NX(_CODE) DO_FINISH__(__,_CODE,1)

// Cancellable do-finish, with tag:

#define DO_FINISH_(_TAG,_CODE) DO_FINISH__(_TAG,_CODE,0)

// Cancellable do-finish, no tag:

#define DO_FINISH(_CODE) DO_FINISH__(__,_CODE,0)

// ASYNC implementation.  __COUNTER__ is a GCC extension that will 
// allocate a unique integer ID on each expansion.  Hence use it once 
// here and use the resulting expanded value in ASYNC_:

#define ASYNC(_BODY) ASYNC_(_BODY, __COUNTER__)

#ifdef CONFIG_LAZY_THC

// DO_FINISH implementation:

#define DO_FINISH__(_TAG,_CODE,_IS_NX)					\
  do {                                                                  \
    finish_t _fb;                                                       \
    finish_t *_fb_info __attribute__((unused)) = &_fb;                  \
    finish_t *_fb_info_ ## _TAG __attribute__((unused)) = _fb_info;     \
    void *_fb_curr_stack __attribute__((unused));			\
    FORCE_FRAME_POINTER_USE;						\
    GET_STACK_POINTER(_fb_info->old_sp);				\
    _thc_startfinishblock(_fb_info, _IS_NX);				\
    do { _CODE } while (0);                                             \
    GET_STACK_POINTER(_fb_curr_stack);					\
    _thc_endfinishblock(_fb_info, _fb_curr_stack);			\
    if (_fb_info->old_sp != _fb_curr_stack) {				\
      RESTORE_OLD_STACK_POINTER(_fb_info->old_sp);			\
      _thc_pendingfree();						\
    }									\
} while (0)

// The basic idea for ASYNC_ is that the contents of the block becomes
// a nested function.  For the lazy implementation, we create an AWE for
// the continuation of the block and store a reference to it on the nested
// function's stackframe.  We then mark this as an async by replacing the
// return address of the nested function with a special marker.  If we block,
// the runtime system looks through the stack for this marker, allocating 
// a lazy stack for any AWE continuations it finds.
//
// We are careful to avoid taking the address of the nested function.
// This prevents GCC trying to generate stack-allocated trampoline functions
// (this is not implemented on Beehive where the I and D caches are not
// coherent).
//
// There are several hacks:
//
// 1. The nested function is given an explicit name in the generated 
//    assembly language code (NESTED_FN_STRING(_C)).  This means that 
//    inline asm can refer to it without needing to take the address
//    of the nested function.
//
// 2. The nested function specifically jumps to the point after the async
//    continuation rather than returning normally, since (i) we have fiddled
//    with the return address to make it a marker and (ii), changing it back 
//    then returning normally confuses the branch prediction hardware leading
//    to an increase in async cost by about 40 cycles (25 cycles -> 65 cycles) 
//
#define ASYNC_(_BODY, _C)						\
  do {									\
    awe_t _awe;                                                         \
    extern void * CONT_RET_FN_NAME(_C) (void);	         		\
									\
    _awe.status     = LAZY_AWE;						\
    _awe.lazy_stack = NULL;						\
    _awe.pts        = NULL;						\
									\
    /* Define nested function containing the body */			\
    auto void _thc_nested_async(FORCE_ARGS_STACK awe_t *awe) __asm__(NESTED_FN_STRING(_C)); \
    __attribute__((noinline,used)) void _thc_nested_async(FORCE_ARGS_STACK awe_t *awe) {  \
      void *_my_fb = _fb_info;						\
      _awe.current_fb = _my_fb;						\
      INIT_LAZY_AWE(awe, &_thc_lazy_awe_marker);			\
      do { _BODY; } while (0);						\
      /* If return address is NULLed then we blocked */			\
      if (__builtin_return_address(0) == NULL) {			\
	/* thc_startasync is performed lazily, we should run */		\
	/* _thc_endasync if we blocked*/				\
	_thc_endasync(_my_fb, __builtin_frame_address(0)+(2*__WORD_SIZE));\
      }									\
      /* Otherwise, return */						\
      RETURN_CONT(CONT_RET_FN_STRING(_C));				\
      debug_printf("ERROR: "NESTED_FN_STRING(_C)" should never returned!"); \
    }									\
    SCHEDULE_CONT(&_awe, _thc_nested_async);                            \
    __asm__ volatile (							\
      "      .globl  " CONT_RET_FN_STRING(_C) "\n\t"			\
      " " CONT_RET_FN_STRING(_C) ":            \n\t"			\
    );                                                                  \
    									\
  } while (0)

#else // EAGER_THC

// DO_FINISH implementation:

#define DO_FINISH__(_TAG,_CODE,_IS_NX)					\
  do {                                                                  \
    finish_t _fb;                                                       \
    finish_t *_fb_info __attribute__((unused)) = &_fb;                  \
    finish_t *_fb_info_ ## _TAG __attribute__((unused)) = _fb_info;     \
    _thc_startfinishblock(_fb_info, _IS_NX);				\
    do { _CODE } while (0);                                             \
    _thc_endfinishblock(_fb_info, NULL);				\
} while (0)

// The basic idea for ASYNC_ is that the contents of the block becomes
// a nested function.  We create an AWE for the continuation of the
// block (passing it to the runtime system via SCHEDULE_CONT).
// We then execute the block on a new stack.  This is a bit cumbersome:
//
// - Allocate the stack memory _thc_allocstack
//
// - Define a "swizzle" function that will transfer execution onto the
//   new stack, capturing the stack and target function address from
//   its environment
// 
// We are careful to avoid taking the address of the nested function.
// This prevents GCC trying to generate stack-allocated trampoline functions
// (this is not implemented on Beehive where the I and D caches are not
// coherent).
//
// There are several hacks:
//
// 1. The nested function is given an explicit name in the generated 
//    assembly language code (NESTED_FN_STRING(_C)).  This means that 
//    inline asm can refer to it without needing to take the address
//    of the nested function.
//
// 2. The swizzle function passes control to the nested function assuming
//    that the calling conventions are the same for the two functions.
//    In particular, the swizzle function is called with the same
//    static chain as the underlying nested function.
#define ASYNC_(_BODY, _C)						\
  do {									\
    awe_t _awe;                                                         \
    void *_new_stack = _thc_allocstack();		       		\
    _awe.current_fb = _fb_info;						\
    /* Define nested function containing the body */			\
    auto void _thc_nested_async(void) __asm__(NESTED_FN_STRING(_C));    \
    __attribute__((noinline,used)) void _thc_nested_async(void) {       \
      void *_my_fb = _fb_info;						\
      void *_my_stack = _new_stack;                                     \
      _thc_startasync(_my_fb, _my_stack);                               \
      do { _BODY; } while (0);						\
      _thc_endasync(_my_fb, _my_stack);					\
      assert(0 && "_thc_endasync returned");				\
    }									\
                                                                        \
    /* Define function to enter _nested on a new stack */               \
    auto void _swizzle(void) __asm__(SWIZZLE_FN_STRING(_C));            \
    SWIZZLE_DEF(_swizzle, _new_stack, NESTED_FN_STRING(_C));            \
                                                                        \
    /* Add AWE for our continuation, then run "_nested" on new stack */	\
    if (!SCHEDULE_CONT(&_awe)) {                                        \
      _swizzle();							\
      assert(0 && "_swizzle returned");					\
    }                                                                   \
  } while (0)

#endif // CONFIG_LAZY_THC

// Helper macros used in creating the assembly language name for the
// nested function

#define THC_STR_(X) #X
#define THC_STR(X) THC_STR_(X)
#define NESTED_FN_NAME(C) _thc_nested_async_ ## C
#define NESTED_FN_STRING(C) THC_STR(NESTED_FN_NAME(C))
#define SWIZZLE_FN_NAME(C) _thc_swizzle_ ## C
#define SWIZZLE_FN_STRING(C) THC_STR(SWIZZLE_FN_NAME(C))
#define CONT_RET_FN_NAME(C) _thc_cont_return_ ## C
#define CONT_RET_FN_STRING(C) THC_STR(CONT_RET_FN_NAME(C))

/*......................................................................*/

// Prototypes for functions to be called by user code (as opposed to
// by the implementations of compiler intrinsics)

// Initialize the runtime system for the given thread:
//
//   - execute "fn(args)" within it, as the first AWE
//
//   - return the result of "fn(args)" when it completes
//
//   - call "idle_fn(idle_args)" whenver there is no work
//     (or assert-fail if idle and idle_fn is NULL)

typedef int (*THCFn_t)(void *);
typedef void (*THCIdleFn_t)(void *);
//extern int THCRun(THCFn_t fn,
//                  void *args,
//                  THCIdleFn_t idle_fn,
//                  void *idle_args);

// An AWE is an asynchronous work element.  It runs to completion,
// possibly producing additional AWEs which may be run subsequently.
typedef struct awe_t awe_t;

// Finish the current AWE, and initialize (*awe_ptr_ptr) with a pointer
// to an AWE for its continuation.  Typically, this will be stashed
// away in a data structure from which it will subsequently be fetched
// and passed to THCSchedule.
void THCSuspend(awe_t **awe_ptr_ptr);

// As THCSuspend, but execute "then_fn(arg)" before returning to the
// scheduler.
typedef void (*THCThenFn_t)(void *);
void THCSuspendThen(awe_t **awe_ptr_ptr, THCThenFn_t then, void *arg);

// Schedule execution of a given AWE at the head/tail of the queue.
void THCSchedule(awe_t *awe_ptr);
void THCScheduleBack(awe_t *awe_ptr);

// Finish the current AWE, returning to the scheduler.
void THCFinish(void);

// Finish the current AWE, creating a new AWE from its continuation, and
// passing this immediately to the scheduler.
void THCYield(void);

// Hint that the runtime system should switch from the current AWE to the
// indicated awe_ptr.  (Currently, the implementation does this immediately
// if awe_ptr runs on the same thread as the caller.  It puts the continuation
// to THCYieldTo on the run-queue.)
void THCYieldTo(awe_t *awe_ptr);

// Cancellation actions.  These are executed in LIFO order when cancellation 
// occurs.  Once cancellation has been requested, it is assumed that no
// further cancellation actions will be added.  Cancellation actions can be 
// added and removed in any order (not just LIFO) -- in practice this occurs
// when they are added/removed in different async branches.
//
// The structure of a cancel_item_t should be treated as opaque: it is 
// defined here so that its size is known, and hence so that it can be
// stack-allocated by callers to THCAdd/RemoveCancelItem.
typedef void (*THCCancelFn_t)(void *);
typedef struct cancel_item_t cancel_item_t;
struct cancel_item_t {
  THCCancelFn_t   fn;
  void           *arg;
  int             was_run;
  cancel_item_t  *prev;
  cancel_item_t  *next;
};

void THCAddCancelItem(cancel_item_t *ci, THCCancelFn_t fn, void *arg);
void THCRemoveCancelItem(cancel_item_t *ci);
int THCCancelItemRan(cancel_item_t *ci);

// Test for cancellation request
int THCIsCancelRequested(void);

// Dump debugging stats (if available, otherwise no-op)
void THCDumpStats(int clear_stats);
void THCIncSendCount(void);
void THCIncRecvCount(void);

/*......................................................................*/

#include "thcsync.h"
#include "thcinternal.h"

#endif // _THC_H_
