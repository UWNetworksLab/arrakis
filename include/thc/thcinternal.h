/* Barrelfish THC language extensions */

#ifndef _THC_INTERNAL_H_
#define _THC_INTERNAL_H_

/***********************************************************************/

typedef struct ptstate_t PTState_t;
typedef struct stack_t stack_t;
typedef struct finish_t finish_t;

// Definition of an AWE, asynchronous work element.  This definition must
// match the assembly-language definitions at the bottom of thc.c which
// access fields in the AWE structure.

enum awe_status {
  EAGER_AWE = 0,
  LAZY_AWE,
  NEEDS_LAZY_STACK,
  ALLOCATED_LAZY_STACK
};

struct awe_t {
  // Fields representing the code to run when the AWE is executed.
  void  *eip;
  void  *ebp;
  void  *esp;

  // Can be EAGER_ASYNC, LAZY_ASYNC or NEEDS_LASY_STACK
  enum awe_status status;

  // Stack which is allocated if awe is caller yields to this AWE.
  void  *lazy_stack; 

  // Link from an AWE to the per-thread state for the thread it
  // runs in.
  PTState_t *pts;

  // Link from an AWE to the immediately-enclosing finish
  finish_t *current_fb;

  // Fields used by the runtime system to link together AWEs, e.g.,
  // on a thread's run-queue, or on a list of waiters on a
  // synchronization object.
  awe_t *prev;
  awe_t *next;
};

/***********************************************************************/

// Definition of a finish block's data structure.  
//
// Finish blocks are held on a linked list threaded through the start_node
// and end_node fields.  The blocks dynamically nested within a given
// finish block are held between these two nodes.  (This enables easy
// iteration through all these dynamically nested blocks).

typedef struct finish_list_t finish_list_t;

struct finish_list_t {
  finish_list_t  *prev;
  finish_list_t  *next;
  finish_t       *fb;
};

struct finish_t {
  void           *old_sp;    /* stack pointer when entering do {} finish */ 
  unsigned long   count;
  awe_t          *finish_awe;
  int             fb_kind;
  int             cancel_requested;
  finish_list_t   start_node;
  finish_list_t   end_node;
  finish_t       *enclosing_fb;
  void           *enclosing_lazy_stack;
  cancel_item_t  *cancel_item;
};

/***********************************************************************/

// Per-thread runtime system state

struct stack_t {
  stack_t *next;
};

struct ptstate_t {

  // Thread-local fields: .............................................

  // Head/tail sentinels of the dispatch list
  awe_t aweHead;
  awe_t aweTail;

  // Immediately-enclosing finish block for the currently running code
  finish_t *current_fb;

  // Initialization / termination flags
  int doneInit;
  int shouldExit;

  // Stack that the thread's dispatch loop will run on
  void *dispatchStack;

  // If we are running on a lazily allocated stack, this will point to its start
  void *curr_lazy_stack;

  // Function to execute whenever the dispatch loop is idle (e.g.,
  // to block the thread until an incoming message which might change
  // the state of the dispatch loop).
  THCIdleFn_t idle_fn;
  void *idle_args;
  void *idle_stack;

  // Stack to be de-allocated on the next execution of the dispatch loop
  // (an async call terminates by re-entering the dispatch loop with
  // pendingFree set to the stack it was using.  It cannot dealloacte
  // its own stack while it is in use).
  void *pendingFree;

  // AWE to enter for the dispatch loop on this thread
  awe_t dispatch_awe;

  // Free stacks for re-use
  stack_t *free_stacks;


#ifndef NDEBUG
  // Debugging statistics
  int stackMemoriesAllocated;
  int stackMemoriesDeallocated;
  int stacksAllocated;
  int stacksDeallocated;
  int finishBlocksStarted;
  int finishBlocksEnded;
  int asyncCallsStarted;
  int asyncCallsEnded;
  int aweCreated;
  int aweResumed;
  int idleStarted;
  int idleComplete;
  int cancelsRequested;
  int cancelsAdded;
  int cancelsRun;
  int cancelsRemoved;
  int getTls;
  int lock;
  int sendCount;
  int recvCount;
#endif

  // Shared fields: ...................................................

  // Latch protecting the dispatch list
  struct thc_latch latch;

  // Head/tail sentinels of the remote dispatch list on which other
  // threads place AWEs that they have unblocks but which belong to
  // this thread
  awe_t aweRemoteHead;
  awe_t aweRemoteTail;
};

typedef void (*THCContFn_t)(void *cont, void *args);

void *_thc_allocstack(void);
void _thc_freestack(void *s);
void _thc_onaltstack(void *s, void *fn, void *args);
void _thc_startasync(void *f, void *stack);
void _thc_endasync(void *f, void *s);
void _thc_startfinishblock(finish_t *fb, int fb_kind);
void _thc_endfinishblock(finish_t *fb, void *stack);
void _thc_do_cancel_request(finish_t *fb);
void _thc_callcont(awe_t *awe, THCContFn_t fn, void *args) __attribute__((returns_twice));
int  _thc_schedulecont(awe_t *awe) __attribute__((returns_twice));
void _thc_lazy_awe_marker(void);
void _thc_pendingfree(void);

/***********************************************************************/

// Symbols declared in the .text.nx section

extern int _start_text_nx;
extern int _end_text_nx;

/***********************************************************************/

/* Macro to force callee-saves to be spilled to the stack */

#if defined(__x86_64__) 
#define KILL_CALLEE_SAVES()						\
  __asm__ volatile ("" : : : "rbx", "r12", "r13", "r14", "r15",         \
		    "memory", "cc")
#elif defined(__i386__)
#ifdef __pic__
#define KILL_CALLEE_SAVES()					        \
  __asm__ volatile ("" : : : "edi", "esi", "esp", "memory", "cc")
#else
#define KILL_CALLEE_SAVES()						\
  __asm__ volatile ("" : : : "ebx", "edi", "esi", "esp", "memory", "cc")
#endif
#elif defined(__arm__)
// see ARM Procedure Call Standard (APCS): 5.1 Machine Registers
// NB: gcc complains about clobbering two registers:
//  . v8 (i.e., r11), is the frame pointer in ARM and cannot be clobbered
//  . v7 is the PIC register
//
#if defined(__pic__)
    #define KILL_CALLEE_SAVES()                                           \
    __asm__ volatile ("" : : : "sp",                                      \
                         "v1", "v2", "v3", "v4", "v5", "v6",              \
                         "s16", "s17", "s18", "s19", "s20", "s21", "s22", \
                         "s23", "s24", "s25", "s26", "s27", "s28", "s29", \
                         "s30", "31",                                     \
                         "memory")
#else // same as before, but including v7
    #define KILL_CALLEE_SAVES()                                           \
    __asm__ volatile ("" : : : "sp",                                      \
                         "v1", "v2", "v3", "v4", "v5", "v6", "v7",        \
                         "s16", "s17", "s18", "s19", "s20", "s21", "s22", \
                         "s23", "s24", "s25", "s26", "s27", "s28", "s29", \
                         "s30", "31",                                     \
                         "memory")

#endif
#else
#error "Need definition of KILL_CALLEE_SAVES" 
#endif

#define __WORD_SIZE (sizeof(void*))

 
/***********************************************************************/

#ifdef CONFIG_LAZY_THC

/***********************************************************************/

#if defined(__x86_64__) 
/* Force args on stack - there must be a better way of doing this, but */
/* regparam(0) doesn't work on x86_64                                  */
#define FORCE_ARGS_STACK      void*__a, void*__b, void*__c, void*__d, \
                              void*__e, void*__f,
#define FORCE_ARGS_STACK_CALL NULL, NULL, NULL, NULL, NULL, NULL,
#elif defined(__i386__)                             
#define FORCE_ARGS_STACK      
#define FORCE_ARGS_STACK_CALL 
#elif defined(__arm__)
#define FORCE_ARGS_STACK assert(0 && "THC not yet implemented on ARM")
#define FORCE_ARGS_STACK_CALL assert(0 && "THC not yet implemented on ARM")
#else
#error "Need definition of FORCE_ARGS_STACK"
#endif

#define FORCE_FRAME_POINTER_USE                                         \
    /* Do a zero byte alloca to force local variable access via ebp  */ \
    /* Note, this does not add any code (even with -O0.              */ \
    __builtin_alloca(0)                                                

#if defined(__x86_64__) 
#define GET_STACK_POINTER(STACK_PTR)					\
  __asm__ volatile ("movq %%rsp, %0       \n\t"				\
		    : "=m"(STACK_PTR) : )
#define RESTORE_OLD_STACK_POINTER(OLD_STACK_PTR)			\
  __asm__ volatile ("movq %0, %%rsp       \n\t"				\
		    : : "m"(OLD_STACK_PTR))
#elif defined(__i386__)
#define GET_STACK_POINTER(STACK_PTR)					\
  __asm__ volatile ("movl %%esp, %0       \n\t"				\
		    : "=m"(STACK_PTR) : )
#define RESTORE_OLD_STACK_POINTER(OLD_STACK_PTR)			\
  __asm__ volatile ("movl %0, %%esp       \n\t"				\
		    : : "m"(OLD_STACK_PTR))
#elif defined(__arm__)
#define GET_STACK_POINTER(_) assert(0 && "THC not yet implemented on ARM")
#define RESTORE_OLD_STACK_POINTER(_) assert(0 && "THC not yet implemented on ARM")
#else
#error "Need definition of GET_STACK_POINTER and RESTORE_OLD_STACK_POINTER"
#endif


#if defined(__x86_64__) || defined(__i386__)
// INIT_LAZY_AWE() is used in the beggining of the nested function in ASYNC_.
// The signature of the nested function is:
//   void _thc_nested_async(FORCE_ARGS_STACK awe_t *awe)
//
// So in INIT_LAZY_AWE, the stack in x86 looks like:
//  sp ->
//        .......
//  rbp-> [ saved rbp ] rbp[0]
//        [ RET ]       rbp[1]
//        [ awe ]       rbp[2] (passed as first arg)
#define THC_LAZY_FRAME_PREV(FRAME_PTR) *((FRAME_PTR)+0)
#define THC_LAZY_FRAME_RET(FRAME_PTR)  *((FRAME_PTR)+1)
#define THC_LAZY_FRAME_AWE(FRAME_PTR)  *((FRAME_PTR)+2)
#endif

#if defined(__x86_64__)
#define INIT_LAZY_AWE(AWE_PTR, LAZY_MARKER)				\
  __asm__ volatile (							\
    " movq 8(%%rbp), %%rsi       \n\t"					\
    " movq %%rsi,    0(%0)       \n\t" /* RIP   (our return address) */	\
    " movq 0(%%rbp), %%rsi       \n\t"					\
    " movq %%rsi,    8(%0)       \n\t" /* RBP                        */	\
    " movq %1,       8(%%rbp)    \n\t" /* put marker as ret address  */ \
    : : "r"((AWE_PTR)), "r"((LAZY_MARKER)) : "rsi" );
#define RETURN_CONT(JMP_ADDR)			                        \
  __asm__ volatile (							\
    " movq %rbp, %rsp            \n\t" /* free frame                 */ \
    " popq %rbp                  \n\t" /* restore rbp                */ \
    " jmp  " JMP_ADDR "          \n\t" /* jump to continuation       */ \
    );
#elif defined(__i386__)
#define INIT_LAZY_AWE(AWE_PTR, LAZY_MARKER)				\
  __asm__ volatile (							\
    " movl 4(%%ebp), %%esi       \n\t"					\
    " movl %%esi,    0(%0)       \n\t" /* EIP   (our return address) */	\
    " movl 0(%%ebp), %%esi       \n\t"					\
    " movl %%esi,    4(%0)       \n\t" /* EBP                        */	\
    " movl %1,       4(%%ebp)    \n\t" /* put marker as ret address  */ \
    : : "r"((AWE_PTR)), "r"((LAZY_MARKER)) : "esi" );
#define RETURN_CONT(JMP_ADDR)			                        \
  __asm__ volatile (							\
    " movl %ebp, %esp            \n\t" /* free frame                 */ \
    " popl %ebp                  \n\t" /* restore ebp                */ \
    " addl $4, %esp              \n\t" /* clean up stack for callee  */ \
    " jmp  " JMP_ADDR "          \n\t" /* jump to continuation       */ \
    );
#elif defined(__arm__)

// *** NOTEs for the adventurous: porting lazy THC to ARM
//
// INIT_LAZY_AWE puts a marker in place of the returned address, which is saved
// in the awe structure. check_for_lazy_awe() checks for this  marker and lazily
// initializes an awe if needed.
//
// In ARM, the caller passes the return address via lr and not the stack.
// Gcc (4.7) usually compiles functions the following way:
//   mov     ip, sp
//   push    {rXX, rYY, fp, ip, lr, pc}
//   sub     fp, ip, #4
//   ....
//   ldm     sp, {rXX, rYY, fp, sp, pc}
//
// So the return address is pushed on the stack by the callee, but I'm not sure
// how consistent is this even if we only consider gcc.
//
// check_for_lazy_awe() and init_lazy_awe() also need to change.

#define INIT_LAZY_AWE(_) assert(0 && "THC not yet implemented on ARM")
#define RETURN_CONT(_) assert(0 && "THC not yet implemented on ARM")
#define GET_LAZY_AWE(_) assert(0 && "THC not yet implemented on ARM")
#else
#error "Need definition of INIT_LAZY_AWE & GET_LAZY_AWE"
#endif

/***********************************************************************/

#define SCHEDULE_CONT(_AWE_PTR, NESTED_FUNC)			\
  ({								\
    KILL_CALLEE_SAVES();					\
    NESTED_FUNC(FORCE_ARGS_STACK_CALL _AWE_PTR);               \
  })

#define CALL_CONT(_FN,_ARG)                                     \
  do {                                                          \
    awe_t _awe;                                                 \
    _awe.status     = EAGER_AWE;				\
    _awe.lazy_stack = NULL;					\
    KILL_CALLEE_SAVES();                                        \
    _thc_callcont(&_awe, (THCContFn_t)(_FN), (_ARG));           \
  } while (0)


#define CALL_CONT_LAZY(_FN,_ARG)                                \
  do {                                                          \
    awe_t _awe;                                                 \
    _awe.status     = LAZY_AWE;					\
    _awe.lazy_stack = NULL;					\
    KILL_CALLEE_SAVES();                                        \
    _thc_callcont(&_awe, (THCContFn_t)(_FN), (_ARG));           \
  } while (0)

/***********************************************************************/

#else /* EAGER_THC */

/***********************************************************************/

// not required in the  lazy CALL_CONT in the eager version
#define FORCE_FRAME_POINTER_USE      /* Not used */ do {} while(0)
#define GET_STACK_POINTER(_)         /* Not used */
#define RESTORE_OLD_STACK_POINTER(_) /* Not used */


// SWIZZLE_DEF:
//  - _NAME: name of the function
//  - _NS:   new stack, address just above top of commited region
//  - _FN:   (nested) function to call:  void _FN(void)

#if (defined(__x86_64__) && (defined(linux) || defined(BARRELFISH)))
#define SWIZZLE_DEF_(_NAME,_NS,_FN)                                     \
  __attribute__((noinline)) void _NAME(void) {                          \
    __asm__ volatile("movq %0, %%rdi      \n\t" /* put NS to %rdi   */  \
                     "subq $8, %%rdi      \n\t" /* fix NS address   */  \
                     "movq %%rsp, (%%rdi) \n\t" /* store sp to NS   */  \
                     "movq %%rdi, %%rsp   \n\t" /* set sp to NS     */  \
                     "call " _FN "        \n\t" /* call _FN         */  \
                     "popq %%rsp          \n\t" /* restore old sp   */  \
                     :                                                  \
                     : "m" (_NS)                                        \
                     : "memory", "cc", "rsi", "rdi");                   \
  }
#define SWIZZLE_DEF(_NAME,_NS,_FN) SWIZZLE_DEF_(_NAME,_NS,_FN)
#elif (defined(__i386__) && (defined(linux) || defined(BARRELFISH)))
#define SWIZZLE_DEF(_NAME,_NS,_FN)                                      \
  __attribute__((noinline)) void _NAME(void) {                          \
    __asm__ volatile("movl %0, %%edx           \n\t"			\
                     "subl $4, %%edx           \n\t"			\
                     "movl %%esp, (%%edx)      \n\t"			\
                     "movl %%edx, %%esp        \n\t"			\
                     "call " _FN "             \n\t"			\
                     "pop %%esp                \n\t"			\
                     :							\
                     : "m" (_NS)                                        \
                     : "memory", "cc", "eax", "edx");			\
  }
#elif defined(__arm__) && (defined(linux) || defined(BARRELFISH))

// Notes:
// - ARM Architecutre Reference Manual ARMv7-A and ARMv7-R:
//   STMDB:
//   "The SP and PC can be in the list in ARM code, but not in Thumb code.
//   However, ARM instructions that include the SP or the PC in the list are
//   deprecated."
// - This can probably be optimized
//
#define SWIZZLE_DEF(_NAME, _NS, _FN)                                          \
    __attribute__((noinline)) void _NAME(void) {                              \
    __asm__ volatile("ldr r0, %0      \n\t" /* set r0 to new stack */         \
                     "mov r1, sp      \n\t" /* set r1 to old stack */         \
                     "stmdb r0!, {r1} \n\t" /* save old stack to new stack */ \
                     "mov sp, r0      \n\t" /* set sp to new stack */         \
                     "bl " _FN "      \n\t" /* call _FN */                    \
                     "ldmia sp, {r1}  \n\t" /* old stack pointer to r1 */     \
                     "mov sp, r1      \n\t" /* restore stack pointer */       \
                     :                                                        \
                     : "m" (_NS)                                              \
                     : "memory", "r0", "r1");                                 \
    }
#else
#error "No definition of SWIZZLE_DEF for THC"
#endif

/***********************************************************************/

#define SCHEDULE_CONT(_AWE_PTR)                 \
  ({                                            \
    KILL_CALLEE_SAVES();                        \
    _thc_schedulecont((awe_t*)_AWE_PTR);        \
  })

#define CALL_CONT(_FN,_ARG)                                     \
  do {                                                          \
    awe_t _awe;                                                 \
    KILL_CALLEE_SAVES();                                        \
    _thc_callcont(&_awe, (THCContFn_t)(_FN), (_ARG));           \
  } while (0)

// no lazy CALL_CONT in the eager version
#define CALL_CONT_LAZY CALL_CONT

#endif // LAZY / EAGER THC

#endif // _THC_INTERNAL_H_
