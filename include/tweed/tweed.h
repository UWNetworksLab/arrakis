/** \file 
 *  \brief A simple work stealing library based upon both cilk and wool. 
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TWEED_H_
#define TWEED_H_

/* remove this to use mfence approach instead of CAS approach */
#define TWEED_USE_CAS

/* remove this to use lock-based approach */
#define TWEED_LOCK_FREE


/* Select waiting mechanism here */
//#define TWEED_WAITING
#define TWEED_LEAPFROG
//#define TWEED_PARK

#include <barrelfish/barrelfish.h>
#include <arch/x86/barrelfish_kpi/asm_inlines_arch.h>
#include <barrelfish/threads.h>


/* Constants */
#define TWEED_LOG_TASK_STACK_SIZE 12
#define TWEED_TASK_STACK_SIZE (1ll << TWEED_LOG_TASK_STACK_SIZE)
#define TWEED_TASK_STACK_MASK (TWEED_TASK_STACK_SIZE - 1)

#define TWEED_TASK_NEW       0x1
#define TWEED_TASK_INLINED   0x2
#define TWEED_TASK_STOLEN    0x4
#define TWEED_TASK_COMPLETE  0x10


/* Main tweed task declaration */
#define MAIN_TASK(name, arg1_name)                                           \
int name ## _task_func (struct generic_task_desc * _tweed_top_,              \
                        void* arg1_name);                                    \
int name ## _task_func (struct generic_task_desc * _tweed_top_,              \
                        void* arg1_name)                         


/* Task declaration macro */
#define TASK(rtype, name, arg_count, ...)                                    \
    rtype name ## _task_func (struct generic_task_desc * _tweed_top_         \
                              _ARGS_FUNC_DEF_ ## arg_count ( __VA_ARGS__ ) );\
    void name ## _task_wrapper (struct generic_task_desc * _tweed_top_,      \
                                void* task_arg);                             \
struct name ## _task_desc {                                                  \
    struct generic_task_desc task;                                           \
    rtype ret;   						             \
    _ARGS_STRUCT_DEF_ ## arg_count ( __VA_ARGS__ );                          \
};								             \
void name ## _task_wrapper (struct generic_task_desc * _tweed_top_,          \
                            void* task_arg) {                                \
    struct name ## _task_desc * task = (struct name ## _task_desc *)task_arg;\
    task->ret = _INLINE_FUNC_ ## arg_count (_tweed_top_, name, task);        \
}                                                                            \
rtype name ## _task_func (struct generic_task_desc * _tweed_top_             \
                          _ARGS_FUNC_DEF_ ## arg_count ( __VA_ARGS__ ) )


/* Task declaration macro for tasks with void return */
#define TASK_VOID(name, arg_count, ...)                                      \
    void name ## _task_func (struct generic_task_desc * _tweed_top_          \
                             _ARGS_FUNC_DEF_ ## arg_count ( __VA_ARGS__ ) ); \
    void name ## _task_wrapper (struct generic_task_desc * _tweed_top_,      \
                                void* task_arg);                             \
struct name ## _task_desc {                                                  \
    struct generic_task_desc task;                                           \
    void * ret;   /* placeholder, not used */                                \
    _ARGS_STRUCT_DEF_ ## arg_count ( __VA_ARGS__ );                          \
};								             \
void name ## _task_wrapper (struct generic_task_desc * _tweed_top_,          \
                            void* task_arg) {                                \
    struct name ## _task_desc * task = (struct name ## _task_desc *)task_arg;\
    _INLINE_FUNC_ ## arg_count (_tweed_top_, name, task);                    \
}                                                                            \
void name ## _task_func (struct generic_task_desc * _tweed_top_             \
                          _ARGS_FUNC_DEF_ ## arg_count ( __VA_ARGS__ ) )


/* Function macros, to be used externally */

#define INIT_TWEED(workers, main_task, args)                                 \
    init_tweed(workers, & main_task ## _task_func, args);    


#define SPAWN(name, arg_count, ...)                                          \
    _SPAWN(TOKEN_CONCAT(_task_, __LINE__),TOKEN_CONCAT(_prev_top_, __LINE__),\
            name, arg_count, ##__VA_ARGS__)


#define _SPAWN(task_id, prev_top_id, name, arg_count, ...)                   \
    if (_tweed_top_ == NULL) {                                               \
        _tweed_top_ = set_top();                                             \
    } else {                                                                 \
        /* move top pointer past last task */                                \
        _tweed_top_ = NEXT_TASK(_tweed_top_,                                 \
                                sizeof(struct name ## _task_desc));          \
    }                                                                        \
    /* Allocate space for new task */                                        \
    if ((~TWEED_TASK_STACK_MASK & ((uint64_t)_tweed_top_)) !=                \
        (~TWEED_TASK_STACK_MASK & ((uint64_t)(((char*)_tweed_top_) +         \
            sizeof(struct name ## _task_desc))))) {                          \
        fprintf(stderr, "Tweed task stack overflowed\n");                    \
        exit(1);                                                             \
    }                                                                        \
    struct name ## _task_desc * task_id =                                    \
        (struct name ## _task_desc *) _tweed_top_;                           \
    INIT_THIEF(task_id);                                                     \
    task_id ->task.size     = sizeof(struct name ## _task_desc);             \
    task_id ->task.f.func   = & name ## _task_wrapper;                       \
    _SET_SPAWN_ARGS_ ## arg_count ( task_id, ##__VA_ARGS__);                 \
    __asm volatile ("" ::: "memory");                                        \
    task_id ->task.balarm   = TWEED_TASK_NEW;                                \


#define CALL(name, arg_cnt, ...)                                             \
    (name ## _task_func (_tweed_top_, ##__VA_ARGS__ )) 


#define SYNC(name, arg_cnt)                                                  \
    _SYNC( TOKEN_CONCAT(_task_, __LINE__), name, arg_cnt)

                                    
#define _SYNC(task_id, name, arg_cnt)                                        \
    ({                                                                       \
        int stolen = 0;                                                      \
        struct name ## _task_desc * task_id =                                \
            (struct name ## _task_desc *) _tweed_top_;                       \
        /* pop task from stack */                                            \
        if (AT_TOP_OF_STACK) {                                               \
            /* already at top of stack, set top to NULL */                   \
            _tweed_top_ = NULL;                                              \
        } else {                                                             \
            /* otherwise release the task data-structure */                  \
            _tweed_top_ = PREV_TASK(_tweed_top_,                             \
                                    sizeof(struct name ## _task_desc));      \
        }                                                                    \
        SET_FUNC_INLINED(task_id);                                           \
        if (WAS_STOLEN(task_id) &&                                           \
            sync_stolen((struct generic_task_desc *)task_id)) {              \
            handle_stolen_task((struct generic_task_desc *)task_id);         \
            stolen=1;                                                        \
        }                                                                    \
        /* Return result */                                                  \
        stolen ? task_id->ret :                                              \
                _INLINE_FUNC_ ## arg_cnt (_tweed_top_, name, task_id);       \
    })



/* Private function macros (only used internally) */

#ifdef TWEED_USE_CAS
// use CAS operations instead of mfences
#define SET_FUNC_INLINED(t)                                                  \
    int __success = cmpxchg128((uint64_t *)&(t->task.f.func),                \
                               (uint64_t)t->task.f.func, TWEED_TASK_NEW,     \
                               (uint64_t)NULL, TWEED_TASK_INLINED);
#define WAS_STOLEN(t) (!__success)
#define INIT_THIEF(t) /* nop */
#else
// use mfence operations
#define SET_FUNC_INLINED(t)                                                  \
        t->task.f.func = NULL;                                               \
        mfence();   
#define WAS_STOLEN(t) (t->task.balarm & TWEED_TASK_STOLEN) != 0
#define INIT_THIEF(t) t->task.thief = NULL
#endif

#define PREV_TASK(x,y)                                                       \
    ((struct generic_task_desc *) (((char*)(x)) - (y)))    
#define NEXT_TASK(x,y)                                                       \
    ((struct generic_task_desc *) (((char*)(x)) + (y)))    
#define AT_TOP_OF_STACK                                                      \
    ((((uint64_t )_tweed_top_) & TWEED_TASK_STACK_MASK) == 0)

#define TOKEN_CONCAT2(x, y) x ## y
#define TOKEN_CONCAT(x, y) TOKEN_CONCAT2(x, y)


/* Decleration of arguments for functions */
#define _ARGS_FUNC_DEF_0()
#define _ARGS_FUNC_DEF_1(arg1_type, arg1_name)                               \
    , arg1_type arg1_name
#define _ARGS_FUNC_DEF_2(arg1_type, arg1_name, arg2_type, arg2_name)         \
    , arg1_type arg1_name, arg2_type arg2_name
#define _ARGS_FUNC_DEF_3(arg1_type, arg1_name, arg2_type, arg2_name,         \
                         arg3_type, arg3_name)                               \
    , arg1_type arg1_name, arg2_type arg2_name, arg3_type arg3_name
#define _ARGS_FUNC_DEF_4(arg1_type, arg1_name, arg2_type, arg2_name,         \
                         arg3_type, arg3_name, arg4_type, arg4_name)         \
    , arg1_type arg1_name, arg2_type arg2_name, arg3_type arg3_name,         \
      arg4_type arg4_name
#define _ARGS_FUNC_DEF_5(arg1_type, arg1_name, arg2_type, arg2_name,         \
                         arg3_type, arg3_name, arg4_type, arg4_name,         \
                         arg5_type, arg5_name)                               \
    , arg1_type arg1_name, arg2_type arg2_name, arg3_type arg3_name,         \
      arg4_type arg4_name, arg5_type arg5_name
#define _ARGS_FUNC_DEF_6(arg1_type, arg1_name, arg2_type, arg2_name,         \
                         arg3_type, arg3_name, arg4_type, arg4_name,         \
                         arg5_type, arg5_name, arg6_type, arg6_name)         \
    , arg1_type arg1_name, arg2_type arg2_name, arg3_type arg3_name,         \
      arg4_type arg4_name, arg5_type arg5_name, arg6_type arg6_name
#define _ARGS_FUNC_DEF_7(arg1_type, arg1_name, arg2_type, arg2_name,         \
                         arg3_type, arg3_name, arg4_type, arg4_name,         \
                         arg5_type, arg5_name, arg6_type, arg6_name,         \
                         arg7_type, arg7_name)                               \
    , arg1_type arg1_name, arg2_type arg2_name, arg3_type arg3_name,         \
      arg4_type arg4_name, arg5_type arg5_name, arg6_type arg6_name,         \
      arg7_type arg7_name
#define _ARGS_FUNC_DEF_8(arg1_type, arg1_name, arg2_type, arg2_name,         \
                         arg3_type, arg3_name, arg4_type, arg4_name,         \
                         arg5_type, arg5_name, arg6_type, arg6_name,         \
                         arg7_type, arg7_name, arg8_type, arg8_name)         \
    , arg1_type arg1_name, arg2_type arg2_name, arg3_type arg3_name,         \
      arg4_type arg4_name, arg5_type arg5_name, arg6_type arg6_name,         \
      arg7_type arg7_name, arg8_type arg8_name
#define _ARGS_FUNC_DEF_9(arg1_type, arg1_name, arg2_type, arg2_name,         \
                         arg3_type, arg3_name, arg4_type, arg4_name,         \
                         arg5_type, arg5_name, arg6_type, arg6_name,         \
                         arg7_type, arg7_name, arg8_type, arg8_name,         \
                         arg9_type, arg9_name)                               \
    , arg1_type arg1_name, arg2_type arg2_name, arg3_type arg3_name,         \
      arg4_type arg4_name, arg5_type arg5_name, arg6_type arg6_name,         \
      arg7_type arg7_name, arg8_type arg8_name, arg9_type arg9_name
#define _ARGS_FUNC_DEF_10(arg1_type, arg1_name, arg2_type, arg2_name,        \
                          arg3_type, arg3_name, arg4_type, arg4_name,        \
                          arg5_type, arg5_name, arg6_type, arg6_name,        \
                          arg7_type, arg7_name, arg8_type, arg8_name,        \
                          arg9_type, arg9_name, arg10_type, arg10_name)      \
    , arg1_type arg1_name, arg2_type arg2_name, arg3_type arg3_name,         \
      arg4_type arg4_name, arg5_type arg5_name, arg6_type arg6_name,         \
      arg7_type arg7_name, arg8_type arg8_name, arg9_type arg9_name,         \
      arg10_type arg10_name


/* Decleration of arguments fields in the task descriptor */
#define _ARGS_STRUCT_DEF_0()
#define _ARGS_STRUCT_DEF_1(arg1_type, arg1_name)                             \
    arg1_type arg1;
#define _ARGS_STRUCT_DEF_2(arg1_type, arg1_name, arg2_type, arg2_name)       \
    arg1_type arg1;                                                          \
    arg2_type arg2;
#define _ARGS_STRUCT_DEF_3(arg1_type, arg1_name, arg2_type, arg2_name,       \
                           arg3_type, arg3_name)                             \
    arg1_type arg1;                                                          \
    arg2_type arg2;                                                          \
    arg3_type arg3;
#define _ARGS_STRUCT_DEF_4(arg1_type, arg1_name, arg2_type, arg2_name,       \
                           arg3_type, arg3_name, arg4_type, arg4_name)       \
    arg1_type arg1;                                                          \
    arg2_type arg2;                                                          \
    arg3_type arg3;                                                          \
    arg4_type arg4;
#define _ARGS_STRUCT_DEF_5(arg1_type, arg1_name, arg2_type, arg2_name,       \
                           arg3_type, arg3_name, arg4_type, arg4_name,       \
                           arg5_type, arg5_name)                             \
    arg1_type arg1;                                                          \
    arg2_type arg2;                                                          \
    arg3_type arg3;                                                          \
    arg4_type arg4;                                                          \
    arg5_type arg5;
#define _ARGS_STRUCT_DEF_6(arg1_type, arg1_name, arg2_type, arg2_name,       \
                           arg3_type, arg3_name, arg4_type, arg4_name,       \
                           arg5_type, arg5_name, arg6_type, arg6_name)       \
    arg1_type arg1;                                                          \
    arg2_type arg2;                                                          \
    arg3_type arg3;                                                          \
    arg4_type arg4;                                                          \
    arg5_type arg5;                                                          \
    arg6_type arg6;
#define _ARGS_STRUCT_DEF_7(arg1_type, arg1_name, arg2_type, arg2_name,       \
                           arg3_type, arg3_name, arg4_type, arg4_name,       \
                           arg5_type, arg5_name, arg6_type, arg6_name,       \
                           arg7_type, arg7_name)                             \
    arg1_type arg1;                                                          \
    arg2_type arg2;                                                          \
    arg3_type arg3;                                                          \
    arg4_type arg4;                                                          \
    arg5_type arg5;                                                          \
    arg6_type arg6;                                                          \
    arg7_type arg7;
#define _ARGS_STRUCT_DEF_8(arg1_type, arg1_name, arg2_type, arg2_name,       \
                           arg3_type, arg3_name, arg4_type, arg4_name,       \
                           arg5_type, arg5_name, arg6_type, arg6_name,       \
                           arg7_type, arg7_name, arg8_type, arg8_name)       \
    arg1_type arg1;                                                          \
    arg2_type arg2;                                                          \
    arg3_type arg3;                                                          \
    arg4_type arg4;                                                          \
    arg5_type arg5;                                                          \
    arg6_type arg6;                                                          \
    arg7_type arg7;                                                          \
    arg8_type arg8;
#define _ARGS_STRUCT_DEF_9(arg1_type, arg1_name, arg2_type, arg2_name,       \
                           arg3_type, arg3_name, arg4_type, arg4_name,       \
                           arg5_type, arg5_name, arg6_type, arg6_name,       \
                           arg7_type, arg7_name, arg8_type, arg8_name,       \
                           arg9_type, arg9_name)                             \
    arg1_type arg1;                                                          \
    arg2_type arg2;                                                          \
    arg3_type arg3;                                                          \
    arg4_type arg4;                                                          \
    arg5_type arg5;                                                          \
    arg6_type arg6;                                                          \
    arg7_type arg7;                                                          \
    arg8_type arg8;                                                          \
    arg9_type arg9;
#define _ARGS_STRUCT_DEF_10(arg1_type, arg1_name, arg2_type, arg2_name,      \
                            arg3_type, arg3_name, arg4_type, arg4_name,      \
                            arg5_type, arg5_name, arg6_type, arg6_name,      \
                            arg7_type, arg7_name, arg8_type, arg8_name,      \
                            arg9_type, arg9_name, arg10_type, arg10_name)    \
    arg1_type arg1;                                                          \
    arg2_type arg2;                                                          \
    arg3_type arg3;                                                          \
    arg4_type arg4;                                                          \
    arg5_type arg5;                                                          \
    arg6_type arg6;                                                          \
    arg7_type arg7;                                                          \
    arg8_type arg8;                                                          \
    arg9_type arg9;                                                          \
    arg10_type arg10;


/* Set spawn args */
#define _SET_SPAWN_ARGS_0(task_name)                                         

#define _SET_SPAWN_ARGS_1(task_name, arg_val_1)                              \
    task_name->arg1 = arg_val_1;

#define _SET_SPAWN_ARGS_2(task_name, arg_val_1, arg_val_2)                   \
    _SET_SPAWN_ARGS_1(task_name, arg_val_1);                                 \
    task_name->arg2 = arg_val_2;

#define _SET_SPAWN_ARGS_3(task_name, arg_val_1, arg_val_2, arg_val_3)        \
    _SET_SPAWN_ARGS_2(task_name, arg_val_1, arg_val_2);                      \
    task_name->arg3 = arg_val_3;                           

#define _SET_SPAWN_ARGS_4(task_name, arg_val_1, arg_val_2, arg_val_3,        \
                          arg_val_4)                                         \
    _SET_SPAWN_ARGS_3(task_name, arg_val_1, arg_val_2, arg_val_3);           \
    task_name->arg4 = arg_val_4;                                               

#define _SET_SPAWN_ARGS_5(task_name, arg_val_1, arg_val_2, arg_val_3,        \
                          arg_val_4, arg_val_5)                              \
    _SET_SPAWN_ARGS_4(task_name, arg_val_1, arg_val_2, arg_val_3, arg_val_4);\
    task_name->arg5 = arg_val_5;

#define _SET_SPAWN_ARGS_6(task_name, arg_val_1, arg_val_2, arg_val_3,        \
                          arg_val_4, arg_val_5, arg_val_6)                   \
    _SET_SPAWN_ARGS_5(task_name, arg_val_1, arg_val_2, arg_val_3, arg_val_4, \
                      arg_val_5);                                            \
    task_name->arg6 = arg_val_6;

#define _SET_SPAWN_ARGS_7(task_name, arg_val_1, arg_val_2, arg_val_3,        \
                          arg_val_4, arg_val_5, arg_val_6, arg_val_7)        \
    _SET_SPAWN_ARGS_6(task_name, arg_val_1, arg_val_2, arg_val_3, arg_val_4, \
                      arg_val_5, arg_val_6);                                 \
    task_name->arg7 = arg_val_7;

#define _SET_SPAWN_ARGS_8(task_name, arg_val_1, arg_val_2, arg_val_3,        \
                          arg_val_4, arg_val_5, arg_val_6, arg_val_7,        \
                          arg_val_8)                                         \
    _SET_SPAWN_ARGS_7(task_name, arg_val_1, arg_val_2, arg_val_3, arg_val_4, \
                      arg_val_5, arg_val_6, arg_val_7);                      \
    task_name->arg8 = arg_val_8;

#define _SET_SPAWN_ARGS_9(task_name, arg_val_1, arg_val_2, arg_val_3,        \
                          arg_val_4, arg_val_5, arg_val_6, arg_val_7,        \
                          arg_val_8, arg_val_9)                              \
    _SET_SPAWN_ARGS_8(task_name, arg_val_1, arg_val_2, arg_val_3, arg_val_4, \
                      arg_val_5, arg_val_6, arg_val_7, arg_val_8);           \
    task_name->arg9 = arg_val_9;

#define _SET_SPAWN_ARGS_10(task_name, arg_val_1, arg_val_2, arg_val_3,       \
                           arg_val_4, arg_val_5, arg_val_6, arg_val_7,       \
                           arg_val_8, arg_val_9, arg_val_10)                 \
    _SET_SPAWN_ARGS_9(task_name, arg_val_1, arg_val_2, arg_val_3, arg_val_4, \
                      arg_val_5, arg_val_6, arg_val_7, arg_val_8, arg_val_9);\
    task_name->arg10 = arg_val_10;


/* Inline function call */
#define _INLINE_FUNC_0(_tweed_top_, name, task_name)                         \
    name ## _task_func(_tweed_top_)
#define _INLINE_FUNC_1(_tweed_top_, name, task_name)                         \
    name ## _task_func(_tweed_top_, task_name->arg1)
#define _INLINE_FUNC_2(_tweed_top_, name, task_name)                         \
    name ## _task_func(_tweed_top_, task_name->arg1, task_name->arg2)
#define _INLINE_FUNC_3(_tweed_top_, name, task_name)                         \
    name ## _task_func(_tweed_top_, task_name->arg1, task_name->arg2,        \
                       task_name->arg3)
#define _INLINE_FUNC_4(_tweed_top_, name, task_name)                         \
    name ## _task_func(_tweed_top_, task_name->arg1, task_name->arg2,        \
                        task_name->arg3, task_name->arg4)
#define _INLINE_FUNC_5(_tweed_top_, name, task_name)                         \
    name ## _task_func(_tweed_top_, task_name->arg1, task_name->arg2,        \
                       task_name->arg3, task_name->arg4, task_name->arg5)
#define _INLINE_FUNC_6(_tweed_top_, name, task_name)                         \
    name ## _task_func(_tweed_top_, task_name->arg1, task_name->arg2,        \
                       task_name->arg3, task_name->arg4, task_name->arg5,    \
                       task_name->arg6)
#define _INLINE_FUNC_7(_tweed_top_, name, task_name)                         \
    name ## _task_func(_tweed_top_, task_name->arg1, task_name->arg2,        \
                       task_name->arg3, task_name->arg4, task_name->arg5,    \
                       task_name->arg6, task_name->arg7)
#define _INLINE_FUNC_8(_tweed_top_, name, task_name)                         \
    name ## _task_func(_tweed_top_, task_name->arg1, task_name->arg2,        \
                       task_name->arg3, task_name->arg4, task_name->arg5,    \
                       task_name->arg6, task_name->arg7, task_name->arg8)
#define _INLINE_FUNC_9(_tweed_top_, name, task_name)                         \
    name ## _task_func(_tweed_top_, task_name->arg1, task_name->arg2,        \
                       task_name->arg3, task_name->arg4, task_name->arg5,    \
                       task_name->arg6, task_name->arg7, task_name->arg8,    \
                       task_name->arg9)
#define _INLINE_FUNC_10(_tweed_top_, name, task_name)                        \
    name ## _task_func(_tweed_top_, task_name->arg1, task_name->arg2,        \
                       task_name->arg3, task_name->arg4, task_name->arg5,    \
                       task_name->arg6, task_name->arg7, task_name->arg8,    \
                       task_name->arg9, task_name->arg10)


/* Data-structures (only to be used internally by tweed macro's) */

struct generic_task_desc;

/** Task wrapper function type */
typedef void(*tweed_task_func_t)(struct generic_task_desc * _tweed_top_,
                                 void * task);

/** Common task structure (extended with arguments and and return
    types as required by a given task */
struct generic_task_desc {

    /* func/thief and balarm need to be together, and in this order for the
       DWCAS operations to work */
    union {
#ifdef TWEED_USE_CAS
        volatile struct worker_desc * thief;
#endif
        volatile tweed_task_func_t func;
    } f;
    volatile long balarm;
    int size; 
#ifndef TWEED_USE_CAS
    volatile struct worker_desc * thief;
#endif
};

/** Main task's description */
struct main_task_desc {                                       
    struct generic_task_desc task;                            
    void* arg1;                                               
};
  
/** Worker descriptor */
struct worker_desc {
    // task descriptors for this worker
    struct generic_task_desc * task_desc_stack;       
    // pointer to first task to steal
    struct generic_task_desc * bot;
#ifndef TWEED_LOCK_FREE                   
    // lock, primarily used by thief 
    spinlock_t lock;
#endif
    // worker's thread
    struct thread * worker_thr;   
    int id;  
    int core_id;
};


/* Library routines, only to be used internally by macro's */
int init_tweed(int workers_requested,
               int(*main_func)(struct generic_task_desc *,void*),
               void* main_args);

struct generic_task_desc * set_top(void);

int sync_stolen(struct generic_task_desc * _tweed_top_);

int handle_stolen_task(struct generic_task_desc * _tweed_top_);

#endif
