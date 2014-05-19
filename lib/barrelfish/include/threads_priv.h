/*
 * Copyright (c) 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_THREADS_PRIV_H
#define LIBBARRELFISH_THREADS_PRIV_H

#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/except.h>

/// Maximum number of thread-local storage keys
#define MAX_TLS         16

/** \brief TLS dynamic thread vector data structure
 *
 * See: ELF handling for thread-local storage. Ulrich Drepper, Dec 2005.
 * http://www.akkadia.org/drepper/tls.pdf
 */
struct tls_dtv {
    uintptr_t gen; ///< Generation count
    void *dtv[0];  ///< Variable-length array of pointers to TLS blocks
};

enum thread_state {
    THREAD_STATE_NULL = 0,
    THREAD_STATE_RUNNABLE,
    THREAD_STATE_BLOCKED,
    THREAD_STATE_EXITED
};

/** \brief A thread of execution / thread control block (TCB)
 *
 * NB: on some ABIs (namely x86_{32,64}), the TLS blocks for initially-loaded
 * (i.e. not dlopen()ed) modules _precede_ this structure in memory. Therefore
 * it's not safe to directly malloc() or free() a thread structure.
 */
struct thread {
    /* XXX: The offsets of the first two fields (self pointer and dispatcher
     * pointer) are depended upon by the ABI and/or assembly code. Don't change!
     */
    struct thread       *self;              ///< Points to itself
    dispatcher_handle_t disp;               ///< Dispatcher affinity
    struct tls_dtv      *tls_dtv;           ///< TLS thread vector
    struct thread       *next, *prev;       ///< Next/prev threads in list
    arch_registers_state_t regs;            ///< Register state snapshot
    void                *stack;             ///< Malloced stack area
    void                *stack_top;         ///< Stack bounds
    void                *exception_stack;   ///< Stack for exception handling
    void                *exception_stack_top; ///< Bounds of exception stack
    exception_handler_fn exception_handler; ///< Exception handler, or NULL
    void                *userptr;           ///< User's thread local pointer
    void                *userptrs[MAX_TLS]; ///< User's thread local pointers
    uintptr_t           yield_epoch;        ///< Yield epoch
    void                *wakeup_reason;     ///< Value returned from block()
    coreid_t            coreid;             ///< XXX: Core ID affinity
    int                 return_value;       ///< Value returned on exit
    struct thread_cond  exit_condition;     ///< Thread exit condition
    struct thread_mutex exit_lock;          ///< Protects exited state
    enum thread_state   state;              ///< Thread state
    bool                paused;             ///< Thread is paused (not runnable)
    bool                detached;           ///< true if detached
    bool                joining;            ///< true if someone is joining
    bool                in_exception;       ///< true iff running exception handler
    bool                used_fpu;           ///< Ever used FPU?
#if defined(__x86_64__)
    uint16_t            thread_seg_selector; ///< Segment selector for TCB
#endif
    arch_registers_fpu_state_t fpu_state;   ///< FPU state
    void                *slab;              ///< Base of slab block containing this TCB
    uintptr_t           id;                 ///< User-defined thread identifier
};

void thread_enqueue(struct thread *thread, struct thread **queue);
struct thread *thread_dequeue(struct thread **queue);
void thread_remove_from_queue(struct thread **queue, struct thread *thread);

/* must only be called by dispatcher, while disabled */
void thread_init_disabled(dispatcher_handle_t handle, bool init_domain);

/// Returns true iff there is non-threaded work to be done on this dispatcher
/// (ie. if we still need to run)
static inline bool havework_disabled(dispatcher_handle_t handle)
{
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    return disp->runq != NULL
#ifdef CONFIG_INTERCONNECT_DRIVER_LMP
            || disp->lmp_send_events_list != NULL
#endif
            ;
}

void *thread_block(struct thread **queue);
void *thread_block_disabled(dispatcher_handle_t handle, struct thread **queue);
void *thread_block_and_release_spinlock_disabled(dispatcher_handle_t handle,
                                                 struct thread **queue,
                                                 spinlock_t *spinlock);
struct thread *thread_unblock_one(struct thread **queue, void *reason);
struct thread *thread_unblock_one_disabled(dispatcher_handle_t handle,
                                           struct thread **queue, void *reason);
struct thread *thread_unblock_all_disabled(dispatcher_handle_t handle,
                                           struct thread **queue, void *reason);

struct thread *thread_create_unrunnable(thread_func_t start_func, void *arg,
                                        size_t stacksize);

void thread_init_remote(dispatcher_handle_t handle, struct thread *thread);
void threads_prepare_to_span(dispatcher_handle_t newdh);

void thread_run_disabled(dispatcher_handle_t handle);
void thread_deliver_exception_disabled(dispatcher_handle_t handle,
                                       enum exception_type type, int subtype,
                                       void *addr, arch_registers_state_t *regs);

#endif // LIBBARRELFISH_THREADS_PRIV_H
