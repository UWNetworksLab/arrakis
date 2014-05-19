/**
 * \file
 * \brief User and kernel code definitions for system-wide tracing
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_TRACE_H
#define LIBBARRELFISH_TRACE_H

#if defined(__x86_64__)
#define TRACING_EXISTS 1
#endif


#if defined(__x86_64__) || defined(__i386__)
#ifndef IN_KERNEL
/* XXX: private includes from libbarrelfish */
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#else // IN_KERNEL
#include <arch/x86/apic.h> // XXX!
#endif // IN_KERNEL
#endif // __x86_64__ || __i386__

#include <barrelfish/sys_debug.h>
#include <barrelfish/waitset.h> // struct event_closure

#include <stdio.h>
#include <string.h> // memcpy

/*
 * turn some tracing on or off
 */
#define TRACE_THREADS
#define TRACE_CSWITCH
//#define NETWORK_LLSTACK_TRACE 1

/* Trace only network related events
 * This will reduce the amount of events recorded, and hence allows
 * recording for longer time. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ONLY_SUB_NET 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE

#if CONFIG_TRACE && NETWORK_LLSTACK_TRACE
#define TRACE_ONLY_LLNET 1
#endif // CONFIG_TRACE && NETWORK_LLSTACK_TRACE


#define CONSOLE_DUMP_BUFLEN (2<<20)

/**
 * The constants for the subsystems and events are generated from the file
 * trace_definitions/trace_defs.pleco that can be included after compiling with
 * #include <trace_definitions/trace_defs.h>
 * .
 */
#include <trace_definitions/trace_defs.h>




#define TRACE_EVENT(s,e,a) ((uint64_t)(s)<<48|(uint64_t)(e)<<32|(a))

/* XXX: this is a temp kludge. The tracing code wants to allocate a fixed buffer
 * for every possible core ID, but this is now too large for sanity, so I've
 * limited it here. -AB 20111229
 */

struct trace_buffer;

#define TRACE_COREID_LIMIT        4
#define TRACE_EVENT_SIZE          16
#define TRACE_MAX_EVENTS          30000        // max number of events
#define TRACE_MAX_APPLICATIONS    128
//#define TRACE_PERCORE_BUF_SIZE    0x1ff00
#define TRACE_PERCORE_BUF_SIZE    (TRACE_EVENT_SIZE * TRACE_MAX_EVENTS + (sizeof (struct trace_buffer)))

#define TRACE_BUF_SIZE (TRACE_COREID_LIMIT*TRACE_PERCORE_BUF_SIZE)    // Size of all trace buffers

// Size of the array storing which subsystems are enabled
#define TRACE_SUBSYS_ENABLED_BUF_SIZE (TRACE_NUM_SUBSYSTEMS * sizeof(bool))

#define TRACE_ALLOC_SIZE (TRACE_BUF_SIZE + TRACE_SUBSYS_ENABLED_BUF_SIZE)

#define TRACE_MAX_BOOT_APPLICATIONS 16


#if defined(__x86_64__)
#define TRACE_TIMESTAMP() rdtsc()

/*
 * \brief compare and set. If the value at address
 *        equals old, set it to new and return true,
 *        otherwise don't write to address and return false
 *
 * NOTE: This is only used by threads on the same core so no lock prefix
 */
static inline bool trace_cas(volatile uintptr_t *address, uintptr_t old,
                             uintptr_t nw)
{
    register bool res;
    __asm volatile("cmpxchgq %2,%0     \n\t"
                   "setz %1            \n\t"
                   : "+m" (*address), "=q" (res)
                   : "r" (nw), "a" (old)
                   : "memory");
    return res;
}


#elif defined(__i386__)


static inline bool trace_cas(volatile uintptr_t *address, uintptr_t old,
                             uintptr_t nw)
{
    return false;
}

#define TRACE_TIMESTAMP() rdtsc()


#elif defined(__arm__)


static inline bool trace_cas(volatile uintptr_t *address, uintptr_t old,
                             uintptr_t nw)
{
    return false;
}

#define TRACE_TIMESTAMP() 0


#else

#warning You need to supply CAS and a timestamp function for this architecture.

#endif


/// Trace event
struct trace_event {
    uint64_t timestamp;
    union {
        uint64_t raw;
        // ... stuff that is embedded in the event
        struct {
            uint32_t word1;
            uint32_t word2;
        } w32;
        struct {
            uint32_t arg;
            uint16_t event;
            uint16_t subsystem;
        } ev;
    } u;
};

// Trace information about an application
struct trace_application {
    char name[8]; ///< Name of the application
    uint64_t dcb; ///< DCB address of the application
};

/// Trace buffer
struct trace_buffer {
    volatile uintptr_t head_index;
    volatile uintptr_t tail_index;

    // ... flags...
    struct trace_buffer *master;       // Pointer to the trace master
    volatile bool     running;
    volatile bool     autoflush;       // Are we flushing automatically?
    volatile uint64_t start_trigger;
    volatile uint64_t stop_trigger;
    volatile uint64_t stop_time;
    int64_t           t_offset;           // Time offset relative to core 0
    uint64_t          t0;              // Start time of trace
    uint64_t          duration;        // Max trace duration
    uint64_t          event_counter;        // Max number of events in trace

    // ... events ...
    struct trace_event events[TRACE_MAX_EVENTS];

    // ... applications ...
    volatile uint8_t num_applications;
    struct trace_application applications[TRACE_MAX_APPLICATIONS];
};

typedef errval_t (* trace_conditional_termination_t)(bool forced);

static __attribute__((unused)) trace_conditional_termination_t
    cond_termination = NULL;

#ifndef IN_KERNEL

extern lvaddr_t trace_buffer_master;
extern lvaddr_t trace_buffer_va;
struct cnoderef;

errval_t trace_init(void);
errval_t trace_disable_domain(void);
void trace_reset_buffer(void);
void trace_reset_all(void);
errval_t trace_setup_on_core(struct capref *retcap);
errval_t trace_setup_child(struct cnoderef taskcn,
                           dispatcher_handle_t handle);
errval_t trace_control(uint64_t start_trigger,
                       uint64_t stop_trigger,
                       uint64_t duration);
errval_t trace_control_fixed_events_counter(uint64_t start_trigger,
                       uint64_t stop_trigger,
                       uint64_t duration,
                       uint64_t event_counter);
errval_t trace_wait(void);
size_t trace_get_event_count(coreid_t specified_core);
errval_t trace_conditional_termination(bool forced);
size_t trace_dump(char *buf, size_t buflen, int *number_of_events);
size_t trace_dump_core(char *buf, size_t buflen, size_t *usedBytes,
        int *number_of_events_dumped, coreid_t specified_core,
        bool first_dump, bool isOnlyOne);
void trace_flush(struct event_closure callback);
void trace_set_autoflush(bool enabled);
errval_t trace_prepare(struct event_closure callback);
errval_t trace_my_setup(void);

errval_t trace_set_subsys_enabled(uint16_t subsys, bool enabled);
errval_t trace_set_all_subsys_enabled(bool enabled);



/**
 * \brief Compute fixed trace buffer address according to
 * given core_id
 *
 * Each core gets its own top-level page table entry, so to use a
 * fixed address need to compute it as an offset from core id.
 *
 * Once we've computed this for a domain, we store it in
 * the dispatcher.
 *
 */
static inline lvaddr_t compute_trace_buf_addr(uint8_t core_id)
{
    assert(core_id < TRACE_COREID_LIMIT);
    lvaddr_t addr = trace_buffer_master + (core_id * TRACE_PERCORE_BUF_SIZE);

    return addr;
}


static inline void set_cond_termination(trace_conditional_termination_t f_ptr)
{
    cond_termination  = f_ptr;
}

#endif // NOT IN_KERNEL

void trace_init_disp(void);

/**
 * \brief Reserve a slot in the trace buffer and write the event.
 *
 * Returns the slot index that was written.
 * Lock-free implementation.
 *
 */
static inline uintptr_t
trace_reserve_and_fill_slot(struct trace_event *ev,
                            struct trace_buffer *buf)
{
    uintptr_t i, nw;
    struct trace_event *slot;

    do {
        i = buf->head_index;

        if (buf->tail_index - buf->head_index == 1 ||
                (buf->tail_index == 0 && (buf->head_index == TRACE_MAX_EVENTS-1))) {
            // Buffer is full, overwrite last event
            return i;
        }

        nw = (i + 1) % TRACE_MAX_EVENTS;

    } while (!trace_cas(&buf->head_index, i, nw));

    // Write the event
    slot = &buf->events[i];
    *slot = *ev;

    return i;
}

/**
 * \brief Write a trace event to the buffer for the current core.
 *
 * Tracing must have been set up by parent of the current domain
 * (by calling trace_setup_child).
 *
 * The per-core buffer must have already been initialized by
 * the monitor (by calling trace_setup_on_core).
 */

#ifndef IN_KERNEL

static inline coreid_t get_my_core_id(void)
{
    // FIXME: This call is not safe.  Figure out better way to do this
    // WARNING: Be very careful about using get_my_core_id function
    // as this function depends on working of disp pointers and they dont work
    // in thread disabled mode when you are about to return to kernel with
    // sys_yield.
    return disp_get_core_id();
}
#endif // IN_KERNEL


#ifdef IN_KERNEL

static inline coreid_t get_my_core_id(void)
{
    return my_core_id;
}

// Kernel-version: uses the global trace buffer variable
static inline errval_t trace_write_event(struct trace_event *ev)
{
#ifdef TRACING_EXISTS
    struct trace_buffer *master = (struct trace_buffer *)kernel_trace_buf;

    if (kernel_trace_buf == 0 || my_core_id >= TRACE_COREID_LIMIT) {
        return TRACE_ERR_NO_BUFFER;
    }

    if (!master->running) {
        if (ev->u.raw == master->start_trigger) {
            master->start_trigger = 0;
            master->t0 = ev->timestamp;
            if (master->duration)
                master->stop_time = ev->timestamp + master->duration;
            master->running = true;
        } else {
            return SYS_ERR_OK;
        }
    }
    struct trace_buffer *trace_buf = (struct trace_buffer*) (kernel_trace_buf
            + my_core_id * TRACE_PERCORE_BUF_SIZE);
    (void) trace_reserve_and_fill_slot(ev, trace_buf);

    if (ev->u.raw == master->stop_trigger ||
            (ev->timestamp>>63 == 0 &&  // Not a DCB event
             ev->timestamp > master->stop_time)) {
        master->stop_trigger = 0;
        master->running = false;
    }
#endif // TRACING_EXISTS
    return SYS_ERR_OK;
}

// Call this function when a new application has been created.
// dcb: pointer to the domain control block struct of the application.
static inline errval_t trace_new_application(char *new_application_name, uintptr_t dcb)
{
#ifdef TRACING_EXISTS

    if (kernel_trace_buf == 0 || my_core_id >= TRACE_COREID_LIMIT) {
        return TRACE_ERR_NO_BUFFER;
    }

    struct trace_buffer *trace_buf = (struct trace_buffer*) (kernel_trace_buf
            + my_core_id * TRACE_PERCORE_BUF_SIZE);

    int i;
    int new_value;
    do {
        i = trace_buf->num_applications;

        if (i == TRACE_MAX_APPLICATIONS)
            return -1; // TODO error code

        new_value = i + 1;

    } while (!trace_cas((uintptr_t*)&trace_buf->num_applications, i, new_value));

    trace_buf->applications[i].dcb = (uint64_t) dcb;
    memcpy(&trace_buf->applications[i].name, new_application_name, 8);

#endif // TRACING_EXISTS
    return SYS_ERR_OK;
}

// During boot of a core the trace buffer is not yet mapped, but we still want
// to store the applications that are started at this time. This would be fixed
// if the Kernel would be responsible for mapping the trace buffer, but currently
// it's the job of the monitor.

extern struct trace_application kernel_trace_boot_applications[];
extern int kernel_trace_num_boot_applications;

static inline void trace_new_boot_application(char* name, uintptr_t dcb)
{
#if defined(TRACING_EXISTS)
    if (kernel_trace_num_boot_applications < TRACE_MAX_BOOT_APPLICATIONS) {

        kernel_trace_boot_applications[kernel_trace_num_boot_applications].dcb = (uint64_t) dcb;
        memcpy(kernel_trace_boot_applications[kernel_trace_num_boot_applications].name, name, 8);

        kernel_trace_num_boot_applications++;
    }
#endif
}

static inline void trace_copy_boot_applications(void)
{
#if defined(TRACING_EXISTS)
    for (int i = 0; i < kernel_trace_num_boot_applications; i++) {
        trace_new_application(kernel_trace_boot_applications[i].name, kernel_trace_boot_applications[i].dcb);
    }
#endif
}
#else // !IN_KERNEL

// User-space version: gets trace buffer pointer out of the current dispatcher
static inline errval_t trace_write_event(struct trace_event *ev)
{
#ifdef TRACING_EXISTS
    dispatcher_handle_t handle = curdispatcher();

    if (((uintptr_t)handle) == ((uintptr_t)NULL)) {
        // FIXME: should return TRACE_ERR_NOT_VALID_HANDLE
        return TRACE_ERR_NO_BUFFER;
    }

    struct dispatcher_generic *disp = get_dispatcher_generic(handle);

    if (disp == NULL) {
        // FIXME: should return TRACE_ERR_NOT_VALID_DISP
        return TRACE_ERR_NO_BUFFER;
    }

    struct trace_buffer *trace_buf = disp->trace_buf;

    if (trace_buf == NULL) {
        return TRACE_ERR_NO_BUFFER;
    }

    struct trace_buffer *master = (struct trace_buffer*)trace_buffer_master;
    //struct trace_buffer *master = trace_buf->master;
    if (master == NULL) {
        return TRACE_ERR_NO_BUFFER;
    }

    if (!master->running) {
        if (ev->u.raw == master->start_trigger) {
            master->start_trigger = 0;
            master->t0 = ev->timestamp;
            if (master->duration != 0) {
                master->stop_time = master->t0 + master->duration;
            }
            master->running = true;

            // Make sure the trigger event is first in the buffer
            (void) trace_reserve_and_fill_slot(ev, trace_buf);
            return SYS_ERR_OK;

        } else {
            return SYS_ERR_OK;
        }
    }
    (void) trace_reserve_and_fill_slot(ev, trace_buf);

    if (ev->u.raw == master->stop_trigger ||
            ev->timestamp > master->stop_time) {
        master->stop_trigger = 0;
        master->running = false;
    }

#endif // TRACING_EXISTS

    return SYS_ERR_OK;
}



#endif // !IN_KERNEL



/**
 * \brief Convenience wrapper to Write a trace event
 *
 */
static inline errval_t trace_event_raw(uint64_t raw)
{
#ifdef CONFIG_TRACE

#if TRACE_ONLY_SUB_NET
    /* we do not want the stats about actual messages sent */
//    return SYS_ERR_OK;
#endif // TRACE_ONLY_SUB_NET


    struct trace_event ev;
    ev.timestamp = TRACE_TIMESTAMP();
    ev.u.raw = raw;
    return trace_write_event(&ev);
#else
    return SYS_ERR_OK;
#endif
}

#ifdef TRACING_EXISTS
#include <stdio.h>
/// Is the subsystem enabled, i.e. should we log events for it?
static inline bool trace_is_subsys_enabled(uint16_t subsys)
{
#ifdef CONFIG_TRACE
    assert(subsys < TRACE_NUM_SUBSYSTEMS);

    uint8_t* base_pointer;
#ifdef IN_KERNEL
    base_pointer = (uint8_t*) kernel_trace_buf;
#else // !IN_KERNEL
    base_pointer = (uint8_t*) trace_buffer_master;
#endif // !IN_KERNEL

    if (base_pointer == NULL) {
        // The trace buffer is not even mapped.
        return false;
    }

    bool* subsystem_states = (bool*) (base_pointer + TRACE_BUF_SIZE);

    return subsystem_states[subsys];
#else // !CONFIG_TRACE
    return false;
#endif // !CONFIG_TRACE
}
#endif // TRACING_EXISTS




static inline errval_t trace_event(uint16_t subsys, uint16_t event, uint32_t arg)
{
#ifdef CONFIG_TRACE

    // Quick hack: Only record network and kernel subsystem.
    // FIXME: This is not atall generic.  Figure out how to use following
    //  trace_is_subsys_enabled call.
/*    if (subsys != TRACE_SUBSYS_NNET && subsys != TRACE_SUBSYS_KERNEL) {
        return SYS_ERR_OK;
    }
*/

    // Check if the subsystem is enabled, i.e. we log events for it
    if (!trace_is_subsys_enabled(subsys)) {
        return SYS_ERR_OK;
    }

    //Recording the events only on the core 1
    // WARNING: Be very careful about using get_my_core_id function
    // as this function depends on working of disp pointers and they dont work
    // in thread disabled mode when you are about to return to kernel with
    // sys_yield.
    // FIXME: You can't hardcode the receiving core id.  It needs to be
    // configurable.
//    if (get_my_core_id() != 1) {
//        return SYS_ERR_OK;
//    }

    struct trace_event ev;
    ev.timestamp = TRACE_TIMESTAMP();
    ev.u.ev.subsystem = subsys;
    ev.u.ev.event     = event;
    ev.u.ev.arg       = arg;


    if (cond_termination != NULL) {
        cond_termination(false);
   }

    return trace_write_event(&ev);
#else
    return SYS_ERR_OK;
#endif
}

#endif // LIBBARRELFISH_TRACE_H
