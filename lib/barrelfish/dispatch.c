/**
 * \file
 * \brief Dispatcher implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/caddr.h>
#include <barrelfish/debug.h>
#include <barrelfish/deferred.h>
#include <barrelfish_kpi/cpu_arch.h>
#include "threads_priv.h"


#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

#ifdef CONFIG_INTERCONNECT_DRIVER_LMP
# include <barrelfish/lmp_chan.h>
#endif

#ifdef FPU_LAZY_CONTEXT_SWITCH
#  include <arch/fpu.h>
#endif

#if defined(__GNUC__) && !defined(__clang__) && !defined(__ICC) \
    && (defined(__x86_64__) || (defined(__i386__) && !defined(__scc__)))
// Disable SSE/MMX -- this code context switches those registers
#       pragma GCC target ("no-mmx,no-sse,no-sse2,no-sse3,no-sse4.1,no-sse4.2,no-sse4,no-sse4a,no-3dnow")
#endif

void disp_run(dispatcher_handle_t handle);
void disp_lrpc(struct lmp_endpoint *ep, uint32_t bufpos,
               uintptr_t arg1, uintptr_t arg2, uintptr_t arg3, uintptr_t arg4,
               dispatcher_handle_t handle);
void disp_pagefault(dispatcher_handle_t handle, lvaddr_t fault_address,
                    uintptr_t error, lvaddr_t ip);
void disp_pagefault_disabled(dispatcher_handle_t handle, lvaddr_t fault_address,
                             uintptr_t error, lvaddr_t ip);
void disp_trap(dispatcher_handle_t handle, uintptr_t irq, uintptr_t error,
               lvaddr_t ip);

static inline void assert_print(const char *str)
{
    sys_print(str, strlen(str));
}

static uint64_t run_counter = 0;
uint64_t disp_run_counter(void)
{
    return run_counter;
}
/**
 * \brief Run entry point
 *
 * This function is called from assembly code when the kernel enters us to
 * give us the CPU.
 *
 * \param disp Dispatcher
 */
void disp_run(dispatcher_handle_t handle)
{
#ifdef __x86_64__
    struct dispatcher_x86_64 *disp_priv = get_dispatcher_x86_64(handle);
    /* load compatibility dispatcher segment to FS */
    __asm volatile("mov %%ax, %%fs"
                   : /* No outputs */
                   : "a" (disp_priv->disp_seg_selector));
#endif
    // We can't call printf(), so do this silly thing...
//    assert_print("FIXME: infinite while loop\n");
//    while(1);

    struct dispatcher_generic* disp_gen = get_dispatcher_generic(handle);
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);

    assert_disabled(disp->disabled);
    ++run_counter;
    disp_gen->timeslice++;
    // Never let 0 be a valid timeslice number
    if(disp_gen->timeslice == 0) {
        disp_gen->timeslice++;
    }

    // Trigger any deferred events
    trigger_deferred_events_disabled(handle, disp->systime);

#ifdef CONFIG_INTERCONNECT_DRIVER_LMP
    // Check for incoming LMP messages
    if (disp->lmp_delivered != disp->lmp_seen) {
        lmp_endpoints_poll_disabled(handle);
    }

    // Trigger any send events for LMP channels
    lmp_channels_retry_send_disabled(handle);
#endif // CONFIG_INTERCONNECT_DRIVER_LMP

    // Run, saving state of previous thread if required
    thread_run_disabled(handle);

    // NOTREACHED
    assert_disabled(!"disp_run: thread_run() returned!\n");
    for(;;);
}

/**
 * \brief LRPC entry point
 *
 * This function is called from assembly code when the kernel enters us to
 * give us the CPU and deliver an LRPC message. The dispatcher is disabled.
 *
 * \param ep LMP endpoint structure
 * \param bufpos Reserved position in endpoint message buffer
 * \param arg1 Message payload
 * \param arg2 Message payload
 * \param arg3 Message payload
 * \param arg4 Message payload
 * \param handle Dispatcher pointer
 *
 * \note Dispatcher pointer comes last here, because AB was too lazy to shuffle
 *        registers through the whole LRPC path when adding it.
 */
void disp_lrpc(struct lmp_endpoint *ep, uint32_t bufpos,
               uintptr_t arg1, uintptr_t arg2, uintptr_t arg3, uintptr_t arg4,
               dispatcher_handle_t handle)
{
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);

    /* deliver LRPC message to buffer */
    lmp_endpoint_store_lrpc_disabled(ep, bufpos, arg1, arg2, arg3, arg4);

    // set hint
    disp->lmp_hint = (lvaddr_t)&ep->k - (lvaddr_t)handle;

    disp_run(handle);
}

/**
 * \brief Initialise the dispatcher, while disabled
 *
 * This function is called to setup the dispatcher structure while still
 * disabled.
 *
 * \param disp Dispatcher
 */
void disp_init_disabled(dispatcher_handle_t handle)
{
    assert_disabled(handle != 0);
    struct dispatcher_generic* disp_gen = get_dispatcher_generic(handle);
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);
    assert_disabled(disp->disabled);

    // Initialize entry points (and LDT on x86_64)
    disp_arch_init(handle);

    disp_gen->timeslice = 1;

    // Initialize important capability pointers
    if (disp_gen->dcb_cap.slot == 0) {
        disp_gen->dcb_cap.cnode = cnode_task;
        disp_gen->dcb_cap.slot = TASKCN_SLOT_DISPATCHER;
    }

    disp_gen->cleanupthread = NULL;
    thread_mutex_init(&disp_gen->cleanupthread_lock);
}

/**
 * \brief Yield the dispatcher's CPU
 *
 * This function yields the CPU. It may only be called while disabled.
 *
 * \param disp Current dispatcher
 */
void disp_yield_disabled(dispatcher_handle_t handle)
{
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);
    assert_disabled(disp->disabled);

#ifdef CONFIG_DEBUG_DEADLOCKS
    disp->yieldcount++;
#endif

    // FIXME:  This perticular trace event is breaking as it is running
    // into problems due to assumptions about segment register %fs
//    trace_event(TRACE_SUBSYS_THREADS, TRACE_EVENT_THREADS_SYS_YIELD, 2);
    sys_yield(CPTR_NULL);
    assert_print("dispatcher PANIC: sys_yield returned");
    for (;;);
}

/**
 * \brief Disable the dispatcher
 *
 * This function disables the current dispatcher, returning a pointer to it.
 * The dispatcher must be enabled.
 *
 * While the dispatcher is disabled, the current thread cannot be preempted,
 * and no incoming LMP messages can be received.
 */
dispatcher_handle_t disp_disable(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);
    assert_disabled(disp->disabled == 0);
    disp->disabled = 1;
    return handle;
}

/**
 * \brief Re-enable the dispatcher
 *
 * This function re-enables the current dispatcher.
 * The dispatcher must be disabled.
 */
void disp_enable(dispatcher_handle_t handle)
{
    assert_disabled(handle == curdispatcher());
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);
    assert_disabled(disp->disabled == 1);
    disp->disabled = 0;
}

/**
 * \brief Return a pointer to the name field of the current dispatcher
 *
 * May be called when the dispatcher is either enabled or disabled.
 *
 * \returns a string of at most #DISP_NAME_LEN characters,
 *      which may not be nul-terminated.
 */
const char *disp_name(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);
    return disp->name;
}

/**
 * \brief Page fault entry point
 *
 * This function is called from assembly code when the kernel
 * enters us to report a page fault while enabled.
 *
 * \param handle Dispatcher
 * \param fault_address Fault address
 * \param error CPU error code
 * \param ip Faulting instruction pointer
 */
void disp_pagefault(dispatcher_handle_t handle, lvaddr_t fault_address,
                    uintptr_t error, lvaddr_t ip)
{
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    arch_registers_state_t *regs = dispatcher_get_enabled_save_area(handle);
    enum pagefault_exception_type fault_type;

    // FIXME: this logic is x86-specific. it needs to be moved into an arch file
#if defined(__x86_64__) || defined(__i386__)
    const uintptr_t ERR_PF_PRESENT         = (1 << 0);
    const uintptr_t ERR_PF_READ_WRITE      = (1 << 1);
    const uintptr_t ERR_PF_USER_SUPERVISOR = (1 << 2);
    const uintptr_t ERR_PF_RESERVED        = (1 << 3);
    const uintptr_t ERR_PF_INSTRUCTION     = (1 << 4);

    if ((error & ERR_PF_INSTRUCTION) != 0) {
        fault_type = PAGEFLT_EXEC;
    } else if ((error & ERR_PF_READ_WRITE) != 0) {
        fault_type = PAGEFLT_WRITE;
    } else {
        fault_type = PAGEFLT_READ;
    }
#else
    assert_print("Warning: don't know how to determine fault type on this arch!\n");
    fault_type = PAGEFLT_NULL;
#endif

    // sanity-check IP in save area
    assert_disabled(ip == registers_get_ip(regs));

    // sanity-check that we were on a thread
    assert_disabled(disp_gen->current != NULL);

    // Save FPU context if used
#ifdef FPU_LAZY_CONTEXT_SWITCH
    if (disp_gen->fpu_thread == disp_gen->current) {
        fpu_copy(&disp_gen->current->fpu_state,
                 dispatcher_get_enabled_fpu_save_area(handle));
    }
#endif

    // try to deliver exception
    thread_deliver_exception_disabled(handle, EXCEPT_PAGEFAULT, fault_type,
                                      (void *)fault_address, regs);

    // it returned: this means the exception couldn't be delivered and the
    // thread is dead. better tell them what happened...

    static char str[256];
    snprintf(str, sizeof(str), "%.*s: unhandled page fault (error code 0x%"
             PRIxPTR ") on %" PRIxPTR " at IP %" PRIxPTR "\n",
             DISP_NAME_LEN, disp->name, error, fault_address, ip);
    assert_print(str);

#if defined(__x86_64__) || defined(__i386__)
    snprintf(str, sizeof(str), "%s page fault due to %s%s, while in %s mode%s\n",
           error & ERR_PF_READ_WRITE ? "write" : "read",
           error & ERR_PF_PRESENT ? "access violation" : "page not present",
           error & ERR_PF_RESERVED ? ", reserved bits set in page table"
           : "",
           error & ERR_PF_USER_SUPERVISOR ? "user" : "supervisor",
           error & ERR_PF_INSTRUCTION ? ", by instruction fetch" : "");
    assert_print(str);
#endif

    // print out stuff
    debug_print_save_area(regs);
    debug_dump(regs);
    //debug_call_chain(regs);

    // run something else
    thread_run_disabled(handle);
}


/**
 * \brief Disabled page fault entry point
 *
 * This function is called from assembly code when the kernel enters us to
 * report a page fault while disabled.
 *
 * \param handle Dispatcher
 * \param fault_address Fault address
 * \param error CPU error code
 * \param ip Faulting instruction pointer
 */
void disp_pagefault_disabled(dispatcher_handle_t handle, lvaddr_t fault_address,
                             uintptr_t error, lvaddr_t ip)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    static char str[256];
    snprintf(str, 256, "%.*s: page fault WHILE DISABLED"
             " (error code 0x%" PRIxPTR ") on %" PRIxPTR " at IP %" PRIxPTR "\n",
             DISP_NAME_LEN, disp->name, error, fault_address, ip);
    assert_print(str);
    if(fault_address == 0) {
        assert_print("NULL pointer dereferenced!\n");
    }

    // NOTE: Based on which code is is causing page fault, only assert_print
    // is safe bet to print anything here.  Anything else would cause
    // page fault in itself.
    assert_disabled(disp->disabled);


    // FIXME: Make sure that following are using assert_print to avoid
    //  loop of disabled pagefaults
    // arch_registers_state_t *regs = dispatcher_get_trap_save_area(handle);
    // debug_print_save_area(regs);

    // disabled by AB, because we can get into a loop of disabled pagefaults
    //    debug_dump(regs);
    //    debug_return_addresses();
    for(;;);
}

#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/capabilities.h>

// Handle FPU not available trap
#ifdef FPU_LAZY_CONTEXT_SWITCH
static void
// workaround internal compiler error in gcc 4.5
#if defined(__x86_64__) && defined(__GNUC__) \
    && __GNUC__ == 4 && __GNUC_MINOR__ == 5 && __GNUC_PATCHLEVEL__ <= 3
__attribute__((optimize(0)))
#endif
handle_fpu_unavailable(dispatcher_handle_t handle,
                       arch_registers_state_t *regs,
                       struct thread *t)
{
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    arch_registers_fpu_state_t *disp_fpu =
        dispatcher_get_enabled_fpu_save_area(handle);

    // Store state for remembered thread
    if (disp_gen->fpu_thread != NULL && disp_gen->fpu_thread != t) {
        // Store state only if last FPU user wasn't current thread
        fpu_copy(&disp_gen->fpu_thread->fpu_state, disp_fpu);
    }

    if(!t->used_fpu) {      // If first time, reset FPU
      /* debug_printf("FPU reset\n"); */
        fpu_init();
        t->used_fpu = true;

        /* uint16_t fpu_status; */
        /* __asm volatile("fnstsw %0" : "=a" (fpu_status)); */
	/* debug_printf("FPU status: %x\n", fpu_status); */
        /* uint16_t fpu_ctrl; */
        /* __asm volatile("fnstcw %0" : "=m" (fpu_ctrl)); */
	/* debug_printf("FPU control: %x\n", fpu_ctrl); */

    } else {                // If not, restore from thread/dispatcher area
        arch_registers_fpu_state_t *fpustate;

	/* debug_printf("restore FPU\n"); */

        if(disp_gen->fpu_thread == t) {
	  /* debug_printf("from dispatcher\n"); */
            fpustate = disp_fpu;

            /* XXX: Potential optimization: If we switched between
             * one FPU using and several non-FPU using threads
             * within the same timeslice, we will get the trap
             * again as we return to the FPU using thread. In that
             * case, the state in the FPU will still be valid, so
             * we don't need to restore it here. We could
             * determine this by remembering the context switch
             * epoch (disp_gen->timeslice) of the last time we got
             * the trap.
             */
        } else {
	  /* debug_printf("from thread\n"); */
            fpustate = &t->fpu_state;
        }

        fpu_restore(fpustate);

	/* debug_printf("restoring from %p of dispatcher %p (handle %x):\n", fpustate, disp_gen, handle); */

	/* for(int i = 0; i <512 + 16; i++) { */
	/*   char str[128]; */
	/*   snprintf(str, 128, "%x ", fpustate->registers[i]); */
	/*   assert_print(str); */
	/* } */
	/* assert_print("\n"); */

        /* uint16_t fpu_status; */
        /* __asm volatile("fnstsw %0" : "=a" (fpu_status)); */
	/* debug_printf("FPU status: %x\n", fpu_status); */
        /* uint16_t fpu_ctrl; */
        /* __asm volatile("fnstcw %0" : "=m" (fpu_ctrl)); */
	/* debug_printf("FPU control: %x\n", fpu_ctrl); */
    }

    // Remember FPU-using thread
    disp_gen->fpu_thread = t;

    // Resume current thread
    disp_resume(handle, regs);
}
#endif

/**
 * \brief Trap entry point
 *
 * This function is called from assembly code when the kernel enters us to
 * report a trap taken while we were enabled.
 *
 * \param handle Dispatcher
 * \param irq Trap vector
 * \param error CPU error code
 * \param ip Faulting instruction pointer
 */
void disp_trap(dispatcher_handle_t handle, uintptr_t irq, uintptr_t error,
               lvaddr_t ip)
{
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);
    arch_registers_state_t *regs = dispatcher_get_trap_save_area(handle);
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);

    // Must've happened on a thread
    struct thread *t = disp_gen->current;
    assert_disabled(t != NULL);

#ifdef FPU_LAZY_CONTEXT_SWITCH
    if(irq == FPU_UNAVAILABLE_TRAP) {
      /* debug_printf("trap! disp handle %x\n", handle); */
        handle_fpu_unavailable(handle, regs, t);
    }
#endif

    // sanity-check IP in save area
    // not valid for debug exceptions?
    // assert_disabled(ip == registers_get_ip(regs));

    enum exception_type type;
#if defined(__i386__) || defined(__x86_64__)
    switch (irq) {
    case IDT_DB:
        type = EXCEPT_SINGLESTEP;
        break;

    case IDT_BP:
        type = EXCEPT_BREAKPOINT;
        break;

    default:
        type = EXCEPT_OTHER;
        break;
    }
#else
    type = EXCEPT_OTHER; // XXX
#endif

    // Save FPU context if used
#ifdef FPU_LAZY_CONTEXT_SWITCH
    if (disp_gen->fpu_thread == t) {
        fpu_copy(&t->fpu_state,
                 dispatcher_get_enabled_fpu_save_area(handle));
    }
#endif

    // deliver exception (shouldn't return)
    thread_deliver_exception_disabled(handle, type, irq, (void *)ip, regs);

    // if it failed, say something
    static char str[256];
    snprintf(str, sizeof(str), "%.*s: unhandled trap (IRQ %" PRIuPTR
             ", error code 0x%" PRIxPTR ") at IP %" PRIxPTR "\n",
             DISP_NAME_LEN, disp->name, irq, error, ip);
    assert_print(str);

     // print out stuff
    debug_print_save_area(regs);
    debug_dump(regs);
    //debug_call_chain(regs);

    // run something else
    thread_run_disabled(handle);
}

void
disp_assert_fail(const char *exp, const char *file, const char *func, const char *line)
{
    const char *dispname = disp_name();

    // We can't call printf(), so do this silly thing...
    assert_print("Dispatcher assertion failed in ");
    for(int i = 0; i < DISP_NAME_LEN && dispname[i] != '\0'; i++) {
        sys_print(&dispname[i], 1);
    }
    assert_print(": ");
    assert_print(exp);
    assert_print(", function ");
    assert_print(func);
    assert_print(", file ");
    assert_print(file);
    assert_print(", line ");
    assert_print(line);
    assert_print(".\n");

    for(;;);
}

void
disp_warn_fail(const char *exp, const char *file, const char *func, const char *line)
{
    const char *dispname = disp_name();

    // We can't call printf(), so do this silly thing...
    assert_print("Dispatcher warning in ");
    for(int i = 0; i < DISP_NAME_LEN && dispname[i] != '\0'; i++) {
        sys_print(&dispname[i], 1);
    }
    assert_print(": ");
    assert_print(exp);
    assert_print(", function ");
    assert_print(func);
    assert_print(", file ");
    assert_print(file);
    assert_print(", line ");
    assert_print(line);
    assert_print(".\n");
}
