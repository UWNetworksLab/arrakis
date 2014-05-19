/**
 * \file
 * \brief Kernel management of dispatchers
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_DISPATCH_H
#define KERNEL_DISPATCH_H

#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/dispatcher_shared_arch.h>
#include <capabilities.h>
#include <misc.h>

extern uint64_t context_switch_counter;

/**
 * \brief Structure to hold information regarding AMD SVM
 */
struct guest {
    struct cte          monitor_ep;     ///< The endpoint to the monitor
    struct cte          ctrl;           ///< The VMKit guest shared data structure
    struct cte          vmcb;           ///< The physical address of the AMD VMCB
};

/**
 * \brief The dispatcher control block.
 *
 * This block holds necessary kernel data to control a user-space dispatcher
 */
struct dcb {
    dispatcher_handle_t disp;           ///< User-mode dispatcher frame pointer
    bool                disabled;       ///< Was dispatcher disabled when last saved?
    struct cte          cspace;         ///< Cap slot for CSpace
    lpaddr_t            vspace;         ///< Address of VSpace root
    struct cte          disp_cte;
    unsigned int        faults_taken;   ///< # of disabled faults or traps taken
    /// Indicates whether this domain shall be executed in VM guest mode
    bool                is_vm_guest;
    struct guest        guest_desc;     ///< Descriptor of the VM Guest
    uint64_t            domain_id;      ///< ID of dispatcher's domain
    systime_t           wakeup_time;    ///< Time to wakeup this dispatcher
    struct dcb          *wakeup_prev, *wakeup_next; ///< Next/prev in timeout queue

#if defined(CONFIG_SCHEDULER_RR)
    struct dcb          *prev, *next;   ///< Prev/Next DCBs in schedule
#elif defined(CONFIG_SCHEDULER_RBED)
    struct dcb          *next;          ///< Next DCB in schedule
    unsigned long       release_time, etime, last_dispatch;
    unsigned long       wcet, period, deadline;
    unsigned short      weight;
    enum task_type      type;
#else
# error Must define a kernel scheduling policy!
#endif
};

/// The currently running dispatcher and FPU dispatcher
extern struct dcb *dcb_current, *fpu_dcb;

void dispatch(struct dcb *dcb) __attribute__ ((noreturn));
errval_t lmp_deliver_payload(struct capability *ep, struct dcb *send,
                             uintptr_t *payload, size_t payload_len,
                             bool captransfer);
errval_t lmp_deliver(struct capability *ep, struct dcb *send,
                     uintptr_t *payload, size_t payload_len,
                     capaddr_t send_cptr, uint8_t send_bits);

/// Deliver an empty LMP as a notification
static inline errval_t lmp_deliver_notification(struct capability *ep)
{
    return lmp_deliver_payload(ep, NULL, NULL, 0, false);
}

/// Reset csc
static inline void dispatch_csc_reset(void)
{
    context_switch_counter = 0;
}

/// Returns csc
static inline uint64_t dispatch_get_csc(void)
{
    return context_switch_counter;
}

#endif
