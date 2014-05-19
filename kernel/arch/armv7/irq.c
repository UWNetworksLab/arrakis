/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <kernel.h>
#include <stdio.h>
#include <string.h>
#include <arm.h>
#include <barrelfish_kpi/lmp.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/sys_debug.h>

#include <arch/armv7/arm_hal.h>
#include <arch/armv7/start_aps.h>
#include <arch/armv7/irq.h>

#include <paging_kernel_arch.h>
#include <dispatch.h>
#include <exec.h>
#include <stdio.h>
#include <syscall.h>
#include <arch/armv7/armv7_syscall.h>
#include <arch/armv7/start_aps.h>

/*
 * Interrupt controller (Cortex-A9 MPU INTC) with up to 128 interrupt requests
 */
#define NUM_INTR                (128+32)

/// Size of hardware IRQ dispatch table == #NIDT - #NEXCEPTIONS exceptions
#define NDISPATCH               (NUM_INTR)

#define GIC_IRQ_PRIO_LOWEST       (0xF)
#define GIC_IRQ_CPU_TRG_ALL       (0x3) // For two cores on the PandaBoard
#define GIC_IRQ_CPU_TRG_BSP       (0x1)
#define GIC_IRQ_EDGE_TRIGGERED    (0x1)
#define GIC_IRQ_LEVEL_SENSITIVE   (0x0)
#define GIC_IRQ_1_TO_N            (0x1)
#define GIC_IRQ_N_TO_N            (0x0)

/**
 * \brief User-space IRQ dispatch table.
 *
 * This is essentially a big CNode holding #NDISPATCH capability
 * entries to local endpoints of user-space applications listening to
 * the interrupts.
 */
static struct cte irq_dispatch[NDISPATCH];

errval_t irq_table_set(unsigned int nidt, capaddr_t endpoint)
{
    errval_t err;
    struct cte *recv;

    err = caps_lookup_slot(&dcb_current->cspace.cap, endpoint, CPTR_BITS, &recv,
            CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_IRQ_LOOKUP);
    }

    assert(recv != NULL);

    // Return w/error if cap is not an endpoint
    if (recv->cap.type != ObjType_EndPoint) {
        return SYS_ERR_IRQ_NOT_ENDPOINT;
    }

    // Return w/error if no listener on endpoint
    if (recv->cap.u.endpoint.listener == NULL) {
        return SYS_ERR_IRQ_NO_LISTENER;
    }

    if (nidt < NDISPATCH) {
        // check that we don't overwrite someone else's handler
        if (irq_dispatch[nidt].cap.type != ObjType_Null) {
            printf("kernel: installing new handler for IRQ %d\n", nidt);
        }
        err = caps_copy_to_cte(&irq_dispatch[nidt], recv, false, 0, 0);

        gic_enable_interrupt(nidt, GIC_IRQ_CPU_TRG_ALL, 0,
                GIC_IRQ_EDGE_TRIGGERED, GIC_IRQ_N_TO_N);
#if 0
        if (err_is_ok(err)) {
            // Unmask interrupt if on PIC
            if(nidt < 16) {
                pic_toggle_irq(nidt, true);
            }
        }
#endif
        return err;
    }

    return SYS_ERR_IRQ_INVALID;
}

errval_t irq_table_delete(unsigned int nidt)
{
    if (nidt < NDISPATCH) {
        irq_dispatch[nidt].cap.type = ObjType_Null;

        /* todo: gic disable irq */

        return SYS_ERR_OK;
    }
    return SYS_ERR_IRQ_INVALID;
}

/**
 * \brief Send interrupt notification to user-space listener.
 *
 * Sends an interrupt notification IDC to a local endpoint that
 * listens for IRQ notifications.
 *
 * \param irq   IRQ# to send in notification.
 */
void send_user_interrupt(int irq)
{
    assert(irq >= 0 && irq < NDISPATCH);
    struct capability *cap = &irq_dispatch[irq].cap;

    // Return on null cap (unhandled interrupt)
    if (cap->type == ObjType_Null) {
        printk(LOG_WARN, "unhandled IRQ %d\n", irq);
        return;
    }

    // Otherwise, cap needs to be an endpoint
    assert(cap->type == ObjType_EndPoint);
    errval_t err = lmp_deliver_notification(cap);
    if (err_is_fail(err)) {
        if (err_no(err) == SYS_ERR_LMP_BUF_OVERFLOW) {
            struct dispatcher_shared_generic *disp =
                    get_dispatcher_shared_generic(
                            cap->u.endpoint.listener->disp);
            printk(LOG_DEBUG, "%.*s: IRQ message buffer overflow\n",
                    DISP_NAME_LEN, disp->name);
        } else {
            printk(LOG_ERR, "Unexpected error delivering IRQ\n");
        }
    }

#ifdef SCHEDULER_RR
    /* XXX: run the handler dispatcher immediately
     * we shouldn't do this (we should let the scheduler decide), but because
     * our default scheduler is braindead, this is a quick hack to make sure
     * that mostly-sane things happen
     */
    dispatch(cap->u.endpoint.listener);
#else
    dispatch(schedule());
#endif
}
