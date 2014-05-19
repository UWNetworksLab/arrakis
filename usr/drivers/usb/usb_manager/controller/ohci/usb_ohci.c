/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include "ohci_device.h"

#include <usb/usb.h>
#include <usb/usb_error.h>

#include <usb_controller.h>
#include "usb_ohci.h"
#include "usb_ohci_memory.h"
#include "usb_ohci_bus.h"
#include "usb_ohci_root_hub.h"

/*
 * our mackerel base
 */
static struct ohci_t ohci_base;

static struct usb_ohci_ed *usb_ohci_init_ed(struct usb_ohci_ed **list)
{
    struct usb_ohci_ed *ed =  usb_ohci_ed_alloc();
    if (ed == NULL) {
        return NULL;
    }
    ed->ed_control.skip=1;

    if (list != NULL) {
        *list = ed;
    }
    return ed;
}

/**
 * \brief   initializes the host controller hardware
 *
 * \param   hc      the host controller
 * \param   suspend flag the host controller should be suspended
 */
static usb_error_t usb_ohci_init_controller(usb_ohci_hc_t *hc, uint8_t suspend)
{
    USB_DEBUG("usb_ohci_init_controller()\n");

    char status[512];
    ohci_control_pr(status, 512, hc->ohci_base);
    printf(status);

    /*
     * check the ownership of the host controller
     */
    if (ohci_control_ir_rdf(hc->ohci_base)) {
        assert(!"REQEUST OWER CHANGE. ");
    }

    // reset the device
    ohci_control_hcfs_wrf(hc->ohci_base, 0);

    /*
     * TODO: Wait till reset is done
     */
    for(uint32_t i = 0; i < 2000000000; i++);

    USB_DEBUG("usb_ohci_init_controller(): Device Reset done.\n");


    ohci_fm_interval_t ival = ohci_fm_interval_rd(hc->ohci_base);

    ohci_cmdstatus_hcr_wrf(hc->ohci_base, 1);

    for (uint16_t i = 0; i < 10; i++) {
        /*
         * TODO: Wait 10 us
         */
        for(uint32_t j = 0; j < 2000000000; j++);

        if (!ohci_cmdstatus_hcr_rdf(hc->ohci_base)) {
            break;
        }
    }
    if (ohci_cmdstatus_hcr_rdf(hc->ohci_base)) {
        debug_printf("OHCI host controller reset timeout.");
        return USB_ERR_IOERROR;
    }

    if (suspend) {
        ohci_control_hcfs_wrf(hc->ohci_base, 3);
        return USB_ERR_OK;
    }

    /*
     * Setting up register values
     */
    // HCCA pointer
    ohci_hcca_wr(hc->ohci_base, usb_ohci_hcca_physaddr());

    // Control ED head pointer
    ohci_ctrl_head_wr(hc->ohci_base, hc->qh_ctrl_first->ed_self);

    // Bulk ED head pointer
    ohci_bulk_head_wr(hc->ohci_base, hc->qh_bulk_first->ed_self);

    USB_DEBUG("usb_ohci_init() - reset and enable interrupts\n");
    // reset the interrupts
    ohci_intdisable_rawwr(hc->ohci_base, 0x0);
    ohci_interrupt_t enabled_intrs = ohci_intenable_rd(hc->ohci_base);
    enabled_intrs= ohci_interrupt_mie_insert(enabled_intrs, 1);
    enabled_intrs= ohci_interrupt_wdh_insert(enabled_intrs, 1);
    enabled_intrs= ohci_interrupt_rd_insert(enabled_intrs, 1);
    enabled_intrs= ohci_interrupt_ue_insert(enabled_intrs, 1);
    enabled_intrs= ohci_interrupt_oc_insert(enabled_intrs, 1);
    enabled_intrs= ohci_interrupt_rhsc_insert(enabled_intrs, 1);
    ohci_intenable_wr(hc->ohci_base, enabled_intrs);

    hc->enabled_intrs = enabled_intrs;

    // setting the desired features
    ohci_control_t ctrl = ohci_control_rd(hc->ohci_base);
    ctrl = ohci_control_ie_insert(ctrl, 1);
    ctrl = ohci_control_ir_insert(ctrl,0);
    ctrl = ohci_control_cle_insert(ctrl, 1);
    ctrl = ohci_control_ble_insert(ctrl, 1);
    ctrl = ohci_control_cbsr_insert(ctrl, 3);
    ctrl = ohci_control_hcfs_insert(ctrl, 2);
    ctrl = ohci_control_pe_insert(ctrl, 1);
    ctrl = ohci_control_rwe_insert(ctrl, 1);
    ctrl = ohci_control_rwc_insert(ctrl, 1);
    ohci_control_wr(hc->ohci_base, ctrl);

    /*
     * the controller is now OPERATIONAL and running.
     */
    debug_printf("OHCI host controller operational now!\n");
    // setting some remaining registers
    ival = ohci_fm_interval_fit_insert(ival, 0);
    ival = ohci_fm_interval_fsmps_insert(ival, (ival-210)*6/7);
    ohci_fm_interval_wr(hc->ohci_base, ival);
    ohci_period_start_wr(hc->ohci_base, (ival)*9/10);

    // setting some root hub fields
    ohci_rh_descra_nocp_wrf(hc->ohci_base, 1);
    ohci_rh_status_lpsc_wrf(hc->ohci_base, 1);

    /*
     * getting the port numbers
     */
    hc->root_hub_num_ports = 0;
    for (uint8_t i = 0; (i < 10) && (hc->root_hub_num_ports == 0); i++) {
        /*
         * TODO: delay
         */
        for(uint32_t j = 0; j < 100000000; j++);
        hc->root_hub_num_ports = ohci_rh_descra_ndp_rdf(hc->ohci_base);
    }
    debug_printf("OHCI CONTROLLER INTIALIZED. Having %u ports\n",
            hc->root_hub_num_ports );

    //char buf[8001];

           // ohci_rh_descra_pr(buf, 15999, hc->ohci_base);
        //    printf(buf);
            //ohci_pr(buf, 5000, hc->ohci_base);
           //printf(buf);

                       uint32_t* test = (uint32_t* )hc->ohci_base->base;
                       test = test + (-0x800+0x44)/4;
                       printf("TEST: %x", (*test)>>16);

                       //ohci_cmdstatus_ocr_wrf(hc->ohci_base, 0x1);
                       usb_ohci_root_hub_interrupt(hc);

    return USB_ERR_OK;
}

/*
 * ------------------------------------------------------------------------
 * Exported Functions
 * ------------------------------------------------------------------------
 */



/**
 * \brief   initializes the OHCI controller hardware
 */
usb_error_t usb_ohci_init(usb_ohci_hc_t *hc, uintptr_t base)
{
    /*
     * initialize the mackerel framework
     */
    ohci_initialize(&ohci_base, (mackerel_addr_t) base);
    hc->ohci_base = &ohci_base;

    /*
     * setup the endpoint descriptors
     */
    hc->qh_bulk_last = usb_ohci_init_ed(&hc->qh_bulk_first);
    hc->qh_ctrl_last = usb_ohci_init_ed(&hc->qh_ctrl_first);
    hc->qh_isoc_last = usb_ohci_init_ed(NULL);

    for (uint16_t i = 0; i<USB_OHCI_NO_EP_DESCRIPTORS; i++) {
        hc->qh_intr_last[i] = usb_ohci_init_ed(NULL);
    }

    /**
     * setup the interrupt transfer type tree
     */
    uint16_t bit = USB_OHCI_NO_EP_DESCRIPTORS / 2;
    uint16_t current;
    uint16_t next;
    while (bit) {
        current = bit;
        while (current & bit) {
            next = (current ^ bit) | (bit / 2);

            usb_ohci_ed_t *ed_current = hc->qh_intr_last[current];
            usb_ohci_ed_t *ed_next = hc->qh_intr_last[next];
            ed_current->next = NULL;
            ed_current->ed_nextED = ed_next->ed_self;

            current++;
        }
        bit >>= 1;
    }

    /*
     * after the last interrupt endpoint the isochronus follows
     */
    usb_ohci_ed_t *intr_ed = hc->qh_intr_last[0];
    intr_ed->next = hc->qh_isoc_last;
    intr_ed->ed_nextED = hc->qh_isoc_last->ed_self;
    /*
     * allocate and initiate the HCCA memory region
     */
    hc->hcca = usb_ohci_hcca_alloc();

    for (uint16_t i = 0; i < USB_OHCI_NO_EP_DESCRIPTORS; i++) {
        hc->hcca->hcca_interrupt_table[i] = hc->qh_intr_last[i
                | USB_OHCI_NO_EP_DESCRIPTORS / 2]->ed_self;
    }

    hc->controller->hcdi_bus_fn = usb_ohci_get_bus_fn();
    hc->controller->usb_revision = USB_REV_1_0;


    /*
     * initialize the hardware
     */
    if (usb_ohci_init_controller(hc, 0) != USB_ERR_OK) {
        return USB_ERR_INVAL;
    }

    USB_DEBUG("usb_ohci_init() - calling usb_ohci_do_poll\n");
    //usb_ohci_do_poll(hc->controller);

    return USB_ERR_OK;

}

/**
 * \brief   this function shuts down the controller
 *
 *
 */
void usb_ohci_detach(usb_ohci_hc_t *hc)
{
    ohci_intdisable_wr(hc->ohci_base, 0xFFFFFFFF);
    ohci_control_hcfs_wrf(hc->ohci_base, 0);
}

/**
 * \brief   the interrupt handler for this host controller
 */
void usb_ohci_interrupt(usb_ohci_hc_t *hc)
{
    struct usb_ohci_hcca *hcca = hc->hcca;
    ohci_interrupt_t status = 0;
    usb_paddr_t done = hcca->hcca_done_head;

    /*
     * check if we have some completed transfers
     */
    if (!done) {
        status = 0;
        /*
         * we have other interrupts at the time hcca done head
         * was written. read status register
         */
        if (USB_OHCI_HCCA_UNMASKED_IRQ(hcca)) {
            status = ohci_intstatus_rd(hc->ohci_base);
        }
        /*
         * we have just the WriteDoneHead interrupt
         */
        if ((hcca - USB_OHCI_HCCA_UNMASKED_IRQ(hcca))) {
            status = ohci_interrupt_wdh_insert(status, 1);
        }
    } else {
        /*
         * the done head has not been written, so read the interrupt status
         * and clear the WriteDoneHead
         */
        status = ohci_intstatus_rd(hc->ohci_base);
        status = ohci_interrupt_wdh_insert(status, 0);
    }

    /*
     * Master Interrupt Enable bit is always set on, so clear it
     */
    status = ohci_interrupt_mie_insert(status, 0);

    if (status == 0) {
        /*
         * there are no interrupts to process, just return
         */
        return;
    }

    // acknowledge the interrupts
    ohci_intstatus_wr(hc->ohci_base, status);

    // get the enabled interrupts and clear the others
    status &= hc->enabled_intrs;

    if (status == 0) {
        /*
         * there are no interrupts to be enabled, so
         * nothing to do left
         */
        return;
    }

    /*
     * ScheduleOverrun Interrupt
     *
     * This bit is set when the USB schedule for the current Frame
     * overruns and after the update of HccaFrameNumber. A scheduling
     * overrun will also cause the SchedulingOverrunCount of HcCommandStatus
     * to be incremented.
     */
    if (ohci_interrupt_so_extract(status)) {
        debug_printf("ScheduleOverrun interrupt...\n");
    }

    /*
     * ResumeDetected
     * This bit is set when HC detects that a device on the USB is
     * asserting resume signaling. It is the transition from no resume
     * signaling to resume signaling causing this bit to be set. This bit
     * is not set when HCD sets the USBRESUME state
     */
    if (ohci_interrupt_rd_extract(status)) {
        debug_printf("ResumeDetected interrupt...\n");
        }

    /*
     * UnrecoverableError
     * This bit is set when HC detects a system error not related to
     * USB. HC should not proceed with any processing nor signaling
     * before the system error has been corrected. HCD clears this bit
     * after HC has been reset.
     */
    if (ohci_interrupt_ue_extract(status)) {
        debug_printf("UnrecoverableError interrupt...\n");

        }

    /*
     * RootHubStatusChange
     *
     * This bit is set when the content of HcRhStatus or the content of
     * any of HcRhPortStatus[NumberofDownstreamPort] has changed.
     */
    if (ohci_interrupt_rhsc_extract(status)) {
        debug_printf("RootHubStatusChange interrupt...\n");

        hc->enabled_intrs = ohci_interrupt_rhsc_insert(hc->enabled_intrs, 0);
        ohci_intdisable_rhsc_wrf(hc->ohci_base, 1);

        usb_ohci_root_hub_interrupt(hc);
        }

    status = ohci_interrupt_rhsc_insert(status, 0);
    status = ohci_interrupt_wdh_insert(status, 0);
    status = ohci_interrupt_so_insert(status, 0);

    if (status) {
        ohci_intdisable_wr(hc->ohci_base, status);
        hc->enabled_intrs &= ~status;
    }

    usb_ohci_do_poll(hc->controller);
}
