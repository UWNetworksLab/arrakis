/**
 * \brief This file contains code to initalize the EHCI controller and to
 *        handle the interrupts
 */

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

#include <usb/usb.h>

#include <usb_controller.h>
#include <usb_device.h>

#include "usb_ehci.h"
#include "usb_ehci_memory.h"
#include "usb_ehci_bus.h"
#include "usb_ehci_root_hub.h"
#include "usb_ehci_xfer.h"

/**
 * \brief   performs a soft reset on the EHCI host controller
 *
 * \param   hc the ehci controller to reset
 */
usb_error_t usb_ehci_hc_reset(usb_ehci_hc_t *hc)
{
    /* write the reset bit */
    ehci_usbcmd_hcr_wrf(&hc->ehci_base, 1);

    /* wait some time to let rest complete */
    USB_WAIT(200);

    for (uint8_t i = 0; i < 10; i++) {
        if (ehci_usbcmd_hcr_rdf(&hc->ehci_base)) {
            /*
             * the host controller sets this bit to 0 if the rest is complete
             * therefore we have to wait some more time
             */
            USB_WAIT(200);
            continue;
        }
    }

    /* last check if reset was completed */
    if (ehci_usbcmd_hcr_rdf(&hc->ehci_base)) {
        return (USB_ERR_TIMEOUT);
    }

    usb_error_t err = USB_ERR_OK;

    /*
     * The EHCI specification says that certain registers must have a
     * defined value after reset while others are undefined.
     *
     * The following code checks if the reset was successful.
     */
    if ((ehci_usbcmd_rawrd(&hc->ehci_base) & 0xFFFFF0FF) != 0x00080000) {
        debug_printf("WARNING: USBCMD has wrong value.\n");
        err = USB_ERR_IOERROR;
    }
    if (ehci_usbsts_rawrd(&hc->ehci_base) != 0x00001000) {
        debug_printf("WARNING: USBST has wrong value.\n");
        err = USB_ERR_IOERROR;
    }
    if (ehci_usbintr_rawrd(&hc->ehci_base) != 0x00000000) {
        debug_printf("WARNING: USBINTR has wrong value.\n");
        err = USB_ERR_IOERROR;
    }
    if (ehci_frindex_rawrd(&hc->ehci_base) != 0x00000000) {
        debug_printf("WARNING:FRINDEX has wrong value.\n");
        err = USB_ERR_IOERROR;
    }
    if (ehci_ctrldssegment_rawrd(&hc->ehci_base) != 0x00000000) {
        debug_printf("WARNING: Reset is succeeded: CTRLDS has wrong value.\n");
        err = USB_ERR_IOERROR;
    }
    if (ehci_configflag_rawrd(&hc->ehci_base) != 0x00000000) {
        debug_printf("WARNING: CONFIGFLG has wrong value.\n");
        err = USB_ERR_IOERROR;
    }
    if ((ehci_portsc_rawrd(&hc->ehci_base, 0) | 0x00001000) != 0x00003000) {
        debug_printf("WARNING: PORTSC has wrong value.\n");
        err = USB_ERR_IOERROR;
    }

    return (err);
}

/**
 * \brief halts the EHCI controller. Running transactions are finished first
 *
 * \param hc the host controller to halt
 */
static usb_error_t usb_ehci_hc_halt(usb_ehci_hc_t *hc)
{
    /* write the reset bit */
    ehci_usbcmd_wr(&hc->ehci_base, 0x0);

    /*
     * Wait some time before start checking
     */
    USB_WAIT(200);

    /* wait until the reset is done */
    for (uint8_t i = 0; i < 10; i++) {
        if (ehci_usbsts_hch_rdf(&hc->ehci_base)) {
            /* all activity halted, return */
            return (USB_ERR_OK);
        }
        USB_WAIT(200);
    }

    /* check if halted */
    if (ehci_usbsts_hch_rdf(&hc->ehci_base)) {
        /* all activity halted, return */
        return (USB_ERR_OK);
    }

    return (USB_ERR_TIMEOUT);
}

usb_error_t usb_ehci_initialize_controller(usb_ehci_hc_t *hc)
{
    /*
     * XXX: 64 bit data structures are not supported at the moment
     */
    if (ehci_hccparams_bit64ac_rdf(&hc->ehci_base)) {
        debug_printf("NYI: Host controller uses 64-bit memory addresses!\n");
        return (USB_ERR_BAD_CONTEXT);

        ehci_ctrldssegment_wr(&hc->ehci_base, 0);
    }

    /*
     * STEP 1: Setting the start addresses of the periodic and asynchronous
     *         transfer lists
     */
    ehci_periodiclistbase_wr(&hc->ehci_base, hc->pframes_phys);
    ehci_asynclistaddr_wr(&hc->ehci_base, hc->qh_async_last->qh_self);

    /*
     * STEP 2: Setting the USBCMD register
     */
    ehci_usbcmd_t cmd = 0;
    // interrupt threshold to 1 micro frame
    cmd = ehci_usbcmd_itc_insert(cmd, 1);
    // enable the async schedule
    cmd = ehci_usbcmd_ase_insert(cmd, 1);
    // enable the periodic schedule
    cmd = ehci_usbcmd_pse_insert(cmd, 1);
    // keep the frame list size
    cmd = ehci_usbcmd_fls_insert(cmd, ehci_usbcmd_fls_rdf(&hc->ehci_base));
    // start the host controller
    cmd = ehci_usbcmd_rs_insert(cmd, 1);

    // write the cmd value to the register
    ehci_usbcmd_wr(&hc->ehci_base, cmd);

    /*
     * STEP 3: Take over the port ownership
     */
    ehci_configflag_cf_wrf(&hc->ehci_base, 1);

    /* wait till the HC is up and running */
    USB_WAIT(200);
    for (uint32_t i = 0; i < 10; i++) {
        if (!ehci_usbsts_hch_rdf(&hc->ehci_base)) {
            break;
        }
        USB_WAIT(200);
    }


    if (ehci_usbsts_hch_rdf(&hc->ehci_base)) {
        /* the host controller is still not started. */
        return (USB_ERR_TIMEOUT);
    }

    /*
     * STEP 4: Enable the interrupts
     */
    ehci_usbintr_wr(&hc->ehci_base, hc->enabled_interrupts);

    /*
     * STEP 5: Check if everything is fine i.e. controller running.
     */
    if (ehci_usbsts_hch_rdf(&hc->ehci_base)) {
        /*
         * the HC does not want to do what we want
         */
        debug_printf("ERROR: Host controller does not start...\n");

        return (USB_ERR_IOERROR);
    }

    /*
     * STEP 4: Enable power on the ports.
     *
     * R/WHost controller has port power control switches. This bit
     * represents the current setting of the switch (0 = off, 1 = on). When
     * power is not available on a port (i.e. PP equals a 0), the port is non-
     * functional and will not report attaches, detaches, etc.
     */
    if (ehci_hcsparams_ppc_rdf(&hc->ehci_base)) {
        for (uint8_t i = 0; i < hc->rh_num_ports; i++) {
            ehci_portsc_pp_wrf(&hc->ehci_base, i, 1);
        }
    }

    debug_printf("EHCI controller up and running.\n");

    return (USB_ERR_OK);
}


/**
 * \brief   Interrupt handler for the EHCI controller hardware
 *
 * \param   hc  host controller
 */
void usb_ehci_interrupt(usb_host_controller_t *host)
{
    USB_DEBUG_TR_ENTER;

    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) host->hc_control;

    /*
     * read the status register and mask out the interrupts [5..0]
     */
    ehci_usbsts_t intr = ehci_usbsts_rawrd(&hc->ehci_base) & 0x3F;

    if (!(intr)) {
        /* there was no interrupt for this controller */
        return;
    }

    if (!(intr & hc->enabled_interrupts)) {
        /* there was an interrupt we dont have enabled */
        USB_DEBUG("Unenabled interrupt happened.\n");
        return;
    }

    /* acknowledge the interrupts */
    ehci_usbsts_wr(&hc->ehci_base, intr);

    intr &= hc->enabled_interrupts;

    if (ehci_usbsts_hse_extract(intr)) {
        /*
         * host system error -> unrecoverable error
         * serious error occurs during a host system access involving the host
         * controller module.
         */
        USB_DEBUG("EHCI controller encountered an unrecoverable error\n");
        USER_PANIC("NYI: handling of unrecoverable errors! \n");

    }

    uint8_t pcd_disabled = 0;

    if (ehci_usbsts_pcd_extract(intr)) {
        /*
         * port change detected
         * there is something going on on the port e.g. a new device is attached
         *
         * This interrupt stays enabled until the port is reset, so to avoid
         * multiple interrupts, disable the port change interrupts for now.
         */
        ehci_usbsts_pcd_insert(hc->enabled_interrupts, 0);

        /* disable the PCD interrupt for now */
        ehci_usbintr_pcie_wrf(&hc->ehci_base, 0);

        pcd_disabled = 1;

        /* handle port status change */
        usb_ehci_roothub_interrupt(hc);
    }

    intr = ehci_usbsts_pcd_insert(intr, 0);
    intr = ehci_usbsts_iaa_insert(intr, 0);
    intr = ehci_usbsts_usbei_insert(intr, 0);
    intr = ehci_usbsts_usbi_insert(intr, 0);

    if (intr != 0) {
        USB_DEBUG("NOTICE: Blocking interrupt %x\n", intr);
        /*
         * there is still an interrupt left, so block on this type
         */
        hc->enabled_interrupts &= ~intr;
        ehci_usbintr_wr(&hc->ehci_base, hc->enabled_interrupts);
    }

    /* poll the USB transfers */
    usb_ehci_poll(hc);

    if (pcd_disabled) {
        /* enable the port status change interrupt again */
        ehci_usbintr_pcie_insert(hc->enabled_interrupts, 1);
        ehci_usbintr_pcie_wrf(&hc->ehci_base, 1);
    }

    USB_DEBUG_TR_RETURN;
}

/**
 * \brief initializes the ehci host controller and creates the queues
 *
 * \param hc the ehci controller to initialize
 * \param base the base address of the ehci controller
 */
usb_error_t usb_ehci_init(usb_ehci_hc_t *hc, uintptr_t base)
{
    /*
     * STEP 1: Mackerel init
     * Initialization of the ehci_t struct has to be done in two steps
     * since the ehci has capability registers and operational registers
     * which offsets has to be read out of the caplength register first.
     */
    ehci_initialize(&hc->ehci_base, (mackerel_addr_t) base, NULL);

    /* getting the operational register offset */
    uint8_t cap_offset = ehci_caplength_rd(&hc->ehci_base);

    if (cap_offset == 0) {
        debug_printf("ERROR: EHCI capability register length is zero.\n");
        return (USB_ERR_INVAL);
    }

    ehci_initialize(&hc->ehci_base, (mackerel_addr_t) base,
            (mackerel_addr_t) (base + cap_offset));

    /*
     * STEP 2: getting the number of ports and the revision
     */
    hc->ehci_revision = ehci_hciversion_rd(&hc->ehci_base);
    hc->rh_num_ports = ehci_hcsparams_n_ports_rdf(&hc->ehci_base);

    debug_printf("Device found: EHCI controller rev: %x.%x with %u ports\n",
            hc->ehci_revision >> 8, hc->ehci_revision & 0xFF, hc->rh_num_ports);

    /*
     * STEP 3: perform halt
     * This stops the execution of (potentially) existing transfers
     */
    usb_error_t err;
    err = usb_ehci_hc_halt(hc);
    if (err != USB_ERR_OK) {
        debug_printf("WARNING: Host controller has not halted properly.\n");
    }

    /*
     * STEP 4: perform host controller reset
     * Performing a reset also checks the register fields for the correct values
     */
    err = usb_ehci_hc_reset(hc);
    if (err != USB_ERR_OK) {
        debug_printf("ERROR: Host controller not reset properly. \n");
    }

    /* after reset this field should have been initialized to a valid value */
    if (ehci_usbcmd_fls_rdf(&hc->ehci_base) == ehci_frame_rsvd) {
        /*
         * this field should be initialized to a correct values
         * having FLS=0x3 is invalid!
         */
        debug_printf("ERROR: Wrong frame length size\n");
        return (USB_ERR_IOERROR);
    }

    /*
     * STEP 5: init the generic host controller
     * This fills out the missing fields of the generic host controller
     */
    hc->controller->hcdi_bus_fn = usb_ehci_get_bus_fn();
    hc->controller->usb_revision = USB_REV_2_0;
    hc->controller->devices = hc->devices;
    hc->controller->devices_max = USB_EHCI_MAX_DEVICES;

    /*
     * STEP 6: Setting up the enabled interrupts
     * this does not activate them, just prepare it for later.
     */
    ehci_usbintr_t en_intrs = 0;
    en_intrs = ehci_usbintr_iaae_insert(en_intrs, 0);
    en_intrs = ehci_usbintr_hsee_insert(en_intrs, 1);
    en_intrs = ehci_usbintr_pcie_insert(en_intrs, 1);
    en_intrs = ehci_usbintr_usbie_insert(en_intrs, 1);
    en_intrs = ehci_usbintr_usbeie_insert(en_intrs, 1);
    hc->enabled_interrupts = en_intrs;

    /*
     * STEP 7: Setting up the initial transfer queues
     */
    usb_ehci_qh_t *qh;

    /* setup the terminate qheue head */
    qh = usb_ehci_qh_alloc();
    qh->qh_next_qtd = USB_EHCI_LINK_TERMINATE;
    qh->qh_alt_next_qtd = USB_EHCI_LINK_TERMINATE;
    qh->qh_status.status = USB_EHCI_QTD_STATUS_HALTED;
    hc->qh_terminate = qh;

    /*
     * STEP 7.1a: Initialize the interrupt transfer queues
     */
    for (uint32_t i = 0; i < USB_EHCI_VFRAMELIST_COUNT; i++) {
        qh = usb_ehci_qh_alloc();

        hc->qh_intr_last[i] = qh;
        qh->qh_ep.ep_speed = USB_EHCI_QH_SPEED_HIGH;
        qh->qh_ep.mult = 1;
        qh->qh_curr_qtd = 0;
        qh->qh_next_qtd = USB_EHCI_LINK_TERMINATE;
        qh->qh_alt_next_qtd = USB_EHCI_LINK_TERMINATE;
        qh->qh_status.status = USB_EHCI_QTD_STATUS_HALTED;
        qh->qh_self |= USB_EHCI_LINKTYPE_QH;
    }

    /*
     * STEP 7.1b: Link the queue heads together that they form the 2ms
     *            polling interval
     */
    uint16_t bit = USB_EHCI_VFRAMELIST_COUNT / 2;
    uint32_t curr, next;

    while (bit) {
        curr = bit;
        while (curr & bit) {
            usb_ehci_qh_t *qh_curr;
            usb_ehci_qh_t *qh_next;

            next = (curr ^ bit) | (bit / 2);

            qh_curr = hc->qh_intr_last[curr];
            qh_next = hc->qh_intr_last[next];

            qh_curr->qh_link = qh_next->qh_self;

            curr++;
        }
        bit >>= 1;
    }

    qh = hc->qh_intr_last[0];
    qh->qh_link = USB_EHCI_LINK_TERMINATE;

    /*
     * STEP 7.2: Initialize the isochronus split transaction queues together
     *           with the isochronus transaction queues
     */

    usb_ehci_sitd_t *sitd;
    usb_ehci_itd_t *itd;

    for (uint32_t i = 0; i < USB_EHCI_VFRAMELIST_COUNT; i++) {

        sitd = usb_ehci_sitd_alloc();
        hc->qh_sitd_fs_last[i] = sitd;

        sitd->sitd_self |= USB_EHCI_LINKTYPE_SITD;

        sitd->sitd_back_link = USB_EHCI_LINK_TERMINATE;

        next = i | (USB_EHCI_VFRAMELIST_COUNT / 2);
        sitd->sitd_next = hc->qh_intr_last[next]->qh_self;

        itd = usb_ehci_itd_alloc();
        hc->qh_itd_hs_last[i] = itd;

        itd->itd_self |= USB_EHCI_LINKTYPE_ITD;

        itd->itd_next = sitd->sitd_self;
    }

    /*
     * STEP 8: Initialize the physical frames for the periodic transfers
     *         and store the periodic frame list iTDs addresses
     */
    if (hc->pframes == NULL) {
        usb_ehci_pframes_alloc(hc);
    }

    assert(hc->pframes);

    usb_paddr_t *pframes = (hc->pframes);

    for (uint32_t i = 0; i < USB_EHCI_FRAMELIST_COUNT; i++) {
        next = i & (USB_EHCI_VFRAMELIST_COUNT - 1);
        (pframes[i]) = hc->qh_itd_hs_last[next]->itd_self;
    }

    /*
     * STEP 9: Initialize the asynchronous transfer queue
     */
    qh = usb_ehci_qh_alloc();

    hc->qh_async_last = qh;
    hc->qh_async_first = qh;

    qh->qh_ep.ep_speed = USB_EHCI_QH_SPEED_HIGH;
    qh->qh_ep.head_reclamation = 1;
    qh->qh_ep.mult = 1;
    qh->qh_link = qh->qh_self | USB_EHCI_LINKTYPE_QH | USB_EHCI_LINK_TERMINATE;
    qh->qh_alt_next_qtd = USB_EHCI_LINK_TERMINATE;
    qh->qh_next_qtd = USB_EHCI_LINK_TERMINATE;
    qh->qh_status.status = USB_EHCI_QTD_STATUS_HALTED;

    /*
     * STEP 10: Allocate the root hub device
     */
    struct usb_device *root_hub = usb_device_alloc(hc->controller, NULL, 0, 0,
            1, USB_SPEED_HIGH, USB_MODE_HOST);

    if (root_hub) {
        usb_hub_init(root_hub);
        hc->rh_device = root_hub;
        hc->controller->root_hub = root_hub;
    } else {
        USER_PANIC("ERROR: No root hub present!");
    }

    hc->devices[USB_ROOTHUB_ADDRESS] = root_hub;

    /*
     * STEP 11: Call the register initialization function
     */
    err = usb_ehci_initialize_controller(hc);

    /*
     * STEP 12: do an initial poll, since the interrupts may be risen before
     *          we started the interrupt handler
     */
    if (err == USB_ERR_OK) {
        usb_ehci_poll(hc);
    }

    return (err);
}
