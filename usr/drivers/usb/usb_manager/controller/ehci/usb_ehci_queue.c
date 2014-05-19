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
#include <barrelfish/barrelfish.h>

#include <usb/usb.h>


#include <usb_controller.h>
#include <usb_xfer.h>

#include "usb_ehci.h"
#include "usb_ehci_xfer.h"
#include "usb_ehci_queue.h"


/**
 * \brief this function enqueues a transfer on the interrupt queue
 *        transfers on this queue are to be scheduled by the host controller
 */
void usb_ehci_enqueue_xfer_intrq(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    /*
     * check if the transfer is already finished
     */
    if (usb_ehci_xfer_is_finished(xfer)) {
        USB_DEBUG("NOTICE: transfer already finished...\n");
        return;
    }

    /*
     * enqueue it on the transfer interrupt queue
     */
    usb_xfer_enqueue(&xfer->host_controller->intr_queue, xfer);

    /*
     * TODO: start timeout handler, if the transfer takes too long to be executed
     *       then it should be removed with a timeout condition
     *
     * if (xfer->timeout != 0) {
     *
     * }
     */
}

/**
 * \brief enqueues a new split isochronus transaction descriptor into the queue
 *
 * \param sitd the siTD to insert into the list
 * \param last the last element of the list
 */
usb_ehci_sitd_t *usb_ehci_enq_fs_td(usb_ehci_sitd_t *sitd,
        usb_ehci_sitd_t *last)
{
    /*
     * update the virtual links
     */
    sitd->next = last->next;
    sitd->prev = last;
    last->next = sitd;

    /*
     * update the physical links
     */
    sitd->sitd_next = last->sitd_next;
    last->sitd_next = sitd->sitd_self;

    return (sitd);
}

/**
 * \brief removes the element from the list
 *
 * \param sitd the siTD element to be removed
 * \param last the last element of the list
 */
usb_ehci_sitd_t *usb_ehci_deq_fs_td(usb_ehci_sitd_t *sitd,
        usb_ehci_sitd_t *last)
{
    /*
     * update virtual pointers
     */
    sitd->prev->next = sitd->next;

    /*
     * update the physical links
     */
    sitd->prev->sitd_next = sitd->sitd_next;

    if (sitd->next) {
        sitd->next->prev = sitd->prev;
    }

    return ((last == sitd) ? sitd->prev : last);
}

/*
 * \brief enqueues a new isochronus transaction descriptor into the queue
 *
 * \param std   the iTD to insert into the list
 * \param last  the last element of the list
 */
usb_ehci_itd_t *usb_ehci_enq_hs_td(usb_ehci_itd_t *std, usb_ehci_itd_t *last)
{
    /*
     * update the virtual links
     */
    std->next = last->next;
    std->prev = last;
    last->next = std;

    /*
     * update the physical links
     */
    std->itd_next = last->itd_next;
    last->itd_next = std->itd_self;

    return (std);

}

/**
 * \brief removes a high speed isochronus transfer descriptor from the list
 *
 * \param std   the iTD to be removed
 * \param last  the last element of the list
 */
usb_ehci_itd_t *usb_ehci_deq_hs_td(usb_ehci_itd_t *std, usb_ehci_itd_t *last)
{
    /*
     * update virtual pointers
     */
    std->prev->next = std->next;

    /*
     * update the physical links
     */
    std->prev->itd_next = std->itd_next;

    if (std->next) {
        std->next->prev = std->prev;
    }

    return ((last == std) ? std->prev : last);
}

/**
 * \brief enqueues a queue head descriptor into the qh list
 *
 * \param qh    the queue head descriptor to insert
 * \param last  the last element fo the list
 */
usb_ehci_qh_t *usb_ehci_enq_qh(usb_ehci_qh_t *qh, usb_ehci_qh_t *last)
{
    USB_DEBUG_TR_ENTER;

    /*
     * a queue head can only be linked once
     */
    if (qh->prev != NULL) {
        USB_DEBUG("NOTICE: Tried to double enqueue a queue head\n");
        return (last);
    }

    qh->next = last->next;
    qh->qh_link = last->qh_link;

    qh->prev = last;

    last->next = qh;
    last->qh_link = qh->qh_self | USB_EHCI_LINKTYPE_QH;

    return (qh);
}

/**
 * \brief removes a queue head from the qh list
 *
 * \param qh    the queue head to be removed
 * \param last  the last element of the list
 */
usb_ehci_qh_t *usb_ehci_deq_qh(usb_ehci_qh_t *qh, usb_ehci_qh_t *last)
{
    /*
     * check if the queue head is in fact on a list
     */
    if (qh->prev == NULL) {
        return (last);
    }

    /*
     * update virtual pointers
     */
    qh->prev->next = qh->next;

    /*
     * update physical pointer
     */
    qh->prev->qh_link = qh->qh_link;

    if (qh->next) {
        qh->next->prev = qh->prev;
    }

    last = ((last == qh) ? qh->prev : last);

    qh->prev = NULL;

    return (last);

}
