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

#include <usb_xfer.h>
#include <usb_device.h>
#include <usb_controller.h>
#include <usb_hub.h>
#include "usb_ohci.h"
#include "usb_ohci_pipe.h"

#include "usb_ohci_xfer.h"

/*
 * ------------------------------------------------------------------------
 * OHCI General Pipe Function
 * ------------------------------------------------------------------------
 */

/*
 * ------------------------------------------------------------------------
 * OHCI Bulk Pipe Functions
 * ------------------------------------------------------------------------
 * These functions provide OHCI specific implementations for bulk type
 * pipes.
 */
static void
usb_ohci_xfer_bulk_open(struct usb_xfer *xfer);
static void
usb_ohci_xfer_bulk_close(struct usb_xfer *xfer);
static void
usb_ohci_xfer_bulk_enter(struct usb_xfer *xfer);
static void
usb_ohci_xfer_bulk_start(struct usb_xfer *xfer);

// data structure containing the function pointers
struct usb_hcdi_pipe_fn usb_ohci_xfer_bulk_fun = {
.open = usb_ohci_xfer_bulk_open, .close = usb_ohci_xfer_bulk_close, .enter =
        usb_ohci_xfer_bulk_enter, .start = usb_ohci_xfer_bulk_start
};

/**
 * \brief Function to open a bulk pipe. Bulk types do not need any
 *        processing so this is a noop.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_bulk_open(struct usb_xfer *xfer)
{
    return; /* noop */
}

/**
 * \brief Function to close a bulk pipe. Calling this function
 *        means to cancel all outstanding requests for this pipe.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_bulk_close(struct usb_xfer *xfer)
{
    usb_ohci_xfer_remove(xfer, USB_ERR_CANCELLED);
}

/**
 * \brief Function to enter a bulk pipe. Bulk type pipes do not need
 *        to be entered thus this is a noop.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_bulk_enter(struct usb_xfer *xfer)
{
    return; /* noop */
}

/**
 * \brief Function to start a bulk pipe. This function sets up the
 queues for the requests
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_bulk_start(struct usb_xfer *xfer)
{
    // get the host controller of this transfer
    usb_ohci_hc_t *hc = (usb_ohci_hc_t *) xfer->host_controller->hc_control;

    // setup the transfer descriptors and queue heads
    usb_ohci_xfer_start(xfer, &hc->qh_bulk_last);

    // enqueue it on the interrupt queue
    usb_ohci_xfer_enqueue(xfer);
}

/*
 * ------------------------------------------------------------------------
 * OHCI Control Pipe Functions
 * ------------------------------------------------------------------------
 * These functions provide OHCI specific implementations for control type
 * pipes.
 */
static void
usb_ohci_xfer_ctrl_open(struct usb_xfer *xfer);
static void
usb_ohci_xfer_ctrl_close(struct usb_xfer *xfer);
static void
usb_ohci_xfer_ctrl_enter(struct usb_xfer *xfer);
static void
usb_ohci_xfer_ctrl_start(struct usb_xfer *xfer);

// data structure containing the function pointers
struct usb_hcdi_pipe_fn usb_ohci_xfer_ctrl_fun = {
.open = usb_ohci_xfer_ctrl_open, .close = usb_ohci_xfer_ctrl_close, .enter =
        usb_ohci_xfer_ctrl_enter, .start = usb_ohci_xfer_ctrl_start
};

/**
 * \brief Function to open a control pipe. Control pipes do not need any
 *        processing so this is a noop.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_ctrl_open(struct usb_xfer *xfer)
{
    return; /* just a noop */
}

/**
 * \brief Function to close a control pipe. Calling this function
 *        means to cancel all outstanding requests for this pipe.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_ctrl_close(struct usb_xfer *xfer)
{
    usb_ohci_xfer_remove(xfer, USB_ERR_CANCELLED);
}

/**
 * \brief Function to enter a control pipe. Control pipes do not need
 *        to be entered thus this is a noop.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_ctrl_enter(struct usb_xfer *xfer)
{
    return; /* noop */
}

/**
 * \brief Function to start a control pipe. This function sets up the
 queues for the requests
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_ctrl_start(struct usb_xfer *xfer)
{
    // get the host controller
    usb_ohci_hc_t *hc = (usb_ohci_hc_t *) xfer->host_controller->hc_control;

    // setup the queue heads and transfer descriptors
    usb_ohci_xfer_start(xfer, &hc->qh_ctrl_last);

    // enqueue it on the interrupt queue
    usb_ohci_xfer_enqueue(xfer);
}

/*
 * ------------------------------------------------------------------------
 * OHCI Interrupt Pipe Functions
 * ------------------------------------------------------------------------
 * These functions provide OHCI specific implementations for interrupt type
 * pipes.
 */
static void
usb_ohci_xfer_intr_open(struct usb_xfer *xfer);
static void
usb_ohci_xfer_intr_close(struct usb_xfer *xfer);
static void
usb_ohci_xfer_intr_enter(struct usb_xfer *xfer);
static void
usb_ohci_xfer_intr_start(struct usb_xfer *xfer);

// data structure containing the function pointers
struct usb_hcdi_pipe_fn usb_ohci_xfer_intr_fun = {
.open = usb_ohci_xfer_intr_open, .close = usb_ohci_xfer_intr_close, .enter =
        usb_ohci_xfer_intr_enter, .start = usb_ohci_xfer_intr_start
};

/**
 * \brief Function to open a interrupt pipe. Requests on interrupt
 *        pipes need to be paced on the correct position in the
 *        interrupt tree. This function places the transfer request
 *        in the positions indicated by the xfer->interval field.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_intr_open(struct usb_xfer *xfer)
{
    usb_ohci_hc_t *hc = (usb_ohci_hc_t *) xfer->host_controller->hc_control;

    uint16_t best;
    uint16_t bit;
    uint16_t x;

    best = 0;
    /*
     * we have USB_OHCI_NO_EP_DESCRIPTORS total endpoint descriptors
     * in the interrupt transfer lists. This consist of half
     * interrupt endpoints and half isochronus endpoints
     */
    bit = USB_OHCI_NO_EP_DESCRIPTORS / 2;

    /*
     * loop over the possible interrupt intervals and find the best
     * bucket to place it. i.e. where the least transfers are
     */
    while (bit) {
        if (xfer->interval >= bit) {
            x = bit;
            best = bit;
            while (x & bit) {
                if (hc->intr_stats[x] < hc->intr_stats[best]) {
                    best = x;
                }
                x++;
            }
            break;
        }
        bit >>= 1;
    }

    // we are going to add the transfer into this interval
    hc->intr_stats[best]++;
    xfer->intr_qh_pos = best;
}

/**
 * \brief Function to close a interrupt pipe. Calling this function
 *        means to cancel all outstanding requests for this pipe.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_intr_close(struct usb_xfer *xfer)
{
    // get the host controller
    usb_ohci_hc_t *hc = (usb_ohci_hc_t *) xfer->host_controller->hc_control;

    // update the usage statistics for the interval type
    hc->intr_stats[xfer->intr_qh_pos]--;

    // remove the transfer
    usb_ohci_xfer_remove(xfer, USB_ERR_CANCELLED);
}

/**
 * \brief Function to enter a interrupt pipe. Interrupt pipes do not need
 *        to be entered thus this is a noop.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_intr_enter(struct usb_xfer *xfer)
{
    return; /* noop */
}

/**
 * \brief Function to start a interrupt pipe. This function sets up the
 queues for the requests
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_intr_start(struct usb_xfer *xfer)
{
    // get the host controller
    usb_ohci_hc_t *hc = (usb_ohci_hc_t *) xfer->host_controller->hc_control;

    // setup the queue heads and the transfer descriptors
    usb_ohci_xfer_start(xfer, &hc->qh_intr_last[xfer->intr_qh_pos]);

    // enqueue it on the interrupt queue
    usb_ohci_xfer_enqueue(xfer);
}

/*
 * ------------------------------------------------------------------------
 * OHCI Isochronus Pipe Functions
 * ------------------------------------------------------------------------
 * These functions provide OHCI specific implementations for Isochronus type
 * pipes.
 */
static void
usb_ohci_xfer_isoc_open(struct usb_xfer *xfer);
static void
usb_ohci_xfer_isoc_close(struct usb_xfer *xfer);
static void
usb_ohci_xfer_isoc_enter(struct usb_xfer *xfer);
static void
usb_ohci_xfer_isoc_start(struct usb_xfer *xfer);

// data structure containing the function pointers
struct usb_hcdi_pipe_fn usb_ohci_xfer_isoc_fun = {
.open = usb_ohci_xfer_isoc_open, .close = usb_ohci_xfer_isoc_close, .enter =
        usb_ohci_xfer_isoc_enter, .start = usb_ohci_xfer_isoc_start
};

/**
 * \brief Function to open a isochronus pipe. There is no special
 *        handling for opening a isochronus pipe.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_isoc_open(struct usb_xfer *xfer)
{
    return; /* noop */
}

/**
 * \brief Function to close a isochronus pipe. Calling this function
 *        means to cancel all outstanding requests for this pipe.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_isoc_close(struct usb_xfer *xfer)
{
    usb_ohci_xfer_remove(xfer, USB_ERR_CANCELLED);
}

/**
 * \brief Function to enter a isochronus pipe. Here we have to place
 *        isochronus request on the correct position after the
 *        last interrupt request on the interrupt tree.
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_isoc_enter(struct usb_xfer *xfer)
{
    // TODO: Implement
    assert(!"NYI: cannot create isochronus transfers at this time");
}

/**
 * \brief Function to start a interrupt pipe. This function sets up the
 queues for the requests
 *
 * \param xfer  usb transfer request
 */
static void usb_ohci_xfer_isoc_start(struct usb_xfer *xfer)
{
    // put the transfer on the interrupt queue
    usb_ohci_xfer_enqueue(xfer);
}

/*
 * Exported functions
 */

struct usb_hcdi_pipe_fn *usb_ohci_get_bulk_pipe_fn(void)
{
    return &usb_ohci_xfer_bulk_fun;
}

struct usb_hcdi_pipe_fn *usb_ohci_get_ctrl_pipe_fn(void)
{
    return &usb_ohci_xfer_ctrl_fun;
}

struct usb_hcdi_pipe_fn *usb_ohci_get_isoc_pipe_fn(void)
{
    return &usb_ohci_xfer_isoc_fun;
}

struct usb_hcdi_pipe_fn *usb_ohci_get_intr_pipe_fn(void)
{
    return &usb_ohci_xfer_intr_fun;
}
