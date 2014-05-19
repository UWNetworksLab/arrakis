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


#include <usb_device.h>
#include <usb_controller.h>
#include <usb_hub.h>
#include <usb_xfer.h>

#include "usb_ehci.h"
#include "usb_ehci_pipe.h"
#include "usb_ehci_queue.h"
#include "usb_ehci_xfer.h"



/*
 * -------------------------------------------------------------------------
 * EHCI bulk transfer support
 * -------------------------------------------------------------------------
 */

/**
 * \brief handles the pipe open event of a new bulk transfer
 */
static void usb_ehci_xfer_bulk_open(struct usb_xfer *xfer)
{
    return; /* no-op */
}


/**
 * \brief handles cancel event of an bulk xfer. this removes the
 *        transfer from the queues with the cancelled state
 */
static void usb_ehci_xfer_bulk_close(struct usb_xfer *xfer)
{
    usb_ehci_xfer_remove(xfer, USB_ERR_CANCELLED);
}


/**
 *  \brief handles the enter event of a new bulk xfer,
 *         there is nothing to do, thus this is a no-op.
 */
static void usb_ehci_xfer_bulk_enter(struct usb_xfer *xfer)
{
    return; /* no-op */
}

/**
 * \brief handles the start event of a new bulk xfer
 */
static void usb_ehci_xfer_bulk_start(struct usb_xfer *xfer)
{
    // get the host controller of this transfer
    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) (xfer->host_controller->hc_control);

    /* setup the standard transfer descriptors */
    usb_ehci_xfer_standard_setup(xfer, &(hc->qh_async_last));

    /* enqueue on the xfer interrupt queue*/
    usb_ehci_enqueue_xfer_intrq(xfer);
}

/// stores the function pointers to the bulk pipe functions
static struct usb_hcdi_pipe_fn usb_ehci_bulk_pipe_fn = {
        .open = usb_ehci_xfer_bulk_open,
        .close = usb_ehci_xfer_bulk_close,
        .enter = usb_ehci_xfer_bulk_enter,
        .start = usb_ehci_xfer_bulk_start
};

/**
 * \brief gets the function pointers for bulk pipe functions
 */
struct usb_hcdi_pipe_fn *usb_ehci_get_bulk_pipe_fn(void)
{
    return (&usb_ehci_bulk_pipe_fn);
}


/*
 * -------------------------------------------------------------------------
 * EHCI control transfer support
 * -------------------------------------------------------------------------
 */

/**
 * \brief opens a new control transfer pipe, there is nothing to be done
 *        here, thus this is a no-op
 */
static void usb_ehci_xfer_ctrl_open(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    return; /* no-op */
}

/**
 * \brief closes a control transfer pipe and canceles outstanding requests
 */
static void usb_ehci_xfer_ctrl_close(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    usb_ehci_xfer_remove(xfer, USB_ERR_CANCELLED);
}

/**
 * \brief handles the control transfer enter event, there is nothing to be
 *        done here
 */
static void usb_ehci_xfer_ctrl_enter(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    return; /* no-op */
}

/**
 * \brief handles the start event of a new control xfer
 */
static void usb_ehci_xfer_ctrl_start(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    // get the host controller of this transfer
    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) (xfer->host_controller->hc_control);

    assert(xfer->error == USB_ERR_OK);

    /* setup the standard transfer descriptors */
    usb_ehci_xfer_standard_setup(xfer, &(hc->qh_async_last));

    assert(xfer->error == USB_ERR_OK);

    /* enqueue on the xfer interrupt queue*/
    usb_ehci_enqueue_xfer_intrq(xfer);

    USB_DEBUG_TR_RETURN;
}


static struct usb_hcdi_pipe_fn usb_ehci_ctrl_pipe_fn = {
        .open = usb_ehci_xfer_ctrl_open,
        .close = usb_ehci_xfer_ctrl_close,
        .enter = usb_ehci_xfer_ctrl_enter,
        .start = usb_ehci_xfer_ctrl_start
};

/**
 * \brief returns the function pointers of the control pipe type
 */
struct usb_hcdi_pipe_fn *usb_ehci_get_ctrl_pipe_fn(void)
{
    return (&usb_ehci_ctrl_pipe_fn);
}


/*
 * -------------------------------------------------------------------------
 * EHCI interrupt transfer type support
 * -------------------------------------------------------------------------
 */

/**
 * \brief handles the opening event of an interrupt transfer
 */
static void usb_ehci_xfer_intr_open(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    // get the host controller of this transfer
    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;

    usb_hub_bandwidth_alloc(xfer);

    /*
     * find the best qh position for the given interrupt interval
     */

    uint16_t interval = USB_EHCI_VFRAMELIST_COUNT / 2;
    uint16_t match = 0;
    uint16_t tmp = 0;

    while(interval) {
        if (xfer->interval >= interval) {
            tmp = interval;
            match = interval;
            while (tmp & interval) {
                if (hc->qh_intr_stat[tmp] < hc->qh_intr_stat[match]) {
                    match = tmp;
                }
                tmp++;
            }
            break;
        }
        interval >>= 1;
    }

    hc->qh_intr_stat[match]++;

    xfer->intr_qh_pos = match;

}

/**
 * \brief handles the closing of an interrupt pipe
 */
static void usb_ehci_xfer_intr_close(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    // get the host controller of this transfer
    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;

    // decrease the number of interrupt transfers
    hc->qh_intr_stat[xfer->intr_qh_pos]--;

    // remove the bandwidth
    usb_ehci_xfer_remove(xfer, USB_ERR_CANCELLED);

    /* we have allocated the bandwidth, so we have to free it again */
    usb_hub_bandwidth_free(xfer);
}

/**
 * \brief handles the enter event of an interupt transfer, this is a noop
 */
static void usb_ehci_xfer_intr_enter(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    return; /* no-op */
}

static void usb_ehci_xfer_intr_start(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    assert(xfer != NULL);

   // get the host controller of this transfer
   usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;

   /* setup the transfer descriptors */
   usb_ehci_xfer_standard_setup(xfer, &hc->qh_intr_last[xfer->intr_qh_pos]);

   usb_ehci_enqueue_xfer_intrq(xfer);
}

/// function pointers to the interrupt pointers
static struct usb_hcdi_pipe_fn usb_ehci_intr_pipe_fn = {
        .open = usb_ehci_xfer_intr_open,
        .close = usb_ehci_xfer_intr_close,
        .enter = usb_ehci_xfer_intr_enter,
        .start = usb_ehci_xfer_intr_start
};

/**
 * \brief gets the function pointers of the intr type transfer functions
 */
struct usb_hcdi_pipe_fn *usb_ehci_get_intr_pipe_fn(void)
{
    return (&usb_ehci_intr_pipe_fn);
}


/*
 * -------------------------------------------------------------------------
 * EHCI high speed isochronus transfer support
 * -------------------------------------------------------------------------
 */

/**
 * \brief handles the open event for an high speed isochronus transfer
 */
static void usb_ehci_xfer_hs_isoc_open(struct usb_xfer *xfer)
{
    // get the host controller of this transfer
    //usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;
    assert(!"NYI");
}

/**
 * \brief handles the close event for an high speed isochronus transfer
 */
static void usb_ehci_xfer_hs_isoc_close(struct usb_xfer *xfer)
{
    usb_ehci_xfer_remove(xfer, USB_ERR_CANCELLED);
}

/**
 * \brief handles the enter event for a high speed isochronus transfer
 */
static void usb_ehci_xfer_hs_isoc_enter(struct usb_xfer *xfer)
{
    // get the host controller of this transfer
    //usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;
    assert(!"NYI");
}

/**
 * \brief handles the start event for a high speed isochronus transfer
 */
static void usb_ehci_xfer_hs_isoc_start(struct usb_xfer *xfer)
{
    // get the host controller of this transfer
    //usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;
    assert(!"NYI");
}


/// function pointers to the interrupt pointers
static struct usb_hcdi_pipe_fn usb_ehci_hs_isoc_pipe_fn = {
        .open = usb_ehci_xfer_hs_isoc_open,
        .close = usb_ehci_xfer_hs_isoc_close,
        .enter = usb_ehci_xfer_hs_isoc_enter,
        .start = usb_ehci_xfer_hs_isoc_start
};

/**
 * \brief gets the function pointers of the intr type transfer functions
 */
struct usb_hcdi_pipe_fn *usb_ehci_get_hs_isoc_pipe_fn(void)
{
    return (&usb_ehci_hs_isoc_pipe_fn);
}


/*
 * -------------------------------------------------------------------------
 * EHCI full speed isochronus transfer support
 * -------------------------------------------------------------------------
 */

/**
 * \brief handles the open event for an full speed isochronus transfer
 */
static void usb_ehci_xfer_fs_isoc_open(struct usb_xfer *xfer)
{
    // get the host controller of this transfer
    //usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;

    assert(!"NYI");
}

/**
 * \brief handles the close event for an full speed isochronus transfer
 */
static void usb_ehci_xfer_fs_isoc_close(struct usb_xfer *xfer)
{
    usb_ehci_xfer_remove(xfer, USB_ERR_CANCELLED);
}

/**
 * \brief handles the enter event for a full speed isochronus transfer
 */
static void usb_ehci_xfer_fs_isoc_enter(struct usb_xfer *xfer)
{
    // get the host controller of this transfer
    //usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;

    assert(!"NYI");
}

/**
 * \brief handles the start event for a full speed isochronus transfer
 */
static void usb_ehci_xfer_fs_isoc_start(struct usb_xfer *xfer)
{
    // get the host controller of this transfer
    //usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;
    assert(!"NYI");
}


/// function pointers to the interrupt pointers
static struct usb_hcdi_pipe_fn usb_ehci_fs_isoc_pipe_fn = {
        .open = usb_ehci_xfer_fs_isoc_open,
        .close = usb_ehci_xfer_fs_isoc_close,
        .enter = usb_ehci_xfer_fs_isoc_enter,
        .start = usb_ehci_xfer_fs_isoc_start
};

/**
 * \brief gets the function pointers of the full speed isochronus transfer
 */
struct usb_hcdi_pipe_fn *usb_ehci_get_fs_isoc_pipe_fn(void)
{
    return (&usb_ehci_fs_isoc_pipe_fn);
}
