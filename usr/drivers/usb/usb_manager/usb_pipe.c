/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include <usb/usb.h>
#include <usb/usb_descriptor.h>

#include <usb_controller.h>

#include <usb_xfer.h>
#include <usb_pipe.h>


/**
 * \brief this function handles the start of a new transfer when it is on the
 *        endpoint queue
 */
void usb_pipe_start(struct usb_xfer_queue *queue)
{
    USB_DEBUG_TR_ENTER;

    /* get the xfer from the queue */
    struct usb_xfer *xfer = queue->current;

    assert(xfer != NULL);

    struct usb_endpoint *ep = xfer->endpoint;

    uint8_t type;

    if (ep->is_stalled) {
        USB_DEBUG_XFER("NOTICE: endpoint is already stalled...\n");

        USB_DEBUG_TR_RETURN;
        return;
    }

    if (xfer->flags.pipe_stalled) {
        USB_DEBUG("NOTICE: Stalling pipe...\n");
        /* TODO: Stall pipe */
        assert(!"NYI: Stall pipe");
    }

    if (xfer->num_frames == 0) {
        USB_DEBUG_XFER("NOTICE: No frames to process... finishing transfer");
        xfer->actual_frames = 0;
        usb_xfer_done(xfer, USB_ERR_OK);
        return;
    }

    if (xfer->interval > 0) {
        type = (ep->descriptor->bmAttributes.xfer_type);
        switch (type) {
            case USB_ENDPOINT_TYPE_BULK:
            case USB_ENDPOINT_TYPE_CONTROL:
               /* TODO: Delay the transfer start... */
                assert(!"NYI: delayed start");
                return;
                break;
            default:
                /* noop */
                break;
        }
    }

    if (xfer->error == USB_ERR_OK) {
        xfer->flags_internal.notify = 1;
        /* call the start function */
        ep->pipe_fn->start(xfer);
    }

    xfer->flags_internal.cancellable = 1;

    if (xfer->error != USB_ERR_OK) {
        /* there was an error while starting, cancel the transfer */
        usb_xfer_done(xfer, 0);
    }

}

/**
 * \brief this function handles the event when a new transfer enters the system
 *
 */
void usb_pipe_enter(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    assert(xfer != NULL);

    struct usb_endpoint *ep = xfer->endpoint;

    /* call the enter function of the pipe */
    (ep->pipe_fn->enter)(xfer);

    xfer->flags_internal.cancellable = 1;

    if (xfer->error != USB_ERR_OK) {
        USB_DEBUG("ERROR: xfer returned with error...");
        usb_xfer_done(xfer, 0);
        USB_DEBUG_TR_RETURN;
        return;
    }

    if (ep->transfers.current != xfer) {
        /* there is already a transfer happening, so enqueue it on the endpoint */
        usb_xfer_enqueue(&ep->transfers, xfer);

        if (ep->transfers.current != NULL) {
            USB_DEBUG_XFER("Some thing is already processing...\n");

            USB_DEBUG_TR_RETURN;
            return;
        }
    }

    if (!ep->transfers.recurse_1) {
        ep->transfers.recurse_1 = 1;
        if (ep->transfers.current == NULL) {
            xfer = ep->transfers.head.first;

            if (xfer) {
                if (xfer->wait_entry.next != NULL) {
                    xfer->wait_entry.next->wait_entry.prev_next = xfer
                            ->wait_entry.prev_next;
                    *(xfer->wait_entry.prev_next) = xfer->wait_entry.next;
                } else {
                    (&ep->transfers.head)->last_next = &(&ep->transfers.head)->first;
                    (&ep->transfers.head)->first = NULL;
                            //xfer->wait_entry.prev_next;
                    xfer->wait_entry.prev_next = &xfer->wait_entry.next;
                }
                xfer->wait_queue = NULL;
                /* set the current xfer to be handled in the queue */
                ep->transfers.current = xfer;
                USB_DEBUG_XFER("ep->transfers.command\n");
                /* execute the start command on the new xfer on the endpoint */
                (ep->transfers.command)(&ep->transfers);
            }
            ep->transfers.recurse_1 = 0;
        }
    }

    USB_DEBUG_TR_RETURN;
}
