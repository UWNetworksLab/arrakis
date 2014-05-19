/**
 * \brief this file contains functions for handling the different transfers
 *        as well as handling different flounder requests
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

#include <if/usb_driver_defs.h>

#include <usb/usb_error.h>
#include <usb/usb_request.h>

#include <usb_controller.h>
#include <usb_transfer.h>
#include <usb_device.h>
#include <usb_request.h>
#include <usb_xfer.h>
#include <usb_endpoint.h>
#include <usb_pipe.h>
#include <usb_memory.h>

/**
 * this struct contains the default setup information for the control transfers
 */
static const struct usb_xfer_config usb_control_ep_cfg[USB_DEVICE_CTRL_XFER_MAX] =
        {
            /*
             * setup information for the default control endpoint
             * This endpoint is used for device requests
             *
             */
            [0] = {
                .type = USB_TYPE_CTRL,
                .endpoint = USB_ENDPOINT_CONTROL,
                .direction = USB_ENDPOINT_DIRECTION_ANY,
                .bufsize = 1024,
                .flags = {
                    .ext_buffer = 1,
                },
                .usb_mode = USB_MODE_DUAL,
                .usb_type = USB_TYPE_CTRL,
            },

            /*
             * this is the setup information for the clear stall requests
             * TODO: clearing up stalls is NYI.
             */

            [1] = {
                .type = USB_TYPE_CTRL,
                .endpoint = USB_ENDPOINT_CONTROL, /* Control pipe */
                .direction = USB_ENDPOINT_DIRECTION_ANY,
                .bufsize = sizeof(struct usb_device_request),
                .timeout = 1000, /* 1 second */
                .interval = 50, /* 50ms */
                .usb_mode = USB_MODE_HOST,
                .usb_type = USB_TYPE_CTRL
            },
        };

/*
 * --------------------------------------------------------------------------
 * transfer done notification
 */

/// struct containing the transfer done state
struct usb_tdone_state {
    struct usb_driver_binding *bind;
    struct usb_xfer *xfer;
    void *buf;
};

/**
 * \brief transfer complete callback that frees up the state
 */
static void usb_transfer_complete_cb(void *a)
{
    free(a);
}

/**
 * \brief handles the transmission of the transfer done notification
 *
 * \param a pointer to the transfer done state
 */
static void usb_transfer_complete_tx(void *a)
{
    struct usb_tdone_state *st = a;
    errval_t err;

    struct event_closure txcont = MKCONT(usb_transfer_complete_cb, st);

    err = usb_driver_transfer_done_notify__tx(st->bind, txcont,
            st->xfer->xfer_id, st->xfer->error, st->buf,
            st->xfer->actual_bytes);

    USB_TX_TRANSER_ERR(usb_transfer_complete_tx);
}

/**
 * \brief callback function that initiates the sending the transfer done msg
 *
 * \param xfer the transfer that was done
 * \param err the outcome of the transfer
 *
 * NOTE: this function sends xfer done notification to the device driver
 *       interrupt transfers may automatically be restarted
 */
static void usb_transfer_complete_notify(struct usb_xfer *xfer, usb_error_t err)
{
    uint32_t data_length = xfer->actual_bytes;

    xfer->error = err;

    switch (xfer->type) {
        case USB_TYPE_INTR:
            xfer->frame_lengths[0] = xfer->max_data_length;
            if (data_length > 0) {
                struct usb_tdone_state *st = malloc(
                        sizeof(struct usb_tdone_state));
                st->xfer = xfer;
                st->buf = xfer->frame_buffers[0]->buffer;
                st->bind = xfer->usb_driver_binding;
                usb_transfer_complete_tx(st);

            }
            /* autorestart if the transfer completed successfully */
            if (xfer->flags.auto_restart && (err == USB_ERR_OK)) {
                usb_transfer_start(xfer);
            }
            break;
        default:
            /* noop */
            break;
    }
}

/*
 * transfer done notification
 * --------------------------------------------------------------------------
 */

/**
 * \brief handles the start of a control transfer
 */
static uint8_t usb_transfer_ctrl_start(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    uint16_t length;

    /* check if the control transfer ended up in a stalled state while active */
    if (xfer->flags.pipe_stalled && xfer->flags_internal.ctrl_active) {
        xfer->flags_internal.ctrl_stall = 1;
        xfer->flags_internal.ctrl_active = 0;
    } else {
        xfer->flags_internal.ctrl_stall = 1;
    }

    if (xfer->num_frames > 2) {
        USB_DEBUG("ERROR: too many frames %u", xfer->num_frames);
        USB_DEBUG_TR_RETURN;
        return (1);
    }

    /* if the ctrl transfer is already active the header was sent, so reset
     * the control header flag
     */
    if (xfer->flags_internal.ctrl_active) {
        if (xfer->flags_internal.ctrl_header) {
            xfer->flags_internal.ctrl_header = 0;
        }
        length = xfer->sum_bytes;
    } else {
        if (xfer->frame_lengths[0] != sizeof(struct usb_device_request)) {
            /*
             * the first frame must be of size device request, otherwise
             * this is an error
             */
            USB_DEBUG("ERROR: wrong frame length: %u / %u",
                   xfer->frame_lengths[0], sizeof(struct usb_device_request));
            USB_DEBUG_TR_RETURN;
            return (1);
        }

        /* get the device request to have access to the length information */
        struct usb_device_request req;

        usb_mem_copy_out(xfer->frame_buffers[0], 0, &req, sizeof(req));

        xfer->flags_internal.remaining_bytes = req.wLength;

        xfer->flags_internal.ctrl_header = 1;

        length = xfer->sum_bytes - sizeof(struct usb_device_request);
    }

    if (length > xfer->flags_internal.remaining_bytes) {
        USB_DEBUG("ERROR: length (%u) remaining length is greater than "
        "remaining length (%u)", length, xfer->flags_internal.remaining_bytes);
        USB_DEBUG_TR_RETURN;
        return (1);
    }

    /* check for short transfers i.e. too less data than expected */
    if (xfer->flags.short_xfer_forced) {
        xfer->flags_internal.remaining_bytes = 0;
    } else {
        if ((length != xfer->max_data_length)
                && (length != xfer->flags_internal.remaining_bytes)
                && (xfer->num_frames != 1)) {
            USB_DEBUG("ERROR: short xfer w/o force_short_xfer\n");
            USB_DEBUG_TR_RETURN;
            return (1);
        }
        xfer->flags_internal.remaining_bytes -= length;
    }

    if ((xfer->flags_internal.remaining_bytes > 0)
            || xfer->flags.manual_status) {
        xfer->flags_internal.ctrl_active = 1;

        if ((!xfer->flags_internal.ctrl_header) && (xfer->num_frames == 1)) {
            USB_DEBUG("ERROR: invalid parameter combination!\n");
            USB_DEBUG_TR_RETURN;
            return (1);
        }
    } else {
        xfer->flags_internal.ctrl_active = 0;
    }

    /* all fine */
    return (0);
}

/**
 * \brief   this function is used to allocate the structure and resources
 *          needed for the default USB control endpoint transfer
 *
 * \param   device  the usb device we want to setup a usb transfer
 */
void usb_transfer_setup_ctrl_default(struct usb_device *device,
        struct usb_request_state *st)
{
    USB_DEBUG_TR_ENTER;
    /* setting up transfers for the USB root hub is not allowed */
    if (device->parent_hub == NULL) {
        USB_DEBUG("ERROR: setting up transfers for root hub not allowed\n");
        return;
    }

    /*
     * since the control transfers are always on the special control
     * ep, we can cache them and reuse later
     */
    struct usb_xfer *xfer = device->ctrl_xfer[0];

    uint8_t xfer_reuse = 0;

    if (xfer) {
        xfer_reuse = ((xfer->device_address == device->device_address)
                && (device->ctrl_ep_desc.wMaxPacketSize
                        == device->device_desc.bMaxPacketSize0));

        if ((device->flags.usb_mode == USB_MODE_DEVICE) && xfer_reuse) {
            assert(!"NYI: device mode\n");
            usb_transfer_start(xfer);
            return;
        }
    }

    if (xfer_reuse) {
        USB_DEBUG_XFER("reusing the xfer... return.\n");
        return;
    }

    /*
     * we cannot reuse the USB transfer so we have to update the fields
     */
    device->ctrl_ep_desc.wMaxPacketSize = device->device_desc.bMaxPacketSize0;
    device->ctrl_ep.descriptor = &device->ctrl_ep_desc;

    usb_transfer_unsetup(device->ctrl_xfer, USB_DEVICE_CTRL_XFER_MAX);

    USB_DEBUG_XFER("setting up device ctrl xfer[0]\n");

    if (usb_transfer_setup(device, 0, &(device->ctrl_xfer[0]),
            usb_control_ep_cfg)) {
        USB_DEBUG("usb_transfer_setup_ctrl_default(): "
        "ERROR: could not allocate default control transfer\n");
        return;
    }

    USB_DEBUG_XFER("setting up device ctrl xfer[1]\n");

    if (usb_transfer_setup(device, 0, &(device->ctrl_xfer[1]),
            usb_control_ep_cfg)) {
        debug_printf("usb_transfer_setup_ctrl_default(): "
                "ERROR: could not allocate default control transfer\n");
        return;
    }

    USB_DEBUG_TR_RETURN;
}

/**
 * \brief   starts a USB transfer
 *
 * \param   xfer    the USB transfer to start
 */
void usb_transfer_start(struct usb_xfer *xfer)
{

    USB_DEBUG_TR_ENTER;

    if (xfer == NULL) {
        USB_DEBUG_XFER("NOTICE: No xfer to start...\n");
        return;
    }

    /*
     * set the started flag
     */

    xfer->flags_internal.started = 1;

    /*
     * if the transfer is already transferring, then we do not
     * have to do sth.
     */
    if (xfer->flags_internal.transferring) {
        USB_DEBUG_XFER("NOTICE: already transferring\n");

        USB_DEBUG_TR_RETURN;
        return;
    }

    /* struct usb_xfer_queue *queue = &(xfer->host_controller->done_queue);

     if (queue->current != xfer) {
     usb_xfer_enqueue(queue, xfer);
     }*/

    /*
     * submitting the transfer to start the USB hardware for the given
     * transfer
     */

    if (!xfer->flags_internal.pipe_open) {
        xfer->flags_internal.pipe_open = 1;
        (xfer->endpoint->pipe_fn->open)(xfer);
    }

    xfer->flags_internal.transferring = 1;
    xfer->flags_internal.done = 0;

    /*
     * check if the transfer is waiting on a queue, and dequeue it
     */
    /* if (xfer->wait_queue) {
     usb_xfer_dequeue(xfer);
     }*/

    // clear the closed flag
    xfer->flags_internal.transfer_closed = 0;

    // clear the DMA delay flag
    xfer->flags_internal.dma_wait = 0;

    // transfers are not immediate cancellable
    xfer->flags_internal.cancellable = 0;

    /*
     * update the status fields of the transfer
     */
    xfer->sum_bytes = 0;
    xfer->actual_bytes = 0;
    xfer->actual_frames = 0;

    xfer->error = USB_ERR_OK;

    if (xfer->device->state < USB_DEVICE_STATE_POWERED) {
        USB_DEBUG("NOTICE: device is not alive anymore...\n");
        usb_xfer_done(xfer, USB_ERR_CANCELLED);
        return;
    }

    if (xfer->num_frames == 0) {
        if (xfer->flags.pipe_stalled) {
            USB_DEBUG_XFER("Want to stall w/o transferring...\n");
            xfer->flags_internal.cancellable = 1;
            assert(!"NYI: stalling\n");
            /* TODO: usb_command_wrapper(&xfer->endpoint->endpoint_q, xfer); */
            return;
        }
        USB_DEBUG(
                "ERROR: invalid number of frames (0) in usb_transfer_start()\n");
        usb_xfer_done(xfer, USB_ERR_INVAL);
        USB_DEBUG_TR_RETURN;
        return;
    }

    for (uint32_t frame = 0; frame < xfer->num_frames; frame++) {
        xfer->frame_lengths[frame + xfer->max_frame_count] =
                xfer->frame_lengths[frame];

        xfer->sum_bytes += xfer->frame_lengths[frame];
        if (xfer->sum_bytes < xfer->frame_lengths[frame]) {
            USB_DEBUG("WARNING: total length wrapped arroud!\n");
            usb_xfer_done(xfer, USB_ERR_INVAL);
            USB_DEBUG_TR_RETURN;
            return;
        }
    }

    xfer->flags_internal.short_frames_ok = 0;
    xfer->flags_internal.short_transfer_ok = 0;

    if (xfer->flags_internal.ctrl_xfer) {
        USB_DEBUG_XFER("usb_transfer_start() - is ctrl transfer...\n");
        if (usb_transfer_ctrl_start(xfer)) {
            debug_printf("WARNING: starting usb ctrl transfer failed..\n");
            usb_xfer_done(xfer, USB_ERR_STALLED);
            USB_DEBUG_TR_RETURN;
            return;
        }
    }

    if ((((xfer)->endpoint_number & 0x80) ? 1 : 0)) {
        if (xfer->flags.short_frames_ok) {
            xfer->flags_internal.short_frames_ok = 1;
            xfer->flags_internal.short_transfer_ok = 1;
        } else if (xfer->flags.short_xfer_ok) {
            xfer->flags_internal.short_transfer_ok = 1;
            if (xfer->flags_internal.ctrl_xfer) {
                xfer->flags_internal.short_frames_ok = 1;
            }
        }
    }

    usb_pipe_enter(xfer);
    USB_DEBUG_TR_RETURN;
}

/**
 * \brief   stops an usb transfer
 *
 * \param   xfer    the usb transfer to stop
 *
 */
void usb_transfer_stop(struct usb_xfer *xfer)
{
    if (xfer == NULL) {
        return;
    }

    /*
     * check if the pipe has already been opened.
     * if not we simply reset the started flag and return
     */
    if (!xfer->flags_internal.pipe_open) {
        if (xfer->flags_internal.started) {
            xfer->flags_internal.started = 0;
        }
        xfer->flags_internal.transferring = 0;
        return;
    }
    xfer->error = USB_ERR_CANCELLED;

    // clear the flags
    xfer->flags_internal.pipe_open = 0;
    xfer->flags_internal.started = 0;
    xfer->flags_internal.done = 0;

    /*
     * now stop the transfer, depending on whether we are
     * currently transferring or not
     */
    if (xfer->flags_internal.transferring) {
        if (xfer->flags_internal.cancellable) {
            if (!xfer->flags_internal.transfer_closed) {
                (xfer->endpoint->pipe_fn->close)(xfer);
                xfer->flags_internal.transfer_closed = 1;
            }
            xfer->flags_internal.transferring = 0;
        }
    } else {
        (xfer->endpoint->pipe_fn->close)(xfer);
        xfer->flags_internal.transferring = 0;
        struct usb_endpoint *ep = xfer->endpoint;
        struct usb_xfer_queue *queue = &(ep->transfers);

        /*
         * check if we need to start the next transfer
         * i.e. dequeue it from the wait queue
         */
        if (queue->current == xfer) {
            if (!queue->recurse_1) {
                queue->recurse_1 = 1;
                xfer = ((&queue->head)->first);

                if (xfer) {
                    if ((xfer->wait_entry.next) != NULL) {
                        (xfer->wait_entry.next)->wait_entry.prev_next = xfer
                                ->wait_entry.prev_next;
                        *(xfer)->wait_entry.prev_next = (xfer->wait_entry.next);
                    } else {
                        (&queue->head)->last_next = &(&queue->head)->first;
                        (&queue->head)->first = NULL;
                        // (&queue->head)->last_next = xfer->wait_entry.prev_next;
                    }

                    xfer->wait_queue = NULL;
                    queue->current = xfer;
                    USB_DEBUG_XFER("calling (queue->command)(queue)\n");
                    (queue->command)(queue);
                }
                queue->recurse_1 = 0;
            }
        }
    }

}

/**
 * \brief   this function undoes the setup of the usb transfers
 *
 * \param   xfers       array of pointers to usb transfers to unsetup
 * \param   xfer_count  the number of xfers to unsetup
 */
void usb_transfer_unsetup(struct usb_xfer **xfers, uint16_t xfer_count)
{
    USB_DEBUG_TR_ENTER;

    if (*xfers == NULL || xfer_count == 0) {
        return;
    }

    struct usb_xfer *xfer;

    while (xfer_count > 0) {
        xfer_count--;
        xfer = xfers[xfer_count];

        if (xfer == NULL) {
            continue;
        }

        xfers[xfer_count] = NULL;

        usb_transfer_stop(xfer);

        while (!usb_transfer_completed(xfer)) {
            xfer->flags_internal.draining = 1;
            event_dispatch(get_default_waitset());
        }

        xfer->endpoint->ref_allocation--;

        (xfer->host_controller->hcdi_bus_fn->xfer_unsetup)(xfer);

        struct usb_device *dev = xfer->device;
        struct usb_xfer *prev = dev->xfers;
        while (prev != NULL) {
            if (dev->xfers == xfer) {
                dev->xfers = xfer->device_xfers_next;
                break;
            } else if (prev->device_xfers_next == xfer) {
                debug_printf("removing from device xfers..\n");
                prev->device_xfers_next = xfer->device_xfers_next;
                break;
            }
            prev = prev->device_xfers_next;
        }

        free(xfer);
    }

    USB_DEBUG_TR_RETURN;
}

/**
 * \brief   this function allocates the resources for a number of usb transfers
 *
 * \param   device      the device we want to allocate the transfers
 * \param   ifaces      array of interfaces
 * \param   usb_xfers   pointer to an array of usb_xfer
 * \param   setups      setup parameter array
 * \para    setup_count the number of setups we have to do
 * \
 */
usb_error_t usb_transfer_setup(struct usb_device *device, const uint8_t iface,
        struct usb_xfer **ret_xfer, const struct usb_xfer_config *setup)
{
    USB_DEBUG_TR_ENTER;

    struct usb_xfer_setup_params params;
    memset(&params, 0, sizeof(params));

    params.device = device;
    params.speed = device->speed;
    params.hc_max_packet_count = 1;
    params.err = USB_ERR_OK;
    params.type = setup->usb_type;
    params.size[0] = 0;
    params.buf = NULL;
    params.xfer_setup = setup;

    struct usb_endpoint *ep = usb_endpoint_lookup(device, iface, setup);

    if ((ep == NULL) || (ep->pipe_fn == NULL)) {
        USB_DEBUG_XFER("WARNING: No associated pipe!\n");USB_DEBUG_TR_RETURN;
        return (USB_ERR_NO_PIPE);
    }

    struct usb_xfer *xfer = malloc(sizeof(struct usb_xfer));
    memset(xfer, 0, sizeof(*xfer));
    xfer->xfer_id = device->xfer_id++;
    xfer->device_xfers_next = device->xfers;
    device->xfers = xfer;
    xfer->xfer_done_cb = setup->xfer_done_cb;
    xfer->type = setup->usb_type;
    xfer->device_address = device->device_address;
    xfer->host_controller = device->controller;
    xfer->device = device;
    xfer->endpoint = ep;
    params.curr_xfer = xfer;
    params.pipe_fn = xfer->endpoint->pipe_fn;

    (device->controller->hcdi_bus_fn->xfer_setup)(&params);

    if (params.err != USB_ERR_OK) {
        USB_DEBUG(
                "ERROR: hcdi_xfer_setup failed: %s\n", usb_get_error_string(params.err));
        return (params.err);
    }

    xfer->endpoint->ref_allocation++;
    assert(xfer->endpoint->ref_allocation);

    *ret_xfer = xfer;

    USB_DEBUG_TR_RETURN;
    return (USB_ERR_OK);
}

/**
 * \brief   checks if the transfer is completed
 *
 * \param   xfer    the USB transer we want to check
 *
 * \return  1:  the transfer is completed
 *          0:  the transfer is in progress
 */
uint8_t usb_transfer_completed(struct usb_xfer *xfer)
{
    /*
     * there is no transfer, so it is completed
     */
    if (xfer == NULL) {
        USB_DEBUG_XFER("usb_transfer_completed() - transfer is null\n");
        return (1);
    }

    /*
     * call poll. this updates the state of the current transfers...
     */
    //(xfer->host_controller->hcdi_bus_fn->xfer_poll)(xfer->host_controller);
    (xfer->host_controller->hcdi_bus_fn->xfer_finished)(xfer);

    /*
     * if the flags say we are transferring at the moment,
     * then we are not completed
     */
    if (xfer->flags_internal.transferring) {
        USB_DEBUG_XFER("usb_transfer_completed() - is transferring\n");
        return (0);
    }

    /*
     * if we are waiting on a queue then we are not finished
     */
    if (xfer->wait_queue) {
        USB_DEBUG_XFER("usb_transfer_completed() - on wait queue\n");
        return (0);
    }

    return (1);
    /*
     * checking the done queue now
     */
    struct usb_xfer_queue *queue;
    queue = &(xfer->host_controller->done_queue);

    /*
     * we are scheduled for callback so not quite finished yet
     */
    if (queue->current == xfer) {
        USB_DEBUG_XFER("usb_transfer_completed() - scheduled for callback\n");
        return (0);
    }USB_DEBUG_XFER("usb_transfer_completed() - transfer is completed.\n");
    return (1);
}

/*
 * --------------------------------------------------------------------------
 * Flounder Callbacks
 * --------------------------------------------------------------------------
 */

static void usb_tx_transfer_generic_cb(void *a)
{
    /// XXX: Pagefaults... free(a);
}

/* ------------------------- transfer setup ------------------------- */

struct usb_tsetup_state {
    uint32_t tid;
    struct usb_manager_binding *bind;
    usb_error_t error;

};

static void usb_tx_transfer_setup_response(void *a)
{
    errval_t err;
    struct usb_tsetup_state *st = (struct usb_tsetup_state *) a;

    struct event_closure txcont = MKCONT(usb_tx_transfer_generic_cb, st);

    err = usb_manager_transfer_setup_response__tx(st->bind, txcont,
            (uint32_t) st->error, st->tid);

    USB_TX_TRANSER_ERR(usb_tx_transfer_setup_response);
}

/**
 *
 */
void usb_rx_transfer_setup_call(struct usb_manager_binding *bind, uint8_t type,
        usb_manager_setup_param_t params)
{
    struct usb_tsetup_state *st = malloc(sizeof(struct usb_tsetup_state));

    if (st == NULL) {
        debug_printf("WARNING: Cannot reply, out of memory!\n");
        return;
    }

    struct usb_xfer_config setup;

    st->bind = bind;
    struct usb_xfer *xfer;
    struct usb_device *dev = (struct usb_device *) bind->st;

    memcpy(&setup, &params, sizeof(params));
    if (dev == NULL) {
        st->tid = 0;
        st->error = USB_ERR_BAD_CONTEXT;
        usb_tx_transfer_setup_response(st);
        return;
    }

    setup.xfer_done_cb = &usb_transfer_complete_notify;

    switch ((usb_type_t) type) {
        case USB_TYPE_BULK:
            USB_DEBUG_IDC("received usb_rx_transfer_setup_call [bulk type]\n");
            /* TODO: Handle transfer setup */
            setup.usb_type = USB_TYPE_BULK;
            st->error = USB_ERR_OK;
            xfer->usb_manager_binding = st->bind;
            xfer->usb_driver_binding = dev->usb_driver_binding;
            st->tid = 123;
            break;
        case USB_TYPE_CTRL:
            USB_DEBUG_IDC("received usb_rx_transfer_setup_call [ctrl type]\n");
            /* TODO: Handle transfer setup */
            setup.usb_type = USB_TYPE_CTRL;
            st->error = USB_ERR_OK;
            xfer->usb_manager_binding = st->bind;
            xfer->usb_driver_binding = dev->usb_driver_binding;
            st->tid = 234;
            break;
        case USB_TYPE_ISOC:
            USB_DEBUG_IDC("received usb_rx_transfer_setup_call [isoc type]\n");
            /* TODO: Handle transfer setup */
            setup.usb_type = USB_TYPE_ISOC;
            st->error = USB_ERR_OK;
            xfer->usb_manager_binding = st->bind;
            xfer->usb_driver_binding = dev->usb_driver_binding;
            st->tid = 345;
            break;
        case USB_TYPE_INTR:
            USB_DEBUG_IDC("received usb_rx_transfer_setup_call [intr type]\n");
            /* TODO: Handle transfer setup */
            setup.usb_type = USB_TYPE_INTR;
            st->error = usb_transfer_setup(dev, params.iface, &xfer, &setup);
            xfer->usb_manager_binding = st->bind;
            xfer->usb_driver_binding = dev->usb_driver_binding;
            st->tid = xfer->xfer_id;
            break;
        default:
            USB_DEBUG("received usb_rx_transfer_setup_call [invalid type]\n");
            st->error = USB_ERR_INVAL;
            break;
    }
    usb_tx_transfer_setup_response(st);
}

/* ------------------------- transfer unsetup ------------------------- */

struct usb_tunsetup_state {
    usb_error_t error;
    struct usb_manager_binding *bind;
};

static void usb_tx_transfer_unsetup_response(void *a)
{
    errval_t err;
    struct usb_tunsetup_state *st = (struct usb_tunsetup_state *) a;

    struct event_closure txcont = MKCONT(usb_tx_transfer_generic_cb, st);

    err = usb_manager_transfer_unsetup_response__tx(st->bind, txcont,
            (uint32_t) st->error);

    USB_TX_TRANSER_ERR(usb_tx_transfer_unsetup_response);
}

/**
 *
 */
void usb_rx_transfer_unsetup_call(struct usb_manager_binding *bind,
        uint32_t tid)
{
    struct usb_tunsetup_state *st = malloc(sizeof(struct usb_tunsetup_state));

    if (st == NULL) {
        debug_printf("WARNING: Cannot reply, out of memory!\n");
        return;
    }

    st->bind = bind;

    usb_tx_transfer_unsetup_response(st);
}

/* ------------------------- transfer start ------------------------- */

struct usb_tstart_state {
    usb_error_t error;
    struct usb_manager_binding *bind;
};

static void usb_tx_transfer_start_response(void *a)
{
    errval_t err;
    struct usb_tstart_state *st = (struct usb_tstart_state *) a;

    USB_DEBUG_IDC("usb_tx_transfer_start_response()\n\n");

    struct event_closure txcont = MKCONT(usb_tx_transfer_generic_cb, st);

    err = usb_manager_transfer_start_response__tx(st->bind, txcont,
            (uint32_t) st->error);

    USB_TX_TRANSER_ERR(usb_tx_transfer_start_response);
}

void usb_rx_transfer_start_call(struct usb_manager_binding *bind, uint32_t tid)
{
    USB_DEBUG_IDC("usb_rx_transfer_start_call()\n");

    struct usb_tstart_state *st = malloc(sizeof(struct usb_tstart_state));

    if (st == NULL) {
        debug_printf("WARNING: Cannot reply, out of memory!\n");
    }
    st->bind = bind;

    struct usb_device *dev = (struct usb_device *) (bind->st);

    assert(dev != NULL);

    struct usb_xfer *xfer = dev->xfers;

    while (xfer) {
        if (xfer->xfer_id == tid) {
            break;
        }
        xfer = xfer->device_xfers_next;
    }

    if (xfer == NULL) {
        USB_DEBUG("no xfer!\n");
        st->error = USB_ERR_BAD_CONTEXT;
        usb_tx_transfer_start_response(st);
    }

    usb_transfer_start(xfer);

    st->error = xfer->error;

    usb_tx_transfer_start_response(st);
}

/* ------------------------- transfer stop ------------------------- */

struct usb_tstop_state {
    struct usb_manager_binding *bind;
    usb_error_t error;
};

static void usb_tx_transfer_stop_response(void *a)
{
    errval_t err;
    struct usb_tstop_state *st = (struct usb_tstop_state *) a;

    USB_DEBUG_IDC("usb_tx_transfer_stop_response()\n");

    struct event_closure txcont = MKCONT(usb_tx_transfer_generic_cb, st);

    err = usb_manager_transfer_stop_response__tx(st->bind, txcont,
            (uint32_t) st->error);

    USB_TX_TRANSER_ERR(usb_tx_transfer_stop_response);
}

void usb_rx_transfer_stop_call(struct usb_manager_binding *bind, uint32_t tid)
{
    struct usb_tstop_state *st = malloc(sizeof(struct usb_tstop_state));

    if (st == NULL) {
        debug_printf("WARNING: Cannot reply, out of memory!\n");
    }

    st->bind = bind;

    usb_tx_transfer_stop_response(st);
}

/* ------------------------- transfer status ------------------------- */

struct usb_tstatus_state {
    struct usb_manager_binding *bind;
    usb_error_t error;
    uint32_t actlen;
    uint32_t length;
    uint32_t actframes;
    uint32_t numframes;
};

static void usb_tx_transfer_status_response(void *a)
{
    errval_t err;
    struct usb_tstatus_state *st = (struct usb_tstatus_state *) a;

    USB_DEBUG_IDC("usb_tx_transfer_status_response()\n");

    struct event_closure txcont = MKCONT(usb_tx_transfer_generic_cb, st);

    err = usb_manager_transfer_status_response__tx(st->bind, txcont,
            (uint32_t) st->error, st->actlen, st->length, st->actframes,
            st->numframes);

    USB_TX_TRANSER_ERR(usb_tx_transfer_status_response);
}

void usb_rx_transfer_status_call(struct usb_manager_binding *bind, uint32_t tid)
{
    struct usb_tstatus_state *st = malloc(sizeof(struct usb_tstatus_state));

    if (st == NULL) {
        debug_printf("WARNING: Cannot reply, out of memory!\n");
        return;
    }

    st->bind = bind;

    usb_tx_transfer_status_response(st);
}

/* ------------------------- transfer state ------------------------- */

struct usb_tstate_state {
    struct usb_manager_binding *bind;
    usb_error_t error;
    uint32_t state;
};

static void usb_tx_transfer_state_response(void *a)
{
    errval_t err;
    struct usb_tstate_state *st = (struct usb_tstate_state *) a;

    USB_DEBUG_IDC("usb_tx_transfer_state_response()\n");

    struct event_closure txcont = MKCONT(usb_tx_transfer_generic_cb, st);

    err = usb_manager_transfer_state_response__tx(st->bind, txcont,
            (uint32_t) st->error, st->state);

    USB_TX_TRANSER_ERR(usb_tx_transfer_state_response);
}

void usb_rx_transfer_state_call(struct usb_manager_binding *bind, uint32_t tid)
{
    struct usb_tstate_state *st = malloc(sizeof(struct usb_tstate_state));

    if (st == NULL) {
        USB_DEBUG_IDC("WARNING: Cannot reply, out of memory!\n");
        return;
    }

    st->bind = bind;

    usb_tx_transfer_state_response(st);
}

/* ------------------------- transfer clear stall ------------------------- */

struct usb_tclearstall_state {
    struct usb_manager_binding *bind;
    usb_error_t error;
};

static void usb_tx_transfer_clear_stall_response(void *a)
{
    errval_t err;
    struct usb_tclearstall_state *st = (struct usb_tclearstall_state *) a;

    USB_DEBUG_IDC("usb_tx_transfer_clear_stall_response()\n");

    struct event_closure txcont = MKCONT(usb_tx_transfer_generic_cb, st);

    err = usb_manager_transfer_clear_stall_response__tx(st->bind, txcont,
            (uint32_t) st->error);

    USB_TX_TRANSER_ERR(usb_tx_transfer_clear_stall_response);
}

void usb_rx_transfer_clear_stall_call(struct usb_manager_binding *bind,
        uint32_t tid)
{
    struct usb_tclearstall_state *st = malloc(
            sizeof(struct usb_tclearstall_state));

    if (st == NULL) {
        debug_printf("WARNING: Cannot reply, out of memory!\n");
        return;
    }

    st->bind = bind;

    usb_tx_transfer_clear_stall_response(st);
}

