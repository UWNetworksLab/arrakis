/**
 * \brief this file contains functions to setup the different xfers
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
#include <usb/usb_descriptor.h>
#include <usb/usb_error.h>

#include <usb_device.h>
#include <usb_controller.h>
#include <usb_memory.h>
#include <usb_xfer.h>

/**
 * \brief   this function adds a usb xfer to a wait queue if it is not already
 *          in one.
 *
 * \param   queue   the queue the xfer should be added to
 * \param   xfer    the transfer to add to the queue
 *
 * Note: a transfer is put on a queue, iff it cannot be handled directly,
 *       therefore a transfer can only be on one wait queue.
 */
void usb_xfer_enqueue(struct usb_xfer_queue *queue, struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    if (xfer->wait_queue == NULL) {

        USB_DEBUG_XFER("add: [%x, q=%p, &f=%p, ln=%p, wn=%p, &wn=%p, pn=%p\n",
                xfer->xfer_id, queue, &queue->head.first,
                queue->head.last_next, xfer->wait_entry.next,
                &xfer->wait_entry.next, xfer->wait_entry.prev_next);

        assert(queue != NULL);

        xfer->wait_queue = queue;

        xfer->wait_entry.next = NULL;
        xfer->wait_entry.prev_next = queue->head.last_next;

        *(queue->head.last_next) = xfer;
        queue->head.last_next = &(xfer->wait_entry.next);

    }

    USB_DEBUG_TR_RETURN;
}

/**
 * \brief   this function removes the usb xfer from the wait queue queue
 *
 * \param   xfer    the transfer to remove to the queue
 *
 */
void usb_xfer_dequeue(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    struct usb_xfer_queue *queue;

    queue = xfer->wait_queue;

    if (queue) {

        USB_DEBUG_XFER("removing the transfer from the wait queue\n");

        if ((xfer->wait_entry.next) != NULL)
            xfer->wait_entry.next->wait_entry.prev_next = xfer->wait_entry
                    .prev_next;
        else {
            queue->head.last_next = xfer->wait_entry.prev_next;
        }

        *xfer->wait_entry.prev_next = xfer->wait_entry.next;

        xfer->wait_queue = NULL;

        USB_DEBUG_XFER("rem: [%x, q=%p, &f=%p, ln=%p, wn=%p, &wn=%p, pn=%p\n",
                xfer->xfer_id, queue, &queue->head.first,
                queue->head.last_next, xfer->wait_entry.next,
                &xfer->wait_entry.next, xfer->wait_entry.prev_next);

    }

    USB_DEBUG_TR_RETURN;
}

/**
 * \brief   this function handles complete transfers by removing them from
 *          the interrupt queue and inserting them into the done queue.
 *
 * \param   xfer   the completed usb transfer
 * \param   err    error condition of the transfer
 *
 */
void usb_xfer_done(struct usb_xfer *xfer, usb_error_t err)
{
    USB_DEBUG_TR_ENTER;

    /*
     * if the transfer started, this flag has to be set to 1. Therefore
     * the transfer may be canceled. Just return then.
     */
    if (!xfer->flags_internal.transferring && !xfer->flags_internal.done) {
        USB_DEBUG_XFER("NOTICE: transfer was not transferring..\n");
        // clear the control active flag and return
        xfer->flags_internal.ctrl_active = 0;

        USB_DEBUG_TR_RETURN;
        return;
    }

    if (!xfer->flags_internal.transferring && !xfer->flags_internal.started) {
        /* the transfer is not transferring and not started nothing to do */
        return;
    }

    if ((!xfer->flags_internal.pipe_open)
            && (!xfer->flags_internal.transfer_closed)) {
        /* check if the pipe was opened and close it */
        xfer->endpoint->pipe_fn->close(xfer);
        xfer->flags_internal.transfer_closed = 1;

    }

    // update error condition
    xfer->error = err;

    if (err != USB_ERR_OK) {
        USB_DEBUG_XFER("Transfer done with error: %s\n", usb_get_error_string(err));
    }

    /*
     * the transfer was enqueued and is waiting on the interrupt queue
     * so we have to dequeue it
     */

    usb_xfer_dequeue(xfer);

    /*
     * check the number of actual frames this should not be bigger than
     * the maximum number of frames
     */
    if (xfer->actual_frames > xfer->num_frames) {
        if (xfer->error == USB_ERR_OK) {
            USER_PANIC("WARNING: actual frames bigger than num frames! PANIC!");
        } else {
            xfer->actual_frames = xfer->num_frames;
        }
    }

    /* calculate the actual size of the transferred data */uint32_t frame = 0;
    xfer->actual_bytes = 0;
    for (frame = 0; frame < xfer->actual_frames; frame++) {
        xfer->actual_bytes += xfer->frame_lengths[frame];
    }

    /* set frame length of the unused frames to be zero */
    while (frame < xfer->num_frames) {
        xfer->frame_lengths[frame] = 0;
        frame++;
    }

    if (xfer->actual_bytes > xfer->sum_bytes) {
        if (xfer->error == USB_ERR_OK) {
            USER_PANIC("WARNING: actual bytes biggar than sum bytes! PANIC!");
        } else {
            xfer->actual_frames = xfer->num_frames;
        }
    }

    /* clear the flags */
    xfer->flags_internal.ctrl_active = 0;
    xfer->flags_internal.transferring = 0;

    /*
     * start the next transfer on this endpoint if there is any
     */
    struct usb_endpoint *ep = xfer->endpoint;
    struct usb_xfer_queue *q = &ep->transfers;

    if (q->current == xfer || q->current == NULL) {
        q->current = NULL;
        if (!q->recurse_1) {
            q->recurse_1 = 1;
            /* get the first transfer of the queue */
            struct usb_xfer *next_xfer = q->head.first;

            if (next_xfer) {
                /* dequeue it */
                if (next_xfer->wait_entry.next != NULL) {
                    (*next_xfer->wait_entry.prev_next)->wait_entry.prev_next =
                            next_xfer->wait_entry.prev_next;
                    *next_xfer->wait_entry.prev_next = next_xfer->wait_entry
                            .next;
                } else {
                    q->head.last_next = &(q->head.first);
                    q->head.first = NULL;
                }
                /* the transfer is not on a wait queue anymore */
                next_xfer->wait_queue = NULL;
                q->current = next_xfer;

                USB_DEBUG_XFER(
                        "rem2: [%x, q=%p, &f=%p, ln=%p, wn=%p, &wn=%p, pn=%p\n",
                        xfer->xfer_id, q, &q->head.first, q->head.last_next,
                        xfer->wait_entry.next, &xfer->wait_entry.next,
                        xfer->wait_entry.prev_next);

                /* start the transfer */
                (q->command)(q);

            } else {
                /* reset the queue */
                (&q->head)->last_next = &q->head.first;
                (&q->head)->first = NULL;
            }
            q->recurse_1 = 0;
        }
        if (!(ep->transfers.current || ep->transfers.head.first)) {
            xfer->endpoint->is_sync = 0;
        }

    }

    /* send the transfer done notification */
    if (xfer->xfer_done_cb && xfer->flags_internal.notify) {
        xfer->flags_internal.notify = 0;
        (xfer->xfer_done_cb)(xfer, err);
    }

    USB_DEBUG_TR_RETURN;
    return;
}

/// struct for the packet sizes
struct usb_std_packet_size {
    struct {
        uint16_t min; /* inclusive */
        uint16_t max; /* inclusive */
    } range;

    uint16_t fixed[4];
};

/**
 * \brief checks the standard packet sizes according to the transfer speed
 *        and the endpoint type
 *
 * \param ptr pointer to the memory location to store the size information
 * \param type the type of the endpoint
 * \param speed the endpoint speed
 */
static void usb_xfer_get_packet_size(struct usb_std_packet_size *ptr,
        uint8_t type, enum usb_speed speed)
{
    /* interrupt type */
    static const uint16_t intr_range_max[USB_SPEED_MAX] = {
        [USB_SPEED_LOW] = 8,
        [USB_SPEED_FULL] = 64,
        [USB_SPEED_HIGH] = 1024,
        [USB_SPEED_VARIABLE] = 1024,
        [USB_SPEED_SUPER] = 1024,
    };

    /* isochronus type */
    static const uint16_t isoc_range_max[USB_SPEED_MAX] = {
        [USB_SPEED_LOW] = 0, /* invalid */
        [USB_SPEED_FULL] = 1023,
        [USB_SPEED_HIGH] = 1024,
        [USB_SPEED_VARIABLE] = 3584,
        [USB_SPEED_SUPER] = 1024,
    };

    /* control type */
    static const uint16_t control_min[USB_SPEED_MAX] = {
        [USB_SPEED_LOW] = 8,
        [USB_SPEED_FULL] = 8,
        [USB_SPEED_HIGH] = 64,
        [USB_SPEED_VARIABLE] = 512,
        [USB_SPEED_SUPER] = 512,
    };

    /* bulk type */
    static const uint16_t bulk_min[USB_SPEED_MAX] = {
        [USB_SPEED_LOW] = 8,
        [USB_SPEED_FULL] = 8,
        [USB_SPEED_HIGH] = 512,
        [USB_SPEED_VARIABLE] = 512,
        [USB_SPEED_SUPER] = 1024,
    };

    uint16_t temp;

    /* reset the fields */
    memset(ptr, 0, sizeof(*ptr));

    switch (type) {
        case USB_ENDPOINT_TYPE_INTR:
            ptr->range.max = intr_range_max[speed];
            break;
        case USB_ENDPOINT_TYPE_ISOCHR:
            ptr->range.max = isoc_range_max[speed];
            break;
        default:
            if (type == USB_ENDPOINT_TYPE_BULK)
                temp = bulk_min[speed];
            else
                temp = control_min[speed];

            ptr->fixed[0] = temp;
            ptr->fixed[1] = temp;
            ptr->fixed[2] = temp;
            ptr->fixed[3] = temp;

            if (speed == USB_SPEED_FULL) {
                ptr->fixed[1] = 16;
                ptr->fixed[2] = 32;
                ptr->fixed[3] = 64;
            }
            break;
    }
}

/**
 * \brief   this function is called from the xfer_setup function of the
 *          respective USB host controller driver. This function sets the
 *          correct values in the usb_xfer struct.
 *
 * \param   param   USB transfer setup parameters
 *
 */
void usb_xfer_setup_struct(struct usb_xfer_setup_params *param)
{
    USB_DEBUG_TR_ENTER;

    struct usb_xfer *xfer = param->curr_xfer;

    /* do a parameter check */
    if ((param->hc_max_packet_size == 0) || (param->hc_max_packet_count == 0)
            || (param->hc_max_frame_size == 0)) {
        USB_DEBUG("WARNING: Invaid setup parameter\n");
        param->err = USB_ERR_INVAL;

        xfer->max_hc_frame_size = 1;
        xfer->max_frame_size = 1;
        xfer->max_packet_size = 1;
        xfer->max_data_length = 0;
        xfer->num_frames = 0;
        xfer->max_frame_count = 0;
        USB_DEBUG_TR_RETURN;
        return;
    }

    const struct usb_xfer_config *setup_config = param->xfer_setup;
    struct usb_endpoint_descriptor *ep_desc = xfer->endpoint->descriptor;

    assert(setup_config);
    assert(ep_desc);

    xfer->max_packet_size = xfer->endpoint->max_packet_size;

    usb_speed_t ep_speed = param->speed;
    uint8_t type = ep_desc->bmAttributes.xfer_type;
    xfer->endpoint_number = ep_desc->bEndpointAddress.ep_number;
    xfer->ed_direction = ep_desc->bEndpointAddress.direction;

    xfer->max_packet_count = 1;

    /* adjust maximum packet count and size */
    switch (ep_speed) {
        case USB_SPEED_HIGH:
            if (type == USB_ENDPOINT_TYPE_ISOCHR
                    || type == USB_ENDPOINT_TYPE_INTR) {
                xfer->max_packet_count += (xfer->max_packet_size >> 11) & 3;
                if (xfer->max_packet_count > 3) {
                    xfer->max_packet_count = 3;
                }
            }
            xfer->max_packet_size &= 0x7FF;
            break;
        case USB_SPEED_SUPER:
            assert(!"NYI: No super speed support right now.");
            break;
        default:
            /* noop */
            break;
    }

    uint32_t num_frlengths;
    uint32_t num_frbuffers;

    /* setup some values of the xfer struct */
    xfer->flags = setup_config->flags;
    xfer->num_frames = setup_config->frames;
    xfer->timeout = setup_config->timeout;
    xfer->interval = setup_config->interval;
    param->bufsize = setup_config->bufsize;

    xfer->flags_internal.usb_mode = param->device->flags.usb_mode;


    struct usb_std_packet_size size;
    usb_xfer_get_packet_size(&size, type, param->speed);

    if (size.range.min || size.range.max) {
        if (xfer->max_packet_size < size.range.min) {
            xfer->max_packet_size = size.range.min;
        }
        if (xfer->max_packet_size > size.range.max) {
            xfer->max_packet_size = size.range.max;
        }
    } else {
        if (xfer->max_packet_size >= size.fixed[3]) {
            xfer->max_packet_size = size.fixed[3];
        } else if (xfer->max_packet_size >= size.fixed[2]) {
            xfer->max_packet_size = size.fixed[2];
        } else if (xfer->max_packet_size >= size.fixed[1]) {
            xfer->max_packet_size = size.fixed[1];
        } else {
            /* only one possibility left */
            xfer->max_packet_size = size.fixed[0];
        }
    }

    /*
     * range checks and filter values according to the host controller
     * values. Maximum packet size and count must not be bigger than supported
     * by the host controller.
     */

    if (xfer->max_packet_count > param->hc_max_packet_count) {
        xfer->max_packet_count = param->hc_max_packet_count;
    }

    if ((xfer->max_packet_size > param->hc_max_packet_size)
            || (xfer->max_packet_size == 0)) {
        xfer->max_packet_size = param->hc_max_packet_size;
    }

    /*
     * compute the maximum frame size as the maximum number of packets
     * multiplied by the maximum packet size
     */
    xfer->max_frame_size = xfer->max_packet_size * xfer->max_packet_count;

    /*
     * isochronus and interrupt transfer need to be checked very INTERVALL ms
     * so we need to setup the interval for this to transfer types
     */
    switch (type) {
        case USB_TYPE_ISOC:
            /* TODO: ISOCHRONUS IMPLEMENTATION */
            assert(!"NYI: isochronus support not yet implemented");
            break;
        case USB_TYPE_INTR:
            if (xfer->interval == 0) {
                /* interval is not set, get it from the endpoint descriptor */
                xfer->interval = ep_desc->bInterval;
                /*
                 * since the frames have different durations for FULL/LOW speed
                 * and high speed devices we have to do conversion
                 * from 125 us -> 1 ms
                 */
                if (param->speed != USB_SPEED_FULL
                        && param->speed != USB_SPEED_LOW) {
                    if (xfer->interval < 4) {
                        /* smallest interval 1 ms*/
                        xfer->interval = 1;
                    } else if (xfer->interval > 16) {
                        /* maximum interval 32ms */
                        xfer->interval = (0x1 << (16 - 4));
                    } else {
                        /* normal interval */
                        xfer->interval = (0x1 << (xfer->interval - 4));
                    }
                }
            }

            /* ensure that the interval is at least 1 */
            xfer->interval += (xfer->interval ? 0 : 1);

            /* calculate the frame down shift value */
            xfer->frame_shift = 0;
            uint32_t tmp = 1;
            while ((tmp != 0) && (tmp < xfer->interval)) {
                xfer->frame_shift++;
                tmp *= 2;
            }

            if (param->speed != USB_SPEED_FULL
                    && param->speed != USB_SPEED_LOW) {
                xfer->frame_shift += 3;
            }

            break;
        default:
            /* noop */
            break;
    }
    uint8_t max_length_zero = 0;

    if ((xfer->max_frame_size == 0) || (xfer->max_packet_size == 0)) {

        max_length_zero = 1;

        /* check for minimum packet size */
        if ((param->bufsize <= 8) && (type != USB_TYPE_CTRL)
                && (type != USB_TYPE_BULK)) {
            xfer->max_packet_size = 8;
            xfer->max_packet_count = 1;
            param->bufsize = 0;
            xfer->max_frame_size = xfer->max_packet_size
                    * xfer->max_packet_count;
        } else {
            /* error condition */
            USB_DEBUG("WARNING: Invalid zero max length.\n");
            param->err = USB_ERR_ZERO_MAXP;
            xfer->max_hc_frame_size = 1;
            xfer->max_frame_size = 1;
            xfer->max_packet_size = 1;
            xfer->max_data_length = 0;
            xfer->num_frames = 0;
            xfer->max_frame_count = 0;
            USB_DEBUG_TR_RETURN;
            return;
        }
    }

    /*
     * if the buffer size is not given, we set it to the default size
     * such that the maximum frame fits into it. For isochronus we have
     * to multiply this by the expected number of frames.
     */
    if (param->bufsize == 0) {
        param->bufsize = xfer->max_frame_size;

        if (type == USB_ENDPOINT_TYPE_ISOCHR) {
            param->bufsize *= xfer->num_frames;
        }
    }

    if (xfer->flags.ext_buffer) {
        param->bufsize += (xfer->max_frame_size - 1);

        if (param->bufsize < xfer->max_frame_size) {
            param->err = USB_ERR_INVAL;
            USB_DEBUG("WARNING: Invalid buffer size.\n");
            xfer->max_hc_frame_size = 1;
            xfer->max_frame_size = 1;
            xfer->max_packet_size = 1;
            xfer->max_data_length = 0;
            xfer->num_frames = 0;
            xfer->max_frame_count = 0;
            USB_DEBUG_TR_RETURN;
            return;
        }
        param->bufsize -= (param->bufsize % xfer->max_frame_size);
        if (type == USB_ENDPOINT_TYPE_CONTROL) {
            /* add the device request size for the setup message
             *  to the buffer length
             */
            param->bufsize += 8;
        }
    }

    xfer->max_data_length = param->bufsize;

    /* calculate the number of required frames depending on the transfer type */
    switch (type) {
        case USB_TYPE_ISOC:
            num_frlengths = xfer->num_frames;
            num_frbuffers = 1;
            break;

        case USB_TYPE_CTRL:
            /*
             * control transfers are special, since they may have a data stage
             * or not, the number of frames depends on this.
             *
             * If the buffer size is 8 bytes, this is just the size of the
             * device request and thus there is no data stage.
             *
             * Set the flag indicating that this is a control transfer
             */

            xfer->flags_internal.ctrl_xfer = 1;

            if (xfer->num_frames == 0) {
                if (param->bufsize <= 8) {
                    /* no data stage of this control request */
                    xfer->num_frames = 1;
                } else {
                    /* there is a data stage of this control request */
                    xfer->num_frames = 2;
                }
            }


            num_frlengths = xfer->num_frames;
            num_frbuffers = xfer->num_frames;

            if (xfer->max_data_length < 8) {
                /* too small length: wrap around or too small buffer size */
                param->err = USB_ERR_INVAL;
                USB_DEBUG("WARNING: invalid max_data_length.\n");
                xfer->max_hc_frame_size = 1;
                xfer->max_frame_size = 1;
                xfer->max_packet_size = 1;
                xfer->max_data_length = 0;
                xfer->num_frames = 0;
                xfer->max_frame_count = 0;
                USB_DEBUG_TR_RETURN;
                return;
            }

            /* subtract the request size */

            xfer->max_data_length -= 8;
            break;

        default:
            /* the num frames is at least one */
            xfer->num_frames = (xfer->num_frames) ? (xfer->num_frames) : 1;
            num_frlengths = xfer->num_frames;
            num_frbuffers = xfer->num_frames;
            break;
    }


    /* allocate the frame buffers and the frame length array */
    xfer->max_frame_count = xfer->num_frames;

    xfer->frame_lengths = malloc(2 * xfer->num_frames * sizeof(uint32_t));
    xfer->frame_buffers = malloc(
            xfer->num_frames * sizeof(struct usb_dma_page));

    for (uint32_t i = 0; i < xfer->num_frames; i++) {
        xfer->frame_lengths[i] = xfer->max_data_length;
        xfer->frame_buffers[i] = usb_mem_dma_alloc(
                param->num_pages * USB_PAGE_SIZE, USB_PAGE_SIZE);
    }

    /*
     * we expect to have no data stage, so set it to the correct value
     */
    if (max_length_zero) {
        xfer->max_data_length = 0;
    }

    /* round up maximum buf size */
    if (param->bufsize_max < param->bufsize) {
        param->bufsize_max = param->bufsize;
    }

    xfer->max_hc_frame_size = (param->hc_max_frame_size
            - (param->hc_max_frame_size % xfer->max_frame_size));

    if (xfer->max_hc_frame_size == 0) {
        USB_DEBUG("WARNING: Invalid max_hc_frame_size.\n");
        param->err = USB_ERR_INVAL;
        xfer->max_hc_frame_size = 1;
        xfer->max_frame_size = 1;
        xfer->max_packet_size = 1;
        xfer->max_data_length = 0;
        xfer->num_frames = 0;
        xfer->max_frame_count = 0;
        USB_DEBUG_TR_RETURN;
        return;
    }

    USB_DEBUG_TR_RETURN;
}

