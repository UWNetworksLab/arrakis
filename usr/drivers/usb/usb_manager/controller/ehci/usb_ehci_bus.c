/**
 * \brief This file contains EHCI specific functions for the USB host controller
 *        driver interface.
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
#include <barrelfish/barrelfish.h>

// Mackerel device
#include "ehci_device.h"

// common USB declarations
#include <usb/usb.h>

// USB manager specific includes
#include <usb_xfer.h>
#include <usb_device.h>
#include <usb_controller.h>
#include <usb_hub.h>
#include <usb_memory.h>

// host controller specific includes
#include "usb_ehci.h"
#include "usb_ehci_root_hub.h"
#include "usb_ehci_bus.h"
#include "usb_ehci_xfer.h"
#include "usb_ehci_pipe.h"
#include "usb_ehci_queue.h"

/**
 * \brief wrapper function to perform the polling of the USB transfers
 *
 * \param hostc pointer to the generic host controller of type ehci
 *
 * Note: this functions is needed to translate the generic poll request
 *       into a host controller specific poll request.
 */
static void usb_ehci_do_poll(usb_host_controller_t *hostc)
{
    assert(hostc->hc_type == USB_EHCI);

    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) (hostc->hc_control);

    usb_ehci_poll(hc);
}

/**
 * \brief executes the polling of the usb transfers
 *
 * \param hc host ehci host controller to poll
 *
 * Note: this function iteratively polls all transfers currently added to the
 *       interrupt queue of this host controller. If a transfer changes its
 *       state to finished, the process is repeated
 */
void usb_ehci_poll(usb_ehci_hc_t *hc)
{

    USB_DEBUG_TR_ENTER;

    struct usb_xfer *xfer = hc->controller->intr_queue.head.first;
    uint8_t repeat = 0;

    /* loop over all transferstill no changes */
    do {
        repeat = 0;
        while (xfer != NULL) {
            /* TODO: Check if needed */
            if (xfer == ((xfer))->wait_entry.next) {
                break;
            }

            if (usb_ehci_xfer_is_finished(xfer)) {
                repeat = 1;  // repeat the polling process
            }
            xfer = xfer->wait_entry.next;
        }
    } while (repeat);

    USB_DEBUG_TR_RETURN;
}

/**
 * \brief stores function pointers to the host controller driver interface
 */
static struct usb_hcdi_bus_fn usb_ehci_bus_fn = {
    .endpoint_init = usb_ehci_endpoint_init,
    .xfer_setup = usb_ehci_xfer_setup,
    .xfer_unsetup = usb_ehci_xfer_unsetup,
    .device_resume = usb_ehci_device_resume,
    .device_suspend = usb_ehci_device_suspend,
    .set_hw_power = usb_ehci_set_power,
    .set_hw_power_sleep = usb_ehci_sleep,
    .roothub_exec = usb_ehci_roothub_exec,
    .xfer_poll = usb_ehci_do_poll,
    .xfer_finished = usb_ehci_xfer_is_finished,
};

/**
 * \brief this function returns a pointer to the ehci hcdi functions
 *
 * \return pointer to struct of function pointers
 */
struct usb_hcdi_bus_fn *usb_ehci_get_bus_fn(void)
{
    return (&usb_ehci_bus_fn);
}

/**
 * \brief sets the ehci controller into sleep mode or wakes it up
 *
 * \param hc    the host controller to sleep / wakeup
 * \param state the state to put the host controller into
 *
 * Upon resuming all the connected devices are also resumed
 */
void usb_ehci_sleep(struct usb_host_controller *hc, uint32_t state)
{
    assert(hc->hc_type == USB_EHCI);

    usb_ehci_hc_t *ehci_hc = hc->hc_control;

    switch (state) {
        case USB_POWER_MODE_SUSPEND:
        case USB_POWER_MODE_OFF:
            /* to suspend the hardware this is just a reset of the
             * host controller
             */
            usb_ehci_hc_reset(ehci_hc);
            break;
        case USB_POWER_MODE_RESUME:
            /* resume: first reset the host controller hardware */
            usb_ehci_hc_reset(ehci_hc);

            /* re-initialize the registers */
            usb_ehci_initialize_controller(ehci_hc);

            /* poll the transfers */
            usb_ehci_do_poll(hc);
            break;
        default:
            /* noop */
            break;
    }
}

/**
 * \brief sets the hardware to a specific power mode
 *
 * \param hc the host controller to change power mode
 *
 * This functio updates the ehci_usbcmd register to its a new value, if there
 * are periodic or asynchronous transfers happening, these are finished first
 */
void usb_ehci_set_power(struct usb_host_controller *hc)
{
    assert(!"NYI: set power");
}

/**
 * \brief resumes a suspended USB device on this host controller
 *
 * \param device the usb devices to to resume
 *
 * All of the already set up usb xfers are also resumed
 */
void usb_ehci_device_resume(struct usb_device *device)
{
    assert(device->controller->hc_type == USB_EHCI);

    struct usb_xfer_queue *queue = &device->controller->intr_queue;
    struct usb_xfer *xfer = queue->head.first;

    while (xfer) {
        /* just handle the xfers that are belonging to this device */
        if (xfer->device == device) {
            usb_ehci_hc_t *hc = device->controller->hc_control;
            switch (xfer->type) {
                case USB_TYPE_BULK:
                case USB_TYPE_CTRL:
                    usb_ehci_enq_qh(
                            xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set],
                            hc->qh_async_last);
                    break;
                case USB_TYPE_INTR:
                    usb_ehci_enq_qh(
                            xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set],
                            hc->qh_intr_last[xfer->intr_qh_pos]);
                    break;
                default:
                    /* noop */
                    break;
            }
        }
        xfer = xfer->wait_entry.next;
    }
}

/**
 * \brief suspends a attached USB device by pausing all its transfers
 *
 * \param device the usb device to suspend
 */
void usb_ehci_device_suspend(struct usb_device *device)
{
    assert(device->controller->hc_type == USB_EHCI);

    struct usb_xfer_queue *queue = &device->controller->intr_queue;
    struct usb_xfer *xfer = queue->head.first;

    while (xfer) {
        if (xfer->device == device) {
            usb_ehci_hc_t *hc = device->controller->hc_control;
            switch (xfer->type) {
                case USB_TYPE_BULK:
                case USB_TYPE_CTRL:
                    usb_ehci_deq_qh(
                            xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set],
                            hc->qh_async_last);
                    break;
                case USB_TYPE_INTR:
                    usb_ehci_deq_qh(
                            xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set],
                            hc->qh_intr_last[xfer->intr_qh_pos]);
                    break;
                default:
                    /* noop */
                    break;
            }

        }
        xfer = xfer->wait_entry.next;
    }

}


/**
 * \brief initializes a device endpoint with the values given by the descriptor
 *
 * \param device  the device, this endpoint belongs
 * \param ep_desc the descriptor of this endpoing
 * \param ep      the endpoint to initialize
 */
void usb_ehci_endpoint_init(struct usb_device *device,
        struct usb_endpoint_descriptor *ep_desc, struct usb_endpoint *ep)
{
    assert(device->controller->hc_type == USB_EHCI);

    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) device->controller->hc_control;

    /*
     * CASE 1: The device is the root hub. Setting up endpoints for the root
     *         hub is not allowed, thus return
     */
    if (device->device_index == hc->rh_device_address) {
        return;
    }

    /*
     * CASE 2: the attached device is not a high speed device and is directly
     *         connected to the root hub, this is not possible, since non
     *         full speed devices are handled by the companion controller.
     *         (root hub does not have a transaction translator)
     */
    if (device->speed != USB_SPEED_HIGH) {
        if ((device->hs_hub_address == 0) || (device->hs_hub_port_number == 0)
                || (device->parent_hs_hub == NULL)
                || (device->parent_hs_hub->hub == NULL)) {
            return;
        }
    }

    /* CASE 3: all fine setup the pipe functions depending on the EP type */
    switch (ep_desc->bmAttributes.xfer_type) {
        case USB_ENDPOINT_TYPE_CONTROL:
            ep->pipe_fn = usb_ehci_get_ctrl_pipe_fn();
            break;
        case USB_ENDPOINT_TYPE_INTR:
            ep->pipe_fn = usb_ehci_get_intr_pipe_fn();
            break;
        case USB_ENDPOINT_TYPE_ISOCHR:
            /*
             * isochronus endpoints need differentiated handling depending on
             * the device speed
             */
            if (device->speed == USB_SPEED_HIGH) {
                ep->pipe_fn = usb_ehci_get_hs_isoc_pipe_fn();
            } else if (device->speed == USB_SPEED_FULL) {
                ep->pipe_fn = usb_ehci_get_fs_isoc_pipe_fn();
            }
            break;
        case USB_ENDPOINT_TYPE_BULK:
            ep->pipe_fn = usb_ehci_get_bulk_pipe_fn();
            break;
        default:
            /* no-op */
            break;
    }
}

