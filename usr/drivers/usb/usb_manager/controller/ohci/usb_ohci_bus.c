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

#include "ohci_device.h"

#include <usb/usb.h>

#include <usb_xfer.h>
#include <usb_device.h>
#include <usb_controller.h>
#include <usb_hub.h>

#include <usb_memory.h>

#include "usb_ohci.h"
#include "usb_ohci_root_hub.h"
#include "usb_ohci_bus.h"


#include "usb_ohci_xfer.h"
#include "usb_ohci_pipe.h"
#include "usb_ohci_queue.h"



/*
 * ------------------------------------------------------------------------
 * OHCI Bus Functions
 * ------------------------------------------------------------------------
 */

/*
 * \brief   this function polls the interrupt queue and checks for each element
 *          if the transfer has been completed. If so the transfer is
 *          removed from the list
 *
 * \param   hc  pointer to the host controller
 */
void usb_ohci_do_poll(struct usb_host_controller *hc)
{
    struct usb_xfer *xfer;

    uint8_t repeat = 1;

    while (repeat) {
        repeat = 0;

        for ((xfer) = (((&hc->intr_queue.head))->first); (xfer); (xfer) =
                (((xfer))->wait_entry.next)) {
            /*
             * check if transfer is transferred
             */
            if (usb_ohci_xfer_is_finished(xfer)) {
                /* queue has been modified */
                repeat = 1;
            }
        }
    }
}

/*
 * \brief   this function initializes the endpoint with the correct
 *          pipe functions
 *
 * \param   device  the device the endpoint belongs to
 * \param   ep_desc the description of the endpoint
 * \param   ep      the endpoint
 */
static void usb_ohci_ep_init(struct usb_device *device,
        struct usb_endpoint_descriptor *ep_desc, struct usb_endpoint *ep)
{
    if (device->flags.usb_mode != USB_MODE_HOST) {
        /* this usb device mode is not supported */
        return;
    }

    usb_ohci_hc_t *hc = (usb_ohci_hc_t *) device->controller->hc_control;

    /*
     * we can only initialize endpoints for function devices
     */
    if (device->device_index != hc->root_hub_address) {
        switch (ep_desc->bmAttributes.xfer_type) {
            case USB_ENDPOINT_TYPE_CONTROL:
                ep->pipe_fn = usb_ohci_get_ctrl_pipe_fn();
                break;
            case USB_ENDPOINT_TYPE_INTR:
                ep->pipe_fn = usb_ohci_get_intr_pipe_fn();
                break;
            case USB_ENDPOINT_TYPE_ISOCHR:
                if (device->speed == USB_SPEED_FULL) {
                    ep->pipe_fn = usb_ohci_get_isoc_pipe_fn();
                }
                break;
            case USB_ENDPOINT_TYPE_BULK:
                ep->pipe_fn = usb_ohci_get_bulk_pipe_fn();
                break;
            default:
                /* transfer type unkown, do nothing */
                break;
        }
    }

}

/**
 * \brief   this function resumes an USB device, this function takes
 *          inserts the USB transfers to their respective transfer lists
 *
 * \param   device  the device to suspend
 */
static void usb_ohci_device_resume(struct usb_device *device)
{
    struct usb_xfer *xfer;
    usb_ohci_hc_t *hc;
    usb_ohci_ed_t *ed;

    hc = (usb_ohci_hc_t *) (device->controller->hc_control);

    for ((xfer) = (((&device->controller->intr_queue.head))->first); (xfer);
            (xfer) = (((xfer))->wait_entry.next)) {
        ed = xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set];
        switch (xfer->type) {
            case USB_TYPE_BULK:
                usb_ohci_append_qh(ed, hc->qh_bulk_last);
                ohci_cmdstatus_blf_wrf(hc->ohci_base, 0x1);
                break;
            case USB_TYPE_CTRL:
                usb_ohci_append_qh(ed, hc->qh_ctrl_last);
                ohci_cmdstatus_clf_wrf(hc->ohci_base, 0x1);
                break;

            case USB_TYPE_INTR:
                usb_ohci_append_qh(ed, hc->qh_intr_last[xfer->intr_qh_pos]);
                break;
            default:
                /* noop */
                break;
        }
    }

}

/**
 * \brief   this function suspends an USB device. the outstanding transfers
 *          for this device are descheduled and inserted into the wait list
 *
 * \param   device the device to resume
 */
static void usb_ohci_device_suspend(struct usb_device *device)
{
    struct usb_xfer *xfer;
    usb_ohci_hc_t *hc;
    usb_ohci_ed_t *ed;

    hc = (usb_ohci_hc_t *) (device->controller->hc_control);

    for ((xfer) = (((&device->controller->intr_queue.head))->first); (xfer);
            (xfer) = (((xfer))->wait_entry.next)) {
        /*
         * only remove those transfers that belong to the given device
         */
        if (xfer->device == device) {
            ed = xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set];
            switch (xfer->type) {
                case USB_TYPE_BULK:
                    usb_ohci_remove_qh(ed, hc->qh_bulk_last);
                    break;
                case USB_TYPE_CTRL:
                    usb_ohci_remove_qh(ed, hc->qh_ctrl_last);
                    break;

                case USB_TYPE_INTR:
                    usb_ohci_remove_qh(ed, hc->qh_intr_last[xfer->intr_qh_pos]);
                    break;
                default:
                    /* noop */
                    break;
            }
        }

    }
}

static void usb_ohci_set_hw_power(struct usb_host_controller *controller)
{
    /*
         * TODO: implement
         */
        assert(!"NYI: Power control not implemented");
}

static void usb_ohci_set_hw_power_sleep(struct usb_host_controller *controller, uint32_t state)
{
    /*
     * TODO: implement
     */
    assert(!"NYI: Power control not implemented");
}



static struct usb_hcdi_bus_fn usb_ohci_bus_fn = {
        .roothub_exec = usb_ohci_roothub_exec,
        .endpoint_init = usb_ohci_ep_init,
            .xfer_setup = usb_ohci_xfer_setup,
            .xfer_unsetup = usb_ohci_xfer_unsetup,
            .device_resume = usb_ohci_device_resume,
            .device_suspend = usb_ohci_device_suspend,
            .set_hw_power = usb_ohci_set_hw_power,
            .set_hw_power_sleep = usb_ohci_set_hw_power_sleep,
            .xfer_poll = usb_ohci_do_poll
};

struct usb_hcdi_bus_fn *usb_ohci_get_bus_fn(void)
{
    return &usb_ohci_bus_fn;
}

