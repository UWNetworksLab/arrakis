/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <barrelfish/barrelfish.h>

#include <usb/usb.h>

#include <usb_controller.h>
#include <usb_xfer.h>
#include <usb_device.h>
#include <usb_endpoint.h>
#include <usb_pipe.h>

/**
 * \brief goes through the endpoints of a device and returns the one that
 *        matches the criteria
 *
 * \param device the device we want to consider for the lookup
 * \param iface  the interface in which we want to look for
 * \param filter filter defining which endpoint we want
 *
 * \return pointer to the endpoint on success
 *         NULL if no endpoint is fould
 *
 * NOTE: Filter may contain a specific direction, endpoint number or type.
 *       Each filter criteria may also be "ANY"
 */
struct usb_endpoint *usb_endpoint_lookup(struct usb_device *device,
        uint8_t iface, const struct usb_xfer_config *filter)
{

    uint8_t ep_index = 0;

    struct usb_endpoint *ep = device->endpoints;

    usb_endpoint_address_t *epaddr;
    usb_endpoint_attributes_t *epattr;

    uint8_t any = 1;

    /* loop over all endpoints */
    for (uint8_t ep_current = 0; ep_current < device->ep_max; ep_current++) {
        if (ep == NULL) {
            /* no endpoint allocated, continue with next */
            continue;
        }

        if ((ep->descriptor == NULL) || (ep->iface_index != iface)) {
            /* there is no descriptor or the wrong interface */
            continue;
        }

        /* we have a valid endpoint sofar, so check filter criteria */
        epaddr = &(ep->descriptor->bEndpointAddress);
        epattr = &(ep->descriptor->bmAttributes);

        /* check for direction */
        if (filter->direction != USB_ENDPOINT_DIRECTION_ANY) {
            any = 0;
            if (epaddr->direction != filter->direction) {
                /* wrong direction, check next next */
                continue;
            }
        }

        /* checking matching endpoint address */
        if (filter->endpoint != USB_ENDPOINT_ADDRESS_ANY) {
            any = 0;
            if (epaddr->ep_number != filter->endpoint) {
                /* wrong endpoint address */
                continue;
            }
        }

        /* checking the matching type */
        if (filter->type != USB_ENDPOINT_TYPE_ANY) {
            any = 0;
            if (epattr->xfer_type != filter->type) {
                /* wrong xfer type */
                continue;
            }
        }

        if (!(ep_index--) && !any) {
            USB_DEBUG_XFER("Endpoint found: iface=0x%x, ep=0x%x\n",
                    iface, ep->endpoint_address);
            return (ep);
        }

        ep++;
    }

    /* the default control endpoint is not located in the endpoints array */
    if (device->ctrl_ep.descriptor && any && !ep_index) {
        USB_DEBUG_XFER("usb_endpoint_lookup(): found default ctrl ep\n");
        return (&device->ctrl_ep);
    }

    /* no matching endpoint found */
    return (NULL);
}

/**
 * \brief initializes the endpoint with the correct data
 *
 * \param device the usb device this endpoint belongs to
 * \param iface_index the interface index this endpoint belongs to
 * \param desc the endpoint descriptor of this endpoint
 * \param ep the endpoint to initialize
 */
void usb_endpoint_init(struct usb_device *device, uint8_t iface_index,
        struct usb_endpoint_descriptor *desc, struct usb_endpoint *ep)
{
    USB_DEBUG_TR_ENTER;

    /*
     * call the endpoint init function of the host controller in a save way
     * This sets the pipe function pointers to the correspinding transfer type
     */
    struct usb_hcdi_bus_fn *bus_fn = device->controller->hcdi_bus_fn;

    if (bus_fn && bus_fn->endpoint_init) {
        (bus_fn->endpoint_init)(device, desc, ep);
    }

    /* set the values of this endpoint */
    ep->descriptor = desc;
    ep->iface_index = iface_index;
    ep->endpoint_address = desc->bEndpointAddress.ep_number;
    ep->max_packet_size = desc->wMaxPacketSize;

    /* initialze the endpoint queue */
    ep->transfers.head.first = NULL;
    ep->transfers.head.last_next = &(ep->transfers.head.first);
    ep->transfers.command = &usb_pipe_start;

    if (ep->pipe_fn == NULL) {
        return;
    }

    /* some devices may need a clear stall on new endpoints */
    if (bus_fn->clear_stall != NULL) {
        (bus_fn->clear_stall)(device, ep);
    }
}
