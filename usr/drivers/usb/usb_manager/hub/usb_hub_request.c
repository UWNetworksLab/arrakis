/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
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

#include <usb/usb.h>

#include <usb/usb_request.h>

#include <usb_device.h>
#include <usb_hub.h>
#include <usb_request.h>

usb_error_t usb_hub_clear_hub_feature(struct usb_device *hub, uint16_t feature)
{
    struct usb_device_request req;
    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bRequest = USB_HUB_REQ_CLEAR_FEATURE;
    req.wValue = feature;
    req.wLength = 0;
    req.wIndex = 0;
    return (usb_exec_request(hub, 0, &req, NULL, NULL));
}

usb_error_t usb_hub_clear_port_feature(struct usb_device *hub, uint16_t feature,
        uint8_t port)
{
    struct usb_device_request req;
    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bRequest = USB_HUB_REQ_CLEAR_FEATURE;
    req.wValue = feature;
    req.wLength = 0;
    req.wIndex = port;
    return (usb_exec_request(hub, 0, &req, NULL, NULL));
}

usb_error_t usb_hub_clear_tt_buffer(struct usb_device *hub, uint8_t dev_addr,
        uint8_t ep_num, uint8_t ep_type, uint8_t direction, uint16_t tt_port)
{
    struct usb_device_request req;

    if (hub->device_desc.bDeviceClass == USB_HUB_CLASS_CODE
            && hub->device_desc.bDeviceProtocol == USB_HUB_PROTOCOL_HSHUBSTT) {
        /*
         * if there is just a single transaction translator in the hub,
         * the port should be 1
         */
        tt_port = 1;
    }

    uint16_t wValue = (ep_num & 0xF) | ((dev_addr & 0x7F) << 4)
            | ((ep_num & 0x80) << 8) | ((ep_type & 0x3) << 12);

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bRequest = USB_HUB_REQ_CLEAR_TT_BUFFER;
    req.wValue = wValue;
    req.wLength = 0;
    req.wIndex = tt_port;
    return (usb_exec_request(hub, 0, &req, NULL, NULL));
}

usb_error_t usb_hub_get_hub_status(struct usb_device *hub,
        struct usb_hub_status *ret_status)

{
    USB_DEBUG_TR_ENTER;
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bRequest = USB_HUB_REQ_GET_STATUS;
    req.wValue = 0;
    req.wLength = sizeof(struct usb_hub_status);
    req.wIndex = 0;
    return (usb_exec_request(hub, 0, &req, ret_status, NULL));
}

usb_error_t usb_hub_get_port_status(struct usb_device *hub, uint16_t port,
        struct usb_hub_port_status *ret_status)
{
    USB_DEBUG_TR_ENTER;

    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bRequest = USB_HUB_REQ_GET_STATUS;
    req.wValue = 0;
    req.wLength = sizeof(struct usb_hub_port_status);
    req.wIndex = port;
    return (usb_exec_request(hub, 0, &req, ret_status, NULL));
}

usb_error_t usb_hub_reset_tt(struct usb_device *hub, uint16_t port)
{
    USB_DEBUG_TR_ENTER;

    struct usb_device_request req;

    if (hub->device_desc.bDeviceClass == USB_HUB_CLASS_CODE
            && hub->device_desc.bDeviceProtocol == USB_HUB_PROTOCOL_HSHUBSTT) {
        /*
         * if there is just a single transaction translator in the hub,
         * the port should be 1
         */
        port = 1;
    }

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bRequest = USB_HUB_REQ_RESET_TT;
    req.wValue = 0;
    req.wLength = 0;
    req.wIndex = port;
    return (usb_exec_request(hub, 0, &req, NULL, NULL));
}

usb_error_t usb_hub_set_hub_feature(struct usb_device *hub, uint16_t feature)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bRequest = USB_HUB_REQ_SET_FEATURE;
    req.wValue = feature;
    req.wLength = 0;
    req.wIndex = 0;
    return (usb_exec_request(hub, 0, &req, NULL, NULL));
}

usb_error_t usb_hub_set_port_feature(struct usb_device *hub, uint16_t feature,
        uint8_t port)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bRequest = USB_HUB_REQ_SET_FEATURE;
    req.wValue = feature;
    req.wLength = 0;
    req.wIndex = port;
    return (usb_exec_request(hub, 0, &req, NULL, NULL));
}

usb_error_t usb_hub_get_tt_state(struct usb_device *hub, uint16_t flags,
        uint16_t port, uint16_t max_length, uint16_t ret_length,
        void *ret_state)
{
    assert(!"NYI: usb_hub_get_tt_state()\n");
    return (USB_ERR_BAD_REQUEST);
}

usb_error_t usb_hub_stop_tt(struct usb_device *hub, uint16_t port)
{
    assert(!"NYI: usb_hub_stop_tt()\n");
    return (USB_ERR_BAD_REQUEST);
}

usb_error_t usb_hub_get_hub_descriptor(struct usb_device *hub, uint16_t nports,
        struct usb_hub_descriptor *ret_desc)
{
    USB_DEBUG_TR_ENTER;
    struct usb_device_request req;

    uint16_t wLength = (nports + 7 + (8 * 8)) / 8;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bRequest = USB_HUB_REQ_GET_DESCRIPTOR;
    req.wValue = USB_DESCRIPTOR_TYPE_HUB<<8;
    req.wLength = wLength;
    req.wIndex = 0;
    return (usb_exec_request(hub, 0, &req, ret_desc, NULL));
}

usb_error_t usb_hub_set_hub_descriptor(struct usb_device *hub,
        uint16_t desc_length, struct usb_hub_descriptor *desc)
{
    return (USB_ERR_BAD_REQUEST);
}

usb_error_t usb_hub_re_enumerate(struct usb_device *hub)
{
    return (USB_ERR_BAD_REQUEST);
}

usb_error_t usb_hub_reset_port(struct usb_device *hub, uint8_t port)
{
    USB_DEBUG_TR_ENTER;
    usb_error_t err;
    struct usb_hub_port_status ps;

    /* clear port reset changes (if any) */
    err = usb_hub_clear_port_feature(hub, USB_HUB_FEATURE_C_PORT_RESET, port);
    if (err != USB_ERR_OK) {
        USB_DEBUG("ERROR: could not clear port reset on port %u\n", port);
        USB_DEBUG_TR_RETURN;
        return (err);
    }

    /* initate the rest sequence */
    err = usb_hub_set_port_feature(hub, USB_HUB_FEATURE_PORT_RESET, port);
    if (err != USB_ERR_OK) {
        USB_DEBUG("ERROR: port reset could not reset port %u\n", port);
        USB_DEBUG_TR_RETURN;
        return (err);
    }

    uint16_t timeout = 0;

    /* wait till the reset sequence is over */
    while (1) {
        USB_WAIT(USB_DELAY_PORT_RESET);

        timeout += USB_DELAY_PORT_RESET;

        err = usb_hub_get_port_status(hub, port, &ps);
        if (err != USB_ERR_OK) {
            USB_DEBUG("ERROR: could not get port status\n");
            USB_DEBUG_TR_RETURN;
            return (err);
        }

        if (!ps.wPortStatus.connection) {
            /* the devie has disappeared, so give up */
            USB_DEBUG("NOTICE: Device %i has disappeared...\n", hub->device_address);
            USB_DEBUG_TR_RETURN;
            return (err);
        }

        if (ps.wPortChange.is_reset) {
            /* the reset sequence is over */
            break;
        }

        if (!ps.wPortStatus.reset) {
            /* check if reset is no longer asserted */
            break;
        }

        if (timeout > 10000) {
            timeout = 0;
            break;
        }
    }

    /*
     * clear the port reset feature, just in case it was not cleared
     * automatically
     */
    err = usb_hub_clear_port_feature(hub, USB_HUB_FEATURE_C_PORT_RESET, port);
    if (err != USB_ERR_OK) {
        USB_DEBUG("ERROR: Could not reset port feature\n");
        USB_DEBUG_TR_RETURN;
        return (err);
    }

    if (timeout == 0) {
        USB_DEBUG("ERROR: timeout happened during reset\n");
        USB_DEBUG_TR_RETURN;
        return (USB_ERR_TIMEOUT);
    }

    /* give the device time to recover from reset */
    USB_WAIT(USB_DELAY_PORT_RECOVERY);

    USB_DEBUG_TR_RETURN;
    return (err);
}

