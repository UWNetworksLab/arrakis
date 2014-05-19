/*
 * \brief This file contains class specific functions for USB hub devices
 *
 *        USB Device Class 0x09
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * ==========================================================================
 * XXX: This is currently unused, since the hub driver is inside the USB
 *      Manager!
 * ==========================================================================
 */

#include <stdlib.h>
#include <barrelfish/barrelfish.h>

#include <usb/usb_request.h>
#include <usb/usb_transfer.h>
#include <usb/class/usb_hub.h>

/**
 * \brief     This request resets a value reported in the hub status.
 *
 * \param     feature the hub feature to clear USB_HUB_FEATURE_*
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_BAD_REQUEST if feature selector is not USB_HUB_FEATURE_*
 *
 * Clearing a feature means disabling that feature. If the feature to clear
 * is a status change, then clearing means acknowledging the change. This
 * request format is used to clear either the C_HUB_LOCAL_POWER or
 * C_HUB_OVER_CURRENT features.
 *
 * The behavior if the hub is not configured is undefined
 */
usb_error_t usb_hub_clear_hub_feature(uint16_t feature)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bRequest = USB_HUB_REQ_CLEAR_FEATURE;
    switch (feature) {
        // only these two features are allowed to be cleared by this request
        case USB_HUB_FEATURE_C_HUB_LOCAL_POWER:
        case USB_HUB_FEATURE_C_HUB_OVER_CURRENT:
            req.wValue = feature;
            break;
        default:
            return USB_ERR_BAD_REQUEST;
            break;
    }
    req.wIndex = 0;
    req.wLength = 0;

    return (usb_do_request(&req));
}

/**
 * \brief     This request resets a value reported in the port status.
 *
 * \param     feature the hub feature to clear USB_HUB_FEATURE_*
 * \param    port    a valid port number for that hub
 * \param    sel        port indicator selector when clearing a port indicator.
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_BAD_REQUEST if feature selector is not USB_HUB_FEATURE_*
 *
 * Clearing a feature disables that feature or triggers the start of a process
 * associated with that feature. Clearing a status change feature means
 * acknowleding the status change. Clearing an already cleared feature is
 * treated as a no-op.
 *
 * Clearing the PORT_SUSPEND feature causes a host-initiated resume on
 * the specified port.
 *
 * Clearing the PORT_ENABLE feature causes the port to be placed in the
 * Disabled state.
 *
 * Clearing the PORT_POWER feature causes the port to be placed in the
 * Powered-off state and may, subject to the constraints due to the hub’s
 * method of power switching, result in power being removed from the port.
 *
 * The behavior if the hub is not configured is undefined
 */
usb_error_t usb_hub_clear_port_feature(uint16_t feature, uint8_t sel,
        uint8_t port)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bRequest = USB_HUB_REQ_CLEAR_FEATURE;
    req.wIndex = (0x00FF & port);
    switch (feature) {
        // only these features are supported by this request
        case USB_HUB_FEATURE_PORT_ENABLE:
        case USB_HUB_FEATURE_PORT_SUSPEND:
        case USB_HUB_FEATURE_PORT_POWER:
        case USB_HUB_FEATURE_PORT_INDICATOR:
        case USB_HUB_FEATURE_C_PORT_CONNECTION:
        case USB_HUB_FEATURE_C_PORT_RESET:
        case USB_HUB_FEATURE_C_PORT_ENABLE:
        case USB_HUB_FEATURE_C_PORT_SUSPEND:
        case USB_HUB_FEATURE_C_PORT_OVER_CURRENT:
            req.wValue = feature;
            break;
        default:
            return (USB_ERR_BAD_REQUEST);
            break;
    }
    req.wLength = 0;

    if (feature == USB_HUB_FEATURE_PORT_INDICATOR) {
        req.wIndex = req.wIndex | (0xFF00 & (sel << 8));
    }

    return (usb_do_request(&req));
}

/**
 * \brief   This request clears the state of a Transaction Translator(TT)
 *             bulk/control buffer after it has been left in a busy state due to
 *             high-speed errors. This request is only defined for non-periodic
 *             endpoints.
 *
 * \param     dev_addr the hub feature to clear USB_HUB_FEATURE_*
 * \param    ep_num    a valid port number for that hub
 * \param    tt_port    the port number if multiple TTs are supported, or 1 else.
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_BAD_REQUEST if feature selector is not USB_HUB_FEATURE_*
 *
 *
 *
 * The behavior if the hub is not configured or applied to a periodic endpoint
 * is undefined.
 */
usb_error_t usb_hub_clear_tt_buffer(uint8_t dev_addr, uint8_t ep_num,
        uint8_t ep_type, uint8_t direction, uint16_t tt_port)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bRequest = USB_HUB_REQ_CLEAR_FEATURE;
    /*
     * bits 3..0     endpoint number
     * bits 10..4    device address
     * bits 12..11    endpoint type
     * bits 14..13    reserved
     * bits 15        direction, 1=IN, 0=OUT
     */
    req.wValue = (0x000F & ep_num) | ((0x007F & dev_addr) << 4);
    req.wValue |= ((0x0003 & ep_type) << 11);
    req.wValue |= ((0x0001 & direction) << 15);
    req.wIndex = tt_port;
    req.wLength = 0;

    return (usb_do_request(&req));;
}

/**
 * \brief    This request returns the hub descriptor. For other descriptors
 *           than hub descriptors use the standard usb_hub_get_descriptor().
 *
 * \param    ret_desc    the returned hub descriptor
 * \param    num_ports    the number of ports of the hub
 *
 * \return   USB_ERR_OK on success
 *           USB_ERR_* on failure
 *
 * All hubs are required to implement one hub descriptor, with descriptor
 * index zero.
 *
 * If the hub is not configured, the response to this request is undefined.
 */
usb_error_t usb_hub_get_hub_descriptor(uint16_t num_ports,
        struct usb_hub_descriptor **ret_desc)
{
    struct usb_device_request req;

    // calculate the length of the data expected
    // each port occupies one bit in the last field
    uint16_t len = (num_ports + 7 + USB_HUB_DESCRIPTOR_MIN_SIZE * 8) / 8;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bRequest = USB_HUB_REQ_GET_DESCRIPTOR;
    req.wValue = (USB_DESCRIPTOR_TYPE_HUB << 8) & 0xFF00;
    req.wIndex = 0;
    req.wLength = len;

    uint16_t ret_length;
    void *ret_data;
    usb_error_t err = usb_do_request_read(&req, &ret_length, &ret_data);

    if (err != USB_ERR_OK) {
        return (err);
    }

    if (ret_length != sizeof(struct usb_hub_descriptor)) {
        return (USB_ERR_INVAL);
    }

    if (ret_desc != NULL) {
        *ret_desc = (struct usb_hub_descriptor *) ret_data;
    }

    return (USB_ERR_OK);
}

/**
 * \brief     This request returns the current hub status and the states that
 *             have changed since the previous acknowledgment.
 *
 * \param     ret_status    the returned hub status
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_* on failure
 *
 * If the hub is not configured, the hub’s response to this request is undefined.
 */
usb_error_t usb_hub_get_hub_status(struct usb_hub_status *ret_status)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bRequest = USB_HUB_REQ_GET_STATUS;
    req.wValue = 0;
    req.wIndex = 0;
    req.wLength = 4;

    uint16_t ret_length;
    void *ret_data;
    usb_error_t err = usb_do_request_read(&req, &ret_length, &ret_data);

    if (err != USB_ERR_OK) {
        return (err);
    }

    if (ret_length != 4) {
        return (USB_ERR_INVAL);
    }

    if (ret_status != NULL) {
        *ret_status = *((struct usb_hub_status *) ret_data);
    }

    return (USB_ERR_OK);
}

/**
 * \brief     This request returns the current port status and the current value
 *             of the port status change bits.
 *
 * \param    port        the port number we want to get the status
 * \param     ret_status    the returned port status
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_* on failure
 *
 * The port number must be a valid port number for that hub, greater than zero.
 *
 * If the hub is not configured, the hub’s response to this request is undefined.
 */
usb_error_t usb_hub_get_port_status(uint16_t port,
        struct usb_hub_port_status *ret_status)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bRequest = USB_HUB_REQ_GET_STATUS;
    req.wValue = 0;
    req.wIndex = port;
    req.wLength = 4;

    uint16_t ret_length;
    void *ret_data;
    usb_error_t err = usb_do_request_read(&req, &ret_length, &ret_data);

    if (err != USB_ERR_OK) {
        return (err);
    }

    if (ret_length != 4) {
        return (USB_ERR_INVAL);
    }

    if (ret_status != NULL) {
        *ret_status = *((struct usb_hub_port_status *) ret_data);
    }

    return (USB_ERR_OK);
}

/**
 * \brief     This request returns the transaction translator in a hub to a
 *             known state.
 *
 * \param    port    the port for which we want to reset the TT
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_* on failure
 *
 * The port number must be a valid port number for that hub, greater than zero.
 * If the hub provides only a single TT, then Port must be set to one.
 *
 * Under some circumstances, a Transaction Translator (TT) in a hub may be in
 * an unknown state such that it is no longer functioning correctly. The
 * Reset_TT request allows the TT to be returned to the state it is in
 * immediately after the hub is configured.
 *
 * If the hub is not configured, the hub’s response to this request is undefined.
 */
usb_error_t usb_hub_reset_tt(uint16_t port)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bRequest = USB_HUB_REQ_RESET_TT;
    req.wValue = 0;
    req.wIndex = port;
    req.wLength = 0;

    return (usb_do_request(&req));
}

/**
 * \brief     This request overwrites the hub descriptor.
 *
 * \param    desc_length    the length of the hub descriptor
 * \param    desc        the new hub descriptor to write
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_* on failure
 *
 * All hubs are required to implement one hub descriptor with descriptor index
 * zero. This request writes the entire hub descriptor at once.
 *
 * If the hub is not configured, the hub’s response to this request is undefined.
 */
usb_error_t usb_hub_set_hub_descriptor(uint16_t desc_length,
        struct usb_hub_descriptor *desc)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bRequest = USB_HUB_REQ_SET_DESCRIPTOR;
    req.wValue = (USB_DESCRIPTOR_TYPE_HUB << 8) & 0xFF00;
    req.wIndex = 0;
    req.wLength = desc_length;

    // TODO: FLOUNDER CALL
    return (usb_do_request_write(&req, desc_length, desc));
}

/**
 * \brief     This request sets a value reported in the hub status.
 *
 * \param    feature    the feature we want to enable for the hub
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_* on failure
 *
 * Setting a feature enables that feature; Status changes may not be
 * acknowledged using this request.
 *
 * If the hub is not configured, the hub’s response to this request is undefined.
 */
usb_error_t usb_hub_set_hub_feature(uint16_t feature)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bRequest = USB_HUB_REQ_SET_FEATURE;
    req.wValue = feature;
    req.wIndex = 0;
    req.wLength = 0;

    // TODO: FLOUNDER CALL
    return (usb_do_request(&req));
}

/**
 * \brief     This request sets a value reported in the port status.
 *
 * \param    feature        the feature to set
 * \param     selector    selector for test modes or 0 otherwise
 * \param    port        the port we want to set the featre
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_* on failure
 *
 * The port number must be a valid port number for that hub, greater than zero.
 *
 * Setting a feature enables that feature or starts a process associated with
 * that feature.
 *
 * If the hub is not configured, the hub’s response to this request is undefined.
 */
usb_error_t usb_hub_set_port_feature(uint16_t feature, uint8_t selector,
        uint8_t port)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bRequest = USB_HUB_REQ_SET_FEATURE;

    switch (feature) {
        case USB_HUB_FEATURE_PORT_RESET:
        case USB_HUB_FEATURE_PORT_SUSPEND:
        case USB_HUB_FEATURE_PORT_POWER:
        case USB_HUB_FEATURE_PORT_TEST:
        case USB_HUB_FEATURE_PORT_INDICATOR:
        case USB_HUB_FEATURE_C_PORT_CONNECTION:
        case USB_HUB_FEATURE_C_PORT_RESET:
        case USB_HUB_FEATURE_C_PORT_ENABLE:
        case USB_HUB_FEATURE_C_PORT_SUSPEND:
        case USB_HUB_FEATURE_C_PORT_OVER_CURRENT:
            req.wValue = feature;
            break;
        default:
            return USB_ERR_BAD_REQUEST;
    };

    req.wIndex = (0x00FF & port);
    if (feature == USB_HUB_FEATURE_PORT_TEST) {
        req.wIndex |= ((0x00FF & selector) << 8);
    }
    req.wLength = 0;

    // TODO: FLOUNDER CALL
    return (usb_do_request(&req));
}

/**
 * \brief     This request returns the internal state of the transaction
 *             translator (TT) in a vendor specific format. A TT receiving this
 *             request must have first been stopped via the Stop_TT request. This
 *             request is provided for debugging purposes.
 *
 * \param    port        the port number we want to get the status
 * \param     flags        vendor dependent flags
 * \param    max_length    the maximum length of data to be returned
 * \param    ret_length    the length of the returned state
 * \param    ret_state    the state returned
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_* on failure
 *
 * The port number must be a valid port number for that hub, greater than zero.
 *
 * f wLength is larger than the actual length of this request, then only the
 * actual length is returned. If wLength is less than the actual length of this
 * request, then only the first wLength bytes of this request are returned;
 *
 * If the hub is not configured, the hub’s response to this request is undefined.
 */
usb_error_t usb_hub_get_tt_state(uint16_t flags, uint16_t port,
        uint16_t max_length, uint16_t ret_length, void **ret_state)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bRequest = USB_HUB_REQ_GET_TT_STATE;
    req.wValue = flags;
    req.wIndex = port;
    req.wLength = max_length;

    void *ret_data;
    usb_error_t err = usb_do_request_read(&req, &ret_length, &ret_data);

    if (err != USB_ERR_OK) {
        return (err);
    }

    if (ret_length != 4) {
        return (USB_ERR_INVAL);
    }

    if (ret_state != NULL) {
        *ret_state = ret_data;
    }

    return (USB_ERR_OK);
}

/**
 * \brief     This request stops the normal execution of the transaction
 *             translator so that the internal TT state can be retrieved via
 *             Get_TT_State. This request is provided for debugging purposes.
 *
 * \param    port    the port number for which the TT shall be stopped
 *
 * \return    USB_ERR_OK on success
 *             USB_ERR_* on failure
 *
 * The only standardized method to restart a TT after a Stop_TT request is via
 * the Reset_TT request.
 *
 * If the hub supports multiple TTs, then wIndex must specify the port number
 * of the TT that is being stopped. If the hub provides only a single TT, then
 * Port must be set to one. For a single TT Hub, the Hub can ignore the Port
 * number.
 *
 * If the hub is not configured, the hub’s response to this request is undefined.
 */
usb_error_t usb_hub_stop_tt(uint16_t port)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.bType.recipient = USB_REQUEST_RECIPIENT_OTHER;
    req.bRequest = USB_HUB_REQ_STOP_TT;
    req.wValue = 0;
    req.wIndex = port;
    req.wLength = 0;

    // TODO: FLOUNDER CALL
    return (usb_do_request(&req));
}
