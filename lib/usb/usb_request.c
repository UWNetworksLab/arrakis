/**
 * \brief This file contains the standard device requests from the USB
 * Specification Rev. 2.0 Section 9.4
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */



#include <usb/usb.h>
#include <usb/usb_request.h>


#include "usb_manager_client.h"

/**
 * \brief this request is used to clear or disable a specific feature.
 *
 * \param recipient the receiver of the event: USB_REQUEST_RECIPIENT_*
 * \param feature   the feature selector: USB_REQUEST_FEATURE_*
 *
 * \return USB_ERR_OK   on success
 *
 * Behaviour of the device upon receiving a request:
 *  - default state:    behaviour not specified
 *  - address state:    valid when recipient is the device or endpoint zero
 *                      other recpients (endpoints and interfaces) get a
 *                      USB_ERR_REQUEST
 *  - configured state: valid request forall recipients
 */
usb_error_t usb_clear_feature(uint8_t recipient, uint8_t recipient_index,
        uint16_t feature)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = (recipient & USB_REQUEST_RECIPIENT_MASK);
    req.bRequest = USB_REQUEST_CLEAR_FEATURE;
    req.wValue = feature;
    req.wIndex = 0;
    if (recipient != USB_REQUEST_RECIPIENT_DEVICE) {
        req.wIndex = recipient_index;
    }
    req.wLength = 0;

    return (usb_do_request(&req));
}

/**
 * \brief this request returns the current device configuration value
 *
 * \param *ret_config   the current configuration value if zero then
 *                      the device is not configured yet
 *
 * \return USB_ERR_OK   on success
 *
 * Behaviour of the device upon receiving a request:
 *  - default state:    behaviour not specified
 *  - address state:    must return the value zero
 *  - configured state: a non zero value must be returned
 */
usb_error_t usb_get_configuration(uint8_t *ret_config)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bRequest = USB_REQUEST_GET_CONFIG;
    req.wValue = 0;
    req.wIndex = 0;
    req.wLength = 1;

    uint16_t ret_length;
    void *ret_data;

    usb_error_t err = usb_do_request_read(&req, &ret_length, &ret_data);

    if (err != USB_ERR_OK) {
        return (err);
    }

    if (ret_length != 1) {
        return (USB_ERR_IOERROR);
    }

    if (ret_config) {
        *ret_config = *((uint8_t *)ret_data);
        free(ret_data);
    }

    return (USB_ERR_OK);
}

/**
 * \brief This request returns the specified descriptor if the descriptor exists.
 *
 * \param desc_type:    descriptor type to look for
 * \param desc_index:   which descirptor to return
 * \param lang:         zero or a language id for string descriptors
 * \param *ret_desc:
 *
 * Behaviour of the device upon receiving a request:
 *  - default state:    valid request
 *  - address state:    valid request
 *  - configured state: valid request
 */
usb_error_t usb_get_descriptor(uint8_t desc_type, uint8_t desc_index,
        uint16_t lang, void **ret_desc, uint16_t *ret_length)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bRequest = USB_REQUEST_GET_DESCRIPTOR;
    req.wValue = (desc_type << 8) | desc_index;

    /* wIndex stores the language id if a string descriptor is requested*/
    if (desc_type == USB_DESCRIPTOR_TYPE_STRING) {
        req.wIndex = lang;
    } else {
        req.wIndex = 0;
    }
    req.wIndex = lang;

    /*
     the maximum bytes to be returned, if length of the descriptor exceeds
     this value, only the first wLength bytes are returned.
     */
    req.wLength = 0;

    usb_descriptor_t *desc;
    uint16_t ret_data_length;

    usb_error_t err = usb_do_request_read(&req, &ret_data_length, (void **)&desc);

    if (err != USB_ERR_OK) {
        return (err);
    }

    /* we check for the size of the returned descriptors */
    switch (desc_type) {
        case USB_DESCRIPTOR_TYPE_DEVICE:
            if (ret_data_length != sizeof(struct usb_device_descriptor)) {
                return (USB_ERR_IOERROR);
            }
            break;
        case USB_DESCRIPTOR_TYPE_INTERFACE:
            if (ret_data_length != sizeof(struct usb_interface_descriptor)) {
                return (USB_ERR_IOERROR);
            }
            break;
        case USB_DESCRIPTOR_TYPE_ENDPOINT:
            if (ret_data_length != sizeof(struct usb_endpoint_descriptor)) {
                return (USB_ERR_IOERROR);
            }
            break;
        case USB_DESCRIPTOR_TYPE_DEVICE_QUALIFIER:
            if (ret_data_length
                    != sizeof(struct usb_device_qualifier_descriptor)) {
                return (USB_ERR_IOERROR);
            }
            break;
        default:
            /* variable sized descriptors */
            if(ret_data_length != desc->bLength) {
                return (USB_ERR_IOERROR);
            }
            break;
    }

    /* ok all should be fine */

    if (ret_length) {
        *ret_length = ret_data_length;
    }

    if (ret_desc) {
        *ret_desc = desc;
    }

    return (USB_ERR_OK);

}

/* XXX: The following functions serve as an easier way to get a specific
 *      descriptor. In fact they are not really needed and the functionality
 *      can be done using the generic get_descriptor request
 */
usb_error_t usb_get_config_descriptor(uint8_t config_index,
        struct usb_config_descriptor *ret_desc)
{
    assert(!"NYI: usb_get_config_descriptor");
    return (USB_ERR_BAD_REQUEST);
}

usb_error_t usb_get_iface_descriptor(uint8_t iface_index,
        struct usb_config_descriptor *ret_desc)
{
    assert(!"NYI: usb_get_iface_descriptor");
    return (USB_ERR_BAD_REQUEST);
}

usb_error_t usb_get_ep_descriptor(uint8_t ep_index,
        struct usb_endpoint_descriptor *ret_desc)
{
    assert(!"NYI: usb_get_ep_descriptor");
    return (USB_ERR_BAD_REQUEST);
}

usb_error_t usb_get_string_descriptor(uint16_t lang_id, uint8_t string_index,
        void *ret_desc)
{
    assert(!"NYI: usb_get_string_descriptor");
    return (USB_ERR_BAD_REQUEST);
}

/**
 * \brief This requests returns the selected alternate setting for the
 *        specified interface
 *
 * \param iface_number:     the interface we want to get the alternate
 *                          setting
 * \param ret_alt_iface:    the alternate setting value
 *
 * \return USB_ERR_OK on success
 *            USB_ERR_* on failure
 *
 * Behaviour of the device upon receiving a request:
 *  - default state:    not specified
 *  - address state:    USB_ERR_REQUEST
 *  - configured state: valid request
 *
 * If the interface does not exists then the device will reply with a
 * USB_ERR_REQUEST
 */
usb_error_t usb_get_alt_iface(uint16_t iface_number, uint8_t *ret_alt_iface)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = USB_REQUEST_RECIPIENT_INTERFACE;
    req.bRequest = USB_REQUEST_GET_INTERFACE;
    req.wValue = 0;
    req.wIndex = (0x00FF & iface_number);
    req.wLength = 1;

    usb_error_t err = USB_ERR_OK;
    void *ret_val;
    uint16_t ret_length;

    err = usb_do_request_read(&req, &ret_length, &ret_val);

    if (err != USB_ERR_OK) {
        return (err);
    }

    if (ret_length != 1) {
        return (USB_ERR_IOERROR);
    }

    if (ret_alt_iface) {
        *ret_alt_iface = *((uint8_t*) (ret_val));
    }

    return (USB_ERR_OK);
}

/**
 * \brief This request returns the status for the specified recipient
 *
 * \param recipient_index:   zero for device status, the endpoint or interface
 *                          number for the respective recipient
 * \param ret_status:       the status of the recipient
 *
 * \return USB_ERR_OK on success
 *
 * Behaviour of the device upon receiving a request:
 *  - default state:    not specified
 *  - address state:    USB_ERR_REQUEST
 *  - configured state: valid request
 *
 * If the interface or endpoint does not exists then the device will reply
 * with a USB_ERR_REQUEST
 */
usb_error_t usb_get_status(uint8_t recipient, uint16_t recipient_index,
        struct usb_status *ret_status)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = (recipient & USB_REQUEST_RECIPIENT_MASK);
    req.bRequest = USB_REQUEST_GET_STATUS;
    req.wValue = 0;

    req.wIndex = recipient_index;
    req.wLength = 2;

    void *ret_data;
    uint16_t ret_length;

    usb_error_t err = usb_do_request_read(&req, &ret_length, &ret_data);

    if (err != USB_ERR_OK) {
        return (err);
    }

    if (ret_length != 2) {
        return (USB_ERR_IOERROR);
    }

    if (ret_status) {
        ret_status->wStatus = *((uint16_t *) ret_data);
        free(ret_data);
    }

    return (USB_ERR_OK);
}

/**
 * \brief This request sets the device address for all future device accesses.
 *
 * \param new_address:  the new address to be used for all subsequent accesses
 *                      new_address < 128 && new_address > 0
 *
 * \return USB_ERR_OK on sucess
 *
 * Behaviour of the device upon receiving a request:
 *  - default state:    device enters the address sate if address > 0
 *  - address state:    device enters default state if address = 0
 *                      OR device changes its address
 *  - configured state: not specified
 *
 */
usb_error_t usb_set_address(uint8_t new_address)
{
    struct usb_device_request req;

    if (new_address > 127) {
        return (USB_ERR_INVAL);
    }

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bRequest = USB_REQUEST_SET_ADDRESS;
    req.wValue = new_address;
    req.wIndex = 0;
    req.wLength = 0;

    return (usb_do_request(&req));

}

/**
 * \brief This request sets the device configuration.
 *
 * \param config_value:  the new configuration to activate, this must be
 *                       zero or match a configuration from the config
 *                       descriptor.
 *
 * \return USB_ERR_OK on sucess
 *
 * Behaviour of the device upon receiving a request:
 *  - default state:    not specified
 *  - address state:    on matching value, the device enters configured
 *                      state
 *  - configured state: if new configuration is zero then device enters
 *                      address state or activates new configuration
 *                      if value is matching.
 *
 *  There will be a request error if the configuration does not match
 */
usb_error_t usb_set_configuration(uint8_t config_value)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bRequest = USB_REQUEST_SET_CONFIG;
    req.wValue = (config_value & 0x00FF);
    req.wIndex = 0;
    req.wLength = 0;

    return (usb_do_request(&req));
}

/**
 * \brief This request is optional and may be used to update existing
 *           descriptors or new descriptors may be added.
 *
 * \param desc_type:    the new configuration to activate, this must be
 *                      zero or match a configuration from the config
 *                      descriptor.
 * \param desc_index:   Used for selecting the descriptor if a device implements
 *                      multiple descriptors of a certain type (config or string)
 * \param language
 * \param descriptor
 *
 * \return USB_ERR_OK on sucess
 *
 * Behaviour of the device upon receiving a request:
 *  - default state:    not specified
 *  - address state:    if supported, a valid request
 *  - configured state: if supported, a valid request
 *
 *  There will be a request error if the configuration does not match
 */
usb_error_t usb_set_descriptor(uint8_t desc_type, uint8_t desc_index,
        uint8_t language, struct usb_descriptor *descriptor)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bRequest = USB_REQUEST_SET_DESCRIPTOR;
    /*
     * the index must be zero if other descriptors as string or config are used
     * The only allowed values for descriptor type are device, configuration,
     * and string descriptor types.
     */
    switch (desc_type) {
        case USB_DESCRIPTOR_TYPE_STRING:
        case USB_DESCRIPTOR_TYPE_CONFIG:
            req.wValue = (desc_type << 8) | desc_index;
            break;
        case USB_DESCRIPTOR_TYPE_DEVICE:
            req.wValue = (desc_type << 8);
            break;
        default:
            return (USB_ERR_BAD_REQUEST);
    }

    req.wIndex = 0;
    req.wLength = 0;

    return (usb_do_request_write(&req, descriptor->bLength, descriptor));
}

/**
 * \brief This request is used to set or enable a specific feature.
 *
 * \param feature:      The feature to enable. This value must be appropriate to
 *                      the recipient.
 * \param recipient:     The recipient of the request: USB_REQUEST_RECIPIENT_*
 * \param test:
 * \param index:
 *
 * \return USB_ERR_OK on sucess
 *
 * Behaviour of the device upon receiving a request:
 *  - default state:    only accepts TEST_MODE
 *  - address state:    request error for specific ep or interface
 *  - configured state: valid request
 *
 *  A SetFeature() request that references a feature that cannot be set or
 *  that does not exist causes a STALL to be returned in the Status stage of
 *  the request.
 */
usb_error_t usb_set_feature(uint8_t recipient, uint16_t feature, uint8_t test,
        uint8_t recipent_index)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = recipient;
    req.bRequest = USB_REQUEST_SET_FEATURE;
    req.wLength = 0;

    req.wIndex = 0;

    if (recipient != USB_REQUEST_RECIPIENT_DEVICE) {
        // the LSb of the wIndex only stores the index of an endpoint/interface
        req.wIndex = recipent_index;
    }

    if (feature == USB_REQUEST_FEATURE_TEST_MODE) {
        if (recipient != USB_REQUEST_RECIPIENT_DEVICE) {
            // only devices may get an TEST_MODE feature request
            return (USB_ERR_BAD_REQUEST);
        } else {
            req.wIndex = (test << 8);
        }
    }
    return (usb_do_request(&req));
}

/**
 * \brief This request allows the host to select an alternate setting
 *        for the specified interface.
 *
 * \param feature:      The feature to enable. This value must be appropriate to
 *                      the recipient.
 * \param recipient:     The recipient of the request: USB_REQUEST_RECIPIENT_*
 * \param test:         The testmode selected
 * \param index:        The number of the interface / endpoint
 *
 * \return USB_ERR_OK on sucess
 *
 * Behaviour of the device upon receiving a request:
 *  - default state:    behaviour not specified
 *  - address state:    device must respons with a request error
 *  - configured state: valid request
 *
 *  If the interface or the alternate setting does not exist, then the device
 *  responds with a Request Error.
 */
usb_error_t usb_set_alt_iface(uint16_t alt_setting, uint16_t interface)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = USB_REQUEST_RECIPIENT_INTERFACE;
    req.bRequest = USB_REQUEST_SET_INTERFACE;
    req.wIndex = (0x00FF & interface);
    req.wValue = (0x00FF & alt_setting);
    req.wLength = 0;

    return (usb_do_request(&req));
}

/**
 * \brief This request is used to set and then report an endpoint’s
 *           synchronization frame.
 *
 * \param endpoint:      The feature to enable. This value must be appropriate
 *                          to the recipient.
 * \param ret_frame:     The recipient of the request: USB_REQUEST_RECIPIENT_*
 *
 * \return USB_ERR_OK on sucess
 *
 * Behavior of the device upon receiving a request:
 *  - default state:    behavior not specified
 *  - address state:    device shall response with a request error
 *  - configured state: valid request
 */
usb_error_t usb_synch_frame(uint8_t endpoint, uint16_t *ret_frame)
{
    struct usb_device_request req;

    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = USB_REQUEST_RECIPIENT_ENDPOINT;
    req.bRequest = USB_REQUEST_SYNCH_FRAME;
    req.wIndex = endpoint;
    req.wValue = 0;
    req.wLength = 2;

    uint16_t ret_length;
    void *ret_data;

    if (ret_frame) {
        *ret_frame = 0;
    }

    usb_error_t err = usb_do_request_read(&req, &ret_length, &ret_data);

    if (err != USB_ERR_OK) {
        return (err);
    }

    /* the returned data must be of length 2 */
    if (ret_length != 2) {
        return (USB_ERR_IOERROR);
    }

    if (ret_frame) {
        *ret_frame = *((uint16_t *) ret_data);
        free(ret_data);
    }
    return (USB_ERR_OK);
}

/**
 * \brief this function executes a device request without a data stage
 *
 * \return USB_ERR_OK on success, error code otherwise
 */
usb_error_t usb_do_request(struct usb_device_request *req)
{
    errval_t err;
    uint32_t ret_status = 0;

    USB_DEBUG_IDC("libusb: usb_do_request()\n");

    err = usb_manager.vtbl.request(&usb_manager, (uint8_t*) req, sizeof(*req),
            &ret_status);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "libusb: do_request rpc failed");
        return (USB_ERR_IDC);
    }

    USB_DEBUG_IDC("libusb: usb_do_request() succeeded\n");

    return ((usb_error_t) ret_status);
}

/**
 * \brief this function executes a device request with a write a data stage
 *
 * \return USB_ERR_OK on success, error code otherwise
 */
usb_error_t usb_do_request_write(struct usb_device_request *req,
        uint16_t length, void *data)
{
    errval_t err;
    uint32_t ret_status;

    USB_DEBUG_IDC("libusb: usb_do_request_write()\n");

    err = usb_manager.vtbl.request_write(&usb_manager, (uint8_t*) req,
            sizeof(*req), (uint8_t *) data, length, &ret_status);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "libusb: do_request_write rpc failed");
        return (USB_ERR_IDC);
    }

    USB_DEBUG_IDC("libusb: usb_do_request_write() succeeded\n");

    return ((usb_error_t) ret_status);
}

/**
 * \brief this function executes a device request with a read a data stage
 *
 * \return USB_ERR_OK on success, error code otherwise
 */
usb_error_t usb_do_request_read(struct usb_device_request *req,
        uint16_t *ret_length, void **ret_data)
{
    errval_t err;
    uint32_t ret_status = 0;
    uint8_t *data = NULL;
    size_t length = 0;
    usb_error_t ret;

    USB_DEBUG_IDC("libusb: usb_do_request_read()\n");

    err = usb_manager.vtbl.request_read(&usb_manager, (uint8_t*) req,
            sizeof(*req), (uint8_t **) &data, &length, &ret_status);

    *ret_length = length;

    ret = (usb_error_t) ret_status;


    if (err_is_fail(err)) {
        DEBUG_ERR(err, "libusb: do_request_write rpc failed");
        *ret_length = 0;
        *ret_data = NULL;
        return (USB_ERR_IDC);
    }

    USB_DEBUG_IDC("libusb: usb_do_request_read() got data (len=%i)\n", *ret_length);

    *ret_data = (void *) data;

    return (ret);
}
