/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_REQUEST_H_
#define USB_REQUEST_H_

struct usb_manager_binding;
struct usb_device;
struct usb_device_request;

/// struct for managing the request state
struct usb_request_state {
    usb_error_t error;
    struct usb_manager_binding *bind;
    void (*callback)(void *a);
    void *data;
    struct usb_device_request *req;
    uint16_t data_length;
    struct usb_xfer *xfer;
};

/*
 * Flounder Callback Functions
 */
void usb_rx_request_read_call(struct usb_manager_binding *_binding,
        uint8_t *request, size_t req_length);
void usb_rx_request_write_call(struct usb_manager_binding *_binding,
        uint8_t *request, size_t req_length, uint8_t *data, size_t data_length);
void usb_rx_request_call(struct usb_manager_binding *_binding, uint8_t *request,
        size_t req_length);

/*
 * Handle the requests
 */
usb_error_t usb_handle_request(struct usb_device *device, uint16_t flags,
        struct usb_device_request *req, struct usb_request_state *req_state,
        void *data, uint16_t *ret_length);

usb_error_t usb_exec_request(struct usb_device *device, uint16_t flags,
        struct usb_device_request *request, void *data, uint16_t *ret_length);

/* handler function for new device initialization */
usb_error_t usb_req_set_address(struct usb_device *udev, uint16_t addr);
usb_error_t usb_req_get_descriptor(struct usb_device *dev,
        uint16_t *actual_lenght, void *desc, uint16_t min_length,
        uint16_t max_length, uint16_t id, uint8_t type, uint8_t index,
        uint8_t retries);
usb_error_t usb_req_get_device_descriptor(struct usb_device *dev,
        struct usb_device_descriptor *desc);
usb_error_t usb_req_get_config_descriptor(struct usb_device *dev,
        struct usb_config_descriptor **cdesc, uint8_t index);
usb_error_t usb_req_get_string_desc(struct usb_device *dev, void *sdesc,
        uint16_t max_len, uint16_t lang_id, uint8_t string_index);
usb_error_t usb_req_get_string(struct usb_device *dev, char *buf, uint16_t len,
        uint8_t string_index);
usb_error_t usb_req_set_config(struct usb_device *dev, uint8_t config);

#endif /* USB_REQUEST_H_ */
