/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


/**
 * =======================================================================
 * This file contains data structures and methods for issuing USB device
 * requests on a specific device.
 * =======================================================================
 */

#ifndef _LIB_USB_REQUEST_H
#define _LIB_USB_REQUEST_H

#include <barrelfish/barrelfish.h>

#include <usb/usb.h>
#include <usb/usb_descriptor.h>
#include <usb/usb_error.h>


struct usb_request_type {
    uint8_t recipient : 5;  // device, interface, endpoint...
    uint8_t type : 2;       // standard, class, vendor
    uint8_t direction : 1;  // host2device or device2host
};
typedef struct usb_request_type  usb_request_type_t;

/**
 * ------------------------------------------------------------------------
 * USB Device Request (USB Specification, Rev 2.0, Section 9.4)
 * ------------------------------------------------------------------------
 * This datastructures defines a 8 byte setup packet used to send a request
 * over the device's default control pipe
 */
struct usb_device_request {
	usb_request_type_t 	bType;      ///< recipient
	uint8_t  			bRequest;   ///< request identifier
	uint16_t 			wValue;     ///< parameter depending on the request
	uint16_t 			wIndex;     ///< interface / endpoint index
	uint16_t 			wLength;    ///< number of bytes in the data stage
}__packed;

typedef struct usb_device_request usb_device_request_t;

// values for the request recipents
#define USB_REQUEST_RECIPIENT_MASK      0x1F
#define USB_REQUEST_RECIPIENT_DEVICE    0
#define USB_REQUEST_RECIPIENT_INTERFACE 1
#define USB_REQUEST_RECIPIENT_ENDPOINT  2
#define USB_REQUEST_RECIPIENT_OTHER     3

// the direction of the request: host to device or device to host
#define USB_REQUEST_WRITE 0
#define USB_REQUEST_READ 1

// type of the request.
#define USB_REQUEST_TYPE_STANDARD 0
#define USB_REQUEST_TYPE_CLASS    1
#define USB_REQUEST_TYPE_VENDOR   2
#define USB_REQUEST_TYPE_RESERVED 3

// masks for the index field depending on the recipent (interface or endpoint)
#define USB_REQUEST_INDEX_IF_NUM    0x00FF
#define USB_REQUEST_INDEX_EP_NUM    0x000F
#define USB_REQUEST_INDEX_DIRECTION 0x0080

#define USB_REQUEST_GET_EP(req) \
			(req->wIndex & USB_REQUEST_INDEX_EP_NUM)
#define USB_REQUEST_GET_IF(req) \
			(req->wIndex & USB_REQUEST_INDEX_IF_NUM)
#define USB_REQUEST_GET_DIR(req) \
			((req->wIndex & USB_REQUEST_INDEX_DIRECTION) >> 8)

//USB Device Request Codes (USB Specification, Rev 2.0, Table 9.4)
#define USB_REQUEST_GET_STATUS     0
#define USB_REQUEST_CLEAR_FEATURE  1
#define USB_REQUEST_SET_FEATURE    3
#define USB_REQUEST_SET_ADDRESS    5
#define USB_REQUEST_GET_DESCRIPTOR 6
#define USB_REQUEST_SET_DESCRIPTOR 7
#define USB_REQUEST_GET_CONFIG     8
#define USB_REQUEST_SET_CONFIG     9
#define USB_REQUEST_GET_INTERFACE 10
#define USB_REQUEST_SET_INTERFACE 11
#define USB_REQUEST_SYNCH_FRAME   12

// feature selectors (used with SET_FEATURE requests)
#define USB_REQUEST_FEATURE_REMOTE_WAKEUP 1
#define USB_REQUEST_FEATURE_ENDPOINT_HALT 0
#define USB_REQUEST_FEATURE_TEST_MODE     2

// status request reply
#define USB_REQUEST_STATUS_SELF_POWERED     0x0001
#define USB_REQUEST_STATUS_REMOTE_WAKEUP    0x0002
#define USB_REQUEST_STATUS_EP_HALT          0x0001
#define USB_DEVICE_IS_SELFPOWERED(status) \
			(status & USB_REQUEST_STATUS_SELF_POWERED)
#define USB_DEVICE_CAN_REMOTE_WAKEUP(status) \
			(status & USB_REQUEST_STATUS_REMOTE_WAKEUP)
#define USB_EP_STATUS_HALTED(status)  \
			(status & USB_REQUEST_STATUS_EP_HALT)

/* FLAGS */
#define USB_REQUEST_FLAG_IGNORE_SHORT_XFER 0x01
#define USB_REQUEST_FLAG_DELAY_STATUS      0x02
#define USB_REQUEST_FLAG_USER_DATA         0x04

/*
 * =======================================================================
 * Standard Device Requests (USB Reference Section 9.4)
 * =======================================================================
 */

usb_error_t usb_clear_feature(uint8_t recipient, uint8_t recipient_index,
        uint16_t feature);

usb_error_t usb_get_configuration(uint8_t *ret_config);

usb_error_t usb_get_descriptor(uint8_t desc_type, uint8_t desc_index,
        uint16_t lang, void **ret_desc, uint16_t *ret_length);

usb_error_t usb_get_device_descriptor(struct usb_device_descriptor *ret_desc);

usb_error_t usb_get_config_descriptor(uint8_t config_index,
									  struct usb_config_descriptor *ret_desc);

usb_error_t usb_get_iface_descriptor(uint8_t iface_index,
									 struct usb_config_descriptor *ret_desc);

usb_error_t usb_get_ep_descriptor(uint8_t ep_index,
								  struct usb_endpoint_descriptor *ret_desc);

usb_error_t usb_get_string_descriptor(uint16_t lang_id, uint8_t string_index,
									  void *ret_desc);

usb_error_t usb_get_alt_iface(uint16_t iface_number, uint8_t *ret_alt_iface);

usb_error_t usb_get_status(uint8_t recipient, uint16_t recipient_index,
                                        struct usb_status *ret_status);

usb_error_t usb_set_address(uint8_t device_address);

usb_error_t usb_set_configuration(uint8_t config_value);

usb_error_t usb_set_descriptor(uint8_t desc_type, uint8_t desc_index,
        uint8_t language, struct usb_descriptor *descriptor);

usb_error_t usb_set_feature(uint8_t recipient, uint16_t feature, uint8_t test,
        uint8_t index);

usb_error_t usb_set_alt_iface(uint16_t alt_setting, uint16_t interface);

usb_error_t usb_synch_frame(uint8_t endpoint, uint16_t *ret_frame);


/* wrappers for executing the request */
usb_error_t usb_do_request(struct usb_device_request *req);

usb_error_t usb_do_request_write(struct usb_device_request *req,
        uint16_t length, void *data);

usb_error_t usb_do_request_read(struct usb_device_request *req,
        uint16_t *ret_length, void **ret_data);


#endif
