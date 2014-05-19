/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _LIBUSB_TRANSFER_H_
#define _LIBUSB_TRANSFER_H_

#include <usb/usb_xfer.h>

/// USB transfer id type
typedef uint32_t usb_xfer_id_t;

/// callback function for completed transfers
typedef void (usb_transfer_cb_t)(usb_error_t err, void *data,
        uint32_t data_length);

// the USB control endpoint
#define USB_ENDPOINT_CONTROL 0

/**
 * this enumeration stores the different states of an USB transfer
 */
enum usb_transfer_state {
    USB_TRANSFER_STATE_SETUP,   ///< transfer is setup
    USB_TRANSFER_STATE_STALLED, ///< transfer is in stalled state
    USB_TRANSFER_STATE_DONE,    ///< transfer is executed successfully
    USB_TRANSFER_STATE_ERROR,   ///< transfer is in error state
};

/// USB transfer state type
typedef enum usb_transfer_state usb_tstate_t;

/**
 * this data structure contains the necessary data for setting up
 * a new USB transfer
 */
struct usb_transfer_setup {
    uint32_t max_bytes;             ///< maximum bytes to to transfer
    uint32_t max_frames;            ///< the maximum bumber of frames
    uint32_t interval;              ///< the interval for interrupt / isochr
    uint32_t timeout;               ///< period till the transfer timeouts
    struct usb_xfer_flags flags;    ///< some specific transfer flags
    uint8_t type;                   ///< the type of the usb pipe
    uint8_t direction;              ///< the direction of the data transfer
    uint8_t endpoint;               ///< the associated endpoint of the transfer
    uint8_t interface;              ///< the itnerface to use
    usb_transfer_cb_t *callback;    ///< the function to call upon completition
};

/// USB transfer setup type
typedef struct usb_transfer_setup usb_transfer_setup_t;

/* setting up / freeing up  transfers  */
usb_error_t usb_transfer_setup_control(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_cb, usb_xfer_id_t *ret_id);

usb_error_t usb_transfer_setup_isoc(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_cb, usb_xfer_id_t *ret_id);

usb_error_t usb_transfer_setup_bulk(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_cb, usb_xfer_id_t *ret_id);

usb_error_t usb_transfer_setup_intr(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_cb, usb_xfer_id_t *ret_id);

usb_error_t usb_transfer_unsetup(usb_xfer_id_t tid);

/* transfer control */
usb_error_t usb_transfer_start(usb_xfer_id_t tid);

usb_error_t usb_transfer_stop(usb_xfer_id_t tid);

/* stall handling */
usb_error_t usb_transfer_clear_stall(usb_xfer_id_t tid);

/* state */
usb_error_t usb_transfer_get_state(usb_xfer_id_t tid, usb_tstate_t *ret_state);

usb_error_t usb_transfer_get_status(usb_xfer_id_t tid, uint32_t *ret_actlen,
        uint32_t *ret_length, uint32_t *ret_actframes, uint32_t *ret_numframes);

#endif
