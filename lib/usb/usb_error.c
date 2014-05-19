/**
 * \brief this file contains the error specifications of the USB
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <usb/usb_error.h>

/**
 * USB error code to string translation table
 */
static const char* usb_errstr_table[USB_ERR_MAX] = {
    [USB_ERR_OK] = "USB_ERR_NORMAL_COMPLETION",
    [USB_ERR_PENDING_REQUESTS] = "USB_ERR_PENDING_REQUESTS",
    [USB_ERR_NOT_STARTED] = "USB_ERR_NOT_STARTED",
    [USB_ERR_INVAL] = "USB_ERR_INVAL",
    [USB_ERR_NOMEM] = "USB_ERR_NOMEM",
    [USB_ERR_CANCELLED] = "USB_ERR_CANCELLED",
    [USB_ERR_BAD_ADDRESS] = "USB_ERR_BAD_ADDRESS",
    [USB_ERR_BAD_BUFSIZE] = "USB_ERR_BAD_BUFSIZE",
    [USB_ERR_BAD_FLAG] = "USB_ERR_BAD_FLAG",
    [USB_ERR_NO_CALLBACK] = "USB_ERR_NO_CALLBACK",
    [USB_ERR_IN_USE] = "USB_ERR_IN_USE",
    [USB_ERR_NO_ADDR] = "USB_ERR_NO_ADDR",
    [USB_ERR_NO_PIPE] = "USB_ERR_NO_PIPE",
    [USB_ERR_ZERO_NFRAMES] = "USB_ERR_ZERO_NFRAMES",
    [USB_ERR_ZERO_MAXP] = "USB_ERR_ZERO_MAXP",
    [USB_ERR_SET_ADDR_FAILED] = "USB_ERR_SET_ADDR_FAILED",
    [USB_ERR_NO_POWER] = "USB_ERR_NO_POWER",
    [USB_ERR_TOO_DEEP] = "USB_ERR_TOO_DEEP",
    [USB_ERR_IOERROR] = "USB_ERR_IOERROR",
    [USB_ERR_NOT_CONFIGURED] = "USB_ERR_NOT_CONFIGURED",
    [USB_ERR_TIMEOUT] = "USB_ERR_TIMEOUT",
    [USB_ERR_SHORT_XFER] = "USB_ERR_SHORT_XFER",
    [USB_ERR_STALLED] = "USB_ERR_STALLED",
    [USB_ERR_INTERRUPTED] = "USB_ERR_INTERRUPTED",
    [USB_ERR_DMA_LOAD_FAILED] = "USB_ERR_DMA_LOAD_FAILED",
    [USB_ERR_BAD_CONTEXT] = "USB_ERR_BAD_CONTEXT",
    [USB_ERR_NO_ROOT_HUB] = "USB_ERR_NO_ROOT_HUB",
    [USB_ERR_NO_INTR_THREAD] = "USB_ERR_NO_INTR_THREAD",
    [USB_ERR_NOT_LOCKED] = "USB_ERR_NOT_LOCKED",
    [USB_ERR_BAD_REQUEST] = "USB_ERR_BAD_REQUEST",
};


/**
 * \brief translates the error code to an error string
 *
 * \param errno the error code to translate
 *
 * \return char array containint the error string.
 */
const char* usb_get_error_string(usb_error_t errno)
{
    if (errno > USB_ERR_MAX) {
        return ("USB_ERROR_UNKNOWN");
    } else {
        return (usb_errstr_table[errno]);
    }
}
