/**
 * \brief this file contains definitions for handling US xfers
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBUSB_XFER_H_
#define LIBUSB_XFER_H_

/**
 * ------------------------------------------------------------------------
 * USB Transfer Flags
 * ------------------------------------------------------------------------
 * This struct is used then a client driver wants to set up a new transfer.
 */
struct usb_xfer_flags {
    uint8_t _unused : 5;            ///< unused bits to fill up the 2 bytes
    uint8_t auto_restart:1;         ///< auto restart interrupt transfers
    uint8_t short_xfer_forced :1;   ///< force a short transmit transfer on last
    uint8_t short_xfer_ok :1;       ///< allow short transfers (small packets)
    uint8_t short_frames_ok :1;     ///< allow short frames
    uint8_t pipe_on_falure :1;      ///< block pipe had a failure condition
    uint8_t buf_size_frame :1;      ///< buffer size shall be multiple of frame
    uint8_t ext_buffer :1;          ///< use external DMA buffer
    uint8_t manual_status :1;       ///< disables automatic status stage on ctrl
    uint8_t pipe_none_ok :1;        ///< ingore USB_ERR_NO_PIPE errors
    uint8_t pipe_stalled :1;        ///< stall the endpoint before starting
    uint8_t prescale :1;            ///< prescale to frames for isochr transfers
};



#endif
