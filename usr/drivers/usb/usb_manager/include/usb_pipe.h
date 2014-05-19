/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_PIPE_H_
#define USB_PIPE_H_


void usb_pipe_start(struct usb_xfer_queue *queue);
void usb_pipe_enter(struct usb_xfer *xfer);

#endif /* USB_PIPE_H_ */
