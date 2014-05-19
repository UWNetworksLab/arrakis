/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_OHCI_XFER_H_
#define _USB_OHCI_XFER_H_

uint8_t usb_ohci_xfer_is_finished(struct usb_xfer *xfer);
void usb_ohci_xfer_done(struct usb_xfer *xfer);
void usb_ohci_xfer_done_isoc(struct usb_xfer *xfer);
void usb_ohci_xfer_remove(struct usb_xfer *xfer, usb_error_t err);
void usb_ohci_xfer_enqueue(struct usb_xfer *xfer);
void usb_ohci_xfer_start(struct usb_xfer *xfer, usb_ohci_ed_t **ed_last);

void usb_ohci_xfer_setup(struct usb_xfer_setup_params *param);
void usb_ohci_xfer_unsetup(struct usb_xfer *xfer);


#endif /* _USB_OHCI_XFER_H_ */
