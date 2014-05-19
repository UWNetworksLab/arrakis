/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_EHCI_XFER_H_
#define USB_EHCI_XFER_H_

void usb_ehci_xfer_remove(struct usb_xfer *xfer, usb_error_t error);
void usb_ehci_xfer_standard_setup(struct usb_xfer *xfer, usb_ehci_qh_t **qh_last);
uint8_t usb_ehci_xfer_is_finished(struct usb_xfer *xfer);


void usb_ehci_xfer_done(struct usb_xfer *xfer);
void usb_ehci_xfer_fs_isoc_done(struct usb_xfer *xfer);
void usb_ehci_xfer_hs_isoc_done(struct usb_xfer *xfer);

void usb_ehci_xfer_setup(struct usb_xfer_setup_params *param);
void usb_ehci_xfer_unsetup(struct usb_xfer *xfer);


#endif /* USB_EHCI_XFER_H_ */
