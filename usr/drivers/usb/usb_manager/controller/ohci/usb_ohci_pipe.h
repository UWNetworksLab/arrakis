/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_OHCI_PIPE_H_
#define USB_OHCI_PIPE_H_

struct usb_hcdi_pipe_fn;

struct usb_hcdi_pipe_fn *usb_ohci_get_bulk_pipe_fn(void);
struct usb_hcdi_pipe_fn *usb_ohci_get_ctrl_pipe_fn(void);
struct usb_hcdi_pipe_fn *usb_ohci_get_isoc_pipe_fn(void);
struct usb_hcdi_pipe_fn *usb_ohci_get_intr_pipe_fn(void);

#endif /* USB_OHCI_PIPE_H_ */
