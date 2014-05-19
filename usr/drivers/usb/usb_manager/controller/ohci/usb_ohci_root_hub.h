/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_OHCI_ROOT_HUB_H_
#define USB_OHCI_ROOT_HUB_H_
/*
 * =======================================================================
 * This file contains the declarations for the OHCI root hub
 * =======================================================================
 */



usb_error_t usb_ohci_roothub_exec(struct usb_device *device,
          struct usb_device_request *req, const void **pptr, uint16_t *plength);
void usb_ohci_root_hub_interrupt(usb_ohci_hc_t *hc);
#endif /* USB_OHCI_ROOT_HUB_H_ */
