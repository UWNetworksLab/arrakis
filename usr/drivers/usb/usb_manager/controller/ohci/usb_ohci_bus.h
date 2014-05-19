/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_OHCI_BUS_H_
#define _USB_OHCI_BUS_H_

struct usb_hcdi_bus_fn *usb_ohci_get_bus_fn(void);
void usb_ohci_do_poll(struct usb_host_controller *hc);


#endif
