/**
 * \brief this file contains function prototypes for some HCDI functions
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_EHCI_BUS_H_
#define USB_EHCI_BUS_H_

/* prototypes */
struct usb_hcdi_bus_fn *usb_ehci_get_bus_fn(void);

void usb_ehci_endpoint_init(struct usb_device *device,
        struct usb_endpoint_descriptor *ep_desc, struct usb_endpoint *ep);

void usb_ehci_device_resume(struct usb_device *device);

void usb_ehci_device_suspend(struct usb_device *device);

void usb_ehci_sleep(struct usb_host_controller *hc, uint32_t state);

void usb_ehci_set_power(struct usb_host_controller *hc);

void usb_ehci_poll(usb_ehci_hc_t *hc);

#endif /* USB_EHCI_BUS_H_ */
