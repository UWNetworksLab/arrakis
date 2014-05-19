/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef USB_ENDPOINT_H_
#define USB_ENDPOINT_H_


struct usb_endpoint *usb_endpoint_lookup(struct usb_device *dev, uint8_t iface,
        const struct usb_xfer_config *filter);
void usb_endpoint_init(struct usb_device *device,
        uint8_t iface_index, struct usb_endpoint_descriptor *desc,
        struct usb_endpoint *ep);

#endif /* USB_ENDPOINT_H_ */
