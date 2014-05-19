/**
 * \brief this file contains definitions for parsing the USB descriptors
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_PARSE_H_
#define USB_PARSE_H_

struct usb_iface_parse_state {
    struct usb_descriptor *desc;
    uint8_t iface_index; /* current interface index */
    uint8_t iface_no_last;
    uint8_t iface_index_alt; /* current alternate setting */
};

struct usb_descriptor *usb_parse_next_descriptor(
        struct usb_config_descriptor *cd, struct usb_descriptor *_desc);

struct usb_interface_descriptor *usb_parse_next_iface(
        struct usb_config_descriptor *cd, struct usb_iface_parse_state *ps);

struct usb_endpoint_descriptor *usb_parse_next_edesc(
        struct usb_config_descriptor *cd, struct usb_endpoint_descriptor *ped);

#endif /* USB_PARSE_H_ */
