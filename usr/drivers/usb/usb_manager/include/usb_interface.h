/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_INTERFACE_H_
#define _USB_INTERFACE_H_


/*
 * ------------------------------------------------------------------------
 * USB Interface
 * ------------------------------------------------------------------------
 * This data structure defines an USB tendpoint reflecting the state on a
 * physical endpoint on the device.
 *
 * Fields:
 *  - descriptor        pointer to the interface descriptor
 *  - alt_setting       alternative setting for this iface
 *  - iface_index       interface index of this interface
 *  - device            pointer to the device
 *  - num_endpoints     the number of endpoints for this interface
 *  - endpoints         array of pointer to the endpoints of this iface
 */
struct usb_interface
{
    struct usb_interface_descriptor *descriptor;
    uint8_t alt_setting;
    uint8_t parent_iface_index;
    uint8_t iface_index;

    struct usb_device *device;

    uint8_t num_endpoints;

    struct usb_endpoint endpoints[USB_ENDPOINT_MAX];

};

#define USB_INTERFACE_INDEX_ANY 0xFF

#endif
