/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_DRIVER_H_
#define USB_DRIVER_H_

#define USB_DEVICE_CLASS_COMPOSITE 0x00
#define USB_DEVICE_CLASS_AUDIO 0x01
#define USB_DEVICE_CLASS_COMM 0x02
#define USB_DEVICE_CLASS_HID 0x03
#define USB_DEVICE_CLASS_PHYSICAL 0x05
#define USB_DEVICE_CLASS_IMAGE 0x06
#define USB_DEVICE_CLASS_PRINTER 0x07
#define USB_DEVICE_CLASS_MSD 0x08
#define USB_DEVICE_CLASS_HUB 0x09
#define USB_DEVICE_CLASS_CDC 0x0A
#define USB_DEVICE_CLASS_SMARTCARD 0x0B
#define USB_DEVICE_CLASS_SECURITY 0x0D
#define USB_DEVICE_CLASS_VIDEO 0x0E
#define USB_DEVICE_CLASS_HEALTH 0x0F
#define USB_DEVICE_CLASS_AV 0x10
#define USB_DEVICE_CLASS_DIAG 0xDC
#define USB_DEVICE_CLASS_WIFI 0xE0
#define USB_DEVICE_CLASS_MISC 0xEF
#define USB_DEVICE_CLASS_APPL 0xFE
#define USB_DEVICE_CLASS_VENDOR 0xFF

void usb_driver_start(struct usb_device *dev);
void usb_driver_connected(struct usb_manager_binding *bind,
        struct usb_driver_binding *driver, uint16_t config);
#endif /* USB_DRIVER_H_ */
