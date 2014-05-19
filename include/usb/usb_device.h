/**
 * \brief this file contains device related definitions for the USB client driver
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBUSB_DEVICE_H
#define LIBUSB_DEVICE_H

/**
 * ------------------------------------------------------------------------
 * USB Endpoint
 * ------------------------------------------------------------------------
 * This data structure defines an USB tendpoint reflecting the state on a
 * physical endpoint on the device.
 */
struct usb_endpoint
{
    uint8_t ep_direction;           ///< the direction of this endpoint
    uint8_t ep_number;              ///< the endpoint number
    uint8_t ep_type;                ///< the type of this endpoint
    uint8_t ep_usage;               ///< for isochronus only: usage
    uint8_t ep_sync;                ///< for isochronus only: sync field
    struct usb_interface *iface;    ///< the parent interface
    uint8_t iface_index;            ///< the interface index
};

/// endpoint status flag for usb_status_t
#define USB_ENDPOINT_STATUS_HALT 0x0001

/// the USB control endpoint
#define USB_ENDPOINT_CONTROL 0

/// the maximum number of endpoints
#define USB_ENDPOINT_MAX 32

/**
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
    uint8_t alt_setting;        ///< alternative settings
    uint8_t parent_iface_index; ///< the parent interface index
    uint8_t iface_number;       ///< interface number of this interface
    uint8_t iface_class;        ///< the interface class code
    uint8_t iface_subclass;     ///< the interface subclass code
    uint8_t iface_protocol;     ///< the interface protocol
    uint8_t num_endpoints;      ///< the number of endpoints in this iface
    uint8_t config;             ///< the configuration value
    struct usb_endpoint endpoints[USB_ENDPOINT_MAX];
};

/// used for lookups
#define USB_INTERFACE_INDEX_ANY 0xFF

/**
 * this struct represents a device for the client drivers
 */
struct usb_device {
    struct usb_interface *ifaces;   ///< the interfaces of the current config
    struct usb_endpoint *endpoints; ///< the endpoints of the current config
    struct usb_config_descriptor *config_desc; ///< configuration descriptor
    uint8_t dev_class;              ///< device class code
    uint8_t dev_subclass;           ///< device sub class code
    uint8_t dev_protocol;           ///< device protocol
    uint16_t vendor;                ///< vendor id
    uint16_t product;               ///< product id
    uint16_t version;               ///< the device version
    uint8_t iface_max;              ///< maximum interfaces
    uint8_t ep_max;                 ///< maximum endpoints
    uint8_t num_config;             ///< the number of configurations
    uint8_t current_config;         ///< the current active configuration
};

typedef struct usb_device usb_device_t;


/*
 * Prototypes
 */
void usb_device_init(void *desc);

uint8_t usb_device_get_num_config(void);

struct usb_interface *usb_device_get_iface(uint8_t iface);

usb_error_t usb_device_get_iface_count(uint8_t *ret_count);

usb_error_t usb_device_get_speed(usb_speed_t *ret_speed);

usb_error_t usb_device_state(void);

struct usb_config_descriptor *usb_device_get_cfg_desc(void);

usb_error_t usb_device_suspend(void);

usb_error_t usb_device_resume(void);

usb_error_t usb_device_powersave(void);

#endif /* USB_DEVICE_H_ */
