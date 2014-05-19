/**
 * \brief this file contains definitions for the standard USB descriptors
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_DESCRIPTOR_H_
#define _USB_DESCRIPTOR_H_

#include <stdint.h>

//USB descriptor codes (USB Specification, Rev 2.0, Table 9.5)
#define USB_DESCRIPTOR_TYPE_DEVICE             1
#define USB_DESCRIPTOR_TYPE_CONFIG             2
#define USB_DESCRIPTOR_TYPE_STRING             3
#define USB_DESCRIPTOR_TYPE_INTERFACE          4
#define USB_DESCRIPTOR_TYPE_ENDPOINT           5
#define USB_DESCRIPTOR_TYPE_DEVICE_QUALIFIER   6
#define USB_DESCRIPTOR_TYPE_OTHER_SPEED_CONFIG 7
#define USB_DESCRIPTOR_TYPE_INTERFACE_POWER    8
#define USB_DESCRIPTOR_TYPE_OTG                9

// USB Specification Release Numbers
#define USB_RELEASE_NUMBER_10 0x0100
#define USB_RELEASE_NUMBER_11 0x0110
#define USB_RELEASE_NUMBER_20 0x0200
#define USB_RELEASE_NUMBER_25 0x0250
#define USB_RELEASE_NUMBER_30 0x0300

// USB release number masks
#define USB_RELEASE_NUMBER_MAJOR 0xFF00
#define USB_RELEASE_NUMBER_MINOR 0x00F0
#define USB_RELEASE_NUMBER_SUB   0x000F

/**
 * ------------------------------------------------------------------------
 * USB Generic Descriptor
 * ------------------------------------------------------------------------
 */
struct usb_descriptor {
    uint8_t bLength;            ///< the length of this descriptor
    uint8_t bDescriptorType;    ///< the type of this descriptor
    uint8_t bDescriptorSubType; ///< the subtype of this descritpor
}__attribute__((packed));

/// type definition for the generic descriptor
typedef struct usb_descriptor usb_descriptor_t;

/**
 * ------------------------------------------------------------------------
 * USB Device Descriptor (USB Specification, Rev 2.0, Section 9.6.1)
 * ------------------------------------------------------------------------
 * General and global information about an USB device. Each USB device
 * has exactly one usb_device_descriptor.
 */
struct usb_device_descriptor {
    uint8_t bLength;            ///< the length of the descriptor (18 bytes)
    uint8_t bDescriptorType;    ///< always USB_DESCRIPTOR_TYPE_DEVICE
    uint16_t bcdUSB;            ///< the USB revision number
    uint8_t bDeviceClass;       ///< device class code (defined by USB-IF)
    uint8_t bDeviceSubClass;    ///< device sub class code (defined by USB-IF)
    uint8_t bDeviceProtocol;    ///< specific protocol used by this device
    uint8_t bMaxPacketSize0;    ///< the max packet size of endpoint 0
    uint16_t idVendor;          ///< the vendor ID
    uint16_t idProduct;         ///< the product ID
    uint16_t bcdDevice;         ///< the revision of the device
    uint8_t iManufacturer;      ///< string index of the manufacturer
    uint8_t iProduct;           ///< string index of the product
    uint8_t iSerialNumber;      ///< string index of the serial number
    uint8_t bNumConfigurations; ///< the number of configurations
};

/// type definition of the device descriptor
typedef struct usb_device_descriptor usb_device_descriptor_t;

/// size information of the device descriptor
#define USB_DEVICE_DESCRIPTOR_SIZE 18

/**
 * ------------------------------------------------------------------------
 * USB Device Qualifier Descriptor (USB Specification, Rev 2.0, Section 9.6.2)
 * ------------------------------------------------------------------------
 * This descriptor contains information about a high-speed capable device
 * that would change, if the device is operating at other speed:
 *  - device runs at full speed -> descriptor returns values for high speed.
 */
struct usb_device_qualifier_descriptor {
    uint8_t bLength;            ///< should be 10 bytes
    uint8_t bDescriptorType;    ///< always USB_DESCRIPTOR_TYPE_DEVICE_QUALIFIER
    uint16_t bcdUSB;            ///< the USB revision
    uint8_t bDeviceClass;       ///< USB device class code (defined by USB-IF)
    uint8_t bDeviceSubClass;    ///< SB Device subclass code (defined by USB-IF)
    uint8_t bDeviceProtocol;    ///< the device protocol to be used
    uint8_t bMaxPacketSize0;    ///< maximum packet size of endpoint 0
    uint8_t bNumConfigurations; ///< the number of configurations
    uint8_t bReserved;          ///< always zero
}__attribute__((packed));

/// type definition of the qualifier descriptor
typedef struct usb_device_qualifier_descriptor usb_device_qualifier_descriptor_t;

// size information of the device qualifier descriptor
#define USB_DEVICE_QUALIFIER_DESCRIPTOR_SIZE 10

/**
 * ------------------------------------------------------------------------
 * USB Configuration Descriptor (USB Specification, Rev 2.0, Section 9.6.3)
 * ------------------------------------------------------------------------
 * This descriptor contains information about a specific device
 * configuration. The bConfigurationValue is used as a parameter to
 * SetConfiguration().
 *
 * wTotalLength: the size of the entire configuration:
 *               config descriptor + interfaces + endpoints
 * Each USB device has one or more configuration descriptors
 */
struct usb_config_descriptor {
    uint8_t bLength;             ///< length of the descriptor in bytes
    uint8_t bDescriptorType;     ///< always USB_DESCRIPTOR_TYPE_CONFIG
    uint16_t wTotalLength;       ///< total length of this descritpor
    uint8_t bNumInterfaces;      ///< the number of interfaces in this config
    uint8_t bConfigurationValue; ///< parameter for SetConfiguration()
    uint8_t iConfiguration;      ///< string index of this configuration
    uint8_t bmAttributes;        ///< configuration characteristics
    uint8_t bMaxPower;           ///< the maximum power consumption (2mA steps)
}__attribute__((packed));

typedef struct usb_config_descriptor usb_config_descriptor_t;

// size information of the configuration descriptor
#define USB_CONFIG_DESCRIPTOR_SIZE 9

// values for the bit map
#define USB_CONFIG_SELF_POWERED     0x40
#define USB_CONFIG_REMOTE_WAKEUP    0x20
#define USB_CONFIG_BUS_POWERED      0x80

/**
 * ------------------------------------------------------------------------
 * USB Interface Descriptor (USB Specification, Rev 2.0, Section 9.6.5)
 * ------------------------------------------------------------------------
 * This descriptor contains information about a specific interface within
 * an USB configuration. The interface descriptor defines an unique set
 * of endpoints within the configuration.
 *
 * Interface descriptors cannot directly be accesses by Get/SetDescriptor(),
 * they are returned as a part of the configuration descriptor.
 */
struct usb_interface_descriptor {
    uint8_t bLength;            ///< should be 9 bytes
    uint8_t bDescriptorType;    ///< always USB_DESCRIPTOR_TYPE_INTERFACE
    uint8_t bInterfaceNumber;   ///< number of this interface within the config
    uint8_t bAlternateSetting;  ///< the alternate setting
    uint8_t bNumEndpoints;      ///< number of used endpoints in this interface
    uint8_t bInterfaceClass;    ///< interface class code (assigned by USB-IF)
    uint8_t bInterfaceSubClass; ///< interface subclass code (assigned by USB-IF)
    uint8_t bInterfaceProtocol; ///< protocol code (qualified by class/subclass)
    uint8_t iInterface;         ///< string index describing this interface
}__attribute__((packed));

/// type definition for the interface descriptor
typedef struct usb_interface_descriptor usb_interface_descriptor_t;

// size information about the interface descriptor
#define USB_INTERFACE_DESCRIPTOR_SIZE 9


struct usb_endpoint_address {
    uint8_t ep_number :4;   ///< the endpoint number
    uint8_t _reserved :3;   ///< should be zero
    uint8_t direction :1;   ///< direction, either IN or OUT
};
typedef struct usb_endpoint_address usb_endpoint_address_t;

struct usb_endpoint_attributes {
    uint8_t xfer_type :2;   ///< the type of this endpoint
    uint8_t sync_type :2;   ///< for isochronus only
    uint8_t usage_type :2;  ///< for isochronous only
    uint8_t _unused :2;     ///< should be zero
};
typedef struct usb_endpoint_attributes usb_endpoint_attributes_t;

/**
 * ------------------------------------------------------------------------
 * USB Endpoint Descriptor (USB Specification, Rev 2.0, Section 9.6.6)
 * ------------------------------------------------------------------------
 * This descriptor contains information about one endpoint for an interface.
 * Endpoint descriptors are always returned as part of the configuration
 * Information by GetConfiguration().
 *
 * Each endpoint has its own endpoint descriptor.
 */
struct usb_endpoint_descriptor {
    uint8_t bLength;            ///< should be 9 bytes
    uint8_t bDescriptorType;    ///< always USB_DESCRIPTOR_TYPE_ENDPOINT
    usb_endpoint_address_t bEndpointAddress;
    usb_endpoint_attributes_t bmAttributes;
    uint16_t wMaxPacketSize;    ///< maximum packet size for this endpoint
    uint8_t bInterval;          ///< interval for polling in (micro)frames
}__attribute__((packed));

typedef struct usb_endpoint_descriptor usb_endpoint_descriptor_t;

// size information of endpoint descriptor
#define USB_ENDPOINT_DESCRIPTOR_SIZE 9

/// used for endpoint lookup
#define USB_ENDPOINT_ADDRESS_ANY    0xFF

// endpoint directions
#define USB_ENDPOINT_DIRECTION_OUT  0x00
#define USB_ENDPOINT_DIRECTION_IN   0x01
#define USB_ENDPOINT_DIRECTION_ANY  0xFF

// endpoint types
#define USB_ENDPOINT_TYPE_CONTROL   0x00
#define USB_ENDPOINT_TYPE_ISOCHR    0x01
#define USB_ENDPOINT_TYPE_BULK      0x02
#define USB_ENDPOINT_TYPE_INTR      0x03
#define USB_ENDPOINT_TYPE_ANY       0xFF

// endpoint synchronization for isochronus transfers
#define USB_ENDPOINT_SYNC_NON_ISO   0x00
#define USB_ENDPOINT_SYNC_NONE      0x00
#define USB_ENDPOINT_SYNC_ASYNC     0x04
#define USB_ENDPOINT_SYNC_ADAPT     0x08
#define USB_ENDPOINT_SYNC_SYNC      0x0A

// endpoint usages for isochronus transfers
#define USB_ENDPOINT_USAGE_NON_ISO  0x00
#define USB_ENDPOINT_USAGE_DATA     0x00
#define USB_ENDPOINT_USAGE_FEEDBACK 0x10
#define USB_ENDPOINT_USAGE_IMPLICIT 0x20
#define USB_ENDPOINT_USAGE_RESERVED 0x30

/**
 * ------------------------------------------------------------------------
 * USB String Descriptor (USB Specification, Rev 2.0, Section 9.6.7)
 * ------------------------------------------------------------------------
 * String descriptors contains string describing certain elements of other
 * USB descriptors. String descriptors are optional. All references to
 * string descriptors are set to zero if they are not implemented by
 * the device
 *
 * The strings are encoded using UNICODE and are NOT null terminated.
 */
struct usb_string_descriptor_languages {
    uint8_t bLength;         ///< length of the descriptor in bytes
    uint8_t bDescriptorType; ///< always USB_DESCRIPTOR_TYPE_STRING
    uint16_t wLangID[128];   ///< language ID code
}__attribute__((packed));

typedef struct usb_string_descriptor_languages usb_string_descriptor_languages_t;

struct usb_string_descriptor {
    uint8_t bLength;         ///< length of the descriptor in bytes
    uint8_t bDescriptorType; ///< always USB_DESCRIPTOR_TYPE_STRING
    char bString[256];       ///< char array containing the string (not null term.)
}__attribute__((packed));

typedef struct usb_string_descriptor usb_string_descriptor_t;

#define USB_STRING_GET_ELEMENT_COUNT(sd)    ((sd->bLength -2 )/2)
#define USB_STRING_GET_STRLEN(sd)           ((sd->bLength -2 ))

/**
 * ------------------------------------------------------------------------
 * USB Generic Descriptor
 * ------------------------------------------------------------------------
 * This descriptor is used to supply the device driver with all the necessary
 * information upon initialization and connection to the usb manager.
 *
 * A request for a configuration descriptor returns the configuration
 * descriptor, all interface descriptors, and endpoint descriptors for all of
 * the interfaces in a single request. The first interface descriptor follows
 * the configuration descriptor. The endpoint descriptors for the first
 * interface follow the first interface descriptor. If there are additional
 * interfaces, their interface descriptor and endpoint descriptors follow the
 * first interface’s endpoint descriptors.
 */
struct usb_generic_descriptor {
    struct usb_device_descriptor device;
    struct usb_config_descriptor config;
    struct usb_interface_descriptor iface[1];
    struct usb_endpoint_descriptor endpoint[1];
};

typedef struct usb_generic_descriptor usb_generic_descriptor_t;

#endif
