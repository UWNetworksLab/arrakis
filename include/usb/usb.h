/**
 * \brief this file contains general declarations for the USB
 */

/* Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBUSB_USB_H_
#define LIBUSB_USB_H_

#include <stdio.h>
#include <usb/usb_error.h>
#include <usb/usb_descriptor.h>

/// definition for the USB
#define USB_MANAGER_SERVICE "usb_manager_service_name"

/**
 * enumeration of the differente USB modes.
 * Currently only the HOST mode is supported
 */
typedef enum usb_mode  {
    USB_MODE_HOST,
    USB_MODE_DEVICE,
    USB_MODE_DUAL
} usb_mode_t;
#define USB_MODE_MAX    (USB_MODE_DUAL+1)


/**
 * The USB device speed enumeration describes all possible USB speed
 * settings for a device.
 */
typedef enum usb_speed {
    USB_SPEED_VARIABLE,
    USB_SPEED_LOW,
    USB_SPEED_FULL,
    USB_SPEED_HIGH,
    USB_SPEED_SUPER,
} usb_speed_t;

#define USB_SPEED_MAX (USB_SPEED_SUPER+1)

/// typedef for the host controller versions
typedef enum usb_hc_version {
    USB_UHCI=0x0100,
    USB_OHCI=0x0110,
    USB_EHCI=0x0200,
    USB_XHCI=0x0300
} usb_hc_version_t;

/// typedef for the different USB revisions
typedef enum usb_revision {
    USB_REV_UNKNOWN,
    USB_REV_PRE_1_0,
    USB_REV_1_0,
    USB_REV_1_1,
    USB_REV_2_0,
    USB_REV_2_5,
    USB_REV_3_0
} usb_revision_t;

/// typedef for the different usb transfer / endpoint types
typedef enum usb_type {
    USB_TYPE_CTRL = 0,
    USB_TYPE_ISOC,
    USB_TYPE_BULK,
    USB_TYPE_INTR
} usb_type_t;


/// typedef for the different power modes of an usb device
typedef enum usb_power {
    USB_POWER_MODE_OFF = 0,
    USB_POWER_MODE_ON = 1,
    USB_POWER_MODE_SAVE = 2,
    USB_POWER_MODE_SUSPEND = 3,
    USB_POWER_MODE_RESUME = 4
} usb_power_t;

/// the maximum power consumption in mA
#define USB_POWER_MAX 500

/// the minimum power requirements in mA
#define USB_POWER_MIN 100

/// the USB physical address type
typedef volatile uintptr_t usb_paddr_t;

/// definition for the default configuration value
#define USB_CONFIGURATION_DEFAULT 1

/// definition if the USB
#define USB_CONFIGURATION_UNCONFIGURED 0xFF

/// generic usb status
struct usb_status {
    uint16_t wStatus;
};
typedef struct usb_status usb_status_t;

#define USB_STATUS_SELF_POWERED     0x0001;
#define USB_STATUS_REMOTE_WAKEUP    0x0002;
#define USB_STATUS_EP_HALT          0x0001;

/*
 * Specific delays
 */
#define USB_DELAY_PORT_RESET 10
#define USB_DELAY_PORT_ROOT_RESET 50
#define USB_DELAY_PORT_RECOVERY 10
#define USB_DELAY_PORT_POWERUP 100
#define USB_DELAY_PORT_RESUME 20
#define USB_DELAY_SET_ADDRESS 2
#define USB_DELAY_RESUME 100
#define USB_DELAY_WAIT 10
#define USB_DELAY_RECOVERY 10

#define USB_WAIT(ms) \
    for (uint32_t wait_i = 0; wait_i < 4*(ms); wait_i++) {printf("%c", 0xE);};


/*
 * debug message control
 */

#define USB_DEBUG(x...) debug_printf(x)
//#define USB_DEBUG(x...)

//#define USB_DEBUG_XFER(x...) USB_DEBUG(x)
#define USB_DEBUG_XFER(x...)

#define USB_DEBUG_HC(x...) USB_DEBUG(x)
//#define USB_DEBUG_HC(x...)

//#define USB_DEBUG_XFER_HC(x...) USB_DEBUG(x)
#define USB_DEBUG_XFER_HC(x...)

//#define USB_DEBUG_REQ(x...) USB_DEBUG(x)
#define USB_DEBUG_REQ(x...)

//#define USB_DEBUG_DEV(x...) USB_DEBUG(x)
#define USB_DEBUG_DEV(x...)

//#define USB_DEBUG_TR_ENTER USB_DEBUG(">> %s()\n",  __func__)
#define USB_DEBUG_TR_ENTER

//#define USB_DEBUG_TR_RETURN USB_DEBUG("<< %s() return\n", __func__)
#define USB_DEBUG_TR_RETURN

#define USB_DEBUG_TR(x...) USB_DEBUG(x)

#define USB_DEBUG_DRIVER(x...) USB_DEBUG(x)
//#define USB_DEBUG_DRIVER(x...)

//#define USB_DEBUG_MEM(x...) USB_DEBUG(x)
#define USB_DEBUG_MEM(x...)

#define USB_DEBUG_IDC(x...)
//#define USB_DEBUG_IDC(x...) USB_DEBUG(x)

//#define USB_DEBUG_HID(x...) debug_printf(x)
#define USB_DEBUG_HID(x...)


usb_error_t usb_lib_init(uint8_t init_config);

#endif
