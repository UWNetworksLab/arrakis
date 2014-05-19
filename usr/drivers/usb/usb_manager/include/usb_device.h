/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_DEVICE_H_
#define USB_DEVICE_H_

#include <usb_xfer.h> /* for usb_endpoint */

struct usb_device;

#define USB_MAX_DEVICES 127


/*
 * prototypes
 */
struct usb_device_flags;
struct usb_endpoint_descriptor;
struct usb_device_descriptor;
struct usb_hub;
struct usb_xfer;
struct usb_interface;
struct usb_host_controller;
struct usb_manager_binding;


/*
 * The USB device state enumeration describes all the possible USB states
 * the device can be in.
 */
enum usb_device_state {
    USB_DEVICE_STATE_DETACHED,
    USB_DEVICE_TATE_ATTACHED,
    USB_DEVICE_STATE_POWERED,
    USB_DEVICE_STATE_ADDRESSED,
    USB_DEVICE_STATE_CONFIGURED,
};

#define USB_DEVICE_START_ADDR 0
#define USB_DEVICE_MIN_COUNT  2
#define USB_DEVICE_CTRL_XFER_MAX 2



/**
 * ------------------------------------------------------------------------
 * USB Device Flags
 * ------------------------------------------------------------------------
 * This data structures contains device related flags
 *
 * Fields:
 *  - usb_mode          USB mode state, host or device mode
 *  - self_powered      set if the device is self powered
 *  - no_strings        set if the device does not have string support
 *  - remote_wakeup     set if remote wakup is enabled
 *  - peer_suspended    set if the peer (=device) is suspended
 *  - self_suspended    set if the device is self suspended
 */
struct usb_device_flags {
    usb_mode_t usb_mode;  /* host or device mode */
    uint8_t self_powered : 1;
    uint8_t no_strings : 1;
    uint8_t remote_wakeup : 1;
    uint8_t peer_suspended : 1;
    uint8_t self_suspended : 1;
};


struct usb_device {
    struct usb_manager_binding *usb_manager_binding;
    struct usb_driver_binding *usb_driver_binding;
    char *path;
    struct usb_device *next_pending;

    struct usb_interface *ifaces;

    uint32_t xfer_id;
    struct usb_xfer *xfers;

    struct usb_endpoint ctrl_ep;    /* Control Endpoint 0 */
    struct usb_endpoint *endpoints;
    struct usb_host_controller *controller;
    struct usb_device *parent_hub;
    struct usb_device *parent_hs_hub;



    usb_speed_t speed;
    enum usb_device_state state;

    struct usb_hub *hub;        /* only if this is a hub */
    struct usb_xfer *ctrl_xfer[USB_DEVICE_CTRL_XFER_MAX];

    struct usb_endpoint *ep_clear_stall;   /* current clear stall endpoint */

    uint16_t power_needed;
    uint16_t language_id;

    uint8_t device_address;
    uint8_t device_index;
    uint8_t config_number;
    uint8_t config_index;
    uint8_t depth;
    uint8_t hub_port_index;
    uint8_t hub_port_number;
    uint8_t hs_hub_address;
    uint8_t hs_hub_port_number;
    uint8_t iface_max;
    uint8_t ep_max;

    struct usb_device_flags flags;

    struct usb_device_descriptor device_desc;
    struct usb_endpoint_descriptor ctrl_ep_desc;
    struct usb_config_descriptor *config_desc;
    uint16_t config_desc_size;

    char *serial_number;
    char *manifacturer;
    char *product;

};
#define USB_DEVICE_IS_ROOTHUB(dev) \
        ((dev)->parent_hub == NULL)

struct usb_device *usb_device_get_by_address(uint8_t address);

struct usb_device *usb_device_alloc(struct usb_host_controller *hc, struct usb_device *parent_hub, uint8_t depth,
        uint8_t portindex, uint8_t portno, usb_speed_t speed, usb_mode_t mode);
void usb_device_free(struct usb_device * device, uint8_t flag);

usb_error_t usb_device_set_configuration(struct usb_device *dev,
        uint8_t config);

#endif /* USB_DEVICE_H_ */
