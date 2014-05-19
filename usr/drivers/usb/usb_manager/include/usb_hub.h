/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/**
 * ==========================================================================
 * This file contains all the usb_manager related function prototypes and
 * struct definitions.
 *
 * Note: This mirrors the usb_hub.h in the USB library, but with the direct
 * references to the usb xfers and devices instad of pointers
 * ==========================================================================
 */

#ifndef USB_HUB_H
#define USB_HUB_H

struct usb_host_controller;

/// the hub descriptor type value
#define USB_DESCRIPTOR_TYPE_HUB 0x29

/// the maximum depth in the USB tree
#define USB_HUB_MAX_DEPTH   5

/// the interval of the interrupt transfer
#define USB_HUB_INTR_INTERVAL 250

/// the number of transfers of the usb hub (just one)
#define USB_HUB_NUM_TRANSFERS 1

/**
 * defines the characteristics of a hub as part of the hub descriptor
 */
struct usb_hub_characteristics {
    uint8_t _reserved;          ///< reserved, should be zero
    uint8_t port_indicator :1;  ///< are port indicators allowed
    uint8_t tt_think_time :2;   ///< transaction translator think time
    uint8_t protection_mode :2;  ///< over current protection mode
    uint8_t compound_device :1;  ///< flag indicating if it is a compund device
    uint8_t power_mode :2;      ///< logical power switching modes
};

/// usb hub characteristics type
typedef struct usb_hub_characteristics usb_hub_characteristics_t;

/*
 * usb_hub_characteristics.power_mode
 */

/// usb hub logical power switching mode: treat all ports at once
#define USB_HUB_POWER_GANGED            0

/// usb hub logical power switching mode: treat all ports individual
#define USB_HUB_POWER_INDIVIDUAL        1

/// usb hub logical power switching mode: no power switching
#define USB_HUB_POWER_NO_SWITCH         2

/*
 * usb_bub_characteristics.protection_mode
 */

/// over-current status is reported on a global (aggregated) base
#define USB_HUB_PROTECTION_GLOBAL       0

/// over-current status is reported on a per port basis
#define USB_HUB_PROTECTION_INDIVIDUAL   1

/// over current status is not reported
#define USB_HUB_PROTECTION_NONE         2

/*
 * usb_hub_characteristics.tt_think_time
 */

/// think time is 8ms
#define USB_HUB_TT_TIME_8       0

/// think time is 16ms
#define USB_HUB_TT_TIME_16      1

/// think time is 24ms
#define USB_HUB_TT_TIME_24      2

/// think time is 32ms
#define USB_HUB_TT_TIME_32      3

//// Returns the delay from power on to power good in ms
#define USB_HUB_POWER_ON_DELAY(hub) ((hub)->bPwrOn2PwrGood*2)

/**
 * ------------------------------------------------------------------------
 * USB Hub Class Descriptor (USB Specification, Rev 2.0, Section 11.23.2.1 )
 * ------------------------------------------------------------------------
 * The device descriptor for USB hub class devices.
 */
struct usb_hub_descriptor {
    uint8_t bDescLength;           ///< the length of the descriptor in bytes
    uint8_t bDescriptorType;       ///< the descriptor type (0x29)
    uint8_t bNbrPorts;             ///< the number of ports
    struct usb_hub_characteristics wHubCharacteristics;  ///> hub characteristics
    uint8_t bPwrOn2PwrGood;        ///< time from power on till accessible (2ms)
    uint8_t bHubContrCurrent;      ///< max power requirements of the hub (mA)
    uint8_t bDeviceRemovable[32];  ///< device removable bitmap (byte granularity)
}__attribute__((packed));

// The maximum supported ports
#define USB_HUB_MAX_PORTS 255

// checks if the device at port is removable
#define USB_HUB_DEVICE_REMOVABLE(desc, port) \
        (!(((desc)->bDeviceRemovable[(port)/8] >> ((port) % 8)) & 1))

// size definition
#define USB_HUB_DESCRIPTOR_MIN_SIZE 8

/**
 * ------------------------------------------------------------------------
 * USB Hub Status (USB Specification, Rev 2.0, Table 11-19 )
 * ------------------------------------------------------------------------
 * The hub status struct is returned when querying the hub with the
 * usb_hub_get_hub_status() request.
 */
struct usb_hub_status {
    uint16_t _reserved :14;         ///< unused, should be zero
    uint8_t local_power_source :1;  ///< local or external power source
    uint8_t over_current :1;        ///< the ports are draining too much power
    uint16_t _reserved_ :14;        ///< unused, should be zero
    uint8_t local_power_change :1;  ///< indicates a change in local power source
    uint8_t over_current_change :1;  ///< indicates a change in over current
}__attribute__((packed));

/// usb hub status type
typedef struct usb_hub_status usb_hub_status_t;

/// check if the hub has a power supply
#define USB_HUB_STATUS_HAS_POWERSUPPLY(st) (st->local_power_source == 0)

/**
 * ------------------------------------------------------------------------
 * USB Hub Port Status (USB Specification, Rev 2.0, Table 11-19 )
 * ------------------------------------------------------------------------
 * The hub status struct is returned when querying a hub port with the
 * usb_hub_get_port_status() request.
 *
 * The bit locations in the wPortStatus and wPortChange fields correspond
 * in a one-to-one fashion where applicable.
 *
 * USB Specification Rev 2.0, Section 11.24.2.7.1, gives a detailed description
 */
struct usb_hub_port_status {
    struct {
        uint8_t connection :1;  ///< there is a device connected to the port
        uint8_t enabled :1;     ///< the port is enabled
        uint8_t suspend :1;     ///< device on that port is suspended
        uint8_t over_current :1;       ///< a over current condition happened
        uint8_t reset :1;       ///< is set when host wants to reset device
        uint8_t link_state :3;  ///< USB 3.0, unused, set to zero
        uint8_t power_state :1;  ///< local power control state
        uint8_t is_ls :1;       ///< attached devices is a low speed device
        uint8_t is_hs :1;       ///< attached device is a high speed device
        uint8_t test_mode :1;   ///< port operates is in test mode
        uint8_t indicator :1;   ///< set if indicator color is sw controlled
        uint8_t _reserved :2;   ///< unused, set to zero
        uint8_t device_mode :1;  ///< impl specific
    } wPortStatus;              ///< port status flags
    struct {
        uint8_t connect :1;     ///< the current connect status has changed
        uint8_t disabled :1;    ///< the port got disabled to an error
        uint8_t resumed :1;     ///< the device resume procedure is completed
        uint8_t over_current :1;   ///< there is a change in over_current status
        uint8_t is_reset :1;    ///< the reset procedure on the port is complete
        uint8_t bh_is_reset :1;
        uint8_t linkstate :1;
        uint8_t configerr :1;
        uint16_t _reserved :8;  ///< unused, set to zero
    } wPortChange;              ///< port change flags
};

typedef struct usb_hub_port_status usb_hub_portstat_t;

/**
 * this structure represent a port on an USB hub
 */
struct usb_hub_port {
    uint8_t restarts;       ///< the number of restarts on this hub
    uint8_t device_index;   ///< the address of the attached device
    usb_mode_t usb_mode;    ///< the mode host or device mode
}__attribute__((packed));

/// US hub port type
typedef struct usb_hub_port usb_hub_port_t;

/// the maximum number of restarts of a USB hub port
#define USB_HUB_MAX_RESTARTS 5

/**
 * this structure defines how many bites are left in a 1ms USB time slot
 * for full-speed isochronus schedules
 */
struct usb_hub_schedule {
    uint16_t total_bytes;   ///< the total bytes of the isoc schedule
    uint8_t frame_bytes;    ///< the bytes in this USB frame
    uint8_t frame_slot;     ///< the slot of this USB frame
};

typedef enum usb_hub_protocol {
    USB_HUB_FSHUB,
    USB_HUB_HSHUB_SINGLE_TT,
    USB_HUB_HSHUB_MULTI_TT,
    USB_HUB_SSHUB
} usb_hub_protocol;

#define USB_HUB_MAX_ISOC 128
#define USB_HUB_MAX_UFRAMES 8

/**
 *
 */
struct usb_hub {
    usb_hub_status_t hub_status;
    usb_hub_portstat_t port_status;
    struct usb_hub_schedule schedule[USB_HUB_MAX_ISOC];
    struct usb_device *device;
    uint16_t uframe_usage[USB_HUB_MAX_UFRAMES];
    uint16_t portpower;
    uint8_t isoc_last;
    uint8_t num_ports;
    usb_hub_protocol protocol;
    struct usb_xfer *xfers[USB_HUB_NUM_TRANSFERS];
    char name[32];
    usb_hub_port_t ports[0];
};

/*
 * device class codes
 */
#define USB_HUB_CLASS_CODE  0x09
#define USB_HUB_SUBCLASS_CODE 0x00
#define USB_HUB_PROTOCOL_FSHUB       0x00
#define USB_HUB_PROTOCOL_HSHUBSTT    0x01
#define USB_HUB_PROTOCOL_HSHUBMTT    0x02
#define USB_HUB_PROTOCOL_SSHUB       0x03

/*
 * interface class code
 */
#define USB_HUB_IFACE_CLASS_CODE     0x09
#define USB_HUB_IFACE_SUBCLASS_CODE      0
#define USB_HUB_IFACE_PROTOCOL_FSHUB       0
#define USB_HUB_IFACE_PROTOCOL_HSHUBSTT    0   /* Yes, same as previous */
#define USB_HUB_IFACE_PROTOCOL_HSHUBMTT    1

/*
 * USB Hub Class Specific Request Codes
 * (USB Specification, Rev 2.0, Table 11.16)
 */
#define USB_HUB_REQ_GET_STATUS       0
#define USB_HUB_REQ_CLEAR_FEATURE    1
#define USB_HUB_REQ_SET_FEATURE      3
#define USB_HUB_REQ_GET_DESCRIPTOR   6
#define USB_HUB_REQ_SET_DESCRIPTOR   7
#define USB_HUB_REQ_CLEAR_TT_BUFFER  8
#define USB_HUB_REQ_RESET_TT         9
#define USB_HUB_REQ_GET_TT_STATE    10
#define USB_HUB_REQ_STOP_TT         11

/*
 * USB Hub Class Specific Request Codes
 * (USB Specification, Rev 2.0, Table 11.17)
 */
#define USB_HUB_FEATURE_C_HUB_LOCAL_POWER   0
#define USB_HUB_FEATURE_C_HUB_OVER_CURRENT  1
#define USB_HUB_FEATURE_PORT_CONNECTION     0
#define USB_HUB_FEATURE_PORT_ENABLE         1
#define USB_HUB_FEATURE_PORT_SUSPEND        2
#define USB_HUB_FEATURE_PORT_OVER_CURRENT   3
#define USB_HUB_FEATURE_PORT_RESET          4
#define USB_HUB_FEATURE_PORT_POWER          8
#define USB_HUB_FEATURE_PORT_LOW_SPEED      9
#define USB_HUB_FEATURE_C_PORT_CONNECTION   16
#define USB_HUB_FEATURE_C_PORT_ENABLE       17
#define USB_HUB_FEATURE_C_PORT_SUSPEND      18
#define USB_HUB_FEATURE_C_PORT_OVER_CURRENT 19
#define USB_HUB_FEATURE_C_PORT_RESET        20
#define USB_HUB_FEATURE_PORT_TEST           21
#define USB_HUB_FEATURE_PORT_INDICATOR      22

/*
 * USB hub functions
 */

/* initialize / deinitialize the hub device */
usb_error_t usb_hub_init(struct usb_device *hub_device);
usb_error_t usb_hub_deinit(struct usb_device *hub_device);

/* explore the hub */
usb_error_t usb_hub_explore(struct usb_device *hub_device);

/*
 * USB hub device functions
 */
void usb_hub_bandwidth_alloc(struct usb_xfer *xfer);
void usb_hub_bandwidth_free(struct usb_xfer *xfer);
uint8_t usb_hub_bandwidth_adjust(struct usb_device *device, uint16_t length,
        uint8_t slot, uint8_t mask);

/*
 * fullspeed schedule managment
 */
void usb_hub_schedule_init(struct usb_hub_schedule *schedule);
uint8_t usb_hub_schedule_alloc(struct usb_hub_schedule *schedule,
        uint8_t *pstart, uint16_t len);
uint16_t usb_hub_schedule_expand(struct usb_device *udev,
        struct usb_hub_schedule **pp_start, struct usb_hub_schedule **pp_end,
        uint16_t isoc_time);

void usb_hub_root_interrupt(struct usb_host_controller *hc);

usb_error_t uhub_query_info(struct usb_hub *hub, uint8_t *ret_nports,
        uint8_t *ret_ptt);

void usb_hub_set_device(struct usb_hub_port *up, struct usb_device *device,
        uint8_t device_index);
struct usb_device *usb_hub_get_device(struct usb_hub *hub,
        struct usb_hub_port *port);

/* USB hubs specific requests */
usb_error_t usb_hub_clear_hub_feature(struct usb_device *hub, uint16_t feature);
usb_error_t usb_hub_clear_port_feature(struct usb_device *hub, uint16_t feature,
        uint8_t port);
usb_error_t usb_hub_clear_tt_buffer(struct usb_device *hub, uint8_t dev_addr,
        uint8_t ep_num, uint8_t ep_type, uint8_t direction, uint16_t tt_port);
usb_error_t usb_hub_get_hub_status(struct usb_device *hub,
        struct usb_hub_status *ret_status);
usb_error_t usb_hub_get_port_status(struct usb_device *hub, uint16_t port,
        struct usb_hub_port_status *ret_status);
usb_error_t usb_hub_reset_tt(struct usb_device *hub, uint16_t port);

usb_error_t usb_hub_set_hub_feature(struct usb_device *hub, uint16_t feature);
usb_error_t usb_hub_set_port_feature(struct usb_device *hub, uint16_t feature,
        uint8_t port);
usb_error_t usb_hub_get_tt_state(struct usb_device *hub, uint16_t flags,
        uint16_t port, uint16_t max_length, uint16_t ret_length,
        void *ret_state);
usb_error_t usb_hub_stop_tt(struct usb_device *hub, uint16_t port);

usb_error_t usb_hub_get_hub_descriptor(struct usb_device *hub, uint16_t nports,
        struct usb_hub_descriptor *ret_desc);
usb_error_t usb_hub_set_hub_descriptor(struct usb_device *hub,
        uint16_t desc_length, struct usb_hub_descriptor *desc);
usb_error_t usb_hub_re_enumerate(struct usb_device *hub);
usb_error_t usb_hub_reset_port(struct usb_device *hub, uint8_t port);
usb_error_t usb_hub_query_info(struct usb_hub *hub, uint8_t *ret_nports,
        uint8_t *ret_tt);
#endif /* USB_HUB_H_ */
