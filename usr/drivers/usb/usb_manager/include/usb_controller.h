/**
 * \brief this file contains the definitions for the generic USB host controller
 *        and the HCDI function interfaces.
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef USB_CONTROLLER_H
#define USB_CONTROLLER_H

#include <usb_xfer.h> /* usb_xfer_done_queue */

/* prototypes */
struct usb_device;
struct usb_device_request;
struct usb_endpoint;
struct usb_host_controller;

/// the address of the root hub
#define USB_ROOTHUB_ADDRESS 1

/* flags representing USB power states */
#define USB_HW_POWER_CONTROL        0x01
#define USB_HW_POWER_BULK           0x02
#define USB_HW_POWER_INTERRUPT      0x04
#define USB_HW_POWER_ISOC           0x08
#define USB_HW_POWER_NON_ROOT_HUB   0x10
#define USB_HW_POWER_SUSPEND        0x20
#define USB_HW_POWER_RESUME         0x40
#define USB_HW_POWER_SHUTDOWN       0x60


/**
 * Host Controller Driver Interface (HCDI) Pipe Functions
 *
 * This struct contains function pointers to the host controller specific
 * functions for the USB pipes. These functions also depend on the endpoint
 * type.
 */
struct usb_hcdi_pipe_fn {
    void (*open)(struct usb_xfer *);    ///< opens a pipe for a new transfer
    void (*close)(struct usb_xfer *);   ///< closes a pipe
    void (*enter)(struct usb_xfer *);   ///< enter a new transfer to the pipe
    void (*start)(struct usb_xfer *);   ///< starts the transfer on the pipe
};

/**
 * Host Controller Driver Interface (HCDI)
 *
 * This struct contains function pointers to host controller specific
 * functions. Not all function pointers may be initialized.
 */
struct usb_hcdi_bus_fn {
    /* execution of a request on the root hub */
    usb_error_t (*roothub_exec)(struct usb_device *, struct usb_device_request *,
            const void **, uint16_t *);

    /* endpoint initialization and de-initialization */
    void (*endpoint_init)(struct usb_device *, struct usb_endpoint_descriptor *,
            struct usb_endpoint *);
    void (*endpoint_uninit)(struct usb_device *, struct usb_endpoint *);

    /* functions to stall / unstall an endpoint */
    void (*set_stall)(struct usb_device *udev, struct usb_xfer *xfer,
            struct usb_endpoint *ep, uint8_t *did_stall);
    void (*clear_stall)(struct usb_device *udev, struct usb_endpoint *ep);

    /* transfer setup and unsetup */
    void (*xfer_setup)(struct usb_xfer_setup_params *parm);
    void (*xfer_unsetup)(struct usb_xfer *);

    /* functions to update the state of the transfers */
    void (*xfer_poll)(struct usb_host_controller *hc);
    uint8_t (*xfer_finished)(struct usb_xfer *);

    /* device power state */
    void (*device_suspend)(struct usb_device *);
    void (*device_resume)(struct usb_device *);
    void (*set_hw_power)(struct usb_host_controller *);
    void (*set_hw_power_sleep)(struct usb_host_controller *, uint32_t);

    /* device initialization and de initialization */
    usb_error_t (*device_init)(struct usb_device *);
    void (*device_uninit)(struct usb_device *);

    /* special functions for setting the device states */
    void (*device_state_change)(struct usb_device *);
    usb_error_t (*set_address)(struct usb_device *, uint16_t);
};

/// usb interrupt handler function type
typedef void (usb_intr_handler_t)(struct usb_host_controller *);


/**
 * struct defining the generic host controller
 */
struct usb_host_controller {
    usb_hc_version_t hc_type;       ///< the type of the host controller
    void *hc_control;               ///< pointer to the host specific controller
    enum usb_revision usb_revision; ///< the usb revision of the HC

    struct usb_hcdi_bus_fn *hcdi_bus_fn; ///< HCDI functions

    /* usb transfers of this host controller */
    struct usb_xfer_queue intr_queue; ///< started usb transfers
    struct usb_xfer_queue done_queue; ///< inactive usb transfers
    uint16_t uframe_usage[8];         ///< the micro frame usage

    /* usb devices connected to this host controller */
    struct usb_device **devices;     ///< pointer to an array of devices
    struct usb_device *root_hub;     ///< pointer to the root hub device
    uint8_t devices_max;             ///< the maximum supported device number

    uint8_t initialized;             ///< initialized flag
    usb_intr_handler_t *handle_intr; ///< unterrupt handler function

    /* host controller list */
    struct usb_host_controller *next;       ///< pointer to the next HC
    struct usb_host_controller **prev_next; ///< the previous next field

};

/// generic host controller type
typedef struct usb_host_controller usb_host_controller_t;




/* function prototypes */

usb_error_t usb_hc_init(usb_host_controller_t *hc, usb_hc_version_t version,
        uintptr_t controller_base);
void usb_hc_intr_handler(void *arg);


#endif /* _USB_CONTROLLER_H_ */
