/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <usb/usb.h>
#include <usb/usb_driver.h>
#include <usb/usb_device.h>

#include "usb_manager_client.h"

/*
 * --------------------------------------------------------------------------
 * Variables for connection management to the USB manager service
 * --------------------------------------------------------------------------
 */

/// the iref of the usb manager for connections
iref_t usb_manager_iref;

/// ower own iref
iref_t usb_driver_iref;

/// our own driver binding with the usb manager
struct usb_driver_binding *driver_binding;

/// the usb manager RPC client structure
struct usb_manager_rpc_client usb_manager;

/// state variables for the ensuring that the exportation process is completed
static volatile uint8_t bound = 0;
static volatile uint8_t exported = 0;
static volatile uint8_t manager_connected = 0;

/*
 * -------------------------------------------------------------------------
 * USB driver service functions
 * -------------------------------------------------------------------------
 */

/**
 * \brief this function is called when the client driver receives a detach
 *        notification
 */
void usb_driver_rx_detach_notify(struct usb_driver_binding *b)
{
    debug_printf("device detached... exiting driver.");
    exit(0);
}

/**
 * vtable with callbacks for received messages
 */
static struct usb_driver_rx_vtbl drv_rx_vtbl = {
    .device_detach_notify = usb_driver_rx_detach_notify,
    .transfer_done_notify = usb_driver_rx_done_notify,
};

/**
 * \brief callback when the service export is successful
 */
static void usb_driver_export_cb(void *st, errval_t err, iref_t iref)
{
    USB_DEBUG_IDC("libusb: export cb completed\n");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }
    /* no need to register with name server */
    usb_driver_iref = iref;

    exported = 1;
}

/**
 * \brief this is the callback function when the USB Manager binds to the client
 *        driver upon library initialization
 */
static errval_t usb_driver_connect_cb(void *st, struct usb_driver_binding *b)
{
    USB_DEBUG_IDC("libusb: usb_driver_connect_cb\b");

    driver_binding = b;

    b->rx_vtbl = drv_rx_vtbl;

    manager_connected = 1;

    return (SYS_ERR_OK);
}

/**
 * \brief this is the callback function when the binding with the USB Manager
 *        is completed
 */
static void usb_bind_cb(void *st, errval_t err, struct usb_manager_binding *b)
{
    USB_DEBUG_IDC("libusb: bind callback complete\n");

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "USB manager binding failed");
    }

    usb_manager_rpc_client_init(&usb_manager, b);

    bound = 1;
}

/**
 * \brief   does the initialization of the USB library and the binding to the
 *          USB manager service
 *
 * \param   init_config the configuration to update
 */
usb_error_t usb_lib_init(uint8_t init_config)
{
    errval_t err;

    debug_printf("libusb: initialization.\n");

    err = usb_driver_export(NULL, usb_driver_export_cb, usb_driver_connect_cb,
            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not export the driver interface");
    }

    err = nameservice_blocking_lookup(USB_MANAGER_SERVICE, &usb_manager_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "USB manager service lookup failed");
    }

    err = usb_manager_bind(usb_manager_iref, usb_bind_cb,
            NULL /* state for bind_cb */, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "USB manager binding failed");
    }

    /* wait untill the binding and exportation of the services is completed */
    while (!bound && !exported) {
        err = event_dispatch(get_default_waitset());
        assert(err_is_ok(err));
    }

    uint32_t ret_status;

    uint8_t *tmp;
    size_t length;

    /* connect with the USB Manager */
    err = usb_manager.vtbl.connect(&usb_manager, usb_driver_iref, init_config,
            &ret_status, &tmp, &length);

    if (((usb_error_t) ret_status) != USB_ERR_OK) {
        debug_printf("libusb: ERROR connecting to the USB manager\n");
        return (ret_status);
    }

    /* check if we got enough data for an generic descriptor */
    if (length < sizeof(struct usb_generic_descriptor)) {
        debug_printf("libusb: ERROR received to less data for the generic "
                "descriptor\n");
        return (USB_ERR_BAD_BUFSIZE);
    }

    /* wait until the USB Manager connects with us. */
    while(!manager_connected) {
        err = event_dispatch(get_default_waitset());
    }

    /*
     * initialize the devices with the descriptors
     * Do not free the tmp, since the data is still used by the descriptors
     */
    usb_device_init(tmp);

    USB_DEBUG_IDC("libusb: driver connected (status=%i)\n", ret_status);

    return (USB_ERR_OK);
}
