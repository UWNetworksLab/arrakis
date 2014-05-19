/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>


#include <usb/usb.h>

#include "usb_manager_service.h"

static struct usb_manager_thc_export_info export_info;
static struct usb_manager_thc_service_binding_t service;
static struct usb_manager_binding *binding;
static iref_t service_iref;


/**
 * \brief this function initalizes the USB Manager service
 *        using THC
 *
 *        XXX: Currently this is not used.
 */
errval_t usb_manager_service_init(void)
{
    errval_t err;

    err = usb_manager_thc_export(&export_info, USB_MANAGER_SERVICE,
            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT, &service_iref);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to export THC service.");
        return (err);
    }

    err = usb_manager_thc_accept(&export_info, &binding);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "THC accept failed.");
        return (err);
    }

    err = usb_manager_thc_init_service(&service, binding, binding);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "THC init failed.");
        return (err);
    }
    return (SYS_ERR_OK);
}


void usb_manager_service_start(void)
{
    usb_manager_service_msg_t msg;

    struct usb_manager_service_selector selector;

    while(1) {

        service.recv_any(&service, &msg, selector);

        switch(msg.msg) {
            case usb_manager_connect:

                break;

            default:

                break;

        }
    }
}
