/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <usb/usb.h>

#include <if/keyboard_defs.h>

#include "usb_keyboard_service.h"

/*
 * Service Variables
 */

/// name of the keyboard service
static const char *keyboard_service_name = "keyboard";

/// connected keyboard clients
static struct keyboard_binding *keyboard_clients = NULL;

static volatile uint8_t keyboard_service_registered = 0;


/*
 * Flounder callbacks
 */

/**
 *
 */
static errval_t usb_keyboard_connect_handler(void *st,
        struct keyboard_binding *b)
{
    USB_DEBUG("usb_keyboard_connect_handler()\n");
    // add to list of clients
    // XXX: abusing st pointer for list node
    b->st = keyboard_clients;
    keyboard_clients = b;
    return (SYS_ERR_OK);
}

/**
 *
 */
static void usb_keyboard_export_cb(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    err = nameservice_register(keyboard_service_name, iref);
    assert(err_is_ok(err));

    keyboard_service_registered = 1;
}

/*
 *
 */



/**
 * \brief   broadcasts the key event to all clients
 *
 * \param   scancode
 * \param   extended
 */
void key_event(uint8_t scancode, bool extended)
{
    struct keyboard_binding *c;
    errval_t err;

    for (c = keyboard_clients; c != NULL; c = c->st) {
        err = c->tx_vtbl.key_event(c, NOP_CONT, scancode, extended);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "unable to send key event");
        }
    }
}

/**
 * \brief   initialize the service and exports it
 */
errval_t usb_keyboard_service_init(void)
{
    errval_t err;

    err = keyboard_export(NULL, usb_keyboard_export_cb,
            usb_keyboard_connect_handler, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        debug_printf("ERROR: Could not export the service\n");
        return (err);
    }

    while (!keyboard_service_registered) {
        event_dispatch(get_default_waitset());
    }

    return (SYS_ERR_OK);
}
