/**
 * \file
 * \brief Legacy keyboard driver server.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/keyboard_defs.h>
#include <if/mouse_defs.h>

#include "lpc_kbd.h"

static const char *kservice_name = "keyboard";
static const char *mservice_name = "mouse";
static struct keyboard_binding *kclients = NULL;
static struct mouse_binding *mclients = NULL;

void key_event(uint8_t scancode, bool extended)
{
    // broadcast to all clients
    for (struct keyboard_binding *c = kclients; c != NULL; c = c->st) {
        errval_t err = c->tx_vtbl.key_event(c, NOP_CONT, scancode, extended);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "unable to send key event");
        }
    }
}

void mouse_event(int32_t xdelta, int32_t ydelta,
                 bool left, bool middle, bool right)
{
    // broadcast to all clients
    for (struct mouse_binding *c = mclients; c != NULL; c = c->st) {
        errval_t err = c->tx_vtbl.mouse_event(c, NOP_CONT, xdelta, ydelta,
                                              left, middle, right);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "unable to send mouse event");
        }
    }
}

static errval_t kconnect_handler(void *st, struct keyboard_binding *b)
{
    // add to list of clients
    // XXX: abusing st pointer for list node
    b->st = kclients;
    kclients = b;
    return SYS_ERR_OK;
}


static errval_t mconnect_handler(void *st, struct mouse_binding *b)
{
    // add to list of clients
    // XXX: abusing st pointer for list node
    b->st = mclients;
    mclients = b;
    return SYS_ERR_OK;
}

static void klisten_cb(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    err = nameservice_register(kservice_name, iref);
    assert(err_is_ok(err));
}

static void mlisten_cb(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    err = nameservice_register(mservice_name, iref);
    assert(err_is_ok(err));
}

int main(int argc, char *argv[])
{
    int r = drv_init();
    if (r != 0) {
        USER_PANIC("LPC driver failed to load: %d\n", r);
        return r;
    }

    errval_t err;
    err = keyboard_export(NULL, klisten_cb, kconnect_handler,
                          get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    err = mouse_export(NULL, mlisten_cb, mconnect_handler,
                       get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    messages_handler_loop();
}
