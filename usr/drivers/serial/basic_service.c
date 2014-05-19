/**
 * \file
 * \brief Basic/raw interface for serial driver.
 */

/*
 * Copyright (c) 2007, 2008, 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/serial_defs.h>

#include "serial.h"
#include "serial_debug.h"

#define SERVICE_SUFFIX "raw"

/// input buffers, double-buffered for safety with async IDC sends
static struct serial_buffer inbuf[2];
static int ninbuf;

/// current consumer of input
static struct serial_binding *terminal;

static void tx_handler(void *arg)
{
    struct serial_binding *b = arg;
    errval_t err;

    // free previously-sent buffer, if there is one
    if (inbuf[!ninbuf].buf != NULL) {
        free(inbuf[!ninbuf].buf);
        inbuf[!ninbuf].buf = NULL;
    }

    // do we have something to send? if not, bail out
    if (inbuf[ninbuf].buf == NULL) {
        return;
    }

    // try to send
    err = b->tx_vtbl.input(b, MKCONT(tx_handler,b), inbuf[ninbuf].buf,
                           inbuf[ninbuf].len);
    if (err_is_ok(err)) {
        // swing buffer pointer
        ninbuf = !ninbuf;
        assert(inbuf[ninbuf].buf == NULL);
    } else if (err_is_fail(err)) {
        DEBUG_ERR(err, "error sending serial input to terminal");
    }
}

static void basic_serial_input(char *data, size_t length)
{
    if (inbuf[ninbuf].buf == NULL) { // allocate a buffer
        inbuf[ninbuf].buf = malloc(length);
        assert(inbuf[ninbuf].buf != NULL);
        memcpy(inbuf[ninbuf].buf, data, length);
        inbuf[ninbuf].len = length;
    } else { // append new data to existing buffer
        inbuf[ninbuf].buf = realloc(inbuf[ninbuf].buf, inbuf[ninbuf].len + length);
        assert(inbuf[ninbuf].buf != NULL);
        memcpy(inbuf[ninbuf].buf + inbuf[ninbuf].len, data, length);
        inbuf[ninbuf].len += length;
    }

    // try to send something, if we're not already doing so
    if (terminal != NULL && inbuf[!ninbuf].buf == NULL) {
        tx_handler(terminal);
    }
}

static void output_handler(struct serial_binding *b, char *c, size_t len)
{
    serial_write(c, len);
    free(c);
}

static void associate_stdin_handler(struct serial_binding *b)
{
    SERIAL_DEBUG("associate_stdin called on basic interface\n");

    terminal = b;
    set_new_input_consumer(basic_serial_input);
    // try to send something, if we have it ready
    if (inbuf[ninbuf].buf != NULL) {
        tx_handler(b);
    }
}

static struct serial_rx_vtbl serial_rx_vtbl = {
    .output = output_handler,
    .associate_stdin = associate_stdin_handler,
};

static errval_t connect_cb(void *st, struct serial_binding *b)
{
    SERIAL_DEBUG("Client connected to basic interface.\n");

    b->rx_vtbl = serial_rx_vtbl;

    terminal = b;
    set_new_input_consumer(basic_serial_input);
    // try to send something, if we have it ready
    if (inbuf[ninbuf].buf != NULL) {
        tx_handler(b);
    }

    return SYS_ERR_OK;
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    size_t size = 0;
    char *service_name = NULL;
    char *driver_name = (char *) st;

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Exporting basic interface failed.\n");
    }

    // build service name as driver_name.SERVICE_SUFFIX
    size = snprintf(NULL, 0, "%s.%s", driver_name, SERVICE_SUFFIX);
    service_name = (char *) malloc(size + 1);
    if (service_name == NULL) {
        USER_PANIC("Error allocating memory.");
    }
    snprintf(service_name, size + 1, "%s.%s", driver_name, SERVICE_SUFFIX);

    SERIAL_DEBUG("About to register basic interface '%s' at nameservice.\n",
                 service_name);

    // register basic serial driver service at nameservice
    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Registering basic interface at "
                       "nameserver failed.");
    }

    free(service_name);
}

void start_basic_service(char *driver_name)
{
    errval_t err;
    err = serial_export(driver_name, export_cb, connect_cb,
                        get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Preparing basic interface for export failed.");
    }

    SERIAL_DEBUG("Exporting basic interface.\n");
}
