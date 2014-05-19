/**
 * \file
 * \brief Terminal interface for serial driver.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/waitset.h>
#include <term/server/server.h>
#include <term/defs.h>

#include "serial.h"
#include "serial_debug.h"

static struct term_server server;

static void terminal_serial_input(char *data, size_t length)
{
    term_server_send(&server, data, length);
}

static void characters_handler(void *st, char *buffer, size_t length)
{
    serial_write(buffer, length);
    free(buffer);
}

static void configuration_handler(void *st, terminal_config_option_t opt,
                                  char *arguments)
{
    SERIAL_DEBUG("Configuration message received. Option: %d, arguments: %s.\n",
                 opt, arguments);
}

static void new_session_handler(void *st, struct capref session_id)
{
    set_new_input_consumer(terminal_serial_input);
}

void start_terminal_service(char *driver_name)
{
    errval_t err;
    iref_t iref = NULL_IREF;
    char *service_name = NULL;
    size_t size = 0;

    struct waitset *ws = get_default_waitset();

    err = term_server_init(&server, &iref, ws, ws, ws, ws, characters_handler,
                           configuration_handler, new_session_handler);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error exporting terminal interface.");
    }

    /* build service name as driver_nameTERM_SESSION_IF_SUFFIX */
    size = snprintf(NULL, 0, "%s%s", driver_name, TERM_SESSION_IF_SUFFIX);
    service_name = (char *) malloc(size + 1);
    if (service_name == NULL) {
        USER_PANIC("Error allocating memory for service name.");
    }
    snprintf(service_name, size + 1, "%s%s", driver_name,
             TERM_SESSION_IF_SUFFIX);

    /* register terminal session interface at nameservice */
    SERIAL_DEBUG("About to register terminal session interface '%s' at "
                 "nameservice.\n", service_name);
    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error registering terminal session interface '%s' "
                       "at nameservice.", service_name);
    }

    SERIAL_DEBUG("Exporting terminal session interface.\n");
}
